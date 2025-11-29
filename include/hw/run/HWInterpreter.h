
#pragma once
#include "FST.h"
#include "dyno/ObjMap.h"
#include "hw/HWPrinter.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "op/IDs.h"
#include "support/Debug.h"

#include "dyno/Constant.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
namespace dyno {

class HWInterpreter {
  HWContext &ctx;
  ModuleIRef module;
  std::ostream &os;
  std::ostream &errs;

public:
  ObjMapVec<Wire, BigInt> wireVals;
  ObjMapVec<Register, BigInt> regVals;
  SmallVec<ProcessIRef, 16> evalStack;
  struct DeferredStore {
    StoreIRef store;
    uint32_t addr;
    BigInt value;
  };
  std::optional<FSTWriter<Register, Wire>> fstWriter;

  // register to operand in TriggerIRef
  ObjMapVec<Register, SmallVec<OperandRef, 1>> triggers;
  ObjMapVec<Trigger, SmallVec<DeferredStore, 16>> deferredStores;
  SmallDenseSet<TriggerRef, 4> firedTriggers;

  bool trace = false;

private:
  void queueTriggers(RegisterIRef reg) {
    for (auto use : triggers[reg.oref()]) {
      auto instr = use.instr().as<TriggerIRef>();
      auto idx = use - instr.other_begin();
      switch (instr.oref()->getMode(idx)) {
      case SensMode::POSEDGE:
        if (regVals[reg.oref()].valueEquals(1))
          firedTriggers.insert(instr.oref());
        break;
      case SensMode::NEGEDGE:
        if (regVals[reg.oref()].valueEquals(0))
          firedTriggers.insert(instr.oref());
        break;
      case SensMode::ANYEDGE:
        firedTriggers.insert(instr.oref());
        break;
      default:
      case SensMode::IFF:
      case SensMode::IFFN:
        report_fatal_error("not yet implemented");
      }
    }
  }
  void onValueChange(RegisterIRef reg) {
    if (fstWriter)
      fstWriter->updateValue(reg.oref(), regVals[reg.oref()]);
    scheduleDependentForEval(reg);
    queueTriggers(reg);
  }
  void scheduleDependentForEval(RegisterIRef reg) {
    for (auto use : reg.oref().uses()) {
      if (!use.instr().isOpc(HW_LOAD))
        continue;
      auto load = use.instr().as<LoadIRef>();
      auto proc = load.parentProc(ctx);
      evalStack.emplace_back(proc);
    }
  }

  GenericBigIntRef getValue(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>())
      return GenericBigIntRef{asConst};
    auto wire = value.as<WireRef>();
    BigInt &val = wireVals[wire];
    if (val.getNumBits() == 0) {
      DYNO_DBG("HWInterpreter", {
        std::print(dbgs(), "undefined value, def instr:\n");
        dumpInstr(wire.getDefI());
      });
      dyno_unreachable("undefined value");
    }
    return GenericBigIntRef{val};
  }

  BigInt runBigIntFunc(std::invocable<const GenericBigIntRef &,
                                      const GenericBigIntRef &> auto func,
                       const GenericBigIntRef &lhs,
                       const GenericBigIntRef &rhs) {
    return func(lhs, rhs);
  }
  BigInt runBigIntFunc(std::invocable<BigInt &, const GenericBigIntRef &,
                                      const GenericBigIntRef &> auto func,
                       const GenericBigIntRef &lhs,
                       const GenericBigIntRef &rhs) {
    auto b = BigInt{};
    func(b, lhs, rhs);
    return b;
  }

  template <auto Func> void runNAry(InstrRef instr) {
    auto &val = wireVals[instr.def(0)->as<WireRef>()];
    val = getValue(instr.other(0)->as<HWValue>());
    BigInt::reduce(
        val, instr.others().drop_front().transform([&](size_t, OperandRef ref) {
          return getValue(ref->as<HWValue>());
        }),
        Func);
  }

  uint32_t evalAddress(OperandRef op) {
    if (op == op.instr().end())
      return 0;
    auto base = op->as<ConstantRef>();
    uint32_t addr = base.getExactVal();

    while ((op + 1) != op.instr().end()) {
      auto term = AddressGenTermOperand{op + 1};
      auto idx = getValue(term.getIdx()).getExactVal();
      assert((!term.getMax() || idx < *term.getMax()) &&
             "address out of bounds");
      addr += term.getFact() * idx;
    }

    return addr;
  }

  void runStore(RegisterIRef reg, GenericBigIntRef val, uint32_t addr) {
    auto &regVal = regVals[reg.oref()];
    auto oldCopy = regVal;
    BigInt::insertOp4S(regVal, regVal, val, addr);
    if (oldCopy != regVal)
      onValueChange(reg);
  }

public:
  void runInstr(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
#define LAMBDA(opc, ib, cb, bigIntFunc)                                        \
  case *opc: {                                                                 \
    wireVals[instr.def(0)->as<WireRef>()] =                                    \
        runBigIntFunc(bigIntFunc<GenericBigIntRef, GenericBigIntRef>,          \
                      getValue(instr.other(0)->as<HWValue>()),                 \
                      getValue(instr.other(1)->as<HWValue>()));                \
  }
      FOR_HW_BIN_OPS(LAMBDA)
#undef LAMBDA

#define LAMBDA(opc, ib, cb, bigIntFunc)                                        \
  case *opc: {                                                                 \
    runNAry<bigIntFunc<BigInt, GenericBigIntRef>>(instr);                      \
    break;                                                                     \
  }
      FOR_HW_N_OPS(LAMBDA)
#undef LAMBDA

    case *HW_CONCAT: {
      bool first = true;
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      for (int i = instr.getNumOthers() - 1; i >= 0; i--) {
        auto raw = getValue(instr.other(i)->as<HWValue>());
        if (first)
          val = raw;
        else
          BigInt::concatOp4S(val, raw, val);
        first = false;
      }
      break;
    }

    case *HW_REPEAT: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto cnt = *instr.def(0)->as<WireRef>().getNumBits() /
                 *instr.other(0)->as<WireRef>().getNumBits();
      BigInt::repeatOp4S(val, getValue(instr.other(0)->as<HWValue>()), cnt);
      assert(val.getNumBits() == instr.def(0)->as<WireRef>().getNumBits());
      break;
    }

    case *HW_SPLICE: {
      auto splice = instr.as<SpliceIRef>();
      auto outW = splice.out()->as<WireRef>();
      auto &out = wireVals[outW];
      auto in = getValue(splice.in()->as<HWValue>());
      BigInt::rangeSelectOp4S(out, in, evalAddress(splice.base()),
                              *outW.getNumBits());
    }

    case *HW_INSERT: {
      auto insert = instr.as<InsertIRef>();
      auto &out = wireVals[insert.out()->as<WireRef>()];
      auto base = getValue(insert.val()->as<HWValue>());
      auto in = getValue(insert.in()->as<HWValue>());
      BigInt::insertOp4S(out, base, in, evalAddress(insert.base()));
      break;
    }

    case *OP_NOT: {
      auto &out = wireVals[instr.def(0)->as<WireRef>()];
      auto val = getValue(instr.other(0)->as<HWValue>());
      BigInt::notOp4S(out, val);
      break;
    }

    case *OP_SEXT:
    case *OP_ZEXT:
    case *OP_TRUNC: {
      auto &out = wireVals[instr.def(0)->as<WireRef>()];
      auto val = getValue(instr.other(0)->as<HWValue>());
      auto bits = *instr.def(0)->as<WireRef>().getNumBits();
      if (instr.isOpc(OP_TRUNC))
        assert(val.getNumBits() > bits);
      else
        assert(val.getNumBits() < bits);
      BigInt::resizeOp4S(out, val, bits, instr.isOpc(OP_SEXT));
      break;
    }

    case *OP_ANYEXT: {
      auto &out = wireVals[instr.def(0)->as<WireRef>()];
      auto val = getValue(instr.other(0)->as<HWValue>());
      auto bits = *instr.def(0)->as<WireRef>().getNumBits();

      assert(val.getNumBits() < bits);
      BigInt::concatOp4S(out, PatBigInt::undef(bits - val.getNumBits()), val);
      break;
    }

    case *HW_STORE_DEFER: {
      auto store = instr.as<StoreIRef>();
      auto addr = evalAddress(store.base());

      deferredStores[store.trigger().oref()].emplace_back(
          store, addr, getValue(store.value()));
      break;
    }

    case *HW_STORE: {
      auto store = instr.as<StoreIRef>();
      auto val = getValue(store.value());
      runStore(store.reg().iref(), val, evalAddress(store.base()));
      break;
    }

    case *HW_LOAD: {
      auto load = instr.as<LoadIRef>();
      auto &val = wireVals[load.value()];
      auto &reg = regVals[load.reg()];
      if (load.isFullReg()) {
        val = reg;
        break;
      }
      BigInt::rangeSelectOp4S(val, reg, evalAddress(load.base()),
                              load.getLen());
      break;
    }

    case *HW_MUX: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto sel = getValue(instr.other(0)->as<HWValue>());
      auto trueV = getValue(instr.other(1)->as<HWValue>());
      auto falseV = getValue(instr.other(2)->as<HWValue>());
      assert(sel.getNumBits() == 1);

      if (sel.allBitsUndef()) {
        BigInt mask; // mask of unequal bits
        BigInt::bitsExactEqual4S(mask, trueV, falseV);
        mask |= PatBigInt::undef(mask.getNumBits());
        // xor in to set unequal bits x
        BigInt::xorOp4S(val, trueV, mask);
      } else
        val = !sel.valueEquals(0) ? trueV : falseV;
      break;
    }

    case *OP_ASSERT: {
      auto val = getValue(instr.other(0)->as<HWValue>());
      if (val.valueEquals(0)) {
        std::print(errs, "Assertion Failed at: ");
        for (auto loc : ctx.sourceLocInfo.getSourceLocs(instr))
          std::print(errs, "{}:{}.{}-{}.{}\n", loc.fileName, loc.beginLine,
                     loc.beginCol, loc.endLine, loc.endCol);
        errs << "\n";
      }
      break;
    }
    }
  }

  void evalProc(ProcessIRef proc) {
    HWPrinter print{dbgs()};
    for (auto instr : proc.block()) {
      runInstr(instr);
      if (trace)
        if (instr.getNumDefs() == 1 && instr.def(0)->is<WireRef>()) {
          dbgs() << wireVals[instr.def(0)->as<WireRef>()] << " <- ";
          print.printInstr(instr);
        }
    }
  }

  void evalActive() {
    // active
    while (!evalStack.empty()) {
      evalProc(evalStack.pop_back_val());
    }
  }

  void evalNBA() {
    // deferred stores
    for (auto trigger : firedTriggers) {
      for (auto deferred : deferredStores[trigger]) {
        runStore(deferred.store.reg().iref(), GenericBigIntRef{deferred.value},
                 deferred.addr);
      }
      deferredStores[trigger].clear();
    }
    firedTriggers.clear();
  }

  void eval() {
    evalActive();
    evalNBA();
  }

  void initialEval() {
    for (auto proc : module.procs())
      evalStack.emplace_back(proc);
    eval();
  }

  void setup() {
    wireVals.resize(ctx.getWires().numIDs());
    regVals.resize(ctx.getRegs().numIDs());
    triggers.resize(ctx.getRegs().numIDs());
    deferredStores.resize(ctx.getRegs().numIDs());

    for (auto reg : ctx.getRegs()) {
      if (reg.getNumBits())
        regVals[reg] =
            PatBigInt::fromFourState(FourState::SX, *reg.getNumBits());
    }

    for (auto trigger : ctx.getTriggers()) {
      auto iref = trigger.iref();
      for (auto use : iref.others())
        triggers[use->as<RegisterRef>()].emplace_back(use);
    }
  }

  void dumpRegs() {
    HWPrinter print{os};
    auto tok = print.bindCtx(ctx);
    for (auto reg : module.ports()) {
      print.printRefOrUse(reg.oref());
      os << ": " << regVals[reg.oref()] << "\n";
    }
    os << "\n";
  };

  void evalPrint() {
    auto oldTrace = trace;
    trace = true;
    std::print(os, "Evaluating module with:\n");
    dumpRegs();
    eval();
    std::print(os, "Finished eval. Results:\n");
    dumpRegs();
    trace = oldTrace;
  }

  void setReg(uint i, BigInt b) {
    auto it = module.block().begin();
    std::advance(it, i);
    auto reg = it->as<RegisterIRef>();
    setReg(reg, b);
  }

  void setReg(RegisterIRef reg, BigInt b) {
    auto &slot = regVals[reg.oref()];
    if (slot == b)
      return;
    slot = b;
    onValueChange(reg);
  }

  BigInt &getReg(unsigned i) {
    auto it = module.block().begin();
    std::advance(it, i);
    return regVals[it->as<RegisterIRef>().oref()];
  }

  void clearRegs() {
    regVals.resize(ctx.getRegs().numIDs());
    for (auto reg : ctx.getRegs()) {
      regVals[reg] = PatBigInt::undef(*reg.getNumBits());
    }
  }

  void fstInitHierarchy() {
    for (auto [ref, val] : regVals) {
      if (!ref)
        continue;
      auto reg = ctx.getRegs().resolve(ref);
      auto names = ctx.regNameInfo.getNames(reg);
      auto name = names.empty() ? "reg" + std::to_string(reg.getObjID())
                                : (*names.begin());
      fstWriter->createVar(reg, RegWireFSTWriter::VarType::INTEGER,
                           RegWireFSTWriter::VarDir::INPUT, *reg.getNumBits(),
                           name.c_str());
    }

    fstWriter->endDefinitions();

    for (auto [ref, val] : regVals) {
      if (!ref)
        continue;
      auto reg = ctx.getRegs().resolve(ref);
      fstWriter->updateValue(reg, regVals[reg]);
    }
  }

  void forwardTime(uint64_t incr) {
    if (fstWriter)
      fstWriter->stepForward(incr);
  }

public:
  HWInterpreter(HWContext &ctx, ModuleIRef module, std::ostream &os,
                std::ostream &errs)
      : ctx(ctx), module(module), os(os), errs(errs) {}
}; // namespace dyno
}; // namespace dyno

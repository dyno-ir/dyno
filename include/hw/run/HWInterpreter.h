#pragma once
#include "op/StringObj.h"
#ifdef ENABLE_FST
#include "../../../tools/dyno-sim/include/FST.h"
#endif
#include "dyno/Instr.h"
#include "dyno/ObjMap.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "hw/Wire.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Debug.h"
#include <cassert>
#include <optional>

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
namespace dyno {

class HWInterpreter {
  Context &ctx;
  ModuleIRef module;
  std::ostream &os;
  std::ostream &errs;

public:
  ObjMapVec<Wire, BigInt> wireVals;
  ObjMapVec<Register, BigInt> regVals;
  SmallDenseSet<ObjRef<Instr>, 16> evalSet;
  SmallVec<ProcessIRef, 16> evalStack;
  struct DeferredStore {
    StoreIRef store;
    uint32_t addr;
    BigInt value;
  };
#ifdef ENABLE_FST
  std::optional<FSTWriter<Register, Wire>> fstWriter;
#endif
  // register to operand in TriggerIRef
  ObjMapVec<Register, SmallVec<OperandRef, 1>> triggers;
  ObjMapVec<Trigger, SmallVec<DeferredStore, 16>> deferredStores;
  // failing deferred asserts
  ObjMapVec<Trigger, SmallDenseSet<ObjRef<Instr>>> deferredAsserts;
  SmallDenseSet<ObjRef<Trigger>, 4> firedTriggers;
  std::optional<Range<InstrRef::iterator>> loopYieldVals;

  bool trace = false;

  struct Config {
    bool undefAddressInsertIsNOP = false;
  };
  Config config;

  enum class RunState { INITIAL, MAIN, FINAL };
  RunState runState = RunState::INITIAL;

  Context &getCtx() { return ctx; }

  static constexpr const char *debugName = "HWInterpreter";
  static constexpr uint32_t debugID = 129;

private:
  void queueTriggers(RegisterIRef reg) {
    for (auto use : triggers[reg.oref()]) {
      auto instr = use.instr().as<TriggerIRef>();
      auto idx = use - instr.other_begin();
      switch (instr.oref()->getMode(idx)) {
      case SensMode::POSEDGE:
        if (regVals[reg.oref()].valueEquals(1))
          firedTriggers.findOrInsert(instr.oref());
        break;
      case SensMode::NEGEDGE:
        if (regVals[reg.oref()].valueEquals(0))
          firedTriggers.findOrInsert(instr.oref());
        break;
      case SensMode::ANYEDGE:
        firedTriggers.findOrInsert(instr.oref());
        break;
      default:
      case SensMode::IFF:
      case SensMode::IFFN:
        report_fatal_error("not yet implemented");
      }
    }
  }
  void onValueChange(RegisterIRef reg) {
#ifdef ENABLE_FST
    if (fstWriter)
      fstWriter->updateValue(reg.oref(), regVals[reg.oref()]);
#endif
    scheduleDependentForEval(reg);
    queueTriggers(reg);
  }
  void scheduleDependentForEval(RegisterIRef reg) {
    // initial/final do not schedule dependent.
    if (runState != RunState::MAIN)
      return;
    auto uses =
        reg.oref()
            .uses()
            .filter([](OperandRef ref) { return ref.instr().isOpc(HW_LOAD); })
            .transform([&](size_t, OperandRef ref) {
              return HWInstrRef{ref.instr()}.parentProc(ctx);
            });

    SmallVec<ProcessIRef, 64> usesVec(uses);
    if (module.block().isSorted()) {
      Range{usesVec}.sort([&](InstrRef lhs, InstrRef rhs) {
        return ctx.getCFG()[lhs].getPos() > ctx.getCFG()[rhs].getPos();
      });
    }

    for (auto proc : usesVec) {
      auto [found, it] = evalSet.findOrInsert(proc);
      if (!found) {
        evalStack.emplace_back(proc);
      }
    }
  }

  GenericBigIntRef getValue(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>())
      return GenericBigIntRef{asConst};
    auto wire = value.as<WireRef>();
    BigInt &val = wireVals[wire];
    if (val.getNumBits() == 0) {
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

  std::optional<uint32_t> evalAddress(OperandRef op) {
    if (op == op.instr().end())
      return 0;
    auto base = op->as<ConstantRef>();
    uint32_t addr = base.getExactVal();

    while ((op + 1) != op.instr().end()) {
      auto term = AddressGenTermOperand{op + 1};
      auto idx = getValue(term.getIdx()).getLimitedVal();
      if (!idx)
        return std::nullopt;
      if ((term.getMax() && idx >= *term.getMax()))
        return std::nullopt;

      addr += term.getFact() * *idx;
      op += 3;
    }

    return addr;
  }

  void runStore(RegisterIRef reg, GenericBigIntRef val, uint32_t addr) {
    auto &regVal = regVals[reg.oref()];
    BigInt old;
    BigInt::rangeSelectOp4S(old, regVal, addr, val.getNumBits());
    BigInt::insertOp4S(regVal, regVal, val, addr);
    assert(regVal.getNumWords() != 0);
    if (old != val)
      onValueChange(reg);
  }

  void failedAssert(InstrRef instr) {
    std::print(errs, "failed assertion: ");
    HWCtxPrinter{ctx, errs}.printInstr(instr, true, false);
#ifdef ENABLE_FST
    // finalize FST
    fstWriter.reset();
#endif
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
    break;                                                                     \
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

#define LAMBDA(opc, pred)                                                      \
  case *opc: {                                                                 \
    wireVals[instr.def(0)->as<WireRef>()] = ConstantRef::fromFourState(        \
        BigInt::icmpOp4S(getValue(instr.other(0)->as<HWValue>()),              \
                         getValue(instr.other(1)->as<HWValue>()), pred));      \
    break;                                                                     \
  }
      FOR_OP_ALL_COMPARE_OPS(LAMBDA)
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
      auto addr = evalAddress(splice.base());
      if (!addr)
        out = PatBigInt::undef(splice.getLen());
      else
        BigInt::rangeSelectOp4S(out, in, *addr, *outW.getNumBits());
      break;
    }

    case *HW_INSERT: {
      auto insert = instr.as<InsertIRef>();
      auto &out = wireVals[insert.out()->as<WireRef>()];
      auto base = getValue(insert.in()->as<HWValue>());
      auto val = getValue(insert.val()->as<HWValue>());

      auto addr = evalAddress(insert.base());

      if (!addr) {
        if (!config.undefAddressInsertIsNOP)
          out = PatBigInt::undef(insert.getMemoryLen());
      } else
        BigInt::insertOp4S(out, base, val, *addr);
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

      TriggerIRef trig = store.trigger();
      if (!trig) {
        auto proc = store.parentProc(ctx).as<ProcessIRef>();
        if (proc.getNumOthers() < 1 || !proc.other(0)->is<TriggerRef>())
          abort();
        trig = proc.other(0)->as<TriggerRef>().iref();
      }
      if (!addr) {
        break;
      }
      deferredStores[trig.oref()].emplace_back(store, *addr,
                                               getValue(store.value()));
      break;
    }

    case *HW_STORE: {
      auto store = instr.as<StoreIRef>();
      auto val = getValue(store.value());
      runStore(store.reg().iref(), val, *evalAddress(store.base()));
      break;
    }

    case *HW_LOAD: {
      auto load = instr.as<LoadIRef>();
      auto &val = wireVals[load.value()];
      auto &reg = regVals[load.reg()];
      if (load.isFullReg()) {
        assert(reg.getNumBits() != 0);
        val = reg;
        assert(val.getNumBits() != 0);
        assert(val.getNumBits() == load.getLen());

        break;
      }
      auto addr = evalAddress(load.base());
      if (!addr) {
        val = PatBigInt::undef(load.getLen());
        break;
      }
      BigInt::rangeSelectOp4S(val, reg, *addr, load.getLen());
      assert(val.getNumBits() != 0);
      assert(val.getNumBits() == load.getLen());
      break;
    }

    case *HW_MUX: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto sel = getValue(instr.other(0)->as<HWValue>());
      auto trueV = getValue(instr.other(1)->as<HWValue>());
      auto falseV = getValue(instr.other(2)->as<HWValue>());
      assert(sel.getNumBits() == 1);

      if (sel.allBitsUndef()) {
        // mask of unequal bits
        BigInt mask = BigInt::bitsExactEqual4S(trueV, falseV);
        mask |= PatBigInt::undef(mask.getNumBits());
        // xor in to set unequal bits x
        BigInt::xnorOp4S(val, trueV, mask);
      } else
        val = !sel.valueEquals(0) ? trueV : falseV;
      break;
    }

    case *OP_ASSERT: {
      auto val = getValue(instr.other(0)->as<HWValue>());
      if (!val.valueEquals(1)) {
        failedAssert(instr);
        report_fatal_error("HWInterpreter: failed assert");
      }
      break;
    }

    case *HW_ASSERT_DEFER: {
      auto trigger = instr.operand(1)->as<TriggerRef>();
      auto val = getValue(instr.operand(0)->as<HWValue>());
      if (val.valueEquals(1)) {
        if (auto it = deferredAsserts[trigger].find(instr))
          deferredAsserts[trigger].erase(it);
      } else {
        deferredAsserts[trigger].findOrInsert(instr);
      }
      break;
    }

    case *HW_PRINT: {
      auto strObjRef = instr.operand(0)->as<StringObjRef>();
      auto &str = strObjRef->data;

      auto argIt = instr.operand(1);

      // todo: full verilog formatting
      uint64_t pos = 0;
      while (1) {
        auto idx = str.find('%', pos);

        if (argIt == instr.end() && idx == std::string::npos) {
          std::print(os, "{}", std::string_view(str).substr(pos));
          break;
        }

        if (idx == std::string::npos) {
          std::print(os, "{}", std::string_view(str).substr(pos));
          if (argIt != instr.end()) {
            dumpInstr(instr, ctx);
            report_fatal_error("too many print args");
          }
          break;
        }

        std::print(os, "{}", std::string_view(str).substr(pos, idx - pos));

        if (argIt == instr.end()) {
          dumpInstr(instr, ctx);
          report_fatal_error("too few print args");
        }

        auto arg = getValue(argIt->as<HWValue>());
        switch (str[idx + 1]) {
        case 'c':
          std::print(os, "{}", (char)arg.getExactVal());
          break;
        case 'x':
          BigInt::stream_hex_4s_vlog(os, arg);
          break;
        case 'b':
          BigInt::stream_bin_4s_vlog(os, arg);
          break;
        case 'd':
        case 'u':
          if (arg.getIs4S())
            os << 'X';
          else
            BigInt::stream_dec(os, arg, str[idx + 1] == 'd');
          break;
        default: {
          dumpInstr(instr, ctx);
          report_fatal_error("invalid fmt string");
        }
        }

        ++argIt;
        pos = idx + 2;
      }
      break;
    }

    case *OP_IF: {
      auto ifInstr = instr.as<IfInstrRef>();
      auto cond = getValue(ifInstr.getCondValue()->as<HWValue>());
      InstrRef yieldI = nullref;
      if (cond.valueEquals(1)) {
        evalBlock(ifInstr.getTrueBlock());
        yieldI = ifInstr.getInnerYieldTrue();
      } else if (ifInstr.hasFalseBlock()) {
        evalBlock(ifInstr.getFalseBlock());
        yieldI = ifInstr.getInnerYieldFalse();
      }

      if (yieldI) {
        auto range = ifInstr.yieldValues().deref().as<WireRef>().zip(
            yieldI.others().deref().as<HWValue>());
        for (auto [dst, src] : range)
          wireVals[dst] = getValue(src);
      }
      break;
    }

    case *OP_SWITCH: {
      auto switchInstr = instr.as<SwitchInstrRef>();
      auto cond = getValue(switchInstr.cond()->as<HWValue>());
      CaseInstrRef caseInstr = nullref;
      CaseInstrRef defaultInstr = nullref;

      for (auto caseI : Range{switchInstr.block()}.as<CaseInstrRef>()) {
        if (caseI.isDefault()) {
          defaultInstr = caseI;
          continue;
        }
        for (auto val : caseI.labels().deref().as<HWValue>()) {
          switch (*caseI.getDialectOpcode()) {
          // todo: casex, casez
          case *OP_CASE:
            if (getValue(val) == cond) {
              caseInstr = caseI;
              break;
            }
            continue;
          case *HW_CASE_X:
            if (BigInt::icmpCaseXEqualOp4S(getValue(val), cond)) {
              caseInstr = caseI;
              break;
            }
            continue;
          case *HW_CASE_Z:
            if (BigInt::icmpCaseZEqualOp4S(getValue(val), cond)) {
              caseInstr = caseI;
              break;
            }
            continue;
          }
          break;
        }
      }
      if (!caseInstr)
        caseInstr = defaultInstr;
      if (!caseInstr)
        break;

      evalBlock(caseInstr.block());

      InstrRef yieldI = caseInstr.getYield();
      if (yieldI) {
        auto range = switchInstr.yieldValues().deref().as<WireRef>().zip(
            yieldI.others().deref().as<HWValue>());
        for (auto [dst, src] : range)
          wireVals[dst] = getValue(src);
      }
      break;
    }

    case *OP_WHILE: {
      auto whileInstr = instr.as<WhileInstrRef>();
      loopYieldVals = whileInstr.inputValues();

      while (true) {
        evalBlock(whileInstr.getCondBlock());
        auto condV = getValue((*loopYieldVals->begin())->as<HWValue>());
        // drop the yielded condition
        loopYieldVals = loopYieldVals->drop_front();
        if (!condV.valueEquals(1))
          break;

        evalBlock(whileInstr.getBodyBlock());
      }

      auto range = whileInstr.yieldValues().deref().as<WireRef>().zip(
          loopYieldVals->deref().as<HWValue>());

      for (auto [dst, src] : range)
        wireVals[dst] = getValue(src);

      break;
    }

    case *OP_FOR: {
      auto forInstr = instr.as<ForInstrRef>();
      loopYieldVals = forInstr.inputValues();

      for (BigInt it = forInstr.getLower()->as<ConstantRef>();
           it != forInstr.getUpper()->as<ConstantRef>();
           it += forInstr.getStep()->as<ConstantRef>()) {
        // handle iterator unyield value manually
        wireVals[forInstr.getUnyield().def(0)->as<WireRef>()] = it;
        evalBlock(forInstr.getBlock());
      }

      auto range = forInstr.yieldValues().deref().as<WireRef>().zip(
          loopYieldVals->deref().as<HWValue>());

      for (auto [dst, src] : range)
        wireVals[dst] = getValue(src);

      break;
    }

    case *OP_YIELD: {
      auto parentI = HWInstrRef{instr}.parentBlock(ctx).defI();
      if (parentI.isOpc(OP_WHILE, OP_DO_WHILE, OP_FOR))
        loopYieldVals = instr.others();

      break;
    }

    case *OP_UNYIELD: {
      auto parentI = HWInstrRef{instr}.parentBlock(ctx).defI();
      auto defs = instr.defs();
      if (parentI.isOpc(OP_FOR))
        defs = defs.drop_front();
      auto range =
          defs.deref().as<WireRef>().zip(loopYieldVals->deref().as<HWValue>());
      for (auto [dst, src] : range)
        wireVals[dst] = getValue(src);
      break;
    }

    case *HW_ASSUME: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto in = getValue(instr.other(0)->as<HWValue>());
      auto cond = getValue(instr.other(1)->as<HWValue>());
      assert(cond.getNumBits() == 1);
      if (!cond.valueEquals(0))
        val = in;
      else
        val = PatBigInt::undef(in.getNumBits());
      break;
    }

    case *HW_ONEHOT_MUX: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      bool found = false;
      for (auto [sel, caseVal] : instr.others().as<HWValue>().pairwise()) {
        auto selV = getValue(sel);
        assert(selV.getNumBits() == 1);
        if (selV.valueEquals(1)) {
          if (found)
            report_fatal_error("more than one select active on one hot mux");
          val = getValue(caseVal);
          found = true;
        }
      }

      if (!found)
        val = PatBigInt::undef(*instr.def(0)->as<WireRef>().getNumBits());

      break;
    }

    default: {
      dumpInstr(instr);
      report_fatal_error("unsupported opcode");
    }
    }

#ifdef ENABLE_FST
    // if (fstWriter)
    //   for (auto def : instr.defs()) {
    //     if (def->is<WireRef>()) {
    //       fstWriter->updateValue(def->as<WireRef>(),
    //                              wireVals[def->as<WireRef>()]);
    //     }
    //   }
#endif
  }

  void evalBlock(BlockRef block) {
    HWPrinter print{dbgs()};
    auto tok = print.bindCtx(ctx);
    for (auto instr : block) {
      runInstr(instr);
      if (trace) {
        for (auto [back, def] : instr.defs().mark_back()) {
          if (!def->is<WireRef>())
            continue;
          dbgs() << wireVals[def->as<WireRef>()];
          dbgs() << (back ? " <- " : ", ");
        }
        print.printInstr(instr, true, false);
      }

      bool debug = false;
      if (debug) {
        for (auto other : instr.others()) {
          if (!other->is<WireRef>())
            continue;
          dumpObj(other->fat());
          std::print(dbgs(), " = {}\n", wireVals[other->as<WireRef>()]);
        }
        for (auto def : instr.defs()) {
          if (!def->is<WireRef>())
            continue;
          dumpObj(def->fat());
          std::print(dbgs(), " <- {}\n", wireVals[def->as<WireRef>()]);
        }
        dumpInstr(instr);
      }
    }
  }

  // required w/o deferred SSA construct, if there are nested store defers
  // that may have been queued during a previous eval.
  void removeProcStoreDefers(ProcessIRef proc) {
    for (auto [trig, stores] : deferredStores) {
      if (!ctx.getStore<Trigger>().exists(trig))
        continue;
      for (auto it = stores.begin(); it != stores.end(); ++it) {
        auto &store = *it;
        if (HWInstrRef{store.store}.parentProc(ctx) == proc) {
          if (stores.erase_unordered(it))
            --it;
          else
            break;
        }
      }
    }
  }

  void removeProcAssertDefers(ProcessIRef proc) {
    for (auto [trig, asserts] : deferredAsserts) {
      if (!ctx.getStore<Trigger>().exists(trig))
        continue;
      for (auto it = asserts.begin(); it != asserts.end();) {
        auto assertI = ctx.resolve(*it);
        if (HWInstrRef{assertI}.parentProc(ctx) == proc) {
          it = asserts.erase(it);
        } else
          ++it;
      }
    }
  }

  void evalProc(ProcessIRef proc) {
    // removeProcStoreDefers(proc);
    removeProcAssertDefers(proc);
    DYNO_DBG(std::print(dbgs(), "eval: "); dumpInstr(proc, ctx, true, false);)
    evalBlock(proc.block());
  }

  void evalActive() {
    // active
    while (!evalStack.empty()) {
      auto ref = evalStack.pop_back_val();
      evalSet.erase(evalSet.find(ref));
      evalProc(ref);
    }
  }

  void evalNBA() {
    // deferred stores
    for (auto trigger : Range{firedTriggers}.resolve(ctx)) {
      for (auto assert : deferredAsserts[trigger])
        failedAssert(ctx.resolve(assert));
      if (!deferredAsserts[trigger].empty())
        report_fatal_error("HWInterpreter: failed assert");
      for (auto deferred : deferredStores[trigger]) {
        runStore(deferred.store.reg().iref(), GenericBigIntRef{deferred.value},
                 deferred.addr);
      }
      deferredStores[trigger].clear();
      deferredAsserts[trigger].clear();
    }
    firedTriggers.clear();
  }

  void eval() {
    while (!evalStack.empty() || !firedTriggers.empty()) {
      if (!evalStack.empty())
        evalActive();
      else if (!firedTriggers.empty())
        evalNBA();
    }
  }

  // initial run for always_comb processes, runs after initialEval for actual
  // initial procs
  void initialCombEval() {
    for (auto proc : module.procs()) {
      if (!proc.isOpc(HW_COMB_PROCESS_DEF, HW_NETLIST_PROCESS_DEF))
        continue;
      evalSet.insert(proc);
      evalStack.emplace_back(proc);
    }
    eval();
  }

  void initialEval() {
    assert(runState == RunState::INITIAL);

    for (auto proc : module.procs()) {
      if (!proc.isOpc(HW_INIT_PROCESS_DEF))
        continue;
      evalSet.insert(proc);
      evalStack.emplace_back(proc);
    }
    eval();
    runState = RunState::MAIN;
  }

  void setup() {
    wireVals.resize(ctx.getStore<Wire>().numIDs());
    regVals.resize(ctx.getStore<Register>().numIDs());
    triggers.resize(ctx.getStore<Register>().numIDs());
    deferredStores.resize(ctx.getStore<Trigger>().numIDs());
    deferredAsserts.resize(ctx.getStore<Trigger>().numIDs());

    auto &regResetValues = ctx.getCtx<HWDialectContext>().regResetValue;

    for (auto reg : ctx.getStore<Register>()) {
      if (regResetValues.inRange(reg) && regResetValues[reg])
        regVals[reg] = ctx.getStore<Constant>().resolve(regResetValues[reg]);
      else if (reg.getNumBits())
        regVals[reg] =
            PatBigInt::fromFourState(FourState::SX, *reg.getNumBits());
    }

    for (auto trigger : ctx.getStore<Trigger>()) {
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

  void setReg(RegisterIRef reg, const BigInt &b) {
    auto &slot = regVals[reg.oref()];
    if (slot == b)
      return;
    slot = b;
    onValueChange(reg);
  }

  void regValueChanged(RegisterIRef reg) { onValueChange(reg); }

  BigInt &getReg(unsigned i) {
    auto it = module.block().begin();
    std::advance(it, i);
    return regVals[it->as<RegisterIRef>().oref()];
  }
  BigInt &getReg(RegisterIRef reg) { return regVals[reg.oref()]; }
  const BigInt &getWire(WireRef wire) { return wireVals[wire]; }

  void clearRegs() {
    regVals.resize(ctx.getStore<Register>().numIDs());
    for (auto reg : ctx.getStore<Register>()) {
      regVals[reg] = PatBigInt::undef(*reg.getNumBits());
    }
  }
#ifdef ENABLE_FST
  void fstInitHierarchy() {
    for (auto [ref, val] : regVals) {
      if (!ctx.getStore<Register>().exists(ref))
        continue;
      auto reg = ctx.getStore<Register>().resolve(ref);
      if (!reg.hasSingleDef() ||
          HWInstrRef{reg.iref()}.parentMod(ctx) != module)
        continue;
      auto names = ctx.getCtx<HWDialectContext>().regNameInfo.getNames(reg);
      SmallVec<const char *, 16> namesVec(names);
      std::string numericName = "r" + std::to_string(reg.getObjID());
      namesVec.emplace_back(numericName.c_str());

      auto regType =
          ctx.getCtx<HWDialectContext>().regTypeInfo.getType(ctx, ref);
      fstWriter->createVar(reg, regType, RegWireFSTWriter::VarDir::INPUT,
                           *reg.getNumBits(), namesVec.front());
    }

    // for (auto [ref, val] : wireVals) {
    //   if (!ctx.getStore<Wire>().exists(ref))
    //     continue;
    //   auto wire = ctx.getStore<Wire>().resolve(ref);
    //   if (!wire.hasSingleDef() ||
    //       HWInstrRef{wire.getDefI()}.parentMod(ctx) != module)
    //     continue;
    //   auto name = std::string("w") + std::to_string(wire.getObjID().num);
    //   fstWriter->createVar(wire, nullref, RegWireFSTWriter::VarDir::INPUT,
    //                        *wire.getNumBits(), name.c_str());
    // }

    fstWriter->endDefinitions();

    for (auto [ref, val] : regVals) {
      if (!ctx.getStore<Register>().exists(ref))
        continue;
      auto reg = ctx.getStore<Register>().resolve(ref);
      if (!reg.hasSingleDef() ||
          HWInstrRef{reg.iref()}.parentMod(ctx) != module)
        continue;
      fstWriter->updateValue(reg, regVals[reg]);
    }
    // for (auto [ref, val] : wireVals) {
    //   if (!ctx.getStore<Wire>().exists(ref))
    //     continue;
    //   auto wire = ctx.getStore<Wire>().resolve(ref);
    //   if (!wire.hasSingleDef() ||
    //       HWInstrRef{wire.getDefI()}.parentMod(ctx) != module)
    //     continue;
    //   fstWriter->updateValue(wire, wireVals[wire].getNumBits() == 0
    //                                    ? PatBigInt::undef(*wire.getNumBits())
    //                                    : wireVals[wire]);
    // }
  }
#endif

  void forwardTime(uint64_t incr) {
#ifdef ENABLE_FST
    if (fstWriter)
      fstWriter->stepForward(incr);
#endif
  }

public:
  HWInterpreter(Context &ctx, ModuleIRef module, std::ostream &os,
                std::ostream &errs)
      : ctx(ctx), module(module), os(os), errs(errs) {}
}; // namespace dyno
}; // namespace dyno

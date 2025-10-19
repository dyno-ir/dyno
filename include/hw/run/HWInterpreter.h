#pragma once
#include "hw/HWPrinter.h"
#include "hw/Register.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"

#include "dyno/Constant.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
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
  bool trace = false;

private:
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
      bool first = true;
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      for (int i = instr.getNumOthers() - 3; i >= 0; i -= 3) {
        auto len = getValue(instr.other(i + 2)->as<HWValue>());
        auto addr = getValue(instr.other(i + 1)->as<HWValue>());
        auto raw = getValue(instr.other(i)->as<HWValue>());
        assert(addr.getNumBits() == 32 && len.getNumBits() == 32);

        if (first)
          BigInt::rangeSelectOp4S(val, raw, addr.getExactVal(),
                                  len.getExactVal());
        else {
          BigInt tmp;
          BigInt::rangeSelectOp4S(tmp, raw, addr.getExactVal(),
                                  len.getExactVal());
          BigInt::concatOp4S(val, tmp, val);
        }
        first = false;
      }
      break;
    }

    case *HW_INSERT: {
      auto &out = wireVals[instr.def(0)->as<WireRef>()];
      auto base = getValue(instr.other(0)->as<HWValue>());
      auto val = getValue(instr.other(1)->as<HWValue>());
      auto addr = getValue(instr.other(2)->as<HWValue>());
      auto len = getValue(instr.other(3)->as<HWValue>());
      runInsert(out, base, val, addr, len);
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

    case *HW_STORE: {
      auto val = getValue(instr.other(0)->as<HWValue>());
      auto &reg = regVals[instr.other(1)->as<RegisterRef>()];
      if (instr.getNumOperands() == 2) {
        reg = val;
        break;
      }
      auto addr = getValue(instr.other(2)->as<HWValue>());
      auto len = getValue(instr.other(3)->as<HWValue>());
      BigInt::insertOp4S(reg, reg, val, addr.getExactVal());
      break;
    }

    case *HW_LOAD: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto &reg = regVals[instr.other(0)->as<RegisterRef>()];
      if (instr.getNumOperands() == 2) {
        val = reg;
        break;
      }
      auto addr = getValue(instr.other(1)->as<HWValue>());
      auto len = getValue(instr.other(2)->as<HWValue>());
      assert(addr.getNumBits() == 32 && len.getNumBits() == 32);
      BigInt::rangeSelectOp4S(val, reg, addr.getExactVal(), len.getExactVal());
      break;
    }

    case *HW_MUX: {
      auto &val = wireVals[instr.def(0)->as<WireRef>()];
      auto sel = getValue(instr.other(0)->as<HWValue>());
      auto trueV = getValue(instr.other(1)->as<HWValue>());
      auto falseV = getValue(instr.other(2)->as<HWValue>());
      assert(sel.getNumBits() == 1);
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

  void eval() {
    // todo: proper ordering, event loop
    wireVals.resize(ctx.getWires().numIDs());
    regVals.resize(ctx.getRegs().numIDs());
    for (auto proc : module.procs()) {
      evalProc(proc);
    }
  }

  void evalPrint() {
    auto oldTrace = trace;
    trace = true;
    std::print(os, "Evaluating module with:\n");
    HWPrinter print{os};

    auto dumpRegs = [&]() {
      for (auto reg : module.ports()) {
        print.printRefOrUse(reg.oref());
        os << ": " << regVals[reg.oref()] << "\n";
      }
    };
    dumpRegs();
    eval();
    std::print(os, "Finished eval. Results:\n");
    dumpRegs();
    trace = oldTrace;
  }

  void setReg(unsigned i, BigInt b) {
    auto it = module.block().begin();
    std::advance(it, i);
    regVals[it->as<RegisterIRef>().oref()] = b;
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

public:
  HWInterpreter(HWContext &ctx, ModuleIRef module, std::ostream &os,
                std::ostream &errs)
      : ctx(ctx), module(module), os(os), errs(errs) {}
}; // namespace dyno
}; // namespace dyno

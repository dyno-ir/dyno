#pragma once
#include "BitRange.h"
#include "HWValue.h"
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/BitRange.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ArrayRef.h"
#include "support/RTTI.h"
#include "support/TemplateUtil.h"
#include "support/Utility.h"
#include <algorithm>
#include <dyno/NewDeleteObjStore.h>
#include <type_traits>

namespace dyno {

class HWContext {

  ConstantStore constants;
  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Function> funcs;
  NewDeleteObjStore<Process> procs;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;

public:
  auto &getConstants() { return constants; }
  auto &getModules() { return modules; }
  auto &getRegs() { return regs; }
  auto &getWires() { return wires; }
  auto &getFuncs() { return funcs; }
  auto &getProcs() { return procs; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }

  ModuleIRef createModule(std::string_view name) {
    auto moduleRef = modules.create(std::string(name));
    auto moduleInstr = InstrRef{instrs.create(2, HW_MODULE_INSTR)};

    InstrBuilder{moduleInstr}.addRef(moduleRef).addRef(createBlock());
    return moduleInstr;
  }

  BlockRef createBlock() {
    auto blockRef = cfg.blocks.create(cfg);
    // auto blockInstrRef =
    //     InstrRef{instrs.create(1 + sizeof...(parents),
    //     DialectID{DIALECT_HW},
    //                            OpcodeID{HW_BLOCK_INSTR})};
    // InstrBuilder build{blockInstrRef};
    // build.addRef(blockRef).other();
    //(([&]() { build.addRef(parents); })(), ...);
    return blockRef;
  }

  ConstantBuilder constBuild() { return ConstantBuilder{constants}; }
};

class HWInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  HWInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  WireRef operandW(uint n) { return operand(n)->as<WireRef>(); }
  WireRef defW(uint n = 0) {
    assert(n < getNumDefs());
    return operandW(n);
  }
  // todo: get rid of ctx params via global directory.
  auto iter(HWContext &ctx) { return ctx.getCFG()[this->as<ObjRef<Instr>>()]; }
  BlockRef parentBlock(HWContext &ctx) { return iter(ctx).blockRef(); }
  FatRefUnion<ProcessIRef, FunctionIRef> parent(HWContext &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      if (auto parent =
              block.defI().dyn_as<FatRefUnion<ProcessIRef, FunctionIRef>>())
        return parent;
      return HWInstrRef{block.defI()}.parent(ctx);
    }
  }
  ProcessIRef parentProc(HWContext &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<ProcessIRef>() && "parent is not process");
    return rv.as<ProcessIRef>();
  }
  FunctionIRef parentFunc(HWContext &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<FunctionIRef>() && "parent is not function");
    return rv.as<FunctionIRef>();
  }
  ModuleIRef parentMod(HWContext &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      if (auto mod = block.defI().dyn_as<ModuleIRef>())
        return mod;
      return HWInstrRef{block.defI()}.parentMod(ctx);
    }
  }
};

class HWInstrBuilder {
public:
  HWContext &ctx;
  BlockRef_iterator<true> insert;

  HWInstrBuilder(HWContext &ctx, BlockRef_iterator<true> insert)
      : ctx(ctx), insert(insert) {}

  HWInstrBuilder(HWContext &ctx) : ctx(ctx), insert() {}

  void insertInstr(InstrRef instr) { insert.insertPrev(instr); }

protected:
  // Compile-time edge cases for adding special types like BitRange
  // todo: rename this and put this in new HWInstrBuilder deriving from instr
  // build
  template <typename T> void addSpecialRef(InstrBuilder &build, const T &ref) {
    if constexpr (std::is_same_v<BitRange, T>) {
      build.addRef(ref.getAddr());
      build.addRef(ref.getLen());
    } else {
      build.addRef(ref);
    }
  }

public:
  template <typename... Ts>
  void addRefs(InstrRef instr, bool addWireDef, Ts... operands) {
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    ([&] { addSpecialRef(build, operands); }(), ...);
  }

  template <typename... Ts> constexpr unsigned getNumOperands() {
    unsigned size = 0;
    (
        // Special types may need more than one operand slot
        [&] {
          if constexpr (std::is_same_v<BitRange, std::decay_t<Ts>>)
            size += 2;
          else
            size += 1;
        }(),
        ...);
    return size;
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectID dialect, OpcodeID opcode, bool addWireDef,
                        Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        addWireDef + getNumOperands<Ts...>(), dialect, opcode)};

    insertInstr(instr);
    addRefs(instr, addWireDef, operands...);
    return HWInstrRef{instr};
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectOpcode opc, bool addWireDef, Ts... operands) {
    return buildInstr(opc.getDialectID(), opc.getOpcodeID(), addWireDef,
                      operands...);
  }

#define COMM_OP(ident, opcode, constFunc)                                      \
  template <IsAnyHWValue T, IsAnyHWValue... Ts>                                \
  HWValue ident(T first, Ts... rest) {                                         \
    if constexpr (!(std::is_same_v<T, WireRef> ||                              \
                    (std::is_same_v<Ts, WireRef> || ...)))                     \
      if (first.template is<ConstantRef>() &&                                  \
          (rest.template is<ConstantRef>() && ...)) {                          \
        ConstantBuilder build{ctx.getConstants()};                             \
        build.val(first.template as<ConstantRef>());                           \
        ([&] { build.constFunc(rest.template as<ConstantRef>()); }(), ...);    \
        return build.get();                                                    \
      }                                                                        \
    auto rv = buildInstr(opcode, true, first, rest...).defW();                 \
    rv->numBits = first.getNumBits();                                          \
    return rv;                                                                 \
  }

  COMM_OP(buildAdd, OP_ADD, add)
  COMM_OP(buildAnd, OP_AND, bitAND)
  COMM_OP(buildOr, OP_OR, bitOR)
  COMM_OP(buildXor, OP_XOR, bitXOR)
  COMM_OP(buildXNor, OP_XNOR, bitXNOR)
  COMM_OP(buildMul, OP_MUL, mul)

  // template <IsAnyHWValue... Ts> HWInstrRef buildAdd2(Ts... operands) {

  //   // canonicalize further by having constants rightmost, possible even
  //   // sort wires by def opcode?

  //   // todo: bitmap
  //   std::array<bool, sizeof...(operands)> isSameOpc = {};
  //   ssize_t operandDelta = 0;
  //   bool anyIsSameOpc = 0;

  //   size_t index = 0;
  //   for (HWValue operand : {operands...}) {
  //     // todo: via constexpr ifs for direct ConstantRef/WireRef
  //     if (auto asWire = operand.template dyn_as<WireRef>();
  //         asWire && asWire.hasSingleDef() &&
  //         asWire.getSingleDef()->instr().isOpc(DialectID{DIALECT_OP},
  //                                              OpcodeID{OP_ADD})) {
  //       anyIsSameOpc = 1;
  //       isSameOpc[index] = 1;
  //       operandDelta = asWire.getSingleDef()->instr().getNumOthers() - 1;
  //     }
  //     index++;
  //   }

  //   if (!anyIsSameOpc) {
  //     return buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_ADD}, true,
  //                       operands...);
  //   }

  //   auto instr = InstrRef{
  //       ctx.getInstrs().create(1 + sizeof...(operands) + operandDelta,
  //                              DialectID{DIALECT_OP}, OpcodeID{OP_ADD})};
  //   insertInstr(instr);
  //   InstrBuilder build{instr};
  //   // todo: size?
  //   build.addRef(ctx.getWires().create());
  //   build.other();

  //   index = 0;
  //   for (HWValue operand : {operands...}) {
  //     if (isSameOpc[index]) {
  //       InstrRef otherInstr = operand.as<WireRef>().getSingleDef()->instr();
  //       for (auto subOp :
  //            operand.as<WireRef>().getSingleDef()->instr().others())
  //         build.addRef(subOp->template as<HWValue>());

  //       ctx.getCFG()[otherInstr].erase();
  //       ctx.getInstrs().destroy(otherInstr);
  //     } else
  //       build.addRef(operand);
  //     index++;
  //   }

  //   return HWInstrRef{instr};
  // }

#define BINOP(ident, opcode, constFunc)                                        \
  template <IsAnyHWValue LHS, IsAnyHWValue RHS>                                \
  HWValue ident(LHS lhs, RHS rhs) {                                            \
    if (lhs.template is<ConstantRef>() && rhs.template is<ConstantRef>()) {    \
      return ConstantBuilder{ctx.getConstants()}                               \
          .val(lhs.template as<ConstantRef>())                                 \
          .constFunc(rhs.template as<ConstantRef>())                           \
          .get();                                                              \
    }                                                                          \
    auto rv = buildInstr(opcode, true, lhs, rhs).defW();                       \
    rv->numBits = rhs.getNumBits();                                            \
    return rv;                                                                 \
  }

  BINOP(buildSub, OP_SUB, sub)
  BINOP(buildUDiv, OP_UDIV, udiv)
  BINOP(buildUMod, OP_UMOD, umod)
  BINOP(buildSDiv, OP_SDIV, sdiv)
  BINOP(buildSMod, OP_SMOD, smod)
  BINOP(buildSLL, OP_SLL, shl)
  BINOP(buildSRL, OP_SRL, lshr)
  BINOP(buildSRA, OP_SRA, ashr)
  BINOP(buildUPow, HW_UPOW, upow)
  BINOP(buildSPow, HW_SPOW, spow)

  template <IsAnyHWValue LHS, IsAnyHWValue RHS>
  HWValue buildICmp(LHS lhs, RHS rhs, BigInt::ICmpPred pred) {
    if (lhs.template is<ConstantRef>() && rhs.template is<ConstantRef>()) {
      auto equal = BigInt::icmpOp4S(lhs.template as<ConstantRef>(),
                                    rhs.template as<ConstantRef>(), pred);
      return ConstantRef::fromFourState(equal);
    }
    auto rv =
        buildInstr(OP_ICMP_EQ.indexAdd(uint(pred), OP_ICMP_SGE), true, lhs, rhs)
            .defW();
    rv->numBits = 1;
    return rv;
  }

  template <IsAnyHWValue T> HWValue buildRedXor(T val) {
    if (val.template is<ConstantRef>()) {
      return ConstantRef::fromFourState(
          BigInt::reductionXOROp4S(val.template as<ConstantRef>()));
    }
    auto rv = buildInstr(HW_RED_XOR, true, val).defW();
    rv->numBits = 1;
    return rv;
  }

  HWValue buildExt(uint32_t newSize, HWValue value, bool sign) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() <= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize, sign)
          .get();
    }

    auto ref = buildInstr(sign ? OP_SEXT : OP_ZEXT, true, value);

    assert(ref.defW().getNumBits().value_or(0) < newSize);
    ref.defW()->numBits = newSize;
    return ref.defW();
  }
  HWValue buildZExt(uint32_t newSize, HWValue value) {
    return buildExt(newSize, value, false);
  }
  HWValue buildSExt(uint32_t newSize, HWValue value) {
    return buildExt(newSize, value, true);
  }
  HWValue buildTrunc(uint32_t newSize, HWValue value) {

    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() >= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize)
          .get();
    }

    auto ref = buildInstr(OP_TRUNC, true, value);

    assert(ref.defW().getNumBits().value_or(UINT32_MAX) > newSize);
    ref.defW()->numBits = newSize;
    return ref.defW();
  }

  HWValue buildResize(HWValue val, uint32_t newSize, bool sign = false) {
    assert(val.getNumBits());

    if (val.getNumBits() < newSize)
      return buildExt(newSize, val, sign);
    else if (val.getNumBits() > newSize)
      return buildTrunc(newSize, val);
    return val;
  }
  HWValue buildUpsize(HWValue val, uint32_t newSize, bool sign = false) {
    assert(val.getNumBits());
    if (val.getNumBits() < newSize)
      return buildExt(newSize, val, sign);
    assert(val.getNumBits() == newSize);
    return val;
  }

private:
  Optional<uint32_t> spliceSingleNumBits(HWValue value, BitRange range) {
    if ((range.hasLen() && range.getLen().as<ConstantRef>())) {
      return range.getLen().as<ConstantRef>().getExactVal();
    }
    if (!range.hasLen() && value.getNumBits()) {
      assert(range == BitRange::full());
      return value.getNumBits();
    }
    return nullopt;
  }
  template <typename... Rest>
  std::tuple<Optional<uint32_t>, uint32_t, bool>
  spliceNumBitsOps(HWValue value, BitRange range, Rest... rest) {
    auto lhs = spliceSingleNumBits(value, range);
    auto [rhsB, rhsN, rhsConst] = spliceNumBitsOps(rest...);

    return std::make_tuple((!lhs || !rhsB) ? nullopt : Optional(*lhs + *rhsB),
                           rhsN + (lhs.value_or(1) != 0),
                           rhsConst && lhs && range.addr.is<ConstantRef>() &&
                               value.is<ConstantRef>());
  }
  std::tuple<Optional<uint32_t>, uint32_t, bool> spliceNumBitsOps() {
    return std::make_tuple(0, 0, true);
  }

public:
  template <typename... Ts> HWValue buildSplice(Ts... operands) {

    auto [len, num, isConst] = spliceNumBitsOps(operands...);
    if (isConst) {
      auto cbuild = ctx.constBuild();
      cbuild.val(0, 0);
      HWValue last;
      (
          [&] {
            // skip operands with constant zero length range
            if constexpr (std::is_same_v<decltype(operands), BitRange>) {
              cbuild.concatRangeLHS(
                  last.as<ConstantRef>(),
                  operands.getAddr().template as<ConstantRef>().getExactVal(),
                  operands.getLen().template as<ConstantRef>().getExactVal());

            } else
              last = operands;
          }(),
          ...);
      return cbuild.get();
    }

    auto instr = HWInstrRef{ctx.getInstrs().create(1 + 3 * num, HW_SPLICE)};
    insertInstr(instr);

    InstrBuilder build{instr};
    build.addRef(ctx.getWires().create(len)).other();

    HWValue last;
    (
        [&] {
          // skip operands with constant zero length range
          if constexpr (std::is_same_v<decltype(operands), BitRange>) {
            if (spliceSingleNumBits(last, operands).value_or(1) == 0)
              return;

            addSpecialRef(build, last);
            addSpecialRef(build, operands);
          } else
            last = operands;
        }(),
        ...);

    return instr.defW();
  }

  HWValue buildSplice(ArrayRef<std::pair<HWValue, BitRange>> values) {

    if (std::all_of(values.begin(), values.end(), [](const auto &pair) {
          return pair.first.template is<ConstantRef>() &&
                 pair.second.isConstant();
        })) {
      auto cbuild = ctx.constBuild();
      if (values.size() == 0)
        return cbuild.val(0, 0).get();

      cbuild.valRange(
          values.back().first.as<ConstantRef>(),
          values.back().second.getAddr().as<ConstantRef>().getExactVal(),
          values.back().second.getLen().as<ConstantRef>().getExactVal());

      for (size_t i = values.size() - 1; i-- > 0;)
        cbuild.concatRange(
            values[i].first.as<ConstantRef>(),
            values[i].second.getAddr().as<ConstantRef>().getExactVal(),
            values[i].second.getLen().as<ConstantRef>().getExactVal());

      return cbuild.get();
    }

    auto instr =
        InstrRef{ctx.getInstrs().create(1 + values.size() * 3, HW_SPLICE)};

    insertInstr(instr);
    InstrBuilder build{instr};

    build.addRef(ctx.getWires().create());
    build.other();

    Optional<uint32_t> numBits = 0;

    for (auto [value, range] : values) {
      if (auto asConst = range.getLen().dyn_as<ConstantRef>();
          asConst && asConst.valueEquals(0))
        continue;

      build.addRef(value);
      addSpecialRef(build, range);

      if (numBits) {
        auto val = spliceSingleNumBits(value, range);
        if (!val)
          numBits = nullopt;
        else
          *numBits += *val;
      }
    }

    auto rv = HWInstrRef{instr}.defW();
    rv->numBits = numBits;
    return rv;
  }

  HWValue buildConcat(ArrayRef<HWValue> values) {

    if (values.size() == 1)
      return values[0];

    if (std::all_of(values.begin(), values.end(),
                    [](const HWValue &val) { return val.is<ConstantRef>(); })) {
      auto cbuild = ctx.constBuild();
      if (values.size() == 0)
        return cbuild.val(0, 0).get();
      cbuild.val(values.back().as<ConstantRef>());
      for (size_t i = values.size() - 1; i-- > 0;)
        cbuild.concat(values[i].as<ConstantRef>());

      return cbuild.get();
    }

    auto instr = InstrRef{ctx.getInstrs().create(1 + values.size(), HW_CONCAT)};

    insertInstr(instr);
    InstrBuilder build{instr};

    build.addRef(ctx.getWires().create());
    build.other();

    Optional<uint32_t> numBits = 0;
    for (auto value : values) {
      build.addRef(value);
      if (auto val = value.getNumBits(); val && numBits)
        *numBits = *numBits + *val;
      else
        numBits = nullopt;
    }
    auto rv = HWInstrRef{instr}.defW();
    rv->numBits = numBits;
    return rv;
  }

  HWValue buildRepeat(HWValue value, HWValue count) {
    if (value.is<ConstantRef>() && count.is<ConstantRef>()) {
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .repeat(count.as<ConstantRef>().getExactVal())
          .get();
    }

    auto rv = buildInstr(HW_REPEAT, true, value, count).defW();

    if (value.getNumBits() && count.is<ConstantRef>()) {
      uint32_t out;
      if (__builtin_umul_overflow(*value.getNumBits(),
                                  count.as<ConstantRef>().getExactVal(), &out))
        abort(); // todo: what type of error should this throw?
      rv->numBits = out;
    }

    return rv;
  }

  HWValue buildCLOG2(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      auto tmp = asConst - BigInt::fromU64(1, asConst.getNumBits());
      if (auto lz = BigInt::leadingZeros4S(tmp)) {
        return ConstantRef::fromU32(asConst.getNumBits() - lz);
      } else
        return ConstantRef::undef32();
    }
    return buildInstr(HW_CLOG2, true, value);
  }

  WireRef buildLoad(RegisterRef reg, BitRange range = BitRange::full()) {
    HWInstrRef ref;
    if (range == BitRange::full())
      ref = buildInstr(HW_LOAD, true, reg);
    else
      ref = buildInstr(HW_LOAD, true, reg, range);
    if (range == BitRange::full())
      ref.defW()->numBits = reg->numBits;
    else if (auto asCRef = range.len.dyn_as<ConstantRef>())
      ref.defW()->numBits = asCRef.getExactVal();
    return ref.defW();
  }

  HWInstrRef buildStore(RegisterRef reg, HWValue value,
                        BitRange range = BitRange::full(), bool defer = false) {
    if (range == BitRange::full())
      return buildInstr(defer ? HW_STORE_DEFER : HW_STORE, false, value, reg);
    return buildInstr(defer ? HW_STORE_DEFER : HW_STORE, false, value, reg,
                      range);
  }

  RegisterRef buildRegister(Optional<uint32_t> bitSize = nullopt) {
    auto regRef = RegisterRef{ctx.getRegs().create(bitSize)};
    auto regInstr = InstrRef{ctx.getInstrs().create(1, HW_REGISTER_INSTR)};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);
    return regRef;
  }

  RegisterRef buildPort(ModuleIRef module, HWOpcode opcode) {
    auto regRef = RegisterRef{ctx.getRegs().create()};
    auto regInstr = InstrRef{ctx.getInstrs().create(1, opcode)};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);

    module.mod()->ports.emplace_back(Module::Port{regRef, opcode});
    return regRef;
  }

  RegisterRef buildInputPort(ModuleIRef module) {
    return buildPort(module, HW_INPUT_REGISTER_INSTR);
  }
  RegisterRef buildOutputPort(ModuleIRef module) {
    return buildPort(module, HW_OUTPUT_REGISTER_INSTR);
  }
  RegisterRef buildInoutPort(ModuleIRef module) {
    return buildPort(module, HW_INOUT_REGISTER_INSTR);
  }
  RegisterRef buildRefPort(ModuleIRef module) {
    return buildPort(module, HW_REF_REGISTER_INSTR);
  }

  ProcessIRef buildProcess(HWOpcode type = HW_COMB_PROCESS_INSTR,
                           ProcSenstv &&sens = ProcSenstv::empty()) {
    assert(type == HW_INIT_PROCESS_INSTR || type == HW_COMB_PROCESS_INSTR ||
           type == HW_SEQ_PROCESS_INSTR || type == HW_FINAL_PROCESS_INSTR ||
           type == HW_LATCH_PROCESS_INSTR);
    auto procRef = ctx.getProcs().create(sens);
    auto procInstRef =
        ProcessIRef{ctx.getInstrs().create(2 + sens.signals.size(), type)};
    InstrBuilder build{procInstRef};
    build.addRef(procRef).addRef(ctx.createBlock()).other();

    for (size_t i = 0; i < sens.signals.size(); i++) {
      build.addRef(sens.signals[i].first);
    }

    insertInstr(procInstRef);
    return procInstRef;
  }

  HWInstrRef buildInstance(ModuleRef module, ArrayRef<RegisterRef> portRegs) {
    auto instr =
        InstrRef{ctx.getInstrs().create(1 + portRegs.size(), HW_INSTANCE)};

    assert(portRegs.size() == module->ports.size());

    InstrBuilder build{instr};
    build.other();
    build.addRef(module);

    for (size_t i = 0; i < portRegs.size(); i++)
      build.addRef(portRegs[i]);

    insertInstr(instr);

    return instr;
  }

  IfInstrRef buildIfElse(HWValue cond, uint yieldPrealloc = 0) {
    InstrRef instrRef =
        InstrRef{ctx.getInstrs().create(3 + yieldPrealloc, OP_IF)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    auto falseBl = ctx.createBlock();
    build.addRef(trueBl).addRef(falseBl);

    for (uint i = 0; i < yieldPrealloc; i++)
      build.addRef(ctx.getWires().create());

    build.other().addRef(cond);

    return IfInstrRef{instrRef};
  }

  IfInstrRef buildIf(HWValue cond) {
    InstrRef instrRef = InstrRef{ctx.getInstrs().create(2, OP_IF)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    build.addRef(trueBl);
    build.other().addRef(cond);
    return IfInstrRef{instrRef};
  }

  SwitchInstrRef buildSwitch(HWValue cond, uint yieldPrealloc = 0) {
    SwitchInstrRef instrRef =
        SwitchInstrRef{ctx.getInstrs().create(2 + yieldPrealloc, OP_SWITCH)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    build.other();
    build.addRef(cond);
    return instrRef;
  }

  CaseInstrRef buildCase(ArrayRef<HWValue> conds) {
    CaseInstrRef instrRef =
        CaseInstrRef{ctx.getInstrs().create(1 + conds.size(), OP_CASE)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock()).other();
    for (auto cond : conds)
      build.addRef(cond);
    return instrRef;
  }
  CaseInstrRef buildDefaultCase() {
    CaseInstrRef instrRef =
        CaseInstrRef{ctx.getInstrs().create(1, OP_CASE_DEFAULT)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    return instrRef;
  }

  template <IsAnyHWValue... Ts> auto buildYield(Ts... value) {
    // todo: ideally this would dynamically add Wires to the associated
    // IfInstrRef. Alternative is a GET_YIELD instr or something. For now
    // naive implementation as reference, just delete old instr and rebuild

    auto opcode = OP_YIELD;
    auto instr = insert.blockRef().defI();
    assert(instr.isOpc(OP_IF, OP_WHILE, OP_DO_WHILE, OP_FOR, OP_CASE,
                       OP_CASE_DEFAULT));

    switch (instr.getDialectOpcode().raw()) {
    case OP_IF.raw(): {
      auto asIf = IfInstrRef{instr};
      if (sizeof...(value) > asIf.getNumYieldValues()) {

        auto newInstr =
            InstrRef{ctx.getInstrs().create(sizeof...(value) + 3, OP_IF)};

        InstrBuilder build{newInstr};

        // copy over true/false blocks and existing defs
        for (uint i = 0; i < instr.getNumDefs(); i++)
          build.addRef(instr.operand(i)->as<FatDynObjRef<>>());

        // new wire defs
        for (uint i = 0; i < sizeof...(value) - asIf.getNumYieldValues(); i++)
          build.addRef(ctx.getWires().create());
        build.other();

        // condition
        build.addRef(instr.operand(instr.getNumDefs())->as<FatDynObjRef<>>());

        HWInstrRef{instr}.iter(ctx).replace(newInstr);
        ctx.getInstrs().destroy(instr);
        instr = newInstr;
        asIf = newInstr;
      }
      size_t idx = 0;
      for (auto val : {value...}) {
        WireRef yieldVal = asIf.getYieldValue(idx).template as<WireRef>();
        if (val.getNumBits()) {
          if (!yieldVal->numBits)
            yieldVal->numBits = val.getNumBits();
          assert(yieldVal->numBits == val.getNumBits());
        }
        idx++;
      }
      break;
    }
    case OP_WHILE.raw(): {
      WhileInstrRef asWhile{instr};
      // while yield has continue as another arg.
      if (sizeof...(Ts) == asWhile.getNumYieldValues() + 1)
        ;
      else {
        abort();
      }
      break;
    }
    case OP_DO_WHILE.raw(): {
      DoWhileInstrRef asDoWhile{instr};
      if (sizeof...(Ts) == asDoWhile.getNumYieldValues() + 1)
        ;
      else {
        abort();
      }
      break;
    }
    default:
      dyno_unreachable("undefined");
    }

    auto yieldInstr = buildInstr(OP_YIELD, false, value...);
    return std::make_pair(yieldInstr, instr);
  }

  template <typename... Ts> auto buildWhile(Ts... inputs) {
    InstrRef instrRef =
        InstrRef{ctx.getInstrs().create(2 + 2 * sizeof...(inputs), OP_WHILE)};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    build.addRef(ctx.createBlock());

    for (uint i = 0; i < sizeof...(inputs); i++)
      build.addRef(ctx.getWires().create());
    build.other();
    ([&]() { build.addRef(inputs); }(), ...);
    return WhileInstrRef{instrRef};
  }

  template <typename... Ts> auto buildDoWhile(Ts... inputs) {
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(1 + 2 * sizeof...(inputs), OP_DO_WHILE)};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());

    for (uint i = 0; i < sizeof...(inputs); i++)
      build.addRef(ctx.getWires().create());
    build.other();

    ([&]() { build.addRef(inputs); }(), ...);
    return DoWhileInstrRef{instrRef};
  }

  auto buildAssert(HWValue value) {
    return buildInstr(OP_ASSERT, false, value);
  }

  auto buildFuncParam(Optional<uint32_t> numBits = nullopt) {
    auto reg = ctx.getRegs().create(numBits);
    auto instr = InstrRef{ctx.getInstrs().create(1, OP_PARAM)};
    insertInstr(instr);
    InstrBuilder build{instr};
    build.addRef(reg);

    auto func = insert.blockRef().defI().as<FunctionIRef>();
    func.func()->params.emplace_back(instr);
    return reg;
  }

  template <typename... Ts> auto buildFuncReturn(Ts... retvals) {
    auto instr = buildInstr(OP_RETURN, false, retvals...);
    return instr;
  }

  auto buildFunc() {
    auto funcRef = FunctionRef{ctx.getFuncs().create()};
    auto funcInstr = FunctionIRef{ctx.getInstrs().create(2, OP_FUNC_INSTR)};
    insertInstr(funcInstr);

    InstrBuilder{funcInstr}.addRef(funcRef).addRef(ctx.createBlock());
    return funcInstr;
  }

  auto buildCall(FunctionIRef func, ArrayRef<HWValueOrReg> args,
                 uint numRetvals = 0) {
    auto callInstr = CallInstrRef{
        ctx.getInstrs().create(numRetvals + 1 + args.size(), OP_CALL)};
    // assert(args.size() == func.func()->params.size() && "param size
    // mismatch");
    insertInstr(callInstr);

    InstrBuilder build{callInstr};
    for (uint i = 0; i < numRetvals; i++)
      build.addRef(ctx.getWires().create());
    build.other();
    build.addRef(func.func());
    for (size_t i = 0; i < args.size(); i++)
      build.addRef(args[i]);

    return callInstr;
  }

  // todo: full constant support
  ConstantRef buildConst(uint bits, uint64_t value) {
    return ConstantBuilder{ctx.getConstants()}.val(bits, value);
  }

  void destroyInstr(InstrRef instr) {
    ctx.getCFG()[instr].erase();
    ctx.getInstrs().destroy(instr);
  }

  void setInsertPoint(BlockRef_iterator<true> it) { insert = it; }
}; // namespace dyno

class HWInstrBuilderStack : public HWInstrBuilder {
  SmallVec<BlockRef_iterator<true>, 16> stack;

public:
  using HWInstrBuilder::HWInstrBuilder;
  void pushInsertPoint(BlockRef_iterator_base newInsertPoint) {
    stack.emplace_back(insert);
    setInsertPoint(newInsertPoint);
  }
  void popInsertPoint() { setInsertPoint(stack.pop_back_val()); }
};
}; // namespace dyno

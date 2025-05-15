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
#include "support/TemplateUtil.h"
#include "support/Utility.h"
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
    auto moduleInstr = InstrRef{
        instrs.create(2, DialectID{DIALECT_HW}, OpcodeID{HW_MODULE_INSTR})};

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

  auto buildFunc(ModuleIRef parent) {
    auto funcRef = FunctionRef{getFuncs().create()};
    auto funcInstr = FuncInstrRef{
        getInstrs().create(2, DialectID{DIALECT_OP}, OpcodeID{OP_FUNC_INSTR})};

    InstrBuilder{funcInstr}.addRef(funcRef).addRef(createBlock());
    parent.block().end().insertPrev(funcInstr);
    return funcInstr;
  }

  ConstantBuilder constBuild() { return ConstantBuilder{constants}; }
};

class HWInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  HWInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  WireRef operandW(uint n) { return operand(n)->as<WireRef>(); }
  InstrRef operandI(uint n) { return operandW(n).getDefI(); }
  WireRef defW(uint n = 0) {
    assert(n < getNumDefs());
    return operandW(n);
  }
  InstrRef defI(uint n = 0) {
    assert(n < getNumDefs());
    return operandI(n);
  }
  // todo: get rid of ctx params via global directory.
  auto iter(HWContext &ctx) { return ctx.getCFG()[this->as<ObjRef<Instr>>()]; }
  BlockRef parentBlock(HWContext &ctx) { return iter(ctx).blockRef(); }
  ProcessIRef parentProc(HWContext &ctx) {
    auto pBlock = parentBlock(ctx);
    return pBlock.defI().as<ProcessIRef>();
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

  template <typename... Ts>
  void addRefs(InstrRef instr, bool addWireDef, Ts... operands) {
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    (
        // Compile-time edge cases for adding special types like BitRange
        [&] {
          if constexpr (std::is_same_v<BitRange,
                                       std::decay_t<decltype(operands)>>) {
            build.addRef(operands.getAddr());
            build.addRef(operands.getLen());
          } else {
            build.addRef(operands);
          }
        }(),
        ...);
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

#define COMM_OP(ident, opcode, constFunc)                                      \
  template <IsAnyHWValue... Ts> HWValue ident(Ts... operands) {                \
    if constexpr (!(std::is_same_v<Ts, WireRef> || ...))                       \
      if ((operands.template is<ConstantRef>() && ...)) {                      \
        ConstantBuilder build{ctx.getConstants()};                             \
        build.val(getFirst(operands...).getNumBits());                         \
        ([&] { build.constFunc(operands.template as<ConstantRef>()); }(),      \
         ...);                                                                 \
        return build.get();                                                    \
      }                                                                        \
    auto rv =                                                                  \
        buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_ADD}, true, operands...) \
            .defW();                                                           \
    rv->numBits = getFirst(operands...).getNumBits();                          \
    return rv;                                                                 \
  }

  //#define COMM_OP(ident, opcode)                                                 \
//  template <IsAnyHWValue... Ts> HWInstrRef ident(Ts... operands) {             \
//    auto rv = buildInstr(DialectID{DIALECT_OP}, OpcodeID{opcode}, true,        \
//                         operands...);                                         \
//    rv.defW()->numBits = getFirst(operands...).getNumBits();                   \
//    return rv;                                                                 \
//  }
  COMM_OP(buildAdd, OP_ADD, add)
  COMM_OP(buildAnd, OP_AND, bitAND)
  COMM_OP(buildOr, OP_OR, bitOR)
  COMM_OP(buildXor, OP_XOR, bitXOR)
  COMM_OP(buildMul, OP_MUL, mul)

  template <IsAnyHWValue... Ts> HWInstrRef buildAdd2(Ts... operands) {

    // canonicalize further by having constants rightmost, possible even
    // sort wires by def opcode?

    // todo: bitmap
    std::array<bool, sizeof...(operands)> isSameOpc = {};
    ssize_t operandDelta = 0;
    bool anyIsSameOpc = 0;

    size_t index = 0;
    for (HWValue operand : {operands...}) {
      // todo: via constexpr ifs for direct ConstantRef/WireRef
      if (auto asWire = operand.template dyn_as<WireRef>();
          asWire && asWire.hasSingleDef() &&
          asWire.getSingleDef()->instr().isOpc(DialectID{DIALECT_OP},
                                               OpcodeID{OP_ADD})) {
        anyIsSameOpc = 1;
        isSameOpc[index] = 1;
        operandDelta = asWire.getSingleDef()->instr().getNumOthers() - 1;
      }
      index++;
    }

    if (!anyIsSameOpc) {
      return buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_ADD}, true,
                        operands...);
    }

    auto instr = InstrRef{
        ctx.getInstrs().create(1 + sizeof...(operands) + operandDelta,
                               DialectID{DIALECT_OP}, OpcodeID{OP_ADD})};
    insertInstr(instr);
    InstrBuilder build{instr};
    // todo: size?
    build.addRef(ctx.getWires().create());
    build.other();

    index = 0;
    for (HWValue operand : {operands...}) {
      if (isSameOpc[index]) {
        InstrRef otherInstr = operand.as<WireRef>().getSingleDef()->instr();
        for (auto subOp :
             operand.as<WireRef>().getSingleDef()->instr().others())
          build.addRef(subOp->template as<HWValue>());

        ctx.getCFG()[otherInstr].erase();
        ctx.getInstrs().destroy(otherInstr);
      } else
        build.addRef(operand);
      index++;
    }

    return HWInstrRef{instr};
  }

#define BINOP(ident, opcode, constFunc)                                        \
  template <IsAnyHWValue LHS, IsAnyHWValue RHS>                                \
  HWValue ident(LHS lhs, RHS rhs) {                                            \
    if (lhs.template is<ConstantRef>() && rhs.template is<ConstantRef>()) {    \
      return ConstantBuilder{ctx.getConstants()}                               \
          .val(lhs.template as<ConstantRef>())                                 \
          .constFunc(rhs.template as<ConstantRef>())                           \
          .get();                                                              \
    }                                                                          \
    auto rv =                                                                  \
        buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_ADD}, true, lhs, rhs)    \
            .defW();                                                           \
    rv->numBits = rhs.getNumBits();                                            \
    return rv;                                                                 \
  }

  BINOP(buildSub, OP_SUB, sub)
  BINOP(buildSDiv, OP_UDIV, udiv)
  BINOP(buildUDiv, OP_UMOD, umod)
  BINOP(buildSLL, OP_SLL, shl)
  BINOP(buildSRL, OP_SRL, lshr)
  BINOP(buildSRA, OP_SRA, ashr)

  HWValue buildExt(uint32_t newSize, HWValue value, bool sign) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() <= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize, sign)
          .get();
    }

    auto ref = buildInstr(DialectID{DIALECT_OP},
                          OpcodeID{sign ? OP_SEXT : OP_ZEXT}, true, value);

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

    auto ref =
        buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_TRUNC}, true, value);

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

  template <typename... Ts> HWInstrRef buildSplice(Ts... operands) {
    static_assert(sizeof...(operands) % 2 == 0 &&
                  "operands must be pairs of HWValue & BitRange");
    return buildInstr(DialectID{DIALECT_HW}, OpcodeID{HW_SPLICE}, true,
                      operands...);
  }

  WireRef buildLoad(RegisterRef reg, BitRange range = BitRange::full()) {
    HWInstrRef ref;
    if (range == BitRange::full())
      ref = buildInstr(DialectID{DIALECT_HW}, OpcodeID{HW_LOAD}, true, reg);
    else
      ref = buildInstr(DialectID{DIALECT_HW}, OpcodeID{HW_LOAD}, true, reg,
                       range);
    if (range == BitRange::full())
      ref.defW()->numBits = reg->numBits;
    else if (auto asCRef = range.len.dyn_as<ConstantRef>())
      ref.defW()->numBits = asCRef.getExactVal();
    return ref.defW();
  }

  HWInstrRef buildStore(RegisterRef reg, FatDynObjRef<> value,
                        BitRange range = BitRange::full()) {
    if (range == BitRange::full())
      return buildInstr(DialectID{DIALECT_HW}, OpcodeID{HW_STORE}, false, value,
                        reg);
    return buildInstr(DialectID{DIALECT_HW}, OpcodeID{HW_STORE}, false, value,
                      reg, range);
  }

  RegisterRef buildRegister() {
    auto regRef = RegisterRef{ctx.getRegs().create()};
    // in order for the reg to use anything it must be an instr as well
    auto regInstr = InstrRef{ctx.getInstrs().create(
        1, DialectID{DIALECT_HW}, OpcodeID{HW_REGISTER_INSTR})};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);
    return regRef;
  }

  RegisterRef buildPort(ModuleIRef module, OpcodeID opcode) {
    auto regRef = RegisterRef{ctx.getRegs().create()};
    auto regInstr =
        InstrRef{ctx.getInstrs().create(1, DialectID{DIALECT_HW}, opcode)};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);

    module.mod()->ports.emplace_back(regRef);
    return regRef;
  }

  RegisterRef buildInputPort(ModuleIRef module) {
    return buildPort(module, OpcodeID{HW_INPUT_REGISTER_INSTR});
  }
  RegisterRef buildOutputPort(ModuleIRef module) {
    return buildPort(module, OpcodeID{HW_OUTPUT_REGISTER_INSTR});
  }
  RegisterRef buildInoutPort(ModuleIRef module) {
    return buildPort(module, OpcodeID{HW_INOUT_REGISTER_INSTR});
  }
  RegisterRef buildRefPort(ModuleIRef module) {
    return buildPort(module, OpcodeID{HW_REF_REGISTER_INSTR});
  }

  ProcessIRef buildProcess() {
    auto procRef = ctx.getProcs().create();
    auto procInstRef = ProcessIRef{ctx.getInstrs().create(
        2, DialectID{DIALECT_HW}, OpcodeID{HW_PROCESS_INSTR})};
    InstrBuilder{procInstRef}.addRef(procRef).addRef(ctx.createBlock());
    insertInstr(procInstRef);
    return procInstRef;
  }

  HWInstrRef buildInstance(ModuleRef module) {

    auto instr = InstrRef{ctx.getInstrs().create(1 + module->ports.size(),
                                                 DialectID{DIALECT_HW},
                                                 OpcodeID{HW_INSTANCE})};

    InstrBuilder build{instr};
    build.other();
    build.addRef(module);

    for (size_t i = 0; i < module->ports.size(); i++)
      build.addRef(buildRegister());

    insertInstr(instr);

    return instr;
  }

  IfInstrRef buildIfElse(FatDynObjRef<> cond) {
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(3, DialectID{DIALECT_OP}, OpcodeID{OP_IF})};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    auto falseBl = ctx.createBlock();
    build.addRef(trueBl).addRef(falseBl).other().addRef(cond);
    return IfInstrRef{instrRef};
  }

  template <typename... Ts> auto buildYield(Ts... value) {
    // todo: ideally this would dynamically add Wires to the associated
    // IfInstrRef. Alternative is a GET_YIELD instr or something. For now naive
    // implementation as reference, just delete old instr and rebuild

    OpcodeID opcode = OpcodeID{OP_YIELD};
    auto instr = insert.blockRef().defI();
    assert(instr.getDialect() == DIALECT_OP &&
           (instr.getOpcode().anyOf(OP_IF, OP_WHILE)));

    switch (instr.getOpcode()) {
    case OP_IF: {
      auto asIf = IfInstrRef{instr};
      if (sizeof...(value) >= asIf.getNumYieldValues()) {

        auto newInstr = InstrRef{ctx.getInstrs().create(
            sizeof...(value) + 3, DialectID{DIALECT_OP}, OpcodeID{OP_IF})};

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
      }
      break;
    }
    case OP_WHILE: {
      WhileInstrRef asWhile{instr};
      // conditional yield is just implicit for now, one more arg
      if (sizeof...(Ts) == asWhile.getNumYieldValues() + 1)
        opcode = OpcodeID{OP_YIELD_COND};
      else {
        assert(sizeof...(Ts) == asWhile.getNumYieldValues() && "todo resizing");
      }
      break;
    }
    default:
      dyno_unreachable("undefined");
    }

    auto yieldInstr =
        buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_YIELD}, false, value...);
    return std::make_pair(yieldInstr, instr);
  }

  template <typename... Ts> auto buildWhile(Ts... inputs) {
    InstrRef instrRef = InstrRef{ctx.getInstrs().create(
        2 + 2 * sizeof...(inputs), DialectID{DIALECT_OP}, OpcodeID{OP_WHILE})};
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

  auto buildFuncParam(FunctionRef func) {
    auto instr =
        buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_PARAM}, true, func);
    func.addParam(instr);
    return instr;
  }

  template <typename... Ts>
  auto buildFuncReturn(FunctionRef func, Ts... retvals) {
    auto instr = buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_RETURN}, false,
                            func, retvals...);
    func.addReturn(instr);
    return instr;
  }

  // todo: full constant support
  ConstantRef buildConst(uint bits, uint64_t value) {
    // still quite problematic, this constant is never deleted.
    // Could do unique_ptr or shared_ptr style implementation for safety.
    return ConstantBuilder{ctx.getConstants()}.val(bits, value);
  }

  void setInsertPoint(BlockRef_iterator<true> it) { insert = it; }
}; // namespace dyno

class HWInstrBuilderStack : public HWInstrBuilder {
  SmallVec<BlockRef_iterator<true>, 4> stack;

public:
  using HWInstrBuilder::HWInstrBuilder;
  void pushInsertPoint(BlockRef_iterator_base newInsertPoint) {
    stack.emplace_back(insert);
    setInsertPoint(newInsertPoint);
  }
  void popInsertPoint() { setInsertPoint(stack.pop_back_val()); }
};
}; // namespace dyno

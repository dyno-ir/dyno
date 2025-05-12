#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "hw/HWConstant.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Utility.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>

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
  // todo: processes & modules

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
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_MODULE_INSTR})};

    InstrBuilder{moduleInstr}.addRef(moduleRef).addRef(createBlock());
    return moduleInstr;
  }

  RegisterRef createRegister(ModuleIRef parent) {
    auto regRef = RegisterRef{regs.create()};
    // in order for the reg to use anything it must be an instr as well
    auto regInstr = InstrRef{
        instrs.create(1, DialectID{DIALECT_RTL}, OpcodeID{HW_REGISTER_INSTR})};
    InstrBuilder{regInstr}.addRef(regRef);
    parent.block().end().insertPrev(regInstr);
    return regRef;
  }

  BlockRef createBlock() {
    auto blockRef = cfg.blocks.create(cfg);
    // auto blockInstrRef =
    //     InstrRef{instrs.create(1 + sizeof...(parents),
    //     DialectID{DIALECT_RTL},
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

  ProcessIRef createProcess(ModuleIRef parent) {
    auto procRef = procs.create();
    auto procInstRef = ProcessIRef{
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_PROCESS_INSTR})};
    InstrBuilder{procInstRef}.addRef(procRef).addRef(createBlock());
    parent.block().end().insertPrev(procInstRef);
    return procInstRef;
  }
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
  HWContext &ctx;
  BlockRef_iterator<true> insert;

public:
  HWInstrBuilder(HWContext &ctx, BlockRef_iterator<true> insert)
      : ctx(ctx), insert(insert) {}

  void insertInstr(InstrRef instr) { insert.insertPrev(instr); }

  template <typename... Ts>
  void addRefs(InstrRef instr, bool addWireDef, Ts... operands) {
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    ([&] { build.addRef(operands); }(), ...);
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectID dialect, OpcodeID opcode, bool addWireDef,
                        Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        addWireDef + sizeof...(operands), dialect, opcode)};

    insertInstr(instr);
    addRefs(instr, addWireDef, operands...);
    return HWInstrRef{instr};
  }

#define COMM_OP(ident, opcode)                                                 \
  template <typename... Ts> HWInstrRef ident(Ts... operands) {                 \
    return buildInstr(DialectID{DIALECT_OP}, OpcodeID{opcode}, true,           \
                      operands...);                                            \
  }
  COMM_OP(buildAdd, OP_ADD)
  COMM_OP(buildAnd, OP_AND)
  COMM_OP(buildOr, OP_OR)
  COMM_OP(buildXor, OP_XOR)
  COMM_OP(buildMul, OP_MUL)

#define BINOP(ident, opcode)                                                   \
  template <typename LHS, typename RHS> HWInstrRef ident(LHS lhs, RHS rhs) {   \
    return buildInstr(DialectID{DIALECT_OP}, OpcodeID{opcode}, true, lhs,      \
                      rhs);                                                    \
  }

  BINOP(buildSub, OP_SUB)
  BINOP(buildSDiv, OP_SDIV)
  BINOP(buildUDiv, OP_UDIV)
  BINOP(buildSLL, OP_SLL)
  BINOP(buildSRL, OP_SRL)
  BINOP(buildSRA, OP_SRA)

  HWInstrRef buildLoad(RegisterRef reg) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_LOAD}, true, reg);
  }

  HWInstrRef buildStore(RegisterRef reg, FatDynObjRef<> value) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_STORE}, false, reg,
                      value);
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
};

}; // namespace dyno

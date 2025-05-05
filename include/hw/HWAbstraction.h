#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "scf/Function.h"
#include "scf/IDs.h"
#include "scf/SCF.h"
#include "support/Utility.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>

namespace dyno {

class HWContext {

  ConstantStore constants;
  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<SCFFunc> funcs;
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
    auto funcRef = SCFFuncRef{getFuncs().create()};
    auto funcInstr = FuncInstrRef{getInstrs().create(2, DialectID{DIALECT_SCF},
                                                     OpcodeID{SCF_FUNC_INSTR})};

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

  template <typename... Ts> HWInstrRef buildAdd(Ts... operands) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_ADD}, true,
                      operands...);
  }

  template <typename LHS, typename RHS> HWInstrRef buildSub(LHS lhs, RHS rhs) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_SUB}, true, lhs, rhs);
  }

  HWInstrRef buildLoad(RegisterRef reg) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_LOAD}, true, reg);
  }

  HWInstrRef buildStore(RegisterRef reg, FatDynObjRef<> value) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_STORE}, false, reg,
                      value);
  }

  IfInstrRef buildIfElse(FatDynObjRef<> cond) {
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(3, DialectID{DIALECT_SCF}, OpcodeID{SCF_IF})};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    auto falseBl = ctx.createBlock();
    build.addRef(trueBl).addRef(falseBl).other().addRef(cond);
    return IfInstrRef{instrRef};
  }

  template <typename... Ts> auto buildSCFYield(Ts... value) {
    // todo: ideally this would dynamically add Wires to the associated
    // IfInstrRef. Alternative is a GET_YIELD instr or something. For now naive
    // implementation as reference, just delete old instr and rebuild

    OpcodeID opcode = OpcodeID{SCF_YIELD};
    auto instr = insert.blockRef().defI();
    assert(instr.getDialect() == DIALECT_SCF &&
           (instr.getOpcode().anyOf(SCF_IF, SCF_WHILE)));

    switch (instr.getOpcode()) {
    case SCF_IF: {
      auto asIf = IfInstrRef{instr};
      if (sizeof...(value) >= asIf.getNumYieldValues()) {

        auto newInstr = InstrRef{ctx.getInstrs().create(
            sizeof...(value) + 3, DialectID{DIALECT_SCF}, OpcodeID{SCF_IF})};

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
    case SCF_WHILE: {
      WhileInstrRef asWhile{instr};
      // conditional yield is just implicit for now, one more arg
      if (sizeof...(Ts) == asWhile.getNumYieldValues() + 1)
        opcode = OpcodeID{SCF_YIELD_COND};
      else {
        assert(sizeof...(Ts) == asWhile.getNumYieldValues() && "todo resizing");
      }
      break;
    }
    default:
      dyno_unreachable("undefined");
    }

    auto yieldInstr = buildInstr(DialectID{DIALECT_SCF}, OpcodeID{SCF_YIELD},
                                 false, value...);
    return std::make_pair(yieldInstr, instr);
  }

  template <typename... Ts> auto buildWhile(Ts... inputs) {
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(2 + 2 * sizeof...(inputs),
                               DialectID{DIALECT_SCF}, OpcodeID{SCF_WHILE})};
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

  auto buildFuncParam(SCFFuncRef func) {
    auto instr =
        buildInstr(DialectID{DIALECT_SCF}, OpcodeID{SCF_PARAM}, true, func);
    func.addParam(instr);
    return instr;
  }

  template <typename... Ts>
  auto buildFuncReturn(SCFFuncRef func, Ts... retvals) {
    auto instr = buildInstr(DialectID{DIALECT_SCF}, OpcodeID{SCF_RETURN}, false,
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

class HWPrinter {
  static constexpr std::array<const DialectInfo *, NUM_DIALECTS> dialectIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) &DialectTraits<x>::info
#include "dyno/DialectIDs.inc"
  };
  std::array<const TyInfo *, NUM_DIALECTS> tyIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::tyInfo
#include "dyno/DialectIDs.inc"
  };
  std::array<const OpcodeInfo *, NUM_DIALECTS> opcodeIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::opcInfo
#include "dyno/DialectIDs.inc"
  };

  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};

  FieldPrinter fieldPrinter{std::cout, dialectI, tyI, opcI};
  RefPrinter refPrinter{fieldPrinter};
  Printer printer{refPrinter};

public:
  HWPrinter() {
    fieldPrinter.setDefaultDialects({DialectID{DIALECT_CORE},
                                     DialectID{DIALECT_SCF},
                                     DialectID{DIALECT_RTL}});
  }

  void printCtx(HWContext &ctx) {
    for (auto instr : ctx.getInstrs()) {
      if (InstrRef{instr}.isOpc(DialectID{DIALECT_RTL},
                                OpcodeID{HW_MODULE_INSTR}))
        printer.printInstr(InstrRef{instr});
    }
  }
};
}; // namespace dyno

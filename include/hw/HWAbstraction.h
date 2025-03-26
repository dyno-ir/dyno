#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/ObjInfo.h"
#include "hw/ObjInfo.h"
#include "hw/Wire.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>
#include <optional>

namespace dyno {

class HWContext {

  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Instr> instrs;
  CFG cfg;

public:
  auto &getWires() { return wires; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }
};

class HWInstrBuilder {
  HWContext &ctx;
  std::optional<InstrRef> insert;

#define ADD_OP(x) core_##x
#include "dyno/CoreOps.inc"

public:
  HWInstrBuilder(HWContext &ctx, std::optional<InstrRef> insert = std::nullopt)
      : ctx(ctx), insert(insert) {}

  void insertInstr(InstrRef instr) {
    if (insert != std::nullopt) {
      auto blockIt = BlockRef_iterator<true>{ctx.getCFG()[*insert]};
      ++blockIt;
      blockIt.insertPrev(instr);
    } else {
      auto blockIt =
          BlockRef{*ctx.getCFG().blocks.create(ctx.getCFG()).getPtr()}.begin();
      ++blockIt;
      blockIt.insertPrev(instr);
    }
    insert = instr;
  }

  template <typename... Ts> void addRefs(InstrRef instr, Ts... operands) {
    auto defWire = ctx.getWires().create();
    InstrBuilder build{instr};
    build.addRef(defWire).other();
    ([&] { build.addRef(operands); }(), ...);
  }

  template <typename... Ts>
  InstrRef buildInstr(OpcodeID opcode, Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        1 + sizeof...(operands), DialectID{DIALECT_CORE}, opcode)};

    insertInstr(instr);
    addRefs(instr, operands...);
    return instr;
  }

  template <typename... Ts> InstrRef buildAdd(Ts... operands) {
    return buildInstr(OpcodeID{core_add}, operands...);
  }

  template <typename LHS, typename RHS> InstrRef buildSub(LHS lhs, RHS rhs) {
    return buildInstr(OpcodeID{core_sub}, lhs, rhs);
  }

  ConstantRef buildConst32(uint32_t value) { return ConstantRef{32, value}; }
};

class HWPrinter {
  // todo: better spot for these
  std::array<const DialectInfo *, 2> dialectIs{&coreDialectInfo,
                                               &rtlDialectInfo};
  std::array<const TyInfo *, 2> tyIs{coreTyInfo, rtlTyInfo};
  std::array<const OpcodeInfo *, 2> opcodeIs{coreOpcodeInfo, rtlOpcodeInfo};

  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};

  RefPrinter refPrinter{std::cout, dialectI, tyI};
  InstrPrinter instrPrinter{refPrinter, opcI};

public:
  void printCtx(HWContext &ctx) {
    for (auto block : ctx.getCFG().blocks) {
      auto asBlockRef = BlockRef{*block.getPtr()};
      for (auto insn : asBlockRef) {
        if (insn.getPtr() == nullptr)
          break;
        instrPrinter.print(insn);
      }
    }
  }
};

}; // namespace dyno

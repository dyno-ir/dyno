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

public:
  HWInstrBuilder(HWContext &ctx, std::optional<InstrRef> insert = std::nullopt)
      : ctx(ctx), insert(insert) {}

  template <typename... Ts> InstrRef buildAdd(Ts... Refs) {
    auto instr = InstrRef{ctx.getInstrs().create(
        1 + sizeof...(Refs), DialectID{DIALECT_CORE}, OpcodeID{0})};

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

    auto defWire = ctx.getWires().create();

    InstrBuilder build{instr};
    build.addRef(defWire).other();
    ([&] { build.addRef(Refs); }(), ...);

    insert = instr;
    return instr;
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
      for (auto insn : asBlockRef)
      {
        if (insn.getPtr() == nullptr) break;
        instrPrinter.print(insn);
      }
    }
  }
};

}; // namespace dyno

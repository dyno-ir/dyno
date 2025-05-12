#pragma once
#include "dyno/IDs.h"
#include "dyno/InstrPrinter.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"
#include "op/IDs.h"

namespace dyno {

class HWPrinter : public Printer {
  static constexpr std::array<const DialectInfo *, NUM_DIALECTS> dialectIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) &DialectTraits<x>::info
#include "dyno/DialectIDs.inc"
  };
  static constexpr std::array<const TyInfo *, NUM_DIALECTS> tyIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::tyInfo
#include "dyno/DialectIDs.inc"
  };
  static constexpr std::array<const OpcodeInfo *, NUM_DIALECTS> opcodeIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::opcInfo
#include "dyno/DialectIDs.inc"
  };

  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};

public:
  HWPrinter(std::ostream &str)
      : Printer(str, dialectIs.data(), tyIs.data(), opcodeIs.data()) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_RTL}});
  }

  virtual bool printTypeCore(FatDynObjRef<> ref, bool def) override {
    switch (ref.getTyID()) {
    case CORE_CONSTANT: {
      str << '#' << ref.as<ConstantRef>();
      return true;
    }
    default:
      return this->Printer::printTypeCore(ref, def);
    }
  }

  void printCtx(HWContext &ctx) {
    for (auto instr : ctx.getInstrs()) {
      if (InstrRef{instr}.isOpc(DialectID{DIALECT_RTL},
                                OpcodeID{HW_MODULE_INSTR}))
        printInstr(InstrRef{instr});
    }
  }
};

}; // namespace dyno

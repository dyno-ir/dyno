#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include <ctime>
namespace dyno {

using MemoryAddressGenTermOperand = AddressGenTermOperandBase<RegisterRef>;

class MemoryPortIRef
    : public OpcodeInstrRef<InstrRef, HW_READ_PORT_DEF, HW_WRITE_PORT_DEF,
                            HW_READ_WRITE_PORT_DEF> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef data() { return other(0); }
  RegisterRef getData() { return data().as<RegisterRef>(); }

  OperandRef base() { return other(1); }
  uint32_t getBase() { return *base()->as<ConstantRef>().getLimitedVal(); }

  OperandRef en() {
    assert(hasEn());
    return other(2);
  }
  OperandRef enPol() {
    assert(hasEn());
    return other(3);
  }
  bool hasEn() { return (getNumOthers() % 3 == 1); }
  std::optional<std::pair<RegisterRef, bool>> getEn() {
    if (hasEn())
      return std::make_pair(en()->as<RegisterRef>(),
                            enPol()->as<ConstantRef>().valueEquals(1));
    return std::nullopt;
  }

  unsigned clockBase() { return hasEn() ? 4 : 2; }
  bool hasClock() { return other(clockBase())->is<ConstantRef>(); }

  OperandRef delay() {
    assert(hasClock());
    return other(clockBase() + 0);
  }
  OperandRef clock() {
    assert(hasClock());
    return other(clockBase() + 1);
  }
  OperandRef clockPol() {
    assert(hasClock());
    return other(clockBase() + 2);
  }

  std::optional<Tuple<unsigned, RegisterRef, bool>> getClock() {
    if (!hasClock())
      return std::nullopt;
    return mk_tuple(delay()->as<ConstantRef>().getExactVal(),
                           clock()->as<RegisterRef>(),
                           clockPol()->as<ConstantRef>().valueEquals(1));
  }

  unsigned termsBase() { return clockBase() + (hasClock() ? 3 : 0); }
  unsigned getNumTerms() {
    auto ops = (getNumOthers() - termsBase());
    assert(ops % 3 == 0);
    return ops / 3;
  }

  auto terms() {
    auto beginIt = other_begin() + termsBase();
    return Range{beginIt, other_end()}.step(3).transform(
        [](size_t, auto ref) { return MemoryAddressGenTermOperand{ref}; });
  }

  auto term(unsigned i = 0) {
    assert(i < getNumTerms());
    return terms().begin()[i];
  }
};

class MemoryInstrRef : public OpcodeInstrRef<InstrRef, HW_MEMORY_DEF> {
public:
  auto block() { return def()->as<BlockRef>(); }
  auto getNumBits() { return other(0)->as<ConstantRef>().getExactVal(); }
  auto ports() { return Range{block()}.as<MemoryPortIRef>(); }
  MemoryInstrRef(InstrRef ref) : OpcodeInstrRef(ref) {}
  using OpcodeInstrRef::OpcodeInstrRef;

  struct Port {
    RegisterRef data;
    uint32_t base;

    // option 2:
    RegisterRef en;
    bool enPol;

    // optional 3:
    uint32_t delay;
    RegisterRef clkReg;
    bool clkPol;

    struct Term {
      RegisterRef addr;
      uint32_t fact;
      uint32_t max;
    };
    SmallVec<Term, 2> terms;
  };
  static constexpr unsigned readPortNOperands = 5;
  auto appendPort(Context &ctx, const Port &port, DialectOpcode opc) {
    auto ib = InstrBuilder{ctx.getStore<Instr>().create(
        3 + (!!port.en * 2) + (!!port.delay * 3) + (port.terms.size() * 3),
        opc)};
    ib.addRef(ctx.getStore<MemoryPort>().create());
    ib.other();

    ib.addRef(port.data);
    ib.addRef(ConstantRef::fromU32(port.base));

    if (port.en) {
      ib.addRef(port.en);
      ib.addRef(ConstantRef::fromBool(port.enPol));
    }

    if (port.delay != 0) {
      ib.addRef(ConstantRef::fromU32(port.delay));
      ib.addRef(port.clkReg);
      ib.addRef(ConstantRef::fromBool(port.clkPol));
    }

    for (auto &term : port.terms) {
      ib.addRef(term.addr);
      ib.addRef(ConstantRef::fromU32(term.fact));
      ib.addRef(ConstantRef::fromU32(term.max));
    }

    block().end().insertPrev(ib.instr());
  }
};
}; // namespace dyno

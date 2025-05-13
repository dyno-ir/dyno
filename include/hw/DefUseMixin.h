#pragma once
#include "dyno/Instr.h"
namespace dyno {

template <typename Derived> class InstrDefUseMixin {

  InstrDefUse &self() { return (*static_cast<Derived *>(this))->defUse; }
  const InstrDefUse &cself() const {
    return (*static_cast<const Derived *>(this))->defUse;
  }

public:
  unsigned getNumDefsAndUses() const { return cself().getNumDefsAndUses(); }
  unsigned getNumDefs() const { return cself().getNumDefs(); }
  unsigned getNumUses() const { return cself().getNumUses(); }

  auto &getDef() { return self().getDef(); }

  auto getSingleDef() { return self().getSingleDef(); }
  auto hasSingleDef() const { return cself().hasSingleDef(); }
  auto getSingleUse() { return self().getSingleUse(); }
  auto hasSingleUse() const { return cself().hasSingleUse(); }

  auto begin() { return self().begin(); }
  auto end() { return self().end(); }
  auto def_begin() { return self().def_begin(); }
  auto def_end() { return self().def_end(); }
  auto use_begin() { return self().use_begin(); }
  auto use_end() { return self().use_end(); }
  auto defs() { return self().defs(); }
  auto uses() { return self().uses(); }
};

}; // namespace dyno

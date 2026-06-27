#pragma once

#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "support/Bits.h"
#include <bit>
namespace dyno {

class LUTInstrRef : public OpcodeInstrRef<HWInstrRef, HW_LUT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  uint32_t size() { return truthTable().getNumBits(); }
  uint32_t numInputs() {
    assert(std::popcount(truthTable().getNumBits()) == 1);
    return clog2(truthTable().getNumBits());
  }
  ConstantRef truthTable() { return other(0).as<ConstantRef>(); }
  auto inputs() { return others().drop_front().as<HWValue>(); }
  WireRef output() { return def()->as<WireRef>(); }
};

class LUTMutInstr : public MutInstr<FatDynObjRef<>> {
public:
  LUTMutInstr(Context &ctx, uint32_t numInputs)
      : MutInstr(ctx, HW_LUT, 1, 1 + numInputs) {}
  template <typename T>
  LUTMutInstr(Context &ctx, ConstantRef truthTable, Range<T> inputs)
      : MutInstr(ctx, HW_LUT, 2 + inputs.size()) {
    this->emplace_back(nullref);
    this->defsDone();
    this->emplace_back(truthTable);
    this->push_back_range(inputs);
  }
  template <typename T>
  LUTMutInstr(Context &ctx, WireRef defW, ConstantRef truthTable,
              Range<T> inputs)
      : MutInstr(ctx, HW_LUT, 2 + inputs.size()) {
    this->emplace_back(defW);
    this->defsDone();
    this->emplace_back(truthTable);
    this->push_back_range(inputs);
  }

  FatDynObjRef<> &truthTable() { return other(0); }
  auto inputs() { return others().drop_front(); }
  FatDynObjRef<> &output() { return def(); }
};

}; // namespace dyno

#pragma once

#include "dyno/Instr.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "support/Utility.h"
namespace dyno {

class ConcatIRef : public OpcodeInstrRef<HWInstrRef, HW_CONCAT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  std::pair<OperandRef, uint32_t> getOperandForBitIdx(uint32_t idx) {
    assert(idx < *def(0)->as<WireRef>().getNumBits());
    uint32_t bits = 0;
    for (auto op : others()) {
      bits += *op->as<HWValue>().getNumBits();
      if (bits > idx)
        return std::pair(op, bits - idx);
    }
    dyno_unreachable("out of bounds");
  }
};

}; // namespace dyno

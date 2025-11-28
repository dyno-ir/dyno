#pragma once

#include "aig/AIG.h"
#include "dyno/ObjMap.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DynBitSet.h"
#include <cassert>

namespace dyno {

class AIGSim {
  using StateVec = UnsizedBitSet<std::vector<uint64_t>>;

  AIG &aig;
  StateVec gateState;
  StateVec specialState;

public:
  AIGSim(AIG &aig)
      : aig(aig), gateState(aig.numGateIDs()),
        specialState(aig.store.fat.numIDs()) {}

  void run() {
    run(aig.gates());
    for (auto &node : aig.outputs) {
      setState(node.as<AIGNodeTRef>(), getState(node.getSingleOperand()));
    }
  }

  template <typename R> void run(R nodes) {
    for (auto node : nodes) {
      bool l = getState(aig[node].operand(0));
      bool r = getState(aig[node].operand(1));
      setState(node, l & r);
    }
  }

  bool getState(AIGNodeTRef node) {
    if (node.invert()) {
      return !getState(node.nonInverted());
    }
    if (node.isSpecial()) [[unlikely]]
      return specialState.get(node.getOffsetIdx());
    return gateState.get(node.idx());
  }

  void setState(AIGNodeTRef node, bool v) {
    assert(!node.invert());
    if (node.isSpecial()) [[unlikely]] {
      specialState[node.getOffsetIdx()] = v;
      return;
    }
    // TODO: use |= and check that it compiles to single instr
    gateState[node.idx()] = v;
  }

  static bool lec(AIG &a, AIG &b) {
    if (a.inputs.size() != b.inputs.size() ||
        a.outputs.size() != b.outputs.size() || a.inputs.size() >= 64) {
      DYNO_DBG(dbgs() << "Incompatible AIG");
      return false;
    }
    uint64_t max = bit_mask_ones<uint64_t>(a.inputs.size());
    AIGSim aSim(a);
    AIGSim bSim(b);
    for (uint64_t v = 0; v <= max; ++v) {
      aSim.specialState.raw()[0] = v;
      bSim.specialState.raw()[0] = v;
      aSim.run();
      bSim.run();
      if (aSim.specialState != bSim.specialState) {
        for (unsigned i = 0; i < a.store.fat.numIDs(); ++i) {
          bool aV = aSim.specialState[i];
          bool bV = bSim.specialState[i];
          DYNO_DBG(dbgs() << i << "(" << aV << ", " << bV << ") ");
        }
        DYNO_DBG(dbgs() << "FAILED!\n");
        return false;
      }
    }
    return true;
  }
};

}; // namespace dyno

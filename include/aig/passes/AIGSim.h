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
  using StateMap = AIGNodeMap<StateVec>;

  AIG &aig;
  StateMap state;

public:
  AIGSim(AIG &aig) : aig(aig), state(aig) {}

  void run() {
    for (auto node : aig.gates()) {
      runGate(node);
    }
    for (auto &node : aig.outputs) {
      setState(node.as<AIGNodeTRef>(), getState(node.getSingleOperand()));
    }
  }

  bool runGate(AIGNodeTRef node) {
    assert(node.isGate());
    bool l = getState(aig[node].operand(0));
    bool r = getState(aig[node].operand(1));
    bool res = l & r;
    setState(node, res);
    return res;
  }

  bool getState(AIGNodeTRef node) {
    if (node.invert()) {
      return !getState(node.nonInverted());
    }
    return state[node];
  }

  void setState(AIGNodeTRef node, bool v) {
    assert(!node.invert());
    // TODO: use |= and check that it compiles to single instr
    state[node] = v;
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
      aSim.state.special.raw()[0] = v;
      bSim.state.special.raw()[0] = v;
      aSim.run();
      bSim.run();
      if (aSim.state.special != bSim.state.special) {
        for (unsigned i = 0; i < a.store.fat.numIDs(); ++i) {
          bool aV = aSim.state.special[i];
          bool bV = bSim.state.special[i];
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

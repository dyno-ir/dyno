#pragma once

#include "aig/AIG.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include <cassert>
#include <print>

namespace dyno {

template <typename T>
static constexpr unsigned bit_increment_plane_num =
    std::bit_width(unsigned(std::numeric_limits<T>::digits)) - 1;
static_assert(bit_increment_plane_num<bool> == 0);
static_assert(bit_increment_plane_num<uint8_t> == 3);

template <typename T> static consteval T bit_increment_plane(unsigned k) {
  unsigned halfPeriod = 1 << k;
  return repeatBits<T>(bit_mask_ones<T>(halfPeriod, halfPeriod),
                       2 * halfPeriod);
}
static_assert(bit_increment_plane<uint8_t>(0) == 0b10101010);
static_assert(bit_increment_plane<uint8_t>(1) == 0b11001100);
static_assert(bit_increment_plane<uint8_t>(2) == 0b11110000);

template <typename T> static consteval auto bit_increment_planes() {
  std::array<T, bit_increment_plane_num<T>> arr;
  for (unsigned i = 0; i < arr.size(); ++i) {
    arr[i] = bit_increment_plane<T>(i);
  }
  return arr;
}

template <std::unsigned_integral Word> class AIGSim {
  using StateMap = AIGNodeMap<std::vector<Word>>;

  AIG &aig;
  StateMap state;

public:
  using word_t = Word;

  AIGSim(AIG &aig) : aig(aig), state(aig) {}

  void run() {
    for (auto node : aig.gates()) {
      runGate(node);
    }
    for (auto &node : aig.outputs) {
      setState(node.template as<AIGNodeTRef>(),
               getState(node.getSingleOperand()));
    }
  }

  word_t runGate(AIGNodeTRef node) {
    assert(node.isGate());
    word_t l = getState(aig[node].operand(0));
    word_t r = getState(aig[node].operand(1));
    word_t res = l & r;
    setState(node, res);
    return res;
  }

  word_t getState(AIGNodeTRef node) {
    if (node.invert()) {
      return ~getState(node.nonInverted());
    }
    return state[node];
  }

  void setState(AIGNodeTRef node, word_t v) {
    assert(!node.invert());
    state[node] = v;
  }

  void resetIO() { state.resetSpecial(aig, 0); }
  void setInput(unsigned i, word_t val) { setState(aig.inputs[i], val); }
  word_t getInput(unsigned i) { return state[aig.inputs[i]]; }
  word_t getOutput(unsigned i) { return state[aig.outputs[i]]; }

  static bool lec(AIG &a, AIG &b) {
    if (a.inputs.size() != b.inputs.size() ||
        a.outputs.size() != b.outputs.size()) {
      DYNO_DBG(dbgs() << "[LEC] Incompatible AIG");
      return false;
    }
    unsigned numInputs = a.inputs.size();
    unsigned numOutputs = a.outputs.size();
    if (numInputs == 0 || numOutputs == 0)
      return true;

    AIGSim aSim(a);
    AIGSim bSim(b);

    aSim.resetIO();
    bSim.resetIO();

    static constexpr auto planes = bit_increment_planes<word_t>();

    unsigned numStaticInputs = std::min(unsigned(planes.size()), numInputs);
    for (unsigned i = 0; i < numStaticInputs; ++i) {
      word_t in = planes[i];
      aSim.setInput(i, in);
      bSim.setInput(i, in);
    }

    while (true) {
      aSim.run();
      bSim.run();
      // Check for output mismatches
      for (unsigned i = 0; i < numOutputs; ++i) {
        word_t aV = aSim.getOutput(i);
        word_t bV = bSim.getOutput(i);
        if (aV != bV) {
          DYNO_DBG(
              std::print(dbgs(), "[LEC] Mismatch: {} ({:x}, {:x})", i, aV, bV));
          return false;
        }
      }
      // Increment inputs
      unsigned i = numStaticInputs;
      for (; i < numInputs; ++i) {
        word_t v = aSim.getInput(i);
        bool isSet = bool(v);
        word_t invertedV = ~v;
        aSim.setInput(i, invertedV);
        bSim.setInput(i, invertedV);
        if (!isSet)
          break;
      }
      // Toggled all inputs back to 0 -> done!
      if (i == numInputs)
        return true;
    }
  }
};
} // namespace dyno

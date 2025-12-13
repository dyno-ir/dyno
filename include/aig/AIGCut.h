#pragma once

#include "aig/AIG.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include <cassert>

namespace dyno {

class AIGCutGenerator;

class AIGCut {
  friend class AIGCutGenerator;

public:
  static constexpr unsigned MaxCut = 8;
  using bloom_t = uint64_t;

  bloom_t bloom = 0;
  StaticVec<AIGNodeTRef, MaxCut, uint8_t> leaves;
  uint32_t numNodes = 0;

  AIGCut() {}

  AIGCut(AIGNodeTRef node) {
    leaves.push_back(node);
    finalize();
  }

  void updateBloom() {
    bloom = 0;
    for (auto node : leaves) {
      bloom |= bit_mask_wrap<bloom_t>(node.idx());
    }
  }

  bool containsBloom(const AIGCut &other) const {
    return (bloom & other.bloom) == other.bloom;
  }

  unsigned countBloomUnion(const AIGCut &other) const {
    return std::popcount(bloom | other.bloom);
  }

  bool containsFast(const AIGCut &other) const {
    if (leaves.size() < other.leaves.size())
      return false;
    if (!containsBloom(other))
      return false;
    return contains(other);
  }

  void finalize() { updateBloom(); }

  bool contains(const AIGCut &other) const {
    auto it = leaves.begin(), itEnd = leaves.end();
    for (auto node : other.leaves) {
      // Skip non-matching
      for (; it != itEnd; ++it) {
        if (*it == node)
          break;
      }
      // Reached end before all leaves were found
      if (it == itEnd)
        return false;
      ++it;
    }
    return true;
  }
};

class AIGCutGenerator {
  AIG &aig;
  AIGNodeVecMap<SmallVec<AIGCut, 10>> cuts;
  unsigned numMaxLeaves = 4;

  bool mergeCut(AIGCut &out, const AIGCut &a, const AIGCut &b) {
    auto aI = a.leaves.begin(), aEnd = a.leaves.end();
    auto bI = b.leaves.begin(), bEnd = b.leaves.end();
    while (aI != aEnd && bI != bEnd) {
      AIGNodeTRef aL = *aI;
      AIGNodeTRef bL = *bI;
      if (aL == bL) {
        out.leaves.push_back(aL);
        ++aI;
        ++bI;
      } else if (aL < bL) {
        out.leaves.push_back(aL);
        ++aI;
      } else {
        out.leaves.push_back(bL);
        ++bI;
      }
      if (out.leaves.size() >= numMaxLeaves)
        return false;
    }
    bool aHasRemain = aI != aEnd;
    bool bHasRemain = bI != bEnd;
    if (!aHasRemain && !bHasRemain)
      return true;
    assert(aHasRemain != bHasRemain);
    auto remainI = aHasRemain ? aI : bI;
    auto remainEnd = aHasRemain ? aEnd : bEnd;
    if (unsigned(remainEnd - remainI) + out.leaves.size() > numMaxLeaves)
      return false;
    for (; remainI != remainEnd; ++remainI) {
      out.leaves.push_back(*remainI);
    }
    return true;
  }

  bool mergeCutFast(AIGCut &out, const AIGCut &a, const AIGCut &b) {
    if (a.countBloomUnion(b) > numMaxLeaves)
      return false;
    return mergeCut(out, a, b);
  }

  bool dedupeCut(AIGNodeTRef node, const AIGCut &newCut) {
    for (auto &cut : cuts[node]) {
      if (cut.containsFast(newCut))
        return true;
    }
    return false;
  }

  void computeCuts(AIGNodeTRef node) {
    DYNO_DBGV(dbgs() << "[Cut] CUTS FOR: " << node << '\n');
    auto &nodeCuts = cuts[node];
    auto &lhsCuts = cuts[aig[node].operand(0)];
    auto &rhsCuts = cuts[aig[node].operand(1)];
    for (auto &lhsCut : lhsCuts) {
      for (auto &rhsCut : rhsCuts) {
        AIGCut newCut;
        DYNO_DBGV(dbgs() << "[Cut] " << Range{lhsCut.leaves} << " "
                         << Range{rhsCut.leaves});
        if (!mergeCutFast(newCut, lhsCut, rhsCut)) {
          DYNO_DBGV(dbgs() << " -> Too Large!\n");
          continue;
        }
        newCut.finalize();
        if (dedupeCut(node, newCut)) {
          DYNO_DBGV(dbgs() << " -> Deduped!\n");
          continue;
        }
        newCut.numNodes = 1 + lhsCut.numNodes + rhsCut.numNodes;
        DYNO_DBGV(dbgs() << " -> " << Range{newCut.leaves} << "("
                         << newCut.numNodes << ")\n");
        nodeCuts.push_back(std::move(newCut));
      }
    }
    nodeCuts.emplace_back(node);
  }

public:
  AIGCutGenerator(AIG &aig) : aig(aig) {}

  void run() {
    assert(numMaxLeaves <= AIGCut::MaxCut);

    cuts.reset(aig);
    cuts[AIGNodeTRef::zero()].emplace_back();
    for (auto &in : aig.inputs) {
      cuts[in].emplace_back(in);
    }
    for (auto node : aig.gates()) {
      computeCuts(node);
    }
  }
};

} // namespace dyno

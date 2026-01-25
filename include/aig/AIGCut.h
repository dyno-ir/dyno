#pragma once

#include "aig/AIG.h"
#include "support/Debug.h"
#include "support/TruthTable.h"
#include <cassert>

namespace dyno {

class AIGCutGenerator;

class AIGCut {
  friend class AIGCutGenerator;

public:
  static constexpr unsigned MaxCut = 6;
  using bloom_t = uint64_t;

  bloom_t bloom = 0;
  StaticVec<AIGNodeTRef, MaxCut, uint8_t> leaves;
  int32_t cost = 0;
  TruthTable truth;

  AIGCut() {}

  AIGCut(AIGNodeTRef node) {
    leaves.push_back(node);
    updateBloom();
  }

  void clear() {
    leaves.clear();
    bloom = 0;
  }

  size_t size() const { return leaves.size(); }

  bool empty() const { return leaves.empty(); }

  bool isTrivial() const { return leaves.size() == 1; }

  void updateBloom() {
    bloom = 0;
    for (auto node : leaves) {
      bloom |= bit_mask_wrap<bloom_t>(node.idx());
    }
  }

  void updateCost(AIG &aig) {
    // a_sz < b_sz
    // a_fanout/a_sz > b_fanout/b_sz
    // a_fanout - a_sz > b_fanout - b_sz
    // a_sz - a_fanout < b_sz - b_fanout
    cost = leaves.size();
    for (auto node : leaves) {
      cost -= aig.getUseCount(node);
    }
  }

  friend bool operator<(const AIGCut &a, const AIGCut &b) {
    if (a.cost == b.cost)
      return a.leaves.size() < b.leaves.size();
    return a.cost < b.cost;
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
public:
  struct Config {
    unsigned numMaxLeaves = 4;
    unsigned numMaxCuts = 7;
    bool computeTruth = true;
  };

private:
  AIG &aig;
  Config config;
  AIGNodeVecMap<SmallVec<AIGCut, 8>> cuts;

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
      if (out.size() >= config.numMaxLeaves)
        return false;
    }
    bool aHasRemain = aI != aEnd;
    bool bHasRemain = bI != bEnd;
    if (!aHasRemain && !bHasRemain)
      return true;
    assert(aHasRemain != bHasRemain);
    auto remainI = aHasRemain ? aI : bI;
    auto remainEnd = aHasRemain ? aEnd : bEnd;
    if (unsigned(remainEnd - remainI) + out.size() > config.numMaxLeaves)
      return false;
    for (; remainI != remainEnd; ++remainI) {
      out.leaves.push_back(*remainI);
    }
    return true;
  }

  bool mergeCutFast(AIGCut &out, const AIGCut &a, const AIGCut &b) {
    if (a.size() + b.size() > config.numMaxLeaves &&
        a.countBloomUnion(b) > config.numMaxLeaves)
      return false;
    return mergeCut(out, a, b);
  }

  static TruthTable expandTruth(const AIGCut &cut, const AIGCut &merged) {
    if (cut.size() == merged.size())
      return cut.truth;
    uint8_t varMap[AIGCut::MaxCut];
    unsigned cI = 0;
    for (unsigned i = 0; i < merged.size() && cI < cut.size(); ++i) {
      if (cut.leaves[cI] != merged.leaves[i])
        continue;
      varMap[cI++] = i;
    }
    return cut.truth.expand({varMap, cut.size()}, merged.size());
  }

  int insertCut(AIGNodeTRef node, const AIGCut &newCut) {
    DYNO_DBGV(dbgs() << Range{newCut.leaves});
    auto &nodeCuts = cuts[node];
    // Discard newCut if dominated by existing cut
    for (auto &cut : nodeCuts) {
      assert(!cut.empty());
      if (newCut.containsFast(cut)) {
        DYNO_DBGV(dbgs() << ", dominated by " << Range{cut.leaves} << '\n');
        return -1;
      }
    }
    // Discard existing cuts dominated by newCut
    auto itEnd = nodeCuts.end();
    auto itInsert = nodeCuts.end();
    for (auto it = nodeCuts.begin(); it != itEnd;) {
      auto &cut = *it;
      if (cut.containsFast(newCut)) {
        DYNO_DBGV(dbgs() << ", dominates " << Range{cut.leaves} << '('
                         << it - nodeCuts.begin() << ')');
        if (itInsert == itEnd) {
          itInsert = it;
        } else {
          nodeCuts.erase_unordered(it);
          itEnd = nodeCuts.end();
          continue;
        }
      }
      ++it;
    }
    // Too many cuts :(
    if (itInsert == itEnd && nodeCuts.size() > config.numMaxCuts) {
      // Evict max cost cut
      itInsert = nodeCuts.begin();
      assert(!nodeCuts.begin()->empty());
      for (auto it = nodeCuts.begin() + 1; it != itEnd; ++it) {
        auto &cut = *it;
        auto &maxCostCut = *itInsert;
        assert(!cut.empty());
        if (maxCostCut < cut)
          itInsert = it;
      }
      if (!(newCut < *itInsert))
        return -1;
    }
    DYNO_DBGV(dbgs() << ", inserted at " << (itInsert - nodeCuts.begin())
                     << ", cost: " << newCut.cost);
    if (itInsert == itEnd) {
      nodeCuts.emplace_back(newCut);
      return nodeCuts.size() - 1;
    } else {
      *itInsert = newCut;
      return itInsert - nodeCuts.begin();
    }
  }

  void computeCuts(AIGNodeTRef node) {
    DYNO_DBGV(dbgs() << "[Cut] CUTS FOR: " << node << '\n');
    auto &nodeCuts = cuts[node];
    AIGNodeTRef lhsRef = aig[node].operand(0);
    AIGNodeTRef rhsRef = aig[node].operand(1);
    auto &lhsCuts = cuts[lhsRef];
    auto &rhsCuts = cuts[rhsRef];
    bool insertedTrivialCut = false;
    for (auto &lhsCut : lhsCuts) {
      for (auto &rhsCut : rhsCuts) {
        AIGCut newCut;
        DYNO_DBGV(dbgs() << "[Cut] " << Range{lhsCut.leaves} << " "
                         << Range{rhsCut.leaves} << " -> ");
        if (!mergeCutFast(newCut, lhsCut, rhsCut)) {
          DYNO_DBGV(dbgs() << "Too Large!\n");
          continue;
        }
        newCut.finalize();
        newCut.updateCost(aig);
        int r = insertCut(node, newCut);
        if (r == -1)
          continue;
        auto &insertedCut = nodeCuts[r];
        insertedTrivialCut |= insertedCut.isTrivial();
        if (config.computeTruth) {
          TruthTable lhsTruth = expandTruth(lhsCut, insertedCut);
          TruthTable rhsTruth = expandTruth(rhsCut, insertedCut);
          if (lhsRef.invert())
            lhsTruth = ~lhsTruth;
          if (rhsRef.invert())
            rhsTruth = ~rhsTruth;
          insertedCut.truth = lhsTruth & rhsTruth;
          DYNO_DBGV(dbgs() << ", truth: "
                           << insertedCut.truth.format(insertedCut.size()));
        }
        DYNO_DBGV(dbgs() << '\n');
      }
    }

    if (!insertedTrivialCut) {
      AIGCut trivialCut(node);
      trivialCut.truth = TruthTable::identity(0);
      nodeCuts.emplace_back(trivialCut);
    }
  }

public:
  AIGCutGenerator(AIG &aig) : aig(aig) {}

  void run() {
    assert(config.numMaxLeaves <= AIGCut::MaxCut);

    cuts.reset(aig);
    auto &zeroCut = cuts[AIGNodeTRef::zero()].emplace_back();
    zeroCut.truth = TruthTable::zero();
    for (auto &in : aig.inputs) {
      auto &inCut = cuts[in].emplace_back(in);
      inCut.truth = TruthTable::identity(0);
    }
    for (auto node : aig.gates()) {
      computeCuts(node);
    }
  }
};

} // namespace dyno

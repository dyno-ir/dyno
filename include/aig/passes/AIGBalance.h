#pragma once

#include "aig/AIG.h"
#include "dyno/ObjMap.h"
#include "support/Debug.h"
#include <algorithm>
#include <cassert>
#include <queue>

namespace dyno {

class AIGBalance {
public:
  void updateHeight(AIGNodeRef node) {
    assert(!node.isSpecial());
    height[node] = 1 + std::max(height[node[0]], height[node[1]]);
  }

  SmallVec<AIGNodeTRef, 8> findMulti(AIGNodeTRef root) {
    assert(root.isGate());
    SmallVec<AIGNodeTRef, 8> dfs;
    SmallVec<AIGNodeTRef, 8> multi;
    auto handleNode = [&](AIGNodeTRef node) {
      if (node.invert() || node.isTerminator() || aig.getUseCount(node) > 2)
        multi.push_back(node);
      else
        dfs.push_back(node);
    };
    aig[root].operands().for_each(handleNode);
    while (!dfs.empty()) {
      auto node = dfs.pop_back_val();
      aig[node].operands().for_each(handleNode);
    }
    return multi;
  }

  AIGNodeTRef createNew(AIGNodeTRef old, AIGNodeTRef lhs, AIGNodeTRef rhs) {
    AIGNodeTRef node = newAig.createAND(lhs, rhs);
    if (!old.isSpecial()) {
      remap.insert(old, node);
      updateHeight(newAig[node]);
    }
    return node;
  }

  void rebalanceMulti(AIGNodeTRef root, SmallVecImpl<AIGNodeTRef> &multi) {
    assert(!multi.empty());
    if (multi.size() == 1) {
      remap.insert(root, multi[0]);
      return;
    }
    if (multi.size() == 2) {
      createNew(root, remap[multi[0]], remap[multi[1]]);
      return;
    }
    for (auto &nodeRef : multi) {
      nodeRef = remap[nodeRef];
    }
    auto comp = [&](AIGNodeTRef l, AIGNodeTRef r) {
      return height[l] > height[r];
    };
    std::make_heap(multi.begin(), multi.end(), comp);
    while (true) {
      std::pop_heap(multi.begin(), multi.end(), comp);
      auto lhs = multi.pop_back_val();
      if (multi.empty())
        return;
      std::pop_heap(multi.begin(), multi.end(), comp);
      auto rhs = multi.pop_back_val();
      auto node = createNew(root, lhs, rhs);
      multi.push_back(node);
      std::push_heap(multi.begin(), multi.end(), comp);
    }
  }

  void balanceImpl(AIGNodeTRef root) {
    if (root.isTerminator() || remap.contains(root))
      return;
    auto multi = findMulti(root);
    DYNO_DBGV(dbgs() << "[Balance] Multigate " << Range{multi});
    AIG::simplifyMulti(multi);
    DYNO_DBGV(dbgs() << " simplified to " << Range{multi} << '\n');
    for (auto &node : multi) {
      balanceImpl(node);
    }
    rebalanceMulti(root, multi);
  }

  AIG run() {
    remap.reset(aig);
    newAig = aig.cloneInputs();
    remap.setupInputMap(aig, newAig);
    height.reset(aig, 0);
    for (auto out : aig.outputs) {
      balanceImpl(out.getSingleOperand());
      newAig.createOutput(remap[out.getSingleOperand()]);
    }
    return std::move(newAig);
  }

  AIG &aig;
  AIG newAig;
  AIGNodeRemap remap;
  AIGNodeVecMap<uint32_t> height;

  AIGBalance(AIG &aig) : aig(aig) {}
};
}; // namespace dyno

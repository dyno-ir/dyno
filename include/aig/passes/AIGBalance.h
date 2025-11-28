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
  // SmallVec<AIGNodeTRef, 16> calcTopoOrder(AIG &aig) {
  //   SmallVec<AIGNodeTRef, 16> dfsStack{Range{aig.outputs}.transform(
  //       [](size_t, FatAIGNodeRef node) { return node.getSingleOperand(); })};
  //   SmallVec<AIGNodeTRef, 16> order;
  //   while (!dfsStack.empty()) {
  //     AIGNodeTRef node = dfsStack.pop_back_val();
  //     if (!node)
  //       continue;
  //     DYNO_DBG(dbgs() << node.idx() << ", invert: " << node.invert() <<
  //     "\n"); order.push_back(node); for (auto op : aig[node]) {
  //       if (op.isSpecial())
  //         continue;
  //       dfsStack.push_back(op);
  //     }
  //   }
  //   std::reverse(order.begin(), order.end());
  //   return order;
  // }

  // void simplifyAll() {
  //   SmallVec<AIGNodeTRef, 16> simplified(aig.numGateIDs(), 0);
  //   auto getSimple = [&](AIGNodeTRef ref) {
  //     auto simple = simplified[ref.idx()];
  //     if (simple)
  //       return simple;
  //     return ref;
  //   };
  //   ThinAIGNodeStore simplifiedThin;
  //   for (auto node : aig.gates()) {
  //     auto l = getSimple(node[0]);
  //     auto r = getSimple(node[1]);
  //     simplified[node.idx()] = simplify(l, r);
  //     simplifiedThin.create(l, r);
  //   }
  //   aig.store.thin = std::move(simplifiedThin);
  //   for(auto output : aig.outputs) {
  //
  //   }
  // }

  //
  // ObjMapVec<AIGNode, unsigned> calcHeightMap(AIG &aig) {
  //   // auto order = calcTopoOrder(aig);
  //   SmallVec<AIGNodeTRef, 16> worklist;
  //   height.resize(aig.store.thin.numIDs() * 2, 0);
  //   for (auto nodeT : order) {
  //     if (nodeT.isSpecial())
  //       continue;
  //     auto node = aig[nodeT];
  //     auto getHeight = [&](AIGNodeTRef ref) {};
  //     height[nodeT] = 1 + std::max(getHeight(node[0]), getHeight(node[1]));
  //   }
  //   return height;
  // }
  //

  unsigned getHeight(AIGNodeTRef node) {
    return node.isSpecial() ? 0 : height[node.idx()];
  }

  void updateHeight(AIGNodeRef node) {
    assert(!node.isSpecial());
    height[node.idx()] = 1 + std::max(getHeight(node[0]), getHeight(node[1]));
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

  AIGNodeTRef toNew(AIGNodeTRef old) {
    if (old.isSpecial()) [[unlikely]]
      return old;
    return AIGNodeTRef(newIDs[old.idx()], old.invert());
  }

  void updateNew(AIGNodeTRef old, AIGNodeTRef node) {
    assert(!old.isSpecial());
    newIDs[old.idx()] = node.idx();
  }

  AIGNodeTRef createNew(AIGNodeTRef old, AIGNodeTRef lhs, AIGNodeTRef rhs) {
    AIGNodeTRef node = newAig.createAND(lhs, rhs);
    if (!old.isSpecial()) {
      updateNew(old, node);
      updateHeight(newAig[node]);
    }
    return node;
  }

  void rebalanceMulti(AIGNodeTRef root, SmallVecImpl<AIGNodeTRef> &multi) {
    assert(!multi.empty());
    if (multi.size() == 1) {
      updateNew(root, multi[0]);
      return;
    }
    if (multi.size() == 2) {
      createNew(root, toNew(multi[0]), toNew(multi[1]));
      return;
    }
    for (auto &nodeRef : multi) {
      nodeRef = toNew(nodeRef);
    }
    auto comp = [&](AIGNodeTRef l, AIGNodeTRef r) {
      return getHeight(l) > getHeight(r);
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
    if (root.isTerminator() || newIDs[root.idx()] != uint32_t(-1))
      return;
    auto multi = findMulti(root);
    DYNO_DBG("AIG Multigate", dumpRefs(multi));
    AIG::simplifyMulti(multi);
    DYNO_DBG(dbgs() << "simplified to: "; dumpRefs(multi));
    for (auto &node : multi) {
      balanceImpl(node);
    }
    rebalanceMulti(root, multi);
  }

  template <typename U> void dumpRefs(const U &range) {
    for (auto node : range) {
      dbgs() << " " << node;
    }
    dbgs() << "\n";
  }

  AIG run() {
    newAig = aig.cloneInputs();
    height.clear();
    height.resize(aig.numGateIDs(), 0);
    newIDs.clear();
    newIDs.resize(aig.numGateIDs(), -1);
    newIDs[0] = 0;
    for (auto out : aig.outputs) {
      balanceImpl(out.getSingleOperand());
      newAig.createOutput(toNew(out.getSingleOperand()));
    }
    return std::move(newAig);
  }

  AIG &aig;
  AIG newAig;
  std::vector<uint32_t> newIDs;
  SmallVec<unsigned, 16> height;

  AIGBalance(AIG &aig) : aig(aig) {}
};
}; // namespace dyno

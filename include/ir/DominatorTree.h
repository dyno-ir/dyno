#pragma once

#include <memory>
#include <unordered_map>
#include <vector>

#include "ir/IR.h"
#include "ir/IRPass.h"

class DominatorTree {
public:
  static const IRInfoID ID;

  unsigned numBlocks = 0;
  std::unordered_map<CFGBlock *, int> dfsPreorderNum;
  std::vector<CFGBlock *> dfsPreorderBlock;
  std::vector<int> dfsPreorderParent;
  std::vector<int> forestParent;
  std::vector<int> forestLabel;
  std::vector<int> semiDominator;
  std::vector<int> immDominator;
};

class DominatorTreePass : public IRPass<Function> {

  const char *name() { return "DominatorTreePass"; }
  void advertise(IRInfo<Function> &info);
  void run(Function &func, IRInfo<Function> &info);
  void invalidate(IRInfo<Function> &info);

  void dfs(CFGBlock &block, int parentId);
  void computeSemiDominators();
  void computeImmDominators();
  void link(int w);
  int eval(int id);

private:
  std::unique_ptr<DominatorTree> tree;
};

class PrintDominatorTreePass : public IRPass<Function> {
  const char *name() { return "PrintDominatorTreePass"; }

  void run(Function &, IRInfo<Function> &info);
};

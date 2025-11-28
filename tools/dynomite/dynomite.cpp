#include "aig/passes/AIGBalance.h"
#include "aig/passes/AIGSim.h"
#include "aig/AIGCut.h"
#include <aig/AIG.h>
#include <aig/AIGPrinter.h>
#include <cassert>
#include <dynomite/SAT.h>
#include <fstream>

using namespace dyno;

int main() {

  std::ofstream os{"dump-aig.dot"};
  AIGDotPrinter aigPrinter(os);
  AIG aig;
  // auto i0 = AIGNodeTRef{aig.createInput()};
  // AIGNodeTRef x = i0;
  // for (unsigned i = 0; i < 20; ++i) {
  //   auto i1 = AIGNodeTRef{aig.createInput()};
  //   x = aig.createAND(i1, x);
  // }
  {
    SmallVec<AIGNodeBuilder, 8> ins;
    for (unsigned i = 0; i < 10; ++i) {
      ins.emplace_back(aig, aig.createInput().as<AIGNodeTRef>());
    }
    ((~(ins[0] & ins[1]) & ins[2] & ins[3] & ins[4]) & (ins[2] & ins[3]))
        .output();
    ((~(ins[0] & ins[1]) & ins[2] & ins[3] & ins[4]) | (ins[2] & ins[3]))
        .output();
  }
  // ((~(ins[0] & ins[1]) & ins[2] & ins[3] & ins[4]) & (ins[2] &
  // ins[3])).output();
  // (~((ins[0] & ins[1]) & ins[2] & ins[3] & ins[4]) & (ins[3] & ins[5] &
  // ins[6])) .output();
  // ((ins[0] & ins[1]) & ~ins[0] & ins[1] & ins[4]).output();
  //(~(ins[0] & ins[1]) & ins[1] & ins[4] & ins[4] & ins[4]).output();
  // aigPrinter.print(aig);

  AIG newAIG = AIGBalance(aig).run();
  assert(AIGSim::lec(aig, newAIG));
  // auto height = AIGBalance(aig).calcHeightMap(aig);

  // aigPrinter.print(aig, [&](auto &os, auto ref) {
  //     // os << "h: " << height[ref];
  // });
  aigPrinter.print(newAIG);

  // SATSolver s;
  // auto a = s.addVar();
  // auto b = s.addVar();
  // auto c = s.addVar();
  // s.addClause({a, b, c});
  // s.addClause({-a, b, c});
  // s.addClause({a, -b, c});
  // s.addClause({a, b, -c});
  // s.addClause({-a, -b});
  // s.addClause({-b, -c});
  // s.addClause({-a, -c});
  // dumpSATSolver(s);
}

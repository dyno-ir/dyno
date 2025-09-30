#include <dynomite/SAT.h>

using namespace dyno;

int main() {
  SATSolver s;
  auto a = s.addVar();
  auto b = s.addVar();
  auto c = s.addVar();
  s.addClause({a, b, c});
  s.addClause({-a, b, c});
  s.addClause({a, -b, c});
  s.addClause({a, b, -c});
  s.addClause({-a, -b});
  s.addClause({-b, -c});
  s.addClause({-a, -c});
  dumpSATSolver(s);
}

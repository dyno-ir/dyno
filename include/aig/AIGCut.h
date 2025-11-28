#pragma once

#include "aig/AIG.h"
#include <cassert>

namespace dyno {

class AIGCut {
  SmallVec<AIGNodeTRef, 8> nodes;
  unsigned numLeaves;
};

class AIGCutGenerator {

};

} // namespace dyno

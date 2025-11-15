#pragma once

#include "dyno/IDImpl.h"
#include "support/SmallVec.h"
#include <cassert>

namespace dyno {

class IDObjStore {
  SmallVec<ObjID, 32> freeIds;
  ObjID::num_t nextId = 0;

public:
  ObjID create() {
    if (!freeIds.empty()) [[unlikely]] {
      return freeIds.pop_back_val();
    }
    return ObjID(nextId++);
  }

  void destroy(ObjID id) {
    assert(id < nextId);
    freeIds.push_back(id);
  }

  ObjID::num_t getNumIDs() { return nextId; }
};
} // namespace dyno

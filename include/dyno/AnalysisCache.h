#pragma once
#include "Instr.h"
#include "support/CallableRef.h"
#include "support/DenseMap.h"

namespace dyno {

template <typename Ref, typename Result> class AnalysisCache {
  DenseMap<Ref, Result> map;

public:
  void clearAll() { map.clear(); }

  Result *find(Ref ref) {
    auto it = map.find(ref);
    if (it == map.end())
      return nullptr;
    return &it.val();
  }

  auto &insert(Ref ref, const Result &result) {
    return insert(ref, Result{result});
  }
  auto &insert(Ref ref, Result &&result) {
    return map.insert(ref, std::move(result)).val();
  }
  auto &insertOrAssign(Ref ref, const Result &result) {
    return insertOrAssign(ref, Result{result});
  }
  auto &insertOrAssign(Ref ref, Result &&result) {
    return map.insertOrAssign(ref, std::move(result)).val();
  }

  auto &findOrInsert(Ref ref, const Result &result) {
    return map.findOrInsert(ref, result).second.val();
  }

  void clear(Ref ref) {
    auto it = map.find(ref);
    if (it)
      map.erase(it);
  }

  auto &raw() { return map; }
};

}; // namespace dyno

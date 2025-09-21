#pragma once
#include "Instr.h"
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

  void insert(Ref ref, Result result) { map.insert(ref, std::move(result)); }
  void clear(Ref ref) {
    auto it = map.find(ref);
    if (it)
      map.erase(it);
  }

  auto &raw() { return map; }
};

}; // namespace dyno

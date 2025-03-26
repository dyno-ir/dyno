#pragma once

#include <cassert>
#include <dyno/Obj.h>
#include <support/Ranges.h>
#include <vector>

namespace dyno {

template <typename K, typename V> class ObjMapVec {
  using Traits = ObjTraits<K>;

public:
  std::vector<V> elements;

  void ensure(ObjRef<K> ref) {
    if (inRange(ref)) [[likely]] {
      return;
    }
    resize(ref.getObjID().num + 1);
  }

  bool inRange(ObjRef<K> ref) { return ref.getObjID() < elements.size(); }

  void reserve(size_t sz) { elements.reserve(sz); }

  void resize(size_t sz) { elements.resize(sz); }

  void clear() { elements.clear(); }

  size_t size() { return elements.size(); }

  /*template <typename VV> void sync(ObjMapVec<K, VV> &o) { resize(o.size()); }*/

  V &operator[](ObjRef<K> ref) {
    assert(inRange(ref));
    return elements[ref.getObjID()];
  }

  V &get_ensure(ObjRef<K> ref) {
    ensure(ref);
    return this->operator[](ref);
  }
};

} // namespace dyno

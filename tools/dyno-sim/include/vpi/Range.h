#pragma once
#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "vpi/IDs.h"

namespace dyno {
class RangeRef;
class VPIRange {};
template <> struct ObjTraits<VPIRange> {
  using FatRefT = RangeRef;
  static constexpr DialectType ty{VPI_RANGE};
};

class VPIRangeRef : public ObjRef<VPIRange> {
public:
  using ObjRef::ObjRef;
  VPIRangeRef(ObjRef<VPIRange> ref) : ObjRef(ref) {}
  VPIRangeRef(uint32_t len) : ObjRef<VPIRange>(ObjID{len}) {}

  // static bool is_impl(DynObjRef ref) { return ref.getType() == VPI_RANGE; }

  // dyno only supports ranges starting at zero, so the whole object can
  // be encoded in the ID for now. If we want to support real ranges we need
  // to make this a backed object.
  uint32_t getLower() const { return 0; }
  uint32_t getUpper() const { return obj - 1; }
  uint32_t getLen() const { return obj; }
  uint32_t getLeft() const { return getUpper(); }
  uint32_t getRight() const { return getLower(); }
};
} // namespace dyno
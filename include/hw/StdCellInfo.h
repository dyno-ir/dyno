#pragma once

#include "dyno/Obj.h"
#include "hw/IDs.h"
namespace dyno {

class StdCellInfo {
public:
  std::optional<double> area;
  std::optional<bool> isFlipFlop;
  // also add elements to FOR_STDCELL_INFO_ELEMENTS in hw/PrintParse.h

  StdCellInfo(ObjRef<StdCellInfo> ref) {}
  StdCellInfo(DynObjRef, FatObjRef<StdCellInfo> other)
      : area(other->area), isFlipFlop(other->isFlipFlop) {}
};

using StdCellInfoRef = FatObjRef<StdCellInfo>;

template <> struct ObjTraits<StdCellInfo> {
  static constexpr DialectType ty{HW_STDCELL_INFO};
  using FatRefT = StdCellInfoRef;
};

}; // namespace dyno

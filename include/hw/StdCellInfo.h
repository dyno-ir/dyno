#pragma once

#include "dyno/Obj.h"
#include "hw/IDs.h"
namespace dyno {

class StdCellInfo {
public:
  std::optional<double> area;
  std::optional<bool> isFlipFlop;

  StdCellInfo(ObjRef<StdCellInfo> ref) {}
};

using StdCellInfoRef = FatObjRef<StdCellInfo>;

template <> struct ObjTraits<StdCellInfo> {
  static constexpr DialectType ty{HW_STDCELL_INFO};
  using FatRefT = StdCellInfoRef;
};

}; // namespace dyno
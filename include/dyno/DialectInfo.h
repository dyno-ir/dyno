#pragma once
#include "Interface.h"
#include "Obj.h"
#include <cstdint>
#include <string_view>

namespace dyno {

#include "DialectIDs.inc"

struct DialectInfo {
  std::string_view name;
};

struct TyInfo {
  std::string_view name;
  // for access when enumerating types
  bool isDefUse;

  constexpr TyInfo(std::string_view name, bool isDefUse) : name(name), isDefUse(isDefUse) {}
};

struct OpcodeInfo {
  std::string_view name;
};

template <uint8_t> struct DialectTraits {
  constexpr static DialectInfo info = DialectInfo{"INVALID"};
};

template <> struct InterfaceTraits<TyInfo> {
  static const TyInfo *dispatch1(DynObjRef ref, const TyInfo **interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const TyInfo &dispatch2(DynObjRef ref, const TyInfo *interface) {
    return interface[ref.getTyID() & ~bit_mask_ones<TyID::num_t>(
                                         2, bit_mask_sz<TyID::num_t> - 2)];
  }
};

} // namespace dyno

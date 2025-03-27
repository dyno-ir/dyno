#pragma once

#include "support/Bits.h"
#include <dyno/Interface.h>
#include <string_view>

namespace dyno {

struct DialectInfo {
  std::string_view name;
};

struct TyInfo {
  std::string_view name;
};

template <> struct InterfaceTraits<TyInfo> {
  static const TyInfo *dispatch1(DynObjRef ref, const TyInfo **interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const TyInfo &dispatch2(DynObjRef ref, const TyInfo *interface) {
    return interface[ref.getTyID() & ~bit_mask_msb<TyID::num_t>()];
  }
};

constexpr DialectInfo coreDialectInfo{"core"};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wc99-designator"
constexpr TyInfo coreTyInfo[] = {{"instr"}, {"constant"}, {"block"}};
#pragma clang diagnostic pop
} // namespace dyno

#pragma once
#include "Interface.h"
#include "Obj.h"
#include <cstdint>
#include <string_view>

namespace dyno {

#include "DialectIDs.inc"

static constexpr size_t MAX_NUM_DIALECTS = 256;

struct DialectInfo {
  std::string_view name;
};

struct TyInfo {
  std::string_view name;
  // for access when enumerating types
  bool isDefUse;

  constexpr TyInfo(std::string_view name, bool isDefUse)
      : name(name), isDefUse(isDefUse) {}
};

struct OpcodeInfo {
  std::string_view name;
};

template <uint8_t> struct DialectTraits {
  constexpr static DialectInfo info = DialectInfo{"INVALID"};
};

template <> struct InterfaceTraits<DialectInfo> {
  static const DialectInfo *dispatch1(DynObjRef ref,
                                      const DialectInfo **interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const DialectInfo &dispatch2(DynObjRef ref,
                                      const DialectInfo *interface) {
    return *interface;
  }
  static const unsigned ID = ~0;
};

template <> struct InterfaceTraits<TyInfo> {
  static const ArrayRef<TyInfo> dispatch1(DynObjRef ref,
                                          ArrayRef<TyInfo> *interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const TyInfo &dispatch2(DynObjRef ref, ArrayRef<TyInfo> interface) {
    return interface[ref.getTyID() & bit_mask_zeros<TyID::num_t>(
                                         1, bit_mask_sz<TyID::num_t> - 1)];
  }
};

// specialize this for more sophisticated initialization.
class Context;
template <DialectID dialect>
constexpr inline void registerDialect(Context *,
                                      const DialectInfo **dialectInfos,
                                      ArrayRef<TyInfo> *typeInfos,
                                      ArrayRef<OpcodeInfo> *opcodeInfos) {
  dialectInfos[dialect.num] = &DialectTraits<dialect>::info;
  typeInfos[dialect.num] = DialectTraits<dialect>::tyInfo;
  opcodeInfos[dialect.num] = DialectTraits<dialect>::opcInfo;
}

struct DialectInfos {
  std::array<const DialectInfo *, MAX_NUM_DIALECTS> dialectInfoArr;
  std::array<ArrayRef<TyInfo>, MAX_NUM_DIALECTS> typeInfoArr;
  std::array<ArrayRef<OpcodeInfo>, MAX_NUM_DIALECTS> opcodeInfoArr;
};

template <DialectID... Dialects> struct AutoDialectInfos {
  DialectInfos infos;

  constexpr void registerDialects() {
    ((registerDialect<Dialects>(nullptr, infos.dialectInfoArr.data(),
                                infos.typeInfoArr.data(),
                                infos.opcodeInfoArr.data())),
     ...);
  }

  constexpr AutoDialectInfos() { registerDialects(); }
};

} // namespace dyno

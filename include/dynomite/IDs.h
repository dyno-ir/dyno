#pragma once

#include "dyno/DialectInfo.h"

namespace dyno {
using DMTType = SpecificDialectType<DialectID{DIALECT_DYNOMITE}>;

enum class DMTTyID : uint8_t { DMT_SAT_CLAUSE };

constexpr DMTType DMT_SAT_CLAUSE{uint8_t(DMTTyID::DMT_SAT_CLAUSE)};

template <> struct DialectTraits<DIALECT_DYNOMITE> {
  constexpr static DialectInfo info{"dmt"};
  constexpr static TyInfo tyInfo[] = {{"sat-clause", false}};
  constexpr static OpcodeInfo opcInfo[] = {{"goof"}};
};

} // namespace dyno

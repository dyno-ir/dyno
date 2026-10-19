#pragma once

#include "dyno/DialectInfo.h"

namespace dyno {
using DMTType = SpecificDialectType<DialectID{DIALECT_DYNOMITE}>;

enum class DMTTyID : uint8_t { DMT_SAT_CLAUSE, DMT_SAT_VAR };

constexpr DMTType DMT_SAT_CLAUSE{uint8_t(DMTTyID::DMT_SAT_CLAUSE)};
constexpr DMTType DMT_SAT_VAR{uint8_t(DMTTyID::DMT_SAT_VAR)};

template <> struct DialectTraits<DIALECT_DYNOMITE> {
  constexpr static DialectInfo info{"dmt"};
  constexpr static TyInfo tyInfo[] = {{"sat-clause", false},
                                      {"sat-var", false}};
  constexpr static OpcodeInfo opcInfo[] = {{"goof"}};
};

} // namespace dyno

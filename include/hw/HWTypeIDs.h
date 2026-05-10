#pragma once

#include "dyno/DialectInfo.h"
#include "type/TypeInfo.h"
namespace dyno::hw {

// type dialect types for debug info
constexpr BaseTypeRef HW_TYPE_VLOG_LOGIC{DIALECT_HW, 0};
constexpr BaseTypeRef HW_TYPE_VLOG_BIT{DIALECT_HW, 1};
constexpr BaseTypeRef HW_TYPE_VLOG_REG{DIALECT_HW, 2};

static_assert(HW_TYPE_VLOG_LOGIC.getDialectID() == DIALECT_HW.num);

constexpr auto hwTypeDialectTypeNames = {"logic", "bit", "reg"};

}; // namespace dyno::hw

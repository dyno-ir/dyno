#include "support/TruthTable.h"

namespace dyno {

static_assert(tt_increment_plane_num<bool> == 0);
static_assert(tt_increment_plane_num<uint8_t> == 3);
static_assert(tt_increment_plane<uint8_t>(0) == 0b10101010);
static_assert(tt_increment_plane<uint8_t>(1) == 0b11001100);
static_assert(tt_increment_planes<uint8_t>[2] == 0b11110000);

} // namespace dyno

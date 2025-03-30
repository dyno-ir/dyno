#include <array>
#include <cstddef>
#include <cstdint>

namespace dyno {

// completely type-erased for speed. We could do templated/typed versions
// for ObjRefs with template param themselves, but wouldn't work for e.g.
// DynObjRef -> FatDynObjRef (requires runtime lookup of type)
// Alternative is allowing only typed refs to be resolved, but I doubt we'd
// want to handle different types very differently anyways (at least all of them
// should have a ObjID to ptr LUT to put in globResolverTable).
class GlobalResolver {
public:
  static constexpr size_t numDialects = 2;
  static constexpr size_t numTypes = 16;

  // plain global symbol
  static std::array<void **, numTypes * numDialects> globResolverTable;

  // it might be worth always making numTypes 256 for less bitmanip.
  static size_t getIdx(uint8_t dialect, uint8_t type)
      __attribute__((always_inline)) {
    return dialect * numTypes + type;
  }
  static void *resolve(uint8_t dialect, uint8_t type)
      __attribute__((always_inline)) {
    return globResolverTable[getIdx(dialect, type)];
  }
  static void registerResolver(uint8_t dialect, uint8_t type, void **table)
      __attribute__((always_inline)) {
    globResolverTable[getIdx(dialect, type)] = table;
  }

  // maybe have some functionality here to (by value) swap out the
  // globResolverTable to allow multiple contexts
};

}; // namespace dyno

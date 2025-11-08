#pragma once

#include "dyno/Instr.h"
#include "dyno/ObjMap.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "support/Bits.h"
#include "support/DedupeMap.h"
#include "support/SlabAllocator.h"
#include <cstdint>
#include <unordered_map>
namespace dyno {

struct DebugSourceLocImpl {
  uint32_t fileName;
  uint32_t beginLine;
  uint32_t beginCol;
  uint32_t endLine;
  uint32_t endCol;
  static constexpr uint32_t hash(const DebugSourceLocImpl &loc) {
    uint32_t acc = hash_u32(loc.fileName);
    acc = hash_combine(acc, hash_u32(loc.beginLine));
    acc = hash_combine(acc, hash_u32(loc.beginCol));
    acc = hash_combine(acc, hash_u32(loc.endLine));
    acc = hash_combine(acc, hash_u32(loc.endCol));
    return acc;
  }

  constexpr friend bool operator==(const DebugSourceLocImpl &lhs,
                                   const DebugSourceLocImpl &rhs) {
    return lhs.fileName == rhs.fileName && lhs.beginLine == rhs.beginLine &&
           lhs.beginCol == rhs.beginCol && lhs.endLine == rhs.endLine &&
           lhs.endCol == rhs.endCol;
  }
};

struct DebugSourceLoc {
  const char *fileName;
  uint32_t beginLine;
  uint32_t beginCol;
  uint32_t endLine;
  uint32_t endCol;
  // maybe even have a genvar idx or smth
};

class StringDedupeMap {
  std::unordered_map<std::string_view, uint32_t> stringMap;
  MixedSizeSlabAllocator<> strtab{sizeof(char)};

public:
  uint32_t getCanonicalIdx(std::string_view str) {
    if (auto it = stringMap.find(str); it != stringMap.end())
      return it->second;

    auto idx = strtab.allocate(str.size() + 1);
    auto *ptr = reinterpret_cast<char *>(strtab.resolve(idx));
    std::copy(str.begin(), str.end(), ptr);
    ptr[str.size()] = '\0';

    auto copy = std::string_view{ptr, str.size() + 1};
    stringMap.insert(std::make_pair(copy, idx));
    return idx;
  }

  const char *getCanonical(std::string_view str) {
    return reinterpret_cast<char *>(strtab.resolve(getCanonicalIdx(str)));
  }

  const char *get(uint32_t idx) {
    return reinterpret_cast<char *>(strtab.resolve(idx));
  }
};

template <typename InstrT> class SourceLocInfo {
  DedupeMap<DebugSourceLocImpl, SlabAllocator<DebugSourceLocImpl>,
            DebugSourceLocImpl::hash>
      srcLocDedupe;
  ObjMapVec<Instr, SmallVec<uint32_t, 1>> instrMap;
  StringDedupeMap strDedupe;

public:
  void addSrcLoc(ObjRef<InstrT> instr, std::string_view name,
                 uint32_t beginLine, uint32_t beginCol, uint32_t endLine,
                 uint32_t endCol) {

    DebugSourceLocImpl loc{strDedupe.getCanonicalIdx(name), beginLine, beginCol,
                           endLine, endCol};
    uint32_t idx = srcLocDedupe.getCanonicalIndex(loc);
    instrMap.get_ensure(instr).emplace_back(idx);
  }

  const char *getStringVal(uint32_t i) { return strDedupe.get(i); }

  auto getSourceLocs(ObjRef<InstrT> instr) {
    auto lambda = [this](size_t, uint32_t idx) {
      auto impl = srcLocDedupe.container[idx];
      return DebugSourceLoc{getStringVal(impl.fileName), impl.beginLine,
                            impl.beginCol, impl.endLine, impl.endCol};
    };
    if (!instrMap.inRange(instr))
      return Range{(unsigned int *)nullptr, (unsigned int *)nullptr}.transform(
          lambda);
    return Range{instrMap[instr]}.transform(lambda);
  }

  void resetDebugInfo(ObjRef<InstrT> instr) {
    if (instrMap.inRange(instr))
      instrMap[instr].clear();
  }

  void copyDebugInfo(ObjRef<InstrT> src, ObjRef<InstrT> dst) {
    if (!instrMap.inRange(src) || instrMap[src].empty())
      return;
    auto &vec = instrMap.get_ensure(dst);
    // todo: better data structure
    for (auto info : instrMap[src]) {
      if (std::find(vec.begin(), vec.end(), info) != vec.end())
        continue;
      vec.emplace_back(info);
    }
  }
};

template <typename ValueObj> class ValueNameInfo {
  StringDedupeMap strDedupe;
  ObjMapVec<ValueObj, SmallVec<uint32_t, 1>> valueMap;

  using RefT = ObjRef<Register>;

public:
  void addName(RefT ref, std::string_view name) {
    auto nameIdx = strDedupe.getCanonicalIdx(name);
    valueMap.get_ensure(ref).emplace_back(nameIdx);
  }

  auto getNames(RefT ref) {
    return Range{valueMap.get_ensure(ref)}.transform(
        [&](size_t, uint32_t idx) { return strDedupe.get(idx); });
  }

  auto clearNames(RefT ref) { return valueMap.get_ensure(ref).clear(); }
};

}; // namespace dyno

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

struct DebugName {
  uint32_t name;
  uint32_t srcOffs;
  uint32_t dstOffs;
  uint32_t len;
};

struct ValueDebugInfo {
  SmallVec<DebugName, 1> names;
};

class DebugInfo {
  DedupeMap<DebugSourceLocImpl, SlabAllocator<DebugSourceLocImpl>,
            DebugSourceLocImpl::hash>
      srcLocDedupe;
  ObjMapVec<Instr, SmallVec<uint32_t, 1>> instrMap;

  std::unordered_map<std::string_view, uint32_t> stringMap;
  std::vector<char> strtab;

  uint32_t addToStrtab(std::string_view str) {
    if (auto it = stringMap.find(str); it != stringMap.end()) {
      return it->second;
    }

    uint32_t pos = strtab.size();
    strtab.resize(pos + str.size() + 1);
    std::copy(str.begin(), str.end(), strtab.begin() + pos);
    strtab.back() = 0;
    return pos;
  }

public:
  void addSrcLoc(ObjRef<Instr> instr, std::string_view name, uint32_t beginLine,
                 uint32_t beginCol, uint32_t endLine, uint32_t endCol) {

    DebugSourceLocImpl loc{addToStrtab(name), beginLine, beginCol, endLine,
                           endCol};
    uint32_t idx = srcLocDedupe.getCanonicalIndex(loc);
    instrMap.get_ensure(instr).emplace_back(idx);
  }

  const char *getStringVal(uint32_t i) { return &strtab[i]; }

  auto getSourceLocs(ObjRef<Instr> instr) {
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

  void resetDebugInfo(ObjRef<Instr> instr) {
    if (instrMap.inRange(instr))
      instrMap[instr].clear();
  }

  void copyDebugInfo(ObjRef<Instr> src, ObjRef<Instr> dst) {
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
}; // namespace dyno

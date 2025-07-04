#pragma once

#include "dyno/Instr.h"
#include "dyno/ObjMap.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include <cstdint>
#include <unordered_map>
namespace dyno {

struct DebugSourceLoc {
  uint32_t fileName;
  uint32_t beginLine;
  uint32_t beginCol;
  uint32_t endLine;
  uint32_t endCol;
};

struct DebugName {
  const char *name;
};

struct InstrDebugInfo {
  SmallVec<DebugSourceLoc, 1> sourceLocs;
};

struct ValueDebugInfo {
  SmallVec<DebugName, 1> names;
};

class DebugInfo {
  // todo: dedupe
  ObjMapVec<Instr, InstrDebugInfo> instrTable;
  ObjMapVec<Wire, InstrDebugInfo> wireTable;
  ObjMapVec<Register, InstrDebugInfo> regTable;

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
    instrTable.get_ensure(instr).sourceLocs.emplace_back(
        addToStrtab(name), beginLine, beginCol, endLine, endCol);
  }

  const char *getStringName(uint32_t i) { return &strtab[i]; }

  InstrDebugInfo *get(ObjRef<Instr> instr) {
    if (!instrTable.inRange(instr))
      return nullptr;
    return &instrTable[instr];
  }

  void resetDebugInfo(ObjRef<Instr> instr) {
    if (auto ptr = get(instr))
      *ptr = InstrDebugInfo{};
  }

  void copyDebugInfo(ObjRef<Instr> src, ObjRef<Instr> dst) {
    auto srcInfo = get(src);
    if (!srcInfo || srcInfo->sourceLocs.empty())
      return;
    instrTable.get_ensure(dst).sourceLocs.push_back_range(
        Range{srcInfo->sourceLocs});
  }
};
}; // namespace dyno

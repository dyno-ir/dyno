#pragma once

#include "hw/HWAbstraction.h"
#include "support/Bits.h"
#include "support/ErrorRecovery.h"
#include <cstdint>
#include <utility>

namespace dyno {

template <typename Fragment, size_t NumInline = 1> struct RegisterFrags {

  SmallVec<Fragment, NumInline> frags;

  auto getInsertItLin(uint32_t dstAddr) {
    auto it = frags.begin();
    while (it != frags.end() && it->dstAddr <= dstAddr)
      it++;
    if (it == frags.begin())
      return it;
    return it - 1;
  }

  auto getInsertItBin(uint32_t dstAddr) {
    auto it = std::upper_bound(
        frags.begin(), frags.end(), dstAddr,
        [](uint32_t value, const auto &frag) { return value < frag.dstAddr; });
    if (it == frags.begin())
      return it;
    return std::prev(it);
  }

  // returns last iterator before regions with higher start addr.
  auto getInsertIt(uint32_t dstAddr) {
    if (frags.size() <= 32) [[likely]] {
      auto rv = getInsertItLin(dstAddr);
      assert(getInsertItBin(dstAddr) == rv);
      return rv;
    }
    auto rv = getInsertItBin(dstAddr);
    assert(getInsertItLin(dstAddr) == rv);
    return rv;
  }
};

struct RegisterValueFragment {
  DynObjRef ref;
  uint32_t srcAddr;
  uint32_t dstAddr;
  uint32_t len;
  bool untouched;
  Optional<uint16_t> triggerID;

  std::pair<HWValue, BitRange> getValue(HWInstrBuilder &build, uint32_t addr,
                                        uint32_t len) {
    addr += srcAddr;
    if (auto regThin = ref.dyn_as<ObjRef<Register>>()) {
      auto reg = RegisterRef{build.ctx.getRegs().resolve(regThin)};
      return std::make_pair(build.buildLoad(reg, BitRange{addr, len}),
                            BitRange{0, len});
    }
    if (auto wireThin = ref.dyn_as<ObjRef<Wire>>()) {
      auto wire = WireRef{build.ctx.getWires().resolve(wireThin)};
      return std::make_pair(wire, BitRange{addr, len});
    }
    if (ref.is<ObjRef<Constant>>()) {
      auto constant = ref.isCustom()
                          ? ConstantRef{ref}
                          : ConstantRef{build.ctx.getConstants().resolve(
                                ref.as<ObjRef<Constant>>())};
      return std::make_pair(constant, BitRange{addr, len});
    }
    dyno_unreachable("unsupported");
    // auto store =
    // StoreIRef{build.ctx.getInstrs().resolve(ref.as<ObjRef<Instr>>())};
    // assert(len <= this->len);
    // return std::make_pair(store.value(), BitRange{addr, len});
  }
};

struct RegisterValue : public RegisterFrags<RegisterValueFragment> {
  using Fragment = RegisterValueFragment;

  // sorted by dst addr
  uint32_t depth;
  bool untouched = false;

  RegisterValue() = default;
  RegisterValue(const RegisterValue &) = default;
  RegisterValue(RegisterValue &&) = default;
  RegisterValue &operator=(const RegisterValue &) = default;
  RegisterValue &operator=(RegisterValue &&) = default;

  RegisterValue(RegisterRef reg, uint32_t depth, bool untouched,
                Optional<uint16_t> triggerID)
      : RegisterFrags(
            {Fragment{reg, 0, 0, *reg->numBits, untouched, triggerID}}),
        depth(depth), untouched(untouched) {}
  RegisterValue(DynObjRef value, uint32_t bits, uint32_t depth, bool untouched,
                Optional<uint16_t> triggerID)
      : RegisterFrags({Fragment{value, 0, 0, bits, untouched, triggerID}}),
        depth(depth), untouched(untouched) {}

  uint32_t getLen() const {
    uint32_t len = 0;
    for (auto frag : frags)
      len += frag.len;
    return len;
  }

  void overwrite(DynObjRef ref, uint32_t srcAddr, uint32_t dstAddr,
                 uint32_t len, bool remainUntouched = false,
                 Optional<uint16_t> triggerID = nullopt) {
    untouched &= remainUntouched;
    uint32_t newStart = dstAddr;
    uint32_t newEnd = dstAddr + len;

    auto startLen = getLen();

    auto it = getInsertIt(newStart);
    auto insertPos = it - frags.begin();

    // fixme: not a fan of having this here. maybe register an error handler
    // in the passes using this to print debug info (we don't know the reg
    // here).
    auto checkTriggerCompat = [triggerID](RegisterValueFragment *it) {
      if (triggerID && it->triggerID && (it->triggerID != triggerID))
        report_fatal_error("overlapping register regions assigned with "
                           "different event triggers");
    };

    // adjust or remove overlapping existing fragments
    while (it != frags.end()) {
      uint32_t start = it->dstAddr;
      uint32_t end = it->dstAddr + it->len;

      if (start >= newEnd)
        break;

      assert(end > newStart);
      checkTriggerCompat(it);

      // full cover: split existing into two
      if (newStart > start && newEnd < end) {
        uint32_t leftLen = newStart - start;
        uint32_t rightLen = end - newEnd;
        // create right
        Fragment right{it->ref,       it->srcAddr + (newEnd - start),
                       newEnd,        rightLen,
                       it->untouched, it->triggerID};
        // shrink left
        it->len = leftLen;
        frags.insert(it + 1, right);
        ++insertPos;
        break;
      }

      // overlap on left: trim right side of existing
      if (start < newStart && end > newStart) {
        it->len = newStart - start;
        ++it;
        ++insertPos;
        continue;
      }

      // overlap on right: trim left side of existing
      if (start < newEnd && end > newEnd) {
        uint32_t cut = newEnd - start;
        it->srcAddr += cut;
        it->dstAddr = newEnd;
        it->len = end - newEnd;
        ++it;
        continue;
      }

      // else: existing perfectly matches new
      it = frags.erase(it);
    }

    // insert new fragment in sorted order
    frags.insert(
        frags.begin() + insertPos,
        Fragment{ref, srcAddr, newStart, len, remainUntouched, triggerID});

    auto endLen = getLen();
    assert(startLen == endLen);
  }

  void appendTop(DynObjRef val, uint32_t srcAddr, uint32_t len,
                 bool untouched = false,
                 Optional<uint16_t> triggerID = nullopt) {
    frags.emplace_back(val, srcAddr, frags.back().dstAddr + frags.back().len,
                       len, untouched, triggerID);
  }

  void appendTop(const RegisterValue &other) {
    auto len = frags.back().dstAddr + frags.back().len;
    for (auto frag : other.frags) {
      frag.dstAddr += len;
      frags.emplace_back(frag);
    }
  }

  RegisterValue getRange(uint32_t addr, uint32_t len) {
    auto it = getInsertIt(addr);

    uint32_t end = addr + len;

    RegisterValue range;

    bool allUntouched = true;

    while (it != frags.end() && len != 0) {
      uint32_t itEnd = it->dstAddr + it->len;

      if (addr < itEnd && it->dstAddr < end) {

        uint32_t start = addr - it->dstAddr;
        if (it->dstAddr > addr)
          start = 0;

        uint32_t pieceLen = std::min(end, itEnd) - std::max(addr, it->dstAddr);
        auto frag = *it;
        frag.dstAddr = start;
        frag.len = pieceLen;
        range.frags.emplace_back(frag);
        allUntouched &= it->untouched;

        addr += pieceLen;
        len -= pieceLen;
        it++;
      } else
        break;
    }

    range.untouched = allUntouched;
    return range;
  }

  HWValue get(HWInstrBuilder &build, uint32_t addr, uint32_t len,
              bool update = true) {
    auto it = getInsertIt(addr);

    auto addrB = addr;
    auto lenB = len;

    uint32_t end = addr + len;

    SmallVec<std::pair<HWValue, BitRange>, 4> operands;

    bool allUntouched = true;

    while (it != frags.end() && len != 0) {
      uint32_t itEnd = it->dstAddr + it->len;

      if (addr < itEnd && it->dstAddr < end) {

        uint32_t start = addr - it->dstAddr;
        if (it->dstAddr > addr)
          start = 0;

        uint32_t pieceLen = std::min(end, itEnd) - std::max(addr, it->dstAddr);
        operands.emplace_back(it->getValue(build, start, pieceLen));
        allUntouched &= it->untouched;

        addr += pieceLen;
        len -= pieceLen;
        it++;
      } else
        break;
    }

    auto rv = build.buildSplice(ArrayRef{operands});
    if (update)
      overwrite(rv, 0, addrB, lenB, allUntouched);
    return rv;
  }

  HWValue get(HWInstrBuilder &build, bool update = true) {
    SmallVec<std::pair<HWValue, BitRange>, 4> operands;
    uint32_t len = 0;
    bool allUntouched = true;
    for (auto frag : frags) {
      operands.emplace_back(frag.getValue(build, 0, frag.len));
      len += frag.len;
      allUntouched &= frag.untouched;
    }
    assert(allUntouched == untouched);

    auto rv = build.buildSplice(ArrayRef{operands});
    if (update)
      overwrite(rv, 0, 0, len, allUntouched);
    return rv;
  }

  void overwriteNoMaterialize(RegisterValue &src, Fragment *frag,
                              uint32_t srcAddr, uint32_t dstAddr, uint32_t len,
                              bool remainUntouched = false,
                              Optional<uint16_t> triggerID = nullopt) {
    auto it = frag;
    auto itO = src.getInsertIt(srcAddr);
    // todo: normalize
    while (len != 0) {
      auto thisRemLen = it->len - (dstAddr - it->dstAddr);
      auto otherRemLen = itO->len - (dstAddr - itO->dstAddr);
      auto pieceLen = std::min(std::min(thisRemLen, otherRemLen), len);

      // other frag limiting -> split ours in half
      if (pieceLen != thisRemLen) {
        it = frags.insert(it, *it);
        it->len = pieceLen;

        (it + 1)->srcAddr += pieceLen;
        (it + 1)->dstAddr += pieceLen;
        (it + 1)->len -= pieceLen;
      }

      // this frag is limiting (or perfect match) -> just completely overwrite
      it->ref = itO->ref;
      it->srcAddr = itO->srcAddr + (dstAddr - itO->dstAddr);
      it->untouched = remainUntouched;
      it->triggerID = triggerID;
      // it->len = it->len; unchanged
      // it->dstAddr = it->dstAddr; unchanged

      srcAddr += pieceLen;
      dstAddr += pieceLen;
      len -= pieceLen;

      ++it;
      // if perfect match
      if (pieceLen == otherRemLen)
        ++itO;
    }
  }

  void overwriteNoMaterialize(RegisterValue &src, uint32_t srcAddr,
                              uint32_t dstAddr, uint32_t len) {
    auto it = src.getInsertIt(dstAddr);
    overwriteNoMaterialize(src, it, srcAddr, dstAddr, len);
  }

  void overwriteNoMaterialize(RegisterValue &src, Fragment *toOverwrite) {
    uint32_t srcAddr = toOverwrite->dstAddr;
    uint32_t dstAddr = toOverwrite->dstAddr;
    uint32_t len = toOverwrite->len;
    overwriteNoMaterialize(src, toOverwrite, srcAddr, dstAddr, len);
  }

  // void replaceDefault(HWInstrBuilder &build, RegisterValue &newDefault) {
  //   for (auto frag : frags) {
  //     if (frag.ref.is<RegisterRef>()) {
  //       // todo: don't always materialize value. can just set ref in many
  //       // cases.
  //       overwrite(newDefault.get(build, frag.dstAddr, frag.len), 0,
  //                 frag.dstAddr, frag.len);
  //     }
  //   }
  // }

  friend bool operator==(const RegisterValue &lhs, const RegisterValue &rhs) {
    if (lhs.frags.size() != rhs.frags.size())
      return false;
    for (size_t i = 0; i < lhs.frags.size(); i++)
      if (lhs.frags[i].ref != rhs.frags[i].ref ||
          lhs.frags[i].srcAddr != rhs.frags[i].srcAddr ||
          lhs.frags[i].dstAddr != rhs.frags[i].dstAddr ||
          lhs.frags[i].len != rhs.frags[i].len)
        return false;
    return true;
  }
};

struct RegValueDiff {
private:
  uint32_t addr_f;
  uint32_t len_f;
  bool untouched_f;
  Optional<uint16_t> triggerID_f;

public:
  using AddrField = BitField<uint32_t, 31, 0>;
  using UntouchedField = BitField<uint32_t, 1, 31>;
  uint32_t &addr() { return addr_f; }
  auto &len() { return len_f; }
  auto &untouched() { return untouched_f; }
  auto &triggerID() { return triggerID_f; }

  RegValueDiff(uint32_t addr_f, uint32_t len_f, bool untouched_f,
               Optional<uint16_t> triggerID_f)
      : addr_f(addr_f), len_f(len_f), untouched_f(untouched_f),
        triggerID_f(triggerID_f) {}
  RegValueDiff() = default;

  auto pair() { return std::make_pair(addr(), len()); }
};

inline auto diffRegisterValues(ArrayRef<RegisterValue *> regVals) {
  SmallVec<RegValueDiff, 4> diffs;

  SmallVec<uint32_t, 4> idxs(regVals.size());

  uint32_t curAddr = 0;

  while (idxs[0] < regVals[0]->frags.size()) {
    uint32_t overlapEnd = UINT32_MAX;
    bool equalChunk = true;

    uint32_t sa;
    DynObjRef ref;
    bool untouched;
    Optional<uint16_t> triggerID;

    bool anyTouched = false;
    Optional<uint16_t> anyTriggerID = nullopt;

    for (size_t i = 0; i < regVals.size(); i++) {
      auto &frag = regVals[i]->frags[idxs[i]];
      uint32_t end = frag.dstAddr + frag.len;
      overlapEnd = std::min(overlapEnd, end);

      uint32_t sa2 = frag.srcAddr + (curAddr - frag.dstAddr);
      DynObjRef ref2 = frag.ref;
      bool untouched2 = frag.untouched;
      auto triggerID2 = frag.triggerID;
      if (i == 0) {
        sa = sa2;
        ref = ref2;
        untouched = untouched2;
        triggerID = triggerID2;
      } else {
        equalChunk &= (ref2 == ref) && (sa2 == sa) &&
                      (untouched2 == untouched) && (triggerID == triggerID2);
      }
      anyTouched |= !untouched2;

      if (triggerID2) {
        if (anyTriggerID && triggerID2 != anyTriggerID)
          report_fatal_error("overlapping register regions assigned with "
                             "different event triggers");

        anyTriggerID = triggerID2;
      }
    }
    uint32_t chunkLen = overlapEnd - curAddr;

    if (!equalChunk) {
      // either start a new diff run or extend the last one
      if (!diffs.empty() &&
          diffs.back().addr() + diffs.back().len() == curAddr &&
          diffs.back().untouched() == !anyTouched &&
          diffs.back().triggerID() == anyTriggerID) {
        diffs.back().len() += chunkLen;
      } else
        diffs.emplace_back(curAddr, chunkLen, !anyTouched, anyTriggerID);
    }

    curAddr = overlapEnd;

    // advance fragment indices when we hit their end
    for (size_t i = 0; i < regVals.size(); i++) {
      auto frag = regVals[i]->frags[idxs[i]];
      uint32_t end = frag.dstAddr + frag.len;
      if (curAddr == end)
        idxs[i]++;
    }
  }

  return diffs;
}

struct RegisterRegionsFragment {
  SmallVec<uint32_t, 1> writerIDs;
  uint32_t dstAddr;
  uint32_t len;
};

struct RegisterRegions : public RegisterFrags<RegisterRegionsFragment> {
  using Fragment = RegisterRegionsFragment;

  void addRegion(uint32_t writerID, uint32_t dstAddr, uint32_t len) {

    auto it = getInsertIt(dstAddr);

    auto isContained = [](RegisterRegionsFragment *it, uint32_t id) {
      return std::find(it->writerIDs.begin(), it->writerIDs.end(), id) !=
             it->writerIDs.end();
    };

    auto addIfNotContained = [](RegisterRegionsFragment *it, uint32_t id) {
      if (std::find(it->writerIDs.begin(), it->writerIDs.end(), id) ==
          it->writerIDs.end()) {
        it->writerIDs.emplace_back(id);
      }
    };

    // it starts earlier
    if (it->dstAddr < dstAddr) {
      if (isContained(it, writerID)) {

        if (it->dstAddr + it->len >= dstAddr + len)
          return;
        dstAddr = it->dstAddr + it->len;
        len -= it->len;

        ++it;
      } else {
        auto pieceLen = dstAddr - it->dstAddr;
        Fragment frag{it->writerIDs, it->dstAddr, pieceLen};
        it = frags.insert(it, frag) + 1;

        it->len -= pieceLen;
        it->dstAddr += pieceLen;
      }
    }

    while (len != 0) {

      uint32_t end = dstAddr + len;
      uint32_t itEnd = it->dstAddr + it->len;

      // it ends later
      if (itEnd > end) {
        if (isContained(it, writerID))
          break;
        Fragment frag{it->writerIDs, it->dstAddr, len};
        it->len = itEnd - end;
        it->dstAddr += len;
        it = frags.insert(it, frag);
        addIfNotContained(it, writerID);
        break;
      }

      addIfNotContained(it, writerID);

      len -= it->len;
      dstAddr += it->len;
      ++it;
    }
  }

  auto getAccessors(uint32_t addr, uint32_t len) {
    SmallDenseSet<uint32_t, 1> writers;
    uint32_t end = addr + len;

    auto it = getInsertIt(addr);
    while (it != frags.end() && len != 0) {
      uint32_t itEnd = it->dstAddr + it->len;

      if (addr < itEnd && it->dstAddr < end) {
        uint32_t pieceLen = std::min(end, itEnd) - std::max(addr, it->dstAddr);

        writers.findOrInsert(Range{it->writerIDs});

        addr += pieceLen;
        len -= pieceLen;
        it++;
      } else
        break;
    }

    return writers;
  }

  explicit RegisterRegions(uint32_t len) : RegisterFrags({{{}, 0, len}}) {}
};

}; // namespace dyno

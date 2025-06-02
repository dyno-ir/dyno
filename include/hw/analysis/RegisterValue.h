#pragma once

#include "hw/HWAbstraction.h"
#include <cstdint>
#include <utility>

namespace dyno {

struct RegisterValue {
  struct Fragment {
    DynObjRef ref;
    uint32_t srcAddr;
    uint32_t dstAddr;
    uint32_t len;

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
  // sorted by dst addr
  SmallVec<Fragment, 1> frags;
  uint32_t depth;
  bool untouched;

  RegisterValue() = default;
  RegisterValue(const RegisterValue &) = default;
  RegisterValue(RegisterValue &&) = default;
  RegisterValue &operator=(const RegisterValue &) = default;
  RegisterValue &operator=(RegisterValue &&) = default;

  RegisterValue(RegisterRef reg, uint32_t depth)
      : frags{{reg, 0, 0, reg->numBits}}, depth(depth) {}
  RegisterValue(DynObjRef value, uint32_t bits, uint32_t depth)
      : frags{{value, 0, 0, bits}}, depth(depth) {}

  // returns last iterator before regions with higher start addr.
  auto getInsertItLin(uint32_t dstAddr) {
    // todo: binary search when large
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

  void overwrite(DynObjRef ref, uint32_t srcAddr, uint32_t dstAddr,
                 uint32_t len) {
    untouched = false;
    uint32_t newStart = dstAddr;
    uint32_t newEnd = dstAddr + len;

    auto it = getInsertIt(newStart);
    auto insertPos = it - frags.begin();

    // adjust or remove overlapping existing fragments
    while (it != frags.end()) {
      uint32_t start = it->dstAddr;
      uint32_t end = it->dstAddr + it->len;

      if (start >= newEnd)
        break;

      if (end <= newStart) {
        ++it;
        assert(false);
        continue;
      }

      // full cover: split existing into two
      if (newStart > start && newEnd < end) {
        uint32_t leftLen = newStart - start;
        uint32_t rightLen = end - newEnd;
        // create right
        Fragment right{it->ref, it->srcAddr + (newEnd - start), newEnd,
                       rightLen};
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
      *it = Fragment{
          ref,
          srcAddr,
          newStart,
          len,
      };
      return;
    }

    // insert new fragment in sorted order
    frags.insert(frags.begin() + insertPos,
                 Fragment{ref, srcAddr, newStart, len});
  }

  HWValue get(HWInstrBuilder &build, uint32_t addr, uint32_t len,
              bool update = true) {
    auto it = getInsertIt(addr);

    auto addrB = addr;
    auto lenB = len;

    uint32_t end = addr + len;

    SmallVec<std::pair<HWValue, BitRange>, 4> operands;

    while (it != frags.end() && len != 0) {
      uint32_t itEnd = it->dstAddr + it->len;

      if (addr < itEnd && it->dstAddr < end) {

        uint32_t start = addr - it->dstAddr;
        if (it->dstAddr > addr)
          start = 0;

        uint32_t pieceLen = std::min(end, itEnd) - std::max(addr, it->dstAddr);
        operands.emplace_back(it->getValue(build, start, pieceLen));

        addr += pieceLen;
        len -= pieceLen;
        it++;
      } else
        break;
    }

    auto rv = build.buildSplice(ArrayRef{operands});
    if (update)
      overwrite(rv, 0, addrB, lenB);
    return rv;
  }

  HWValue get(HWInstrBuilder &build, bool update = true) {
    SmallVec<std::pair<HWValue, BitRange>, 4> operands;
    uint32_t len = 0;
    for (auto frag : frags) {
      operands.emplace_back(frag.getValue(build, 0, frag.len));
      len += frag.len;
    }

    auto rv = build.buildSplice(ArrayRef{operands});
    if (update)
      overwrite(rv, 0, 0, len);
    return rv;
  }

  void overwriteNoMaterialize(RegisterValue &src, Fragment *frag,
                              uint32_t srcAddr, uint32_t dstAddr,
                              uint32_t len) {
    auto it = frag;
    auto itO = src.getInsertIt(srcAddr);
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

auto diffRegisterValues(ArrayRef<RegisterValue *> regVals) {
  SmallVec<std::pair<uint32_t, uint32_t>, 4> diffs;

  SmallVec<uint32_t, 4> idxs(regVals.size());

  uint32_t curAddr = 0;

  while (idxs[0] < regVals[0]->frags.size()) {
    uint32_t overlapEnd = UINT32_MAX;
    bool equalChunk = true;

    uint32_t sa;
    DynObjRef ref;

    for (size_t i = 0; i < regVals.size(); i++) {
      auto &frag = regVals[i]->frags[idxs[i]];
      uint32_t end = frag.dstAddr + frag.len;
      overlapEnd = std::min(overlapEnd, end);

      uint32_t sa2 = frag.srcAddr + (curAddr - frag.dstAddr);
      DynObjRef ref2 = frag.ref;
      if (i == 0) {
        sa = sa2;
        ref = ref2;
      } else
        equalChunk &= (ref == ref2) && (sa2 == sa);
    }
    uint32_t chunkLen = overlapEnd - curAddr;

    if (!equalChunk) {
      // either start a new diff run or extend the last one
      if (!diffs.empty() &&
          diffs.back().first + diffs.back().second == curAddr) {
        diffs.back().second += chunkLen;
      } else
        diffs.emplace_back(curAddr, chunkLen);
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

}; // namespace dyno

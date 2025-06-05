#pragma once

#include "Bits.h"
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

template <typename Container, Container::value_type DefaultWord = 0>
class UnsizedBitSet {
  Container storage;
  using word_t = Container::value_type;
  static constexpr size_t WordBits = bit_mask_sz<word_t>;
  static constexpr size_t wordIdx(size_t i) { return i / WordBits; }
  static constexpr size_t bitIdx(size_t i) { return i % WordBits; }
  static constexpr word_t bitMsk(size_t i) { return word_t(1) << bitIdx(i); }

public:
  UnsizedBitSet(const UnsizedBitSet &) = default;
  UnsizedBitSet(UnsizedBitSet &&) = default;
  UnsizedBitSet &operator=(const UnsizedBitSet &) = default;
  UnsizedBitSet &operator=(UnsizedBitSet &&) = default;

  UnsizedBitSet() = default;
  UnsizedBitSet(size_t preallocBits)
      : storage(round_up_div(preallocBits, WordBits)) {}

  void set(size_t i) { storage[wordIdx(i)] |= bitMsk(i); }
  void clear(size_t i) { storage[wordIdx(i)] &= ~bitMsk(i); }
  bool get(size_t i) const { storage[wordIdx(i)] & bitMsk(i); }

  void modifyRange(size_t i, size_t len, word_t value) {
    if (len == 0) [[unlikely]]
      return;

    uint64_t lowerIdx = i / WordBits;
    uint64_t upperIdx = (i + len - 1) / WordBits;

    uint64_t lowerOffs = (i % WordBits);
    uint64_t upperOffs = ((i + len - 1) % WordBits);

    if (lowerIdx == upperIdx) {
      word_t mask =
          bit_mask_ones<word_t>((upperOffs + 1) - lowerOffs, lowerOffs);
      storage[lowerIdx] &= ~mask;
      storage[lowerIdx] |= mask & value;
      return;
    }

    storage[lowerIdx] &= bit_mask_ones<word_t>(lowerOffs);
    storage[lowerIdx] |= bit_mask_zeros<word_t>(lowerOffs) & value;

    for (size_t i = lowerIdx + 1; i != upperIdx; i++)
      storage[i] = value;

    storage[upperIdx] &= bit_mask_zeros<word_t>(upperOffs + 1);
    storage[upperIdx] |= bit_mask_ones<word_t>(upperOffs + 1) & value;
  }
  void setRange(size_t i, size_t len) { modifyRange(i, len, ~word_t(0)); }
  void clearRange(size_t i, size_t len) { modifyRange(i, len, word_t(0)); }

  void ensureBits(size_t i) { ensureWords(round_up_div(i, WordBits)); }
  void ensureWords(size_t words) {
    if (storage.size() <= words) [[unlikely]]
      storage.resize(words, DefaultWord);
  }
  void resizeBits(size_t i) { resizeWords(round_up_div(i, WordBits)); }
  void resizeWords(size_t words) { storage.resize(words, DefaultWord); }
  void setDyn(size_t i) {
    ensureBits(i + 1);
    set(i);
  }
  void clearDyn(size_t i) {
    ensureBits(i + 1);
    clear(i);
  }
  bool getDyn(size_t i) const {
    auto word = wordIdx(i);
    if (word >= storage.size())
      return DefaultWord & bitMsk(i);
    return storage[word] & bitMsk(i);
  }
  void modifyRangeDyn(size_t i, size_t len, word_t value) {
    ensureBits(i + len);
    modifyRange(i, len, value);
  }
  void setRangeDyn(size_t i, size_t len) { modifyRangeDyn(i, len, ~word_t(0)); }
  void clearRangeDyn(size_t i, size_t len) {
    modifyRangeDyn(i, len, word_t(0));
  }

  word_t getWordDyn(size_t i) const {
    if (i >= storage.size())
      return DefaultWord;
    return storage[i];
  }

  word_t numWords() const { return storage.size(); }

  friend bool operator==(const UnsizedBitSet &lhs, const UnsizedBitSet &rhs) {
    if (lhs.numWords() != rhs.numWords()) {
      const UnsizedBitSet *longer =
          lhs.numWords() > rhs.numWords() ? &lhs : &rhs;
      size_t min = std::min(lhs.numWords(), rhs.numWords());
      size_t max = std::max(lhs.numWords(), rhs.numWords());
      for (size_t i = min; i < max; i++)
        if (longer->storage[i] != DefaultWord)
          return false;
    }

    for (size_t i = 0; i < lhs.numWords(); i++)
      if (lhs.storage[i] != rhs.storage[i])
        return false;

    return true;
  }
  UnsizedBitSet &operator|=(const UnsizedBitSet &o) {
    ensureWords(o.storage.size());
    for (size_t i = 0; i < storage.size(); ++i) {
      storage[i] |= o.getWordDyn(i);
    }
    return *this;
  }
  UnsizedBitSet &operator&=(const UnsizedBitSet &o) {
    ensureWords(o.storage.size());
    for (size_t i = 0; i < storage.size(); ++i) {
      storage[i] &= o.getWordDyn(i);
    }
    return *this;
  }
  UnsizedBitSet &nand(const UnsizedBitSet &o) {
    ensureWords(o.storage.size());
    for (size_t i = 0; i < storage.size(); ++i) {
      storage[i] &= o.getWordDyn(i);
    }
    return *this;
  }
  UnsizedBitSet &nor(const UnsizedBitSet &o) {
    ensureWords(o.storage.size());
    for (size_t i = 0; i < storage.size(); ++i) {
      storage[i] |= o.getWordDyn(i);
    }
    return *this;
  }

  size_t count() const {
    assert(DefaultWord == 0 && "not well defined");
    size_t cnt = 0;
    for (size_t i = 0; i < storage.size(); ++i) {
      cnt += std::popcount(storage[i]);
    }
    return cnt;
  }
};

class DynBitSet {
public:
  using word_t = uint64_t;
  static constexpr size_t wordBits = 8 * sizeof(word_t);

  static constexpr size_t numWordsForBits(size_t numBits) {
    return (numBits + wordBits - 1) / wordBits;
  }

  static constexpr size_t wordIdx(size_t i) { return i / wordBits; }
  static constexpr size_t bitIdx(size_t i) { return i % wordBits; }
  static constexpr word_t bitMsk(size_t i) { return word_t(1) << bitIdx(i); }

  DynBitSet() : numBits(0) {}

  DynBitSet(size_t numBits)
      : numBits(numBits), words(numWordsForBits(numBits)) {}

  size_t size() const { return numBits; }

  size_t count() const {
    size_t cnt = 0;
    for (size_t i = 0; i < words.size(); ++i) {
      cnt += std::popcount(words[i]);
    }
    return cnt;
  }

  void set(size_t i) {
    assert(i < numBits);
    words[wordIdx(i)] |= bitMsk(i);
  }
  void clr(size_t i) {
    assert(i < numBits);
    words[wordIdx(i)] &= ~bitMsk(i);
  }
  void set(size_t i, bool v) {
    assert(i < numBits);
    if (v) {
      set(i);
    } else {
      clr(i);
    }
  }

  void clrAll() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = 0;
    }
  }
  void setAll() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = ~word_t(0);
    }
    clrUndefined();
  }

  bool tst(size_t i) const { return words[wordIdx(i)] & bitMsk(i); }

  DynBitSet &operator&=(const DynBitSet &o) {
    assert(isBinopCompatible(o));
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] &= o.words[i];
    }
    return *this;
  }

  DynBitSet &operator|=(const DynBitSet &o) {
    assert(isBinopCompatible(o));
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] |= o.words[i];
    }
    return *this;
  }

  void flip() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = ~words[i];
    }
    clrUndefined();
  }

  template <typename CB> void for_each(CB cb) const {
    for (size_t i = 0; i < numBits; ++i) {
      if (tst(i)) {
        cb(i);
      }
    }
  }

private:
  size_t numBits;
  std::vector<word_t> words;

  bool isBinopCompatible(const DynBitSet &o) const {
    return numBits == o.numBits;
  }

  void clrUndefined() {
    if (numBits % wordBits == 0)
      return;
    words[words.size() - 1] &= bitMsk(numBits) - 1;
  }
};

class TriangularBitSet {
public:
  static constexpr size_t idx(size_t a, size_t b) {
    assert(a != b);
    size_t h, l;
    if (a < b) {
      l = a;
      h = b;
    } else {
      l = b;
      h = a;
    }
    return (h * (h - 1)) / 2 + l;
  }

  TriangularBitSet(size_t sz) : bits(sz != 0 ? idx(sz, 0) : 0) {}

  void set(size_t i, size_t j) { bits.set(idx(i, j)); }
  void clr(size_t i, size_t j) { bits.clr(idx(i, j)); }
  void set(size_t i, size_t j, bool v) {
    if (v) {
      set(i, j);
    } else {
      clr(i, j);
    }
  }

  bool tst(size_t i, size_t j) { return bits.tst(idx(i, j)); }

  DynBitSet bits;
};

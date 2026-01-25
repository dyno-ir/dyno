#pragma once

#include "Bits.h"
#include "support/ArrayRef.h"
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <vector>

template <typename containter, size_t SymbolBits,
          containter::value_type DefaultWord = 0>
class UnsizedSymbSet {
public:
  using Container = containter;

protected:
  Container storage;
  using container_t = Container;
  using word_t = typename Container::value_type;
  using value_type = word_t;
  using reference = DynBitField<word_t>;

  static constexpr size_t WordBits = bit_mask_sz<word_t>;
  static constexpr size_t WordSymbs = WordBits / SymbolBits;

  static constexpr size_t wordIdx(size_t i) { return i / WordSymbs; }
  static constexpr size_t symbIdx(size_t i) { return i % WordSymbs; }
  static constexpr word_t symbMask(size_t i) {
    return bit_mask_ones<word_t>(SymbolBits) << symbIdx(i);
  }

public:
  UnsizedSymbSet(const UnsizedSymbSet &) = default;
  UnsizedSymbSet(UnsizedSymbSet &&) = default;
  UnsizedSymbSet &operator=(const UnsizedSymbSet &) = default;
  UnsizedSymbSet &operator=(UnsizedSymbSet &&) = default;

  UnsizedSymbSet() = default;
  UnsizedSymbSet(size_t preallocSymbs)
      : storage(round_up_div(preallocSymbs, WordSymbs)) {}

  void ensureSymbs(size_t i) { ensureWords(round_up_div(i, WordSymbs)); }
  void ensureSymbsExp(size_t i) { ensureWordsExp(round_up_div(i, WordSymbs)); }

  void ensureWords(size_t words) {
    if (storage.size() <= words) [[unlikely]]
      storage.resize(words, DefaultWord);
  }
  void ensureWordsExp(size_t words) {
    if (storage.size() <= words) [[unlikely]]
      storage.resize(ceil_to_pow2(words), DefaultWord);
  }
  void resizeSymbs(size_t i) { resizeWords(round_up_div(i, WordSymbs)); }
  void resizeWords(size_t words) { storage.resize(words, DefaultWord); }

  DynBitField<word_t> at_unchecked(size_t i) {
    return DynBitField{storage[wordIdx(i)], symbIdx(i) * SymbolBits,
                       SymbolBits};
  }
  const DynBitField<const word_t> at_unchecked(size_t i) const {
    return DynBitField{storage[wordIdx(i)], symbIdx(i) * SymbolBits,
                       SymbolBits};
  }
  auto at(size_t i) {
    ensureSymbsExp(i + 1);
    return DynBitField{storage[wordIdx(i)], symbIdx(i) * SymbolBits,
                       SymbolBits};
  }
  DynBitField<word_t> operator[](size_t i) { return at_unchecked(i); }
  const DynBitField<const word_t> operator[](size_t i) const {
    return at_unchecked(i);
  }

  auto raw() const { return ArrayRef{storage}; }
  auto raw() { return MutArrayRef{storage}; }
};

template <typename Container, size_t SymbolBits,
          Container::value_type DefaultWord = 0>
class DynSymbSet : public UnsizedSymbSet<Container, SymbolBits, DefaultWord> {
  using Base = UnsizedSymbSet<Container, SymbolBits, DefaultWord>;

protected:
  size_t numSymbs = 0;
  using Base::storage;

public:
  using reference = DynBitField<typename Base::container_t::value_type>;
  using value_type = reference;
  using Base::at_unchecked;

  DynSymbSet() = default;
  DynSymbSet(size_t resizeTo) : DynSymbSet() { this->resize(resizeTo); }

  auto size() const { return numSymbs; }
  auto at(size_t i) {
    assert(i < numSymbs && "oob");
    return at_unchecked(i);
  }
  void clear() {
    numSymbs = 0;
    storage.clear();
  }

  void resize(size_t i) {
    numSymbs = i;
    this->resizeSymbs(numSymbs);
  }

  void reserve(size_t i) {
    this->storage.reserve(round_up_div(i, Base::WordSymbs));
  }

  reference front() {
    assert(size() != 0);
    return *begin();
  }

  reference back() {
    assert(size() != 0);
    return end()[-1];
  }

  void push_back(unsigned val) {
    resize(size() + 1);
    back() = val;
  }

  void pop_back() {
    assert(size() != 0);
    resize(size() - 1);
  }

  bool operator==(const DynSymbSet &other) const {
    if (numSymbs != other.numSymbs)
      return false;

    size_t min = std::min(this->storage.size(), other.storage.size());
    for (size_t i = 0; i < min; i++) {
      if (this->raw()[i] != other.raw()[i])
        return false;
    }

    for (size_t i = min; i < this->storage.size(); i++)
      if (this->raw()[i] != DefaultWord)
        return false;

    for (size_t i = min; i < other.storage.size(); i++)
      if (other.raw()[i] != DefaultWord)
        return false;

    return true;
  }

  class iterator {
    Container::value_type *word;
    size_t symb;

  public:
    iterator(Container::value_type *word, size_t symb)
        : word(word), symb(symb) {}
    using iterator_category = std::random_access_iterator_tag;
    using reference = DynBitField<typename Base::Container::value_type>;
    using value_type = reference;
    using difference_type = ptrdiff_t;

    iterator &operator+=(ssize_t val) {
      symb += val;
      auto incr = symb / Base::WordSymbs;
      symb %= Base::WordSymbs;
      word += incr;
      return *this;
    }
    iterator &operator-=(ssize_t val) { *this += -val; }

    iterator operator+(ssize_t val) {
      auto copy{*this};
      copy += val;
      return copy;
    }
    iterator operator-(ssize_t val) { return (*this) + (-val); }

    iterator &operator++() { return *this += 1; }
    iterator &operator--() { return *this -= 1; }
    iterator operator++(int) {
      auto old{*this};
      ++(*this);
      return old;
    }
    iterator operator--(int) {
      auto old{*this};
      --(*this);
      return old;
    }

    reference operator*() {
      return reference{*word, symb * SymbolBits, SymbolBits};
    }
    reference operator[](ssize_t i) { return *((*this) + i); }

    friend difference_type operator-=(iterator lhs, iterator rhs) {
      return (lhs.word - rhs.word) * Base::WordSymbs + (lhs.symb - rhs.symb);
    }
    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.word == rhs.word && lhs.symb == rhs.symb;
    }
  };

  iterator begin() { return iterator{storage.begin(), 0}; }
  iterator end() {
    return iterator{storage.begin() + (numSymbs / Base::WordSymbs),
                    (numSymbs % Base::WordSymbs)};
  }
};

template <typename Container, size_t SymbolBits,
          Container::value_type DefaultWord>
struct std::hash<DynSymbSet<Container, SymbolBits, DefaultWord>> {
  size_t
  operator()(const DynSymbSet<Container, SymbolBits, DefaultWord> &set) const {
    std::hash<typename Container::value_type> hashElem;
    size_t acc = 0;
    for (auto elem : set.raw()) {
      acc = hash_combine64(acc, hashElem(elem));
    }
    acc = hash_combine64(acc, set.size());
    return acc;
  }
};

template <typename Container, Container::value_type DefaultWord = 0>
class UnsizedBitSet : public UnsizedSymbSet<Container, 1, DefaultWord> {
  using Base = UnsizedSymbSet<Container, 1, DefaultWord>;
  using word_t = Container::value_type;

protected:
  using Base::storage;
  using Base::symbIdx;
  using Base::symbMask;
  using Base::WordBits;
  using Base::wordIdx;
  using Base::WordSymbs;

public:
  using Base::ensureWords;
  using Base::resizeWords;
  using typename Base::reference;
  using typename Base::value_type;
  UnsizedBitSet(const UnsizedBitSet &) = default;
  UnsizedBitSet(UnsizedBitSet &&) = default;
  UnsizedBitSet &operator=(const UnsizedBitSet &) = default;
  UnsizedBitSet &operator=(UnsizedBitSet &&) = default;

  UnsizedBitSet() = default;
  UnsizedBitSet(size_t preallocBits) : Base(preallocBits) {}

  void set(size_t i) { storage[wordIdx(i)] |= symbMask(i); }
  void clear(size_t i) { storage[wordIdx(i)] &= ~symbMask(i); }
  bool get(size_t i) const { return storage[wordIdx(i)] & symbMask(i); }

  void clear() { storage.clear(); }

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

  void resizeBits(size_t i) { this->resizeSymbs(i); }
  void ensureBits(size_t i) { this->ensureSymbs(i); }

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
      return DefaultWord & symbMask(i);
    return storage[word] & symbMask(i);
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

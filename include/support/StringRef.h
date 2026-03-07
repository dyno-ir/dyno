#pragma once
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Ranges.h"
#include <cstring>
#include <limits>

template <typename Derived> class StringRefMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &cself() const { return *static_cast<const Derived *>(this); }

public:
  operator ArrayRef<char>() { return {self().data(), self().size()}; }
  operator Range<const char *>() { return {self().begin(), self().end()}; }
  operator std::string_view() { return {self().data(), self().size()}; }

  Derived substr(size_t pos, size_t n) const {
    return Derived{cself().begin() + pos, cself().begin() + pos + n};
  }
  Derived substr(size_t pos) const { return substr(pos, cself().size() - pos); }

  bool deepEquals(const Derived &other) const {
    if (cself().size() != other.size())
      return false;
    return std::memcmp(cself().begin(), other.begin(), cself().size()) == 0;
  }
  friend bool operator==(const Derived &a, const Derived &b) {
    return a.deepEquals(b);
  }
};

class StringRef : public ArrayRef<char>, public StringRefMixin<StringRef> {
public:
  using ArrayRef::ArrayRef;

  template <typename T> StringRef(T &&t) : StringRef(t.begin(), t.end()) {}
  StringRef(const char *data) : StringRef(data, strlen(data)) {}
};

class MutStringRef : public MutArrayRef<char>,
                     public StringRefMixin<StringRef> {
public:
  using MutArrayRef::MutArrayRef;

  template <typename T>
  MutStringRef(T &&t) : MutStringRef(t.begin(), t.end()) {}
  MutStringRef(char *data) : MutStringRef(data, strlen(data)) {}
};

// 4+ GiB string, 8 inline chars
class BigSSOStringRef : public StringRefMixin<BigSSOStringRef> {

public:
  using value_type = char;
  using iterator = char *;
  using pointer = char *;
  using reference = char &;

private:
  uint64_t len;
  union {
    const char *ptr;
    char inl[8];
  };
  constexpr BigSSOStringRef(uint64_t len) : len(len), inl{} {}

public:
  static constexpr BigSSOStringRef getInvalid(uint32_t nth) {
    assert(nth <= 1);
    return BigSSOStringRef(std::numeric_limits<uint64_t>::max() - nth);
  }
  bool isInline() const { return len <= 8; }

  const char *begin() const {
    return __builtin_unpredictable(isInline()) ? &inl[0] : ptr;
  }
  const char *end() const { return begin() + len; }
  size_t size() const { return len; }
  const char *data() const { return begin(); }
  const char &operator[](size_t i) const {
    assert(i < size());
    return begin()[i];
  }

  BigSSOStringRef() = default;
  BigSSOStringRef(const BigSSOStringRef &) = default;
  BigSSOStringRef(BigSSOStringRef &&) = default;
  BigSSOStringRef &operator=(const BigSSOStringRef &) = default;
  BigSSOStringRef &operator=(BigSSOStringRef &&) = default;

  BigSSOStringRef(const char *data, size_t len) : len(len) {
    memcpy(inl, data, std::min(len, 8ZU));
    if (__builtin_unpredictable(len > 8)) {
      this->ptr = data;
    }
  }
  BigSSOStringRef(const char *begin, const char *end)
      : BigSSOStringRef(begin, end - begin) {}

  template <typename T>
  BigSSOStringRef(T &&t) : BigSSOStringRef(t.begin(), t.end()) {}
  BigSSOStringRef(const char *data) : BigSSOStringRef(data, strlen(data)) {}
};

// 4 GiB max string, 12 inline chars.
// Note: This kind of const-style infects other code: if naively
// converted to string_view or StringRef the inline buffer might
// go out of scope, so non-temp downstream uses should use SSOStringRef too.
class SSOStringRef : public StringRefMixin<SSOStringRef> {
public:
  using value_type = char;
  using iterator = char *;
  using pointer = char *;
  using reference = char &;

private:
  uint32_t len;
  char inl[4];
  union {
    const char *ptr;
    char inl2[8];
  };
  constexpr SSOStringRef(uint32_t len) : len(len), inl{}, inl2{} {}

public:
  static constexpr SSOStringRef getInvalid(uint32_t nth) {
    assert(nth <= 1);
    return SSOStringRef(std::numeric_limits<uint32_t>::max() - nth);
  }

  bool isInline() const { return len <= 12; }

  const char *begin() const {
    return __builtin_unpredictable(isInline()) ? &inl[0] : ptr;
  }
  const char *end() const { return begin() + len; }
  uint32_t size() const { return len; }
  const char *data() const { return begin(); }
  const char &operator[](size_t i) const {
    assert(i < size());
    return begin()[i];
  }

  SSOStringRef() = default;
  SSOStringRef(const SSOStringRef &) = default;
  SSOStringRef(SSOStringRef &&) = default;
  SSOStringRef &operator=(const SSOStringRef &) = default;
  SSOStringRef &operator=(SSOStringRef &&) = default;

  SSOStringRef(const char *data, size_t len) : len(len) {
    memcpy(inl, data, std::min(len, 12ZU));
    if (__builtin_unpredictable(len > 12)) {
      this->ptr = data;
    }
  }
  SSOStringRef(const char *begin, const char *end)
      : SSOStringRef(begin, end - begin) {}

  template <typename T>
  SSOStringRef(T &&t) : SSOStringRef(t.begin(), t.end()) {}
  SSOStringRef(const char *data)
      : SSOStringRef(data,
                     strnlen(data, std::numeric_limits<uint32_t>::max())) {}
};

template <> struct DenseMapInfo<BigSSOStringRef> {
  static constexpr BigSSOStringRef getEmptyKey() {
    return BigSSOStringRef::getInvalid(0);
  }
  static constexpr BigSSOStringRef getTombstoneKey() {
    return BigSSOStringRef::getInvalid(1);
  }
  static uint32_t getHashValue(const BigSSOStringRef &s) {
    return strhash_u32(s.data(), s.size());
  }
  static bool isEqual(const BigSSOStringRef &a, const BigSSOStringRef &b) {
    return a.deepEquals(b);
  }
};
template <> struct DenseMapInfo<SSOStringRef> {
  static constexpr SSOStringRef getEmptyKey() {
    return SSOStringRef::getInvalid(0);
  }
  static constexpr SSOStringRef getTombstoneKey() {
    return SSOStringRef::getInvalid(1);
  }
  static uint32_t getHashValue(const SSOStringRef &s) {
    return strhash_u32(s.data(), s.size());
  }
  static bool isEqual(const SSOStringRef &a, const SSOStringRef &b) {
    return a.deepEquals(b);
  }
};

template <> struct std::hash<SSOStringRef> {
  uint32_t operator()(const SSOStringRef &ref) const {
    return strhash_u32(ref.data(), ref.size());
  }
};
template <> struct std::hash<BigSSOStringRef> {
  uint32_t operator()(const BigSSOStringRef &ref) const {
    return strhash_u32(ref.data(), ref.size());
  }
};
template <> struct std::hash<StringRef> {
  uint32_t operator()(const StringRef &ref) const {
    return strhash_u32(ref.data(), ref.size());
  }
};

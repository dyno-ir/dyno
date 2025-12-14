#pragma once

#include <cassert>
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

class Tokenizer {
  std::string_view s, delims;

public:
  constexpr Tokenizer(std::string_view str, std::string_view d = " ")
      : s(str), delims(d) {}

  class iterator {
    Tokenizer *parent;
    size_t pos;
    size_t len;

  public:
    constexpr iterator &operator++() {
      pos += len;
      pos = parent->s.find_first_not_of(parent->delims, pos);
      if (pos == std::string_view::npos) {
        len = 0;
        return *this;
      }
      size_t end = parent->s.find_first_of(parent->delims, pos);
      if (end == std::string_view::npos)
        end = parent->s.size();
      len = end - pos;
      return *this;
    }

    constexpr iterator operator++(int) {
      auto copy{*this};
      ++(*this);
      return copy;
    }

    constexpr std::string_view operator*() { return parent->s.substr(pos, len); }

    constexpr bool operator==(const iterator &other) const {
      auto rv = other.parent == this->parent && other.pos == this->pos;
      assert(!rv || this->len == other.len);
      return rv;
    }

    constexpr iterator(Tokenizer *parent, size_t pos) : parent(parent), pos(pos), len(0) {
      // first increment does not advance as len == 0, just primes.
      ++(*this);
    }
  };

  constexpr iterator begin() { return iterator{this, 0}; }
  constexpr iterator end() { return iterator{this, s.length()}; }
};

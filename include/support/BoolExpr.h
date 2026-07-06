#pragma once
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/DenseMap.h"
#include "support/DenseMapInfo.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include "support/TwoLevelSet.h"
#include "support/Utility.h"
#include "support/Optional.h"
#include <bit>
#include <cstdint>
#include <iterator>
#include <type_traits>

struct SmallBoolExprTerm {
  using term_mask_t = uint64_t;

  term_mask_t pos;
  term_mask_t neg;

  bool isFalse() const { return pos & neg; }
  bool isTrue() const { return pos == 0 && neg == 0; }

  auto literalIdxs() { return IntRange{pos | neg, (pos | neg) + 1}.set_bits(); }
  auto enumerate() {
    return literalIdxs().transform([pos = this->pos](size_t, size_t idx) {
      return std::make_pair(idx, !!(pos & (1 << idx)));
    });
  }

  void set(unsigned idx, bool val) {
    term_mask_t mask = bit_mask_zeros<term_mask_t>(1, idx);
    pos = (pos & mask) | (val << idx);
    neg = (neg & mask) | (!val << idx);
  }
  void inv(unsigned idx) {
    pos ^= 1ull << idx;
    neg ^= 1ull << idx;
    assert(!isFalse());
  }
  void unset(unsigned idx) {
    term_mask_t mask = bit_mask_zeros<term_mask_t>(1, idx);
    pos = (pos & mask);
    neg = (neg & mask);
  }
  unsigned size() const { return std::popcount(pos | neg); }
  friend bool operator==(const SmallBoolExprTerm &lhs,
                         const SmallBoolExprTerm &rhs) {
    return lhs.pos == rhs.pos && lhs.neg == rhs.neg;
  }
  SmallBoolExprTerm with_set(unsigned idx, bool val) {
    auto cp{*this};
    cp.set(idx, val);
    return cp;
  }
  SmallBoolExprTerm with_unset(unsigned idx) {
    auto cp{*this};
    cp.unset(idx);
    return cp;
  }
  SmallBoolExprTerm with_inv(unsigned idx) {
    auto cp{*this};
    cp.inv(idx);
    return cp;
  }

  dyno::Optional<bool> get(unsigned idx) {
    if (!((pos | neg) & (1 << idx)))
      return dyno::nullopt;
    return pos & (1 << idx);
  }

  void mark_delete() {
    pos = ~0ULL;
    neg = ~0ULL;
  }
  bool is_marked_delete() { return pos == ~0ULL && neg == ~0ULL; }

  static SmallBoolExprTerm zero() { return SmallBoolExprTerm{0, 0}; }
};

template <> struct DenseMapInfo<SmallBoolExprTerm> {
  static constexpr SmallBoolExprTerm getEmptyKey() {
    return SmallBoolExprTerm{~0ULL, ~0ULL};
  }
  static constexpr SmallBoolExprTerm getTombstoneKey() {
    return SmallBoolExprTerm{~0ULL, ~0ULL - 1};
  }
  // no hashing
  static unsigned getHashValue(const SmallBoolExprTerm &k) {
    if constexpr (std::is_same_v<decltype(k.pos), uint64_t>) {
      return hash_combine64(hash_u64(k.pos), hash_u64(k.neg));
    }
    // } else if constexpr (std::is_same_v<decltype(k.pos), uint32_t>) {
    //   return hash_u64(std::bit_cast<uint64_t>(k));
    // } else if constexpr (std::is_same_v<decltype(k.pos), uint16_t>) {
    //   return hash_u32(std::bit_cast<uint32_t>(k));
    // } else if constexpr (std::is_same_v<decltype(k.pos), uint8_t>) {
    //   return hash_u32((uint32_t)std::bit_cast<uint16_t>(k));
    // }
    unreachable();
  }
  static bool isEqual(const SmallBoolExprTerm &lhs,
                      const SmallBoolExprTerm &rhs) {
    return lhs.pos == rhs.pos && lhs.neg == rhs.neg;
  }
};

class SmallBoolExprDNF {
  SmallVec<SmallBoolExprTerm, 16> terms;

  void insertTerm(SmallDenseMap<SmallBoolExprTerm, uint32_t> &termsSet,
                  SmallBoolExprTerm term, unsigned termIdx) {
    for (auto [idx, val] : term.enumerate()) {
      // create copy with term negated
      auto copy = term;
      // set to constant value s.t. terms can find each other.
      // either full match (dedupe) or single lit inverse
      copy.set(idx, 0);
      auto it = termsSet.find(copy);
      if (it == termsSet.end())
        continue;

      // found, check if perfect match or literal inverted
      auto otherIdx = it.val();
      auto &otherTerm = terms[otherIdx];

      if (otherTerm == term) { // perfectly matching, delete this one
        term.mark_delete();
        return; // done
      } else if (otherTerm == term.with_inv(idx)) {
        term.mark_delete();

        // unhash otherTerm
        for (auto [idx, val] : otherTerm.enumerate()) {
          auto copy = term;
          copy.set(idx, 0);
          if (auto it = termsSet.find(copy))
            termsSet.erase(it);
        }
        otherTerm.unset(idx);

        // recursive call
        insertTerm(termsSet, otherTerm, otherIdx);
        return;

      } else
        unreachable();

      assert(otherTerm == term || otherTerm == term.with_inv(idx));
    }

    // not found or simplified, insert
    for (auto [idx, val] : term.enumerate()) {
      // create copy with term negated
      auto copy = term;
      // set to constant value s.t. terms can find each other.
      // either full match (dedupe) or single lit inverse
      copy.set(idx, 0);
      termsSet.findOrInsert(copy, termIdx);
    }
  }

  bool subsumes(SmallBoolExprTerm t1, SmallBoolExprTerm t2) {

    bool pos_subset = (t1.pos & t2.pos) == t1.pos;
    bool neg_subset = (t1.neg & t2.neg) == t1.neg;

    return pos_subset && neg_subset;
  }

  void gcTerms() {
    // todo: copy into local mem if possible
    uint64_t outputIdx = 0;
    for (auto term : terms) {
      if (term.is_marked_delete() || term.isFalse())
        continue;
      if (term.isTrue()) {
        setTrue();
        return;
      }
      terms[outputIdx++] = term;
    }
    terms.resize(outputIdx);
  }

public:
  void simplify() {
    gcTerms();

    SmallDenseMap<SmallBoolExprTerm, uint32_t> termsSet;
    for (auto [termIdx, term] : Range{terms}.enumerate()) {
      if (term.size() < 2 || term.is_marked_delete())
        continue;
      insertTerm(termsSet, term, termIdx);
    }

    gcTerms();

    Range{terms}.stable_sort(
        [](auto &termA, auto &termB) { return termA.size() < termB.size(); });

    for (size_t i = 0; i < terms.size(); i++) {
      if (terms[i].is_marked_delete())
        continue;
      for (size_t j = i + 1; j < terms.size(); j++) {
        if (terms[j].is_marked_delete())
          continue;
        if (subsumes(terms[i], terms[j])) {
          terms[j].mark_delete();
        }
      }
    }

    gcTerms();
  }

  void addAND(unsigned lit, bool pol) {
    bool marked = false;
    for (auto &term : terms) {
      if (term.get(lit).value_or(pol) != pol) {
        term.mark_delete();
        marked = true;
      }
      term.set(lit, pol);
    }
    if (marked)
      gcTerms();
  }

  void insertTerm(SmallBoolExprTerm term) {
    if (term.isFalse())
      return;
    if (term.isTrue()) {
      setTrue();
      return;
    }
    terms.emplace_back(term);
  }

  bool isTrue() const { return terms.size() == 1 && terms.front().size() == 0; }
  bool isFalse() const { return terms.size() == 0; }

  auto begin() { return terms.begin(); }
  auto end() { return terms.end(); }
  auto size() const { return terms.size(); }
  auto empty() const { return terms.empty(); }

  void setTrue() { terms = {{}}; }
  void setFalse() { terms = {}; }
};

template <typename LitType> class TypedSmallBoolExprDNF {
  StaticVec<LitType, 64> table;
  SmallBoolExprDNF expr;

public:
  bool isTrue() const { return expr.isTrue(); }
  bool isFalse() const { return expr.isFalse(); }

  static TypedSmallBoolExprDNF falseExpr() { return TypedSmallBoolExprDNF(); }
  static TypedSmallBoolExprDNF trueExpr() {
    auto rv = TypedSmallBoolExprDNF();
    rv.expr.setTrue();
    return rv;
  }

  template <typename T> // std::pair<LitType, bool>
  void addTerm(Range<T> literals) {
    SmallBoolExprTerm term = SmallBoolExprTerm::zero();
    for (auto [val, pol] : literals) {
      auto idx = Range{table}.find_idx(val);
      if (!idx) {
        idx = table.size();
        table.push_back(val);
      }
      term.set(*idx, pol);
    }
    expr.insertTerm(term);
  }

  void addTerms(TypedSmallBoolExprDNF &other) {
    for (auto elem : other)
      addTerm(elem);
  }

  void addAND(LitType lit, bool pol) {
    auto idx = Range{table}.find_idx(lit);
    if (!idx) {
      idx = table.size();
      table.push_back(lit);
    }

    expr.addAND(*idx, pol);
  }

  void simplify() {
    // todo: consolidate terms (or do when allocing new)
    return expr.simplify();
  }

  auto size() const { return expr.size(); }
  auto empty() const { return expr.empty(); }

  struct iterator
      : public base_iterator<iterator, std::random_access_iterator_tag,
                             intptr_t> {
    SmallBoolExprTerm *ptr;
    const LitType *table;

  public:
    iterator &operator+=(intptr_t diff) {
      ptr += diff;
      return *this;
    }
    intptr_t operator-(iterator other) const { return ptr - other.ptr; }
    auto operator<=>(iterator other) const { return ptr <=> other.ptr; }
    auto operator==(iterator other) const { return ptr == other.ptr; }

    auto operator*() const {
      return ptr->enumerate().transform(
          [table = this->table](size_t, auto pair) {
            return std::make_pair(table[pair.first], pair.second);
          });
    }
    auto val() { return *ptr; }
    iterator() {}
    iterator(SmallBoolExprTerm *ptr, const LitType *table)
        : ptr(ptr), table(table) {}
  };

  iterator begin() { return iterator{expr.begin(), table.data()}; }
  iterator end() { return iterator{expr.end(), table.data()}; }

  __attribute__((used)) std::string toString() {
    std::string str;
    for (auto term : Range{iterator{expr.begin(), table.data()},
                           iterator{expr.end(), table.data()}}) {
      str += "(";

      for (auto [ref, inv] : term) {
        if (inv)
          str += "!";
        str += std::to_string(ref.getObjID().num);
        str += "&";
      }
      if (!term.empty())
        str.resize(str.size() - 1);
      str += ") |";
    }
    if (!str.empty())
      str.resize(str.size() - 2);
    return str;
  }
};

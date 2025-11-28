#pragma once

#include "dyno/IDObjStore.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/ObjPool.h"
#include "dynomite/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/PtrBitField.h"
#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstdint>
#include <new>
#include <type_traits>

namespace dyno {

class SATLit {
  uint32_t num;
  static constexpr uint32_t invalid = ~0;

public:
  constexpr static SATLit fromRaw(uint32_t val) {
    SATLit lit;
    lit.setRaw(val);
    return lit;
  }

  SATLit() = default;
  constexpr SATLit(nullref_t) : num(invalid) {}

  constexpr SATLit(ObjID varID, bool negated) : num(varID << 1 | negated) {}

  constexpr bool isNegated() const { return num & 1; }

  constexpr ObjID getVarID() const { return ObjID(num >> 1); }

  constexpr explicit operator bool() const { return num == invalid; }

  friend bool operator==(SATLit a, SATLit b) { return a.num == b.num; }

  constexpr void setRaw(uint32_t val) { num = val; }

  constexpr uint32_t raw() { return num; }
  constexpr uint64_t raw64hi() { return raw64lo() << 32; }
  constexpr uint64_t raw64lo() { return num; }

  constexpr SATLit negated() const { return {getVarID(), !isNegated()}; }
  constexpr SATLit nonNegated() const { return {getVarID(), false}; }

  constexpr SATLit operator-() const { return negated(); }
};
static_assert(sizeof(SATLit) == 4);

template <typename Derived> class SATClauseMixin {
private:
  Derived &self() { return static_cast<Derived &>(*this); }

public:
  using iterator = SATLit *;

  SATLit &lit(unsigned n) {
    assert(n < self().getNumLiterals());
    return *(self().begin() + n);
  }
  SATLit &operator[](unsigned i) { return lit(i); }
};

class SATClause : public TrailingObjArr<SATClause, SATLit>,
                  public SATClauseMixin<SATClause> {
  friend class TrailingObjArr;
  friend class SATClauseRef;
  unsigned redundant : 1;
  unsigned lbd;
  unsigned numLiterals;

public:
  using iterator = SATLit *;

  SATClause(DynObjRef, unsigned numLiterals) : numLiterals(numLiterals) {}

  iterator begin() { return trailing(); }
  iterator end() { return trailing() + numLiterals; }
  size_t getNumLiterals() { return numLiterals; }

protected:
  size_t getNumTrailing() { return numLiterals; }
};

class SATBinClause : public SATClauseMixin<SATBinClause> {
  std::array<SATLit, 2> litArr;

public:
  using iterator = SATLit *;

  static SATBinClause fromRaw(uint64_t val) {
    return {SATLit::fromRaw(bit_select(val, 0, 32)),
            SATLit::fromRaw(bit_select(val, 1, 32))};
  }

  // BinSATClause(SATLit litA) {
  //   assert(litA);
  //   lit[0] = litA;
  //   lit[1] = nullref;
  // }

  SATBinClause(SATLit litA, SATLit litB) {
    assert(litA && litB);
    litArr[0] = litA;
    litArr[1] = litB;
  }

  iterator begin() { return litArr.begin(); }
  iterator end() { return litArr.end(); }

  uint64_t raw() { return litArr[0].raw64lo() | litArr[1].raw64hi(); }

  // bool isUnary() { return !bool(lit[1]); }

  unsigned getNumLiterals() { return litArr.size(); }
};
static_assert(sizeof(SATBinClause) == 8);
static_assert(std::is_trivially_destructible_v<SATBinClause>);

class SATClauseRef : public FatObjRef<SATClause>, SATClauseMixin<SATClauseRef> {
  friend class SATClause;

public:
  using FatObjRef::FatObjRef;

  iterator begin() { return (*this)->begin(); }
  iterator end() { return (*this)->end(); }
};

// class SATBinClauseOrPtr {
//   PtrBitField<SATClause, 1> ptr;
//
// public:
//   SATBinClauseOrPtr(SATBinClause binClause) : ptr(binClause.raw() << 1 | 1) {}
//
//   SATBinClauseOrPtr(SATClauseRef ref) : ptr(ref.getPtr(), 0) {}
//
//   bool isBin() { return ptr.field(); }
//
//   SATBinClause asBin() {
//     assert(isBin());
//     return SATBinClause::fromRaw(ptr.num >> 1);
//   }
//
//   SATClause *asPtr() {
//     assert(!isBin());
//     return ptr.getPtr();
//   }
//   SATClause *operator->() { return asPtr(); }
//   SATClause &operator*() { return *asPtr(); }
// };

// class SATClauseTRef : public ObjRef<SATClause>, SATClauseMixin<SATClauseRef>
// {
//   friend class SATClause;
//   public:
// };

using SATClauseStore = NewDeleteObjStore<SATClause>;
using SATVarStore = IDObjStore;

template <> struct ObjTraits<SATClause> {
  static constexpr DialectType ty{DMT_SAT_CLAUSE};
  using FatRefT = SATClauseRef;
};

class SATClauseBuilder {
  SATClauseRef ref;

  SATClauseBuilder() {}
};

class SATWatches {
public:
  SATWatches() {}
};

class SATSolver {
public:
  SATClauseStore clauses;
  SATVarStore vars;
  std::vector<uint32_t> watches;
  std::vector<SATLit> trail;

  SATLit addVar() { return SATLit{vars.create(), false}; }

  void addClause(std::initializer_list<SATLit> lits) {
    auto ref = clauses.create(lits.size());
    std::copy_n(lits.begin(), lits.size(), ref.begin());
  }

  size_t getNumLiterals() { return 2 * vars.getNumIDs(); }

  void solve() {
    watches.clear();
    watches.resize(getNumLiterals());
  }

  bool assign() {}
  bool analyze() {}
};

inline void dumpClause(SATClauseRef c) {
  dbgs() << "{";
  bool first = true;
  for (auto &lit : c) {
    if (!first) {
      dbgs() << ", ";
    }
    if (lit.isNegated()) {
      dbgs() << "-";
    }
    dbgs() << lit.getVarID();
    first = false;
  }
  dbgs() << "}";
}

inline void dumpSATSolver(SATSolver &s) {
  dbgs() << "Clauses:\n";
  for (auto c : s.clauses) {
    dumpClause(c);
    dbgs() << "\n";
  }
}

}; // namespace dyno

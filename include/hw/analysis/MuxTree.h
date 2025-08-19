#pragma once
#include "dyno/Obj.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/DenseMapInfo.h"
#include "support/DenseMultimap.h"
#include "support/DynBitSet.h"
#include "support/Optional.h"
#include "support/SmallVec.h"
#include <bit>
#include <cstdint>
#include <unordered_map>
namespace dyno {

struct BoolExprLiteral {
  uint16_t id : 14;
  uint16_t inverse : 1;
  uint16_t clauseBegin : 1;

  BoolExprLiteral canonical() const {
    auto rv = *this;
    rv.clauseBegin = false;
    return rv;
  }

  friend bool operator==(BoolExprLiteral lhs, BoolExprLiteral rhs) {
    return std::bit_cast<uint16_t>(lhs) == std::bit_cast<uint16_t>(rhs);
  }

  void mark() { id = (1 << 14) - 1; }

  bool isMarked() const { return id == (1 << 14) - 1; }
};
}; // namespace dyno

template <> struct DenseMapInfo<dyno::BoolExprLiteral> {
  constexpr static bool isEqual(const dyno::BoolExprLiteral &lhs,
                                const dyno::BoolExprLiteral &rhs) {
    return DenseMapInfo<uint16_t>::isEqual(std::bit_cast<uint16_t>(lhs),
                                           std::bit_cast<uint16_t>(rhs));
  }
  constexpr static dyno::BoolExprLiteral getEmptyKey() {
    return std::bit_cast<dyno::BoolExprLiteral>(
        DenseMapInfo<uint16_t>::getEmptyKey());
  }
  constexpr static dyno::BoolExprLiteral getTombstoneKey() {
    return std::bit_cast<dyno::BoolExprLiteral>(
        DenseMapInfo<uint16_t>::getTombstoneKey());
  }
  constexpr static uint16_t getHashValue(const dyno::BoolExprLiteral &c) {
    return DenseMapInfo<uint16_t>::getHashValue(std::bit_cast<uint16_t>(c));
  }
};

namespace dyno {

struct SmallBoolExprCNF {
  SmallVec<BoolExprLiteral, 8> literals;

  struct ClauseRef {
    SmallBoolExprCNF *parent;
    uint32_t idx;
    uint32_t len;

    uint32_t size() { return len; }

    BoolExprLiteral &front() { return parent->literals[idx]; }
    BoolExprLiteral &back() { return parent->literals[idx + len - 1]; }
    BoolExprLiteral *begin() { return &front(); }
    BoolExprLiteral *end() { return &back() + 1; }

    BoolExprLiteral &operator[](uint32_t i) {
      assert(i < len);
      return begin()[i];
    }

    void dump() {
      dbgs() << "(";
      for (auto [j, lit] : Range{*this}.enumerate()) {
        if (j != 0)
          dbgs() << " | ";
        dbgs() << (lit.inverse ? "!" : "") << lit.id;
      }
      dbgs() << ")\n";
    }
  };

  struct ClauseIterator {
    ClauseRef current;
    void findLen() {
      current.len = 0;
      auto &lits = current.parent->literals;
      if (current.idx == lits.size())
        return;
      assert(lits[current.idx].clauseBegin);
      do
        current.len++;
      while (current.idx + current.len < lits.size() &&
             !lits[current.idx + current.len].clauseBegin);
    }

    ClauseIterator &operator++() {
      current.idx += current.len;
      findLen();
      return *this;
    }
    ClauseIterator operator++(int) {
      auto copy(*this);
      ++*this;
      return copy;
    }

    ClauseRef &operator*() { return current; }
    ClauseRef *operator->() { return &current; }

    ClauseIterator(ClauseRef ref) : current(ref) { findLen(); }

    friend bool operator==(const ClauseIterator &lhs,
                           const ClauseIterator &rhs) {
      auto rv = lhs.current.idx == rhs.current.idx &&
                lhs.current.parent == rhs.current.parent;
      assert(!rv || lhs.current.len == rhs.current.len);
      return rv;
    }
  };

  auto clauses() {
    auto begin = ClauseIterator{ClauseRef{this, 0, 0}};
    auto end = ClauseIterator{ClauseRef{this, this->literals.size(), 0}};
    return Range{begin, end};
  }

  std::optional<bool>
  simplifyImpl(uint numLiterals, SmallVecImpl<Optional<uint8_t>> &known,
               UnsizedBitSet<SmallVec<uint64_t, 2>, ~uint64_t(0)> &keepClause) {
    // for clauses size > 1 either delete known vals or the entire clause.
    UnsizedBitSet<SmallVec<uint64_t, 2>> ignoreClause;
    bool run = true;
    do {
      run = false;
      for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
        if (clause.len == 1 || !keepClause.getDyn(clauseIdx) ||
            ignoreClause.getDyn(clauseIdx))
          continue;

        uint nonMarkedLen = 0;
        uint numDeleted = 0;
        uint nonDeletedIdx = ~0U;

        SmallVec<Optional<uint8_t>, 16> contains(numLiterals);

        for (auto [litIdx, lit] : Range{clause}.enumerate()) {
          if (lit.isMarked())
            continue;
          auto val = !lit.inverse;
          nonMarkedLen++;

          if (!contains[lit.id].has())
            contains[lit.id] = val;
          else if (contains[lit.id].value() == val) {
            lit.mark();
            numDeleted++;
            continue;
          } else {
            keepClause.clearDyn(clauseIdx);
            numDeleted = 0;
            break;
          }

          if (!known[lit.id].has())
            nonDeletedIdx = litIdx;
          else if (known[lit.id] != val) {
            lit.mark();
            numDeleted++;
            continue;
          } else {
            keepClause.clearDyn(clauseIdx);
            numDeleted = 0;
            break;
          }
        }

        if (numDeleted != 0) {
          uint rem = nonMarkedLen - numDeleted;
          if (rem == 0) {
            // unsat
            makeUnsat();
            return false;
          }
          if (rem == 1) {
            known[clause[nonDeletedIdx].id] = !clause[nonDeletedIdx].inverse;
            ignoreClause.setDyn(clauseIdx);
            run = true;
          }
        }
      }
    } while (run);

    // sort all clauses
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      auto compare = [](const BoolExprLiteral &lhs,
                        const BoolExprLiteral &rhs) { return lhs.id < rhs.id; };
      if (!keepClause.getDyn(clauseIdx))
        continue;
      clause.front().clauseBegin = false;
      std::sort(clause.begin(), clause.end(), compare);
      clause.front().clauseBegin = true;
    }
    // this->dump();

    // find superset clauses
    SmallVec<SmallVec<uint32_t, 8>, 8> uses;
    uses.resize(numLiterals * 2);
    for (auto [clauseIdx, clause] : clauses().enumerate()) {
      if (clause.size() < 2 || !keepClause.getDyn(clauseIdx))
        continue;
      for (auto lit : clause) {
        if (lit.isMarked())
          continue;
        uses[lit.id << 1 | lit.inverse].emplace_back(clause.idx);
      }
    }
    for (auto [clauseIdx, clause] : clauses().enumerate()) {
      if (clause.size() < 2 || !keepClause.getDyn(clauseIdx))
        continue;
      SmallVec<uint32_t, 8> intersect;
      bool first = true;

      for (auto lit : clause) {
        if (lit.isMarked())
          continue;
        auto &other = uses[lit.id << 1 | lit.inverse];
        if (first) {
          intersect = other;
          first = false;
          continue;
        }

        size_t out = 0;
        size_t i = 0, j = 0;
        while (i != intersect.size() && j != other.size()) {
          if (intersect[i] == other[j]) {
            intersect[out++] = intersect[i];
            i++;
            j++;
          } else if (intersect[i] < other[j])
            i++;
          else
            j++;
        }
        intersect.downsize(out);
      }

      // dbgs() << "compare:\n";
      // clause.dump();
      for (auto otherIdx : intersect) {
        if (otherIdx == clause.idx)
          continue;
        auto clauseIt = ClauseIterator{ClauseRef{this, otherIdx, 0}};
        auto other = *clauseIt;

        uint otherClauseIdx = 0;
        for (uint i = 0; i < otherIdx; i++)
          otherClauseIdx += literals[i].clauseBegin;

        if (!keepClause.getDyn(otherClauseIdx))
          continue;
        // other.dump();
        keepClause.clearDyn(otherClauseIdx);

        for (auto lit : other) {
          if (lit.isMarked())
            continue;
          auto &arr = uses[lit.id << 1 | lit.inverse];
          auto it = std::find(arr.begin(), arr.end(), other.idx);
          assert(it);
          arr.erase(it);
        }
      }
      // dbgs() << "\n\n";
    }

    // combine clauses only differing by one negation
    DenseMultimap<uint32_t, uint32_t> combineMap;
    do {
      run = false;
      combineMap.clear();
      for (auto [clauseIdx, clause] : clauses().enumerate()) {
        if (clause.size() == 1 || !keepClause.getDyn(clauseIdx))
          continue;

        uint32_t totalHash = 0;
        uint32_t numValidLits = 0;
        for (auto lit : clause) {
          if (!lit.isMarked()) {
            totalHash ^= hash_u32(lit.id << 1 | lit.inverse);
            numValidLits++;
          }
        }

        if (numValidLits == 1)
          continue;

        for (auto [litIdx, lit] : Range{clause}.enumerate()) {
          if (lit.isMarked())
            continue;

          uint32_t hashWO = totalHash ^ hash_u32(lit.id << 1 | lit.inverse);
          uint32_t hash = hashWO ^ hash_u32(lit.id << 16);

          auto it = combineMap.find(hash);
          for (; it != combineMap.end(); it = combineMap.find_next(it)) {
            auto otherClauseIdx = it.val();
            auto clauseIt = ClauseIterator{ClauseRef{this, otherClauseIdx, 0}};
            auto &other = *clauseIt;

            if (otherClauseIdx == clause.idx)
              continue;

            Optional<uint32_t> diffIdx = nullopt;
            {
              uint32_t a = 0, b = 0;
              for (uint32_t i = 0; i < numValidLits; i++) {
                while (other[a].isMarked())
                  a++;
                while (clause[b].isMarked())
                  b++;

                if (other[a].id != clause[b].id)
                  goto not_matching;
                if (other[a].inverse != clause[b].inverse) {
                  if (diffIdx)
                    goto not_matching;
                  diffIdx = a;
                }

                a++;
                b++;
              }
              while (a < other.len && other[a].isMarked())
                a++;
              while (b < clause.len && clause[b].isMarked())
                b++;
              if (a != other.len || b != clause.len)
                goto next_clause;
            }

            // perfectly equal, just delete this
            if (!diffIdx) {
              for (auto [key, val] : combineMap)
                assert(val != clause.idx);
              keepClause.clearDyn(clauseIdx);
              goto next_clause;
            } else {
              keepClause.clearDyn(clauseIdx);
              other[*diffIdx].mark();
              it.erase();

              for (size_t i = 0; i < litIdx; i++) {
                auto lit = clause[i];
                if (lit.isMarked())
                  continue;
                uint32_t hashWO =
                    totalHash ^ hash_u32(lit.id << 1 | lit.inverse);
                uint32_t hash = hashWO ^ hash_u32(lit.id << 16);
                auto it = combineMap.find(hash);
                assert(it != combineMap.end());
                while (it != combineMap.end()) {
                  auto next = combineMap.find_next(it);
                  if (it.val() == clause.idx)
                    it.erase();
                  it = next;
                }
              }

              // delete stale
              // reconstruct other's total hash
              uint32_t totalHash =
                  hashWO ^ hash_u32(lit.id << 1 | !lit.inverse);
              for (auto litO : other) {
                if (litO.isMarked())
                  continue;
                uint32_t hash = totalHash ^
                                hash_u32(litO.id << 1 | litO.inverse) ^
                                (hash_u32(litO.id << 16));
                auto it = combineMap.find(hash);
                assert(it != combineMap.end());
                while (it != combineMap.end()) {
                  auto next = combineMap.find_next(it);
                  if (it.val() == otherClauseIdx)
                    it.erase();
                  it = next;
                }
              }

              // add new
              totalHash = hashWO;
              for (auto litO : other) {
                if (litO.isMarked())
                  continue;
                uint32_t hash = totalHash ^
                                hash_u32(litO.id << 1 | litO.inverse) ^
                                hash_u32(litO.id << 16);
                combineMap.insert(hash, otherClauseIdx);
              }

              run = true;
              goto next_clause;
            }

          not_matching:;
          }
          combineMap.insert(hash, clause.idx);
        }
      next_clause:;
      }
    } while (run);

    size_t outputIdx = 0;
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      if (!keepClause.getDyn(clauseIdx))
        continue;
      auto pos = outputIdx;
      for (auto lit : clause) {
        if (!lit.isMarked())
          this->literals[outputIdx++] = lit;
      }
      if (pos == outputIdx) {
        makeUnsat();
        return false;
      }
      this->literals[pos].clauseBegin = true;
    }

    if (outputIdx == 0) {
      makeTrue();
      return true;
    }

    // DEBUG("MuxTreeOptimization", {
    //   std::print(dbgs(), "simplified condition from {} to {} literals\n",
    //              literals.size(), outputIdx);
    // })

    this->literals.downsize(outputIdx);
    // this->dump();
    return std::nullopt;
  }

  bool findAndDedupeKnown(
      uint numLiterals, SmallVecImpl<Optional<uint8_t>> &known,
      UnsizedBitSet<SmallVec<uint64_t, 2>, ~uint64_t(0)> &keepClause) {

    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      if (clause.len == 1) {
        bool val = !clause.front().inverse;
        auto &slot = known[clause.front().id];
        if (slot) {
          if (*slot != val) {
            makeUnsat();
            return false;
          }

          keepClause.clearDyn(clauseIdx);
        }
        slot = val;
      }
    }

    return true;
  }

  std::optional<bool> simplify(uint numLiterals) {
    if (isTrue())
      return true;
    if (isUnsat())
      return false;
    // todo: dedupe literals and clause subset for non-singleton clause
    // this->dump();
    SmallVec<Optional<uint8_t>, 16> known(numLiterals);
    UnsizedBitSet<SmallVec<uint64_t, 2>, ~uint64_t(0)> keepClause;

    auto sat = findAndDedupeKnown(numLiterals, known, keepClause);
    if (!sat)
      return false;
    return simplifyImpl(numLiterals, known, keepClause);
  }

  // assuming other is simplified.
  std::optional<bool> simplifyWith(SmallBoolExprCNF &other, uint numLiterals,
                                   bool checkSAT = true) {
    this->addAsGlobalAND(other);
    auto res = this->simplify(numLiterals);
    if (res.has_value())
      return res;

    if (checkSAT) {
      auto temp = *this;
      auto sat = satSolve(numLiterals);
      if (!sat) {
        makeUnsat();
        return false;
      }
      *this = temp;
    }

    auto hashClause = [](ClauseRef clause) {
      uint32_t hash = 0;
      for (auto lit : clause)
        hash = hash_combine(hash, hash_u32(lit.id << 1 | lit.inverse));
      if (hash == DenseMapInfo<uint32_t>::getEmptyKey() ||
          hash == DenseMapInfo<uint32_t>::getTombstoneKey()) [[unlikely]]
        hash = 0;
      return hash;
    };

    DenseMultimap<uint32_t, uint32_t> deleteMap;
    for (auto clause : other.clauses()) {
      deleteMap.insert(hashClause(clause), clause.idx);
    }

    UnsizedBitSet<SmallVec<uint64_t, 2>> deleteClause;

    for (auto [clauseIdx, clause] : this->clauses().enumerate()) {
      auto it = deleteMap.find(hashClause(clause));
      for (; it != deleteMap.end(); it = deleteMap.find_next(it)) {
        auto otherClauseIdx = it.val();
        auto otherClauseIt =
            ClauseIterator{ClauseRef{&other, otherClauseIdx, 0}};
        auto other = *otherClauseIt;

        if (other.len != clause.len)
          goto next;
        for (uint i = 0; i < clause.len; i++)
          if (other[i] != clause[i])
            goto next;
        deleteClause.setDyn(clauseIdx);
      next:;
      }
    }

    size_t outIdx = 0;
    for (auto [clauseIdx, clause] : this->clauses().enumerate()) {
      if (deleteClause.getDyn(clauseIdx))
        continue;
      for (auto lit : clause)
        literals[outIdx++] = lit;
    }
    literals.downsize(outIdx);

    if (outIdx == 0) {
      makeTrue();
      return true;
    }

    return std::nullopt;
  }

  bool satSolve(uint numLiterals) {
    SmallVec<Optional<uint8_t>, 16> known(numLiterals);
    SmallVec<uint8_t, 16> polarity(numLiterals);
    for (auto lit : literals) {
      known[lit.id] = !lit.inverse;
      auto &slot = polarity[lit.id];
      if (slot == 0)
        slot = 1 + lit.inverse;
      else if (slot != lit.inverse + 1) {
        slot = 3;
        known[lit.id] = nullopt;
      }
    }

    UnsizedBitSet<SmallVec<uint64_t, 2>, ~uint64_t(0)> keepClause;
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      if (clause.len == 1) {
        keepClause.clearDyn(clauseIdx);
        bool val = !clause.front().inverse;
        auto &slot = known[clause.front().id];
        if (slot) {
          if (*slot != val) {
            makeUnsat();
            return false;
          }
        }
        slot = val;
      } else {
        for (auto lit : clause) {
          if (polarity[lit.id] != 3) {
            keepClause.clearDyn(clauseIdx);
            break;
          }
        }
      }
    }

    auto rv = simplifyImpl(numLiterals, known, keepClause);
    if (rv.has_value()) {
      return *rv;
    }

    SmallBoolExprCNF copy = *this;

    // just pick first literal, no heuristic
    literals.emplace_back(BoolExprLiteral{literals[0]});
    literals.back().inverse = 0;
    literals.back().clauseBegin = true;
    auto ifTrue = satSolve(numLiterals);
    *this = copy;
    if (ifTrue) {
      return true;
    }

    literals.emplace_back(BoolExprLiteral{literals[0]});
    literals.back().inverse = 1;
    literals.back().clauseBegin = true;

    auto ifFalse = satSolve(numLiterals);
    *this = copy;
    if (ifFalse) {
      return true;
    }

    makeUnsat();
    return false;
  }

  bool isUnsat() const { return literals.size() == 0; }
  bool isTrue() const { return literals.size() == 1 && literals[0].isMarked(); }

  void makeTrue() {
    literals = {BoolExprLiteral{0, 0, 1}};
    literals.front().mark();
  }
  void makeUnsat() { literals.clear(); }

  std::optional<SmallBoolExprCNF> negated(uint numLiterals) {
    if (isTrue()) {
      SmallBoolExprCNF rv;
      rv.makeUnsat();
      return rv;
    }
    if (isUnsat()) {
      SmallBoolExprCNF rv;
      rv.makeTrue();
      return rv;
    }

    uint64_t count = 1;
    SmallVec<ClauseRef, 8> clauseVec;
    for (auto clause : clauses()) {
      count *= clause.len;
      clauseVec.emplace_back(clause);
    }
    if (count > 2000000) {
      dbgs() << count;
      return std::nullopt;
    }

    SmallBoolExprCNF exprOut;
    for (uint32_t i = 0; i < count; i++) {
      uint32_t n = i;

      auto pos = exprOut.literals.size();

      for (auto clause : clauseVec) {
        uint32_t rem = n % clause.len;
        n /= clause.len;
        auto lit = clause[rem];
        lit.clauseBegin = false;
        lit.inverse = !lit.inverse;
        exprOut.literals.emplace_back(lit);
      }
      exprOut.literals[pos].clauseBegin = true;

      if (exprOut.literals.size() > 1024) {
        auto sat = exprOut.simplify(numLiterals);
        if (sat.has_value() && sat.value() == true)
          exprOut.literals.clear();
        if (sat.has_value() && sat.value() == false) {
          return exprOut;
        }
      }
    }

    exprOut.simplify(numLiterals);
    return exprOut;
  }

  void dump(bool newl = true) {

    for (auto [i, clause] : clauses().enumerate()) {
      dbgs() << (i == 0 ? "(" : " & (");
      for (auto [j, lit] : Range{clause}.enumerate()) {
        if (j != 0)
          dbgs() << " | ";
        dbgs() << (lit.inverse ? "!" : "") << lit.id;
      }
      dbgs() << ")";
    }
    if (newl)
      dbgs() << "\n";
  }

  void dump2() {
    dbgs() << "\n";
    dbgs() << "\n";
    for (auto [i, clause] : clauses().enumerate()) {
      for (auto [j, lit] : Range{clause}.enumerate()) {
        if (j != 0)
          dbgs() << " ";
        dbgs() << (lit.inverse ? "-" : "") << (lit.id + 1);
      }
      dbgs() << "\n";
    }
  }

  void addTseitin(SmallBoolExprCNF &expr, uint16_t tseitinLitID) {
    assert(0 && "todo");
    // proxy implies all clauses
    for (auto clause : expr.clauses()) {
      literals.push_back_range(Range{clause});
      literals.emplace_back(BoolExprLiteral{tseitinLitID, 1, false});
    }
  }

  void purgeLiteral(BoolExprLiteral purgeLit) {
    for (auto &lit : literals) {
      if (lit.id != purgeLit.id)
        continue;
      assert(lit.inverse == purgeLit.inverse && "invalid purge");
      lit.mark();
    }

    size_t outputIdx = 0;
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      auto pos = outputIdx;
      for (auto lit : clause) {
        if (!lit.isMarked())
          this->literals[outputIdx++] = lit;
      }
      this->literals[pos].clauseBegin = true;
    }
    literals.downsize(outputIdx);
  }
  void purgeLiterals(UnsizedBitSet<SmallVec<uint64_t, 2>> &toPurge) {
    for (auto &lit : literals) {
      if (!toPurge.getDyn(lit.id))
        continue;
      lit.mark();
    }
    size_t outputIdx = 0;
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      auto pos = outputIdx;
      for (auto lit : clause) {
        if (!lit.isMarked())
          this->literals[outputIdx++] = lit;
      }
      this->literals[pos].clauseBegin = true;
    }
    literals.downsize(outputIdx);
  }

  void addAsGlobalAND(const SmallBoolExprCNF &other) {
    if (isUnsat() || other.isUnsat()) {
      makeUnsat();
      return;
    }
    if (isTrue()) {
      *this = other;
    }
    if (other.isTrue()) {
      return;
    }

    literals.push_back_range(Range{other.literals});
  }
  void addAsGlobalOR(SmallBoolExprCNF &other, uint numLiterals) {
    if (isTrue())
      return;
    if (isUnsat()) {
      *this = other;
      return;
    }
    if (other.isTrue()) {
      *this = other;
      return;
    }
    if (other.isUnsat()) {
      return;
    }

    this->dump();
    other.dump();

    SmallBoolExprCNF copy = *this;
    literals.clear();

    for (auto lhs : copy.clauses()) {
      for (auto rhs : other.clauses()) {
        literals.push_back_range(Range{lhs});
        auto mid = literals.size();
        literals.push_back_range(Range{rhs});
        literals[mid].clauseBegin = 0;
      }
    }

    this->dump();
    simplify(numLiterals);
    this->dump();
  }

  auto evalWithBoundVars2(SmallBoolExprCNF &orig, SmallBoolExprCNF &expr,
                          SmallBoolExprCNF &exprNeg, uint numLiterals) {
    SmallBoolExprCNF outTrue = orig;
    auto trueBranchRes = outTrue.simplifyWith(expr, numLiterals);
    bool trueBranchSat = !trueBranchRes.has_value() || trueBranchRes.value();

    exprNeg.simplify(numLiterals);
    SmallBoolExprCNF outFalse = orig;

    auto falseBranchRes = outFalse.simplifyWith(exprNeg, numLiterals);
    bool falseBranchSat = !falseBranchRes.has_value() || falseBranchRes.value();

    return std::make_tuple(outTrue, trueBranchSat, outFalse, falseBranchSat);
  }
};
}; // namespace dyno

namespace dyno {
struct MuxTree {
  struct Entry {
    SmallBoolExprCNF expr;
    DynObjRef output;
  };
  SmallVec<Entry, 4> entries;

  struct InputSignal {
    ObjRef<Wire> wire;
    uint32_t idx;
  };
  SmallVec<InputSignal, 4> conditions;

  InstrRef root;
};
}; // namespace dyno

template <> struct DenseMapInfo<dyno::MuxTree::InputSignal> {
  constexpr static bool isEqual(const dyno::MuxTree::InputSignal &lhs,
                                const dyno::MuxTree::InputSignal &rhs) {
    return DenseMapInfo<uint64_t>::isEqual(std::bit_cast<uint64_t>(lhs),
                                           std::bit_cast<uint64_t>(rhs));
  }
  constexpr static dyno::MuxTree::InputSignal getEmptyKey() {
    return std::bit_cast<dyno::MuxTree::InputSignal>(
        DenseMapInfo<uint64_t>::getEmptyKey());
  }
  constexpr static dyno::MuxTree::InputSignal getTombstoneKey() {
    return std::bit_cast<dyno::MuxTree::InputSignal>(
        DenseMapInfo<uint64_t>::getTombstoneKey());
  }
  constexpr static uint64_t getHashValue(const dyno::MuxTree::InputSignal &c) {
    return DenseMapInfo<uint64_t>::getHashValue(std::bit_cast<uint64_t>(c));
  }
};

namespace dyno {

class MuxtreeAnalysis {
public:
  SmallDenseMap<MuxTree::InputSignal, uint32_t, 2> conditionsDedupeMap;

  auto getCondIdx(MuxTree *muxtree, WireRef cond, uint32_t bit) {
    MuxTree::InputSignal val{cond, bit};
    auto [found, it] = conditionsDedupeMap.findOrInsert(val, [&]() {
      auto rv = muxtree->conditions.size();
      muxtree->conditions.emplace_back(val);
      return rv;
    });
    return it.val();
  }

  void analyzeCond(MuxTree *muxtree, SmallBoolExprCNF &cond, WireRef wire) {
    if (auto instr = wire.getDefI();
        instr.isOpc(OP_ICMP_EQ) && instr.other(1)->is<ConstantRef>()) {
      wire = wire.getDefI().other(0)->as<WireRef>();
      auto constant = instr.other(1)->as<ConstantRef>();

      for (uint i = 0; i < constant.getNumBits(); i++) {
        switch (constant.getBit(i)) {
        case FourState::S0:
          cond.literals.emplace_back(getCondIdx(muxtree, wire, i), 1, 1);
          break;
        case FourState::S1:
          cond.literals.emplace_back(getCondIdx(muxtree, wire, i), 0, 1);
          break;
        case FourState::SZ:
        case FourState::SX:
          break;
        }
      }
      return;
    } else if (instr.isOpc(OP_AND)) {
      cond.makeTrue();
      for (auto op : instr.others()) {
        analyzeCond(muxtree, cond, op->as<WireRef>());
      }
      return;
    } else if (instr.isOpc(OP_NOT)) {
      analyzeCond(muxtree, cond, instr.other(0)->as<WireRef>());
      cond = *cond.negated(muxtree->conditions.size());
      cond.simplify(muxtree->conditions.size());
      return;
    } else if (instr.isOpc(OP_OR)) {
      cond.makeUnsat();
      for (auto op : instr.others()) {
        SmallBoolExprCNF subExpr;
        analyzeCond(muxtree, subExpr, op->as<WireRef>());
        cond.addAsGlobalOR(subExpr, muxtree->conditions.size());
      }
      return;
    } else if (instr.isOpc(HW_SPLICE)) {
      if (auto offs = instr.other(1)->dyn_as<ConstantRef>()) {
        // todo: trunc, negate
        cond.literals.emplace_back(getCondIdx(muxtree,
                                              instr.other(0)->as<WireRef>(),
                                              offs.getExactVal()),
                                   0, 1);
        return;
      }
    } else if (instr.isOpc(OP_TRUNC)) {
      cond.literals.emplace_back(
          getCondIdx(muxtree, instr.other(0)->as<WireRef>(), 0), 0, 1);
      return;
    }

    cond.literals.emplace_back(getCondIdx(muxtree, wire, 0), 0, 1);
  }

  MuxTree analyzeMuxTree(InstrRef root, bool matchMultiUse = false) {
    return analyzeMuxTree(root, [](InstrRef) {}, matchMultiUse);
  }

  MuxTree analyzeMuxTree(InstrRef root,
                         std::invocable<InstrRef> auto visitedCallback,
                         bool matchMultiUse = false) {
    conditionsDedupeMap.clear();
    SmallVec<std::tuple<HWValue, uint32_t>, 32> worklist{
        {root.def(0)->as<WireRef>(), 1}};

    SmallVec<SmallBoolExprCNF, 4> prefixes;

    MuxTree muxtreeVal;
    MuxTree *muxtree = &muxtreeVal;
    muxtree->root = root;

    while (!worklist.empty()) {
      auto [val, idx] = worklist.back();
      if (val.is<ConstantRef>()) {
        SmallBoolExprCNF expr;
        expr.makeTrue();
        for (auto &prefix : prefixes)
          expr.addAsGlobalAND(prefix);
        muxtree->entries.emplace_back(expr, val.as<FatDynObjRef<>>());
        worklist.pop_back();
        continue;
      }
      auto asWire = val.as<WireRef>();
      auto defI = asWire.getSingleDef()->instr();
      if (!defI.isOpc(HW_MUX) || (!matchMultiUse && defI != root &&
                                  !defI.def(0)->as<WireRef>().hasSingleUse())) {
        SmallBoolExprCNF expr;
        expr.makeTrue();
        for (auto &prefix : prefixes)
          expr.addAsGlobalAND(prefix);
        muxtree->entries.emplace_back(SmallBoolExprCNF{expr},
                                      val.as<FatDynObjRef<>>());
        worklist.pop_back();
        continue;
      }
      auto outOperand = *asWire.getSingleDef();
      auto instr = outOperand.instr();
      auto operand = *(instr.other_begin() + idx);
      if (operand == instr.other_end()) {
        worklist.pop_back();
        prefixes.pop_back();
        continue;
      }

      // on first touch push prefix and do callback
      if (operand == instr.other(1)) {
        visitedCallback(instr);
        auto &prefix = prefixes.emplace_back();
        analyzeCond(muxtree, prefix, instr.other(0)->as<WireRef>());
      }

      switch (*instr.getDialectOpcode()) {
      case *HW_MUX: {
        std::get<1>(worklist.back()) += 1;
        worklist.emplace_back(operand->as<HWValue>(), 1);
        if (operand != instr.other(1)) {
          prefixes.back() =
              *prefixes.back().negated(muxtree->conditions.size());
        }
        break;
      }
      default:
        dyno_unreachable("invalid instr");
      }
    }
    return muxtreeVal;
  }

  void simplifyConditions(MuxTree *tree) {
    for (size_t i = 0; i < tree->entries.size(); i++) {
      auto &entry = tree->entries[i];
      bool cont = false;
      do {
        entry.expr.simplify(tree->conditions.size());
        if (entry.expr.isUnsat()) {
          tree->entries.erase(tree->entries.begin() + i);
          i--;
          break;
        }
      } while (cont);
    }
  }

  void dedupeMuxTreeOutputs(MuxTree *tree) {
    SmallDenseMap<DynObjRef, SmallVec<uint32_t, 2>, 2> dedupeMap;
    for (auto [i, rule] : Range{tree->entries}.enumerate()) {
      dedupeMap[rule.output].emplace_back(i);
    }
    SmallVec<MuxTree::Entry, 4> newEntries;
    newEntries.reserve(dedupeMap.size());
    for (auto [out, rules] : dedupeMap) {
      auto &entry = newEntries.emplace_back();
      entry.output = out;
      entry.expr = tree->entries[rules[0]].expr;

      for (auto rule : Range{rules}.drop_front()) {
        entry.expr.addAsGlobalOR(tree->entries[rule].expr,
                                 tree->conditions.size());
      }
      entry.expr.simplify(tree->conditions.size());
    }
    tree->entries = std::move(newEntries);
  }

  void printMuxTree(HWContext &ctx, MuxTree *tree) {
    DEBUG("MuxTreeAnalysis", {
      dbgs() << "mux tree at: ";
      if (tree->root)
        dumpInstr(tree->root, ctx);
      else
        dbgs() << "\n";
      for (auto entry : tree->entries) {

        entry.expr.dump(false);
        dbgs() << ": ";

        dumpObj(ctx.resolveObj(entry.output));
        dbgs() << "\n";
      }
    });
  }
};
}; // namespace dyno

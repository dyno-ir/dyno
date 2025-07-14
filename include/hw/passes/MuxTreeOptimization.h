#pragma once

#include "dyno/Constant.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DenseMapInfo.h"
#include "support/DenseMultimap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/SlabAllocator.h"
#include <bit>
#include <climits>

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

  std::optional<bool> simplify(uint numLiterals) {
    // todo: dedupe literals and clause subset for non-singleton clause
    // this->dump();
    SmallVec<Optional<uint8_t>, 16> known(numLiterals);
    UnsizedBitSet<SmallVec<uint64_t, 2>, ~uint64_t(0)> keepClause;

    // find and dedupe known vals, sort longer clauses
    for (auto [clauseIdx, clause] : Range{clauses()}.enumerate()) {
      if (clause.len == 1) {
        bool val = !clause.front().inverse;
        auto &slot = known[clause.front().id];
        if (slot) {
          if (*slot != val) {
            // unsat
            this->literals.clear();
            return false;
          }
          keepClause.clearDyn(clauseIdx);
        }
        slot = val;
      }
    }

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
            this->literals.clear();
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

      //dbgs() << "compare:\n";
      //clause.dump();
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
        //other.dump();
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
      //dbgs() << "\n\n";
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

        for (auto lit : clause) {
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
            // assert(other.len == numValidLits ||
            // other[numValidLits].isMarked());

            //dbgs() << "merging on idx " << int(diffIdx.value_or(-1)) << ":\n";
            //dbgs() << clause.idx << ": ";
            //clause.dump();
            //dbgs() << other.idx << ": ";
            //other.dump();
            //dbgs() << "\n\n";

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
      this->literals[pos].clauseBegin = true;
    }

    // DEBUG("MuxTreeOptimization", {
    //   std::print(dbgs(), "simplified condition from {} to {} literals\n",
    //              literals.size(), outputIdx);
    // })

    this->literals.downsize(outputIdx);
    // this->dump();
    return std::nullopt;
  }

  // assuming other is simplified.
  std::optional<bool> simplifyWith(SmallBoolExprCNF &other, uint numLiterals) {
    this->addAsGlobalAND(other);
    auto sat = this->simplify(numLiterals);
    if (sat.has_value())
      return sat;

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

    if (outIdx == 0)
      return true;

    return std::nullopt;
  }

  // bool simplifyCondition() {
  //   SmallVec<ClauseRef, 8> terms;
  //   for (auto clause : clauses()) {
  //     if (clause.size() == 0)
  //       continue;
  //     terms.emplace_back(clause);
  //   }

  //   for (auto &term : terms) {
  //     term.front().clauseBegin = false;
  //     std::sort(term.begin(), term.end(),
  //               [](const BoolExprLiteral &lhs, const BoolExprLiteral &rhs) {
  //                 return lhs.id < rhs.id;
  //               });
  //   }

  //   SmallDenseSet<BoolExprLiteral, 1> known;
  //   for (auto term : terms) {
  //     if (term.size() == 1) {
  //       known.findOrInsert(term.front());

  //       auto copy = term.front();
  //       copy.inverse = !copy.inverse;
  //       // unsatisfiable
  //       if (known.contains(copy)) {
  //         literals.clear();
  //         return true;
  //       }
  //       continue;
  //     }
  //   }

  //   SmallVec<BoolExprLiteral, 8> output;
  //   output.push_back_range(
  //       Range{known}.transform([](size_t, BoolExprLiteral l) {
  //         l.clauseBegin = true;
  //         return l;
  //       }));

  //   size_t outputIdx = 0;
  //   for (size_t i = 0; i < terms.size(); i++) {
  //     if (terms[i].size() == 1)
  //       continue;
  //     for (auto term : terms[i]) {
  //       // todo: simple remove if known 0?
  //       if (known.contains(term))
  //         goto skip;
  //     }
  //     terms[outputIdx++] = terms[i];
  //   skip:;
  //   }
  //   terms.downsize(outputIdx);

  //   while (true) {
  //     for (size_t i = 0; i < terms.size(); i++) {
  //       for (size_t j = 0; j < terms.size(); j++) {
  //         auto termA = terms[i];
  //         if (termA.size() == 1)
  //           continue;
  //         if (i >= j)
  //           continue;
  //         auto termB = terms[j];
  //         if (termA.size() != termB.size())
  //           continue;

  //         Optional<size_t> oppositeIdx = nullopt;
  //         for (size_t k = 0; k < termA.size(); k++) {
  //           if (termA[k].id != termB[k].id)
  //             goto has_difference;
  //           if (termA[k].inverse != termB[k].inverse) {
  //             if (oppositeIdx)
  //               goto has_difference;
  //             oppositeIdx = k;
  //           }
  //         }

  //         if (!oppositeIdx) {
  //           output.push_back_range(Range{termA.begin(), termA.end()});
  //         } else {
  //           auto firstIdx = output.size();
  //           output.push_back_range(Range{termA.begin(),
  //           &termA[*oppositeIdx]}); output.push_back_range(
  //               Range{&termA[*oppositeIdx] + 1, termA.end()});
  //           output[firstIdx].clauseBegin = true;
  //         }

  //         terms.erase(terms.begin() + j);
  //         terms.erase(terms.begin() + i);

  //         goto outer_cont;

  //       has_difference:;
  //       }
  //     }
  //     break;

  //   outer_cont:;
  //   }

  //   for (auto term : terms) {
  //     size_t firstIdx = output.size();
  //     output.push_back_range(Range{term});
  //     output[firstIdx].clauseBegin = true;
  //   }

  //   DEBUG("MuxTreeOptimization", {
  //     std::print(dbgs(), "simplified condition from {} to {} literals\n",
  //                literals.size(), output.size());
  //   })
  //   auto rv = literals.size() != output.size();
  //   literals = std::move(output);
  //   return rv;
  // }

  bool isUnsat() const { return literals.size() == 0; }

  auto negated2() {
    uint32_t count = 1;
    SmallVec<ClauseRef, 8> clauseVec;
    for (auto clause : clauses()) {
      count *= clause.len;
      clauseVec.emplace_back(clause);
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
    }

    return exprOut;
  }

  auto negated() {
    SmallBoolExprCNF negated;

    // first collect all of the single clauses to make one single clause.
    for (auto clause : clauses()) {
      if (clause.size() != 1)
        continue;
      auto lit = clause.front();
      lit.inverse = !lit.inverse;
      lit.clauseBegin = false;
      negated.literals.emplace_back(lit);
    }
    if (!negated.literals.empty())
      negated.literals.front().clauseBegin = true;

    for (auto clause : clauses()) {
      if (clause.size() <= 1)
        continue;

      SmallBoolExprCNF negatedNew;

      for (auto lit : clause) {
        auto pos = negatedNew.literals.size();
        for (auto origClause : negated.clauses()) {
          negatedNew.literals.push_back_range(Range{origClause});
        }
        lit.inverse = true;
        lit.clauseBegin = false;
        negatedNew.literals.emplace_back(lit);
        negatedNew.literals[pos].clauseBegin = true;
      }

      negated = std::move(negatedNew);
    }
    return negated;
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
    literals.push_back_range(Range{other.literals});
  }
  void addAsGlobalOR(SmallBoolExprCNF &other, uint numLiterals) {
    // todo properly
    auto lhs = this->negated2();
    // lhs.dump();
    lhs.simplify(numLiterals);
    // lhs.dump();
    auto rhs = other.negated2();
    // rhs.dump();
    rhs.simplify(numLiterals);
    // rhs.dump();

    lhs.addAsGlobalAND(rhs);
    // lhs.dump();
    lhs.simplify(numLiterals);
    // lhs.dump();

    *this = lhs.negated2();
    // this->dump();
    this->simplify(numLiterals);
    // this->dump();
  }

  /*
  // this is assuming we split the expression. One case is where the reduction
  // AND of bound vars is one, the other zero.
  auto evalWithBoundVars(ArrayRef<Optional<uint8_t>> boundVars,
                         bool singleBound) {
    SmallBoolExprCNF outTrue;
    SmallBoolExprCNF outFalse;

    // in the true term we know that all bound vars are as specified.
    // in the false term we know that not all bound vars are as specified (but
    // some may).
    // -> we can reason about true a lot better.

    // result of expr if all bound vars are as specified
    std::optional<bool> ifBoundTrue = std::nullopt;

    // result if not.
    std::optional<bool> ifBoundFalse = std::nullopt;

    // if the formula is unsatisfiable in one branch, can drop it from there.
    // if the formula is true in one branch, add value as leaf node there
    // (resolved).

    for (auto clause : clauses()) {
      if (clause.size() == 1) {
        if (boundVars[clause.front().id]) {
          // if equal to bound just drop but keep clause
          // if not equal to bound it's UNSAT
          bool eq = (!clause.front().inverse) == *boundVars[clause.front().id];
          if (!eq) {
            ifBoundTrue = false;
          } else {
            if (singleBound)
              ifBoundFalse = false;
          }
          goto outer_cont;
        }
      }

      for (auto &lit : clause) {
        if (boundVars[lit.id]) {
          bool eq = (!lit.inverse) == *boundVars[lit.id];
          // if equal we can drop the expr in the true output expr
          // if not equal we can drop the literal in the true output expr
          if (eq)
            ; // drop term, do nothing
          else {
            auto pos = outTrue.literals.size();
            outTrue.literals.push_back_range(Range{clause.begin(), &lit});
            outTrue.literals.push_back_range(Range{&lit + 1, clause.end()});
            outTrue.literals[pos].clauseBegin = true;
          }

          // do the same in false branch if single bound.
          if (singleBound) {
            if (!eq)
              ; // drop term, do nothing
            else {
              auto pos = outFalse.literals.size();
              outFalse.literals.push_back_range(Range{clause.begin(), &lit});
              outFalse.literals.push_back_range(Range{&lit + 1, clause.end()});
              outFalse.literals[pos].clauseBegin = true;
            }
          } else {
            outFalse.literals.push_back_range(Range{clause});
          }
          // todo: simplify multiple in clause, don't immediately jump to next.
          goto outer_cont;
        }
      }

      outTrue.literals.push_back_range(Range{clause});
      outFalse.literals.push_back_range(Range{clause});

    outer_cont:;
    }

    if (ifBoundTrue && *ifBoundTrue == false)
      outTrue.literals.clear();
    if (ifBoundFalse && *ifBoundFalse == false)
      outFalse.literals.clear();

    bool trueBranchSat = !ifBoundTrue || (*ifBoundTrue == true);
    bool falseBranchSat = !ifBoundFalse || (*ifBoundFalse == true);

    return std::make_tuple(outTrue, trueBranchSat, outFalse, falseBranchSat);
  }*/

  auto evalWithBoundVars2(SmallBoolExprCNF &orig, SmallBoolExprCNF &expr,
                          uint numLiterals) {
    // bool isPureConj = true;
    // size_t numClauses = 0;
    // for (auto clause : expr.clauses()) {
    //   if (clause.len == 0)
    //     isPureConj = false;
    //   numClauses++;
    // }
    // bool isPureDisj = numClauses++;

    // if (isPureConj || isPureDisj) {
    //   SmallBoolExprCNF negExpr = expr.negated();
    //   SmallBoolExprCNF *bufExpr = &expr;
    //   SmallBoolExprCNF *invExpr = &negExpr;

    //   UnsizedBitSet<SmallVec<uint64_t, 2>> purgeLits;
    //   for (auto clause : expr.clauses())
    //     purgeLits.setDyn(clause.front().id);

    //   SmallBoolExprCNF outTrue = orig;
    //   outTrue.addAsGlobalAND(*bufExpr);
    //   outTrue.dump();
    //   outTrue.simplify(numLiterals);
    //   outTrue.dump();
    //   bool trueBranchSat = !outTrue.isUnsat();
    //   if (trueBranchSat && isPureConj) {
    //     outTrue.purgeLiterals(purgeLits);
    //   }
    //   outTrue.dump();

    //   SmallBoolExprCNF outFalse = orig;
    //   outFalse.addAsGlobalAND(*invExpr);
    //   outFalse.dump();
    //   outFalse.simplify(numLiterals);
    //   outFalse.dump();
    //   bool falseBranchSat = !outFalse.isUnsat();
    //   if (falseBranchSat && isPureDisj) {
    //     outFalse.purgeLiterals(purgeLits);
    //   }
    //   outFalse.dump();

    //   return std::make_tuple(outTrue, trueBranchSat, outFalse,
    //   falseBranchSat);

    // } else {
    SmallBoolExprCNF outTrue = orig;
    //outTrue.dump();
    auto trueBranchRes = outTrue.simplifyWith(expr, numLiterals);
    //outTrue.dump();
    bool trueBranchSat = !trueBranchRes.has_value() || trueBranchRes.value();

    SmallBoolExprCNF exprNeg = expr.negated2();
    exprNeg.simplify(numLiterals);

    SmallBoolExprCNF outFalse = orig;
    //outFalse.dump();
    auto falseBranchRes = outFalse.simplifyWith(exprNeg, numLiterals);
    //outFalse.dump();
    bool falseBranchSat = !falseBranchRes.has_value() || falseBranchRes.value();

    return std::make_tuple(outTrue, trueBranchSat, outFalse, falseBranchSat);
    //}
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
class MuxTreeOptimizationPass {

  SlabAllocator<MuxTree> muxTreeAlloc;

  HWContext &ctx;
  HWInstrBuilder build;

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
        printMuxTree(tree);
      } while (cont);
    }
  }

  MuxTree *analyzeMuxTree(InstrRef root) {
    SmallVec<std::tuple<HWValue, uint32_t>, 32> worklist{
        {root.def(0)->as<WireRef>(), 1}};
    // SmallVec<BoolExprLiteral, 8> prefix;

    SmallVec<SmallBoolExprCNF, 4> prefixes;

    MuxTree *muxtree = muxTreeAlloc.allocate();
    muxtree->root = root;

    SmallDenseMap<MuxTree::InputSignal, uint32_t, 2> conditionsDedupeMap;
    auto getCondIdx = [&](WireRef cond, uint32_t bit) {
      MuxTree::InputSignal val{cond, bit};
      auto [found, it] = conditionsDedupeMap.findOrInsert(val, [&]() {
        auto rv = muxtree->conditions.size();
        muxtree->conditions.emplace_back(val);
        return rv;
      });
      return it.val();
    };

    while (!worklist.empty()) {
      auto [val, idx] = worklist.back();
      if (val.is<ConstantRef>()) {
        SmallBoolExprCNF expr;
        for (auto &prefix : prefixes)
          expr.addAsGlobalAND(prefix);
        muxtree->entries.emplace_back(expr, val.as<FatDynObjRef<>>());
        worklist.pop_back();
        continue;
      }
      auto asWire = val.as<WireRef>();
      if (!asWire.getSingleDef()->instr().isOpc(HW_MUX)) {
        SmallBoolExprCNF expr;
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

      // on first touch push prefix.
      if (operand == instr.other(1)) {
        auto &prefix = prefixes.emplace_back();
        WireRef selWire = instr.other(0)->as<WireRef>();
        if (auto instr = selWire.getDefI();
            instr.isOpc(OP_ICMP_EQ) && instr.other(1)->is<ConstantRef>()) {
          selWire = selWire.getDefI().other(0)->as<WireRef>();
          auto constant = instr.other(1)->as<ConstantRef>();

          for (uint i = 0; i < constant.getNumBits(); i++) {
            switch (constant.getBit(i)) {
            case FourState::S0:
              prefix.literals.emplace_back(getCondIdx(selWire, i), 1, 1);
              break;
            case FourState::S1:
              prefix.literals.emplace_back(getCondIdx(selWire, i), 0, 1);
              break;
            case FourState::SZ:
            case FourState::SX:
              break;
            }
          }

        } else
          prefix.literals.emplace_back(getCondIdx(selWire, 0), 0, 1);
      }

      switch (*instr.getDialectOpcode()) {
      // case *HW_SELECT: {
      //   prefix.back().rhs = operand->thin();
      //   if (operand + 1 == instr.other_end()) {
      //     worklist.back().second += 1;
      //     worklist.emplace_back(operand->as<HWValue>(), 1);
      //     prefix.back().lhs = ConstantRef::fromBool(0);
      //     prefix.back().rhs = ConstantRef::fromBool(0);
      //   } else {
      //     worklist.back().second += 2;
      //     worklist.emplace_back((operand + 1)->as<HWValue>(), 1);
      //   }
      //   break;
      // }
      case *HW_MUX: {
        std::get<1>(worklist.back()) += 1;
        worklist.emplace_back(operand->as<HWValue>(), 1);
        if (operand != instr.other(1)) {
          prefixes.back() = prefixes.back().negated2();
        }
        break;
      }
      default:
        dyno_unreachable("invalid instr");
      }
    }

    printMuxTree(muxtree);

    return muxtree;
  }

  struct Selector {
    HWValue signal;
    SmallVec<InstrRef, 4> depending;
  };

  struct MuxTree2 {
    SmallVec<Selector, 4> selector;
  };

  auto analyzeMuxTree2(InstrRef root) {
    SmallVec<InstrRef, 32> worklist{root};

    SmallDenseMap<ObjRef<Wire>, SmallVec<InstrRef, 4>, 1> depending;

    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      auto selectVar = instr.other(0)->as<WireRef>();
      if (auto def = selectVar.getSingleDef()->instr(); def.isOpc(OP_ICMP_EQ)) {
        selectVar = def.other(0)->as<WireRef>();
      }
      depending[selectVar].emplace_back(instr);

      for (auto operand : instr.others())
        if (auto asWire = operand->dyn_as<WireRef>();
            asWire && asWire.getDefI().isOpc(HW_MUX)) {
          worklist.emplace_back(asWire.getDefI());
        }
    }

    for (auto [obj, vec] : depending) {
      auto wire = ctx.getWires().resolve(obj);
      dbgs() << "input: ";
      dumpInstr(wire.getDefI());
      dbgs() << "depends: {\n";
      for (auto dep : vec)
        dumpInstr(dep);
      dbgs() << "}\n";
    }
    dbgs() << "\n\n";
    // return MuxTree2{};
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
      // entry.expr.dump();
    }

    tree->entries = std::move(newEntries);
    // printMuxTree(tree);
  }

  void printMuxTree(MuxTree *tree) {
    DEBUG("MuxTreeOptimization", {
      dbgs() << "mux tree at: ";
      if (tree->root)
        dumpInstr(tree->root);
      else
        dbgs() << "\n";
      for (auto entry : tree->entries) {

        entry.expr.dump(false);
        dbgs() << ": ";

        // for (auto [i, match] : Range{entry.expr.literals}.enumerate()) {
        //   auto &cond = tree->conditions[match.id];
        //   if (match.clauseBegin)
        //     dbgs() << (i == 0 ? "(" : ") && (");
        //   else if (i != 0)
        //     dbgs() << " || ";
        //   dumpObj(ctx.resolveObj(cond.wire));
        //   dbgs() << "[" << cond.idx << "]";
        //   dbgs() << (match.inverse ? " == 0" : " == 1");
        // }
        // dbgs() << "): ";
        dumpObj(ctx.resolveObj(entry.output));
        dbgs() << "\n";
      }
    });
  }

  // heuristic to select expression to MUX on next. You can e.g. just return
  // tree->entry[0].expr to always select between element 0 and the rest, thus
  // building a linear MUX chain. What this actually does is greedily peel of
  // elements that resolve the assignment for an entire var. If there's none of
  // those left, it just selects the single boolean signal as close to 50/50 as
  // possible.
  SmallBoolExprCNF getBestMUXExpr(MuxTree *tree) {
    uint maxNumSingleton = 0;
    uint maxNumSingletonIdx;
    for (auto [idx, entry] : Range{tree->entries}.enumerate()) {
      auto inv = entry.expr.negated2();
      inv.simplify(tree->conditions.size());

      uint numSingleton = 0;
      for (auto clause : inv.clauses())
        if (clause.size() == 1)
          numSingleton++;

      // dbgs() << "entry #" << idx << " would give " << numSingleton
      //        << " singletons\n";

      if (numSingleton > maxNumSingleton) {
        maxNumSingletonIdx = idx;
        maxNumSingleton = numSingleton;
      }
    }
    if (maxNumSingleton != 0) {
      return tree->entries[maxNumSingletonIdx].expr;
    }

    dbgs() << "no singletons found, instead selecting max entropy single bit\n";

    uint bestLitIdx = 0;
    uint bestTotal = UINT_MAX;
    double bestRatio = 1.0;

    for (uint litIdx = 0; litIdx < tree->conditions.size(); litIdx++) {
      SmallBoolExprCNF testExpr;
      testExpr.literals.emplace_back(
          BoolExprLiteral{uint16_t(litIdx), 0, true});
      SmallBoolExprCNF testExprInv = testExpr.negated2();

      uint numTrue = 0;
      uint numFalse = 0;

      for (auto &entry : tree->entries) {
        SmallBoolExprCNF cond = entry.expr;
        cond.addAsGlobalAND(testExpr);
        // dbgs() << "checking true case: ";
        // cond.dump();
        auto trueRes = cond.simplify(tree->conditions.size());
        if (trueRes.has_value() && trueRes.value() == false) {
          numFalse++;
          continue;
        }
        cond = entry.expr;
        cond.addAsGlobalAND(testExprInv);
        // dbgs() << "checking false case: ";
        // cond.dump();
        auto falseRes = cond.simplify(tree->conditions.size());
        if (falseRes.has_value() && falseRes.value() == false) {
          numTrue++;
          continue;
        }
      }

      uint total = numTrue + numFalse;
      if (total != tree->entries.size()) {
        uint diff = tree->entries.size() - total;
        numTrue += diff;
        numFalse += diff;
        total += 2 * diff;
      }

      double ratio = (double)numTrue / total;
      ratio = std::min(ratio, 1.0 - ratio);
      if (total < bestTotal && ratio < bestRatio) {
        bestLitIdx = litIdx;
        bestTotal = total;
        bestRatio = ratio;
      }
    }

    SmallBoolExprCNF selExpr;
    selExpr.literals.emplace_back(uint16_t(bestLitIdx), 0, true);
    return selExpr;
  }

  std::pair<WireRef, bool> getLiteralVal(MuxTree *tree, BoolExprLiteral cond) {
    WireRef out = ctx.getWires().resolve(tree->conditions[cond.id].wire);
    if (out.getNumBits() != 1)
      out = build.buildSplice(out, BitRange{tree->conditions[cond.id].idx, 1u})
                .as<WireRef>();

    return std::make_pair(out, !!cond.inverse);
  }

  HWValue getExprVal(MuxTree *tree, SmallBoolExprCNF &expr) {
    SmallVec<HWValue, 4> andOperands;
    for (auto clause : expr.clauses()) {
      SmallVec<HWValue, 4> orOperands;
      for (auto lit : clause) {
        auto [val, inv] = getLiteralVal(tree, lit);
        if (inv)
          val = build.buildNot(val).as<WireRef>();
        orOperands.emplace_back(val);
      }
      if (orOperands.size() == 1)
        andOperands.emplace_back(orOperands.front());
      else
        andOperands.emplace_back(
            build.buildInstr(OP_OR, true, ArrayRef{orOperands}).defW());
    }
    HWValue sel;
    if (andOperands.size() == 1)
      sel = andOperands[0];
    else
      sel = build.buildInstr(OP_AND, true, ArrayRef{andOperands}).defW();

    return sel;
  }

  template <std::invocable<MutArrayRef<BoolExprLiteral>> Func>
  void forAllClauses(MuxTree::Entry &entry, Func &&func) {
    for (auto clause : entry.expr.clauses()) {
      func(MutArrayRef{clause.begin(), clause.end()});
    }
  }

  HWValue lowerMuxTreeSimple(MuxTree *tree) {
    // left = false, right = true
    if (tree->root)
      build.setInsertPoint(tree->root);

    if (tree->entries.size() == 1) {
      return ctx.resolveObj(tree->entries.front().output);
    }

    printMuxTree(tree);
    if (tree->entries.size() == 2) {
      return build.buildMux(getExprVal(tree, tree->entries[0].expr),
                            ctx.resolveObj(tree->entries[0].output),
                            ctx.resolveObj(tree->entries[1].output));
    }

    auto selExpr = getBestMUXExpr(tree);
    DEBUG("MuxTreeOptimization", {
      dbgs() << "splitting tree on: ";
      selExpr.dump();
    });
    dbgs() << "\n\n";

    MuxTree *left = muxTreeAlloc.allocate();
    MuxTree *right = muxTreeAlloc.allocate();

    left->conditions = tree->conditions;
    left->root = nullref;

    right->conditions = tree->conditions;
    right->root = nullref;

    // only valid if the corresponding tree is empty.
    DynObjRef leftSingleOutput = nullref;
    DynObjRef rightSingleOutput = nullref;

    // go thru entries. consider the entry expression in the cases that the
    // expression we MUX on is true (right subtree) or false (left subtree). If
    // the entry expression becomes unsatisfiable in either subtree we can
    // safely discard it.
    for (auto entry : tree->entries) {
      // decide whether to put entry in left, right or both.
      assert(!entry.expr.isUnsat());
      auto [ifTrueRem, trueBrSat, ifFalseRem, falseBrSat] =
          entry.expr.evalWithBoundVars2(entry.expr, selExpr,
                                        tree->conditions.size());

      if (!trueBrSat && !falseBrSat)
        continue;

      if (trueBrSat)
        rightSingleOutput = entry.output;
      if (falseBrSat)
        leftSingleOutput = entry.output;

      if (falseBrSat) {
        left->entries.emplace_back(
            MuxTree::Entry{std::move(ifFalseRem), entry.output});
      }
      if (trueBrSat) {
        right->entries.emplace_back(
            MuxTree::Entry{std::move(ifTrueRem), entry.output});
      }
    }

    DEBUG("MuxTreeOptimization", {
      dbgs() << "left subtree\n";
      printMuxTree(left);

      dbgs() << "right subtree\n";
      printMuxTree(right);
    })

    HWValue leftVal;
    HWValue rightVal;

    if (left->entries.size() == 0)
      leftVal = ctx.resolveObj(leftSingleOutput);
    else
      leftVal = lowerMuxTreeSimple(left);

    if (right->entries.size() == 0)
      rightVal = ctx.resolveObj(rightSingleOutput);
    else
      rightVal = lowerMuxTreeSimple(right);

    auto sel = getExprVal(tree, selExpr);
    return build.buildMux(sel, rightVal, leftVal);
  }

  void runOnProcess(ProcessIRef proc) {
    // assume proc is already flat for now, ignore SCF constructs.
    for (auto instr :
         Range{proc.block().begin(), proc.block().end()}.reverse()) {
      switch (*instr.getDialectOpcode()) {
        // case *HW_SELECT:
        //   analyzeMuxTree(instr);
        //   break;

      case *HW_MUX:
        auto muxtree = analyzeMuxTree(instr);
        simplifyConditions(muxtree);
        dedupeMuxTreeOutputs(muxtree);
        printMuxTree(muxtree);
        auto wire = lowerMuxTreeSimple(muxtree);
        instr.def(0)->as<WireRef>().replaceAllUsesWith(wire);
        return;
        break;
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit MuxTreeOptimizationPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};
}; // namespace dyno

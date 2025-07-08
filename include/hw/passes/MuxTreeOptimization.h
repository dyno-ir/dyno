#pragma once

#include "dyno/Constant.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/SlabAllocator.h"
#include <bit>

namespace dyno {

struct BoolExprLiteral {
  uint16_t id : 14;
  uint16_t inverse : 1;
  uint16_t clauseBegin : 1;

  BoolExprLiteral canonicalKey() const {
    auto rv = *this;
    rv.inverse = false;
    rv.clauseBegin = false;
    return rv;
  }
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

  bool simplifyCondition() {
    SmallVec<ClauseRef, 8> terms;
    for (auto clause : clauses()) {
      if (clause.size() == 0)
        continue;
      terms.emplace_back(clause);
    }

    for (auto &term : terms) {
      term.front().clauseBegin = false;
      std::sort(term.begin(), term.end(),
                [](const BoolExprLiteral &lhs, const BoolExprLiteral &rhs) {
                  return lhs.id < rhs.id;
                });
    }

    SmallDenseSet<BoolExprLiteral, 1> known;
    for (auto term : terms) {
      if (term.size() == 1) {
        known.findOrInsert(term.front());

        auto copy = term.front();
        copy.inverse = !copy.inverse;
        // unsatisfiable
        if (known.contains(copy)) {
          literals.clear();
          return true;
        }
        continue;
      }
    }

    SmallVec<BoolExprLiteral, 8> output;
    output.push_back_range(
        Range{known}.transform([](size_t, BoolExprLiteral l) {
          l.clauseBegin = true;
          return l;
        }));

    size_t outputIdx = 0;
    for (size_t i = 0; i < terms.size(); i++) {
      if (terms[i].size() == 1)
        continue;
      for (auto term : terms[i]) {
        // todo: simple remove if known 0?
        if (known.contains(term))
          goto skip;
      }
      terms[outputIdx++] = terms[i];
    skip:;
    }
    terms.downsize(outputIdx);

    while (true) {
      for (size_t i = 0; i < terms.size(); i++) {
        for (size_t j = 0; j < terms.size(); j++) {
          auto termA = terms[i];
          if (termA.size() == 1)
            continue;
          if (i >= j)
            continue;
          auto termB = terms[j];
          if (termA.size() != termB.size())
            continue;

          Optional<size_t> oppositeIdx = nullopt;
          for (size_t k = 0; k < termA.size(); k++) {
            if (termA[k].id != termB[k].id)
              goto has_difference;
            if (termA[k].inverse != termB[k].inverse) {
              if (oppositeIdx)
                goto has_difference;
              oppositeIdx = k;
            }
          }

          if (!oppositeIdx) {
            output.push_back_range(Range{termA.begin(), termA.end()});
          } else {
            auto firstIdx = output.size();
            output.push_back_range(Range{termA.begin(), &termA[*oppositeIdx]});
            output.push_back_range(
                Range{&termA[*oppositeIdx] + 1, termA.end()});
            output[firstIdx].clauseBegin = true;
          }

          terms.erase(terms.begin() + j);
          terms.erase(terms.begin() + i);

          goto outer_cont;

        has_difference:;
        }
      }
      break;

    outer_cont:;
    }

    for (auto term : terms) {
      size_t firstIdx = output.size();
      output.push_back_range(Range{term});
      output[firstIdx].clauseBegin = true;
    }

    DEBUG("MuxTreeOptimization", {
      std::print(dbgs(), "simplified condition from {} to {} literals\n",
                 literals.size(), output.size());
    })
    auto rv = literals.size() != output.size();
    literals = std::move(output);
    return rv;
  }

  bool isUnsat() const { return literals.size() == 0; }

  void addAsGlobalAND(const SmallBoolExprCNF &other) {
    literals.push_back_range(Range{other.literals});
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

    negated.literals.front().clauseBegin = true;

    for (auto clause : clauses()) {
      if (clause.size() <= 1)
        continue;

      SmallBoolExprCNF negatedNew;

      for (auto lit : clause) {
        for (auto origClause : negated.clauses()) {
          negatedNew.literals.push_back_range(Range{origClause});
        }
        lit.inverse = true;
        lit.clauseBegin = false;
        negatedNew.literals.emplace_back(lit);
      }

      negated = std::move(negatedNew);
    }
    return negated;
  }

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
        if (!boundVars[clause.front().id])
          continue;
        // if equal to bound just drop but keep clause
        // if not equal to bound it's UNSAT
        bool eq = (!clause.front().inverse) == *boundVars[clause.front().id];
        if (!eq) {
          ifBoundTrue = false;
        } else {
          if (singleBound)
            ifBoundFalse = false;
        }
        continue;
      }

      for (auto &lit : clause) {
        if (!boundVars[lit.id])
          continue;
        bool eq = (!clause.front().inverse) == *boundVars[clause.front().id];
        // if equal we can drop the expr in the true output expr
        // if not equal we can drop the literal in the true output expr
        if (eq)
          ; // drop term, do nothing
        else {
          outTrue.literals.push_back_range(Range{clause.begin(), &lit});
          outTrue.literals.push_back_range(Range{&lit + 1, clause.end()});
        }

        // do the same in false branch if single bound.
        if (singleBound) {
          if (!eq)
            ; // drop term, do nothing
          else {
            outFalse.literals.push_back_range(Range{clause.begin(), &lit});
            outFalse.literals.push_back_range(Range{&lit + 1, clause.end()});
          }
        } else {
          outFalse.literals.push_back_range(Range{clause});
        }

        continue;
      }

      outTrue.literals.push_back_range(Range{clause});
      outFalse.literals.push_back_range(Range{clause});
    }

    if (*ifBoundTrue && ifBoundTrue == false)
      outTrue.literals.clear();
    if (*ifBoundFalse && ifBoundFalse == false)
      outFalse.literals.clear();

    return std::make_pair(outTrue, outFalse);
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
      bool cont;
      do {
        cont = entry.expr.simplifyCondition();
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
          prefixes.back() = prefixes.back().negated();
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

  // void dedupeMuxTreeOutputs(MuxTree *tree) {
  //   SmallDenseMap<DynObjRef, SmallVec<uint32_t, 2>, 2> dedupeMap;
  //   for (auto [i, rule] : Range{tree->entries}.enumerate()) {
  //     dedupeMap[rule.output].emplace_back(i);
  //   }
  //   SmallVec<MuxTree::Entry, 4> newEntries;
  //   newEntries.reserve(dedupeMap.size());
  //   for (auto [out, rules] : dedupeMap) {
  //     auto &entry = newEntries.emplace_back(SmallVec<MatchPair, 2>{}, out);
  //     for (auto rule : rules) {
  //       uint first = entry.matches.size();
  //       entry.matches.push_back_range(Range{tree->entries[rule].matches});
  //       entry.matches[first].newClause = true;
  //     }
  //   }

  //   tree->entries = std::move(newEntries);
  //   printMuxTree(tree);
  // }

  void printMuxTree(MuxTree *tree) {
    DEBUG("MuxTreeOptimization", {
      dbgs() << "mux tree at: ";
      if (tree->root)
        dumpInstr(tree->root);
      for (auto entry : tree->entries) {
        for (auto [i, match] : Range{entry.expr.literals}.enumerate()) {
          auto &cond = tree->conditions[match.id];
          if (match.clauseBegin)
            dbgs() << (i == 0 ? "(" : ") && (");
          else if (i != 0)
            dbgs() << " || ";
          dumpObj(ctx.resolveObj(cond.wire));
          dbgs() << "[" << cond.idx << "]";
          dbgs() << (match.inverse ? " == 0" : " == 1");
        }
        dbgs() << "): ";
        dumpObj(ctx.resolveObj(entry.output));
        dbgs() << "\n";
      }
    });
  }

  BoolExprLiteral getMostUsedCondition(MuxTree *tree) {
    SmallDenseMap<BoolExprLiteral, std::array<uint16_t, 2>, 2> uses;
    for (auto entry : tree->entries) {
      for (auto cond : entry.expr.literals) {
        auto copy = cond;
        copy.inverse = 0;
        copy.clauseBegin = 0;
        uses[copy][cond.inverse]++;
      }
    }

    DEBUG("MuxTreeOptimization", {
      for (auto [cond, numUses] : uses) {
        dbgs() << "condition #" << cond.id << " used " << numUses[1] << "/"
               << numUses[0] << " times\n";
      }
    })
    BoolExprLiteral max;
    uint maxNumUses = 0;
    for (auto [cond, numUses] : uses) {
      if (numUses[0] + numUses[1] > maxNumUses) {
        max = cond;
        maxNumUses = numUses[0] + numUses[1];
      }
    }
    return max;
  }

  std::pair<WireRef, bool> getConditionVal(MuxTree *tree,
                                           BoolExprLiteral cond) {
    WireRef out = ctx.getWires().resolve(tree->conditions[cond.id].wire);
    if (out.getNumBits() != 1)
      out = build.buildSplice(out, BitRange{tree->conditions[cond.id].idx, 1u})
                .as<WireRef>();

    return std::make_pair(out, !!cond.inverse);
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
      SmallVec<HWValue, 4> andOperands;
      forAllClauses(tree->entries[0], [&](ArrayRef<BoolExprLiteral> term) {
        if (term.size() == 0)
          return;
        SmallVec<HWValue, 4> orOperands;
        for (auto lit : term) {
          auto [val, inv] = getConditionVal(tree, lit);
          if (inv)
            val = build.buildXNor(val).as<WireRef>();
          orOperands.emplace_back(val);
        }
        if (orOperands.size() == 1)
          andOperands.emplace_back(orOperands.front());
        else
          andOperands.emplace_back(
              build.buildInstr(OP_OR, true, ArrayRef{orOperands}).defW());
      });
      HWValue sel;
      if (andOperands.size() == 1)
        sel = andOperands[0];
      else
        sel = build.buildInstr(OP_AND, true, ArrayRef{andOperands}).defW();

      return build.buildMux(sel, ctx.resolveObj(tree->entries[0].output),
                            ctx.resolveObj(tree->entries[1].output));
    }

    auto cond = getMostUsedCondition(tree);
    DEBUG("MuxTreeOptimization", {
      dbgs() << "splitting tree on:";
      dumpObj(ctx.resolveObj(tree->conditions[cond.id].wire));
      dbgs() << "[" << tree->conditions[cond.id].idx << "]\n";
    });
    dbgs() << "\n\n";

    MuxTree *left = muxTreeAlloc.allocate();
    MuxTree *right = muxTreeAlloc.allocate();

    left->conditions = tree->conditions;
    left->root = nullref;

    right->conditions = tree->conditions;
    right->root = nullref;

    // only valid if the corresponding tree is empty.
    DynObjRef leftSingleOutput;
    DynObjRef rightSingleOutput;

    for (auto entry : tree->entries) {
      // decide whether to put entry in left, right or both.
      SmallVec<BoolExprLiteral, 4> newConds;
      SmallVec<BoolExprLiteral, 2> newCondsTrueOnly;
      SmallVec<BoolExprLiteral, 2> newCondsFalseOnly;
      std::optional<bool> subtree;
      assert(!entry.expr.isUnsat());
      bool unsat = false;

      auto handleTerm = [&](ArrayRef<BoolExprLiteral> term) {
        // for (auto lit : term) {
        //   dbgs() << "{idx=" << lit.idx << ", bit=" << lit.bit << "}, ";
        // }
        // dbgs() << "\n";

        if (term.size() == 0)
          return;
        if (term.size() == 1) {
          if (term[0].id == cond.id) {
            bool val = !term[0].inverse;
            if (subtree.has_value() && *subtree != val)
              unsat = true;
            subtree = val;
            (!val ? leftSingleOutput : rightSingleOutput) = entry.output;
            return;
          }
        }

        for (auto [i, lit] : Range{term}.enumerate()) {
          if (lit.id == cond.id) {

            bool val = !!lit.inverse;

            auto &arr = (val ? newCondsTrueOnly : newCondsFalseOnly);
            auto idx = arr.size();
            for (uint j = 0; j < term.size(); j++) {
              if (i == j)
                continue;
              arr.emplace_back(term[j]);
            }
            arr[idx].clauseBegin = true;

            (val ? leftSingleOutput : rightSingleOutput) = entry.output;
            return;
          }
        }
        for (auto lit : term)
          newConds.emplace_back(lit);
      };
      forAllClauses(entry, handleTerm);

      if (unsat)
        continue;

      if ((!subtree || subtree == false) &&
          newConds.size() + newCondsFalseOnly.size() != 0) {
        auto &leftEntry = left->entries.emplace_back(
            MuxTree::Entry{SmallBoolExprCNF{newConds}, entry.output});
        leftEntry.expr.literals.push_back_range(Range{newCondsFalseOnly});
      }
      if ((!subtree || subtree == true) &&
          newConds.size() + newCondsTrueOnly.size() != 0) {
        auto &rightEntry = right->entries.emplace_back(
            MuxTree::Entry{SmallBoolExprCNF{newConds}, entry.output});
        rightEntry.expr.literals.push_back_range(Range{newCondsTrueOnly});
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

    auto [sel, inv] = getConditionVal(tree, cond);
    if (inv)
      std::swap(leftVal, rightVal);

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

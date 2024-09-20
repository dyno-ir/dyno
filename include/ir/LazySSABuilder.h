#pragma once

#include "ir/IR.h"
#include "ir/SSAIRBuilder.h"
#include <cassert>
#include <unordered_map>

// Construct SSA using lazy reaching definitions analysis
// Based on approach by M. Braun et al:
// https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf

class SSARenumberTable {
public:
  Operand *get(SymbolId id) {
    auto it = symbols.find(id);
    if (it == symbols.end()) {
      return nullptr;
    }
    return it->second;
  }

  void set(SymbolId id, Operand &def) { symbols[id] = &def; }
  void set(SymbolId id, Operand *def) {
    if (!def)
      return;
    set(id, *def);
  }

  std::unordered_map<SymbolId, Operand *> symbols;
  std::vector<std::pair<Instr *, SymbolId>> incompletePhis;
  bool marked = false;
  bool sealed = false;
};

class LazySSABuilder {
public:
  static Operand *load(SymbolId id, SSAType &type, CFGBlock &block) {
    auto &renum = SSARenumberTable::getTable(block);
    Operand *def = renum.get(id);
    if (def) {
      assert(def->ssaDef().type() == type);
      return def;
    }
    if (!renum.sealed) {
      auto phi = SSAIRBuilder(block.getFirstSentry()).emitPhi(type);
      def = &phi->getDef();
      renum.set(id, def);
      renum.incompletePhis.emplace_back(phi.get(), id);
      return def;
    }
    unsigned numPreds = block.getNumPredecessors();
    if (numPreds == 0) {
      return nullptr;
    }
    if (numPreds == 1) {
      def = load(id, type, block.operand().ssaDef().begin()->getParentBlock());
      if (!def)
        return nullptr;
      assert(def->ssaDef().type() == type);
      renum.set(id, def);
      return def;
    }
    if (renum.marked) {
      /*
      auto phi = IRBuilder().emitPhi();
      phi.setupOperands(numPreds);
      def = &phi->getOperandUnchecked(0);
      insert
      renum.set(id, def);
      return def;
      */
      assert(false && "Endless recursion");
      return nullptr;
    }
    renum.marked = true;
    auto phi = SSAIRBuilder(block.getFirstSentry()).emitPhi(type);
    def = &phi->getDef();
    renum.set(id, def);
    populatePhi(phi, id);
    def = &foldPhi(phi);
    renum.set(id, def);
    renum.marked = false;
    return def;
  }

  static void store(SymbolId id, Operand &def, CFGBlock &block) {
    auto &renum = SSARenumberTable::getTable(block);
    renum.set(id, def);
  }

  static void sealBlock(CFGBlock &block) {
    auto &renum = SSARenumberTable::getTable(block);
    assert(!renum.sealed);
    renum.sealed = true;
    for (auto [instr, id] : renum.incompletePhis) {
      populatePhi(PhiInstrPtr(instr), id);
      Operand &foldedDef = foldPhi(PhiInstrPtr(instr));
      renum.set(id, foldedDef);
    }
  }

private:
  static Operand &foldPhi(PhiInstrPtr phi) {
    Operand *nonSelfDef = nullptr;
    for (unsigned i = 0, iEnd = phi.getNumPredecessors(); i < iEnd; ++i) {
      Operand &op = phi.getPredecessorDef(i);
      if (&op == &phi->getDef() || &op == nonSelfDef) {
        continue;
      }
      if (nonSelfDef) {
        return phi->getDef();
      }
      nonSelfDef = &op;
    }
    assert(nonSelfDef && "Phi must use at least one real value");
    std::vector<Instr *> worklist;
    for (auto &use : phi->getDef().ssaDef()) {
      if (!use.getParent().isPhi() || &use.getParent() == phi.get()) {
        continue;
      }
      worklist.push_back(&use.getParent());
    }
    phi->getDef().ssaDef().replaceAllUses(*nonSelfDef);
    phi->deleteThis();
    for (auto *instr : worklist) {
      assert(false && "Update renum");
      foldPhi(PhiInstrPtr(instr));
    }
    return *nonSelfDef;
  }

  static void populatePhi(PhiInstrPtr phi, SymbolId id) {
    SSAType &type = phi->getDef().ssaDef().type();
    phi.setupPredecessors(phi->getParent().getNumPredecessors());
    unsigned predNum = 0;
    for (auto &use : phi->getParent().operand().ssaDef()) {
      CFGBlock &predBlock = use.getParentBlock();
      Operand *predDef = load(id, type, predBlock);
      if (!predDef) {
        predDef = &SSAIRBuilder(predBlock.getLast()).emitUndefined(type);
      }
      phi.setPredecessor(predNum, *predDef, predBlock);
      ++predNum;
    }
  }
};

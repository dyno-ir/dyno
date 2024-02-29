#pragma once

#include "ir/IR.h"
#include "ir/InstrBuilder.h"
#include <algorithm>
#include <cassert>
#include <memory>
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

  static void init(Block &block) {
    assert(!block.userData);
    block.userData = new SSARenumberTable();
  }
  static void destroy(Block &block) {
    delete static_cast<SSARenumberTable *>(block.userData);
    block.userData = nullptr;
  }

  static void destroy(Function &func) {
    for (auto &b : func) {
      destroy(b);
    }
  }

  static SSARenumberTable &getTable(Block &b) {
    if (!b.userData) {
      init(b);
    }
    return *static_cast<SSARenumberTable *>(b.userData);
  }

  std::unordered_map<SymbolId, Operand *> symbols;
  std::vector<std::pair<Instr *, SymbolId>> incompletePhis;
  bool marked = false;
  bool sealed = false;
};

class SSABuilder {
public:
  static Operand *load(SymbolId id, SSAType &type, Block &block) {
    auto &renum = SSARenumberTable::getTable(block);
    Operand *def = renum.get(id);
    if (def) {
      assert(def->ssaDefType() == type);
      return def;
    }
    if (!renum.sealed) {
      auto phi = InstrBuilder(block.getFirstSentry()).emitPhi(type);
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
      def = load(id, type, block.getDef().ssaDef().begin()->getParentBlock());
      if (!def)
        return nullptr;
      assert(def->ssaDefType() == type);
      renum.set(id, def);
      return def;
    }
    if (renum.marked) {
      /*
      auto phi = InstrBuilder().emitPhi();
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
    auto phi = InstrBuilder(block.getFirstSentry()).emitPhi(type);
    def = &phi->getDef();
    renum.set(id, def);
    populatePhi(phi, id);
    def = &foldPhi(phi);
    renum.set(id, def);
    renum.marked = false;
    return def;
  }

  static void store(SymbolId id, Operand &def, Block &block) {
    auto &renum = SSARenumberTable::getTable(block);
    renum.set(id, def);
  }

  static void sealBlock(Block &block) {
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
    phi.setupPredecessors(phi->getParent().getNumPredecessors());
    unsigned predNum = 0;
    for (auto &use : phi->getParent().getDef().ssaDef()) {
      Block &predBlock = use.getParentBlock();
      Operand *predDef = load(id, phi->getDef().ssaDefType(), predBlock);
      assert(predDef);
      phi.setPredecessor(predNum, *predDef, predBlock);
      ++predNum;
    }
  }
};

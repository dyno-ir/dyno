#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/Process.h"
#include "support/Bits.h"
#include <array>

namespace dyno {

class AIGNode {
  std::array<ObjID, 2> op;

public:
  AIGNode(ObjID lhs, ObjID rhs) : op{lhs, rhs} {}
};

class AIGNodeTRef : public ObjRef<AIGNode> {
public:
  using InvertField = BitField<ObjID::num_t, 1, 0>;
  using CustomField = BitField<ObjID::num_t, 1, 1>;
  using IdxField = BitField<ObjID::num_t, 30, 2>;

  auto invert() { return InvertField{obj.num}; }
  auto custom() { return CustomField{obj.num}; }
  auto idx() { return IdxField{obj.num}; }

  AIGNodeTRef inverted() {
    AIGNodeTRef rv = *this;
    rv.invert() = !rv.invert();
    return rv;
  }

  AIGNodeTRef(ObjID id) : ObjRef<AIGNode>(id) {}
};

class AIGNodeRef : public AIGNodeTRef {
  AIGNode *ptr;

public:
  AIGNode *operator->() { return ptr; }
  AIGNode &operator*() { return *ptr; }
  AIGNodeRef(AIGNodeTRef ref, AIGNode *ptr) : AIGNodeTRef(ref), ptr(ptr) {}
};

class AIGNodeStore {
  std::vector<AIGNode> objects;

public:
  AIGNode *resolve(AIGNodeTRef &ref) { return &objects[ref.idx()]; }

  template <typename... Args> AIGNodeRef create(Args... args) {
    uint32_t id = objects.size();
    AIGNode &node = objects.emplace_back(std::forward<Args>(args)...);
    return AIGNodeRef{AIGNodeTRef{ObjID{id}}, &node};
  }
};

class FatAIGNode {
  InstrDefUse defUse;
  AIGNode node;
};

class AIG {
public:
  AIGNodeStore store;
  AIGNodeRef createNode(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return store.create(lhs.getObjID(), rhs.getObjID());
  }
};

class AIGBuilder {
  AIG aig;
  ObjMapVec<Wire, ThinArrayRef<AIGNodeTRef>> wireToAIGNode;
  std::vector<AIGNodeTRef> wireToAIGNodeStorage;

public:
  ArrayRef<AIGNodeTRef> resolveWire(WireRef wire) {
    return wireToAIGNode[wire].resolve(ArrayRef{wireToAIGNodeStorage});
  }

  void buildAnd(WireRef out, HWValue lhs, HWValue rhs) {
    if (lhs.is<WireRef>() && rhs.is<WireRef>()) {
      auto lhsNodes = resolveWire(lhs.as<WireRef>());
      auto rhsNodes = resolveWire(rhs.as<WireRef>());

      for (uint i = 0; i < *lhs.as<WireRef>().getNumBits(); i++) {
        auto node = aig.createNode(lhsNodes[i], rhsNodes[i]);
        wireToAIGNodeStorage.emplace_back(node);
      }


    }
  }
};

class AIGConstructPass {
  HWContext &ctx;

  AIGBuilder abuild;

  void handleInstr(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
    case *HW_LOAD: {

      // make an AIG primary input
      break;
    }
    case *HW_STORE: {

      // AIG output
      break;
    }

    case *OP_AND: {
      break;
    }
    }
  }

  void runOnProc(ProcessIRef proc) {
    for (auto instr : proc.block()) {
      handleInstr(instr);
    }
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProc(proc);
    }
  }

public:
  void run() {
    for (auto mod : ctx.getModules()) {
    }
  }

  explicit AIGConstructPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno

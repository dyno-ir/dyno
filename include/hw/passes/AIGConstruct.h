#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/Process.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include <array>
#include <limits>

namespace dyno {

class AIGObjID : public ObjID {
public:
  constexpr static num_t FAT_ID_SPACE = 1 << 24;
  constexpr static num_t FAT_ID_START =
      std::numeric_limits<ObjID::num_t>::max() - FAT_ID_SPACE + 1;
  using ObjID::ObjID;
  using InvertField = BitField<ObjID::num_t, 1, 0>;
  using CustomField = BitField<ObjID::num_t, 1, 1>;
  using IdxField = BitField<ObjID::num_t, 30, 2>;
  using IdxFieldC = BitField<const ObjID::num_t, 30, 2>;

  auto invert() { return InvertField{num}; }
  auto custom() { return CustomField{num}; }
  auto idx() { return IdxField{num}; }
  auto idx() const { return IdxFieldC{num}; }
  bool isSpecial() const { return idx() > FAT_ID_START; }

  AIGObjID(uint32_t idx, bool invert = false, bool custom = false) {
    this->idx() = idx;
    this->invert() = invert;
    this->custom() = custom;
  }
};

class AIGNode {
  std::array<ObjID, 2> op;

public:
  AIGNode(AIGObjID lhs, AIGObjID rhs) : op{lhs, rhs} {}
  AIGNode() = default;

  static constexpr uint32_t hash(AIGNode node) {
    // todo specialized hashing
    return hash_combine(hash_u32(node.op[0]), hash_u32(node.op[1]));
  }
};

class AIGNodeTRef : public ObjRef<AIGNode> {
public:
  auto invert() { return AIGObjID{obj}.invert(); }
  auto custom() { return AIGObjID{obj}.custom(); }
  auto idx() { return AIGObjID{obj}.idx(); }
  bool isSpecial() { return AIGObjID{obj}.isSpecial(); };
  AIGObjID getObjID() const { return AIGObjID{obj}; }

  AIGNodeTRef inverted() const {
    AIGNodeTRef rv = *this;
    rv.invert() = !rv.invert();
    return rv;
  }

  AIGNodeTRef(ObjID id) : ObjRef<AIGNode>(id) {}

  AIGNodeTRef(uint32_t idx, bool invert = false, bool custom = false)
      : ObjRef<AIGNode>(AIGObjID{idx, invert, custom}) {}
};

// maybe this can just be a FatObjRef?
class AIGNodeRef : public AIGNodeTRef {
  AIGNode *ptr;

public:
  AIGNode *operator->() { return ptr; }
  AIGNode &operator*() { return *ptr; }
  AIGNodeRef(AIGNodeTRef ref, AIGNode *ptr) : AIGNodeTRef(ref), ptr(ptr) {}
  AIGNode *getPtr() { return ptr; }
};

class FatAIGNode {
public:
  InstrDefUse defUse;
  AIGNode node;

  FatAIGNode(ObjRef<FatAIGNode>) {};
  FatAIGNode(DynObjRef, AIGObjID lhs, AIGObjID rhs) : node(lhs, rhs) {}
};

class FatAIGNodeRef : public FatObjRef<FatAIGNode> {
public:
  using FatObjRef<FatAIGNode>::FatObjRef;
  explicit operator AIGNodeRef() { return AIGNodeRef{obj, &(*this)->node}; }
  FatAIGNodeRef(FatObjRef<FatAIGNode> ref) : FatObjRef<FatAIGNode>(ref) {}
};

template <> struct ObjTraits<FatAIGNode> {
  using FatRefT = FatObjRef<FatAIGNode>;
};

class ThinAIGNodeStore {
  DedupeMap<AIGNode, std::vector<AIGNode>, AIGNode::hash> dedupeMap;

public:
  AIGNodeRef resolve(AIGNodeTRef ref) {
    return AIGNodeRef{ref, &dedupeMap.container[ref.idx()]};
  }
  template <typename... Args> AIGNodeRef create(Args... args) {
    uint32_t index = dedupeMap.getCanonicalIndex(AIGNode{args...});
    auto thin = AIGNodeTRef{index};
    return resolve(thin);
  }
};
template <typename BaseStore> class FatAIGNodeStore {
  BaseStore store;

public:
  using value_type = BaseStore::value_type;
  using FatRefT = FatAIGNodeRef;

  // transforms an AIGNodeRef into a FatObjRef<FatAIGNode>
  // only works for fat nodes ofc, and loses inversion/custom info.
  static FatObjRef<FatAIGNode> transformIn(AIGNodeRef ref) {
    assert(ref.isSpecial());
    ObjID id{ref.getObjID().idx() - AIGObjID::FAT_ID_START};
    // if we end up dealing a lot with these can encode inversion and user bit
    // into custom.
    return FatObjRef<FatAIGNode>{id, ref.getPtr()};
  }
  static ObjRef<FatAIGNode> transformIn(AIGNodeTRef ref) {
    assert(ref.isSpecial());
    ObjID id{ref.getObjID().idx() - AIGObjID::FAT_ID_START};
    return ObjRef<FatAIGNode>{id};
  }

  static FatAIGNodeRef transformOut(FatObjRef<FatAIGNode> ref,
                                    bool invert = false, bool custom = false) {
    AIGObjID id{ref.getObjID(), invert, custom};
    return FatAIGNodeRef{FatObjRef<FatAIGNode>{id, ref.getPtr()}};
  }

  template <typename... Args> FatAIGNodeRef create(Args... args) {
    return transformOut(store.create(std::forward<Args>(args)...));
  }

  FatAIGNodeRef resolve(AIGNodeTRef ref) {
    return transformOut(store.resolve(transformIn(ref)), ref.invert(),
                        ref.custom());
  }
};

class AIGNodeStore {
  ThinAIGNodeStore thin;
  FatAIGNodeStore<NewDeleteObjStore<FatAIGNode>> fat;

public:
  AIGNodeRef resolve(AIGNodeTRef ref) {
    if (ref.isSpecial()) [[unlikely]] {
      return fat.resolve(ref).as<AIGNodeRef>();
    }
    return thin.resolve(ref);
  }
  template <typename... Args> AIGNodeRef create(Args... args) {
    return thin.create(std::forward<Args>(args)...);
  }
  template <typename... Args> FatAIGNodeRef createSpecial(Args... args) {
    return fat.create(std::forward<Args>(args)...);
  }
};

class AIG {
public:
  AIGNodeStore store;

  AIGNodeRef createNode(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    if (lhs.getObjID() > rhs.getObjID())
      std::swap(lhs, rhs);
    return store.create(lhs.getObjID(), rhs.getObjID());
  }

  FatAIGNodeRef createInput() { return store.createSpecial(); }
  FatAIGNodeRef createOutput(AIGNodeTRef val) {
    // better not perform any logic in the output, so both inputs are just
    // the actual output value.
    return store.createSpecial(val.getObjID(), val.getObjID());
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

      uint32_t pos = wireToAIGNodeStorage.size();
      auto numBits = *lhs.as<WireRef>().getNumBits();
      for (uint i = 0; i < numBits; i++) {
        auto node = aig.createNode(lhsNodes[i], rhsNodes[i]);
        wireToAIGNodeStorage.emplace_back(node);
      }
      wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
    }
    assert(0 && "todo");
  }

  MutArrayRef<AIGNodeTRef> buildInput(WireRef wire) {
    auto numBits = *wire.getNumBits();
    auto arr = ThinArrayRef<AIGNodeTRef>{(uint32_t)wireToAIGNodeStorage.size(),
                                         numBits};
    for (uint i = 0; i < numBits; i++) {
      auto node = aig.createInput();
      wireToAIGNodeStorage.emplace_back(node.as<AIGNodeRef>());
    }
    wireToAIGNode[wire] = arr;
    return arr.resolve(MutArrayRef{wireToAIGNodeStorage});
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

#pragma once

#include "aig/IDs.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
#include "support/ArrayRef.h"
#include "support/DedupeMap.h"
#include "support/RTTI.h"

namespace dyno {

class AIGNodeTRef;

class AIGObjID : public ObjID {
public:
  constexpr static num_t FAT_ID_SPACE = 1u << 24;
  constexpr static num_t FAT_ID_START = (1u << 30) - FAT_ID_SPACE;
  using ObjID::ObjID;
  using InvertField = BitField<ObjID::num_t, 1, 0>;
  using CustomField = BitField<ObjID::num_t, 1, 1>;
  using IdxField = BitField<ObjID::num_t, 30, 2>;
  using IdxFieldC = BitField<const ObjID::num_t, 30, 2>;

  auto invert() { return InvertField{num}; }
  auto custom() { return CustomField{num}; }
  auto idx() { return IdxField{num}; }
  auto idx() const { return IdxFieldC{num}; }
  bool isSpecial() const { return idx() >= FAT_ID_START; }

  AIGObjID(uint32_t idx, bool invert = false, bool custom = false) {
    this->idx() = idx;
    this->invert() = invert;
    this->custom() = custom;
  }
  AIGObjID(ObjID obj) : ObjID(obj) {}
};

class AIGNode {
public:
  std::array<AIGObjID, 2> op;

  AIGNode(AIGObjID lhs, AIGObjID rhs) : op{lhs, rhs} {}
  AIGNode() = default;

  static constexpr uint32_t hash(AIGNode node) {
    // todo specialized hashing
    return hash_combine(hash_u32(node.op[0]), hash_u32(node.op[1]));
  }

  friend constexpr bool operator==(AIGNode lhs, AIGNode rhs) {
    return lhs.op == rhs.op;
  }

  AIGNodeTRef operator[](uint idx);
};

class AIGNodeTRef : public ObjRef<AIGNode> {
public:
  using ObjRef<AIGNode>::ObjRef;
  auto invert() { return AIGObjID::InvertField{obj.num}; }
  auto custom() { return AIGObjID::CustomField{obj.num}; }
  auto idx() { return AIGObjID::IdxField{obj.num}; }
  bool isSpecial() const { return AIGObjID{obj}.isSpecial(); };
  AIGObjID getObjID() const { return AIGObjID{obj}; }

  AIGNodeTRef inverted() const {
    AIGNodeTRef rv = *this;
    rv.invert() = !rv.invert();
    return rv;
  }
  AIGNodeTRef nonInverted() const {
    AIGNodeTRef rv = *this;
    rv.invert() = 0;
    return rv;
  }

  AIGNodeTRef(ObjID id) : ObjRef<AIGNode>(id) {}
  AIGNodeTRef(uint32_t idx, bool invert = false, bool custom = false)
      : ObjRef<AIGNode>(AIGObjID{idx, invert, custom}) {}
  AIGNodeTRef(ObjRef<AIGNode> ref) : ObjRef<AIGNode>(ref) {}
};

inline AIGNodeTRef AIGNode::operator[](uint i) { return op[i]; }

class FatAIGNodeRef;

class AIGNodeRef : public AIGNodeTRef, public ByValueRTTIUtilMixin<AIGNodeRef> {
  AIGNode *ptr;

public:
  using ByValueRTTIUtilMixin<AIGNodeRef>::as;
  using ByValueRTTIUtilMixin<AIGNodeRef>::is;
  using ByValueRTTIUtilMixin<AIGNodeRef>::dyn_as;

  AIGNode *operator->() { return ptr; }
  AIGNode &operator*() { return *ptr; }
  AIGNodeRef(AIGNodeTRef ref, AIGNode *ptr) : AIGNodeTRef(ref), ptr(ptr) {}
  AIGNode *getPtr() { return ptr; }

  AIGNodeRef inverted() const {
    AIGNodeRef rv = *this;
    rv.invert() = !rv.invert();
    return rv;
  }
  AIGNodeRef nonInverted() const {
    AIGNodeRef rv = *this;
    rv.invert() = 0;
    return rv;
  }

  explicit operator FatAIGNodeRef() const;
  static bool is_impl(FatAIGNodeRef);
  static bool is_impl(AIGNodeTRef) { return true; }
};

class FatAIGNode {
public:
  InstrDefUse defUse;
  AIGNode node;

  FatAIGNode(ObjRef<FatAIGNode>) {};
  FatAIGNode(DynObjRef, AIGObjID lhs, AIGObjID rhs) : node(lhs, rhs) {}
};

class FatAIGNodeRef : public FatObjRef<FatAIGNode>,
                      public ByValueRTTIUtilMixin<FatAIGNodeRef> {
public:
  using ByValueRTTIUtilMixin<FatAIGNodeRef>::as;
  using ByValueRTTIUtilMixin<FatAIGNodeRef>::is;
  using ByValueRTTIUtilMixin<FatAIGNodeRef>::dyn_as;
  using FatObjRef<FatAIGNode>::FatObjRef;
  FatAIGNodeRef(FatObjRef<FatAIGNode> ref) : FatObjRef<FatAIGNode>(ref) {}
  FatAIGNodeRef(ObjID obj, FatAIGNode *ptr) : FatObjRef<FatAIGNode>(obj, ptr) {}

  static bool is_impl(AIGNodeTRef ref) { return ref.isSpecial(); }
  static bool is_impl(ObjRef<AIGNode> ref) {
    return AIGNodeTRef{ref}.isSpecial();
  }
  using FatObjRef<FatAIGNode>::is_impl;

  explicit operator AIGNodeRef() const {
    return AIGNodeRef{obj, &(*this)->node};
  }

  FatAIGNodeRef inverted() const {
    FatAIGNodeRef rv = *this;
    AIGObjID::InvertField{rv.obj.num} = !AIGObjID::InvertField{rv.obj.num};
    return rv;
  }
  FatAIGNodeRef nonInverted() const {
    FatAIGNodeRef rv = *this;
    AIGObjID::InvertField{rv.obj.num} = 0;
    return rv;
  }
};
inline bool AIGNodeRef::is_impl(FatAIGNodeRef) { return true; }
inline AIGNodeRef::operator FatAIGNodeRef() const {

  assert(isSpecial());
  FatAIGNodeRef *ptr = reinterpret_cast<FatAIGNodeRef *>(
      reinterpret_cast<char *>(this->ptr) - offsetof(FatAIGNode, node));
  return FatAIGNodeRef{obj, ptr};
}

template <> struct ObjTraits<FatAIGNode> {
  using FatRefT = FatAIGNodeRef;
  static constexpr DialectType ty{AIG_FAT_NODE};
};

template <> struct ObjTraits<AIGNode> {
  using FatRefT = AIGNodeRef;
  static constexpr DialectType ty{AIG_NODE};
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

  auto begin() { return dedupeMap.container.begin(); }
  auto end() { return dedupeMap.container.end(); }
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
    AIGObjID id{ref.getObjID() + AIGObjID::FAT_ID_START, invert, custom};
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
public:
  ThinAIGNodeStore thin;
  FatAIGNodeStore<NewDeleteObjStore<FatAIGNode>> fat;

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

  AIGNodeRef createAND(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createNode(lhs, rhs);
  }
  AIGNodeRef createOR(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createNode(lhs.inverted(), rhs.inverted()).inverted();
  }
  AIGNodeRef createNAND(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createNode(lhs, rhs).inverted();
  }
  AIGNodeRef createNOR(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createNode(lhs.inverted(), rhs.inverted());
  }
  AIGNodeRef createMUX(AIGNodeTRef sel, AIGNodeTRef trueV, AIGNodeTRef falseV) {
    return createOR(createAND(sel, trueV), createAND(sel.inverted(), falseV));
  }
  AIGNodeRef createXOR(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createOR(createAND(lhs, rhs.inverted()),
                    createAND(lhs.inverted(), rhs));
  }
  AIGNodeRef createXNOR(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    return createNOR(createAND(lhs, rhs.inverted()),
                     createAND(lhs.inverted(), rhs));
  }
  AIGNodeTRef createNOT(AIGNodeTRef lhs) { return lhs.inverted(); }

  FatAIGNodeRef createInput() { return store.createSpecial(); }
  FatAIGNodeRef createOutput(AIGNodeTRef val) {
    // better not perform any logic in the output, so both inputs are just
    // the actual output value.
    return store.createSpecial(val.getObjID(), val.getObjID());
  }

  AIGNodeRef getZero() { return store.resolve(AIGNodeTRef{0, false, false}); }
  AIGNodeRef getOne() { return getZero().inverted(); }

  AIG() {
    auto constZeroNode = store.create();
    assert(constZeroNode.idx() == 0);
  }
};

class AIGObj {
public:
  InstrDefUse defUse;
  AIG aig;

  template <typename... Args>
  AIGObj(ObjRef<AIGObj>, Args... args) : aig(std::forward<Args>(args)...) {}
};

using AIGObjRef = FatObjRef<AIGObj>;

template <> struct ObjTraits<AIGObj> {
  using FatRefT = FatObjRef<AIGObj>;
  static constexpr DialectType ty{AIG_AIG};
};
}; // namespace dyno

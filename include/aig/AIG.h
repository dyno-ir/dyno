#pragma once

#include "aig/IDs.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "support/Debug.h"
#include "support/DedupeMap.h"
#include "support/RTTI.h"
#include "support/Utility.h"
#include <algorithm>
#include <cassert>

namespace dyno {

class AIGNodeTRef;
class FatAIGNodeRef;
class AIG;

class AIGObjID : public ObjID {
public:
  // FIXME: 30 too smol with custom field removed
  constexpr static num_t FAT_ID_SPACE = 1u << 24;
  constexpr static num_t FAT_ID_START = (1u << 30) - FAT_ID_SPACE;
  using ObjID::ObjID;
  using InvertField = BitField<ObjID::num_t, 1, 0>;
  using InvertFieldC = BitField<const ObjID::num_t, 1, 0>;
  // using CustomField = BitField<ObjID::num_t, 1, 1>;
  using IdxField = BitField<ObjID::num_t, 31, 1>;
  using IdxFieldC = BitField<const ObjID::num_t, 31, 1>;

  constexpr auto invert() { return InvertField{num}; }
  // auto custom() { return CustomField{num}; }
  constexpr auto idx() { return IdxField{num}; }
  constexpr auto idx() const { return IdxFieldC{num}; }
  constexpr bool isSpecial() const { return idx() >= FAT_ID_START; }

  constexpr AIGObjID(uint32_t idx, bool invert = false) {
    num = 0; // Prevent benign uninitialized read for constexpr
    this->idx() = idx;
    this->invert() = invert;
    // this->custom() = custom;
  }
  constexpr AIGObjID(ObjID obj) : ObjID(obj) {}
};

class AIGNode {
public:
  std::array<AIGObjID, 2> op;

  AIGNode(AIGObjID lhs, AIGObjID rhs) : op{lhs, rhs} {}

  static constexpr uint32_t hash(AIGNode node) {
    // todo specialized hashing
    return hash_combine(hash_u32(node.op[0]), hash_u32(node.op[1]));
  }

  friend constexpr bool operator==(AIGNode lhs, AIGNode rhs) {
    return lhs.op == rhs.op;
  }

  AIGNodeTRef operator[](unsigned idx);
};

class AIGNodeTRef : public ObjRef<AIGNode> {
public:
  static consteval AIGNodeTRef zero() { return AIGNodeTRef{0}; }
  static consteval AIGNodeTRef one() { return AIGNodeTRef{0, true}; }
  static constexpr AIGNodeTRef from(bool v) { return AIGNodeTRef{0, v}; }

  using ObjRef<AIGNode>::ObjRef;
  auto invert() { return AIGObjID::InvertField{obj.num}; }
  auto idx() { return AIGObjID::IdxField{obj.num}; }
  auto invert() const { return AIGObjID::InvertFieldC{obj.num}; }
  auto idx() const { return AIGObjID::IdxFieldC{obj.num}; }
  bool isSpecial() const { return AIGObjID{obj}.isSpecial(); };
  AIGObjID getObjID() const { return AIGObjID{obj}; }

  bool isTerminator() {
    assert(bool(*this));
    return idx() == 0 || isSpecial();
  }
  bool isGate() { return !isTerminator(); }

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

  unsigned getOffsetIdx() const {
    if (isSpecial())
      return idx() - AIGObjID::FAT_ID_START;
    return idx();
  }

  bool isZero() const { return idx() == 0 && !invert(); }
  bool isOne() const { return idx() == 0 && invert(); }

  // AIGNodeTRef() = delete;
  AIGNodeTRef(ObjID id) : ObjRef<AIGNode>(id) {}
  explicit constexpr AIGNodeTRef(uint32_t idx, bool invert = false)
      : ObjRef<AIGNode>(AIGObjID{idx, invert}) {}
  AIGNodeTRef(ObjRef<AIGNode> ref) : ObjRef<AIGNode>(ref) {}
  AIGNodeTRef(nullref_t) : ObjRef<AIGNode>(ObjID::invalid()) {}

  static bool is_impl(FatAIGNodeRef);

  friend bool operator<(const AIGNodeTRef &lhs, const AIGNodeTRef &rhs) {
    return lhs.getObjID() < rhs.getObjID();
  }

  friend std::ostream &operator<<(std::ostream &os, const AIGNodeTRef &ref) {
    if (ref.invert())
      os << "!";
    if (ref.isSpecial())
      os << "&";
    os << ref.getOffsetIdx();
    return os;
  }
};

inline AIGNodeTRef AIGNode::operator[](unsigned i) { return op[i]; }

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

  bool isOutput() { return isSpecial() && operand(0) != nullref; }

  bool isInput() { return isSpecial() && operand(0) == nullref; }

  auto operands() {
    assert(isGate());
    return Range{ptr->op}.cast<AIGNodeTRef>();
  }
  auto begin() { return operands().begin(); }
  auto end() { return operands().end(); }

  AIGNodeTRef operand(unsigned i) { return ptr->op[i]; }
  AIGNodeTRef operator[](unsigned i) { return ptr->op[i]; }

  // void replaceOperands(AIGNodeTRef a, AIGNodeTRef b) {
  //   if (a.getObjID() > b.getObjID())
  //     std::swap(a, b);
  //   ptr->op = {a.getObjID(), b.getObjID()};
  // }

  explicit operator FatAIGNodeRef() const;
  static bool is_impl(FatAIGNodeRef);
  static bool is_impl(AIGNodeTRef) { return true; }
};

class FatAIGNode {
public:
  InstrDefUse defUse;
  AIGNode node{ObjID::invalid(), ObjID::invalid()};

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
  operator AIGNodeTRef() const { return AIGNodeTRef{obj}; }

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
  AIGObjID getObjID() {
    AIGObjID aigObj{obj};
    if (!aigObj.isSpecial())
      dyno_unreachable("invalid");
    return aigObj;
  }

  AIGNodeTRef getSingleOperand() {
    assert(getPtr()->node[0] == getPtr()->node[1]);
    return getPtr()->node[0];
  }
};
inline bool AIGNodeRef::is_impl(FatAIGNodeRef) { return true; }
inline bool AIGNodeTRef::is_impl(FatAIGNodeRef) { return true; }
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
  friend class AIG;
  friend class AIGNodeStore;
  DedupeMap<AIGNode, std::vector<AIGNode>, AIGNode::hash> dedupeMap;

public:
  AIGNodeRef resolve(AIGNodeTRef ref) {
    return AIGNodeRef{ref, &dedupeMap.container[ref.idx()]};
  }

  std::pair<AIGNodeRef, bool> create(AIGNodeTRef a, AIGNodeTRef b) {
    assert(a.getObjID() <= b.getObjID() && "AIG gate l <= r violation");
    auto [index, isNew] =
        dedupeMap.canonicalize(AIGNode{a.getObjID(), b.getObjID()});
    auto thin = AIGNodeTRef{index};
    AIGNodeRef ref = resolve(thin);
    assert((ref[0].isSpecial() || ref[0].idx() < ref.idx()) &&
           (ref[1].isSpecial() || ref[1].idx() < ref.idx()) &&
           "AIG topo order violation");
    return {ref, isNew};
  }

  auto objs() {
    return Range{dedupeMap.container}.transform(
        [](uint32_t idx, AIGNode &node) {
          auto ref = AIGNodeTRef{idx, false};
          if (ref.isSpecial())
            dyno_unreachable("ids oob");
          return AIGNodeRef{ref, &node};
        });
  }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }
  ObjID::num_t numIDs() { return dedupeMap.container.size(); }
  uint32_t size() { return dedupeMap.size(); }
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
                                    bool invert = false) {
    AIGObjID id{ref.getObjID() + AIGObjID::FAT_ID_START, invert};
    return FatAIGNodeRef{FatObjRef<FatAIGNode>{id, ref.getPtr()}};
  }

  template <typename... Args> FatAIGNodeRef create(Args &&...args) {
    return transformOut(store.create(std::forward<Args>(args)...));
  }

  FatAIGNodeRef resolve(AIGNodeTRef ref) {
    return transformOut(store.resolve(transformIn(ref)), ref.invert());
  }

  ObjID::num_t numIDs() { return store.numIDs(); }
};

class AIGNodeStore {
  friend class AIG;
  ThinAIGNodeStore thin;
  FatAIGNodeStore<NewDeleteObjStore<FatAIGNode>> fat;
  std::vector<unsigned> useCountGates;
  std::vector<unsigned> useCountIns;

public:
  AIGNodeRef resolve(AIGNodeTRef ref) {
    if (ref.isSpecial()) [[unlikely]] {
      return fat.resolve(ref).as<AIGNodeRef>();
    }
    return thin.resolve(ref);
  }

  AIGNodeRef create(AIGNodeTRef a, AIGNodeTRef b) {
    assert(a && b);
    auto [ref, isNew] = thin.create(a, b);
    if (isNew) {
      useCountGates.push_back(0);
      assert(useCountGates.size() == thin.numIDs());
      if (a.isSpecial()) [[unlikely]]
        ++useCountIns[a.getOffsetIdx()];
      else
        ++useCountGates[a.idx()];
      if (b.isSpecial()) [[unlikely]]
        ++useCountIns[b.getOffsetIdx()];
      else
        ++useCountGates[b.idx()];
    }
    return ref;
  }

  template <typename... Args> FatAIGNodeRef createSpecial(Args &&...args) {
    auto ref = fat.create(std::forward<Args>(args)...);
    useCountIns.push_back(0);
    assert(useCountIns.size() == fat.numIDs());
    return ref;
  }

  unsigned getUseCount(AIGNodeTRef ref) {
    if (ref.isSpecial()) [[unlikely]]
      return useCountIns[ref.getOffsetIdx()];
    return useCountGates[ref.idx()];
  }
};

class AIG {
  AIGNodeStore store;

public:
  std::vector<FatAIGNodeRef> inputs;
  std::vector<FatAIGNodeRef> outputs;

  static AIGNodeTRef simplify(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    assert(lhs.getObjID() <= rhs.getObjID());
    if (lhs == rhs)
      return lhs;
    if (lhs == rhs.inverted())
      return AIGNodeTRef::zero();
    assert(!rhs.isZero());
    if (lhs.isZero())
      return lhs;
    assert(!rhs.isOne());
    if (lhs.isOne())
      return rhs;
    return nullref;
  }

  static void simplifyMulti(SmallVecImpl<AIGNodeTRef> &multi) {
    std::sort(multi.begin(), multi.end(),
              [](auto l, auto r) { return l.getObjID() < r.getObjID(); });
    if (multi.size() < 3)
      return;
    unsigned iOut = 0;
    for (unsigned i = 1; i < multi.size(); ++i) {
      auto simple = simplify(multi[iOut], multi[i]);
      // TODO: early return for 0 and 1
      if (simple) {
        multi[iOut] = simple;
      } else {
        multi[++iOut] = multi[i];
      }
    }
    multi.downsize(iOut + 1);
  }

  AIG cloneInputs() {
    AIG cloned;
    for (auto &input : inputs) {
      auto in = cloned.createInput();
      // assert(in.getObjID().idx() == input.getObjID().idx());
    }
    return cloned;
  }

  AIGNodeRef createNode(AIGNodeTRef lhs, AIGNodeTRef rhs) {
    assert(lhs && rhs && "AIG gate with invalid refs");
    if (lhs.getObjID() > rhs.getObjID())
      std::swap(lhs, rhs);
    AIGNodeTRef simple = simplify(lhs, rhs);
    if (simple) {
      DYNO_DBGV(dbgs() << "[AIG Node] " << lhs << ", " << rhs
                       << " -> simplified to " << simple << "\n");
      return store.resolve(simple);
    }
    AIGNodeRef node = store.create(lhs.getObjID(), rhs.getObjID());
    DYNO_DBGV(dbgs() << "[AIG Node] " << lhs << ", " << rhs << " -> " << node
                     << "\n");
    return node;
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

  FatAIGNodeRef createInput() {
    auto node = store.createSpecial(AIGObjID{ObjID::invalid()},
                                    AIGObjID{ObjID::invalid()});
    inputs.emplace_back(node);
    return node;
  }
  FatAIGNodeRef createOutput(AIGNodeTRef val) {
    // better not perform any logic in the output, so both inputs are just
    // the actual output value.
    auto node = store.createSpecial(val.getObjID(), val.getObjID());
    outputs.emplace_back(node);
    return node;
  }

  auto gates() { return store.thin.objs().drop_front(); }
  unsigned numGateIDs() { return store.thin.numIDs(); }
  unsigned numSpecialIDs() { return store.fat.numIDs(); }

  unsigned getUseCount(AIGNodeTRef ref) { return store.getUseCount(ref); }

  AIGNodeRef getZero() { return store.resolve(AIGNodeTRef{0, false}); }
  AIGNodeRef getOne() { return getZero().inverted(); }

  AIGNodeRef operator[](AIGNodeTRef ref) { return store.resolve(ref); }

  AIG() {
    auto [zeroNode, _] = store.thin.create(ObjID::invalid(), ObjID::invalid());
    store.useCountGates.push_back(0);
    assert(zeroNode.idx() == 0);
  }
};

class AIGNodeBuilder {
  AIG &aig;
  AIGNodeTRef lhs;

  AIGNodeBuilder builder(AIGNodeTRef ref) const {
    return AIGNodeBuilder{aig, ref};
  }

public:
  AIGNodeBuilder(AIG &aig, AIGNodeTRef lhs) : aig(aig), lhs(lhs) {}
  operator AIGNodeTRef() { return lhs; }
  AIGNodeTRef get() { return lhs; }

  AIGNodeBuilder operator&(AIGNodeTRef rhs) const {
    return builder(aig.createAND(lhs, rhs));
  }
  AIGNodeBuilder operator|(AIGNodeTRef rhs) const {
    return builder(aig.createOR(lhs, rhs));
  }
  AIGNodeBuilder operator^(AIGNodeTRef rhs) const {
    return builder(aig.createXOR(lhs, rhs));
  }
  AIGNodeBuilder operator&(bool v) const {
    return *this & AIGNodeTRef::from(v);
  }
  AIGNodeBuilder operator|(bool v) const {
    return *this | AIGNodeTRef::from(v);
  }
  AIGNodeBuilder operator^(bool v) const {
    return *this ^ AIGNodeTRef::from(v);
  }
  AIGNodeBuilder operator~() const { return builder(aig.createNOT(lhs)); }
  AIGNodeBuilder mux(AIGNodeTRef trueV, AIGNodeTRef falseV) const {
    return builder(aig.createMUX(lhs, trueV, falseV));
  }
  AIGNodeBuilder output() const {
    return builder(aig.createOutput(lhs).as<AIGNodeTRef>());
  }
};

// TODO: consider extending ObjMap with ObjTrait overrides, but this behaves
// sufficiently different, so prob better to just keep separate impls
// FIMXE: should really canon special-ids to map into normal space
template <typename Container> class AIGNodeMap {
public:
  using value_type = typename Container::value_type;
  using reference = typename Container::reference;
  using V = typename Container::value_type;
  Container gates;
  Container special;

  AIGNodeMap() = default;

  AIGNodeMap(AIG &aig)
      : gates(aig.numGateIDs()), special(aig.numSpecialIDs()) {}

  AIGNodeMap(AIG &aig, V defaultVal)
      : gates(aig.numGateIDs(), defaultVal),
        special(aig.numSpecialIDs(), defaultVal) {}

  void reset(AIG &aig) {
    gates.resize(aig.numGateIDs());
    special.resize(aig.numSpecialIDs());
  }

  void reset(AIG &aig, V defaultVal) {
    resetSpecial(aig, defaultVal);
    gates.clear();
    gates.resize(aig.numGateIDs(), defaultVal);
  }

  void resetSpecial(AIG &aig, V defaultVal) {
    special.clear();
    special.resize(aig.numSpecialIDs(), defaultVal);
  }

  reference operator[](AIGNodeTRef node) {
    if (node.isSpecial()) [[unlikely]]
      return special[node.getOffsetIdx()];
    return gates[node.idx()];
  }
};

template <typename T> using AIGNodeVecMap = AIGNodeMap<std::vector<T>>;

class AIGNodeRemap {
  AIGNodeVecMap<uint32_t> idMap;

private:
  void init() { idMap.gates[0] = 0; }

public:
  AIGNodeRemap() { idMap.gates.resize(1, 0); }
  AIGNodeRemap(AIG &aig) : idMap(aig, AIGObjID::invalid()) { init(); }

  void reset(AIG &aig) {
    idMap.reset(aig, AIGObjID::invalid());
    init();
  }

  void setupInputMap(AIG &oldAig, AIG &newAig) {
    assert(oldAig.inputs.size() == newAig.inputs.size());
    for (auto [oldIn, newIn] : Range{oldAig.inputs}.zip(newAig.inputs)) {
      insert(oldIn.as<AIGNodeTRef>(), newIn.as<AIGNodeTRef>());
    }
  }

  AIGNodeTRef operator[](AIGNodeTRef node) {
    return AIGNodeTRef(idMap[node], node.invert());
  }

  bool contains(AIGNodeTRef node) { return idMap[node] != AIGObjID::invalid(); }

  void insert(AIGNodeTRef oldNode, AIGNodeTRef newNode) {
    idMap[oldNode] = newNode.idx();
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

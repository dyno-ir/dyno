#pragma once

#include "ir/SSAType.h"
#include "support/MachineInt.h"
#include "support/RTTI.h"
#include "support/Utility.h"
#include <cassert>
#include <functional>
#include <iterator>

class Operand;
class Instr;
class CFGBlock;

class ExternSSADef;
class GlobalDef;

class Reg {
public:
  friend class std::hash<Reg>;
  using num_t = unsigned;
  static constexpr num_t vRegMsk = num_t(1) << (sizeof(num_t) * 8 - 1);

  constexpr Reg() = default;

  static constexpr Reg vReg(num_t idx) { return Reg(idx).toVReg(); }

  // FIXME: make explicit
  constexpr Reg(num_t num) : num(num) {}

  constexpr explicit operator bool() const { return num; }

  // FIXME: make explicit
  constexpr operator num_t() const { return num; }

  constexpr num_t getNum() const { return num; }
  constexpr num_t getIdx() const { return num & ~vRegMsk; }

  constexpr bool isVReg() { return num & vRegMsk; }
  constexpr bool isPhysReg() { return !(num & vRegMsk); }

  constexpr Reg toVReg() { return Reg(num | vRegMsk); }

private:
  num_t num = 0;
};

template <> struct std::hash<Reg> {
  size_t operator()(const Reg &reg) const { return reg.num; }
};

class BrCond {
public:
  enum Kind { EQ, NE, LT, LTU, LE, LEU, GT, GTU, GE, GEU };

  constexpr BrCond(Kind kind) : kind(kind) {}

  static constexpr BrCond eq() { return BrCond(EQ); }
  static constexpr BrCond ne() { return BrCond(NE); }
  static constexpr BrCond lt() { return BrCond(LT); }
  static constexpr BrCond ltu() { return BrCond(LTU); }
  static constexpr BrCond le() { return BrCond(LE); }
  static constexpr BrCond leu() { return BrCond(LEU); }
  static constexpr BrCond gt() { return BrCond(GT); }
  static constexpr BrCond gtu() { return BrCond(GTU); }
  static constexpr BrCond ge() { return BrCond(GE); }
  static constexpr BrCond geu() { return BrCond(GEU); }

  static const char *kindName(Kind kind);

  constexpr BrCond negate() {
    switch (kind) {
    case EQ:
      return BrCond(NE);
    case NE:
      return BrCond(EQ);
    case LT:
      return BrCond(GE);
    case LTU:
      return BrCond(GEU);
    case LE:
      return BrCond(GT);
    case LEU:
      return BrCond(GTU);
    case GT:
      return BrCond(LE);
    case GTU:
      return BrCond(LEU);
    case GE:
      return BrCond(LT);
    case GEU:
      return BrCond(LTU);
    }
  }

  constexpr BrCond commute() {
    switch (kind) {
    case EQ:
    case NE:
      break;
    case LT:
      return BrCond(GT);
    case LTU:
      return BrCond(GTU);
    case LE:
      return BrCond(GE);
    case LEU:
      return BrCond(GEU);
    case GT:
      return BrCond(LT);
    case GTU:
      return BrCond(LTU);
    case GE:
      return BrCond(LE);
    case GEU:
      return BrCond(LEU);
    }
    return *this;
  }

  friend bool operator==(const BrCond &a, const BrCond &b) {
    return a.getKind() == b.getKind();
  }

  Kind getKind() const { return kind; }

  constexpr bool isSigned() {
    switch (kind) {
    case LT:
    case LE:
    case GT:
    case GE:
      return true;
    default:
      return false;
    }
  }

private:
  Kind kind;
};

class SubOperand {
public:
  const Operand &operand() const {
    return reinterpret_cast<const Operand &>(*this);
  }
  Operand &operand() { return reinterpret_cast<Operand &>(*this); }
};

class DefUseChain : public SubOperand {
  friend class Operand;
  friend class SSADef;
  friend class SSAUse;
  friend class RegDefUse;
  friend class RegDefUseRoot;

public:
  DefUseChain() {}
  DefUseChain(const DefUseChain &) = delete;
  DefUseChain &operator=(const DefUseChain &) = delete;
  ~DefUseChain() { unlink(); }

  template <bool DEF = false, bool USE = false> class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = Operand;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

    iterator() {}
    iterator(DefUseChain *ref) : mPtr(ref) {}

    reference operator*() const { return mPtr->operand(); }
    pointer operator->() const { return &mPtr->operand(); }

    iterator &operator++();

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.mPtr == b.mPtr;
    }

  private:
    DefUseChain *mPtr = nullptr;
  };
  static_assert(std::forward_iterator<iterator<>>);

protected:
  DefUseChain *chNext = nullptr;
  DefUseChain *chPrev = nullptr;
  union {
    Reg contentReg;
    SSAType *contentType;
    Operand *contentDef;
  };

  bool isLinked() const { return chNext; }

  void unlink() {
    assert(isLinked());
    chPrev->chNext = chNext;
    chNext->chPrev = chPrev;
    chNext = nullptr;
    chPrev = nullptr;
  }

  void linkSelf() {
    chNext = this;
    chPrev = this;
  }

  void insertNext(DefUseChain &o) {
    assert(isLinked());
    assert(!o.isLinked());
    o.chNext = chNext;
    o.chPrev = this;
    chNext->chPrev = &o;
    chNext = &o;
  }

  void insertPrev(DefUseChain &o) {
    assert(isLinked());
    assert(!o.isLinked());
    o.chNext = this;
    o.chPrev = chPrev;
    chPrev->chNext = &o;
    chPrev = &o;
  }

  void unlinkAll() {
    DefUseChain *curr = this;
    do {
      DefUseChain &tmp = *curr;
      curr = chNext;
      tmp.chPrev = nullptr;
      tmp.chNext = nullptr;
    } while (curr);
  }

  size_t count() const {
    assert(isLinked());
    size_t count = 0;
    for (DefUseChain *curr = chNext; curr != this; curr = curr->chNext) {
      ++count;
    }
    return count;
  }

  bool empty() const {
    assert(isLinked());
    return chNext == this;
  }

  bool countExactly(size_t n) const { return count() == n; }

  void splice(DefUseChain &o);
};
static_assert(std::is_standard_layout_v<DefUseChain>);

class RegInfo;
class RegDefUse : public DefUseChain {
public:
  RegDefUse(Reg reg) {
    contentReg = reg;
    insertDefUse();
  }

  Reg reg() const {
    assert(isLinked());
    return contentReg;
  }

  void replace(Reg reg) {
    unlink();
    contentReg = reg;
    insertDefUse();
  }

  bool isDef();
  bool isUse();

private:
  void insertDefUse();
};
static_assert(std::is_standard_layout_v<RegDefUse>);

class SSAUse : public DefUseChain {
public:
  friend class SSADef;
  friend class Operand;

  SSAUse(Operand &defOp) { init(defOp); }

  void replace(Operand &defOp) {
    unlink();
    init(defOp);
  }

  Operand &getDef() const {
    assert(isLinked() && contentDef);
    return *contentDef;
  }
  Instr &getDefInstr() const;

protected:
  void init(Operand &defOp);
};
static_assert(std::is_standard_layout_v<SSAUse>);

class SSADef : public DefUseChain {
  friend class SSAUse;
  friend class Operand;

public:
  SSADef(SSAType &type) : SSADef(&type) {}
  SSADef(SSAType *type) {
    linkSelf();
    contentType = type;
  }

  void setType(SSAType *type) { contentType = type; }
  void setType(SSAType &type) { contentType = &type; }
  SSAType *getType() { return contentType; }
  SSAType &type() {
    assert(contentType);
    return *contentType;
  }

  SSADef(const SSADef &o) = delete;
  SSADef &operator=(const SSADef &o) = delete;
  ~SSADef() {}

  void replaceAllUses(Operand &newDefOp);

  void replaceAllUses(Reg reg);
  void replace(Reg reg);

  iterator<> begin() { return chNext; }
  iterator<> end() { return this; }

  bool hasExactlyNUses(unsigned n) const { return countExactly(n); }
  bool hasUses() const { return !empty(); }
  unsigned getNumUses() const { return count(); }
};
static_assert(std::is_standard_layout_v<SSADef>);

class OperandChain {
public:
  OperandChain() {}
  OperandChain(const OperandChain &o) = delete;
  OperandChain &operator=(const OperandChain &o) = delete;
  ~OperandChain() { deallocate(); }

  using iterator = Operand *;
  iterator begin();
  iterator end();

  void allocate(unsigned cap);
  void deallocate();
  Operand &getOperand(unsigned n);
  unsigned getCapacity() { return capacity; }
  bool isAllocated() { return operands; }

private:
  unsigned capacity = 0;
  Operand *operands = nullptr;
};

class Operand {
  friend class Instr;
  friend class DefUseChain;
  friend class SSADef;

public:
  enum Kind {
    EMPTY,
    SSA_DEF_TYPE,
    SSA_DEF_EXTERN,
    SSA_USE,
    REG_DEF,
    REG_USE,
    BLOCK,
    TYPE,
    IMM32,
    MINT,
    BRCOND,
    OP_CHAIN,
  };
  static constexpr bool kindIsSSADef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_EXTERN:
      return true;
    default:
      return false;
    }
  }
  static constexpr bool kindIsSSARegDef(Kind kind) {
    return kind == SSA_DEF_TYPE;
  }
  static constexpr bool kindIsSSAUse(Kind kind) { return kind == SSA_USE; }
  static constexpr bool kindIsReg(Kind kind) {
    return kind == REG_DEF || kind == REG_USE;
  }
  static constexpr bool kindIsDefUseChain(Kind kind) {
    switch (kind) {
    case EMPTY:
    case SSA_DEF_TYPE:
    case SSA_DEF_EXTERN:
    case SSA_USE:
    case REG_DEF:
    case REG_USE:
      return true;
    default:
      return false;
    }
  }
  static constexpr bool kindIsDef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_EXTERN:
    case REG_DEF:
      return true;
    default:
      return false;
    }
  }

  Operand() {}
  Operand(const Operand &o) = delete;
  Operand &operator=(const Operand &o) {
    data = o.data;
    return *this;
  }
  ~Operand() {}

  Kind getKind() { return kind; }

  template <Kind K, typename... ARGS>
  void emplace(Instr *parent, ARGS &&...args) {
    this->parent = parent;
    data.emplace<K>(std::forward<ARGS>(args)...);
  }

  SSADef &ssaDef() {
    assert(kindIsSSADef(kind));
    return data.contentDef;
  }

  ExternSSADef &ssaDefExtern() {
    assert(kind == SSA_DEF_EXTERN);
    return reinterpret_cast<ExternSSADef &>(*this);
  }

  template <typename T> T &ssaDefExtern() { return as<T>(ssaDefExtern()); }

  CFGBlock &ssaDefBlock() { return ssaDefExtern<CFGBlock>(); }

  CFGBlock &block() const {
    assert(kind == BLOCK && data.contentBlock);
    return *data.contentBlock;
  }

  SSAType &type() const {
    assert(kind == TYPE && data.contentType);
    return *data.contentType;
  }

  BrCond brCond() const {
    assert(kind == BRCOND);
    return data.contentBrCond;
  }

  MInt &mInt() {
    assert(kind == MINT);
    return data.contentMInt;
  }

  OperandChain &opChain() {
    assert(kind == OP_CHAIN);
    return data.contentOpChain;
  }
  const OperandChain &opChain() const {
    assert(kind == OP_CHAIN);
    return data.contentOpChain;
  }

  SSAUse &ssaUse() {
    assert(kind == SSA_USE);
    return data.contentUse;
  }
  const SSAUse &ssaUse() const {
    assert(kind == SSA_USE);
    return data.contentUse;
  }

  int32_t imm32() const {
    assert(kind == IMM32);
    return data.contentImm32;
  }

  Reg reg() const {
    assert(kindIsReg(kind));
    return data.contentReg.reg();
  }
  RegDefUse &regDefUse() {
    assert(kindIsReg(kind));
    return data.contentReg;
  }

  void setParent(Instr *instr) { parent = instr; }

  Instr &getParent() const {
    assert(parent);
    return *parent;
  }

  CFGBlock &getParentBlock() const;

  bool isReg() const { return kindIsReg(kind); }
  bool isRegDef() const { return kind == REG_DEF; }
  bool isRegUse() const { return kind == REG_USE; }
  bool isSSADef() const { return kindIsSSADef(kind); }
  bool isSSARegDef() const { return kindIsSSARegDef(kind); }
  bool isSSAUse() const { return kindIsSSAUse(kind); }
  bool isSSARegUse() const {
    return isSSAUse() && ssaUse().getDef().isSSARegDef();
  }

  bool isImplicit() const { return flags.isImplicit; }
  void setImplicit(bool v) { flags.isImplicit = v; }

private:
  // This needs to be the first member, so that OperandData*
  // Operand and pointers to OperandData members are pointer-interconvertible
  union OperandData {
    SSADef contentDef;
    SSAUse contentUse;
    OperandChain contentOpChain;
    RegDefUse contentReg;
    int32_t contentImm32;
    MInt contentMInt;
    CFGBlock *contentBlock;
    SSAType *contentType;
    BrCond contentBrCond;

    OperandData() {}
    OperandData(const OperandData &) = delete;
    OperandData(OperandData &&) = delete;
    OperandData &operator=(OperandData &&) = delete;
    ~OperandData() { destroy(); }

    OperandData &operator=(const OperandData &o) {
      switch (o.operand().kind) {
      case EMPTY:
        destroy();
        break;
      case SSA_DEF_TYPE:
      case SSA_DEF_EXTERN:
        assert(false && "Cannot copy SSA def");
        break;
      case SSA_USE:
        emplace<SSA_USE>(o.contentUse.getDef());
        break;
      case REG_DEF:
        emplace<REG_DEF>(o.contentReg.reg());
        break;
      case REG_USE:
        emplace<REG_USE>(o.contentReg.reg());
        break;
      case BLOCK:
        emplace<BLOCK>(o.contentBlock);
        break;
      case TYPE:
        emplace<TYPE>(o.contentType);
        break;
      case IMM32:
        emplace<IMM32>(o.contentImm32);
        break;
      case BRCOND:
        emplace<BRCOND>(o.contentBrCond);
        break;
      case OP_CHAIN:
        assert(false && "Don't copy operand chain!");
        break;
      case MINT:
        emplace<MINT>(o.contentMInt);
        break;
      }
      return *this;
    }

    void destroy() {
      switch (operand().kind) {
      case EMPTY:
      case BLOCK:
      case TYPE:
      case IMM32:
        break;
      case REG_DEF:
      case REG_USE:
        contentReg.~RegDefUse();
        break;
      case SSA_DEF_TYPE:
      case SSA_DEF_EXTERN:
        contentDef.~SSADef();
        break;
      case SSA_USE:
        contentUse.~SSAUse();
        break;
      case OP_CHAIN:
        contentOpChain.~OperandChain();
        break;
      case BRCOND:
        contentBrCond.~BrCond();
        break;
      case MINT:
        contentMInt.~MInt();
        break;
      }
      operand().kind = EMPTY;
    }

    template <Kind K, typename... ARGS> void emplace(ARGS &&...args) {
      destroy();
      operand().kind = K;
      if constexpr (kindIsSSADef(K)) {
        new (&contentDef) SSADef(std::forward<ARGS>(args)...);
      } else if constexpr (K == SSA_USE) {
        new (&contentUse) SSAUse(std::forward<ARGS>(args)...);
      } else if constexpr (K == REG_DEF || K == REG_USE) {
        new (&contentReg) RegDefUse(std::forward<ARGS>(args)...);
      } else if constexpr (K == IMM32) {
        new (&contentImm32) int32_t(std::forward<ARGS>(args)...);
      } else if constexpr (K == MINT) {
        new (&contentMInt) MInt(std::forward<ARGS>(args)...);
      } else if constexpr (K == BRCOND) {
        new (&contentBrCond) BrCond(std::forward<ARGS>(args)...);
      } else if constexpr (K == BLOCK) {
        new (&contentBlock) CFGBlock *(std::forward<ARGS>(args)...);
      } else if constexpr (K == TYPE) {
        new (&contentType) SSAType *(std::forward<ARGS>(args)...);
      } else if constexpr (K == EMPTY) {
      } else if constexpr (K == OP_CHAIN) {
        new (&contentOpChain) OperandChain(std::forward<ARGS>(args)...);
      } else {
        static_assert(dependent_false_v<ARGS...>,
                      "Operand kind cannot be emplaced with these args");
      }
    }

    const Operand &operand() const {
      return reinterpret_cast<const Operand &>(*this);
    }
    Operand &operand() { return reinterpret_cast<Operand &>(*this); }
  } data;
  static_assert(sizeof(OperandData) == 3 * sizeof(void *),
                "OperandData size changed");
  static_assert(std::is_standard_layout_v<Operand::OperandData>);

  Kind kind = EMPTY;
  struct {
    unsigned isImplicit : 1 = 0;
  } flags;
  Instr *parent = nullptr;

  DefUseChain &defUseChain() {
    assert(kindIsDefUseChain(kind));
    return reinterpret_cast<DefUseChain &>(data);
  }
};
static_assert(std::is_standard_layout_v<Operand>);

class ExternSSADef {
public:
  enum Kind {
    GLOBAL_START,
    GLOBAL_FUNCTION,
    GLOBAL_STATIC_MEMORY,
    GLOBAL_END,
    FUNC_START,
    FUNC_REG,
    FUNC_BLOCK,
    FUNC_PARAM,
    FUNC_FRAME,
    FUNC_MEMORY_ACCESS,
    FUNC_END
  };

  ExternSSADef(Kind kind) : kind(kind) {
    defOp.emplace<Operand::SSA_DEF_EXTERN>(nullptr, nullptr);
  }
  ExternSSADef(Kind kind, SSAType &type) : kind(kind) {
    defOp.emplace<Operand::SSA_DEF_EXTERN>(nullptr, type);
  }

  Kind getKind() const { return kind; }

  Operand &operand() { return defOp; }

  static constexpr bool kindIsGlobal(Kind kind) {
    return kind < GLOBAL_END && kind > GLOBAL_START;
  }

  static constexpr bool kindIsLocal(Kind kind) {
    return kind < FUNC_END && kind > FUNC_START;
  }

  bool isGlobal() { return kindIsGlobal(kind); }

  bool isLocal() { return kindIsLocal(kind); }

  GlobalDef &global() { return as<GlobalDef>(*this); }

  CFGBlock &block() { return as<CFGBlock>(*this); }

protected:
  ~ExternSSADef() = default;

private:
  Operand defOp; // This needs to be the first member, so that Operand*
                 // is pointer-interconvertible with ExternSSADef*
  Kind kind;
};
static_assert(std::is_standard_layout_v<ExternSSADef>);

inline void SSAUse::init(Operand &defOp) {
  SSADef &def = defOp.ssaDef();
  def.insertNext(*this);
  contentDef = &defOp;
}

inline Instr &SSAUse::getDefInstr() const { return getDef().getParent(); }

inline void SSADef::replaceAllUses(Operand &newDefOp) {
  for (DefUseChain *curr = chNext; curr != this;) {
    curr->contentDef = &newDefOp;
    curr = curr->chNext;
  }
  newDefOp.ssaDef().splice(*this);
}

inline void DefUseChain::splice(DefUseChain &o) {
  assert(isLinked() && o.isLinked());
  if (o.chNext == &o)
    return;

  o.chNext->chPrev = chPrev;
  o.chPrev->chNext = this;
  chPrev->chNext = o.chNext;
  chPrev = o.chPrev;

  o.linkSelf();
}

template <bool DEF, bool USE>
inline DefUseChain::iterator<DEF, USE> &
DefUseChain::iterator<DEF, USE>::operator++() {
  if constexpr (DEF) {
    mPtr = mPtr->chPrev;
    if (!mPtr->operand().isRegDef()) {
      mPtr = nullptr;
    }
  } else if constexpr (USE) {
    mPtr = mPtr->chNext;
    if (!mPtr->operand().isRegUse()) {
      mPtr = nullptr;
    }
  } else {
    mPtr = mPtr->chNext;
  }
  return *this;
}

inline bool RegDefUse::isDef() { return operand().isRegDef(); }

inline bool RegDefUse::isUse() { return operand().isRegUse(); }

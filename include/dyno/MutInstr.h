#pragma once
#include "dyno/Context.h"
#include "dyno/Instr.h"

namespace dyno {

// Wrapper around InstrRef to make it and its operands modifiable, for use
// during Instr build process.
// Operands can be accessed, modified, appended etc via std::vector like
// interface.
template <IsFatDynObjRef T> class MutInstr {
  static_assert(std::is_trivially_constructible_v<T> &&
                std::is_trivially_destructible_v<T>);
  MutInstr(const MutInstr &) = default;
  MutInstr &operator=(const MutInstr &) = default;

public:
  InstrRef instr;
  Context *ctx;
  uint32_t sz;

  MutInstr(MutInstr &&o) {
    *this = o;
    o.instr = nullref;
  }
  MutInstr &operator=(MutInstr &&o) {
    (*this) = o;
    o.instr = nullref;
  }

  // reseves operands only, append with emplace_back/defsDone
  MutInstr(Context &ctx, DialectOpcode opc, uint32_t reserve = 64)
      : ctx(&ctx), sz(0) {
    // unlikely that we hit the exact size so better go for a big initial alloc.
    instr = ctx.getStore<Instr>()._create_no_hook(reserve, opc);
    instr->numDefs = 0;
  }

  // resizes to correct size, access via []/def/other
  MutInstr(Context &ctx, DialectOpcode opc, uint32_t defs, uint32_t others)
      : ctx(&ctx), sz(others + defs) {
    instr = ctx.getStore<Instr>()._create_no_hook(others + defs, opc);
    instr->numDefs = defs;
  }

  ~MutInstr() {
    if (instr) [[unlikely]]
      ctx->getStore<Instr>()._destroy_no_hook(instr);
  }

  T *begin() { return reinterpret_cast<T *>(&**instr.begin()); }
  T *end() { return reinterpret_cast<T *>(&**instr.begin()) + sz; }

  T *def_begin() { return reinterpret_cast<T *>(&**instr.begin()); }
  T *def_end() {
    return reinterpret_cast<T *>(&**instr.begin()) + instr->numDefs;
  }
  auto defs() { return Range{def_begin(), def_end()}; }
  auto &def(unsigned i = 0) {
    assert(i < instr->numDefs);
    return def_begin()[i];
  }

  T *other_begin() {
    return reinterpret_cast<T *>(&**instr.begin()) + instr->numDefs;
  }
  T *other_end() { return reinterpret_cast<T *>(&**instr.begin()) + sz; }
  auto others() { return Range{other_begin(), other_end()}; }
  auto &other(unsigned i) {
    assert(i < sz - instr->numDefs);
    return other_begin()[i];
  }

  T &operator[](size_t i) { return begin()[i]; }

  void reserve(size_t sz) {
    if (sz <= instr.getNumOperands())
      return;
    instr = ctx->getStore<Instr>().realloc<T>(instr, sz);
  }

  void resize(size_t sz) {
    this->sz = sz;
    if (sz <= instr.getNumOperands())
      return;
    instr = ctx->getStore<Instr>().realloc<T>(instr, sz);
  }

  void shrink_to_fit() {
    if (sz == instr.getNumOperands())
      return;
    instr = ctx->getStore<Instr>().realloc<T>(instr, sz);
  }

  auto getDialectOpcode() { return instr.getDialectOpcode(); }

  void setOpcode(DialectOpcode opc) {
    instr->opc = opc.getOpcodeID();
    instr->dialect = opc.getDialectID();
  }

  auto setNumDefs(uint32_t numDefs) { instr->numDefs = numDefs; }
  // Sets numDefs to current vector size. Same as other in builder, but other
  // already defined here.
  void defsDone() { setNumDefs(size()); }

  auto size() const { return sz; }
  auto getNumDefs() const { return instr->numDefs; }
  auto getNumOthers() const { return sz - instr->numDefs; }
  auto getNumOperands() const { return sz; }

  template <typename... Args> T &emplace_back(Args &&...args) {
    if (sz == instr.getNumOperands()) [[unlikely]]
      reserve(2 * instr.getNumOperands());
    auto &ref = *std::construct_at(&begin()[sz], std::forward<Args>(args)...);
    ++sz;
    return ref;
  }
  T &push_back(const T &t) { emplace_back(t); }
  T &push_back(T &&t) { emplace_back(std::forward<T>(t)); }
  template <typename It> void push_back_range(Range<It> range) {
    if constexpr (requires { range.size(); })
      reserve(size() + range.size());
    for (const auto &item : range) {
      emplace_back(item);
    }
  }
  template <typename It> void push_back_range(It begin, It end) {
    push_back_range(Range{begin, end});
  }

  InstrRef build() {
    shrink_to_fit();
    ctx->getStore<Instr>()._call_create_hooks(instr);

    auto defsR = defs();
    auto othersR = others();

    InstrBuilder ib{instr};
    // todo: need to make sure that this is folded properly
    ib.addRefs(defsR).other().addRefs(othersR);

    instr = nullref;
    return ib.instr();
  }
};

// MutInstr, but with opcode specified late in builder.
template <IsFatDynObjRef T> class OperandVec : public MutInstr<T> {
public:
  OperandVec(Context &ctx, uint32_t numDefs, uint32_t numUses)
      : MutInstr<T>(ctx, _CORE_INVALID_OPC, numDefs + numUses) {
    this->setNumDefs(numDefs);
  }
  OperandVec(Context &ctx, uint32_t numDefs = 1)
      : MutInstr<T>(ctx, _CORE_INVALID_OPC, std::max(numDefs, 64U)) {
    this->setNumDefs(numDefs);
  }

  OperandVec(Context &ctx) : MutInstr<T>(ctx, _CORE_INVALID_OPC) {}

  void pushPlaceholderDefs() {
    assert(this->size() == 0 && "already has operands pushed back?");
    for (unsigned i = 0; i < this->getNumDefs(); i++)
      this->emplace_back(nullref);
  }

  InstrRef build(DialectOpcode opc) {
    this->setOpcode(opc);
    return this->MutInstr<T>::build();
  }
};

// Operand vector for others/uses only. Essentially, OperandVec for the common
// case where all defs are placeholders to be filled by the builder. Requires
// numDefs to be known at construction time.
template <IsFatDynObjRef T> class OtherVec : public OperandVec<T> {
public:
  OtherVec(Context &ctx, uint32_t numDefs, uint32_t numUses)
      : OperandVec<T>(ctx, numDefs, numUses) {
    this->OperandVec<T>::pushPlaceholderDefs();
  }
  OtherVec(Context &ctx, uint32_t numDefs = 1) : OperandVec<T>(ctx, numDefs) {
    this->OperandVec<T>::pushPlaceholderDefs();
  }
  void defsDone() = delete;
  void pushPlaceholderDefs() = delete;
  void setNumDefs() = delete;
};
} // namespace dyno

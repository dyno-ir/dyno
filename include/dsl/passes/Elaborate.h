#pragma once

#include "dsl/DSLBuilder.h"
#include "dyno/DestroyMap.h"
#include "dyno/ObjMap.h"
#include "dyno/Pass.h"
#include "hw/AutoDebugInfo.h"
#include "support/Queue.h"
#include "support/Result.h"
#include "support/StringRef.h"
#include "support/TempBind.h"
#include "support/TwoLevelSet.h"
#include "support/UnionFind.h"
#include <optional>

namespace dyno {

enum class ElabState { Done, Blocked };

struct ElabError {
  enum class Kind {
    InvalidType,
    UnsupportedInstruction,
    TypeMismatch,
    InfiniteType,
    GeneralizedVariable,
    UnresolvedType,
    UnresolvedWork,
    UnknownName,
    DuplicateName,
    InvalidBinding,
    NotCallable,
    UnsupportedOperator
  };

  Kind kind;
  InstrRef origin = nullref;
  DSLTypeRef lhs = nullref;
  DSLTypeRef rhs = nullref;
  StringObjRef member = nullref;
  SmallVec<InstrRef, 2> related = {};
};

using ElabResult = Result<ElabState, ElabError>;

class ElaboratePass : public Pass<ElaboratePass> {
  struct Work {
    InstrRef instr;
    BlockRef scope;
    unsigned level;
  };

  struct TyVarState {
    using KeyInfo = DenseMapInfo<DSLTypeRef>;

    unsigned level;
    DSLTypeRef binding = nullref;
    SmallVec<Work, 2> waiting = {};

    bool isGeneralized() const { return binding == KeyInfo::getTombstoneKey(); }
    void generalize() { binding = KeyInfo::getTombstoneKey(); }
  };
  template <typename T> using TyVarMap = ObjMapVec<DSLTyVar, T>;
  UnionFind<ObjRef<DSLTyVar>, TyVarMap> tyVarEquality;
  TyVarMap<std::optional<TyVarState>> vars;
  Context &ctx;
  BuilderStack<DSLBuilder> build;
  TempBindVal<AutoCopyDebugInfoStack> autoDebugInfo;

  Queue<Work> worklist;
  std::optional<ElabError> lastError;

  struct ScopeState {
  private:
    struct Binding {
      DSLValue visible = nullref;
      DSLValue forward = nullref;
      bool declared = false;
    };
    TwoLevelMap<StringRef, Binding> bindings;

  public:
    DSLTypeRef result = nullref;
    unsigned level = 0;

    ElabResult declare(StringObjRef name, DSLValue value);
    void declareForward(StringRef name, DSLValue value);
    void capture(StringRef name, DSLValue value);
    DSLValue lookup(StringRef name);
    ScopeState makeChild(DSLTypeRef retTy);
  };
  ObjMapVec<Block, std::optional<ScopeState>> scopes;
  SmallVec<BlockRef, 8> sourceBlocks;
  DestroyMap<Instr> destroyMap;

  ElabResult elaboratePattern(OperandRef pattern);
  ElabResult prepareBlock(BlockRef block, ScopeState scope);
  Result<DSLTypeRef, ElabError> typeOf(DSLValue value);
  ElabResult elaborateSource(const Work &work);
  ElabResult elaborateOperator(const Work &work);
  void replaceName(InstrRef instr, DSLValue target);

  TyVarState &state(DSLTyVarRef ref);
  void notify(DSLTyVarRef ref);
  ElabResult occursAndLower(DSLTyVarRef var, DSLTypeRef type);
  ElabResult bind(DSLTyVarRef var, DSLTypeRef type);
  ElabResult elaborate(const Work &work);
  ElabResult normalize(DSLTypeRef type);
  ElabResult normalizeInstr(InstrRef instr);
  ElabResult orderTypes(BlockRef block);

public:
  explicit ElaboratePass(Context &ctx) : ctx(ctx), build(ctx) {}
  static ElaboratePass make(Context &ctx) { return ElaboratePass{ctx}; }

  DSLTyVarRef createTyVar(unsigned level);
  DSLTypeRef resolve(DSLTypeRef type);
  ElabResult unify(DSLTypeRef lhs, DSLTypeRef rhs);
  ElabResult generalize(DSLTyVarRef ref);

  void schedule(InstrRef instr, BlockRef scope, unsigned level);
  ElabResult waitOn(DSLTyVarRef ref, const Work &work);
  ElabResult runWorklist();
  ElabResult run(BlockRef block);
  bool runPass(BlockRef block) { return bool(run(block)); }
  const std::optional<ElabError> &getError() const { return lastError; }
  static constexpr auto runFuncs = mk_tuple(&ElaboratePass::runPass);
};

} // namespace dyno

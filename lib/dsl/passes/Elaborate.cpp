#include "dsl/passes/Elaborate.h"
#include "dsl/DSLInstr.h"
#include "dsl/DSLPrinter.h"
#include "op/OpInstr.h"
#include "op/StructuredControlFlow.h"
#include "support/Debug.h"
#include "support/MacroUtil.h"
#include <algorithm>

namespace dyno {

DSLTyVarRef ElaboratePass::createTyVar(unsigned level) {
  DSLTyVarRef ref = ctx.getStore<DSLTyVar>().create();
  auto &entry = vars.get_ensure(ref);
  entry.emplace(TyVarState{level});
  return ref;
}

ElaboratePass::TyVarState &ElaboratePass::state(DSLTyVarRef ref) {
  return *vars[ref];
}

DSLTypeRef ElaboratePass::resolve(DSLTypeRef type) {
  while (auto ref = type.dyn_as<DSLTyVarRef>()) {
    ref = DSLTyVarRef(tyVarEquality.find(ref).getObjID());
    DSLTypeRef binding = state(ref).binding;
    if (!binding || state(ref).isGeneralized())
      return DSLTypeRef(ref);
    type = binding;
  }
  return type;
}

ElabResult ElaboratePass::occursAndLower(DSLTyVarRef var, DSLTypeRef type) {
  DSLTypeRef target = type;
  SmallVec<DSLTypeRef, 8> pending{type};
  SmallVec<DSLTyVarRef, 8> reachable;
  ObjMapVec<DSLTy, bool> visited;
  while (!pending.empty()) {
    type = resolve(pending.pop_back_val());
    if (auto ref = type.dyn_as<DSLTyVarRef>()) {
      if (ref == var)
        return ElabError{ElabError::Kind::InfiniteType, nullref, var, target};
      if (state(ref).isGeneralized())
        return ElabError{ElabError::Kind::GeneralizedVariable, nullref, var,
                         ref};
      reachable.push_back(ref);
      continue;
    }
    InstrRef shape = type.getTypeI();
    if (!shape)
      continue;
    auto ref = type.as<DSLTyRef>();
    if (visited.get_ensure(ref))
      continue;
    visited[ref] = true;
    for (auto operand : shape.others())
      if (auto child = operand->dyn_as<DSLTypeRef>())
        pending.push_back(child);
  }
  unsigned level = state(var).level;
  for (auto ref : reachable)
    state(ref).level = std::min(state(ref).level, level);
  return ElabState::Done;
}

ElabResult ElaboratePass::bind(DSLTyVarRef var, DSLTypeRef type) {
  if (state(var).isGeneralized())
    return ElabError{ElabError::Kind::GeneralizedVariable, nullref, var, type};
  if (auto other = type.dyn_as<DSLTyVarRef>()) {
    if (state(other).isGeneralized())
      return ElabError{ElabError::Kind::GeneralizedVariable, nullref, var,
                       type};
    auto merge = tyVarEquality.unite(var, other);
    if (!merge.merged)
      return ElabState::Done;
    DSLTyVarRef parent(merge.parent.getObjID());
    DSLTyVarRef child(merge.child.getObjID());
    state(parent).level = std::min(state(parent).level, state(child).level);
    SmallVec<Work, 2> waiting = std::move(state(child).waiting);
    state(child).waiting.clear();
    for (auto work : waiting)
      state(parent).waiting.push_back(work);
    notify(parent);
    return ElabState::Done;
  }
  DYNO_EXPECT(occursAndLower(var, type));
  state(var).binding = type;
  notify(var);
  return ElabState::Done;
}

ElabResult ElaboratePass::unify(DSLTypeRef lhs, DSLTypeRef rhs) {
  lhs = resolve(lhs);
  rhs = resolve(rhs);
  if (lhs == rhs)
    return ElabState::Done;
  if (auto var = lhs.dyn_as<DSLTyVarRef>())
    return bind(var, rhs);
  if (auto var = rhs.dyn_as<DSLTyVarRef>())
    return bind(var, lhs);
  InstrRef a = lhs.getTypeI();
  InstrRef b = rhs.getTypeI();
  if (!a || !b || a.isOpc(DSL_STRUCT, DSL_ENUM) ||
      b.isOpc(DSL_STRUCT, DSL_ENUM) ||
      a.getDialectOpcode() != b.getDialectOpcode() ||
      a.getNumOthers() != b.getNumOthers())
    return ElabError{ElabError::Kind::TypeMismatch, nullref, lhs, rhs};
  for (unsigned i = 0; i < a.getNumOthers(); ++i) {
    DYNO_EXPECT(
        unify(a.other(i)->as<DSLTypeRef>(), b.other(i)->as<DSLTypeRef>()));
  }
  return ElabState::Done;
}

ElabResult ElaboratePass::generalize(DSLTyVarRef ref) {
  DSLTypeRef resolved = resolve(ref);
  ref = resolved.dyn_as<DSLTyVarRef>();
  if (!ref)
    return ElabError{ElabError::Kind::InvalidType, nullref, resolved};
  state(ref).generalize();
  notify(ref);
  return ElabState::Done;
}

void ElaboratePass::schedule(InstrRef instr, BlockRef scope, unsigned level) {
  worklist.push_back({instr, scope, level});
}

ElabResult ElaboratePass::waitOn(DSLTyVarRef ref, const Work &work) {
  DSLTypeRef resolved = resolve(ref);
  ref = resolved.dyn_as<DSLTyVarRef>();
  if (!ref)
    return ElabState::Done;
  if (state(ref).isGeneralized())
    return ElabError{ElabError::Kind::GeneralizedVariable, work.instr, ref};
  state(ref).waiting.push_back(work);
  DYNO_DBG({
    dbgs() << "wait on ";
    dumpObj(ref, ctx);
    dbgs() << "\n  ";
    dumpInstr(DSLInstrRef(work.instr), ctx);
  });
  return ElabState::Blocked;
}

void ElaboratePass::notify(DSLTyVarRef ref) {
  auto waiting = std::move(state(ref).waiting);
  state(ref).waiting.clear();
  for (auto work : waiting) {
    DYNO_DBG({
      dbgs() << "retry after change to ";
      dumpObj(ref, ctx);
      dbgs() << "\n  ";
      dumpInstr(DSLInstrRef(work.instr), ctx);
    });
    worklist.push_back(work);
  }
}

ElabResult ElaboratePass::elaborate(const Work &work) {
  InstrRef instr = work.instr;
  if (instr.getNumDefs()) {
    if (auto type = instr.def()->dyn_as<DSLTyRef>()) {
      if (!instr.isOpc(DSL_FUNC, DSL_TUPLE))
        return ElabError{ElabError::Kind::UnsupportedInstruction, instr, type};
      if (instr.getNumDefs() != 1 ||
          (instr.isOpc(DSL_FUNC) && instr.getNumOthers() != 2) ||
          (instr.isOpc(DSL_TUPLE) && instr.getNumOthers() < 2))
        return ElabError{ElabError::Kind::InvalidType, instr, type};
      for (auto operand : instr.others())
        if (!operand->is<DSLTypeRef>())
          return ElabError{ElabError::Kind::InvalidType, instr, type};
      return ElabState::Done;
    }
  }
  return elaborateSource(work);
}

ElabResult ElaboratePass::runWorklist() {
  auto debug = autoDebugInfo.emplace(ctx);
  while (!worklist.empty()) {
    Work work = worklist.pop_front_val();
    auto insertion = build.changeInsertIt(work.instr);
    auto source = autoDebugInfo->addWithToken(work.instr);
    auto result = elaborate(work);
    if (!result) {
      if (!result.error().origin)
        result.error().origin = work.instr;
      return result.error();
    }
  }
  for (auto [ref, entry] : vars)
    if (entry && !entry->waiting.empty())
      return ElabState::Blocked;
  return ElabState::Done;
}

ElabResult ElaboratePass::normalize(DSLTypeRef type) {
  SmallVec<DSLTypeRef, 8> pending{type};
  ObjMapVec<DSLTy, bool> visited;
  while (!pending.empty()) {
    type = resolve(pending.pop_back_val());
    if (type.is<DSLTyVarRef>())
      return ElabError{ElabError::Kind::UnresolvedType, nullref, type};
    InstrRef shape = type.getTypeI();
    if (!shape)
      continue;
    auto ref = type.as<DSLTyRef>();
    if (visited.get_ensure(ref))
      continue;
    visited[ref] = true;
    for (auto operand : shape.others()) {
      auto operandType = operand->dyn_as<DSLTypeRef>();
      if (!operandType)
        continue;
      DSLTypeRef child = resolve(operandType);
      operand.replace(child);
      pending.push_back(child);
    }
  }
  return ElabState::Done;
}

ElabResult ElaboratePass::normalizeInstr(InstrRef instr) {
  for (auto operand : instr) {
    if (auto operandType = operand->dyn_as<DSLTypeRef>()) {
      DSLTypeRef type = resolve(operandType);
      DYNO_EXPECT(normalize(type));
      if (!operand.isDef())
        operand.replace(type);
    } else if (auto value = operand->dyn_as<DSLValRef>()) {
      DSLTypeRef type = resolve(value->ty);
      DYNO_EXPECT(normalize(type));
      value->ty = type;
    }
  }
  return ElabState::Done;
}

ElabResult ElaboratePass::orderTypes(BlockRef block) {
  ObjMapVec<DSLTy, uint8_t> visited;
  SmallVec<InstrRef, 16> ordered;
  auto visit = [&](this auto &&self, DSLTyRef type) -> ElabResult {
    uint8_t status = visited.get_ensure(type);
    if (status == 2)
      return ElabState::Done;
    if (status == 1)
      return ElabError{ElabError::Kind::InfiniteType, type.getDefI(), type};
    visited[type] = 1;
    InstrRef instr = type.getDefI();
    if (instr.isOpc(DSL_STRUCT, DSL_ENUM)) {
      visited[type] = 2;
      return ElabState::Done;
    }
    for (auto operand : instr.others())
      if (auto child = operand->dyn_as<DSLTyRef>())
        DYNO_EXPECT(self(child));
    visited[type] = 2;
    ordered.push_back(instr);
    return ElabState::Done;
  };
  for (auto sourceBlock : sourceBlocks)
    for (auto instr : sourceBlock)
      if (instr.getNumDefs())
        if (auto type = instr.def()->dyn_as<DSLTyRef>())
          DYNO_EXPECT(visit(type));
  InstrRef anchor = nullref;
  for (auto instr : block)
    if (!instr.getNumDefs() || !instr.def()->is<DSLTyRef>()) {
      anchor = instr;
      break;
    }

  for (auto instr : ordered) {
    ctx.getCFG()[instr].erase();
    auto insertion = anchor ? ctx.getCFG()[anchor] : block.end();
    insertion.insertPrev(instr);
  }
  return ElabState::Done;
}

ElabResult ElaboratePass::run(BlockRef block) {
  lastError.reset();
  worklist.clear();
  scopes.clear();
  sourceBlocks.clear();
  destroyMap.clear();
  destroyMap.resize(ctx.getStore<Instr>().numIDs());
  auto destroyHook = destroyMap.registerCreateHook(ctx.getStore<Instr>());
  for (auto [ref, entry] : vars)
    if (entry)
      entry->waiting.clear();
  auto execute = [&]() -> ElabResult {
    DYNO_EXPECT(prepareBlock(block, ScopeState{}));
    DYNO_EXPECT(result, runWorklist());
    if (result == ElabState::Blocked) {
      for (auto [ref, entry] : vars)
        if (entry && !entry->waiting.empty())
          return ElabError{ElabError::Kind::UnresolvedWork,
                           entry->waiting.front().instr};
    }
    for (auto sourceBlock : sourceBlocks) {
      for (auto instr : sourceBlock) {
        auto normalized = normalizeInstr(instr);
        if (!normalized) {
          if (!normalized.error().origin)
            normalized.error().origin = instr;
          return normalized.error();
        }
      }
    }
    DYNO_EXPECT(orderTypes(block));
    worklist.clear();
    destroyMap.apply(ctx.getStore<Instr>(),
                     [&](InstrRef instr) { ctx.destroy(instr); });
    destroyMap.clear();
    return ElabState::Done;
  };
  auto result = execute();
  if (!result)
    lastError = result.error();
  return result;
}

static bool isSupportedOperator(InstrRef instr) {
  if (auto arith = instr.dyn_as<ArithInstrRef>())
    return arith.isOpc(OP_ADD, OP_SUB, OP_MUL);
  if (auto comp = instr.dyn_as<ComparisonInstrRef>())
    return !comp.isVerilogEquality();
  if (auto bitwise = instr.dyn_as<BitwiseInstrRef>())
    return !bitwise.isOpc(OP_XOR);
  return instr.is<ShiftInstrRef>();
}

ElabResult ElaboratePass::elaboratePattern(OperandRef pattern) {
  if (pattern->is<StringObjRef>() || pattern->is<DSLUnitRef>())
    return ElabState::Done;
  auto value = pattern->dyn_as<DSLValRef>();
  if (!value)
    return ElabError{ElabError::Kind::InvalidBinding, pattern.instr()};
  InstrRef instr = value.getDefI();
  if (instr.isOpc(DSL_TUPLE))
    return ElabError{ElabError::Kind::UnsupportedInstruction, pattern.instr()};
  auto variable = instr.dyn_as<DSLVarInstrRef>();
  if (!variable || value.getNumUses() != 1)
    return ElabError{ElabError::Kind::InvalidBinding, pattern.instr()};
  pattern.replace(variable.getName());
  ctx.destroyInstr(instr);
  return ElabState::Done;
}

ElabResult ElaboratePass::prepareBlock(BlockRef block, ScopeState scope) {
  SmallVec<OperandRef, 8> patterns;
  for (auto instr : block) {
    if (auto assignment = instr.dyn_as<DSLAssignInstrRef>()) {
      if (assignment.isBinding())
        patterns.push_back(assignment.pattern());
    } else if (auto function = instr.dyn_as<DSLFuncInstrRef>()) {
      if (!function.isTy()) {
        auto param = (*function.getBody().begin()).as<DSLParamInstrRef>();
        patterns.push_back(param.pattern());
      }
    }
  }
  // Parameter pattern expressions can live in the enclosing block.
  for (auto pattern : patterns)
    DYNO_EXPECT(elaboratePattern(pattern));
  unsigned level = scope.level;
  bool functionBody = bool(scope.result);
  scopes.get_ensure(block) = std::move(scope);
  sourceBlocks.push_back(block);
  for (auto instr : block) {
    for (auto def : instr.defs()) {
      if (auto value = def->dyn_as<DSLValRef>()) {
        if (!value->ty)
          value->ty = createTyVar(level);
      }
    }
    auto binding = instr.dyn_as<DSLBindingInstrRef>();
    if (!binding)
      continue;
    auto initializer = binding.getInitializer().dyn_as<DSLValRef>();
    if (!initializer) {
      if (binding.isOpc(DSL_LET))
        return ElabError{ElabError::Kind::InvalidBinding, instr};
      continue;
    }
    auto assignment = initializer.getDefI().dyn_as<DSLAssignInstrRef>();
    if (!assignment) {
      if (binding.isOpc(DSL_LET))
        return ElabError{ElabError::Kind::InvalidBinding, instr};
      continue;
    }
    if (functionBody && binding.isOpc(DSL_DEF))
      return ElabError{ElabError::Kind::UnsupportedInstruction, assignment};
    auto name = assignment.getPattern().dyn_as<StringObjRef>();
    if (!name)
      return ElabError{ElabError::Kind::UnsupportedInstruction, assignment};
    if (!functionBody)
      scopes[block]->declareForward(name->data, assignment.getInitializer());
  }
  for (auto instr : block)
    schedule(instr, block, level);
  return ElabState::Done;
}

Result<DSLTypeRef, ElabError> ElaboratePass::typeOf(DSLValue value) {
  if (auto ref = value.dyn_as<DSLValRef>())
    return ref->ty;
  if (value.is<DSLUnitRef>())
    return DSLTypeRef(dsl_unit);
  if (value.is<DSLTypeRef>())
    return DSLTypeRef(dsl_typename);
  return ElabError{ElabError::Kind::InvalidType};
}

ElabResult ElaboratePass::ScopeState::declare(StringObjRef name,
                                              DSLValue value) {
  if (name->data == "_")
    return ElabState::Done;
  Binding &binding = bindings[name->data];
  if (binding.declared) {
    ElabError error{ElabError::Kind::DuplicateName};
    error.member = name;
    return error;
  }
  binding.declared = true;
  binding.visible = value;
  return ElabState::Done;
}

void ElaboratePass::ScopeState::declareForward(StringRef name, DSLValue value) {
  if (name == "_")
    return;
  bindings[name].forward = value;
}

void ElaboratePass::ScopeState::capture(StringRef name, DSLValue value) {
  bindings[name].visible = value;
}

DSLValue ElaboratePass::ScopeState::lookup(StringRef name) {
  auto found = bindings.find(name);
  if (found == bindings.end())
    return nullref;
  const Binding &binding = found.val();
  if (binding.visible)
    return binding.visible;
  return result ? binding.forward : DSLValue(nullref);
}

ElaboratePass::ScopeState
ElaboratePass::ScopeState::makeChild(DSLTypeRef retTy) {
  ScopeState child = *this;
  for (auto &[name, binding] : child.bindings)
    binding.declared = false;
  child.level = level + 1;
  child.result = retTy;
  return child;
}

void ElaboratePass::replaceName(InstrRef instr, DSLValue target) {
  auto value = instr.def()->as<DSLValRef>();
  instr.def().replace(FatDynObjRef<>(nullref));
  build.buildOp(DSL_REF, value, {target});
  build.cfg[instr].erase();
  destroyMap.mark(instr);
}

ElabResult ElaboratePass::elaborateSource(const Work &work) {
  InstrRef instr = work.instr;
  if (isSupportedOperator(instr))
    return elaborateOperator(work);
  ScopeState &scope = *scopes[work.scope];
  if (auto ret = instr.dyn_as<DSLReturnInstrRef>()) {
    if (!scope.result)
      return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
    DYNO_EXPECT(type, typeOf(ret.getValue()));
    DYNO_EXPECT(unify(scope.result, type));
    return ElabState::Done;
  }
  if (auto binding = instr.dyn_as<DSLBindingInstrRef>()) {
    auto initializer = binding.getInitializer().dyn_as<DSLValRef>();
    if (!initializer)
      return ElabState::Done;
    auto assignment = initializer.getDefI().dyn_as<DSLAssignInstrRef>();
    if (!assignment)
      return ElabState::Done;
    DYNO_EXPECT(scope.declare(assignment.getPattern().as<StringObjRef>(),
                              assignment.getInitializer()));
    return ElabState::Done;
  }
  if (!instr.getNumDefs())
    return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
  auto value = instr.def()->dyn_as<DSLValRef>();
  if (!value)
    return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
  auto finish = [&](DSLTypeRef type) -> ElabResult {
    DYNO_EXPECT(unify(value->ty, type));
    return ElabState::Done;
  };
  if (auto function = instr.dyn_as<DSLFuncInstrRef>()) {
    BlockRef body = function.getBody();
    auto param = (*body.begin()).def()->as<DSLValRef>();
    if (!param->ty)
      param->ty = createTyVar(scope.level + 1);
    DSLTyVarRef retTy = createTyVar(scope.level + 1);
    DSLTyRef signature =
        build.buildType(DSL_FUNC, ArrayRef<DSLTypeRef>{retTy, param->ty});
    DYNO_EXPECT(finish(signature));
    ScopeState child = scope.makeChild(retTy);
    if (auto binding = function.findBinding()) {
      auto name = binding.getPattern().as<StringObjRef>();
      if (name->data != "_")
        child.capture(name->data, value);
    }
    if (InstrRef yield = getBlockYield(body)) {
      auto insertion = build.changeInsertIt(yield);
      auto source = autoDebugInfo->addWithToken(yield);
      build.buildReturn(yield.other(0)->as<DSLValue>());
      build.cfg[yield].erase();
      destroyMap.mark(yield);
    }
    DYNO_EXPECT(prepareBlock(body, std::move(child)));
    return ElabState::Done;
  }
  if (auto param = instr.dyn_as<DSLParamInstrRef>()) {
    if (param.getPattern().is<DSLUnitRef>())
      return finish(dsl_unit);
    auto name = param.getPattern().dyn_as<StringObjRef>();
    if (!name)
      return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
    DYNO_EXPECT(scope.declare(name, value));
    return ElabState::Done;
  }
  if (auto assignment = instr.dyn_as<DSLAssignInstrRef>()) {
    if (!assignment.isBinding())
      return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
    return finish(dsl_unit);
  }
  if (auto variable = instr.dyn_as<DSLVarInstrRef>()) {
    StringObjRef name = variable.getName();
    DSLValue target = scope.lookup(name->data);
    if (!target) {
      ElabError error{ElabError::Kind::UnknownName, instr};
      error.member = name;
      return error;
    }
    DYNO_EXPECT(type, typeOf(target));
    DYNO_EXPECT(finish(type));
    replaceName(instr, target);
    return ElabState::Done;
  }
  if (auto ref = instr.dyn_as<DSLRefInstrRef>()) {
    DYNO_EXPECT(type, typeOf(ref.getTarget()));
    return finish(type);
  }
  if (auto literal = instr.dyn_as<DSLIntLitInstrRef>()) {
    uint32_t width =
        literal.isUnsuffixed() ? 32 : literal.getLiteral().getNumBits();
    auto integer = DSLIntRef(width, literal.isSigned());
    if (!width)
      return ElabError{ElabError::Kind::InvalidType, instr, integer};
    return finish(integer);
  }
  if (instr.is<DSLBoolLitInstrRef>())
    return finish(dsl_bool);
  if (instr.is<DSLStringLitInstrRef>())
    return finish(dsl_string);
  if (auto tuple = instr.dyn_as<DSLTupleInstrRef>()) {
    SmallVec<DSLTypeRef, 4> elements;
    for (auto element : tuple.getElements()) {
      DYNO_EXPECT(type, typeOf(element));
      elements.push_back(type);
    }
    return finish(build.buildType(DSL_TUPLE, elements));
  }
  if (auto apply = instr.dyn_as<DSLApplyInstrRef>()) {
    DYNO_EXPECT(callee, typeOf(apply.getCallee()));
    DYNO_EXPECT(arg, typeOf(apply.getArg()));
    DSLTypeRef signature = resolve(callee);
    if (signature.is<DSLTyVarRef>()) {
      DSLTyRef expected =
          build.buildType(DSL_FUNC, ArrayRef<DSLTypeRef>{value->ty, arg});
      DYNO_EXPECT(unify(signature, expected));
      return ElabState::Done;
    }
    InstrRef tyInstr = signature.getTypeI();
    auto shape =
        tyInstr ? tyInstr.dyn_as<DSLFuncInstrRef>() : DSLFuncInstrRef(nullref);
    if (!shape)
      return ElabError{ElabError::Kind::NotCallable, instr, signature};
    DYNO_EXPECT(unify(shape.getParamTy(), arg));
    return finish(shape.getRetTy());
  }
  return ElabError{ElabError::Kind::UnsupportedInstruction, instr};
}

ElabResult ElaboratePass::elaborateOperator(const Work &work) {
  InstrRef instr = work.instr;
  auto value = instr.def()->as<DSLValRef>();
  auto comp = instr.dyn_as<ComparisonInstrRef>();
  auto arith = instr.dyn_as<ArithInstrRef>();
  auto bitwise = instr.dyn_as<BitwiseInstrRef>();
  auto shift = instr.dyn_as<ShiftInstrRef>();
  bool logical = bitwise && bitwise.isOpc(OP_NOT);
  bool neg = arith && arith.isNeg();
  DSLTypeRef resultTy = value->ty;
  if (comp || logical)
    DYNO_EXPECT(unify(resultTy, dsl_bool));
  SmallVec<DSLTypeRef, 4> types;
  for (auto operand : instr.others()) {
    DYNO_EXPECT(type, typeOf(operand->as<DSLValue>()));
    DSLTypeRef resolved = resolve(type);
    types.push_back(resolved);
    if (auto var = resolved.dyn_as<DSLTyVarRef>())
      return waitOn(var, work);
  }
  DSLTypeRef first = types.front();
  bool equality = comp && comp.isEquality();
  bool add = arith && arith.isOpc(OP_ADD);
  auto integer = first.dyn_as<DSLIntRef>();
  if ((logical && !first.is<DSLBoolRef>()) ||
      (!logical && !integer &&
       !(first.is<DSLStringRef>() && (equality || add)) &&
       !(first.is<DSLBoolRef>() && equality)))
    return ElabError{ElabError::Kind::UnsupportedOperator, instr, first};
  for (size_t i = 1; i < types.size(); ++i) {
    auto expected = shift ? DSLTypeRef(DSLIntRef(32, false)) : first;
    DYNO_EXPECT(unify(types[i], expected));
  }
  if (!comp && !logical)
    DYNO_EXPECT(unify(resultTy, first));
  DialectOpcode opcode = instr.getDialectOpcode();
  if (integer && integer.isSigned()) {
    if (comp)
      opcode = comp.getSignedOpcode();
    else if (shift)
      opcode = shift.getSignedOpcode();
  }
  if (opcode != instr.getDialectOpcode() || neg) {
    SmallVec<DSLValue, 4> args;
    if (neg) {
      auto constant = ctx.getStore<Constant>().findOrInsert(
          BigInt::fromU64(0, integer.getNumBits()));
      InstrRef literal = build.buildIntLit(constant, integer.isSigned(), false);
      auto zero = literal.def()->as<DSLValRef>();
      zero->ty = first;
      args.push_back(zero);
    }
    for (auto operand : instr.others())
      args.push_back(operand->as<DSLValue>());
    instr.def().replace(FatDynObjRef<>(nullref));
    build.buildOp(opcode, value, args);
    build.cfg[instr].erase();
    destroyMap.mark(instr);
  }
  return ElabState::Done;
}

} // namespace dyno

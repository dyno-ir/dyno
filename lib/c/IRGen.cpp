#include "c/IRGen.h"
#include "c/AST.h"
#include "c/ASTVisitor.h"
#include "c/MemoryLayout.h"
#include "c/Semantics.h"
#include "c/Symbol.h"
#include "c/Type.h"
#include "ir/IR.h"
#include "ir/SSAIRBuilder.h"
#include "ir/LazySSABuilder.h"
#include <cassert>
#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <iostream>

namespace c {

namespace {
  using Invariants = IRInvariants;
  using IRBuilder = SSAIRBuilder<Invariants>;

SSAType &irType(Type::Kind kind) {
  switch (kind) {
  case Type::BOOL:
    return IntSSAType::get(1);
  case Type::SCHAR:
  case Type::UCHAR:
    return IntSSAType::get(8);
  case Type::SSHORT:
  case Type::USHORT:
    return IntSSAType::get(16);
  case Type::SINT:
  case Type::UINT:
  case Type::PTR:
    return IntSSAType::get(32);
  default:
    assert(false && "Unsupported type for direct IR conversion");
    __builtin_unreachable();
  }
}

constexpr BrCond irBrCond(AST::Kind kind, bool isSigned) {
  switch (kind) {
  case AST::EQ:
    return BrCond::eq();
  case AST::NEQ:
    return BrCond::ne();
  case AST::LT:
    return isSigned ? BrCond::lt() : BrCond::ltu();
  case AST::GT:
    return isSigned ? BrCond::gt() : BrCond::gtu();
  case AST::LTE:
    return isSigned ? BrCond::le() : BrCond::leu();
  case AST::GTE:
    return isSigned ? BrCond::ge() : BrCond::geu();
  default:
    __builtin_unreachable();
  }
}

class StorageBuilder {

public:
  StorageBuilder(IRBuilder &ir) : ir(ir) {}
  enum StorageKind {
    EMPTY,
    SSA,
    STACK,
  };

  class StorageInfo {
  public:
    StorageInfo(Symbol &symbol) : symbol(&symbol) {}
    Symbol *symbol;
    StorageKind kind = EMPTY;
    Operand *ptr = nullptr;
    std::vector<Operand *> ssaDefs;
  };

  StorageInfo &declareLocal(Symbol &s) {
    auto [it, succ] = slots.try_emplace(s.getId(), s);
    return it->second;
  }

  void clear() { slots.clear(); }

  void trackSSADef(StorageInfo &s, Operand &operand) {
    assert(s.kind == SSA);
    s.ssaDefs.push_back(&operand);
  }

  StorageInfo &get(Symbol &symbol) { return get(symbol.getId()); }

  StorageInfo &get(SymbolId id) {
    auto it = slots.find(id);
    assert(it != slots.end());
    return it->second;
  }

private:
   IRBuilder &ir;
  std::unordered_map<SymbolId, StorageInfo> slots;
};

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor>,
                        public ExpressionSemantics::Handler {
public:
  class ExprState {
  public:
    ExprState(Symbol *tmpSymbol, Operand *tmpOperand, ExpressionSemantics sema)
        : tmpSymbol(tmpSymbol), tmpOperand(tmpOperand), sema(std::move(sema)) {}

    Symbol *tmpSymbol;
    Operand *tmpOperand;
    ExpressionSemantics sema;
  };

  IRGenASTVisitor(ASTContext &ctx, SymbolTable &sym)
      : ExpressionSemantics::Handler(ctx), sema(*this), storage(ir), sym(sym) {}

  void visitTranslationUnit(TranslationUnitAST &ast) {
    sym.pushScope(ast.scope);
    for (auto &d : ast.declarations) {
      dispatch(d.get());
    }
    sym.popScope();
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    funcAST = &ast;

    ir.createAndSetFunction(std::string(ast.decl.ident));
    storage.clear();
    ir.createAndSetBlock();
    SSABuilder::sealBlock(ir.getBlock());
    sym.pushScope(ast.funcScope);
    sym.pushScope(ast.blockScope);

    auto returnTypeKind = ast.getType().getBaseType().getKind();
    if (returnTypeKind != Type::VOID) {
      ir.getFunc().returnTypes.push_back(&irType(returnTypeKind));
    }
    for (auto [ident, ty] : ast.getType().getParams()) {
      auto &ssaTy = irType(ty->getKind());
      ir.getFunc().paramTypes.push_back(&ssaTy);
      auto *s = sym.getSymbol(ident, Symbol::Namespace::ORDINARY);
      assert(s);
      auto &sInfo = storage.declareLocal(*s);
      allocateSSA(sInfo);

      tmpSymbol = sInfo.symbol;
      tmpOperand = nullptr;
      sema.fromSymbol(*tmpSymbol);
      store(ir->emitParamRef(ssaTy));
    }

    dispatch(ast.getStatement());

    sym.popScope();
    sym.popScope();
    SSARenumberTable::destroy(ir.getFunc());

    funcAST = nullptr;
  }

  void visitDeclaration(DeclarationAST &ast) {
    for (auto &[d, initializer] : ast.declarators) {
      auto *s = sym.getSymbol(d.ident, Symbol::Namespace::ORDINARY);
      assert(s);
      if (sym.scope().isFile()) {
        if (s->getKind() != Symbol::EXTERN) {
          continue;
        }
        auto &staticMem = ir.getProgram().createStaticMemory(
            std::string(d.ident), GlobalDef::Linkage::EXTERNAL);
        auto [size, align] = mem.getSizeAndAlignment(s->getType());
        staticMem.size = size;
        staticMem.align = align;
        assert(!initializer);
      } else if (sym.scope().isBlock()) {
        auto &sInfo = storage.declareLocal(*s);
        auto tyKind = s->getType().getKind();
        if ((Type::isInteger(tyKind) || tyKind == Type::PTR) &&
            !s->isAddrTaken()) {
          allocateSSA(sInfo);
        } else {
          allocateStack(sInfo);
        }
        if (initializer) {
          dispatch(*initializer);
          sema.expectRValueImplicitConv();
          sema.expectTypeKindImplicitConv(sInfo.symbol->getType().getKind());
          auto rhs = saveExprState();

          tmpSymbol = sInfo.symbol;
          tmpOperand = nullptr;
          sema.fromSymbol(*tmpSymbol);
          store(*rhs.tmpOperand);
        }
      }
    }
  }

  void allocateStack(StorageBuilder::StorageInfo &s) {
    assert(s.kind == StorageBuilder::EMPTY);
    auto [size, align] = mem.getSizeAndAlignment(s.symbol->getType());

    Operand &ptr = ir->emitExternRef(
        irType(Type::PTR),
        ir.getFunc().getFrameLayout().createFrameEntry(size, align));

    s.kind = StorageBuilder::STACK;
    s.ptr = &ptr;
  }

  void allocateSSA(StorageBuilder::StorageInfo &s) {
    assert(s.kind == StorageBuilder::EMPTY);
    s.kind = StorageBuilder::SSA;
    s.ptr = nullptr;
  }

  void visitCompoundSt(CompoundStAST &ast) {
    sym.pushScope(ast.scope);
    for (auto &child : ast.children) {
      dispatch(child.get());
    }
    sym.popScope();
  }

  void visit(AST &ast) { assert(false && "Unsupported"); }

  void visitIntConst(IntConstAST &ast) {
    ir->emitConstInt(ast.num);
    tmpOperand = &ir.getDef();
    sema.fromType(*ast.type);
  }

  void visitVar(VarAST &ast) {
    tmpOperand = nullptr;
    tmpSymbol = sym.getSymbol(ast.ident, Symbol::Namespace::ORDINARY);
    if (!tmpSymbol) {
      error("Symbol undeclared");
    }
    sema.fromSymbol(*tmpSymbol);
  }

  void visitUnop(UnopAST &ast) {
    switch (ast.getKind()) {
    case AST::ADDR: {
      dispatch(ast.getSubExpression());
      sema.expectLValue();
      loadAddr();
      assert(tmpOperand);
      sema.setCategory(ExpressionSemantics::RVALUE);
      sema.ptrType();
      break;
    }
    case AST::DEREF: {
      dispatch(ast.getSubExpression());
      sema.expectRValueImplicitConv();
      sema.expectTypeKindImplicitConv(Type::PTR);
      sema.setCategory(ExpressionSemantics::LVALUE_MEM);
      sema.baseType();
      break;
    }
    case AST::LOG_NOT: {
      ir->emitNot(*genBoolExpression(ast.getSubExpression()));
      tmpOperand = &ir.getDef();
      break;
    }
    case AST::BIT_NOT: {
      dispatch(ast.getSubExpression());
      sema.expectPromotedInt();
      ir->emitNot(*tmpOperand);
      tmpOperand = &ir.getDef();
      break;
    }
    case AST::PLUS: {
      genUsualArithUnop(ast);
      break;
    }
    case AST::MINUS: {
      ir->emitNeg(*genUsualArithUnop(ast));
      tmpOperand = &ir->getDef();
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  void visitMemberAccess(MemberAccessAST &ast) {
    dispatch(*ast.child);
    sema.expectLValue();
    loadAddr();
    assert(tmpOperand);
    auto &structTy = as<StructType>(sema.getType());
    auto idx = structTy.getNamedMemberIdx(ast.ident);
    if (idx < 0) {
      error("Invalid member ident");
    }
    assert(ast.getKind() == AST::ACCESS_MEMBER);
    size_t offset = 0;
    if (!structTy.isUnion()) {
      offset = mem.getStructLayout(structTy).offsets[idx];
    }
    if (offset) {
      ir->emitConstInt(
          MInt{tmpOperand->ssaDef().type().intType().getBits(), offset});
      ir->emitAdd(*tmpOperand, ir.getDef());
      tmpOperand = &ir.getDef();
    }

    sema.setType(structTy.getMemberType(idx));
    sema.setCategory(ExpressionSemantics::LVALUE_MEM);
  }

  void visitBinop(BinopAST &ast) {
    switch (ast.getKind()) {
    case AST::ASSIGN: {
      dispatch(ast.getRHS());
      sema.expectRValueImplicitConv();
      auto rhs = saveExprState();

      dispatch(ast.getLHS());
      sema.expectLValue();
      auto lhs = saveExprState();

      restoreExprState(std::move(rhs));
      sema.expectTypeKindImplicitConv(lhs.sema.getTypeKind());
      rhs = saveExprState();

      restoreExprState(std::move(lhs));
      store(*rhs.tmpOperand);
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::ADD:
    case AST::SUB:
    case AST::MUL:
    case AST::BIT_AND:
    case AST::BIT_OR:
    case AST::BIT_XOR: {
      auto [lhs, rhs] = genUsualArithBinop(ast);
      switch (ast.getKind()) {
      case AST::ADD:
        ir->emitAdd(*lhs, *rhs);
        break;
      case AST::SUB:
        ir->emitSub(*lhs, *rhs);
        break;
      case AST::MUL:
        ir->emitMul(*lhs, *rhs, Type::isSigned(sema.getTypeKind()));
        break;
      case AST::BIT_AND:
        ir->emitAnd(*lhs, *rhs);
        break;
      case AST::BIT_OR:
        ir->emitOr(*lhs, *rhs);
        break;
      case AST::BIT_XOR:
        ir->emitXor(*lhs, *rhs);
        break;
      default:
        __builtin_unreachable();
      }
      tmpOperand = &ir.getDef();
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::LSHIFT:
    case AST::RSHIFT: {
      dispatch(ast.getRHS());
      sema.expectPromotedInt();
      auto rhs = saveExprState();

      dispatch(ast.getLHS());
      sema.expectPromotedInt();
      if (rhs.tmpOperand->ssaDef().type() != tmpOperand->ssaDef().type()) {
        ir->emitExtOrTrunc(Instr::EXT_Z, tmpOperand->ssaDef().type(),
                           *rhs.tmpOperand);
        rhs.tmpOperand = &ir.getDef();
      }
      if (ast.getKind() == AST::LSHIFT) {
        ir->emitShiftLeft(*tmpOperand, *rhs.tmpOperand);
      } else {
        if (Type::isSigned(sema.getTypeKind())) {
          ir->emitShiftRightArith(*tmpOperand, *rhs.tmpOperand);
        } else {
          ir->emitShiftRightLogical(*tmpOperand, *rhs.tmpOperand);
        }
      }
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::EQ:
    case AST::NEQ:
    case AST::LT:
    case AST::LTE:
    case AST::GT:
    case AST::GTE: {
      auto [lhs, rhs] = genUsualArithBinop(ast);
      ir->emitCmp(irBrCond(ast.getKind(), Type::isSigned(sema.getTypeKind())),
                  *lhs, *rhs);
      tmpOperand = &ir.getDef();
      sema.fromType(ctx.make_type<BasicType>(Type::BOOL));
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  Operand *genUsualArithUnop(UnopAST &ast) {
    dispatch(ast.getSubExpression());
    sema.expectPromotedInt();
    return tmpOperand;
  }

  std::pair<Operand *, Operand *> genUsualArithBinop(BinopAST &ast) {
    dispatch(ast.getLHS());
    sema.expectRValueImplicitConv();
    auto lhs = saveExprState();

    dispatch(ast.getRHS());
    sema.expectRValueImplicitConv();
    Type::Kind commonTyKind = ExpressionSemantics::usualArithConv(
        lhs.sema.getTypeKind(), sema.getTypeKind());
    sema.expectTypeKindImplicitConv(commonTyKind);
    auto rhs = saveExprState();

    restoreExprState(std::move(lhs));
    sema.expectTypeKindImplicitConv(commonTyKind);
    return {tmpOperand, rhs.tmpOperand};
  }

  Operand *genBoolExpression(AST &expr) {
    dispatch(expr);
    sema.expectRValueImplicitConv();
    sema.expectTypeKindImplicitConv(Type::BOOL);
    return tmpOperand;
  }

  void visitIfSt(IfStAST &ast) {
    CFGBlock &trueBlock = ir.createBlock();
    CFGBlock *falseBlock = ast.hasElseStatement() ? &ir.createBlock() : nullptr;
    CFGBlock &exitBlock = ir.createBlock();

    Operand *condExpr = genBoolExpression(ast.getExpression());
    ir->emitBrCond(*condExpr, trueBlock, falseBlock ? *falseBlock : exitBlock);

    SSABuilder::sealBlock(trueBlock);
    ir.setBlock(trueBlock);
    dispatch(ast.getStatement());
    ir->emitBr(exitBlock);

    if (falseBlock) {
      SSABuilder::sealBlock(*falseBlock);
      ir.setBlock(falseBlock);
      dispatch(ast.getElseStatement());
      ir->emitBr(exitBlock);
    }

    SSABuilder::sealBlock(exitBlock);
    ir.setBlock(exitBlock);
  }

  void visitWhileSt(WhileStAST &ast) {
    CFGBlock &hdrBlock = ir.createBlock();
    CFGBlock &loopBlock = ir.createBlock();
    CFGBlock &exitBlock = ir.createBlock();
    ir->emitBr(hdrBlock);

    ir.setBlock(hdrBlock);
    Operand *condExpr = genBoolExpression(ast.getExpression());
    ir->emitBrCond(*condExpr, loopBlock, exitBlock);
    SSABuilder::sealBlock(loopBlock);
    ir.setBlock(loopBlock);
    dispatch(ast.getStatement());
    ir->emitBr(hdrBlock);
    SSABuilder::sealBlock(hdrBlock);

    SSABuilder::sealBlock(exitBlock);
    ir.setBlock(exitBlock);
  }

  void visitReturnSt(ReturnStAST &ast) {
    assert(funcAST);
    if (ast.expr) {
      dispatch(*ast.expr);
      sema.expectRValueImplicitConv();
      sema.expectTypeImplicitConv(funcAST->getType().getBaseType());
      ir->emitReturn(*tmpOperand);
    } else {
      ir->emitReturn();
    }
  }

  void visitFunctionCall(FunctionCallAST &ast) {
    dispatch(*ast.child);
    sema.expectCategory(ExpressionSemantics::LVALUE_SYMBOL);
    sema.expectTypeKind(Type::FUNC);
    auto &funcTy = static_cast<FuncType &>(tmpSymbol->getType());
    auto *func = ir.getProgram().getFunction(tmpSymbol->getName());
    assert(func);

    if (ast.args.size() != funcTy.getNumParams()) {
      error("Number of call arguments doesn't match parameters");
    }
    std::vector<Operand *> argOperands;
    unsigned argNum = 0;
    for (auto &arg : ast.args) {
      dispatch(*arg);
      sema.expectRValueImplicitConv();
      sema.expectTypeImplicitConv(funcTy.getParamType(argNum));
      argOperands.push_back(tmpOperand);
      ++argNum;
    }
    ir->emitCall(*func, argOperands);

    tmpOperand = &ir->getDef();
    sema.fromType(funcTy.getBaseType());
  }

  [[noreturn]] void error(std::string_view err) {
    std::cerr << "[IRGen] " << err << "\n";
    assert(false);
    exit(1);
  }

  void loadAddr() {
    assert(sema.isLValue());
    if (sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL) {
      assert(!tmpOperand && tmpSymbol);
      auto &s = storage.get(*tmpSymbol);
      tmpOperand = s.ptr;
      if (s.kind == StorageBuilder::SSA) {
        assert(!tmpOperand);
        return;
      }
    }
    // Do nothing for LVALUE_MEM, because pointer is already in tmpOperand
    assert(tmpOperand);
  }

  void store(Operand &operand) {
    loadAddr();
    if (!tmpOperand) {
      assert(sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL &&
             tmpSymbol);
      auto &s = storage.get(*tmpSymbol);
      assert(s.kind == StorageBuilder::SSA);
      SSABuilder::store(tmpSymbol->getId(), operand, ir.getBlock());
      storage.trackSSADef(s, *tmpOperand);
      tmpOperand = nullptr;
      return;
    }
    auto [size, align] = mem.getSizeAndAlignment(sema.getType());
    ir->emitStore(operand, *tmpOperand,
                  ir.getFunc().createMemoryAccess(size, align));
    tmpOperand = nullptr;
  }

  void load() {
    loadAddr();
    if (!tmpOperand) {
      assert(sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL &&
             tmpSymbol);
      auto &s = storage.get(*tmpSymbol);
      assert(s.kind == StorageBuilder::SSA);
      tmpOperand = SSABuilder::load(tmpSymbol->getId(),
                                    irType(sema.getTypeKind()), ir.getBlock());
      assert(tmpOperand);
      return;
    }
    auto [size, align] = mem.getSizeAndAlignment(sema.getType());
    ir->emitLoad(irType(sema.getTypeKind()), *tmpOperand,
                 ir.getFunc().createMemoryAccess(size, align));
    tmpOperand = &ir.getDef();
  }

  ExpressionSemantics::Result semanticConvLValue() {
    load();
    return ExpressionSemantics::SUCCESS;
  }

  ExpressionSemantics::Result semanticConvBool() {
    ir->emitConstInt(
        MInt::zero(tmpOperand->ssaDef().type().intType().getBits()));
    ir->emitCmp(BrCond::ne(), *tmpOperand, ir.getDef());
    tmpOperand = &ir.getDef();
    return ExpressionSemantics::SUCCESS;
  }

  ExpressionSemantics::Result semanticConvInt(Type::Kind dstKind) {
    ir->emitExtOrTrunc(Type::isSigned(sema.getTypeKind()) ? Instr::EXT_S
                                                          : Instr::EXT_Z,
                       irType(dstKind), *tmpOperand);
    tmpOperand = &ir.getDef();
    // TODO: proper conversion between signed and unsigned
    return ExpressionSemantics::SUCCESS;
  }

  void semanticError(ExpressionSemantics::Result err) {
    error("Semantics error");
  }

  ExprState saveExprState() { return ExprState(tmpSymbol, tmpOperand, sema); }

  void restoreExprState(ExprState &&state) {
    tmpOperand = state.tmpOperand;
    tmpSymbol = state.tmpSymbol;
    sema = std::move(state.sema);
  }

  Operand *tmpOperand = nullptr;
  Symbol *tmpSymbol = nullptr;
  ExpressionSemantics sema;

  FunctionDefinitionAST *funcAST = nullptr;

  IRBuilder ir;
  StorageBuilder storage;
  SymbolTable &sym;
  MemoryLayout mem;
};
} // namespace

std::unique_ptr<Program> IRGenAST(TranslationUnitAST &ast, ASTContext &ctx,
                                  SymbolTable &sym) {
  auto gen = IRGenASTVisitor(ctx, sym);
  sym.clearScopeStack();
  gen.ir.startProgram();
  gen.dispatch(ast);
  return gen.ir.endProgram();
}
}; // namespace c

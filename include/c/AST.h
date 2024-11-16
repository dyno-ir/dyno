#pragma once
#include "c/Symbol.h"
#include "c/Type.h"
#include "support/MachineInt.h"
#include <cassert>
#include <cstddef>
#include <memory>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace c {

class ASTContext {
public:
  template <typename T, typename... Args> T &make_type(Args &&...args) {
    static_assert(!std::is_same_v<BasicType, T>);
    auto *t = new T(std::forward<Args>(args)...);
    types.emplace_back(t);
    return *t;
  }

  template <std::same_as<BasicType> T> BasicType &make_type(Type::Kind kind) {
    assert(Type::isBasic(kind) && "Expected basic kind");
    int idx = kind - Type::BASIC_START - 1;
    return basicTypes[idx];
  }

  Type &qualifyType(Type &ty, QualifiedType::Qualifier qualifer) {
    if (qualifer.none())
      return ty;
    return make_type<QualifiedType>(&ty, qualifer);
  }

private:
  std::array<BasicType, Type::NUM_BASIC> basicTypes = BasicType::createAll();

  std::vector<std::unique_ptr<Type>> types;
};

class AST {
public:
  using Ptr = std::unique_ptr<AST>;
  enum Kind {
    EMPTY,
    ASSIGN,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MUL,
    ASSIGN_DIV,
    ASSIGN_MOD,
    ASSIGN_AND,
    ASSIGN_OR,
    ASSIGN_XOR,
    ASSIGN_LSHIFT,
    ASSIGN_RSHIFT,
    TERNARY,
    COMMA,
    SIZEOF,
    ALIGNOF,
    PLUS,
    MINUS,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    BIT_AND,
    BIT_OR,
    BIT_NOT,
    BIT_XOR,
    LSHIFT,
    RSHIFT,
    LOG_AND,
    LOG_OR,
    LOG_NOT,
    INC_PRE,
    INC_POST,
    DEC_PRE,
    DEC_POST,
    DEREF,
    ACCESS_MEMBER,
    ACCESS_ARRAY,
    FUNCTION_CALL,
    CAST,
    ADDR,
    VAR,
    TYPE,
    CONST_INT,
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,
    ST_LABEL_NAMED,
    ST_LABEL_CASE,
    ST_LABEL_DEFAULT,
    ST_COMPOUND,
    ST_IF,
    ST_WHILE,
    ST_DO_WHILE,
    ST_SWITCH,
    ST_FOR,
    ST_CONTINUE,
    ST_BREAK,
    ST_RETURN,
    ST_GOTO,
    INITIALIZER_LIST,
    DECLARATOR,
    DECLARATION,
    FUNCTION_DEFINITION,
    TRANSLATION_UNIT,
  };

  static const char *kindName(Kind kind);

  AST(Kind kind) : kind(kind) {}
  virtual ~AST() = default;

  Kind getKind() { return kind; }

private:
  Kind kind;
};

class UnopAST : public AST {
public:
  UnopAST(Kind kind, Ptr child) : AST(kind), child(std::move(child)) {}

  AST &getSubExpression() { return *child; }

  std::unique_ptr<AST> child;
};

class BinopAST : public AST {
public:
  BinopAST(Kind kind, Ptr lhs, Ptr rhs)
      : AST(kind), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  AST &getLHS() { return *lhs; }
  AST &getRHS() { return *rhs; }

  Ptr lhs, rhs;
};

class TypeAST : public AST {
public:
  TypeAST(Type *type) : AST(TYPE), type(type) {}
  Type *type;
};

class InitializerListAST : public AST {
public:
  class Entry {
  public:
    Ptr designation;
    Ptr initializer;
  };

  InitializerListAST() : AST(INITIALIZER_LIST) {}

  std::vector<Entry> entries;
};

class TernaryAST : public AST {
public:
  TernaryAST(Ptr cond, Ptr lhs, Ptr rhs)
      : AST(TERNARY), cond(std::move(cond)), lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}

  AST &getLHS() { return *lhs; }
  AST &getRHS() { return *rhs; }
  AST &getCondition() { return *cond; }

  Ptr cond, lhs, rhs;
};

class IntConstAST : public AST {
public:
  IntConstAST(MInt num, Type *type) : AST(CONST_INT), num(num), type(type) {}

  MInt num;
  Type *type;
};

class CastAST : public AST {
public:
  CastAST(Ptr child, Type *type)
      : AST(CAST), child(std::move(child)), type(type) {}

  Ptr child;
  Type *type;
};

class MemberAccessAST : public AST {
public:
  MemberAccessAST(Ptr child, std::string_view ident)
      : AST(ACCESS_MEMBER), ident(ident), child(std::move(child)) {}
  std::string_view ident;
  Ptr child;
};

class ArrAccessAST : public AST {
public:
  ArrAccessAST(Ptr arr, Ptr expr)
      : AST(ACCESS_ARRAY), arr(std::move(arr)), expr(std::move(expr)) {}
  Ptr arr;
  Ptr expr;
};

class FunctionCallAST : public AST {
public:
  FunctionCallAST(Ptr child) : AST(FUNCTION_CALL), child(std::move(child)) {}

  std::vector<Ptr> args;
  Ptr child;
};

class VarAST : public AST {
public:
  std::string_view ident;
  VarAST(std::string_view varName) : AST(VAR), ident(varName) {}
};

class CompoundStAST : public AST {
public:
  CompoundStAST() : AST(ST_COMPOUND), scope(Scope::BLOCK) {}
  std::vector<Ptr> children;
  Scope scope;
};

class LabelStAST : public AST {
public:
  LabelStAST(Ptr st, std::string_view ident)
      : AST(ST_LABEL_NAMED), st(std::move(st)), ident(ident) {}
  LabelStAST(Ptr st) : AST(ST_LABEL_DEFAULT), st(std::move(st)) {}
  LabelStAST(Ptr st, Ptr expr)
      : AST(ST_LABEL_CASE), st(std::move(st)), expr(std::move(expr)) {}
  Ptr st;
  Ptr expr;
  std::string_view ident;
};

class IfStAST : public AST {
public:
  IfStAST(Ptr expr, Ptr st, Ptr stElse = nullptr)
      : AST(ST_IF), expr(std::move(expr)), st(std::move(st)),
        stElse(std::move(stElse)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }
  AST &getElseStatement() { return *stElse; }
  bool hasElseStatement() { return bool(stElse); }

  Ptr expr, st, stElse;
};

class WhileStAST : public AST {
public:
  WhileStAST(Kind kind, Ptr expr, Ptr st)
      : AST(kind), expr(std::move(expr)), st(std::move(st)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }

  Ptr expr, st;
};

class GotoStAST : public AST {
public:
  GotoStAST(std::string_view ident) : AST(ST_GOTO), ident(ident) {}
  std::string_view ident;
};

class SwitchStAST : public AST {
public:
  SwitchStAST(Ptr expr, Ptr st)
      : AST(ST_SWITCH), expr(std::move(expr)), st(std::move(st)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }

  Ptr expr, st;
};

class ForStAST : public AST {
public:
  ForStAST(Ptr initClause, Ptr exprCond, Ptr exprIter, Ptr st)
      : AST(ST_FOR), initClause(std::move(initClause)),
        exprCond(std::move(exprCond)), exprIter(std::move(exprIter)),
        st(std::move(st)) {}

  Ptr initClause, exprCond, exprIter, st;
};

class ReturnStAST : public AST {
public:
  ReturnStAST(Ptr expr) : AST(ST_RETURN), expr(std::move(expr)) {}
  Ptr expr;
};
class LoopCtrlStAST : public AST {
public:
  LoopCtrlStAST(Kind kind) : AST(kind) {}
};

class DeclaratorAST : public AST {
public:
  DeclaratorAST() : AST(DECLARATOR) {}

  DeclaratorAST(DerivedType *type)
      : AST(DECLARATOR), typeBase(type), type(type) {}

  DeclaratorAST(Type *type, DerivedType *typeBase,
                std::string_view ident = std::string_view())
      : AST(DECLARATOR), typeBase(typeBase), type(type), ident(ident) {}

  void spliceEnd(DeclaratorAST o) {
    assert(canSplice() && "Expected spliceable DeclaratorAST");
    if (!o.ident.empty()) {
      ident = o.ident;
    }
    if (type) {
      typeBase->setBaseType(o.type);
    } else {
      type = o.type;
    }
    typeBase = o.typeBase;
  }

  bool canSplice() { return bool(type) == bool(typeBase); }

  DerivedType *typeBase = nullptr;
  Type *type = nullptr;
  std::string_view ident;
};

class DeclarationAST : public AST {
public:
  DeclarationAST(Symbol::Kind symbolKind)
      : AST(DECLARATION), symbolKind(symbolKind) {}

  Symbol::Kind symbolKind;
  std::vector<std::pair<DeclaratorAST, Ptr>> declarators;
};

class FunctionDefinitionAST : public AST {
public:
  FunctionDefinitionAST(Symbol::Kind symbolKind, DeclaratorAST decl)
      : AST(FUNCTION_DEFINITION), symbolKind(symbolKind), decl(std::move(decl)),
        funcScope(Scope::FUNC), blockScope(Scope::BLOCK) {}

  Symbol::Kind symbolKind = Symbol::EMPTY;
  DeclaratorAST decl;
  Ptr st;
  Scope funcScope;
  Scope blockScope;

  CompoundStAST &getStatement() {
    return *static_cast<CompoundStAST *>(st.get());
  }
  FuncType &getType() { return *static_cast<FuncType *>(decl.type); }
};

class TranslationUnitAST : public AST {
public:
  TranslationUnitAST() : AST(TRANSLATION_UNIT), scope(Scope::FILE) {}

  std::vector<Ptr> declarations;
  Scope scope;
};

class ASTError {
public:
  enum Kind {
    EMPTY,
    NOP,
    OTHER,
    EXPECTED_TOKEN,
  };
  ASTError(Kind kind) : kind(kind) {}
  Kind getKind() { return kind; }
  bool isEmpty() { return kind == EMPTY; };
  bool isNop() { return kind == NOP; };

private:
  Kind kind;
};

template <typename T> class ASTResult {
public:
  ASTResult(T &&res) : mRes(std::move(res)), mErr(ASTError::EMPTY) {}
  ASTResult(const T &res) : mRes(res), mErr(ASTError::EMPTY) {}

  ASTResult(ASTError err) : mErr(err) {}

  explicit operator bool() { return mErr.isEmpty(); }

  ASTError &err() {
    assert(!mErr.isEmpty() && "Invalid ASTError accessed");
    return mErr;
  }

  T &res() {
    assert(mErr.isEmpty() && "Invalid ASTResult accessed");
    return mRes;
  }

  T &&moveRes() { return std::move(res()); }

  bool isNop() { return mErr.isNop(); }

  T &operator*() { return res(); }
  T *operator->() { return &res(); }

protected:
  T mRes;
  ASTError mErr;
};

class ASTPtrResult : public ASTResult<std::unique_ptr<AST>> {
public:
  ASTPtrResult() : ASTResult(ASTError::NOP) {}
  ASTPtrResult(std::nullptr_t) : ASTResult(ASTError::NOP) {}
  ASTPtrResult(AST *ast) : ASTResult(std::unique_ptr<AST>(ast)) {
    assert(ast && "Use NOP instead of nullptr");
  }
  ASTPtrResult(ASTError err) : ASTResult(err) {}

  operator AST::Ptr &&() {
    assert(mErr.isEmpty() && "Invalid ASTResult accessed");
    return std::move(mRes);
  }

  template <class U>
  ASTPtrResult(std::unique_ptr<U> &&ast) : ASTResult(std::move(ast)) {}
};

} // namespace c

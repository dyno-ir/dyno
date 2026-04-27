#pragma once
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/DebugInfo.h"
#include "support/Bits.h"
#include "support/CallableRef.h"
#include "support/Lexer.h"
#include "support/Tokenizer.h"
#include "support/VectorLUT.h"
#include "test/IDs.h"
#include "type/TypeContext.h"
#include "type/TypeInfo.h"
#include <format>

namespace dyno {
class TypeDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_TYPE};
  PrinterBase *base;
  OStreamWrapper os;

  void printBaseType(BaseTypeRef baseT) {
    if (!base->ctx) {
      std::print(os, "{}.<type #{}>",
                 base->dialectI[baseT.getDialectID()]->name, baseT.getID());
    }
    os << base->ctx->getCtx<TypeDialectContext>()
              .baseTypeNames[baseT.getDialectID()][baseT.getID()];
  }

  void printArrayType(ArrayTypeRef arrT) {
    if (!base->ctx) {
      std::print(os, "<array #{}>", arrT.getObjID().num);
      return;
    }
    printTypeImpl(base->ctx->resolve(arrT->element));
    if (arrT->start == nullref) {
      os << "[";
      base->printRefOrUse(base->ctx->resolve(arrT->len));
      os << "]";
    } else {
      os << "[";
      base->printRefOrUse(base->ctx->resolve(arrT->start));
      os << "+:";
      base->printRefOrUse(base->ctx->resolve(arrT->len));
      os << "]";
    }
  }

  void printStructType(StructTypeRef structT) {
    if (!base->ctx) {
      std::print(os, "<struct #{}>", structT.getObjID().num);
      return;
    }

    os << "struct { ";
    for (auto &elem : structT->elemns) {
      printTypeImpl(base->ctx->resolve(elem.type));
      os << " ";
      base->ctx->getCtx<TypeDialectContext>().strings.get(elem.ident);
      os << "; ";
    }
    os << "}";
  }

  void printEnumType(EnumTypeRef enumT) {
    if (!base->ctx) {
      std::print(os, "<enum #{}>", enumT.getObjID().num);
      return;
    }

    os << "enum : ";
    printTypeImpl(enumT->underlying);
    os << " { ";
    for (auto &elem : enumT->elemns) {
      base->ctx->getCtx<TypeDialectContext>().strings.get(elem.ident);
      os << " = ";
      base->printRefOrUse(base->ctx->resolve(elem.value));
      os << ", ";
    }
    os << "}";
  }

  bool printTypeImpl(FatTypeRef type) {
    switch (type.typeID()) {
    case TypeDialectTypeID::BASE:
      printBaseType(type.as<BaseTypeRef>());
      break;
    case TypeDialectTypeID::ARRAY:
      printArrayType(type.as<ArrayTypeRef>());
      break;
    case TypeDialectTypeID::STRUCT:
      printStructType(type.as<StructTypeRef>());
      break;
    case TypeDialectTypeID::ENUM:
      printEnumType(type.as<EnumTypeRef>());
      break;
    default:
      return false;
    }
    return true;
  }

  bool printType(FatDynObjRef<> ref, bool def) {
    return printTypeImpl(ref.as<FatTypeRef>());
  }

  TypeDialectPrinter(PrinterBase *base) : base(base), os(base->str) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        dialect,
        CallableRef{this, BindMethod<&TypeDialectPrinter::printType>::fv});
  }
};

class TypeDialectParser {
  ParserBase &base;
  uint32_t structKW;
  uint32_t enumKW;
  VectorLUT<BaseTypeRef> baseTypeKWs;

  FatTypeRef parseStructType() {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    lexer->popEnsure(DynoLexer::op_cbropen);
    SmallVec<StructTypeObj::StructElem, 16> elems;

    while (!lexer->peekIs(DynoLexer::op_cbrclose)) {
      auto fieldType = parseTypeDyn();
      auto ident = lexer->popEnsure(Token::IDENTIFIER);
      auto identStr = lexer->GetIdent(ident.ident.idx);
      auto identIdx =
          ctx->getCtx<TypeDialectContext>().strings.getCanonicalIdx(identStr);
      elems.emplace_back(StructTypeObj::StructElem{
          .type = fieldType.as<TypeRef>(), .ident = identIdx});
      lexer->popEnsure(DynoLexer::op_semicolon);
    }

    lexer->popEnsure(DynoLexer::op_cbrclose);
    auto &store = ctx->getStore<StructTypeObj>();
    StructTypeObj obj{std::move(elems)};
    return store.create(obj);
  }

  FatTypeRef parseEnumType() {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    EnumTypeObj enumObj;
    lexer->popEnsure(DynoLexer::op_colon);
    enumObj.underlying = parseTypeDyn();

    lexer->popEnsure(DynoLexer::op_cbropen);

    while (!lexer->peekIs(DynoLexer::op_cbrclose)) {
      auto ident = lexer->popEnsure(Token::IDENTIFIER);
      auto identStr = lexer->GetIdent(ident.ident.idx);
      auto identIdx =
          ctx->getCtx<TypeDialectContext>().strings.getCanonicalIdx(identStr);
      lexer->popEnsure(DynoLexer::op_equals);

      auto operand = base.parseOperand();
      if (!operand) {
        lexer->printError(operand.error());
        report_fatal_error("lexer error");
      }
      if (operand->isDef)
        report_fatal_error("expected use");

      enumObj.elemns.emplace_back(operand->ref, identIdx);

      if (!lexer->popIf(DynoLexer::op_comma))
        break;
    }

    lexer->popEnsure(DynoLexer::op_cbrclose);
    auto &store = ctx->getStore<EnumTypeObj>();
    return store.create(enumObj);
  }

  FatTypeRef parseArrayTypeImpl(FatTypeRef elemType) {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    lexer->popEnsure(DynoLexer::op_abropen);
    auto startOperand = base.parseOperand();
    if (!startOperand) {
      lexer->printError(startOperand.error());
      report_fatal_error("lexer error");
    }
    if (startOperand->isDef)
      report_fatal_error("expected use");

    DynObjRef start = nullref;
    DynObjRef len = startOperand->ref;

    if (lexer->popIf(DynoLexer::op_pluscolon)) {
      start = startOperand->ref;
      auto lenOperand = base.parseOperand();
      if (!lenOperand) {
        lexer->printError(lenOperand.error());
        report_fatal_error("lexer error");
      }
      if (lenOperand->isDef)
        report_fatal_error("expected use");
      len = lenOperand->ref;
    }

    lexer->popEnsure(DynoLexer::op_abrclose);
    auto &store = ctx->getStore<ArrayTypeObj>();
    ArrayTypeObj obj{start, len, elemType.as<TypeRef>()};
    return store.create(obj);
  }

  FatTypeRef parseArrayType() {
    auto elemType = parseBaseType();
    return parseArrayTypeImpl(elemType);
  }

  FatTypeRef parseBaseType() {
    auto &lexer = *base.lexer;
    auto ident = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
    auto it = baseTypeKWs.find(ident);
    if (!it)
      report_fatal_error("invalid base type: {}", lexer.GetIdent(ident));
    return *it;
  }

public:
  static constexpr DialectID dialect{DIALECT_TYPE};

  friend class ParserBase;

  explicit TypeDialectParser(ParserBase *base) : base(*base) {
    base->interfaces.registerVal<typename ParserBase::obj_parse_fn>(
        dialect,
        CallableRef{this, BindMethod<&TypeDialectParser::parseType>::fv});
    // fake keywords, these are actually idents. this is to avoid registering
    // in DynoLexer and thus excluding from ident usage elsewhere.
    structKW = base->lexer->GetIdentIdx("struct");
    enumKW = base->lexer->GetIdentIdx("enum");
    auto &baseTypeNames = base->ctx.getCtx<TypeDialectContext>().baseTypeNames;
    for (auto [dialectID, dial] : Range{baseTypeNames.getRaw()}.enumerate()) {
      if (dial.empty())
        continue;
      for (auto [typeID, elem] : Range{dial}.enumerate()) {
        auto kw = base->lexer->GetIdentIdx(elem);
        baseTypeKWs.insert(
            kw, BaseTypeRef{DialectID(uint8_t(dialectID)), uint32_t(typeID)});
      }
    }
  }

  FatDynObjRef<> parseType(DialectType type, ArrayRef<char> name, bool isDef) {
    switch (type.type) {
    case TYPE_STRUCT.type:
      return parseStructType();
    case TYPE_ENUM.type:
      return parseEnumType();
    case TYPE_ARRAY.type:
      return parseArrayType();
    case TYPE_BASE.type:
      return parseBaseType();
    default:
      return nullref;
    }
  }
  FatTypeRef parseTypeDyn() {
    auto &lex = *base.lexer;
    if (lex.peekIs(Token::IDENTIFIER)) {
      auto ident = lex.Peek();
      if (ident.ident.idx == structKW) {
        return parseStructType();
      } else if (ident.ident.idx == enumKW) {
        return parseEnumType();
      }
    }
    auto base = parseBaseType();
    if (lex.popIf(DynoLexer::op_abropen))
      return parseArrayTypeImpl(base);

    return base;
  }
};
}; // namespace dyno

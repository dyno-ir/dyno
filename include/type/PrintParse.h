#pragma once
#include "dyno/DebugInfo.h"
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "support/Bits.h"
#include "support/CallableRef.h"
#include "support/Lexer.h"
#include "support/Tokenizer.h"
#include "support/VectorLUT.h"
#include "test/IDs.h"
#include "type/IDs.h"
#include "type/TypeContext.h"
#include "type/TypeInfo.h"
#include <format>

namespace dyno {
class TypeDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_TYPE};
  PrinterBase *base;

  void printBaseType(BaseTypeRef baseT) {
    auto &os = base->str;
    if (!base->ctx) {
      std::print(os, "{}.<type #{}>",
                 base->dialectI[baseT.getDialectID()]->name, baseT.getID());
    }
    os << base->ctx->getCtx<TypeDialectContext>()
              .baseTypeNames[baseT.getDialectID()][baseT.getID()];
  }

  void printArrayType(ArrayTypeRef arrT) {
    auto &os = base->str;
    if (!base->ctx) {
      std::print(os, "<array #{}>", arrT.getObjID().num);
      return;
    }
    printTypeNested(base->ctx->resolve(arrT->element));
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
    auto &os = base->str;
    if (!base->ctx) {
      std::print(os, "<struct #{}>", structT.getObjID().num);
      return;
    }

    os << "struct(";
    base->indentPrint.addIndent();
    base->indentPrint.printNewLineIndent();
    for (auto [back, elem] : Range{structT->elemns}.mark_back()) {
      os << '\"'
         << base->ctx->getCtx<TypeDialectContext>().strings.get(elem.ident)
         << '\"';
      os << ": ";
      printTypeNested(base->ctx->resolve(elem.type));
      if (!back) {
        os << ",";
        base->indentPrint.printNewLineIndent();
      }
    }
    base->indentPrint.removeIndent();
    base->indentPrint.printNewLineIndent();
    os << ")";
  }

  void printEnumType(EnumTypeRef enumT) {
    auto &os = base->str;
    if (!base->ctx) {
      std::print(os, "<enum #{}>", enumT.getObjID().num);
      return;
    }

    os << "enum(";
    printTypeNested(enumT->underlying);
    os << ",";
    base->indentPrint.addIndent();
    base->indentPrint.printNewLineIndent();
    for (auto [back, elem] : Range{enumT->elemns}.mark_back()) {
      os << '\"'
         << base->ctx->getCtx<TypeDialectContext>().strings.get(elem.ident)
         << '\"';
      os << ": ";
      base->printRefOrUse(base->ctx->resolve(elem.value));
      if (!back) {
        os << ",";
        base->indentPrint.printNewLineIndent();
      }
    }
    base->indentPrint.removeIndent();
    base->indentPrint.printNewLineIndent();
    os << ")";
  }

  bool printTypeNested(FatTypeRef type) {
    switch (type.typeID()) {
    // non-deduped printing for base/array. these have bounded size
    // and should be easy to read.
    case TypeDialectTypeID::BASE:
      printBaseType(type.as<BaseTypeRef>());
      break;
    case TypeDialectTypeID::ARRAY:
      printArrayType(type.as<ArrayTypeRef>());
      break;
    // deduped printing for struct/enum. defined once then backref.
    case TypeDialectTypeID::STRUCT:
    case TypeDialectTypeID::ENUM:
      base->printRefOrUse(type, true);
      break;
    default:
      return false;
    }
    return true;
  }

  bool printType(FatDynObjRef<> ref, bool def) {
    auto &os = base->str;
    auto type = ref.as<FatTypeRef>();
    switch (type.typeID()) {
    case TypeDialectTypeID::BASE:
      // we don't want to clutter base/array when nested, but need dyno
      // type when top level for disambiguation.
      os << "type.base(";
      printBaseType(type.as<BaseTypeRef>());
      os << ")";
      break;
    case TypeDialectTypeID::ARRAY:
      os << "array(";
      printArrayType(type.as<ArrayTypeRef>());
      os << ")";
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

  TypeDialectPrinter(PrinterBase *base) : base(base) {
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

  Result<FatTypeRef, ParseError> parseStructType() {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
    SmallVec<StructTypeObj::StructElem, 16> elems;

    while (!lexer->peekIs(DynoLexer::op_rbrclose)) {
      DYNO_EXPECT(identTok, lexer->popExpect(Token::STRING_LITERAL));
      auto ident = identTok.strLit.value;
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_colon));

      DYNO_EXPECT(fieldType, parseTypeDyn());
      auto identIdx =
          ctx->getCtx<TypeDialectContext>().strings.getCanonicalIdx(ident);
      elems.emplace_back(StructTypeObj::StructElem{
          .type = fieldType.as<TypeRef>(), .ident = identIdx});
      if (!lexer->popIf(DynoLexer::op_comma))
        break;
    }

    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
    auto &store = ctx->getStore<StructTypeObj>();
    StructTypeObj obj{std::move(elems)};
    return store.create(obj);
  }

  Result<FatTypeRef, ParseError> parseEnumType() {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    EnumTypeObj enumObj;
    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));

    DYNO_EXPECT(underlying, parseTypeDyn());
    enumObj.underlying = underlying;

    if (lexer->popIf(DynoLexer::op_comma))
      while (!lexer->peekIs(DynoLexer::op_rbrclose)) {
        DYNO_EXPECT(identTok, lexer->popExpect(Token::STRING_LITERAL));
        auto ident = identTok.strLit.value;
        auto identIdx =
            ctx->getCtx<TypeDialectContext>().strings.getCanonicalIdx(ident);
        DYNO_EXPECT(lexer->popExpect(DynoLexer::op_colon));

        DYNO_EXPECT(operand, base.parseUseOperand());
        enumObj.elemns.emplace_back(operand, identIdx);

        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }

    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
    auto &store = ctx->getStore<EnumTypeObj>();
    return store.create(enumObj);
  }

  Result<FatTypeRef, ParseError> parseArrayTypeImpl(FatTypeRef elemType) {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;

    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_abropen));
    DYNO_EXPECT(startOperand, base.parseOperand());

    DynObjRef start = nullref;
    DynObjRef len = startOperand.ref;

    if (lexer->popIf(DynoLexer::op_pluscolon)) {
      start = startOperand.ref;
      DYNO_EXPECT(lenOperand, base.parseOperand());
      len = lenOperand.ref;
    }

    DYNO_EXPECT(lexer->popExpect(DynoLexer::op_abrclose));
    auto &store = ctx->getStore<ArrayTypeObj>();
    ArrayTypeObj obj{start, len, elemType.as<TypeRef>()};
    return store.create(obj);
  }

  Result<FatTypeRef, ParseError> parseArrayType() {
    auto state = base.lexer->getState();
    DYNO_EXPECT(arrType, parseTypeDyn());
    if (!arrType.is<ArrayTypeRef>())
      return base.lexer->makeErrorStartingAtToLast(state,
                                                   "expected array type");
    return arrType;
  }

  Result<FatTypeRef, ParseError> parseBaseType() {
    auto &lexer = *base.lexer;
    DYNO_EXPECT(identTok, lexer.peekExpect(Token::IDENTIFIER));
    auto it = baseTypeKWs.find(identTok.ident.idx);
    if (!it)
      return lexer.makeErrorOnPeekToken("invalid base type: {}",
                                        lexer.GetIdent(identTok.ident.idx));
    lexer.Pop();
    return *it;
  }

public:
  static constexpr DialectID dialect{DIALECT_TYPE};

  friend class ParserBase;

  explicit TypeDialectParser(ParserBase *base) : base(*base) {
    base->interfaces.registerVal<typename ParserBase::obj_parse_fn>(
        dialect,
        CallableRef{this, BindMethod<&TypeDialectParser::parseType>::fv});
  }

  void registerKWs() {
    baseTypeKWs.clear();

    // fake keywords, these are actually idents. this is to avoid registering
    // in DynoLexer and thus excluding from ident usage elsewhere.
    structKW = base.lexer->GetIdentIdx("struct");
    enumKW = base.lexer->GetIdentIdx("enum");
    auto &baseTypeNames = base.ctx.getCtx<TypeDialectContext>().baseTypeNames;
    for (auto [dialectID, dial] : Range{baseTypeNames.getRaw()}.enumerate()) {
      if (dial.empty())
        continue;
      for (auto [typeID, elem] : Range{dial}.enumerate()) {
        auto kw = base.lexer->GetIdentIdx(elem);
        baseTypeKWs.insert(
            kw, BaseTypeRef{DialectID(uint8_t(dialectID)), uint32_t(typeID)});
      }
    }
  }

  Result<FatDynObjRef<>, ParseError>
  parseType(DialectType type, ArrayRef<char> name, bool isDef) {
    // todo: efficient method
    registerKWs();

    switch (type.type) {
    case TYPE_STRUCT.type: {
      DYNO_EXPECT(rv, parseStructType());
      return rv;
    }
    case TYPE_ENUM.type: {
      DYNO_EXPECT(rv, parseEnumType());
      return rv;
    }
    case TYPE_ARRAY.type: {
      DYNO_EXPECT(base.lexer->popExpect(DynoLexer::op_rbropen));
      DYNO_EXPECT(rv, parseArrayType());
      DYNO_EXPECT(base.lexer->popExpect(DynoLexer::op_rbrclose));
      return rv;
    }
    case TYPE_BASE.type: {
      DYNO_EXPECT(base.lexer->popExpect(DynoLexer::op_rbropen));
      DYNO_EXPECT(rv, parseBaseType());
      DYNO_EXPECT(base.lexer->popExpect(DynoLexer::op_rbrclose));
      return rv;
    }
    default:
      return nullref;
    }
  }
  Result<FatTypeRef, ParseError> parseTypeDyn() {
    auto &lex = *base.lexer;

    // named, go back up to root parser
    if (lex.peekIs(Token::PCT_IDENTIFIER)) {
      auto rv = base.parseOperand();
      if (!rv) {
        base.lexer->printError(rv.error());
        report_fatal_error("expected identifier");
      }
      if (!rv.value().ref.is<FatTypeRef>())
        base.lexer->printErrorOnPeekToken("expected type");
      auto ref = rv.value().ref.as<FatTypeRef>();
      while (lex.peekIs(DynoLexer::op_abropen)) {
        DYNO_EXPECT(next, parseArrayTypeImpl(ref));
        ref = next;
      }
      return ref;
    }

    if (lex.peekIs(Token::IDENTIFIER)) {
      auto ident = lex.Peek();
      if (ident.ident.idx == structKW) {
        return parseStructType();
      } else if (ident.ident.idx == enumKW) {
        return parseEnumType();
      }
    }
    DYNO_EXPECT(base, parseBaseType());
    FatTypeRef iter = base;
    while (lex.peekIs(DynoLexer::op_abropen)) {
      DYNO_EXPECT(next, parseArrayTypeImpl(iter));
      iter = next;
    }
    return iter;
  }
};
}; // namespace dyno

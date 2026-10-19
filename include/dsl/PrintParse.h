#pragma once

#include "dsl/DSLContext.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Parser.h"

namespace dyno {

class DSLDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_DSL};
  PrinterBase *base;
  DSLDialectPrinter(PrinterBase *base) : base(base) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        dialect,
        CallableRef{this, BindMethod<&DSLDialectPrinter::printType>::fv});
  }

  bool printType(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case DSL_INT.type:
    case DSL_UINT.type:
      base->printTypeDefault(ref);
      base->str << "(" << ref.as<DSLIntRef>().getNumBits() << ")";
      return true;
    case DSL_VAL.type: {
      auto ty = ref.as<DSLValRef>()->getTy();
      if (!ty)
        return false;
      base->printTypeDefault(ref);
      base->str << "(";
      base->printRefOrUse(ty);
      base->str << ")";
      return true;
    }
    default:
      return false;
    }
  }
};

class DSLDialectParser {
  ParserBase *base;

public:
  static constexpr DialectID dialect{DIALECT_DSL};

  explicit DSLDialectParser(ParserBase *base) : base(base) {
    base->interfaces.registerVal<ParserBase::obj_parse_fn>(
        dialect,
        CallableRef{this, BindMethod<&DSLDialectParser::parseObj>::fv});
  }

  Result<FatDynObjRef<>, ParseError> parseObj(DialectType type,
                                              ArrayRef<char> name, bool isDef) {
    auto &lexer = *base->lexer;
    switch (type.type) {
    case DSL_INT.type:
    case DSL_UINT.type: {
      DYNO_EXPECT(lexer.popExpect(DynoLexer::op_rbropen));
      DYNO_EXPECT(width, lexer.popExpect(Token::INT_LITERAL));
      if (width.intLit.isSigned)
        return lexer.makeErrorOnPeekToken(
            "expected a positive integer bit width");
      DYNO_EXPECT(lexer.popExpect(DynoLexer::op_rbrclose));
      return DSLIntRef(width.intLit.value, type == DSL_INT);
    }
    case DSL_VAL.type: {
      auto val = base->ctx.getStore<DSLVal>().create();
      if (!lexer.popIf(DynoLexer::op_rbropen))
        return val;
      DYNO_EXPECT(ty, base->parseOperand());
      auto typeRef = ty.ref.dyn_as<DSLTypeRef>();
      if (!typeRef)
        return lexer.makeErrorOnPeekToken("expected a DSL type ref");
      val->ty = typeRef;
      DYNO_EXPECT(lexer.popExpect(DynoLexer::op_rbrclose));
      return val;
    }
    case DSL_TY.type:
      return base->ctx.getStore<DSLTy>().create();
    case DSL_TYVAR.type:
      return base->ctx.getStore<DSLTyVar>().create();
    case DSL_UNIT.type:
      return dsl_unit;
    case DSL_BOOL.type:
      return dsl_bool;
    case DSL_STRING.type:
      return dsl_string;
    case DSL_TYPENAME.type:
      return dsl_typename;
    default:
      return nullref;
    }
  }
};
} // namespace dyno

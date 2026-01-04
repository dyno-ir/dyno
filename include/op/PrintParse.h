#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/InstrPrinter.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "op/MapObj.h"
#include "support/Lexer.h"
#include "support/TemplateUtil.h"

namespace dyno {
class OpDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_OP};
  PrinterBase *base;
  OpDialectPrinter(PrinterBase *base) : base(base) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        dialect, MemberRef{this, BindMethod<&OpDialectPrinter::printType>::fv});
  }

  bool printType(FatDynObjRef<> ref, bool def) {
    auto &str = base->str;

    switch (ref.getTyID()) {
    case OP_MAP.type: {
      auto asMap = ref.as<MapRef>();
      bool first = true;
      std::print(str, "map(");
      for (auto [key, value] : asMap->data) {
        if (!first)
          std::print(str, ", ");
        std::print(str, "\"{}\" : \"{}\"", key, value);
        first = false;
      }
      std::print(str, ")");
      break;
    }

    default:
      return false;
    }
    return true;
  }
};

template <typename Derived> class OpDialectParser {
  ParserBase<Derived> *base;
  // todo: real reference wrapper
public:
  static constexpr DialectID dialect{DIALECT_OP};
  NewDeleteObjStore<MapObj> *mapStore;
  OpDialectParser(ParserBase<Derived> *base) : base(base) {
    base->interfaces
        .template registerVal<typename ParserBase<Derived>::obj_parse_fn>(
            dialect,
            MemberRef{this,
                      BindMethod<&OpDialectParser<Derived>::parseObj>::fv});
  }

  FatDynObjRef<> parseObj(DialectType type, ArrayRef<char> name) {
    DynoLexer &lexer = *base->lexer;

    switch (type.type) {
    case OP_MAP.type: {
      lexer.popEnsure(DynoLexer::op_rbropen);
      std::map<std::string, std::string> map;
      while (lexer.peekIs(Token::STRING_LITERAL)) {
        auto key = lexer.Pop().strLit.value;
        lexer.popEnsure(DynoLexer::op_colon);
        auto val = lexer.popEnsure(Token::STRING_LITERAL).strLit.value;
        map.insert(std::make_pair(key, val));
        if (!lexer.popIf(DynoLexer::op_comma))
          break;
      }
      lexer.popEnsure(DynoLexer::op_rbrclose);
      return mapStore->create(std::move(map));
    }

    default:
      return nullref;
    }
  }
};
}; // namespace dyno
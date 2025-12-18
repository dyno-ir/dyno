#pragma once

#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/SensList.h"

namespace dyno {

class HWParser : public Parser<HWParser> {
  HWContext &ctx;

public:
  explicit HWParser(HWContext &ctx) : ctx(ctx) {
    interfaces.registerVal(
        DIALECT_HW, static_cast<Parser::obj_parse_fn>(&HWParser::parseHW));
    this->sourceLocInfo = &ctx.sourceLocInfo;
  }
  auto &getInstrs() { return ctx.getInstrs(); }
  auto &getConstants() { return ctx.getConstants(); }
  auto &getCFG() { return ctx.getCFG(); }

  FatDynObjRef<> parseHW(DialectType type, ArrayRef<char> name) {
    switch (*type) {
    case *HW_MODULE: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto str = lexer->popEnsure(Token::STRING_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      return ctx.getModules().create(std::string(str.strLit.value));
    }
    case *HW_REGISTER: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto bits = lexer->popEnsure(Token::INT_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      auto reg = ctx.getRegs().create(bits.intLit.value);
      if (!name.empty() && !std::isdigit(name[0]))
        ctx.regNameInfo.addName(reg, std::string_view{name});
      return reg;
    }
    case *HW_WIRE: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto bits = lexer->popEnsure(Token::INT_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      return ctx.getWires().create(bits.intLit.value);
    }
    case *HW_PROCESS: {
      return ctx.getProcs().create();
    }
    case *HW_TRIGGER: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto trigger = ctx.getTriggers().create();
      while (lexer->peekIs(Token::IDENTIFIER)) {
        auto ident =
            lexer->GetIdent(lexer->popEnsure(Token::IDENTIFIER).ident.idx);
        if (ident == "pos")
          trigger->addMode(SensMode::POSEDGE);
        else if (ident == "neg")
          trigger->addMode(SensMode::NEGEDGE);
        else if (ident == "any")
          trigger->addMode(SensMode::ANYEDGE);
        else if (ident == "iff")
          trigger->addMode(SensMode::IFF);
        else if (ident == "iffn")
          trigger->addMode(SensMode::IFFN);
        else
          abort();
        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      lexer->popEnsure(DynoLexer::op_rbrclose);
      return trigger;
    }
    }

    return nullref;
  }
};

}; // namespace dyno

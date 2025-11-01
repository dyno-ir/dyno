#pragma once

#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "support/ErrorRecovery.h"

namespace dyno {

class HWParser : public Parser<HWParser> {
  HWContext &ctx;

public:
  explicit HWParser(HWContext &ctx) : ctx(ctx) {
    interfaces.registerVal(
        DIALECT_HW, static_cast<Parser::obj_parse_fn>(&HWParser::parseHW));
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
      if (!name.empty())
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
    }

    return nullref;
  }
};

}; // namespace dyno
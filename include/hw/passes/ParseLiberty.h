#pragma once
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "support/ErrorRecovery.h"
#include "support/Lexer.h"
#include "support/SlabAllocator.h"
#include <fstream>
#include <iterator>
#include <variant>

namespace dyno {

class LibertyLexer : public Lexer<false, false, true> {
public:
  // #define FOR_KEYWORDS(x) x(library) x(define)
  // #define FUNC(x) #x,
  //   constexpr static auto Keywords = std::to_array({FOR_KEYWORDS(FUNC)});
  // #undef FUNC
  // #define FUNC(x) kw_##x,
  constexpr static std::array<const char *, 0> Keywords;

  constexpr static auto Operators =
      std::to_array({":", "(", ")", "{", "}", ",", ";", "\\"});
  enum OperatorEnum {
    _op_start = Lexer::TOK_OPS_START - 1,
    op_colon,
    op_rbropen,
    op_rbrclose,
    op_cbropen,
    op_cbrclose,
    op_comma,
    op_semicolon
  };

  LibertyLexer(std::string &&src, std::string &&srcPath)
      : Lexer(std::move(src), std::move(srcPath), Operators, Keywords) {}
};

class LibertyParser {
  LibertyLexer &lexer;

public:
  struct Var {
    Token name;
    Token val;

    Var(Token name, Token val) : name(name), val(val) {}
  };

  struct Block {
    uint32_t name;
    SmallVec<Token, 2> params;
    SmallVec<Var, 4> vars;
    SmallVec<Block *, 2> blocks;
  };
  SlabAllocator<Block> blockStore;

private:
  std::variant<Block *, Var> parseSingle() {
    auto ident = lexer.popEnsure(Token::IDENTIFIER);
    if (lexer.popIf(LibertyLexer::op_colon)) {
      auto val = lexer.Pop();
      auto rv = Var{ident, val};
      lexer.popEnsure(LibertyLexer::op_semicolon);
      return rv;
    }

    Block *bl = blockStore.allocate();
    bl->name = ident.ident.idx;

    if (lexer.popIf(LibertyLexer::op_rbropen)) {
      while (!lexer.peekIs(LibertyLexer::op_rbrclose)) {
        bl->params.emplace_back(
            lexer.popEnsure(Token::STRING_LITERAL, Token::INT_LITERAL,
                            Token::IDENTIFIER, Token::NUMERIC_LITERAL));
        if (!lexer.popIf(LibertyLexer::op_comma))
          break;
      }
      lexer.popEnsure(LibertyLexer::op_rbrclose);
    }

    if (lexer.popIf(LibertyLexer::op_semicolon))
      return bl;

    if (lexer.popIf(LibertyLexer::op_cbropen)) {
      while (!lexer.peekIs(LibertyLexer::op_cbrclose)) {
        auto res = parseSingle();
        if (auto block = std::get_if<Block *>(&res))
          bl->blocks.emplace_back(*block);
        else if (auto var = std::get_if<Var>(&res))
          bl->vars.emplace_back(*var);
      }
      lexer.popEnsure(LibertyLexer::op_cbrclose);
    }
    return bl;
  }

public:
  Block *parse() {
    auto rv = parseSingle();
    if (auto block = std::get_if<Block *>(&rv))
      return *block;
    return nullptr;
  }

public:
  LibertyParser(LibertyLexer &lexer) : lexer(lexer), blockStore() {}
};

class LibertyToDyno {
  HWContext &ctx;
  LibertyLexer &lexer;

public:
  void copyIntoCtx(LibertyParser::Block *block) {
    // proper way would be making these real lexer keywords,
    // but then we would have to add keywords for all liberty constructs or
    // add special handling in parser for these.
    uint32_t kwLibrary = lexer.GetIdentIdx("library");
    uint32_t kwCell = lexer.GetIdentIdx("cell");
    uint32_t kwPin = lexer.GetIdentIdx("pin");
    uint32_t kwDirection = lexer.GetIdentIdx("direction");
    uint32_t kwFunction = lexer.GetIdentIdx("function");

    auto err = []() { report_fatal_error("liberty format error"); };

    if (block->name != kwLibrary)
      err();

    for (auto *object : block->blocks) {
      if (object->name != kwCell)
        continue;

      if (object->params.size() != 1 ||
          object->params.front().type != Token::STRING_LITERAL)
        err();
      auto mod =
          ctx.createModule(object->params[0].strLit.value, HW_STDCELL_DEF);
      HWInstrBuilder build{ctx};
      build.setInsertPoint(mod.block().end());

      for (auto sub : object->blocks) {
        if (sub->name != kwPin)
          continue;
        std::optional<Token> direction = std::nullopt;
        std::optional<Token> function = std::nullopt;
        for (auto var : sub->vars) {
          if (var.name.type != Token::IDENTIFIER)
            continue;
          auto relevantKey = [&](uint32_t kw, std::optional<Token> &opt) {
            if (var.name.ident.idx == kw) {
              if (opt)
                err();
              opt = var.val;
            }
          };
          relevantKey(kwDirection, direction);
          relevantKey(kwFunction, function);
        }
        auto fmtCheck = [&](std::optional<Token> &opt) {
          if (!opt || opt->type != Token::STRING_LITERAL)
            err();
        };
        fmtCheck(direction);
        RegisterRef port;
        if (direction->strLit.value == "input")
          port = build.buildInputPort(mod);
        else if (direction->strLit.value == "output")
          port = build.buildOutputPort(mod);
        else if (direction->strLit.value == "internal")
          port = build.buildRegister();
        else
          err();

        if (sub->params.size() != 1 ||
            sub->params[0].type != Token::STRING_LITERAL)
          break;
        port->numBits = 1;
        ctx.regNameInfo.addName(port, sub->params[0].strLit.value);
      }
    }
  }

public:
  LibertyToDyno(HWContext &ctx, LibertyLexer &lexer) : ctx(ctx), lexer(lexer) {}
};

class ParseLibertyPass {
  HWContext &ctx;

public:
  void run() {
    std::string path = "sky130_fd_sc_hd__tt_025C_1v80.lib";
    std::ifstream ifs{path};
    std::string code(std::istreambuf_iterator<char>{ifs},
                     std::istreambuf_iterator<char>{});
    LibertyLexer lex{std::move(code), std::move(path)};
    LibertyParser parse{lex};
    auto block = parse.parse();
    if (!block)
      report_fatal_error("unable to parse lib file");
    LibertyToDyno libToDyno{ctx, lex};
    libToDyno.copyIntoCtx(block);
  }

  explicit ParseLibertyPass(HWContext &ctx) : ctx(ctx) {}
};
}; // namespace dyno

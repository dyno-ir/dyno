#pragma once
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/SensList.h"
#include "support/ErrorRecovery.h"
#include "support/Lexer.h"
#include "support/MMap.h"
#include "support/SlabAllocator.h"
#include "support/Utility.h"
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

  LibertyLexer(ArrayRef<char> src, std::string &&srcPath)
      : Lexer(src, std::move(srcPath), Operators, Keywords) {}
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

template <std::invocable<std::string_view> FuncT> class LibertyExprParser {
  std::string_view expr;
  size_t i = 0;
  HWInstrBuilder &build;
  FuncT nameLookupFunc;

  HWValue parseTerminal() {
    while (isspace(expr[i]))
      i++;
    switch (expr[i++]) {
    case '!':
      return build.buildNot(parseExpr());
    case '0':
      return ConstantRef::fromBool(false);
    case '1':
      return ConstantRef::fromBool(true);
    default: {
      i--;
      bool inQuotes = false;
      if (expr[i] == '\\' && expr[i + 1] == '\"') {
        inQuotes = true;
        i += 2;
      }
      uint start = i;
      while (i != expr.size() && (isalnum(expr[i]) || expr[i] == '_'))
        i++;
      auto sub = expr.substr(start, i - start);
      if (inQuotes) {
        if (expr[i] != '\\' || expr[i + 1] != '\"')
          report_fatal_error("liberty expr format \"\\\"\" delimit error");
      }
      RegisterRef reg = nameLookupFunc(sub);
      return build.buildLoad(reg);
    }
    case '(': {
      auto rv = parseExpr();
      checkChar(')');
      return rv;
    }
    }
  }

  void checkChar(char c) {
    if (expr[i] != c)
      report_fatal_error("liberty format");
    i++;
  }
  template <typename... Values> bool charIs(char c, Values... values) {
    return ((c == values) || ...);
  }

  HWValue parseExpr(uint minPrec = 0) {
    auto lhs = parseTerminal();
    while (1) {
      if (i == expr.size())
        return lhs;
      switch (expr[i++]) {

      case '\'': {
        return build.buildNot(lhs);
      }

      case '^': {
        if (3 < minPrec)
          return lhs;
        return build.buildXor(lhs, parseExpr(3));
      }
      case ' ': {
        uint j = 0;
        while ((i + j < expr.size() - 1) && expr[i + j] == ' ')
          j++;
        // try to match a real operator, only treat space as AND if none found.
        if (charIs(expr[i + j], '&', '*', '^', '|', '+'))
          break;
        i += j + 1;
        [[fallthrough]];
      }
      case '&':
      case '*': {
        if (2 < minPrec)
          return lhs;
        lhs = build.buildAnd(lhs, parseExpr(2));
        break;
      }

      case '+':
      case '|': {
        if (1 < minPrec)
          return lhs;
        lhs = build.buildOr(lhs, parseExpr(1));
        break;
      }

      default:
        i--;
        return lhs;
      }
    }
  }

public:
  HWValue parseExpr(std::string_view expr) {
    i = 0;
    this->expr = expr;
    return parseExpr();
  }
  LibertyExprParser(HWInstrBuilder &build, FuncT nameLookupFunc)
      : build(build), nameLookupFunc(nameLookupFunc) {}
};

class LibertyToDyno {
  HWContext &ctx;
  LibertyLexer &lexer;

  void err() { report_fatal_error("liberty format error"); };

  void handleFF(LibertyParser::Block *block, HWInstrBuilderStack &build,
                HWInstrBuilder &regBuild, ArrayRef<RegisterRef> outs) {

#define ATTRS_RSTVAL(x) x(clear_preset_var1) x(clear_preset_var2)
#define ATTRS_BEXPR(x)                                                         \
  x(clear) x(clocked_on) x(clocked_on_also) x(next_state) x(preset)
#define ATTRS(x) ATTRS_BEXPR(x) ATTRS_RSTVAL(x)
#define LAMBDA(x) const uint32_t kw_##x = lexer.GetIdentIdx(#x);
    ATTRS(LAMBDA)
#undef LAMBDA

    auto proc = build.buildProcess();
    build.pushInsertPoint(proc.block().end());

    struct AbstractFF {
#define LAMBDA(x) HWValue val_##x = nullref;
      ATTRS_BEXPR(LAMBDA)
#undef LAMBDA
#define LAMBDA(x) char val_##x = 0;
      ATTRS_RSTVAL(LAMBDA)
#undef LAMBDA
    };

    AbstractFF ff;
    auto handleRstval = [&](char &out, Token val) {
      std::string_view str;
      if (val.type == Token::IDENTIFIER)
        str = lexer.GetIdent(val.ident.idx);
      else if (val.type == Token::STRING_LITERAL)
        str = val.strLit.value;
      else
        err();

      if (str.size() != 1 || (str[0] != 'L' && str[0] != 'H' && str[0] != 'N' &&
                              str[0] != 'T' && str[0] != 'X'))
        err();

      out = str[0];
    };

    auto handleExpr = [&](HWValue &out, Token val) {
      if (val.type != Token::STRING_LITERAL)
        err();
      out = parseExpr(val.strLit.value, build);
    };

    for (auto var : block->vars) {
      if (var.name.type != Token::IDENTIFIER)
        err();

#define LAMBDA(x)                                                              \
  if (var.name.ident.idx == kw_##x) {                                          \
    handleRstval(ff.val_##x, var.val);                                         \
  }
      ATTRS_RSTVAL(LAMBDA)
#undef LAMBDA
#define LAMBDA(x)                                                              \
  if (var.name.ident.idx == kw_##x) {                                          \
    handleExpr(ff.val_##x, var.val);                                           \
  }
      ATTRS_BEXPR(LAMBDA)
#undef LAMBDA
    }

    if (!ff.val_next_state)
      err();
    HWValue val = ff.val_next_state;

    if (!ff.val_clocked_on)
      err();

    auto clkReg = regBuild.buildRegister(1);
    build.buildStore(clkReg, ff.val_clocked_on);
    auto dReg = regBuild.buildRegister(1);

    if (ff.val_clocked_on_also)
      report_fatal_error("liberty format: clocked_on_also unsupported");

    bool clearPrio = false;
    if (ff.val_clear && ff.val_preset) {
      char other;
      if (ff.val_clear_preset_var1 == 'L') {
        clearPrio = true;
        other = 'H';
      } else if (ff.val_clear_preset_var1 == 'H') {
        clearPrio = false;
        other = 'L';
      } else
        report_fatal_error("liberty parse: unsupported val_clear_preset_var1");

      if (outs.size() == 2)
        if (ff.val_clear_preset_var2 != other)
          report_fatal_error(
              "liberty parse: unsupported val_clear_preset_var2");
    }

    build.buildStore(dReg, val);

    auto ib = regBuild.buildInstrRaw(HW_FLIP_FLOP, 4 + 3 * !!ff.val_clear +
                                                       3 * !!ff.val_preset);
    regBuild.setInsertPoint(ib.instr());
    ib.other()
        .addRef(clkReg)
        .addRef(ConstantRef::fromBool(1))
        .addRef(dReg)
        .addRef(outs[0]);

    auto addClear = [&]() {
      if (ff.val_clear) {
        auto clrReg = regBuild.buildRegister(1);
        build.buildStore(clrReg, ff.val_clear);
        ib.addRef(clrReg)
            .addRef(ConstantRef::fromBool(1))
            .addRef(ConstantRef::fromBool(0));
      }
    };
    auto addPreset = [&]() {
      if (ff.val_preset) {
        auto presetReg = regBuild.buildRegister(1);
        build.buildStore(presetReg, ff.val_preset);
        ib.addRef(presetReg)
            .addRef(ConstantRef::fromBool(1))
            .addRef(ConstantRef::fromBool(1));
      }
    };

    if (clearPrio) {
      addClear();
      addPreset();
    } else {
      addPreset();
      addClear();
    }

    build.popInsertPoint();
    if (outs.size() == 2) {
      build.pushInsertPoint(build.buildProcess().block().end());
      build.buildStore(outs[1], build.buildNot(build.buildLoad(outs[0])));
      build.popInsertPoint();
    }
  }

  std::unordered_map<std::string_view, RegisterRef> portNameMap;
  HWValue parseExpr(std::string_view expr, HWInstrBuilder &build) {
    auto exprParse = LibertyExprParser{build, [&](std::string_view ident) {
                                         auto it = portNameMap.find(ident);
                                         if (it == portNameMap.end())
                                           report_fatal_error(
                                               "liberty format: unknown ident");
                                         return it->second;
                                       }};
    return exprParse.parseExpr(expr);
  }

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
    uint32_t kwFF = lexer.GetIdentIdx("ff");
    uint32_t kwLatch = lexer.GetIdentIdx("latch");
    uint32_t kwArea = lexer.GetIdentIdx("area");

    if (block->name != kwLibrary)
      err();
    HWInstrBuilderStack build{ctx};

    SmallVec<std::pair<RegisterRef, Token>, 16> funcList;
    SmallVec<std::tuple<RegisterRef, RegisterRef, LibertyParser::Block *>, 16>
        ffList;

    for (auto *object : block->blocks) {
      if (object->name != kwCell)
        continue;

      if (object->params.size() != 1 ||
          object->params.front().type != Token::STRING_LITERAL)
        err();

      auto info = ctx.getStdCellInfos().create();
      auto mod = ctx.createStdCell(object->params[0].strLit.value, info);

      build.setInsertPoint(mod.block().end());
      portNameMap.clear();

      for (auto var : object->vars) {
        if (var.name.type != Token::IDENTIFIER)
          continue;
        if (var.name.ident.idx == kwArea) {
          if (var.val.type != Token::NUMERIC_LITERAL)
            err();
          info->area = std::stod(std::string(var.val.numericLit.value));
        }
      }

      for (auto sub : object->blocks) {
        if (sub->name == kwFF || sub->name == kwLatch) {
          SmallVec<RegisterRef, 2> outs;
          for (auto param : sub->params) {
            if (param.type != Token::STRING_LITERAL)
              err();
            auto reg = build.buildRegister(1);
            ctx.regNameInfo.addName(reg, param.strLit.value);
            portNameMap.emplace(param.strLit.value, reg);
            outs.emplace_back(reg);
          }

          if (sub->name == kwLatch)
            continue;

          if (outs.size() == 0)
            err();
          ffList.emplace_back(outs[0], outs.size() == 1 ? nullref : outs[1],
                              sub);
          continue;
        }

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
        portNameMap.insert(std::make_pair(sub->params[0].strLit.value, port));
        ctx.regNameInfo.addName(port, sub->params[0].strLit.value);

        if (function) {
          fmtCheck(function);
          funcList.emplace_back(port, *function);
        }
      }

      while (!funcList.empty()) {
        auto [port, function] = funcList.pop_back_val();
        build.pushInsertPoint(build.buildProcess().block().begin());
        auto val = parseExpr(function.strLit.value, build);
        build.buildStore(port, val);
        build.popInsertPoint();
      }

      while (!ffList.empty()) {
        // fixme: is assuming all outputs are flop flop q's
        info->isFlipFlop = true;
        auto [out0, out1, block] = ffList.pop_back_val();
        HWInstrBuilder regBuild{ctx};
        regBuild.setInsertPoint(mod.regs_end());
        handleFF(block, build, regBuild, std::to_array({out0, out1}));
      }
    }
  }

public:
  LibertyToDyno(HWContext &ctx, LibertyLexer &lexer) : ctx(ctx), lexer(lexer) {}
}; // namespace dyno

class ParseLibertyPass {
  HWContext &ctx;

public:
  struct Config {
    std::string path;
  };
  Config config;

  void run() {
    MMap mmap{config.path};
    if (!mmap)
      report_fatal_error("could not open liberty file");
    LibertyLexer lex{mmap, std::string(config.path)};
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

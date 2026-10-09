#pragma once
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDs.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/MemoryPort.h"
#include "hw/Module.h"
#include "hw/Register.h"
#include "hw/StdCellInfo.h"
#include "support/CallableRef.h"
#include "support/ErrorRecovery.h"
#include "support/Lexer.h"
#include "support/Ranges.h"
#include "support/TemplateUtil.h"
#include "type/TypeContext.h"
#include "type/TypeInfo.h"
#include <array>
#include <cctype>
#include <charconv>

#define FOR_STDCELL_INFO_ELEMENTS(FUNC) FUNC(area), FUNC(isFlipFlop)
#define EXPAND_MEMBERS(nm) asInfo->nm
#define EXPAND_NAMES(nm) #nm

namespace dyno {
class HWDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_HW};

  PrinterBase *base;
  TempBindPtr<ValueNameInfo<Register>> regNames;

  HWDialectPrinter(const HWDialectPrinter &) = default;
  HWDialectPrinter(HWDialectPrinter &&) = default;
  HWDialectPrinter &operator=(const HWDialectPrinter &) = default;
  HWDialectPrinter &operator=(HWDialectPrinter &&) = default;

  HWDialectPrinter(PrinterBase *base) : base(base) {

    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        DIALECT_HW,
        CallableRef{this, BindMethod<&HWDialectPrinter::printHWType>::fv});

    base->interfaces.registerVal<PrinterBase::name_fn>(
        DIALECT_HW,
        CallableRef{this, BindMethod<&HWDialectPrinter::getObjectName>::fv});
  }

  bool printHWType(FatDynObjRef<> ref, bool def) {
    auto &str = base->str;

    switch (ref.getTyID()) {
    case HW_WIRE.type: {
      WireRef asWire = ref.as<WireRef>();
      str << "wire";
      if (asWire->numBits.isReg() && base->ctx) {
        str << "(";
        base->printRefOrUse(base->ctx->resolve(asWire->numBits.getReg()));
        str << ")";
      } else if (asWire->numBits)
        str << "(" << *asWire->numBits << ")";
      break;
    }
    case HW_MODULE.type: {
      ModuleRef asModule = ref.as<ModuleRef>();
      base->str << "module(\"" << asModule->name << "\")";
      break;
    }
    case HW_REGISTER.type: {
      RegisterRef asReg = ref.as<RegisterRef>();
      str << "register";
      auto type =
          base->ctx ? base->ctx->getCtx<HWDialectContext>().regTypeInfo.getType(
                          *base->ctx, asReg)
                    : nullref;

      bool names = base->ctx && !base->ctx->getCtx<HWDialectContext>()
                                     .regNameInfo.getNames(asReg)
                                     .empty();
      DynObjRef initVal = nullref;
      if (base->ctx) {
        auto &regResetValue =
            base->ctx->getCtx<HWDialectContext>().regResetValue;
        if (regResetValue.inRange(asReg))
          initVal = regResetValue[asReg];
      }

      if (asReg->numBits.isReg() || asReg->numBits || type || names || initVal)
        str << "(";
      if (asReg->numBits.isReg() && base->ctx) {
        base->printRefOrUse(base->ctx->resolve(asReg->numBits.getReg()));
      } else if (asReg->numBits) {
        str << *asReg->numBits;

        if (names || type || initVal)
          str << ((names || initVal) ? ", " : ",");
      }
      if (initVal) {
        base->printRefOrUse(base->ctx->resolve(initVal));
        if (names || type)
          str << (names ? ", " : ",");
      }
      if (names) {
        for (auto [back, nm] : base->ctx->getCtx<HWDialectContext>()
                                   .regNameInfo.getNames(asReg)
                                   .mark_back()) {
          str << "\"" << nm << "\"";
          if (!back)
            str << ", ";
        }

        if (type)
          str << ",";
      }
      if (type) {
        if (type.is<StructTypeRef>() || type.is<EnumTypeRef>()) {
          base->indentPrint.addIndent();
          if (!base->isIntroduced(type))
            base->indentPrint.printNewLineIndent();
          else
            str << " ";
          base->printRefOrUse(type, true);
          base->indentPrint.removeIndent();
        } else {
          str << " ";
          base->printRefOrUse(type);
        }
      }

      if (asReg->numBits || type || names)
        str << ")";
      break;
    }
    case HW_PROCESS.type: {
      // ProcessRef asProc = ref.as<ProcessRef>();
      str << "process";
      break;
    }
    case HW_TRIGGER.type: {
      str << "trigger";
      auto asTrigger = ref.as<TriggerRef>();
      if (asTrigger->size() != 0) {
        str << "(";
        for (size_t i = 0; i < asTrigger->size(); i++) {
          auto arr =
              std::array<const char *, 5>{"pos", "neg", "any", "iff", "iffn"};
          str << arr[size_t(asTrigger->getMode(i))];
          if (i != asTrigger->size() - 1)
            str << ", ";
        }
        str << ")";
      }
      break;
    }
    case HW_MEM_PORT.type: {
      auto asPort = ref.as<MemoryPortRef>();
      str << "mem_port(";
      str << asPort->delay;

      for (auto &meta : asPort->writeForwardMeta) {
        std::print(str, ", ({}, {})", meta.oldTime, meta.unkTime);
      }

      str << ")";
      break;
    }
    case HW_STDCELL_INFO.type: {
      auto asInfo = ref.as<StdCellInfoRef>();
      str << "stdcell_info(";

      auto list = mk_tuple(FOR_STDCELL_INFO_ELEMENTS(EXPAND_MEMBERS));
      auto names = std::to_array({FOR_STDCELL_INFO_ELEMENTS(EXPAND_NAMES)});
      list.apply([&](auto &...args) {
        size_t i = 0;
        bool any = false;
        (
            [&] {
              if (args) {
                if (any)
                  str << ", ";
                // fixme: floating point in parser.
                std::print(str, "\"{}\": {}", names[i], unsigned(*args));
                i++;
                any = true;
              }
            }(),
            ...);
      });
      str << ")";
      break;
    }
    default:
      return false;
    }
    return true;
  }

  std::optional<IntroducedName> getObjectName(FatDynObjRef<> ref) {
    switch (ref.getTyID()) {
    case HW_MODULE.type:
      return ref.as<ModuleRef>()->name.c_str();
    case HW_REGISTER.type: {
      return IntroducedName{ref.getObjID(), {'r', '\0'}};
      // if (!regNames)
      //   return IntroducedName{ref.getObjID(), {'r', '\0'}};
      // auto range = regNames->getNames(ref.as<RegisterRef>());
      // if (range.begin() == range.end())
      //   return IntroducedName{ref.getObjID(), {'r', '\0'}};
      // // todo: what about multiple and collisions?
      // return *range.begin();
    }
    case HW_WIRE.type: {
      return IntroducedName{ref.getObjID(), {'w', '\0'}};
    }
    }
    return std::nullopt;
  }
};

// Parser is templated for access to context's object stores. We could make
// type-erased object store wrapper but would always incur overhead.
class HWDialectParser {
  ParserBase &base;

public:
  static constexpr DialectID dialect{DIALECT_HW};

  explicit HWDialectParser(ParserBase *base) : base(*base) {
    base->interfaces.template registerVal<typename ParserBase::obj_parse_fn>(
        DIALECT_HW,
        CallableRef{this, BindMethod<&HWDialectParser::parseHW>::fv});
  }

  Result<FatDynObjRef<>, ParseError> parseHW(DialectType type,
                                             ArrayRef<char> name, bool isDef) {
    auto *lexer = &*base.lexer;
    auto *ctx = &base.ctx;
    switch (*type) {
    case *HW_MODULE: {
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
      DYNO_EXPECT(str, lexer->popExpect(Token::STRING_LITERAL));
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
      return ctx->getStore<Module>().create(std::string(str.strLit.value));
    }
    case *HW_REGISTER: {
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
      auto reg = ctx->getStore<Register>().create();
      auto &regNameInfo = ctx->getCtx<HWDialectContext>().regNameInfo;

      // can't have init val without numBits
      bool seenNumBits = false;

      while (!lexer->peekIs(DynoLexer::op_cbrclose)) {
        if (lexer->peekIs(Token::INT_LITERAL)) {
          reg->numBits = lexer->popEnsure(Token::INT_LITERAL).intLit.value;
          seenNumBits = true;
        } else if (lexer->peekIs(Token::STRING_LITERAL)) {
          auto name = lexer->Pop().strLit.value;
          regNameInfo.addName(reg, std::string_view{name});
        } else {
          auto state = base.lexer->getState();
          DYNO_EXPECT(type, base.parseUseOperand());
          if (type.is<FatTypeRef>())
            base.ctx.getCtx<HWDialectContext>().regTypeInfo.setType(
                reg, type.as<FatTypeRef>());
          else if (!seenNumBits && type.getType() == HW_REGISTER) {
            reg->numBits = type.as<RegisterRef>();
            seenNumBits = true;
          } else if (seenNumBits &&
                     type.getType() == Any{CORE_CONSTANT, HW_REGISTER}) {
            base.ctx.getCtx<HWDialectContext>().regResetValue.get_ensure(reg) =
                type;
          } else
            return base.lexer->makeErrorStartingAtToLast(
                state, "expected type or init val (constant/register)");
        }
        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));

      // only add ident name if no names listed
      if (regNameInfo.getNames(reg).empty() && !name.empty() &&
          !isdigit(name[0]) &&
          !(name[0] == 'r' && Range{name.begin() + 1, name.end()}.all(
                                  [](char c) { return isdigit(c); })))
        regNameInfo.addName(reg, std::string_view{name});
      return reg;
    }
    case *HW_WIRE: {
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
      OptionalU32OrReg bits;
      if (lexer->peekIs(Token::INT_LITERAL)) {
        bits = lexer->Pop().intLit.value;
      } else {
        auto state = base.lexer->getState();
        DYNO_EXPECT(op, base.parseUseOperand())
        if (op.getType() != HW_REGISTER)
          return base.lexer->makeErrorStartingAtToLast(
              state, "expected integer or register");

        bits = op.as<RegisterRef>();
      }
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
      return ctx->getStore<Wire>().create(bits);
    }
    case *HW_PROCESS: {
      return ctx->getStore<Process>().create();
    }
    case *HW_TRIGGER: {
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
      auto trigger = ctx->getStore<Trigger>().create();
      while (lexer->peekIs(Token::IDENTIFIER)) {
        auto ident = lexer->GetIdent(lexer->Peek().ident.idx);
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
          return lexer->makeErrorOnPeekToken("invalid sensitivity mode: \"{}\"",
                                             ident);
        lexer->Pop();
        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
      return trigger;
    }
    case *HW_MEM_PORT: {
      auto ref = ctx->getStore<MemoryPort>().create();
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
      ref->delay = lexer->popEnsure(Token::INT_LITERAL).intLit.value;
      if (lexer->popIf(DynoLexer::op_comma)) {
        while (!lexer->peekIs(DynoLexer::op_rbrclose)) {
          DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));
          DYNO_EXPECT(a, lexer->popExpect(Token::INT_LITERAL));
          DYNO_EXPECT(lexer->popExpect(DynoLexer::op_comma));
          DYNO_EXPECT(b, lexer->popExpect(Token::INT_LITERAL));
          DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
          ref->writeForwardMeta.emplace_back(a.intLit.value, b.intLit.value);

          if (!lexer->popIf(DynoLexer::op_comma))
            break;
        }
      }
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
      return ref;
    }
    case *HW_POINTER: {
      return ctx->getStore<Pointer>().create();
    }
    case *HW_STDCELL_INFO: {
      auto asInfo = ctx->getStore<StdCellInfo>().create();
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbropen));

      auto list = mk_tuple(FOR_STDCELL_INFO_ELEMENTS(EXPAND_MEMBERS));
      auto names = std::to_array({FOR_STDCELL_INFO_ELEMENTS(EXPAND_NAMES)});

      while (lexer->peekIs(Token::STRING_LITERAL)) {
        DYNO_EXPECT(tok, lexer->popExpect(Token::STRING_LITERAL));
        DYNO_EXPECT(lexer->popExpect(DynoLexer::op_colon));
        auto res = list.apply([&](auto &...args) {
          unsigned i = 0;
          return ([&] {
            if (names[i++] == tok.strLit.value) {
              // todo: non int
              args = lexer->popEnsure(Token::INT_LITERAL).intLit.value;
              return true;
            }
            return false;
          }() || ...);
        });
        if (!res)
          return lexer->makeErrorOnPeekToken("invalid stdcell_info key");
        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      DYNO_EXPECT(lexer->popExpect(DynoLexer::op_rbrclose));
      return asInfo;
    }
    }

    return nullref;
  }
};

}; // namespace dyno

#undef FOR_STDCELL_INFO_ELEMENTS
#undef EXPAND_MEMBERS
#undef EXPAND_NAMES

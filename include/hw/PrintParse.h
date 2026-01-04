#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/Module.h"
#include "support/CallableRef.h"
#include "support/TemplateUtil.h"

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
        MemberRef{this, BindMethod<&HWDialectPrinter::printHWType>::fv});

    base->interfaces.registerVal<PrinterBase::name_fn>(
        DIALECT_HW,
        MemberRef{this, BindMethod<&HWDialectPrinter::getObjectName>::fv});

  }

  bool printHWType(FatDynObjRef<> ref, bool def) {
    auto &str = base->str;

    switch (ref.getTyID()) {
    case HW_WIRE.type: {
      WireRef asWire = ref.as<WireRef>();
      str << "wire";
      if (asWire->numBits)
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
      if (asReg->numBits) {
        str << "(" << *asReg->numBits << ")";
      }
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
    default:
      return false;
    }
    return true;
  }

  using IntroducedName = PrinterBase::IntroducedName;

  std::optional<IntroducedName> getObjectName(FatDynObjRef<> ref) {
    switch (ref.getTyID()) {
    case HW_MODULE.type:
      return ref.as<ModuleRef>()->name.c_str();
    case HW_REGISTER.type: {
      if (!regNames)
        return IntroducedName{ref.getObjID(), {'r', '\0'}};
      auto range = regNames->getNames(ref.as<RegisterRef>());
      if (range.begin() == range.end())
        return IntroducedName{ref.getObjID(), {'r', '\0'}};
      // todo: what about multiple and collisions?
      return *range.begin();
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
template <typename Derived> class HWDialectParser {
  ParserBase<Derived> &base;

public:
  static constexpr DialectID dialect{DIALECT_HW};
  // this has to be initialized by the outer Parser wrapper (HWParser.h)
  HWContext *ctx;

  HWDialectParser(const HWDialectParser &) = default;
  HWDialectParser(HWDialectParser &&) = default;
  HWDialectParser &operator=(const HWDialectParser &) = default;
  HWDialectParser &operator=(HWDialectParser &&) = default;

  explicit HWDialectParser(ParserBase<Derived> *base) : base(*base) {
    base->interfaces
        .template registerVal<typename ParserBase<Derived>::obj_parse_fn>(
            DIALECT_HW,
            MemberRef{this, BindMethod<&HWDialectParser::parseHW>::fv});
  }

  FatDynObjRef<> parseHW(DialectType type, ArrayRef<char> name) {
    auto *lexer = &*base.lexer;
    switch (*type) {
    case *HW_MODULE: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto str = lexer->popEnsure(Token::STRING_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      return ctx->getModules().create(std::string(str.strLit.value));
    }
    case *HW_REGISTER: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto bits = lexer->popEnsure(Token::INT_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      auto reg = ctx->getRegs().create(bits.intLit.value);
      if (!name.empty() && !isdigit(name[0]))
        ctx->regNameInfo.addName(reg, std::string_view{name});
      return reg;
    }
    case *HW_WIRE: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto bits = lexer->popEnsure(Token::INT_LITERAL);
      lexer->popEnsure(DynoLexer::op_rbrclose);
      return ctx->getWires().create(bits.intLit.value);
    }
    case *HW_PROCESS: {
      return ctx->getProcs().create();
    }
    case *HW_TRIGGER: {
      lexer->popEnsure(DynoLexer::op_rbropen);
      auto trigger = ctx->getTriggers().create();
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

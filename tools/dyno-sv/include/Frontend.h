
#include "dyno/Constant.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/LoadStore.h"
#include "slang/ast/ASTVisitor.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/SystemSubroutine.h"
#include "slang/ast/expressions/AssignmentExpressions.h"
#include "slang/ast/expressions/CallExpression.h"
#include "slang/ast/expressions/ConversionExpression.h"
#include "slang/ast/expressions/LiteralExpressions.h"
#include "slang/ast/expressions/MiscExpressions.h"
#include "slang/ast/expressions/OperatorExpressions.h"
#include "slang/ast/expressions/SelectExpressions.h"
#include "slang/ast/statements/ConditionalStatements.h"
#include "slang/ast/statements/LoopStatements.h"
#include "slang/ast/statements/MiscStatements.h"
#include "slang/ast/symbols/BlockSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/MemberSymbols.h"
#include "slang/ast/symbols/ParameterSymbols.h"
#include "slang/ast/symbols/PortSymbols.h"
#include "slang/ast/symbols/SubroutineSymbols.h"
#include "slang/ast/symbols/ValueSymbol.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/ast/types/AllTypes.h"
#include "slang/ast/types/Type.h"
#include "slang/diagnostics/TextDiagnosticClient.h"
#include "slang/numeric/ConstantValue.h"
#include "slang/text/SourceLocation.h"
#include "slang/text/SourceManager.h"
#include "support/Debug.h"
#include <memory>
namespace dyno {

class SlangErrorPrinter {
  slang::SourceManager &sm;

public:
  void error(slang::SourceRange range) {
    auto client = std::make_shared<slang::TextDiagnosticClient>();
    client->showColors(true);
    slang::DiagnosticEngine engine{sm};
    engine.addClient(client);
    auto code = slang::DiagCode{slang::DiagSubsystem::Compilation, 0xFFFF};
    engine.setMessage(code, "unsupported");
    engine.setSeverity(code, slang::DiagnosticSeverity::Error);
    engine.issue(slang::Diagnostic{code, range.start()});

    std::cerr << client->getString();

    sm.getFileName(range.start());

    abort();
  }

  void error(slang::SourceLocation range) {
    auto client = std::make_shared<slang::TextDiagnosticClient>();
    client->showColors(true);
    slang::DiagnosticEngine engine{sm};
    engine.addClient(client);
    auto code = slang::DiagCode{slang::DiagSubsystem::Compilation, 0xFFFF};
    engine.setMessage(code, "unsupported");
    engine.setSeverity(code, slang::DiagnosticSeverity::Error);
    engine.issue(slang::Diagnostic{code, range});

    std::cerr << client->getString();

    sm.getFileName(range);

    abort();
  }

public:
  explicit SlangErrorPrinter(slang::SourceManager &sm) : sm(sm) {}
};

class VisitorAST : public slang::ast::ASTVisitor<VisitorAST, true, true> {
public:
  struct Value;

  HWContext &ctx;
  slang::SourceManager &sm;
  ModuleIRef mod;
  ProcessIRef proc;
  HWInstrBuilderStack build;
  BlockRef_iterator<true> regsBackIt;

  SlangErrorPrinter print;

  AutoDebugInfoStack debugInfoStack;

  using RegisterOrConstantRef = FatRefUnion<RegisterRef, ConstantRef>;

  DenseMap<const slang::ast::Symbol *, RegisterOrConstantRef> vars;
  DenseMap<const slang::ast::InstanceBodySymbol *, ObjRef<Module>> moduleMap;
  DenseMap<const slang::ast::SubroutineSymbol *, ObjRef<Function>> functionMap;
  bool curModIsInterface;

  SmallVec<Value *, 4> assignLVStack;

  VisitorAST(HWContext &ctx, slang::SourceManager &sm)
      : ctx(ctx), sm(sm), build(ctx), print(sm), debugInfoStack(ctx) {}

  struct Value : public RTTIUtilMixin<Value> {
    const slang::ast::Type *type;
    enum ValueKind : uint8_t {
      VK_R,
      VK_L,
      VK_CCL,
    };

    ValueKind kind;
    bool isLValue() { return kind != VK_R; }

  protected:
    Value(ValueKind kind, const slang::ast::Type *type)
        : type(type), kind(kind) {}

  public:
    Value() : kind(VK_R) {}

    HWValue getValue() {
      assert(!isLValue());
      return (*this).as<RValue>().value;
    }
    HWValue proGetValue(HWInstrBuilder &build) {
      switch (kind) {
      case VK_R:
        return getValue();
      case VK_L: {
        auto &asLVal = this->as<RegLValue>();
        auto ldVal = build.buildLoad(asLVal.lvReg, asLVal.numBits,
                                     asLVal.baseAddr, ArrayRef{asLVal.terms});
        assert(ldVal->numBits == type->getBitstreamWidth());
        return ldVal;
      }

      case VK_CCL: {
        auto &asCCLV = this->as<ConcatLValue>();
        SmallVec<HWValue, 4> buf{asCCLV.lvValues.size()};
        for (auto [idx, regLV] : Range{asCCLV.lvValues}.reverse().enumerate()) {
          buf[idx] = regLV->proGetValue(build);
        }
        return build.buildConcat(ArrayRef{buf.begin(), buf.end()});
      }
      }
    }

    uint32_t getNumBits() {
      switch (kind) {
      case VK_R: {
        auto &asRVal = this->as<RValue>();
        return *asRVal.value.getNumBits();
      }
      case VK_L: {
        auto &asLVal = this->as<RegLValue>();
        return asLVal.numBits;
      }

      case VK_CCL: {
        auto &asCCLV = this->as<ConcatLValue>();
        uint32_t sum = 0;
        for (auto &regLV : Range{asCCLV.lvValues}) {
          sum += regLV->numBits;
        }
        return sum;
      }
      }
    }

    static std::unique_ptr<Value> slice(HWInstrBuilder build, Value &value,
                                        uint32_t numBits, uint32_t baseAddr,
                                        ArrayRef<AddressGenTerm> terms,
                                        const slang::ast::Type *type) {
      switch (value.kind) {
      case Value::VK_CCL:
        // if (auto cIdx = idx.dyn_as<ConstantRef>()) {
        //   auto &asCCL = value.as<ConcatLValue>();
        //   for (auto &regLV : asCCL.lvValues) {
        //     if (!lvRange.isConstant())
        //       break;
        //     if (BitRange::equalsWithDefaultSize(
        //             BitRange{cbuild.get(), lvRange.getLen()}, range,
        //             reg->numBits)) {
        //       return std::make_unique<RegLValue>(reg, type, range);
        //     }
        //     cbuild.add(lvRange.hasLen() ? lvRange.getLen().as<ConstantRef>()
        //                                 :
        //                                 ConstantRef::fromU32(*reg->numBits));
        //   }
        // }
        // [[fallthrough]];
      case Value::VK_R: {
        auto splice = build.buildSplice(value.proGetValue(build), numBits,
                                        baseAddr, terms);
        if (auto asWire = splice.dyn_as<WireRef>())
          asWire->numBits = type->getBitstreamWidth();
        return std::make_unique<RValue>(splice, type);
      }
      case Value::VK_L: {
        auto lv = value.as<RegLValue>();
        assert(numBits <= lv.numBits);
        lv.numBits = numBits;
        lv.baseAddr += baseAddr;
        lv.terms.push_back_range(Range{terms});
        lv.type = type;
        return std::make_unique<RegLValue>(lv);
      }
      }
    }

    virtual ~Value() = default;
  };

  struct RValue : public Value {
    HWValue value;

    RValue(HWValue value, const slang::ast::Type *type)
        : Value(VK_R, type), value(value) {}

    static bool is_impl(const Value &v) { return v.kind == VK_R; }
  };

  struct LValue : public Value {
    static bool is_impl(const Value &v) {
      return v.kind == VK_L || v.kind == VK_CCL;
    }

  protected:
    LValue(ValueKind kind, const slang::ast::Type *type) : Value(kind, type) {
      assert(is_impl(*this));
    }

  public:
    void storeVal(HWInstrBuilder build, HWValue val, bool defer = false) {
      switch (kind) {
      case Value::VK_L: {
        auto asRegLV = this->as<RegLValue>();
        assert(val.getNumBits() == asRegLV.numBits);
        build.buildStore(asRegLV.lvReg, val, defer, nullref, asRegLV.baseAddr,
                         ArrayRef{asRegLV.terms});
        break;
      }
      case Value::VK_CCL: {
        auto &asCCLV = this->as<ConcatLValue>();

        uint32_t offs = 0;
        for (auto &regLV : asCCLV.lvValues) {
          build.buildStore(regLV->lvReg,
                           build.buildSplice(val, regLV->numBits, offs), defer,
                           nullref, regLV->baseAddr, ArrayRef{regLV->terms});
          offs += regLV->numBits;
        }
        break;
      }
      default:
        dyno_unreachable("expected lval");
      }
    }
  };

  struct RegLValue : public LValue {
    RegisterRef lvReg;
    uint32_t numBits;
    uint32_t baseAddr;
    SmallVec<AddressGenTerm, 2> terms;

    RegLValue(RegisterRef reg, const slang::ast::Type *type)
        : LValue(VK_L, type), lvReg(reg), numBits(type->getBitstreamWidth()),
          baseAddr(0), terms() {}

    static bool is_impl(const Value &v) { return v.kind == VK_L; }
    bool fullReg() const {
      return baseAddr == 0 && terms.size() == 0 && numBits == lvReg->numBits;
    }
  };
  struct ConcatLValue : public LValue {
    SmallVec<std::unique_ptr<RegLValue>, 4> lvValues;

    ConcatLValue(std::span<std::unique_ptr<Value>> lvals,
                 const slang::ast::Type *type)
        : LValue(VK_CCL, type), lvValues() {
      lvValues.reserve(lvals.size());
      for (auto &val : Range{lvals}.reverse()) {
        if (val->is<RegLValue>()) {
          lvValues.emplace_back(
              std::unique_ptr<RegLValue>{&val.release()->as<RegLValue>()});
        } else if (auto asCCLV = val->dyn_as<ConcatLValue>()) {
          for (auto &val : asCCLV->lvValues)
            lvValues.emplace_back(
                std::unique_ptr<RegLValue>{&val.release()->as<RegLValue>()});
        } else
          dyno_unreachable("expected lvalue");
      }
    }

    static bool is_impl(const Value &v) { return v.kind == VK_CCL; }
  };

  void pushSourceRange(slang::SourceRange range) {
    auto bufStart = range.start().buffer();
    auto bufEnd = range.end().buffer();

    uint64_t startIndex = range.start().offset();
    bool first = true;

    while (bufStart != bufEnd) {
      slang::SourceLocation start{bufStart, startIndex};
      slang::SourceLocation end{bufStart,
                                sm.getSourceText(bufStart).size() - 1};
      debugInfoStack.addDebugInfo(
          first, sm.getRawFileName(bufStart), sm.getLineNumber(start),
          sm.getColumnNumber(start), sm.getLineNumber(end),
          sm.getColumnNumber(end));
      startIndex = 0;
      first = false;
      // fixme: this should be an increment of buffers.
      bufStart = bufEnd;
    }
    slang::SourceLocation start{bufStart, startIndex};
    debugInfoStack.addDebugInfo(
        first, sm.getRawFileName(bufStart), sm.getLineNumber(start),
        sm.getColumnNumber(start), sm.getLineNumber(range.end()),
        sm.getColumnNumber(range.end()));
  }
  void popSourceRange() { debugInfoStack.popDebugInfo(); }
  struct DebugInfoSourceRange {
    VisitorAST *self;
    DebugInfoSourceRange(VisitorAST *self, slang::SourceRange range)
        : self(self) {
      self->pushSourceRange(range);
    }
    ~DebugInfoSourceRange() { self->popSourceRange(); }
  };

  void
  extractIFModuleVarsImpl(SmallVecImpl<const slang::ast::ValueSymbol *> &rv,
                          const slang::ast::InstanceBodySymbol &body,
                          bool extractPorts = false) {
    for (auto &member : body.members()) {
      switch (member.kind) {
      case slang::ast::SymbolKind::Net:
      case slang::ast::SymbolKind::Variable: {
        auto &asVar = member.as<slang::ast::ValueSymbol>();
        if (asVar.getFirstPortBackref() && !extractPorts)
          break;
        rv.emplace_back(&asVar);
        break;
      }
      case slang::ast::SymbolKind::Instance: {
        auto &asInst = member.as<slang::ast::InstanceSymbol>();
        if (!asInst.isInterface())
          break;

        extractIFModuleVarsImpl(rv, asInst.body, true);
        break;
      }
      default:
        continue;
      }
    }
  }

  auto extractIFModuleVars(const slang::ast::InstanceBodySymbol &body,
                           bool extractPorts = false) {
    SmallVec<const slang::ast::ValueSymbol *, 8> rv;
    extractIFModuleVarsImpl(rv, body, extractPorts);
    return rv;
  }

  auto extractIFModuleVarsPorts(const slang::ast::InstanceBodySymbol &body) {
    return extractIFModuleVars(body, true);
  }

  void handle(const slang::ast::InstanceSymbol &node) {
    assert((node.isModule() || node.isInterface()) &&
           "only module and interface supported rn");
    auto &body = *(node.getCanonicalBody() ?: &node.body);
    auto [found, it] = moduleMap.findOrInsert(&body, [&] {
      return ctx.createModule(node.name).def()->as<ObjRef<Module>>();
    });

    if (found)
      return;

    ModuleIRef module =
        ModuleRef{it.val(), ctx.getModules()[it.val()]}.getSingleDef()->instr();

    build.setInsertPoint(module.block().end());

    for (const auto &port : body.getPortList()) {
      if (auto *ps = port->as_if<slang::ast::PortSymbol>()) {
        HWOpcode ptype;
        switch (ps->direction) {
        case slang::ast::ArgumentDirection::In:
          ptype = HW_INPUT_REGISTER_DEF;
          break;
        case slang::ast::ArgumentDirection::Out:
          ptype = HW_OUTPUT_REGISTER_DEF;
          break;
        case slang::ast::ArgumentDirection::InOut:
          ptype = HW_INOUT_REGISTER_DEF;
          break;
        case slang::ast::ArgumentDirection::Ref:
          ptype = HW_REF_REGISTER_DEF;
          break;
        }
        auto reg = build.buildPort(module, ptype);
        reg->numBits = ps->getType().getBitstreamWidth();
        ctx.regNameInfo.addName(reg, ps->name);

      } else if (auto asIFS = port->as_if<slang::ast::InterfacePortSymbol>()) {
        // this runs if you have a port in your param list.
        // step into the interface, get all vars/ports, append those as ports.
        auto &ifInstance =
            asIFS->getConnection().first->as<slang::ast::InstanceSymbol>();
        auto ifVars = extractIFModuleVarsPorts(ifInstance.body);

        std::string ifName{asIFS->name};

        for (auto ifVar : ifVars) {
          auto reg = build.buildPort(module, HW_REF_REGISTER_DEF);
          reg->numBits = ifVar->getType().getBitstreamWidth();
          ctx.regNameInfo.addName(reg,
                                  (ifName + std::string{ifVar->name}).c_str());
        }
      }
    }

    // if the current module is an interface, expose all its vars as ports.
    if (node.isInterface()) {
      std::string ifName{node.name};
      dbgs() << "interface: " << body.getDeclaringDefinition()->name << "\n";
      for (auto ifPort : extractIFModuleVars(body)) {
        auto reg = build.buildPort(module, HW_REF_REGISTER_DEF);
        reg->numBits = ifPort->getType().getBitstreamWidth();
        ctx.regNameInfo.addName(reg,
                                (ifName + std::string{ifPort->name}).c_str());
      }
    }
    visitDefault(node);
  }

  void handle_modules() {
    // can probably do this in parallel
    for (auto [slangMod, dynoMod] : moduleMap) {
      ModuleRef fDynoMod{dynoMod, ctx.getModules()[dynoMod]};
      mod = fDynoMod.getSingleDef()->instr();
      vars.clear();
      curModIsInterface = slangMod->parentInstance->isInterface();

      regsBackIt = mod.regs_end();
      build.setInsertPoint(regsBackIt);
      --regsBackIt;

      size_t i = 0;
      for (auto port : slangMod->getPortList()) {
        if (auto *asPS = port->as_if<slang::ast::PortSymbol>()) {
          vars.insert(asPS->internalSymbol, fDynoMod->ports[i].reg);
          if (auto *init = asPS->getInitializer()) {
            auto proc = build.buildProcess(HW_INIT_PROCESS_DEF);
            build.pushInsertPoint(proc.block().end());
            build.buildStore(fDynoMod->ports[i].reg,
                             handle_expr(*init)->proGetValue(build));
            build.popInsertPoint();
          }
        } else if (auto *asIFS =
                       port->as_if<slang::ast::InterfacePortSymbol>()) {
          auto &ifInstance =
              asIFS->getConnection().first->as<slang::ast::InstanceSymbol>();

          auto [found, ifMod] = moduleMap.findOrInsert(&ifInstance.body, [&] {
            return ctx.createModule(ifInstance.name)
                .def()
                ->as<ObjRef<Module>>();
          });

          // auto ifMod = moduleMap.find(&ifInstance.body);
          // assert(ifMod && "no interface module?");
          for (auto *var : extractIFModuleVarsPorts(ifInstance.body)) {
            vars.insert(var, fDynoMod->ports[i++].reg);
          }
          i--;

        } else
          dyno_unreachable("unsupported port");

        i++;
      }

      if (curModIsInterface)
        for (auto ifPort : extractIFModuleVars(*slangMod)) {
          if (i >= fDynoMod->ports.size())
            print.error(ifPort->location);
          vars.insert(ifPort, fDynoMod->ports[i++].reg);
        }

      handle_member_list(slangMod->Scope::members());
    }
  }

  RegisterRef makeReg(uint32_t size, std::string_view name = {}) {
    build.pushInsertPoint(regsBackIt.succ());
    auto reg = build.buildRegister();
    regsBackIt = build.insert.pred();
    reg->numBits = size;
    build.popInsertPoint();
    if (!name.empty())
      ctx.regNameInfo.addName(reg, name);
    return reg;
  }

  RegisterRef makeOrFindReg(const slang::ast::Symbol &symb) {
    assert(symb.getDeclaredType());
    uint32_t numBits = symb.getDeclaredType()->getType().getBitstreamWidth();
    auto reg =
        vars.findOrInsert(&symb, [&] { return makeReg(numBits, symb.name); })
            .second.val()
            .as<RegisterRef>();
    return reg;
  }

  void handle_member_list(
      std::ranges::subrange<slang::ast::Scope::iterator> members) {
    for (auto &member : members) {
      switch (member.kind) {
      case slang::ast::SymbolKind::InterfacePort:
      case slang::ast::SymbolKind::Modport:
      case slang::ast::SymbolKind::Port:
        // std::cout << "port " << &member << ", "
        //           << member.as<slang::ast::PortSymbol>().name << "\n";
        break;
      case slang::ast::SymbolKind::Parameter: {
        auto &asParam = member.as<slang::ast::ParameterSymbol>();
        if (asParam.getValue().bad())
          break;
        vars.insert(&asParam.symbol,
                    toDynoConstant(asParam.getValue().integer()));
        break;
      }
      case slang::ast::SymbolKind::Variable: {
        auto &asVar = member.as<slang::ast::VariableSymbol>();
        if (asVar.getFirstPortBackref()) {
          // there's prob a better way to figure out whether a var is a port
          assert(vars.contains(&asVar) && "port not in vars?");
          break;
        }
        auto reg = makeOrFindReg(asVar);

        if (auto *init = asVar.getInitializer()) {
          auto proc = build.buildProcess(HW_INIT_PROCESS_DEF);
          build.pushInsertPoint(proc.block().begin());
          build.buildStore(reg, handle_expr(*init)->proGetValue(build));
          build.popInsertPoint();
        }
        break;
      }
      case slang::ast::SymbolKind::Net: {
        auto &asNet = member.as<slang::ast::NetSymbol>();

        auto reg = makeOrFindReg(asNet);
        if (auto *init = asNet.getInitializer()) {
          auto proc = build.buildProcess(HW_COMB_PROCESS_DEF);
          build.pushInsertPoint(proc.block().begin());
          build.buildStore(reg, handle_expr(*init)->proGetValue(build));
          build.popInsertPoint();
        }
        break;
      };

      case slang::ast::SymbolKind::ProceduralBlock: {
        auto &proc = member.as<slang::ast::ProceduralBlockSymbol>();
        handle_proc(proc);
        break;
      }

      case slang::ast::SymbolKind::StatementBlock: {
        // this creates a scope but its members are a copy of a sibling
        // procedural block. we don't care much about scopes and don't want
        // duplicate processes so ignore.
        break;
      }

      case slang::ast::SymbolKind::Instance: {
        auto &asInst = member.as<slang::ast::InstanceSymbol>();
        auto &body = *(asInst.getCanonicalBody() ?: &asInst.body);

        auto it = moduleMap.find(&body);
        if (!it)
          abort();

        SmallVec<RegisterRef, 16> portRegs;

        for (auto [connIdx, conn] :
             Range{asInst.getPortConnections()}.enumerate()) {

          std::unique_ptr<Value> val;

          auto expr = conn->getExpression();
          if (!expr) {
            auto mod = ctx.getModules().resolve(it.val());
            portRegs.emplace_back(makeReg(*mod->ports[connIdx].reg.getNumBits(),
                                          "__unused_port"));
            continue;
          }
          assert(expr);

          // interface args
          if (auto *asArbSym =
                  expr->as_if<slang::ast::ArbitrarySymbolExpression>()) {
            auto &asInst = asArbSym->symbol->as<slang::ast::InstanceSymbol>();
            auto ifVars = extractIFModuleVarsPorts(asInst.body);

            for (auto *ifVar : ifVars) {
              auto it = vars.find(ifVar);
              assert(it && "interface var not found");
              auto reg = it.val().as<RegisterRef>();
              portRegs.emplace_back(reg);
            }
            continue;
          }

          auto proc = build.buildProcess();
          build.pushInsertPoint(proc.block().end());

          // output args are wrapped in an empty RHS assignment
          if (auto *asAssign =
                  expr->as_if<slang::ast::AssignmentExpression>()) {
            assert(asAssign->right().kind ==
                   slang::ast::ExpressionKind::EmptyArgument);
            val = handle_expr(asAssign->left());
          } else
            val = handle_expr(*conn->getExpression());

          build.popInsertPoint();

          if (auto *asLV = val->dyn_as<RegLValue>(); asLV && asLV->fullReg()) {
            portRegs.emplace_back(asLV->lvReg);
          } else {
            build.pushInsertPoint(regsBackIt.succ());
            portRegs.emplace_back(build.buildRegister());
            regsBackIt = build.insert.pred();
            build.popInsertPoint();

            if (auto *psym = conn->port.as_if<slang::ast::PortSymbol>())
              portRegs.back()->numBits = psym->getType().getBitstreamWidth();

            if (portRegs.back().getNumBits() != val->getNumBits())
              print.error(expr->sourceRange);

            build.pushInsertPoint(proc.block().end());
            build.buildStore(portRegs.back(), val->proGetValue(build));
            build.popInsertPoint();
          }
        }

        if (asInst.isInterface()) {
          // for interfaces, expose internal vars as ports
          for (auto *ifVar : extractIFModuleVars(asInst.body)) {
            auto [found, it] = vars.findOrInsert(ifVar, [&] {
              build.pushInsertPoint(regsBackIt.succ());
              auto rv =
                  build.buildRegister(ifVar->getType().getBitstreamWidth());
              regsBackIt = build.insert.pred();
              build.popInsertPoint();
              return rv;
            });
            portRegs.emplace_back(it.val().as<RegisterRef>());
            if (!curModIsInterface)
              assert(!found);
          }

          for (auto [i, ifPort] :
               Range{asInst.getPortConnections()}.enumerate()) {
            vars.insertOrAssign(
                ifPort->port.as<slang::ast::PortSymbol>().internalSymbol,
                portRegs[i]);
          }
        }

        ModuleRef instMod = ModuleRef{it.val(), ctx.getModules()[it.val()]};
        build.buildInstance(instMod, portRegs);
        break;
      }

      case slang::ast::SymbolKind::GenerateBlockArray: {
        auto &asGenBA = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (auto submember : asGenBA.entries) {
          handle_member_list(submember->members());
        }
        break;
      }

      case slang::ast::SymbolKind::GenerateBlock: {
        auto &asGenBl = member.as<slang::ast::GenerateBlockSymbol>();
        if (!asGenBl.isUninstantiated)
          handle_member_list(asGenBl.members());
        break;
      }

      case slang::ast::SymbolKind::ContinuousAssign: {
        auto &asCAssign = member.as<slang::ast::ContinuousAssignSymbol>();
        auto proc = build.buildProcess();
        build.pushInsertPoint(proc.block().begin());
        handle_expr(asCAssign.getAssignment());
        build.popInsertPoint();
        break;
      }

      case slang::ast::SymbolKind::Subroutine: {
        auto &asSubr = member.as<slang::ast::SubroutineSymbol>();
        auto [found, it] = functionMap.findOrInsert(
            &asSubr, [&] { return build.buildFunc().func(); });
        auto func = FunctionRef{it.val(), ctx.getFuncs()[it.val()]}.iref();

        build.pushInsertPoint(func.getBlock().begin());
        for (auto arg : asSubr.getArguments()) {
          vars.insert(arg,
                      build.buildFuncParam(arg->getType().getBitstreamWidth()));
        }
        handle_stmt(asSubr.getBody());
        build.popInsertPoint();
        break;
      }

      case slang::ast::SymbolKind::UninstantiatedDef: {
        break;
      }

      case slang::ast::SymbolKind::TypeAlias: {
        break;
      }

      case slang::ast::SymbolKind::TransparentMember: {
        break;
      }

      case slang::ast::SymbolKind::InstanceArray: {
        break;
      }

      case slang::ast::SymbolKind::TypeParameter: {
        break;
      }

      case slang::ast::SymbolKind::WildcardImport: {
        break;
      }

      case slang::ast::SymbolKind::Genvar: {
        break;
      }

      default: {
        std::cerr << "unsupported slang symbol: " << member.kind << "\n";
        print.error(member.location);
        abort();
      }
      }
    }
  }

  void handle_timing(const slang::ast::TimingControl &timing, SensList &sens) {
    switch (timing.kind) {
    case slang::ast::TimingControlKind::SignalEvent: {
      auto &asSigEvt = timing.as<slang::ast::SignalEventControl>();
      auto &sym = asSigEvt.expr.as<slang::ast::ValueExpressionBase>().symbol;
      auto it = vars.find(&sym);
      assert(it && "unknown var");

      if (!it.val().is<RegisterRef>())
        abort();

      SensMode mode;
      switch (asSigEvt.edge) {
      case slang::ast::EdgeKind::None:
        abort();
      case slang::ast::EdgeKind::PosEdge:
        mode = SensMode::POSEDGE;
        break;
      case slang::ast::EdgeKind::NegEdge:
        mode = SensMode::NEGEDGE;
        break;
      case slang::ast::EdgeKind::BothEdges:
        mode = SensMode::ANYEDGE;
        break;
      }

      sens.signals.emplace_back(
          std::make_pair(it.val().as<RegisterRef>(), mode));

      break;
    }
    case slang::ast::TimingControlKind::EventList: {
      auto &asEvtList = timing.as<slang::ast::EventListControl>();
      for (auto sub : asEvtList.events) {
        handle_timing(*sub, sens);
      }
      break;
    }
    case slang::ast::TimingControlKind::ImplicitEvent: {
      return;
    }
    default:
      abort();
    }
  }

  void handle_proc(const slang::ast::ProceduralBlockSymbol &block) {
    assert(mod);
    auto *stmt = &block.getBody();

    SensList sens;
    if (auto *asTimed = stmt->as_if<slang::ast::TimedStatement>()) {
      stmt = &asTimed->stmt;
      handle_timing(asTimed->timing, sens);
    }

    HWOpcode opc;
    switch (block.procedureKind) {
    case slang::ast::ProceduralBlockKind::Initial:
      opc = HW_INIT_PROCESS_DEF;
      break;
    case slang::ast::ProceduralBlockKind::Final:
      opc = HW_FINAL_PROCESS_DEF;
      break;
    case slang::ast::ProceduralBlockKind::Always: {
      bool seq =
          std::any_of(sens.signals.begin(), sens.signals.end(), [](auto pair) {
            return pair.second == SensMode::NEGEDGE ||
                   pair.second == SensMode::POSEDGE;
          });
      opc = seq ? HW_SEQ_PROCESS_DEF : HW_COMB_PROCESS_DEF;
      break;
    }
    case slang::ast::ProceduralBlockKind::AlwaysComb:
      opc = HW_COMB_PROCESS_DEF;
      break;
    case slang::ast::ProceduralBlockKind::AlwaysLatch:
      opc = HW_LATCH_PROCESS_DEF;
      break;
    case slang::ast::ProceduralBlockKind::AlwaysFF:
      opc = HW_SEQ_PROCESS_DEF;
      break;
    }

    build.pushInsertPoint(regsBackIt.succ());
    auto trigger = build.buildTrigger(sens);
    build.popInsertPoint();
    proc = build.buildProcess(opc, trigger);
    build.pushInsertPoint(proc.block().end());
    handle_stmt(*stmt);
    build.popInsertPoint();
  }

  struct DefaultValueTypeWalker {
    DefaultValueTypeWalker(
        HWInstrBuilder &build, HWValue defVal, bool sign,
        ArrayRef<std::pair<const slang::ast::Type *, HWValue>> typeDefaults)
        : build(build), defVal(defVal), sign(sign), typeDefaults(typeDefaults) {
    }
    HWInstrBuilder &build;
    const HWValue defVal;
    const bool sign;
    const ArrayRef<std::pair<const slang::ast::Type *, HWValue>> typeDefaults;

    HWValue operator()(const slang::ast::Type *type) {
      // if there's a default override for this type, return that
      auto tryFind = [&](const slang::ast::Type *type) -> HWValue {
        auto it = std::find_if(
            typeDefaults.begin(), typeDefaults.end(),
            [&](const auto &a) { return type->isMatching(*a.first); });
        if (it != typeDefaults.end())
          return it->second;
        return nullref;
      };

      if (auto val = tryFind(type))
        return build.buildResize(val, type->getBitstreamWidth(), sign);

      switch (type->kind) {
      case slang::ast::SymbolKind::EnumType: {
        auto &asEnum = type->as<slang::ast::EnumType>();
        return build.buildResize(defVal, asEnum.baseType.getBitstreamWidth(),
                                 sign);
      }
      case slang::ast::SymbolKind::PackedStructType: {
        auto &asPStr = type->as<slang::ast::PackedStructType>();
        SmallVec<HWValue, 8> sub{};
        for (auto &member : asPStr.members())
          sub.emplace_back((*this)(&member.getDeclaredType()->getType()));
        return build.buildConcat(sub);
      }
      case slang::ast::SymbolKind::UnpackedStructType: {
        auto &asUStr = type->as<slang::ast::UnpackedStructType>();
        SmallVec<HWValue, 8> sub{};
        for (auto &member : asUStr.members())
          sub.emplace_back((*this)(&member.getDeclaredType()->getType()));
        std::reverse(sub.begin(), sub.end());
        return build.buildConcat(sub);
      }
      case slang::ast::SymbolKind::PackedArrayType: {
        auto &asPArr = type->as<slang::ast::PackedArrayType>();
        if (!asPArr.isSimpleBitVector()) {
          auto val = (*this)(&asPArr.elementType);
          return build.buildRepeat(val, asPArr.range.width());
        }
        if (auto val = tryFind(&asPArr.elementType))
          return build.buildResize(val, type->getBitstreamWidth(), sign);
      }
        [[fallthrough]];
      case slang::ast::SymbolKind::PredefinedIntegerType:
      case slang::ast::SymbolKind::ScalarType: {
        return build.buildResize(defVal, type->getBitstreamWidth(), sign);
      }
      case slang::ast::SymbolKind::FixedSizeUnpackedArrayType: {
        auto &asUArr = type->as<slang::ast::FixedSizeUnpackedArrayType>();
        auto val = (*this)(&asUArr.elementType);
        return build.buildRepeat(val, asUArr.range.width());
      }
      case slang::ast::SymbolKind::TypeAlias: {
        auto &asAlias = type->as<slang::ast::TypeAliasType>();
        return (*this)(&asAlias.targetType.getType());
      }
      // /case slang::ast::SymbolKind::
      default:
        abort();
      }
    }
  };

  void handle_stmt(const slang::ast::Statement &stmt) {
    DebugInfoSourceRange debugInfoSrcRange{this, stmt.sourceRange};
    switch (stmt.kind) {
    case slang::ast::StatementKind::Empty:
      break;
    case slang::ast::StatementKind::Block: {
      // todo: var scope
      handle_stmt(stmt.as<slang::ast::BlockStatement>().body);
      break;
    }
    case slang::ast::StatementKind::ExpressionStatement: {
      const auto &expr_s = stmt.as<slang::ast::ExpressionStatement>();
      handle_expr(expr_s.expr);
      break;
    }
    case slang::ast::StatementKind::List: {
      const auto &list = stmt.as<slang::ast::StatementList>();
      for (const auto &l_stmt : list.list)
        handle_stmt(*l_stmt);
      break;
    }

    case slang::ast::StatementKind::VariableDeclaration: {
      auto &asVarDecl = stmt.as<slang::ast::VariableDeclStatement>();
      build.pushInsertPoint(regsBackIt.succ());
      auto reg = build.buildRegister();
      regsBackIt = build.insert.pred();
      build.popInsertPoint();

      vars.findOrInsert(&asVarDecl.symbol, reg);
      reg->numBits = asVarDecl.symbol.getType().getBitstreamWidth();

      if (auto *init = asVarDecl.symbol.getInitializer())
        build.buildStore(reg, handle_expr(*init)->proGetValue(build));
      break;
    }

    case slang::ast::StatementKind::Invalid:
      break;

    case slang::ast::StatementKind::Return: {
      auto &asRet = stmt.as<slang::ast::ReturnStatement>();
      if (asRet.expr)
        build.buildFuncReturn(handle_expr(*asRet.expr)->proGetValue(build));
      else
        build.buildFuncReturn();
      break;
    }
    case slang::ast::StatementKind::Continue:
    case slang::ast::StatementKind::Break:
    case slang::ast::StatementKind::Disable:
      abort();
    case slang::ast::StatementKind::Conditional: {
      auto &asCond = stmt.as<slang::ast::ConditionalStatement>();
      if (asCond.conditions.size() != 1)
        abort();
      auto &cond = asCond.conditions[0];
      if (cond.pattern)
        abort();

      auto condVal = handle_expr(*cond.expr)->proGetValue(build);
      auto condBool = makeBool(condVal);

      if (!asCond.ifFalse) {
        auto ifInstr = build.buildIf(condBool);
        build.pushInsertPoint(ifInstr.getTrueBlock().end());
        handle_stmt(asCond.ifTrue);
        build.popInsertPoint();
      } else {
        auto ifElseInstr = build.buildIfElse(condBool);
        build.pushInsertPoint(ifElseInstr.getTrueBlock().end());
        handle_stmt(asCond.ifTrue);
        build.popInsertPoint();

        build.pushInsertPoint(ifElseInstr.getFalseBlock().end());
        handle_stmt(*asCond.ifFalse);
        build.popInsertPoint();
      }
      break;
    }
    case slang::ast::StatementKind::Case: {
      auto &asCase = stmt.as<slang::ast::CaseStatement>();
      auto swInstr =
          build.buildSwitch(handle_expr(asCase.expr)->proGetValue(build));
      SmallVec<HWValue, 8> labels;
      build.pushInsertPoint(build.insert.pred());

      DialectOpcode opc;
      switch (asCase.condition) {
      case slang::ast::CaseStatementCondition::Inside:
      case slang::ast::CaseStatementCondition::Normal:
        opc = OP_CASE;
        break;
      case slang::ast::CaseStatementCondition::WildcardXOrZ:
        opc = HW_CASE_X;
        break;
      case slang::ast::CaseStatementCondition::WildcardJustZ:
        opc = HW_CASE_Z;
        break;
        //// what is this?
        //print.error(asCase.sourceRange);
        break;
      }

      for (auto &swCase : asCase.items) {
        labels.resize(swCase.expressions.size());
        for (auto [idx, expr] : Range{swCase.expressions}.enumerate())
          labels[idx] = handle_expr(*expr)->proGetValue(build);

        build.pushInsertPoint(swInstr.block().end());
        auto caseInstr = build.buildCase(labels, opc);
        build.popInsertPoint();

        build.pushInsertPoint(caseInstr.block().end());
        handle_stmt(*swCase.stmt);
        build.popInsertPoint();
      }

      build.pushInsertPoint(swInstr.block().end());
      auto defInstr = build.buildDefaultCase();
      build.popInsertPoint();

      if (auto defCase = asCase.defaultCase) {
        build.pushInsertPoint(defInstr.block().end());
        handle_stmt(*defCase);
        build.popInsertPoint();
      }
      build.popInsertPoint();
      break;
    }
    case slang::ast::StatementKind::WhileLoop: {
      auto &asWhileLoop = stmt.as<slang::ast::WhileLoopStatement>();
      auto whileInstr = build.buildWhile();

      build.pushInsertPoint(whileInstr.getCondBlock().end());
      auto cond = makeBool(handle_expr(asWhileLoop.cond)->proGetValue(build));
      build.buildYield(cond);
      build.popInsertPoint();

      build.pushInsertPoint(whileInstr.getBodyBlock().end());
      handle_stmt(asWhileLoop.body);
      // build.buildYield(ConstantRef::fromBool(true));
      build.popInsertPoint();
      break;
    }
    case slang::ast::StatementKind::DoWhileLoop: {
      auto &asDoWhileLoop = stmt.as<slang::ast::DoWhileLoopStatement>();
      auto whileInstr = build.buildDoWhile();

      build.pushInsertPoint(whileInstr.getBlock().end());
      handle_stmt(asDoWhileLoop.body);
      auto cond = makeBool(handle_expr(asDoWhileLoop.cond)->proGetValue(build));
      build.buildYield(cond);
      build.popInsertPoint();
      break;
    }
    case slang::ast::StatementKind::ForLoop: {
      auto &asForLoop = stmt.as<slang::ast::ForLoopStatement>();

      if (asForLoop.initializers.size() != 0) {
        assert(asForLoop.loopVars.size() == 0);
        for (auto *init : asForLoop.initializers)
          handle_expr(*init);
      } else {
        for (auto *var : asForLoop.loopVars) {
          makeOrFindReg(*var);
          if (auto *init = var->getInitializer()) {
            handle_expr(*init);
          }
        }
      }

      // no specialization to for yet, generate as while
      auto whileInstr = build.buildWhile();

      build.pushInsertPoint(whileInstr.getCondBlock().end());
      auto cond =
          makeBool(handle_expr(*asForLoop.stopExpr)->proGetValue(build));
      build.buildYield(cond);
      build.popInsertPoint();

      build.pushInsertPoint(whileInstr.getBodyBlock().end());
      handle_stmt(asForLoop.body);
      for (auto *step : asForLoop.steps) {
        handle_expr(*step);
      }
      // build.buildYield(ConstantRef::fromBool(true));
      build.popInsertPoint();
      break;
    }
    case slang::ast::StatementKind::ImmediateAssertion: {
      auto &asAssert = stmt.as<slang::ast::ImmediateAssertionStatement>();
      if (asAssert.isDeferred ||
          asAssert.assertionKind != slang::ast::AssertionKind::Assert ||
          asAssert.isFinal || asAssert.ifFalse ||
          (asAssert.ifTrue &&
           asAssert.ifTrue->kind != slang::ast::StatementKind::Empty))
        abort();
      build.buildAssert(
          makeBool(handle_expr(asAssert.cond)->proGetValue(build)));
      break;
    }
    case slang::ast::StatementKind::PatternCase:
    case slang::ast::StatementKind::RepeatLoop:
    case slang::ast::StatementKind::ForeachLoop:
    case slang::ast::StatementKind::ForeverLoop:
    case slang::ast::StatementKind::ConcurrentAssertion:
    case slang::ast::StatementKind::DisableFork:
    case slang::ast::StatementKind::Wait:
    case slang::ast::StatementKind::WaitFork:
    case slang::ast::StatementKind::WaitOrder:
    case slang::ast::StatementKind::EventTrigger:
      abort();

    case slang::ast::StatementKind::ProceduralAssign: {
      // auto &asPAssign = stmt.as<slang::ast::ProceduralAssignStatement>();
      // if (asPAssign.isForce)
      //   abort();
      // handle_expr(asPAssign.assignment);
      // break;
    }

    case slang::ast::StatementKind::ProceduralDeassign:
    case slang::ast::StatementKind::RandCase:
    case slang::ast::StatementKind::RandSequence:
    case slang::ast::StatementKind::ProceduralChecker:
    case slang::ast::StatementKind::Timed:
      abort();
    }
  }

  std::tuple<uint32_t, uint32_t, int32_t>
  getArrayElemWidthAndLengthAndBase(const slang::ast::Type &type) {
    switch (type.kind) {
    case slang::ast::SymbolKind::PackedArrayType: {
      auto &asPArrT = type.as<slang::ast::PackedArrayType>();
      return std::make_tuple(asPArrT.elementType.getBitstreamWidth(),
                             asPArrT.range.width(), asPArrT.range.lower());
      break;
    }
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType: {
      auto &asUArrT = type.as<slang::ast::FixedSizeUnpackedArrayType>();
      return std::make_tuple(asUArrT.elementType.getBitstreamWidth(),
                             asUArrT.range.width(), asUArrT.range.lower());
    }
    case slang::ast::SymbolKind::TypeAlias: {
      auto &asTypeAlias = type.as<slang::ast::TypeAliasType>();
      return getArrayElemWidthAndLengthAndBase(
          asTypeAlias.targetType.getType());
    }
    case slang::ast::SymbolKind::PredefinedIntegerType: {
      auto &asPredefInt = type.as<slang::ast::PredefinedIntegerType>();
      return std::make_tuple(1, asPredefInt.bitWidth, 0);
    }
    case slang::ast::SymbolKind::PackedStructType: {
      auto &asPStruct = type.as<slang::ast::PackedStructType>();
      return std::make_tuple(1, asPStruct.bitWidth, 0);
    }
    case slang::ast::SymbolKind::EnumType: {
      auto &asEnumType = type.as<slang::ast::EnumType>();
      return getArrayElemWidthAndLengthAndBase(asEnumType.baseType);
    }
    default:
      abort();
    }
  }

  HWValue makeBool(HWValue val, bool inverse = false) {
    if (val.getNumBits() == 1 && !inverse)
      return val;
    return build.buildICmp(val, ctx.constBuild().zeroLike(val).get(),
                           inverse ? BigInt::ICMP_EQ : BigInt::ICMP_NE);
  }

  ConstantRef toDynoConstant(const slang::SVInt &svint) {
    ConstantRef ref;
    ArrayRef data{reinterpret_cast<const uint32_t *>(svint.getRawPtr()),
                  2 * svint.getNumWords()};
    if (!svint.hasUnknown()) {
      ref = ConstantBuilder{ctx.getConstants()}.raw(
          (unsigned)svint.getBitWidth(), data);
    } else {
      SmallVec<uint32_t, 16> buf(round_up_div(2 * svint.getBitWidth(), 32U));

      std::span<const uint16_t> data16{
          reinterpret_cast<const uint16_t *>(data.data()), data.size() * 2};
      for (size_t i = 0; i < buf.size(); i++) {
        buf[i] = (unpack_bits(data16[i + data16.size() / 2]) << 1) |
                 unpack_bits(data16[i]);
        // flip x and z
        uint32_t mask = (buf[i] & repeatBits(0b10U, 2)) >> 1;
        buf[i] ^= mask;
      }

      ref = ConstantBuilder{ctx.getConstants()}.raw(2 * svint.getBitWidth(),
                                                    std::move(buf), 0, 1);
    }
    return ref;
  }

  ConstantRef toDynoConstant(const slang::ConstantValue &value) {
    if (value.isInteger())
      return toDynoConstant(value.integer());
    if (value.isContainer()) {
      auto cbuild = ctx.constBuild();
      cbuild.val(0, 0);
      for (auto elem : Range{value.elements()}.reverse()) {
        cbuild.concat(toDynoConstant(elem));
      }
      return cbuild.get();
    }
    abort();
  }

  std::unique_ptr<Value> handle_expr(const slang::ast::Expression &expr) {
    DebugInfoSourceRange debugInfoSrcRange{this, expr.sourceRange};

    switch (expr.kind) {
    case slang::ast::ExpressionKind::Assignment: {
      const auto &assign = expr.as<slang::ast::AssignmentExpression>();

      auto lhs = handle_expr(assign.left());
      assignLVStack.emplace_back(lhs.get());
      auto rhs = handle_expr(assign.right());
      assignLVStack.pop_back();

      lhs->as<LValue>().storeVal(build, rhs->proGetValue(build),
                                 assign.isNonBlocking());
      return rhs;
    }
    case slang::ast::ExpressionKind::LValueReference: {
      // to implement op-assign, slang uses regular assignment and operators
      // with this special LValueReference token on one side. The value produced
      // by an LValueReference is the top of the assignment lvalue stack.
      assert(!assignLVStack.empty() && "lvalue stack empty?");
      return std::make_unique<RValue>(assignLVStack.back()->proGetValue(build),
                                      assignLVStack.back()->type);
    }

    case slang::ast::ExpressionKind::BinaryOp: {
      const auto &binop = expr.as<slang::ast::BinaryExpression>();
      // std::cout << binop.kind << "\n";

      switch (binop.op) {
      case slang::ast::BinaryOperator::LogicalOr:
      case slang::ast::BinaryOperator::LogicalImplication:
      case slang::ast::BinaryOperator::LogicalAnd: {
        auto lhs = handle_expr(binop.left());
        auto lhsVal = lhs->proGetValue(build);

        auto lhsBool =
            makeBool(lhsVal, binop.op == slang::ast::BinaryOperator::LogicalOr);
        auto ifElse = build.buildIfElse(lhsBool);
        build.pushInsertPoint(ifElse.getTrueBlock().end());

        auto rhs = handle_expr(binop.right());
        auto rhsVal = rhs->proGetValue(build);

        auto rhsBool = makeBool(rhsVal);
        ifElse = build.buildYield(rhsBool).second;

        build.popInsertPoint();
        build.pushInsertPoint(ifElse.getFalseBlock().end());
        ifElse = build
                     .buildYield(ConstantRef::fromBool(
                         binop.op != slang::ast::BinaryOperator::LogicalAnd))
                     .second;
        build.popInsertPoint();

        return std::make_unique<RValue>(ifElse.getYieldValue(0)->as<WireRef>(),
                                        expr.type);
      };
      default:
        break;
      }

      auto lhs = handle_expr(binop.left());
      auto rhs = handle_expr(binop.right());

      HWValue val;

      auto lhsVal = lhs->proGetValue(build);
      auto rhsVal = rhs->proGetValue(build);

      // for some reason Slang sometimes omits implicit conversions (?)
      // maybe just for self-determined operands?
      if (auto width = expr.type->getBitstreamWidth(); width && width != 1) {
        lhsVal = build.buildResize(lhsVal, width, lhs->type->isSigned());
        rhsVal = build.buildResize(rhsVal, width, rhs->type->isSigned());
      }

      switch (binop.op) {
      case slang::ast::BinaryOperator::Add:
        val = build.buildAdd(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Subtract:
        val = build.buildSub(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Multiply:
        val = build.buildMul(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Divide:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildSDiv(lhsVal, rhsVal);
        else
          val = build.buildUDiv(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Mod:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildSMod(lhsVal, rhsVal);
        else
          val = build.buildUMod(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftLeft:
      case slang::ast::BinaryOperator::LogicalShiftLeft:
        val = build.buildSLL(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftRight:
        val = build.buildSRA(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::LogicalShiftRight:
        val = build.buildSRL(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryAnd:
        val = build.buildAnd(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryOr:
        val = build.buildOr(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryXor:
        val = build.buildXor(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryXnor:
        val = build.buildNot(build.buildXor(lhsVal, rhsVal));
        break;
      case slang::ast::BinaryOperator::Equality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_EQ);
        break;
      case slang::ast::BinaryOperator::Inequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_NE);
        break;
      case slang::ast::BinaryOperator::CaseEquality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_CEQ);
        break;
      case slang::ast::BinaryOperator::CaseInequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_CNE);
        break;
      case slang::ast::BinaryOperator::GreaterThanEqual:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SGE);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_UGE);
        break;
      case slang::ast::BinaryOperator::GreaterThan:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SGT);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_UGT);
        break;
      case slang::ast::BinaryOperator::LessThanEqual:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SLE);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_ULE);
        break;
      case slang::ast::BinaryOperator::LessThan:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SLT);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_ULT);
        break;
      case slang::ast::BinaryOperator::WildcardEquality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_WEQ);
        break;
      case slang::ast::BinaryOperator::WildcardInequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_WNE);
        break;
      case slang::ast::BinaryOperator::LogicalEquivalence: {
        val = build.buildICmp(makeBool(lhsVal), makeBool(rhsVal),
                              BigInt::ICMP_EQ);
        break;
      }
      case slang::ast::BinaryOperator::Power: {
        // signedness of pow only depends on RHS
        if (rhs->type->isSigned())
          val = build.buildSPow(lhsVal, rhsVal);
        else
          val = build.buildUPow(lhsVal, rhsVal);
        break;
      }

      default:
        dyno_unreachable("");
      }

      if (auto wire = val.dyn_as<WireRef>())
        if (auto width = binop.type->getBitstreamWidth())
          wire->numBits = width;

      return std::make_unique<RValue>(val, expr.type.get());
      break;
    }
    case slang::ast::ExpressionKind::UnaryOp: {
      const auto &unop = expr.as<slang::ast::UnaryExpression>();

      auto operand = handle_expr(unop.operand());
      auto operandVal = operand->proGetValue(build);

      // assert(operandVal.getNumBits() == unop.type->getBitstreamWidth());
      HWValue val;

      switch (unop.op) {
      case slang::ast::UnaryOperator::Plus:
        return operand;
        break;
      case slang::ast::UnaryOperator::Minus:
        val = build.buildSub(ctx.constBuild().zeroLike(operandVal).get(),
                             operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseNot:
        val = build.buildXor(ctx.constBuild().onesLike(operandVal).get(),
                             operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseAnd:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().onesLike(operandVal).get(),
                              BigInt::ICMP_EQ);
        break;
      case slang::ast::UnaryOperator::BitwiseOr:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_NE);
        break;
      case slang::ast::UnaryOperator::BitwiseXor:
        val = build.buildRedXor(operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseNand:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().onesLike(operandVal).get(),
                              BigInt::ICMP_NE);
        break;
      case slang::ast::UnaryOperator::BitwiseNor:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_EQ);
      case slang::ast::UnaryOperator::BitwiseXnor:
        val = build.buildXor(build.buildRedXor(operandVal),
                             ConstantRef::fromFourState(FourState::S1));

      case slang::ast::UnaryOperator::LogicalNot:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_EQ);
        break;

      case slang::ast::UnaryOperator::Preincrement:
      case slang::ast::UnaryOperator::Predecrement:
      case slang::ast::UnaryOperator::Postincrement:
      case slang::ast::UnaryOperator::Postdecrement: {
        assert(operand->isLValue());

        bool incr = unop.op == slang::ast::UnaryOperator::Preincrement ||
                    unop.op == slang::ast::UnaryOperator::Postincrement;

        auto one = ctx.constBuild().oneLike(operandVal).get();
        auto nextV = incr ? build.buildAdd(operandVal, one)
                          : build.buildSub(operandVal, one);

        operand->as<LValue>().storeVal(build, nextV);

        bool post = unop.op == slang::ast::UnaryOperator::Postdecrement ||
                    unop.op == slang::ast::UnaryOperator::Postincrement;

        val = post ? operandVal : nextV;
        break;
      }
      }

      return std::make_unique<RValue>(val, expr.type);
    }
    case slang::ast::ExpressionKind::HierarchicalValue:
    case slang::ast::ExpressionKind::NamedValue: {
      auto const &nval = expr.as<slang::ast::ValueExpressionBase>();
      auto [found, sig] = vars.findOrInsert(&nval.symbol, [&] {
        // when the enum is used in a structs, enum values aren't propagated to
        // nval.getConstant()
        if (auto enumMember = nval.symbol.as_if<slang::ast::EnumValueSymbol>())
          return RegisterOrConstantRef{toDynoConstant(enumMember->getValue())};
        if (auto param = nval.symbol.as_if<slang::ast::ParameterSymbol>()) {
          auto &value = param->getValue();
          return RegisterOrConstantRef{toDynoConstant(value)};
        }
        return nval.getConstant()
                   ? RegisterOrConstantRef{toDynoConstant(*nval.getConstant())}
                   : RegisterOrConstantRef{makeReg(
                         nval.type->getBitstreamWidth(), nval.symbol.name)};
      });

      if (auto reg = sig.val().dyn_as<RegisterRef>())
        return std::make_unique<RegLValue>(reg, expr.type);
      else
        return std::make_unique<RValue>(sig.val().as<ConstantRef>(), expr.type);
    }
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::IntegerLiteral>();
      return std::make_unique<RValue>(toDynoConstant(asLit.getValue()),
                                      expr.type);
    }
    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();

      return std::make_unique<RValue>(toDynoConstant(asLit.getValue()),
                                      expr.type);
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto &asConv = expr.as<slang::ast::ConversionExpression>();
      auto src = handle_expr(asConv.operand());
      HWValue val = src->proGetValue(build);
      uint32_t newWidth = asConv.type->getBitstreamWidth();
      if (!val.getNumBits())
        print.error(asConv.operand().sourceRange);
      if (newWidth > *val.getNumBits())
        val = build.buildExt(newWidth, val, src->type->isSigned());
      else if (newWidth < *val.getNumBits())
        val = build.buildTrunc(newWidth, val);

      return std::make_unique<RValue>(val, expr.type);
    }

    case slang::ast::ExpressionKind::RangeSelect: {
      auto &asRangeS = expr.as<slang::ast::RangeSelectExpression>();
      auto value = handle_expr(asRangeS.value());

      auto rangeLeft =
          build.buildUpsize(handle_expr(asRangeS.left())->proGetValue(build),
                            32, asRangeS.left().type->isSigned());
      auto rangeRight =
          build.buildUpsize(handle_expr(asRangeS.right())->proGetValue(build),
                            32, asRangeS.right().type->isSigned());

      HWValue offs;
      HWValue len;

      switch (asRangeS.getSelectionKind()) {
      case slang::ast::RangeSelectionKind::Simple: {
        offs = rangeRight;
        auto sub = build.buildSub(rangeLeft, rangeRight);
        auto add = build.buildAdd(sub, ctx.constBuild().val(32, 1).get());
        len = add;
        break;
      }
      case slang::ast::RangeSelectionKind::IndexedUp: {
        offs = rangeLeft;
        len = rangeRight;
        break;
      }
      case slang::ast::RangeSelectionKind::IndexedDown:
        len = rangeRight;
        auto sub = build.buildSub(rangeLeft, rangeRight);
        auto add = build.buildAdd(sub, ctx.constBuild().val(32, 1).get());
        offs = add;
        break;
      }
      auto [ewidth, ecount, shift] =
          getArrayElemWidthAndLengthAndBase(*asRangeS.value().type);

      if (!len.is<ConstantRef>())
        print.error(expr.sourceRange);

      return Value::slice(
          build, *value, ewidth * len.as<ConstantRef>().getExactVal(), -shift,
          std::to_array({AddressGenTerm{offs, ewidth, ecount}}), expr.type);
    }

    case slang::ast::ExpressionKind::ElementSelect: {
      auto &asElemS = expr.as<slang::ast::ElementSelectExpression>();

      auto value = handle_expr(asElemS.value());
      auto index = build.buildUpsize(
          handle_expr(asElemS.selector())->proGetValue(build), 32);

      auto [ewidth, ecount, shift] =
          getArrayElemWidthAndLengthAndBase(*asElemS.value().type);

      return Value::slice(
          build, *value, ewidth, -shift,
          std::to_array({AddressGenTerm{index, ewidth, ecount}}), asElemS.type);
    }
    case slang::ast::ExpressionKind::MemberAccess: {
      auto &asMemberAcc = expr.as<slang::ast::MemberAccessExpression>();
      if (!(asMemberAcc.value().type->isStruct() ||
            (asMemberAcc.value().type->isUnion() &&
             !asMemberAcc.value().type->isTaggedUnion())))
        abort();

      if (asMemberAcc.member.kind != slang::ast::SymbolKind::Field)
        abort();

      auto &asField = asMemberAcc.member.as<slang::ast::FieldSymbol>();
      if (asField.bitOffset >= UINT32_MAX)
        abort();

      auto value = handle_expr(asMemberAcc.value());
      auto bits = asField.getType().getBitstreamWidth();
      assert(expr.type->getBitstreamWidth() == bits);
      return Value::slice(build, *value, bits, asField.bitOffset,
                          ArrayRef<AddressGenTerm>::emptyRef(), expr.type);
    }

    case slang::ast::ExpressionKind::ConditionalOp: {
      auto &asCond = expr.as<slang::ast::ConditionalExpression>();
      if (asCond.conditions.size() != 1 || asCond.conditions[0].pattern)
        abort();

      auto sel = handle_expr(*asCond.conditions[0].expr)->proGetValue(build);

      if (auto asConst = sel.dyn_as<ConstantRef>()) {
        auto val = asConst.valueEquals(0)
                       ? handle_expr(asCond.right())->proGetValue(build)
                       : handle_expr(asCond.left())->proGetValue(build);
        return std::make_unique<RValue>(val, expr.type);
      }

      auto ifElse = build.buildIfElse(sel, 1);

      build.pushInsertPoint(ifElse.getTrueBlock().end());
      build.buildYield(handle_expr(asCond.left())->proGetValue(build));
      build.popInsertPoint();

      build.pushInsertPoint(ifElse.getFalseBlock().end());
      build.buildYield(handle_expr(asCond.right())->proGetValue(build));
      build.popInsertPoint();

      return std::make_unique<RValue>(ifElse.getYieldValue(0)->as<HWValue>(),
                                      expr.type);
    }

    case slang::ast::ExpressionKind::SimpleAssignmentPattern:
    case slang::ast::ExpressionKind::Concatenation: {
      bool isAssignPat =
          expr.kind == slang::ast::ExpressionKind::SimpleAssignmentPattern;
      bool isAssignPatLHS = false;

      std::span<const slang::ast::Expression *const> operands;
      if (isAssignPat) {
        auto &asSimpleAs =
            expr.as<slang::ast::SimpleAssignmentPatternExpression>();
        operands = asSimpleAs.elements();
        isAssignPatLHS = asSimpleAs.isLValue;
      } else {
        auto &asCC = expr.as<slang::ast::ConcatenationExpression>();
        operands = asCC.operands();
      }

      SmallVec<std::unique_ptr<Value>, 4> values{operands.size()};

      bool lvalue = true;
      for (auto [idx, expr] : Range{operands}.enumerate()) {
        const slang::ast::Expression *evalExpr = expr;
        if (isAssignPatLHS) {
          // in assignment patterns, slang's AST has assignments with empty RHS
          // for all elements
          assert(expr->as<slang::ast::AssignmentExpression>().right().kind ==
                 slang::ast::ExpressionKind::EmptyArgument);
          evalExpr = &expr->as<slang::ast::AssignmentExpression>().left();
        }
        values[idx] = handle_expr(*evalExpr);
        lvalue &= values[idx]->isLValue();
      }
      if (lvalue)
        return std::make_unique<ConcatLValue>(values, expr.type);

      SmallVec<HWValue, 4> buf{values.size()};
      for (auto [idx, val] : Range{values}.enumerate()) {
        buf[idx] = val->proGetValue(build);
      }
      auto val = build.buildConcat(ArrayRef{buf.begin(), buf.end()});
      return std::make_unique<RValue>(val, expr.type);
    }
    case slang::ast::ExpressionKind::Replication: {
      auto &asRepl = expr.as<slang::ast::ReplicationExpression>();

      auto val = handle_expr(asRepl.concat())->proGetValue(build);
      auto cnt = handle_expr(asRepl.count())->proGetValue(build);

      if (!cnt.is<ConstantRef>() || !cnt.as<ConstantRef>().getLimitedVal())
        print.error(asRepl.count().sourceRange);

      return std::make_unique<RValue>(
          build.buildRepeat(val, cnt.as<ConstantRef>().getExactVal()),
          expr.type);
    }
    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      auto &asStructAs =
          expr.as<slang::ast::StructuredAssignmentPatternExpression>();

      uint32_t totalLen = expr.type->getBitstreamWidth();

      HWValue defaultVal;
      bool defaultSext;
      if (asStructAs.defaultSetter) {
        defaultVal = handle_expr(*asStructAs.defaultSetter)->proGetValue(build);
        defaultSext =
            asStructAs.defaultSetter->type->isSigned() ||
            asStructAs.defaultSetter->kind ==
                slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral;
      } else {
        defaultVal =
            std::make_unique<RValue>(
                ctx.constBuild().zero(expr.type->getBitstreamWidth()).get(),
                expr.type)
                ->proGetValue(build);
        defaultSext = false;
      }

      SmallVec<std::pair<const slang::ast::Type *, HWValue>, 2> typeDefaults;
      for (auto &typeSetter : asStructAs.typeSetters) {
        typeDefaults.emplace_back(
            typeSetter.type, handle_expr(*typeSetter.expr)->proGetValue(build));
      }

      HWValue cur;

      if (asStructAs.defaultSetter || !typeDefaults.empty()) {
        DefaultValueTypeWalker typeWalker{build, defaultVal, defaultSext,
                                          typeDefaults};
        cur = typeWalker(expr.type);
      } else
        cur = defaultVal;

      auto spliceIn = [&](HWValue newVal, HWValue offs, HWValue len) {
        assert(len.as<ConstantRef>().valueEquals(*newVal.getNumBits()));
        cur = build.buildInsert(cur, newVal,
                                offs.as<ConstantRef>().getExactVal());
      };

      for (auto &indexSetter : asStructAs.indexSetters) {
        auto idx = handle_expr(*indexSetter.index)->proGetValue(build);
        auto [esize, ecount, shift] = getArrayElemWidthAndLengthAndBase(*asStructAs.type);
        idx = build.buildAdd(idx, ConstantRef::fromU32(shift));
        idx = build.buildMul(idx, ConstantRef::fromU32(esize));

        spliceIn(handle_expr(*indexSetter.expr)->proGetValue(build), idx,
                 ConstantRef::fromU32(esize));
      }

      for (auto &memberSetter : asStructAs.memberSetters) {
        auto &asField = memberSetter.member->as<slang::ast::FieldSymbol>();

        uint32_t offs = asField.bitOffset;
        uint32_t len = asField.getType().getBitstreamWidth();

        HWValue newVal = handle_expr(*memberSetter.expr)->proGetValue(build);
        spliceIn(newVal, ConstantRef::fromU32(offs), ConstantRef::fromU32(len));
      }

      assert(cur.getNumBits() == totalLen);

      return std::make_unique<RValue>(cur, expr.type);
    }
    case slang::ast::ExpressionKind::Call: {
      auto &asCall = expr.as<slang::ast::CallExpression>();
      if (!std::holds_alternative<const slang::ast::SubroutineSymbol *>(
              asCall.subroutine)) {

        auto builtin = std::get<1>(asCall.subroutine);

        switch (builtin.subroutine->knownNameId) {
        case slang::parsing::KnownSystemName::Clog2: {
          auto val = build.buildCLOG2(
              handle_expr(*asCall.arguments().front())->proGetValue(build));
          return std::make_unique<RValue>(val, expr.type);
        }
        case slang::parsing::KnownSystemName::Bits: {
          return std::make_unique<RValue>(
              ConstantRef::fromU32(
                  asCall.arguments().front()->type->getBitstreamWidth()),
              expr.type);
        }
        case slang::parsing::KnownSystemName::Signed: {
          return std::make_unique<RValue>(
              handle_expr(*asCall.arguments().front())->proGetValue(build),
              expr.type);
        }
        case slang::parsing::KnownSystemName::Unsigned: {
          return std::make_unique<RValue>(
              handle_expr(*asCall.arguments().front())->proGetValue(build),
              expr.type);
        }
        default:
          abort();
        }
      }
      auto subr = std::get<0>(asCall.subroutine);
      SmallVec<HWValueOrReg, 8> args{asCall.arguments().size()};
      for (auto [idx, arg] : Range{asCall.arguments()}.enumerate()) {
        HWValueOrReg var;
        auto bind = handle_expr(*arg);

        if (!bind->isLValue())
          var = bind->getValue();
        else if (auto rlv = bind->dyn_as<RegLValue>(); rlv && rlv->fullReg())
          var = rlv->lvReg;
        else {
          if (subr->getArguments()[idx]->direction !=
              slang::ast::ArgumentDirection::In)
            // todo: proxy register for passing elements of unpacked struct or
            // vector. can maybe also do subregister instruction that is a view
            // into a parent reg.
            abort();

          var = bind->proGetValue(build);
        }

        args[idx] = var;
      }

      auto [found, it] = functionMap.findOrInsert(subr, [&] {
        build.pushInsertPoint(regsBackIt.succ());
        auto rv = build.buildFunc().func();
        build.popInsertPoint();
        return rv;
      });
      auto func = FunctionRef{it.val(), ctx.getFuncs()[it.val()]}.iref();

      if (subr->returnValVar) {
        auto rv = build.buildCall(func, args, 1);
        auto callRV = rv.retvals().begin()[0]->as<WireRef>();
        callRV->numBits = subr->returnValVar->getType().getBitstreamWidth();
        return std::make_unique<RValue>(callRV, expr.type);
      }
      build.buildCall(func, args, 0);
      return std::make_unique<RValue>(nullref, nullptr);
    }
    case slang::ast::ExpressionKind::StringLiteral: {
      auto &asStringLit = expr.as<slang::ast::StringLiteral>();
      auto constant = toDynoConstant(asStringLit.getIntValue());
      return std::make_unique<RValue>(constant, expr.type);
    }

    case slang::ast::ExpressionKind::Inside: {
      auto &asInside = expr.as<slang::ast::InsideExpression>();
      auto val = handle_expr(asInside.left());
      SmallVec<HWValue, 4> vals;
      for (auto range : asInside.rangeList()) {
        dbgs() << range->kind << "\n";
      }
      break;
    }

    case slang::ast::ExpressionKind::ArbitrarySymbol:
    case slang::ast::ExpressionKind::Invalid:
    case slang::ast::ExpressionKind::RealLiteral:
    case slang::ast::ExpressionKind::TimeLiteral:
    case slang::ast::ExpressionKind::NullLiteral:
    case slang::ast::ExpressionKind::UnboundedLiteral:
    case slang::ast::ExpressionKind::Streaming:
    case slang::ast::ExpressionKind::DataType:
    case slang::ast::ExpressionKind::TypeReference:
    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
    case slang::ast::ExpressionKind::EmptyArgument:
    case slang::ast::ExpressionKind::ValueRange:
    case slang::ast::ExpressionKind::Dist:
    case slang::ast::ExpressionKind::NewArray:
    case slang::ast::ExpressionKind::NewClass:
    case slang::ast::ExpressionKind::NewCovergroup:
    case slang::ast::ExpressionKind::CopyClass:
    case slang::ast::ExpressionKind::MinTypMax:
    case slang::ast::ExpressionKind::ClockingEvent:
    case slang::ast::ExpressionKind::AssertionInstance:
    case slang::ast::ExpressionKind::TaggedUnion:
      break;
    }

    abort();
  }
};
}; // namespace dyno

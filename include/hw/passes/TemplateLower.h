#pragma once

#include "dyno/Context.h"
#include "dyno/Lexer.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/PrintParse.h"
#include "hw/StdCellInfo.h"
#include "support/Bits.h"
#include "support/Tokenizer.h"
#include "support/TwoLevelSet.h"
#include <limits>
namespace dyno {

class TemplateLowerPass : public Pass<TemplateLowerPass> {
  Context &ctx;
  Vec<InstrRef> worklist;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(std::string, templateModuleNames, "")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  SmallDenseMap<DialectOpcode, ObjRef<Module>> templates;
  void setUpTemplates() {
    Tokenizer moduleNmRange{config.templateModuleNames, " ,;:"};
    TwoLevelMap<SSOStringRef, DialectOpcode> map;

    DynoLexer lexer{ctx.getDialectInfos(), ArrayRef<char>::emptyRef(), ""};
    for (auto [opc, modNm] : Range{moduleNmRange}.pairwise()) {
      lexer.reset(opc);
      auto parsedOpc = lexer.popOpcode();
      if (!parsedOpc)
        report_fatal_error("invalid opcode key: \"{}\"", opc);
      map[modNm] = *parsedOpc;
    }
    const uint32_t specified = map.size();
    uint32_t found = 0;
    for (auto mod : ctx.getStore<Module>()) {
      if (auto it = map.find(mod->name)) {
        templates[it.val()] = mod;
        found++;
        map.erase(it);
      }
    }
    if (found != specified)
      report_fatal_error(
          "did not find all referenced template modules in context, missing: ");
  }

  void runOnInstr(InstrRef instr) {
    auto it = templates.find(instr.getDialectOpcode());
    assert(it);

    // defSz, paramSz, def, params[]
    MutInstr<FatDynObjRef<>> instance{
        ctx, HW_STDCELL_INSTANCE, instr.getNumDefs(), instr.getNumDefs() + 3};
    instance.other(0) = ctx.resolve(it.val());
    for (unsigned i = 0; i < instr.getNumDefs(); i++) {
      auto def = instr.def(i)->as<HWValue>();
      instance.def(i) = def;
      instr.def(i).replace(FatDynObjRef{nullref});
      // def sizes as params (just constant inputs)
      instance.other(i + 1) = ConstantRef::fromU32(*def.getNumBits());
    }
    unsigned idx = instr.getNumDefs() + 1;

    uint64_t paramSize = Range{instr.others().as<HWValue>().tf([](HWValue val) {
                           return *val.getNumBits();
                         })}.sum_t<uint64_t>();
    if (paramSize > std::numeric_limits<uint32_t>::max())
      report_fatal_error(ctx, instr,
                         "total param size does not fit into 32 bits");

    instance.other(idx++) = ConstantRef::fromU32(paramSize);

    HWInstrBuilder build{ctx, instr};
    SmallVec<HWValue, 16> operands{instr.others().as<HWValue>().reverse()};
    auto inputsCC = build.buildConcat(operands);
    instance.other(idx++) = inputsCC;
    build.build(std::move(instance));
    build.destroyInstr(instr);
  }

  void runWorklist() {
    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      runOnInstr(instr);
    }
  }

  void runOnProcess(ProcessIRef proc) {
    worklist.push_back_range(Range{proc.block()}.filter([&](auto instr) {
      return templates.contains(instr.getDialectOpcode());
    }));
    runWorklist();
  }
  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void runWrapper(auto &&runFunc) {
    setUpTemplates();
    runFunc();
    templates.clear();
  }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&TemplateLowerPass::run, &TemplateLowerPass::runModule);

  explicit TemplateLowerPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return TemplateLowerPass{ctx}; }
};
}; // namespace dyno

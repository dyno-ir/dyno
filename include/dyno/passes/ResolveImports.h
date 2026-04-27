#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"

namespace dyno {

class ResolveImportsPass : public Pass<ResolveImportsPass> {
  Context &ctx;

public:
  void runOnSymbol(SymbolRef symb) {
    if (!symb->defUse.hasSingleDef())
      return;

    auto defInstr = symb->defUse.getSingleDef()->instr();
    assert(defInstr.isOpc(CORE_EXPORT));

    auto actual = defInstr.other(0)->fat();

    SmallVec<ObjRef<Instr>, 16> deleteList;
    for (auto use : symb->defUse.uses()) {
      auto useInstr = use.instr();
      if (useInstr.isOpc(CORE_IMPORT)) {
        useInstr.def().as<FatDynObjRef<InstrDefUse>>()->replaceAllUsesWith(
            actual);
        deleteList.emplace_back(useInstr);
      } else {
        use.replace(actual);
      }
    }

    for (auto instr : deleteList)
      ctx.destroyInstr(ctx.resolve(instr));
  }
  void run() {
    for (auto symb : ctx.getStore<Symbol>())
      runOnSymbol(symb);
  }

  static constexpr auto runFuncs =
      mk_tuple(&ResolveImportsPass::run, &ResolveImportsPass::runOnSymbol);

  explicit ResolveImportsPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return ResolveImportsPass{ctx}; }
};
}; // namespace dyno

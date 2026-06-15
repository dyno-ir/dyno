#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include "support/Ranges.h"
#include "support/SlabAllocator.h"
#include "support/StringRef.h"
#include "support/Tokenizer.h"
#include "support/TwoLevelSet.h"
#include <fstream>
#include <string>
namespace dyno {

// Dump header for structural access for internal registers during sim, for
// verilator style C++ TBs.
class DumpSimHeaderPass : public Pass<DumpSimHeaderPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(std::string, path, "sim_header.h")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  // simple naive tree structure for dumping
  struct RegRef {
    std::string name;
    ObjRef<Register> reg;
    uint32_t addr;
    uint32_t len;
  };
  struct Node {
    std::string name;
    TwoLevelMap<SSOStringRef, Node *> children;
    SmallVec<RegRef, 32> regs;

    TwoLevelMap<SSOStringRef, uint32_t> regNameDuplicateCnt;
  };
  SlabAllocator<Node> nodes;

  void addToTree(RegisterIRef reg, const char *name) {
    Node *node = &nodes[0];
    const char *last = nullptr;
    auto tokenizer = Tokenizer{name, "."};
    for (auto [back, tok] : Range{tokenizer}.mark_back()) {
      if (back) {
        last = tok.data();
        break;
      }
      node = node->children
                 .findOrInsert(
                     tok, [&]() { return nodes.allocate(std::string(tok)); })
                 .second.val();
    }
    assert(last);
    // todo: sub elements for struct type regs
    node->regs.emplace_back(last, reg.oref(), 0, *reg.getNumBits());

    // deduplicate names
    auto [found, it] = node->regNameDuplicateCnt.findOrInsert(
        SSOStringRef{last}, []() { return 0; });
    if (found) {
      it.val()++;
      node->regs.back().name += "__" + std::to_string(it.val());
    }
  }

  void runOnRegister(RegisterIRef reg) {
    auto &regNameInfo = ctx.getCtx<HWDialectContext>().regNameInfo;
    auto names = regNameInfo.getNames(reg.oref());

    for (auto name : names) {
      addToTree(reg, name);
    }
  }

  void dumpNode(std::ostream &os, Node *node, std::string prefix = "") {
    for (auto [nm, child] : node->children)
      dumpNode(os, child, prefix + "__" + node->name);

    std::print(os, "struct ModInst__{}__{} {{\n", prefix, node->name);

    std::print(os, "static constexpr const char* name = \"{}\";\n", node->name);

    for (auto [nm, child] : node->children) {
      std::print(os, "ModInst__{}__{}__{} {};\n", prefix, node->name,
                 child->name, child->name);
    }

    std::print(os, "\n");

    for (auto reg : node->regs) {
      std::print(os, "SimRegisterHandle {};\n", reg.name);
    }

    std::print(os, "template <typename Func>\nconstexpr void forAllRegs(Func&& "
                   "func [[maybe_unused]]) {{\n");
    for (auto reg : node->regs)
      std::print(os, "  func(\"{}\", {});\n", reg.name, reg.name);
    std::print(os, "}}\n");

    std::print(os, "auto getAllSubModules() {{ return std::tie(\n");
    for (auto [back, nm, child] : Range{node->children}.mark_back().flat()) {
      std::print(os, "{}", child->name);
      if (!back)
        std::print(os, ", ");
    }
    std::print(os, "); }}\n");

    std::print(os, "constexpr auto operator->() {{ return this; }}\n");
    std::print(os, "constexpr auto operator->() const {{ return this; }}\n");

    std::print(os, "}};\n");
  }

public:
  void runOnModule(ModuleIRef mod) {
    nodes.emplace_back("Top");

    // Build Tree
    for (auto reg : mod.regs())
      runOnRegister(reg);

    std::ofstream of(config.path);
    std::print(of, "#pragma once\n#include \"hw/SimHeader.h\"\n#include <tuple>\n\n");
    dumpNode(of, &nodes[0]);

    nodes.clear();
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

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
      mk_tuple(&DumpSimHeaderPass::run, &DumpSimHeaderPass::runModule);

  explicit DumpSimHeaderPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return DumpSimHeaderPass{ctx}; }
};
}; // namespace dyno

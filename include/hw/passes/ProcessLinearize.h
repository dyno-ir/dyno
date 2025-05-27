#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/Obj.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "support/DenseMap.h"
#include "support/DenseSet.h"
#include "support/DynBitSet.h"
#include "support/Utility.h"

namespace dyno {

class ProcessLinearizePass {

  using BitSet = UnsizedBitSet<SmallVec<uint64_t, 2>>;

  // todo: slab allocator
  struct Custom {
    enum BitField : uint8_t { VISITED = 1, PRE_VISITED = 2 };
    uint8_t bitField = 0;
    SmallDenseSet<ProcessIRef, 1> predsSet;
    BitSet dependingOutputs;
  };

  using CustInstrRef = CustomInstrRef<InstrRef, Custom *>;
  using CustProcIRef = CustomInstrRef<ProcessIRef, Custom *>;
  HWContext &ctx;

  static constexpr ProcessIRef blackBoxWrite(uint16_t id) {
    assert(id < 0xFFFF);
    auto rv = ProcessIRef{ObjID::invalid(), (Instr *)nullptr, uint16_t(1 + id)};
    return rv;
  }
  static constexpr bool isBlackBoxWrite(ProcessIRef proc) {
    return proc.getObjID() == ObjID::invalid() && proc.getCustom() >= 1;
  }

public:
  explicit ProcessLinearizePass(HWContext &ctx) : ctx(ctx), copier(ctx) {}
  std::vector<CustProcIRef> procs;
  DeepCopier copier;

  void findDeps(ModuleIRef module) {

    uint inputIdxCnt = 0;
    uint outputIdxCnt = 0;

    for (auto reg : module.regs()) {

      SmallDenseSet<ProcessIRef, 1> writers;

      bool regIsAnyInput =
          reg.isOpc(HW_INPUT_REGISTER_INSTR, HW_INOUT_REGISTER_INSTR,
                    HW_REF_REGISTER_INSTR);

      bool regIsAnyOutput =
          reg.isOpc(HW_OUTPUT_REGISTER_INSTR, HW_INOUT_REGISTER_INSTR,
                    HW_REF_REGISTER_INSTR);

      for (auto access : reg.oref().uses()) {
        auto instr = HWInstrRef{access.instr()};
        switch (instr.getDialectOpcode().raw()) {

        case HW_STORE.raw(): {
          writers.insert(instr.parentProc(ctx));
          break;
        }

        case HW_INSTANCE.raw(): {
          auto other = instr.other(0)->as<ModuleRef>();
          bool isInputFromInst = other->ports[access.getNum() - 1].portType.is(
              HW_INOUT_REGISTER_INSTR, HW_OUTPUT_REGISTER_INSTR,
              HW_REF_REGISTER_INSTR);
          bool isOutputToInst = other->ports[access.getNum() - 1].portType.is(
              HW_INOUT_REGISTER_INSTR, HW_INPUT_REGISTER_INSTR,
              HW_REF_REGISTER_INSTR);

          regIsAnyInput |= isInputFromInst;
          regIsAnyOutput |= isOutputToInst;
          break;
        }

        case HW_LOAD.raw():
          break;

        case HW_STORE_DEFER.raw():
          break;

        case HW_TRIGGER_INSTR.raw():
          break;

        default:
          dyno_unreachable("register ref'd by unexpected instr");
        }
      }

      if (regIsAnyInput)
        writers.insert(blackBoxWrite(inputIdxCnt++));

      SmallVec<ProcessIRef, 4> writersVec{writers.size()};
      std::copy(writers.begin(), writers.end(), writersVec.begin());

      for (auto access : reg.oref().uses()) {
        auto instr = HWInstrRef{access.instr()};

        switch (instr.getDialectOpcode().raw()) {
        case HW_LOAD.raw(): {
          auto proc = CustProcIRef{instr.parentProc(ctx)};
          auto &custom = proc.getOrEmplace([] { return new Custom(); });
          custom->predsSet.insert(ArrayRef{writersVec});
          break;
        }
        case HW_STORE.raw(): {
          if (regIsAnyOutput) {
            auto proc = CustProcIRef{instr.parentProc(ctx)};
            auto &custom = proc.getOrEmplace([] { return new Custom(); });
            custom->dependingOutputs.setDyn(outputIdxCnt);
          }
          break;
        }
        default:
          break;
        }
      }

      if (regIsAnyOutput)
        outputIdxCnt++;
    }
  }

  void visit(SmallVecImpl<ProcessIRef> &ordered, CustProcIRef proc) {
    if ((proc.get()->bitField & Custom::VISITED))
      return;
    if ((proc.get()->bitField & Custom::PRE_VISITED)) {
      std::cerr << "cyclic:\n";
      dumpInstr(proc);
      abort();
    }

    proc.get()->bitField |= Custom::PRE_VISITED;

    for (auto depend : Range{proc.get()->predsSet}) {
      if (isBlackBoxWrite(depend))
        continue;
      if (depend == proc)
        continue;
      visit(ordered, depend);
    }

    proc.get()->bitField |= Custom::VISITED;
    ordered.emplace_back(proc);
  }

  void linearize(ModuleIRef module) {
    SmallVec<ProcessIRef, 16> ordered;
    for (auto proc : module.procs()) {
      visit(ordered, proc);
    }

    constexpr bool Conservative = true;

    if constexpr (Conservative) {
      // iterate thru ordered in reverse order and mark all dependencies.
      for (auto proc : Range{ordered}.reverse()) {
        auto &custom = *CustProcIRef{proc}.get();
        for (auto pred : Range{custom.predsSet}) {
          if (isBlackBoxWrite(pred))
            continue;
          CustProcIRef{pred}.get()->dependingOutputs |= custom.dependingOutputs;
        }
      }

      std::cerr << "ordered:\n";
      for (auto proc : ordered) {
        dumpInstr(proc);
      }
      std::cerr << "\n\n\n";

      size_t cnt = ordered.size();
      SmallVec<uint32_t, 16> toMerge;
      while (cnt != 0) {
        toMerge.clear();
        BitSet *repr = nullptr;

        bool skippedAny = false;

        for (auto [i, proc] : Range{ordered}.enumerate()) {
          if (!proc)
            continue;

          auto &other = CustProcIRef{proc}.get()->dependingOutputs;

          if (!repr) {
            repr = &other;
            toMerge.emplace_back(i);
            continue;
          }

          if (other == *repr ||
              (!skippedAny && (repr->count() == 0 || other.count() == 0))) {
            if (repr->count() == 0)
              repr = &other;
            toMerge.emplace_back(i);
          } else
            skippedAny = true;
        }

        auto proc = ordered[toMerge[0]];
        ordered[toMerge[0]] = nullref;

        for (auto idx : Range{toMerge}.drop_front()) {
          // todo: move
          copier.deepCopyInstrs(ordered[idx].block().begin(),
                                proc.block().end().pred());

          if (auto *ptr = CustProcIRef{ordered[idx]}.get())
            delete ptr;
          HWInstrBuilder{ctx}.destroyInstr(ordered[idx]);
          ordered[idx] = nullref;
        }

        cnt -= toMerge.size();
      }
    } else {
      if (ordered.empty())
        return;

      auto proc = ordered.front();
      for (auto mergeProc : Range{ordered}.drop_front()) {
        // todo: move
        copier.deepCopyInstrs(mergeProc.block().begin(),
                              proc.block().end().pred());

        if (auto *ptr = CustProcIRef{mergeProc}.get())
          delete ptr;
        HWInstrBuilder{ctx}.destroyInstr(mergeProc);
      }
    }
  }

  void runOnModule(ModuleIRef module) {
    findDeps(module);
    linearize(module);
  }

  void run() {
    for (auto instr : ctx.getInstrs()) {
      InstrRef{instr}.clearCustomStorage();
    }

    for (auto mod : ctx.getModules()) {
      runOnModule(ModuleRef{mod}.iref());
    }

    for (auto instr : ctx.getInstrs()) {
      if (auto *val = CustInstrRef{instr}.get())
        delete val;
    }
  }
};
}; // namespace dyno

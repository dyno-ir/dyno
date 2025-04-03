#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "scf/IDs.h"
#include "support/SmallVec.h"
#include "support/Utility.h"

namespace dyno {

class HWContext;

class Module {

  // category order in this enum is maintained in defUse via manual hooking.
public:
  enum UseClass { UC_DEF, UC_REG, UC_PROC, UC_FUNC, UC_COUNT };

private:
  static uint classifyUse(InstrRef ref) {
    uint32_t type = (ref.getDialect() << 16) | ref.getOpcode();
    switch (type) {
    case (DIALECT_RTL << 16 | HW_REGISTER_INSTR):
      return UC_REG;
    case (DIALECT_RTL << 16 | HW_PROCESS_INSTR):
      return UC_PROC;
    case (DIALECT_SCF << 16 | SCF_FUNC_INSTR):
      return UC_FUNC;
    default:
      dyno_unreachable("type cannot use module");
    }
  }
  static uint classifyIdx(Module *mod, uint idx) {
    // i bet this is faster than binary search
    for (size_t i = 0; i < UC_COUNT; i++)
      if (mod->catBounds[i] > idx)
        return i;
    dyno_unreachable("not classified");
  }
  static uint classifyIdxBinSearch(Module *mod, uint idx) {
    size_t lb = 0;
    size_t ub = UC_COUNT - 1;

    while (true) {
      size_t center = lb + (ub - lb) / 2;

      size_t lower = (center == 0) ? 0 : mod->catBounds[center - 1];
      size_t upper = mod->catBounds[center];

      if (idx < lower)
        ub = center;
      else if (idx >= upper)
        lb = center + 1;
      else
        return center;
    }
  }
  static bool insertHook(InstrDefUse *self, OperandRef ref) {
    uint useClassID = ref.isDef() ? UC_DEF : classifyUse(ref.instr());
    Module *asModule = reinterpret_cast<Module *>(self);
    // O(#categories) insertion, keeps inter-category order but not intra (base
    // case of this for use+def is implemented in instrDefUse)
    for (uint id = UC_COUNT - 1; id != useClassID; id--) {
      self->manual_move(asModule->catBounds[id - 1], asModule->catBounds[id]);
      asModule->catBounds[id]++;
    }
    self->manual_insert(asModule->catBounds[useClassID]++, ref);
    return true;
  }
  static bool eraseHook(InstrDefUse *self, DynObjRef ref) {
    Module *asModule = reinterpret_cast<Module *>(self);

    uint useClassID = classifyIdx(asModule, ref.getCustom());
    // move last ref in same category into slot we're freeing
    self->manual_move(asModule->catBounds[useClassID] - 1, ref.getCustom());
    for (uint id = useClassID; id < UC_COUNT - 1; id++) {
      // move last of next category into last of current category (now first of
      // next category)
      self->manual_move(asModule->catBounds[id + 1] - 1,
                        asModule->catBounds[id] - 1);
      asModule->catBounds[id]--;
    }

    self->manual_pop_back();
    asModule->catBounds[UC_COUNT - 1]--;
    return true;
  }

public:
  InstrDefUse defUse;
  std::string name;
  std::array<uint32_t, UC_COUNT> catBounds = {};

  // todo: fast ordered (inline linked list) smallvec wrapper?
  SmallVec<FatObjRef<Register>, 8> ports;

  Module(DynObjRef, std::string name) : name(name) {
    defUse.setInsertHook(insertHook);
    defUse.setEraseHook(eraseHook);
  }
};

class ModuleRef : public FatObjRef<Module> {
public:
  using FatObjRef<Module>::FatObjRef;
  ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}
  InstrRef getModuleInstr() { return ptr->defUse.getSingleDef()->instr(); }

private:
  auto usesOfCategory(Module::UseClass uc) {
    return Range{ptr->defUse.begin() + ((uc == 0) ? 0 : ptr->catBounds[uc - 1]),
                 ptr->defUse.begin() + ptr->catBounds[uc]};
  }

public:
  auto procs() {
    return usesOfCategory(Module::UC_PROC);
    // return ptr->defUse.uses().filter([](OperandRef ref) {
    //   return ref.instr().getDialect() == DIALECT_RTL &&
    //          ref.instr().getOpcode() == HW_PROCESS_INSTR;
    // });
  }
  auto regs() {
    return usesOfCategory(Module::UC_REG);
    // return ptr->defUse.uses().filter([](OperandRef ref) {
    //   return ref.instr().getDialect() == DIALECT_RTL &&
    //          ref.instr().getOpcode() == HW_REGISTER_INSTR;
    // });
  }
  auto funcs() {
    return usesOfCategory(Module::UC_FUNC);
    // return ptr->defUse.uses().filter([](OperandRef ref) {
    //   return ref.instr().getDialect() == DIALECT_SCF &&
    //          ref.instr().getOpcode() == SCF_FUNC_INSTR;
    // });
  }

  void addPort(RegisterRef ref, Register::PortType portType) {
    ref.getPtr()->portIndex = ptr->ports.size();
    ref.getPtr()->portType = portType;
    ptr->ports.emplace_back(ref);
  }
};

template <> struct ObjTraits<Module> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_MODULE};
  using FatRefT = ModuleRef;
};

}; // namespace dyno

#pragma once
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Utility.h"
#include <cstdint>

namespace dyno {

enum class SensMode : uint8_t {
  POSEDGE,
  NEGEDGE,
  ANYEDGE,
  IFF,
  IFFN,
  NUM_SENS_MODES
};

struct SensList {
  SmallVec<std::pair<RegisterRef, SensMode>, 2> signals;

  // static SensList empty() { return SensList{}; }
  explicit operator bool() const { return !signals.empty(); }
};

class SensModesTag {};

class SensModesRef : public FatObjRef<SensModesTag> {
public:
  using FatObjRef::FatObjRef;

  SensModesRef(FatObjRef ref) : FatObjRef(ref) {}

  static SensModesRef fromSensList(const SensList &list) {
    assert(list.signals.size() < 16 && "can only store small sens lists");
    uint32_t obj = 0;
    for (size_t i = 0; i < list.signals.size(); i++) {
      obj |= (unsigned(list.signals[i].second) << (i * 2));
    }

    return FatObjRef<SensModesTag>{ObjID{obj}, (SensModesTag *)nullptr, 1};
  }
  static SensModesRef fromArray(ArrayRef<SensMode> modes) {
    assert(modes.size() < 16 && "can only store small sens lists");
    uint32_t obj = 0;
    for (size_t i = 0; i < modes.size(); i++) {
      obj |= (unsigned(modes[i]) << (i * 2));
    }

    return FatObjRef<SensModesTag>{ObjID{obj}, (SensModesTag *)nullptr, 1};
  }
};

template <> struct ObjTraits<SensModesTag> {
  static constexpr DialectType ty{HW_SENS_MODES};
  using FatRefT = SensModesRef;
};

class TriggerIRef;

class Trigger {
  // defUse might not even be required.
public:
  InstrDefUse defUse;

  using modes_t = uint64_t;

private:
  modes_t sensModes;
  uint8_t numModes;

  static constexpr size_t BitsPerMode = clog2((size_t)SensMode::NUM_SENS_MODES);
  static constexpr size_t ModeMask = bit_mask_ones<size_t>(BitsPerMode);
  static constexpr size_t MaxNumModes = bit_mask_sz<modes_t> / BitsPerMode;

public:
  SensMode getMode(size_t i) {
    assert(i < numModes);
    return SensMode((sensModes >> (BitsPerMode * i)) & ModeMask);
  }
  void inverseMode(size_t i) {
    assert(i < numModes);
    SensMode newMode;
    switch (getMode(i)) {
    case SensMode::POSEDGE:
      newMode = SensMode::NEGEDGE;
      break;
    case SensMode::NEGEDGE:
      newMode = SensMode::POSEDGE;
      break;
    case SensMode::ANYEDGE:
      newMode = SensMode::ANYEDGE;
      break;
    case SensMode::IFF:
      newMode = SensMode::IFFN;
      break;
    case SensMode::IFFN:
      newMode = SensMode::IFF;
      break;
    default:
      dyno_unreachable("unknown sens mode");
    }
    setMode(i, newMode);
  }
  void setMode(size_t i, SensMode sensMode) {
    assert(i < numModes);
    sensModes &= ~(ModeMask << (BitsPerMode * i));
    sensModes |= (modes_t(sensMode) << (BitsPerMode * i));
  }
  void resize(size_t newNumModes) {
    assert(newNumModes <= MaxNumModes && "max num modes is static");
    numModes = newNumModes;
  }
  size_t size() { return numModes; }
  void addMode(SensMode mode) {
    resize(numModes + 1);
    setMode(numModes - 1, mode);
  }

  auto modesRaw() { return sensModes; }

  Trigger(DynObjRef) : sensModes(0), numModes(0) {}
  Trigger(DynObjRef, const Trigger &other)
      : sensModes(other.sensModes), numModes(other.numModes) {}
};

class TriggerRef : public FatObjRef<Trigger> {
public:
  using FatObjRef::FatObjRef;
  TriggerRef(FatObjRef ref) : FatObjRef(ref) {}

  TriggerIRef iref();
};

template <> struct ObjTraits<Trigger> {
  static constexpr DialectType ty{HW_TRIGGER};
  using FatRefT = TriggerRef;
};

class TriggerIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  TriggerIRef(InstrRef ref) : InstrRef(ref) {}

  TriggerRef oref() { return def(0)->as<TriggerRef>(); }
  auto regs() { return this->others().deref().as<RegisterRef>(); };

  static bool is_impl(FatObjRef<Instr> ref) {
    return InstrRef{ref}.isOpc(HW_TRIGGER_DEF);
  }
  static bool is_impl(FatDynObjRef<> ref) {
    if (auto iref = ref.dyn_as<InstrRef>())
      return iref.isOpc(HW_TRIGGER_DEF);
    return false;
  }
};

inline TriggerIRef TriggerRef::iref() {
  return ptr->defUse.getSingleDef()->instr().as<TriggerIRef>();
};

}; // namespace dyno

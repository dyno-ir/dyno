
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "hw/passes/ParseDyno.h"
#include "hw/run/HWInterpreter.h"
#include "ieee1800/sv_vpi_user.h"
#include "ieee1800/vpi_user.h"
#include "support/SmallVec.h"
#include "support/Tokenizer.h"
#include "vpi/Iterator.h"
#include "vpi/Range.h"
#include <cstring>
#include <dlfcn.h>
#include <string_view>

using namespace dyno;

void load_vpi_module(const char *path) {
  void *handle = dlopen(path, RTLD_NOW);
  if (!handle) {
    fprintf(stderr, "dlopen failed: %s\n", dlerror());
    exit(1);
  }

  void (**startup_routines)() =
      (void (**)())dlsym(handle, "vlog_startup_routines");
  if (!startup_routines) {
    fprintf(stderr, "No vlog_startup_routines found\n");
    return;
  }

  for (int i = 0; startup_routines[i] != nullptr; i++)
    startup_routines[i](); // Call each startup routine
}

struct Args {
  int argc;
  char **argv;
};
Args args;

class Callback {
public:
  PLI_INT32 (*func)(struct t_cb_data *);
  char *user_data;
  uint64_t time;
  uint reason;
  uint idx;
};

class Callbacks {
public:
  std::vector<SmallVec<Callback *, 4>> callbacks;

  void ensure(uint idx) {
    if (idx >= callbacks.size())
      callbacks.resize(idx + 1);
  }
  SmallVecImpl<Callback *> &get(uint reason) {
    ensure(reason);
    return callbacks[reason];
  }
  void deleteAll(uint reason) {
    while (!callbacks[reason].empty())
      remove(callbacks[reason].back());
  }

  Callback *insert(Callback c) {
    auto &v = get(c.reason);
    c.idx = v.size();
    Callback *ptr = new Callback(c);
    v.emplace_back(ptr);
    return ptr;
  }

  void remove(Callback *c) {
    auto &v = get(c->reason);
    v.back()->idx = c->idx;
    v.erase_unordered(v.begin() + c->idx);
  }
};

class VPIHandler {
public:
  HWContext *ctx;
  HWInterpreter *interpreter;
  ModuleIRef top;
  InstrRef topInstance;
  Callbacks callbacks;
  NewDeleteObjStore<Iterator> iterators;
  uint64_t time = 0;
  bool finishFlag = false;

  FatDynObjRef<> fromName(std::string_view name) {
    FatDynObjRef ref = nullref;
    for (auto t : Tokenizer{name, "."}) {
      if (!ref) {
        if (t != "top")
          return nullref;
        ref = top;
        continue;
      } else {
        if (auto mod = ref.dyn_as<ModuleIRef>()) {
          bool found;
          for (auto reg : mod.regs()) {
            found = false;
            // todo: better data structure
            for (auto nm : ctx->regNameInfo.getNames(reg.oref())) {
              if (nm == t) {
                ref = reg;
                found = true;
                break;
              }
            }
            if (found)
              break;
          }
          if (found)
            continue;
        }
        // todo: submodules, interfaces, ...
      }

      return nullref;
    }
    return ref;
  }
};

VPIHandler *handler;

// Build a wrapper top module in which the actual top is instantiated.
// This is so we have an instance of our top module to point to.
InstrRef createTopInstance(HWContext &ctx, ModuleIRef topLevelModule) {
  ModuleIRef mod = ctx.createModule("__Top");
  HWInstrBuilder build{ctx};
  build.setInsertPoint(mod.block().begin());

  SmallVec<RegisterRef, 16> ports;
  for (auto port : topLevelModule.ports())
    ports.emplace_back(
        build.buildPort(mod, port.getOpcode(), port.getNumBits()));

  return build.buildInstance(topLevelModule.mod(), ports);
}

int main(int argc, char **argv) {
  HWContext ctx;

  if (argc != 3) {
    fprintf(stderr, "usage: %s <dyno file> <cocotb lib>\n", argv[0]);
    return -1;
  }

  args = Args{argc, argv};

  ParseDynoPass parse{ctx};
  parse.config.fileName = std::string(argv[1]);

  parse.run();
  HWPrinter print{std::cout};
  print.printCtx(ctx);

  auto mod = *ctx.getModules().begin();
  mod.iref().rebuildSignature();
  auto topInstance = createTopInstance(ctx, mod.iref());

  HWInterpreter interpreter{ctx, mod.iref(), std::cout, std::cerr};
  interpreter.fstWriter.emplace("trace.fst");
  interpreter.setup();
  interpreter.fstInitHierarchy();
  interpreter.trace = true;

  handler = new VPIHandler;
  handler->ctx = &ctx;
  handler->interpreter = &interpreter;
  handler->top = mod.iref(); // todo: real top
  handler->topInstance = topInstance;
  load_vpi_module(argv[2]);

  SmallVec<Callback *, 4> cbs = handler->callbacks.get(cbStartOfSimulation);
  for (auto *cb : cbs) {
    t_cb_data data = {};
    data.reason = cbStartOfSimulation;
    data.user_data = cb->user_data;
    cb->func(&data);
  }

  while (!handler->finishFlag) {
    interpreter.evalActive();

    SmallVec<Callback *, 4> cbs = handler->callbacks.get(cbReadWriteSynch);
    for (auto *cb : cbs) {
      t_cb_data data = {};
      data.reason = cbReadWriteSynch;
      data.user_data = cb->user_data;
      cb->func(&data);
    }
    interpreter.evalNBA();

    SmallVec<Callback *, 4> cbs2 = handler->callbacks.get(cbAfterDelay);
    for (auto *cb : cbs2) {
      t_cb_data data = {};
      data.reason = cbAfterDelay;
      data.user_data = cb->user_data;
      cb->func(&data);
      interpreter.forwardTime(cb->time);
      handler->time += cb->time;
    }
  }
}

DynObjRef fromHandle(vpiHandle handle) {
  return std::bit_cast<DynObjRef>(handle);
}
vpiHandle toHandle(DynObjRef ref) { return std::bit_cast<vpiHandle>(ref); }

PLI_INT32 vpi_control(PLI_INT32 operation, ...) {
  switch (operation) {
  case vpiFinish:
    handler->finishFlag = true;
    return 0;
  default:
    abort();
  }
}

vpiHandle vpi_handle_by_name(PLI_BYTE8 *name, vpiHandle scope) {
  assert(scope == NULL && "unsupported");
  auto ref = handler->fromName(std::string_view{name});
  if (!ref)
    return nullptr;
  return toHandle(ref);
}

PLI_INT32 vpi_chk_error(p_vpi_error_info error_info_p) { return 0; }

PLI_INT32 vpi_free_object(vpiHandle object) {
  return vpi_release_handle(object);
}

PLI_INT32 vpi_release_handle(vpiHandle object) { return 1; }

PLI_INT32 vpi_get(PLI_INT32 property, vpiHandle object) {
  auto ref = fromHandle(object);
  switch (property) {
  case vpiType: {
    switch (*ref.getType()) {
    case *HW_MODULE:
      return vpiModule;
    case *HW_REGISTER:
      return vpiReg;
    case *CORE_INSTR: {
      auto instr = handler->ctx->getInstrs().resolve(ref.as<ObjRef<Instr>>());
      switch (*instr.getDialectOpcode()) {
      case *HW_INSTANCE:
        return vpiModule;

      case *HW_REGISTER_DEF:
      case *HW_INPUT_REGISTER_DEF:
      case *HW_OUTPUT_REGISTER_DEF:
      case *HW_INOUT_REGISTER_DEF:
      case *HW_REF_REGISTER_DEF:
        return vpiReg;
      default:
        abort();
      }
    }
    }
    break;
  }
  case vpiTimePrecision: {
    return -9; // todo: hardcoded 1 ns rn
  }

  case vpiVector: {
    // assuming reg
    auto instr = handler->ctx->getInstrs().resolve(ref.as<ObjRef<Instr>>());
    auto reg = instr.as<RegisterIRef>();
    return reg.getNumBits() != 1;
  }

  case vpiSize: {
    // assuming reg
    auto instr = handler->ctx->getInstrs().resolve(ref.as<ObjRef<Instr>>());
    auto reg = instr.as<RegisterIRef>();
    return *reg.getNumBits();
  }
  }
  abort();
}

PLI_BYTE8 *vpi_get_str(PLI_INT32 property, vpiHandle object) {
  static std::vector<char> data;
  switch (property) {
  case vpiType: {
    static const char name[] = "logic";
    data.clear();
    data.insert(data.end(), name, name + sizeof(name));
    return data.data();
  }
  case vpiFullName: {
    auto ref = fromHandle(object);
    switch (*ref.getType()) {
    case *HW_MODULE: {
      auto mod = handler->ctx->getModules().resolve(ref.as<ObjRef<Module>>());
      data.clear();
      data.insert(data.end(), mod->name.c_str(),
                  mod->name.c_str() + mod->name.size() + 1);
      return data.data();
    }
    case *CORE_INSTR: {
      auto instr = handler->ctx->getInstrs().resolve(ref.as<ObjRef<Instr>>());
      switch (*instr.getDialectOpcode()) {
      case *HW_INSTANCE: {
        // todo: once instances are specific
        static const char name[] = "top";
        data.clear();
        data.insert(data.end(), name, name + sizeof(name));
        return data.data();
      }
      default:
        abort();
      }
    }
    default:
      abort();
    }
  }
  default:
    abort();
  }
}

void vpi_get_time(vpiHandle object, p_vpi_time time_p) {
  if (time_p->type == vpiSimTime) {
    time_p->high = 0;
    time_p->low = 0;
  } else if (time_p->type == vpiScaledRealTime) {
    time_p->real = 0;
  }
}

GenericBigIntRef getValue(FatDynObjRef<> ref) {
  if (auto asConstRef = ref.dyn_as<ConstantRef>())
    return GenericBigIntRef{asConstRef};
  else if (auto asWire = ref.dyn_as<WireRef>())
    return GenericBigIntRef{handler->interpreter->wireVals[asWire]};
  else if (auto asRegister = ref.dyn_as<RegisterRef>())
    return GenericBigIntRef{handler->interpreter->regVals[asRegister]};
  else if (auto instr = ref.dyn_as<InstrRef>()) {
    if (auto refIRef = instr.dyn_as<RegisterIRef>())
      return GenericBigIntRef{handler->interpreter->regVals[refIRef.oref()]};
  }
  abort();
}

void vpi_get_value(vpiHandle expr, p_vpi_value value_p) {
  switch (value_p->format) {
  case vpiObjTypeVal:;
    abort();
  case vpiIntVal: {
    auto val = getValue(handler->ctx->resolveObj(fromHandle(expr)));
    value_p->value.integer = val.getExactVal();
    return;
  }
  case vpiBinStrVal: {
    static std::vector<char> buffer;
    buffer.clear();
    std::stringstream str;
    auto val = getValue(handler->ctx->resolveObj(fromHandle(expr)));
    BigInt::stream_bin_4s_vlog(str, val, false);
    buffer.insert(buffer.end(), str.view().begin(), str.view().end());
    buffer.push_back(0);
    value_p->value.str = buffer.data();
    return;
  }
  }
  value_p->value.integer = 0;
}

PLI_INT32 vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p) {
  vlog_info_p->argc = args.argc;
  vlog_info_p->argv = args.argv;
  static char name[] = "dyno";
  static char version[] = "0.0.1";
  vlog_info_p->product = name;
  vlog_info_p->version = version;
  return 1;
}

vpiHandle vpi_handle(PLI_INT32 type, vpiHandle refHandle) {
  auto ref = fromHandle(refHandle);
  switch (type) {
  case vpiLeftRange: {
    auto ref = fromHandle(refHandle).as<VPIRangeRef>();
    return toHandle(ConstantRef::fromU32(ref.getLeft()));
  }
  case vpiRightRange: {
    auto ref = fromHandle(refHandle).as<VPIRangeRef>();
    return toHandle(ConstantRef::fromU32(ref.getRight()));
  }
  default:
    abort();
  }
  return NULL;
}

vpiHandle vpi_handle_by_index(vpiHandle object, PLI_INT32 indx) { return NULL; }

vpiHandle vpi_iterate(PLI_INT32 type, vpiHandle refHandle) {
  switch (type) {
  case vpiModule:
  case vpiInstance: {
    assert(refHandle == nullptr);
    std::function<DynObjRef()> func = [i = 0]() mutable -> DynObjRef {
      if (i)
        return nullref;
      ++i;
      // only returns top module for now. make this recurse instead to find all
      // instances.
      return handler->topInstance;
    };
    return toHandle(handler->iterators.create(std::move(func)));
  }
  case vpiRange: {
    uint32_t bits;
    auto ref = fromHandle(refHandle);
    switch (*ref.getType()) {
    case *HW_REGISTER:
      bits = *handler->ctx->getRegs()
                  .resolve(ref.as<ObjRef<Register>>())
                  .getNumBits();
      break;
    case *HW_WIRE:
      bits = *handler->ctx->getWires()
                  .resolve(ref.as<ObjRef<Wire>>())
                  .getNumBits();
      break;
    case *CORE_INSTR: {
      auto instr = handler->ctx->getInstrs().resolve(ref.as<ObjRef<Instr>>());
      switch (*instr.getDialectOpcode()) {
      case *HW_REGISTER_DEF:
      case *HW_INPUT_REGISTER_DEF:
      case *HW_OUTPUT_REGISTER_DEF:
      case *HW_INOUT_REGISTER_DEF:
      case *HW_REF_REGISTER_DEF:
        bits = *instr.as<RegisterIRef>().getNumBits();
        break;
      default:
        abort();
      }
      break;
    }
    default:
      abort();
    }
    std::function<DynObjRef()> func = [i = 0, bits]() mutable -> DynObjRef {
      if (i)
        return nullref;
      ++i;
      return VPIRangeRef{bits};
    };
    return toHandle(handler->iterators.create(std::move(func)));
  }
  //  {
  //   assert(refHandle == nullptr);
  //   std::function<DynObjRef()> func =
  //       [ref = handler->ctx->getModules().begin()]() mutable -> DynObjRef {
  //     if (ref == handler->ctx->getModules().end())
  //       return nullref;
  //     auto rv = *ref;
  //     ++ref;
  //     return rv;
  //   };
  //   return toHandle(handler->iterators.create(std::move(func)));
  // }
  case vpiInternalScope: {
  }
  }
  abort();
}

vpiHandle vpi_scan(vpiHandle iterator) {
  auto ref =
      handler->iterators.resolve(fromHandle(iterator).as<ObjRef<Iterator>>());
  auto next = ref->next();
  if (!next)
    return NULL;
  return toHandle(next);
}

vpiHandle vpi_put_value(vpiHandle object, p_vpi_value value_p,
                        p_vpi_time time_p, PLI_INT32 flags) {

  assert(time_p->type == vpiSimTime && time_p->low == 0 && time_p->high == 0);
  auto ref = fromHandle(object);
  auto reg = handler->ctx->getInstrs()
                 .resolve(ref.as<ObjRef<Instr>>())
                 .as<RegisterIRef>();

  switch (value_p->format) {
  case vpiScalar: {
    auto scalar = value_p->value.scalar;
    scalar ^= (scalar >> 1); // flip x and z
    // todo: this should go on an event queue
    handler->interpreter->setReg(reg, PatBigInt::fromFourState(scalar, 1));
    break;
  }
  case vpiBinStrVal: {
    std::string_view view{value_p->value.str};
    auto val = BigInt::parseBin(ArrayRef{view});
    if (!val)
      abort();
    handler->interpreter->setReg(reg, *val);
    break;
  }
  case vpiIntVal: {
    handler->interpreter->setReg(reg,
                                 BigInt::fromU64(value_p->value.integer, 32));
    break;
  }
  default:
    abort();
  }
  return NULL;
}

vpiHandle vpi_register_cb(p_cb_data cb_data_p) {
  switch (cb_data_p->reason) {
  case cbStartOfSimulation:
  case cbEndOfSimulation:
  case cbReadWriteSynch: {

    return (vpiHandle)handler->callbacks.insert(
        Callback{cb_data_p->cb_rtn, cb_data_p->user_data, ~0UL,
                 uint(cb_data_p->reason), 0});
  }
  case cbAfterDelay: {
    if (cb_data_p->time->type != vpiSimTime)
      abort();
    uint64_t t = (uint64_t(cb_data_p->time->high) << 32) | cb_data_p->time->low;
    // t += handler->time;
    return (vpiHandle)handler->callbacks.insert(
        Callback{cb_data_p->cb_rtn, cb_data_p->user_data, t,
                 uint(cb_data_p->reason), 0});
  }
  default:
    abort();
    return NULL;
  }
}

PLI_INT32 vpi_remove_cb(vpiHandle cb_obj) {
  Callback *ptr = reinterpret_cast<Callback *>(cb_obj);
  handler->callbacks.remove(ptr);
  return 0;
}

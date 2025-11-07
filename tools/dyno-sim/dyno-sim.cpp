
#include "dyno/Obj.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/passes/ParseDyno.h"
#include "hw/run/HWInterpreter.h"
#include "ieee1800/vpi_user.h"
#include "support/ErrorRecovery.h"
#include "support/SlabAllocator.h"
#include <dlfcn.h>

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

class Tokenizer {
  std::string_view s, delims;
  size_t pos = 0;

public:
  Tokenizer(std::string_view str, std::string_view d = " ")
      : s(str), delims(d) {}
  std::optional<std::string_view> next() {
    pos = s.find_first_not_of(delims, pos);
    if (pos == std::string_view::npos)
      return std::nullopt;
    size_t end = s.find_first_of(delims, pos);
    auto token = s.substr(pos, (end == std::string_view::npos) ? s.size() - pos
                                                               : end - pos);
    pos = (end == std::string_view::npos) ? s.size() : end + 1;
    return token;
  }
};

class Callback {
public:
  PLI_INT32 (*func)(struct t_cb_data *);
  char* user_data;
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
  HWContext ctx;
  ModuleIRef top;
  Callbacks callbacks;

  FatDynObjRef<> fromName(std::string_view name) {
    Tokenizer tok{name, "."};
    FatDynObjRef ref = nullref;
    while (auto t = tok.next()) {

      if (!ref) {
        if (t != "top")
          report_fatal_error("must start with top");
        ref = top;
        break;
      } else {
        if (auto mod = ref.dyn_as<ModuleIRef>()) {
          for (auto reg : mod.regs()) {
            // todo: better data structure
            for (auto name : ctx.regNameInfo.getNames(reg.oref())) {
              ref = reg;
              break;
            }
          }
        }
        // todo: submodules, interfaces, ...
      }
    }

    return ref;
  }
};

VPIHandler *handler;

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
  HWInterpreter interpreter{ctx, mod.iref(), std::cout, std::cerr};

  handler = new VPIHandler;
  load_vpi_module(argv[2]);

  auto &cbs = handler->callbacks.get(cbStartOfSimulation);
  for (auto *cb : cbs) {
    t_cb_data data = {};
    data.reason = cbStartOfSimulation;
    data.user_data = cb->user_data;
    cb->func(&data);
  }
}

DynObjRef fromHandle(vpiHandle handle) {
  return std::bit_cast<DynObjRef>(handle);
}
vpiHandle toHandle(DynObjRef ref) { return std::bit_cast<vpiHandle>(ref); }

PLI_INT32 vpi_control(PLI_INT32 operation, ...) {
  switch (operation) {
  case vpiFinish:
    exit(0);
  default:
    abort();
  }
}

vpiHandle vpi_handle_by_name(PLI_BYTE8 *name, vpiHandle scope) {
  assert(scope == NULL && "unsupported");
  return toHandle(handler->fromName(std::string_view{name}));
}

PLI_INT32 vpi_chk_error(p_vpi_error_info error_info_p) { return 0; }

PLI_INT32 vpi_free_object(vpiHandle object) {
  return vpi_release_handle(object);
}

PLI_INT32 vpi_release_handle(vpiHandle object) { return 1; }

PLI_INT32 vpi_get(PLI_INT32 property, vpiHandle object) { return 0; }

PLI_BYTE8 *vpi_get_str(PLI_INT32 property, vpiHandle object) {
  static char buf[128] = "";
  return buf;
}

void vpi_get_time(vpiHandle object, p_vpi_time time_p) {
  if (time_p->type == vpiSimTime) {
    time_p->high = 0;
    time_p->low = 0;
  } else if (time_p->type == vpiScaledRealTime) {
    time_p->real = 0;
  }
}

void vpi_get_value(vpiHandle expr, p_vpi_value value_p) {
  switch (value_p->format) {
  case vpiObjTypeVal:;
    // ...
  }
  value_p->value.integer = 0;
}

PLI_INT32 vpi_get_vlog_info(p_vpi_vlog_info vlog_info_p) {
  vlog_info_p->argc = args.argc;
  vlog_info_p->argv = args.argv;
  static char name[] = "dyno";
  vlog_info_p->product = name;
  vlog_info_p->version = name;
  return 1;
}

vpiHandle vpi_handle(PLI_INT32 type, vpiHandle refHandle) { return NULL; }

vpiHandle vpi_handle_by_index(vpiHandle object, PLI_INT32 indx) { return NULL; }

vpiHandle vpi_iterate(PLI_INT32 type, vpiHandle refHandle) { return NULL; }

vpiHandle vpi_scan(vpiHandle iterator) { return NULL; }

vpiHandle vpi_put_value(vpiHandle object, p_vpi_value value_p,
                        p_vpi_time time_p, PLI_INT32 flags) {
  return NULL;
}

vpiHandle vpi_register_cb(p_cb_data cb_data_p) {
  switch (cb_data_p->reason) {
  case cbStartOfSimulation:
  case cbEndOfSimulation: {
    return (vpiHandle)handler->callbacks.insert(
        Callback{cb_data_p->cb_rtn, cb_data_p->user_data, uint(cb_data_p->reason), 0});
  }
  default:
    return NULL;
  }
}

PLI_INT32 vpi_remove_cb(vpiHandle cb_obj) {
  Callback* ptr = reinterpret_cast<Callback*>(cb_obj);
  handler->callbacks.remove(ptr);
  return 0;
}
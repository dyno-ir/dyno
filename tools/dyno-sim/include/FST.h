#pragma once

#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "fstapi.h"
#include "hw/Register.h"
#include "hw/Wire.h"
// RAII wrapper around fst writer API
template <typename... ValueTypes> class FSTWriter {
  dyno::StaticGenericObjVecMap<fstHandle, ValueTypes...> map;

public:
  FSTWriter(const char *name) {
    writer = fstWriterCreate(name, 1);
    fstWriterSetTimescale(writer, -9);
    fstWriterSetScope(writer, fstScopeType::FST_ST_VCD_MODULE, "$rootio", NULL);
    fstWriterSetScope(writer, fstScopeType::FST_ST_VCD_MODULE, "top", NULL);
  }
  ~FSTWriter() { fstWriterClose(writer); }
  void stepForward(uint64_t t) { fstWriterEmitTimeChange(writer, time += t); }

  template <typename T>
  void updateValue(T ref, const dyno::BigIntAPI auto &value) {
    auto handle = map[ref];
    std::stringstream str;
    dyno::BigInt::stream_bin_4s_vlog(str, value, false);
    fstWriterEmitValueChange(writer, handle,
                             reinterpret_cast<const void *>(str.str().c_str()));
  }

  void endDefinitions() {
    fstWriterSetUpscope(writer);
    fstWriterSetUpscope(writer);
  }

  enum class VarType {
    INTEGER = FST_VT_VCD_INTEGER,
    PARAMETER = FST_VT_VCD_PARAMETER,
    REG = FST_VT_VCD_REG,
    WIRE = FST_VT_VCD_WIRE,
    STRING = FST_VT_GEN_STRING,
    BIT = FST_VT_SV_BIT,
    LOGIC = FST_VT_SV_LOGIC
  };

  enum class VarDir {
    IMPLICIT = FST_VD_IMPLICIT,
    INPUT = FST_VD_INPUT,
    OUTPUT = FST_VD_OUTPUT,
    INOUT = FST_VD_INOUT,
  };

  template <typename T>
  void createVar(T ref, VarType type, VarDir dir, uint32_t numBits,
                 const char *name) {
    auto handle = fstWriterCreateVar(writer, (fstVarType)type, (fstVarDir)dir,
                                     numBits, name, (fstHandle)0);
    map.get_ensure(ref) = handle;
  }

private:
  fstWriterContext *writer;
  uint64_t time = 0;
};

using RegWireFSTWriter = FSTWriter<dyno::Register, dyno::Wire>;
#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "fstapi.h"
#include "hw/HWTypeIDs.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
#include "type/TypeContext.h"
#include "type/TypeInfo.h"
#include <sstream>
// #include <spanstream>
//  RAII wrapper around fst writer API
template <typename... ValueTypes> class FSTWriter {
  struct Fragment {
    fstHandle handle;
    uint32_t bits;
  };

  dyno::Context &ctx;
  dyno::StaticGenericObjVecMap<SmallVec<Fragment, 1>, ValueTypes...> map;

public:
  FSTWriter(dyno::Context &ctx, const char *name) : ctx(ctx) {
    writer = fstWriterCreate(name, 1);
    fstWriterSetTimescale(writer, -9);
    fstWriterSetScope(writer, fstScopeType::FST_ST_VCD_MODULE, "$rootio", NULL);
    fstWriterSetScope(writer, fstScopeType::FST_ST_VCD_MODULE, "top", NULL);
  }
  ~FSTWriter() { fstWriterClose(writer); }
  void stepForward(uint64_t t) { fstWriterEmitTimeChange(writer, time += t); }

  template <typename T>
  void updateValue(T ref, const dyno::BigIntAPI auto &value) {
    uint32_t offs = 0;
    auto &frags = map[ref];
    if (frags.empty())
      return;
    for (auto [handle, bits] : Range{frags}.reverse()) {
      SmallVec<char, 2048> buf(bits);
      // std::ospanstream str{buf};
      std::stringstream str;

      dyno::BigInt temp;
      dyno::BigInt::rangeSelectOp4S(temp, value, offs, bits);
      offs += bits;

      dyno::BigInt::stream_bin_4s_vlog(str, temp, false);
      fstWriterEmitValueChange(writer, handle, std::move(str).str().c_str());
      // reinterpret_cast<const void *>(buf.data()));
    }
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
  void createStruct(T ref, dyno::StructTypeRef type, VarDir dir,
                    const char *name) {
    fstWriterSetScope(writer, fstScopeType::FST_ST_VCD_STRUCT, name, nullptr);
    for (auto &elem : type->elemns) {
      createVarRecursive(
          ref, ctx.resolve(elem.type), dir,
          ctx.getCtx<dyno::TypeDialectContext>().strings.get(elem.ident));
    }
    fstWriterSetUpscope(writer);
  }

  static fstVarType baseTypeToFstType(dyno::BaseTypeRef type) {
    if (type == dyno::hw::HW_TYPE_VLOG_LOGIC)
      return fstVarType::FST_VT_SV_LOGIC;
    if (type == dyno::hw::HW_TYPE_VLOG_BIT)
      return fstVarType::FST_VT_SV_BIT;
    if (type == dyno::hw::HW_TYPE_VLOG_REG)
      return fstVarType::FST_VT_VCD_REG;
    dyno_unreachable("invalid base type");
  }

  template <typename T>
  void createBase(T ref, dyno::BaseTypeRef type, VarDir dir, const char *name) {
    map[ref].emplace_back(fstWriterCreateVar(writer, baseTypeToFstType(type),
                                             (fstVarDir)dir, 1, name,
                                             (fstHandle)0),
                          1);
  }

  template <typename T>
  void createArray(T ref, dyno::ArrayTypeRef type, VarDir dir,
                   const char *name) {
    uint32_t cnt = type->len.as<dyno::ConstantRef>().getExactVal();
    if (auto asBase = type->element.dyn_as<dyno::BaseTypeRef>()) {
      map[ref].emplace_back(
          fstWriterCreateVar(writer, baseTypeToFstType(asBase), (fstVarDir)dir,
                             cnt, name, (fstHandle)0),
          cnt);
    } else {

      for (uint32_t i = cnt; i-- > 0;) {
        std::string nm = std::string(name) + "[" + std::to_string(i) + "]";
        createVarRecursive(ref, ctx.resolve(type->element), dir, nm.c_str());
      }
    }
  }

  template <typename T>
  void createEnum(T ref, dyno::EnumTypeRef type, VarDir dir, const char *name) {
    uint32_t elem_count = type->elemns.size();
    SmallVec<const char *, 16> literalPtrs;
    SmallVec<std::string, 16> valueStrings;
    SmallVec<const char *, 16> valuePtrs;
    literalPtrs.reserve(elem_count);
    valueStrings.reserve(elem_count);
    valuePtrs.reserve(elem_count);

    for (auto &elem : type->elemns) {
      literalPtrs.push_back(
          ctx.getCtx<dyno::TypeDialectContext>().strings.get(elem.ident));

      std::stringstream ss;
      dyno::BigInt::stream_bin_4s_vlog(
          ss, ctx.resolve(elem.value).template as<dyno::ConstantRef>(), false);
      valueStrings.push_back(ss.str());
      valuePtrs.push_back(valueStrings.back().c_str());
    }

    auto enumHandle = fstWriterCreateEnumTable(
        writer, name, elem_count, type->underlying.getBitLen(ctx),
        literalPtrs.data(), valuePtrs.data());

    assert(type->underlying.is<dyno::BaseTypeRef>() ||
           (type->underlying.is<dyno::ArrayTypeRef>() &&
            type->underlying.as<dyno::ArrayTypeRef>()
                ->element.is<dyno::BaseTypeRef>()) &&
               "enum underlying must be simple type");

    createVarRecursive(ref, type->underlying, dir, name);
    fstWriterEmitEnumTableRef(writer, enumHandle);
  }

  template <typename T>
  void createVarRecursive(T ref, dyno::FatTypeRef type, VarDir dir,
                          const char *name) {
    if (auto asStruct = type.dyn_as<dyno::StructTypeRef>())
      return createStruct(ref, asStruct, dir, name);
    if (auto asBase = type.dyn_as<dyno::BaseTypeRef>())
      return createBase(ref, asBase, dir, name);
    if (auto asArray = type.dyn_as<dyno::ArrayTypeRef>())
      return createArray(ref, asArray, dir, name);
    if (auto asEnum = type.dyn_as<dyno::EnumTypeRef>())
      return createEnum(ref, asEnum, dir, name);
  }

  template <typename T>
  void createVar(T ref, dyno::FatTypeRef type, VarDir dir, uint32_t numBits,
                 const char *name) {
    map.get_ensure(ref);
    if (numBits > 4096)
      return;
    if (type)
      createVarRecursive(ref, type, dir, name);
    else {
      map[ref].emplace_back(fstWriterCreateVar(writer, FST_VT_SV_LOGIC,
                                               (fstVarDir)dir, numBits, name,
                                               (fstHandle)0),
                            numBits);
    }
  }

  uint64_t getTime() const { return time; }

private:
  fstWriterContext *writer;
  uint64_t time = 0;
};

using RegWireFSTWriter = FSTWriter<dyno::Register, dyno::Wire>;

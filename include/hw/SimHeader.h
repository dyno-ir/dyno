#pragma once
#include "dyno/Constant.h"
#include "hw/Register.h"
#include "hw/run/HWInterpreter.h"
#include "support/Bits.h"
#include "support/SmallVec.h"
#include "type/TypeInfo.h"

class SimRegisterHandle {
  dyno::HWInterpreter *interp = nullptr;
  dyno::ObjRef<dyno::Register> reg;
  uint32_t addr;
  uint32_t len;

public:
  SimRegisterHandle(dyno::HWInterpreter *interp,
                    dyno::ObjRef<dyno::Register> reg, uint32_t addr,
                    uint32_t len)
      : interp(interp), reg(reg), addr(addr), len(len) {}
  template <dyno::BigIntAPI T> void operator=(const auto &o) {
    interp->setReg(reg, o);
  }
  SimRegisterHandle() = default;

  SimRegisterHandle operator[](uint32_t index) {
    if (addr == 0 && len == interp->getCtx().resolve(reg).getNumBits()) {
      auto &ctx = interp->getCtx();
      auto type =
          ctx.getCtx<dyno::HWDialectContext>().regTypeInfo.getType(ctx, reg);
      switch (type.typeID()) {
      case dyno::TypeDialectTypeID::ARRAY: {
        auto asArray = ctx.resolve(type).as<dyno::ArrayTypeRef>();
        auto len = asArray->len.as<dyno::ConstantRef>().getExactVal();
        auto start = asArray->start
                         ? asArray->start.as<dyno::ConstantRef>().getExactVal()
                         : 0;

        auto elemSize =
            ctx.resolve(asArray->element).getBitLen(interp->getCtx());
        auto idx = (index - start) * elemSize;

        assert(index - start < len);

        return SimRegisterHandle(interp, reg, idx, elemSize);
      }
      }
    }
    assert(len > 32);
    return SimRegisterHandle{interp, reg, addr + index * 32, 32};
  }

  // return length of array for array types. Length in bits is returned for non
  // array types.
  uint32_t arrSize() {
    if (addr == 0 && len == interp->getCtx().resolve(reg).getNumBits()) {
      auto &ctx = interp->getCtx();
      auto type =
          ctx.getCtx<dyno::HWDialectContext>().regTypeInfo.getType(ctx, reg);
      switch (type.typeID()) {
      case dyno::TypeDialectTypeID::ARRAY: {
        auto asArray = ctx.resolve(type).as<dyno::ArrayTypeRef>();
        auto len = asArray->len.as<dyno::ConstantRef>().getExactVal();
        auto start = asArray->start
                         ? asArray->start.as<dyno::ConstantRef>().getExactVal()
                         : 0;
        return len - start;
      }
      }
    }
    return len - addr;
  }

  uint32_t operator=(uint32_t word) {
    assert(len <= 32);
    if (get().valueEquals(word))
      return word;
    auto iref = interp->getCtx().resolve(reg).iref();
    auto &val = interp->getReg(iref);
    dyno::BigInt::insertOp4S(val, val, dyno::ConstantRef{len, word, 0, 0},
                             addr);
    interp->regValueChanged(iref);
    return word;
  }

  explicit operator uint32_t() const {
    assert(len <= 32);
    auto &val = interp->getReg(interp->getCtx().resolve(reg).iref());
    dyno::BigInt sel;
    dyno::BigInt::rangeSelectOp4S(sel, val, addr, len);
    if (sel.getIs4S())
      sel.conv4To2State();
    return sel.getExactVal();
  }

  operator uint64_t() const {
    if (len <= 32)
      return uint32_t(*this);
    auto arr = data();
    assert(arr.size() == 2);
    return uint64_t(arr[1]) << 32 | arr[0];
  }

  explicit operator bool() const { return uint32_t(*this); }

  uint32_t u32() const { return uint32_t(*this); }
  uint64_t u64() const { return uint64_t(*this); }

  SmallVec<uint32_t, 4> data() const {
    auto val = get();
    if (val.getIs4S())
      val.conv4To2State();

    SmallVec<uint32_t, 4> rv(reserve_tag, val.getNumWords());
    uint32_t extNumWords = round_up_div(val.getNumBits(), dyno::WordBits);
    rv.push_back_range(Range{val.getWords()});
    for (uint32_t i = rv.size(); i < extNumWords; i++)
      rv.emplace_back(repeatBits(val.getExtend(), 2));

    return rv;
  }

  const dyno::BigInt &getFull() const {
    auto &ref = interp->getReg(interp->getCtx().resolve(reg).iref());
    return ref;
  }

  dyno::FourState getBit(uint32_t i) const {
    assert(i < len);
    return getFull().getBit(addr + i);
  }
  dyno::BigInt getRange(uint32_t addr, uint32_t len) const {
    dyno::BigInt rv;
    assert(len <= this->len);
    dyno::BigInt::rangeSelectOp4S(rv, getFull(), this->addr + addr, len);
    return rv;
  }
  dyno::BigInt get() const {
    if (this->addr == 0 &&
        this->len == interp->getCtx().resolve(reg).getNumBits()) {
      auto &rv = getFull();
      assert(rv.getNumBits() == len);
      return rv;
    } else
      return getRange(0, len);
  }
};

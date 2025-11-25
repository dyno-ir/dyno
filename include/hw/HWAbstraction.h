#pragma once
#include "BitRange.h"
#include "HWValue.h"
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/BitRange.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "hw/Wire.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ArrayRef.h"
#include "support/ErrorRecovery.h"
#include "support/RTTI.h"
#include "support/Utility.h"
#include <algorithm>
#include <dyno/NewDeleteObjStore.h>
#include <iterator>
#include <type_traits>

namespace dyno {

class HWInstrBuilder {
public:
  HWContext &ctx;
  BlockRef_iterator<true> insert;

  HWInstrBuilder(HWContext &ctx, BlockRef_iterator<true> insert)
      : ctx(ctx), insert(insert) {}

  HWInstrBuilder(HWContext &ctx, InstrRef insert) : ctx(ctx) {
    setInsertPoint(insert);
  }

  HWInstrBuilder(HWContext &ctx) : ctx(ctx), insert() {}

  void insertInstr(InstrRef instr) { insert.insertPrev(instr); }

protected:
  // Compile-time edge cases for adding special types like BitRange
  // todo: rename this and put this in new HWInstrBuilder deriving from instr
  // build
  template <typename T> void addSpecialRef(InstrBuilder &build, const T &ref) {
    if constexpr (std::is_same_v<BitRange, T>) {
      build.addRef(ref.getAddr());
      build.addRef(ref.getLen());
    } else if constexpr (std::is_same_v<AddressGenTerm, T>) {
      build.addRef(ref.getIdx());
      build.addRef(ConstantRef::fromU32(ref.getFact()));
      build.addRef(ConstantRef::fromU32(ref.getMax().value_or(~0u)));
    } else if constexpr (std::is_same_v<AddressGenTermOperand, T>) {
      build.addRef(ref.getIdx());
      build.addRef(ConstantRef::fromU32(ref.getFact()));
      build.addRef(ConstantRef::fromU32(ref.getMax().value_or(~0u)));
    } else if constexpr (IsArrayRef<std::decay_t<T>> ||
                         IsRange<std::decay_t<T>>) {
      for (auto elem : ref)
        addSpecialRef(build, elem);
    } else {
      build.addRef(ref);
    }
  }

public:
  template <typename... Ts>
  void addRefs(InstrRef instr, bool addWireDef, Ts... operands) {
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    ([&] { addSpecialRef(build, operands); }(), ...);
  }

  template <typename... Ts> unsigned getNumOperands(Ts... ts) {
    unsigned size = 0;
    (
        // Special types may need more than one operand slot
        [&] {
          if constexpr (std::is_same_v<BitRange, std::decay_t<Ts>>)
            size += 2;
          else if constexpr (std::is_same_v<AddressGenTerm, std::decay_t<Ts>>)
            size += 3;
          else if constexpr (std::is_same_v<AddressGenTermOperand,
                                            std::decay_t<Ts>>)
            size += 3;
          else if constexpr (IsArrayRef<std::decay_t<Ts>>) {
            auto len = ts.end() - ts.begin();
            if (!ts.empty())
              size += len * getNumOperands(ts[0]);
          } else if constexpr (IsRange<std::decay_t<Ts>>) {
            for (auto elem : ts)
              size += getNumOperands(elem);
          } else
            size += 1;
        }(),
        ...);
    return size;
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectID dialect, OpcodeID opcode, bool addWireDef,
                        Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        addWireDef + getNumOperands<Ts...>(operands...), dialect, opcode)};

    insertInstr(instr);
    addRefs(instr, addWireDef, operands...);
    return HWInstrRef{instr};
  }

  HWInstrRef buildInstr(DialectOpcode opc, bool addWireDef,
                        ArrayRef<HWValue> operands) {
    auto instr =
        InstrRef{ctx.getInstrs().create(addWireDef + operands.size(), opc)};

    insertInstr(instr);
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    for (auto op : operands)
      build.addRef(op);
    return HWInstrRef{instr};
  }

  InstrBuilder buildInstrRaw(DialectOpcode opc, unsigned numOperands) {
    auto instr = InstrRef{ctx.getInstrs().create(numOperands, opc)};
    insertInstr(instr);
    return InstrBuilder{instr};
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectOpcode opc, bool addWireDef, Ts... operands) {
    return buildInstr(opc.getDialectID(), opc.getOpcodeID(), addWireDef,
                      operands...);
  }

#define COMM_OP(opcode, ident, constFunc, bigIntFunc)                          \
  template <IsAnyHWValue T, IsAnyHWValue... Ts>                                \
  HWValue ident(T first, Ts... rest) {                                         \
    if constexpr (!(std::is_same_v<T, WireRef> ||                              \
                    (std::is_same_v<Ts, WireRef> || ...)))                     \
      if (first.template is<ConstantRef>() &&                                  \
          (rest.template is<ConstantRef>() && ...)) {                          \
        ConstantBuilder build{ctx.getConstants()};                             \
        build.val(first.template as<ConstantRef>());                           \
        ([&] { build.constFunc(rest.template as<ConstantRef>()); }(), ...);    \
        return build.get();                                                    \
      }                                                                        \
    auto rv = buildInstr(opcode, true, first, rest...).defW();                 \
    rv->numBits = first.getNumBits();                                          \
    return rv;                                                                 \
  }                                                                            \
  /*note: Destroys the operands array!*/                                       \
  HWValue ident(MutArrayRef<HWValue> operands) {                               \
    Range{operands}.sort(commutativeOpOperandOrder);                           \
    bool multipleConstants = operands.size() >= 2 &&                           \
                             operands.end()[-1].is<ConstantRef>() &&           \
                             operands.end()[-2].is<ConstantRef>();             \
                                                                               \
    auto bits = *operands[0].getNumBits();                                     \
    assert(operands.drop_front().all(                                          \
        [&](HWValue val) { return val.getNumBits() == bits; }));               \
                                                                               \
    if (multipleConstants) {                                                   \
      auto cbuild = ctx.constBuild();                                          \
      cbuild.val(bits, 0);                                                     \
                                                                               \
      size_t index = operands.size() - 1;                                      \
      while (operands.size() != 0) {                                           \
        auto last = operands[index].dyn_as<ConstantRef>();                     \
        cbuild.constFunc(last);                                                \
        if (!last)                                                             \
          break;                                                               \
        index--;                                                               \
      }                                                                        \
      index++;                                                                 \
      operands = MutArrayRef{operands.data(), index};                          \
      operands.back() = cbuild.get();                                          \
    }                                                                          \
                                                                               \
    if (operands.size() == 1)                                                  \
      return operands[0];                                                      \
                                                                               \
    auto defW = ctx.getWires().create(bits);                                   \
    auto ib = buildInstrRaw(opcode, 1 + operands.size());                      \
    ib.addRef(defW).other();                                                   \
    ib.addRefs(operands);                                                      \
                                                                               \
    return defW;                                                               \
  }

  static bool commutativeOpWireOrder(WireRef lhs, WireRef rhs) {
    return lhs.getSingleDef()->instr().getObjID() <
           rhs.getSingleDef()->instr().getObjID();
  }
  static bool commutativeOpOperandOrder(HWValue lhs, HWValue rhs) {
    if (lhs.is<WireRef>() && rhs.is<WireRef>())
      return commutativeOpWireOrder(lhs.as<WireRef>(), rhs.as<WireRef>());
    return lhs.is<WireRef>();
  }

  FOR_HW_COMM_OPS(COMM_OP)

  // template <IsAnyHWValue... Ts> HWInstrRef buildAdd2(Ts... operands) {

  //   // canonicalize further by having constants rightmost, possible even
  //   // sort wires by def opcode?

  //   // todo: bitmap
  //   std::array<bool, sizeof...(operands)> isSameOpc = {};
  //   ssize_t operandDelta = 0;
  //   bool anyIsSameOpc = 0;

  //   size_t index = 0;
  //   for (HWValue operand : {operands...}) {
  //     // todo: via constexpr ifs for direct ConstantRef/WireRef
  //     if (auto asWire = operand.template dyn_as<WireRef>();
  //         asWire && asWire.hasSingleDef() &&
  //         asWire.getSingleDef()->instr().isOpc(DialectID{DIALECT_OP},
  //                                              OpcodeID{OP_ADD})) {
  //       anyIsSameOpc = 1;
  //       isSameOpc[index] = 1;
  //       operandDelta = asWire.getSingleDef()->instr().getNumOthers() - 1;
  //     }
  //     index++;
  //   }

  //   if (!anyIsSameOpc) {
  //     return buildInstr(DialectID{DIALECT_OP}, OpcodeID{OP_ADD}, true,
  //                       operands...);
  //   }

  //   auto instr = InstrRef{
  //       ctx.getInstrs().create(1 + sizeof...(operands) + operandDelta,
  //                              DialectID{DIALECT_OP}, OpcodeID{OP_ADD})};
  //   insertInstr(instr);
  //   InstrBuilder build{instr};
  //   // todo: size?
  //   build.addRef(ctx.getWires().create());
  //   build.other();

  //   index = 0;
  //   for (HWValue operand : {operands...}) {
  //     if (isSameOpc[index]) {
  //       InstrRef otherInstr = operand.as<WireRef>().getSingleDef()->instr();
  //       for (auto subOp :
  //            operand.as<WireRef>().getSingleDef()->instr().others())
  //         build.addRef(subOp->template as<HWValue>());

  //       ctx.getCFG()[otherInstr].erase();
  //       ctx.getInstrs().destroy(otherInstr);
  //     } else
  //       build.addRef(operand);
  //     index++;
  //   }

  //   return HWInstrRef{instr};
  // }

#define BINOP(opcode, ident, constFunc, bigIntFunc)                            \
  template <IsAnyHWValue LHS, IsAnyHWValue RHS>                                \
  HWValue ident(LHS lhs, RHS rhs) {                                            \
    if (lhs.template is<ConstantRef>() && rhs.template is<ConstantRef>()) {    \
      return ConstantBuilder{ctx.getConstants()}                               \
          .val(lhs.template as<ConstantRef>())                                 \
          .constFunc(rhs.template as<ConstantRef>())                           \
          .get();                                                              \
    }                                                                          \
    auto rv = buildInstr(opcode, true, lhs, rhs).defW();                       \
    rv->numBits = rhs.getNumBits();                                            \
    return rv;                                                                 \
  }

  FOR_HW_BIN_OPS(BINOP)

  template <IsAnyHWValue LHS, IsAnyHWValue RHS>
  HWValue buildICmp(LHS lhs, RHS rhs, BigInt::ICmpPred pred) {
    if (lhs.template is<ConstantRef>() && rhs.template is<ConstantRef>()) {
      auto equal = BigInt::icmpOp4S(lhs.template as<ConstantRef>(),
                                    rhs.template as<ConstantRef>(), pred);
      return ConstantRef::fromFourState(equal);
    }
    auto rv = buildInstr(OP_ICMP_EQ.indexAdd(unsigned(pred), OP_ICMP_SGE), true,
                         lhs, rhs)
                  .defW();
    rv->numBits = 1;
    return rv;
  }

  template <IsAnyHWValue T> HWValue buildRedXor(T val) {
    SmallVec<HWValue, 32> bits;
    bits.reserve(*val.getNumBits());
    for (unsigned i = 0; i < *val.getNumBits(); i++) {
      bits.emplace_back(buildSplice(val, 1, i));
    }
    return buildXor(bits);
    // todo: backend support for HW_RED_XOR
    // if (val.template is<ConstantRef>()) {
    //   return ConstantRef::fromFourState(
    //       BigInt::reductionXOROp4S(val.template as<ConstantRef>()));
    // }
    // auto rv = buildInstr(HW_RED_XOR, true, val).defW();
    // rv->numBits = 1;
    // return rv;
  }

  HWValue buildExt(uint32_t newSize, HWValue value, bool sign) {
    if (newSize == value.getNumBits())
      return value;
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() <= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize, sign)
          .get();
    }

    auto ref = buildInstr(sign ? OP_SEXT : OP_ZEXT, true, value);

    assert(ref.defW().getNumBits().value_or(0) < newSize);
    ref.defW()->numBits = newSize;
    return ref.defW();
  }
  HWValue buildExt(uint32_t newSize, HWValue value, DialectOpcode type) {
    if (newSize == value.getNumBits())
      return value;
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() <= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize, type.is(OP_SEXT))
          .get();
    }
    auto ref = buildInstr(type, true, value);
    assert(ref.defW().getNumBits().value_or(0) < newSize);
    ref.defW()->numBits = newSize;
    return ref.defW();
  }

  HWValue buildZExt(uint32_t newSize, HWValue value) {
    return buildExt(newSize, value, false);
  }
  HWValue buildSExt(uint32_t newSize, HWValue value) {
    return buildExt(newSize, value, true);
  }
  HWValue buildTrunc(uint32_t newSize, HWValue value) {
    if (newSize == 0)
      return ConstantRef::zeroBitZero();
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      assert(asConst.getNumBits() >= newSize);
      return ctx.constBuild()
          .val(value.as<ConstantRef>())
          .resize(newSize)
          .get();
    }
    auto wire = value.as<WireRef>();
    if (wire.getNumBits() == newSize)
      return wire;
    else if (wire.getDefI().isOpc(OP_TRUNC)) {
      return buildTrunc(newSize, wire.getDefI().other(0)->as<HWValue>());
    } else if (wire.getDefI().isOpc(OP_ZEXT, OP_SEXT)) {
      auto src = wire.getDefI().other(0)->as<HWValue>();
      if (*src.getNumBits() >= newSize)
        return buildTrunc(newSize, src);
      else
        return buildExt(newSize, src, wire.getDefI().isOpc(OP_SEXT));
    }

    auto ref = buildInstr(OP_TRUNC, true, value);
    assert(ref.defW().getNumBits().value_or(UINT32_MAX) > newSize);
    ref.defW()->numBits = newSize;
    return ref.defW();
  }

  HWValue buildResize(HWValue val, uint32_t newSize, bool sign = false) {
    assert(val.getNumBits());

    if (*val.getNumBits() < newSize)
      return buildExt(newSize, val, sign);
    else if (*val.getNumBits() > newSize)
      return buildTrunc(newSize, val);
    return val;
  }
  HWValue buildUpsize(HWValue val, uint32_t newSize, bool sign = false) {
    assert(val.getNumBits());
    if (*val.getNumBits() < newSize)
      return buildExt(newSize, val, sign);
    assert(*val.getNumBits() == newSize);
    return val;
  }

private:
  Optional<uint32_t> spliceSingleNumBits(HWValue value, BitRange range) {
    if ((range.hasLen() && range.getLen().as<ConstantRef>())) {
      return range.getLen().as<ConstantRef>().getExactVal();
    }
    if (!range.hasLen() && value.getNumBits()) {
      assert(range == BitRange::full());
      return value.getNumBits();
    }
    return nullopt;
  }
  template <typename... Rest>
  std::tuple<Optional<uint32_t>, uint32_t, bool>
  spliceNumBitsOps(HWValue value, BitRange range, Rest... rest) {
    auto lhs = spliceSingleNumBits(value, range);
    auto [rhsB, rhsN, rhsConst] = spliceNumBitsOps(rest...);

    return std::make_tuple((!lhs || !rhsB) ? nullopt : Optional(*lhs + *rhsB),
                           rhsN + (lhs.value_or(1) != 0),
                           rhsConst && lhs && range.addr.is<ConstantRef>() &&
                               value.is<ConstantRef>());
  }
  std::tuple<Optional<uint32_t>, uint32_t, bool> spliceNumBitsOps() {
    return std::make_tuple(0, 0, true);
  }

  Optional<uint32_t> concatSingleNumBits(HWValue value) {
    return value.getNumBits();
  }
  template <typename... Rest>
  std::tuple<Optional<uint32_t>, uint32_t, bool>
  concatNumBitsOps(HWValue value, Rest... rest) {
    auto lhs = concatSingleNumBits(value);
    auto [rhsB, rhsN, rhsConst] = concatNumBitsOps(rest...);

    return std::make_tuple((!lhs || !rhsB) ? nullopt : Optional(*lhs + *rhsB),
                           rhsN + (lhs.value_or(1) != 0),
                           rhsConst && lhs && value.is<ConstantRef>());
  }
  std::tuple<Optional<uint32_t>, uint32_t, bool> concatNumBitsOps() {
    return std::make_tuple(0, 0, true);
  }

  HWValue buildConstSpliceOfConcat(WireRef wire, uint32_t numBits,
                                   uint32_t baseAddr) {
    if (!wire.hasSingleDef() || !wire.getDefI().isOpc(HW_CONCAT))
      return nullref;
    auto concat = wire.getDefI();

    uint32_t bits = 0;
    uint32_t lowerOffs, upperOffs;

    // concat operands are in reverse (big endian) order
    auto it = *concat.other_end() - 1;

    while (bits < baseAddr && it != (concat.other_begin() - 1)) {
      if (!it->as<HWValue>().getNumBits()) [[unlikely]]
        return nullref;
      bits += *it->as<HWValue>().getNumBits();
      --it;
    }
    // bits now >= baseAddr

    auto lower = it;
    lowerOffs = bits;

    while (bits < baseAddr + numBits && it != (concat.other_begin() - 1)) {
      if (!it->as<HWValue>().getNumBits()) [[unlikely]]
        return nullref;
      bits += *it->as<HWValue>().getNumBits();
      --it;
    }
    upperOffs = bits;

    // bits now >= baseAddr + numBits

    auto upper = it;

    if (lower == upper) {
      auto prev = std::next(lower); // inverse iteration
      auto prevOffs = lowerOffs - *prev->as<HWValue>().getNumBits();
      assert(*prev->as<HWValue>().getNumBits() >= numBits);
      return buildSplice(prev->as<HWValue>(), numBits, baseAddr - prevOffs);
    }

    if (lowerOffs == baseAddr && upperOffs - lowerOffs == numBits) {
      if (upper + 1 == lower)
        return lower->as<HWValue>();
      auto concat = buildInstr(HW_CONCAT, true,
                               Range{upper + 1, lower + 1}.as<HWValue>());
      concat.defW()->numBits = numBits;
      return concat.defW();
    }

    return nullref;
  }

public:
  template <typename... Ts>
  HWValue buildSplice(HWValue src, uint32_t numBits, uint32_t baseAddr,
                      Ts... terms) {
    if (sizeof...(terms) == 0 && baseAddr == 0)
      return buildTrunc(numBits, src);

    if (baseAddr >= *src.as<HWValue>().getNumBits()) {
      return ctx.constBuild().undef(numBits).get();
    }

    if constexpr (sizeof...(terms) == 0) {
      if (auto asConst = src.dyn_as<ConstantRef>()) {
        auto cbuild = ctx.constBuild();
        return cbuild.valRange(asConst, baseAddr, numBits).get();
      }

      if (numBits == src.getNumBits() && baseAddr == 0)
        return src;

      if (baseAddr == 0)
        return buildTrunc(numBits, src);

      auto wire = src.as<WireRef>();
      if (auto val = buildConstSpliceOfConcat(wire, numBits, baseAddr))
        return val;

      HWInstrRef instr =
          buildInstr(HW_SPLICE, true, src, ConstantRef::fromU32(baseAddr));
      instr.defW()->numBits = numBits;
      return instr.defW();
    }

    auto instr = buildInstr(HW_SPLICE, true, src,
                            ConstantRef::fromU32(baseAddr), terms...);
    instr.defW()->numBits = numBits;
    return instr.defW();
  }

  HWValue buildSplice(HWValue src, uint32_t numBits, uint32_t baseAddr,
                      IsRange auto terms) {
    if (terms.empty())
      return buildSplice(src, numBits, baseAddr);
    auto len = std::distance(terms.begin(), terms.end());
    auto ib = buildInstrRaw(HW_SPLICE, 1 + 2 + 3 * len);
    auto defW = ctx.getWires().create(numBits);
    ib.addRef(defW).other();
    ib.addRef(src);
    ib.addRef(ConstantRef::fromU32(baseAddr));
    addSpecialRef(ib, terms);
    return defW;
  }

  // template <typename... Ts> HWValue buildSplice(Ts... operands) {
  //   static_assert(sizeof...(operands) == 2);

  //   auto [len, num, isConst] = spliceNumBitsOps(operands...);
  //   if (isConst) {
  //     auto cbuild = ctx.constBuild();
  //     cbuild.val(0, 0);
  //     HWValue last;
  //     (
  //         [&] {
  //           // skip operands with constant zero length range
  //           if constexpr (std::is_same_v<decltype(operands), BitRange>) {
  //             cbuild.concatRangeLHS(
  //                 last.as<ConstantRef>(),
  //                 operands.getAddr().template
  //                 as<ConstantRef>().getExactVal(), operands.getLen().template
  //                 as<ConstantRef>().getExactVal());

  //           } else
  //             last = operands;
  //         }(),
  //         ...);
  //     return cbuild.get();
  //   }

  //   auto instr = HWInstrRef{ctx.getInstrs().create(1 + 3 * num, HW_SPLICE)};
  //   insertInstr(instr);

  //   InstrBuilder build{instr};
  //   build.addRef(ctx.getWires().create(len)).other();

  //   HWValue last;
  //   (
  //       [&] {
  //         // skip operands with constant zero length range
  //         if constexpr (std::is_same_v<decltype(operands), BitRange>) {
  //           if (spliceSingleNumBits(last, operands).value_or(1) == 0)
  //             return;

  //           addSpecialRef(build, last);
  //           addSpecialRef(build, operands);
  //         } else
  //           last = operands;
  //       }(),
  //       ...);

  //   return instr.defW();
  // }

  HWValue buildMultiSplice(ArrayRef<std::pair<HWValue, BitRange>> values) {
    if (std::all_of(values.begin(), values.end(), [](const auto &pair) {
          return pair.first.template is<ConstantRef>() &&
                 pair.second.isConstant();
        })) {
      auto cbuild = ctx.constBuild();
      cbuild.val(0, 0);
      for (size_t i = 0; i < values.size(); i++)
        cbuild.concatRangeLHS(
            values[i].first.as<ConstantRef>(),
            values[i].second.getAddr().as<ConstantRef>().getExactVal(),
            values[i].second.getLen().as<ConstantRef>().getExactVal());

      return cbuild.get();
    }

    if (values.size() == 1) {
      auto val = values.front().first;
      auto range = values.front().second;
      if (range.isConstant()) {
        return buildSplice(val, range.len.as<ConstantRef>().getExactVal(),
                           range.addr.as<ConstantRef>().getExactVal());
      }
      return buildSplice(val, range.len.as<ConstantRef>().getExactVal(), 0,
                         AddressGenTerm{range.addr, 1});
    }

    unsigned numOperands = 0;
    for (auto [value, range] : values) {
      if (auto asConst = range.getLen().dyn_as<ConstantRef>();
          asConst && asConst.valueEquals(0))
        continue;
      numOperands++;
    }

    auto instr = InstrRef{ctx.getInstrs().create(1 + numOperands, HW_CONCAT)};
    insertInstr(instr);

    auto insertSave = insert;
    setInsertPoint(instr);

    InstrBuilder build{instr};

    build.addRef(ctx.getWires().create());
    build.other();

    Optional<uint32_t> numBits = 0;

    for (auto [value, range] : values) {
      if (auto asConst = range.getLen().dyn_as<ConstantRef>();
          asConst && asConst.valueEquals(0))
        continue;

      auto val = value;
      if (!range.isFull()) {
        if (range.isConstant()) {
          val = buildSplice(val, range.len.as<ConstantRef>().getExactVal(),
                            range.addr.as<ConstantRef>().getExactVal());
        } else {
          val = buildSplice(val, range.len.as<ConstantRef>().getExactVal(), 0,
                            AddressGenTerm{range.addr, 1});
        }
      }
      build.addRef(val);

      if (numBits) {
        auto val = spliceSingleNumBits(value, range);
        if (!val)
          numBits = nullopt;
        else
          *numBits += *val;
      }
    }
    insert = insertSave;
    auto rv = HWInstrRef{instr}.defW();
    rv->numBits = numBits;
    return rv;
  }

  template <typename... Ts>
  HWValue buildInsert(HWValue src, HWValue val, uint32_t baseAddr,
                      Ts... terms) {
    if (baseAddr == 0 && sizeof...(terms) == 0) {
      auto rv = buildInstr(HW_INSERT, true, src, val).defW();
      rv->numBits = src.getNumBits();
      return rv;
    }
    auto rv = buildInstr(HW_INSERT, true, src, val,
                         ConstantRef::fromU32(baseAddr), terms...)
                  .defW();
    rv->numBits = src.getNumBits();
    return rv;
  }

  template <IsAnyHWValue... Ts> HWValue buildConcat(Ts... operands) {
    static_assert(sizeof...(operands) > 1);
    auto [len, num, isConst] = concatNumBitsOps(operands...);
    if (isConst) {
      auto cbuild = ctx.constBuild();
      cbuild.val(0, 0);
      ([&] { cbuild.concatLHS(operands.template as<ConstantRef>()); }(), ...);
      return cbuild.get();
    }

    auto instr = HWInstrRef{ctx.getInstrs().create(1 + num, HW_CONCAT)};
    insertInstr(instr);

    InstrBuilder build{instr};
    build.addRef(ctx.getWires().create(len)).other();
    (
        [&] {
          if constexpr (std::is_convertible_v<decltype(operands), HWValue>)
            if (operands.getNumBits() == 0)
              return;
          build.addRef(operands);
        }(),
        ...);

    return instr.defW();
  }

  HWValue buildConcat(ArrayRef<HWValue> values) {
    if (values.size() == 1)
      return values[0];

    if (std::all_of(values.begin(), values.end(),
                    [](const HWValue &val) { return val.is<ConstantRef>(); })) {
      auto cbuild = ctx.constBuild();
      if (values.size() == 0)
        return cbuild.val(0, 0).get();
      for (auto val : values)
        cbuild.concatLHS(val.as<ConstantRef>());

      return cbuild.get();
    }

    auto instr = InstrRef{ctx.getInstrs().create(1 + values.size(), HW_CONCAT)};

    insertInstr(instr);
    InstrBuilder build{instr};

    build.addRef(ctx.getWires().create());
    build.other();

    Optional<uint32_t> numBits = 0;
    for (auto value : values) {
      build.addRef(value);
      if (auto val = value.getNumBits(); val && numBits)
        *numBits = *numBits + *val;
      else
        numBits = nullopt;
    }
    auto rv = HWInstrRef{instr}.defW();
    rv->numBits = numBits;
    return rv;
  }

  HWValue buildRepeat(HWValue value, unsigned count) {
    if (value.is<ConstantRef>()) {
      return ctx.constBuild().val(value.as<ConstantRef>()).repeat(count).get();
    }

    if (count == 1)
      return value;
    if (count == 0)
      return ConstantRef::zeroBitZero();

    auto rv = buildInstr(HW_REPEAT, true, value).defW();

    if (value.getNumBits()) {
      uint32_t out;
      if (__builtin_umul_overflow(*value.getNumBits(), count, &out))
        report_fatal_error("repeat wire length does not fit in unsigned");
      rv->numBits = out;
    }

    return rv;
  }

  HWValue buildNot(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      return ctx.constBuild().val(asConst).bitNOT().get();
    }
    auto wire = value.as<WireRef>();
    if (wire.getDefI().isOpc(OP_NOT))
      return wire.getDefI().other(0)->as<HWValue>();
    auto rv = buildInstr(OP_NOT, true, wire).defW();
    rv->numBits = wire.getNumBits();
    return rv;
  }

  HWValue buildCLOG2(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      auto tmp = asConst - BigInt::fromU64(1, asConst.getNumBits());
      if (auto lz = BigInt::leadingZeros4S(tmp)) {
        return ConstantRef::fromU32(asConst.getNumBits() - *lz);
      } else
        return ConstantRef::undef32();
    }
    auto rv = buildInstr(HW_CLOG2, true, value).defW();
    rv->numBits = 32;
    return rv;
  }

  WireRef buildLoad(RegisterRef reg) {
    HWInstrRef ref;
    ref = buildInstr(HW_LOAD, true, reg);
    ref.defW()->numBits = reg.getNumBits();
    return ref.defW();
  }

  template <typename... Ts>
  WireRef buildLoad(RegisterRef reg, uint32_t numBits, uint32_t baseAddr = 0,
                    Ts... addressGenTerms) {
    HWInstrRef ref;
    if (baseAddr == 0 && sizeof...(addressGenTerms) == 0)
      ref = buildInstr(HW_LOAD, true, reg);
    else
      ref = buildInstr(HW_LOAD, true, reg, ConstantRef::fromU32(baseAddr),
                       addressGenTerms...);
    ref.defW()->numBits = numBits;
    return ref.defW();
  }

  template <typename... Ts>
  HWInstrRef buildStore(RegisterRef reg, HWValue value, bool defer = false,
                        TriggerIRef trigger = nullref, uint32_t baseAddr = 0,
                        Ts... addressGenTerms) {
    assert(!(!defer && trigger) && "trigger on non-deferred store");
    auto opc = defer ? HW_STORE_DEFER : HW_STORE;
    if (sizeof...(addressGenTerms) > 0 || baseAddr != 0) {
      if (trigger) {
        return buildInstr(opc, false, value, reg, trigger.oref(),
                          ConstantRef::fromU32(baseAddr), addressGenTerms...);
      }
      return buildInstr(opc, false, value, reg, ConstantRef::fromU32(baseAddr),
                        addressGenTerms...);
    } else {
      if (trigger) {
        return buildInstr(opc, false, value, reg, trigger.oref());
      }
      return buildInstr(opc, false, value, reg);
    }
  }

  RegisterRef buildRegister(Optional<uint32_t> bitSize = nullopt) {
    auto regRef = RegisterRef{ctx.getRegs().create(bitSize)};
    auto regInstr = InstrRef{ctx.getInstrs().create(1, HW_REGISTER_DEF)};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);
    return regRef;
  }

  RegisterRef buildPort(ModuleIRef module, HWOpcode opcode,
                        Optional<uint32_t> bitSize = nullopt) {
    auto regRef = RegisterRef{ctx.getRegs().create(bitSize)};
    auto regInstr = InstrRef{ctx.getInstrs().create(1, opcode)};
    InstrBuilder{regInstr}.addRef(regRef);
    insertInstr(regInstr);

    module.mod()->ports.emplace_back(Module::Port{regRef, opcode});
    return regRef;
  }

  RegisterRef buildInputPort(ModuleIRef module) {
    return buildPort(module, HW_INPUT_REGISTER_DEF);
  }
  RegisterRef buildOutputPort(ModuleIRef module) {
    return buildPort(module, HW_OUTPUT_REGISTER_DEF);
  }
  RegisterRef buildInoutPort(ModuleIRef module) {
    return buildPort(module, HW_INOUT_REGISTER_DEF);
  }
  RegisterRef buildRefPort(ModuleIRef module) {
    return buildPort(module, HW_REF_REGISTER_DEF);
  }

  ProcessIRef buildProcess(HWOpcode type = HW_COMB_PROCESS_DEF,
                           TriggerIRef trigger = nullref) {
    assert(type == HW_INIT_PROCESS_DEF || type == HW_COMB_PROCESS_DEF ||
           type == HW_SEQ_PROCESS_DEF || type == HW_FINAL_PROCESS_DEF ||
           type == HW_LATCH_PROCESS_DEF || type == HW_NETLIST_PROCESS_DEF);
    auto procRef = ctx.getProcs().create();
    auto procInstRef =
        ProcessIRef{ctx.getInstrs().create(2 + (trigger != nullref), type)};
    InstrBuilder build{procInstRef};
    build.addRef(procRef).addRef(ctx.createBlock()).other();

    if (trigger)
      build.addRef(trigger.oref());

    insertInstr(procInstRef);
    return procInstRef;
  }

  // HWInstrRef buildEventDelay(RegisterRef dReg, RegisterRef qReg,
  //                            const SensList &sens) {
  //   auto instrRef = ProcessIRef{
  //       ctx.getInstrs().create(3 + sens.signals.size(), HW_TRIGGER_DEF)};
  //   insertInstr(instrRef);
  //   InstrBuilder build{instrRef};
  //   build.other().addRef(dReg).addRef(qReg).addRef(
  //       SensModesRef::fromSensList(sens));

  //   for (size_t i = 0; i < sens.signals.size(); i++) {
  //     build.addRef(sens.signals[i].first);
  //   }

  //   return instrRef;
  // }

  TriggerIRef buildTrigger(const SensList &list) {
    if (list.signals.empty())
      return nullref;

    auto instrRef = TriggerIRef{
        ctx.getInstrs().create(1 + list.signals.size(), HW_TRIGGER_DEF)};
    insertInstr(instrRef);
    TriggerRef trigRef = TriggerRef{ctx.getTriggers().create()};

    InstrBuilder build{instrRef};
    build.addRef(trigRef).other();

    for (auto [reg, mode] : list.signals) {
      build.addRef(reg);
      trigRef->addMode(mode);
    }

    return instrRef;
  }

  HWInstrRef buildInstance(ModuleRef module, ArrayRef<RegisterRef> portRegs) {
    auto instr =
        InstrRef{ctx.getInstrs().create(1 + portRegs.size(), HW_INSTANCE)};

    assert(portRegs.size() == module->ports.size());

    InstrBuilder build{instr};
    build.other();
    build.addRef(module);

    for (size_t i = 0; i < portRegs.size(); i++)
      build.addRef(portRegs[i]);

    insertInstr(instr);

    return instr;
  }

  IfInstrRef buildIfElse(HWValue cond, unsigned yieldPrealloc = 0) {
    assert(cond.getNumBits() == 1);
    InstrRef instrRef =
        InstrRef{ctx.getInstrs().create(3 + yieldPrealloc, OP_IF)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    auto falseBl = ctx.createBlock();
    build.addRef(trueBl).addRef(falseBl);

    for (unsigned i = 0; i < yieldPrealloc; i++)
      build.addRef(ctx.getWires().create());

    build.other().addRef(cond);

    return IfInstrRef{instrRef};
  }

  IfInstrRef buildIf(HWValue cond) {
    assert(cond.getNumBits() == 1);
    InstrRef instrRef = InstrRef{ctx.getInstrs().create(2, OP_IF)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    auto trueBl = ctx.createBlock();
    build.addRef(trueBl);
    build.other().addRef(cond);
    return IfInstrRef{instrRef};
  }

  IfInstrRef buildIfElse(IfInstrRef old, unsigned yieldPrealloc = 0,
                         BlockRef falseBlock = nullref) {
    InstrRef instrRef =
        InstrRef{ctx.getInstrs().create(3 + yieldPrealloc, OP_IF)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};

    build.addRef(old.getTrueBlock())
        .addRef(old.hasFalseBlock() ? old.getFalseBlock()
                                    : (falseBlock ?: ctx.createBlock()));

    old.operand(0).replace(FatDynObjRef<>{nullref});
    if (old.hasFalseBlock())
      old.operand(1).replace(FatDynObjRef<>{nullref});

    for (unsigned i = 0; i < yieldPrealloc; i++) {
      if (i >= old.getNumYieldValues())
        build.addRef(ctx.getWires().create());
      else {
        auto operand = old.getYieldValue(i);
        build.addRef(operand->as<FatDynObjRef<>>());
        operand.replace(FatDynObjRef<>{nullref});
      }
    }

    build.other().addRef(old.getCondValue()->as<FatDynObjRef<>>());
    return IfInstrRef{instrRef};
  }

  SwitchInstrRef buildSwitch(HWValue cond, unsigned yieldPrealloc = 0) {
    SwitchInstrRef instrRef =
        SwitchInstrRef{ctx.getInstrs().create(2 + yieldPrealloc, OP_SWITCH)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    build.other();
    build.addRef(cond);
    return instrRef;
  }

  CaseInstrRef buildCase(ArrayRef<HWValue> conds,
                         DialectOpcode type = OP_CASE) {
    CaseInstrRef instrRef =
        CaseInstrRef{ctx.getInstrs().create(1 + conds.size(), type)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock()).other();
    for (auto cond : conds)
      build.addRef(cond);
    return instrRef;
  }
  CaseInstrRef buildDefaultCase() {
    CaseInstrRef instrRef =
        CaseInstrRef{ctx.getInstrs().create(1, OP_CASE_DEFAULT)};
    insertInstr(instrRef);
    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    return instrRef;
  }

  template <IsAnyHWValue... Ts> auto buildYield(Ts... value) {
    // todo: ideally this would dynamically add Wires to the associated
    // IfInstrRef. Alternative is a GET_YIELD instr or something. For now
    // naive implementation as reference, just delete old instr and rebuild

    auto instr = insert.blockRef().defI();
    assert(instr.isOpc(OP_IF, OP_WHILE, OP_DO_WHILE, OP_FOR, OP_CASE, HW_CASE_X,
                       HW_CASE_Z, OP_CASE_DEFAULT));

    switch (instr.getDialectOpcode().raw()) {
    case OP_IF.raw(): {
      auto asIf = IfInstrRef{instr};
      if (sizeof...(value) > asIf.getNumYieldValues()) {

        auto newInstr =
            InstrRef{ctx.getInstrs().create(sizeof...(value) + 3, OP_IF)};

        InstrBuilder build{newInstr};

        // copy over true/false blocks and existing defs
        for (unsigned i = 0; i < instr.getNumDefs(); i++)
          build.addRef(instr.operand(i)->as<FatDynObjRef<>>());

        // new wire defs
        for (unsigned i = 0; i < sizeof...(value) - asIf.getNumYieldValues();
             i++)
          build.addRef(ctx.getWires().create());
        build.other();

        // condition
        build.addRef(instr.operand(instr.getNumDefs())->as<FatDynObjRef<>>());

        HWInstrRef{instr}.iter(ctx).replace(newInstr);
        instr->destroyOperands();
        ctx.getInstrs().destroy(instr);
        instr = newInstr;
        asIf = newInstr;
      }
      size_t idx = 0;
      for (auto val : {value...}) {
        WireRef yieldVal = asIf.getYieldValue(idx)->as<WireRef>();
        if (val.getNumBits()) {
          if (!yieldVal->numBits)
            yieldVal->numBits = val.getNumBits();
          assert(yieldVal->numBits == val.getNumBits());
        }
        idx++;
      }
      break;
    }
    case OP_WHILE.raw(): {
      WhileInstrRef asWhile{instr};
      // while yield has continue as another arg.
      if (sizeof...(Ts) == asWhile.getNumYieldValues() + 1)
        ;
      else {
        abort();
      }
      break;
    }
    case OP_DO_WHILE.raw(): {
      DoWhileInstrRef asDoWhile{instr};
      if (sizeof...(Ts) == asDoWhile.getNumYieldValues() + 1)
        ;
      else {
        abort();
      }
      break;
    }
    default:
      dyno_unreachable("undefined");
    }

    auto yieldInstr = buildInstr(OP_YIELD, false, value...);
    return std::make_pair(yieldInstr, instr);
  }

  auto buildYield(InstrRef old, ArrayRef<HWValue> addedYieldVals) {
    assert(!old || old.isOpc(OP_YIELD));

    unsigned newYieldVals =
        addedYieldVals.size() + (old ? old.getNumOperands() : 0);

    auto instr = InstrRef{ctx.getInstrs().create(newYieldVals, OP_YIELD)};
    insertInstr(instr);
    InstrBuilder ibuild{instr};
    ibuild.other();

    if (old)
      for (auto operand : old)
        ibuild.addRef(operand->as<FatDynObjRef<>>());
    for (auto val : addedYieldVals)
      ibuild.addRef(val);

    return instr;
  }

  auto extendOrNewYield(BlockRef block, ArrayRef<HWValue> addedYieldVals) {
    InstrRef lastYield = nullref;
    if (!block.empty() && block.end().pred()->isOpc(OP_YIELD))
      lastYield = block.end().pred().instr();
    setInsertPoint(block.end());
    return std::make_pair(lastYield, buildYield(lastYield, addedYieldVals));
  }

  auto buildUnyield(InstrRef old, ArrayRef<WireRef> addedYieldVals) {
    assert(!old || old.isOpc(OP_UNYIELD));

    unsigned newYieldVals =
        addedYieldVals.size() + (old ? old.getNumOperands() : 0);

    auto instr = InstrRef{ctx.getInstrs().create(newYieldVals, OP_UNYIELD)};
    insertInstr(instr);
    InstrBuilder ibuild{instr};

    if (old)
      for (auto def : old) {
        ibuild.addRef(def->as<FatDynObjRef<>>());
        def.replace(FatDynObjRef<>{nullref});
      }
    for (auto wire : addedYieldVals)
      ibuild.addRef(wire);

    return instr;
  }

  auto extendOrNewUnyield(BlockRef block, ArrayRef<WireRef> addedYieldVals) {
    InstrRef lastYield = nullref;
    if (!block.empty() && block.begin()->isOpc(OP_UNYIELD))
      lastYield = block.begin().instr();
    setInsertPoint(block.begin());
    return std::make_pair(lastYield, buildUnyield(lastYield, addedYieldVals));
  }

  InstrRef addOperands(InstrRef old, ArrayRef<WireRef> newDefs,
                       ArrayRef<HWValue> newUses) {
    setInsertPoint(ctx.getCFG()[old]);
    auto newInstr = InstrRef{ctx.getInstrs().create(
        old.getNumOperands() + newDefs.size() + newUses.size(),
        old.getDialectOpcode())};
    insertInstr(newInstr);
    // todo: steal slots in defUse rather than shuffling around.
    InstrBuilder ibuild{newInstr};
    for (auto def : old.defs()) {
      ibuild.addRef(def->as<FatDynObjRef<>>());
      def.replace(FatDynObjRef<>(nullref));
    }
    for (auto def : newDefs) {
      ibuild.addRef(def);
    }
    ibuild.other();
    for (auto use : old.others()) {
      ibuild.addRef(use->as<FatDynObjRef<>>());
      use.replace(FatDynObjRef<>(nullref));
    }
    for (auto use : newUses) {
      ibuild.addRef(use);
    }
    return newInstr;
  }

  template <typename... Ts> auto buildWhile(Ts... inputs) {
    InstrRef instrRef =
        InstrRef{ctx.getInstrs().create(2 + 2 * sizeof...(inputs), OP_WHILE)};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());
    build.addRef(ctx.createBlock());

    for (unsigned i = 0; i < sizeof...(inputs); i++)
      build.addRef(ctx.getWires().create());
    build.other();
    ([&]() { build.addRef(inputs); }(), ...);
    return WhileInstrRef{instrRef};
  }

  template <typename... Ts> auto buildDoWhile(Ts... inputs) {
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(1 + 2 * sizeof...(inputs), OP_DO_WHILE)};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(ctx.createBlock());

    for (unsigned i = 0; i < sizeof...(inputs); i++)
      build.addRef(ctx.getWires().create());
    build.other();

    ([&]() { build.addRef(inputs); }(), ...);
    return DoWhileInstrRef{instrRef};
  }

  auto buildAssert(HWValue value, TriggerIRef deferTrigger = nullref) {
    if (deferTrigger)
      return buildInstr(HW_ASSERT_DEFER, false, value, deferTrigger.oref());
    return buildInstr(OP_ASSERT, false, value);
  }

  auto buildFuncParam(Optional<uint32_t> numBits = nullopt) {
    auto reg = ctx.getRegs().create(numBits);
    auto instr = InstrRef{ctx.getInstrs().create(1, OP_PARAM)};
    insertInstr(instr);
    InstrBuilder build{instr};
    build.addRef(reg);

    auto func = insert.blockRef().defI().as<FunctionIRef>();
    func.func()->params.emplace_back(instr);
    return reg;
  }

  template <typename... Ts> auto buildFuncReturn(Ts... retvals) {
    auto instr = buildInstr(OP_RETURN, false, retvals...);
    return instr;
  }

  auto buildFunc() {
    auto funcRef = FunctionRef{ctx.getFuncs().create()};
    auto funcInstr = FunctionIRef{ctx.getInstrs().create(2, OP_FUNCTION_DEF)};
    insertInstr(funcInstr);

    InstrBuilder{funcInstr}.addRef(funcRef).addRef(ctx.createBlock());
    return funcInstr;
  }

  auto buildCall(FunctionIRef func, ArrayRef<HWValueOrReg> args,
                 unsigned numRetvals = 0) {
    auto callInstr = CallInstrRef{
        ctx.getInstrs().create(numRetvals + 1 + args.size(), OP_CALL)};
    // assert(args.size() == func.func()->params.size() && "param size
    // mismatch");
    insertInstr(callInstr);

    InstrBuilder build{callInstr};
    for (unsigned i = 0; i < numRetvals; i++)
      build.addRef(ctx.getWires().create());
    build.other();
    build.addRef(func.func());
    for (size_t i = 0; i < args.size(); i++)
      build.addRef(args[i]);

    return callInstr;
  }

  // todo: full constant support
  ConstantRef buildConst(unsigned bits, uint64_t value) {
    return ConstantBuilder{ctx.getConstants()}.val(bits, value);
  }

  HWValue buildMux(HWValue sel, HWValue trueV, HWValue falseV) {
    assert(sel.getNumBits() == 1);
    assert(trueV.getNumBits() == falseV.getNumBits());
    auto rv = buildInstr(HW_MUX, true, sel, trueV, falseV).defW();
    rv->numBits = trueV.getNumBits();
    return rv;
  }

  void destroyObj(FatDynObjRef<> obj) {
    if (obj == nullref)
      return;

    if (Operand::isDefUseOperand(obj)) {
      reinterpret_cast<InstrDefUse *>(obj.getPtr())
          ->replaceAllUsesWith(nullref);
    }

    switch (*obj.getType()) {
    case *CORE_INSTR: {
      destroyInstr(obj.as<InstrRef>());
      break;
    }
    case *CORE_BLOCK: {
      destroyBlock(obj.as<BlockRef>());
      break;
    }
    case *OP_FUNC: {
      ctx.getFuncs().destroy(obj.as<FunctionRef>());
      break;
    }
    case *HW_REGISTER: {
      ctx.getRegs().destroy(obj.as<RegisterRef>());
      break;
    }
    case *HW_WIRE: {
      ctx.getWires().destroy(obj.as<WireRef>());
      break;
    }
    case *HW_PROCESS: {
      ctx.getProcs().destroy(obj.as<ProcessRef>());
      break;
    }
    case *HW_TRIGGER: {
      ctx.getTriggers().destroy(obj.as<TriggerRef>());
      break;
    }
    case *HW_MODULE: {
      ctx.getModules().destroy(obj.as<ModuleRef>());
      break;
    }
    default:
      dyno_unreachable("deleting unknown object");
    }
  }
  void destroyInstr(InstrRef instr) {
    for (auto oref : instr.defs()) {
      auto obj = oref->fat();
      destroyObj(obj);
    }

    if (ctx.getCFG().contains(instr))
      ctx.getCFG()[instr].erase();
    instr->destroyOthers();
    ctx.getInstrs().destroy(instr);
  }
  void destroyBlock(BlockRef block) {
    SmallVec<InstrRef, 16> toDestroy{block.size()};
    for (auto [i, instr] : Range{block}.reverse().enumerate())
      toDestroy[i] = instr;

    for (auto instr : toDestroy)
      destroyInstr(instr);

    ctx.getCFG().blocks.destroy(block);
  }

  void setInsertPoint(BlockRef_iterator<true> it) { insert = it; }
  void setInsertPoint(InstrRef ref) { insert = ctx.getCFG()[ref]; }
}; // namespace dyno

class HWInstrBuilderStack : public HWInstrBuilder {
  SmallVec<BlockRef_iterator<true>, 16> stack;

public:
  using HWInstrBuilder::HWInstrBuilder;
  template <any_of<BlockRef_iterator_base, InstrRef> T>
  void pushInsertPoint(T newInsertPoint) {
    stack.emplace_back(insert);
    setInsertPoint(newInsertPoint);
  }
  void popInsertPoint() { setInsertPoint(stack.pop_back_val()); }
};
}; // namespace dyno

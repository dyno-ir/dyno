
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Module.h"
#include "hw/Register.h"
#include "slang/ast/ASTVisitor.h"
#include "slang/ast/Compilation.h"
#include "slang/ast/Expression.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/expressions/AssignmentExpressions.h"
#include "slang/ast/expressions/ConversionExpression.h"
#include "slang/ast/expressions/LiteralExpressions.h"
#include "slang/ast/expressions/MiscExpressions.h"
#include "slang/ast/expressions/Operator.h"
#include "slang/ast/expressions/OperatorExpressions.h"
#include "slang/ast/expressions/SelectExpressions.h"
#include "slang/ast/symbols/BlockSymbols.h"
#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/PortSymbols.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/diagnostics/DiagnosticEngine.h"
#include "slang/diagnostics/Diagnostics.h"
#include "slang/driver/Driver.h"
#include "slang/numeric/SVInt.h"
#include "support/Bits.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <iostream>
#include <ostream>
#include <ranges>
#include <slang/syntax/SyntaxNode.h>
#include <type_traits>

using namespace dyno;

class VisitorAST : public slang::ast::ASTVisitor<VisitorAST, true, true> {
public:
  HWContext &ctx;
  ModuleIRef mod;
  ProcessIRef proc;
  // BlockRef blockRef;
  HWInstrBuilderStack build;

  DenseMap<const slang::ast::Symbol *, RegisterRef> vars;
  DenseMap<const slang::ast::InstanceBodySymbol *, ObjRef<Module>> moduleMap;

  VisitorAST(HWContext &ctx) : ctx(ctx), build(ctx) {}

  struct Value : public RTTIUtilMixin<Value> {
    const slang::ast::Type *type;
    enum ValueKind : uint8_t {
      VK_R,
      VK_L,
      VK_CCL,
    };

    ValueKind kind;
    bool isLValue() { return kind != VK_R; }

  protected:
    Value(ValueKind kind, const slang::ast::Type *type)
        : type(type), kind(kind) {}

  public:
    Value() : kind(VK_R) {}

    HWValue getValue() {
      assert(!isLValue());
      return (*this).as<RValue>().value;
    }
    HWValue proGetValue(HWInstrBuilder &build) {

      switch (kind) {
      case VK_R:
        return getValue();
      case VK_L: {
        auto &asLVal = this->as<RegLValue>();
        auto ldVal = build.buildLoad(asLVal.lvReg, asLVal.lvBitRange);
        if (ldVal->numBits < type->getBitstreamWidth())
          return build.buildExt(type->getBitstreamWidth(), ldVal,
                                type->isSigned());
        return ldVal;
      }

      case VK_CCL:
        abort();
      }
    }

    virtual ~Value() = default;

    // RegisterRef getLVReg() {
    //   assert(kind == VK_L);
    //   return lvReg;
    // }
    // BitRange getLVBitRange() {
    //   assert(kind == VK_L);
    //   return lvBitRange;
    // }
    // static Value rvalue(HWValue value, const slang::ast::Type *type) {
    //   return Value{value, type};
    // }
    // static Value lvalue(RegisterRef reg, const slang::ast::Type *type,
    //                     BitRange range = BitRange::full()) {
    //   return Value{reg, type, range};
    // }
  };

  struct RValue : public Value {
    HWValue value;

    RValue(HWValue value, const slang::ast::Type *type)
        : Value(VK_R, type), value(value) {}

    static bool is_impl(const Value &v) { return v.kind == VK_R; }
  };

  struct LValue : public Value {
    static bool is_impl(const Value &v) {
      return v.kind == VK_L || v.kind == VK_CCL;
    }

  protected:
    LValue(ValueKind kind, const slang::ast::Type *type) : Value(kind, type) {
      assert(is_impl(*this));
    }
  };

  struct RegLValue : public LValue {
    RegisterRef lvReg;
    BitRange lvBitRange;

    RegLValue(RegisterRef reg, const slang::ast::Type *type,
              BitRange range = BitRange::full())
        : LValue(VK_L, type), lvReg(reg), lvBitRange(range) {}

    static bool is_impl(const Value &v) { return v.kind == VK_L; }

    static std::unique_ptr<Value> slice(HWInstrBuilder &build, RegLValue lv,
                                        const slang::ast::Type *type,
                                        BitRange range) {
      if (lv.lvBitRange == BitRange::full())
        return std::make_unique<RegLValue>(lv.lvReg, type, range);

      // fixme: can you ever sign extend here?
      auto newAddr =
          build.buildAdd(build.buildUpsize(lv.lvBitRange.getAddr(), 32),
                         build.buildUpsize(range.getAddr(), 32));
      auto newLen = range.getLen();

      return std::make_unique<RegLValue>(lv.lvReg, type,
                                         BitRange{newAddr, newLen});
    }
  };
  struct ConcatLValue : public LValue {
    SmallVec<std::pair<RegisterRef, BitRange>, 4> lvValues;

    ConcatLValue(std::span<std::unique_ptr<Value>> lvals,
                 const slang::ast::Type *type)
        : LValue(VK_CCL, type), lvValues() {
      lvValues.reserve(lvals.size());
      for (auto &val : lvals) {
        if (auto asLV = val->dyn_as<RegLValue>())
          lvValues.emplace_back(std::make_pair(asLV->lvReg, asLV->lvBitRange));
        else if (auto asCCLV = val->dyn_as<ConcatLValue>()) {
          lvValues.push_back_range(Range{asCCLV->lvValues});
        } else
          dyno_unreachable("expected lvalue");
      }
    }

    static bool is_impl(const Value &v) { return v.kind == VK_CCL; }
  };

  void handle(const slang::ast::InstanceSymbol &node) {
    assert(node.isModule() && "only module supported rn");
    auto &body = *(node.getCanonicalBody() ?: &node.body);
    auto [found, it] = moduleMap.findOrInsert(&body, [&] {
      return ctx.createModule(node.name).def()->as<ObjRef<Module>>();
    });

    if (found)
      return;

    ModuleIRef module =
        ModuleRef{it.val(), ctx.getModules()[it.val()]}.getSingleDef()->instr();

    for (const auto &port : body.getPortList()) {
      auto &ps = port->as<slang::ast::PortSymbol>();
      OpcodeID ptype;
      switch (ps.direction) {
      case slang::ast::ArgumentDirection::In:
        ptype = OpcodeID{HW_INPUT_REGISTER_INSTR};
        break;
      case slang::ast::ArgumentDirection::Out:
        ptype = OpcodeID{HW_OUTPUT_REGISTER_INSTR};
        break;
      case slang::ast::ArgumentDirection::InOut:
        ptype = OpcodeID{HW_INOUT_REGISTER_INSTR};
        break;
      case slang::ast::ArgumentDirection::Ref:
        ptype = OpcodeID{HW_REF_REGISTER_INSTR};
        break;
      }
      build.setInsertPoint(module.block().end());
      auto reg = build.buildPort(module, ptype);
      if (auto width = ps.getType().getBitstreamWidth())
        reg->numBits = width;
    }

    visitDefault(node);
  }

  void handle_modules() {
    // can probably do this in parallel
    for (auto [slangMod, dynoMod] : moduleMap) {
      ModuleRef fDynoMod{dynoMod, ctx.getModules()[dynoMod]};
      mod = fDynoMod.getSingleDef()->instr();
      vars.clear();

      for (auto [i, port] :
           slangMod->getPortList() | std::ranges::views::enumerate) {
        vars.insert(port->as<slang::ast::PortSymbol>().internalSymbol,
                    fDynoMod->ports[i]);
        // std::cout << "inserted " << port << ", " << port->name << "\n";
      }
      build.setInsertPoint(mod.block().end());
      handle_member_list(slangMod->Scope::members());
    }
  }

  void handle_member_list(
      std::ranges::subrange<slang::ast::Scope::iterator> members) {
    for (auto &member : members) {
      switch (member.kind) {
      case slang::ast::SymbolKind::Port:
        // std::cout << "port " << &member << ", "
        //           << member.as<slang::ast::PortSymbol>().name << "\n";
        break;
      case slang::ast::SymbolKind::Parameter:
        break;
      case slang::ast::SymbolKind::Variable:
        // std::cout << "var " << &member << ", "
        //           << member.as<slang::ast::VariableSymbol>().name << "\n";
        break;

      case slang::ast::SymbolKind::ProceduralBlock: {
        handle_proc(member.as<slang::ast::ProceduralBlockSymbol>());
        break;
      }

      case slang::ast::SymbolKind::Instance: {
        auto &asInst = member.as<slang::ast::InstanceSymbol>();
        auto &body = *(asInst.getCanonicalBody() ?: &asInst.body);
        auto it = moduleMap.find(&body);
        if (!it)
          abort();

        auto instance = build.buildInstance(
            ModuleRef{it.val(), ctx.getModules()[it.val()]});

        for (auto [i, conn] :
             asInst.getPortConnections() | std::ranges::views::enumerate) {

          auto proc = build.buildProcess();
          build.pushInsertPoint(proc.block().end());

          std::unique_ptr<Value> val;

          if (auto asAssign = conn->getExpression()
                                  ->as_if<slang::ast::AssignmentExpression>()) {
            assert(asAssign->right().kind ==
                   slang::ast::ExpressionKind::EmptyArgument);
            val = handle_expr(asAssign->left());
          } else
            val = handle_expr(*conn->getExpression());

          auto ireg = instance.other(i + 1)->as<RegisterRef>();
          ireg->numBits = conn->port.as<slang::ast::PortSymbol>()
                              .getType()
                              .getBitstreamWidth();
          build.buildStore(ireg, val->proGetValue(build));
          build.popInsertPoint();
        }
        break;
      }

      case slang::ast::SymbolKind::GenerateBlockArray: {
        // todo
        auto &asGenBA = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (auto submember : asGenBA.entries) {
          std::cout << submember->name << "\n";
          handle_member_list(submember->members());
        }
        // asGenBA.entries
        std::cout << asGenBA.name << "\n";
        break;
      }

      default:
        abort();
      }
    }
  }

  void handle_proc(const slang::ast::ProceduralBlockSymbol &block) {
    assert(mod);
    proc = build.buildProcess();
    build.pushInsertPoint(proc.block().end());
    handle_stmt(block.getBody());
    build.popInsertPoint();
  }

  void handle_stmt(const slang::ast::Statement &stmt) {

    switch (stmt.kind) {
    case slang::ast::StatementKind::Empty:
      break;
    case slang::ast::StatementKind::Block: {
      // todo: var scope
      handle_stmt(stmt.as<slang::ast::BlockStatement>().body);
      break;
    }
    case slang::ast::StatementKind::ExpressionStatement: {
      const auto &expr_s = stmt.as<slang::ast::ExpressionStatement>();
      handle_expr(expr_s.expr);
      break;
    }
    case slang::ast::StatementKind::Timed: {
      auto &timed_s = stmt.as<slang::ast::TimedStatement>();
      // todo: ?
      handle_stmt(timed_s.stmt);
      break;
    }
    case slang::ast::StatementKind::List: {
      const auto &list = stmt.as<slang::ast::StatementList>();
      for (const auto &l_stmt : list.list)
        handle_stmt(*l_stmt);
      break;
    }

    case slang::ast::StatementKind::Invalid:
    case slang::ast::StatementKind::VariableDeclaration:
    case slang::ast::StatementKind::Return:
    case slang::ast::StatementKind::Continue:
    case slang::ast::StatementKind::Break:
    case slang::ast::StatementKind::Disable:
    case slang::ast::StatementKind::Conditional:
    case slang::ast::StatementKind::Case:
    case slang::ast::StatementKind::PatternCase:
    case slang::ast::StatementKind::ForLoop:
    case slang::ast::StatementKind::RepeatLoop:
    case slang::ast::StatementKind::ForeachLoop:
    case slang::ast::StatementKind::WhileLoop:
    case slang::ast::StatementKind::DoWhileLoop:
    case slang::ast::StatementKind::ForeverLoop:
    case slang::ast::StatementKind::ImmediateAssertion:
    case slang::ast::StatementKind::ConcurrentAssertion:
    case slang::ast::StatementKind::DisableFork:
    case slang::ast::StatementKind::Wait:
    case slang::ast::StatementKind::WaitFork:
    case slang::ast::StatementKind::WaitOrder:
    case slang::ast::StatementKind::EventTrigger:
    case slang::ast::StatementKind::ProceduralAssign:
    case slang::ast::StatementKind::ProceduralDeassign:
    case slang::ast::StatementKind::RandCase:
    case slang::ast::StatementKind::RandSequence:
    case slang::ast::StatementKind::ProceduralChecker:
      break;
    }
  }

  ConstantRef toDynoConstant(const slang::SVInt &svint) {
    ConstantRef ref;
    std::span<const uint32_t> data{
        reinterpret_cast<const uint32_t *>(svint.getRawPtr()),
        2 * svint.getNumWords()};
    if (!svint.hasUnknown()) {
      ref = ConstantBuilder{ctx.getConstants()}.raw(
          (unsigned)svint.getBitWidth(), data);
    } else {
      SmallVec<uint32_t, 16> buf{round_up_div(2 * svint.getBitWidth(), 32U)};

      std::span<const uint16_t> data16{
          reinterpret_cast<const uint16_t *>(data.data()), data.size() * 2};
      for (size_t i = 0; i < buf.size(); i++) {
        buf[i] = (unpack_bits(data16[i + data16.size() / 2]) << 1) |
                 unpack_bits(data16[i]);
        // flip x and z
        uint32_t mask = (buf[i] & repeatBits(0b10U, 2)) >> 1;
        buf[i] ^= mask;
      }

      ref = ConstantBuilder{ctx.getConstants()}.raw(2 * svint.getBitWidth(),
                                                    std::move(buf), 0, 1);
    }
    return ref;
  }

  std::unique_ptr<Value> handle_expr(const slang::ast::Expression &expr) {

    switch (expr.kind) {
    case slang::ast::ExpressionKind::Assignment: {
      const auto &assign = expr.as<slang::ast::AssignmentExpression>();

      // todo: assign.isBlocking()

      auto rhs = handle_expr(assign.right());
      auto lhs = handle_expr(assign.left());

      if (!lhs->is<LValue>())
        abort();

      build.buildStore(lhs->as<RegLValue>().lvReg, rhs->proGetValue(build),
                       lhs->as<RegLValue>().lvBitRange);
      return rhs;
    }
    case slang::ast::ExpressionKind::BinaryOp: {
      const auto &binop = expr.as<slang::ast::BinaryExpression>();
      // std::cout << binop.kind << "\n";

      switch (binop.op) {
      case slang::ast::BinaryOperator::LogicalOr:
      case slang::ast::BinaryOperator::LogicalImplication:
      case slang::ast::BinaryOperator::LogicalAnd: {
        auto lhs = handle_expr(binop.left());
        auto lhsVal = lhs->proGetValue(build);

        auto zero = ctx.constBuild().zeroLike(lhsVal).get();

        auto lhsBool =
            build.buildICmp(lhsVal, zero,
                            binop.op == slang::ast::BinaryOperator::LogicalOr
                                ? BigInt::ICMP_EQ
                                : BigInt::ICMP_NE);

        auto ifElse = build.buildIfElse(lhsBool);
        build.pushInsertPoint(ifElse.getTrueBlock().end());

        auto rhs = handle_expr(binop.right());
        auto rhsVal = rhs->proGetValue(build);

        auto rhsBool = build.buildICmp(rhsVal, zero, BigInt::ICMP_NE);
        ifElse = build.buildYield(rhsBool).second;

        build.popInsertPoint();
        build.pushInsertPoint(ifElse.getFalseBlock().end());
        ifElse = build
                     .buildYield(ConstantRef::fromBool(
                         binop.op != slang::ast::BinaryOperator::LogicalAnd))
                     .second;
        build.popInsertPoint();

        return std::make_unique<RValue>(ifElse.getYieldValue(0).as<WireRef>(),
                                        expr.type);
      };
      default:
        break;
      }

      auto lhs = handle_expr(binop.left());
      auto rhs = handle_expr(binop.right());

      HWValue val;

      auto lhsVal = lhs->proGetValue(build);
      auto rhsVal = rhs->proGetValue(build);

      // for some reason Slang sometimes omits implicit conversions (?)
      // maybe just for self-determined operands?
      if (auto width = expr.type->getBitstreamWidth(); width && width != 1) {
        lhsVal = build.buildUpsize(lhsVal, width, lhs->type->isSigned());
        rhsVal = build.buildUpsize(rhsVal, width, rhs->type->isSigned());
      }

      switch (binop.op) {
      case slang::ast::BinaryOperator::Add:
        val = build.buildAdd(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Subtract:
        val = build.buildSub(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Multiply:
        val = build.buildMul(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Divide:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildSDiv(lhsVal, rhsVal);
        else
          val = build.buildUDiv(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Mod:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildSMod(lhsVal, rhsVal);
        else
          val = build.buildUMod(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftLeft:
      case slang::ast::BinaryOperator::LogicalShiftLeft:
        val = build.buildSLL(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftRight:
        val = build.buildSRA(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::LogicalShiftRight:
        val = build.buildSRL(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryAnd:
        val = build.buildAnd(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryOr:
        val = build.buildOr(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryXor:
        val = build.buildXor(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::BinaryXnor:
        val = build.buildXNor(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Equality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_EQ);
        break;
      case slang::ast::BinaryOperator::Inequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_NE);
        break;
      case slang::ast::BinaryOperator::CaseEquality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_CEQ);
        break;
      case slang::ast::BinaryOperator::CaseInequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_CNE);
        break;
      case slang::ast::BinaryOperator::GreaterThanEqual:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SGE);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_UGE);
        break;
      case slang::ast::BinaryOperator::GreaterThan:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SGT);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_UGT);
        break;
      case slang::ast::BinaryOperator::LessThanEqual:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SLE);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_ULE);
        break;
      case slang::ast::BinaryOperator::LessThan:
        if (lhs->type->isSigned() && rhs->type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_SLT);
        else
          val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_ULT);
        break;
      case slang::ast::BinaryOperator::WildcardEquality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_WEQ);
        break;
      case slang::ast::BinaryOperator::WildcardInequality:
        val = build.buildICmp(lhsVal, rhsVal, BigInt::ICMP_WNE);
        break;
      case slang::ast::BinaryOperator::LogicalEquivalence: {
        auto zero = ctx.constBuild().zeroLike(lhsVal).get();
        val = build.buildICmp(build.buildICmp(lhsVal, zero, BigInt::ICMP_NE),
                              build.buildICmp(rhsVal, zero, BigInt::ICMP_NE),
                              BigInt::ICMP_EQ);
        break;
      }
      case slang::ast::BinaryOperator::Power: {
        // signedness of pow only depends on RHS
        if (rhs->type->isSigned())
          val = build.buildSPow(lhsVal, rhsVal);
        else
          val = build.buildUPow(lhsVal, rhsVal);
        break;
      }

      default:
        dyno_unreachable("");
      }

      if (auto wire = val.dyn_as<WireRef>())
        if (auto width = binop.type->getBitstreamWidth())
          wire->numBits = width;

      return std::make_unique<RValue>(val, expr.type.get());
      break;
    }
    case slang::ast::ExpressionKind::UnaryOp: {
      const auto &unop = expr.as<slang::ast::UnaryExpression>();

      auto operand = handle_expr(unop.operand());
      auto operandVal = operand->proGetValue(build);

      // assert(operandVal.getNumBits() == unop.type->getBitstreamWidth());

      HWValue val;

      switch (unop.op) {
      case slang::ast::UnaryOperator::Plus:
        return operand;
        break;
      case slang::ast::UnaryOperator::Minus:
        val = build.buildSub(ctx.constBuild().zeroLike(operandVal).get(),
                             operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseNot:
        val = build.buildXor(ctx.constBuild().onesLike(operandVal).get(),
                             operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseAnd:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().onesLike(operandVal).get(),
                              BigInt::ICMP_EQ);
        break;
      case slang::ast::UnaryOperator::BitwiseOr:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_NE);
        break;
      case slang::ast::UnaryOperator::BitwiseXor:
        val = build.buildRedXor(operandVal);
        break;
      case slang::ast::UnaryOperator::BitwiseNand:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().onesLike(operandVal).get(),
                              BigInt::ICMP_NE);
        break;
      case slang::ast::UnaryOperator::BitwiseNor:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_EQ);
      case slang::ast::UnaryOperator::BitwiseXnor:
        val = build.buildXor(build.buildRedXor(operandVal),
                             ConstantRef::fromFourState(FourState::S1));

      case slang::ast::UnaryOperator::LogicalNot:
        val = build.buildICmp(operandVal,
                              ctx.constBuild().zeroLike(operandVal).get(),
                              BigInt::ICMP_EQ);
        break;

      case slang::ast::UnaryOperator::Preincrement:
      case slang::ast::UnaryOperator::Predecrement:
      case slang::ast::UnaryOperator::Postincrement:
      case slang::ast::UnaryOperator::Postdecrement: {
        assert(operand->isLValue());

        bool incr = unop.op == slang::ast::UnaryOperator::Preincrement ||
                    unop.op == slang::ast::UnaryOperator::Postincrement;

        auto one = ctx.constBuild().oneLike(operandVal).get();
        auto nextV = incr ? build.buildAdd(operandVal, one)
                          : build.buildSub(operandVal, one);

        build.buildStore(operand->as<RegLValue>().lvReg, nextV,
                         operand->as<RegLValue>().lvBitRange);

        bool post = unop.op == slang::ast::UnaryOperator::Postdecrement ||
                    unop.op == slang::ast::UnaryOperator::Postincrement;

        val = post ? operandVal : nextV;
        break;
      }
      }

      return std::make_unique<RValue>(val, expr.type);
    }

    case slang::ast::ExpressionKind::NamedValue: {
      auto const &nval = expr.as<slang::ast::NamedValueExpression>();
      auto sig = vars.find(&nval.symbol);
      assert(sig != vars.end());

      return std::make_unique<RegLValue>(sig.val(), expr.type);
    }
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::IntegerLiteral>();
      return std::make_unique<RValue>(toDynoConstant(asLit.getValue()),
                                      expr.type);
    }
    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
      return std::make_unique<RValue>(toDynoConstant(asLit.getValue()),
                                      expr.type);
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto &asConv = expr.as<slang::ast::ConversionExpression>();
      auto src = handle_expr(asConv.operand());
      HWValue val = src->proGetValue(build);
      uint32_t newWidth = asConv.type->getBitstreamWidth();
      if (newWidth > val.getNumBits())
        val = build.buildExt(newWidth, val, src->type->isSigned());
      else if (newWidth < val.getNumBits())
        val = build.buildTrunc(newWidth, val);

      return std::make_unique<RValue>(val, expr.type);
    }

    case slang::ast::ExpressionKind::RangeSelect: {
      auto &asRangeS = expr.as<slang::ast::RangeSelectExpression>();
      auto value = handle_expr(asRangeS.value());

      auto rangeLeft =
          build.buildUpsize(handle_expr(asRangeS.left())->proGetValue(build),
                            32, asRangeS.left().type->isSigned());
      auto rangeRight =
          build.buildUpsize(handle_expr(asRangeS.right())->proGetValue(build),
                            32, asRangeS.right().type->isSigned());

      HWValue offs;
      HWValue len;

      switch (asRangeS.getSelectionKind()) {
      case slang::ast::RangeSelectionKind::Simple: {
        offs = rangeRight;
        auto sub = build.buildSub(rangeLeft, rangeRight);
        auto add = build.buildAdd(sub, ctx.constBuild().val(32, 1).get());
        len = add;
        break;
      }
      case slang::ast::RangeSelectionKind::IndexedUp: {
        offs = rangeLeft;
        len = rangeRight;
        break;
      }
      case slang::ast::RangeSelectionKind::IndexedDown:
        len = rangeRight;
        auto sub = build.buildSub(rangeLeft, rangeRight);
        auto add = build.buildAdd(sub, ctx.constBuild().val(32, 1).get());
        offs = add;
        break;
      }
      if (value->is<LValue>()) {
        return RegLValue::slice(build, value->as<RegLValue>(), expr.type,
                                BitRange{offs, len});
      }

      auto splice = build.buildSplice(value->getValue(), BitRange{offs, len});
      splice.defW()->numBits = expr.type->getBitstreamWidth();
      return std::make_unique<RValue>(splice.defW(), expr.type);
    }

    case slang::ast::ExpressionKind::ElementSelect: {
      auto &asElemS = expr.as<slang::ast::ElementSelectExpression>();

      auto value = handle_expr(asElemS.value());
      auto index = build.buildUpsize(
          handle_expr(asElemS.selector())->proGetValue(build), 32);

      return RegLValue::slice(build, value->as<RegLValue>(), expr.type,
                              BitRange{index, ConstantRef::fromU32(1)});
    }

    case slang::ast::ExpressionKind::Invalid:
    case slang::ast::ExpressionKind::RealLiteral:
    case slang::ast::ExpressionKind::TimeLiteral:

    case slang::ast::ExpressionKind::NullLiteral:
    case slang::ast::ExpressionKind::UnboundedLiteral:
    case slang::ast::ExpressionKind::StringLiteral:
    case slang::ast::ExpressionKind::HierarchicalValue:
    case slang::ast::ExpressionKind::ConditionalOp:
    case slang::ast::ExpressionKind::Inside:
    case slang::ast::ExpressionKind::Concatenation: {
      auto &asConcat = expr.as<slang::ast::ConcatenationExpression>();
      SmallVec<std::unique_ptr<Value>, 4> values{asConcat.operands().size()};

      bool lvalue = true;
      for (auto [idx, expr] : Range{asConcat.operands()}.enumerate()) {
        values[idx] = handle_expr(*expr);
        lvalue &= values[idx]->isLValue();
      }
      if (lvalue)
        return std::make_unique<ConcatLValue>(values, expr.type);

      SmallVec<std::pair<HWValue, BitRange>, 4> buf{values.size()};
      for (auto [idx, val] : Range{values}.enumerate()) {
        buf[idx].first = val->proGetValue(build);
        buf[idx].second = BitRange::full();
      }
      auto val = build.buildSplice(ArrayRef{buf.begin(), buf.end()});
      return std::make_unique<RValue>(val, expr.type);
    }
    case slang::ast::ExpressionKind::Replication:
    case slang::ast::ExpressionKind::Streaming:

    case slang::ast::ExpressionKind::MemberAccess:
    case slang::ast::ExpressionKind::Call:
    case slang::ast::ExpressionKind::DataType:
    case slang::ast::ExpressionKind::TypeReference:
    case slang::ast::ExpressionKind::ArbitrarySymbol:
    case slang::ast::ExpressionKind::LValueReference:
    case slang::ast::ExpressionKind::SimpleAssignmentPattern:
    case slang::ast::ExpressionKind::StructuredAssignmentPattern:
    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
    case slang::ast::ExpressionKind::EmptyArgument:
    case slang::ast::ExpressionKind::ValueRange:
    case slang::ast::ExpressionKind::Dist:
    case slang::ast::ExpressionKind::NewArray:
    case slang::ast::ExpressionKind::NewClass:
    case slang::ast::ExpressionKind::NewCovergroup:
    case slang::ast::ExpressionKind::CopyClass:
    case slang::ast::ExpressionKind::MinTypMax:
    case slang::ast::ExpressionKind::ClockingEvent:
    case slang::ast::ExpressionKind::AssertionInstance:
    case slang::ast::ExpressionKind::TaggedUnion:
      break;
    }

    abort();
  }
};

int main(int argc, char **argv) {
  slang::driver::Driver driver;
  driver.addStandardArgs();
  if (!driver.parseCommandLine(argc, argv))
    return 1;

  if (!driver.processOptions())
    return 1;

  std::unique_ptr<slang::ast::Compilation> compilation;

  bool compilation_ok;
  compilation_ok = driver.parseAllSources();
  compilation = driver.createCompilation();
  driver.reportCompilation(*compilation, false);
  auto diag = compilation->getSemanticDiagnostics();

  if (!compilation_ok) {
    printf("slang-reflect: errors found during compilation\n");
    return 1;
  }

  if (!driver.reportDiagnostics(false))
    return 1;

  HWContext ctx;

  VisitorAST visitor{ctx};
  compilation->getRoot().visit(visitor);
  visitor.handle_modules();

  std::cout << "\n\n\n";

  HWPrinter print{std::cout};
  print.printCtx(ctx);
}

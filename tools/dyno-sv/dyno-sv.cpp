
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
#include <iostream>
#include <ostream>
#include <ranges>
#include <slang/syntax/SyntaxNode.h>

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

  struct Value {
    const slang::ast::Type *type;
    bool isLValue;

  private:
    union {
      HWValue value;
      struct {
        RegisterRef lvReg;
        BitRange lvBitRange;
      };
    };
    Value(HWValue value, const slang::ast::Type *type)
        : type(type), isLValue(false), value(value) {}
    Value(RegisterRef reg, const slang::ast::Type *type, BitRange bitRange)
        : type(type), isLValue(true), lvReg(reg), lvBitRange(bitRange) {}

  public:
    Value() : isLValue(false) {}
    HWValue getValue() {
      assert(!isLValue);
      return value;
    }
    HWValue proGetValue(HWInstrBuilder &build) {
      if (!isLValue)
        return getValue();

      auto ldVal = build.buildLoad(getLVReg(), getLVBitRange());
      if (ldVal->numBits < type->getBitstreamWidth())
        return build.buildExt(type->getBitstreamWidth(), ldVal,
                              type->isSigned());
      return ldVal;
    }
    RegisterRef getLVReg() {
      assert(isLValue);
      return lvReg;
    }
    BitRange getLVBitRange() {
      assert(isLValue);
      return lvBitRange;
    }
    static Value rvalue(HWValue value, const slang::ast::Type *type) {
      return Value{value, type};
    }
    static Value lvalue(RegisterRef reg, const slang::ast::Type *type,
                        BitRange range = BitRange::full()) {
      return Value{reg, type, range};
    }
    static Value lvalueSlice(HWInstrBuilder &build, Value lv,
                             const slang::ast::Type *type, BitRange range) {
      assert(lv.isLValue);
      if (lv.getLVBitRange() == BitRange::full())
        return lvalue(lv.getLVReg(), type, range);

      // fixme: can you ever sign extend here?
      auto newAddr =
          build.buildAdd(build.buildUpsize(lv.getLVBitRange().getAddr(), 32),
                         build.buildUpsize(range.getAddr(), 32));
      auto newLen = range.getLen();

      return lvalue(lv.getLVReg(), type, BitRange{newAddr, newLen});
    }

    Value(const Value &rhs) { *this = rhs; }
    Value &operator=(const Value &rhs) {
      this->type = rhs.type;
      isLValue = rhs.isLValue;
      if (isLValue) {
        this->lvBitRange = rhs.lvBitRange;
        this->lvReg = rhs.lvReg;
      } else {
        this->value = rhs.value;
      }
      return *this;
    }
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

          Value val;

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
          build.buildStore(ireg, val.proGetValue(build));
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

  Value handle_expr(const slang::ast::Expression &expr) {

    switch (expr.kind) {
    case slang::ast::ExpressionKind::Assignment: {
      const auto &assign = expr.as<slang::ast::AssignmentExpression>();

      // todo: assign.isBlocking()

      auto rhs = handle_expr(assign.right());
      auto lhs = handle_expr(assign.left());

      if (!lhs.isLValue)
        abort();

      build.buildStore(lhs.getLVReg(), rhs.proGetValue(build),
                       lhs.getLVBitRange());
      return rhs;
    }
    case slang::ast::ExpressionKind::BinaryOp: {
      const auto &binop = expr.as<slang::ast::BinaryExpression>();
      // std::cout << binop.kind << "\n";

      auto lhs = handle_expr(binop.left());
      auto rhs = handle_expr(binop.right());

      HWValue val;

      auto lhsVal = lhs.proGetValue(build);
      auto rhsVal = rhs.proGetValue(build);

      // for some reason Slang sometimes omits implicit conversions (?)
      if (auto width = expr.type->getBitstreamWidth(); width && width != 1) {
        lhsVal = build.buildUpsize(lhsVal, width);
        rhsVal = build.buildUpsize(rhsVal, width);
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
        if (expr.type->isSigned())
          val = build.buildSDiv(lhsVal, rhsVal);
        else
          val = build.buildUDiv(lhsVal, rhsVal);
        break;
      case slang::ast::BinaryOperator::Mod:
        if (expr.type->isSigned())
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
        abort();

      case slang::ast::BinaryOperator::Equality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_EQ);
        break;
      case slang::ast::BinaryOperator::Inequality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_NE);
        break;
      case slang::ast::BinaryOperator::CaseEquality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_CEQ);
        break;
      case slang::ast::BinaryOperator::CaseInequality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_CNE);
        break;
      case slang::ast::BinaryOperator::GreaterThanEqual:
        if (lhs.type->isSigned() && rhs.type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_SGE);
        else
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_UGE);
        break;
      case slang::ast::BinaryOperator::GreaterThan:
        if (lhs.type->isSigned() && rhs.type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_SGT);
        else
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_UGT);
        break;
      case slang::ast::BinaryOperator::LessThanEqual:
        if (lhs.type->isSigned() && rhs.type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_SLE);
        else
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_ULE);
        break;
      case slang::ast::BinaryOperator::LessThan:
        if (lhs.type->isSigned() && rhs.type->isSigned())
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_SLT);
        else
          val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_ULT);
        break;
      case slang::ast::BinaryOperator::WildcardEquality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_WEQ);
        break;
      case slang::ast::BinaryOperator::WildcardInequality:
        val = build.buildICmp(lhsVal, rhsVal, dyno::BigInt::ICMP_WNE);
        break;

      case slang::ast::BinaryOperator::LogicalAnd:
      case slang::ast::BinaryOperator::LogicalOr:
      case slang::ast::BinaryOperator::LogicalImplication:
      case slang::ast::BinaryOperator::LogicalEquivalence:
      case slang::ast::BinaryOperator::Power:
        abort();
        break;
      }

      if (auto wire = val.dyn_as<WireRef>())
        if (auto width = binop.type->getBitstreamWidth())
          wire->numBits = width;

      return Value::rvalue(val, expr.type.get());
      break;
    }
    case slang::ast::ExpressionKind::UnaryOp: {
      const auto &unop = expr.as<slang::ast::UnaryExpression>();

      auto operand = handle_expr(unop.operand());
      auto operandVal = operand.proGetValue(build);

      assert(operandVal.getNumBits() == unop.type->getBitstreamWidth());

      switch (unop.op) {
      case slang::ast::UnaryOperator::Plus:
        return operand;
      case slang::ast::UnaryOperator::Minus:
        return Value::rvalue(
            build.buildSub(ctx.constBuild().zeroLike(operandVal).get(),
                           operandVal),
            unop.type);
      case slang::ast::UnaryOperator::BitwiseNot:
        return Value::rvalue(
            build.buildXor(ctx.constBuild().onesLike(operandVal).get(),
                           operandVal),
            unop.type);

      case slang::ast::UnaryOperator::BitwiseAnd:
      case slang::ast::UnaryOperator::BitwiseOr:
      case slang::ast::UnaryOperator::BitwiseXor:
      case slang::ast::UnaryOperator::BitwiseNand:
      case slang::ast::UnaryOperator::BitwiseNor:
      case slang::ast::UnaryOperator::BitwiseXnor:
        abort();

      case slang::ast::UnaryOperator::LogicalNot:
        abort();

      case slang::ast::UnaryOperator::Preincrement:
      case slang::ast::UnaryOperator::Predecrement:
      case slang::ast::UnaryOperator::Postincrement:
      case slang::ast::UnaryOperator::Postdecrement:
        abort();
        break;
      }
    }

    case slang::ast::ExpressionKind::NamedValue: {
      auto const &nval = expr.as<slang::ast::NamedValueExpression>();
      auto sig = vars.find(&nval.symbol);
      assert(sig != vars.end());

      return Value::lvalue(sig.val(), expr.type);
      break;
    }
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::IntegerLiteral>();
      return Value::rvalue(toDynoConstant(asLit.getValue()), expr.type);
    }
    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
      return Value::rvalue(toDynoConstant(asLit.getValue()), expr.type);
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto &asConv = expr.as<slang::ast::ConversionExpression>();
      auto src = handle_expr(asConv.operand());
      HWValue val = src.proGetValue(build);
      uint32_t newWidth = asConv.type->getBitstreamWidth();
      if (newWidth > val.getNumBits())
        val = build.buildExt(newWidth, val, src.type->isSigned());
      else if (newWidth < val.getNumBits())
        val = build.buildTrunc(newWidth, val);

      return Value::rvalue(val, expr.type);
    }

    case slang::ast::ExpressionKind::RangeSelect: {
      auto &asElemS = expr.as<slang::ast::RangeSelectExpression>();
      auto value = handle_expr(asElemS.value());

      auto rangeLeft =
          build.buildUpsize(handle_expr(asElemS.left()).proGetValue(build), 32,
                            asElemS.left().type->isSigned());
      auto rangeRight =
          build.buildUpsize(handle_expr(asElemS.right()).proGetValue(build), 32,
                            asElemS.right().type->isSigned());

      HWValue offs;
      HWValue len;

      switch (asElemS.getSelectionKind()) {
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
      if (value.isLValue) {
        return Value::lvalueSlice(build, value, expr.type, BitRange{offs, len});
      }

      auto splice = build.buildSplice(value.getValue(), BitRange{offs, len});
      splice.defW()->numBits = expr.type->getBitstreamWidth();
      return Value::rvalue(splice.defW(), expr.type);
    }

    case slang::ast::ExpressionKind::ElementSelect:

    case slang::ast::ExpressionKind::Invalid:
    case slang::ast::ExpressionKind::RealLiteral:
    case slang::ast::ExpressionKind::TimeLiteral:

    case slang::ast::ExpressionKind::NullLiteral:
    case slang::ast::ExpressionKind::UnboundedLiteral:
    case slang::ast::ExpressionKind::StringLiteral:
    case slang::ast::ExpressionKind::HierarchicalValue:
    case slang::ast::ExpressionKind::ConditionalOp:
    case slang::ast::ExpressionKind::Inside:
    case slang::ast::ExpressionKind::Concatenation:
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

  HWContext ctx;

  VisitorAST visitor{ctx};
  compilation->getRoot().visit(visitor);
  visitor.handle_modules();

  std::cout << "\n\n\n";

  HWPrinter print{std::cout};
  print.printCtx(ctx);
}

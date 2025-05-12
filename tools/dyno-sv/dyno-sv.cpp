
#include "dyno/Constant.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWPrinter.h"
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
#include "slang/ast/symbols/BlockSymbols.h"
#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/PortSymbols.h"
#include "slang/diagnostics/DiagnosticEngine.h"
#include "slang/diagnostics/Diagnostics.h"
#include "slang/driver/Driver.h"
#include "slang/numeric/SVInt.h"
#include "support/Bits.h"
#include "support/SmallVec.h"
#include <iostream>
#include <ostream>
#include <slang/syntax/SyntaxNode.h>

using namespace dyno;

class VisitorAST : public slang::ast::ASTVisitor<VisitorAST, true, true> {
public:
  HWContext &ctx;
  ModuleIRef mod;
  ProcessIRef proc;
  BlockRef blockRef;
  std::unordered_map<std::string_view, RegisterRef> vars;

  VisitorAST(HWContext &ctx) : ctx(ctx) {}

  struct Value {
    bool is_signed;
    FatDynObjRef<> value;
  };

  void handle(const slang::ast::InstanceSymbol &node) {
    mod = ctx.createModule(node.name);
    for (const auto &port : node.body.getPortList()) {
      auto &ps = port->as<slang::ast::PortSymbol>();
      Register::PortType ptype;
      switch (ps.direction) {
      case slang::ast::ArgumentDirection::In:
        ptype = Register::PORT_IN;
        break;
      case slang::ast::ArgumentDirection::Out:
        ptype = Register::PORT_OUT;
        break;
      case slang::ast::ArgumentDirection::InOut:
        ptype = Register::PORT_INOUT;
        break;
      case slang::ast::ArgumentDirection::Ref:
        ptype = Register::PORT_REF;
        break;
      }

      auto reg = ctx.createRegister(mod);
      mod.addPort(reg, ptype);
      vars[port->name] = reg;
    }

    for (auto &member : node.body.Scope::members()) {
      switch (member.kind) {
      case slang::ast::SymbolKind::Parameter:
      case slang::ast::SymbolKind::Port:
      case slang::ast::SymbolKind::Variable:
        // todo: support var defs
        break;

      case slang::ast::SymbolKind::ProceduralBlock: {
        handle_proc(member.as<slang::ast::ProceduralBlockSymbol>());
        break;
      }

      default:
        abort();
      }
    }

    visitDefault(node);
  }

  void handle_proc(const slang::ast::ProceduralBlockSymbol &block) {
    assert(mod);
    proc = ctx.createProcess(mod);
    blockRef = proc.block();
    handle_stmt(block.getBody());
  }

  void handle_stmt(const slang::ast::Statement &stmt) {
    assert(blockRef);

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
    assert(blockRef);

    HWInstrBuilder build{ctx, blockRef.begin()};

    switch (expr.kind) {
    case slang::ast::ExpressionKind::Assignment: {
      const auto &assign = expr.as<slang::ast::AssignmentExpression>();
      // std::cout << assign.kind << "\n";

      auto val = handle_expr(assign.right());

      if (assign.left().kind != slang::ast::ExpressionKind::NamedValue)
        abort();
      const auto &left = assign.left().as<slang::ast::NamedValueExpression>();
      auto sig = vars.find(left.symbol.name);
      assert(sig != vars.end());

      build.buildStore(sig->second, val.value);

      return val;
      break;
    }
    case slang::ast::ExpressionKind::BinaryOp: {
      const auto &binop = expr.as<slang::ast::BinaryExpression>();
      // std::cout << binop.kind << "\n";

      auto lhs = handle_expr(binop.left());
      auto rhs = handle_expr(binop.right());

      HWInstrRef instr;

      switch (binop.op) {
      case slang::ast::BinaryOperator::Add:
        instr = build.buildAdd(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::Subtract:
        instr = build.buildSub(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::Multiply:
        instr = build.buildMul(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::Divide:
        switch (binop.getEffectiveSign(false)) {
        case slang::ast::Expression::EffectiveSign::Unsigned:
        case slang::ast::Expression::EffectiveSign::Either:
          // todo: Either?
          instr = build.buildUDiv(lhs.value, rhs.value);
          break;
        case slang::ast::Expression::EffectiveSign::Signed:
          instr = build.buildSDiv(lhs.value, rhs.value);
          break;
        }
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftLeft:
      case slang::ast::BinaryOperator::LogicalShiftLeft:
        instr = build.buildSLL(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::ArithmeticShiftRight:
        instr = build.buildSRA(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::LogicalShiftRight:
        instr = build.buildSRL(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::BinaryAnd:
        instr = build.buildAnd(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::BinaryOr:
        instr = build.buildOr(lhs.value, rhs.value);
        break;
      case slang::ast::BinaryOperator::BinaryXor:
        instr = build.buildXor(lhs.value, rhs.value);
        break;
      default:
        abort();
      }

      return Value{false, instr.defW()};
      break;
    }
    case slang::ast::ExpressionKind::UnaryOp: {

      const auto &unop = expr.as<slang::ast::UnaryExpression>();
      std::cout << unop.kind << "\n";
      break;
    }

    case slang::ast::ExpressionKind::NamedValue: {
      auto const &nval = expr.as<slang::ast::NamedValueExpression>();
      auto sig = vars.find(nval.symbol.name);
      assert(sig != vars.end());

      return Value{(bool)expr.getEffectiveSign(false),
                   build.buildLoad(sig->second).defW()};
      break;
    }
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::IntegerLiteral>();
      return Value{(bool)asLit.getEffectiveSign(false),
                   toDynoConstant(asLit.getValue())};
    }
    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto &asLit = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
      return Value{(bool)asLit.getEffectiveSign(false),
                   toDynoConstant(asLit.getValue())};
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto &asConv = expr.as<slang::ast::ConversionExpression>();
      return handle_expr(asConv.operand());
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
    case slang::ast::ExpressionKind::Concatenation:
    case slang::ast::ExpressionKind::Replication:
    case slang::ast::ExpressionKind::Streaming:
    case slang::ast::ExpressionKind::ElementSelect:
    case slang::ast::ExpressionKind::RangeSelect:
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

/*
class IRPrinter {
public:
  void print_module(std::ostream &os, ModuleRef mod) {
    os << "module %" << mod->name << " {\n";
    for (SignalRef signal : mod->signals) {
      if (signal->stype != Signal::INTERNAL) {
        os << Signal::signal_type_names[signal->stype] << " bit("
           << signal->range.len << ") %" << signal->name << "\n";
      }
    }
    os << "\n";

    for (auto proc : mod->procs) {
      print_proc(os, proc);
    }
    os << "}\n";
  }

  void print_proc(std::ostream &os, ProcessRef proc) {
    os << "proc {\n";
    print_block(os, &proc->body);
    os << "}\n";
  }

  void print_block(std::ostream &os, BlockInstrRef block) {
    for (auto &inst : block->members) {
      print_inst(os, inst);
    }
  }

  size_t count = 0;
  std::unordered_map<InstrRef, size_t> idx_map;

  void print_inst(std::ostream &os, InstrRef inst) {
    switch (inst->opcode) {
    case Opcode::LOAD: {
      std::cout << "%" << count << " = load \""
                << ((LoadInstr *)inst)->base->name << "\"\n";
      idx_map[inst] = count++;
      break;
    }
    case Opcode::STORE_NB:
    case Opcode::STORE: {
      auto store = inst->opcode == Opcode::STORE_NB ? "store_nb" : "store";
      std::cout << "%" << count << " = " << store << " \""
                << ((StoreInstr *)inst)->base->name << "\" " << "%"
                << idx_map[((StoreInstr *)inst)->data] << "\n";
      idx_map[inst] = count++;
      break;
    }
    case Opcode::BINOP: {
      const char *binop;
      switch (((BinopInstr *)inst)->op) {
      case BinOpcode::ADD:
        binop = "add";
        break;
      case BinOpcode::SUB:
        binop = "sub";
        break;
      case BinOpcode::AND:
        binop = "and";
        break;
      case BinOpcode::OR:
        binop = "or";
        break;
      case BinOpcode::XOR:
        binop = "xor";
        break;
      }
      std::cout << "%" << count << " = " << binop << " %"
                << idx_map[((BinopInstr *)inst)->lhs] << " " << "%"
                << idx_map[((BinopInstr *)inst)->rhs] << "\n";
      idx_map[inst] = count++;
      break;
    }
    case Opcode::BLOCK: {
      print_block(os, ((BlockInstrRef)inst));
      break;
    }
    default:
      break;
    }
  }
};
*/

struct Visitor : public slang::ast::ASTVisitor<Visitor, true, true> {
  int count = 0;
  template <typename T> void handle(const T &t) {
    if constexpr (std::is_base_of_v<slang::ast::Statement, T>) {
      ;
    }
    visitDefault(t);
    printf("visit\n");
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

  std::cout << "\n\n\n";

  HWPrinter print{std::cout};
  print.printCtx(ctx);
}

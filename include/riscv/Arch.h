#pragma once

#include "ir/Alignment.h"
#include "ir/Arch.h"
#include "ir/ArchIRBuilder.h"
#include "ir/FrameLayout.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPatExecutor.h"
#include "ir/IRVisitor.h"
#include "ir/Operand.h"
#include "ir/RegAlloc.h"
#include "ir/SSAIRBuilder.h"
#include "support/Ranges.h"
#include "support/Utility.h"
#include <array>
#include <cassert>
#include <initializer_list>

namespace riscv {

constexpr unsigned XLEN = 32;
constexpr unsigned XLEN_BYTES = XLEN / 8;
constexpr unsigned XLEN_ALIGNEXP = 2;

#include "riscv/Arch.dsl.riscv.h"

class ArchIRBuilder : public ::ArchIRBuilder {
  void frameLoadReg(IRBuilder &b, Reg reg,
                    FrameDef &frameDef) const override {
    // FIXME: proper regclass handling
    assert(frameDef.getSize() == 4);
    Reg addrReg = b.getFunction().getRegInfo().cloneVReg(reg);
    b.emitExternRef(addrReg, frameDef);
    Instr &i = b.emitInstr(LW, 3);
    i.emplaceOperand<Operand::REG_DEF>(reg);
    i.emplaceOperand<Operand::REG_USE>(addrReg);
    i.emplaceOperand<Operand::IMM32>(0);
  }

  void frameStoreReg(IRBuilder &b, Reg reg,
                     FrameDef &frameDef) const override {
    // FIXME: proper regclass handling
    assert(frameDef.getSize() == 4);
    Reg addrReg = b.getFunction().getRegInfo().cloneVReg(reg);
    b.emitExternRef(addrReg, frameDef);
    Instr &i = b.emitInstr(SW, 3);
    i.emplaceOperand<Operand::REG_USE>(reg);
    i.emplaceOperand<Operand::REG_USE>(addrReg);
    i.emplaceOperand<Operand::IMM32>(0);
  }
};

class Arch : public ::Arch {
public:
  std::span<const ArchReg> getArchRegs() override { return archRegs; };
  const ArchReg *getArchReg(unsigned kind) override {
    return riscv::getArchReg(kind);
  }
  const ArchInstr *getArchInstr(unsigned kind) override {
    return riscv::getArchInstr(kind);
  }
  const ArchRegClass *getArchRegClass(unsigned kind) override {
    return riscv::getArchRegClass(kind);
  }

  const ArchIRBuilder &getArchIRBuilder() override {
    return archIRBuilder;
  }

  ArchIRBuilder archIRBuilder;
};

class ABILoweringPass : public IRPass<Function> {
public:
  const char *name() override { return "RiscVABILoweringPass"; }

  static constexpr auto inRegs =
      std::to_array<Reg>({ALIAS_a0, ALIAS_a1, ALIAS_a2, ALIAS_a3, ALIAS_a4,
                          ALIAS_a5, ALIAS_a6, ALIAS_a7});
  static constexpr auto outRegs = std::to_array<Reg>({ALIAS_a0, ALIAS_a1});
  static constexpr auto callClobberedRegs = std::to_array<Reg>(
      {ALIAS_ra, ALIAS_t0, ALIAS_t1, ALIAS_t2, ALIAS_t3, ALIAS_t4, ALIAS_t5,
       ALIAS_t6, ALIAS_a0, ALIAS_a1, ALIAS_a2, ALIAS_a3, ALIAS_a4, ALIAS_a5,
       ALIAS_a6, ALIAS_a7});

  void run(Function &func, IRInfo<Function> &info) override {
    if (func.empty()) {
      return;
    }
    std::vector<Instr *> worklist;

    auto &abiTy = IntSSAType::get(32);

    auto &entryBB = func.getFirst();
    SSAIRBuilder ir(entryBB.getFirstSentry());
    size_t paramNum = 0;
    for (auto it = entryBB.begin(), itEnd = entryBB.end(); it != itEnd;) {
      auto &instr = *it;
      ++it;
      if (instr.getKind() != Instr::REF_PARAM) {
        continue;
      }
      worklist.push_back(&instr);
      if (paramNum < inRegs.size()) {
        ir.emitCopy(abiTy, inRegs[paramNum]);
        assert(instr.getDef().ssaDef().type() == abiTy &&
               "Illegal parameter type");
        instr.getDef().ssaDef().replaceAllUses(ir.getDef());
      } else {
        assert(false && "Stack passing unimplemented");
      }
      ++paramNum;
    }
    for (auto *instr : worklist) {
      instr->deleteThis();
    }
    worklist.clear();

    for (auto &block : func) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;
        SSAIRBuilder ir(instr);
        if (instr.getKind() == Instr::RET) {
          size_t paramNum = 0;
          Instr *i = func.createInstr(JALR, 3 + instr.getNumOperands());
          i->emplaceOperand<Operand::REG_DEF>(X0);
          i->emplaceOperand<Operand::REG_USE>(ALIAS_ra);
          i->emplaceOperand<Operand::IMM32>(0);
          for (auto &op : instr) {
            assert(op.ssaUse().getDef().ssaDef().type() == abiTy);
            assert(paramNum < outRegs.size());
            ir.emitCopy(outRegs[paramNum], op.ssaUse().getDef());
            i->emplaceOperand<Operand::REG_USE>(outRegs[paramNum]);
            i->getLastOperand().setImplicit(true);
            ++paramNum;
          }
          ir.emit(i);
          instr.deleteThis();
        } else if (instr.getKind() == Instr::CALL) {
          Instr *i = func.createInstr(
              PSEUDO_CALL, 1 + callClobberedRegs.size() + inRegs.size());
          for (auto reg : callClobberedRegs) {
            i->emplaceOperand<Operand::REG_DEF>(reg);
            i->getLastOperand().setImplicit(true);
          }
          Operand &funcDef = instr.getOther().ssaUse().getDef();
          i->emplaceOperand<Operand::SSA_USE>(funcDef);
          size_t paramNum = 0;
          for (auto it = instr.other_begin() + 1, itEnd = instr.other_end();
               it != itEnd; ++it) {
            auto &op = *it;
            assert(op.ssaUse().getDef().ssaDef().type() == abiTy);
            assert(paramNum < inRegs.size());
            ir.emitCopy(inRegs[paramNum], op.ssaUse().getDef());
            i->emplaceOperand<Operand::REG_USE>(inRegs[paramNum]);
            i->getLastOperand().setImplicit(true);
            ++paramNum;
          }
          ir.emit(i);
          paramNum = 0;
          for (auto it = instr.def_begin(), itEnd = instr.def_end();
               it != itEnd; ++it) {
            auto &op = *it;
            assert(op.ssaDef().type() == abiTy);
            assert(paramNum < outRegs.size());
            ir.emitCopy(abiTy, outRegs[paramNum]);
            op.ssaDef().replaceAllUses(ir.getDef());
            ++paramNum;
          }
          instr.deleteThis();
        }
      }
    }
  }
};

class PreRAOptPass : public IRPass<Function>, public IRVisitor<PreRAOptPass> {
public:
  const char *name() override { return "RiscVPreRAOptPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    arch = &info.getArch();
    this->func = &func;
    dispatch(func);
  }

  void visitInstr(Instr &instr) {
    switch (instr.getKind()) {
    case Instr::COPY:
      foldCopy(instr);
      break;
    }
  }

  void foldCopy(Instr &i) {
    Reg dst = i.getDef().reg();
    Reg src = i.getOther().reg();
    // Propagate some copies that are not coalesced by RegAlloc
    // e.g COPY def(VReg) X0
    // This cannot be coalesced because X0 cannot legally be assigned to an
    // alive def and is thus not included in the RegClass.
    // TODO: Possible solutions:
    // - compute RegClass from register requirements of defs and uses of a VReg
    // - handle this using proper copy propagation instead of coalescing
    // FIXME: this does not reach a fixed-point
    // FIXME: check regclass requirements before rewriting
    if (dst.isVReg() && src.isPhysReg() &&
        arch->getArchReg(src.getNum())->noLiveness) {
      auto &dstRoot = func->getRegInfo().defUseRoot(dst);
      if (dstRoot.getSingleDef()) {
        i.deleteThis();
        assert(dstRoot.count_defs() == 0);
        dstRoot.replace(src);
      }
    }
  }

private:
  ::Arch *arch;
  Function *func;
};

class PostRALoweringPass : public IRPass<Function>,
                           public IRVisitor<PostRALoweringPass> {
public:
  void run(Function &func, IRInfo<Function> &info) override {
    this->func = &func;
    dispatch(func);
  }

  void visitInstr(Instr &instr) {
    switch (instr.getKind()) {
    case BEQ:
    case BNE:
    case BLT:
    case BLTU:
    case BGE:
    case BGEU:
      lowerCondBr(instr);
      break;
    case JAL:
      lowerJmp(instr);
      break;
    case Instr::COPY:
      lowerCopy(instr);
      break;
    }
  }

  void lowerCopy(Instr &i) {
    Instr *newI = func->createInstr(ADDI, 3);
    newI->allocateOperands(3);
    newI->emplaceOperand<Operand::REG_DEF>(i.getOperand(0).reg());
    newI->emplaceOperand<Operand::REG_USE>(i.getOperand(1).reg());
    newI->emplaceOperand<Operand::IMM32>(0);
    i.insertPrev(newI);
    i.deleteThis();
  }

  void lowerJmp(Instr &i) {
    assert(i.getOperand(0).reg() == X0);
    if (&i.getParent().getNextNode() ==
        &i.getOperand(1).ssaUse().getDef().ssaDefBlock()) {
      i.deleteThis();
      return;
    }
    // FIXME: branch relaxation
  }

  void lowerCondBr(Instr &i) {
    Instr *newBr = func->createInstr(i.getKind(), 3);
    newBr->addOperand(i.getOperand(0));
    newBr->addOperand(i.getOperand(1));
    newBr->addOperand(i.getOperand(2));
    i.insertPrev(newBr);
    Instr *newJmp = func->createInstr(JAL, 2);
    newJmp->emplaceOperand<Operand::REG_USE>(X0);
    newJmp->addOperand(i.getOperand(3));
    i.insertPrev(newJmp);
    lowerJmp(*newJmp);
    i.deleteThis();
  }

  const char *name() override { return "RiscVPostRALoweringPass"; }

  Function *func;
};

class FrameLoweringPass : public IRPass<Function> {
  const char *name() override { return "RiscVFrameLoweringPass"; }

  static constexpr auto savedRegs = std::to_array<Reg>(
      {ALIAS_ra, ALIAS_s0, ALIAS_s1, ALIAS_s2, ALIAS_s3, ALIAS_s4, ALIAS_s5,
       ALIAS_s6, ALIAS_s7, ALIAS_s8, ALIAS_s9, ALIAS_s10, ALIAS_s11});

  void run(Function &func, IRInfo<Function> &info) override {
    clobber = &info.query<RegClobber>();
    calcClobberedSavedRegs();
    calcFrameOffsets(func.getFrameLayout());

    IRBuilder prologueIr(func.getEntry().getFirstSentry());
    insertPrologue(prologueIr);

    for (auto &block : func) {
      for (auto &instr : block) {
        if (instr.getKind() != JALR)
          continue;
        if (instr.getOperand(1).reg() != ALIAS_ra)
          continue;
        assert(instr.getOperand(0).reg() == X0);
        assert(instr.getOperand(2).imm32() == 0);

        IRBuilder epilogueIr(instr);
        insertEpilogue(epilogueIr);
      }
    }
    materializeFrameRefs(func.getFrameLayout());
  }

  // if frame size constant: sp = fp - frameSize
  // fp + offset = sp - frameSize + offset
  int32_t getFPOffset(FrameDef &frameDef) {
    return -(int32_t)frameSize + (int32_t)frameOffsets[frameDef.getId()];
  }
  int32_t getFPOffsetForRegSlot(size_t idx) {
    return -((int32_t)idx + 1) * (int32_t)XLEN_BYTES;
  }
  int32_t getSPOffset(FrameDef &frameDef) {
    return (int32_t)frameSize + getFPOffset(frameDef);
  }
  int32_t getSPOffsetForRegSlot(size_t idx) {
    return (int32_t)frameSize + getFPOffsetForRegSlot(idx);
  }

  void calcClobberedSavedRegs() {
    clobberedSavedRegs.clear();
    for (auto reg : savedRegs) {
      if (clobber->phys.contains(reg)) {
        clobberedSavedRegs.push_back(reg);
      }
    }
  }

  void calcFrameOffsets(FrameLayout &frameLayout) {
    frameSize = 0;
    frameAlign = Alignment(4);
    frameOffsets = std::vector<size_t>(frameLayout.getNumEntries());
    for (auto &frameEntry : frameLayout.entries()) {
      Alignment align = frameEntry.getAlign();
      frameSize = align.alignSize(frameSize);
      frameOffsets[frameEntry.getId()] = frameSize;
      frameSize += frameEntry.getSize();
    }
    frameSize += clobberedSavedRegs.size() * XLEN_BYTES;
    frameSize = frameAlign.alignSize(frameSize);
  }

  void materializeFrameRefs(FrameLayout &frameLayout) {
    for (auto &frameEntry : frameLayout.entries()) {
      for (auto &use : make_earlyincr_range(frameEntry.operand().ssaDef())) {
        Instr &instr = use.getParent();
        switch (instr.getKind()) {
        case Instr::REF_EXTERN: {
          Instr &i = IRBuilder(instr).emitInstr(ADDI, 3);
          i.emplaceOperand<Operand::REG_DEF>(instr.getOperand(0).reg());
          i.emplaceOperand<Operand::REG_USE>(ALIAS_sp);
          i.emplaceOperand<Operand::IMM32>(getSPOffset(frameEntry));
          instr.deleteThis();
          break;
        }
        default:
          UNREACHABLE("Illegal use of frame def");
        }
      }
    }
  }

  void insertPrologue(IRBuilder &ir) {
    if (frameSize == 0)
      return;
    Instr &i = ir.emitInstr(riscv::ADDI, 3);
    i.emplaceOperand<Operand::REG_DEF>(ALIAS_sp);
    i.emplaceOperand<Operand::REG_USE>(ALIAS_sp);
    i.emplaceOperand<Operand::IMM32>(-(int32_t)frameSize);
    for (size_t idx = 0; idx < clobberedSavedRegs.size(); ++idx) {
      Instr &i = ir.emitInstr(SW, 3);
      i.emplaceOperand<Operand::REG_USE>(clobberedSavedRegs[idx]);
      i.emplaceOperand<Operand::REG_USE>(ALIAS_sp);
      i.emplaceOperand<Operand::IMM32>(getSPOffsetForRegSlot(idx));
    }
  }

  void insertEpilogue(IRBuilder &ir) {
    if (frameSize == 0)
      return;
    for (size_t idx = 0; idx < clobberedSavedRegs.size(); ++idx) {
      Instr &i = ir.emitInstr(LW, 3);
      i.emplaceOperand<Operand::REG_DEF>(clobberedSavedRegs[idx]);
      i.emplaceOperand<Operand::REG_USE>(ALIAS_sp);
      i.emplaceOperand<Operand::IMM32>(getSPOffsetForRegSlot(idx));
    }
    Instr &i = ir.emitInstr(riscv::ADDI, 3);
    i.emplaceOperand<Operand::REG_DEF>(ALIAS_sp);
    i.emplaceOperand<Operand::REG_USE>(ALIAS_sp);
    i.emplaceOperand<Operand::IMM32>((int32_t)frameSize);
  }

private:
  RegClobber *clobber;
  size_t frameSize;
  Alignment frameAlign;
  std::vector<size_t> frameOffsets;
  std::vector<Reg> clobberedSavedRegs;
};

class AsmPrinterPass : public IRPass<Program> {
public:
  AsmPrinterPass(std::ostream &out);
  const char *name() override { return "RiscVAsmPrinterPass"; }
  void run(Program &prog, IRInfo<Program> &info) override {
    blockToNum.clear();
    currBlockNum = 0;

    out << ".option nopic\n";
    out << ".attribute arch, \"rv32i\"\n";
    out << ".text\n";
    for (auto &func : prog.functions()) {
      printFunction(func);
    }
    for (auto &mem : prog.staticMemories()) {
      printStaticMemory(mem);
    }
  }

  void printStaticMemory(StaticMemory &mem) {
    if (mem.initializer.empty()) {
      out << ".bss\n";
    } else {
      out << ".data\n";
    }
    printGlobalHeader(mem);
    for (auto b : mem.initializer) {
      out << ".byte " << (unsigned char)b << "\n";
    }
    out << ".zero " << mem.size - mem.initializer.size() << "\n";
  }

  void printGlobalName(GlobalDef &def) { out << def.getName(); }

  void printGlobalHeader(GlobalDef &def) {
    if (def.getLinkage() == GlobalDef::Linkage::EXTERNAL) {
      out << ".globl " << def.getName() << "\n";
    }
    printGlobalName(def);
    out << ":\n";
  }

  void printFunction(Function &func) {
    for (auto &block : func) {
      blockToNum[&block] = currBlockNum++;
    }
    printGlobalHeader(func);
    for (auto &block : func) {
      printBlock(block);
    }
  }

  void printBlock(CFGBlock &block) {
    printBlockLabel(block);
    out << ":\n";
    currIndent = 1;
    for (auto &instr : block) {
      printIndent();
      printInstr(instr);
      out << "\n";
    }
  }

  void printInstr(Instr &instr) {
    const ArchInstr *archInstr = getArchInstr(instr.getKind());
    if (!archInstr || !archInstr->alias) {
      assert(false && "Illegal instruction");
      return;
    }
    out << archInstr->alias;
    if (instr.getNumOperands() == 0)
      return;

    out << " ";

    switch (instr.getKind()) {
    case LBU:
    case LB:
    case LHU:
    case LH:
    case LW:
    case SB:
    case SH:
    case SW:
      printLoadStoreOperands(instr);
      return;
    }

    bool first = true;
    for (auto &op : instr) {
      if (op.isImplicit())
        continue;
      if (first) {
        first = false;
      } else {
        out << ", ";
      }
      printOperand(op);
    }
  }

  void printLoadStoreOperands(Instr &instr) {
    printOperand(instr.getOperand(0));
    out << ", ";
    printOperand(instr.getOperand(2));
    out << "(";
    printOperand(instr.getOperand(1));
    out << ")";
  }

  void printBlockLabel(CFGBlock &block) { out << ".Lbb" << blockToNum[&block]; }

  void printOperand(Operand &op) {
    switch (op.getKind()) {
    case Operand::EMPTY:
      break;
    case Operand::SSA_DEF_TYPE:
    case Operand::SSA_DEF_EXTERN:
    case Operand::TYPE:
    case Operand::BLOCK:
    case Operand::BRCOND:
    case Operand::OP_CHAIN:
    case Operand::MINT:
      assert(false && "Illegal operand");
      break;
    case Operand::SSA_USE: {
      Operand &def = op.ssaUse().getDef();
      if (def.getKind() == Operand::SSA_DEF_EXTERN) {
        printExternSSADef(def.ssaDefExtern());
        break;
      }
      assert(false && "Illegal operand");
      break;
    }
    case Operand::REG_DEF:
    case Operand::REG_USE: {
      const ArchReg *archReg = getArchReg(op.reg());
      if (!archReg || !archReg->alias) {
        assert(false && "Illegal register");
        return;
      }
      out << archReg->alias;
      break;
    }
    case Operand::IMM32: {
      out << op.imm32();
      break;
    }
    }
  }

  void printExternSSADef(ExternSSADef &def) {
    if (def.isGlobal()) {
      printGlobalName(def.global());
    } else if (is<CFGBlock>(def)) {
      printBlockLabel(def.block());
    } else {
      assert(false && "Illegal operand");
    }
  }

  void printIndent() {
    for (unsigned i = 0; i < currIndent; ++i) {
      out << "  ";
    }
  }

private:
  unsigned currIndent;
  size_t currBlockNum;
  std::unordered_map<CFGBlock *, unsigned> blockToNum;
  std::ostream &out;
};

} // namespace riscv

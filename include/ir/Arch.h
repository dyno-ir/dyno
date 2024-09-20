#pragma once

#include "ir/IR.h"
#include <span>

struct ArchReg {
  const Reg reg;
  const char *name;
  const bool noLiveness;
  const char *alias;
};

struct ArchInstr {
  const unsigned kind;
  const char *name;
  const char *alias;
};

struct ArchRegClass {
  const unsigned kind;
  const char *name;
  const std::span<const ArchReg *> regs;
};

class ArchIRBuilder;

class Arch {
public:
  virtual std::span<const ArchReg> getArchRegs() = 0;
  virtual const ArchReg *getArchReg(unsigned kind) = 0;
  virtual const ArchInstr *getArchInstr(unsigned kind) = 0;
  virtual const ArchRegClass *getArchRegClass(unsigned kind) = 0;
  virtual const ArchIRBuilder &getArchIRBuilder() = 0;
};

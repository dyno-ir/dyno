#pragma once

#include "ir/IRBuilder.h"

class ArchIRBuilder {
public:
  virtual void frameStoreReg(IRBuilder &b, Reg reg,
                             FrameDef &frameDef) const = 0;
  virtual void frameLoadReg(IRBuilder &b, Reg reg,
                            FrameDef &frameDef) const = 0;
};

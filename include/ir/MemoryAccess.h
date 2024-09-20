#pragma once

#include "ir/Alignment.h"
#include "ir/Operand.h"

class MemoryAccessDef : public ExternSSADef {
public:
  static bool is_impl(const ExternSSADef &o) {
    return o.getKind() == FUNC_MEMORY_ACCESS;
  }

  MemoryAccessDef(unsigned id, size_t size, Alignment align)
      : ExternSSADef(FUNC_MEMORY_ACCESS), id(id), size(size), align(align) {}

  size_t getSize() { return size; }

  Alignment getAlign() { return align; }

  unsigned getID() { return id; }

private:
  unsigned id;
  size_t size;
  Alignment align;
};

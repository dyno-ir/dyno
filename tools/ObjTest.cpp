#if 0
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "hw/IDs.h"
#include "hw/ObjInfo.h"
#include "hw/Wire.h"
#include <dyno/Instr.h>
#include <dyno/InstrPrinter.h>
#include <dyno/Interface.h>
#include <dyno/NewDeleteObjStore.h>
#include <iostream>

using namespace dyno;

static std::array dialectIs{&coreDialectInfo, &rtlDialectInfo};
static std::array tyIs{coreTyInfo, rtlTyInfo};
static std::array opcodeIs{coreOpcodeInfo, rtlOpcodeInfo};


void basic()
{

}

int main() {
  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};
  RefPrinter refPrinter(std::cout, dialectI, tyI);
  InstrPrinter instrPrinter(refPrinter, opcI);

  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Instr> instrs;

  auto instr1 = instrs.create(2, DialectID{DIALECT_HW}, OpcodeID{0});
  auto instr2 = instrs.create(2, DialectID{DIALECT_HW}, OpcodeID{1});
  auto instr3 = instrs.create(1, DialectID{DIALECT_CORE}, OpcodeID{0});
  auto instr4 = instrs.create(1, DialectID{DIALECT_HW}, OpcodeID{3});
  auto wire1 = wires.create();
  auto wire2 = wires.create();

  InstrBuilder(InstrRef{instr1}).addRef(wire2).other().add<Constant>(32, 69);
  InstrBuilder(InstrRef{instr2}).other().add<Constant>(32, 69).addRef(wire2);
  InstrBuilder(InstrRef{instr3}).other().addRef(wire2);
  InstrBuilder(InstrRef{instr4}).other().addRef(wire2);

  for (auto wire : wires) {
    refPrinter.introduceRef(wire);
  }
  /*std::cout << "---\n";*/
  /*for (auto instr : instrs) {*/
  /*  instrPrinter.print(InstrRef{instr});*/
  /*}*/

  // instrs.destroy(instr3);

  std::cout << "---\n";
  for (auto op : InstrRef{instr1}.def().defUse()) {
    std::cout << (op.isDef() ? "def: " : "use: ");
    refPrinter.print(op.instr());
    std::cout << '\n';
  }

  for (auto op : instrs) {
    instrPrinter.print(InstrRef{op});
  }

  std::cout << "Def use of instr1\n";
  for (auto def : InstrRef{instr1}.defs()) {
    for (auto use : def.defUse()) {
      instrPrinter.print(use.instr());
    }
    std::cout << "----------\n";
  }
}
#endif

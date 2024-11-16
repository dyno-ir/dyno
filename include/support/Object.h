#pragma once

enum class CoreDialectTyID {
  INSTR,
};

//
//  CFG& cfg = ir.get<ControlFlowGraph>()
//  Instr& i = ir.get<Instr>(ref);
//  ObjRef = ir.create<Instr>();
//  ObjRef = ir.get<CFGBlock>();
//  for(b:cfg){ for(i:b){}}
//  ir.resolve()
//

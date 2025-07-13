{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
SmallVec<FatDynObjRef<>, 4> r4;
SmallVec<FatDynObjRef<>, 4> r5;
InstrRef::iterator r6;
InstrRef r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
InstrRef::iterator r10;
InstrRef::iterator r11;
InstrRef::iterator r12;
FatDynObjRef<>* r13;
FatDynObjRef<>* r14;
FatDynObjRef<>* r15;
FatDynObjRef<>* r16;
FatDynObjRef<>* r18;
FatDynObjRef<>* r19;
FatDynObjRef<>* r21;
FatDynObjRef<>* r22;
if (!r0.isOpc(OP_ADD)) goto fail_0;
if (r0.getNumOperands() < 2) goto fail_0;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
for (r6 = r3;; r6++) {
if (r6 == r2) break;
if (r4.size() > 0) goto cont_0_0;
if ((*r6)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto cont_0_0;
r7 = getDefInstr(r6);
if (!r7.isOpc(OP_SUB)) goto cont_0_0;
if (r7.getNumOperands() > 3) goto cont_0_0;
if (r7.getNumOperands() < 3) goto cont_0_0;
r8 = r7.begin();
r9 = r7.end();
r10 = std::next(r8);
r11 = std::next(r10);
r12 = std::next(r11);
appendCopy(r4, r6);
goto loop_0_0_continue;
cont_0_0:;
appendCopy(r5, r6);
goto loop_0_0_continue;
cont_0_1:;
loop_0_0_continue:;
}
loop_0_0_break:;
if (r4.size() < 1) goto fail_0;
r13 = r4.begin();
r14 = r4.end();
if (*r13 != (*r8)->fat()) goto fail_0;
r15 = r5.begin();
r16 = r5.end();
if (r6 != r2) goto fail_0;

  FatDynObjRef<> w = ctx.getWires().create((*Range{r8, r10}.begin())->as<WireRef>().getNumBits()); RefRange{r18, r19} = w;

  FatDynObjRef<> constant = cbuild.val("1'h1"_bv).resize(*(*Range{r1, r3}.begin())->as<WireRef>().getNumBits(), false).get();
  RefRange{r21, r22} = constant;
auto r17 = build.buildInstrRaw(OP_XNOR, (r19-r18)+(r12-r11));
copyOperands(r17, r18, r19);
r17.other();
copyOperands(r17, r11, r12);
auto r20 = build.buildInstrRaw(OP_ADD, (r3-r1)+(r16-r15)+(r11-r10)+(r19-r18)+(r22-r21));
copyOperands(r20, r1, r3);
r20.other();
copyOperands(r20, r15, r16);
copyOperands(r20, r10, r11);
copyOperands(r20, r18, r19);
copyOperands(r20, r21, r22);
deleteIfSingleUse(matched, ctx, r7);
deleteF(matched, ctx, r0);
return true;
}
fail_0:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
InstrRef::iterator r10;
FatDynObjRef<>* r12;
FatDynObjRef<>* r13;
FatDynObjRef<>* r15;
FatDynObjRef<>* r16;
if (!r0.isOpc(OP_SUB)) goto fail_1;
if (r0.getNumOperands() > 3) goto fail_1;
if (r0.getNumOperands() < 3) goto fail_1;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_1;
r4 = getDefInstr(r3);
if (!r4.isOpc(OP_ADD)) goto fail_1;
if (r4.getNumOperands() < 1) goto fail_1;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
for (r8 = r7;; r8++) {
if (r8 == r6) break;
loop_1_0_continue:;
}
loop_1_0_break:;
if (r8 != r6) goto fail_1;
r9 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_1;
r10 = std::next(r9);

  FatDynObjRef<> w = ctx.getWires().create((*Range{r5, r7}.begin())->as<WireRef>().getNumBits()); RefRange{r12, r13} = w;

  FatDynObjRef<> constant = cbuild.val("1'h1"_bv).resize(*(*Range{r1, r3}.begin())->as<WireRef>().getNumBits(), false).get();
  RefRange{r15, r16} = constant;
auto r11 = build.buildInstrRaw(OP_XNOR, (r13-r12)+(r10-r9));
copyOperands(r11, r12, r13);
r11.other();
copyOperands(r11, r9, r10);
auto r14 = build.buildInstrRaw(OP_ADD, (r3-r1)+(r8-r7)+(r13-r12)+(r16-r15));
copyOperands(r14, r1, r3);
r14.other();
copyOperands(r14, r7, r8);
copyOperands(r14, r12, r13);
copyOperands(r14, r15, r16);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_1:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
FatDynObjRef<>* r7;
FatDynObjRef<>* r8;
if (!r0.isOpc(OP_SUB)) goto fail_2;
if (r0.getNumOperands() > 3) goto fail_2;
if (r0.getNumOperands() < 3) goto fail_2;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = std::next(r3);
if ((*r4)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_2;
r5 = std::next(r4);

  FatDynObjRef<> negated = cbuild.val((*Range{r4, r5}.begin())->as<ConstantRef>()).neg().get();
  RefRange{r7, r8} = negated;
auto r6 = build.buildInstrRaw(OP_ADD, (r3-r1)+(r4-r3)+(r8-r7));
copyOperands(r6, r1, r3);
r6.other();
copyOperands(r6, r3, r4);
copyOperands(r6, r7, r8);
deleteF(matched, ctx, r0);
return true;
}
fail_2:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
if (!r0.isOpc(HW_LOAD)) goto fail_3;
if (r0.getNumOperands() < 3) goto fail_3;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = std::next(r3);
if ((*r4)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_3;
if (!(*r4)->as<ConstantRef>().valueEqualsS("1'h0"_bv)) goto fail_3;
r5 = std::next(r4);

  auto asConstant = (*Range{r5, std::next(r5)}.begin())->dyn_as<ConstantRef>();
  if (!asConstant) goto fail_3;
  if (!asConstant.valueEquals(*(*Range{r3, r4}.begin())->as<RegisterRef>().getNumBits()))
    goto fail_3;
r6 = std::next(r5);
if (r6 != r2) goto fail_3;
auto r7 = build.buildInstrRaw(HW_LOAD, (r3-r1)+(r4-r3));
copyOperands(r7, r1, r3);
r7.other();
copyOperands(r7, r3, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_3:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
if (!r0.isOpc(HW_STORE)) goto cont_4_0;
goto goto_4_0;
cont_4_0:;
if (!r0.isOpc(HW_STORE_DEFER)) goto fail_4;
goto_4_0:;
if (r0.getNumOperands() < 3) goto fail_4;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = std::next(r3);
if ((*r4)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_4;
if (!(*r4)->as<ConstantRef>().valueEqualsS("1'h0"_bv)) goto fail_4;
r5 = std::next(r4);

  auto asConstant = (*Range{r5, std::next(r5)}.begin())->dyn_as<ConstantRef>();
  if (!asConstant) goto fail_4;
  if (!asConstant.valueEquals(*(*Range{r3, r4}.begin())->as<RegisterRef>().getNumBits()))
    goto fail_4;
r6 = std::next(r5);
if (r6 != r2) goto fail_4;
auto r7 = build.buildInstrRaw(r0.getDialectOpcode(), (r3-r1)+(r4-r3));
r7.other();
copyOperands(r7, r1, r3);
copyOperands(r7, r3, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_4:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
if (!r0.isOpc(HW_STORE_DEFER)) goto fail_5;
if (r0.getNumOperands() < 4) goto fail_5;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = std::next(r3);
if ((*r4)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_5;
if (!(*r4)->as<ConstantRef>().valueEqualsS("1'h0"_bv)) goto fail_5;
r5 = std::next(r4);

  auto asConstant = (*Range{r5, std::next(r5)}.begin())->dyn_as<ConstantRef>();
  if (!asConstant) goto fail_5;
  if (!asConstant.valueEquals(*(*Range{r3, r4}.begin())->as<RegisterRef>().getNumBits()))
    goto fail_5;
r6 = std::next(r5);
r7 = std::next(r6);
if (r7 != r2) goto fail_5;
auto r8 = build.buildInstrRaw(HW_STORE_DEFER, (r3-r1)+(r4-r3)+(r7-r6));
r8.other();
copyOperands(r8, r1, r3);
copyOperands(r8, r3, r4);
copyOperands(r8, r6, r7);
deleteF(matched, ctx, r0);
return true;
}
fail_5:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
if (!r0.isOpc(HW_CONCAT)) goto fail_6;
if (r0.getNumOperands() < 2) goto fail_6;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
for (r4 = r3;; r4++) {
if (r4 == r2) goto fail_6;
if ((*r4)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto cont_6_0;
if ((*r4)->as<ConstantRef>() != "0'"_bv) goto cont_6_0;
r5 = std::next(r4);
goto loop_6_0_break;
cont_6_0:;
loop_6_0_continue:;
}
loop_6_0_break:;
for (r6 = r5;; r6++) {
if (r6 == r2) break;
loop_6_1_continue:;
}
loop_6_1_break:;
if (r6 != r2) goto fail_6;
auto r7 = build.buildInstrRaw(HW_CONCAT, (r3-r1)+(r4-r3)+(r6-r5));
copyOperands(r7, r1, r3);
r7.other();
copyOperands(r7, r3, r4);
copyOperands(r7, r5, r6);
deleteF(matched, ctx, r0);
return true;
}
fail_6:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
if (!r0.isOpc(HW_CONCAT)) goto fail_7;
if (r0.getNumOperands() > 2) goto fail_7;
if (r0.getNumOperands() < 2) goto fail_7;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = std::next(r3);
replaceAllUses(replaced, r1, r3);
deleteF(matched, ctx, r0);
return true;
}
fail_7:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
FatDynObjRef<>* r10;
FatDynObjRef<>* r11;
if (!r0.isOpc(OP_TRUNC)) goto fail_8;
if (r0.getNumOperands() > 2) goto fail_8;
if (r0.getNumOperands() < 2) goto fail_8;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_8;
r4 = getDefInstr(r3);
if (!r4.isOpc(OP_ZEXT)) goto cont_8_0;
goto goto_8_0;
cont_8_0:;
if (!r4.isOpc(OP_SEXT)) goto cont_8_1;
goto goto_8_1;
cont_8_1:;
if (!r4.isOpc(OP_ANYEXT)) goto fail_8;
goto_8_1:;
goto_8_0:;
if (r4.getNumOperands() > 2) goto fail_8;
if (r4.getNumOperands() < 2) goto fail_8;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
r8 = std::next(r7);
r9 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_8;

  auto inBits = (*RefRange{r7, r8}.begin())->as<HWValue>().getNumBits();
  auto outBits = (*RefRange{r1, r3}.begin())->as<WireRef>().getNumBits();
  if (!(*outBits < *inBits))
    goto fail_8;
auto r12 = build.buildInstrRaw(OP_TRUNC, (r3-r1)+(r8-r7));
copyOperands(r12, r1, r3);
r12.other();
copyOperands(r12, r7, r8);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_8:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
FatDynObjRef<>* r10;
FatDynObjRef<>* r11;
if (!r0.isOpc(OP_TRUNC)) goto fail_9;
if (r0.getNumOperands() > 2) goto fail_9;
if (r0.getNumOperands() < 2) goto fail_9;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_9;
r4 = getDefInstr(r3);
if (!r4.isOpc(OP_ZEXT)) goto cont_9_0;
goto goto_9_0;
cont_9_0:;
if (!r4.isOpc(OP_SEXT)) goto cont_9_1;
goto goto_9_1;
cont_9_1:;
if (!r4.isOpc(OP_ANYEXT)) goto fail_9;
goto_9_1:;
goto_9_0:;
if (r4.getNumOperands() > 2) goto fail_9;
if (r4.getNumOperands() < 2) goto fail_9;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
r8 = std::next(r7);
r9 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_9;

  auto inBits = (*RefRange{r7, r8}.begin())->as<HWValue>().getNumBits();
  auto outBits = (*RefRange{r1, r3}.begin())->as<WireRef>().getNumBits();
  if (!(*outBits == *inBits))
    goto fail_9;
replaceAllUses(replaced, r1, r7);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_9:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
FatDynObjRef<>* r10;
FatDynObjRef<>* r11;
if (!r0.isOpc(OP_TRUNC)) goto fail_10;
if (r0.getNumOperands() > 2) goto fail_10;
if (r0.getNumOperands() < 2) goto fail_10;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_10;
r4 = getDefInstr(r3);
if (!r4.isOpc(OP_ZEXT)) goto cont_10_0;
goto goto_10_0;
cont_10_0:;
if (!r4.isOpc(OP_SEXT)) goto cont_10_1;
goto goto_10_1;
cont_10_1:;
if (!r4.isOpc(OP_ANYEXT)) goto fail_10;
goto_10_1:;
goto_10_0:;
if (r4.getNumOperands() > 2) goto fail_10;
if (r4.getNumOperands() < 2) goto fail_10;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
r8 = std::next(r7);
r9 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_10;

  auto inBits = (*RefRange{r7, r8}.begin())->as<HWValue>().getNumBits();
  auto outBits = (*RefRange{r1, r3}.begin())->as<WireRef>().getNumBits();
  if (!(*outBits > *inBits))
    goto fail_10;
auto r12 = build.buildInstrRaw(r4.getDialectOpcode(), (r3-r1)+(r8-r7));
copyOperands(r12, r1, r3);
r12.other();
copyOperands(r12, r7, r8);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_10:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
FatDynObjRef<>* r10;
FatDynObjRef<>* r11;
ConstantRef r13;
FatDynObjRef<>* r14;
FatDynObjRef<>* r15;
if (!r0.isOpc(OP_TRUNC)) goto fail_11;
if (r0.getNumOperands() > 2) goto fail_11;
if (r0.getNumOperands() < 2) goto fail_11;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_11;
r4 = getDefInstr(r3);
if (!r4.isOpc(HW_LOAD)) goto fail_11;
if (r4.getNumOperands() > 2) goto fail_11;
if (r4.getNumOperands() < 2) goto fail_11;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
r8 = std::next(r7);
r9 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_11;

  if ((*RefRange{r5, r7}.begin())->as<WireRef>().getNumUses() != 1)
    goto fail_11;

  FatDynObjRef<> c = ConstantRef::fromU32(*(*Range{r1, r3}.begin())->as<HWValue>().getNumBits());
  RefRange{r14, r15} = c;
auto r12 = build.buildInstrRaw(HW_LOAD, (r3-r1)+(r8-r7)+1+(r15-r14));
copyOperands(r12, r1, r3);
r12.other();
copyOperands(r12, r7, r8);
r13 = cbuild.val("32'h00000000"_bv).get();
copyOperand(r12, r13);
copyOperands(r12, r14, r15);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_11:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef::iterator r8;
InstrRef::iterator r9;
InstrRef::iterator r10;
InstrRef::iterator r11;
FatDynObjRef<>* r12;
FatDynObjRef<>* r13;
FatDynObjRef<>* r15;
FatDynObjRef<>* r16;
if (!r0.isOpc(OP_TRUNC)) goto fail_12;
if (r0.getNumOperands() > 2) goto fail_12;
if (r0.getNumOperands() < 2) goto fail_12;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_12;
r4 = getDefInstr(r3);
if (!r4.isOpc(HW_LOAD)) goto fail_12;
if (r4.getNumOperands() > 4) goto fail_12;
if (r4.getNumOperands() < 4) goto fail_12;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
r8 = std::next(r7);
r9 = std::next(r8);
r10 = std::next(r9);
r11 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_12;

  if ((*RefRange{r5, r7}.begin())->as<WireRef>().getNumUses() != 1)
    goto fail_12;

  FatDynObjRef<> c = ConstantRef::fromU32(*(*Range{r1, r3}.begin())->as<HWValue>().getNumBits());
  RefRange{r15, r16} = c;
auto r14 = build.buildInstrRaw(HW_LOAD, (r3-r1)+(r8-r7)+(r9-r8)+(r16-r15));
copyOperands(r14, r1, r3);
r14.other();
copyOperands(r14, r7, r8);
copyOperands(r14, r8, r9);
copyOperands(r14, r15, r16);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_12:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
InstrRef::iterator r5;
if (!r0.isOpc(HW_CONCAT)) goto fail_13;
if (r0.getNumOperands() > 3) goto fail_13;
if (r0.getNumOperands() < 3) goto fail_13;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_13;
if (!(*r3)->as<ConstantRef>().valueEqualsS("1'h0"_bv)) goto fail_13;
r4 = std::next(r3);
r5 = std::next(r4);
auto r6 = build.buildInstrRaw(OP_ZEXT, (r3-r1)+(r5-r4));
copyOperands(r6, r1, r3);
r6.other();
copyOperands(r6, r4, r5);
deleteF(matched, ctx, r0);
return true;
}
fail_13:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef r4;
InstrRef::iterator r5;
InstrRef::iterator r6;
InstrRef::iterator r7;
InstrRef r8;
InstrRef::iterator r9;
InstrRef::iterator r10;
InstrRef::iterator r11;
InstrRef::iterator r12;
InstrRef::iterator r13;
InstrRef::iterator r14;
InstrRef::iterator r15;
InstrRef::iterator r16;
InstrRef::iterator r17;
InstrRef::iterator r18;
if (!r0.isOpc(HW_CONCAT)) goto fail_14;
if (r0.getNumOperands() > 3) goto fail_14;
if (r0.getNumOperands() < 3) goto fail_14;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
r4 = getDefInstr(r3);
if (!r4.isOpc(HW_REPEAT)) goto fail_14;
if (r4.getNumOperands() < 2) goto fail_14;
r5 = r4.begin();
r6 = r4.end();
r7 = std::next(r5);
if ((*r7)->as<FatDynObjRef<>>().getType() != HW_WIRE) goto fail_14;
r8 = getDefInstr(r7);
if (!r8.isOpc(HW_SPLICE)) goto fail_14;
if (r8.getNumOperands() < 3) goto fail_14;
r9 = r8.begin();
r10 = r8.end();
r11 = std::next(r9);
r12 = std::next(r11);

  FatDynObjRef<> c = ConstantRef::fromU32(*(*Range{r11, r12}.begin())->as<HWValue>().getNumBits() - 1);
  if ((*Range{r12, std::next(r12)}.begin())->as<FatDynObjRef<>>() != c)
    goto fail_14;
r13 = std::next(r12);
if ((*r13)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_14;
if ((*r13)->as<ConstantRef>() != "32'h00000001"_bv) goto fail_14;
r14 = std::next(r13);
if (r14 != r10) goto fail_14;
r15 = std::next(r7);
if ((*r7)->fat() != (*r9)->fat()) goto fail_14;

  auto lhsBits = *(*Range{r1, r3}.begin())->as<HWValue>().getNumBits();
  auto rhsBits = *(*Range{r11, r12}.begin())->as<HWValue>().getNumBits();
  FatDynObjRef<> diff = ConstantRef::fromU32(lhsBits - rhsBits);
  if ((*Range{r15, std::next(r15)}.begin())->as<FatDynObjRef<>>() != diff)
    goto fail_14;
r16 = std::next(r15);
if (r16 != r6) goto fail_14;
r17 = std::next(r3);
if ((*r3)->fat() != (*r5)->fat()) goto fail_14;
r18 = std::next(r17);
if ((*r17)->fat() != (*r11)->fat()) goto fail_14;
auto r19 = build.buildInstrRaw(OP_SEXT, (r3-r1)+(r12-r11));
copyOperands(r19, r1, r3);
r19.other();
copyOperands(r19, r11, r12);
deleteIfSingleUse(matched, ctx, r8);
deleteIfSingleUse(matched, ctx, r4);
deleteF(matched, ctx, r0);
return true;
}
fail_14:;
{
InstrRef::iterator r1;
InstrRef::iterator r2;
InstrRef::iterator r3;
InstrRef::iterator r4;
FatDynObjRef<>* r5;
FatDynObjRef<>* r6;
if (!r0.isOpc(OP_TRUNC)) goto fail_15;
if (r0.getNumOperands() > 2) goto fail_15;
if (r0.getNumOperands() < 2) goto fail_15;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_15;
r4 = std::next(r3);

  FatDynObjRef<> temp = cbuild.val((*RefRange{r3, r4}.begin())->as<ConstantRef>()).resize(*(*RefRange{r1, r3}.begin())->as<WireRef>().getNumBits()).get();
  RefRange{r5, r6} = temp;
replaceAllUses(replaced, r1, r5);
deleteF(matched, ctx, r0);
return true;
}
fail_15:;

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
if (!r0.isOpc(HW_STORE)) goto fail_4;
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
auto r7 = build.buildInstrRaw(HW_STORE, (r3-r1)+(r4-r3));
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
if (!r4.isOpc(OP_ZEXT)) goto fail_8;
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
InstrRef::iterator r4;
FatDynObjRef<>* r5;
FatDynObjRef<>* r6;
if (!r0.isOpc(OP_TRUNC)) goto fail_9;
if (r0.getNumOperands() > 2) goto fail_9;
if (r0.getNumOperands() < 2) goto fail_9;
r1 = r0.begin();
r2 = r0.end();
r3 = std::next(r1);
if ((*r3)->as<FatDynObjRef<>>().getType() != CORE_CONSTANT) goto fail_9;
r4 = std::next(r3);

  FatDynObjRef<> temp = cbuild.val((*RefRange{r3, r4}.begin())->as<ConstantRef>()).resize(*(*RefRange{r1, r3}.begin())->as<WireRef>().getNumBits()).get();
  RefRange{r5, r6} = temp;
replaceAllUses(replaced, r1, r5);
deleteF(matched, ctx, r0);
return true;
}
fail_9:;

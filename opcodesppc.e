OPT MODULE

-> EEC/opcodesppc.e

/* EEC by Samuel Crow et al. [samuraileumas yahoo com] is Copyright (c)2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2009 */
/* Released under the ECX COMPILER LICENSE, See CompilerLicense.md */


-> moved ppc opcode stuff into this new module 2009

->EXPORT DEF g_codeptr:PTR TO LONG

/**************************************************************
***************************************************************
******************** PPC OPCODES ******************************
***************************************************************
**************************************************************/


PROC i6_5_5_5_10_1(heap,a,b,c,d,e,f)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,10) OR e,1) OR f
ENDPROC

PROC i6_5_5_5_10_1(heap,a,b,c,d,e,f,g)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,5) OR e,5) OR f,1) OR g
ENDPROC

PROC i6_5_5_5_1_9_1(heap,a,b,c,d,e,f,g)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,1) OR e,9) OR f,1) OR g
ENDPROC

PROC i6_5_5_16(heap,a,b,c,d)
   heap.codeptr[]++ := Shl(Shl(Shl(a,5) OR b,5) OR c,16) OR (d AND $FFFF)
ENDPROC

PROC i6_5_10_10_1(heap,a,b,c,d,e)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(a,5) OR b,10) OR c,10) OR d,1) OR e
ENDPROC

PROC i6_10_5_10_1(heap,a,b,c,d,e)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(a,10) OR b,5) OR c,10) OR d,1) OR e
ENDPROC

PROC i6_24_1_1(heap,a,b,c,d)
   heap.codeptr[]++ := Shl(Shl(Shl(a,24) OR (b AND $FFFFFF),1) OR c,1) OR d
ENDPROC

PROC i6_5_5_14_1_1(heap,a,b,c,d,e,f)
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,14) OR (d AND $3FFF),1) OR e,1) OR f
ENDPROC

PROC i6_5_5_5_5_6(heap,a,b,c,d,e,f) -> v48
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,5) OR e,6) OR f
ENDPROC

PROC i6_5_5_5_11(heap,a,b,c,d,e) -> v48
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,11) OR e
ENDPROC

PROC i6_5_5_5_1_10(heap,a,b,c,d,e,f) -> v48
   heap.codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,1) OR e,10) OR f
ENDPROC

->--------------------------------------------------------------

EXPORT PROC ppcadd(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,266,rc)
EXPORT PROC ppcaddc(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,10,rc)
EXPORT PROC ppcadde(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,138,rc)
EXPORT PROC ppcaddi(heap,d,a,simm) IS i6_5_5_16(heap,14,d,a,simm)
EXPORT PROC ppcaddic(heap,d,a,simm) IS i6_5_5_16(heap,12,d,a,simm)
EXPORT PROC ppcaddic_(heap,d,a,simm) IS i6_5_5_16(heap,13,d,a,simm)
EXPORT PROC ppcaddis(heap,d,a,simm) IS i6_5_5_16(heap,15,d,a,simm)
EXPORT PROC ppcaddme(heap,d,a,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,NIL,oe,234,rc)
EXPORT PROC ppcaddze(heap,d,a,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,NIL,oe,202,rc)
EXPORT PROC ppcand(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,28,rc)
EXPORT PROC ppcandc(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,60,rc)
EXPORT PROC ppcandi_(heap,s,a,uimm) IS i6_5_5_16(heap,28,s,a,uimm)
EXPORT PROC ppcandis_(heap,s,a,uimm) IS i6_5_5_16(heap,29,s,a,uimm)
EXPORT PROC ppcb(heap,li,aa,lk) IS i6_24_1_1(heap,18,li,aa,lk)
EXPORT PROC ppcbc(heap,bo,bl,bd,aa,lk) IS i6_5_5_14_1_1(heap,16,bo,bl,bd,aa,lk)
EXPORT PROC ppcbcctr(heap,bo,bl,lk) IS i6_5_5_5_10_1(heap,19,bo,bl,NIL,528,lk)
EXPORT PROC ppcbclr(heap,bo,bl,lk) IS i6_5_5_5_10_1(heap,19,bo,bl,NIL,16,lk)
EXPORT PROC ppccmp(heap,crf,l,a,b) IS i6_5_5_5_10_1(heap,31,Shl(crf,2) OR l,a,b,NIL,NIL)
EXPORT PROC ppccmpi(heap,crf,l,a,simm) IS i6_5_5_16(heap,11,Shl(crf,2) OR l,a,simm)
EXPORT PROC ppccmpl(heap,crf,l,a,b) IS i6_5_5_5_10_1(heap,31,Shl(crf,2) OR l,a,b,32,NIL)
EXPORT PROC ppccmpli(heap,crf,l,a,uimm) IS i6_5_5_16(heap,10,Shl(crf,2) OR l,a,uimm)
EXPORT PROC ppccntlzd(heap,s,a,rc) IS i6_5_5_5_10_1(heap,31,s,a,NIL,58,rc)
EXPORT PROC ppccntlzw(heap,s,a,rc) IS i6_5_5_5_10_1(heap,31,s,a,NIL,26,rc)
EXPORT PROC ppccrand(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,257,0)
EXPORT PROC ppccrandc(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,129,0)
EXPORT PROC ppccreqv(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,289,0)
EXPORT PROC ppccrnand(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,225,0)
EXPORT PROC ppccrnor(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,33,0)
EXPORT PROC ppccror(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,449,0)
EXPORT PROC ppccrorc(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,417,0)
EXPORT PROC ppccrxor(heap,crbD,crbA,crbB) IS i6_5_5_5_10_1(heap,19,crbD,crbA,crbB,193,0)
EXPORT PROC ppcdcba(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,758,0)
EXPORT PROC ppcdcbf(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,86,0)
EXPORT PROC ppcdcbi(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,470,0)
EXPORT PROC ppcdcbst(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,54,0)
EXPORT PROC ppcdcbt(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,278,0)
EXPORT PROC ppcdcbtst(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,246,0)
EXPORT PROC ppcdcbz(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,1014,0)
EXPORT PROC ppcdivd(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,489,rc)
EXPORT PROC ppcdivdu(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,457,rc)
EXPORT PROC ppcdivw(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,491,rc)
EXPORT PROC ppcdivwu(heap,d,a,b,oe,rc) IS i6_5_5_5_1_9_1(heap,31,d,a,b,oe,459,rc)
EXPORT PROC ppceciwx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,310,0)
EXPORT PROC ppcecowx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,438,0)
EXPORT PROC ppceieio(heap) IS i6_5_5_5_10_1(heap,31,NIL,NIL,NIL,854,0)
EXPORT PROC ppceqv(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,284,rc)
EXPORT PROC ppcextsb(heap,s,a,rc) IS i6_5_5_5_10_1(heap,31,s,a,NIL,954,rc)
EXPORT PROC ppcextsh(heap,s,a,rc) IS i6_5_5_5_10_1(heap,31,s,a,NIL,922,rc)
EXPORT PROC ppcextsw(heap,s,a,rc) IS i6_5_5_5_10_1(heap,31,s,a,NIL,986,rc)
EXPORT PROC ppcfabs(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,264,rc)
EXPORT PROC ppcfadd(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,21,rc)
EXPORT PROC ppcfadds(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,21,rc)
EXPORT PROC ppcfcfid(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,846,rc)
EXPORT PROC ppcfcmpo(crfD,a,b) IS i6_5_5_5_10_1(heap,63,Shl(crfD,2),a,b,32,0)
EXPORT PROC ppcfcmpu(crfD,a,b) IS i6_5_5_5_10_1(heap,63,Shl(crfD,2),a,b,0,0)
EXPORT PROC ppcfctid(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,814,rc)
EXPORT PROC ppcfctidz(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,815,rc)
EXPORT PROC ppcfctiw(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,14,rc)
EXPORT PROC ppcfctiwz(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,15,rc)
EXPORT PROC ppcfdiv(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,18,rc)
EXPORT PROC ppcfdivs(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,18,rc)
EXPORT PROC ppcfmadd(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,c,29,rc)
EXPORT PROC ppcfmadds(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,c,29,rc)
EXPORT PROC ppcfmr(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,72,rc)
EXPORT PROC ppcfmsub(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,c,28,rc)
EXPORT PROC ppcfmsubs(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,c,28,rc)
EXPORT PROC ppcfmul(heap,d,a,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,NIL,c,25,rc)
EXPORT PROC ppcfmuls(heap,d,a,c,rc) IS i6_5_5_5_10_1(heap,59,d,a,NIL,c,25,rc)
EXPORT PROC ppcfnabs(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,136,rc)
EXPORT PROC ppcfneg(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,40,rc)
EXPORT PROC ppcfnmadd(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,c,31,rc)
EXPORT PROC ppcfnmadds(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,c,31,rc)
EXPORT PROC ppcfnmsub(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,c,39,rc)
EXPORT PROC ppcfnmsubs(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,c,39,rc)
EXPORT PROC ppcfres(heap,d,b,rc) IS i6_5_5_5_10_1(heap,59,d,NIL,b,NIL,24,rc)
EXPORT PROC ppcfrsp(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,12,rc)
EXPORT PROC ppcfrsqrte(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,NIL,26,rc)
EXPORT PROC ppcfsel(heap,d,a,b,c,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,c,23,rc)
EXPORT PROC ppcfsqrt(heap,d,b,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,b,NIL,22,rc)
EXPORT PROC ppcfsqrts(heap,d,b,rc) IS i6_5_5_5_10_1(heap,59,d,NIL,b,NIL,22,rc)
EXPORT PROC ppcfsub(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,63,d,a,b,NIL,20,rc)
EXPORT PROC ppcfsubs(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,59,d,a,b,NIL,20,rc)
EXPORT PROC ppcicbi(heap,a,b) IS i6_5_5_5_10_1(heap,31,NIL,a,b,982,0)
EXPORT PROC ppcisync(heap) IS i6_5_5_5_10_1(heap,19,NIL,NIL,NIL,150,0)
EXPORT PROC ppclbz(heap,d,a,disp) IS i6_5_5_16(heap,34,d,a,disp)
EXPORT PROC ppclbzu(heap,d,a,disp) IS i6_5_5_16(heap,35,d,a,disp)
EXPORT PROC ppclbzux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,119,0)
EXPORT PROC ppclbzx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,87,0)
EXPORT PROC ppcld(heap,d,a,ds) IS i6_5_5_16(heap,58,d,a,Shl(ds,2))
EXPORT PROC ppcldarx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,84,0)
EXPORT PROC ppcldu(heap,d,a,ds) IS i6_5_5_16(heap,58,d,a,Shl(ds,2) OR 1)
EXPORT PROC ppcldux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,53,0)
EXPORT PROC ppcldx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,21,0)
EXPORT PROC ppclfd(heap,d,a,disp) IS i6_5_5_16(heap,50,d,a,disp)
EXPORT PROC ppclfdu(heap,d,a,disp) IS i6_5_5_16(heap,51,d,a,disp)
EXPORT PROC ppclfdux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,631,0)
EXPORT PROC ppclfdx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,599,0)
EXPORT PROC ppclfs(heap,d,a,disp) IS i6_5_5_16(heap,48,d,a,disp)
EXPORT PROC ppclfsu(heap,d,a,disp) IS i6_5_5_16(heap,49,d,a,disp)
EXPORT PROC ppclfsux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,567,0)
EXPORT PROC ppclfsx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,535,0)
EXPORT PROC ppclha(heap,d,a,disp) IS i6_5_5_16(heap,42,d,a,disp)
EXPORT PROC ppclhau(heap,d,a,disp) IS i6_5_5_16(heap,43,d,a,disp)
EXPORT PROC ppclhaux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,375,0)
EXPORT PROC ppclhax(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,343,0)
EXPORT PROC ppclhbrx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,790,0)
EXPORT PROC ppclhz(heap,d,a,disp) IS i6_5_5_16(heap,40,d,a,disp)
EXPORT PROC ppclhzu(heap,d,a,disp) IS i6_5_5_16(heap,41,d,a,disp)
EXPORT PROC ppclhzux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,311,0)
EXPORT PROC ppclhzx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,279,0)
EXPORT PROC ppclmw(heap,d,a,disp) IS i6_5_5_16(heap,46,d,a,disp)
EXPORT PROC ppclswi(heap,d,a,nb) IS i6_5_5_5_10_1(heap,31,d,a,nb,597,0)
EXPORT PROC ppclswx(heap,d,a,nb) IS i6_5_5_5_10_1(heap,31,d,a,nb,533,0)
EXPORT PROC ppclwa(heap,d,a,ds) IS i6_5_5_16(heap,58,d,a,Shl(ds,2))
EXPORT PROC ppclwarx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,20,0)
EXPORT PROC ppclwaux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,373,0)
EXPORT PROC ppclwax(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,341,0)
EXPORT PROC ppclwbrx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,534,0)
EXPORT PROC ppclwz(heap,d,a,disp) IS i6_5_5_16(heap,32,d,a,disp)
EXPORT PROC ppclwzu(heap,d,a,disp) IS i6_5_5_16(heap,33,d,a,disp)
EXPORT PROC ppclwzux(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,55,0)
EXPORT PROC ppclwzx(heap,d,a,b) IS i6_5_5_5_10_1(heap,31,d,a,b,23,0)
EXPORT PROC ppcmcrf(heap,crfD,crfS) IS i6_5_5_5_10_1(heap,19,Shl(crfD,2),Shl(crfS,2),NIL,NIL,0)
EXPORT PROC ppcmcrfs(heap,crfD,crfS) IS i6_5_5_5_10_1(heap,63,Shl(crfD,2),Shl(crfS,2),NIL,64,0)
EXPORT PROC ppcmcrxr(heap,crfD) IS i6_5_5_5_10_1(heap,31,Shl(crfD,2),NIL,NIL,512,0)
EXPORT PROC ppcmfcr(heap,d) IS i6_5_5_5_10_1(heap,31,d,NIL,NIL,19,0)
EXPORT PROC ppcmffs(heap,d,rc) IS i6_5_5_5_10_1(heap,63,d,NIL,NIL,583,rc)
EXPORT PROC ppcmfmsr(heap,d) IS i6_5_5_5_10_1(heap,31,d,NIL,NIL,83,0)
EXPORT PROC ppcmfspr(heap,d,spr) IS i6_5_10_10_1(heap,31,d,Shl(spr,5),339,0)
EXPORT PROC ppcmfsr(heap,d,sr) IS i6_5_5_5_10_1(heap,31,d,sr,NIL,595,0)
EXPORT PROC ppcmfsrin(heap,d,b) IS i6_5_5_5_10_1(heap,31,d,NIL,b,659,0)
EXPORT PROC ppcmftb(heap,d,tbr) IS i6_5_10_10_1(heap,31,d,tbr,371,0)
EXPORT PROC ppcmtcrf(heap,s,crm) IS i6_5_10_10_1(heap,31,s,Shl(crm,1),144,0)
EXPORT PROC ppcmtfsb0(heap,crbD,rc) IS i6_5_5_5_10_1(heap,63,crbD,NIL,NIL,70,rc)
EXPORT PROC ppcmtfsb1(heap,crbD,rc) IS i6_5_5_5_10_1(heap,63,crbD,NIL,NIL,38,rc)
EXPORT PROC ppcmtfsf(heap,fm,b,rc) IS i6_10_5_10_1(heap,63,Shl(fm,1),b,711,rc)
EXPORT PROC ppcmtfsfi(heap,crfD,imm,rc) IS i6_5_5_5_10_1(heap,63,Shl(crfD,2),NIL,Shl(imm,1),134,rc)
EXPORT PROC ppcmtmsr(heap,s) IS i6_5_5_5_10_1(heap,31,s,NIL,NIL,146,0)
EXPORT PROC ppcmtspr(heap,s,spr) IS i6_5_10_10_1(heap,31,s,Shl(spr,5),467,0)
EXPORT PROC ppcmtsr(heap,s,sr) IS i6_5_5_5_10_1(heap,31,s,sr,NIL,210,0)
EXPORT PROC ppcmtsrin(heap,s,b) IS i6_5_5_5_10_1(heap,31,s,NIL,b,242,0)
EXPORT PROC ppcmulhd(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,73,rc)
EXPORT PROC ppcmulhdu(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,9,rc)
EXPORT PROC ppcmulhw(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,75,rc)
EXPORT PROC ppcmulhwu(heap,d,a,b,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,11,rc)
EXPORT PROC ppcmulld(heap,d,a,b,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,Shl(oe,9) OR 233,rc)
EXPORT PROC ppcmulli(heap,d,a,simm) IS i6_5_5_16(heap,7,d,a,simm)
EXPORT PROC ppcmullw(heap,d,a,b,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,Shl(oe,9) OR 235,rc)
EXPORT PROC ppcnand(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,476,rc)
EXPORT PROC ppcneg(heap,d,a,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,NIL,Shl(oe,9) OR 104,rc)
EXPORT PROC ppcnor(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,124,rc)
EXPORT PROC ppcor(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,444,rc)
EXPORT PROC ppcorc(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,412,rc)
EXPORT PROC ppcori(heap,s,a,uimm) IS i6_5_5_16(heap,24,s,a,uimm)
EXPORT PROC ppcoris(heap,s,a,uimm) IS i6_5_5_16(heap,25,s,a,uimm)
EXPORT PROC ppcrfi(heap) IS i6_5_5_5_10_1(heap,19,NIL,NIL,NIL,50,0)
EXPORT PROC ppcrldcl(heap,s,a,b,mb,rc) IS i6_5_5_5_10_1(heap,30,s,a,b,Shl(mb,4) OR 8,rc)
EXPORT PROC ppcrldcr(heap,s,a,b,me,rc) IS i6_5_5_5_10_1(heap,30,s,a,b,Shl(me,4) OR 9,rc)
EXPORT PROC ppcrldic(heap,s,a,sh,mb,rc) IS i6_5_5_5_10_1(heap,30,s,a,sh,Shl(Shl(mb,3) OR 2,1) OR sh,rc)
EXPORT PROC ppcrldicl(heap,s,a,sh,mb,rc) IS i6_5_5_5_10_1(heap,30,s,a,sh,Shl(Shl(mb,3) OR 0,1) OR sh,rc)
EXPORT PROC ppcrldicr(heap,s,a,sh,me,rc) IS i6_5_5_5_10_1(heap,30,s,a,sh,Shl(Shl(me,3) OR 1,1) OR sh,rc)
EXPORT PROC ppcrldimi(heap,s,a,sh,mb,rc) IS i6_5_5_5_10_1(heap,30,s,a,sh,Shl(Shl(mb,3) OR 3,1) OR sh,rc)
EXPORT PROC ppcrlwimi(heap,s,a,sh,mb,me,rc) IS i6_5_5_5_10_1(heap,20,s,a,sh,Shl(mb,5) OR me,rc)
EXPORT PROC ppcrlwinm(heap,s,a,sh,mb,me,rc) IS i6_5_5_5_10_1(heap,21,s,a,sh,Shl(mb,5) OR me,rc)
EXPORT PROC ppcrlwnm(heap,s,a,b,mb,me,rc) IS i6_5_5_5_10_1(heap,23,s,a,b,Shl(mb,5) OR me,rc)
EXPORT PROC ppcsc(heap) IS i6_5_5_16(heap,17,NIL,NIL,2)
EXPORT PROC ppcslbia(heap) IS i6_5_5_5_10_1(heap,31,NIL,NIL,NIL,498,0)
EXPORT PROC ppcslbie(heap,b) IS i6_5_5_5_10_1(heap,31,NIL,NIL,b,434,0)
EXPORT PROC ppcsld(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,27,rc)
EXPORT PROC ppcslw(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,24,rc)
EXPORT PROC ppcsrad(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,794,rc)
EXPORT PROC ppcsradi(heap,s,a,sh,rc) IS i6_5_5_5_10_1(heap,31,s,a,sh,Shl(413,1) OR sh,rc)
EXPORT PROC ppcsraw(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,792,rc)
EXPORT PROC ppcsrawi(heap,s,a,sh,rc) IS i6_5_5_5_10_1(heap,31,s,a,sh,824,rc)
EXPORT PROC ppcsrd(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,539,rc)
EXPORT PROC ppcsrw(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,536,rc)
EXPORT PROC ppcstb(heap,s,a,disp) IS i6_5_5_16(heap,38,s,a,disp)
EXPORT PROC ppcstbu(heap,s,a,disp) IS i6_5_5_16(heap,39,s,a,disp)
EXPORT PROC ppcstbux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,247,0)
EXPORT PROC ppcstbx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,215,0)
EXPORT PROC ppcstd(heap,s,a,ds) IS i6_5_5_16(heap,62,s,a,Shl(ds,2))
EXPORT PROC ppcstdcx_(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,214,1)
EXPORT PROC ppcstdu(heap,s,a,ds) IS i6_5_5_16(heap,62,s,a,Shl(ds,2) OR 1)
EXPORT PROC ppcstdux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,181,0)
EXPORT PROC ppcstdx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,149,0)
EXPORT PROC ppcstfd(heap,s,a,disp) IS i6_5_5_16(heap,54,s,a,disp)
EXPORT PROC ppcstfdu(heap,s,a,disp) IS i6_5_5_16(heap,55,s,a,disp)
EXPORT PROC ppcstfdux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,759,0)
EXPORT PROC ppcstfdx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,727,0)
EXPORT PROC ppcstfiwx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,983,0)
EXPORT PROC ppcstfs(heap,s,a,disp) IS i6_5_5_16(heap,52,s,a,disp)
EXPORT PROC ppcstfsu(heap,s,a,disp) IS i6_5_5_16(heap,53,s,a,disp)
EXPORT PROC ppcstfsux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,695,0)
EXPORT PROC ppcstfsx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,663,0)
EXPORT PROC ppcsth(heap,s,a,disp) IS i6_5_5_16(heap,44,s,a,disp)
EXPORT PROC ppcsthbrx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,918,0)
EXPORT PROC ppcsthu(heap,s,a,disp) IS i6_5_5_16(heap,45,s,a,disp)
EXPORT PROC ppcsthux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,439,0)
EXPORT PROC ppcsthx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,407,0)
EXPORT PROC ppcstmw(heap,s,a,disp) IS i6_5_5_16(heap,47,s,a,disp)
EXPORT PROC ppcstswi(heap,s,a,nb) IS i6_5_5_5_10_1(heap,31,s,a,nb,725,0)
EXPORT PROC ppcstswx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,661,0)
EXPORT PROC ppcstw(heap,s,a,disp) IS i6_5_5_16(heap,36,s,a,disp)
EXPORT PROC ppcstwbrx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,662,0)
EXPORT PROC ppcstwcx_(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,150,1)
EXPORT PROC ppcstwu(heap,s,a,disp) IS i6_5_5_16(heap,37,s,a,disp)
EXPORT PROC ppcstwux(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,183,0)
EXPORT PROC ppcstwx(heap,s,a,b) IS i6_5_5_5_10_1(heap,31,s,a,b,151,0)
EXPORT PROC ppcsubf(heap,d,a,b,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,Shl(oe,9) OR 40,rc)
EXPORT PROC ppcsubfc(heap,d,a,b,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,Shl(oe,9) OR 8,rc)
EXPORT PROC ppcsubfe(heap,d,a,b,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,b,Shl(oe,9) OR 136,rc)
EXPORT PROC ppcsubfic(heap,d,a,simm) IS i6_5_5_16(heap,8,d,a,simm)
EXPORT PROC ppcsubfme(heap,d,a,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,NIL,Shl(oe,9) OR 232,rc)
EXPORT PROC ppcsubfze(heap,d,a,oe,rc) IS i6_5_5_5_10_1(heap,31,d,a,NIL,Shl(oe,9) OR 200,rc)
EXPORT PROC ppcsync(heap) IS i6_5_5_5_10_1(heap,31,NIL,NIL,NIL,598,0)
EXPORT PROC ppctd(heap,to,a,b) IS i6_5_5_5_10_1(heap,31,to,a,b,68,0)
EXPORT PROC ppctdi(heap,to,a,simm) IS i6_5_5_16(heap,2,to,a,simm)
EXPORT PROC ppctlbia(heap) IS i6_5_5_5_10_1(heap,31,NIL,NIL,NIL,370,0)
EXPORT PROC ppctlbie(heap,b) IS i6_5_5_5_10_1(heap,31,NIL,NIL,b,306,0)
EXPORT PROC ppctlbsync(heap) IS i6_5_5_5_10_1(heap,31,NIL,NIL,NIL,566,0)
EXPORT PROC ppctw(heap,to,a,b) IS i6_5_5_5_10_1(heap,31,to,a,b,4,0)
EXPORT PROC ppctwi(heap,to,a,simm) IS i6_5_5_16(heap,3,to,a,simm)
EXPORT PROC ppcxor(heap,s,a,b,rc) IS i6_5_5_5_10_1(heap,31,s,a,b,316,rc)
EXPORT PROC ppcxori(heap,s,a,uimm) IS i6_5_5_16(heap,26,s,a,uimm)
EXPORT PROC ppcxoris(heap,s,a,uimm) IS i6_5_5_16(heap,27,s,a,uimm)

-> Altivec
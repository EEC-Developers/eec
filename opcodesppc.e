OPT MODULE

-> ECX/opcodesppc.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2009 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */


-> moved ppc opcode stuff into this new module 2009

EXPORT DEF g_codeptr:PTR TO LONG

/**************************************************************
***************************************************************
******************** PPC OPCODES ******************************
***************************************************************
**************************************************************/


PROC i6_5_5_5_10_1(a,b,c,d,e,f)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,10) OR e,1) OR f
ENDPROC

PROC i6_5_5_5_5_5_1(a,b,c,d,e,f,g)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,5) OR e,5) OR f,1) OR g
ENDPROC

PROC i6_5_5_5_1_9_1(a,b,c,d,e,f,g)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,1) OR e,9) OR f,1) OR g
ENDPROC

PROC i6_5_5_16(a,b,c,d)
   g_codeptr[]++ := Shl(Shl(Shl(a,5) OR b,5) OR c,16) OR (d AND $FFFF)
ENDPROC

PROC i6_5_10_10_1(a,b,c,d,e)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(a,5) OR b,10) OR c,10) OR d,1) OR e
ENDPROC

PROC i6_10_5_10_1(a,b,c,d,e)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(a,10) OR b,5) OR c,10) OR d,1) OR e
ENDPROC

PROC i6_24_1_1(a,b,c,d)
   g_codeptr[]++ := Shl(Shl(Shl(a,24) OR (b AND $FFFFFF),1) OR c,1) OR d
ENDPROC

PROC i6_5_5_14_1_1(a,b,c,d,e,f)
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,14) OR (d AND $3FFF),1) OR e,1) OR f
ENDPROC

PROC i6_5_5_5_5_6(a,b,c,d,e,f) -> v48
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,5) OR e,6) OR f
ENDPROC

PROC i6_5_5_5_11(a,b,c,d,e) -> v48
   g_codeptr[]++ := Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,11) OR e
ENDPROC

PROC i6_5_5_5_1_10(a,b,c,d,e,f) -> v48
   g_codeptr[]++ := Shl(Shl(Shl(Shl(Shl(a,5) OR b,5) OR c,5) OR d,1) OR e,10) OR f
ENDPROC

->--------------------------------------------------------------

EXPORT PROC ppcadd(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,266,rc)
EXPORT PROC ppcaddc(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,10,rc)
EXPORT PROC ppcadde(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,138,rc)
EXPORT PROC ppcaddi(d,a,simm) IS i6_5_5_16(14,d,a,simm)
EXPORT PROC ppcaddic(d,a,simm) IS i6_5_5_16(12,d,a,simm)
EXPORT PROC ppcaddic_(d,a,simm) IS i6_5_5_16(13,d,a,simm)
EXPORT PROC ppcaddis(d,a,simm) IS i6_5_5_16(15,d,a,simm)
EXPORT PROC ppcaddme(d,a,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,NIL,oe,234,rc)
EXPORT PROC ppcaddze(d,a,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,NIL,oe,202,rc)
EXPORT PROC ppcand(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,28,rc)
EXPORT PROC ppcandc(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,60,rc)
EXPORT PROC ppcandi_(s,a,uimm) IS i6_5_5_16(28,s,a,uimm)
EXPORT PROC ppcandis_(s,a,uimm) IS i6_5_5_16(29,s,a,uimm)
EXPORT PROC ppcb(li,aa,lk) IS i6_24_1_1(18,li,aa,lk)
EXPORT PROC ppcbc(bo,bl,bd,aa,lk) IS i6_5_5_14_1_1(16,bo,bl,bd,aa,lk)
EXPORT PROC ppcbcctr(bo,bl,lk) IS i6_5_5_5_10_1(19,bo,bl,NIL,528,lk)
EXPORT PROC ppcbclr(bo,bl,lk) IS i6_5_5_5_10_1(19,bo,bl,NIL,16,lk)
EXPORT PROC ppccmp(crf,l,a,b) IS i6_5_5_5_10_1(31,Shl(crf,2) OR l,a,b,NIL,NIL)
EXPORT PROC ppccmpi(crf,l,a,simm) IS i6_5_5_16(11,Shl(crf,2) OR l,a,simm)
EXPORT PROC ppccmpl(crf,l,a,b) IS i6_5_5_5_10_1(31,Shl(crf,2) OR l,a,b,32,NIL)
EXPORT PROC ppccmpli(crf,l,a,uimm) IS i6_5_5_16(10,Shl(crf,2) OR l,a,uimm)
EXPORT PROC ppccntlzd(s,a,rc) IS i6_5_5_5_10_1(31,s,a,NIL,58,rc)
EXPORT PROC ppccntlzw(s,a,rc) IS i6_5_5_5_10_1(31,s,a,NIL,26,rc)
EXPORT PROC ppccrand(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,257,0)
EXPORT PROC ppccrandc(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,129,0)
EXPORT PROC ppccreqv(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,289,0)
EXPORT PROC ppccrnand(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,225,0)
EXPORT PROC ppccrnor(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,33,0)
EXPORT PROC ppccror(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,449,0)
EXPORT PROC ppccrorc(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,417,0)
EXPORT PROC ppccrxor(crbD,crbA,crbB) IS i6_5_5_5_10_1(19,crbD,crbA,crbB,193,0)
EXPORT PROC ppcdcba(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,758,0)
EXPORT PROC ppcdcbf(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,86,0)
EXPORT PROC ppcdcbi(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,470,0)
EXPORT PROC ppcdcbst(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,54,0)
EXPORT PROC ppcdcbt(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,278,0)
EXPORT PROC ppcdcbtst(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,246,0)
EXPORT PROC ppcdcbz(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,1014,0)
EXPORT PROC ppcdivd(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,489,rc)
EXPORT PROC ppcdivdu(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,457,rc)
EXPORT PROC ppcdivw(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,491,rc)
EXPORT PROC ppcdivwu(d,a,b,oe,rc) IS i6_5_5_5_1_9_1(31,d,a,b,oe,459,rc)
EXPORT PROC ppceciwx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,310,0)
EXPORT PROC ppcecowx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,438,0)
EXPORT PROC ppceieio() IS i6_5_5_5_10_1(31,NIL,NIL,NIL,854,0)
EXPORT PROC ppceqv(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,284,rc)
EXPORT PROC ppcextsb(s,a,rc) IS i6_5_5_5_10_1(31,s,a,NIL,954,rc)
EXPORT PROC ppcextsh(s,a,rc) IS i6_5_5_5_10_1(31,s,a,NIL,922,rc)
EXPORT PROC ppcextsw(s,a,rc) IS i6_5_5_5_10_1(31,s,a,NIL,986,rc)
EXPORT PROC ppcfabs(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,264,rc)
EXPORT PROC ppcfadd(d,a,b,rc) IS i6_5_5_5_10_1(63,d,a,b,21,rc)
EXPORT PROC ppcfadds(d,a,b,rc) IS i6_5_5_5_10_1(59,d,a,b,21,rc)
EXPORT PROC ppcfcfid(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,846,rc)
EXPORT PROC ppcfcmpo(crfD,a,b) IS i6_5_5_5_10_1(63,Shl(crfD,2),a,b,32,0)
EXPORT PROC ppcfcmpu(crfD,a,b) IS i6_5_5_5_10_1(63,Shl(crfD,2),a,b,0,0)
EXPORT PROC ppcfctid(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,814,rc)
EXPORT PROC ppcfctidz(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,815,rc)
EXPORT PROC ppcfctiw(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,14,rc)
EXPORT PROC ppcfctiwz(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,15,rc)
EXPORT PROC ppcfdiv(d,a,b,rc) IS i6_5_5_5_10_1(63,d,a,b,18,rc)
EXPORT PROC ppcfdivs(d,a,b,rc) IS i6_5_5_5_10_1(59,d,a,b,18,rc)
EXPORT PROC ppcfmadd(d,a,b,c,rc) IS i6_5_5_5_5_5_1(63,d,a,b,c,29,rc)
EXPORT PROC ppcfmadds(d,a,b,c,rc) IS i6_5_5_5_5_5_1(59,d,a,b,c,29,rc)
EXPORT PROC ppcfmr(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,72,rc)
EXPORT PROC ppcfmsub(d,a,b,c,rc) IS i6_5_5_5_5_5_1(63,d,a,b,c,28,rc)
EXPORT PROC ppcfmsubs(d,a,b,c,rc) IS i6_5_5_5_5_5_1(59,d,a,b,c,28,rc)
EXPORT PROC ppcfmul(d,a,c,rc) IS i6_5_5_5_5_5_1(63,d,a,NIL,c,25,rc)
EXPORT PROC ppcfmuls(d,a,c,rc) IS i6_5_5_5_5_5_1(59,d,a,NIL,c,25,rc)
EXPORT PROC ppcfnabs(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,136,rc)
EXPORT PROC ppcfneg(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,40,rc)
EXPORT PROC ppcfnmadd(d,a,b,c,rc) IS i6_5_5_5_5_5_1(63,d,a,b,c,31,rc)
EXPORT PROC ppcfnmadds(d,a,b,c,rc) IS i6_5_5_5_5_5_1(59,d,a,b,c,31,rc)
EXPORT PROC ppcfnmsub(d,a,b,c,rc) IS i6_5_5_5_5_5_1(63,d,a,b,c,39,rc)
EXPORT PROC ppcfnmsubs(d,a,b,c,rc) IS i6_5_5_5_5_5_1(59,d,a,b,c,39,rc)
EXPORT PROC ppcfres(d,b,rc) IS i6_5_5_5_5_5_1(59,d,NIL,b,NIL,24,rc)
EXPORT PROC ppcfrsp(d,b,rc) IS i6_5_5_5_10_1(63,d,NIL,b,12,rc)
EXPORT PROC ppcfrsqrte(d,b,rc) IS i6_5_5_5_5_5_1(63,d,NIL,b,NIL,26,rc)
EXPORT PROC ppcfsel(d,a,b,c,rc) IS i6_5_5_5_5_5_1(63,d,a,b,c,23,rc)
EXPORT PROC ppcfsqrt(d,b,rc) IS i6_5_5_5_5_5_1(63,d,NIL,b,NIL,22,rc)
EXPORT PROC ppcfsqrts(d,b,rc) IS i6_5_5_5_5_5_1(59,d,NIL,b,NIL,22,rc)
EXPORT PROC ppcfsub(d,a,b,rc) IS i6_5_5_5_5_5_1(63,d,a,b,NIL,20,rc)
EXPORT PROC ppcfsubs(d,a,b,rc) IS i6_5_5_5_5_5_1(59,d,a,b,NIL,20,rc)
EXPORT PROC ppcicbi(a,b) IS i6_5_5_5_10_1(31,NIL,a,b,982,0)
EXPORT PROC ppcisync() IS i6_5_5_5_10_1(19,NIL,NIL,NIL,150,0)
EXPORT PROC ppclbz(d,a,disp) IS i6_5_5_16(34,d,a,disp)
EXPORT PROC ppclbzu(d,a,disp) IS i6_5_5_16(35,d,a,disp)
EXPORT PROC ppclbzux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,119,0)
EXPORT PROC ppclbzx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,87,0)
EXPORT PROC ppcld(d,a,ds) IS i6_5_5_16(58,d,a,Shl(ds,2))
EXPORT PROC ppcldarx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,84,0)
EXPORT PROC ppcldu(d,a,ds) IS i6_5_5_16(58,d,a,Shl(ds,2) OR 1)
EXPORT PROC ppcldux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,53,0)
EXPORT PROC ppcldx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,21,0)
EXPORT PROC ppclfd(d,a,disp) IS i6_5_5_16(50,d,a,disp)
EXPORT PROC ppclfdu(d,a,disp) IS i6_5_5_16(51,d,a,disp)
EXPORT PROC ppclfdux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,631,0)
EXPORT PROC ppclfdx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,599,0)
EXPORT PROC ppclfs(d,a,disp) IS i6_5_5_16(48,d,a,disp)
EXPORT PROC ppclfsu(d,a,disp) IS i6_5_5_16(49,d,a,disp)
EXPORT PROC ppclfsux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,567,0)
EXPORT PROC ppclfsx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,535,0)
EXPORT PROC ppclha(d,a,disp) IS i6_5_5_16(42,d,a,disp)
EXPORT PROC ppclhau(d,a,disp) IS i6_5_5_16(43,d,a,disp)
EXPORT PROC ppclhaux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,375,0)
EXPORT PROC ppclhax(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,343,0)
EXPORT PROC ppclhbrx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,790,0)
EXPORT PROC ppclhz(d,a,disp) IS i6_5_5_16(40,d,a,disp)
EXPORT PROC ppclhzu(d,a,disp) IS i6_5_5_16(41,d,a,disp)
EXPORT PROC ppclhzux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,311,0)
EXPORT PROC ppclhzx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,279,0)
EXPORT PROC ppclmw(d,a,disp) IS i6_5_5_16(46,d,a,disp)
EXPORT PROC ppclswi(d,a,nb) IS i6_5_5_5_10_1(31,d,a,nb,597,0)
EXPORT PROC ppclswx(d,a,nb) IS i6_5_5_5_10_1(31,d,a,nb,533,0)
EXPORT PROC ppclwa(d,a,ds) IS i6_5_5_16(58,d,a,Shl(ds,2))
EXPORT PROC ppclwarx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,20,0)
EXPORT PROC ppclwaux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,373,0)
EXPORT PROC ppclwax(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,341,0)
EXPORT PROC ppclwbrx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,534,0)
EXPORT PROC ppclwz(d,a,disp) IS i6_5_5_16(32,d,a,disp)
EXPORT PROC ppclwzu(d,a,disp) IS i6_5_5_16(33,d,a,disp)
EXPORT PROC ppclwzux(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,55,0)
EXPORT PROC ppclwzx(d,a,b) IS i6_5_5_5_10_1(31,d,a,b,23,0)
EXPORT PROC ppcmcrf(crfD,crfS) IS i6_5_5_5_10_1(19,Shl(crfD,2),Shl(crfS,2),NIL,NIL,0)
EXPORT PROC ppcmcrfs(crfD,crfS) IS i6_5_5_5_10_1(63,Shl(crfD,2),Shl(crfS,2),NIL,64,0)
EXPORT PROC ppcmcrxr(crfD) IS i6_5_5_5_10_1(31,Shl(crfD,2),NIL,NIL,512,0)
EXPORT PROC ppcmfcr(d) IS i6_5_5_5_10_1(31,d,NIL,NIL,19,0)
EXPORT PROC ppcmffs(d,rc) IS i6_5_5_5_10_1(63,d,NIL,NIL,583,rc)
EXPORT PROC ppcmfmsr(d) IS i6_5_5_5_10_1(31,d,NIL,NIL,83,0)
EXPORT PROC ppcmfspr(d,spr) IS i6_5_10_10_1(31,d,Shl(spr,5),339,0)
EXPORT PROC ppcmfsr(d,sr) IS i6_5_5_5_10_1(31,d,sr,NIL,595,0)
EXPORT PROC ppcmfsrin(d,b) IS i6_5_5_5_10_1(31,d,NIL,b,659,0)
EXPORT PROC ppcmftb(d,tbr) IS i6_5_10_10_1(31,d,tbr,371,0)
EXPORT PROC ppcmtcrf(s,crm) IS i6_5_10_10_1(31,s,Shl(crm,1),144,0)
EXPORT PROC ppcmtfsb0(crbD,rc) IS i6_5_5_5_10_1(63,crbD,NIL,NIL,70,rc)
EXPORT PROC ppcmtfsb1(crbD,rc) IS i6_5_5_5_10_1(63,crbD,NIL,NIL,38,rc)
EXPORT PROC ppcmtfsf(fm,b,rc) IS i6_10_5_10_1(63,Shl(fm,1),b,711,rc)
EXPORT PROC ppcmtfsfi(crfD,imm,rc) IS i6_5_5_5_10_1(63,Shl(crfD,2),NIL,Shl(imm,1),134,rc)
EXPORT PROC ppcmtmsr(s) IS i6_5_5_5_10_1(31,s,NIL,NIL,146,0)
EXPORT PROC ppcmtspr(s,spr) IS i6_5_10_10_1(31,s,Shl(spr,5),467,0)
EXPORT PROC ppcmtsr(s,sr) IS i6_5_5_5_10_1(31,s,sr,NIL,210,0)
EXPORT PROC ppcmtsrin(s,b) IS i6_5_5_5_10_1(31,s,NIL,b,242,0)
EXPORT PROC ppcmulhd(d,a,b,rc) IS i6_5_5_5_10_1(31,d,a,b,73,rc)
EXPORT PROC ppcmulhdu(d,a,b,rc) IS i6_5_5_5_10_1(31,d,a,b,9,rc)
EXPORT PROC ppcmulhw(d,a,b,rc) IS i6_5_5_5_10_1(31,d,a,b,75,rc)
EXPORT PROC ppcmulhwu(d,a,b,rc) IS i6_5_5_5_10_1(31,d,a,b,11,rc)
EXPORT PROC ppcmulld(d,a,b,oe,rc) IS i6_5_5_5_10_1(31,d,a,b,Shl(oe,9) OR 233,rc)
EXPORT PROC ppcmulli(d,a,simm) IS i6_5_5_16(7,d,a,simm)
EXPORT PROC ppcmullw(d,a,b,oe,rc) IS i6_5_5_5_10_1(31,d,a,b,Shl(oe,9) OR 235,rc)
EXPORT PROC ppcnand(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,476,rc)
EXPORT PROC ppcneg(d,a,oe,rc) IS i6_5_5_5_10_1(31,d,a,NIL,Shl(oe,9) OR 104,rc)
EXPORT PROC ppcnor(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,124,rc)
EXPORT PROC ppcor(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,444,rc)
EXPORT PROC ppcorc(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,412,rc)
EXPORT PROC ppcori(s,a,uimm) IS i6_5_5_16(24,s,a,uimm)
EXPORT PROC ppcoris(s,a,uimm) IS i6_5_5_16(25,s,a,uimm)
EXPORT PROC ppcrfi() IS i6_5_5_5_10_1(19,NIL,NIL,NIL,50,0)
EXPORT PROC ppcrldcl(s,a,b,mb,rc) IS i6_5_5_5_10_1(30,s,a,b,Shl(mb,4) OR 8,rc)
EXPORT PROC ppcrldcr(s,a,b,me,rc) IS i6_5_5_5_10_1(30,s,a,b,Shl(me,4) OR 9,rc)
EXPORT PROC ppcrldic(s,a,sh,mb,rc) IS i6_5_5_5_10_1(30,s,a,sh,Shl(Shl(mb,3) OR 2,1) OR sh,rc)
EXPORT PROC ppcrldicl(s,a,sh,mb,rc) IS i6_5_5_5_10_1(30,s,a,sh,Shl(Shl(mb,3) OR 0,1) OR sh,rc)
EXPORT PROC ppcrldicr(s,a,sh,me,rc) IS i6_5_5_5_10_1(30,s,a,sh,Shl(Shl(me,3) OR 1,1) OR sh,rc)
EXPORT PROC ppcrldimi(s,a,sh,mb,rc) IS i6_5_5_5_10_1(30,s,a,sh,Shl(Shl(mb,3) OR 3,1) OR sh,rc)
EXPORT PROC ppcrlwimi(s,a,sh,mb,me,rc) IS i6_5_5_5_10_1(20,s,a,sh,Shl(mb,5) OR me,rc)
EXPORT PROC ppcrlwinm(s,a,sh,mb,me,rc) IS i6_5_5_5_10_1(21,s,a,sh,Shl(mb,5) OR me,rc)
EXPORT PROC ppcrlwnm(s,a,b,mb,me,rc) IS i6_5_5_5_10_1(23,s,a,b,Shl(mb,5) OR me,rc)
EXPORT PROC ppcsc() IS i6_5_5_16(17,NIL,NIL,2)
EXPORT PROC ppcslbia() IS i6_5_5_5_10_1(31,NIL,NIL,NIL,498,0)
EXPORT PROC ppcslbie(b) IS i6_5_5_5_10_1(31,NIL,NIL,b,434,0)
EXPORT PROC ppcsld(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,27,rc)
EXPORT PROC ppcslw(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,24,rc)
EXPORT PROC ppcsrad(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,794,rc)
EXPORT PROC ppcsradi(s,a,sh,rc) IS i6_5_5_5_10_1(31,s,a,sh,Shl(413,1) OR sh,rc)
EXPORT PROC ppcsraw(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,792,rc)
EXPORT PROC ppcsrawi(s,a,sh,rc) IS i6_5_5_5_10_1(31,s,a,sh,824,rc)
EXPORT PROC ppcsrd(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,539,rc)
EXPORT PROC ppcsrw(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,536,rc)
EXPORT PROC ppcstb(s,a,disp) IS i6_5_5_16(38,s,a,disp)
EXPORT PROC ppcstbu(s,a,disp) IS i6_5_5_16(39,s,a,disp)
EXPORT PROC ppcstbux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,247,0)
EXPORT PROC ppcstbx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,215,0)
EXPORT PROC ppcstd(s,a,ds) IS i6_5_5_16(62,s,a,Shl(ds,2))
EXPORT PROC ppcstdcx_(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,214,1)
EXPORT PROC ppcstdu(s,a,ds) IS i6_5_5_16(62,s,a,Shl(ds,2) OR 1)
EXPORT PROC ppcstdux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,181,0)
EXPORT PROC ppcstdx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,149,0)
EXPORT PROC ppcstfd(s,a,disp) IS i6_5_5_16(54,s,a,disp)
EXPORT PROC ppcstfdu(s,a,disp) IS i6_5_5_16(55,s,a,disp)
EXPORT PROC ppcstfdux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,759,0)
EXPORT PROC ppcstfdx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,727,0)
EXPORT PROC ppcstfiwx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,983,0)
EXPORT PROC ppcstfs(s,a,disp) IS i6_5_5_16(52,s,a,disp)
EXPORT PROC ppcstfsu(s,a,disp) IS i6_5_5_16(53,s,a,disp)
EXPORT PROC ppcstfsux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,695,0)
EXPORT PROC ppcstfsx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,663,0)
EXPORT PROC ppcsth(s,a,disp) IS i6_5_5_16(44,s,a,disp)
EXPORT PROC ppcsthbrx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,918,0)
EXPORT PROC ppcsthu(s,a,disp) IS i6_5_5_16(45,s,a,disp)
EXPORT PROC ppcsthux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,439,0)
EXPORT PROC ppcsthx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,407,0)
EXPORT PROC ppcstmw(s,a,disp) IS i6_5_5_16(47,s,a,disp)
EXPORT PROC ppcstswi(s,a,nb) IS i6_5_5_5_10_1(31,s,a,nb,725,0)
EXPORT PROC ppcstswx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,661,0)
EXPORT PROC ppcstw(s,a,disp) IS i6_5_5_16(36,s,a,disp)
EXPORT PROC ppcstwbrx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,662,0)
EXPORT PROC ppcstwcx_(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,150,1)
EXPORT PROC ppcstwu(s,a,disp) IS i6_5_5_16(37,s,a,disp)
EXPORT PROC ppcstwux(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,183,0)
EXPORT PROC ppcstwx(s,a,b) IS i6_5_5_5_10_1(31,s,a,b,151,0)
EXPORT PROC ppcsubf(d,a,b,oe,rc) IS i6_5_5_5_10_1(31,d,a,b,Shl(oe,9) OR 40,rc)
EXPORT PROC ppcsubfc(d,a,b,oe,rc) IS i6_5_5_5_10_1(31,d,a,b,Shl(oe,9) OR 8,rc)
EXPORT PROC ppcsubfe(d,a,b,oe,rc) IS i6_5_5_5_10_1(31,d,a,b,Shl(oe,9) OR 136,rc)
EXPORT PROC ppcsubfic(d,a,simm) IS i6_5_5_16(8,d,a,simm)
EXPORT PROC ppcsubfme(d,a,oe,rc) IS i6_5_5_5_10_1(31,d,a,NIL,Shl(oe,9) OR 232,rc)
EXPORT PROC ppcsubfze(d,a,oe,rc) IS i6_5_5_5_10_1(31,d,a,NIL,Shl(oe,9) OR 200,rc)
EXPORT PROC ppcsync() IS i6_5_5_5_10_1(31,NIL,NIL,NIL,598,0)
EXPORT PROC ppctd(to,a,b) IS i6_5_5_5_10_1(31,to,a,b,68,0)
EXPORT PROC ppctdi(to,a,simm) IS i6_5_5_16(2,to,a,simm)
EXPORT PROC ppctlbia() IS i6_5_5_5_10_1(31,NIL,NIL,NIL,370,0)
EXPORT PROC ppctlbie(b) IS i6_5_5_5_10_1(31,NIL,NIL,b,306,0)
EXPORT PROC ppctlbsync() IS i6_5_5_5_10_1(31,NIL,NIL,NIL,566,0)
EXPORT PROC ppctw(to,a,b) IS i6_5_5_5_10_1(31,to,a,b,4,0)
EXPORT PROC ppctwi(to,a,simm) IS i6_5_5_16(3,to,a,simm)
EXPORT PROC ppcxor(s,a,b,rc) IS i6_5_5_5_10_1(31,s,a,b,316,rc)
EXPORT PROC ppcxori(s,a,uimm) IS i6_5_5_16(26,s,a,uimm)
EXPORT PROC ppcxoris(s,a,uimm) IS i6_5_5_16(27,s,a,uimm)

-> Altivec
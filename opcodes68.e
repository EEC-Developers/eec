
-> EEC/opcodes68.e

/* EEC by Samuel Crow et al. [samuraileumas yahoo com] is Copyright (c)2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See CompilerLicense.md */


-> moved opcode stuff into this new module 2008

OPT MODULE
OPT PREPROCESS
OPT LARGE

-> switch to no globals
-> EXPORT DEF g_codeptr:PTR TO LONG

/* assembler 68k */

EXPORT ENUM M0,M1,M2,M3,M4,M5,M6,M7
EXPORT ENUM SIZE_B,SIZE_W,SIZE_L   -> was B,W,L
EXPORT ENUM T,F,HI,LS,CC,CS,NE,EQ,VC,VS,PL,MI,GE,LT,GT,LE

EXPORT ENUM FL,FS,FX,FP,FW,FD,FB
EXPORT ENUM FCF,FCEQ,FCOGT,FCOGE,FCOLT,FCOLE,FCOGL,
            FCOR,FCUN,FCUEQ,FCUGT,FCUGE,FCULT,FCULE,
            FCNE,FCT,FCSF,FCSEQ,FCGT,FCGE,FCLT,FCLE,
            FCGL,FCGLE,FCNGLE,FCNGL,FCNLE,FCNLT,FCNGE,FCNGT

#define put32(g, v) g.codeptr[]++ := v

EXPORT PROC put16(g:PTR TO LONG, v)
   PutInt(g.codeptr, v)
   g.codeptr := g.codeptr + 2
ENDPROC

EXPORT PROC put8(g:PTR TO LONG, v)
   PutChar(g.codeptr, v)
   g.codeptr := g.codeptr + 1
ENDPROC

/**************************************************************
***************************************************************
******************** 68K OPCODES  *****************************
***************************************************************
**************************************************************/

PROC extW(heap,da,reg,wl,scale,bd)
   DEF t=0
   t := Shl(Shl(Shl(Shl(da,3) OR reg, 1) OR wl,2) OR scale, 9) OR (bd AND $FF)
ENDPROC t


->#define BD32EXT extW(heap,0,0,0,0,0,1,3,0)
#define IxExt(heap,idrx,scale,bd) extW(heap,0,idrx,1,scDwn(scale), bd)

-> 1/2/4/8 to 0/1/2/3
#define scDwn(sc) ListItem([0,0,1,0,2,0,0,0,3],sc)



-> optimisations implemented:
-> add.s #i,xxx / sub.s #i,xxx -> addq.s #i,xxx / subq.s #i,xxx
-> addq #0,xxx / subq #0,xxx -> remove
-> divs.l #1, ... / divu.l #1, ... -> remove
-> muls.l #1, ... / mulu.l #1, ... -> remove
-> and.l #$FFFFFF00, dx -> clr.b dx
-> clr.b -> eor.b dx,dx (fix for 68000 but doesn't hurt)
-> and.l #$FFFF0000, dx -> clr.w dx
-> clr.w dx -> eor.w dx,dx (fix for 68000 but doesn't hurt)
-> and.w #$FF00, dx -> clr.b dx
-> WHOOPS could be better than clr.l dx -> moveq #0, dx (wrong cpu flags) Sam
-> instead do clr.l dx -> eor.l dx,dx
-> cmp.s #0,xxx -> tst.s xxx
-> lea ofs(ax),ax -> addq.l #ofs,ax / subq.l #ofs,ax
-> move.l #i,dx -> moveq #i, dx
-> move.l #i, ax -> move.w #i, ax
-> move.l #0, ax -> sub.l ax, ax
-> move.l #0, (ax)/(ax)+/-(ax)/ofs(ax)/(ax,dx.l*s) -> clr.l (ax)/(ax)+/-(ax)/ofs(ax)/(ax,dx.l*s)
-> move.l #i, -(a7) -> pea i.l
-> move.w #0, dx/(ax)/(ax)+/-(ax) -> clr.w dx/(ax)/(ax)+/-(ax)
-> WHOPPS! wrong... move.w #i, -(a7) -> pea i.w
-> move.w #0, ofs(ax)/(ax,dx.l*s) -> clr.w ofs(ax)/(ax,dx.l*s)
-> pea abs.l -> pea abs.w
-> move.b #0, dx/(ax)/ofs(ax)/(ax,dx.l*s) -> clr.b dx/(ax)/ofs(ax)/(ax,dx.l*s)

/********** ADD *************/

PROC putADD(heap, heap,reg,size,mode,mreg)
   put16(heap,Shl(Shl(Shl(%1101000 OR reg,3) OR size,3) OR mode,3) OR mreg)
ENDPROC

EXPORT PROC adddxdx(heap,s,dx1,dx2) IS putADD(heap, dx2,s,M0,dx1)
EXPORT PROC adddxax(heap,s,dx,ax) IS putADD(heap, ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC adddxaxp(heap,s,dx,ax) IS putADD(heap, dx,s+4,M2,ax)
EXPORT PROC adddxaxpi(heap,s,dx,ax) IS putADD(heap, dx,s+4,M3,ax)
EXPORT PROC adddxaxpd(heap,s,dx,ax) IS putADD(heap, dx,s+4,M4,ax)
EXPORT PROC adddxaxpofs(heap,s,dx,ax,ofs)
   putADD(heap, dx,s+4,M5,ax)
   put16(heap,ofs)
ENDPROC
EXPORT PROC adddxaxpx(heap,s,dx,ax,idrx,scale,d)
   putADD(heap, dx,s+4,M6,ax)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC addaxdx(heap,s,ax,dx) IS putADD(heap, dx,s,M1,ax)
EXPORT PROC addaxax(heap,s,ax1,ax2) IS putADD(heap, ax2,ListItem([0,3,7],s),M1,ax1)

EXPORT PROC addlimmax(heap,imm,ax)
   IF imm < 9 AND (imm > 0)
         RETURN addqax(heap,SIZE_L,imm,ax)
   ELSEIF imm = $80000000  -> fix

   ELSEIF Abs(imm) < 32768
         RETURN leaaxpofsax(heap,ax,imm,ax)
   ENDIF
   putADD(heap, ax,7,M7,4)
   put32(heap,imm)
ENDPROC
EXPORT PROC addwimmax(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN addqax(heap,SIZE_W,imm,ax)
   putADD(heap, ax,3,M7,4)
   put16(heap,imm)
ENDPROC
EXPORT PROC addaxpdx(heap,s,axp,dx) IS putADD(heap, dx,s,M2,axp)
EXPORT PROC addaxpax(heap,s,axp,ax) IS putADD(heap, ax,ListItem([0,3,7],s),M2,axp)
EXPORT PROC addaxpidx(heap,s,axp,dx) IS putADD(heap, dx,s,M3,axp)
EXPORT PROC addaxpddx(heap,s,axp,dx) IS putADD(heap, dx,s,M4,axp)
EXPORT PROC addaxpofsdx(heap,s,axp,ofs,dx)
   putADD(heap, dx,s,M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addaxpofsax(heap,s,axp,ofs,dx)
   putADD(heap, dx,ListItem([0,3,7],s),M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addaxpxdx(heap,s,axp,idrx,scale,d,dx)
   putADD(heap, dx,s,M6,axp)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC addaxpxax(heap,s,axp,idrx,scale,d,dx)
   putADD(heap, dx,ListItem([0,3,7],s),M6,axp)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC

PROC putADDI(heap,size,mode,mreg, imm)
   put16(heap,Shl(Shl(%0000011000 OR size,3) OR mode,3) OR mreg)
   IF size = 2
      put32(heap,imm)
   ELSE
      put16(heap,imm)
   ENDIF
ENDPROC

EXPORT PROC addlimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(heap,SIZE_L,imm,dx) ELSE putADDI(heap,SIZE_L,M0,dx,imm)
ENDPROC

EXPORT PROC addwimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(heap,SIZE_W,imm,dx) ELSE putADDI(heap,SIZE_W,M0,dx,imm)
ENDPROC
EXPORT PROC addbimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(heap,SIZE_B,imm,dx) ELSE putADDI(heap,SIZE_B,M0,dx,imm)
ENDPROC

EXPORT PROC addlimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(heap,SIZE_L,imm,ax) ELSE putADDI(heap,SIZE_L,M2,ax,imm)
ENDPROC

EXPORT PROC addwimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(heap,SIZE_W,imm,ax) ELSE putADDI(heap,SIZE_W,M2,ax,imm)
ENDPROC

EXPORT PROC addbimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(heap,SIZE_B,imm,ax) ELSE putADDI(heap,SIZE_B,M2,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpi(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpi(heap,SIZE_L,imm,ax) ELSE putADDI(heap,SIZE_L,M3,ax,imm)
ENDPROC

EXPORT PROC addwimmaxpi(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(heap,SIZE_W,imm,ax) ELSE putADDI(heap,SIZE_W,M3,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpd(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpd(heap,SIZE_L,imm,ax) ELSE putADDI(heap,SIZE_L,M4,ax,imm)
ENDPROC

EXPORT PROC addwimmaxpd(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpd(heap,SIZE_W,imm,ax) ELSE putADDI(heap,SIZE_W,M4,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(heap,SIZE_L,imm,ax,ofs)
   putADDI(heap,SIZE_L,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addwimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(heap,SIZE_W,imm,ax,ofs)
   putADDI(heap,SIZE_W,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addbimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(heap,SIZE_B,imm,ax,ofs)
   putADDI(heap,SIZE_B,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addlimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(heap,SIZE_L,imm,ax,idrx,scale,d)
   putADDI(heap,SIZE_L,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC addwimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(heap,SIZE_W,imm,ax,idrx,scale,d)
   putADDI(heap,SIZE_W,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC addbimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(heap,SIZE_B,imm,ax,idrx,scale,d)
   putADDI(heap,SIZE_B,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC


/********* ADDQ ***********/

PROC putADDQ(heap,d,s,m,r)
   IF d = NIL THEN RETURN
   put16(heap,Shl(Shl(Shl(%01010000 OR Shl(IF d = 8 THEN 0 ELSE d,1),2) OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC addqdx(heap,s,d,dx) IS putADDQ(heap,d,s,M0,dx)
EXPORT PROC addqax(heap,s,d,ax) IS putADDQ(heap,d,s,M1,ax)
EXPORT PROC addqaxp(heap,s,d,axp) IS putADDQ(heap,d,s,M2,axp)
EXPORT PROC addqaxpi(heap,s,d,axp) IS putADDQ(heap,d,s,M3,axp)
EXPORT PROC addqaxpd(heap,s,d,axp) IS putADDQ(heap,d,s,M4,axp)
EXPORT PROC addqaxpofs(heap,s,d,axp,ofs)
   putADDQ(heap,d,s,M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC addqaxpx(heap,s,d,axp,idrx,scale,dis)
   putADDQ(heap,d,s,M6,axp)
   put16(heap,IxExt(heap,idrx,scale,dis))
ENDPROC

/********* ADDX ***********/

PROC putADDX(heap,rx,s,rm,ry)
   put16(heap,Shl(Shl(Shl(%1101000 OR rx, 3) OR %100 OR s, 3) OR rm, 3) OR ry)
ENDPROC

EXPORT PROC addxdxdx(heap,s,dx1,dx2) IS putADDX(heap,dx2,s,0,dx1)
EXPORT PROC addxaxpdaxpd(heap,s,ax1,ax2) IS putADDX(heap,ax2,s,1,ax1)

/********* SUBX ***********/

PROC putSUBX(heap,rx,s,rm,ry)
   put16(heap,Shl(Shl(Shl(%1001000 OR rx, 3) OR %100 OR s, 3) OR rm, 3) OR ry)
ENDPROC

EXPORT PROC subxdxdx(heap,s,dx1,dx2) IS putSUBX(heap,dx2,s,0,dx1)
EXPORT PROC subxaxpdaxpd(heap,s,ax1,ax2) IS putSUBX(heap,ax2,s,1,ax1)

/********* SUB ************/

PROC putSub(heap,r,s,m,mr)
   put16(heap,Shl(Shl(Shl(%1001000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC subdxdx(heap,s,dx1,dx2) IS putSub(heap,dx2,s,M0,dx1)
EXPORT PROC subdxax(heap,s,dx,ax) IS putSub(heap,ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC subdxaxp(heap,s,dx,ax) IS putSub(heap,dx,s+4,M2,ax)
EXPORT PROC subdxaxpi(heap,s,dx,ax) IS putSub(heap,dx,s+4,M3,ax)
EXPORT PROC subdxaxpd(heap,s,dx,ax) IS putSub(heap,dx,s+4,M4,ax)
EXPORT PROC subdxaxpofs(heap,s,dx,ax,ofs)
   putSub(heap,dx,s+4,M5,ax)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subdxaxpx(heap,s,dx,ax,idrx,scale,d)
   putSub(heap,dx,s+4,M6,ax)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC subaxdx(heap,s,ax,dx) IS putSub(heap,dx,s,M1,ax)
EXPORT PROC subaxax(heap,s,ax1,ax2) IS putSub(heap,ax2,ListItem([0,3,7],s),M1,ax1)
EXPORT PROC sublimmax(heap,imm,ax)
   IF imm < 9 AND (imm > 0)
      RETURN subqax(heap,SIZE_L,imm,ax)
   ELSEIF imm = $80000000 -> fix

   ELSEIF Abs(imm) < 32768
      RETURN leaaxpofsax(heap,ax,-imm,ax)
   ENDIF
   putSub(heap,ax,ListItem([0,3,7],SIZE_L),M7,4)
   put32(heap,imm)
ENDPROC
EXPORT PROC subwimmax(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqax(heap,SIZE_W,imm,ax)
   putSub(heap,ax,ListItem([0,3,7],SIZE_W),M7,4)
   put16(heap,imm)
ENDPROC

EXPORT PROC subaxpdx(heap,s,axp,dx) IS putSub(heap,dx,s,M2,axp)
EXPORT PROC subaxpax(heap,s,axp,dx) IS putSub(heap,dx,ListItem([0,3,7],s),M2,axp)
EXPORT PROC subaxpidx(heap,s,axp,dx) IS putSub(heap,dx,s,M3,axp)
EXPORT PROC subaxpddx(heap,s,axp,dx) IS putSub(heap,dx,s,M4,axp)
EXPORT PROC subaxpofsdx(heap,s,axp,ofs,dx)
   putSub(heap,dx,s,M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subaxpofsax(heap,s,axp,ofs,dx)
   putSub(heap,dx,ListItem([0,3,7],s),M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subaxpxdx(heap,s,axp,idrx,scale,d,dx)
   putSub(heap,dx,s,M6,axp)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC subaxpxax(heap,s,axp,idrx,scale,d,dx)
   putSub(heap,dx,ListItem([0,3,7],s),M6,axp)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC

PROC putSUBI(heap,s,m,r,i)
   put16(heap,Shl(Shl(%0000010000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN put32(heap,i) ELSE put16(heap,i)
ENDPROC

EXPORT PROC sublimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(heap,SIZE_L,imm,dx)
   putSUBI(heap,SIZE_L,M0,dx,imm)
ENDPROC
EXPORT PROC subwimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(heap,SIZE_W,imm,dx)
   putSUBI(heap,SIZE_W,M0,dx,imm)
ENDPROC
EXPORT PROC subbimmdx(heap,imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(heap,SIZE_B,imm,dx)
   putSUBI(heap,SIZE_B,M0,dx,imm)
ENDPROC
EXPORT PROC sublimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(heap,SIZE_L,imm,ax)
   putSUBI(heap,SIZE_L,M2,ax,imm)
ENDPROC
EXPORT PROC subwimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(heap,SIZE_W,imm,ax)
   putSUBI(heap,SIZE_W,M2,ax,imm)
ENDPROC
EXPORT PROC subbimmaxp(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(heap,SIZE_B,imm,ax)
   putSUBI(heap,SIZE_B,M2,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpi(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpi(heap,SIZE_L,imm,ax)
   putSUBI(heap,SIZE_L,M3,ax,imm)
ENDPROC
EXPORT PROC subwimmaxpi(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpi(heap,SIZE_W,imm,ax)
   putSUBI(heap,SIZE_W,M3,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpd(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpd(heap,SIZE_L,imm,ax)
   putSUBI(heap,SIZE_L,M4,ax,imm)
ENDPROC
EXPORT PROC subwimmaxpd(heap,imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpd(heap,SIZE_W,imm,ax)
   putSUBI(heap,SIZE_W,M4,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(heap,SIZE_L,imm,ax,ofs)
   putSUBI(heap,SIZE_L,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subwimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(heap,SIZE_W,imm,ax,ofs)
   putSUBI(heap,SIZE_W,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subbimmaxpofs(heap,imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(heap,SIZE_B,imm,ax,ofs)
   putSUBI(heap,SIZE_B,M5,ax,imm)
   put16(heap,ofs)
ENDPROC
EXPORT PROC sublimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(heap,SIZE_L,imm,ax,idrx,scale,d)
   putSUBI(heap,SIZE_L,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC subwimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(heap,SIZE_W,imm,ax,idrx,scale,d)
   putSUBI(heap,SIZE_W,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC
EXPORT PROC subbimmaxpx(heap,imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(heap,SIZE_B,imm,ax,idrx,scale,d)
   putSUBI(heap,SIZE_B,M6,ax,imm)
   put16(heap,IxExt(heap,idrx,scale,d))
ENDPROC

/********* SUBQ ***********/

PROC putSUBQ(heap,d,s,m,r)
   IF d = NIL THEN RETURN
   put16(heap,Shl(Shl(Shl(%01010001 OR Shl(IF d = 8 THEN 0 ELSE d,1),2) OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC subqdx(heap,s,d,dx) IS putSUBQ(heap,d,s,M0,dx)
EXPORT PROC subqax(heap,s,d,ax) IS putSUBQ(heap,d,s,M1,ax)
EXPORT PROC subqaxp(heap,s,d,axp) IS putSUBQ(heap,d,s,M2,axp)
EXPORT PROC subqaxpi(heap,s,d,axp) IS putSUBQ(heap,d,s,M3,axp)
EXPORT PROC subqaxpd(heap,s,d,axp) IS putSUBQ(heap,d,s,M4,axp)
EXPORT PROC subqaxpofs(heap,s,d,axp,ofs)
   putSUBQ(heap,d,s,M5,axp)
   put16(heap,ofs)
ENDPROC
EXPORT PROC subqaxpx(heap,s,d,axp,idrx,scale,dis)
   putSUBQ(heap,d,s,M6,axp)
   put16(heap,IxExt(heap,idrx,scale,dis))
ENDPROC

/******** DIVS.L / DIVU.L **********/

-> fixed 1.10.0
PROC putDIVL(heap,m,dx,dq,s,dr,sus)
   put16(heap,Shl(%0100110001000 OR m,3) OR dx)
   put16(heap,Shl(Shl(Shl(dq,1) OR sus,1) OR s,10) OR dr)
ENDPROC
EXPORT PROC divsldxdrdq(heap,s,dx,dr,dq)
ENDPROC putDIVL(heap,M0,dx,dq,s,dr,1)
EXPORT PROC divslaxpdrdq(heap,s,axp,dr,dq)
ENDPROC putDIVL(heap,M2,axp,dq,s,dr,1)
EXPORT PROC divslaxpofsdrdq(heap,s,axp,ofs,dr,dq)
ENDPROC putDIVL(heap,M5,axp,dq,s,dr,1) BUT put16(heap,ofs)
EXPORT PROC divslimmdrdq(heap,s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putDIVL(heap,M7,4,dq,s,dr,1)
   put32(heap,imm)
ENDPROC
EXPORT PROC divuldxdrdq(heap,s,dx,dr,dq)
ENDPROC putDIVL(heap,M0,dx,dq,s,dr,0)
EXPORT PROC divulaxpdrdq(heap,s,axp,dr,dq)
ENDPROC putDIVL(heap,M2,axp,dq,s,dr,0)
EXPORT PROC divulaxpofsdrdq(heap,s,axp,ofs,dr,dq)
ENDPROC putDIVL(heap,M5,axp,dq,s,dr,0) BUT put16(heap,ofs)
EXPORT PROC divulimmdrdq(heap,s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putDIVL(heap,M7,4,dq,s,dr,0)
   put32(heap,imm)
ENDPROC


/******** MULS.L / MULU.L **********/

PROC putMULL(heap,m,dx,dq,s,dr,sus)
   put16(heap,Shl(%0100110000000 OR m,3) OR dx)
   put16(heap,Shl(Shl(Shl(dq,1) OR sus,1) OR s,10) OR dr)
ENDPROC
EXPORT PROC mulsldxdrdq(heap,s,dx,dr,dq)
ENDPROC putMULL(heap,M0,dx,dq,s,dr,1)
EXPORT PROC mulslaxpdrdq(heap,s,axp,dr,dq)
ENDPROC putMULL(heap,M2,axp,dq,s,dr,1)
EXPORT PROC mulslaxpofsdrdq(heap,s,axp,ofs,dr,dq)
ENDPROC putMULL(heap,M5,axp,dq,s,dr,1) BUT put16(heap,ofs)
EXPORT PROC mulslimmdrdq(heap,s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putMULL(heap,M7,4,dq,s,dr,1)
   put32(heap,imm)
ENDPROC
EXPORT PROC mululdxdrdq(heap,s,dx,dr,dq)
ENDPROC putMULL(heap,M0,dx,dq,s,dr,0)
EXPORT PROC mululaxpdrdq(heap,s,axp,dr,dq)
ENDPROC putMULL(heap,M2,axp,dq,s,dr,0)
EXPORT PROC mululaxpofsdrdq(heap,s,axp,ofs,dr,dq)
ENDPROC putMULL(heap,M5,axp,dq,s,dr,0) BUT put16(heap,ofs)
EXPORT PROC mululimmdrdq(heap,s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putMULL(heap,M7,4,dq,s,dr,0)
   put32(heap,imm)
ENDPROC


/******** MULS.W / MULU.W **********/

PROC putMULSW(heap,r,m,mr)
   put16(heap,Shl(Shl(Shl(%1100000 OR r,3) OR %111,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC mulswdxdx(heap,dx1,dx2) IS putMULSW(heap,dx2,M0,dx1)
EXPORT PROC mulswaxpdx(heap,axp,dx) IS putMULSW(heap,dx,M2,axp)
EXPORT PROC mulswaxpofsdx(heap,axp,ofs,dx)
ENDPROC putMULSW(heap,dx,M5,axp) BUT put16(heap,ofs)
EXPORT PROC mulswimmdx(heap,imm,dx)
ENDPROC putMULSW(heap,dx,M7,4) BUT put16(heap,imm)

PROC putMULUW(heap,r,m,mr)
   put16(heap,Shl(Shl(Shl(%1100000 OR r,3) OR %011,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC muluwdxdx(heap,dx1,dx2) IS putMULUW(heap,dx2,M0,dx1)
EXPORT PROC muluwaxpdx(heap,axp,dx) IS putMULUW(heap,dx,M2,axp)
EXPORT PROC muluwaxpofsdx(heap,axp,ofs,dx)
ENDPROC putMULUW(heap,dx,M5,axp) BUT put16(heap,ofs)
EXPORT PROC muluwimmdx(heap,imm,dx)
ENDPROC putMULUW(heap,dx,M7,4) BUT put16(heap,imm)

/******** DIVS.W / DIVU.W **********/

PROC putDIVSW(heap,r,m,mr)
   put16(heap,Shl(Shl(Shl(%1000000 OR r,3) OR %111,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC divswdxdx(heap,dx1,dx2) IS putDIVSW(heap,dx2,M0,dx1)
EXPORT PROC divswaxpdx(heap,axp,dx) IS putDIVSW(heap,dx,M2,axp)
EXPORT PROC divswaxpofsdx(heap,axp,ofs,dx)
ENDPROC putDIVSW(heap,dx,M5,axp) BUT put16(heap,ofs)
EXPORT PROC divswimmdx(heap,imm,dx)
ENDPROC putDIVSW(heap,dx,M7,4) BUT put16(heap,imm)

PROC putDIVUW(heap,r,m,mr)
   put16(heap,Shl(Shl(Shl(%1000000 OR r,3) OR %011,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC divuwdxdx(heap,dx1,dx2) IS putDIVUW(heap,dx2,M0,dx1)
EXPORT PROC divuwaxpdx(heap,axp,dx) IS putDIVUW(heap,dx,M2,axp)
EXPORT PROC divuwaxpofsdx(heap,axp,ofs,dx)
ENDPROC putDIVUW(heap,dx,M5,axp) BUT put16(heap,ofs)
EXPORT PROC divuwimmdx(heap,imm,dx)
ENDPROC putDIVUW(heap,dx,M7,4) BUT put16(heap,imm)


/********* CLR *********************/

PROC putCLR(heap,s,m,r)
   put16(heap,Shl(Shl(%0100001000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC clrdx(heap,s,dx) IS putEOR(heap,s,dx,dx)
EXPORT PROC clrax(heap,s,ax) IS putCLR(heap,s,M1,ax)
EXPORT PROC clraxp(heap,s,axp) IS putCLR(heap,s,M2,axp)
EXPORT PROC clraxpi(heap,s,axpi) IS putCLR(heap,s,M3,axpi)
EXPORT PROC clraxpd(heap,s,axpd) IS putCLR(heap,s,M4,axpd)
EXPORT PROC clraxpofs(heap,s,axp,ofs) IS putCLR(heap,s,M5,axp) BUT put16(heap,ofs)
EXPORT PROC clraxpx(heap,s,axp,idrx,scale,d) IS putCLR(heap,s,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/*********** CMP ********************/

PROC putCMPI(heap,s,m,r,i)
   put16(heap,Shl(Shl(%0000110000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN put32(heap,i) ELSE put16(heap,i)
ENDPROC

EXPORT PROC cmplimmdx(heap,imm,dx)
   IF imm=0 THEN RETURN tstdx(heap,SIZE_L,dx)
ENDPROC putCMPI(heap,SIZE_L,M0,dx,imm)
EXPORT PROC cmpwimmdx(heap,imm,dx)
   IF imm=0 THEN RETURN tstdx(heap,SIZE_W,dx)
ENDPROC putCMPI(heap,SIZE_W,M0,dx,imm)
EXPORT PROC cmpbimmdx(heap,imm,dx)
   IF imm=0 THEN RETURN tstdx(heap,SIZE_B,dx)
ENDPROC putCMPI(heap,SIZE_B,M0,dx,imm)
EXPORT PROC cmplimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxp(heap,SIZE_L,axp)
ENDPROC putCMPI(heap,SIZE_L,M2,axp,imm)
EXPORT PROC cmpwimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxp(heap,SIZE_W,axp)
ENDPROC putCMPI(heap,SIZE_W,M2,axp,imm)
EXPORT PROC cmpbimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxp(heap,SIZE_B,axp)
ENDPROC putCMPI(heap,SIZE_B,M2,axp,imm)
EXPORT PROC cmplimmaxpi(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxpi(heap,SIZE_L,axp)
ENDPROC putCMPI(heap,SIZE_L,M3,axp,imm)
EXPORT PROC cmpwimmaxpi(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxpi(heap,SIZE_W,axp)
ENDPROC putCMPI(heap,SIZE_W,M3,axp,imm)
EXPORT PROC cmpbimmaxpi(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxpi(heap,SIZE_W,axp)
ENDPROC putCMPI(heap,SIZE_B,M3,axp,imm)
EXPORT PROC cmplimmaxpd(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxpd(heap,SIZE_L,axp)
ENDPROC putCMPI(heap,SIZE_L,M4,axp,imm)
EXPORT PROC cmpwimmaxpd(heap,imm,axp)
   IF imm=0 THEN RETURN tstaxpd(heap,SIZE_W,axp)
ENDPROC putCMPI(heap,SIZE_W,M4,axp,imm)
EXPORT PROC cmplimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(heap,SIZE_L,axp,ofs)
ENDPROC putCMPI(heap,SIZE_L,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC cmpwimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(heap,SIZE_W,axp,ofs)
ENDPROC putCMPI(heap,SIZE_W,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC cmpbimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(heap,SIZE_B,axp,ofs)
ENDPROC putCMPI(heap,SIZE_B,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC cmplimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(heap,SIZE_L,axp,idrx,scale,d)
ENDPROC putCMPI(heap,SIZE_L,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC cmpwimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(heap,SIZE_W,axp,idrx,scale,d)
ENDPROC putCMPI(heap,SIZE_W,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC cmpbimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(heap,SIZE_B,axp,idrx,scale,d)
ENDPROC putCMPI(heap,SIZE_B,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

PROC putCMP(heap,r,s,m,mr)
   put16(heap,Shl(Shl(Shl(%1011000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC cmpdxax(heap,s,dx,ax) IS putCMP(heap,ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC cmpdxdx(heap,s,dx1,dx2) IS putCMP(heap,dx2,s,M0,dx1)
EXPORT PROC cmpaxdx(heap,s,ax,dx) IS putCMP(heap,dx,s,M1,ax)
EXPORT PROC cmpaxpdx(heap,s,ax,dx) IS putCMP(heap,dx,s,M2,ax)
EXPORT PROC cmpaxpidx(heap,s,ax,dx) IS putCMP(heap,dx,s,M3,ax)
EXPORT PROC cmpaxpddx(heap,s,ax,dx) IS putCMP(heap,dx,s,M4,ax)
EXPORT PROC cmpaxpofsdx(heap,s,ax,ofs,dx)
ENDPROC putCMP(heap,dx,s,M5,ax) BUT put16(heap,ofs)
EXPORT PROC cmpaxpxdx(heap,s,ax,idrx,scale,d,dx)
ENDPROC putCMP(heap,dx,s,M6,ax) BUT put16(heap,IxExt(heap,idrx,scale,d))


EXPORT PROC cmpaxax(heap,s,ax1,ax2) IS putCMP(heap,ax2,ListItem([0,3,7],s),M1,ax1)
EXPORT PROC cmpaxpax(heap,s,ax1,ax2) IS putCMP(heap,ax2,ListItem([0,3,7],s),M2,ax1)
EXPORT PROC cmpaxpiax(heap,s,ax1,ax2) IS putCMP(heap,ax2,ListItem([0,3,7],s),M3,ax1)
EXPORT PROC cmpaxpdax(heap,s,ax1,ax2) IS putCMP(heap,ax2,ListItem([0,3,7],s),M4,ax1)
EXPORT PROC cmpaxpofsax(heap,s,ax1,ofs,ax2)
ENDPROC putCMP(heap,ax2,ListItem([0,3,7],s),M5,ax1) BUT put16(heap,ofs)
EXPORT PROC cmpaxpxax(heap,s,ax1,ax2,idrx,scale,d)
ENDPROC putCMP(heap,ax2,ListItem([0,3,7],s),M6,ax1) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC cmplimmax(heap,imm,ax)
   IF imm=0 THEN RETURN tstax(heap,SIZE_L,ax)
   putCMP(heap,ax,ListItem([0,3,7],SIZE_L),M7,4)
   put32(heap,imm)
ENDPROC
EXPORT PROC cmpwimmax(heap,imm,ax)
   IF imm=0 THEN RETURN tstax(heap,SIZE_W,ax)
ENDPROC putCMP(heap,ax,ListItem([0,3,7],SIZE_W),M7,4) BUT put16(heap,imm)

/*********** CMP2 ***************/

EXPORT PROC cmp2axpdx(heap,s,axp,dx)
   put32(heap,Shl(Shl(Shl(Shl(Shl(%0000000 OR s,3) OR %011,3) OR M2,3) OR axp,4) OR dx,12) OR NIL)
ENDPROC

/*********** CMPM ***************/
EXPORT PROC cmpmaxpiaxpi(heap,s, ax1, ax2)
   put16(heap,Shl(Shl(%1011000 OR ax2, 3) OR s OR %100, 6) OR %001000 OR ax1)
ENDPROC

/************ EXT ***************/

EXPORT PROC extW(heap,dx)  IS put16(heap,%0100100010000000 OR dx)
EXPORT PROC extl(heap,dx)  IS put16(heap,%0100100011000000 OR dx)
EXPORT PROC extbl(heap,dx) IS put16(heap,%0100100111000000 OR dx)

/************ NEG ***************/

PROC putNEG(heap,s,m,r)
   put16(heap,Shl(Shl(%0100010000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC negdx(heap,s,dx) IS putNEG(heap,s,M0,dx)
EXPORT PROC negaxp(heap,s,axp) IS putNEG(heap,s,M2,axp)
EXPORT PROC negaxpi(heap,s,axp) IS putNEG(heap,s,M3,axp)
EXPORT PROC negaxpd(heap,s,axp) IS putNEG(heap,s,M4,axp)
EXPORT PROC negaxpofs(heap,s,axp,ofs)
ENDPROC putNEG(heap,s,M5,axp) BUT put16(heap,ofs)
EXPORT PROC negaxpx(heap,s,axp,idrx,scale,d)
ENDPROC putNEG(heap,s,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ NEGX ***************/

PROC putNEGX(heap,s,m,r)
   put16(heap,Shl(Shl(%0100000000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC negxdx(heap,s,dx) IS putNEGX(heap,s,M0,dx)
EXPORT PROC negxaxp(heap,s,axp) IS putNEGX(heap,s,M2,axp)
EXPORT PROC negxaxpi(heap,s,axp) IS putNEGX(heap,s,M3,axp)
EXPORT PROC negxaxpd(heap,s,axp) IS putNEGX(heap,s,M4,axp)
EXPORT PROC negxaxpofs(heap,s,axp,ofs)
ENDPROC putNEGX(heap,s,M5,axp) BUT put16(heap,ofs)
EXPORT PROC negxaxpx(heap,s,axp,idrx,scale,d)
ENDPROC putNEGX(heap,s,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ NOP ****************/

EXPORT PROC nop(heap) IS put16(heap,%0100111001110001)

/************ AND *****************/

PROC putANDI(heap,s,m,r,i)
   put16(heap,Shl(Shl(%0000001000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN put32(heap,i) ELSE put16(heap,i)
ENDPROC

EXPORT PROC andlimmdx(heap,imm,dx)
   IF imm=$FFFFFF00
      RETURN clrdx(heap,SIZE_B,dx)
   ELSEIF imm=$FFFF0000
      RETURN clrdx(heap,SIZE_W,dx)
   ENDIF
ENDPROC putANDI(heap,SIZE_L,M0,dx,imm)
EXPORT PROC andwimmdx(heap,imm,dx)
   IF imm=$FF00 THEN RETURN clrdx(heap,SIZE_B,dx)
ENDPROC putANDI(heap,SIZE_W,M0,dx,imm)
EXPORT PROC andbimmdx(heap,imm,dx)
ENDPROC putANDI(heap,SIZE_B,M0,dx,imm)
EXPORT PROC andlimmaxp(heap,imm,ax)
ENDPROC putANDI(heap,SIZE_L,M2,ax,imm)
EXPORT PROC andwimmaxp(heap,imm,ax)
ENDPROC putANDI(heap,SIZE_W,M2,ax,imm)
EXPORT PROC andbimmaxp(heap,imm,ax)
ENDPROC putANDI(heap,SIZE_B,M2,ax,imm)
EXPORT PROC andlimmaxpi(heap,imm,ax)
ENDPROC putANDI(heap,SIZE_L,M3,ax,imm)
EXPORT PROC andlimmaxpd(heap,imm,ax)
ENDPROC putANDI(heap,SIZE_L,M4,ax,imm)
EXPORT PROC andlimmaxpofs(heap,imm,ax,ofs)
ENDPROC putANDI(heap,SIZE_L,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC andwimmaxpofs(heap,imm,ax,ofs)
ENDPROC putANDI(heap,SIZE_W,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC andbimmaxpofs(heap,imm,ax,ofs)
ENDPROC putANDI(heap,SIZE_B,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC andlimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putANDI(heap,SIZE_L,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC andwimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putANDI(heap,SIZE_W,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC andbimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putANDI(heap,SIZE_B,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

PROC putAND(heap,r,s,m,mr)
   put16(heap,Shl(Shl(Shl(%1100000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC anddxdx(heap,s,dx1,dx2) IS putAND(heap,dx2,s,M0,dx1)
EXPORT PROC andaxpdx(heap,s,axp,dx) IS putAND(heap,dx,s,M2,axp)
EXPORT PROC andaxpidx(heap,s,axp,dx) IS putAND(heap,dx,s,M3,axp)
EXPORT PROC andaxpddx(heap,s,axp,dx) IS putAND(heap,dx,s,M4,axp)
EXPORT PROC andaxpofsdx(heap,s,axp,ofs,dx)
ENDPROC putAND(heap,dx,s,M5,axp) BUT put16(heap,ofs)
EXPORT PROC andaxpxdx(heap,s,axp,idrx,scale,d,dx)
ENDPROC putAND(heap,dx,s,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ OR *****************/

PROC putORI(heap,s,m,r,i)
   put16(heap,Shl(Shl(%0000000000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN put32(heap,i) ELSE put16(heap,i)
ENDPROC

EXPORT PROC orlimmdx(heap,imm,dx) IS putORI(heap,SIZE_L,M0,dx,imm)
EXPORT PROC orwimmdx(heap,imm,dx) IS putORI(heap,SIZE_W,M0,dx,imm)
EXPORT PROC orbimmdx(heap,imm,dx) IS putORI(heap,SIZE_B,M0,dx,imm)
EXPORT PROC orlimmaxp(heap,imm,ax) IS putORI(heap,SIZE_L,M2,ax,imm)
EXPORT PROC orwimmaxp(heap,imm,ax) IS putORI(heap,SIZE_W,M2,ax,imm)
EXPORT PROC orbimmaxp(heap,imm,ax) IS putORI(heap,SIZE_B,M2,ax,imm)
EXPORT PROC orlimmaxpi(heap,imm,ax) IS putORI(heap,SIZE_L,M3,ax,imm)
EXPORT PROC orlimmaxpd(heap,imm,ax) IS putORI(heap,SIZE_L,M4,ax,imm)
EXPORT PROC orlimmaxpofs(heap,imm,ax,ofs)
ENDPROC putORI(heap,SIZE_L,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC orwimmaxpofs(heap,imm,ax,ofs)
ENDPROC putORI(heap,SIZE_W,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC orbimmaxpofs(heap,imm,ax,ofs)
ENDPROC putORI(heap,SIZE_B,M5,ax,imm) BUT put16(heap,ofs)
EXPORT PROC orlimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putORI(heap,SIZE_L,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC orwimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putORI(heap,SIZE_W,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC orbimmaxpx(heap,imm,ax,idrx,scale,d)
ENDPROC putORI(heap,SIZE_B,M6,ax,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

PROC putOR(heap,r,s,m,mr)
   put16(heap,Shl(Shl(Shl(%1000000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC ordxdx(heap,s,dx1,dx2) IS putOR(heap,dx2,s,M0,dx1)
EXPORT PROC oraxpdx(heap,s,axp,dx) IS putOR(heap,dx,s,M2,axp)
EXPORT PROC oraxpidx(heap,s,axp,dx) IS putOR(heap,dx,s,M3,axp)
EXPORT PROC oraxpddx(heap,s,axp,dx) IS putOR(heap,dx,s,M4,axp)
EXPORT PROC oraxpofsdx(heap,s,axp,ofs,dx)
ENDPROC putOR(heap,dx,s,M5,axp) BUT put16(heap,ofs)
EXPORT PROC oraxpxdx(heap,s,axp,idrx,scale,d,dx)
ENDPROC putOR(heap,dx,s,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ EOR ***************/

PROC putEOR(heap,r,s,m,mr)
   put16(heap,Shl(Shl(Shl(%1011000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC eordxdx(heap,s,dx1,dx2) IS putEOR(heap,dx1,ListItem([4,5,6],s),M0,dx2)
EXPORT PROC eordxaxp(heap,s,dx,axp) IS putEOR(heap,dx,ListItem([4,5,6],s),M2,axp)
EXPORT PROC eordxaxpi(heap,s,dx,axp) IS putEOR(heap,dx,ListItem([4,5,6],s),M3,axp)
EXPORT PROC eordxaxpd(heap,s,dx,axp) IS putEOR(heap,dx,ListItem([4,5,6],s),M4,axp)
EXPORT PROC eordxaxpofs(heap,s,dx,axp,ofs)
ENDPROC putEOR(heap,dx,ListItem([4,5,6],s),M5,axp) BUT put16(heap,ofs)
EXPORT PROC eordxaxpx(heap,s,dx,axp,idrx,scale,d)
ENDPROC putEOR(heap,dx,ListItem([4,5,6],s),M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

PROC putEORI(heap,s,m,r,i)
   put16(heap,Shl(Shl(%0000101000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN put32(heap,i) ELSE put16(heap,i)
ENDPROC

EXPORT PROC eorlimmdx(heap,imm,dx) IS putEORI(heap,SIZE_L,M0,dx,imm)
EXPORT PROC eorwimmdx(heap,imm,dx) IS putEORI(heap,SIZE_W,M0,dx,imm)
EXPORT PROC eorbimmdx(heap,imm,dx) IS putEORI(heap,SIZE_B,M0,dx,imm)
EXPORT PROC eorlimmaxp(heap,imm,axp) IS putEORI(heap,SIZE_L,M2,axp,imm)
EXPORT PROC eorwimmaxp(heap,imm,axp) IS putEORI(heap,SIZE_W,M2,axp,imm)
EXPORT PROC eorbimmaxp(heap,imm,axp) IS putEORI(heap,SIZE_B,M2,axp,imm)
EXPORT PROC eorlimmaxpi(heap,imm,axp) IS putEORI(heap,SIZE_L,M3,axp,imm)
EXPORT PROC eorwimmaxpi(heap,imm,axp) IS putEORI(heap,SIZE_W,M3,axp,imm)
EXPORT PROC eorlimmaxpd(heap,imm,axp) IS putEORI(heap,SIZE_L,M4,axp,imm)
EXPORT PROC eorwimmaxpd(heap,imm,axp) IS putEORI(heap,SIZE_W,M4,axp,imm)
EXPORT PROC eorlimmaxpofs(heap,imm,axp,ofs)
ENDPROC putEORI(heap,SIZE_L,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC eorwimmaxpofs(heap,imm,axp,ofs)
ENDPROC putEORI(heap,SIZE_W,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC eorbimmaxpofs(heap,imm,axp,ofs)
ENDPROC putEORI(heap,SIZE_B,M5,axp,imm) BUT put16(heap,ofs)
EXPORT PROC eorlimmaxpx(heap,imm,axp,idrx,scale,d)
ENDPROC putEORI(heap,SIZE_L,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC eorwimmaxpx(heap,imm,axp,idrx,scale,d)
ENDPROC putEORI(heap,SIZE_W,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC eorbimmaxpx(heap,imm,axp,idrx,scale,d)
ENDPROC putEORI(heap,SIZE_B,M6,axp,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ NOT *****************/

PROC putNOT(heap,s,m,r)
   put16(heap,Shl(Shl(%0100011000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC notdx(heap,s,dx) IS putNOT(heap,s,M0,dx)
EXPORT PROC notaxp(heap,s,ax) IS putNOT(heap,s,M2,ax)
EXPORT PROC notaxpi(heap,s,ax) IS putNOT(heap,s,M3,ax)
EXPORT PROC notaxpd(heap,s,ax) IS putNOT(heap,s,M4,ax)
EXPORT PROC notaxpofs(heap,s,ax,ofs) IS putNOT(heap,s,M5,ax) BUT put16(heap,ofs)
EXPORT PROC notaxpx(heap,s,ax,idrx,scale,d) IS putNOT(heap,s,M6,ax) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ TST *****************/

PROC putTST(heap,s,m,r)
   put16(heap,Shl(Shl(%0100101000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC tstdx(heap,s,dx) IS putTST(heap,s,M0,dx)
EXPORT PROC tstax(heap,s,ax) IS putTST(heap,s,M1,ax)
EXPORT PROC tstaxp(heap,s,ax) IS putTST(heap,s,M2,ax)
EXPORT PROC tstaxpi(heap,s,ax) IS putTST(heap,s,M3,ax)
EXPORT PROC tstaxpd(heap,s,ax) IS putTST(heap,s,M4,ax)
EXPORT PROC tstaxpofs(heap,s,ax,ofs)
ENDPROC putTST(heap,s,M5,ax) BUT put16(heap,ofs)
EXPORT PROC tstaxpx(heap,s,ax,idrx,scale,d)
ENDPROC putTST(heap,s,M6,ax) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ Scc *****************/

EXPORT PROC sccdx(heap,cond,dx)
ENDPROC put16(heap,Shl(Shl(Shl(%01010000 OR cond,2) OR %11,3) OR M0,3) OR dx)

/************ ASL / ASR ************/

PROC putSHA(heap,nr,dr,s,ir,x,r)
   put16(heap,Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %00, 3) OR r)
ENDPROC

EXPORT PROC asldxdx(heap,s,dx1,dx2) IS putSHA(heap,dx1,1,s,1,%00,dx2)
EXPORT PROC aslimmdx(heap,s,imm,dx) IS putSHA(heap,IF imm=8 THEN 0 ELSE imm,1,s,0,%00,dx)
EXPORT PROC asrdxdx(heap,s,dx1,dx2) IS putSHA(heap,dx1,0,s,1,%00,dx2)
EXPORT PROC asrimmdx(heap,s,imm,dx) IS putSHA(heap,IF imm=8 THEN 0 ELSE imm,0,s,0,%00,dx)

/*********** LSL / LSR ****************/

PROC putSH(heap,nr,dr,s,ir,x,r)
   put16(heap,Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %01, 3) OR r)
ENDPROC

EXPORT PROC lsldxdx(heap,s,dx1,dx2) IS putSH(heap,dx1,1,s,1,%01,dx2)
EXPORT PROC lslimmdx(heap,s,imm,dx) IS putSH(heap,IF imm=8 THEN 0 ELSE imm,1,s,0,%01,dx)
EXPORT PROC lsrdxdx(heap,s,dx1,dx2) IS putSH(heap,dx1,0,s,1,%01,dx2)
EXPORT PROC lsrimmdx(heap,s,imm,dx) IS putSH(heap,IF imm=8 THEN 0 ELSE imm,0,s,0,%01,dx)

/*********** ROL / ROR ****************/

PROC putRO(heap,nr,dr,s,ir,x,r)
   put16(heap,Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %11, 3) OR r)
ENDPROC

EXPORT PROC roldxdx(heap,s,dx1,dx2) IS putRO(heap,dx1,1,s,1,%11,dx2)
EXPORT PROC rolimmdx(heap,s,imm,dx) IS putRO(heap,IF imm=8 THEN 0 ELSE imm,1,s,0,%11,dx)
EXPORT PROC rordxdx(heap,s,dx1,dx2) IS putRO(heap,dx1,0,s,1,%11,dx2)
EXPORT PROC rorimmdx(heap,s,imm,dx) IS putRO(heap,IF imm=8 THEN 0 ELSE imm,0,s,0,%11,dx)

/*********** ROXL / ROXR ****************/

PROC putROX(heap,nr,dr,s,ir,x,r)
   put16(heap,Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %10, 3) OR r)
ENDPROC

EXPORT PROC roxldxdx(heap,s,dx1,dx2) IS putROX(heap,dx1,1,s,1,%10,dx2)
EXPORT PROC roxlimmdx(heap,s,imm,dx) IS putROX(heap,IF imm=8 THEN 0 ELSE imm,1,s,0,%10,dx)
EXPORT PROC roxrdxdx(heap,s,dx1,dx2) IS putROX(heap,dx1,0,s,1,%10,dx2)
EXPORT PROC roxrimmdx(heap,s,imm,dx) IS putROX(heap,IF imm=8 THEN 0 ELSE imm,0,s,0,%10,dx)

/************ SWAP **********************/

EXPORT PROC swapdx(heap,dx) IS put16(heap,%0100100001000000 OR dx)

/************ EXG ***********************/

EXPORT PROC exgdxdx(heap,dx1,dx2)
ENDPROC put16(heap,Shl(Shl(Shl(%1100000 OR dx1,1) OR 1,5) OR %01000,3) OR dx2)
EXPORT PROC exgaxax(heap,ax1,ax2)
ENDPROC put16(heap,Shl(Shl(Shl(%1100000 OR ax1,1) OR 1,5) OR %01001,3) OR ax2)
EXPORT PROC exgdxax(heap,dx,ax)
ENDPROC put16(heap,Shl(Shl(Shl(%1100000 OR dx,1) OR 1,5) OR %10001,3) OR ax)

/************ LEA **********************/

PROC putLEA(heap,ax,m,r)
   put16(heap,Shl(Shl(Shl(%0100000 OR ax,3) OR %111,3) OR m,3) OR r)
ENDPROC

EXPORT PROC leaaxpax(axp,ax) IS putLEA(heap,ax,M2,axp)
EXPORT PROC leaaxpofsax(axp,ofs,ax)
   IF axp=ax
      IF Abs(ofs) < 9
         RETURN IF ofs > 0 THEN addqax(heap,SIZE_L,ofs,ax) ELSE subqax(heap,SIZE_L,-ofs,ax)
      ENDIF
   ENDIF
ENDPROC putLEA(heap,ax,M5,axp) BUT put16(heap,ofs)
EXPORT PROC leaaxpxax(heap,axp,idrx,scale,d,ax)
ENDPROC putLEA(heap,ax,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC leaabswax(heap,absw,ax)
ENDPROC putLEA(heap,ax,M7,0) BUT put16(heap,absw)
EXPORT PROC leaabslax(heap,absl,ax)
   putLEA(heap,ax,M7,1)
   put32(heap,absl)
ENDPROC
EXPORT PROC leapcpofsax(heap,ofs,ax)
ENDPROC putLEA(heap,ax,M7,2) BUT put16(heap,ofs)

/************ LINK *******************/

EXPORT PROC linkw(heap,ax,data)
ENDPROC put16(heap,%0100111001010000 OR ax) BUT put16(heap,data)
EXPORT PROC linkl(heap,ax,data)
   put16(heap,%0100100000001000 OR ax)
   put32(heap,data)
ENDPROC

/************ MOVE *******************/

#define Msize(_s_) ListItem([1,3,2],_s_)

EXPORT PROC putMOVE(heap,s,r1,m1,m2,r2)
   put16(heap,Shl(Shl(Shl(Shl(%0000 OR s,3) OR r1,3) OR m1,3) OR m2,3) OR r2)
ENDPROC

EXPORT PROC movedxdx(heap,s,dx1,dx2) IS putMOVE(heap,Msize(s),dx2,M0,M0,dx1)
EXPORT PROC movedxax(heap,s,dx,ax) IS putMOVE(heap,ListItem([0,3,2],s),ax,M1,M0,dx)
EXPORT PROC movedxaxp(heap,s,dx,axp) IS putMOVE(heap,Msize(s),axp,M2,M0,dx)
EXPORT PROC movedxaxpi(heap,s,dx,axp) IS putMOVE(heap,Msize(s),axp,M3,M0,dx)
EXPORT PROC movedxaxpd(heap,s,dx,axp) IS putMOVE(heap,Msize(s),axp,M4,M0,dx)
EXPORT PROC movedxaxpofs(heap,s,dx,axp,ofs) IS putMOVE(heap,Msize(s),axp,M5,M0,dx) BUT put16(heap,ofs)
EXPORT PROC movedxaxpx(heap,s,dx,axp,idrx,scale,d) IS putMOVE(heap,Msize(s),axp,M6,M0,dx) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxdx(heap,s,ax,dx) IS putMOVE(heap,Msize(s),dx,M0,M1,ax)
EXPORT PROC moveaxax(heap,s,ax1,ax2) IS putMOVE(heap,ListItem([0,3,2],s),ax2,M1,M1,ax1)
EXPORT PROC moveaxaxp(heap,s,ax,axp) IS putMOVE(heap,Msize(s),axp,M2,M1,ax)
EXPORT PROC moveaxaxpi(heap,s,ax,axp) IS putMOVE(heap,Msize(s),axp,M3,M1,ax)
EXPORT PROC moveaxaxpd(heap,s,ax,axp) IS putMOVE(heap,Msize(s),axp,M4,M1,ax)
EXPORT PROC moveaxaxpofs(heap,s,ax,axp,ofs) IS putMOVE(heap,Msize(s),axp,M5,M1,ax) BUT put16(heap,ofs)
EXPORT PROC moveaxaxpx(heap,s,ax,axp,idrx,scale,d) IS putMOVE(heap,Msize(s),axp,M6,M1,ax) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC moveaxpdx(heap,s,axp,dx) IS putMOVE(heap,Msize(s),dx,M0,M2,axp)
EXPORT PROC moveaxpax(heap,s,axp,ax) IS putMOVE(heap,ListItem([0,3,2],s),ax,M1,M2,axp)
EXPORT PROC moveaxpaxp(heap,s,axp1,axp2) IS putMOVE(heap,Msize(s),axp2,M2,M2,axp1)
EXPORT PROC moveaxpaxpi(heap,s,axp,axpi) IS putMOVE(heap,Msize(s),axpi,M3,M2,axp)
EXPORT PROC moveaxpaxpd(heap,s,axp,axpd) IS putMOVE(heap,Msize(s),axpd,M4,M2,axp)
EXPORT PROC moveaxpaxpofs(heap,s,axp,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M2,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpaxpx(heap,s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M2,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC moveaxpidx(heap,s,axp,dx) IS putMOVE(heap,Msize(s),dx,M0,M3,axp)
EXPORT PROC moveaxpiax(heap,s,axp,ax) IS putMOVE(heap,ListItem([0,3,2],s),ax,M1,M3,axp)
EXPORT PROC moveaxpiaxp(heap,s,axp1,axp2) IS putMOVE(heap,Msize(s),axp2,M2,M3,axp1)
EXPORT PROC moveaxpiaxpi(heap,s,axp,axpi) IS putMOVE(heap,Msize(s),axpi,M3,M3,axp)
EXPORT PROC moveaxpiaxpd(heap,s,axp,axpd) IS putMOVE(heap,Msize(s),axpd,M4,M3,axp)
EXPORT PROC moveaxpiaxpofs(heap,s,axp,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M3,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpiaxpx(heap,s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M3,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC moveaxpddx(heap,s,axp,dx) IS putMOVE(heap,Msize(s),dx,M0,M4,axp)
EXPORT PROC moveaxpdax(heap,s,axp,ax) IS putMOVE(heap,ListItem([0,3,2],s),ax,M1,M4,axp)
EXPORT PROC moveaxpdaxp(heap,s,axp1,axp2) IS putMOVE(heap,Msize(s),axp2,M2,M4,axp1)
EXPORT PROC moveaxpdaxpi(heap,s,axp,axpi) IS putMOVE(heap,Msize(s),axpi,M3,M4,axp)
EXPORT PROC moveaxpdaxpd(heap,s,axp,axpd) IS putMOVE(heap,Msize(s),axpd,M4,M4,axp)
EXPORT PROC moveaxpdaxpofs(heap,s,axp,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M4,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpdaxpx(heap,s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M4,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC moveaxpofsdx(heap,s,axp,ofs,dx)
ENDPROC putMOVE(heap,Msize(s),dx,M0,M5,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsax(heap,s,axp,ofs,ax)
ENDPROC putMOVE(heap,ListItem([0,3,2],s),ax,M1,M5,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsaxp(heap,s,axp1,ofs,axp2)
ENDPROC putMOVE(heap,Msize(s),axp2,M2,M5,axp1) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsaxpi(heap,s,axp,ofs,axpi)
ENDPROC putMOVE(heap,Msize(s),axpi,M3,M5,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsaxpd(heap,s,axp,ofs,axpd)
ENDPROC putMOVE(heap,Msize(s),axpd,M4,M5,axp) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsaxpofs(heap,s,axp,ofs1,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M5,axp) BUT put16(heap,ofs1) BUT put16(heap,ofs)
EXPORT PROC moveaxpofsaxpx(heap,s,axp,ofs,axp2,idrx,scale,d)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M5,axp) BUT put16(heap,ofs) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC moveaxpxdx(heap,s,axp,idrx,scale,d,dx)
ENDPROC putMOVE(heap,Msize(s),dx,M0,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxpxax(heap,s,axp,idrx,scale,d,ax)
ENDPROC putMOVE(heap,ListItem([0,3,2],s),ax,M1,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxpxaxp(heap,s,axp1,idrx,scale,d,axp2)
ENDPROC putMOVE(heap,Msize(s),axp2,M2,M6,axp1) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxpxaxpi(heap,s,axp,idrx,scale,d,axpi)
ENDPROC putMOVE(heap,Msize(s),axpi,M3,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxpxaxpd(heap,s,axp,idrx,scale,d,axpd)
ENDPROC putMOVE(heap,Msize(s),axpd,M4,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC moveaxpxaxpofs(heap,s,axp,idrx,scale,d,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d)) BUT put16(heap,ofs)
EXPORT PROC moveaxpxaxpx(heap,s,axp,idrx,scale,d1,axp2,idrx2,scale2,d2)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M6,axp) BUT put16(heap,IxExt(heap,idrx,scale,d1)) BUT put16(heap,IxExt(heap,idrx2,scale2,d2))

EXPORT PROC movepcpofsdx(heap,s,ofs,dx)
ENDPROC putMOVE(heap,Msize(s),dx,M0,M7,2) BUT put16(heap,ofs)
EXPORT PROC movepcpofsax(heap,s,ofs,ax)
ENDPROC putMOVE(heap,ListItem([0,3,2],s),ax,M1,M7,2) BUT put16(heap,ofs)
EXPORT PROC movepcpofsaxp(heap,s,ofs,axp2)
ENDPROC putMOVE(heap,Msize(s),axp2,M2,M7,2) BUT put16(heap,ofs)
EXPORT PROC movepcpofsaxpi(heap,s,ofs,axpi)
ENDPROC putMOVE(heap,Msize(s),axpi,M3,M7,2) BUT put16(heap,ofs)
EXPORT PROC movepcpofsaxpd(heap,s,ofs,axpd)
ENDPROC putMOVE(heap,Msize(s),axpd,M4,M7,2) BUT put16(heap,ofs)
EXPORT PROC movepcpofsaxpofs(heap,s,ofs1,axp2,ofs)
ENDPROC putMOVE(heap,Msize(s),axp2,M5,M7,2) BUT put16(heap,ofs1) BUT put16(heap,ofs)
EXPORT PROC movepcpofsaxpx(heap,s,ofs,axp2,idrx,scale,d)
ENDPROC putMOVE(heap,Msize(s),axp2,M6,M7,2) BUT put16(heap,ofs) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC putMOVEI(heap,s,r1,m1,imm)
   put16(heap,Shl(Shl(Shl(Shl(Msize(s),3) OR r1,3) OR m1,3) OR M7,3) OR 4)
   IF s = SIZE_L THEN put32(heap,imm) ELSE put16(heap,imm)
ENDPROC

EXPORT PROC movelimmdx(heap,imm,dx)
   IF imm <> $80000000 -> fix
      IF Abs(imm) < 8 THEN RETURN moveqdx(heap,imm,dx)
   ENDIF
ENDPROC putMOVEI(heap,SIZE_L,dx,M0,imm)
EXPORT PROC movelimmax(heap,imm,ax)
   IF imm=0
      RETURN subaxax(heap,SIZE_L,ax,ax)
   ELSEIF imm = $80000000 -> fix
      ->
   ELSEIF Abs(imm) < 32767
      RETURN movewimmax(heap,imm,ax)
   ENDIF
ENDPROC putMOVEI(heap,SIZE_L,ax,M1,imm)
EXPORT PROC movelimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN clraxp(heap,SIZE_L,axp)
ENDPROC putMOVEI(heap,SIZE_L,axp,M2,imm)
EXPORT PROC movelimmaxpi(heap,imm,axp)
   IF imm=0 THEN RETURN clraxpi(heap,SIZE_L,axp)
ENDPROC putMOVEI(heap,SIZE_L,axp,M3,imm)
EXPORT PROC movelimmaxpd(heap,imm,axp)
   IF imm=0
      RETURN clraxpd(heap,SIZE_L,axp)
   ELSEIF axp=7
      RETURN peaabsl(heap,imm)
   ENDIF
ENDPROC putMOVEI(heap,SIZE_L,axp,M4,imm)
EXPORT PROC movelimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(heap,SIZE_L,axp,ofs)
ENDPROC putMOVEI(heap,SIZE_L,axp,M5,imm) BUT put16(heap,ofs)
EXPORT PROC movelimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(heap,SIZE_L,axp,idrx,scale,d)
ENDPROC putMOVEI(heap,SIZE_L,axp,M6,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))
EXPORT PROC movewimmdx(heap,imm,dx)
   IF imm=0 THEN RETURN clrdx(heap,SIZE_W,dx)
ENDPROC putMOVEI(1,dx,M0,imm)
EXPORT PROC movewimmax(heap,imm,ax)
ENDPROC putMOVEI(1,ax,M1,imm)
EXPORT PROC movewimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN clraxp(heap,SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M2,imm)
EXPORT PROC movewimmaxpi(heap,imm,axp)
   IF imm=0 THEN RETURN clraxpi(heap,SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M3,imm)
EXPORT PROC movewimmaxpd(heap,imm,axp)
   IF imm=0 THEN RETURN clraxpd(heap,SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M4,imm)
EXPORT PROC movewimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(heap,SIZE_W,axp,ofs)
ENDPROC putMOVEI(1,axp,M5,imm) BUT put16(heap,ofs)
EXPORT PROC movewimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(heap,SIZE_W,axp,idrx,scale,d)
ENDPROC putMOVEI(1,axp,M6,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

EXPORT PROC movebimmdx(heap,imm,dx)
   IF imm=0 THEN RETURN clrdx(heap,SIZE_B,dx)
ENDPROC putMOVEI(0,dx,M0,imm)
EXPORT PROC movebimmaxp(heap,imm,axp)
   IF imm=0 THEN RETURN clraxp(heap,SIZE_B,axp)
ENDPROC putMOVEI(0,axp,M2,imm)
EXPORT PROC movebimmaxpofs(heap,imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(heap,SIZE_B,axp,ofs)
ENDPROC putMOVEI(0,axp,M5,imm) BUT put16(heap,ofs)
EXPORT PROC movebimmaxpx(heap,imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(heap,SIZE_B,axp,idrx,scale,d)
ENDPROC putMOVEI(0,axp,M6,imm) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************* MOVEM **************/

EXPORT PROC movemregsaxp(heap,s,mask,axp)
   put16(heap,Shl(Shl(%0100100010 OR ListItem([0,0,1],s),3) OR M2,3) OR axp)
   put16(heap,mask)
ENDPROC
EXPORT PROC movemregsaxpd(heap,s,mask,axp)
   put16(heap,Shl(Shl(%0100100010 OR ListItem([0,0,1],s),3) OR M4,3) OR axp)
   put16(heap,mask)
ENDPROC
EXPORT PROC movemaxpregs(heap,s,axp,mask)
   put16(heap,Shl(Shl(%0100110010 OR ListItem([0,0,1],s),3) OR M2,3) OR axp)
   put16(heap,mask)
ENDPROC
EXPORT PROC movemaxpiregs(heap,s,axp,mask)
   put16(heap,Shl(Shl(%0100110010 OR ListItem([0,0,1],s),3) OR M3,3) OR axp)
   put16(heap,mask)
ENDPROC

/************ MOVEQ ***************/

EXPORT PROC moveqdx(heap,imm,dx)
ENDPROC put16(heap,Shl(%0111000 OR dx,9) OR (imm AND $FF))

/************ PEA *****************/

EXPORT PROC peaaxp(heap,axp) IS put16(heap,%0100100001010000 OR axp)
EXPORT PROC peaaxpofs(heap,axp,ofs) IS put16(heap,%0100100001101000 OR axp) BUT put16(heap,ofs)
EXPORT PROC peaabsw(heap,word) IS put16(heap,%0100100001111000) BUT put16(heap,word)
EXPORT PROC peaabsl(heap,long)
   IF long <> $80000000  -> fix
      IF Abs(long) < 32767 THEN RETURN peaabsw(heap,long)
   ENDIF
   put16(heap,%0100100001111001)
   put32(heap,long)
ENDPROC
EXPORT PROC peapcpofs(ofs)
   put16(heap,%0100100001111010)
   put16(heap,ofs)
ENDPROC

/*********** UNLK ******************/

EXPORT PROC unlkax(heap,ax) IS put16(heap,%0100111001011000 OR ax)

/*********** Bcc ******************/

EXPORT PROC bccofs8(heap,cc,ofs) IS put16(heap,Shl(%01100000 OR cc,8) OR (ofs AND $FF))
EXPORT PROC bccofs16(heap,cc,ofs)
ENDPROC put16(heap,Shl(%01100000 OR cc,8) OR $00) BUT put16(heap,ofs)
EXPORT PROC bccofs32(heap,cc,ofs)
   put16(heap,Shl(%01100000 OR cc,8) OR $FF)
   put32(heap,ofs)
ENDPROC

/*********** DBcc ****************/

EXPORT PROC dbccofs(heap,cc,dx,ofs)
   put16(heap,Shl(Shl(%01010000 OR cc,5) OR %11001,3) OR dx)
   put16(heap,ofs)
ENDPROC

/*********** BSR *****************/

->PROC bsrofs8(ofs) IS put16(heap,%0110000100000000 OR (ofs AND $FF))
EXPORT PROC bsrofs16(heap,ofs)
ENDPROC put16(heap,%0110000100000000) BUT put16(heap,ofs)
EXPORT PROC bsrofs32(heap,ofs)
   put16(heap,%0110000111111111)
   put32(heap,ofs)
ENDPROC

/************ JMP ****************/

EXPORT PROC jmpaxp(heap,axp) IS put16(heap,%0100111011010000 OR axp)
EXPORT PROC jmpaxpofs(axp,ofs)
ENDPROC put16(heap,%0100111011101000 OR axp) BUT put16(heap,ofs)
EXPORT PROC jmpaxpx(heap,axp,idrx,scale,d)
ENDPROC put16(heap,%0100111011110000 OR axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/************ JSR ****************/

EXPORT PROC jsraxp(heap,axp) IS put16(heap,%0100111010010000 OR axp)
EXPORT PROC jsraxpofs(heap,axp,ofs)
ENDPROC put16(heap,%0100111010101000 OR axp) BUT put16(heap,ofs)
EXPORT PROC jsraxpx(axp,idrx,scale,d)
ENDPROC put16(heap,%0100111010110000 OR axp) BUT put16(heap,IxExt(heap,idrx,scale,d))

/*********** RTD ****************/

EXPORT PROC rtdofs(heap,ofs) IS put16(heap,%0100111001110100) BUT put16(heap,ofs)

/*********** RTS ****************/

EXPORT PROC rts_(heap) IS put16(heap,%0100111001110101)

/*********** RTR ****************/

EXPORT PROC rtr_(heap) IS put16(heap,%0100111001110111)

/*********** RTE ****************/

EXPORT PROC rte_(heap) IS put16(heap,%0100111001110011)



/* ------- FLOATING POINT -------- */


/*********** FABS ********************/

EXPORT PROC fabsfpxfpx(heap,fpx1,fpx2)
   put16(heap,%111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2, 7) OR %001100)
ENDPROC

/*********** FADD *****************/

EXPORT PROC faddfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(fpx1,3) OR fpx2,7) OR %0100010)
ENDPROC
EXPORT PROC fadddxfpx(heap,fs,dx,fpx)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(Shl(Shl(dx,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
ENDPROC
EXPORT PROC faddaxpfpx(heap,fs,axp,ofs,fpx)
   put16(heap,%1111001000001000)
   put16(heap,Shl(Shl(Shl(Shl(axp,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
ENDPROC
EXPORT PROC faddaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,%1111001000010100)
   put16(heap,Shl(Shl(Shl(Shl(axp,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
   put16(heap,ofs)
ENDPROC
EXPORT PROC faddsimmfpx(heap,imm,fpx)
   put16(heap,%1111001000011110)
   put16(heap,Shl(Shl(%0010000 OR FS,3) OR fpx,7) OR %0100010)
   put32(heap,imm)
ENDPROC


/*********** FCMP ***************/

EXPORT PROC fcmpfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpdxfpx(heap,fs,dx,fpx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpaxpfpx(heap,fs,axp,fpx)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M5,6) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
   put16(heap,ofs)
ENDPROC
EXPORT PROC fcmpsimmfpx(heap,imm,fpx)
   put16(heap,Shl(%1111001000000 OR M7,3) OR 4)
   put16(heap,Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0111000)
   put32(heap,imm)
ENDPROC

/*********** FDIV *************/

EXPORT PROC fdivfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0100000)
ENDPROC
EXPORT PROC fdivdxfpx(heap,fs,dx,fpx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
ENDPROC
EXPORT PROC fdivaxpfpx(heap,fs,axp,fpx)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
ENDPROC
EXPORT PROC fdivaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M5,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
   put16(heap,ofs)
ENDPROC
EXPORT PROC fdivsimmfpx(heap,imm,fpx)
   put16(heap,Shl(%1111001000000 OR M7,3) OR 4)
   put16(heap,Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0100000)
   put32(heap,imm)
ENDPROC

/************ FMOVE ************/

EXPORT PROC fmovefpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0000000)
ENDPROC
EXPORT PROC fmovedxfpx(heap,fs,dx,fpx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpfpx(heap,fs,axp,fpx)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpifpx(heap,fs,axp,fpx)
   put16(heap,Shl(%1111001000000 OR M3,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M5,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
   put16(heap,ofs)
ENDPROC

EXPORT PROC fmovesimmfpx(heap,imm,fpx)
   put16(heap,Shl(%1111001000000 OR M7,3) OR 4)
   put16(heap,Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0000000)
   put32(heap,imm)
ENDPROC

EXPORT PROC fmovefpxdx(heap,fs,fpx,dx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxp(heap,fs,fpx,axp)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxpd(heap,fs,fpx,axp)
   put16(heap,Shl(%1111001000000 OR M4,3) OR axp)
   put16(heap,Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxpofs(heap,fs,fpx,axp,ofs)
   put16(heap,Shl(%1111001000000 OR M5,3) OR axp)
   put16(heap,Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
   put16(heap,ofs)
ENDPROC


/*********** FMUL *************/

EXPORT PROC fmulfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0100011)
ENDPROC
EXPORT PROC fmuldxfpx(heap,fs,dx,fpx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
ENDPROC
EXPORT PROC fmulaxpfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
ENDPROC
EXPORT PROC fmulaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M5,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
   put16(heap,ofs)
ENDPROC
EXPORT PROC fmulsimmfpx(heap,imm,fpx)
   put16(heap,Shl(%1111001000000 OR M7,3) OR 4)
   put16(heap,Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0100011)
   put32(heap,imm)
ENDPROC

/*********** FNEG **************/

EXPORT PROC fnegfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,16) OR fpx2,7) OR %0011010)
ENDPROC

/************ FScc *************/

EXPORT PROC fsccdx(heap,cond,dx)
   put16(heap,Shl(%1111001001000 OR M0,3) OR dx)
   put16(heap,cond)
ENDPROC

/*********** FSUB *****************/

EXPORT PROC fsubfpxfpx(heap,fpx1,fpx2)
   put16(heap,%1111001000000000)
   put16(heap,Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0101000)
ENDPROC
EXPORT PROC fsubdxfpx(heap,fs,dx,fpx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
ENDPROC
EXPORT PROC fsubaxpfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M2,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
ENDPROC
EXPORT PROC fsubaxpofsfpx(heap,fs,axp,ofs,fpx)
   put16(heap,Shl(%1111001000000 OR M5,3) OR axp)
   put16(heap,Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
   put16(heap,ofs)
ENDPROC
EXPORT PROC fsubsimmfpx(heap,imm,fpx)
   put16(heap,Shl(%1111001000000 OR M7,3) OR 4)
   put16(heap,Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0101000)
   put32(heap,imm)
ENDPROC

EXPORT SET FPIAR,
    FPSR,
    FPCR

EXPORT PROC fmoveldxfpcr(heap,dx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(%100000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelfpcrdx(heap,dx)
   put16(heap,Shl(%1111001000000 OR M0,3) OR dx)
   put16(heap,Shl(%101000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelaxpifpcr(heap,ax)
   put16(heap,Shl(%1111001000000 OR M3,3) OR ax)
   put16(heap,Shl(%100000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelfpcraxpd(heap,ax)
   put16(heap,Shl(%1111001000000 OR M4,3) OR ax)
   put16(heap,Shl(%101000 OR FPCR,10))
ENDPROC


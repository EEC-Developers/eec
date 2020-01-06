
-> ECX/opcodes68.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */


-> moved opcode stuff into this new module 2008

OPT MODULE
OPT PREPROCESS
OPT LARGE

EXPORT DEF g_codeptr:PTR TO LONG

/* assembler 68k */

EXPORT ENUM M0,M1,M2,M3,M4,M5,M6,M7
EXPORT ENUM SIZE_B,SIZE_W,SIZE_L   -> was B,W,L
EXPORT ENUM T,F,HI,LS,CC,CS,NE,EQ,VC,VS,PL,MI,GE,LT,GT,LE

EXPORT ENUM FL,FS,FX,FP,FW,FD,FB
EXPORT ENUM FCF,FCEQ,FCOGT,FCOGE,FCOLT,FCOLE,FCOGL,
            FCOR,FCUN,FCUEQ,FCUGT,FCUGE,FCULT,FCULE,
            FCNE,FCT,FCSF,FCSEQ,FCGT,FCGE,FCLT,FCLE,
            FCGL,FCGLE,FCNGLE,FCNGL,FCNLE,FCNLT,FCNGE,FCNGT

#define Put32(v) g_codeptr[]++ := v

EXPORT PROC put16(v)
   PutInt(g_codeptr, v)
   g_codeptr := g_codeptr + 2
ENDPROC

EXPORT PROC put8(v)
   PutChar(g_codeptr, v)
   g_codeptr := g_codeptr + 1
ENDPROC

/**************************************************************
***************************************************************
******************** 68K OPCODES  *****************************
***************************************************************
**************************************************************/

PROC extW(da,reg,wl,scale,bd)
   DEF t=NIL
   t := Shl(Shl(Shl(Shl(da,3) OR reg, 1) OR wl,2) OR scale, 9) OR (bd AND $FF)
ENDPROC t


->#define BD32EXT extW(0,0,0,0,0,1,3,0)
#define IxExt(idrx,scale,bd) extW(0,idrx,1,scDwn(scale), bd)

-> 1/2/4/8 to 0/1/2/3
#define scDwn(sc) ListItem([0,0,1,0,2,0,0,0,3],sc)



-> optimisations implemented:
-> add.s #i,xxx / sub.s #i,xxx -> addq.s #i,xxx / subq.s #i,xxx
-> addq #0,xxx / subq #0,xxx -> remove
-> divs.l #1, ... / divu.l #1, ... -> remove
-> muls.l #1, ... / mulu.l #1, ... -> remove
-> and.l #$FFFFFF00, dx -> clr.b dx
-> and.l #$FFFF0000, dx -> clr.w dx
-> and.w #$FF00, dx -> clr.b dx
-> clr.l dx -> moveq #0, dx
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

PROC putADD(reg,size,mode,mreg)
   put16(Shl(Shl(Shl(%1101000 OR reg,3) OR size,3) OR mode,3) OR mreg)
ENDPROC

EXPORT PROC adddxdx(s,dx1,dx2) IS putADD(dx2,s,M0,dx1)
EXPORT PROC adddxax(s,dx,ax) IS putADD(ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC adddxaxp(s,dx,ax) IS putADD(dx,s+4,M2,ax)
EXPORT PROC adddxaxpi(s,dx,ax) IS putADD(dx,s+4,M3,ax)
EXPORT PROC adddxaxpd(s,dx,ax) IS putADD(dx,s+4,M4,ax)
EXPORT PROC adddxaxpofs(s,dx,ax,ofs)
   putADD(dx,s+4,M5,ax)
   put16(ofs)
ENDPROC
EXPORT PROC adddxaxpx(s,dx,ax,idrx,scale,d)
   putADD(dx,s+4,M6,ax)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC addaxdx(s,ax,dx) IS putADD(dx,s,M1,ax)
EXPORT PROC addaxax(s,ax1,ax2) IS putADD(ax2,ListItem([0,3,7],s),M1,ax1)

EXPORT PROC addlimmax(imm,ax)
   IF imm < 9 AND (imm > 0)
         RETURN addqax(SIZE_L,imm,ax)
   ELSEIF imm = $80000000  -> fix

   ELSEIF Abs(imm) < 32768
         RETURN leaaxpofsax(ax,imm,ax)
   ENDIF
   putADD(ax,7,M7,4)
   Put32(imm)
ENDPROC
EXPORT PROC addwimmax(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN addqax(SIZE_W,imm,ax)
   putADD(ax,3,M7,4)
   put16(imm)
ENDPROC
EXPORT PROC addaxpdx(s,axp,dx) IS putADD(dx,s,M2,axp)
EXPORT PROC addaxpax(s,axp,ax) IS putADD(ax,ListItem([0,3,7],s),M2,axp)
EXPORT PROC addaxpidx(s,axp,dx) IS putADD(dx,s,M3,axp)
EXPORT PROC addaxpddx(s,axp,dx) IS putADD(dx,s,M4,axp)
EXPORT PROC addaxpofsdx(s,axp,ofs,dx)
   putADD(dx,s,M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC addaxpofsax(s,axp,ofs,dx)
   putADD(dx,ListItem([0,3,7],s),M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC addaxpxdx(s,axp,idrx,scale,d,dx)
   putADD(dx,s,M6,axp)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC addaxpxax(s,axp,idrx,scale,d,dx)
   putADD(dx,ListItem([0,3,7],s),M6,axp)
   put16(IxExt(idrx,scale,d))
ENDPROC

PROC putADDI(size,mode,mreg, imm)
   put16(Shl(Shl(%0000011000 OR size,3) OR mode,3) OR mreg)
   IF size = 2
      Put32(imm)
   ELSE
      put16(imm)
   ENDIF
ENDPROC

EXPORT PROC addlimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(SIZE_L,imm,dx) ELSE putADDI(SIZE_L,M0,dx,imm)
ENDPROC

EXPORT PROC addwimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(SIZE_W,imm,dx) ELSE putADDI(SIZE_W,M0,dx,imm)
ENDPROC
EXPORT PROC addbimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN addqdx(SIZE_B,imm,dx) ELSE putADDI(SIZE_B,M0,dx,imm)
ENDPROC

EXPORT PROC addlimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(SIZE_L,imm,ax) ELSE putADDI(SIZE_L,M2,ax,imm)
ENDPROC

EXPORT PROC addwimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(SIZE_W,imm,ax) ELSE putADDI(SIZE_W,M2,ax,imm)
ENDPROC

EXPORT PROC addbimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(SIZE_B,imm,ax) ELSE putADDI(SIZE_B,M2,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpi(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpi(SIZE_L,imm,ax) ELSE putADDI(SIZE_L,M3,ax,imm)
ENDPROC

EXPORT PROC addwimmaxpi(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxp(SIZE_W,imm,ax) ELSE putADDI(SIZE_W,M3,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpd(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpd(SIZE_L,imm,ax) ELSE putADDI(SIZE_L,M4,ax,imm)
ENDPROC

EXPORT PROC addwimmaxpd(imm,ax)
   IF imm < 9 AND (imm > 0) THEN addqaxpd(SIZE_W,imm,ax) ELSE putADDI(SIZE_W,M4,ax,imm)
ENDPROC

EXPORT PROC addlimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(SIZE_L,imm,ax,ofs)
   putADDI(SIZE_L,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC addwimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(SIZE_W,imm,ax,ofs)
   putADDI(SIZE_W,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC addbimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpofs(SIZE_B,imm,ax,ofs)
   putADDI(SIZE_B,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC addlimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(SIZE_L,imm,ax,idrx,scale,d)
   putADDI(SIZE_L,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC addwimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(SIZE_W,imm,ax,idrx,scale,d)
   putADDI(SIZE_W,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC addbimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN addqaxpx(SIZE_B,imm,ax,idrx,scale,d)
   putADDI(SIZE_B,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC


/********* ADDQ ***********/

PROC putADDQ(d,s,m,r)
   IF d = NIL THEN RETURN
   put16(Shl(Shl(Shl(%01010000 OR Shl(IF d = 8 THEN 0 ELSE d,1),2) OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC addqdx(s,d,dx) IS putADDQ(d,s,M0,dx)
EXPORT PROC addqax(s,d,ax) IS putADDQ(d,s,M1,ax)
EXPORT PROC addqaxp(s,d,axp) IS putADDQ(d,s,M2,axp)
EXPORT PROC addqaxpi(s,d,axp) IS putADDQ(d,s,M3,axp)
EXPORT PROC addqaxpd(s,d,axp) IS putADDQ(d,s,M4,axp)
EXPORT PROC addqaxpofs(s,d,axp,ofs)
   putADDQ(d,s,M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC addqaxpx(s,d,axp,idrx,scale,dis)
   putADDQ(d,s,M6,axp)
   put16(IxExt(idrx,scale,dis))
ENDPROC

/********* ADDX ***********/

PROC putADDX(rx,s,rm,ry)
   put16(Shl(Shl(Shl(%1101000 OR rx, 3) OR %100 OR s, 3) OR rm, 3) OR ry)
ENDPROC

EXPORT PROC addxdxdx(s,dx1,dx2) IS putADDX(dx2,s,0,dx1)
EXPORT PROC addxaxpdaxpd(s,ax1,ax2) IS putADDX(ax2,s,1,ax1)

/********* SUBX ***********/

PROC putSUBX(rx,s,rm,ry)
   put16(Shl(Shl(Shl(%1001000 OR rx, 3) OR %100 OR s, 3) OR rm, 3) OR ry)
ENDPROC

EXPORT PROC subxdxdx(s,dx1,dx2) IS putSUBX(dx2,s,0,dx1)
EXPORT PROC subxaxpdaxpd(s,ax1,ax2) IS putSUBX(ax2,s,1,ax1)

/********* SUB ************/

PROC putSUB(r,s,m,mr)
   put16(Shl(Shl(Shl(%1001000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC subdxdx(s,dx1,dx2) IS putSUB(dx2,s,M0,dx1)
EXPORT PROC subdxax(s,dx,ax) IS putSUB(ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC subdxaxp(s,dx,ax) IS putSUB(dx,s+4,M2,ax)
EXPORT PROC subdxaxpi(s,dx,ax) IS putSUB(dx,s+4,M3,ax)
EXPORT PROC subdxaxpd(s,dx,ax) IS putSUB(dx,s+4,M4,ax)
EXPORT PROC subdxaxpofs(s,dx,ax,ofs)
   putSUB(dx,s+4,M5,ax)
   put16(ofs)
ENDPROC
EXPORT PROC subdxaxpx(s,dx,ax,idrx,scale,d)
   putSUB(dx,s+4,M6,ax)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC subaxdx(s,ax,dx) IS putSUB(dx,s,M1,ax)
EXPORT PROC subaxax(s,ax1,ax2) IS putSUB(ax2,ListItem([0,3,7],s),M1,ax1)
EXPORT PROC sublimmax(imm,ax)
   IF imm < 9 AND (imm > 0)
      RETURN subqax(SIZE_L,imm,ax)
   ELSEIF imm = $80000000 -> fix

   ELSEIF Abs(imm) < 32768
      RETURN leaaxpofsax(ax,-imm,ax)
   ENDIF
   putSUB(ax,ListItem([0,3,7],SIZE_L),M7,4)
   Put32(imm)
ENDPROC
EXPORT PROC subwimmax(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqax(SIZE_W,imm,ax)
   putSUB(ax,ListItem([0,3,7],SIZE_W),M7,4)
   put16(imm)
ENDPROC

EXPORT PROC subaxpdx(s,axp,dx) IS putSUB(dx,s,M2,axp)
EXPORT PROC subaxpax(s,axp,dx) IS putSUB(dx,ListItem([0,3,7],s),M2,axp)
EXPORT PROC subaxpidx(s,axp,dx) IS putSUB(dx,s,M3,axp)
EXPORT PROC subaxpddx(s,axp,dx) IS putSUB(dx,s,M4,axp)
EXPORT PROC subaxpofsdx(s,axp,ofs,dx)
   putSUB(dx,s,M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC subaxpofsax(s,axp,ofs,dx)
   putSUB(dx,ListItem([0,3,7],s),M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC subaxpxdx(s,axp,idrx,scale,d,dx)
   putSUB(dx,s,M6,axp)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC subaxpxax(s,axp,idrx,scale,d,dx)
   putSUB(dx,ListItem([0,3,7],s),M6,axp)
   put16(IxExt(idrx,scale,d))
ENDPROC

PROC putSUBI(s,m,r,i)
   put16(Shl(Shl(%0000010000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN Put32(i) ELSE put16(i)
ENDPROC

EXPORT PROC sublimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(SIZE_L,imm,dx)
   putSUBI(SIZE_L,M0,dx,imm)
ENDPROC
EXPORT PROC subwimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(SIZE_W,imm,dx)
   putSUBI(SIZE_W,M0,dx,imm)
ENDPROC
EXPORT PROC subbimmdx(imm,dx)
   IF imm < 9 AND (imm > 0) THEN RETURN subqdx(SIZE_B,imm,dx)
   putSUBI(SIZE_B,M0,dx,imm)
ENDPROC
EXPORT PROC sublimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(SIZE_L,imm,ax)
   putSUBI(SIZE_L,M2,ax,imm)
ENDPROC
EXPORT PROC subwimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(SIZE_W,imm,ax)
   putSUBI(SIZE_W,M2,ax,imm)
ENDPROC
EXPORT PROC subbimmaxp(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxp(SIZE_B,imm,ax)
   putSUBI(SIZE_B,M2,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpi(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpi(SIZE_L,imm,ax)
   putSUBI(SIZE_L,M3,ax,imm)
ENDPROC
EXPORT PROC subwimmaxpi(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpi(SIZE_W,imm,ax)
   putSUBI(SIZE_W,M3,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpd(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpd(SIZE_L,imm,ax)
   putSUBI(SIZE_L,M4,ax,imm)
ENDPROC
EXPORT PROC subwimmaxpd(imm,ax)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpd(SIZE_W,imm,ax)
   putSUBI(SIZE_W,M4,ax,imm)
ENDPROC
EXPORT PROC sublimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(SIZE_L,imm,ax,ofs)
   putSUBI(SIZE_L,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC subwimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(SIZE_W,imm,ax,ofs)
   putSUBI(SIZE_W,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC subbimmaxpofs(imm,ax,ofs)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpofs(SIZE_B,imm,ax,ofs)
   putSUBI(SIZE_B,M5,ax,imm)
   put16(ofs)
ENDPROC
EXPORT PROC sublimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(SIZE_L,imm,ax,idrx,scale,d)
   putSUBI(SIZE_L,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC subwimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(SIZE_W,imm,ax,idrx,scale,d)
   putSUBI(SIZE_W,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC
EXPORT PROC subbimmaxpx(imm,ax,idrx,scale,d)
   IF imm < 9 AND (imm > 0) THEN RETURN subqaxpx(SIZE_B,imm,ax,idrx,scale,d)
   putSUBI(SIZE_B,M6,ax,imm)
   put16(IxExt(idrx,scale,d))
ENDPROC

/********* SUBQ ***********/

PROC putSUBQ(d,s,m,r)
   IF d = NIL THEN RETURN
   put16(Shl(Shl(Shl(%01010001 OR Shl(IF d = 8 THEN 0 ELSE d,1),2) OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC subqdx(s,d,dx) IS putSUBQ(d,s,M0,dx)
EXPORT PROC subqax(s,d,ax) IS putSUBQ(d,s,M1,ax)
EXPORT PROC subqaxp(s,d,axp) IS putSUBQ(d,s,M2,axp)
EXPORT PROC subqaxpi(s,d,axp) IS putSUBQ(d,s,M3,axp)
EXPORT PROC subqaxpd(s,d,axp) IS putSUBQ(d,s,M4,axp)
EXPORT PROC subqaxpofs(s,d,axp,ofs)
   putSUBQ(d,s,M5,axp)
   put16(ofs)
ENDPROC
EXPORT PROC subqaxpx(s,d,axp,idrx,scale,dis)
   putSUBQ(d,s,M6,axp)
   put16(IxExt(idrx,scale,dis))
ENDPROC

/******** DIVS.L / DIVU.L **********/

-> fixed 1.10.0
PROC putDIVL(m,dx,dq,s,dr,sus)
   put16(Shl(%0100110001000 OR m,3) OR dx)
   put16(Shl(Shl(Shl(dq,1) OR sus,1) OR s,10) OR dr)
ENDPROC
EXPORT PROC divsldxdrdq(s,dx,dr,dq)
ENDPROC putDIVL(M0,dx,dq,s,dr,1)
EXPORT PROC divslaxpdrdq(s,axp,dr,dq)
ENDPROC putDIVL(M2,axp,dq,s,dr,1)
EXPORT PROC divslaxpofsdrdq(s,axp,ofs,dr,dq)
ENDPROC putDIVL(M5,axp,dq,s,dr,1) BUT put16(ofs)
EXPORT PROC divslimmdrdq(s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putDIVL(M7,4,dq,s,dr,1)
   Put32(imm)
ENDPROC
EXPORT PROC divuldxdrdq(s,dx,dr,dq)
ENDPROC putDIVL(M0,dx,dq,s,dr,0)
EXPORT PROC divulaxpdrdq(s,axp,dr,dq)
ENDPROC putDIVL(M2,axp,dq,s,dr,0)
EXPORT PROC divulaxpofsdrdq(s,axp,ofs,dr,dq)
ENDPROC putDIVL(M5,axp,dq,s,dr,0) BUT put16(ofs)
EXPORT PROC divulimmdrdq(s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putDIVL(M7,4,dq,s,dr,0)
   Put32(imm)
ENDPROC


/******** MULS.L / MULU.L **********/

PROC putMULL(m,dx,dq,s,dr,sus)
   put16(Shl(%0100110000000 OR m,3) OR dx)
   put16(Shl(Shl(Shl(dq,1) OR sus,1) OR s,10) OR dr)
ENDPROC
EXPORT PROC mulsldxdrdq(s,dx,dr,dq)
ENDPROC putMULL(M0,dx,dq,s,dr,1)
EXPORT PROC mulslaxpdrdq(s,axp,dr,dq)
ENDPROC putMULL(M2,axp,dq,s,dr,1)
EXPORT PROC mulslaxpofsdrdq(s,axp,ofs,dr,dq)
ENDPROC putMULL(M5,axp,dq,s,dr,1) BUT put16(ofs)
EXPORT PROC mulslimmdrdq(s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putMULL(M7,4,dq,s,dr,1)
   Put32(imm)
ENDPROC
EXPORT PROC mululdxdrdq(s,dx,dr,dq)
ENDPROC putMULL(M0,dx,dq,s,dr,0)
EXPORT PROC mululaxpdrdq(s,axp,dr,dq)
ENDPROC putMULL(M2,axp,dq,s,dr,0)
EXPORT PROC mululaxpofsdrdq(s,axp,ofs,dr,dq)
ENDPROC putMULL(M5,axp,dq,s,dr,0) BUT put16(ofs)
EXPORT PROC mululimmdrdq(s,imm,dr,dq)
   IF imm=1 THEN RETURN
   putMULL(M7,4,dq,s,dr,0)
   Put32(imm)
ENDPROC


/******** MULS.W / MULU.W **********/

PROC putMULSW(r,m,mr)
   put16(Shl(Shl(Shl(%1100000 OR r,3) OR %111,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC mulswdxdx(dx1,dx2) IS putMULSW(dx2,M0,dx1)
EXPORT PROC mulswaxpdx(axp,dx) IS putMULSW(dx,M2,axp)
EXPORT PROC mulswaxpofsdx(axp,ofs,dx)
ENDPROC putMULSW(dx,M5,axp) BUT put16(ofs)
EXPORT PROC mulswimmdx(imm,dx)
ENDPROC putMULSW(dx,M7,4) BUT put16(imm)

PROC putMULUW(r,m,mr)
   put16(Shl(Shl(Shl(%1100000 OR r,3) OR %011,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC muluwdxdx(dx1,dx2) IS putMULUW(dx2,M0,dx1)
EXPORT PROC muluwaxpdx(axp,dx) IS putMULUW(dx,M2,axp)
EXPORT PROC muluwaxpofsdx(axp,ofs,dx)
ENDPROC putMULUW(dx,M5,axp) BUT put16(ofs)
EXPORT PROC muluwimmdx(imm,dx)
ENDPROC putMULUW(dx,M7,4) BUT put16(imm)

/******** DIVS.W / DIVU.W **********/

PROC putDIVSW(r,m,mr)
   put16(Shl(Shl(Shl(%1000000 OR r,3) OR %111,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC divswdxdx(dx1,dx2) IS putDIVSW(dx2,M0,dx1)
EXPORT PROC divswaxpdx(axp,dx) IS putDIVSW(dx,M2,axp)
EXPORT PROC divswaxpofsdx(axp,ofs,dx)
ENDPROC putDIVSW(dx,M5,axp) BUT put16(ofs)
EXPORT PROC divswimmdx(imm,dx)
ENDPROC putDIVSW(dx,M7,4) BUT put16(imm)

PROC putDIVUW(r,m,mr)
   put16(Shl(Shl(Shl(%1000000 OR r,3) OR %011,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC divuwdxdx(dx1,dx2) IS putDIVUW(dx2,M0,dx1)
EXPORT PROC divuwaxpdx(axp,dx) IS putDIVUW(dx,M2,axp)
EXPORT PROC divuwaxpofsdx(axp,ofs,dx)
ENDPROC putDIVUW(dx,M5,axp) BUT put16(ofs)
EXPORT PROC divuwimmdx(imm,dx)
ENDPROC putDIVUW(dx,M7,4) BUT put16(imm)


/********* CLR *********************/

PROC putCLR(s,m,r)
   put16(Shl(Shl(%0100001000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC clrdx(s,dx)
   IF s = SIZE_L THEN RETURN moveqdx(0,dx)
ENDPROC putCLR(s,M0,dx)
EXPORT PROC clrax(s,ax) IS putCLR(s,M1,ax)
EXPORT PROC clraxp(s,axp) IS putCLR(s,M2,axp)
EXPORT PROC clraxpi(s,axpi) IS putCLR(s,M3,axpi)
EXPORT PROC clraxpd(s,axpd) IS putCLR(s,M4,axpd)
EXPORT PROC clraxpofs(s,axp,ofs) IS putCLR(s,M5,axp) BUT put16(ofs)
EXPORT PROC clraxpx(s,axp,idrx,scale,d) IS putCLR(s,M6,axp) BUT put16(IxExt(idrx,scale,d))

/*********** CMP ********************/

PROC putCMPI(s,m,r,i)
   put16(Shl(Shl(%0000110000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN Put32(i) ELSE put16(i)
ENDPROC

EXPORT PROC cmplimmdx(imm,dx)
   IF imm=0 THEN RETURN tstdx(SIZE_L,dx)
ENDPROC putCMPI(SIZE_L,M0,dx,imm)
EXPORT PROC cmpwimmdx(imm,dx)
   IF imm=0 THEN RETURN tstdx(SIZE_W,dx)
ENDPROC putCMPI(SIZE_W,M0,dx,imm)
EXPORT PROC cmpbimmdx(imm,dx)
   IF imm=0 THEN RETURN tstdx(SIZE_B,dx)
ENDPROC putCMPI(SIZE_B,M0,dx,imm)
EXPORT PROC cmplimmaxp(imm,axp)
   IF imm=0 THEN RETURN tstaxp(SIZE_L,axp)
ENDPROC putCMPI(SIZE_L,M2,axp,imm)
EXPORT PROC cmpwimmaxp(imm,axp)
   IF imm=0 THEN RETURN tstaxp(SIZE_W,axp)
ENDPROC putCMPI(SIZE_W,M2,axp,imm)
EXPORT PROC cmpbimmaxp(imm,axp)
   IF imm=0 THEN RETURN tstaxp(SIZE_B,axp)
ENDPROC putCMPI(SIZE_B,M2,axp,imm)
EXPORT PROC cmplimmaxpi(imm,axp)
   IF imm=0 THEN RETURN tstaxpi(SIZE_L,axp)
ENDPROC putCMPI(SIZE_L,M3,axp,imm)
EXPORT PROC cmpwimmaxpi(imm,axp)
   IF imm=0 THEN RETURN tstaxpi(SIZE_W,axp)
ENDPROC putCMPI(SIZE_W,M3,axp,imm)
EXPORT PROC cmpbimmaxpi(imm,axp)
   IF imm=0 THEN RETURN tstaxpi(SIZE_W,axp)
ENDPROC putCMPI(SIZE_B,M3,axp,imm)
EXPORT PROC cmplimmaxpd(imm,axp)
   IF imm=0 THEN RETURN tstaxpd(SIZE_L,axp)
ENDPROC putCMPI(SIZE_L,M4,axp,imm)
EXPORT PROC cmpwimmaxpd(imm,axp)
   IF imm=0 THEN RETURN tstaxpd(SIZE_W,axp)
ENDPROC putCMPI(SIZE_W,M4,axp,imm)
EXPORT PROC cmplimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(SIZE_L,axp,ofs)
ENDPROC putCMPI(SIZE_L,M5,axp,imm) BUT put16(ofs)
EXPORT PROC cmpwimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(SIZE_W,axp,ofs)
ENDPROC putCMPI(SIZE_W,M5,axp,imm) BUT put16(ofs)
EXPORT PROC cmpbimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN tstaxpofs(SIZE_B,axp,ofs)
ENDPROC putCMPI(SIZE_B,M5,axp,imm) BUT put16(ofs)
EXPORT PROC cmplimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(SIZE_L,axp,idrx,scale,d)
ENDPROC putCMPI(SIZE_L,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC cmpwimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(SIZE_W,axp,idrx,scale,d)
ENDPROC putCMPI(SIZE_W,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC cmpbimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN tstaxpx(SIZE_B,axp,idrx,scale,d)
ENDPROC putCMPI(SIZE_B,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))

PROC putCMP(r,s,m,mr)
   put16(Shl(Shl(Shl(%1011000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC cmpdxax(s,dx,ax) IS putCMP(ax,ListItem([0,3,7],s),M0,dx)
EXPORT PROC cmpdxdx(s,dx1,dx2) IS putCMP(dx2,s,M0,dx1)
EXPORT PROC cmpaxdx(s,ax,dx) IS putCMP(dx,s,M1,ax)
EXPORT PROC cmpaxpdx(s,ax,dx) IS putCMP(dx,s,M2,ax)
EXPORT PROC cmpaxpidx(s,ax,dx) IS putCMP(dx,s,M3,ax)
EXPORT PROC cmpaxpddx(s,ax,dx) IS putCMP(dx,s,M4,ax)
EXPORT PROC cmpaxpofsdx(s,ax,ofs,dx)
ENDPROC putCMP(dx,s,M5,ax) BUT put16(ofs)
EXPORT PROC cmpaxpxdx(s,ax,idrx,scale,d,dx)
ENDPROC putCMP(dx,s,M6,ax) BUT put16(IxExt(idrx,scale,d))


EXPORT PROC cmpaxax(s,ax1,ax2) IS putCMP(ax2,ListItem([0,3,7],s),M1,ax1)
EXPORT PROC cmpaxpax(s,ax1,ax2) IS putCMP(ax2,ListItem([0,3,7],s),M2,ax1)
EXPORT PROC cmpaxpiax(s,ax1,ax2) IS putCMP(ax2,ListItem([0,3,7],s),M3,ax1)
EXPORT PROC cmpaxpdax(s,ax1,ax2) IS putCMP(ax2,ListItem([0,3,7],s),M4,ax1)
EXPORT PROC cmpaxpofsax(s,ax1,ofs,ax2)
ENDPROC putCMP(ax2,ListItem([0,3,7],s),M5,ax1) BUT put16(ofs)
EXPORT PROC cmpaxpxax(s,ax1,ax2,idrx,scale,d)
ENDPROC putCMP(ax2,ListItem([0,3,7],s),M6,ax1) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC cmplimmax(imm,ax)
   IF imm=0 THEN RETURN tstax(SIZE_L,ax)
   putCMP(ax,ListItem([0,3,7],SIZE_L),M7,4)
   Put32(imm)
ENDPROC
EXPORT PROC cmpwimmax(imm,ax)
   IF imm=0 THEN RETURN tstax(SIZE_W,ax)
ENDPROC putCMP(ax,ListItem([0,3,7],SIZE_W),M7,4) BUT put16(imm)

/*********** CMP2 ***************/

EXPORT PROC cmp2axpdx(s,axp,dx)
   Put32(Shl(Shl(Shl(Shl(Shl(%0000000 OR s,3) OR %011,3) OR M2,3) OR axp,4) OR dx,12) OR NIL)
ENDPROC

/*********** CMPM ***************/
EXPORT PROC cmpmaxpiaxpi(s, ax1, ax2)
   put16(Shl(Shl(%1011000 OR ax2, 3) OR s OR %100, 6) OR %001000 OR ax1)
ENDPROC

/************ EXT ***************/

EXPORT PROC extw(dx)  IS put16(%0100100010000000 OR dx)
EXPORT PROC extl(dx)  IS put16(%0100100011000000 OR dx)
EXPORT PROC extbl(dx) IS put16(%0100100111000000 OR dx)

/************ NEG ***************/

PROC putNEG(s,m,r)
   put16(Shl(Shl(%0100010000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC negdx(s,dx) IS putNEG(s,M0,dx)
EXPORT PROC negaxp(s,axp) IS putNEG(s,M2,axp)
EXPORT PROC negaxpi(s,axp) IS putNEG(s,M3,axp)
EXPORT PROC negaxpd(s,axp) IS putNEG(s,M4,axp)
EXPORT PROC negaxpofs(s,axp,ofs)
ENDPROC putNEG(s,M5,axp) BUT put16(ofs)
EXPORT PROC negaxpx(s,axp,idrx,scale,d)
ENDPROC putNEG(s,M6,axp) BUT put16(IxExt(idrx,scale,d))

/************ NEGX ***************/

PROC putNEGX(s,m,r)
   put16(Shl(Shl(%0100000000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC negxdx(s,dx) IS putNEGX(s,M0,dx)
EXPORT PROC negxaxp(s,axp) IS putNEGX(s,M2,axp)
EXPORT PROC negxaxpi(s,axp) IS putNEGX(s,M3,axp)
EXPORT PROC negxaxpd(s,axp) IS putNEGX(s,M4,axp)
EXPORT PROC negxaxpofs(s,axp,ofs)
ENDPROC putNEGX(s,M5,axp) BUT put16(ofs)
EXPORT PROC negxaxpx(s,axp,idrx,scale,d)
ENDPROC putNEGX(s,M6,axp) BUT put16(IxExt(idrx,scale,d))

/************ NOP ****************/

EXPORT PROC nop() IS put16(%0100111001110001)

/************ AND *****************/

PROC putANDI(s,m,r,i)
   put16(Shl(Shl(%0000001000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN Put32(i) ELSE put16(i)
ENDPROC

EXPORT PROC andlimmdx(imm,dx)
   IF imm=$FFFFFF00
      RETURN clrdx(SIZE_B,dx)
   ELSEIF imm=$FFFF0000
      RETURN clrdx(SIZE_W,dx)
   ENDIF
ENDPROC putANDI(SIZE_L,M0,dx,imm)
EXPORT PROC andwimmdx(imm,dx)
   IF imm=$FF00 THEN RETURN clrdx(SIZE_B,dx)
ENDPROC putANDI(SIZE_W,M0,dx,imm)
EXPORT PROC andbimmdx(imm,dx)
ENDPROC putANDI(SIZE_B,M0,dx,imm)
EXPORT PROC andlimmaxp(imm,ax)
ENDPROC putANDI(SIZE_L,M2,ax,imm)
EXPORT PROC andwimmaxp(imm,ax)
ENDPROC putANDI(SIZE_W,M2,ax,imm)
EXPORT PROC andbimmaxp(imm,ax)
ENDPROC putANDI(SIZE_B,M2,ax,imm)
EXPORT PROC andlimmaxpi(imm,ax)
ENDPROC putANDI(SIZE_L,M3,ax,imm)
EXPORT PROC andlimmaxpd(imm,ax)
ENDPROC putANDI(SIZE_L,M4,ax,imm)
EXPORT PROC andlimmaxpofs(imm,ax,ofs)
ENDPROC putANDI(SIZE_L,M5,ax,imm) BUT put16(ofs)
EXPORT PROC andwimmaxpofs(imm,ax,ofs)
ENDPROC putANDI(SIZE_W,M5,ax,imm) BUT put16(ofs)
EXPORT PROC andbimmaxpofs(imm,ax,ofs)
ENDPROC putANDI(SIZE_B,M5,ax,imm) BUT put16(ofs)
EXPORT PROC andlimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putANDI(SIZE_L,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC andwimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putANDI(SIZE_W,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC andbimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putANDI(SIZE_B,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))

PROC putAND(r,s,m,mr)
   put16(Shl(Shl(Shl(%1100000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC anddxdx(s,dx1,dx2) IS putAND(dx2,s,M0,dx1)
EXPORT PROC andaxpdx(s,axp,dx) IS putAND(dx,s,M2,axp)
EXPORT PROC andaxpidx(s,axp,dx) IS putAND(dx,s,M3,axp)
EXPORT PROC andaxpddx(s,axp,dx) IS putAND(dx,s,M4,axp)
EXPORT PROC andaxpofsdx(s,axp,ofs,dx)
ENDPROC putAND(dx,s,M5,axp) BUT put16(ofs)
EXPORT PROC andaxpxdx(s,axp,idrx,scale,d,dx)
ENDPROC putAND(dx,s,M6,axp) BUT put16(IxExt(idrx,scale,d))

/************ OR *****************/

PROC putORI(s,m,r,i)
   put16(Shl(Shl(%0000000000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN Put32(i) ELSE put16(i)
ENDPROC

EXPORT PROC orlimmdx(imm,dx) IS putORI(SIZE_L,M0,dx,imm)
EXPORT PROC orwimmdx(imm,dx) IS putORI(SIZE_W,M0,dx,imm)
EXPORT PROC orbimmdx(imm,dx) IS putORI(SIZE_B,M0,dx,imm)
EXPORT PROC orlimmaxp(imm,ax) IS putORI(SIZE_L,M2,ax,imm)
EXPORT PROC orwimmaxp(imm,ax) IS putORI(SIZE_W,M2,ax,imm)
EXPORT PROC orbimmaxp(imm,ax) IS putORI(SIZE_B,M2,ax,imm)
EXPORT PROC orlimmaxpi(imm,ax) IS putORI(SIZE_L,M3,ax,imm)
EXPORT PROC orlimmaxpd(imm,ax) IS putORI(SIZE_L,M4,ax,imm)
EXPORT PROC orlimmaxpofs(imm,ax,ofs)
ENDPROC putORI(SIZE_L,M5,ax,imm) BUT put16(ofs)
EXPORT PROC orwimmaxpofs(imm,ax,ofs)
ENDPROC putORI(SIZE_W,M5,ax,imm) BUT put16(ofs)
EXPORT PROC orbimmaxpofs(imm,ax,ofs)
ENDPROC putORI(SIZE_B,M5,ax,imm) BUT put16(ofs)
EXPORT PROC orlimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putORI(SIZE_L,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC orwimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putORI(SIZE_W,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC orbimmaxpx(imm,ax,idrx,scale,d)
ENDPROC putORI(SIZE_B,M6,ax,imm) BUT put16(IxExt(idrx,scale,d))

PROC putOR(r,s,m,mr)
   put16(Shl(Shl(Shl(%1000000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC ordxdx(s,dx1,dx2) IS putOR(dx2,s,M0,dx1)
EXPORT PROC oraxpdx(s,axp,dx) IS putOR(dx,s,M2,axp)
EXPORT PROC oraxpidx(s,axp,dx) IS putOR(dx,s,M3,axp)
EXPORT PROC oraxpddx(s,axp,dx) IS putOR(dx,s,M4,axp)
EXPORT PROC oraxpofsdx(s,axp,ofs,dx)
ENDPROC putOR(dx,s,M5,axp) BUT put16(ofs)
EXPORT PROC oraxpxdx(s,axp,idrx,scale,d,dx)
ENDPROC putOR(dx,s,M6,axp) BUT put16(IxExt(idrx,scale,d))

/************ EOR ***************/

PROC putEOR(r,s,m,mr)
   put16(Shl(Shl(Shl(%1011000 OR r,3) OR s,3) OR m,3) OR mr)
ENDPROC

EXPORT PROC eordxdx(s,dx1,dx2) IS putEOR(dx1,ListItem([4,5,6],s),M0,dx2)
EXPORT PROC eordxaxp(s,dx,axp) IS putEOR(dx,ListItem([4,5,6],s),M2,axp)
EXPORT PROC eordxaxpi(s,dx,axp) IS putEOR(dx,ListItem([4,5,6],s),M3,axp)
EXPORT PROC eordxaxpd(s,dx,axp) IS putEOR(dx,ListItem([4,5,6],s),M4,axp)
EXPORT PROC eordxaxpofs(s,dx,axp,ofs)
ENDPROC putEOR(dx,ListItem([4,5,6],s),M5,axp) BUT put16(ofs)
EXPORT PROC eordxaxpx(s,dx,axp,idrx,scale,d)
ENDPROC putEOR(dx,ListItem([4,5,6],s),M6,axp) BUT put16(IxExt(idrx,scale,d))

PROC putEORI(s,m,r,i)
   put16(Shl(Shl(%0000101000 OR s,3) OR m,3) OR r)
   IF s = SIZE_L THEN Put32(i) ELSE put16(i)
ENDPROC

EXPORT PROC eorlimmdx(imm,dx) IS putEORI(SIZE_L,M0,dx,imm)
EXPORT PROC eorwimmdx(imm,dx) IS putEORI(SIZE_W,M0,dx,imm)
EXPORT PROC eorbimmdx(imm,dx) IS putEORI(SIZE_B,M0,dx,imm)
EXPORT PROC eorlimmaxp(imm,axp) IS putEORI(SIZE_L,M2,axp,imm)
EXPORT PROC eorwimmaxp(imm,axp) IS putEORI(SIZE_W,M2,axp,imm)
EXPORT PROC eorbimmaxp(imm,axp) IS putEORI(SIZE_B,M2,axp,imm)
EXPORT PROC eorlimmaxpi(imm,axp) IS putEORI(SIZE_L,M3,axp,imm)
EXPORT PROC eorwimmaxpi(imm,axp) IS putEORI(SIZE_W,M3,axp,imm)
EXPORT PROC eorlimmaxpd(imm,axp) IS putEORI(SIZE_L,M4,axp,imm)
EXPORT PROC eorwimmaxpd(imm,axp) IS putEORI(SIZE_W,M4,axp,imm)
EXPORT PROC eorlimmaxpofs(imm,axp,ofs)
ENDPROC putEORI(SIZE_L,M5,axp,imm) BUT put16(ofs)
EXPORT PROC eorwimmaxpofs(imm,axp,ofs)
ENDPROC putEORI(SIZE_W,M5,axp,imm) BUT put16(ofs)
EXPORT PROC eorbimmaxpofs(imm,axp,ofs)
ENDPROC putEORI(SIZE_B,M5,axp,imm) BUT put16(ofs)
EXPORT PROC eorlimmaxpx(imm,axp,idrx,scale,d)
ENDPROC putEORI(SIZE_L,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC eorwimmaxpx(imm,axp,idrx,scale,d)
ENDPROC putEORI(SIZE_W,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC eorbimmaxpx(imm,axp,idrx,scale,d)
ENDPROC putEORI(SIZE_B,M6,axp,imm) BUT put16(IxExt(idrx,scale,d))

/************ NOT *****************/

PROC putNOT(s,m,r)
   put16(Shl(Shl(%0100011000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC notdx(s,dx) IS putNOT(s,M0,dx)
EXPORT PROC notaxp(s,ax) IS putNOT(s,M2,ax)
EXPORT PROC notaxpi(s,ax) IS putNOT(s,M3,ax)
EXPORT PROC notaxpd(s,ax) IS putNOT(s,M4,ax)
EXPORT PROC notaxpofs(s,ax,ofs) IS putNOT(s,M5,ax) BUT put16(ofs)
EXPORT PROC notaxpx(s,ax,idrx,scale,d) IS putNOT(s,M6,ax) BUT put16(IxExt(idrx,scale,d))

/************ TST *****************/

PROC putTST(s,m,r)
   put16(Shl(Shl(%0100101000 OR s,3) OR m,3) OR r)
ENDPROC

EXPORT PROC tstdx(s,dx) IS putTST(s,M0,dx)
EXPORT PROC tstax(s,ax) IS putTST(s,M1,ax)
EXPORT PROC tstaxp(s,ax) IS putTST(s,M2,ax)
EXPORT PROC tstaxpi(s,ax) IS putTST(s,M3,ax)
EXPORT PROC tstaxpd(s,ax) IS putTST(s,M4,ax)
EXPORT PROC tstaxpofs(s,ax,ofs)
ENDPROC putTST(s,M5,ax) BUT put16(ofs)
EXPORT PROC tstaxpx(s,ax,idrx,scale,d)
ENDPROC putTST(s,M6,ax) BUT put16(IxExt(idrx,scale,d))

/************ Scc *****************/

EXPORT PROC sccdx(cond,dx)
ENDPROC put16(Shl(Shl(Shl(%01010000 OR cond,2) OR %11,3) OR M0,3) OR dx)

/************ ASL / ASR ************/

PROC putSHA(nr,dr,s,ir,x,r)
   put16(Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %00, 3) OR r)
ENDPROC

EXPORT PROC asldxdx(s,dx1,dx2) IS putSHA(dx1,1,s,1,%00,dx2)
EXPORT PROC aslimmdx(s,imm,dx) IS putSHA(IF imm=8 THEN 0 ELSE imm,1,s,0,%00,dx)
EXPORT PROC asrdxdx(s,dx1,dx2) IS putSHA(dx1,0,s,1,%00,dx2)
EXPORT PROC asrimmdx(s,imm,dx) IS putSHA(IF imm=8 THEN 0 ELSE imm,0,s,0,%00,dx)

/*********** LSL / LSR ****************/

PROC putSH(nr,dr,s,ir,x,r)
   put16(Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %01, 3) OR r)
ENDPROC

EXPORT PROC lsldxdx(s,dx1,dx2) IS putSH(dx1,1,s,1,%01,dx2)
EXPORT PROC lslimmdx(s,imm,dx) IS putSH(IF imm=8 THEN 0 ELSE imm,1,s,0,%01,dx)
EXPORT PROC lsrdxdx(s,dx1,dx2) IS putSH(dx1,0,s,1,%01,dx2)
EXPORT PROC lsrimmdx(s,imm,dx) IS putSH(IF imm=8 THEN 0 ELSE imm,0,s,0,%01,dx)

/*********** ROL / ROR ****************/

PROC putRO(nr,dr,s,ir,x,r)
   put16(Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %11, 3) OR r)
ENDPROC

EXPORT PROC roldxdx(s,dx1,dx2) IS putRO(dx1,1,s,1,%11,dx2)
EXPORT PROC rolimmdx(s,imm,dx) IS putRO(IF imm=8 THEN 0 ELSE imm,1,s,0,%11,dx)
EXPORT PROC rordxdx(s,dx1,dx2) IS putRO(dx1,0,s,1,%11,dx2)
EXPORT PROC rorimmdx(s,imm,dx) IS putRO(IF imm=8 THEN 0 ELSE imm,0,s,0,%11,dx)

/*********** ROXL / ROXR ****************/

PROC putROX(nr,dr,s,ir,x,r)
   put16(Shl(Shl(Shl(Shl(Shl(%1110000 OR nr,1) OR dr,2) OR s,1) OR ir,2) OR %10, 3) OR r)
ENDPROC

EXPORT PROC roxldxdx(s,dx1,dx2) IS putROX(dx1,1,s,1,%10,dx2)
EXPORT PROC roxlimmdx(s,imm,dx) IS putROX(IF imm=8 THEN 0 ELSE imm,1,s,0,%10,dx)
EXPORT PROC roxrdxdx(s,dx1,dx2) IS putROX(dx1,0,s,1,%10,dx2)
EXPORT PROC roxrimmdx(s,imm,dx) IS putROX(IF imm=8 THEN 0 ELSE imm,0,s,0,%10,dx)

/************ SWAP **********************/

EXPORT PROC swapdx(dx) IS put16(%0100100001000000 OR dx)

/************ EXG ***********************/

EXPORT PROC exgdxdx(dx1,dx2)
ENDPROC put16(Shl(Shl(Shl(%1100000 OR dx1,1) OR 1,5) OR %01000,3) OR dx2)
EXPORT PROC exgaxax(ax1,ax2)
ENDPROC put16(Shl(Shl(Shl(%1100000 OR ax1,1) OR 1,5) OR %01001,3) OR ax2)
EXPORT PROC exgdxax(dx,ax)
ENDPROC put16(Shl(Shl(Shl(%1100000 OR dx,1) OR 1,5) OR %10001,3) OR ax)

/************ LEA **********************/

PROC putLEA(ax,m,r)
   put16(Shl(Shl(Shl(%0100000 OR ax,3) OR %111,3) OR m,3) OR r)
ENDPROC

EXPORT PROC leaaxpax(axp,ax) IS putLEA(ax,M2,axp)
EXPORT PROC leaaxpofsax(axp,ofs,ax)
   IF axp=ax
      IF Abs(ofs) < 9
         RETURN IF ofs > 0 THEN addqax(SIZE_L,ofs,ax) ELSE subqax(SIZE_L,-ofs,ax)
      ENDIF
   ENDIF
ENDPROC putLEA(ax,M5,axp) BUT put16(ofs)
EXPORT PROC leaaxpxax(axp,idrx,scale,d,ax)
ENDPROC putLEA(ax,M6,axp) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC leaabswax(absw,ax)
ENDPROC putLEA(ax,M7,0) BUT put16(absw)
EXPORT PROC leaabslax(absl,ax)
   putLEA(ax,M7,1)
   Put32(absl)
ENDPROC
EXPORT PROC leapcpofsax(ofs,ax)
ENDPROC putLEA(ax,M7,2) BUT put16(ofs)

/************ LINK *******************/

EXPORT PROC linkw(ax,data)
ENDPROC put16(%0100111001010000 OR ax) BUT put16(data)
EXPORT PROC linkl(ax,data)
   put16(%0100100000001000 OR ax)
   Put32(data)
ENDPROC

/************ MOVE *******************/

#define Msize(_s_) ListItem([1,3,2],_s_)

EXPORT PROC putMOVE(s,r1,m1,m2,r2)
   put16(Shl(Shl(Shl(Shl(%0000 OR s,3) OR r1,3) OR m1,3) OR m2,3) OR r2)
ENDPROC

EXPORT PROC movedxdx(s,dx1,dx2) IS putMOVE(Msize(s),dx2,M0,M0,dx1)
EXPORT PROC movedxax(s,dx,ax) IS putMOVE(ListItem([0,3,2],s),ax,M1,M0,dx)
EXPORT PROC movedxaxp(s,dx,axp) IS putMOVE(Msize(s),axp,M2,M0,dx)
EXPORT PROC movedxaxpi(s,dx,axp) IS putMOVE(Msize(s),axp,M3,M0,dx)
EXPORT PROC movedxaxpd(s,dx,axp) IS putMOVE(Msize(s),axp,M4,M0,dx)
EXPORT PROC movedxaxpofs(s,dx,axp,ofs) IS putMOVE(Msize(s),axp,M5,M0,dx) BUT put16(ofs)
EXPORT PROC movedxaxpx(s,dx,axp,idrx,scale,d) IS putMOVE(Msize(s),axp,M6,M0,dx) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxdx(s,ax,dx) IS putMOVE(Msize(s),dx,M0,M1,ax)
EXPORT PROC moveaxax(s,ax1,ax2) IS putMOVE(ListItem([0,3,2],s),ax2,M1,M1,ax1)
EXPORT PROC moveaxaxp(s,ax,axp) IS putMOVE(Msize(s),axp,M2,M1,ax)
EXPORT PROC moveaxaxpi(s,ax,axp) IS putMOVE(Msize(s),axp,M3,M1,ax)
EXPORT PROC moveaxaxpd(s,ax,axp) IS putMOVE(Msize(s),axp,M4,M1,ax)
EXPORT PROC moveaxaxpofs(s,ax,axp,ofs) IS putMOVE(Msize(s),axp,M5,M1,ax) BUT put16(ofs)
EXPORT PROC moveaxaxpx(s,ax,axp,idrx,scale,d) IS putMOVE(Msize(s),axp,M6,M1,ax) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC moveaxpdx(s,axp,dx) IS putMOVE(Msize(s),dx,M0,M2,axp)
EXPORT PROC moveaxpax(s,axp,ax) IS putMOVE(ListItem([0,3,2],s),ax,M1,M2,axp)
EXPORT PROC moveaxpaxp(s,axp1,axp2) IS putMOVE(Msize(s),axp2,M2,M2,axp1)
EXPORT PROC moveaxpaxpi(s,axp,axpi) IS putMOVE(Msize(s),axpi,M3,M2,axp)
EXPORT PROC moveaxpaxpd(s,axp,axpd) IS putMOVE(Msize(s),axpd,M4,M2,axp)
EXPORT PROC moveaxpaxpofs(s,axp,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M2,axp) BUT put16(ofs)
EXPORT PROC moveaxpaxpx(s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(Msize(s),axp2,M6,M2,axp) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC moveaxpidx(s,axp,dx) IS putMOVE(Msize(s),dx,M0,M3,axp)
EXPORT PROC moveaxpiax(s,axp,ax) IS putMOVE(ListItem([0,3,2],s),ax,M1,M3,axp)
EXPORT PROC moveaxpiaxp(s,axp1,axp2) IS putMOVE(Msize(s),axp2,M2,M3,axp1)
EXPORT PROC moveaxpiaxpi(s,axp,axpi) IS putMOVE(Msize(s),axpi,M3,M3,axp)
EXPORT PROC moveaxpiaxpd(s,axp,axpd) IS putMOVE(Msize(s),axpd,M4,M3,axp)
EXPORT PROC moveaxpiaxpofs(s,axp,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M3,axp) BUT put16(ofs)
EXPORT PROC moveaxpiaxpx(s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(Msize(s),axp2,M6,M3,axp) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC moveaxpddx(s,axp,dx) IS putMOVE(Msize(s),dx,M0,M4,axp)
EXPORT PROC moveaxpdax(s,axp,ax) IS putMOVE(ListItem([0,3,2],s),ax,M1,M4,axp)
EXPORT PROC moveaxpdaxp(s,axp1,axp2) IS putMOVE(Msize(s),axp2,M2,M4,axp1)
EXPORT PROC moveaxpdaxpi(s,axp,axpi) IS putMOVE(Msize(s),axpi,M3,M4,axp)
EXPORT PROC moveaxpdaxpd(s,axp,axpd) IS putMOVE(Msize(s),axpd,M4,M4,axp)
EXPORT PROC moveaxpdaxpofs(s,axp,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M4,axp) BUT put16(ofs)
EXPORT PROC moveaxpdaxpx(s,axp,axp2,idrx,scale,d)
ENDPROC putMOVE(Msize(s),axp2,M6,M4,axp) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC moveaxpofsdx(s,axp,ofs,dx)
ENDPROC putMOVE(Msize(s),dx,M0,M5,axp) BUT put16(ofs)
EXPORT PROC moveaxpofsax(s,axp,ofs,ax)
ENDPROC putMOVE(ListItem([0,3,2],s),ax,M1,M5,axp) BUT put16(ofs)
EXPORT PROC moveaxpofsaxp(s,axp1,ofs,axp2)
ENDPROC putMOVE(Msize(s),axp2,M2,M5,axp1) BUT put16(ofs)
EXPORT PROC moveaxpofsaxpi(s,axp,ofs,axpi)
ENDPROC putMOVE(Msize(s),axpi,M3,M5,axp) BUT put16(ofs)
EXPORT PROC moveaxpofsaxpd(s,axp,ofs,axpd)
ENDPROC putMOVE(Msize(s),axpd,M4,M5,axp) BUT put16(ofs)
EXPORT PROC moveaxpofsaxpofs(s,axp,ofs1,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M5,axp) BUT put16(ofs1) BUT put16(ofs)
EXPORT PROC moveaxpofsaxpx(s,axp,ofs,axp2,idrx,scale,d)
ENDPROC putMOVE(Msize(s),axp2,M6,M5,axp) BUT put16(ofs) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC moveaxpxdx(s,axp,idrx,scale,d,dx)
ENDPROC putMOVE(Msize(s),dx,M0,M6,axp) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxpxax(s,axp,idrx,scale,d,ax)
ENDPROC putMOVE(ListItem([0,3,2],s),ax,M1,M6,axp) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxpxaxp(s,axp1,idrx,scale,d,axp2)
ENDPROC putMOVE(Msize(s),axp2,M2,M6,axp1) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxpxaxpi(s,axp,idrx,scale,d,axpi)
ENDPROC putMOVE(Msize(s),axpi,M3,M6,axp) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxpxaxpd(s,axp,idrx,scale,d,axpd)
ENDPROC putMOVE(Msize(s),axpd,M4,M6,axp) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC moveaxpxaxpofs(s,axp,idrx,scale,d,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M6,axp) BUT put16(IxExt(idrx,scale,d)) BUT put16(ofs)
EXPORT PROC moveaxpxaxpx(s,axp,idrx,scale,d1,axp2,idrx2,scale2,d2)
ENDPROC putMOVE(Msize(s),axp2,M6,M6,axp) BUT put16(IxExt(idrx,scale,d1)) BUT put16(IxExt(idrx2,scale2,d2))

EXPORT PROC movepcpofsdx(s,ofs,dx)
ENDPROC putMOVE(Msize(s),dx,M0,M7,2) BUT put16(ofs)
EXPORT PROC movepcpofsax(s,ofs,ax)
ENDPROC putMOVE(ListItem([0,3,2],s),ax,M1,M7,2) BUT put16(ofs)
EXPORT PROC movepcpofsaxp(s,ofs,axp2)
ENDPROC putMOVE(Msize(s),axp2,M2,M7,2) BUT put16(ofs)
EXPORT PROC movepcpofsaxpi(s,ofs,axpi)
ENDPROC putMOVE(Msize(s),axpi,M3,M7,2) BUT put16(ofs)
EXPORT PROC movepcpofsaxpd(s,ofs,axpd)
ENDPROC putMOVE(Msize(s),axpd,M4,M7,2) BUT put16(ofs)
EXPORT PROC movepcpofsaxpofs(s,ofs1,axp2,ofs)
ENDPROC putMOVE(Msize(s),axp2,M5,M7,2) BUT put16(ofs1) BUT put16(ofs)
EXPORT PROC movepcpofsaxpx(s,ofs,axp2,idrx,scale,d)
ENDPROC putMOVE(Msize(s),axp2,M6,M7,2) BUT put16(ofs) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC putMOVEI(s,r1,m1,imm)
   put16(Shl(Shl(Shl(Shl(Msize(s),3) OR r1,3) OR m1,3) OR M7,3) OR 4)
   IF s = SIZE_L THEN Put32(imm) ELSE put16(imm)
ENDPROC

EXPORT PROC movelimmdx(imm,dx)
   IF imm <> $80000000 -> fix
      IF Abs(imm) < 8 THEN RETURN moveqdx(imm,dx)
   ENDIF
ENDPROC putMOVEI(SIZE_L,dx,M0,imm)
EXPORT PROC movelimmax(imm,ax)
   IF imm=0
      RETURN subaxax(SIZE_L,ax,ax)
   ELSEIF imm = $80000000 -> fix
      ->
   ELSEIF Abs(imm) < 32767
      RETURN movewimmax(imm,ax)
   ENDIF
ENDPROC putMOVEI(SIZE_L,ax,M1,imm)
EXPORT PROC movelimmaxp(imm,axp)
   IF imm=0 THEN RETURN clraxp(SIZE_L,axp)
ENDPROC putMOVEI(SIZE_L,axp,M2,imm)
EXPORT PROC movelimmaxpi(imm,axp)
   IF imm=0 THEN RETURN clraxpi(SIZE_L,axp)
ENDPROC putMOVEI(SIZE_L,axp,M3,imm)
EXPORT PROC movelimmaxpd(imm,axp)
   IF imm=0
      RETURN clraxpd(SIZE_L,axp)
   ELSEIF axp=7
      RETURN peaabsl(imm)
   ENDIF
ENDPROC putMOVEI(SIZE_L,axp,M4,imm)
EXPORT PROC movelimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(SIZE_L,axp,ofs)
ENDPROC putMOVEI(SIZE_L,axp,M5,imm) BUT put16(ofs)
EXPORT PROC movelimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(SIZE_L,axp,idrx,scale,d)
ENDPROC putMOVEI(SIZE_L,axp,M6,imm) BUT put16(IxExt(idrx,scale,d))
EXPORT PROC movewimmdx(imm,dx)
   IF imm=0 THEN RETURN clrdx(SIZE_W,dx)
ENDPROC putMOVEI(1,dx,M0,imm)
EXPORT PROC movewimmax(imm,ax)
ENDPROC putMOVEI(1,ax,M1,imm)
EXPORT PROC movewimmaxp(imm,axp)
   IF imm=0 THEN RETURN clraxp(SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M2,imm)
EXPORT PROC movewimmaxpi(imm,axp)
   IF imm=0 THEN RETURN clraxpi(SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M3,imm)
EXPORT PROC movewimmaxpd(imm,axp)
   IF imm=0 THEN RETURN clraxpd(SIZE_W,axp)
ENDPROC putMOVEI(1,axp,M4,imm)
EXPORT PROC movewimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(SIZE_W,axp,ofs)
ENDPROC putMOVEI(1,axp,M5,imm) BUT put16(ofs)
EXPORT PROC movewimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(SIZE_W,axp,idrx,scale,d)
ENDPROC putMOVEI(1,axp,M6,imm) BUT put16(IxExt(idrx,scale,d))

EXPORT PROC movebimmdx(imm,dx)
   IF imm=0 THEN RETURN clrdx(SIZE_B,dx)
ENDPROC putMOVEI(0,dx,M0,imm)
EXPORT PROC movebimmaxp(imm,axp)
   IF imm=0 THEN RETURN clraxp(SIZE_B,axp)
ENDPROC putMOVEI(0,axp,M2,imm)
EXPORT PROC movebimmaxpofs(imm,axp,ofs)
   IF imm=0 THEN RETURN clraxpofs(SIZE_B,axp,ofs)
ENDPROC putMOVEI(0,axp,M5,imm) BUT put16(ofs)
EXPORT PROC movebimmaxpx(imm,axp,idrx,scale,d)
   IF imm=0 THEN RETURN clraxpx(SIZE_B,axp,idrx,scale,d)
ENDPROC putMOVEI(0,axp,M6,imm) BUT put16(IxExt(idrx,scale,d))

/************* MOVEM **************/

EXPORT PROC movemregsaxp(s,mask,axp)
   put16(Shl(Shl(%0100100010 OR ListItem([0,0,1],s),3) OR M2,3) OR axp)
   put16(mask)
ENDPROC
EXPORT PROC movemregsaxpd(s,mask,axp)
   put16(Shl(Shl(%0100100010 OR ListItem([0,0,1],s),3) OR M4,3) OR axp)
   put16(mask)
ENDPROC
EXPORT PROC movemaxpregs(s,axp,mask)
   put16(Shl(Shl(%0100110010 OR ListItem([0,0,1],s),3) OR M2,3) OR axp)
   put16(mask)
ENDPROC
EXPORT PROC movemaxpiregs(s,axp,mask)
   put16(Shl(Shl(%0100110010 OR ListItem([0,0,1],s),3) OR M3,3) OR axp)
   put16(mask)
ENDPROC

/************ MOVEQ ***************/

EXPORT PROC moveqdx(imm,dx)
ENDPROC put16(Shl(%0111000 OR dx,9) OR (imm AND $FF))

/************ PEA *****************/

EXPORT PROC peaaxp(axp) IS put16(%0100100001010000 OR axp)
EXPORT PROC peaaxpofs(axp,ofs) IS put16(%0100100001101000 OR axp) BUT put16(ofs)
EXPORT PROC peaabsw(word) IS put16(%0100100001111000) BUT put16(word)
EXPORT PROC peaabsl(long)
   IF long <> $80000000  -> fix
      IF Abs(long) < 32767 THEN RETURN peaabsw(long)
   ENDIF
   put16(%0100100001111001)
   Put32(long)
ENDPROC
EXPORT PROC peapcpofs(ofs)
   put16(%0100100001111010)
   put16(ofs)
ENDPROC

/*********** UNLK ******************/

EXPORT PROC unlkax(ax) IS put16(%0100111001011000 OR ax)

/*********** Bcc ******************/

EXPORT PROC bccofs8(cc,ofs) IS put16(Shl(%01100000 OR cc,8) OR (ofs AND $FF))
EXPORT PROC bccofs16(cc,ofs)
ENDPROC put16(Shl(%01100000 OR cc,8) OR $00) BUT put16(ofs)
EXPORT PROC bccofs32(cc,ofs)
   put16(Shl(%01100000 OR cc,8) OR $FF)
   Put32(ofs)
ENDPROC

/*********** DBcc ****************/

EXPORT PROC dbccofs(cc,dx,ofs)
   put16(Shl(Shl(%01010000 OR cc,5) OR %11001,3) OR dx)
   put16(ofs)
ENDPROC

/*********** BSR *****************/

->PROC bsrofs8(ofs) IS put16(%0110000100000000 OR (ofs AND $FF))
EXPORT PROC bsrofs16(ofs)
ENDPROC put16(%0110000100000000) BUT put16(ofs)
EXPORT PROC bsrofs32(ofs)
   put16(%0110000111111111)
   Put32(ofs)
ENDPROC

/************ JMP ****************/

EXPORT PROC jmpaxp(axp) IS put16(%0100111011010000 OR axp)
EXPORT PROC jmpaxpofs(axp,ofs)
ENDPROC put16(%0100111011101000 OR axp) BUT put16(ofs)
EXPORT PROC jmpaxpx(axp,idrx,scale,d)
ENDPROC put16(%0100111011110000 OR axp) BUT put16(IxExt(idrx,scale,d))

/************ JSR ****************/

EXPORT PROC jsraxp(axp) IS put16(%0100111010010000 OR axp)
EXPORT PROC jsraxpofs(axp,ofs)
ENDPROC put16(%0100111010101000 OR axp) BUT put16(ofs)
EXPORT PROC jsraxpx(axp,idrx,scale,d)
ENDPROC put16(%0100111010110000 OR axp) BUT put16(IxExt(idrx,scale,d))

/*********** RTD ****************/

EXPORT PROC rtdofs(ofs) IS put16(%0100111001110100) BUT put16(ofs)

/*********** RTS ****************/

EXPORT PROC rts_() IS put16(%0100111001110101)

/*********** RTR ****************/

EXPORT PROC rtr_() IS put16(%0100111001110111)

/*********** RTE ****************/

EXPORT PROC rte_() IS put16(%0100111001110011)



/* ------- FLOATING POINT -------- */


/*********** FABS ********************/

EXPORT PROC fabsfpxfpx(fpx1,fpx2)
   put16(%111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2, 7) OR %001100)
ENDPROC

/*********** FADD *****************/

EXPORT PROC faddfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(fpx1,3) OR fpx2,7) OR %0100010)
ENDPROC
EXPORT PROC fadddxfpx(fs,dx,fpx)
   put16(%1111001000000000)
   put16(Shl(Shl(Shl(Shl(dx,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
ENDPROC
EXPORT PROC faddaxpfpx(fs,axp,ofs,fpx)
   put16(%1111001000001000)
   put16(Shl(Shl(Shl(Shl(axp,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
ENDPROC
EXPORT PROC faddaxpofsfpx(fs,axp,ofs,fpx)
   put16(%1111001000010100)
   put16(Shl(Shl(Shl(Shl(axp,3) OR %010,3) OR fs,3) OR fpx,7) OR %0100010)
   put16(ofs)
ENDPROC
EXPORT PROC faddsimmfpx(imm,fpx)
   put16(%1111001000011110)
   put16(Shl(Shl(%0010000 OR FS,3) OR fpx,7) OR %0100010)
   Put32(imm)
ENDPROC


/*********** FCMP ***************/

EXPORT PROC fcmpfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpdxfpx(fs,dx,fpx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpaxpfpx(fs,axp,fpx)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
ENDPROC
EXPORT PROC fcmpaxpofsfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M5,6) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0111000)
   put16(ofs)
ENDPROC
EXPORT PROC fcmpsimmfpx(imm,fpx)
   put16(Shl(%1111001000000 OR M7,3) OR 4)
   put16(Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0111000)
   Put32(imm)
ENDPROC

/*********** FDIV *************/

EXPORT PROC fdivfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0100000)
ENDPROC
EXPORT PROC fdivdxfpx(fs,dx,fpx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
ENDPROC
EXPORT PROC fdivaxpfpx(fs,axp,fpx)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
ENDPROC
EXPORT PROC fdivaxpofsfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100000)
   put16(ofs)
ENDPROC
EXPORT PROC fdivsimmfpx(imm,fpx)
   put16(Shl(%1111001000000 OR M7,3) OR 4)
   put16(Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0100000)
   Put32(imm)
ENDPROC

/************ FMOVE ************/

EXPORT PROC fmovefpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0000000)
ENDPROC
EXPORT PROC fmovedxfpx(fs,dx,fpx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpfpx(fs,axp,fpx)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpifpx(fs,axp,fpx)
   put16(Shl(%1111001000000 OR M3,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmoveaxpofsfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
   put16(ofs)
ENDPROC

EXPORT PROC fmovesimmfpx(imm,fpx)
   put16(Shl(%1111001000000 OR M7,3) OR 4)
   put16(Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0000000)
   Put32(imm)
ENDPROC

EXPORT PROC fmovefpxdx(fs,fpx,dx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxp(fs,fpx,axp)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxpd(fs,fpx,axp)
   put16(Shl(%1111001000000 OR M4,3) OR axp)
   put16(Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
ENDPROC
EXPORT PROC fmovefpxaxpofs(fs,fpx,axp,ofs)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
   put16(ofs)
ENDPROC


/*********** FMUL *************/

EXPORT PROC fmulfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0100011)
ENDPROC
EXPORT PROC fmuldxfpx(fs,dx,fpx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
ENDPROC
EXPORT PROC fmulaxpfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
ENDPROC
EXPORT PROC fmulaxpofsfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0100011)
   put16(ofs)
ENDPROC
EXPORT PROC fmulsimmfpx(imm,fpx)
   put16(Shl(%1111001000000 OR M7,3) OR 4)
   put16(Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0100011)
   Put32(imm)
ENDPROC

/*********** FNEG **************/

EXPORT PROC fnegfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,16) OR fpx2,7) OR %0011010)
ENDPROC

/************ FScc *************/

EXPORT PROC fsccdx(cond,dx)
   put16(Shl(%1111001001000 OR M0,3) OR dx)
   put16(cond)
ENDPROC

/*********** FSUB *****************/

EXPORT PROC fsubfpxfpx(fpx1,fpx2)
   put16(%1111001000000000)
   put16(Shl(Shl(%000000 OR fpx1,3) OR fpx2,7) OR %0101000)
ENDPROC
EXPORT PROC fsubdxfpx(fs,dx,fpx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
ENDPROC
EXPORT PROC fsubaxpfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M2,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
ENDPROC
EXPORT PROC fsubaxpofsfpx(fs,axp,ofs,fpx)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0101000)
   put16(ofs)
ENDPROC
EXPORT PROC fsubsimmfpx(imm,fpx)
   put16(Shl(%1111001000000 OR M7,3) OR 4)
   put16(Shl(Shl(%010000 OR FS,3) OR fpx,7) OR %0101000)
   Put32(imm)
ENDPROC

EXPORT SET FPIAR,
    FPSR,
    FPCR

EXPORT PROC fmoveldxfpcr(dx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(%100000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelfpcrdx(dx)
   put16(Shl(%1111001000000 OR M0,3) OR dx)
   put16(Shl(%101000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelaxpifpcr(ax)
   put16(Shl(%1111001000000 OR M3,3) OR ax)
   put16(Shl(%100000 OR FPCR,10))
ENDPROC

EXPORT PROC fmovelfpcraxpd(ax)
   put16(Shl(%1111001000000 OR M4,3) OR ax)
   put16(Shl(%101000 OR FPCR,10))
ENDPROC


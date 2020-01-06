-> ECX/assembler.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT PREPROCESS

-> assembler.e, created May 2008. code extracted from codegen.e and ecxmain.e

->#define DBG_ASSEM

MODULE '*opcodes68'
MODULE '*opcodesppc'
MODULE '*compiler'
MODULE '*binary'  -> REFxxx
MODULE '*common' -> reportErr(), reportIErr()

EXPORT DEF
   g_codeptr:PTR TO LONG,
   g_codebuf,
   g_codelablist:PTR TO genlab,
   link_reloc32list:PTR TO LONG,
   link_nrofreloc32s,
   link_codesize

DEF g_relstrings

-> 1.10.0: moved here. now of genlab and put in g_codelablist as
-> well as g_relstrings
EXPORT OBJECT relstr OF genlab
   next:PTR TO relstr
   string:PTR TO CHAR
ENDOBJECT


#define NewLabel (g_codelablist := NEW [0,g_codelablist,0,0]:genlab)
#define PutLabel(codelab) genlab.offset := g_codeptr - g_codebuf + link_codesize
#define AddReloc(ofs) link_reloc32list := link_nrofreloc32s++ BUT NEW [link_reloc32list, ofs]:LONG
#define CurrentOffset (g_codeptr - g_codebuf + link_codesize)
#define PutAlign(size) g_codeptr := g_codeptr + (size-1) AND (-size)

EXPORT PROC newLabel()
   DEF r
   r := NewLabel
ENDPROC r
EXPORT PROC putLabel(genlab:PTR TO genlab)
   PutLabel(genlab)
ENDPROC
EXPORT PROC addReloc(ofs)
   AddReloc(ofs)
ENDPROC
EXPORT PROC currentOffset()
   DEF r
   r := CurrentOffset
ENDPROC r
EXPORT PROC putAlign(size)
   PutAlign(size)
ENDPROC

EXPORT PROC put24ofs(code:PTR TO LONG, ofs)
   code[] := ofs AND $3FFFFFC OR code[]
ENDPROC

EXPORT PROC patchReferences()
   DEF lab:PTR TO codelab, labref:PTR TO labref, a, t, b

   #ifdef DBG_ASSEM
   DEBUGF('patchReferences()\n')
   #endif

   lab := g_codelablist
   WHILE lab
      labref := lab.labrefs
      WHILE labref
         a := labref.type
         SELECT a
         CASE REF32 -> 68k
            b := lab.offset - labref.offset
            PutLong(g_codebuf + labref.offset - link_codesize, b)
         CASE REF24 -> ppc only
            b := lab.offset - labref.offset
            put24ofs(g_codebuf + labref.offset - link_codesize, b)
         CASE REF16 -> 68k only
            b := lab.offset - labref.offset
            IF b > 32767 THEN reportErr('too large distance', IF lab.identID THEN lab.name ELSE NIL)
            PutInt(g_codebuf + labref.offset - link_codesize, b)
         CASE REF14  -> ppc only
            b := lab.offset - labref.offset
            IF b > 32764 THEN reportErr('too large distance', IF lab.identID THEN lab.name ELSE NIL)
            PutInt((t := g_codebuf + labref.offset - link_codesize + 2), Int(t) OR b)
         CASE REFADR  -> 68k / ppc
            PutLong(g_codebuf + labref.offset - link_codesize, lab.offset)
            AddReloc(labref.offset)
         CASE REF1616 -> ppc only
            b := lab.offset - labref.offset
            t := g_codebuf + labref.offset - link_codesize
            PutInt(t + 6, b AND $FFFF)
            PutInt(t + 10, (Shr(b,16) AND $FFFF) + IF b AND $8000 THEN 1 ELSE 0)
         DEFAULT
            WriteF('type = \d\n', a)	
            IF lab.identID THEN WriteF('name = \s\n', lab.name)
            reportIErr(' patchRefs() type')
         ENDSELECT
         labref := labref.next
      ENDWHILE
      lab := lab.codelink
   ENDWHILE

    #ifdef DBG_ASSEM
   DEBUGF('patchReferences() DONE\n')
   #endif
ENDPROC

-> 1.10.0, now merges strings (MUST be estrings!)
EXPORT PROC addRelStr(str)
   DEF relstr:PTR TO relstr, ref:PTR TO labref
   relstr := g_relstrings
   WHILE relstr
      IF EstrLen(str) = EstrLen(relstr.string)
         IF StrCmp(relstr.string, str, EstrLen(str))
            RETURN relstr
         ENDIF
      ENDIF
      relstr := relstr.next
   ENDWHILE
   relstr := NEW  [NIL,
                   g_codelablist,
                   NIL,
                   NIL,
                   g_relstrings, str]:relstr
   g_relstrings := relstr
   g_codelablist := relstr
ENDPROC relstr

-> moved back here 1.9.0
-> 1.10.0 mods for merged strings
EXPORT PROC putRelocStrings()
   DEF relstr:PTR TO relstr, t

    #ifdef DBG_ASSEM
   DEBUGF('putRelocStrings()\n')
   #endif

   relstr := g_relstrings
   WHILE relstr
      relstr.offset := CurrentOffset
      CopyMem(relstr.string, g_codeptr, t := EstrLen(relstr.string))
      g_codeptr := g_codeptr + t + 1
      relstr := relstr.next
   ENDWHILE
   PutAlign(4)
ENDPROC

EXPORT PROC putReloc(lab:PTR TO codelab)
   #ifdef DBG_ASSEM
   DEBUGF('putReloc32($\h): \d\n', lab, lab.offset)
   #endif
   IF lab.offset = NIL
      lab.labrefs := NEW [lab.labrefs,CurrentOffset,REFADR]:labref
      g_codeptr++
   ELSE
      AddReloc(CurrentOffset)
      g_codeptr[]++ := lab.offset
   ENDIF
ENDPROC

-> handy
-> v45. better have in one place
-> "reg" CANNOT be r0 !
EXPORT PROC ppcliw(reg,val)
   IF (Abs(val) < 32768) AND (val <> $80000000) -> 1.10.0 fix
      ppcaddi(reg,0,val AND $FFFF)
   ELSE
      ppcaddi(reg,0,val AND $FFFF)
      ppcaddis(reg,reg,Shr(val,16) AND $FFFF + IF val AND $8000 THEN 1 ELSE 0)
   ENDIF
ENDPROC

-> v45.
EXPORT PROC ppcaddiw(d,s,val)
   ppcaddi(d,s,val AND $FFFF)
   IF (Abs(val) > $7FFF) OR (val = $80000000)  -> 1.10.0 fix
      ppcaddis(d,d,Shr(val,16) AND $FFFF + IF val AND $8000 THEN 1 ELSE 0)
   ENDIF
ENDPROC

-> v55
-> may trash r0 !
-> dreg may NOT be r0 !
-> dreg and sreg may NOT be same !
EXPORT PROC ppcdiviw(dreg, sreg, imm)
   SELECT imm
   CASE 1
      RETURN
   CASE 2
      ppcsrawi(sreg,dreg,1,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 4
      ppcsrawi(sreg,dreg,2,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 8
      ppcsrawi(sreg,dreg,3,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 16
      ppcsrawi(sreg,dreg,4,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 32
      ppcsrawi(sreg,dreg,5,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 64
      ppcsrawi(sreg,dreg,6,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 128
      ppcsrawi(sreg,dreg,7,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 256
      ppcsrawi(sreg,dreg,8,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 512
      ppcsrawi(sreg,dreg,9,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 1024
      ppcsrawi(sreg,dreg,10,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 2048
      ppcsrawi(sreg,dreg,11,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 4096
      ppcsrawi(sreg,dreg,12,0)
      ppcaddze(dreg,dreg,0,0)
   CASE 8192
      ppcsrawi(sreg,dreg,13,0)
      ppcaddze(dreg,dreg,0,0)
   DEFAULT
      ppcliw(dreg,imm)
      ppcdivw(dreg,sreg,dreg,0,0)
   ENDSELECT
ENDPROC

EXPORT PROC ppcblab(lab:PTR TO genlab,aa,lk)
   IF lab.offset
      ppcb(Shr(lab.offset - CurrentOffset,2),aa,lk)
   ELSE
      lab.labrefs := NEW [lab.labrefs, CurrentOffset, REF24]:labref
      ppcb(NIL,aa,lk)
   ENDIF
ENDPROC

EXPORT PROC ppcbclab(bo,bl,lab:PTR TO genlab,aa,lk)
   DEF d
   IF lab.offset
      d := lab.offset - CurrentOffset
      IF Abs(d) > 32764 THEN reportErr('too large distance',NIL)
      ppcbc(bo,bl,Shr(d,2),aa,lk)
   ELSE
      lab.labrefs := NEW [lab.labrefs, CurrentOffset, REF14]:labref
      ppcbc(bo,bl,NIL,aa,lk)
   ENDIF
ENDPROC

EXPORT PROC ppcaddilab(d,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppcaddi(d,a,glob.offset)
ENDPROC

EXPORT PROC ppclacode(d,lab:PTR TO genlab)
   DEF t
   IF lab.offset
      ppcb(1,0,1)
      t := lab.offset-CurrentOffset
      IF Abs(t) < $7FFF
         ppcmfspr(d,8)
         ppcaddi(d,d,t)
      ELSE
         ppcmfspr(d,8)
         ppcaddi(d,d,t AND $FFFF)
         ppcaddis(d,d,Shr(t,16) AND $FFFF + IF t AND $8000 THEN 1 ELSE 0)
      ENDIF
   ELSE
      ppcb(1,0,1)
      lab.labrefs := NEW [lab.labrefs, CurrentOffset, REF1616]:labref
      ppcmfspr(d,8)
      ppcaddi(d,d,NIL)
      ppcaddis(d,d,NIL)
   ENDIF
ENDPROC

EXPORT PROC ppclwzlab(d,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppclwz(d,a,glob.offset)
ENDPROC

EXPORT PROC ppcstwlab(s,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppcstw(s,a,glob.offset)
ENDPROC

EXPORT PROC ppclfslab(d,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppclfs(d,a,glob.offset)
ENDPROC

EXPORT PROC ppclfdlab(d,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppclfd(d,a,glob.offset)
ENDPROC

EXPORT PROC ppcstfslab(s,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppcstfs(s,a,glob.offset)
ENDPROC

EXPORT PROC ppcstfdlab(s,a,glob:PTR TO gvar)
   glob.labrefs := NEW [glob.labrefs, CurrentOffset+2]:LONG
   ppcstfd(s,a,glob.offset)
ENDPROC


/* 68k low level stuff */

#define ADDGLOBREF(_a_) lab.labrefs := NEW [lab.labrefs, CurrentOffset+_a_]:LONG

EXPORT PROC bsrlab(lab:PTR TO genlab, forcefar=FALSE)
   DEF t
   #ifdef DBG_ASSEM
   DEBUGF('bsrlab(lab=$\h) \d\n', lab, lab.offset)
   #endif
   t := lab.offset - CurrentOffset-2
   IF lab.offset
      IF forcefar -> 2.0, nilcheck
         bsrofs32(t)
      ELSEIF Abs(t) < 32767
         bsrofs16(t)
      ELSE
         bsrofs32(t)
      ENDIF
   ELSE
      lab.labrefs := NEW [lab.labrefs, CurrentOffset+2, REF32]:labref
      bsrofs32(NIL)
   ENDIF
ENDPROC

EXPORT PROC bcclab(cc,lab:PTR TO codelab)
   DEF pcofs, t
   #ifdef DBG_ASSEM
   DEBUGF('bcclab(cc=\d, lab=$\h)\n', cc, lab)
   #endif
   pcofs := CurrentOffset + 2
   IF lab.offset
      t := lab.offset - pcofs
      IF Abs(t) < 128
         RETURN bccofs8(cc,t)
      ELSEIF Abs(t) < 32767
         RETURN bccofs16(cc,t)
      ELSE
         reportErr('Too large distance', lab.name) -> too large distance
      ENDIF
   ENDIF
   lab.labrefs := NEW [lab.labrefs, pcofs, REF16]:labref
   bccofs16(cc,NIL)
ENDPROC

EXPORT PROC dbcclab(cc,dx,lab:PTR TO codelab)
   DEF t
   t := lab.offset - CurrentOffset - 2
   IF Abs(t) > 32767 THEN reportErr('Too large distance', lab.name)
   IF lab.offset
      dbccofs(cc,dx,t)
   ELSE
      lab.labrefs := NEW [lab.labrefs, CurrentOffset+2, REF16]:labref
      dbccofs(cc,dx,NIL)
   ENDIF
ENDPROC

-> optimisations implemented:
-> move.l #0, lab(ax) -> clr.l lab(ax)
-> cmp.l #0, lab(ax) -> tst.l lab(ax)
-> add.l #imm, lab(ax) -> addq.l #imm, lab(ax)
-> sub.l #imm, lab(ax) -> subq.l #imm, lab(ax)

EXPORT PROC moveldxaxplab(dx,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   movedxaxpofs(SIZE_L,dx,axp,NIL)
ENDPROC

EXPORT PROC movelaxaxplab(ax,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   moveaxaxpofs(SIZE_L,ax,axp,NIL)
ENDPROC

EXPORT PROC movelaxpaxplab(axp1,axp2,lab:PTR TO gvar)
   ADDGLOBREF(2)
   moveaxpaxpofs(SIZE_L,axp1,axp2,NIL)
ENDPROC

EXPORT PROC movelaxpiaxplab(axp1,axp2,lab:PTR TO gvar)
   ADDGLOBREF(2)
   moveaxpiaxpofs(SIZE_L,axp1,axp2,NIL)
ENDPROC

EXPORT PROC movelaxpdaxplab(axp1,axp2,lab:PTR TO gvar)
   ADDGLOBREF(2)
   moveaxpdaxpofs(SIZE_L,axp1,axp2,NIL)
ENDPROC

EXPORT PROC movelaxpofsaxplab(axp1,ofs,axp2,lab:PTR TO gvar)
   ADDGLOBREF(4)
   moveaxpofsaxpofs(SIZE_L,axp1,ofs,axp2,NIL)
ENDPROC

EXPORT PROC movelaxplabaxplab(axp1,lab1:PTR TO gvar,axp2,lab2:PTR TO gvar)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2]:LONG
   lab2.labrefs := NEW [lab2.labrefs, CurrentOffset+4]:LONG
   moveaxpofsaxpofs(SIZE_L,axp1,NIL,axp2,NIL)
ENDPROC

EXPORT PROC movelaxpxaxplab(axp1,idrx,scale,d,axp2,lab:PTR TO gvar)
   ADDGLOBREF(4)
   moveaxpxaxpofs(SIZE_L,axp1,idrx,scale,d,axp2,NIL)
ENDPROC

EXPORT PROC movelimmaxplab(imm,axp,lab:PTR TO gvar)
   IF imm=0 THEN RETURN clrlaxplab(axp,lab)
   ADDGLOBREF(6)
   movelimmaxpofs(imm,axp,NIL)
ENDPROC

EXPORT PROC clrlaxplab(axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   clraxpofs(SIZE_L,axp,NIL)
ENDPROC

EXPORT PROC movelpcplabaxplab(lab1:PTR TO codelab,axp,lab2:PTR TO gvar)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   lab2.labrefs := NEW [lab2.labrefs, CurrentOffset+4]:LONG
   movepcpofsaxpofs(SIZE_L,NIL,axp,NIL)
ENDPROC
EXPORT PROC movelpcplabdx(lab1:PTR TO codelab,dx)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsdx(SIZE_L,NIL,dx)
ENDPROC
EXPORT PROC movelpcplabax(lab1:PTR TO codelab,ax)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsax(SIZE_L,NIL,ax)
ENDPROC
EXPORT PROC movelpcplabaxp(lab1:PTR TO codelab,ax)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsaxp(SIZE_L,NIL,ax)
ENDPROC
EXPORT PROC movelpcplabaxpi(lab1:PTR TO codelab,ax)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsaxpi(SIZE_L,NIL,ax)
ENDPROC
EXPORT PROC movelpcplabaxpd(lab1:PTR TO codelab,ax)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsaxpd(SIZE_L,NIL,ax)
ENDPROC
EXPORT PROC movelpcplabaxpofs(lab1:PTR TO codelab,axp,ofs)
   lab1.labrefs := NEW [lab1.labrefs, CurrentOffset+2,REF16]:labref
   movepcpofsaxpofs(SIZE_L,NIL,axp,ofs)
ENDPROC

EXPORT PROC movelaxplabdx(axp,lab:PTR TO gvar,dx)
   ADDGLOBREF(2)
   moveaxpofsdx(SIZE_L,axp,NIL,dx)
ENDPROC

EXPORT PROC movelaxplabax(axp,lab:PTR TO gvar,ax)
   ADDGLOBREF(2)
   moveaxpofsax(SIZE_L,axp,NIL,ax)
ENDPROC

EXPORT PROC moveaxplabaxp(s,axp1,lab:PTR TO gvar,axp2)
   ADDGLOBREF(2)
   moveaxpofsaxp(s,axp1,NIL,axp2)
ENDPROC

EXPORT PROC movelaxplabaxpi(axp1,lab:PTR TO gvar,axp2)
   ADDGLOBREF(2)
   moveaxpofsaxpi(SIZE_L,axp1,NIL,axp2)
ENDPROC

EXPORT PROC movelaxplabaxpd(axp1,lab:PTR TO gvar,axp2)
   ADDGLOBREF(2)
   moveaxpofsaxpd(SIZE_L,axp1,NIL,axp2)
ENDPROC

EXPORT PROC moveaxplabaxpofs(s,axp1,lab:PTR TO gvar,axp2,ofs)
   ADDGLOBREF(2)
   moveaxpofsaxpofs(s,axp1,NIL,axp2,ofs)
ENDPROC

EXPORT PROC moveaxplabaxpx(s,axp1,lab:PTR TO gvar,axp2,idrx,scale,d)
   ADDGLOBREF(2)
   moveaxpofsaxpx(s,axp1,NIL,axp2,idrx,scale,d)
ENDPROC

EXPORT PROC leapcplabax(lab:PTR TO codelab,ax)
   DEF pcofs, t
   pcofs := CurrentOffset + 2
   IF lab.offset
      t := lab.offset - pcofs
      IF Abs(t) < 32768
         leapcpofsax(t, ax)
      ELSE
         ->lab.labrefs := NEW [lab.labrefs, pcofs, REFADR]:labref
         putMOVEI(SIZE_L,ax,M1,lab.offset) ->movelimmax(NIL, ax)
         AddReloc(pcofs)
      ENDIF
   ELSE
      lab.labrefs := NEW [lab.labrefs, pcofs, REFADR]:labref
      putMOVEI(SIZE_L,ax,M1,NIL) ->movelimmax(NIL, ax)
   ENDIF
ENDPROC

EXPORT PROC leaaxplabax(axp,lab:PTR TO gvar,ax)
   ADDGLOBREF(2)
   leaaxpofsax(axp,NIL,ax)
ENDPROC

EXPORT PROC cmplimmaxplab(imm,ax,lab:PTR TO gvar)
   IF imm=0 THEN RETURN tstlaxplab(ax,lab)
   ADDGLOBREF(6)
   cmplimmaxpofs(imm,ax,NIL)
ENDPROC

EXPORT PROC tstlaxplab(ax,lab:PTR TO gvar)
   ADDGLOBREF(2)
   tstaxpofs(SIZE_L,ax,NIL)
ENDPROC

EXPORT PROC cmplaxplabdx(ax,lab:PTR TO gvar,dx)
   ADDGLOBREF(2)
   cmpaxpofsdx(SIZE_L,ax,NIL,dx)
ENDPROC

EXPORT PROC cmplaxplabax(axp,lab:PTR TO gvar,ax)
   ADDGLOBREF(2)
   cmpaxpofsax(SIZE_L,axp,NIL,ax)
ENDPROC

EXPORT PROC sublimmaxplab(imm,axp,lab:PTR TO gvar)
   IF Abs(imm) < 9 AND (imm > 0) THEN RETURN subqlaxplab(imm,axp,lab)
   ADDGLOBREF(6)
   sublimmaxpofs(imm,axp,NIL)
ENDPROC

EXPORT PROC subqlaxplab(imm,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   subqaxpofs(SIZE_L,imm,axp,NIL)
ENDPROC

EXPORT PROC subldxaxplab(dx,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   subdxaxpofs(SIZE_L,dx,axp,NIL)
ENDPROC

EXPORT PROC sublaxplabdx(axp,lab:PTR TO gvar,dx)
   ADDGLOBREF(2)
   subaxpofsdx(SIZE_L,axp,NIL,dx)
ENDPROC

EXPORT PROC sublaxplabax(axp,lab:PTR TO gvar,ax)
   ADDGLOBREF(2)
   subaxpofsax(SIZE_L,axp,NIL,ax)
ENDPROC

EXPORT PROC addlimmaxplab(imm,axp,lab:PTR TO gvar)
   IF Abs(imm) < 9 AND (imm > 0) THEN RETURN addqlaxplab(imm, axp, lab)
   ADDGLOBREF(6)
   addlimmaxpofs(imm,axp,NIL)
ENDPROC

EXPORT PROC addqlaxplab(imm,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   addqaxpofs(SIZE_L,imm,axp,NIL)
ENDPROC

EXPORT PROC addldxaxplab(dx,axp,lab:PTR TO gvar)
   ADDGLOBREF(2)
   adddxaxpofs(SIZE_L,dx,axp,NIL)
ENDPROC

EXPORT PROC addlaxplabdx(axp,lab:PTR TO gvar,dx)
   ADDGLOBREF(2)
   addaxpofsdx(SIZE_L,axp,NIL,dx)
ENDPROC

EXPORT PROC addlaxplabax(axp,lab:PTR TO gvar,ax)
   ADDGLOBREF(2)
   addaxpofsax(SIZE_L,axp,NIL,ax)
ENDPROC

EXPORT PROC fmoveaxplabfpx(fs,axp,lab:PTR TO gvar,fpx)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%010000 OR fs,3) OR fpx,7) OR %0000000)
   ADDGLOBREF(0)
   put16(NIL)
ENDPROC

EXPORT PROC fmovefpxaxplab(fs,fpx,axp,lab:PTR TO gvar)
   put16(Shl(%1111001000000 OR M5,3) OR axp)
   put16(Shl(Shl(%011000 OR fs,3) OR fpx,7) OR %0000000)
   ADDGLOBREF(0)
   put16(NIL)
ENDPROC

-> ECX/libstubs.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE, PREPROCESS

-> libstubs.e extracted 2009 from initcode.e
-> initcode.e. created module May 2008. code extracted from codegen.e and ecxmain.e

-> 1.10.0 : removed library specific initcode.

->#define DBG_LIBSTUBS

MODULE '*binary'
MODULE '*opcodes68'
MODULE '*opcodesppc'
MODULE '*runtime'
MODULE '*compiler'
MODULE '*common'
MODULE '*assembler'

MODULE 'exec/lists'

EXPORT DEF g_nrofentries, g_entrytable:PTR TO LONG
EXPORT DEF g_codeptr:PTR TO LONG, g_codebuf, link_codesize
EXPORT DEF g_gvarlist:PTR TO gvar
EXPORT DEF link_globaldatasize
EXPORT DEF g_databuf:PTR TO LONG, g_databufsize
EXPORT DEF g_modulelist:PTR TO mlh
EXPORT DEF g_objectlist:PTR TO object

#define Put32(v) g_codeptr[]++ := v

EXPORT PROC putEntries68K()
   DEF a, e:PTR TO entry, b, proc:PTR TO proc
   DEF stub:PTR TO genlab, par:PTR TO var
     /* make actual entries */
   IF Odd(g_codeptr) THEN g_codeptr := g_codeptr + 1
   FOR a := 0 TO g_nrofentries-1
      e := g_entrytable[a]
      IF e.prochln
         proc := e.prochln.ident
         IF proc = NIL THEN reportErr('unknown identifier', e.prochln.name)
         IF proc.identID <> IDENT_LABEL THEN reportErr('not suitable for entry', e.prochln.name)
         IF proc.ltype <> LTYPE_PROC THEN reportErr('not suitable for entry', e.prochln.name)
         proc.referenced := 1
         putLabel(e.label)
         e.nrofargs := proc.nrofargs
         IF proc.cpu = 0
            libstub68k_68k(e, proc)
         ELSE
            libstub68k_ppc(e, proc)
         ENDIF
      ELSE -> EMPTY
         putLabel(e.label)
         moveqdx(0,0)
         rts_()
      ENDIF
   ENDFOR

ENDPROC

EXPORT PROC putEntriesPPC()
   DEF a, e:PTR TO entry, proc:PTR TO proc
   /* make actual entries */
   FOR a := 0 TO g_nrofentries-1
      e := g_entrytable[a]
      putAlign(4)
      IF e.prochln
         proc := e.prochln.ident
         IF proc = NIL THEN reportErr('unknown identifier', e.prochln.name)
         IF proc.identID <> IDENT_LABEL THEN reportErr('not suitable for entry', e.prochln.name)
         IF proc.ltype <> LTYPE_PROC THEN reportErr('not suitable for entry', e.prochln.name)
         proc.referenced := 1
         putLabel(e.label)
         e.nrofargs := proc.nrofargs
         IF e.type = 0 -> 68k abi in ppc mode ?
            IF proc.cpu = 0 -> 68k->68k ?
               libstub68k_68k(e, proc)
            ELSE -> 68k->ppc
               libstub68k_ppc(e, proc)
            ENDIF
         ELSE
            IF proc.cpu = 1
               libstubppc_ppc(e, proc)
            ELSE
               libstubppc_68k(e, proc)
            ENDIF
         ENDIF
      ELSE -> EMPTY
         IF e.type = 1 -> ppc abi
            putLabel(e.label)
            ppcaddi(3,0,0)
            ppcbclr(20,0,0)
         ELSE  -> 68k abi
            putLabel(e.label)
            moveqdx(0,0)
            rts_()
         ENDIF
      ENDIF
   ENDFOR

ENDPROC

PROC libstubppc_ppc(e:PTR TO entry, proc:PTR TO proc)
   DEF a, endstub:PTR TO genlab, t, par:PTR TO var

   FOR t := 0 TO proc.nrofargs-1  -> v48
      par := proc.argarray[t]
      e.regs[t].rtype := par.rtype
      e.regs[t].rnum := par.rnum
   ENDFOR
   SELECT 20 OF proc.mret.ros[0] -> 2.0
   CASE RX  ; e.return := 0
   CASE FPX ; e.return := 1
   CASE VX  ; e.return := 2
   CASE X2R ; e.return := 3
   ENDSELECT
   ppcstwu(1,1,-64)
   ppcmfspr(0,8)
   ppcstw(0,1,68)
   ppcstw(13,1,60)
   ppclwz(13,e.basernum,PRIVBASE_ENV)    -> changed 1.10.0
   FOR t := 0 TO proc.nrofargs-1  -> v48
      par := proc.argarray[t]
      IF par.rtype = PPCARGRTYPE_STACKRX
         ppclwz(0,1,par.rnum+64)
         ppcstw(0,1,par.rnum)
      ELSEIF par.rtype = PPCARGRTYPE_STACKRX2  -> 2.0
         ppclfd(0,1,par.rnum+64)
         ppcstfd(0,1,par.rnum)
      ENDIF
   ENDFOR
   ppcblab(proc,0,1)
   ppclwz(0,1,68)
   ppclwz(13,1,60)
   ppcmtspr(0,8)
   ppcaddi(1,1,64)
   ppcbclr(20,0,0)

ENDPROC

PROC libstubppc_68k(e:PTR TO entry, proc:PTR TO proc)
   DEF a, endstub:PTR TO genlab
      ppcstwu(1, 1, -16)
      ppcmfspr(0,8)
      ppcstw(0,1,20)
      FOR a := 0 TO proc.nrofargs-1
         ppcstw(e.regs[a].rnum, 2, a*4)
      ENDFOR
      ppclwz(0, e.basernum, PRIVBASE_ENV)
      ppcstw(0, 2, 48) -> a4
      -> do the twist
      endstub := newLabel()
      ppcblab(endstub,0,1) -> skip 68k stubcode
      -> stub: from regs to stack as params for procedure
      FOR a := 0 TO proc.nrofargs-1 DO movedxaxpd(SIZE_L,a,7)
      bsrlab(proc)
      leaaxpofsax(7,proc.nrofargs*4,7)
      rts_()
      putAlign(4) -> align 4
      putLabel(endstub)
      ppcmfspr(3,8) -> to r3
      ppclwz(0,2,104) -> emulcalldirect68k to r0
      ppcmtspr(0,9) -> ctr
      ppcbcctr(20,0,1) -> bctrl
      ppclwz(0,1,20)
      ppcmtspr(0,8)
      ppcaddi(1,1,16)
      ppcbclr(20,0,0)

ENDPROC

PROC libstub68k_68k(e:PTR TO entry, proc:PTR TO proc)
   DEF b

   -> save regs..
   movemregsaxpd(SIZE_L, %0011111100111110, 7)
   -> get a4 from libbase pointed to by A6
   moveaxpofsax(SIZE_L,6,PRIVBASE_ENV,4)   -> changed 1.10.0
   -> push regs as proc-params
   FOR b := 0 TO proc.nrofargs-1
      IF e.regs[b].rtype = 1
         moveaxaxpd(SIZE_L,e.regs[b].rnum,7)
      ELSEIF e.regs[b].rtype = 0
         movedxaxpd(SIZE_L,e.regs[b].rnum,7)
      ENDIF
   ENDFOR
   -> call proc
   bsrlab(proc)
   -> cleanup stack
   IF proc.nrofargs THEN leaaxpofsax(7,proc.nrofargs*4,7)
   -> restore regs
   movemaxpiregs(SIZE_L,7, %0111110011111100)
   -> return
   rts_()

ENDPROC

PROC libstub68k_ppc(e:PTR TO entry, proc:PTR TO proc)
   DEF b, stub:PTR TO genlab, par:PTR TO var

   stub := newLabel()
   Put32($FF000000) -> TRAP_LIB
   putReloc(stub)
   putAlign(4)
   putLabel(stub)
   ppcstwu(1,1,-64)
   ppcmfspr(0,8)
   ppclwz(12,2,56) -> bas
   ppcstw(13,1,60)
   ppcstw(0,1,68)
   ppclwz(13,12,PRIVBASE_ENV)      -> changed 1.10.0
   FOR b := 0 TO proc.nrofargs-1
      par := proc.argarray[b]
      IF par.rtype = 0
         IF e.regs[b].rtype = 0
            ppclwz(par.rnum, 2, e.regs[b].rnum*4)
         ELSE
            ppclwz(par.rnum, 2, e.regs[b].rnum*4+32)
         ENDIF
      ELSEIF par.rtype = 1
         IF e.regs[b].rtype = 0
            ppclwz(0, 2, e.regs[b].rnum*4)
            ppcstw(0, 1, par.rnum)
         ELSE
            ppclwz(0, 2, e.regs[b].rnum*4+32)
            ppcstw(0, 1, par.rnum)
         ENDIF
      ELSE
         reportErr('procedure is not suitable as entry', proc.name)
      ENDIF
   ENDFOR
   ppcblab(proc,0,1)
   ppclwz(0,1,68)
   ppclwz(13,1,60)
   ppcmtspr(0,8)
   ppcaddi(1,1,64)
   ppcbclr(20,0,0)

ENDPROC

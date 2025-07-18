
-> EEC/000gen.e
/* formerly ECX/m68gen.e */

/* EEC by Samuel D. Crow [samuraileumas yahoo com] is Copyright (c) 2019-2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

-> File created May 2008, extracted from codegen.e.


OPT MODULE
OPT PREPROCESS
OPT LARGE

->#define DBG_M68GEN

MODULE '*codegen'
MODULE '*compiler'
MODULE '*assembler'
MODULE '*opcodes68'
MODULE '*opcodesppc'
MODULE '*common'
MODULE '*inline68'
MODULE '*support'
MODULE '*runtime'
MODULE '*binary'

MODULE 'exec/lists'

#define DOSICBICOPTI

EXPORT OBJECT m68amiga OF codegen
   lasttempDREG
   lasttempAREG
   lasttempFREG
ENDOBJECT

EXPORT DEF g_codeptr:PTR TO LONG,
           g_codebuf,
           g_currentproc:PTR TO proc,
           g_rwreflist:PTR TO rwref,
           g_globalsize,
           g_databufsize,
           g_databuf:PTR TO LONG,
           g_numregalloc,
           g_numfregalloc,
           g_gvarlist:PTR TO gvar,
           g_regusetab:PTR TO oreg

EXPORT DEF g_od:PTR TO LONG -> shared with inline.e

EXPORT DEF g_lastx:PTR TO lastx

DEF g_exceptlab:PTR TO codelab
DEF g_endexceptlab:PTR TO codelab
DEF g_procendlab:PTR TO codelab

DEF g_d64bottom -> offset of bottom of d64 stack
DEF g_lastd64 -> actuall frameoffset and always negative

EXPORT DEF g_librarymode, g_optmodule, g_optroundnear

EXPORT DEF
   g_dreg, -> RX/DRX
   g_areg, -> RX/ARX
   g_freg, -> FPX
   g_vreg, -> VX
   g_d64reg, -> D64 v55
   g_ireg0,  -> R3/D0   (non obtainable)
   g_ireg1,  -> R4/D1
   g_freg0,  -> FP1, F1
   g_stackreg, -> R1/A7  (non obtainable, dedicated)
   g_selfreg,  -> R12/A0
   g_globreg,  -> R13/A4 (non obtainable, dedicated)
   g_framereg, -> R1/A5  (non obtainable, dedicated)
   g_atemp,   -> R11/A6 (non obtainable)
   g_dtemp,   -> R12/D3 (non obtain)
   g_ftemp,   -> FP0/F0 (non obtain)
   g_d64temp -> v55

EXPORT DEF g_multireturn:PTR TO multireturn

EXPORT DEF g_modulelist:PTR TO mlh, link_globaldatasize -> for .putInit()

PROC init() OF m68amiga

   #ifdef DBG_M68GEN
   DEBUGF('m8amiga.init()\n')
   #endif

   DREG := DRX
   AREG := ARX
   FREG := FPX
   D64REG := D64 -> 2.3

   IREG0 := 0 -> D0
   IREG1 := 1 -> D1
   FREG0 := 1

   STACKREG := 7 -> A7
   SELFREG  := 0 -> A0 self arg
   GLOBREG  := 4 -> A4
   FRAMEREG := 5 -> A5
   ATEMP := 6 -> A6. non obtainable ptr scratch
   DTEMP := 3 -> d3 v50
   FTEMP := 0 -> f0 v50
   D64TEMP := 0   -> is set by self.doprochead()

   self.lasttempDREG := 1
   self.lasttempAREG := 1
   self.lasttempFREG := 1

ENDPROC

PROC putInit(globalsize, ivarsize) OF m68amiga
   #ifdef DBG_M68GEN
   DEBUGF('put68KInit()\n')
   #endif
   moveaxpofsax(SIZE_L,7,4,0) -> move.l 4(a7), a0 -> rwmemory
   putRWInit()
   leaaxpofsax(0,ivarsize,0) -> lea IVARSSIZE(a0), a0
   moveaxax(SIZE_L,0,GLOBREG) -> a4 is set up !
   -> set up a0 to globaldata
   addlimmax(globalsize, 0) -> add.l #g_globalsize, a0
   self.putGlobInit()
   putModulesInit()
   moveaxdx(SIZE_L,0,0) -> 1.10.0, return end of globdata so we may place stak after also in libmode
   rts_() -> returns endofglobdata in d0
ENDPROC


PROC putRWInit()
   DEF mod:PTR TO moduleheader
   DEF info:PTR TO modinfo
   DEF t
   DEF lptr:PTR TO LONG
   DEF a

   mod := g_modulelist.head
   WHILE mod.succ
      info := mod.datainfo
      IF info
         info := info + mod
         lptr := info + SIZEOF modinfo
         a := info.count+1
         WHILE a--
            t := lptr[]++
            IF t THEN movelimmaxpi(t, 0) ELSE addqax(SIZE_L,4,0)
         ENDWHILE
      ENDIF
      mod := mod.succ
   ENDWHILE

   lptr := g_databuf
   a := Shr(g_databufsize, 2) + 1
   WHILE a--
      t := lptr[]++
      IF t THEN movelimmaxpi(t, 0) ELSE addqax(SIZE_L,4,0)
   ENDWHILE

ENDPROC


-> 2.2 now called directly from main in module mode
PROC putGlobInit() OF m68amiga
   DEF gvar:PTR TO gvar
   -> initialise globals (v44)
   gvar := g_gvarlist
   WHILE gvar
      link_globaldatasize := link_globaldatasize + initGvar(gvar)
      gvar := gvar.next
   ENDWHILE

   IF g_optmodule THEN rts_()  -> 2.2

ENDPROC

PROC putModulesInit()
   DEF mod:PTR TO moduleheader, info:PTR TO modinfo, t
   mod := g_modulelist.head
   WHILE mod.succ
      info := IF mod.headsize < 160 THEN NIL ELSE mod.globinitinfo
      IF info
         IF mod.cpu <> CPU_M68 THEN reportErr('init code cpu mismatch for module', mod.mname)
         info := info + mod
         t := info.misc - (currentOffset() + 2)
         bsrofs32(t)
      ENDIF
      mod := mod.succ
   ENDWHILE
ENDPROC

#define AxToGlobal(x) IF gvar.link THEN movelaxaxplab(x, GLOBREG, gvar) ELSE \
                        moveaxaxpofs(SIZE_L,x,GLOBREG,gvar.offset)

#define FpxToGlobal(x) IF gvar.link THEN fmovefpxaxplab(FD,x,GLOBREG,gvar.offset) ELSE \
                        fmovefpxaxpofs(FD,x,GLOBREG,gvar.offset)

PROC initGvar(gvar:PTR TO gvar)
       DEF t, globaldatasize=0
      IF gvar.type.numes
         SELECT 256 OF gvar.type.esize
         CASE 1
            IF gvar.cmplx  -> STRING
               globaldatasize := 8 + (t := gvar.type.numes + 1 + 3 AND $FFFC)
               addqax(SIZE_L,4,0) -> next=NIL
               movelimmaxpi(Shl(gvar.type.numes,16),0) -> max,curr
               clraxp(SIZE_L,0)
               AxToGlobal(0)
               addlimmax(t,0)
            ELSE           -> ARRAY OF CHAR
               globaldatasize := (t := gvar.type.numes + 3 AND $FFFC)
               AxToGlobal(0)
               addlimmax(t,0)
            ENDIF
         CASE 2   -> ARRAY OF INT
            globaldatasize := (t := gvar.type.numes * 2 + 3 AND $FFFFFC)
            AxToGlobal(0)
            addlimmax(t,0)
         CASE 4
            IF gvar.cmplx  -> LIST
               globaldatasize := 8 + (t := gvar.type.numes * 4)
               addqax(SIZE_L,4,0) -> next=NIL
               movelimmaxpi(Shl(gvar.type.numes,16),0) -> max,curr
               AxToGlobal(0)
               addlimmax(t,0)
            ELSE           -> ARRAY OF LONG
               globaldatasize := (t := gvar.type.numes * 4)
               AxToGlobal(0)
               addlimmax(t,0)
            ENDIF
         CASE 8
              globaldatasize := (t := gvar.type.numes * 8)
               AxToGlobal(0)
               addlimmax(t,0)
         CASE 255 -> (ARRAY OF) OBJECT
            IF gvar.type.object.nrofmethods
               addWarning('methods will not be installed for global')
            ENDIF
            globaldatasize := (t := gvar.type.numes *
                                 gvar.type.object.sizeof + 3 AND $FFFFFC)
            AxToGlobal(0)
            addlimmax(t,0)
         ENDSELECT
      ELSEIF gvar.defo
         IF gvar.type.size = 4
            movelimmax(gvar.defd, 1)
            AxToGlobal(1)
         ELSE
            IF gvar.type.flags AND MEMBF_FLOAT
               fmovesimmfpx(gvar.defd, 0)
               FpxToGlobal(0)
            ELSE -> 2.2
               movelimmaxpd(gvar.defd, 7)
               movelimmaxpd(Shr(gvar.defd,31), 7)
               fmoveaxpifpx(FD, 7, 0)
               FpxToGlobal(0)
            ENDIF
         ENDIF
      ENDIF
ENDPROC globaldatasize

PROC abiArgs(array:PTR TO LONG, numargs, flags) OF m68amiga
   DEF va:PTR TO arg, a, fnum=1
   -> 1.8.0 flags does nada in 68k mode for now
   FOR a := 0 TO numargs-1
      va := array[a]
      IF va.type.size = 4
         va.rtype := M68ARGTYPE_STACK -> stack
         va.rnum := NIL -> not used
      ELSEIF va.type.size = 8
         IF va.type.flags AND MEMBF_FLOAT
            IF fnum < 8
               va.rtype := M68ARGTYPE_FREG -> float reg
               va.rnum := fnum++
            ELSE
               reportErr('too many REAL arguments for procedure (bug author)')
            ENDIF
         ELSE
            -> 2.2.4, wide!
            va.rtype := M68ARGTYPE_STACK_WIDE
            va.rnum := NIL
         ENDIF
      ENDIF
   ENDFOR
ENDPROC



PROC abiReturns(mret:PTR TO multireturn) OF m68amiga
   DEF reg=0, freg=1, t
   t := mret.ros[0]
   SELECT t
   CASE DREG ; mret.rds[0] := reg++
   CASE FREG ; mret.rds[0] := freg++
   CASE X2R  ; mret.rds[0] := freg++
   DEFAULT   ; reportIErr('m68amiga.abiReturns 0 t=?')
   ENDSELECT
   t := mret.ros[1]
   SELECT t
   CASE DREG ; mret.rds[1] := reg++
   CASE FREG ; mret.rds[1] := freg++
   CASE X2R  ; mret.rds[1] := freg++
   DEFAULT   ; reportIErr('m68amiga.abiReturns 1 t=?')
   ENDSELECT
   t := mret.ros[2]
   SELECT t
   CASE DREG ; mret.rds[2] := reg++
   CASE FREG ; mret.rds[2] := freg++
   CASE X2R  ; mret.rds[2] := freg++
   DEFAULT   ; reportIErr('m68amiga.abiReturns 2 t=?')
   ENDSELECT
   t := mret.ros[3]
   SELECT t
   CASE DREG ; mret.rds[3] := reg++
   CASE FREG ; mret.rds[3] := freg++
   CASE X2R  ; mret.rds[3] := freg++
   DEFAULT   ; reportIErr('m68amiga.abiReturns 3 t=?')
   ENDSELECT
ENDPROC



-> v50: support for doubleargs
-> 2.3 added wide support
PROC doProcfunc(buf:PTR TO item, meth=NIL:PTR TO proc) OF m68amiga
   DEF n:PTR TO item, hln:PTR TO hln, proc:PTR TO proc
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg -> v44, 49
   DEF a, as, as2, param:PTR TO var
   DEF stub:PTR TO codelab, endstub:PTR TO codelab, trap:PTR TO codelab
   DEF nrofparams, t:PTR TO LONG
   DEF o,d, var:PTR TO var, float
   DEF argtype
   DEF it_funccall:PTR TO it_funccall
   DEF ro, rd
   DEF rems=0:PTR TO LONG, noraise

   it_funccall := buf.info
   hln := it_funccall.data ->n.info
   nrofparams := it_funccall.numpars ->buf.num
   n := it_funccall + SIZEOF it_funccall ->n++ -> skip label



   #ifdef DBG_M68GEN
   DEBUGF('.doProcfunc() \s numargs=\d\n', hln.name, nrofparams)
   #endif


   IF meth = NIL
      self.saveObtained(regusecopy)
      proc := hln.ident
      IF proc = NIL THEN reportErr('unknown function', hln.name)
      proc.referenced := 1
   ELSE
      proc := meth
   ENDIF

   ro := proc.mret.ros[0]
   rd := proc.mret.rds[0]

   IF nrofparams > proc.nrofargs THEN reportErr('too many parameters for procedure', proc.name)
   IF (nrofparams+proc.nrofdefaults) < proc.nrofargs THEN reportErr('too few parameters for procedure', proc.name)


      FOR a := 0 TO proc.nrofargs-1
         param := proc.argarray[a]
         IF n.data
            n, as, as2 := self.doExpression(n)
            IF param.rtype = M68ARGTYPE_FREG -> doublearg ? v50
               inst_copy(as,as2, FPX,param.rnum)
               self.copyObtainToFREG(FPX,param.rnum) -> put an obtain on register
            ELSEIF param.rtype = M68ARGTYPE_STACK_WIDE -> 2.3
               self.tox2r(as, as2, FTEMP)
               inst_push(8, FREG, FTEMP)
               rems++ ; rems++
            ELSE
               inst_push(4, as,as2)
               rems++
            ENDIF
         ELSE
            IF param.rtype = M68ARGTYPE_FREG -> doublearg ? v50
               inst_copy(DV, param.defd, FPX, param.rnum)
               self.copyObtainToFREG(FPX,param.rnum)  -> put an obtain on register
            ELSEIF param.rtype = M68ARGTYPE_STACK_WIDE -> 2.3
               self.tox2r(DV, param.defd, FTEMP)
               inst_push(8, FREG, FTEMP)
               rems++ ; rems++
            ELSE
               inst_push(4, DV, param.defd)
               rems++
            ENDIF
         ENDIF
      ENDFOR
      IF meth = NIL
         IF proc.cpu = 1 -> 68k calling ppc
            t := [DRX,0,0,DRX,1,4,DRX,2,8,DRX,3,12,ARX,0,32,ARX,1,36,ARX,2,40,ARX,3,44]
            FOR a := (proc.nrofargs-1) TO 0 STEP -1 -> into 68k trash regs
               IF t[a*3] = DRX THEN moveaxpidx(SIZE_L,7, t[a*3+1]) ELSE moveaxpiax(SIZE_L,7, t[a*3+1])
            ENDFOR

            trap := newLabel()
            stub := newLabel()
            endstub := newLabel()

            bsrlab(trap)
            bcclab(T,endstub) -> v49

            putLabel(trap)
            Put32($FF000000)
            putReloc(stub)

            putAlign(4) -> v49
            putLabel(stub)
            ppcstwu(1,1,-16) -> v49
            ppcmfspr(0,8) -> v49
            ppcstw(0,1,16+FRAMEPPC_LINKREG) -> v49
            ppcstw(13,1,8) -> v49
            ppclwz(13,2,48) -> v49
            FOR a := 0 TO proc.nrofargs-1 -> from emulhand into ppcregs
               ppclwz(a+3,2,t[a*3+2])  -> v49 fix
            ENDFOR
            ppcblab(proc,0,1) -> call ppc proc
            ppclwz(13,1,8) -> v49
            ppclwz(0,1,16+FRAMEPPC_LINKREG) -> v49
            ppcmtspr(0,8) -> v49
            ppcaddi(1,1,16) -> v49
            ppcbclr(20,0,0) -> v49

            putLabel(endstub)  -> v49
         ELSE
            -> v50, release any doubleargregisters
            FOR a := 0 TO nrofparams-1
               param := proc.argarray[a]
               IF param.rtype = 1 THEN self.releaseFREG(param.rnum,0)
            ENDFOR
            -> call function
            inst_goslab(proc)
            inst_incarx(STACKREG, rems)
         ENDIF
         ro, rd := self.secureReturn(ro, rd, regusecopy) -> v50
         self.loadObtained(regusecopy)
         clearRegs()
      ENDIF


   #ifdef DBG_M68GEN
   DEBUGF('dofunction() \s DONE\n', hln.name)
   #endif

   IF proc.raise -> 2.2
      noraise := newLabel()
      inst_bic(DV,proc.raise.trigval,proc.raise.condition, ro,rd,noraise)
      inst_push(4,DV,proc.raise.excval)
      bsrlab(self.raise_lif)
      def_label(noraise)
   ENDIF

   n := buf[1]

   g_multireturn := proc.mret -> v49


ENDPROC n, ro, rd



-> 1.10.0 added support for new style varfunc
-> 2.3 added wide support
PROC doVarfunc(n:PTR TO item) OF m68amiga
   DEF v:PTR TO var, r, count=0
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg
   DEF var:PTR TO var, o, d, arx
   DEF it_funccall:PTR TO it_funccall
   DEF ro, rd, a, param:PTR TO arg

   r := n[1]

   it_funccall := n.info
   v := it_funccall.data
   n := it_funccall + SIZEOF it_funccall

   #ifdef DBG_M68GEN
   DEBUGF('dovfunction() \s \n', v.hln.name)
   #endif

   self.saveObtained(regusecopy)

   IF v.argsarray = FALSE

      ro := DREG
      rd := IREG0

      WHILE n.data
         count++
         n, o, d := self.doExpression(n)
         inst_push(4,o,d)
      ENDWHILE


   ELSE -> 1.10.0

      FOR a := 0 TO ListLen(v.argsarray)-1
         param := v.argsarray[a]
         n, o, d := self.doExpression(n)
         IF param.rtype = M68ARGTYPE_FREG -> doublearg ?
            inst_copy(o,d, FPX,param.rnum)
            self.copyObtainToFREG(FPX,param.rnum) -> put an obtain on register
         ELSEIF param.rtype = M68ARGTYPE_STACK_WIDE -> 2.3
            inst_push(8, o,d)
            count++ ; count++
         ELSE
            inst_push(4, o,d)
            count++
         ENDIF
      ENDFOR

      ro := IF v.multiret THEN v.multiret.ros[0] ELSE DREG
      rd := IF v.multiret THEN v.multiret.rds[0] ELSE IREG0

   ENDIF

   arx := self.copyObtainToAREG(v.o,v.d) -> v49
   jsraxp(arx)
   self.releaseAREG(arx,0) -> v49
   inst_incarx(STACKREG, count*4)

   ro, rd := self.secureReturn(ro, rd, regusecopy)

   self.loadObtained(regusecopy)
   clearRegs()


   g_multireturn := IF v.multiret THEN v.multiret ELSE [ro,DREG,DREG,DREG,rd,1,2,3]:LONG

   #ifdef DBG_M68GEN
   DEBUGF('dovfunction() \s DONE\n', v.hln.name)
   #endif

ENDPROC r, ro, rd



-> v50: added support for doubleargs
PROC doIntfunc(n:PTR TO item) OF m68amiga
   DEF buf:PTR TO item, r:PTR TO raise, o, d, a, t
   DEF numparams, defs:PTR TO LONG
   DEF ifunc:PTR TO lif, regusecopy[REGUSETABSIZE]:ARRAY OF oreg
   DEF hln:PTR TO hln, varargsofs:PTR TO LONG, var:PTR TO var
   DEF noraise:PTR TO genlab, numvarargs
   DEF o2,d2
   DEF ifNumArgs
   DEF it_funccall:PTR TO it_funccall
   DEF ro, rd , fpx=1
   DEF ha:hintargs

   ha.hregisvar := FALSE

   buf := n
   it_funccall := n.info
   numparams := it_funccall.numpars
   ifunc := it_funccall.data
   n := it_funccall + SIZEOF it_funccall

   ifNumArgs := ListLen(ifunc.params)

   #ifdef DBG_M68GEN
   DEBUGF('doifunction() \s \n', ifunc.name)
   #endif

   self.saveObtained(regusecopy)

   defs := ifunc.defaults

   SELECT 5 OF ifunc.returns
   CASE 0
      ro := DRX
      rd := IREG0
   CASE 1
      ro := FPX
      rd := FREG0
   CASE 2
      ro := X2R
      rd := 1
   DEFAULT
      reportIErr('m68.doIntFunc ifunc.returns ?')
   ENDSELECT


   FOR a := 0 TO ifNumArgs-1
      IF n.data
         t := ListItem(ifunc.params,a)
         IF t = 1 -> doublearg ?
            ha.hregop := FREG
            ha.hregnum := fpx
            n, o, d := self.doExpression(n, ha)
            inst_copy(o,d, FPX,fpx)
            self.copyObtainToFREG(FPX,fpx)
            fpx++
         ELSEIF t = 2 -> wide
            n, o, d := self.doExpression(n)
            self.tox2r(o, d, FTEMP)
            inst_push(8, FREG, FTEMP)
         ELSE
            n, o, d := self.doExpression(n)
            inst_push(4,o,d)
         ENDIF
      ELSE  /* default value used */
         t := ListItem(ifunc.params,a)
         IF t = 1 -> doublearg ?
            inst_copy(DV,defs[]++, FPX,fpx)
            self.copyObtainToFREG(FPX,fpx)
            fpx++
         ELSEIF t = 2 -> wide
            self.tox2r(DV, defs[]++, FTEMP)
            inst_push(8, FREG, FTEMP)
         ELSE
            inst_push(4,DV,defs[]++)
         ENDIF
      ENDIF
   ENDFOR

   -> fixing to support 64bit varargs
   IF ifunc.flags = 2 -> varargs ?
      numvarargs := numparams - ifNumArgs
      inst_decarx(7, numvarargs * 8) -> alloc enough for wide arguments!
      -> push varargs
      varargsofs := 0
      WHILE n.data
         n, o, d := self.doExpression(n)
         SELECT 20 OF o
         CASE VAR64, X2R, FPX, D64
            inst_copy(o,d, ARXPO,AxSizeOfs(7,8,varargsofs++))
            varargsofs++
         DEFAULT
            inst_copy(o,d, ARXPO,AxSizeOfs(7,4,varargsofs++))
         ENDSELECT
      ENDWHILE
      inst_push(4, DV,numvarargs*8)
      inst_goslab(ifunc)
      inst_incarx(7, ifNumArgs+1*4+(numvarargs*8))
   ELSE
      WHILE fpx > 1
         fpx--
         self.releaseFREG(fpx,0)
      ENDWHILE
      inst_goslab(ifunc)
      inst_incarx(7, ifNumArgs*4)
   ENDIF

   ro, rd := self.secureReturn(ro, rd, regusecopy) -> v50

   self.loadObtained(regusecopy)

   clearRegs()

   IF ifunc.raise
      noraise := newLabel()
      inst_bic(DV,ifunc.raise.trigval,ifunc.raise.condition, ro,rd,noraise)
      inst_push(4,DV,ifunc.raise.excval)
      bsrlab(self.raise_lif)->g_internalfuncs[IF68K_RAISE]) -> Raise
      def_label(noraise)
   ENDIF

   g_multireturn := [ro,DREG,DREG,DREG,rd,1,2,3]:LONG

   #ifdef DBG_M68GEN
   DEBUGF('doiunction() \s DONE\n', ifunc.name)
   #endif

ENDPROC buf[1], ro, rd




PROC doLibfunc(n:PTR TO item) OF m68amiga
   DEF buf:PTR TO item, a, o, d, libbase:PTR TO var, r:PTR TO raise
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, lfunc:PTR TO lfunc, hln:PTR TO hln
   DEF numparams, t, var:PTR TO var
   DEF o2, d2, t2, noraise:PTR TO genlab
   DEF it_funccall:PTR TO it_funccall
   DEF ro, rd

   buf := n
   it_funccall := n.info
   numparams := it_funccall.numpars
   lfunc := it_funccall.data
   libbase := it_funccall.info
   n := it_funccall + SIZEOF it_funccall

   IF numparams <> lfunc.nrofargs THEN reportErr('wrong nr of parameters for function', lfunc.name)

   #ifdef DBG_M68GEN
   DEBUGF('dolfunction() \s \n', lfunc.name)
   #endif

   IF lfunc.type = 1 THEN reportErr('cannot call sysv from 68k (duh)')

   ro := DREG
   rd := IREG0

   self.saveObtained(regusecopy)


   /* save regs ? */
   FOR a := numparams-1 TO 0 STEP -1 -> reverse order !
      t := lfunc.regs[a].rnum
      IF lfunc.regs[a].rtype
         SELECT 8 OF t
         CASE 4, 5      ; moveaxaxpd(SIZE_L,t,7) -> a4,a5
         ENDSELECT
      ELSE
         SELECT 16 OF t
         CASE 4,5,6,7   ; movedxaxpd(SIZE_L,t,7) -> d4-d7
         ENDSELECT
      ENDIF
   ENDFOR

   /* put on stack */
   FOR a := 0 TO numparams-1
      n, o, d := self.doExpression(n)
      inst_push(4,o,d)
   ENDFOR

   /* set librarybase */
   inst_copy(libbase.o,libbase.d, ARX,6)

   /* set up parameters (by copying to regs) */
   FOR a := numparams-1 TO 0 STEP -1
      t2 := lfunc.regs[a]
      o2 := IF lfunc.regs[a].rtype THEN ARX ELSE DRX
      d2 := lfunc.regs[a].rnum
      inst_pop(4,o2,d2)
   ENDFOR

   jsraxpofs(6,lfunc.baseofs)
   /* restore regs ? */
   FOR a := 0 TO numparams-1 -> non-reversed..
      t := lfunc.regs[a].rnum
      IF lfunc.regs[a].rtype
         SELECT 16 OF t
         CASE 4,5       ; moveaxpiax(SIZE_L,7,t) -> a4,a5
         ENDSELECT
      ELSE
         SELECT 16 OF t
         CASE 4,5,6,7   ; moveaxpidx(SIZE_L,7,t)  -> d4-d7
         ENDSELECT
      ENDIF
   ENDFOR

   ro, rd := self.secureReturn(ro, rd, regusecopy)

   self.loadObtained(regusecopy)
   clearRegs()

   IF lfunc.raise
      noraise := newLabel()
      inst_bic(DV,lfunc.raise.trigval,lfunc.raise.condition, ro,rd,noraise)
      inst_push(4,DV,lfunc.raise.excval)
      bsrlab(self.raise_lif)->g_internalfuncs[IF68K_RAISE]) -> Raise
      def_label(noraise)
   ENDIF

   g_multireturn := [DREG,DREG,NIL,NIL,0,1,0,0]:LONG


   #ifdef DBG_M68GEN
   DEBUGF('dolfunction() \s DONE\n', lfunc.name)
   #endif

ENDPROC buf[1], ro, rd


-> 2.3 added wide support
PROC doReturn(buf, ret) OF m68amiga
   DEF n:PTR TO item
   DEF o,d, x, t
   DEF a=0, t1:PTR TO CHAR, t2:PTR TO CHAR
   DEF m:PTR TO multireturn
   DEF numrets
   DEF ha:hintargs

   ha.hregisvar := FALSE

   m := g_currentproc.mret

   #ifdef DBG_M68GEN
   DEBUGF('doreturn($\h,\d)\n', buf, ret)
   #endif

   n := buf
   numrets := n.num

   IF numrets > 3 THEN reportErr('too many returnvalues')

   n++

   self.endLocalClasses(g_currentproc)

   FOR a := 0 TO numrets-1
      ha.hregop := m.ros[a]
      ha.hregnum := m.rds[a]
      n, o, d := self.doExpression(n, ha)
      IF m.ros[a] = X2R
         self.tox2r(o,d,m.rds[a])
      ELSE
         inst_copy(o,d, m.ros[a],m.rds[a])
      ENDIF
      IF m.ros[a] = DREG
         self.copyObtainToDREG(DREG,m.rds[a]) -> obtain register
      ELSEIF (m.ros[a] = FREG) OR (m.ros[a] = X2R)
         self.copyObtainToFREG(FREG,m.rds[a]) -> obtain register
      ENDIF
   ENDFOR

   FOR a := 0 TO numrets-1
      IF m.ros[a] = DREG
         self.releaseDREG(m.rds[a],0)
      ELSEIF (m.ros[a] = FREG) OR (m.ros[a] = X2R)
         self.releaseFREG(m.rds[a],0)
      ENDIF
   ENDFOR

   IF numrets = 0 THEN inst_copy(DV,NIL, DREG,IREG0)

   IF ret THEN inst_ret(0)

   #ifdef DBG_M68GEN
   DEBUGF('doreturn DONE()\n')
   #endif

ENDPROC n, NIL, NIL



-> v50 (1.5.1)
-> v55: arg: neg=>unary
-> v58, changes
PROC mathlogichead(n:PTR TO item, mode, unary, ha_:PTR TO hintargs) OF m68amiga
   DEF yreg, regusecopy[REGUSETABSIZE]:ARRAY OF oreg, ro, rd
   DEF o,d, ha:hintargs

   -> 2.3
   SELECT mode
   CASE MODE_DEFAULT
      self.saveObtained(regusecopy) -> 1.8.0
      yreg := self.obtainDREG()
      ha.hregop := DREG
      ha.hregnum := yreg
      ha.hregisvar := FALSE
   CASE MODE_FLOAT
      yreg := self.obtainFREG()
      ha.hregop := FREG
      ha.hregnum := yreg
      ha.hregisvar := FALSE
   CASE MODE_D64
      yreg := self.obtainD64()
      ha.hregop := D64REG
      ha.hregnum := yreg
      ha.hregisvar := FALSE
   DEFAULT
      reportIErr('mathlogichead mode')
   ENDSELECT

   n, o, d := self.doSingleExp(n, ha)

   IF mode = MODE_FLOAT

      ->yreg := self.obtainFREG() -> we need a write mode obtain
      inst_copy(o,d, FREG,yreg)
      IF unary = "_"
         inst_fnegreg(yreg)
      ELSEIF unary = KW_ABS -> 2.2
         inst_fabsreg(yreg)
      ENDIF
      n, ro, rd := self.domathlogictail(n, MODE_FLOAT, yreg)
   ELSEIF mode = MODE_DEFAULT
      ->yreg := self.obtainDREG() -> we need a write mode obtain
      inst_copy(o,d, DREG,yreg)
      IF unary = "-"
         inst_negreg(yreg)
      ELSEIF unary = "~"
         inst_notreg(yreg)
      ELSEIF unary = KW_ABS -> 2.2
         inst_absreg(yreg)
      ENDIF
      n, ro, rd := self.domathlogictail(n, MODE_DEFAULT, yreg)
   ELSEIF mode = MODE_D64 -> 1.10.0
      ->yreg := self.obtainD64() -> we need a write mode obtain
      inst_copy(o,d, D64REG,yreg)
      IF unary = "-"
         inst_negd64(yreg)
      ELSEIF unary = "~"
         inst_notd64(yreg)
      ELSEIF unary = KW_ABS  -> 2.2
         inst_absd64(yreg)
      ENDIF
      n, ro, rd := self.domathlogictail(n, MODE_D64, yreg)
   ENDIF

   IF mode = MODE_DEFAULT

      -> 1.8.0
      ro, rd := self.secureReturn(ro, rd, regusecopy)

      self.loadObtained(regusecopy)

      clearRegs()

   ENDIF

ENDPROC n, ro, rd

-> 1.8.1
PROC m68RegisterizeD(o,d:PTR TO var) OF m68amiga
   IF o = DREG
      RETURN d
   ELSEIF o = VAR
      IF d.intreg AND d.trok AND (d.treg<8) THEN RETURN d.treg
   ENDIF
   inst_copy(o,d, DREG,DTEMP)
ENDPROC DTEMP

-> 1.8.1
PROC m68RegisterizeA(o,d:PTR TO var) OF m68amiga
   IF o = AREG
      RETURN d
   ELSEIF o = VAR
      IF d.intreg AND d.trok AND (d.treg>7) THEN RETURN d.treg-8
   ENDIF
   inst_copy(o,d, AREG,ATEMP)
ENDPROC ATEMP

-> 1.8.1
PROC m68RegisterizeF(o,d:PTR TO var) OF m68amiga
   IF o = FREG
      RETURN d
   ELSEIF o = VAR64
      IF d.intreg AND d.trok THEN RETURN d.treg
   ENDIF
   inst_copy(o,d, FREG,FTEMP)
ENDPROC FTEMP


/*

0 Dx
1 Ax
2 (Ax)
3 (Ax)+
4 -(Ax)
5 ofs(Ax)              arg   local   .memb(Ax:obj) LibFunc(Ax)
6 lab(Ax)              global
7 (Ax,Dx.L*Scale)      (Ax,Dx.L) (Ax,Dx)
8 #imm                 #constexp
9 lab(PC)              lab   proc
*/

->-- 68k single operand parser ---

PROC getop68(n:PTR TO item, values:PTR TO LONG)
   DEF t, ax, dx, scale=1, ri, as,as2, ofs, node:PTR TO var
   DEF id, memb:PTR TO member, hln:PTR TO hln, proc:PTR TO proc
   DEF var:PTR TO var, reg:PTR TO reg, lab:PTR TO codelab

   t := n.data
   SELECT 256 OF t
   CASE "-" -> -(Ax)
      n++
      n++
      reg := n.info
      RETURN n[2], 4, ListAdd(values, [reg.num])
   CASE IT_VALUE -> ofs(Ax), ofs(Ax,Dx), ofs(Ax,Dx*scale)
      ofs := n.info
      n++
      n++ -> skip "("
      reg := n.info
      ax := reg.num
      n++
      IF n.data <> IT_REG
         RETURN n[1], 5, ListAdd(values, [ax, ofs])
      ELSE
         -> indexed
         reg := n.info
         dx := reg.num
         IF n.num
            IF n.num <> "L" THEN reportErr('wrong size of value')
         ENDIF
         n++
         IF n.data = IT_VALUE
            scale := n.info
            n++
         ELSE
            scale := 1
         ENDIF
         n++ -> skip ")"
         RETURN n, 7, ListAdd(values, [ax, dx, scale, ofs])
      ENDIF
   CASE "("
      n++
      reg := n.info
      n++
      ax := reg.num
      -> (Ax), (Ax)+
      n++
      IF n.data = "+" -> (Ax)+
         RETURN n[1], 3, ListAdd(values, [ax])
      ELSE
         RETURN n, 2, ListAdd(values, [ax])
      ENDIF
   CASE "#"
      n++
      RETURN n[1], 8, ListAdd(values, [n.info])
   CASE IT_VARIABLE
      var := n.info
      IF var.o = DRX -> 2.3, was forgotten
         RETURN n[1], 0, ListAdd(values, [var.d])
      ELSE
         IF var.link
            RETURN n[1], 6, ListAdd(values, [var.breg,var])
         ELSE
            IF n.num
               ofs := 4-n.num
            ELSE
               ofs := 0
            ENDIF
            n++
            RETURN n, 5, ListAdd(values, [var.breg,var.offset+ofs])
         ENDIF
      ENDIF
   CASE IT_LABEL
      hln := n.info
      lab := hln.ident
      n++
      IF lab = NIL THEN reportErr('unknown identifier', hln.name)
      lab.referenced := 1
      RETURN n, 9, ListAdd(values, [lab])
   CASE IT_REG
      reg := n.info
      n++
      RETURN n, IF reg.type = DRX THEN 0 ELSE 1, ListAdd(values, [reg.num])
   DEFAULT
      RETURN NIL
   ENDSELECT
ENDPROC

PROC doAsm(n:PTR TO item) OF m68amiga
   DEF t, a
   DEF values[20]:LIST
   DEF asm:PTR TO asm
   DEF inst:PTR TO inst68 , lptr:PTR TO LONG

   #ifdef DBG_M68GEN
   DEBUGF('m68amiga.doAsm($\h).. ', n)
   #endif

   #ifdef DBG_M68GEN
   DEBUGF('$\h $\h $\h\n', values[0], values[1], values[2])
   #endif

   asm := n.info
   inst := asm.data
   t := n.num

   #ifdef DBG_M68GEN
   DEBUGF('"\s" ops=\d.. ', asm.name, inst.nrofops)
   #endif

   /* instruction */
   n++

   /* size */
   IF t
      SELECT t
      CASE "L"  ; lptr := ListItem(inst,1)  -> .L
      CASE "W"  ; lptr := ListItem(inst,2)  -> .W
      CASE "B"  ; lptr := ListItem(inst,3) -> .B
      CASE "S"  ; lptr := ListItem(inst,4) -> .S
      CASE "D"  ; lptr := ListItem(inst,5) -> .D
      DEFAULT ; reportIErr(' illegal size')
      ENDSELECT
   ELSE
      lptr := ListItem(inst,inst.defasize+1) -> .L
   ENDIF

   IF lptr = NIL THEN reportErr('illegal size for instruction', asm.name)

   FOR a := 1 TO inst.nrofops
      IF n.data <> 10
         #ifdef DBG_M68GEN
         DEBUGF('op\d ', a)
         #endif
         n, t := getop68(n, values)
         IF n = NIL THEN reportErr('operand syntax')
         IF t >= ListLen(lptr) THEN reportErr('illegal operand')
         lptr := lptr[t]
         IF lptr = NIL THEN reportErr('illegal operand')
      ELSE
         reportErr('operand expected')
      ENDIF
      IF a < inst.nrofops
         IF n.data <> "," THEN reportErr('"," expected')
         n++
      ENDIF
   ENDFOR

   #ifdef DBG_M68GEN
   DEBUGF('$\h $\h $\h\n', values[0], values[1], values[2])
   #endif

   g_od := values
   testEval(lptr)

   #ifdef DBG_M68GEN
   DEBUGF('m68amiga.doAsm() DONE\n')
   #endif

ENDPROC n

-> 2.0.0 workaround for now
PROC testEval(code)
   DEF array[256]:ARRAY
ENDPROC Eval(code)


/* expects size as "sizeas" */
-> v50, killed two args, now returns register instead (DREG)
PROC newMemory(sizeas,sizeas2) OF m68amiga
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
   DEF ro, rd
   ro := DREG
   rd := IREG0
   self.saveObtained(regusecopy)
   inst_push(4,sizeas,sizeas2)
   inst_goslab(self.fastnew_lif)
   inst_incarx(7,4)
   ro, rd := self.secureReturn(ro, rd, regusecopy)
   self.loadObtained(regusecopy)
   clearRegs()
ENDPROC rd



PROC endMemory(sizeas,sizeas2, as,as2) OF m68amiga
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
   self.saveObtained(regusecopy)
   inst_push(4,as,as2)
   inst_push(4,sizeas,sizeas2)
   inst_goslab(self.fastdispose_lif)
   inst_incarx(7,2*4)
   self.loadObtained(regusecopy)
   clearRegs()
   inst_copy(DV,NIL,as,as2)
ENDPROC



-> v50: killed two args, now returns register instead (AREG)
PROC newClassObject(obj:PTR TO object) OF m68amiga
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
   DEF ro, rd
   self.saveObtained(regusecopy)
   inst_push(4,DV,obj.sizeof+IF obj.flags AND OFLAG_NOCLASSINFO THEN 0 ELSE 4)
   inst_goslab(self.fastnew_lif)
   inst_incarx(7,1*4)
   inst_copy(DREG,IREG0, AREG,ATEMP)
   IF (obj.flags AND OFLAG_NOCLASSINFO) = FALSE
      inst_labadr(obj, SELFREG)
      inst_copy(AREG,SELFREG,ARXPO,AxSizeOfs(ATEMP,4,0))
      inst_incarx(ATEMP,4)
   ENDIF
   self.loadObtained(regusecopy)
   clearRegs()
ENDPROC ATEMP


PROC endClassObject(obj:PTR TO object, varo, vard) OF m68amiga
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object, meth:PTR TO proc
   DEF nil
   nil := newLabel()
   inst_bic(varo,vard,ISEQ,DV,NIL,nil)
   IF obj.destofs > -1  -> call .end() ?
      self.saveObtained(regusecopy)
      meth := obj.methodtable[Shr(obj.destofs,2)]
      inst_copy(varo,vard, AREG, SELFREG)
      IF meth.flags AND PROCF_CLMETH -> 1.6.1
         IF obj.offset
            inst_goslab(meth)
         ELSE -> module -> module
            inst_labadr(obj, ATEMP)
            inst_copy(ARXPO,AxSizeOfs(ATEMP,4,obj.destofs), AREG, ATEMP)
            inst_gosarx(ATEMP)
         ENDIF
      ELSE
         inst_copy(ARXPO,AxSizeOfs(SELFREG,4,OBJECT_CLASSINFO), AREG,ATEMP)
         inst_copy(ARXPO,AxSizeOfs(ATEMP,4,obj.destofs), AREG, ATEMP)
         inst_gosarx(ATEMP)
      ENDIF
      self.loadObtained(regusecopy)
      clearRegs()
   ENDIF
   inst_copy(varo,vard,DREG, IREG0)
   self.saveObtained(regusecopy)
   IF (obj.flags AND OFLAG_NOCLASSINFO) = FALSE
      inst_copy(DREG,IREG0,AREG,ATEMP)
      inst_copy(ARXPO,AxSizeOfs(ATEMP,4,OBJECT_CLASSINFO), AREG,ATEMP)
      inst_copy(ARXPO,AxSizeOfs(ATEMP,4,CLASSINFO_SIZE), DREG, IREG1)
      inst_addreg(DV,4, IREG1)
      inst_subreg(DV,4, IREG0)
   ELSE
      inst_copy(DV,obj.sizeof, DREG,IREG1)
   ENDIF
   inst_push(4,DREG, IREG0)
   inst_push(4,DREG,IREG1)
   inst_goslab(self.fastdispose_lif)
   inst_incarx(7,2*4)
   self.loadObtained(regusecopy)
   clearRegs()
   inst_copy(DV,NIL, varo,vard)
   def_label(nil)
ENDPROC



PROC nilCheck(areg,line) OF m68amiga
   cmplimmax(0,areg)    -> 2.0 fix
   bccofs8(NE,18)
   putMOVEI(SIZE_L,7,M4,"NIL")
   putMOVEI(SIZE_L,7,M4,line)
   bsrlab(self.throw_lif, TRUE) -> 2.0 fix
ENDPROC



PROC getFreeDREG() OF m68amiga
   DEF r, a
   DEF var:PTR TO var, array[3]:ARRAY OF CHAR

    #ifdef DBG_M68GEN
   DEBUGF('getFreeDREG():')
   #endif


   -> v50
   FOR a := 0 TO 2 DO array[a] := NIL

   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF var.treg < 3 THEN array[var.treg] := 1
      ENDIF
      var := var.next
   ENDWHILE

IF g_currentproc

   var := g_currentproc.args32
   WHILE var
      IF var.intreg
         IF var.treg < 3 THEN array[var.treg] := 1
      ENDIF
      var := var.next
   ENDWHILE
   var := g_currentproc.locals32
   WHILE var
      IF var.intreg
         IF var.treg < 3 THEN array[var.treg] := 1
      ENDIF
      var := var.next
   ENDWHILE

ENDIF

   -> obtain next free reg that is not caching anything
   r := self.lasttempDREG + 1
   FOR a := 1 TO 3
      IF r > 2 THEN r := 0
      IF g_regusetab[r].obtains = NIL
         IF array[r] = NIL
            #ifdef DBG_M68GEN
            DEBUGF('\d\n', r)
            #endif
            RETURN r
         ENDIF
      ENDIF
      r++
   ENDFOR

   -> ok, just obtain next free reg
   r := self.lasttempDREG + 1
   FOR a := 1 TO 3
      IF r > 2 THEN r := 0
      IF g_regusetab[r].obtains = NIL
         #ifdef DBG_M68GEN
         DEBUGF('\d\n', r)
         #endif
         RETURN r
      ENDIF
      r++
   ENDFOR

   -> ok lets settle for DTEMP then

ENDPROC DTEMP

PROC getFreeAREG() OF m68amiga
   DEF r, a
   DEF var:PTR TO var, array[4]:ARRAY OF CHAR

   #ifdef DBG_M68GEN
   DEBUGF('getFreeAREG():')
   #endif


   -> v50
   FOR a := 0 TO 3 DO array[a] := NIL

   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF (var.treg > 7) AND (var.treg < 11) THEN array[var.treg-8] := 1
      ENDIF
      var := var.next
   ENDWHILE

IF g_currentproc

   var := g_currentproc.args32
   WHILE var
      IF var.intreg
         IF (var.treg > 7) AND (var.treg < 11) THEN array[var.treg-8] := 1
      ENDIF
      var := var.next
   ENDWHILE
   var := g_currentproc.locals32
   WHILE var
      IF var.intreg
         IF (var.treg > 7) AND (var.treg < 11) THEN array[var.treg-8] := 1
      ENDIF
      var := var.next
   ENDWHILE

ENDIF

   -> obtain next free reg not caching anything
   r := self.lasttempAREG + 1
   FOR a := 1 TO 3
      IF r > 3 THEN r := 0
      IF array[r] = NIL
         IF g_regusetab[r+8].obtains = NIL -> not in use ?
            #ifdef DBG_M68GEN
            DEBUGF('\d\n', r)
            #endif
            RETURN r, FALSE
         ENDIF
      ENDIF
      r++
   ENDFOR


   -> obtain next free reg
   r := self.lasttempAREG + 1
   FOR a := 1 TO 3
      IF r > 3 THEN r := 0
      IF g_regusetab[r+8].obtains = NIL -> not in use ?
         #ifdef DBG_M68GEN
         DEBUGF('\d\n', r)
         #endif
         RETURN r, FALSE
      ENDIF
      r++
   ENDFOR

   -> ok, lets settle for ATEMP then

ENDPROC ATEMP

PROC getFreeFREG() OF m68amiga
   DEF r, a
   DEF var:PTR TO var, array[8]:ARRAY OF CHAR

   #ifdef DBG_M68GEN
   DEBUGF('getFreeFREG(): ')
   #endif

    -> v50
   FOR a := 1 TO 7 DO array[a] := NIL

   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF var.type.size = 8
            IF (var.treg < 8) THEN array[var.treg] := 1
         ENDIF
      ENDIF
      var := var.next
   ENDWHILE

IF g_currentproc

   var := g_currentproc.args64
   WHILE var
      IF var.intreg
         IF (var.treg < 8) THEN array[var.treg] := 1
      ENDIF
      var := var.next
   ENDWHILE
   var := g_currentproc.locals64
   WHILE var
      IF var.intreg
         IF (var.treg < 8) THEN array[var.treg] := 1
      ENDIF
      var := var.next
   ENDWHILE

ENDIF

    -> obtain next free non caching reg
   r := self.lasttempFREG + 1
   FOR a := 1 TO 7
      IF r > 7 THEN r := 1
      IF array[r] = NIL
         IF g_regusetab[r+32].obtains = NIL -> not in use ?
            RETURN r
         ENDIF
      ENDIF
      r++
   ENDFOR


   -> else just obtain next free reg
   r := self.lasttempFREG + 1
   FOR a := 1 TO 7
      IF r > 7 THEN r := 1
      IF g_regusetab[r+32].obtains = NIL -> not in use ?
         RETURN r
      ENDIF
      r++
   ENDFOR


   /* settle for FTEMP */

ENDPROC FTEMP


-> 1.5.3
PROC lockDREG(dreg) OF m68amiga
   DEF r=FALSE
   #ifdef DBG_M68GEN
   DEBUGF('lockDREG(\d)\n', dreg)
   #endif


   IF g_regusetab[dreg].obtains
      movedxaxpd(SIZE_L,dreg,7)
      #ifdef DBG_M68GEN
      DEBUGF('..spilled it (use was=\d)\n', g_regusetab[dreg].obtains)
      #endif
      r := TRUE
   ENDIF

   ->IF g_regusetab[dreg].obtains THEN reportIErr('lockDREG m68amiga')
   g_regusetab[dreg].obtains := g_regusetab[dreg].obtains + 1
   g_regusetab[dreg].write := g_regusetab[dreg].write + 1
   self.lasttempDREG := dreg
ENDPROC r

PROC lockAREG(areg) OF m68amiga
   DEF r=FALSE
   #ifdef DBG_M68GEN
   DEBUGF('lockAREG(\d)\n', areg)
   #endif


   IF g_regusetab[areg+8].obtains
      moveaxaxpd(SIZE_L,areg,7)
      #ifdef DBG_M68GEN
      DEBUGF('..spilled it (use was=\d)\n', g_regusetab[areg].obtains)
      #endif
      r := TRUE
   ENDIF

   ->IF g_regusetab[areg+8].obtains THEN reportIErr('lockAREG m68amiga')
   g_regusetab[areg+8].obtains := g_regusetab[areg+8].obtains + 1
   g_regusetab[areg+8].write := g_regusetab[areg+8].write + 1
   self.lasttempAREG := areg
ENDPROC r

PROC lockFREG(freg) OF m68amiga
   DEF r=FALSE
   #ifdef DBG_M68GEN
   DEBUGF('lockFREG(\d)\n', freg)
   #endif

   IF g_regusetab[freg+32].obtains
      movedxaxpd(SIZE_L,freg,7)
      #ifdef DBG_M68GEN
      DEBUGF('..spilled it (use was=\d)\n', g_regusetab[freg+32].obtains)
      #endif
      r := TRUE
   ENDIF
   ->IF g_regusetab[freg+32].obtains THEN reportIErr('lockFREG m68amiga')
   g_regusetab[freg+32].obtains := g_regusetab[freg+32].obtains + 1
   g_regusetab[freg+32].write := g_regusetab[freg+32].write + 1
   self.lasttempFREG := freg
ENDPROC r

-> v50
PROC resetObtainStart() OF m68amiga
   self.lasttempAREG := 3
   self.lasttempDREG := 2
   self.lasttempFREG := 7
ENDPROC


PROC obtainDREG() OF m68amiga -> returns 0-2
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('m68ObtainDREG():')
   #endif


   r := self.getFreeDREG()
   IF r = DTEMP
       -> 1.10.0 obtain already in writemode register fix
      r := self.lasttempDREG
      FOR t := 0 TO 2
         r++
         IF r > 2 THEN r := 0
         IF g_regusetab[r].write THEN t := 3
      ENDFOR
   ENDIF

   s := self.lockDREG(r)

ENDPROC r, s

PROC releaseDREG(r,retbool) OF m68amiga
   #ifdef DBG_M68GEN
   DEBUGF('m68ReleaseDREG(\d)\n', r)
   #endif


   IF g_regusetab[r].obtains > 1
      IF g_regusetab[r].write
         IF retbool THEN movedxdx(SIZE_L,r,3)
         moveaxpidx(SIZE_L,7,r) ; m68SetDReg(r, NIL,NIL)
         g_regusetab[r].obtains := g_regusetab[r].obtains - 1
         g_regusetab[r].write := g_regusetab[r].write - 1
         IF retbool THEN RETURN 3
      ELSE
         g_regusetab[r].obtains := g_regusetab[r].obtains - 1
      ENDIF
   ELSEIF g_regusetab[r].obtains = 1
      g_regusetab[r].obtains := 0
      g_regusetab[r].write := FALSE
   ELSEIF g_regusetab[r].obtains = 0
      IF r < 4 THEN reportIErr(' releaseDREG() use=0 r < 4')
      #ifdef DBG_M68GEN
      DEBUGF('m68ReleaseDREG(\d ALLOCATED)\n', r)
      #endif
   ENDIF

ENDPROC r

PROC obtainAREG() OF m68amiga -> returns 0/1/2/3, spillbool
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('m68ObtainAREG()')
   #endif


   r := self.getFreeAREG()
   IF r = ATEMP
       -> 1.10.0 obtain already in writemode register fix
      r := self.lasttempAREG
      FOR t := 0 TO 3
         r++
         IF r > 3 THEN r := 0
         IF g_regusetab[r+8].write THEN t := 4
      ENDFOR
   ENDIF

   s := self.lockAREG(r)

ENDPROC r, s

PROC releaseAREG(r,retbool) OF m68amiga
   #ifdef DBG_M68GEN
   DEBUGF('m68ReleaseAREG(\d)\n', r)
   #endif


   IF g_regusetab[r+8].obtains > 1
      IF g_regusetab[r+8].write
         IF retbool THEN moveaxax(SIZE_L,r,6) BUT m68SetAReg(6,NIL,NIL)
         moveaxpiax(SIZE_L,7,r) ; m68SetAReg(r,NIL,NIL)
         g_regusetab[r+8].obtains := g_regusetab[r+8].obtains - 1
         g_regusetab[r+8].write := g_regusetab[r+8].write - 1
         IF retbool THEN RETURN 6
      ELSE
         g_regusetab[r+8].obtains := g_regusetab[r+8].obtains - 1
      ENDIF
   ELSEIF g_regusetab[r+8].obtains = 1
      g_regusetab[r+8].obtains := 0
      g_regusetab[r+8].write := FALSE
   ELSE
      reportIErr(' 68K, releaseAREG, use < 1')
   ENDIF
ENDPROC r

PROC obtainFREG() OF m68amiga -> returns 1-7
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('m68ObtainFREG()\n')
   #endif

   r := self.getFreeFREG()
   IF r = FTEMP
      -> 1.10.0 obtain already in writemode register fix
      r := self.lasttempFREG
      FOR t := 1 TO 7
         r++
         IF r > 7 THEN r := 1
         IF g_regusetab[r+32].write THEN t := 8
      ENDFOR
   ENDIF

   s := self.lockFREG(r)

ENDPROC r, s

PROC releaseFREG(r,retbool) OF m68amiga
   #ifdef DBG_M68GEN
   DEBUGF('m68ReleaseFREG(\d)\n', r)
   #endif
   IF r = 0 THEN reportIErr('68k releaseFREG() r=0!')
   IF g_regusetab[r+32].obtains > 1
      IF g_regusetab[r+32].write
         IF retbool THEN fmovefpxfpx(r,0) BUT m68SetFReg(0,NIL,NIL)
         fmoveaxpifpx(FD,7,r) ; m68SetFReg(r,NIL,NIL)
         g_regusetab[r+32].obtains := g_regusetab[r+32].obtains - 1
         g_regusetab[r+32].write := g_regusetab[r+32].write - 1
         IF retbool THEN RETURN 0
      ELSE
         g_regusetab[r+32].obtains := g_regusetab[r+32].obtains - 1
      ENDIF
   ELSEIF g_regusetab[r+32].obtains = 1
      g_regusetab[r+32].obtains := 0
      g_regusetab[r+32].write := FALSE
   ELSEIF g_regusetab[r+32].obtains = 0
      reportIErr('releaseFREG() use = 0')
      #ifdef DBG_M68GEN
      DEBUGF('m68ReleaseFREG(\d ALLOCATED)\n', r)
      #endif
   ENDIF
ENDPROC r

PROC saveObtained(regusecopy:PTR TO oreg) OF m68amiga
   DEF r
   #ifdef DBG_M68GEN
   DEBUGF('m68SaveObtainRegs()\n')
   #endif
   CopyMem(g_regusetab, regusecopy, REGUSETABSIZE * SIZEOF oreg)
   FOR r := 0 TO 2 -> d0-d2
      IF g_regusetab[r].obtains
         movedxaxpd(SIZE_L,r,7)
         #ifdef DBG_M68GEN
         DEBUGF('   \d saved, use=\d\n', r, g_regusetab[r].obtains)
         #endif
         g_regusetab[r].obtains := 0
         g_regusetab[r].write := FALSE
      ENDIF
   ENDFOR
   FOR r := 8 TO 11 -> a0-a3
      IF g_regusetab[r].obtains
         moveaxaxpd(SIZE_L,r-8,7)
         #ifdef DBG_M68GEN
         DEBUGF('   \d saved, use=\d\n', r, g_regusetab[r].obtains)
         #endif
         g_regusetab[r].obtains := 0
         g_regusetab[r].write := FALSE
      ENDIF
   ENDFOR
   FOR r := 33 TO 39 -> fp1-fp7
      IF g_regusetab[r].obtains
         fmovefpxaxpd(FD,r-32,7)
         #ifdef DBG_M68GEN
         DEBUGF('   \d saved, use=\d\n', r, g_regusetab[r].obtains)
         #endif
         g_regusetab[r].obtains := 0
         g_regusetab[r].write := FALSE
      ENDIF
   ENDFOR
ENDPROC

PROC loadObtained(regusecopy:PTR TO oreg) OF m68amiga
   DEF r
   #ifdef DBG_M68GEN
   DEBUGF('m68LoadObtainedRegs()\n')
   #endif


   FOR r := 39 TO 33 STEP -1 -> fp1 - fp7
      IF regusecopy[r].obtains
         fmoveaxpifpx(FD,7,r-32)
         #ifdef DBG_M68GEN
         DEBUGF('   \d loaded, use=\d\n', r, regusecopy[r].obtains)
         #endif
      ENDIF
   ENDFOR
   FOR r := 11 TO 8 STEP -1 -> a3-a0
      IF regusecopy[r].obtains
         moveaxpiax(SIZE_L,7,r-8)
         #ifdef DBG_M68GEN
         DEBUGF('   \d loaded, use=\d\n', r, regusecopy[r].obtains)
         #endif
      ENDIF
   ENDFOR
   FOR r := 2 TO 0 STEP -1 -> d2-d0
      IF regusecopy[r].obtains
         moveaxpidx(SIZE_L,7,r)
         #ifdef DBG_M68GEN
         DEBUGF('   \d loaded, use=\d\n', r, regusecopy[r].obtains)
         #endif
      ENDIF
   ENDFOR
   CopyMem(regusecopy, g_regusetab, REGUSETABSIZE * SIZEOF oreg)
ENDPROC

PROC copyObtainToDREG(o,d:PTR TO var) OF m68amiga
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('copyObtainToDREG o=\d d=\d\n', o,d)
   #endif

   IF o = VAR
      IF d.trok AND d.intreg
         IF (d.treg < 3)
            r := d.treg
            IF g_regusetab[r].write = FALSE
               g_regusetab[r].obtains := g_regusetab[r].obtains + 1
               RETURN r, NIL
            ENDIF
         ELSEIF (d.treg < 8) AND (d.treg > 3)
            r := d.treg
            RETURN r, NIL
         ENDIF
      ENDIF
   ELSEIF o = DRX
      IF d > 3
         RETURN d, NIL
      ELSEIF (d < 3)
         IF g_regusetab[d].write = FALSE
            g_regusetab[d].obtains := g_regusetab[d].obtains + 1
            RETURN d, NIL
         ENDIF
      ENDIF
   ENDIF

   -> v50, try to obtain free register first
   r := self.getFreeDREG()
   IF r <> DTEMP
      g_regusetab[r].obtains := 1
      g_regusetab[r].write := NIL
      inst_copy(o,d, DRX, r)
      #ifdef DBG_M68GEN
      DEBUGF('...copyObtain PUT IN FREE D\d\n', r)
      #endif
      RETURN r, FALSE
   ENDIF

   -> worst case scenario
   r, s := self.obtainDREG()
   self.todx(o,d,r)
   #ifdef DBG_M68GEN
   DEBUGF('...copyObtain PUT IN SPILLED D\d\n', r)
   #endif

ENDPROC r, s

PROC copyObtainToAREG(o,d:PTR TO var) OF m68amiga
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('copyObtainToAREG o=\d d=\d\n', o,d)
   #endif

   IF o = VAR
      IF d.trok AND d.intreg
         IF (d.treg > 7) AND (d.treg < 12)
            r := d.treg - 8
            IF g_regusetab[r+8].write = FALSE
               g_regusetab[r+8].obtains := g_regusetab[r+8].obtains + 1
               RETURN r, NIL
            ENDIF
         ENDIF
      ENDIF
      IF d.o = DRX -> regvar 1.8.0 improvement for deref
         o := d.o
         d := d.d
      ENDIF
   ELSEIF o = ARX
      IF (d >= 0) AND (d < 4)
         IF g_regusetab[d+8].write = FALSE
            g_regusetab[d+8].obtains := g_regusetab[d+8].obtains + 1
            RETURN d, NIL
         ENDIF
      ENDIF
   ENDIF

   -> v50, try to obtain free register first
   r := self.getFreeAREG()
   IF r <> ATEMP
      g_regusetab[r+8].obtains := 1
      g_regusetab[r+8].write := NIL
      inst_copy(o,d, ARX, r)
      #ifdef DBG_M68GEN
      DEBUGF('...copyObtain PUT IN FREE A\d\n', r)
      #endif
      RETURN r, FALSE
   ENDIF

   -> worst case scenario
   r, s := self.obtainAREG()
   self.toax(o,d,r)
   #ifdef DBG_M68GEN
   DEBUGF('...copyObtain PUT IN SPILLED A\d\n', r)
   #endif

ENDPROC r, s

PROC m68SetDReg(reg, o,d:PTR TO var)
   DEF a:REG
   DEF t, var:REG PTR TO var

   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF var.type.size = 4
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
      ENDIF
      var := var.next
   ENDWHILE

   IF g_currentproc
      -> clear any entries in variable-tables that references this reg
      var := g_currentproc.args32
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
      var := g_currentproc.locals32
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
   ENDIF

   IF o = VAR
      d.intreg := TRUE
      d.treg := reg
   ENDIF

ENDPROC o,d

PROC m68SetAReg(reg, o,d:PTR TO var)
   DEF a:REG
   DEF t, var:REG PTR TO var

   reg := reg + 8

   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF var.type.size = 4
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
      ENDIF
      var := var.next
   ENDWHILE

   IF g_currentproc
      -> clear any entries in variable-tables that references this reg
      var := g_currentproc.args32
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
      var := g_currentproc.locals32
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
   ENDIF

   IF o = VAR
      d.intreg := TRUE
      d.treg := reg
   ENDIF

ENDPROC o,d

PROC m68SetFReg(reg, o,d:PTR TO var)
  DEF a:REG
   DEF t, var:REG PTR TO var

   -> 1.6.1
   var := g_gvarlist
   WHILE var
      IF var.intreg
         IF var.type.size = 8
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
      ENDIF
      var := var.next
   ENDWHILE

   IF g_currentproc
      -> clear any entries in variable-tables that references this reg
      var := g_currentproc.args64
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
      var := g_currentproc.locals64
      WHILE var
         IF var.intreg
            IF var.treg = reg THEN var.intreg := FALSE
         ENDIF
         var := var.next
      ENDWHILE
   ENDIF

   IF o = VAR64
      d.intreg := TRUE
      d.treg := reg
   ENDIF

ENDPROC

PROC copyObtainToFREG(o,d:PTR TO var) OF m68amiga
   DEF r, s, t

   #ifdef DBG_M68GEN
   DEBUGF('copyObtainToFREG o=\d d=\d\n', o,d)
   #endif

   IF o = VAR64
      IF d.trok AND d.intreg AND (d.treg>0)
         r := d.treg
         IF g_regusetab[r+32].write = FALSE
            g_regusetab[r+32].obtains := g_regusetab[r+32].obtains + 1
            RETURN r, NIL
         ENDIF
      ENDIF
   ELSEIF o = FPX
      IF (d > 0) AND (d < 8)
         IF g_regusetab[d+32].write = FALSE
            g_regusetab[d+32].obtains := g_regusetab[d+32].obtains + 1
            RETURN d, NIL
         ENDIF
      ENDIF
   ENDIF

   -> v50, try to obtain free register first
   r := self.getFreeFREG()
   IF r <> FTEMP
      g_regusetab[r+32].obtains := 1
      g_regusetab[r+32].write := NIL
      inst_copy(o,d, FPX, r)
      #ifdef DBG_M68GEN
      DEBUGF('...copyObtain PUT IN FREE F\d\n', r)
      #endif
      RETURN r, FALSE
   ENDIF

   -> worst case scenario
   r, s := self.obtainFREG()
   self.tofreg(o,d,r)
   #ifdef DBG_M68GEN
   DEBUGF('...copyObtain PUT IN SPILLED F\d\n', r)
   #endif

ENDPROC r, s

-> v50
-> 2.3, use getFreeXReg() isntead of XTEMP ?
PROC secureReturn(ro, rd, ruc:PTR TO oreg) OF m68amiga
   #ifdef DBG_M68GEN
   DEBUGF('secureReturn(\d,\d)\n', ro, rd)
   #endif
   SELECT ro
   CASE DRX
      IF ruc[rd].obtains
         movedxdx(SIZE_L, rd, DTEMP)
         m68SetDReg(DTEMP,NIL,NIL)
         RETURN DRX, DTEMP
      ENDIF
   CASE ARX
      IF ruc[rd+8].obtains
         moveaxax(SIZE_L, rd, ATEMP)
         m68SetAReg(ATEMP,NIL,NIL)
         RETURN ARX, ATEMP
      ENDIF
   CASE FPX
      IF ruc[rd+32].obtains
         fmovefpxfpx(rd, FTEMP)
         m68SetFReg(FTEMP,NIL,NIL)
         RETURN FPX, FTEMP
      ENDIF
   CASE X2R -> 2.3
      fmovefpxaxpofs(FD, rd, FRAMEREG, D64TEMP)
      RETURN D64, D64TEMP
   ENDSELECT
   #ifdef DBG_M68GEN
   DEBUGF('secureReturn DONE\n')
   #endif
ENDPROC ro, rd

-> 2.3
PROC copyObtainToD64(o,d:PTR TO var) OF m68amiga
   DEF r, i, s
   #ifdef DBG_M68GEN
   DEBUGF('copyObtainToD64(\d,$\h)\n', o, d)
   #endif

   IF o = D64
      IF d <> D64TEMP
         -> reuse the D64 if it is in shared mode
         i := g_d64bottom - d / 8
         IF g_regusetab[64+i].write = FALSE
            g_regusetab[64+i].obtains := g_regusetab[64+i].obtains + 1
            #ifdef DBG_M68GEN
            DEBUGF('copyObtainToD64: reusing \d, index \d\n', d, i)
            #endif
            RETURN d, NIL
         ENDIF
      ENDIF
   ENDIF

   r := self.getFreeD64()
   IF r <> D64TEMP
      i := g_d64bottom - r / 8
      g_regusetab[64+i].obtains := 1
      g_lastd64 := r
      self.tod64(o,d, r)
      #ifdef DBG_M68GEN
      DEBUGF('copyObtainToD64: copied into \d, index \d\n', d, i)
      #endif
      RETURN r, NIL
   ENDIF

   #ifdef DBG_M68GEN
   DEBUGF('copyObtainToD64: no free reg available!..\n')
   #endif

   r, s := self.obtainD64()

   self.tod64(o,d, r)

ENDPROC r, s

-> 2.3
PROC obtainD64() OF m68amiga
   DEF r, t, i, s
   #ifdef DBG_M68GEN
   DEBUGF('obtainD64()\d\n', r)
   #endif

   r := self.getFreeD64()
   IF r = D64TEMP
      r := g_lastd64
      FOR t := 1 TO (D64STACKSIZE/8)
         r := r - 8
         IF r < (g_d64bottom - D64STACKSIZE) THEN r := g_d64bottom - 8
         i := g_d64bottom - r / 8
         IF g_regusetab[64+i].write THEN t := D64STACKSIZE -> end loop
      ENDFOR
   ENDIF

   #ifdef DBG_M68GEN
   DEBUGF('obtainD64(): locking \d\n', r)
   #endif

   s := self.lockD64(r)

ENDPROC r, s

-> 2.3
PROC releaseD64(r,retbool) OF m68amiga
   DEF i
   #ifdef DBG_M68GEN
   DEBUGF('releaseD64(\d,\d) ', r, retbool)
   #endif
   i := g_d64bottom - r / 8
   IF g_regusetab[64+i].obtains = 0 THEN reportIErr('releaseD64: too many!')
   g_regusetab[64+i].obtains := g_regusetab[64+i].obtains - 1
   IF g_regusetab[64+i].write
      g_regusetab[64+i].write := g_regusetab[64+i].write - 1
   ENDIF
   #ifdef DBG_M68GEN
   DEBUGF(': register (with index \d) obtains became \d, writes \d\n',
      i, g_regusetab[64+i].obtains, g_regusetab[64+i].write)
   #endif
   g_lastd64 := r + 8
ENDPROC r

-> 2.3
PROC lockD64(r) OF m68amiga
   DEF i
   #ifdef DBG_M68GEN
   DEBUGF('lockD64(\d) ',r )
   #endif
   i := g_d64bottom - r / 8
   g_regusetab[64+i].obtains := g_regusetab[64+i].obtains + 1
   g_regusetab[64+i].write := g_regusetab[64+i].write + 1
   g_lastd64 := r
   #ifdef DBG_M68GEN
   DEBUGF(': register (with index \d) obtains became \d, writes \d\n',
      i, g_regusetab[64+i].obtains, g_regusetab[64+i].write)
   #endif
ENDPROC FALSE

PROC getFreeD64() OF m68amiga
   DEF r, a, i

   #ifdef DBG_M68GEN
   DEBUGF('getFreeD64() ')
   #endif

   r := g_lastd64

   FOR a := 1 TO (D64STACKSIZE/8)
      r := r - 8
      IF r < (g_d64bottom - D64STACKSIZE) THEN r := g_d64bottom - 8
      i := g_d64bottom - r / 8
      IF g_regusetab[64+i].obtains = NIL
         #ifdef DBG_M68GEN
         DEBUGF('\d\n', r)
         #endif
         RETURN r
      ENDIF
   ENDFOR

   #ifdef DBG_M68GEN
   DEBUGF('\d (D64TEMP)\n', D64TEMP)
   #endif

   -> ok lets settle for D64TEMP then

ENDPROC D64TEMP

-> 1.10.0
PROC copy(o1,d1:PTR TO var,o2,d2:PTR TO var) OF m68amiga
   DEF treg

   #ifdef DBG_M68GEN
   DEBUGF('m68amiga.copy(o1=\d,d1=$\h,o2=\d,d2=$\h\n', o1,d1,o2,d2)
   #endif

      IF o1 = DRX
         self.dxto(d1, o2,d2)
      ELSEIF o2 = DRX
         self.todx(o1,d1, d2)
      ELSEIF o1 = ARX
         self.axto(d1,o2,d2)
      ELSEIF o2 = ARX
         self.toax(o1,d1,d2)
      ELSEIF o1 = FPX
         self.fregto(d1, o2,d2)
      ELSEIF o2 = FPX
         self.tofreg(o1,d1,d2)
      ELSEIF o1 = D64
         self.d64to(d1, o2,d2)
      ELSEIF o2 = D64
         self.tod64(o1,d1,d2)
      ELSEIF o1 = VAR
         IF d1.intreg AND d1.trok
            self.dxto(d1.treg, o2,d2)
         ELSE
            treg := self.obtainDREG()
            self.todx(o1,d1, treg)
            self.dxto(treg, o2,d2)
            self.releaseDREG(treg,0)
         ENDIF
      ELSEIF o1 = X2R
         self.x2rto(d1, o2, d2)
      ELSEIF o1 = VAR64 -> v45
         IF d1.type.flags AND MEMBF_FLOAT
            IF d1.intreg AND d1.trok
               self.fregto(d1.treg, o2,d2)
            ELSE
               treg := self.obtainFREG()
               self.tofreg(o1,d1, treg)
               self.fregto(treg, o2,d2)
               self.releaseFREG(treg,0)
            ENDIF
         ELSEIF d1.link = FALSE -> 2.3
            self.d64to(d1.offset, o2, d2)
         ELSEIF (o2 = VAR64) OR -> 2.3
                  ((o2 = ARXPO) AND (ARXPOsize(d2) = 8)) OR
                   ((o2 = ARXPX) AND (ARXPXsize(d2) = 8))
            treg := self.obtainFREG()
            self.tofreg(o1, d1, treg)
            self.fregto(treg, o2, d2)
            self.releaseFREG(treg,0)
         ELSE
            treg := self.obtainFREG()
            self.tofreg(o1,d1, treg)
            self.fregto(treg, o2,d2)
            self.releaseFREG(treg,0)
         ENDIF
      ELSEIF (o1 = ARXPO) AND (ARXPOsize(d1) = 8)
         IF ARXPOflags(d1) AND MEMBF_FLOAT
            treg := self.obtainFREG()
            self.tofreg(o1,d1,treg)
            self.fregto(treg,o2,d2)
            self.releaseFREG(treg,0)
         ELSEIF (o2 = VAR64) OR -> 2.3
                ((o2 = ARXPO) AND (ARXPOsize(d2) = 8)) OR
                ((o2 = ARXPX) AND (ARXPXsize(d2) = 8))
            treg := self.obtainFREG()
            self.tofreg(o1, d1, treg)
            self.fregto(treg, o2, d2)
            self.releaseFREG(treg,0)
         ELSE -> 1.10.0
            treg := self.obtainD64()
            self.tod64(o1,d1,treg)
            self.d64to(treg,o2,d2)
            self.releaseD64(treg,0)
         ENDIF
      ELSEIF (o1 = ARXPX) AND (ARXPXsize(d1) = 8)
         IF ARXPXflags(d1) AND MEMBF_FLOAT
            treg := self.obtainFREG()
            self.tofreg(o1,d1,treg)
            self.fregto(treg,o2,d2)
            self.releaseFREG(treg,0)
         ELSEIF (o2 = VAR64) OR -> 2.3
                ((o2 = ARXPO) AND (ARXPOsize(d2) = 8)) OR
                ((o2 = ARXPX) AND (ARXPXsize(d2) = 8))
            treg := self.obtainFREG()
            self.tofreg(o1, d1, treg)
            self.fregto(treg, o2, d2)
            self.releaseFREG(treg,0)
         ELSE -> 1.10.0
            treg := self.obtainD64()
            self.tod64(o1,d1,treg)
            self.d64to(treg,o2,d2)
            self.releaseD64(treg,0)
         ENDIF
      ELSEIF o2 = VAR64
         IF d2.type.flags AND MEMBF_FLOAT
            treg := self.obtainFREG()
            self.tofreg(o1,d1, treg)
            self.fregto(treg, o2,d2)
            self.releaseFREG(treg,0)
         ELSE
            treg := self.obtainD64()
            self.tod64(o1,d1, treg)
            self.d64to(treg, o2,d2)
            self.releaseD64(treg,0)
         ENDIF
      ELSE
         treg := self.obtainDREG()
         self.todx(o1,d1, treg)
         self.dxto(treg, o2,d2)
         self.releaseDREG(treg,0)
      ENDIF
ENDPROC


PROC dispatcher2(iid, o1,d1:PTR TO var,o2,d2:PTR TO var, e1, e2, e3) OF m68amiga
   SELECT iid
   CASE IID_FADDREG
      SELECT o1
      CASE FPX   ; faddfpxfpx(d1,d2)
      CASE DV    ; faddsimmfpx(d1,d2)
      ENDSELECT
      m68SetFReg(d2,NIL,NIL)
   CASE IID_FSUBREG
      SELECT o1
      CASE FPX   ; fsubfpxfpx(d1,d2)
      CASE DV    ; fsubsimmfpx(d1,d2)
      ENDSELECT
      m68SetFReg(d2,NIL,NIL)
   CASE IID_FMULREG
      SELECT o1
      CASE FPX   ; fmulfpxfpx(d1,d2)
      CASE DV    ; fmulsimmfpx(d1,d2)
      ENDSELECT
      m68SetFReg(d2,NIL,NIL)
   CASE IID_FDIVREG
      SELECT o1
      CASE FPX   ; fdivfpxfpx(d1,d2)
      CASE DV    ; fdivsimmfpx(d1,d2)
      ENDSELECT
      m68SetFReg(d2,NIL,NIL)
   CASE IID_I2FREG
      fmovedxfpx(FL,d1,d2)
      m68SetFReg(d2,NIL,NIL)
   CASE IID_F2IREG
      IF g_optroundnear
         fmovefpxdx(FL,d1,d2)
         m68SetDReg(d2,NIL,NIL)
      ELSE
         -> 2.0: round towards zero by default
         -> we have to save/set/restore fpcr to do this
         fmovelfpcraxpd(7)
         movelimmdx($10, d2)
         fmoveldxfpcr(d2)
         fmovefpxdx(FL,d1,d2)
         fmovelaxpifpcr(7)
         m68SetDReg(d2,NIL,NIL)
      ENDIF
   CASE IID_FSICREG
      SELECT o1
      CASE FPX  ; fcmpfpxfpx(d1,e2)
      CASE DV   ; fcmpsimmfpx(d1,e2)
      ENDSELECT
      SELECT e1
      CASE ISEQ ; fsccdx(FCEQ,e3)
      CASE ISNE ; fsccdx(FCNE,e3)
      CASE ISGT ; fsccdx(FCOGT,e3)
      CASE ISLT ; fsccdx(FCOLT,e3)
      CASE ISGE ; fsccdx(FCOGE,e3)
      CASE ISLE ; fsccdx(FCOLE,e3)
      DEFAULT   ; Throw("68K", 'b68_fsic() - unknown cond')
      ENDSELECT
      extbl(e3)
      m68SetDReg(e3,NIL,NIL)
   CASE IID_FNEGREG
      fnegfpxfpx(d2,d2)
      m68SetFReg(d2,NIL,NIL)
   CASE IID_FABSREG -> 2.2
      fabsfpxfpx(d2,d2)
      m68SetFReg(d2,NIL,NIL)
   DEFAULT
      reportIErr('dispatcher2 iid=?')
   ENDSELECT

ENDPROC

-> 2.3, under construction
PROC dispatcher3(iid,o1,d1:PTR TO var,o2,d2:PTR TO var,e1,e2,e3) OF m68amiga
   DEF t, t2, t3, codelab, eqlab, ltlab, gtlab, end
   #ifdef DBG_M68GEN
   DEBUGF('dispatcher: iid=\d,o1=\d,d1=$\h,o2=\d,d2=$\h,e1=\d,e2=\d,e3=\d\n', iid,o1,d1,o2,d2,e1,e2,e3)
   #endif

   SELECT 256 OF iid
   CASE IID_SICD64
      self.compare64(d1, e2)
      SELECT e1
      CASE ISEQ ; sccdx(EQ,e3)
      CASE ISNE ; sccdx(NE,e3)
      CASE ISGT ; sccdx(GT,e3)
      CASE ISLT ; sccdx(LT,e3)
      CASE ISGE ; sccdx(GE,e3)
      CASE ISLE ; sccdx(LE,e3)
      DEFAULT   ; Throw("68K", 'm68_sic64() - unknown cond')
      ENDSELECT
      extbl(e3)
      m68SetDReg(e3,NIL,NIL)
   CASE IID_MULD64 -> 1.10.0
      -> note: does 32bit*32bit=>64bit!
      t := self.obtainDREG()
      t2 := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, t)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t2)
      mulsldxdrdq(1, t2, t2, t)
      movedxaxpofs(SIZE_L, t2, FRAMEREG, d2)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4)
      self.releaseDREG(t, 0)
      self.releaseDREG(t2, 0)
      m68SetDReg(t, NIL, NIL)
      m68SetDReg(t2, NIL, NIL)
   CASE IID_DIVD64
      -> note: does 64bit/32bit=>32bit!
      t := self.obtainDREG()
      t2 := self.obtainDREG()
      t3 := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t2)
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, t3)
      divsldxdrdq(1, t3, t, t2)
      movedxaxpofs(SIZE_L, t2, FRAMEREG, d2+4) -> lower result
      asrl31dx(t2)
      movedxaxpofs(SIZE_L, t2, FRAMEREG, d2) -> upper result
      self.releaseDREG(t, 0)
      self.releaseDREG(t2, 0)
      self.releaseDREG(t3, 0)
      m68SetDReg(t, NIL, NIL)
      m68SetDReg(t2, NIL, NIL)
      m68SetDReg(t3, NIL, NIL)
   CASE IID_ADDD64
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, DTEMP)
      adddxaxpofs(SIZE_L, DTEMP, FRAMEREG, d2+4)
      moveaxpofsdx(SIZE_L, FRAMEREG, d1, DTEMP)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t)
      addxdxdx(SIZE_L, DTEMP, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2)
      self.releaseDREG(t, 0)
      m68SetDReg(DTEMP, NIL, NIL)
      m68SetDReg(t, NIL, NIL)
   CASE IID_SUBD64
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, DTEMP)
      subdxaxpofs(SIZE_L, DTEMP, FRAMEREG, d2+4)
      moveaxpofsdx(SIZE_L, FRAMEREG, d1, DTEMP)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t)
      subxdxdx(SIZE_L, DTEMP, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2)
      self.releaseDREG(t, 0)
      m68SetDReg(DTEMP, NIL, NIL)
      m68SetDReg(t, NIL, NIL)
   CASE IID_SHRD64
      t := self.obtainDREG()
      t2 := self.obtainDREG()
      
      eqlab := newLabel()
      ltlab := newLabel()
      gtlab := newLabel()
      end := newLabel()
      
      cmpwimmdx(32, d1)
      
      bcclab(LT, ltlab)
      bcclab(EQ, eqlab)
      bcclab(GT, gtlab)
       
      def_label(ltlab) /* < 32 bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t) -> high word in t
      movedxdx(SIZE_L, t, t2) -> copy in t2
      lsrdxdx(SIZE_L, d1, t) -> shift high in t
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2) -> save high
      movelimmdx(32, t) -> 32 in t
      subdxdx(SIZE_L, d1, t) -> subtract shift number from t (32)
      -> shift copy of the old hig word upwards to get special value
      lsldxdx(SIZE_L, t, t2)
      -> now lets do lower word
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t) -> lower word in t
      lsrdxdx(SIZE_L, d1, t) -> shift lower
      -> now or in missing bits into lower result
      ordxdx(SIZE_L, t2, t) -> lower is done in t now
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4) -> save lower result
      
      bcclab(T, end)
      def_label(eqlab) /* 32 bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t) -> high word in t
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4) -> save lower result
      clrdx(SIZE_L, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2) -> save upper result
      
      bcclab(T, end)
      def_label(gtlab) /* > 32bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t) -> high word in t
      clrdx(SIZE_L, t2)
      movedxaxpofs(SIZE_L, t2, FRAMEREG, d2) -> save upper result
      movedxdx(SIZE_L, d1, t2)
      subwimmdx(32, t2) -> subtract 32 from shift value
      lsrdxdx(SIZE_L, t2, t) -> shift high word 
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4) -> save lower result
      
      def_label(end)      
      self.releaseDREG(t2, 0)
      self.releaseDREG(t, 0)
   CASE IID_SHLD64
      t := self.obtainDREG()
      t2 := self.obtainDREG()
      
      eqlab := newLabel()
      ltlab := newLabel()
      gtlab := newLabel()
      end := newLabel()
      
      cmpwimmdx(32, d1)
      
      bcclab(LT, ltlab)
      bcclab(EQ, eqlab)
      bcclab(GT, gtlab)
       
      def_label(ltlab) /* < 32 bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t) -> low word in t
      movedxdx(SIZE_L, t, t2) -> copy in t2
      lsldxdx(SIZE_L, d1, t) -> shift low in t
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4) -> save low
      movelimmdx(32, t) -> 32 in t
      subdxdx(SIZE_L, d1, t) -> subtract shift number from t (32)
      -> shift copy of the old low word downwards to get special value
      lsrdxdx(SIZE_L, t, t2)
      -> now lets do upper word
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t) -> upper word in t
      lsrdxdx(SIZE_L, d1, t) -> shift upper
      -> now or in missing bits into upper result
      ordxdx(SIZE_L, t2, t) -> higher is done in t now
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2) -> save upper result
      
      bcclab(T, end)
      def_label(eqlab) /* 32 bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t) -> low word in t
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2) -> save upper result
      clrdx(SIZE_L, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4) -> save lower result
            
      bcclab(T, end)
      def_label(gtlab) /* > 32bits shift */
      
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t) -> low word in t
      clrdx(SIZE_L, t2)
      movedxaxpofs(SIZE_L, t2, FRAMEREG, d2+4) -> save lower result
      movedxdx(SIZE_L, d1, t2)
      subwimmdx(32, t2) -> subtract 32 from shift value
      lsldxdx(SIZE_L, t2, t) -> shift low word 
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2) -> save upper result
      
      def_label(end)
      self.releaseDREG(t2, 0)
      self.releaseDREG(t, 0)
   CASE IID_ANDD64
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t)
      andaxpofsdx(SIZE_L, FRAMEREG, d1, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t)
      andaxpofsdx(SIZE_L, FRAMEREG, d1+4, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4)
      m68SetDReg(t,NIL,NIL)
      self.releaseDREG(t, 0)
   CASE IID_ORD64
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d2, t)
      oraxpofsdx(SIZE_L, FRAMEREG, d1, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2)
      moveaxpofsdx(SIZE_L, FRAMEREG, d2+4, t)
      oraxpofsdx(SIZE_L, FRAMEREG, d1+4, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2+4)
      m68SetDReg(t,NIL,NIL)
      self.releaseDREG(t, 0)
   CASE IID_XORD64
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, d1, t)
      eordxaxpofs(SIZE_L, t, FRAMEREG, d2)
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, t)
      eordxaxpofs(SIZE_L, t, FRAMEREG, d2+4)
      m68SetDReg(t,NIL,NIL)
      self.releaseDREG(t, 0)
   CASE IID_F2D64
      reportIErr('dispatcher3 f2d64 not implemented')
   CASE IID_D642F
      reportIErr('dispatcher3 d642f not implemented')
   CASE IID_I2D64
      movedxaxpofs(SIZE_L, d1, FRAMEREG, d2+4)
      t := DTEMP
      movedxdx(SIZE_L, d1, t)
      asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, d2)
      m68SetDReg(t, NIL, NIL)
   CASE IID_D642I
      moveaxpofsdx(SIZE_L, FRAMEREG,d1+4, d2)
      m68SetDReg(d2,NIL,NIL)
   CASE IID_NEGD64
      reportIErr('dispatcher3 negd64 not implemented')
   CASE IID_NOTD64   -> 2.2
      moveaxpofsdx(SIZE_L, FRAMEREG, d1, DTEMP)
      notdx(SIZE_L, DTEMP)
      movedxaxpofs(SIZE_L, DTEMP, FRAMEREG, d1)
      moveaxpofsdx(SIZE_L, FRAMEREG, d1+4, DTEMP)
      notdx(SIZE_L, DTEMP)
      movedxaxpofs(SIZE_L, DTEMP, FRAMEREG, d1+4)
      m68SetDReg(DTEMP,NIL,NIL)
   CASE IID_ABSD64   -> 2.2
      reportIErr('dispatcher3 absd64 not implemented')
   DEFAULT
      reportIErr('dispatcher3() iid=?')
   ENDSELECT

ENDPROC

PROC asrimm5dx(s, imm,dx)
   DEF i
   WHILE imm > 0
      i := Min(8, imm)
      asrimmdx(s, i, dx)
      imm := imm - i
   ENDWHILE
ENDPROC

PROC asrl31dx(dx)
   movewimmdx(31, dx)
   asrdxdx(SIZE_L, dx, dx)
ENDPROC


PROC compare64(d64a, d64b) OF m68amiga
   DEF d, done, revcompare

   done := newLabel()
   revcompare := newLabel()

   d := DTEMP

   moveaxpofsdx(SIZE_L, FRAMEREG, d64b, d)
   cmpaxpofsdx(SIZE_L, FRAMEREG, d64a, d)
   bcclab(NE, done)
   -> high words equal, lets compare low words, _unsigned_.
   -> but there is no unsigned compare command on 68k faik..
   -> so we will have to emulate it.
   -> 1. if both values have high bit set we can simply compare signed.
   -> 2. if both values have high bit cleared we can simply compare signed.
   -> 3. otherwise we should exchange operand 1 and operand 2. then compare signed.
   -> 3a. actually we never switch operands, we just compare d64b with d64a instead.
   moveaxpofsdx(SIZE_L, FRAMEREG, d64a+4, d)
   moveaxpofsaxpofs(SIZE_L, FRAMEREG, d64b+4, 7, -4)
   eordxaxpofs(SIZE_L, d, 7, -4) -> compare bits
   moveaxpofsdx(SIZE_L, 7, -4, d)
   andlimmdx($80000000, d) -> test upper bit result
   bcclab(NE, revcompare)
   -> compare as usual
   moveaxpofsdx(SIZE_L, FRAMEREG, d64b+4, d)
   cmpaxpofsdx(SIZE_L, FRAMEREG, d64a+4, d)
   bcclab(T, done)

   def_label(revcompare)
   -> compare reversed
   moveaxpofsdx(SIZE_L, FRAMEREG, d64a+4, d)
   cmpaxpofsdx(SIZE_L, FRAMEREG, d64b+4, d)

   def_label(done)

   m68SetDReg(d,NIL,NIL)
ENDPROC

-> used by m68amiga.compareReg()
PROC test64(d64) OF m68amiga-> 2.2.2
   DEF t
   t := DTEMP ->self.obtainDREG()  -> hmm..
   moveaxpofsdx(SIZE_L, FRAMEREG, d64, t)
   oraxpofsdx(SIZE_L, FRAMEREG, d64+4, t)
   m68SetDReg(t,NIL,NIL)
   ->self.releaseDREG(t, 0)
ENDPROC

PROC dispatcher(iid,o1,d1:PTR TO var,o2,d2:PTR TO var,e1,e2,e3) OF m68amiga
   DEF codelab:PTR TO codelab, proc:PTR TO proc
   DEF cond, reverse, drx, ax, noraise:PTR TO codelab, exc, exci, do, s
   DEF voff:PTR TO LONG, a, va:PTR TO var, fpx, treg, t, t2

   #ifdef DBG_M68GEN
   DEBUGF('dispatcher: iid=\d,o1=\d,d1=$\h,o2=\d,d2=$\h,e1=\d,e2=\d,e3=\d\n', iid,o1,d1,o2,d2,e1,e2,e3)
   #endif


   SELECT 256 OF iid
   CASE IID_VARADR
      IF d1.link
         leaaxplabax(d1.breg, d1,d2)
      ELSE
         leaaxpofsax(d1.breg,d1.offset,d2)
      ENDIF
      m68SetAReg(d2,NIL,NIL)
   CASE IID_LABADR
      leapcplabax(d1,d2)
      m68SetAReg(d2,NIL,NIL)
   CASE IID_BIC
      cond := e1
#ifdef DOSICBICOPTI
      IF g_lastx.iid = IID_SICREG  -> 1.6.0
      IF g_codeptr = g_lastx.end -> sic just before us.
      IF o2 = DV
      IF d2 = NIL
      IF o1 = DRX
      IF d1 = g_lastx.rx
         IF cond = ISNE
            g_codeptr := g_lastx.start -> back up
            o2 := DRX
            d2 := g_lastx.rx
            o1 := g_lastx.o
            d1 := g_lastx.d
            cond := g_lastx.cond->reverseCondition(g_lastsic[3])
         ELSEIF cond = ISEQ
            g_codeptr := g_lastx.start -> back up
            o2 := DRX
            d2 := g_lastx.rx
            o1 := g_lastx.o
            d1 := g_lastx.d
            cond := reverseCondition(g_lastx.cond)
         ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
#endif
      codelab := e2
      reverse := self.private_cmp(o1,d1,o2,d2, cond)
      SELECT cond
      CASE ISEQ ; bcclab(EQ,codelab)
      CASE ISNE ; bcclab(NE,codelab)
      CASE ISGT ; IF reverse THEN bcclab(LT,codelab) ELSE bcclab(GT,codelab)
      CASE ISLT ; IF reverse THEN bcclab(GT,codelab) ELSE bcclab(LT,codelab)
      CASE ISGE ; IF reverse THEN bcclab(LE,codelab) ELSE bcclab(GE,codelab)
      CASE ISLE ; IF reverse THEN bcclab(GE,codelab) ELSE bcclab(LE,codelab)
      DEFAULT   ; Throw("68K", 'b68_bic() - unknown cond')
      ENDSELECT
   CASE IID_NEGREG
      negdx(SIZE_L,d1)
      m68SetDReg(d1,NIL,NIL)
   CASE IID_NOTREG
      notdx(SIZE_L,d1)
      m68SetDReg(d1,NIL,NIL)
   CASE IID_ABSREG   -> 2.2
      codelab := newLabel()
      tstdx(SIZE_L,d1)
      bcclab(GE, codelab)
      negdx(SIZE_L,d1)
      putLabel(codelab)
      m68SetDReg(d1,NIL,NIL)
   CASE IID_SICREG
      cond := e1
      drx := e2
      g_lastx.iid := IID_SICREG
      g_lastx.start := g_codeptr
      g_lastx.o := o1
      g_lastx.d := d1
      g_lastx.cond := cond
      g_lastx.rx := drx
      reverse := self.private_cmp(o1,d1,DRX,drx,cond)
      SELECT cond
      CASE ISEQ ; sccdx(EQ,drx)
      CASE ISNE ; sccdx(NE,drx)
      CASE ISGT ; IF reverse THEN sccdx(LT,drx) ELSE sccdx(GT,drx)
      CASE ISLT ; IF reverse THEN sccdx(GT,drx) ELSE sccdx(LT,drx)
      CASE ISGE ; IF reverse THEN sccdx(LE,drx) ELSE sccdx(GE,drx)
      CASE ISLE ; IF reverse THEN sccdx(GE,drx) ELSE sccdx(LE,drx)
      DEFAULT   ; Throw("68K", 'b68_sic() - unknown cond')
      ENDSELECT
      extbl(drx)
      m68SetDReg(drx,NIL,NIL)
      g_lastx.end := g_codeptr -> 1.5.4
   CASE IID_PUSH  -> 1.8.1
      IF e1 = 4
         IF o1 = ARX
            treg := d1
            moveaxaxpd(SIZE_L,treg, 7)
         ELSE
            treg := self.m68RegisterizeD(o1,d1)
            movedxaxpd(SIZE_L,treg, 7)
         ENDIF
      ELSE
         treg := self.m68RegisterizeF(o1,d1)
         fmovefpxaxpd(FD, treg, 7)
      ENDIF
   CASE IID_POP   -> 1.8.1
      IF e1 = 4
         SELECT o1
         CASE DRX  ; moveaxpidx(SIZE_L, 7, d1)
         CASE ARX  ; moveaxpiax(SIZE_L, 7, d1)
         DEFAULT
            treg := DTEMP
            moveaxpidx(SIZE_L, 7, treg)
            self.dxto(treg, o1,d1)
         ENDSELECT
      ELSE
         IF o1 = FPX
            fmoveaxpifpx(FD, 7, d1)
         ELSE
            treg := FTEMP
            fmoveaxpifpx(FD, 7, treg)
            self.fregto(treg, o1,d1)
         ENDIF
      ENDIF
   CASE IID_RET
      IF g_exceptlab THEN self.restorehandler() -> 1.8.0
      IF g_procendlab THEN bcclab(T,g_procendlab) ELSE rts_()
   CASE IID_GOLAB
      bcclab(T,d1)
   CASE IID_GOARX
      jmpaxp(d1)
   CASE IID_ADDREG
      SELECT o1
      CASE DRX   ; adddxdx(SIZE_L,d1,d2)
      CASE DV    ; addlimmdx(d1,d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_SUBREG
      SELECT o1
      CASE DRX   ; subdxdx(SIZE_L,d1,d2)
      CASE DV    ; sublimmdx(d1,d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_ORREG
      SELECT o1
      CASE DRX   ; ordxdx(SIZE_L,d1,d2)
      CASE DV    ; orlimmdx(d1,d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
    CASE IID_XORREG
      SELECT o1
      CASE DRX   ; eordxdx(SIZE_L,d1,d2)
      CASE DV    ; eorlimmdx(d1,d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_ANDREG
      SELECT o1
      CASE DRX   ; anddxdx(SIZE_L,d1,d2)
      CASE DV    ; andlimmdx(d1,d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_MULREG
      SELECT o1  -> 1.10.0 fix
      CASE DRX    ; mulsldxdrdq(0, d1, d2, d2)
      CASE DV     ; mulslimmdrdq(0, d1, d2, d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_DIVREG
      SELECT o1  -> even better 1.10.0 fix
      CASE DRX    ; divsldxdrdq(0, d1, d2, d2)
      CASE DV     ; divslimmdrdq(0, d1, d2, d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_SHRREG
      SELECT o1
      CASE DRX   ; lsrdxdx(SIZE_L,d1,d2)
      CASE DV    ; REPEAT
                 ;    lsrimmdx(SIZE_L,IF d1 > 8 THEN 8 ELSE d1, d2)
                 ;    d1 := d1 - 8
                 ; UNTIL d1 < 1
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_ASRREG
      SELECT o1
      CASE DRX   ; asrdxdx(SIZE_L,d1,d2)
      CASE DV    ; asrimm5dx(SIZE_L, d1, d2)
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)
   CASE IID_SHLREG
      SELECT o1
      CASE DRX   ; lsldxdx(SIZE_L,d1,d2)
      CASE DV    ; REPEAT
                 ;    lslimmdx(SIZE_L,IF d1 > 8 THEN 8 ELSE d1, d2)
                 ;    d1 := d1 - 8
                 ; UNTIL d1 < 1
      ENDSELECT
      m68SetDReg(d2,NIL,NIL)

   CASE IID_INCVAR -> 1.5.3: d1:var, d2:val
      IF d1.type.size = 8 -> 2.3
         IF d1.link
            fmoveaxplabfpx(FD, d1.breg, d1, FTEMP)
            fmovefpxaxpofs(FD, FTEMP, STACKREG, -8)

            t := self.obtainDREG()
            t2 := self.obtainDREG()
            movelimmdx(0, t)
            moveaxpofsdx(SIZE_L, STACKREG, -8, t2)
            addlimmaxpofs(d2, STACKREG, -4)
            IF d2 < 0 THEN subxdxdx(SIZE_L, t, t2) ELSE addxdxdx(SIZE_L, t, t2)
            movedxaxpofs(SIZE_L, t2, STACKREG, -8)
            self.releaseDREG(t2, 0)
            self.releaseDREG(t, 0)

            fmoveaxpofsfpx(FD, STACKREG, -8, FTEMP)
            fmovefpxaxplab(FD, FTEMP, d1.breg, d1)

            m68SetDReg(t2, NIL, NIL)
            m68SetDReg(t, NIL, NIL)
            m68SetFReg(FTEMP, NIL, NIL)
         ELSE
            t := self.obtainDREG()
            t2 := self.obtainDREG()
            movelimmdx(0, t)
            moveaxpofsdx(SIZE_L, d1.breg, d1.offset, t2)
            addlimmaxpofs(d2, d1.breg, d1.offset + 4)
            IF d2 < 0 THEN subxdxdx(SIZE_L, t, t2) ELSE addxdxdx(SIZE_L, t, t2)
            movedxaxpofs(SIZE_L, t2, d1.breg, d1.offset)
            self.releaseDREG(t2, 0)
            self.releaseDREG(t, 0)
            m68SetDReg(t2, NIL, NIL)
            m68SetDReg(t, NIL, NIL)
         ENDIF
      ELSE
         IF d1.o = DRX
            addlimmdx(d2,d1.d)
            ->m68SetDReg(d1.d, NIL,NIL)
         ELSE
            IF d1.intreg AND d1.trok
               IF d1.treg<8
                  addlimmdx(d2, d1.treg)
                  m68SetDReg(d1.treg, NIL,NIL)
                  self.dxto(d1.treg, VAR,d1)
               ELSE
                  addlimmax(d2, d1.treg-8)
                  m68SetAReg(d1.treg-8, NIL,NIL)
                  self.axto(d1.treg-8, VAR,d1)
               ENDIF
            ELSE
               IF d1.link
                  addlimmaxplab(d2,d1.breg,d1)
               ELSE
                  addlimmaxpofs(d2,d1.breg,d1.offset)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CASE IID_INCARX -> 1.5.3 d1:arx, d2:val
      addlimmax(d2, d1)
      m68SetAReg(d1,NIL,NIL)
   CASE IID_GETRWD
      IF g_optmodule
         g_rwreflist := NEW [g_rwreflist, g_codeptr-g_codebuf+2]:rwref
         putMOVEI(SIZE_L,d2,M1,d1)
         addaxax(SIZE_L,4,d2)
      ELSE
         d1 := d1 - g_databufsize - IVARSSIZE
         IF d1 < -32767
            putMOVEI(SIZE_L,d2,M1,d1)
            addaxax(SIZE_L,4,d2)
         ELSE
            leaaxpofsax(4,d1,d2)
         ENDIF
      ENDIF
      m68SetAReg(d2,NIL,NIL)
   CASE IID_GOSLAB
      bsrlab(d1)
   CASE IID_GOSARX
      jsraxp(d1)
   CASE IID_GETIMMSTR
      t := addRelStr(e1)
      leapcplabax(t, e2)
      m68SetAReg(e2,NIL,NIL)
   DEFAULT           ; reportIErr('.dispatcher() iid=?')
   ENDSELECT

ENDPROC


PROC prochead(table,tabsize,ftable, ftabsize) OF m68amiga
   DEF proc:PTR TO proc, a, voff:PTR TO LONG, drx, va:PTR TO var, t
   DEF voff2:PTR TO LONG, fpx

      proc := g_currentproc
      IF currentOffset() = NIL THEN nop() ELSE putAlign(2) -> v47, modules
      putLabel(proc)
      g_exceptlab := IF proc.handle THEN newLabel() ELSE NIL
      g_endexceptlab := IF proc.handle THEN newLabel() ELSE NIL
      g_procendlab := newLabel()

      clearRegs() -> v49, 1.4.7, wa forgotten

      -> 2.2 fixed problem with double args and normal args mix
      voff := 0
      fpx := 0
      FOR a := 0 TO proc.nrofargs-1
         va := proc.argarray[a]
         IF va.rtype = M68ARGTYPE_FREG -> doublearg ?
            IF fpx > 7 THEN reportErr('Too many double arguments allocated', proc.name)
            fpx++
            va.rnum := fpx
            va.intreg := TRUE
            va.treg := fpx
            va.offset := voff - 8
            voff := va.offset
         ENDIF
      ENDFOR

      -> 2.3 improved
      voff2 := 8 -> stack arguments
      FOR a := proc.nrofargs-1 TO 0 STEP -1
         va := proc.argarray[a]
         IF va.rtype = M68ARGTYPE_STACK -> int arg
            va := proc.argarray[a]
            va.offset := voff2++
            va.intreg := FALSE
         ELSEIF va.rtype = M68ARGTYPE_STACK_WIDE
            va := proc.argarray[a]
            va.offset := voff2++ ; voff2++
            va.intreg := FALSE
         ENDIF
      ENDFOR


      /* locals32 */
      drx := 7
      va := proc.locals32
      WHILE va
         IF va.o = DRX
            va.d := drx
            IF drx < 4 THEN reportErr('Too many registers allocated', proc.name)
            drx--
         ELSE
            va.offset := voff--
         ENDIF
         va := va.next
      ENDWHILE


      /* locals64 */
      va := proc.locals64
      WHILE va
         IF va.o = FPX
            reportErr('cannot put in register', va.hln.name)
         ENDIF
         va.offset := voff - 8
         voff := va.offset
         va := va.next
      ENDWHILE

      /* locals00 */
      va := proc.locals00
      WHILE va
         SELECT 256 OF va.type.esize
         CASE 1
            IF va.cmplx
               voff := voff - (va.type.numes + 1 + 3 AND $FFFC)
               va.offset := voff
               voff--
            ELSE
               voff := voff - (va.type.numes + 3 AND $FFFC)
               va.offset := voff
            ENDIF
         CASE 2
            voff := voff - (va.type.numes * 2 + 3 AND $FFFC)
            va.offset := voff
         CASE 4
            IF va.cmplx
               voff := voff - (va.type.numes * 4 + 3 AND $FFFC)
               va.offset := voff
               voff := voff - 4
            ELSE
               voff := voff - (va.type.numes * 4 + 3 AND $FFFC)
               va.offset := voff
            ENDIF
         CASE 8
            voff := voff - (va.type.numes * 8 + 3 AND $FFFC)
            va.offset := voff
         CASE 255 -> object
            voff := voff - (va.type.numes * va.type.object.sizeof + 3 AND $FFFC)
            va.offset := voff
            IF va.type.object.nrofmethods
            IF va.type.numes = 1
               voff--
            ENDIF ; ENDIF
         ENDSELECT
         IF va.o = DRX
            va.d := drx
            IF drx < 4 THEN reportErr('Too many registers allocated', proc.name)
            drx--
         ELSE
            va.o := ARXPO
            va.d := AxSizeOfs(5,0,va.offset)
         ENDIF
         va := va.next
      ENDWHILE


      /* autregalloc */
      FOR a := 0 TO tabsize-1
         EXIT drx < 4
         EXIT (g_numregalloc<>-1) AND ((7-drx) >= g_numregalloc)
         va := Long(table+(a*4))
         va.o := DRX
         va.d := drx
         drx--
      ENDFOR

      proc.numregalloc := 7-drx

      -> 2.3, for D64
      D64TEMP := voff - 8
      g_d64bottom := D64TEMP
      g_lastd64 := g_d64bottom
      voff := g_d64bottom - D64STACKSIZE

      proc.framesize := -voff -> v58

      linkw(5,voff)

      IF proc.handle
         -> save d3-d7
         movemregsaxpd(SIZE_L,%0001111100000000, 7)
         -> 2.2.3, save exception global on stack
         moveaxpofsaxpd(SIZE_L, 4, IV_exception, 7)
         -> save old exception stuff globals
         moveaxpofsaxpd(SIZE_L,4,$FFB0,7) -> -80 = stack return
         moveaxpofsaxpd(SIZE_L,4,$FFB4,7) -> -76 = code return
         moveaxpofsaxpd(SIZE_L,4,$FFA8,7) -> -88 = saved a5
         -> set exception stuff globals
         moveaxaxpofs(SIZE_L,7,4,$FFB0)   -> -80 = stack return
         leapcplabax(g_exceptlab, ATEMP)
         moveaxaxpofs(SIZE_L,ATEMP,4,$FFB4) -> -76 = code return
         moveaxaxpofs(SIZE_L,5,4,$FFA8)   -> -88 = saved a5
      ELSEIF drx < 7
         IF drx = 6
            movemregsaxpd(SIZE_L,%0001000100000000, 7)
         ELSEIF drx = 5
            movemregsaxpd(SIZE_L,%0001001100000000, 7)
         ELSEIF drx = 4
            movemregsaxpd(SIZE_L,%0001011100000000, 7)
         ELSEIF drx = 3
            movemregsaxpd(SIZE_L,%0001111100000000, 7)
         ENDIF
      ELSE
         movedxaxpd(SIZE_L,3,7) -> save d3
      ENDIF

      IF proc.object
         va := proc.selfvar
         IF va.o = DRX
            moveaxdx(SIZE_L,SELFREG,va.d)
         ELSE
            moveaxaxpofs(SIZE_L,SELFREG,va.breg,va.offset) -> set selfvar
            va.intreg := TRUE
            va.treg := SELFREG+8
         ENDIF
      ENDIF

      -> 1.9.0 was forgotten. store float regs into stack as param-vars
      FOR a := 0 TO proc.nrofargs-1
         va := proc.argarray[a]
         IF va.rtype = 1 -> doublearg ?
            fmovefpxaxpofs(FD,va.rnum,va.breg,va.offset)
         ENDIF
      ENDFOR

      va := proc.args32  -> v46, get args into regs
      WHILE va
         IF va.o = DRX THEN moveaxpofsdx(SIZE_L,va.breg,va.offset,va.d)
         va := va.next
      ENDWHILE

      /* locals32 */
      va := proc.locals32
      WHILE va
         /* set defaultvalues */
         IF va.defo
            IF va.o = DRX THEN movelimmdx(va.defd, va.d) ELSE movelimmaxpofs(va.defd,5,va.offset)
         ENDIF
         voff := va.offset + 4
         va := va.next
      ENDWHILE

      /* locals64 */
      va := proc.locals64
      WHILE va
         IF va.defo
            IF va.type.flags AND MEMBF_FLOAT
               fmovesimmfpx(va.defd,0)
               fmovefpxaxpofs(FD,0,FRAMEREG,va.offset)
            ELSE
               movelimmaxpofs(IF va.defd AND $80000000 THEN -1 ELSE 0, FRAMEREG, va.offset)
               movelimmaxpofs(va.defd, FRAMEREG, va.offset + 4)
            ENDIF
         ENDIF
         va := va.next
      ENDWHILE

      /* locals00 */
      va := proc.locals00
      WHILE va
         t := va.type.esize
         SELECT t
         CASE 1
            IF va.o = DRX
               leaaxpofsax(5,va.offset,0)
               moveaxdx(SIZE_L,0,va.d)
            ENDIF
            IF va.cmplx
               IF va.o <> DRX THEN leaaxpofsax(5,va.offset,0)
               movelimmaxp(NIL,0) -> nil-term
               movelimmaxpd(Shl(va.type.numes,16),0) -> max,curr
            ENDIF
         CASE 2
            IF va.o = DRX
               leaaxpofsax(5,va.offset,0)
               moveaxdx(SIZE_L,0,va.d)
            ENDIF
         CASE 4
            IF va.o = DRX
               leaaxpofsax(5,va.offset,0)
               moveaxdx(SIZE_L,0,va.d)
            ENDIF
            IF va.cmplx
               IF va.o <> DRX THEN leaaxpofsax(5,va.offset,0)
               movelimmaxpd(Shl(va.type.numes,16),0) -> max,curr
            ENDIF
         CASE 8
            IF va.o = DRX
               leaaxpofsax(5,va.offset,0)
               moveaxdx(SIZE_L,0,va.d)
            ENDIF
         CASE 255 -> object
            IF va.o = DRX
               leaaxpofsax(5,va.offset,0)
               moveaxdx(SIZE_L,0,va.d)
            ENDIF
            IF (va.type.object.nrofmethods>0) AND (va.type.numes = 1)-> v46
               leapcplabax(va.type.object, 0)
               moveaxaxpofs(SIZE_L, 0, va.breg, va.offset-4)
               t := va.type.object.sizeof + 3 AND $FFFC
               WHILE t > 0
                  t := t - 4
                  movelimmaxpofs(NIL,va.breg,va.offset+t)
               ENDWHILE
            ENDIF
         ENDSELECT
         va := va.next
      ENDWHILE

ENDPROC

PROC exceptblock(do) OF m68amiga
   clearRegs()
   self.restorehandler()
   IF do = FALSE THEN bcclab(T, g_endexceptlab)
   putLabel(g_exceptlab)
ENDPROC

PROC restorehandler() OF m68amiga
   moveaxpiaxpofs(SIZE_L,7,4,$FFA8) -> -88 = a5 return
   moveaxpiaxpofs(SIZE_L,7,4,$FFB4) -> -76 = code return
   moveaxpiaxpofs(SIZE_L,7,4,$FFB0) -> -80 = stack return
ENDPROC

PROC endproc1() OF m68amiga
   clearRegs()
   IF g_endexceptlab THEN putLabel(g_endexceptlab)
ENDPROC

PROC endproc2() OF m68amiga
   DEF va:PTR TO var, proc:PTR TO proc
   proc := g_currentproc
   putLabel(g_procendlab)


   IF proc.handle
      moveaxpiaxpofs(SIZE_L,7,4,IV_exception) -> 2.2.3 restore exception global
      movemaxpiregs(SIZE_L,7,%0000000011111000)
   ELSEIF proc.numregalloc = 1
      movemaxpiregs(SIZE_L,7,%0000000010001000)
   ELSEIF proc.numregalloc = 2
      movemaxpiregs(SIZE_L,7,%0000000011001000)
   ELSEIF proc.numregalloc = 3
      movemaxpiregs(SIZE_L,7,%0000000011101000)
   ELSEIF proc.numregalloc = 4
      movemaxpiregs(SIZE_L,7,%0000000011111000)
   ELSE
      moveaxpidx(SIZE_L, 7, 3)
   ENDIF

   unlkax(5)
   rts_()
   g_exceptlab := NIL
   g_endexceptlab := NIL
   g_procendlab := NIL
ENDPROC

-> v1.5.3
PROC quotehead() OF m68amiga
   DEF q, endq
   q := newLabel()
   endq := newLabel()
   inst_golab(endq)
   def_label(q)
ENDPROC q, endq

PROC quotetail(q, endq, eo, ed, qarx) OF m68amiga
   inst_copy(eo,ed, DREG,IREG0)
   rts_()
   def_label(endq)
   inst_labadr(q,qarx)
ENDPROC



PROC todx(o,d:PTR TO var, reg) OF m68amiga
   DEF t, ix, size, scale

   SELECT o
   CASE DV    ; movelimmdx(d, reg)
   CASE DRX   ; IF reg <> d THEN movedxdx(SIZE_L,d,reg)
   CASE ARX   ; moveaxdx(SIZE_L,d,reg)
   CASE VAR   ; IF d.intreg AND d.trok
              ;    IF d.treg < 8
              ;       IF d.treg <> reg THEN movedxdx(SIZE_L,d.treg,reg)
              ;    ELSE
              ;       moveaxdx(SIZE_L,d.treg-8,reg)
              ;    ENDIF
              ; ELSEIF d.link=NIL
              ;    moveaxpofsdx(SIZE_L,d.breg, d.offset,reg)
              ; ELSE
              ;    movelaxplabdx(d.breg,d,reg)
              ; ENDIF
   CASE VAR64
      IF d.type.flags AND MEMBF_FLOAT
         IF d.intreg AND d.trok
            IF d.treg <> reg THEN fmovefpxdx(FS,d.treg,reg)
         ELSEIF d.link=NIL
            t := self.obtainFREG()
            fmoveaxpofsfpx(FD,d.breg,d.offset,t)
            m68SetFReg(t, NIL,NIL)
            fmovefpxdx(FS,t,reg)
            self.releaseFREG(t,0)
         ELSE
            t := self.obtainFREG()
            fmoveaxplabfpx(FD,d.breg,d,t)
            fmovefpxdx(FS,t,reg)
            self.releaseFREG(t,0)
         ENDIF
      ELSE
         IF d.link = FALSE
            moveaxpofsdx(SIZE_L, d.breg, d.offset + 4, reg)
         ELSE
            t := self.obtainFREG()
            fmoveaxplabfpx(FD, d.breg, d, t)
            fmovefpxaxpofs(FD, t, STACKREG, -8)
            moveaxpofsdx(SIZE_L, STACKREG, -4, reg)
            m68SetFReg(t, NIL, NIL)
            self.releaseFREG(t, 0)
         ENDIF
      ENDIF
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 4 ; moveaxpofsdx(SIZE_L,ARXPOarx(d),ARXPOofs(d), reg)
      CASE 2 ; IF ARXPOflags(d) AND MEMBF_SIGNED
             ;    moveaxpofsdx(SIZE_W,ARXPOarx(d),ARXPOofs(d), reg) ; extl(reg)
             ; ELSE
             ;    clrdx(SIZE_L,reg) ; moveaxpofsdx(SIZE_W,ARXPOarx(d),ARXPOofs(d), reg)
             ; ENDIF
      CASE 1 ; IF ARXPOflags(d) AND MEMBF_SIGNED
             ;    moveaxpofsdx(SIZE_B,ARXPOarx(d),ARXPOofs(d),reg) ; extbl(reg)
             ; ELSE
             ;    clrdx(SIZE_L,reg) ; moveaxpofsdx(SIZE_B,ARXPOarx(d),ARXPOofs(d),reg)
             ; ENDIF
      CASE 0 ; t := self.obtainAREG()
             ; leaaxpofsax(ARXPOarx(d),ARXPOofs(d),t)
             ; moveaxdx(SIZE_L,t, reg)
             ; m68SetAReg(t, NIL,NIL)
             ; self.releaseAREG(t,0)
      CASE 8 ; moveaxpofsdx(SIZE_L,ARXPOarx(d),ARXPOofs(d)+4, reg)
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 4 ; moveaxpxdx(SIZE_L,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg)
      CASE 2 ; IF ARXPXflags(d) AND MEMBF_SIGNED
             ;    moveaxpxdx(SIZE_W,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg) ; extl(reg)
             ; ELSE
             ;    clrdx(SIZE_L,reg) ; moveaxpxdx(SIZE_W,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg)
             ; ENDIF
      CASE 1 ; IF ARXPXflags(d) AND MEMBF_SIGNED
             ;    moveaxpxdx(SIZE_B,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg) ; extbl(reg)
             ; ELSE
             ;    clrdx(SIZE_L,reg) ; moveaxpxdx(SIZE_B,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg)
             ; ENDIF
      CASE 0 ; t := self.obtainAREG()
             ; leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, t)
             ; moveaxdx(SIZE_L,t,reg)
             ; m68SetAReg(t, NIL,NIL)
             ; self.releaseAREG(t, 0)
      CASE 8 ; t := self.obtainAREG() ;
             ; leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, t)
             ; addlimmax(4,t)
             ; moveaxpdx(SIZE_L,t,reg)
             ; m68SetAReg(t, NIL,NIL)
             ; self.releaseAREG(t,0)
      ENDSELECT
   CASE FPX
      fmovefpxdx(FS,d,reg)
   CASE D64
      moveaxpofsdx(SIZE_L, FRAMEREG, d+4, reg)
   DEFAULT
      Throw("68K", 'todx() o')
   ENDSELECT

   m68SetDReg(reg, o,d) -> update table

ENDPROC

PROC dxto(reg, o,d:PTR TO var) OF m68amiga
   DEF t, t2, size, ix, scale

   SELECT o
   CASE DRX       ; IF d <> reg
                  ;    movedxdx(SIZE_L,reg,d) ; m68SetDReg(d,NIL,NIL)
                  ; ENDIF
   CASE ARX       ; movedxax(SIZE_L,reg,d) ; m68SetAReg(d,NIL,NIL)
   CASE VAR       ; IF d.link
                  ;    moveldxaxplab(reg,d.breg,d)
                  ;    d.intreg := TRUE
                  ;    d.treg := reg
                  ; ELSE
                  ;    movedxaxpofs(SIZE_L,reg,d.breg,d.offset)
                  ;    d.intreg := TRUE
                  ;    d.treg := reg
                  ; ENDIF
   CASE VAR64
      IF d.type.flags AND MEMBF_FLOAT
         t := self.obtainFREG()
         fmovedxfpx(FS,reg,t)
         IF d.link
            fmovefpxaxplab(FD,t,d.breg,d)
         ELSE
            fmovefpxaxpofs(FD,t,d.breg,d.offset)
         ENDIF
         m68SetFReg(t, NIL,NIL)
         self.releaseFREG(t,0)
      ELSE
         t := self.obtainDREG()
         movedxdx(SIZE_L, reg, t)
         IF d.link
            t2 := self.obtainFREG()
            movedxaxpd(SIZE_L, reg, STACKREG)
            asrl31dx(t) 
            movedxaxpd(SIZE_L, t, STACKREG)
            fmoveaxpifpx(FD, STACKREG, t2)
            fmovefpxaxplab(FD, t2, d.breg, d)
            self.releaseFREG(t2, 0)
            m68SetFReg(t2, NIL, NIL)
         ELSE
            movedxaxpofs(SIZE_L, reg, d.breg, d.offset + 4)
            asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
            movedxaxpofs(SIZE_L, t, d.breg, d.offset)
         ENDIF
         self.releaseDREG(t, 0)
         m68SetDReg(t, NIL, NIL)
      ENDIF
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8
         IF ARXPOflags(d) AND MEMBF_FLOAT
            t := self.obtainFREG()
            fmovedxfpx(FS, reg, t)
            fmovefpxaxpofs(FD, t, ARXPOarx(d), ARXPOofs(d))
            m68SetFReg(t, NIL,NIL)
            self.releaseFREG(t,0)
         ELSE
            t := self.obtainDREG()
            movedxdx(SIZE_L, reg, t)
            movedxaxpofs(SIZE_L, reg, ARXPOarx(d), ARXPOofs(d)+4)
            asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
            movedxaxpofs(SIZE_L, t, ARXPOarx(d), ARXPOofs(d))
            self.releaseDREG(t, 0)
            m68SetDReg(t, NIL, NIL)
         ENDIF
      CASE 4 ; movedxaxpofs(SIZE_L,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 2 ; movedxaxpofs(SIZE_W,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 1 ; movedxaxpofs(SIZE_B,reg,ARXPOarx(d),ARXPOofs(d))
      DEFAULT ; reportIErr('unimplemented')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 4 ; movedxaxpx(SIZE_L,reg,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0)
      CASE 2 ; movedxaxpx(SIZE_W,reg,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0)
      CASE 1 ; movedxaxpx(SIZE_B,reg,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0)
      DEFAULT ; reportIErr('unimplemented')
      ENDSELECT
   CASE FPX
      fmovedxfpx(FS,reg,d) ; m68SetFReg(d, NIL,NIL)
   CASE D64
      movedxaxpofs(SIZE_L, reg, FRAMEREG, d+4) -> lower
      asrl31dx(reg)
      movedxaxpofs(SIZE_L, reg, FRAMEREG, d) -> upper
      moveaxpofsdx(SIZE_L, FRAMEREG, d+4, reg) -> reload
   DEFAULT
      Throw("68K", 'dxto() o')
   ENDSELECT

ENDPROC

PROC toax(o,d:PTR TO var, reg) OF m68amiga
   DEF t, ix, size, scale

   SELECT o
   CASE DV    ; movelimmax(d, reg)
   CASE DRX   ; movedxax(SIZE_L,d,reg)
   CASE ARX   ; IF d <> reg THEN moveaxax(SIZE_L,d,reg)
   CASE VAR   ; IF d.intreg AND d.trok
              ;    IF d.treg < 8
              ;       movedxax(SIZE_L,d.treg,reg)
              ;    ELSE
              ;       IF d.treg <> (reg+8) THEN moveaxax(SIZE_L,d.treg-8,reg)
              ;    ENDIF
              ; ELSEIF d.link=NIL
              ;    moveaxpofsax(SIZE_L,d.breg, d.offset,reg)
              ; ELSE
              ;    movelaxplabax(d.breg,d,reg)
              ; ENDIF
   CASE VAR64 ; reportIErr('68k var64->ax')
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 4 ; moveaxpofsax(SIZE_L,ARXPOarx(d),ARXPOofs(d), reg)
      CASE 0 ; leaaxpofsax(ARXPOarx(d),ARXPOofs(d),reg)
      CASE 2 ; IF ARXPOflags(d) AND MEMBF_SIGNED
             ;    t := self.obtainDREG() ; moveaxpofsdx(SIZE_W,ARXPOarx(d),ARXPOofs(d),t)
             ;    extl(t) ; movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ELSE
             ;    t := self.obtainDREG() ; clrdx(SIZE_L,t) ; moveaxpofsdx(SIZE_W,ARXPOarx(d),ARXPOofs(d),t)
             ;    movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ENDIF
      CASE 1 ; IF ARXPOflags(d) AND MEMBF_SIGNED
             ;    t := self.obtainDREG() ;
             ;    moveaxpofsdx(SIZE_B,ARXPOarx(d),ARXPOofs(d),t) ; extbl(t)
             ;    movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ELSE
             ;    t := self.obtainDREG() ; clrdx(SIZE_L,t)
             ;    moveaxpofsdx(SIZE_B,ARXPOarx(d),ARXPOofs(d),t)
             ;    movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ENDIF
      DEFAULT ; Throw("68K", 'ARXPO size')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 4 ; moveaxpxax(SIZE_L,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg)
      CASE 0 ; leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, reg)
      CASE 2 ; IF ARXPXflags(d) AND MEMBF_SIGNED
             ;    t := self.obtainDREG() ; moveaxpxdx(SIZE_W,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0,t)
             ;    extl(t) ; movedxax(SIZE_L,t,reg) ; self.releaseDREG(t,0)
             ; ELSE
             ;    t := self.obtainDREG() ; clrdx(SIZE_L,t)
             ;    moveaxpxdx(SIZE_W,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0,t)
             ;    movedxax(SIZE_L,t,reg) ; self.releaseDREG(t,0)
             ; ENDIF
      CASE 1 ; IF ARXPXflags(d) AND MEMBF_SIGNED
             ;    t := self.obtainDREG()
             ;    moveaxpxdx(SIZE_B,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0,t) ; extbl(t)
             ;    movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ELSE
             ;    t := self.obtainDREG() ; clrdx(SIZE_L,t)
             ;    moveaxpxdx(SIZE_B,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0,t)
             ;    movedxax(SIZE_L,t,reg) ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
             ; ENDIF
      DEFAULT ; Throw("68K", 'ARXPX size')
      ENDSELECT
   DEFAULT
      Throw("68K", 'toax() o')
   ENDSELECT

   m68SetAReg(reg, o,d) -> update table

ENDPROC

PROC axto(reg, o,d:PTR TO var) OF m68amiga
   DEF t, size, ix, scale

   SELECT o
   CASE DRX       ; moveaxdx(SIZE_L,reg,d) ; m68SetDReg(d,NIL,NIL)
   CASE ARX       ; IF d <> reg
                  ;    moveaxax(SIZE_L,reg,d) ; m68SetAReg(d,NIL,NIL)
                  ; ENDIF
   CASE VAR       ; IF d.link
                  ;    movelaxaxplab(reg,d.breg,d)
                  ;    d.intreg := TRUE
                  ;    d.treg := reg+8
                  ; ELSE
                  ;    moveaxaxpofs(SIZE_L,reg, d.breg, d.offset)
                  ;    d.intreg := TRUE
                  ;    d.treg := reg+8
                  ; ENDIF
   CASE VAR64     ; reportIErr('68k ax->var64')
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 4 ; moveaxaxpofs(SIZE_L,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 2 ; moveaxaxpofs(SIZE_W,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 1 ; t := self.obtainDREG() ; moveaxdx(SIZE_L,reg,t)
             ; movedxaxpofs(SIZE_B,t,ARXPOarx(d),ARXPOofs(d))
             ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
      DEFAULT ; Throw("68K", 'ARXPO size')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 4 ; moveaxaxpx(SIZE_L,reg,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0)
      CASE 2 ; moveaxaxpx(SIZE_W,reg,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0)
      CASE 1 ; t := self.obtainDREG() ; moveaxdx(SIZE_L,reg,t)
             ; movedxaxpx(SIZE_B,t,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0)
             ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
      DEFAULT ; Throw("68K", 'ARXPX size')
      ENDSELECT
   DEFAULT
      Throw("68K", 'axto() o')
   ENDSELECT

ENDPROC

PROC tofreg(o,d:PTR TO var, reg) OF m68amiga
   DEF t, ix, size, scale

   SELECT o
   CASE DV    ; fmovesimmfpx(d, reg)
   CASE DRX   ; fmovedxfpx(FS,d,reg)
   CASE ARX   ; t := self.obtainDREG()
              ; moveaxdx(SIZE_L,d,t)
              ; fmovedxfpx(FS,t,reg)
              ; m68SetDReg(t,NIL,NIL)
              ; self.releaseDREG(t,0)
   CASE VAR   ; IF d.intreg AND d.trok AND (d.treg<8)
              ;    fmovedxfpx(FS,d.treg,reg)
              ; ELSEIF d.link=NIL
              ;    fmoveaxpofsfpx(FS,d.breg, d.offset,reg)
              ; ELSE
              ;    t := self.obtainDREG()
              ;    movelaxplabdx(d.breg,d,t)
              ;    fmovedxfpx(FS,t,reg)
              ;    m68SetDReg(t,NIL,NIL)
              ;    self.releaseDREG(t,0)
              ; ENDIF
   CASE VAR64 ; IF d.intreg AND d.trok
              ;    fmovefpxfpx(d.treg,reg)
              ; ELSEIF d.link = FALSE
              ;    fmoveaxpofsfpx(FD,d.breg,d.offset,reg)
              ; ELSE
              ;    fmoveaxplabfpx(FD,d.breg,d, reg)
              ; ENDIF
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8 ; fmoveaxpofsfpx(FD,ARXPOarx(d),ARXPOofs(d), reg)
      CASE 4 ; fmoveaxpofsfpx(FS,ARXPOarx(d),ARXPOofs(d), reg)
      CASE 2 ; reportErr('value is not suitable for float')
      CASE 1 ; reportErr('value is not suitable for float')
      CASE 0 ; reportErr('value is not suitable for float')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8 ; t := self.obtainAREG()
             ; leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
             ; fmoveaxpfpx(FD,t,reg)
             ; m68SetAReg(t,NIL,NIL) ; self.releaseAREG(t,0)
      CASE 4 ; t := self.obtainDREG()
             ; moveaxpxdx(SIZE_L,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
             ; fmovedxfpx(FS,t,reg)
             ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
      CASE 2 ; reportErr('value is not suitable for float')
      CASE 1 ; reportErr('value is not suitable for float')
      CASE 0 ; reportErr('value is not suitable for float')
      ENDSELECT
   CASE FPX
      IF d <> reg THEN fmovefpxfpx(d,reg)
   CASE D64
      fmoveaxpofsfpx(FD, FRAMEREG, d, reg)
   DEFAULT
      Throw("68K", 'tofreg() o')
   ENDSELECT

   m68SetFReg(reg, o,d) -> update table

ENDPROC

PROC fregto(reg, o,d:PTR TO var) OF m68amiga
   DEF t, size, ix, scale

   SELECT o
   CASE DRX       ; fmovefpxdx(FS,reg,d) ; m68SetDReg(d,NIL,NIL)
   CASE ARX       ; nop()
   CASE VAR       ; IF d.link
                  ;    t := self.obtainDREG()
                  ;    fmovefpxdx(FS,reg, t)
                  ;    moveldxaxplab(t,d.breg,d)
                  ;    m68SetDReg(t,NIL,NIL)
                  ;    self.releaseDREG(t,0)
                  ; ELSE
                  ;    fmovefpxaxpofs(FS,reg, d.breg, d.offset)
                  ; ENDIF
   CASE VAR64     ; IF d.link = FALSE
                  ;     fmovefpxaxpofs(FD,reg,d.breg,d.offset)
                  ; ELSE
                  ;     fmovefpxaxplab(FD,reg,d.breg,d)
                  ; ENDIF
                  ; d.intreg := TRUE
                  ; d.treg := reg
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8 ; fmovefpxaxpofs(FD,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 4 ; fmovefpxaxpofs(FS,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 2 ; reportErr('value is not suitable for float')
      CASE 1 ; reportErr('value is not suitable for float')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8 ; t := self.obtainAREG()
             ; leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
             ; fmovefpxaxp(FD,reg,t)
             ; m68SetAReg(t,NIL,NIL) ; self.releaseAREG(t,0)
      CASE 4 ; t := self.obtainDREG()
             ; fmovefpxdx(FS,reg,t)
             ; movedxaxpx(SIZE_L,t,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0)
             ; m68SetDReg(t,NIL,NIL) ; self.releaseDREG(t,0)
      CASE 2 ; reportErr('value is not suitable for float')
      CASE 1 ; reportErr('value is not suitable for float')
      ENDSELECT
   CASE FPX
      IF reg <> d THEN fmovefpxfpx(reg,d) BUT m68SetFReg(d,NIL,NIL)
   CASE D64
      fmovefpxaxpofs(FD, reg, FRAMEREG, d)
   DEFAULT
      Throw("68K", 'fregto() o')
   ENDSELECT

ENDPROC


-> 2.3
PROC tod64(o,d:PTR TO var, reg) OF m68amiga
   DEF t, ix, size, scale, t2


   SELECT o
   CASE DV
      movelimmaxpofs(IF d AND $80000000 THEN -1 ELSE 0, FRAMEREG, reg)
      movelimmaxpofs(d, FRAMEREG, reg + 4)
   CASE DRX
      movedxaxpofs(SIZE_L, d, FRAMEREG, reg + 4)
      asrl31dx(d)
      movedxaxpofs(SIZE_L, d, FRAMEREG, reg)
      moveaxpofsdx(SIZE_L, FRAMEREG, reg + 4, d) -> reload
   CASE ARX
      reportIErr('tod64->arx')
   CASE VAR
      t := self.obtainDREG()
      IF d.intreg AND d.trok
         IF d.treg <> t THEN movedxdx(SIZE_L, d.treg, t)
      ELSEIF d.link=NIL
         moveaxpofsdx(SIZE_L, d.breg, d.offset, t)
      ELSE
         movelaxplabdx(d.breg, d, t)
      ENDIF
      movedxaxpofs(SIZE_L, t, FRAMEREG, reg + 4)
      asrl31dx(t)
      movedxaxpofs(SIZE_L, t, FRAMEREG, reg)
      self.releaseDREG(t, 0)
      m68SetDReg(t, NIL,NIL)
   CASE VAR64
      t := self.obtainFREG()
      IF d.link THEN fmoveaxplabfpx(FD,d.breg,d, t) ELSE fmoveaxpofsfpx(FD,d.breg,d.offset, t)
      fmovefpxaxpofs(FD, t, FRAMEREG, reg)
      self.releaseFREG(t, 0)
      m68SetFReg(t, NIL,NIL)
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8
         t := self.obtainFREG()
         fmoveaxpofsfpx(FD,ARXPOarx(d),ARXPOofs(d), t)
         fmovefpxaxpofs(FD, t, FRAMEREG, reg)
         self.releaseFREG(t, 0)
         m68SetFReg(t, NIL,NIL)
      CASE 4
         t := self.obtainDREG()
         moveaxpofsdx(SIZE_L,ARXPOarx(d),ARXPOofs(d), t)
         movedxaxpofs(SIZE_L, t, FRAMEREG, reg + 4)
         asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
         movedxaxpofs(SIZE_L, t, FRAMEREG, reg)
         self.releaseDREG(t, 0)
         m68SetDReg(t, NIL, NIL)
      CASE 2
         reportErr('unsupported conversion')
      CASE 1
         reportErr('unsupported conversion')
      CASE 0
         reportErr('unsupported conversion')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8
         t := self.obtainAREG()
         t2 := self.obtainFREG()
         leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
         fmoveaxpfpx(FD, t, t2)
         fmovefpxaxpofs(FD, t2, FRAMEREG, reg)
         m68SetAReg(t,NIL,NIL)
         self.releaseAREG(t,0)
         self.releaseFREG(t2, 0)
         m68SetFReg(t2, NIL,NIL)
      CASE 4
         t := self.obtainDREG()
         moveaxpxdx(SIZE_L,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
         movedxaxpofs(SIZE_L, t, FRAMEREG, reg + 4)
         asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
         movedxaxpofs(SIZE_L, t, FRAMEREG, reg)
         m68SetDReg(t,NIL,NIL)
         self.releaseDREG(t,0)
      CASE 2
         reportErr('unsupported conversion')
      CASE 1
         reportErr('unsupported conversion')
      CASE 0
         reportErr('unsupported conversion')
      ENDSELECT
   CASE FPX
      fmovefpxaxpofs(FD, d, FRAMEREG, reg)
   CASE D64
      IF d <> reg
         t := self.obtainFREG()
         fmoveaxpofsfpx(FD, FRAMEREG, d, t)
         fmovefpxaxpofs(FD, t, FRAMEREG, reg)
         m68SetFReg(t, NIL,NIL)
         self.releaseFREG(t, 0)
      ENDIF
   DEFAULT
      Throw("M68", 'tod64() o')
   ENDSELECT

ENDPROC

-> 2.3
PROC d64to(reg, o,d:PTR TO var) OF m68amiga
   DEF t, size, ix, scale, t2

   #ifdef DBG_M68GEN
   DEBUGF('d64to(\d, \d, $\h)\n', reg, o, d)
   #endif

   SELECT o
   CASE DRX
      moveaxpofsdx(SIZE_L, FRAMEREG, reg + 4, d)
      m68SetDReg(d, NIL, NIL)
   CASE ARX
      reportIErr('d64to->arx')
   CASE VAR
      t := self.obtainDREG()
      moveaxpofsdx(SIZE_L, FRAMEREG, reg+4, t)
      IF d.link
         moveldxaxplab(t,d.breg,d)
      ELSE
         movedxaxpofs(SIZE_L,t, d.breg, d.offset)
      ENDIF
      m68SetDReg(t,NIL,NIL)
      self.releaseDREG(t,0)
   CASE VAR64
      t := self.obtainFREG()
      fmoveaxpofsfpx(FD, FRAMEREG, reg, t)
      IF d.link = FALSE
         fmovefpxaxpofs(FD,t,d.breg,d.offset)
      ELSE
         fmovefpxaxplab(FD,t,d.breg,d)
      ENDIF
      m68SetFReg(t, NIL,NIL)
      self.releaseFREG(t, 0)
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8
         t := self.obtainFREG()
         fmoveaxpofsfpx(FD, FRAMEREG, reg, t)
         fmovefpxaxpofs(FD,t,ARXPOarx(d),ARXPOofs(d))
         self.releaseFREG(t, 0)
         m68SetFReg(t, NIL, NIL)
      CASE 4
         t := self.obtainDREG()
         moveaxpofsdx(SIZE_L, FRAMEREG, reg+4, t)
         movedxaxpofs(SIZE_L,t,ARXPOarx(d),ARXPOofs(d))
         self.releaseDREG(t, 0)
         m68SetDReg(t, NIL, NIL)
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8
         t := self.obtainAREG()
         t2 := self.obtainFREG()
         fmoveaxpofsfpx(FD, FRAMEREG, reg, t2)
         leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
         fmovefpxaxp(FD,t2,t)
         m68SetAReg(t,NIL,NIL)
         m68SetFReg(t2,NIL,NIL)
         self.releaseFREG(t2,0)
         self.releaseAREG(t,0)
      CASE 4
         moveaxpofsaxpx(SIZE_L,FRAMEREG, reg, ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0)
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE FPX
      fmoveaxpofsfpx(FD, FRAMEREG, reg, d)
      m68SetFReg(d,NIL,NIL)
   CASE D64
      IF reg <> d
         t := self.obtainFREG()
         fmoveaxpofsfpx(FD, FRAMEREG, reg, t)
         fmovefpxaxpofs(FD, t, FRAMEREG, d)
         m68SetFReg(t, NIL,NIL)
         self.releaseFREG(t, 0)
      ENDIF
   CASE X2R
      fmoveaxpofsfpx(FD, FRAMEREG, reg, d)
      m68SetFReg(d,NIL,NIL)
   DEFAULT
      Throw("68K", 'd64to() o')
   ENDSELECT

ENDPROC


PROC x2rto(reg, o,d:PTR TO var) OF m68amiga
   DEF t, size, ix, scale

   SELECT o
   CASE DRX
      fmovefpxaxpofs(FD,reg,STACKREG,-8)
      moveaxpofsdx(SIZE_L, STACKREG,-4,d)
      m68SetDReg(d,NIL,NIL)
   CASE ARX
      reportIErr('x2rtp->arx')
   CASE VAR
      t := self.obtainDREG()
      fmovefpxaxpofs(FD,reg,STACKREG,-8)
      moveaxpofsdx(SIZE_L, STACKREG,-4,t)
      IF d.link
         moveldxaxplab(t,d.breg,d)
      ELSE
         movedxaxpofs(SIZE_L,t, d.breg, d.offset)
      ENDIF
      self.releaseDREG(t,0)
      m68SetDReg(t,NIL,NIL)
   CASE VAR64
      IF d.link = FALSE
         fmovefpxaxpofs(FD,reg,d.breg,d.offset)
      ELSE
         fmovefpxaxplab(FD,reg,d.breg,d)
      ENDIF
      d.intreg := FALSE
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8
         fmovefpxaxpofs(FD,reg,ARXPOarx(d),ARXPOofs(d))
      CASE 4
         fmovefpxaxpofs(FD, reg, STACKREG, -8)
         moveaxpofsaxpofs(SIZE_L, STACKREG, -4, ARXPOarx(d), ARXPOofs(d))
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8
         t := self.obtainAREG()
         leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
         fmovefpxaxp(FD,reg,t)
         m68SetAReg(t,NIL,NIL) ; self.releaseAREG(t,0)
      CASE 4
         fmovefpxaxpofs(FD, reg, STACKREG, -8)
         moveaxpofsaxpx(SIZE_L, STACKREG, -4, ARXPXarx(d), ARXPXidrx(d), ARXPXscale(d), 0)
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE FPX
      IF reg <> d THEN fmovefpxfpx(reg,d) BUT m68SetFReg(d,NIL,NIL)
   CASE D64
      fmovefpxaxpofs(FD, reg, FRAMEREG, d)
   CASE X2R
      IF reg <> d THEN fmovefpxfpx(reg,d) BUT m68SetFReg(d,NIL,NIL)
   DEFAULT
      Throw("68K", 'x2rto() o')
   ENDSELECT

ENDPROC

PROC tox2r(o,d:PTR TO var, reg) OF m68amiga
   DEF t, ix, size, scale

   SELECT o
   CASE DV
      movelimmaxpofs(IF d AND $80000000 THEN -1 ELSE 0, STACKREG, -8)
      movelimmaxpofs(d, STACKREG, -4)
      fmoveaxpofsfpx(FD, STACKREG, -8, reg)
   CASE DRX
      movedxaxpd(SIZE_L, d, STACKREG)
      asrl31dx(d)
      movedxaxpd(SIZE_L, d, STACKREG)
      moveaxpofsdx(SIZE_L, STACKREG, 4, d) ->reload
      fmoveaxpifpx(FD, STACKREG, reg)
   CASE ARX
      reportIErr('tox2r arx')
   CASE VAR
      t := self.obtainDREG()
      IF d.intreg AND d.trok AND (d.treg<8)
         movedxaxpd(SIZE_L, d.treg, STACKREG)
         movedxdx(SIZE_L, d.treg, t)
         asrl31dx(t) 
         movedxaxpd(SIZE_L,t, STACKREG)
         fmoveaxpifpx(FD, STACKREG, reg)
      ELSE
         IF d.link
            movelaxplabdx(d.breg, d, t)
         ELSE
            moveaxpofsdx(SIZE_L, d.breg,d.offset,t)
         ENDIF
         movedxaxpd(SIZE_L, t, STACKREG)
         asrl31dx(t) 
         movedxaxpd(SIZE_L, t, STACKREG)
         fmoveaxpifpx(FD, STACKREG, reg)
      ENDIF
      self.releaseDREG(t, 0)
      m68SetDReg(t, NIL, NIL)
   CASE VAR64 ; IF d.intreg AND d.trok
              ;    fmovefpxfpx(d.treg,reg)
              ; ELSEIF d.link = FALSE
              ;    fmoveaxpofsfpx(FD,d.breg,d.offset,reg)
              ; ELSE
              ;    fmoveaxplabfpx(FD,d.breg,d, reg)
              ; ENDIF
   CASE ARXPO
      size := ARXPOsize(d)
      SELECT size
      CASE 8
         fmoveaxpofsfpx(FD,ARXPOarx(d),ARXPOofs(d), reg)
      CASE 4
         t := self.obtainDREG()
         moveaxpofsdx(SIZE_L,ARXPOarx(d),ARXPOofs(d), t)
         movedxaxpd(SIZE_L, t, STACKREG)
         asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
         movedxaxpd(SIZE_L, t, STACKREG)
         fmoveaxpifpx(FD, STACKREG, reg)
         m68SetDReg(t,NIL,NIL)
         self.releaseDREG(t,0)
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      CASE 0 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE ARXPX
      size := ARXPXsize(d)
      SELECT size
      CASE 8
         t := self.obtainAREG()
         leaaxpxax(ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d),0, t)
         fmoveaxpfpx(FD,t,reg)
         m68SetAReg(t,NIL,NIL)
         self.releaseAREG(t,0)
      CASE 4
         t := self.obtainDREG()
         moveaxpxdx(SIZE_L,ARXPXarx(d),ARXPXidrx(d),ARXPXscale(d), 0, t)
         movedxaxpd(SIZE_L, t, STACKREG)
         asrl31dx(t) ->asrimm5dx(SIZE_L, 31, t)
         movedxaxpd(SIZE_L, t, STACKREG)
         fmoveaxpifpx(FD, STACKREG, reg)
         m68SetDReg(t,NIL,NIL)
         self.releaseDREG(t,0)
      CASE 2 ; reportErr('unsupported conversion')
      CASE 1 ; reportErr('unsupported conversion')
      CASE 0 ; reportErr('unsupported conversion')
      ENDSELECT
   CASE FPX
      IF d <> reg THEN fmovefpxfpx(d,reg)
   CASE D64
      fmoveaxpofsfpx(FD, FRAMEREG, d, reg)
   CASE X2R
      IF d <> reg THEN fmovefpxfpx(d,reg)
   DEFAULT
      Throw("68K", 'tox2r() o')
   ENDSELECT

   m68SetFReg(reg, o,d) -> update table

ENDPROC

-> v44.. o2:DRX/DVAL only
-> v58: o2:DRX/DVAL/D64 only
PROC private_cmp(o1,d1:PTR TO var,o2,d2,cond) OF m68amiga
   DEF t, reverse=FALSE

   IF o2 = DRX
      SELECT o1
      CASE DRX    ; cmpdxdx(SIZE_L,d1,d2)
      CASE ARX    ; cmpaxdx(SIZE_L,d1,d2)
      CASE VAR    ; IF d1.link
                  ;    cmplaxplabdx(d1.breg,d1,d2)
                  ; ELSE
                  ;    cmpaxpofsdx(SIZE_L,d1.breg,d1.offset,d2)
                  ; ENDIF
      CASE DV     ; cmplimmdx(d1,d2)
      DEFAULT
         t := self.obtainDREG()
         inst_copy(o1,d1,DRX,t)
         cmpdxdx(SIZE_L,t, d2)
         self.releaseDREG(t,0)
      ENDSELECT
   ELSEIF o2 = DV
      reverse := TRUE
      SELECT o1
      CASE DRX    ; cmplimmdx(d2,d1)
      CASE ARX    ; cmplimmax(d2,d1)
      CASE VAR    ; IF d1.link
                  ;    cmplimmaxplab(d2,d1.breg,d1)
                  ; ELSE
                  ;    cmplimmaxpofs(d2,d1.breg,d1.offset)
                  ; ENDIF
      CASE DV     ; t := self.obtainDREG()
                  ; movelimmdx(d1,t)
                  ; cmplimmdx(d2,t)
                  ; self.releaseDREG(t,0)
      CASE D64 -> 2.3
         reverse := FALSE
         IF d2 = NIL
            self.test64(d1)
         ELSE
            t := self.obtainD64()
            self.tod64(o2,d2, t)
            self.compare64(d1, t)
            self.releaseD64(t, 0)
         ENDIF
      DEFAULT
         t := self.obtainDREG()
         inst_copy(o1,d1,DRX,t)
         cmplimmdx(d2,t)
         self.releaseDREG(t,0)
      ENDSELECT
   ELSEIF o2 = D64 -> 2.3
      t := self.obtainD64()
      self.tod64(o1,d1, t)
      self.compare64(t, d2)
      self.releaseD64(t, 0)
   ELSE
      reportIErr('m68amiga.priv_cmp o2 ?')
   ENDIF

ENDPROC reverse

-> v58. ptr comes on stack now
PROC doMethod(n:PTR TO item, as, as2, postkey, type:PTR TO member) OF m68amiga
   DEF t:PTR TO item, meth:PTR TO proc, mid
   DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg
   DEF it_funccall:PTR TO it_funccall, hln:PTR TO hln
   DEF ro, rd, arx=-1

   #ifdef DBG_PPC
   DEBUGF('doMethod($\h,\d,\d,\d,$\h)\n', n, as,as2,postkey,type)
   #endif

   IF type.object = NIL THEN reportErr('only objects have methods')


      t := n
      it_funccall := n.info
      hln := it_funccall.data
      n := it_funccall + SIZEOF it_funccall

      IF postkey = KW_SUPER
         IF type.object.super = NIL THEN reportErr('object has no super class', type.object.name)
         meth, mid := findMethod(type.object.super, hln.name)
      ELSE
         meth, mid := findMethod(type.object, hln.name)
      ENDIF

      IF meth = NIL  THEN reportErr('unknown method', hln.name)

      IF postkey = KW_NEW
         ro := as
         rd := as2
      ELSE
         ro := meth.mret.ros[0]
         rd := meth.mret.rds[0]
      ENDIF

      IF meth.flags AND PROCF_CLMETH -> v47
         IF postkey = KW_NEW THEN reportErr('cannot NEW/END class method')
         self.saveObtained(regusecopy)
         n := self.doProcfunc(t,meth)
         inst_pop(4, AREG,meth.selfreg)
         IF meth.offset
            inst_goslab(meth)
         ELSE -> module -> module
            inst_labadr(type.object, ATEMP)
            inst_copy(ARXPO,AxSizeOfs(ATEMP,4,mid*4), AREG, ATEMP)
            inst_gosarx(ATEMP)
         ENDIF
         ro, rd := self.secureReturn(ro, rd, regusecopy)
         inst_incarx(STACKREG, meth.nrofargs*4)
         self.loadObtained(regusecopy)
         clearRegs()
         RETURN n, ro, rd
      ENDIF

      /* v2.5 */
      IF postkey=KW_NEW
         IF (type.numes > 0) THEN reportErr('cannot NEW this')
         arx := self.newClassObject(type.object)
         inst_copy(AREG,arx, as,as2)
      ELSEIF postkey=KW_END
         reportErr('cannot END this')
      ENDIF

      self.saveObtained(regusecopy)

      n := self.doProcfunc(t, meth)

      inst_copy(as,as2, AREG,meth.selfreg)

      IF postkey = KW_SUPER
         /* get super-classinfo */
         inst_labadr(type.object.super, ATEMP) -> R11/A6
      ELSE
         /* get classinfo */
         inst_copy(ARXPO, AxSizeOfs(meth.selfreg, 4, OBJECT_CLASSINFO), AREG,ATEMP)
      ENDIF

      /* get method */
      inst_copy(ARXPO, AxSizeOfs(ATEMP, 4, mid*4), AREG,ATEMP)

      /* call method */
      inst_gosarx(ATEMP)

      inst_incarx(STACKREG,meth.nrofargs*4)

      ro, rd := self.secureReturn(ro, rd, regusecopy)

      self.loadObtained(regusecopy)
      clearRegs()

   #ifdef DBG_CODEGEN
   DEBUGF('doMethod done\n')
   #endif



ENDPROC n, ro, rd-> v58

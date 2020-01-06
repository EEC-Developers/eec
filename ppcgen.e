
-> ECX/ppcgen.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT PREPROCESS
OPT LARGE

-> May 2008, ppcgen.e created, extracted from codegen.e.

->#define DBG_PPCGEN

MODULE '*codegen'
MODULE '*compiler'
MODULE '*support'
MODULE '*common'
MODULE '*assembler'
MODULE '*opcodes68'
MODULE '*opcodesppc'
MODULE '*binary'
MODULE '*inlineppc'
MODULE '*runtime'

MODULE 'exec/lists'

#define PPCEQ 12,2
#define PPCNE 4,2
#define PPCLT 12,0
#define PPCGT 12,1
#define PPCLE 4,1
#define PPCGE 4,0

#define DOSICBICOPTI
#define USEMULDIVSHIFT


EXPORT DEF g_codeptr:PTR TO LONG,
	        g_codebuf,
	        g_currentproc:PTR TO proc,
	        g_rwreflist:PTR TO rwref,
	        g_globalsize,
	        g_databuf:PTR TO LONG, -> for rw init
	        g_databufsize,
	        g_numregalloc,
	        g_numfregalloc,
	        g_stacksize,
	        g_gvarlist:PTR TO gvar,
	        g_regusetab:PTR TO oreg,
	        g_modulelist:PTR TO mlh, -> for glob/rw init
	        link_globaldatasize -> for glob init

EXPORT DEF g_od:PTR TO LONG, g_punct -> shared with inline.e
EXPORT DEF g_lastx:PTR TO lastx

DEF g_exceptlab:PTR TO codelab
DEF g_endexceptlab:PTR TO codelab
DEF g_procendlab:PTR TO codelab

DEF g_localstackoffset:PTR TO LONG -> ppc only
DEF g_localstackstart -> ppc only

-> v2.3
DEF g_d64bottom, g_lastd64

EXPORT DEF g_optroundnear, g_optmodule,g_optosid

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

EXPORT DEF g_sizeofptr
EXPORT DEF g_multireturn:PTR TO multireturn

EXPORT OBJECT ppc OF codegen
	lasttempREG
	lasttempFREG
	lasttempVREG
	tempvecstackofs -> v48 for vectors starting at localstack-bottom.. not used yet
	p_esoffset  -> exceptstruct offset on stack
	lif_private_i2f:PTR TO lif -> v51 points to I_2_F lif
	-> 1.10.0
	lif_private_d642f:PTR TO lif
	lif_private_f2d64:PTR TO lif
	lif_private_shl64:PTR TO lif
	lif_private_shr64:PTR TO lif
	lif_private_asr64:PTR TO lif
	lif_private_div64:PTR TO lif
ENDOBJECT

EXPORT OBJECT ppcsysv OF ppc
ENDOBJECT

PROC init() OF ppcsysv
	DREG := RX
	AREG := RX
	FREG := FPX
	VREG := VX
	D64REG := D64

	IREG0 := 3  -> r3
	IREG1 := 4  -> r4
	FREG0 := 1

	STACKREG := 1  -> stack
	SELFREG  := 12 -> self arg
	GLOBREG  := 13 -> global reg
	FRAMEREG := 1  -> frameptr (same as stack)

	ATEMP   := 11  -> non obtainable ptr scratch
	DTEMP   := 12  -> v50, same as selfreg
	FTEMP   := 0   -> v50
	D64TEMP := 0   -> is set by self.doprochead()

	self.lasttempREG := 3
	self.lasttempFREG := 1
	self.lasttempVREG := 2


ENDPROC

PROC putInit(globalsize, ivarsize) OF ppc
	#ifdef DBG_PPCGEN
	DEBUGF('putPPCInit()\n')
	#endif
	ppcmfspr(12,8) -> mflr r12
	putRWInit()
	ppcaddi(3,3,ivarsize)
	ppcor(3,GLOBREG,3,0) -> R13 is set up!
	ppcaddi(3,3,globalsize)
	self.putGlobInit()
	putModulesInit()
	ppcmtspr(12,8) -> mtlr r12
	ppcbclr(20,0,0) -> blr, returns endofglobdata in r3
ENDPROC

PROC putModulesInit()
	DEF mod:PTR TO moduleheader, info:PTR TO modinfo, t
	mod := g_modulelist.head
	WHILE mod.succ
	   info := IF mod.headsize < 160 THEN NIL ELSE mod.globinitinfo
	   IF info
	      IF mod.cpu <> CPU_PPC THEN reportErr('init code cpu mismatch for module', mod.mname)
	      info := info + mod
	      t := Div(info.misc - currentOffset(), 4)
	      ppcb(t, 0, 1) -> bl
	   ENDIF
	   mod := mod.succ
	ENDWHILE
ENDPROC

PROC putRWInit()
	DEF mod:PTR TO moduleheader
	DEF offset:PTR TO LONG
	DEF info:PTR TO modinfo
	DEF a
	DEF t
	DEF lptr:PTR TO LONG
	-> init rw-data !
	ppcaddi(4,0,-1)
	ppcaddi(5,0,1)
	ppcaddi(6,0,2)
	ppcaddi(7,0,3)
	ppcaddi(8,0,4)
	ppcaddi(9,0,8)
	offset := 0
	mod := g_modulelist.head
	WHILE mod.succ
	   info := mod.datainfo
	   IF info
	      info := info + mod
	      lptr := info + SIZEOF modinfo
	      a := info.count+1
	      WHILE a--
	         t := lptr[]++
	         SELECT t
	         CASE 0  ; offset++
	         CASE -1 ; ppcstw(4,3,offset++)
	         CASE 1  ; ppcstw(5,3,offset++)
	         CASE 2  ; ppcstw(6,3,offset++)
	         CASE 3  ; ppcstw(7,3,offset++)
	         CASE 4  ; ppcstw(8,3,offset++)
	         CASE 8  ; ppcstw(9,3,offset++)
	         DEFAULT
	            IF t AND $FFFF = NIL
	               ppcaddis(10,0,Shr(t,16) AND $FFFF)
	            ELSE
	               ppcliw(10,t)
	            ENDIF
	            ppcstw(10,3,offset++)
	         ENDSELECT
	         IF offset = $7FFC
	            ppcaddi(3,3,$7FFC)
	            offset := 0
	         ENDIF
	      ENDWHILE
	   ENDIF
	   mod := mod.succ
	ENDWHILE


	lptr := g_databuf
	a := Shr(g_databufsize, 2) + 1
	WHILE a--
	   t := lptr[]++
	   SELECT t
	   CASE 0  ; offset++
	   CASE -1 ; ppcstw(4,3,offset++)
	   CASE 1  ; ppcstw(5,3,offset++)
	   CASE 2  ; ppcstw(6,3,offset++)
	   CASE 3  ; ppcstw(7,3,offset++)
	   CASE 4  ; ppcstw(8,3,offset++)
	   CASE 8  ; ppcstw(9,3,offset++)
	   DEFAULT
	      IF t AND $FFFF = NIL
	         ppcaddis(10,0,Shr(t,16) AND $FFFF)
	      ELSE
	         ppcliw(10,t)
	      ENDIF
	      ppcstw(10,3,offset++)
	   ENDSELECT
	   IF offset = $7FFC
	      ppcaddi(3,3,$7FFC)
	      offset := 0
	   ENDIF
	ENDWHILE
	ppcaddi(3,3,offset)
ENDPROC



-> 2.2 now called directly from main if in module mode
PROC putGlobInit() OF ppc
	DEF gvar:PTR TO gvar
	-> initialise user globals
	gvar := g_gvarlist
	WHILE gvar
	   link_globaldatasize := link_globaldatasize + initGvar(gvar)
	   gvar := gvar.next
	ENDWHILE
	IF g_optmodule THEN ppcbclr(20,0,0) -> blr
ENDPROC

#define ToGlobal(r) IF gvar.link THEN ppcstwlab(r,GLOBREG,gvar) ELSE ppcstw(r,GLOBREG,gvar.offset)
#define ToGlobalD(r) IF gvar.link THEN ppcstfdlab(r,GLOBREG,gvar) ELSE ppcstfd(r,GLOBREG,gvar.offset)

PROC initGvar(gvar:PTR TO gvar)
	DEF t, t2, globaldatasize=0
	   IF gvar.type.numes
	      SELECT 256 OF gvar.type.esize
	      CASE 1
	         IF gvar.cmplx  -> STRING
	            globaldatasize := 4 + (t := gvar.type.numes + 1 + 3 AND $FFFFFC)
	            ppcaddis(0,0,gvar.type.numes)
	            ppcstw(0,3,0)
	            ppcaddi(3,3,4)
	            ToGlobal(3)
	            -> memory is cleared already ->ppcstb(0,3,4) -> nilterm
	            ppcaddi(3,3,t)
	         ELSE           -> ARRAY OF CHAR
	            globaldatasize := (t := gvar.type.numes + 3 AND $FFFFFC)
	            ToGlobal(3)
	            ppcaddiw(3,3,t)
	         ENDIF
	      CASE 2   -> ARRAY OF INT
	         globaldatasize :=  (t := gvar.type.numes * 2 + 3 AND $FFFFFC)
	         ToGlobal(3)
	         ppcaddiw(3,3,t)
	      CASE 4
	         IF gvar.cmplx  -> LIST
	            globaldatasize := 4 + (t := gvar.type.numes * 4)
	            ppcaddis(0,0,gvar.type.numes)
	            ppcstw(0,3,0)
	            ppcaddi(3,3,4)
	            ToGlobal(3)
	            ppcaddiw(3,3,t)
	         ELSE           -> ARRAY OF LONG
	            globaldatasize := (t := gvar.type.numes * 4)
	            ToGlobal(3)
	            ppcaddiw(3,3,t)
	         ENDIF
	      CASE 8
	            globaldatasize := (t := gvar.type.numes * 8)
	            ToGlobal(3)
	            ppcaddiw(3,3,t)
	      CASE 255 -> (ARRAY OF) OBJECT
	         IF gvar.type.object.nrofmethods THEN addWarning('methods will not be installed for global')
	         globaldatasize := (t := gvar.type.numes *
	                              gvar.type.object.sizeof + 3 AND $FFFFFC)
	         ToGlobal(3)
	         ppcaddiw(3,3,t)
	      ENDSELECT
	   ELSEIF gvar.defo
	      IF gvar.type.size = 4
	         ppcliw(10,gvar.defd)
	         ToGlobal(10)
	      ELSEIF gvar.type.flags AND MEMBF_FLOAT -> double
	         ppcliw(10,gvar.defd)
	         ppcstw(10,1,-4)
	         ppclfs(0,1,-4)
	         ToGlobalD(0)
	      ELSE -> 2.2: forgotten WIDE
	         ppcliw(10,gvar.defd)
	         ppcstw(10, 1, -4)
	         ppcsrawi(10, 10, 31, 0)
	         ppcstw(10, 1, -8)
	         ppclfd(0, 1, -8)
	         ToGlobalD(0)
	      ENDIF
	   ENDIF
ENDPROC globaldatasize

PROC abiArgs(array:PTR TO LONG, numargs, flags) OF ppcsysv
	DEF va:PTR TO arg, a, rnum=3, fnum=1, sofs=8:PTR TO LONG, vnum=2
	FOR a := 0 TO numargs-1
	   va := array[a]
	   IF va.type.size = 4
	      IF rnum < 11
	         va.rtype := PPCARGRTYPE_RX
	         va.rnum := rnum++
	      ELSE
	         va.rtype := PPCARGRTYPE_STACKRX
	         va.rnum := sofs++
	      ENDIF
	   ELSEIF va.type.size = 8
	      IF va.type.flags AND MEMBF_FLOAT
	         IF fnum < 9
	            va.rtype := PPCARGRTYPE_FX
	            va.rnum := fnum++
	         ELSE
	            reportErr('too many REAL arguments for procedure (bug author)')
	         ENDIF
	      ELSE -> 1.10.0
	         IF rnum > 9
	            va.rtype := PPCARGRTYPE_STACKRX2
	            va.rnum := sofs++
	            sofs++
	         ELSE
	            va.rtype := PPCARGRTYPE_RX2
	            va.rnum := rnum
	            rnum := rnum + 2
	         ENDIF
	      ENDIF
	   ELSEIF va.type.size = 16
	      IF vnum < 13
	         va.rtype := PPCARGRTYPE_VX
	         va.rnum := vnum++
	      ELSE
	         reportErr('too many VECTOR arguments for procedure (bug author)')
	      ENDIF
	   ENDIF
	ENDFOR

ENDPROC

PROC abiReturns(mret:PTR TO multireturn) OF ppcsysv
	DEF reg=3, freg=1, vreg=2, t
	t := mret.ros[0]
	SELECT t
	CASE DREG ; mret.rds[0] := reg++
	CASE FREG ; mret.rds[0] := freg++
	CASE VREG ; mret.rds[0] := vreg++
	CASE X2R  ; mret.rds[0] := reg++ ; reg++  -> 1.10.0
	DEFAULT   ; reportIErr('ppcsysv.abiReturns 0 t=?')
	ENDSELECT
	t := mret.ros[1]
	SELECT t
	CASE DREG ; mret.rds[1] := reg++
	CASE FREG ; mret.rds[1] := freg++
	CASE VREG ; mret.rds[1] := vreg++
	CASE X2R  ; mret.rds[1] := reg++ ; reg++  -> 1.10.0
	DEFAULT   ; reportIErr('ppcsysv.abiReturns 1 t=?')
	ENDSELECT
	t := mret.ros[2]
	SELECT t
	CASE DREG ; mret.rds[2] := reg++
	CASE FREG ; mret.rds[2] := freg++
	CASE VREG ; mret.rds[2] := vreg++
	CASE X2R  ; mret.rds[2] := reg++ ; reg++  -> 1.10.0
	DEFAULT   ; reportIErr('ppcsysv.abiReturns 2 t=?')
	ENDSELECT
	t := mret.ros[3]
	SELECT t
	CASE DREG ; mret.rds[3] := reg++
	CASE FREG ; mret.rds[3] := freg++
	CASE VREG ; mret.rds[3] := vreg++
	CASE X2R  ; mret.rds[3] := reg++ ; reg++  -> 1.10.0
	DEFAULT   ; reportIErr('ppcsysv.abiReturns 3 t=?')
	ENDSELECT
ENDPROC

-> v50, now uses new parameterpassing
-> v55, wide args
PROC doProcfunc(buf:PTR TO item, meth=NIL:PTR TO proc) OF ppc
	DEF n:PTR TO item, hln:PTR TO hln, proc:PTR TO proc
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg -> v44, 49
	DEF a, as, as2, param:PTR TO var
	DEF stub:PTR TO codelab, endstub:PTR TO codelab
	DEF nrofparams, t:PTR TO LONG
	DEF o,d, var:PTR TO var, float
	DEF argtype
	DEF it_funccall:PTR TO it_funccall
	DEF ro, rd
	DEF ha:hintargs, noraise

	ha.hregisvar := FALSE

	it_funccall := buf.info
	hln := it_funccall.data ->n.info
	nrofparams := it_funccall.numpars ->buf.num
	n := it_funccall + SIZEOF it_funccall ->n++ -> skip label


	#ifdef DBG_PPCGEN
	DEBUGF('dofunction() \s \n', hln.name)
	#endif

	IF meth = NIL
	   self.saveObtained(regusecopy)
	   proc := hln.ident
	   IF proc = NIL THEN reportErr('unknown function', hln.name)
	   proc.referenced := 1
	ELSE
	   proc := meth
	ENDIF

	IF nrofparams > proc.nrofargs THEN reportErr('too many parameters for procedure', proc.name)
	IF (nrofparams+proc.nrofdefaults) < proc.nrofargs THEN reportErr('too few parameters for procedure', proc.name)

	/* evaluate expressions and put on stack or in register */
	FOR a := 0 TO nrofparams-1
	   param := proc.argarray[a]
	   IF param.rtype = PPCARGRTYPE_STACKRX  -> put in parameter store area ?
	      n, o, d := self.doExpression(n)
	      inst_copy(o,d, DREG,DTEMP)
	      inst_copy(DREG,DTEMP, ARXPO,AxSizeOfs(1,PTRSIZE,param.rnum))
	   ELSEIF param.rtype = PPCARGRTYPE_RX
	      ha.hregop := DREG
	      ha.hregnum := param.rnum
	      n, o, d := self.doExpression(n, ha)
	      inst_copy(o,d, RX,param.rnum)
	      self.copyObtainToDREG(RX,param.rnum) -> put obtain on register
	   ELSEIF param.rtype = PPCARGRTYPE_FX
	      ha.hregop := FREG
	      ha.hregnum := param.rnum
	      n, o, d := self.doExpression(n, ha)
	      inst_copy(o,d, FPX,param.rnum)
	      self.copyObtainToFREG(FPX,param.rnum) -> put obtain on register
	   ELSEIF param.rtype = PPCARGRTYPE_RX2 -> 2.0.0 WIDE/X2R
	      n, o, d := self.doExpression(n)
	      ->inst_copy(o,d, D64,D64TEMP)
	      ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), RX, param.rnum)
	      ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), RX, param.rnum+1)
	      self.tox2r(o,d,param.rnum)
	      self.copyObtainToDREG(RX,param.rnum) -> put obtain on register
	      self.copyObtainToDREG(RX,param.rnum+1) -> put obtain on register
	   ELSEIF param.rtype = PPCARGRTYPE_STACKRX2
	      n, o, d := self.doExpression(n)
	      inst_copy(o,d, D64,param.rnum) -> clever eh !
	   ENDIF
	ENDFOR


	-> should we put some defaults ?
	IF  proc.nrofargs > nrofparams
	   a := nrofparams
	   WHILE a < proc.nrofargs
	      param := proc.argarray[a]
	      IF param.rtype = PPCARGRTYPE_RX
	         inst_copy(DV,param.defd, RX,param.rnum)
	         self.copyObtainToDREG(RX,param.rnum) -> put an obtain on register
	      ELSEIF param.rtype = PPCARGRTYPE_FX
	         inst_copy(DV,param.defd, FPX,param.rnum)
	         self.copyObtainToFREG(FPX,param.rnum) -> put an obtain on register
	      ELSEIF param.rtype = PPCARGRTYPE_STACKRX
	         inst_copy(DV,param.defd, RX,DTEMP)
	         inst_copy(RX,DTEMP, ARXPO,AxSizeOfs(1,PTRSIZE,param.rnum))
	      ELSEIF param.rtype = PPCARGRTYPE_RX2
	         ->inst_copy(DV,param.defd, D64, D64TEMP)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), RX, param.rnum)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), RX, param.rnum+1)
	         self.tox2r(DV,param.defd,param.rnum)
	         self.copyObtainToDREG(RX,param.rnum) -> put obtain on register
	         self.copyObtainToDREG(RX,param.rnum+1) -> put obtain on register
	      ELSEIF param.rtype = PPCARGRTYPE_STACKRX2
	         inst_copy(DV,param.defd, RX,DTEMP)
	         inst_copy(RX,DTEMP, D64,param.rnum) -> clever eh !
	      ENDIF
	      a++
	  ENDWHILE
	ENDIF

	FOR a := nrofparams-1 TO 0 STEP -1
	   param := proc.argarray[a]
	   IF param.rtype = PPCARGRTYPE_RX
	      self.releaseDREG(param.rnum,0)
	   ELSEIF param.rtype = PPCARGRTYPE_FX
	      self.releaseFREG(param.rnum,0)
	   ELSEIF param.rtype = PPCARGRTYPE_RX2
	      self.releaseDREG(param.rnum,0)
	      self.releaseDREG(param.rnum+1,0)
	   ENDIF
	ENDFOR

	IF meth = NIL
	   IF proc.cpu = CPU_PPC
	      inst_goslab(proc)
	   ELSE  -> ppc calling 68k ! (max 8 params)
	      IF g_optosid <> OSID_MORPHOS
	         reportErr('automagic ppc->68k calling not implemented for target', proc.name)
	      ENDIF
	      -> 68k passing out exception currently a problem
	      -> copy into emulhandle regs
	      FOR a := 0 TO proc.nrofargs-1
	         param := proc.argarray[a]
	         ppcstw(param.rnum,2,a*4)
	      ENDFOR
	      -> copy r13 into emulhandle.a4
	      ppcstw(13,2,48)
	      -> do the twist
	      endstub := newLabel()
	      ppcblab(endstub,0,1) -> skip 68k stubcode
	      -> stub: from 68k regs to stack as params for procedure
	      FOR a := 0 TO proc.nrofargs-1 DO IF a > 7 THEN moveaxaxpd(SIZE_L,a-8,7) ELSE movedxaxpd(SIZE_L,a,7)
	      bsrlab(proc)
	      leaaxpofsax(7,proc.nrofargs*4,7)
	      rts_()
	      putAlign(4) -> align 4
	      putLabel(endstub)
	      ppcmfspr(3,8) -> to r3
	      ppclwz(0,2,104) -> emulcalldirect68k to r0
	      ppcmtspr(0,8) -> lr
	      ppcbclr(20,0,1)
	   ENDIF

	   ro := proc.mret.ros[0]
	   rd := proc.mret.rds[0]

	   ro, rd := self.secureReturn(ro, rd, regusecopy) -> v50
	   self.loadObtained(regusecopy)
	   clearRegs()


	   #ifdef DBG_PPCGEN
	   DEBUGF('dofunction() \s DONE ro=\d rd=\h\n', hln.name, ro, rd)
	   #endif
	ELSE
	   #ifdef DBG_PPCGEN
	   DEBUGF('dofunction() (method) \s DONE\n', hln.name)
	   #endif
	ENDIF

	n := buf[1]

	-> 2.2
	IF proc.raise
	   noraise := newLabel()
	   inst_bic(DV,proc.raise.trigval,proc.raise.condition, ro,rd,noraise)
	   inst_copy(DV,proc.raise.excval, DREG,IREG0)
	   ppcblab(self.raise_lif,0,0)
	   def_label(noraise)
	ENDIF

	g_multireturn := proc.mret

ENDPROC n, ro, rd

-> 1.9.0, added support for new style varfunc
PROC doVarfunc(n:PTR TO item) OF ppc
	DEF v:PTR TO var, r, count=0
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg
	DEF o, d, t
	DEF it_funccall:PTR TO it_funccall
	DEF ro, rd
	DEF ha:hintargs
	DEF arg:PTR TO arg, a

	ha.hregisvar := FALSE

	r := n[1]
	it_funccall := n.info
	v := it_funccall.data
	n := it_funccall + SIZEOF it_funccall

	#ifdef DBG_PPCGEN
	DEBUGF('dovfunction() \s \n', v.hln.name)
	#endif

	ro := RX
	rd := IREG0

	self.saveObtained(regusecopy)

	IF v.argsarray = FALSE

	   /* evaluate expressions and put in regs or on stack */
	   WHILE (t := n.data)
	      IF count > 7  -> put in parameter store area ?
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, ARXPO,AxSizeOfs(1,PTRSIZE,count-8*4+8))
	      ELSE
	         ha.hregop := RX
	         ha.hregnum := count+3
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, RX,count+3)
	         self.copyObtainToDREG(RX,count+3) -> put an obtain on register
	      ENDIF
	      count++
	   ENDWHILE

	   WHILE count
	      count--
	      IF count <= 7 THEN self.releaseDREG(count+3, 0)-> release regs
	   ENDWHILE

	ELSE -> 1.9.0


	    /* evaluate expressions and put on stack or in register */
	   FOR a := 0 TO it_funccall.numpars-1
	      arg := v.argsarray[a]
	      IF arg.rtype = PPCARGRTYPE_STACKRX  -> put in parameter store area ?
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, DREG,DTEMP)
	         inst_copy(DREG,DTEMP, ARXPO,AxSizeOfs(1,PTRSIZE,arg.rnum))
	      ELSEIF arg.rtype = PPCARGRTYPE_RX
	         ha.hregop := DREG
	         ha.hregnum := arg.rnum
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, RX,arg.rnum)
	         self.copyObtainToDREG(RX,arg.rnum) -> put obtain on register
	      ELSEIF arg.rtype = PPCARGRTYPE_FX
	         ha.hregop := FREG
	         ha.hregnum := arg.rnum
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, FPX,arg.rnum)
	         self.copyObtainToFREG(FPX,arg.rnum) -> put obtain on register
	      ELSEIF arg.rtype = PPCARGRTYPE_RX2
	         ha.hregop := NIL
	         ha.hregnum := -1
	         n, o, d := self.doExpression(n, ha)
	         ->inst_copy(o,d, D64, D64TEMP)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), RX, arg.rnum)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), RX, arg.rnum+1)
	         self.tox2r(o,d,arg.rnum)
	         self.copyObtainToDREG(RX,arg.rnum) -> put obtain on register
	         self.copyObtainToDREG(RX,arg.rnum+1) -> put obtain on register
	      ELSEIF arg.rtype = PPCARGRTYPE_STACKRX2
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, D64, arg.rnum) -> ohyeah
	      ENDIF
	   ENDFOR

	   FOR a := it_funccall.numpars-1 TO 0 STEP -1
	      arg := v.argsarray[a]
	      IF arg.rtype = PPCARGRTYPE_RX
	         self.releaseDREG(arg.rnum,0)
	      ELSEIF arg.rtype = PPCARGRTYPE_FX
	         self.releaseFREG(arg.rnum,0)
	      ELSEIF arg.rtype = PPCARGRTYPE_RX2
	         self.releaseDREG(arg.rnum,0)
	         self.releaseDREG(arg.rnum+1,0)
	      ENDIF
	   ENDFOR

	ENDIF

	inst_copy(v.o,v.d, RX,ATEMP)
	inst_gosarx(ATEMP)

	ro, rd := self.secureReturn(ro, rd, regusecopy) -> v50

	self.loadObtained(regusecopy)
	clearRegs()

	g_multireturn := IF v.multiret THEN v.multiret ELSE [DREG,DREG,DREG,DREG,3,4,5,6]:LONG


	#ifdef DBG_PPCGEN
	DEBUGF('dovfunction() \s DONE\n', v.hln.name)
	#endif

ENDPROC r, ro, rd

-> v50 new parameterpassing
-> v55 64bit varargs support
PROC doIntfunc(n:PTR TO item) OF ppc
	DEF buf:PTR TO item, r:PTR TO raise, o, d, a, t
	DEF numparams, defs:PTR TO LONG
	DEF ifunc:PTR TO lif, regusecopy[REGUSETABSIZE]:ARRAY OF oreg
	DEF hln:PTR TO hln, numvarargs=0, var:PTR TO var
	DEF noraise:PTR TO genlab
	DEF return=FALSE, o2,d2
	DEF ifNumArgs
	DEF it_funccall:PTR TO it_funccall
	DEF ro, rd, ro2, rd2
	DEF ha:hintargs
	DEF rx=3, fx=1

	ha.hregisvar := FALSE

	buf := n
	it_funccall := n.info
	numparams := it_funccall.numpars
	ifunc := it_funccall.data
	n := it_funccall + SIZEOF it_funccall

	ifNumArgs := ListLen(ifunc.params)

	#ifdef DBG_PPCGEN
	DEBUGF('doifunction() \s \n', ifunc.name)
	#endif

	-> suckass solution for now 2.0.0

	return := ListItem(ifunc.returns, 0)
	SELECT return
	CASE 0
	   ro := DREG
	   rd := IREG0
	CASE 1
	   ro := FREG
	   rd := FREG0
	CASE 2
	   ro := X2R
	   rd := IREG0
	DEFAULT
	   ro := NIL
	ENDSELECT

	return := IF ListLen(ifunc.returns) = 2 THEN ListItem(ifunc.returns, 1) ELSE -1
	SELECT return
	CASE 0
	   ro2 := DREG
	   rd2 := IF ro = FREG THEN IREG0 ELSE IREG1
	CASE 1
	   ro2 := FREG
	   rd2 := FREG0
	CASE 2
	   ro2 := X2R
	   rd2 := 5 ->IREG2
	DEFAULT
	   ro2 := NIL
	ENDSELECT

	self.saveObtained(regusecopy)

	defs := ifunc.defaults

	IF ifunc.flags <> 2  -> ppc non varargs ?

	   /* evaluate expressions and put on stack or registers */
	   FOR a := 0 TO numparams-1
	      IF a > 7  -> put in parameter store area ?
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, DREG,DTEMP)
	         inst_copy(DREG,DTEMP, ARXPO,AxSizeOfs(1,4,a-8*4+8))
	      ELSE
	         t := ListItem(ifunc.params,a)
	         IF t = 1
	            o2 := FPX
	            d2 := fx++
	         ELSEIF t = 0
	            o2 := RX
	            d2 := rx++
	         ELSEIF t = 2 -> 2.0.0 X2R WIDE
	            o2 := D64
	            d2 := D64TEMP
	         ENDIF

	         ha.hregop := o2
	         ha.hregnum := d2
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, o2,d2)
	         IF o2 = FPX
	            self.copyObtainToFREG(FPX,d2) -> put an pbtain on register
	         ELSEIF o2 = RX
	            self.copyObtainToDREG(RX,d2) -> put an pbtain on register
	         ELSEIF o2 = D64
	            self.copyObtainToDREG(RX, rx)
	            self.copyObtainToDREG(RX, rx+1)
	            inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), RX,rx)
	            inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), RX,rx+1)
	            rx++ ; rx++
	         ENDIF
	      ENDIF
	   ENDFOR
	   IF n.data THEN reportIErr(' doifunction, not completed')


	   -> should we put some defaults ?
	   IF ifNumArgs > numparams
	      FOR a := numparams TO ifNumArgs-1
	         IF a < 8
	            t := ListItem(ifunc.params,a)
	            IF t = 1
	               o2 := FPX
	               d2 := fx++
	            ELSEIF t = 0
	               o2 := RX
	               d2 := rx++
	            ENDIF
	            inst_copy(DV,defs[]++, o2,d2)
	            IF o2 = FPX
	               self.copyObtainToFREG(FPX,d2) -> put an pbtain on register
	            ELSEIF o2 = RX
	               self.copyObtainToDREG(RX,d2) -> put an pbtain on register
	            ENDIF
	         ELSE
	            inst_copy(DV,defs[]++, RX,DTEMP)
	            inst_copy(RX,DTEMP, ARXPO,AxSizeOfs(1,4,a-8*4+8))
	         ENDIF
	      ENDFOR
	   ENDIF

	   -> release registers
	   FOR a := ifNumArgs-1 TO 0 STEP -1
	      IF a < 8
	         t := ListItem(ifunc.params,a)
	         IF t = 1
	            o2 := FPX
	            d2 := fx--
	         ELSEIF t = 0
	            o2 := RX
	            d2 := rx--
	         ELSEIF t = 2 -> 2.0.0
	            o2 := D64
	            d2 := D64TEMP
	         ENDIF
	         IF o2 = FPX
	            self.releaseFREG(d2,0) -> release register
	         ELSEIF o2 = RX
	            self.releaseDREG(d2,0) -> release register
	         ELSEIF o2 = D64
	            rx-- ; rx--
	            self.releaseDREG(rx+1,0) -> release register
	            self.releaseDREG(rx,0) -> release register
	         ENDIF
	      ENDIF
	   ENDFOR

	   IF ifunc.flags = 1 -> inline ?
	      CopyMemQuick(ifunc.code, g_codeptr, ifunc.codelen)
	      g_codeptr := g_codeptr + ifunc.codelen
	   ELSE
	      inst_goslab(ifunc)
	   ENDIF

	ELSEIF ifunc.flags = 2 -> ppc varargs ?
	   -> put ALL parameters on stack
	   t := g_localstackoffset
	   WHILE n.data
	      n, o, d := self.doExpression(n)
	      SELECT o -> 1.10.0 support 8byte values
	      CASE D64    ; inst_push(8, o, d)
	      CASE VAR64  ; inst_push(8, o, d)
	      CASE X2R    ; inst_push(8, o, d)
	      CASE FPX    ; inst_push(8, o, d)
	      DEFAULT     ; inst_push(4, o, d)
	      ENDSELECT
	   ENDWHILE
	   -> put fixed params in regs
	   FOR a := 0 TO ifNumArgs-1
	      inst_copy(ARXPO,AxSizeOfs(1,4,t+(a*4)), RX,a+3)
	   ENDFOR
	   -> now always creates varargs array regardless to
	   -> avoid problem with OS4 DebugF() *TEMP FIX*
	   ->IF numparams > ifNumArgs
	      -> get address of varargs in next reg
	      inst_copy(ARXPO,AxSizeOfs(1,0,t+(a*4)), RX,a+3)
	   ->ENDIF
	   -> call function
	   inst_goslab(ifunc)
	   g_localstackoffset := t
	ENDIF

	ro, rd := self.secureReturn(ro, rd, regusecopy)

	self.loadObtained(regusecopy)

	-> 2.0.0 crap but hey
	IF ro2 = X2R
	   -> copy regs into a non obtained WIDE "register"
	   inst_copy(RX,rd2, ARXPO,AxSizeOfs(1,4,g_localstackoffset))
	   inst_copy(RX,rd2+1, ARXPO,AxSizeOfs(1,4,g_localstackoffset+4))
	   ro2 := D64
	   rd2 := g_localstackoffset
	ENDIF

	clearRegs()

	IF ifunc.raise
	   noraise := newLabel()
	   inst_bic(DV,ifunc.raise.trigval,ifunc.raise.condition, ro,rd,noraise)
	   t := ifunc.raise.excval
	   inst_copy(DV,t, DREG,IREG0)
	   ppcblab(self.raise_lif,0,0)->g_internalfuncsppc[6],0,0) -> Raise
	   def_label(noraise)
	ENDIF

	g_multireturn := [ro,
	                  ro2,
	                  NIL,
	                  NIL,
	                  rd,
	                  rd2,
	                  NIL,
	                  NIL]:LONG

	#ifdef DBG_PPCGEN
	DEBUGF('doiunction() \s DONE\n', ifunc.name)
	#endif

ENDPROC buf[1], ro, rd

-> v50: new parameterpassing for sysv ....
PROC doLibfunc(n:PTR TO item) OF ppcsysv
	DEF buf:PTR TO item, a, o, d, libbase:PTR TO var, r:PTR TO raise
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, lfunc:PTR TO lfunc, hln:PTR TO hln
	DEF numparams, t, var:PTR TO var
	DEF o2, d2, t2, noraise:PTR TO genlab
	DEF it_funccall:PTR TO it_funccall
	DEF ro, rd, saved2
	DEF ha:hintargs->, regusecopy2[REGUSETABSIZE]:ARRAY OF oreg

	ha.hregisvar := FALSE

	buf := n
	it_funccall := n.info
	numparams := it_funccall.numpars
	lfunc := it_funccall.data
	libbase := it_funccall.info
	n := it_funccall + SIZEOF it_funccall

	IF numparams <> lfunc.nrofargs THEN reportErr('wrong nr of parameters for function', lfunc.name)

	#ifdef DBG_PPCGEN
	DEBUGF('dolfunction(/\d) buf.num=\d \a\s\a buf.info=$\h\n', lfunc.nrofargs, buf.num, lfunc.name, buf.info)
	#endif


	-> 2.2.1. float and wide wasnt implemented
	t := lfunc.return
	SELECT t
	CASE 0
	   ro := RX
	   rd := 3
	CASE 1
	   ro := FPX
	   rd := 1
	CASE 3
	   ro := X2R
	   rd := 3
	ENDSELECT

	self.saveObtained(regusecopy)


	IF lfunc.type = 1 -> sysv ?

	   FOR a := 0 TO numparams-1
	      IF lfunc.regs[a].rtype = PPCARGRTYPE_STACKRX  -> put in parameter store area ?
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, ARXPO,AxSizeOfs(1,4,lfunc.regs[a].rnum))
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_RX
	         ha.hregop := RX
	         ha.hregnum := lfunc.regs[a].rnum
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, RX,lfunc.regs[a].rnum)
	         self.copyObtainToDREG(RX,lfunc.regs[a].rnum)
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_FX
	         ha.hregop := FREG
	         ha.hregnum := lfunc.regs[a].rnum
	         n, o, d := self.doExpression(n, ha)
	         inst_copy(o,d, FPX,lfunc.regs[a].rnum)
	         self.copyObtainToFREG(FPX,lfunc.regs[a].rnum)
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_RX2 -> 2.0.0 WIDE/X2R
	         n, o, d := self.doExpression(n)
	         ->inst_copy(o,d, D64,D64TEMP)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), RX, lfunc.regs[a].rnum)
	         ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), RX, lfunc.regs[a].rnum+1)
	         self.tox2r(o,d,lfunc.regs[a].rnum)
	         self.copyObtainToDREG(RX,lfunc.regs[a].rnum) -> put obtain on register
	         self.copyObtainToDREG(RX,lfunc.regs[a].rnum+1) -> put obtain on register
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_STACKRX2
	         n, o, d := self.doExpression(n)
	         inst_copy(o,d, D64,lfunc.regs[a].rnum) -> clever eh !
	      ENDIF
	   ENDFOR

	   -> release regs
	   FOR a := 0 TO numparams-1
	      IF lfunc.regs[a].rtype = PPCARGRTYPE_RX
	         self.releaseDREG(lfunc.regs[a].rnum,0)
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_FX
	         self.releaseFREG(lfunc.regs[a].rnum,0)
	      ELSEIF lfunc.regs[a].rtype = PPCARGRTYPE_RX2
	         self.releaseDREG(lfunc.regs[a].rnum+1,0)
	         self.releaseDREG(lfunc.regs[a].rnum,0)
	      ENDIF
	   ENDFOR

	   IF lfunc.flags AND LFFLAG_KERNELFUNC -> 1.7.1
	      -> in this mode, basernum is register containing lfunc #
	      -> baseofs is lfunc #
	      -> and we use SC instruction to make system call
	      ppcaddi(lfunc.basernum, 0, lfunc.baseofs)
	      ppcsc()
	   ELSEIF lfunc.basertype = 0
	      IF lfunc.basernum
	         inst_copy(libbase.o,libbase.d, RX,lfunc.basernum)
	         ppclwz(0,lfunc.basernum,lfunc.baseofs)
	      ELSE
	         inst_copy(libbase.o,libbase.d, RX,11)
	         ppclwz(0,11,lfunc.baseofs)
	      ENDIF
	      ppcmtspr(0,9)
	      ppcbcctr(20,0,1)
	   ELSE
	      inst_copy(libbase.o,libbase.d, RX,11)
	      inst_copy(RX,11, ARXPO,AxSizeOfs(1,4,lfunc.basernum))
	      ppclwz(0,11,lfunc.baseofs)
	      ppcmtspr(0,9)
	      ppcbcctr(20,0,1)
	   ENDIF

	ELSEIF lfunc.type = 0  -> 68k abi

	   -> 2.0
	   IF g_optosid <> OSID_MORPHOS THEN reportErr('wrong abi for library function', lfunc.name)

	   FOR a := 0 TO numparams-1
	      EXIT a > 5
	      ha.hregop := DREG
	      ha.hregnum := a + 3
	      n, o, d := self.doExpression(n, ha)
	      inst_copy(o,d, DREG,a+3)
	      self.copyObtainToDREG(DREG,a+3)
	   ENDFOR

	    -> release regs
	   FOR a := 0 TO numparams-1
	      EXIT a > 5
	      self.releaseDREG(a+3,0)
	   ENDFOR

	   -> put regs in emulhande
	   FOR a := 0 TO numparams-1
	      EXIT a > 5
	      IF lfunc.regs[a].rtype = 0
	         d2 := AxSizeOfs(2,4,lfunc.regs[a].rnum*4)
	      ELSE
	         d2 := AxSizeOfs(2,4,lfunc.regs[a].rnum*4+32)
	      ENDIF
	      inst_copy(DREG,a+3, ARXPO, d2)
	   ENDFOR

	   -> now do args 6...
	   FOR a := 6 TO numparams-1
	      n, o, d := self.doExpression(n)
	      IF lfunc.regs[a].rtype = 0
	         d2 := AxSizeOfs(2,4,lfunc.regs[a].rnum*4)
	      ELSE
	         d2 := AxSizeOfs(2,4,lfunc.regs[a].rnum*4+32)
	      ENDIF
	      inst_copy(o,d, ARXPO,d2)
	   ENDFOR

	   inst_copy(libbase.o,libbase.d, RX,11)
	   inst_copy(RX,11, ARXPO,AxSizeOfs(2,4,6+8*4)) /* emul a6*/
	   inst_copy(ARXPO,AxSizeOfs(2,4,100), RX,11) /* emulcalldirectos */
	   inst_copy(DV,lfunc.baseofs, RX,3) /* ofs */
	   ppcmtspr(11,9)
	   ppcbcctr(20,0,1)

	ELSE

	   reportErr('unknown ABI for libraryfunction', lfunc.name)

	ENDIF

	ro, rd := self.secureReturn(ro, rd, regusecopy)

	self.loadObtained(regusecopy)
	clearRegs()

	IF lfunc.raise
	   noraise := newLabel()
	   inst_bic(DV,lfunc.raise.trigval,lfunc.raise.condition, ro,rd,noraise)
	   t := lfunc.raise.excval
	   inst_copy(DV,t, DREG,IREG0)
	   ppcblab(self.raise_lif,0,0)->g_internalfuncsppc[6],0,0) -> Raise
	   def_label(noraise)
	ENDIF

	g_multireturn := [ro,ARXPO,ARXPO,ARXPO,
	                  rd,AxSizeOfs(2,4,4),AxSizeOfs(2,4,8),AxSizeOfs(2,4,12)]:LONG


	#ifdef DBG_PPCGEN
	DEBUGF('dolfunction() \s DONE\n', lfunc.name)
	#endif

ENDPROC buf[1], ro, rd

PROC doReturn(buf, ret) OF ppcsysv
	DEF n:PTR TO item
	DEF o,d, x, t
	DEF a=0, t1:PTR TO CHAR, t2:PTR TO CHAR
	DEF numrets
	DEF m:PTR TO multireturn
	DEF ha:hintargs

	ha.hregisvar := FALSE

	m := g_currentproc.mret

	#ifdef DBG_PPCGEN
	DEBUGF('doreturn($\h,\d)\n', buf, ret)
	#endif

	n := buf
	numrets := n.num

	IF numrets > 4 THEN reportErr('too many returnvalues')

	n++

	self.endLocalClasses(g_currentproc)

	FOR a := 0 TO numrets-1
	   ha.hregop := IF m.ros[a] = X2R THEN D64REG ELSE m.ros[a]
	   ha.hregnum := IF m.ros[a] = X2R THEN D64TEMP ELSE m.rds[a]
	   n, o, d := self.doExpression(n, ha)
	   IF m.ros[a] = X2R
	      self.tox2r(o,d,m.rds[a])
	      ->inst_copy(o, d, D64REG, D64TEMP)
	      ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP), DREG, m.rds[a])
	      ->inst_copy(ARXPO,AxSizeOfs(1,4,D64TEMP+4), DREG, m.rds[a]+1)
	   ELSE
	      inst_copy(o,d, m.ros[a],m.rds[a])
	   ENDIF
	   IF m.ros[a] = DREG
	      self.copyObtainToDREG(DREG,m.rds[a]) -> obtain register
	   ELSEIF m.ros[a] = FREG
	      self.copyObtainToFREG(FREG,m.rds[a]) -> obtain register
	   ELSEIF m.ros[a] = X2R -> 2.0.0
	      self.copyObtainToDREG(DREG,m.rds[a]) -> obtain register
	      self.copyObtainToDREG(DREG,m.rds[a]+1) -> obtain register
	   ENDIF
	ENDFOR

	FOR a := 0 TO numrets-1
	   IF m.ros[a] = DREG
	      self.releaseDREG(m.rds[a],0)
	   ELSEIF m.ros[a] = FREG
	      self.releaseFREG(m.rds[a],0)
	   ELSEIF m.ros[a] = X2R
	      self.releaseDREG(m.rds[a],0)
	      self.releaseDREG(m.rds[a]+1,0)
	   ENDIF
	ENDFOR

	IF numrets = 0 THEN inst_copy(DV,NIL, DREG,IREG0)

	IF ret THEN inst_ret(0)

	#ifdef DBG_PPCGEN
	DEBUGF('doreturn DONE()\n')
	#endif

ENDPROC n, NIL, NIL

-> used ONLY by ppc.mathlogichead()
-> 1.8.1, now also used by IID_PUSH/POP
PROC ppcRegisterizeD(o,d:PTR TO var) OF ppc
	IF o = DREG
	   RETURN d
	ELSEIF o = VAR
	   IF d.intreg AND d.trok AND (d.treg>2) THEN RETURN d.treg
	ENDIF
	inst_copy(o,d, DREG,DTEMP)
ENDPROC DTEMP

-> used ONLY by ppc.mathlogichead()
-> 1.8.1, now also used by IID_PUSH/POP
PROC ppcRegisterizeF(o,d:PTR TO var) OF ppc
	IF o = FREG
	   RETURN d
	ELSEIF o = VAR64
	   IF d.intreg AND d.trok THEN RETURN d.treg
	ENDIF
	inst_copy(o,d, FREG,FTEMP)
ENDPROC FTEMP

-> 1.10.0
PROC ppcRegisterizeD64(o,d:PTR TO var) OF ppc
	IF o = D64
	   RETURN d
	ELSEIF o = VAR64
	   IF d.intreg AND d.trok THEN RETURN d.treg
	ENDIF
	inst_copy(o,d, D64,D64TEMP)
ENDPROC D64TEMP

-> optimizes
-> "x := -y"
-> "x := y OP z"
-> "x := !-y"
-> "x := !y OP z"
-> "x := !y * z + a"
-> "x := !y * z - a"
-> "x" might be argreg to function too..

-> v58, changes
PROC mathlogichead(n:PTR TO item, mode, unary, ha:PTR TO hintargs) OF ppc
	DEF xreg=-1, yreg
	DEF zo, zd, zreg, ao, ad, areg
	DEF t, val, immbool, fused, end:PTR TO item
	DEF immreg -> 1.5.5 for temp immediate values >16bit
	DEF o, d, e, ha2:hintargs, t2  -> 2.3
	DEF fha:hintargs, iha:hintargs, wha:hintargs -> 2.3
 
      -> 2.3
   fha.hregop := FREG
   fha.hregnum := -1
   fha.hregisvar := FALSE
   
   iha.hregop := DREG
   iha.hregnum := -1
   iha.hregisvar := FALSE

   wha.hregop := D64REG
   wha.hregnum := -1
   wha.hregisvar := FALSE


	e := n
	n++

	IF ha
	   IF ha.hregnum > -1
	      IF mode = MODE_FLOAT
	         IF ha.hregop = FREG
	            xreg := ha.hregnum
	         ENDIF
	      ELSEIF mode = MODE_DEFAULT
	         IF ha.hregop = DREG
	            xreg := ha.hregnum
	         ENDIF
	      ELSEIF mode = MODE_D64
	         IF ha.hregop = D64REG
	            xreg := ha.hregnum
	         ENDIF
	      ELSE
	         reportIErr('mathlogichead ppc, gulp!')
	      ENDIF
	   ENDIF
	ENDIF


	IF mode = MODE_FLOAT  /*** FLOATING POINT ***/

	   ->IF xreg = -1 THEN xreg := FTEMP

	   IF unary
	      t2, o, d := self.doSingleExp(e, fha)  -> 2.3
	      IF n.data <> NIL
	         IF xreg = -1
	            xreg := self.getFreeFREG()
	         ELSEIF ha.hregisvar
	            xreg := FTEMP
	         ENDIF
	         IF xreg = FTEMP
	            xreg := self.obtainFREG() -> obtain if more
	         ELSE
	            self.lockFREG(xreg)
	         ENDIF
	      ENDIF
	      yreg := self.ppcRegisterizeF(o,d)
	      IF n.data = NIL
	         IF xreg = -1 THEN xreg := self.getFreeFREG()
	      ENDIF

	      IF unary = "-"
	         ppcfneg(xreg,yreg,0)
	      ELSEIF unary = KW_ABS -> 2.2
	         ppcfabs(xreg,yreg,0)
	      ENDIF
	   ELSEIF (n[1].data <> "-") AND (n[1].data <> KW_ABS)

	      fused := FALSE

	      t := n.data
	      SELECT 256 OF t
	      CASE "+", "-", "*", "/"
	         n++
	         IF t = "*"
	            IF n[1].data = "+"
	               fused := 1
	               end := n[3]
	            ELSEIF n[1].data = "-"
	               fused := -1
	               end := n[3]
	            ELSE
	               end := n[1]
	            ENDIF
	         ELSE
	            end := n[1]
	            IF (t = "/") AND (n.data = IT_VALUE) -> 2.1
	               -> float division by immediate value, lets optimise into multiplication
	               t := "*"
	               n.info := !1.0/n.info
	            ENDIF
	         ENDIF
	         t2, o, d := self.doSingleExp(e, fha) -> 2.3
	         IF end.data <> NIL
	            IF xreg = -1
	               xreg := self.getFreeFREG()
	            ELSEIF ha.hregisvar
	               xreg := FTEMP
	            ENDIF
	            IF xreg = FTEMP
	               xreg := self.obtainFREG() -> obtain if more
	            ELSE
	               self.lockFREG(xreg)
	            ENDIF
	         ENDIF
	         yreg := self.copyObtainToFREG(o,d)
	         n, zo, zd := self.doExpression(n)
	         IF fused
	            zreg := self.copyObtainToFREG(zo,zd)
	            n++ -> skip "+"/"-"
	            n, ao, ad := self.doExpression(n)
	            areg := self.ppcRegisterizeF(ao,ad)
	         ELSE
	            zreg := self.ppcRegisterizeF(zo,zd)
	         ENDIF
	         IF n.data = NIL
	            IF xreg = -1 THEN xreg := self.getFreeFREG()
	         ENDIF
	      DEFAULT
	         yreg := self.obtainFREG() -> we need a write mode obtain
	         ha2.hregop := FREG  -> 2.3..
	         ha2.hregnum := yreg
	         ha2.hregisvar := FALSE
	         t2, o, d := self.doSingleExp(e, ha2)
	         inst_copy(o,d, FREG,yreg)
	         RETURN self.domathlogictail(n, MODE_FLOAT, yreg)
	      ENDSELECT

	      SELECT t
	      CASE "+"
	         ppcfadd(xreg,yreg,zreg,0)
	      CASE "-"
	         ppcfsub(xreg,yreg,zreg,0)
	      CASE "*"
	         IF fused = 1
	            ppcfmadd(xreg,yreg,areg,zreg,0)
	         ELSEIF fused = -1
	            ppcfmsub(xreg,yreg,areg,zreg,0)
	         ELSE
	            ppcfmul(xreg,yreg,zreg,0)
	         ENDIF
	      CASE "/"
	         ppcfdiv(xreg,yreg,zreg,0)
	      ENDSELECT
	      IF fused THEN self.releaseFREG(zreg,0)
	      self.releaseFREG(yreg,0)

	   ELSE

	      yreg := self.obtainFREG() -> we need a write mode obtain
	      ha2.hregop := FREG  -> 2.3..
	      ha2.hregnum := yreg
	      ha2.hregisvar := FALSE
	      t2, o, d := self.doSingleExp(e, ha2)
	      inst_copy(o,d, FREG,yreg)
	      RETURN self.domathlogictail(n, MODE_FLOAT, yreg)

	   ENDIF

	   ppcSetFReg(xreg,NIL,NIL)

	   IF n.data = NIL
	      RETURN n, FREG, xreg
	   ELSE
	      RETURN self.domathlogictail(n,MODE_FLOAT, xreg)
	   ENDIF

	ELSEIF mode = MODE_DEFAULT      /*** INTEGER ***/

	   ->IF xreg = -1 THEN xreg := DTEMP

	   t := n[1].data

	   IF unary
	      t2, o, d := self.doSingleExp(e, iha)
	      IF n.data <> NIL
	         IF xreg = -1
	            xreg := self.getFreeDREG()
	         ELSEIF ha.hregisvar
	            xreg := DTEMP
	         ENDIF
	         IF xreg = DTEMP
	            xreg := self.obtainDREG() -> obtain if more
	         ELSE
	            self.lockDREG(xreg)
	         ENDIF
	      ENDIF
	      yreg := self.ppcRegisterizeD(o,d)
	      IF n.data = NIL
	         IF xreg = -1 THEN xreg := self.getFreeDREG()
	      ENDIF
	      IF unary = "-"
	         ppcneg(xreg,yreg,0,0)
	      ELSEIF unary = "~"
	         ppcnor(yreg,xreg,yreg,0)
	      ELSEIF unary = KW_ABS -> 2.2
	         ppc_abs(xreg,yreg)
	      ENDIF
	   ELSEIF (n.data = KW_AND) AND (t = "~")  -> 2.2
	      n++
	      n++
	      -> AND Not => ANDC
	      t2, o, d := self.doSingleExp(e, iha)
	      IF n[1].data
	         IF xreg = -1
	            xreg := self.getFreeDREG()
	         ELSEIF ha.hregisvar
	            xreg := DTEMP
	         ENDIF
	         IF xreg = DTEMP
	            xreg := self.obtainDREG() -> obtain if more
	         ELSE
	            self.lockDREG(xreg)
	         ENDIF
	      ENDIF
	      yreg := self.copyObtainToDREG(o,d)
	      n, zo, zd := self.doExpression(n)
	      zreg := self.ppcRegisterizeD(zo,zd)
	      IF n.data = NIL
	         IF xreg = -1 THEN xreg := self.getFreeDREG()
	      ENDIF
	      ppcandc(yreg,xreg,zreg,0)
	      self.releaseDREG(yreg,0)
	   ELSEIF (t <> "-") AND (t <> "~") AND (t <> KW_ABS)

	      immbool := FALSE

	      t := n.data
	      SELECT 256 OF t
	      CASE "+", "-", "*", "/", KW_AND, KW_OR, KW_SHL, KW_SHR, KW_XOR, KW_ASR
	         t2, o, d := self.doSingleExp(e, iha)
	         n++
	         IF n[1].data <> NIL
	            IF xreg = -1
	               xreg := self.getFreeDREG()
	            ELSEIF ha.hregisvar
	               xreg := DTEMP
	            ENDIF
	            IF xreg = DTEMP
	               xreg := self.obtainDREG() -> obtain if more
	            ELSE
	               self.lockDREG(xreg)
	            ENDIF
	         ENDIF
	         yreg := self.copyObtainToDREG(o,d)
	         IF n.data = IT_VALUE
	            immbool := TRUE
	            val := n.info
	            immreg := self.obtainDREG()
	            n++
	         ELSE
	            n, zo, zd := self.doExpression(n)
	            zreg := self.ppcRegisterizeD(zo,zd)
	         ENDIF
	         IF n.data = NIL
	            IF xreg = -1 THEN xreg := self.getFreeDREG()
	         ENDIF

	      DEFAULT
	         yreg := self.obtainDREG() -> we need a write mode obtain
	         ha2.hregop := DREG  -> 2.3..
	         ha2.hregnum := yreg
	         ha2.hregisvar := FALSE
	         t2, o, d := self.doSingleExp(e, ha2)
	         inst_copy(o,d, DREG,yreg)
	         RETURN self.domathlogictail(n, MODE_DEFAULT, yreg)
	      ENDSELECT

	      SELECT 256 OF t
	      CASE "+"
	         IF immbool
	            ppcaddiw(xreg,yreg,val)
	         ELSE
	            ppcadd(xreg,yreg,zreg,0,0)
	         ENDIF
	      CASE "-"
	         IF immbool
	            ppcaddiw(xreg,yreg,-val)
	         ELSE
	            ppcsubf(xreg,zreg,yreg,0,0)
	         ENDIF
	      CASE "*"
	         IF immbool
	            IF (Abs(val) < 32768) AND (val <> $80000000)
	               ppcmulli(xreg,yreg,val)
	            ELSE
	               inst_copy(DV,val, DREG,immreg)
	               ppcmullw(xreg,yreg, immreg, 0, 0)
	            ENDIF
	         ELSE
	            ppcmullw(xreg,yreg,zreg,0,0)
	         ENDIF
	      CASE "/"
	         IF immbool
#ifndef USEMULDIVSHIFT
	            inst_copy(DV,val, DREG,immreg)
	            ppcdivw(xreg,yreg, immreg, 0, 0)
#endif
#ifdef USEMULDIVSHIFT
	            ppcdiviw(xreg,yreg,val) -> changed 1.10.0
#endif
	         ELSE
	            ppcdivw(xreg,yreg,zreg,0,0)
	         ENDIF
	      CASE KW_AND
	         IF immbool
	            IF val AND $FFFF0000
	               inst_copy(DV,val, RX,immreg)
	               ppcand(yreg,xreg,immreg,0)
	            ELSE
	               ppcandi_(yreg,xreg,val)
	            ENDIF
	         ELSE
	            ppcand(yreg,xreg,zreg,0)
	         ENDIF
	      CASE KW_OR
	         IF immbool
	            IF val AND $FFFF0000
	               inst_copy(DV,val, RX,immreg)
	               ppcor(yreg,xreg,immreg,0)
	            ELSE
	               ppcori(yreg,xreg,val)
	            ENDIF
	         ELSE
	            ppcor(yreg,xreg,zreg,0)
	         ENDIF
	      CASE KW_XOR -> 1.10.0
	         IF immbool
	            IF val AND $FFFF0000
	               inst_copy(DV,val, RX,immreg)
	               ppcxor(yreg,xreg,immreg,0)
	            ELSE
	               ppcxori(yreg,xreg,val)
	            ENDIF
	         ELSE
	            ppcxor(yreg,xreg,zreg,0)
	         ENDIF
	      CASE KW_SHR
	         IF immbool
	            ppcrlwinm(yreg,xreg,32-val,val,31,0) -> srwi xreg,yreg,n.info
	         ELSE
	            ppcsrw(yreg,xreg,zreg,0)
	         ENDIF
	      CASE KW_ASR -> 1.10.0
	         IF immbool
	            ppcsrawi(yreg,xreg,val,0)
	         ELSE
	            ppcsraw(yreg,xreg,zreg,0)
	         ENDIF
	      CASE KW_SHL
	         IF immbool
	            ppcrlwinm(yreg,xreg,val,0,31-val,0) -> slwi xreg,yreg,n.info
	         ELSE
	            ppcslw(yreg,xreg,zreg,0)
	         ENDIF
	      ENDSELECT
	      IF immbool THEN self.releaseDREG(immreg,0) -> 1.5.5
	      self.releaseDREG(yreg,0)

	   ELSE

	      yreg := self.obtainDREG() -> we need a write mode obtain
	      ha2.hregop := DREG  -> 2.3..
	      ha2.hregnum := yreg
	      ha2.hregisvar := FALSE
	      t2, o, d := self.doSingleExp(e, ha2)
	      inst_copy(o,d, DREG,yreg)
	      RETURN self.domathlogictail(n, MODE_DEFAULT, yreg)

	   ENDIF

	   ppcSetReg(xreg,NIL,NIL)

	   IF n.data = NIL
	      RETURN n, DREG, xreg
	   ELSE
	      RETURN self.domathlogictail(n,mode,xreg)
	   ENDIF

	ELSEIF mode = MODE_D64  -> 1.10.0

	   yreg := self.obtainD64() -> we need a write mode obtain
	   ha2.hregop := D64REG  -> 2.3..
	   ha2.hregnum := yreg
	   ha2.hregisvar := FALSE
	   t2, o, d := self.doSingleExp(e, ha2)
	   inst_copy(o,d, D64,yreg)
	   IF unary = "-"
	      inst_negd64(yreg)
	   ELSEIF unary = "~"
	      inst_notd64(yreg)
	   ELSEIF unary = KW_ABS
	      inst_absd64(yreg)
	   ENDIF
	   RETURN self.domathlogictail(n, MODE_D64, yreg)

	ELSE
	   reportIErr('.mathlogichead() mode=?')
	ENDIF
ENDPROC

PROC doAsm(n:PTR TO item) OF ppc
	DEF nrofops=NIL, ops:PTR TO LONG
	DEF t:PTR TO item, ri, as,as2, hln:PTR TO hln, ident:PTR TO ident
	DEF asm:PTR TO asm, var:PTR TO var
	DEF o:PTR TO object, m:PTR TO member
	DEF i:PTR TO instppc, reg:PTR TO reg
	DEF lab:PTR TO codelab
	DEF lfunc:PTR TO lfunc
	DEF odarray[10]:ARRAY OF LONG -> 1.6.1
	DEF instppc:PTR TO instppc

	g_od := odarray

	asm := n.info
	IF n.num = "."
	   g_punct := 1
	ELSEIF n.num
	   reportErr('syntax for instruction', asm.name)
	ELSE
	   g_punct := 0
	ENDIF

	#ifdef DBG_PPCGEN
	DEBUGF('ppc.doAsm($\h) \d, "\s", $\h\n', n, asm.identID, asm.name, asm.data)
	#endif

	n++

	i := asm.data

	ops := i.operands

	WHILE n.data <> 10
	   t := n.data
	   SELECT t
	   CASE IT_REG
	      reg := n.info
	      IF ops[nrofops] <> reg.type THEN reportErr('wrong type of operand for instruction', asm.name)
	      g_od[nrofops] := reg.num
	      nrofops++ ; n++
	   CASE IT_VALUE
	      t := n[1]
	      IF t.data = "("
	         t++
	         IF t.data <> IT_REG THEN reportErr('register expected for instruction', asm.name)
	         reg := t.info
	         IF ops[nrofops] <> reg.type THEN reportErr('wrong operand type for instruction', asm.name)
	         g_od[nrofops] := reg.num
	         nrofops++
	         t++
	         IF ops[nrofops] <> DV THEN reportErr('wrong type of operand for instruction', asm.name)
	         g_od[nrofops] := n.info
	         IF t.data <> ")" THEN reportErr('")" expected for instruction', asm.name)
	         n := t[1] ; nrofops++
	      ELSE
	         IF ops[nrofops] <> DV THEN reportErr('wrong type of operand for instruction', asm.name)
	         g_od[nrofops] := n.info
	         n++ ; nrofops++
	      ENDIF
	   CASE IT_VARIABLE -> var ?
	      var := n.info
	      IF ops[nrofops] <> RX THEN reportErr('wrong type of operand for instruction', asm.name)
	      IF var.o = RX
	         ->IF ops[nrofops] <> RX THEN reportErr('wrong type of operand for instruction', asm.name)
	         g_od[nrofops] := var.d
	      ELSEIF var.link
	         g_od[nrofops] := -1 -> signals link global !
	         nrofops++
	         IF ops[nrofops] <> DV THEN reportErr('wrong type of operand for instruction', asm.name)
	         g_od[nrofops] := var  -> ! yep
	      ELSE
	         g_od[nrofops] := var.breg
	         nrofops++
	         IF ops[nrofops] <> DV THEN reportErr('wrong type of operand for instruction', asm.name)
	         g_od[nrofops] := var.offset
	      ENDIF
	      n++ ; nrofops++
	   CASE IT_LABEL -> proc/asmlabel
	      hln := n.info
	      lab := hln.ident
	      IF lab = NIL THEN reportErr('unknown identifier', hln.name)
	      lab.referenced := 1
	      g_od[nrofops] := lab
	      IF ops[nrofops] <> LAB THEN reportErr('wrong type of label operand', hln.name)
	      n++ ; nrofops++
	   CASE "." -> member ?
	      n++
	      t := n
	      n++
	      IF n.data <> "(" THEN reportErr('"(" expected')
	      n++
	      IF n.data = IT_VARIABLE -> var ?
	         var := n.info
	         IF var.o <> RX THEN reportErr('register expected', hln.name)
	         reg := var.d
	      ELSEIF n.data = IT_REG
	         reg := n.info
	         var := NIL
	      ELSE
	         reportErr('register expected', hln.name)
	      ENDIF
	      n++
	      IF n.data = ":"
	         n++
	         IF n.data <> IT_OBJECT THEN reportErr('object expected')
	         o := n.info
	         n++
	      ELSEIF var
	         IF var.type.object = NIL THEN reportErr('object expected')
	         o := var.type.object
	      ELSE
	         reportErr('object expected')
	      ENDIF
	      m := findMember(o, t.info.name)
	      IF m = NIL THEN reportErr('unknown member', t.info.name)
	      IF ops[nrofops] <> RX THEN reportErr('wrong type of operand')
	      g_od[nrofops] := reg.num
	      nrofops++
	      IF ops[nrofops] <> DV THEN reportErr('wrong type of operand')
	      g_od[nrofops] := m.offset
	      nrofops++
	      IF n.data <> ")" THEN reportErr('")" expected')
	      n++
	   DEFAULT
	      reportErr('operand syntax for instruction', asm.name)
	   ENDSELECT
	   IF n.data <> ","
	      IF n.data <> 10 THEN reportErr('operand syntax for instruction', asm.name)
	   ELSE
	      n++
	   ENDIF
	ENDWHILE


	-> same nr of operands ?
	IF nrofops <> i.numops THEN reportErr('wrong # of operands for instruction', asm.name)

	-> punctation correct ?
	IF g_punct = 0 -> no punct
	   IF i.punct = PR THEN reportErr('"." required for instruction', asm.name)
	ELSE -> punct used
	   IF i.punct = PN THEN reportErr('"." not allowed for instruction', asm.name)
	ENDIF

	testEval(i.qcode)

ENDPROC n

-> 2.0.0 workaround for now
PROC testEval(code)
	DEF array[256]:ARRAY
ENDPROC Eval(code)

-> v50: killed two args, now returns reg instead  (DREG)
PROC newMemory(sizeas,sizeas2) OF ppcsysv
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
	DEF ro, rd
	ro := DREG
	rd := IREG0
	self.saveObtained(regusecopy)
	inst_copy(sizeas,sizeas2, DREG,IREG0)
	inst_goslab(self.fastnew_lif)
	ro, rd := self.secureReturn(ro, rd, regusecopy)
	self.loadObtained(regusecopy)
	clearRegs()
ENDPROC rd

PROC endMemory(sizeas,sizeas2, as,as2) OF ppcsysv
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
	self.saveObtained(regusecopy)
	inst_copy(as,as2, DREG,IREG0)
	inst_copy(sizeas,sizeas2, DREG,IREG1)
	inst_goslab(self.fastdispose_lif)
	self.loadObtained(regusecopy)
	clearRegs()
	inst_copy(DV,NIL,as,as2)
ENDPROC

-> v50: killed two args, now returns register (AREG)
PROC newClassObject(obj:PTR TO object) OF ppcsysv
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object
	DEF ro, rd
	self.saveObtained(regusecopy)
	inst_copy(DV,obj.sizeof+IF obj.flags AND OFLAG_NOCLASSINFO THEN 0 ELSE 4, DREG,IREG0)
	inst_goslab(self.fastnew_lif)
	inst_copy(DREG,IREG0, AREG,ATEMP)
	IF (obj.flags AND OFLAG_NOCLASSINFO) = FALSE
	   inst_labadr(obj, SELFREG)
	   inst_copy(AREG,SELFREG,ARXPO,AxSizeOfs(ATEMP,4,0))
	   inst_incarx(ATEMP,4)
	ENDIF
	self.loadObtained(regusecopy)
	clearRegs()
ENDPROC ATEMP

PROC endClassObject(obj:PTR TO object, varo, vard) OF ppcsysv
	DEF regusecopy[REGUSETABSIZE]:ARRAY OF oreg, o:PTR TO object, meth:PTR TO proc
	DEF nil
	nil := newLabel()
	inst_bic(varo,vard,ISEQ,DV,NIL,nil)
	IF obj.destofs > -1  -> call .end() ?
	   self.saveObtained(regusecopy)
	   meth := obj.methodtable[Shr(obj.destofs,2)]
	   inst_copy(varo,vard, AREG, SELFREG)
	   IF meth.flags AND PROCF_CLMETH -> 1.6.1
	      IF meth.offset
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
	inst_copy(varo,vard, DREG,IREG0)
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
	inst_goslab(self.fastdispose_lif)
	self.loadObtained(regusecopy)
	clearRegs()
	inst_copy(DV,NIL, varo,vard)
	def_label(nil)
ENDPROC

PROC nilCheck(areg,line) OF ppc
	DEF lab:PTR TO genlab
	lab := newLabel()
	ppccmpi(0,0,areg,0)
	ppcbclab(PPCNE,lab,0,0)
	ppcliw(3,"NIL")
	ppcliw(4,line)
	ppcblab(self.throw_lif,0,1)->g_internalfuncsppc[7],0,1) -> Throw()
	putLabel(lab)
ENDPROC

/*

	  System V4: (MorphOS)

	  R0        : [scratch]
	  R1        : stack pointer
	  R2        : reserved for system
	  R3        : librarybases [scratch]
	  R3 - R5   : return-values [scratch]
	  R3 - R10  : parameters to functions and procedures [scratch]
	  R11, R12  : [scratch]
	  R13       : global environment (rw-data pointer)
	  R14 - R19 : not used [save]
	  R20 - R31 : register variables [save]

	  F1        : float return
	  F1 - F8   : float args
	  F0 - F13  : float scratch
	  F0,  F13  : non obtainable scratch
	  F20 - F31 : float regvars


*/


-> BI-field
-> lt=0
-> gt=1
-> eq=2
-> BO-field
-> false=4
-> true=12

-> 1.10.0
-> todo: optimise wide/wideptr[]/bla.wide := wide/wideptr[]/bla.wide
PROC copy(o1,d1:PTR TO var, o2,d2:PTR TO var) OF ppc
	DEF treg

	#ifdef DBG_PPCGEN
	DEBUGF('ppc.copy(o1=\d,d1=$\h,o2=\d,d2=$\h\n', o1,d1,o2,d2)
	#endif
	   IF o1 = RX
	      self.regto(d1, o2,d2)
	   ELSEIF o2 = RX
	      self.toreg(o1,d1, d2)
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
	         self.regto(d1.treg, o2,d2)
	      ELSE
	         treg := self.obtainDREG()
	         self.toreg(o1,d1, treg)
	         self.regto(treg, o2,d2)
	         self.releaseDREG(treg,0)
	      ENDIF
	   ELSEIF o1 = X2R  -> fix 20100513
	      self.x2rto(d1, o2, d2)
	   ELSEIF o1 = VAR64
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
	      ELSE -> 1.10.0
	         treg := self.obtainD64()
	         self.tod64(o1,d1, treg)
	         self.d64to(treg, o2,d2)
	         self.releaseD64(treg,0)
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
	   ELSEIF o2 = VAR64 -> 1.6.1
	      IF d2.type.flags AND MEMBF_FLOAT
	         treg := self.obtainFREG()
	         self.tofreg(o1,d1, treg)
	         self.fregto(treg, o2,d2)
	         self.releaseFREG(treg,0)
	      ELSE -> 1.10.0
	         treg := self.obtainD64()
	         self.tod64(o1,d1, treg)
	         self.d64to(treg, o2,d2)
	         self.releaseD64(treg,0)
	      ENDIF
	   ELSE
	      treg := self.obtainDREG()
	      self.toreg(o1,d1, treg)
	      self.regto(treg, o2,d2)
	      self.releaseDREG(treg,0)
	   ENDIF

ENDPROC


-> 1.10.0
PROC dispatcher2(iid,o1,d1:PTR TO var,o2,d2:PTR TO var,e1,e2,e3)  OF ppc

	#ifdef DBG_PPCGEN
	DEBUGF('dispatcher2: iid=\d,o1=\d,d1=$\h,o2=\d,d2=$\h,e1=\d,e2=\d,e3=\d\n', iid,o1,d1,o2,d2,e1,e2,e3)
	#endif

	SELECT 256 OF iid
	CASE IID_FSICREG
	   -> e1:cond
	   -> e2:freg
	   -> e3:reg

	   g_lastx.iid := IID_FSICREG
	   g_lastx.start := g_codeptr   -> not made use of this yet
	   g_lastx.o := FPX
	   g_lastx.d := d1
	   g_lastx.cond := e1
	   g_lastx.rx := e2

	   ppcfcmpu(0,e2,d1)

	   ppcmfcr(e3)
	   SELECT e1
	   CASE ISEQ ; ppcrlwinm(e3,e3,2,0,31,0) -> rotlwi e3,e3,2
	             ; ppcsrawi(e3,e3,31,0)
	   CASE ISNE ; ppcrlwinm(e3,e3,2,0,31,0) -> rotlwi e3,e3,2
	             ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   CASE ISGT ; ppcrlwinm(e3,e3,1,0,31,0) -> rotlwi e3,e3,1
	             ; ppcsrawi(e3,e3,31,0)
	   CASE ISLT ; ppcsrawi(e3,e3,31,0)
	   CASE ISGE ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   CASE ISLE ; ppcrlwinm(e3,e3,1,0,31,0) -> rotlwi e3,e3,1
	             ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   ENDSELECT
	   ppcSetFReg(e3,NIL,NIL)
	   g_lastx.end := g_codeptr -> 1.5.4
	CASE IID_I2FREG
	   -> 1.10.0 now inline !
	   -> trashes r11, r0, f13 !
	   ppcb(3,0,1)
	   Put32($43300000)
	   Put32($80000000)
	   ppcmfspr(11, 8)
	   ppclfd(13,11,0)
	   ppcaddis(0,0,$4330)
	   ppcstw(0,1,-8) -> upper half
	   ppcxoris(d1,d1,$8000)
	   ppcstw(d1,1,-4) -> lower half
	   ppclfd(d2,1,-8)
	   ppcfsub(d2,d2,13,0)
	   ppcSetReg(d1,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(d2,NIL,NIL)
	   ppcSetFReg(13,NIL,NIL)
	CASE IID_F2IREG
	   IF g_optroundnear THEN ppcfctiw(d1,d1,0) ELSE ppcfctiwz(d1,d1,0)
	   ppcSetFReg(d1,NIL,NIL)
	   ppcstfd(d1,1,-8)
	   ppclwz(d2,1,-4)
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_FMULREG
	   ppcfmul(d2,d2,d1,0)
	   ppcSetFReg(d2,NIL,NIL)
	CASE IID_FDIVREG
	   ppcfdiv(d2,d2,d1,0)
	   ppcSetFReg(d2,NIL,NIL)
	CASE IID_FADDREG
	   ppcfadd(d2,d2,d1,0)
	   ppcSetFReg(d2,NIL,NIL)
	CASE IID_FSUBREG
	   ppcfsub(d2,d2,d1,0)
	   ppcSetFReg(d2,NIL,NIL)
	CASE IID_FNEGREG
	   ppcfneg(d2,d2,0)
	   ppcSetFReg(d2,NIL,NIL)
	DEFAULT
	      reportIErr('dispatcher2() iid=?')
	ENDSELECT

ENDPROC

-> 1.10.0
PROC dispatcher3(iid,o1,d1:PTR TO var,o2,d2:PTR TO var,e1,e2,e3) OF ppc
	DEF t, codelab
	#ifdef DBG_PPCGEN
	DEBUGF('dispatcher: iid=\d,o1=\d,d1=$\h,o2=\d,d2=$\h,e1=\d,e2=\d,e3=\d\n', iid,o1,d1,o2,d2,e1,e2,e3)
	#endif

	SELECT 256 OF iid
	CASE IID_SICD64 -> 1.10.0
	   ppcCompare64(d1, e2)
	   ppcmfcr(e3)
	   SELECT e1
	   CASE ISEQ ; ppcrlwinm(e3,e3,2,0,31,0) -> rotlwi e3,e3,2
	             ; ppcsrawi(e3,e3,31,0)
	   CASE ISNE ; ppcrlwinm(e3,e3,2,0,31,0) -> rotlwi e3,e3,2
	             ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   CASE ISGT ; ppcrlwinm(e3,e3,1,0,31,0) -> rotlwi e3,e3,1
	             ; ppcsrawi(e3,e3,31,0)
	   CASE ISLT ; ppcsrawi(e3,e3,31,0)
	   CASE ISGE ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   CASE ISLE ; ppcrlwinm(e3,e3,1,0,31,0) -> rotlwi e3,e3,1
	             ; ppcsrawi(e3,e3,31,0)
	             ; ppcnor(e3,e3,e3,0)
	   ENDSELECT
	   ppcSetReg(e3, NIL,NIL) -> trashed
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_MULD64 -> 1.10.0
	   t := self.obtainDREG()
	   ppclwz(12, 1, d2+4) -> l2
	   ppclwz(11, 1, d1+4) -> l1
	   ppcmullw(0, 12, 11, 0, 0) -> l1*l2
	   ppcstw(0, 1, d2+4) -> low done
	   ppcmulhwu(t,12,11,0) -> l1*l2
	   ppclwz(0, 1, d2)   -> h2
	   ppcmullw(0, 0, 11, 0, 0) -> h2*l1
	   ppcadd(t, t, 0, 0, 0) -> Rt+=R0
	   ppclwz(0, 1, d1)   -> h1
	   ppcmullw(0, 0, 12, 0, 0) -> h1*l2
	   ppcadd(t, t, 0, 0, 0) -> Rt+=R0
	   ppcstw(t, 1, d2) -> high done
	   self.releaseDREG(t, 0)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	   ppcSetReg(0, NIL,NIL)
	CASE IID_DIVD64 -> 1.10.0
	   ppcaddi(11, 1, d2)
	   ppcaddi(12, 1, d1)
	   ppcblab(self.lif_private_div64,0,1)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_ADDD64 -> 1.10.0
	   ppclwz(11, 1, d2+4) -> l2
	   ppclwz(0, 1, d1+4) -> l1
	   ppcaddc(0, 11, 0, 0, 0)
	   ppcstw(0, 1, d2+4)  -> lr
	   ppclwz(11, 1, d2)   -> h2
	   ppclwz(0, 1, d1)   -> h1
	   ppcadde(0, 11, 0, 0, 0)
	   ppcstw(0, 1, d2)    -> hr
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_SUBD64 -> 1.10.0
	   ppclwz(11, 1, d2+4) -> l2
	   ppclwz(0, 1, d1+4) -> l1
	   ppcsubfc(0, 0, 11, 0, 0)
	   ppcstw(0, 1, d2+4)  -> lr
	   ppclwz(11, 1, d2)   -> h2
	   ppclwz(0, 1, d1)   -> h1
	   ppcsubfe(0, 0, 11, 0, 0)
	   ppcstw(0, 1, d2)    -> hr
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_SHRD64 -> 1.10.0
	   ppcaddi(11, 1, d2)
	   ppcor(d1, 12, d1, 0)
	   ppcblab(self.lif_private_shr64,0,1)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	CASE IID_SHLD64 -> 1.10.0
	   ppcaddi(11, 1, d2)
	   ppcor(d1, 12, d1, 0)
	   ppcblab(self.lif_private_shl64,0,1)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	CASE IID_ANDD64 -> 1.10.0
	   ppclwz(0, 1, d2) -> h2
	   ppclwz(11, 1, d1) -> h1
	   ppcand(0, 0, 11, 0)
	   ppcstw(0, 1, d2) -> hr
	   ppclwz(0, 1, d2+4) -> l2
	   ppclwz(11, 1, d1+4) -> l1
	   ppcand(0, 0, 11, 0)
	   ppcstw(0, 1, d2+4) -> lr
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_ORD64 -> 1.10.0
	   ppclwz(0, 1, d2) -> h2
	   ppclwz(11, 1, d1) -> h1
	   ppcor(0, 0, 11, 0)
	   ppcstw(0, 1, d2) -> hr
	   ppclwz(0, 1, d2+4) -> l2
	   ppclwz(11, 1, d1+4) -> l1
	   ppcor(0, 0, 11, 0)
	   ppcstw(0, 1, d2+4) -> lr
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_XORD64 -> 1.10.0
	   ppclwz(0, 1, d2) -> h2
	   ppclwz(11, 1, d1) -> h1
	   ppcxor(0, 0, 11, 0)
	   ppcstw(0, 1, d2) -> hr
	   ppclwz(0, 1, d2+4) -> l2
	   ppclwz(11, 1, d1+4) -> l1
	   ppcxor(0, 0, 11, 0)
	   ppcstw(0, 1, d2+4) -> lr
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	CASE IID_F2D64 -> 1.10.0
	   ppcstfd(d1, 1, d2)
	   ppcaddi(11, 1, d2)
	   ppcblab(self.lif_private_f2d64,0,1)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	CASE IID_D642F -> 1.10.0
	   ppcaddi(11, 1, d1)
	   ppcblab(self.lif_private_d642f,0,1)
	   ppclfd(d2, 1, d1)
	   ppcSetReg(11,NIL,NIL)
	   ppcSetReg(12,NIL,NIL)
	   ppcSetReg(0,NIL,NIL)
	   ppcSetFReg(d2, NIL,NIL)
	CASE IID_I2D64 -> 1.10.0
	   ppcstw(d1, 1, d2+4)
	   ppcsrawi(d1,0,31,0)
	   ppcstw(0,1,d2)
	   ppcSetReg(0, NIL,NIL)
	CASE IID_D642I -> 1.10.0
	   ppclwz(d2,1,d1+4)
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_NEGD64   -> 1.10.0
	   ppclwz(0, 1, d1+4) -> low
	   ppcnor(0,0,0,0)   -> not
	   ppcaddic(0,0,1)   -> add 1 carrying
	   ppcstw(0, 1, d1+4) -> low result
	   ppclwz(0, 1, d1) -> high
	   ppcnor(0,0,0,0)   -> not
	   ppcaddze(0,0,0,0)   -> add zero extended
	   ppcstw(0, 1, d1) -> high result
	   ppcSetReg(0,NIL,NIL)
	CASE IID_NOTD64   -> 2.2
	   ppclwz(0, 1, d1+4) -> low
	   ppcnor(0,0,0,0)   -> not
	   ppcstw(0, 1, d1+4) -> low result
	   ppclwz(0, 1, d1) -> high
	   ppcnor(0,0,0,0)   -> not
	   ppcstw(0, 1, d1) -> high result
	   ppcSetReg(0,NIL,NIL)
	CASE IID_ABSD64   -> 2.2
	   codelab := newLabel()
	   ppclwz(0, 1, d1) -> high
	   ppccmpi(0,0,0,0) -> cmpwi CR0, R0, 0
	   ppcbclab(PPCGE,codelab,0,0)
	   ppclwz(0, 1, d1+4) -> low
	   ppcnor(0,0,0,0)   -> not
	   ppcaddic(0,0,1)   -> add 1 carrying
	   ppcstw(0, 1, d1+4) -> low result
	   ppclwz(0, 1, d1) -> high
	   ppcnor(0,0,0,0)   -> not
	   ppcaddze(0,0,0,0)   -> add zero extended
	   ppcstw(0, 1, d1) -> high result
	   putLabel(codelab)
	   ppcSetReg(0,NIL,NIL)
	DEFAULT
	   reportIErr('dispatcher3() iid=?')
	ENDSELECT

ENDPROC


PROC dispatcher(iid,o1,d1:PTR TO var,o2,d2:PTR TO var,e1,e2,e3)  OF ppc
	DEF proc:PTR TO proc, t, treg, treg2, t1, t2, t3, t4, t5, cond, reverse, rx
	DEF codelab:PTR TO codelab, fpx
	DEF voff:PTR TO LONG, va:PTR TO var


	#ifdef DBG_PPCGEN
	DEBUGF('dispatcher: iid=\d,o1=\d,d1=$\h,o2=\d,d2=$\h,e1=\d,e2=\d,e3=\d\n', iid,o1,d1,o2,d2,e1,e2,e3)
	#endif


	SELECT 256 OF iid
	CASE IID_VARADR
	   IF d1.link
	      ppcaddilab(d2, d1.breg, d1)
	   ELSE
	      ppcaddi(d2,d1.breg,d1.offset)
	   ENDIF
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_LABADR
	   ppclacode(d2,d1)
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_NEGREG
	   ppcneg(d1,d1,0,0)
	   ppcSetReg(d1,NIL,NIL)
	CASE IID_NOTREG -> 1.10.0
	   ppcnor(d1,d1,d1,0)
	   ppcSetReg(d1,NIL,NIL)
	CASE IID_ABSREG -> 2.2
	   ppc_abs(d1,d1)
	   ppcSetReg(d1,NIL,NIL)
	CASE IID_RET
	   IF g_exceptlab THEN self.restorehandler() -> 1.8.0
	   IF g_procendlab THEN ppcblab(g_procendlab,0,0) ELSE ppcbclr(0,0,0)
	CASE IID_BIC
	   cond := e1
	   t5 := FALSE
#ifdef DOSICBICOPTI
	   IF g_codeptr = g_lastx.end -> sic just before us.
	   IF o2 = DV
	   IF d2 = NIL
	   IF d1 = g_lastx.rx
	   IF o1 = RX
	   IF g_lastx.iid = IID_SICREG
	      ->IF (g_lastx.o = RX) OR (g_lastx.o = DV)
	         IF cond = ISNE  -> IF, WHILE, UNTILN
	            g_codeptr := g_lastx.start -> back up
	            o2 := RX
	            d2 := g_lastx.rx
	            o1 := g_lastx.o
	            d1 := g_lastx.d
	            cond := g_lastx.cond
	         ELSEIF cond = ISEQ -> UNTIL, IFN, WHILEN
	            g_codeptr := g_lastx.start -> back up
	            o2 := RX
	            d2 := g_lastx.rx
	            o1 := g_lastx.o
	            d1 := g_lastx.d
	            cond := reverseCondition(g_lastx.cond)
	         ENDIF
	      ->ENDIF
	   ELSEIF g_lastx.iid = IID_FSICREG   -> 2.1
	      IF g_lastx.o = FPX
	         t5 := TRUE -> bool to do float comparison futher below
	         IF cond = ISNE  -> IF, WHILE, UNTILN
	            g_codeptr := g_lastx.start -> back up
	            o2 := FPX
	            d2 := g_lastx.rx
	            o1 := g_lastx.o
	            d1 := g_lastx.d
	            cond := g_lastx.cond
	         ELSEIF cond = ISEQ -> UNTIL, IFN, WHILEN
	            g_codeptr := g_lastx.start -> back up
	            o2 := FPX
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
	   ENDIF
#endif
	   codelab := e2
	   IF t5
	      -> float comparison opti
	      ppcfcmpu(0,d2,d1)
	      reverse := FALSE
	   ELSE
	      reverse := self.compareReg(o1,d1, o2,d2, cond)
	   ENDIF
	   SELECT cond
	   CASE ISEQ ; ppcbclab(PPCEQ,codelab,0,0)
	   CASE ISNE ; ppcbclab(PPCNE,codelab,0,0)
	   CASE ISGT ; IF reverse THEN ppcbclab(PPCLT,codelab,0,0) ELSE ppcbclab(PPCGT,codelab,0,0)
	   CASE ISLT ; IF reverse THEN ppcbclab(PPCGT,codelab,0,0) ELSE ppcbclab(PPCLT,codelab,0,0)
	   CASE ISGE ; IF reverse THEN ppcbclab(PPCLE,codelab,0,0) ELSE ppcbclab(PPCGE,codelab,0,0)
	   CASE ISLE ; IF reverse THEN ppcbclab(PPCGE,codelab,0,0) ELSE ppcbclab(PPCLE,codelab,0,0)
	   DEFAULT   ; Throw("PPC", 'mpc_bic() - unknown cond')
	   ENDSELECT
	CASE IID_SICREG
	   cond := e1
	   rx := e2

	   g_lastx.iid := IID_SICREG -> 1.8.0. forgot it 1.7 something !
	   g_lastx.start := g_codeptr
	   g_lastx.o := o1
	   g_lastx.d := d1
	   g_lastx.cond := cond
	   g_lastx.rx := rx

	   t2 := self.obtainDREG()

	   SELECT o1
	   CASE RX
	      t1 := d1
	   CASE DV
	      t1 := DTEMP->self.obtainDREG()
	      self.toreg(o1,d1, t1)
	      ->t1 := self.releaseDREG(t1,1)
	   DEFAULT
	      Throw("PPC", 'iid_sicreg o1')
	   ENDSELECT

	   SELECT cond
	   CASE ISEQ
	      ppcsubf(t2,rx,t1,0,0)
	      ppcaddic(t2,t2,-1)
	      ppcsubfe(rx,rx,rx,0,0)
	   CASE ISNE
	      ppcsubf(t2,rx,t1,0,0)
	      ppcsubfic(t2,t2,0)
	      ppcsubfe(rx,rx,rx,0,0)
	   CASE ISGT
	      t3 := self.obtainDREG()
	      ppcsubfc(t2,rx,t1,0,0)
	      ppcrlwinm(rx,t2,32-31,31,31,0) -> srwi t2,rx,31
	      ppcrlwinm(t1,t3,32-31,31,31,0) -> srwi t3,t1,31
	      ppcsubfe(rx,t3,t2,0,0)
	      ppcSetReg(t3,NIL,NIL)
	      self.releaseDREG(t3,0)
	   CASE ISLT
	      t3 := self.obtainDREG()
	      ppcsubfc(t2,t1,rx,0,0)
	      ppcrlwinm(t1,t2,32-31,31,31,0) -> srwi t2,t1,31
	      ppcrlwinm(rx,t3,32-31,31,31,0) -> srwi t3,rx,31
	      ppcsubfe(rx,t3,t2,0,0)
	      ppcSetReg(t3,NIL,NIL)
	      self.releaseDREG(t3,0)
	   CASE ISGE
	      t3 := self.obtainDREG()
	      ppcxoris(t1,t2,$8000) -> xoris t3,t1,$8000
	      ppcsubf(t3,t1,rx,0,0)
	      ppcaddc(0,t2,t3,0,0)
	      ppcsubfe(rx,rx,rx,0,0)
	      ppcSetReg(t3,NIL,NIL)
	      self.releaseDREG(t3,0)
	   CASE ISLE
	      t3 := self.obtainDREG()
	      ppcxoris(rx,t2,$8000) -> xoris t3,rx, $8000
	      ppcsubf(t3,rx,t1,0,0)
	      ppcaddc(0,t2,t3,0,0)
	      ppcsubfe(rx,rx,rx,0,0)
	      ppcSetReg(t3,NIL,NIL)
	      self.releaseDREG(t3,0)
	   DEFAULT
	      Throw("PPC", 'mpc_sicreg() - unknown cond')
	   ENDSELECT
	   self.releaseDREG(t2,0)
	   ppcSetReg(t2, NIL,NIL) -> trashed
	   ppcSetReg(rx, NIL,NIL) -> trashed
	   g_lastx.end := g_codeptr   -> 1.5.4
	CASE IID_MULREG
	   SELECT o1
	   CASE DV  ; IF (Abs(d1) > $7FFF) OR (d1 = $80000000)
	            ;    treg := self.obtainDREG()
	            ;    self.toreg(o1,d1, treg)
	            ;    ppcmullw(d2,d2,treg,0,0)
	            ;    self.releaseDREG(treg,0)
	            ; ELSE
	                 ppcmulli(d2,d2,d1)  -> 16bit!
	            ; ENDIF
	   CASE RX  ; ppcmullw(d2,d2,d1,0,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_DIVREG
	   SELECT o1
	   CASE RX ; ppcdivw(d2,d2,d1,0,0)
	   CASE DV -> changed 1.10.0
#ifdef USEMULDIVSHIFT
	      treg := self.obtainDREG()
	      ppcdiviw(treg, d2, d1)
	      self.toreg(RX,treg,d2)
	      self.releaseDREG(treg,0)
#endif
#ifndef USEMULDIVSHIFT
	      treg := self.obtainDREG()
	      self.toreg(o1,d1,treg) ; ppcdivw(d2,d2,treg,0,0)
	      self.releaseDREG(treg,0)
#endif
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_SHRREG
	   SELECT o1
	   CASE DV ; ppcrlwinm(d2,d2,32-d1,d1,31,0)
	   CASE RX   ; ppcsrw(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	 CASE IID_ASRREG -> 1.10.0
	   SELECT o1
	   CASE DV   ; ppcsrawi(d2,d2,d1,0)
	   CASE RX   ; ppcsraw(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_SHLREG
	   SELECT o1
	   CASE DV ; ppcrlwinm(d2,d2,d1,0,31-d1,0)
	   CASE RX ; ppcslw(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_ADDREG
	   SELECT o1
	   CASE DV ; ppcaddiw(d2,d2,d1)
	   CASE RX ; ppcadd(d2,d2,d1,0,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_SUBREG
	   SELECT o1
	   CASE DV ; d1 := -d1
	           ; ppcaddiw(d2,d2,d1)
	   CASE RX ; ppcsubf(d2,d1,d2,0,0)
	   ENDSELECT
	   ppcSetReg(d2, NIL,NIL)
	CASE IID_INCVAR -> 1.5.3: d1:var, d2:val
	   IF d1.o = RX
	      ppcaddi(d1.d, d1.d, d2)
	      ppcSetReg(d1.d, NIL,NIL)
	   ELSEIF d1.o = VAR64 -> 1.10.0
	      treg := self.obtainDREG()
	      IF d1.link
	         ppclfdlab(13, d1.breg, d1)
	         ppcstfd(13, 1, -8)
	         ppclwz(0, 1, -8)
	         ppclwz(treg, 1, -4)
	      ELSE
	         ppclwz(0, d1.breg, d1.offset)
	         ppclwz(treg, d1.breg, d1.offset+4)
	      ENDIF
	      ppcaddic(treg, treg, d2)
	      IF d2 < 0 THEN ppcsubfze(0,0,0,0) ELSE ppcaddze(0, 0, 0, 0)  -> fixed 2.2.2
	      IF d1.link
	         ppcstw(0, 1, -8)
	         ppcstw(treg, 1, -4)
	         ppclfd(13, 1, -8)
	         ppcstfdlab(13, d1.breg, d1)
	      ELSE
	         ppcstw(0, d1.breg, d1.offset)
	         ppcstw(treg, d1.breg, d1.offset+4)
	      ENDIF
	      self.releaseDREG(treg,0)
	   ELSE
	      IF d1.intreg AND d1.trok
	         ppcaddi(d1.treg,d1.treg, d2)
	         ppcSetReg(d1.treg,NIL,NIL)
	         self.regto(d1.treg, VAR,d1)
	      ELSE
	         treg := self.obtainDREG()
	         self.toreg(VAR,d1, treg)
	         ppcaddi(treg,treg, d2)
	         ppcSetReg(treg,NIL,NIL)
	         self.regto(treg, VAR,d1)
	         self.releaseDREG(treg,0)
	      ENDIF
	   ENDIF
	CASE IID_INCARX -> 1.5.3 d1:arx, d2:val
	   ppcaddi(d1,d1,d2)
	   ppcSetReg(d1,NIL,NIL)
	CASE IID_ANDREG
	   SELECT o1
	   CASE DV
	      IF d1 AND $FFFF0000
	         treg := self.obtainDREG()
	         ppcliw(treg,d1)
	         ppcSetReg(treg,NIL,NIL)
	         ppcand(d2,d2,treg,0)
	         self.releaseDREG(treg,0)
	      ELSE
	         ppcandi_(d2,d2,d1)
	      ENDIF
	   CASE RX
	      ppcand(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_ORREG
	   SELECT o1
	   CASE DV
	      ppcori(d2,d2,d1 AND $FFFF)
	      IF d1 AND $FFFF0000 THEN ppcoris(d2,d2,Shr(d1,16) AND $FFFF)
	   CASE RX
	      ppcor(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_XORREG -> 1.10.0
	   SELECT o1
	   CASE DV
	      ppcxori(d2,d2,d1 AND $FFFF)
	      IF d1 AND $FFFF0000 THEN ppcxoris(d2,d2,Shr(d1,16) AND $FFFF)
	   CASE RX
	      ppcxor(d2,d2,d1,0)
	   ENDSELECT
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_GOLAB
	   ppcblab(d1, 0, 0)
	CASE IID_GOARX
	   ppcmtspr(d1, 9) -> to ctr reg
	   ppcbcctr(20,0,0) -> branch ctr
	CASE IID_GETRWD
	   IF g_optmodule
	      g_rwreflist := NEW [g_rwreflist, g_codeptr-g_codebuf]:rwref
	      ppcaddi(d2,GLOBREG,d1 AND $FFFF)
	      ppcaddis(d2,d2,Shr(d1,16) AND $FFFF)
	   ELSE
	      d1 := d1 - g_databufsize - IVARSSIZE
	      IF Abs(d1) < $7FFF
	         ppcaddi(d2,GLOBREG,d1 AND $FFFF)
	      ELSE
	         ppcaddi(d2,GLOBREG,d1 AND $FFFF)
	         ppcaddis(d2,d2,Shr(d1,16) AND $FFFF + IF d1 AND $8000 THEN 1 ELSE 0)
	      ENDIF
	   ENDIF
	   ppcSetReg(d2,NIL,NIL)
	CASE IID_PUSH  -> 1.8.1
	   ->#ifdef DBG_TEMP
	   ->DEBUGF('iid_push: g_localstackoffset = \d, e1 = \d\n', g_localstackoffset, e1)
	   ->#endif
	   IF g_localstackoffset >= g_currentproc.framesize THEN reportIErr('pushed too far')
	   IF e1 = 4
	      treg := self.ppcRegisterizeD(o1,d1)
	      self.regto(treg, ARXPO,AxSizeOfs(STACKREG,4,g_localstackoffset))
	      g_localstackoffset++
	   ELSE
	      treg := self.ppcRegisterizeF(o1,d1)
	      self.fregto(treg, ARXPO,AxSizeOfs(STACKREG,8,g_localstackoffset))
	      g_localstackoffset := g_localstackoffset + 8
	   ENDIF
	CASE IID_POP   -> 1.8.1
	   ->#ifdef DBG_TEMP
	   ->DEBUGF('iid_pop: g_localstackoffset = \d, e1 = \d, g_localstackstart = \d\n',
	   ->g_localstackoffset, e1, g_localstackstart)
	   ->#endif
	   IF e1 = 4
	      g_localstackoffset--
	      IF o1 = RX
	         self.toreg(ARXPO,AxSizeOfs(STACKREG,4,g_localstackoffset), d1)
	      ELSE
	         treg := DTEMP
	         self.toreg(ARXPO,AxSizeOfs(STACKREG,4,g_localstackoffset), treg)
	         self.regto(treg, o1,d1)
	      ENDIF
	   ELSE
	      g_localstackoffset := g_localstackoffset - 8
	      IF o1 = FPX
	         self.tofreg(ARXPO,AxSizeOfs(STACKREG,8,g_localstackoffset), d1)
	      ELSE
	         treg := FTEMP
	         self.tofreg(ARXPO,AxSizeOfs(STACKREG,8,g_localstackoffset), treg)
	         self.fregto(treg, o1,d1)
	      ENDIF
	   ENDIF
	   IF g_localstackoffset < g_localstackstart THEN reportIErr('poped too far')

	CASE IID_GETIMMSTR
	   /* 1.10.0 keep this code around!
	   t1 := EstrLen(e1)
	   t2 := t1 + 1 + 3 AND -4
	   ppcb(Div(t2,4)+1, 0, 1)
	   CopyMem(e1, g_codeptr, t1 + 1)
	   g_codeptr := g_codeptr + t2
	   ppcmfspr(e2,8)
	   */
	   -> 1.10.0 now uses REF1616 for immedstrings
	   t := addRelStr(e1)
	   ppclacode(e2, t)
	   ppcSetReg(e2,NIL,NIL)
	CASE IID_GOSLAB
	   ppcblab(d1, 0, 1)
	CASE IID_GOSARX
	   ppcmtspr(d1,9)
	   ppcbcctr(20,0,1)
	DEFAULT
	   reportIErr('dispatcher() iid=?')
	ENDSELECT


ENDPROC

-> new stackframe,R1 (the old one was just stupid)
-> v45:

/*
00: old frame save
04: linkreg save (if linkreg will be trashed, or HANDLE is used)
08: parameter store area. Max(proc.maxcallargs-8*4,0) bytes.
...................
PV: arguments 1..8. Min(proc.nrofargs*4,32) bytes.
..................
LV: local variables. proc.nroflocals*4 bytes.
..................
ES: exceptstruct - 12 bytes (if HANDLE)
RV: nvregs save area - 48 bytes (if HANDLE)
LS: local stack block. size=proc.maxcllargs+proc.maxcalldepth*8
...................
-- 16 byte alignment --
LD: local data block
...................
-- 16 byte alignment --
*/

-> 1.10.0: added WIDE stuff
PROC prochead(alloctab:PTR TO LONG,tabsize, allocftab:PTR TO LONG, ftabsize) OF ppcsysv
	DEF voff:PTR TO LONG, va:PTR TO var, a
	DEF treg=31, freg=31, vreg=31
	DEF t, proc:PTR TO proc, dh, dl

	clearRegs()
	proc := g_currentproc
	g_procendlab := newLabel()
	g_exceptlab := IF proc.handle THEN newLabel() ELSE NIL
	g_endexceptlab := IF proc.handle THEN newLabel() ELSE NIL
	putAlign(4)
	IF currentOffset() = NIL THEN ppcor(0,0,0,0) -> v47, modules, v52
	putLabel(proc)

	/* space for suproutine params 9..n */
	voff := SIZEOF_FRAMEPPC + (Max(proc.maxcallargs-4,0)*8)

	/*
	**  1. Allocate (v.offset) arguments and locals
	**  2. Allocate User and auto registers
	*/


	FOR a := 0 TO proc.nrofargs-1
	   va := proc.argarray[a]
	   IF va.rtype = PPCARGRTYPE_RX -> RX
	      va.offset := voff++
	      IF va.o = RX
	         va.d := treg
	         IF treg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	         treg--
	      ELSE
	         va.intreg := TRUE
	         va.treg := va.rnum
	      ENDIF
	   ELSEIF va.rtype = PPCARGRTYPE_STACKRX
	      -> we do not know va.offset at this time, set it later
	      IF va.o = RX
	         va.d := treg
	         IF treg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	         treg--
	      ENDIF
	   ELSEIF va.rtype = PPCARGRTYPE_FX      -> FX
	      voff := voff + 7 AND -8
	      va.offset := voff
	      voff := voff + 8
	      IF va.o = FPX
	         va.d := freg
	         IF freg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	         freg--
	      ELSE
	         va.intreg := TRUE
	         va.treg := va.rnum
	      ENDIF
	   ELSEIF va.rtype = PPCARGRTYPE_VX -> Vx
	      -> not used yet
	   ELSEIF va.rtype = PPCARGRTYPE_RX2 -> RX2/VAR64
	      voff := voff + 7 AND -8
	      va.offset := voff
	      voff := voff + 8
	      va.intreg := FALSE
	      va.trok := FALSE
	      va.treg := NIL
	   ELSEIF va.rtype = PPCARGRTYPE_STACKRX2 -> RX2/VAR64 on stack
	      -> set offset later
	   ENDIF
	ENDFOR

	/* locals32 */
	va := proc.locals32
	WHILE va
	   va.offset := voff++
	   IF va.o = RX
	      va.d := treg
	      IF treg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	      treg--
	   ENDIF
	   va := va.next
	ENDWHILE

	/* locals64 */
	voff := voff + 7 AND -8
	va := proc.locals64
	WHILE va
	   va.offset := voff
	   voff := voff + 8
	   IF va.type.flags AND MEMBF_FLOAT
	      IF va.o = FPX
	         va.d := freg
	         IF freg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	         freg--
	      ENDIF
	   ENDIF
	   va := va.next
	ENDWHILE

	/* locals128 v48 */
	voff := voff + 15 AND -16
	va := proc.locals128
	WHILE va
	   va.offset := voff
	   voff := voff + 16
	   IF va.o = VX
	      va.d := vreg
	      IF vreg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	      vreg--
	   ENDIF
	   va := va.next
	ENDWHILE

	/* locals00 */
	va := proc.locals00
	WHILE va
	   IF va.o = RX
	      va.d := treg
	      IF treg < 20 THEN reportErr('Too many registers allocated for procedure', proc.name)
	      treg--
	   ENDIF
	   voff := voff + 3 AND -4
	   SELECT 256 OF va.type.esize
	   CASE 1
	      IF va.cmplx THEN voff++
	      va.offset := voff
	      voff := voff + va.type.numes
	   CASE 2
	      va.offset := voff
	      voff := voff + (2 * va.type.numes)
	   CASE 4
	      IF va.cmplx THEN voff++
	      va.offset := voff
	      voff := voff + (4 * va.type.numes)
	   CASE 8
	      va.offset := voff
	      voff := voff + (8 * va.type.numes)
	   CASE 16
	      va.offset := voff
	      voff := voff + (16 * va.type.numes)
	   CASE 255 -> object
	      IF va.type.object.nrofmethods
	      IF va.type.numes = 1
	         voff++
	      ENDIF ; ENDIF
	      va.offset := voff
	      voff := voff + (va.type.object.sizeof * va.type.numes)
	   ENDSELECT
	   va := va.next
	ENDWHILE

	voff := voff + 3 AND -4


	/* auto reg alloc */
	FOR a := 0 TO tabsize-1
	   EXIT (g_numregalloc<>-1) AND ((31-treg) >= g_numregalloc)
	   va := alloctab[a]
	   IF treg > 19
	      va.o := RX
	      va.d := treg
	      #ifdef DBG_PPCGEN
	      DEBUGF('PROC "\s" stuffed variable "\s" (use=\d) into register r\d\n', proc.name, va.hln.name, va.usage, treg)
	      #endif
	      treg--
	   ENDIF
	ENDFOR

	FOR a := 0 TO ftabsize-1
	   EXIT (g_numfregalloc<>-1) AND ((31-freg) >= g_numfregalloc)
	   va := allocftab[a]
	   ->IF va.float
	      IF freg > 19
	         va.o := FPX
	         va.d := freg
	         #ifdef DBG_PPCGEN
	         DEBUGF('PROC "\s" stuffed variable "\s" (use=\d) into register f\d\n', proc.name, va.hln.name, va.usage, freg)
	         #endif
	         freg--
	      ENDIF
	   ->ELSE
	      ->
	   ->ENDIF

	ENDFOR

	/*
	FOR a := 0 TO vtabsize-1
	   EXIT (g_numvregalloc<>-1) AND ((31-vreg) >= g_numvregalloc)
	   va := allocvtab[a]
	   IF vreg > 19
	      va.o := VX
	      va.d := vreg
	      #ifdef DBG_PPCGEN
	      DEBUGF('PROC "\s" stuffed variable "\s" (use=\d) into register v\d\n', proc.name, va.hln.name, va.usage, vreg)
	      #endif
	      vreg--
	   ENDIF

	ENDFOR
	*/

	/* Now lets allocate space to save/restore regvars */
	FOR a := 0 TO proc.nrofargs-1
	   va := proc.argarray[a]
	   IF va.rtype = PPCARGRTYPE_RX          -> RX
	      va.saveoffset := va.offset
	   ELSEIF va.rtype = PPCARGRTYPE_STACKRX
	      IF va.o = RX THEN va.saveoffset := voff++
	   ELSEIF va.rtype = PPCARGRTYPE_FX      -> FX
	      va.saveoffset := va.offset
	   ENDIF
	ENDFOR

	va := proc.locals32
	WHILE va
	   va.saveoffset := va.offset
	   va := va.next
	ENDWHILE

	/* locals64 */
	va := proc.locals64
	WHILE va
	   va.saveoffset := va.offset
	   va := va.next
	ENDWHILE

	/* locals128 v48 */
	va := proc.locals128
	WHILE va
	   va.saveoffset := va.offset
	   va := va.next
	ENDWHILE

	/* locals00 */
	va := proc.locals00
	WHILE va
	   IF va.o = RX THEN va.saveoffset := voff++
	   va := va.next
	ENDWHILE

	/* exceptstruct ? */
	IF proc.handle
	   self.p_esoffset := voff
	  voff := voff + SIZEOF_ES + 48 + 96 -> exceptstruct + nvregs save
	ENDIF

	-> 1.10.0
	D64TEMP := voff
	voff := voff + 8

	/* local stack inside the frame */
	g_localstackoffset := voff
	g_localstackstart := voff
	voff := voff +
	       (proc.maxcalldepth * proc.maxcallargs * 8) +
	       (proc.maxcallargs + proc.maxcalldepth * 8) +
	       64                         -> d64 regs

	g_d64bottom := voff
	g_lastd64 := voff

	/* framesize */
	proc.framesize := voff + 15 AND -16

	/* args 9..n */
	voff := proc.framesize + SIZEOF_FRAMEPPC
	FOR a := 0 TO proc.nrofargs-1
	   va := proc.argarray[a]
	   IF va.rtype = PPCARGRTYPE_STACKRX
	      va.offset := voff
	      voff++
	   ELSEIF va.rtype = PPCARGRTYPE_STACKRX2 -> v 2.0
	      va.offset := voff
	      voff++ ; voff++
	   ENDIF
	ENDFOR


	proc.numregalloc := 31-treg
	proc.numfregalloc := 31-freg


	/* setup frame */
	ppcstwu(FRAMEREG, STACKREG, -proc.framesize)

	ppcmfspr(0,8)
	ppcstw(0, FRAMEREG, proc.framesize + FRAMEPPC_LINKREG)

	/* exception stuff */
	IF proc.handle
	   ppclwzlab(0, GLOBREG, self.pvar_exceptstruct) -> save old struct pointer
	   ppcstw(0, FRAMEREG, self.p_esoffset) -> in frame
	   ppcaddi(0, FRAMEREG, self.p_esoffset) -> address of exceptstruct in r0
	   ppcstwlab(0, GLOBREG, self.pvar_exceptstruct) -> set new handlerstruct
	   ppcstw(1,FRAMEREG,self.p_esoffset+4) -> save stackpointer in struct
	   ppclacode(11, g_exceptlab)
	   ppcstw(11, FRAMEREG, self.p_esoffset+8) -> save exceptcode in struct

	   ->2.2.3
	  ppclwzlab(11, GLOBREG, self.ivar_exception)
	  ppcstw(11, FRAMEREG, self.p_esoffset+ES_OLDEXCEPTION)

	   -> nv-regs
	  ppcstmw(20,1,self.p_esoffset+SIZEOF_ES)
	  t := self.p_esoffset+SIZEOF_ES+48
	   FOR a := 31 TO 20 STEP -1
	      ppcstfd(a,FRAMEREG,t)
	      t := t + 8
	   ENDFOR
	ELSEIF (treg<31) OR (freg<31) OR (vreg<31) -> save user regvars ?
	   va := proc.args32
	   WHILE va
	      IF va.o = RX THEN ppcstw(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.args64
	   WHILE va
	      IF va.o = FPX THEN ppcstfd(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.args128
	   WHILE va
	      IF va.o = VX THEN reportIErr(' vector reg var used')
	      va := va.next
	   ENDWHILE
	   va := proc.locals32
	   WHILE va
	      IF va.o = RX THEN ppcstw(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals00
	   WHILE va
	      IF va.o = RX THEN ppcstw(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals64
	   WHILE va
	      IF va.o = FPX THEN ppcstfd(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals128
	   WHILE va
	      IF va.o = VX THEN reportIErr(' vector reg var used')
	      va := va.next
	   ENDWHILE
	ENDIF

	 /* get args into vars */
	FOR a := 0 TO proc.nrofargs-1
	   va := proc.argarray[a]
	   IF va.usage = 0 -> 1.6.1
	      -> do nothing
	   ELSEIF va.rtype = PPCARGRTYPE_RX
	      IF va.o = RX
	         ppcor(va.rnum,va.d,va.rnum,0)
	      ELSE
	         ppcstw(va.rnum, va.breg, va.offset)
	         va.intreg := TRUE
	         va.treg := va.rnum
	      ENDIF
	   ELSEIF va.rtype = PPCARGRTYPE_STACKRX
	      IF va.o = RX THEN ppclwz(va.d,va.breg,va.offset)
	   ELSEIF va.rtype = PPCARGRTYPE_FX
	      IF va.o = FPX
	         ppcfmr(va.d,va.rnum,0)
	      ELSE
	         ppcstfd(va.rnum, va.breg, va.offset)
	         va.intreg := TRUE
	         va.treg := va.rnum
	      ENDIF
	   ELSEIF va.rtype = PPCARGRTYPE_RX2 -> RX2 (was: D64)
	      ppcstw(va.rnum, va.breg, va.offset)
	      ppcstw(va.rnum+1, va.breg, va.offset+4)
	   ENDIF
	ENDFOR

	/* get self into var ? */
	IF proc.object
	   IF proc.selfvar.o = RX
	      ppcor(proc.selfreg, proc.selfvar.d, proc.selfreg,0)
	   ELSE
	      ppcstw(proc.selfreg, proc.selfvar.breg, proc.selfvar.offset) -> set selfvar ?
	   ENDIF
	   proc.selfvar.intreg := TRUE
	   proc.selfvar.treg := proc.selfreg
	ENDIF

	/* init locals32 */
	va := proc.locals32
	WHILE va
	   IF va.defo
	      IF va.o = RX
	         ppcliw(va.d,va.defd)
	      ELSE
	         ppcliw(11,va.defd)
	         ppcstw(11,va.breg,va.offset)
	      ENDIF
	   ENDIF
	   va := va.next
	ENDWHILE

	/* init locals64 */
	va := proc.locals64
	WHILE va
	   IF va.defo
	      IF va.type.flags AND MEMBF_FLOAT
	         IF va.o = FPX -> 2.3
	            ppcliw(11, va.defd)
	            ppcstw(11, STACKREG, -4)
	            ppclfs(va.d, STACKREG, -4)
	         ELSE
	            dh, dl := singToDoub(va.defd)
	            ppcliw(11, dh)
	            ppcstw(11, va.breg, va.offset)
	            ppcliw(11, dl)
	            ppcstw(11, va.breg, va.offset + 4)
	         ENDIF
	      ELSE
	         ppcliw(11, va.defd)
	         ppcstw(11, va.breg, va.offset + 4)
	         ppcsrawi(11, 11, 31, 0)
	         ppcstw(11, va.breg, va.offset)
	      ENDIF
	   ENDIF
	   va := va.next
	ENDWHILE

	/* init locals00 */
	va := proc.locals00
	WHILE va
	   IF va.o = RX
	      ppcaddi(va.d, va.breg, va.offset)
	   ELSE
	      va.o := ARXPO
	      va.d := AxSizeOfs(1,0,va.offset)
	   ENDIF
	   t := va.type.esize
	   SELECT t
	   CASE 1
	      IF va.cmplx
	         ppcaddis(0,0, va.type.numes) -> max,curr
	         ppcstw(0, va.breg, va.offset-4) -> store it
	         ppcaddi(0,0,0) -> nilterm
	         ppcstw(0,va.breg, va.offset) -> store it
	      ENDIF
	   CASE 4
	      IF va.cmplx
	         ppcaddis(0,0, va.type.numes) -> max,curr
	         ppcstw(0, va.breg, va.offset-4) -> store it
	      ENDIF
	   CASE 255
	      IF (va.type.object.nrofmethods>0) AND
	         (va.type.numes = 1)
	         ppclacode(11, va.type.object)
	         ppcstw(11, va.breg, va.offset-4)
	         ppcaddi(0,0,0)
	         t := va.type.object.sizeof + 3 AND $FFFC
	         WHILE t > 0
	            t := t - 4
	            ppcstw(0,va.breg,va.offset+t)
	         ENDWHILE
	      ENDIF
	   ENDSELECT
	   va := va.next
	ENDWHILE

ENDPROC

PROC exceptblock(do) OF ppc
	clearRegs()
	self.restorehandler()
	IF do = FALSE THEN ppcblab(g_endexceptlab,0,0)
	putLabel(g_exceptlab)
ENDPROC

-> 1.8.0. used by self.exceptblock() and dispatcher/IID_RET
PROC restorehandler() OF ppc
	ppclwz(0, FRAMEREG, self.p_esoffset)
	ppcstwlab(0, GLOBREG, self.pvar_exceptstruct)
ENDPROC

PROC endproc1() OF ppc
	clearRegs()
	IF g_endexceptlab THEN putLabel(g_endexceptlab)
ENDPROC

PROC endproc2() OF ppc
	DEF proc:PTR TO proc, va:PTR TO var, a, t
	clearRegs()
	proc := g_currentproc
	putLabel(g_procendlab)
	IF proc.handle
	  ppclwz(0,FRAMEREG,self.p_esoffset+ES_OLDEXCEPTION) -> fix 2.2.3
	   ppcstwlab(0,GLOBREG,self.ivar_exception) -> v45
	  t := self.p_esoffset+SIZEOF_ES+48           -> v49
	   FOR a := 31 TO 20 STEP -1
	      ppclfd(a,FRAMEREG,t)
	      t := t + 8
	   ENDFOR
	  ppclmw(20,1,self.p_esoffset+SIZEOF_ES)
	ENDIF
	ppclwz(0, FRAMEREG, proc.framesize+FRAMEPPC_LINKREG)
	ppcmtspr(0,8)
	IF proc.numregalloc OR proc.numfregalloc -> restore user regvars ?
	   va := proc.args32
	   WHILE va
	      IF va.o = RX THEN ppclwz(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.args64
	   WHILE va
	      IF va.o = FPX THEN ppclfd(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.args128
	   WHILE va
	      IF va.o = VX THEN NOP
	      va := va.next
	   ENDWHILE
	   va := proc.locals32
	   WHILE va
	      IF va.o = RX THEN ppclwz(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals00
	   WHILE va
	      IF va.o = RX THEN ppclwz(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals64
	   WHILE va
	      IF va.o = FPX THEN ppclfd(va.d, va.breg, va.saveoffset)
	      va := va.next
	   ENDWHILE
	   va := proc.locals128
	   WHILE va
	      IF va.o = VX THEN NOP
	      va := va.next
	   ENDWHILE
	ENDIF
	ppcaddi(STACKREG, STACKREG, proc.framesize)
	ppcbclr(20,0,0) -> branch lr
	g_procendlab := NIL
	g_exceptlab := NIL
	g_endexceptlab := NIL
ENDPROC

-> 1.5.3
PROC quotehead() OF ppc
	DEF endq, ax
	endq := newLabel()
	ax := self.obtainAREG()
	ppcblab(endq,0,1)
	clearRegs()
	ppcmfspr(ax,8)
ENDPROC endq, ax

PROC quotetail(endq, ax, eo, ed, qarx) OF ppc
	ppcmtspr(ax,8)
	inst_copy(eo,ed, DREG,IREG0)
	ppcbclr(20,0,0)
	def_label(endq)
	self.releaseAREG(ax,0)
	ppcmfspr(qarx,8)
ENDPROC


-> set new reg-content.
PROC ppcSetReg(reg, o,d:PTR TO var)
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
	   IF reg > 0 THEN d.intreg := TRUE
	   d.treg := reg
	ENDIF

ENDPROC o,d

-> note: var64.treg is 0-12 for floatregs 0-12.
PROC ppcSetFReg(reg,o,d:PTR TO var)
	DEF a:REG
	DEF t, var:PTR TO var

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

ENDPROC o,d



-> 1.5.3
PROC lockDREG(dreg) OF ppc
	DEF r=FALSE
#ifdef DBG_PPCGEN
DEBUGF('ppc.lockDREG(\d)\n', dreg)
#endif
	IF g_regusetab[dreg].obtains
	   ppcstw(dreg,1,g_localstackoffset++)
	   #ifdef DBG_PPCGEN
	   DEBUGF('..spilled it (use was=\d)\n', g_regusetab[dreg].obtains)
	   #endif
	   r := TRUE
	ENDIF
	g_regusetab[dreg].obtains := g_regusetab[dreg].obtains + 1
	g_regusetab[dreg].write := g_regusetab[dreg].write + 1
	self.lasttempREG := dreg
ENDPROC r

PROC lockAREG(areg) OF ppc
	DEF r=FALSE
#ifdef DBG_PPCGEN
DEBUGF('ppc.lockAREG(\d)\n', areg)
#endif
	IF g_regusetab[areg].obtains
	   ppcstw(areg,1,g_localstackoffset++)
	   #ifdef DBG_PPCGEN
	   DEBUGF('..spilled it (use was=\d)\n', g_regusetab[areg].obtains)
	   #endif
	   r := TRUE
	ENDIF
	g_regusetab[areg].obtains := g_regusetab[areg].obtains + 1
	g_regusetab[areg].write := g_regusetab[areg].write + 1
	self.lasttempREG := areg
ENDPROC r

PROC lockFREG(freg) OF ppc
	DEF r=FALSE
#ifdef DBG_PPCGEN
DEBUGF('ppc.lockFREG(\d)\n', freg)
#endif
	IF g_regusetab[freg+32].obtains
	   ppcstfd(freg,1,g_localstackoffset++) ; g_localstackoffset++
	   #ifdef DBG_PPCGEN
	   DEBUGF('..spilled it (use was=\d)\n', g_regusetab[freg+32].obtains)
	   #endif
	   r := TRUE
	ENDIF
	g_regusetab[freg+32].obtains := g_regusetab[freg+32].obtains + 1
	g_regusetab[freg+32].write := g_regusetab[freg+32].write + 1
	self.lasttempFREG := freg
ENDPROC r

-> v50
PROC resetObtainStart() OF ppcsysv
	self.lasttempREG := 10
	self.lasttempFREG := 12
ENDPROC

-> 1.5.4 only called by ppcsysv.getFreeDREG()/getFreeAREG()
PROC ppcsysvGetFreeReg(ppcsysv:PTR TO ppcsysv)
	DEF t, r
	DEF var:PTR TO var, array[32]:ARRAY OF CHAR

	#ifdef DBG_PPCGEN
	DEBUGF('ppcsysvGetFreeReg(): ')
	#endif


	-> v50
	FOR t := 3 TO 10 DO array[t] := NIL

	var := g_gvarlist
	WHILE var
	   IF var.intreg
	      IF var.type.size = 4
	         IF (var.treg < 32) THEN array[var.treg] := 1
	      ENDIF
	   ENDIF
	   var := var.next
	ENDWHILE
	var := IF g_currentproc THEN g_currentproc.args32 ELSE NIL
	WHILE var
	   IF var.intreg
	      IF (var.treg < 32) THEN array[var.treg] := 1
	   ENDIF
	   var := var.next
	ENDWHILE
	var := IF g_currentproc THEN g_currentproc.locals32 ELSE NIL
	WHILE var
	   IF var.intreg
	      IF (var.treg < 32) THEN array[var.treg] := 1
	   ENDIF
	   var := var.next
	ENDWHILE

	/* try free registers that is not mirroring vars */

	r := ppcsysv.lasttempREG
	FOR t := 1 TO 8
	   r--
	   IF r < 3 THEN r := 10
	   IF array[r] = NIL -> not caching ?
	      IF g_regusetab[r].obtains = NIL -> not in use ?
	         #ifdef DBG_PPCGEN
	         DEBUGF('\d\n', r)
	         #endif
	         RETURN r
	      ENDIF
	   ENDIF
	ENDFOR

	/* try any free register */

	r := ppcsysv.lasttempREG
	FOR t := 1 TO 8
	   r--
	   IF r < 3 THEN r := 10
	   IF g_regusetab[r].obtains = NIL -> not in use ?
	      #ifdef DBG_PPCGEN
	      DEBUGF('\d\n', r)
	      #endif
	      RETURN r
	   ENDIF
	ENDFOR

ENDPROC -1 -> indicate error

PROC getFreeDREG() OF ppcsysv
	DEF r
	r := ppcsysvGetFreeReg(self)
	IF r <> -1 THEN RETURN r
ENDPROC DTEMP

PROC getFreeAREG() OF ppcsysv
	DEF r
	r := ppcsysvGetFreeReg(self)
	IF r <> -1 THEN RETURN r
ENDPROC ATEMP

PROC getFreeFREG() OF ppcsysv
	DEF r, t
	DEF var:PTR TO var, array[32]:ARRAY OF CHAR

	#ifdef DBG_PPCGEN
	DEBUGF('ppcsysv.getFreeFREG(): ')
	#endif

	 -> v50
	FOR t := 1 TO 12 DO array[t] := NIL

	var := g_gvarlist
	WHILE var
	   IF var.intreg
	      IF var.type.size = 8
	         IF (var.treg < 32) THEN array[var.treg] := 1
	      ENDIF
	   ENDIF
	   var := var.next
	ENDWHILE

IF g_currentproc

	var := g_currentproc.args64
	WHILE var
	   IF var.intreg
	      IF (var.treg < 32) THEN array[var.treg] := 1
	   ENDIF
	   var := var.next
	ENDWHILE
	var := g_currentproc.locals64
	WHILE var
	   IF var.intreg
	      IF (var.treg < 32) THEN array[var.treg] := 1
	   ENDIF
	   var := var.next
	ENDWHILE

ENDIF

	/* try free register that does not mirror any vars */

	r := self.lasttempFREG
	IF r > 12 THEN r := 1
	FOR t := 1 TO 12
	   r--
	   IF r < 1 THEN r := 12
	   IF array[r] = NIL -> not caching ?
	      IF g_regusetab[r+32].obtains = NIL -> not in use ?
	         #ifdef DBG_PPCGEN
	         DEBUGF('\d\n', r)
	         #endif
	         RETURN r
	      ENDIF
	   ENDIF
	ENDFOR

	/* try any free register */

	r := self.lasttempFREG
	IF r > 12 THEN r := 1
	FOR t := 1 TO 12
	   r--
	   IF r < 1 THEN r := 12
	   IF g_regusetab[r+32].obtains = NIL -> not in use ?
	      #ifdef DBG_PPCGEN
	      DEBUGF('\d\n', r)
	      #endif
	      RETURN r
	   ENDIF
	ENDFOR

	/* settle for FTEMP */

ENDPROC FTEMP

PROC obtainDREG() OF ppcsysv
	DEF r, s, t
	r := self.getFreeDREG()
	IF r = DTEMP
	   -> 1.10.0 obtain already in writemode register fix
	   r := self.lasttempREG
	   FOR t := 3 TO 10
	      r--
	      IF r < 3 THEN r := 10
	      IF g_regusetab[r].write THEN t := 11
	   ENDFOR
	ENDIF
	s := self.lockDREG(r)
ENDPROC r, s

PROC obtainAREG() OF ppcsysv
	DEF r, s, t
	r := self.getFreeAREG()
	IF r = ATEMP
	    -> 1.10.0 obtain already in writemode register fix
	   r := self.lasttempREG
	   FOR t := 3 TO 10
	      r--
	      IF r < 3 THEN r := 10
	      IF g_regusetab[r].write THEN t := 11
	   ENDFOR
	ENDIF
	s := self.lockAREG(r)
ENDPROC r, s

PROC ppcsysvReleaseReg(r,retbool)
	#ifdef DBG_PPCGEN
	DEBUGF('ppcReleaseReg(\d,\d)\n', r, retbool)
	IF r > 31 THEN reportIErr(' ppcsysvReleaseReg r > 31')
	IF r < 3 THEN reportIErr(' ppcsysvReleaseReg r < 3')
	#endif
	IF g_regusetab[r].obtains > 1
	   IF g_regusetab[r].write
	      IF retbool THEN ppcor(r,ATEMP,r,0) BUT ppcSetReg(ATEMP,NIL,NIL)
	      ppclwz(r,1,g_localstackoffset--)
	      ppcSetReg(r,NIL,NIL)
	      g_regusetab[r].obtains := g_regusetab[r].obtains - 1
	      g_regusetab[r].write := g_regusetab[r].write - 1
	      IF retbool THEN RETURN ATEMP
	   ELSE
	      g_regusetab[r].obtains := g_regusetab[r].obtains - 1
	   ENDIF
	ELSEIF g_regusetab[r].obtains = 1
	   g_regusetab[r].obtains := NIL
	   g_regusetab[r].write := FALSE
	ELSEIF g_regusetab[r].obtains = 0
	   IF r < 20
	      reportIErr(' releaseREG() r is not obtained before\n')
	      WriteF('r=\d\n', r)
	   ENDIF
	   -> v48, must be allocated reg..
	   #ifdef DBG_PPCGEN
	   DEBUGF('ppcReleaseReg(\d ALLOCATED,\d)\n', r, retbool)
	   #endif
	ENDIF
ENDPROC r

PROC releaseDREG(r,b) OF ppcsysv IS ppcsysvReleaseReg(r,b)
PROC releaseAREG(r,b) OF ppcsysv IS ppcsysvReleaseReg(r,b)


-> 1.5.4
PROC obtainFREG() OF ppcsysv
	DEF r, s, t
	r := self.getFreeFREG()
	IF r = FTEMP
	    -> 1.10.0 obtain already in writemode register fix
	   r := self.lasttempFREG
	   FOR t := 1 TO 12
	      r--
	      IF r < 1 THEN r := 12
	      IF g_regusetab[r+32].write THEN t := 13
	   ENDFOR
	ENDIF
	s := self.lockFREG(r)
ENDPROC r, s

PROC releaseFREG(r,retbool) OF ppcsysv
	#ifdef DBG_PPCGEN
	DEBUGF('ppcReleaseFReg(\d,\d)\n', r, retbool)
	#endif
	IF g_regusetab[r+32].obtains > 1
	   IF g_regusetab[r+32].write
	      IF retbool THEN ppcfmr(0,r,0) BUT ppcSetFReg(0,NIL,NIL)
	      g_localstackoffset := g_localstackoffset - 8
	      ppclfd(r,1,g_localstackoffset)
	      ppcSetFReg(r, NIL,NIL)
	      g_regusetab[r+32].obtains := g_regusetab[r+32].obtains - 1
	      g_regusetab[r+32].write := g_regusetab[r+32].write - 1
	      IF retbool THEN RETURN 0
	   ELSE
	      g_regusetab[r+32].obtains := g_regusetab[r+32].obtains - 1
	   ENDIF
	ELSEIF g_regusetab[r+32].obtains = 1
	   g_regusetab[r+32].obtains := NIL
	   g_regusetab[r+32].write := FALSE
	ELSEIF g_regusetab[r+32].obtains = NIL -> allocated ?
	   IF r < 20 THEN reportIErr(' releaseFREG() r = \d\n', r)
	ENDIF
ENDPROC r

PROC saveObtained(regusecopy:PTR TO oreg) OF ppcsysv -> used when calling subroutines and so on
	DEF r
	#ifdef DBG_PPCGEN
	DEBUGF('ppcSaveObtainedRegs()\n')
	#endif
	CopyMem(g_regusetab, regusecopy, REGUSETABSIZE * SIZEOF oreg)
	FOR r := 3 TO 10
	   IF g_regusetab[r].obtains
	      #ifdef DBG_PPCGEN
	      DEBUGF('   r\d saved (use=\d) stackoffset = \d\n',
	      r, g_regusetab[r].obtains, g_localstackoffset)
	      #endif
	      inst_push(4,RX,r)
	      g_regusetab[r].obtains := 0
	      g_regusetab[r].write := FALSE
	   ENDIF
	ENDFOR
	FOR r := (1+32) TO (12+32)
	   IF g_regusetab[r].obtains
	      #ifdef DBG_PPCGEN
	      DEBUGF('   f\d saved (use=\d) stackoffset = \d\n',
	      r-32, g_regusetab[r].obtains, g_localstackoffset)
	      #endif
	      inst_push(8,FPX,r-32)
	      g_regusetab[r].obtains := 0
	      g_regusetab[r].write := FALSE
	   ENDIF
	ENDFOR
ENDPROC

PROC loadObtained(regusecopy:PTR TO oreg) OF ppcsysv
	DEF r
	#ifdef DBG_PPCGEN
	DEBUGF('ppcLoadObtainedRegs()\n')
	#endif
	FOR r := (12+32) TO (1+32) STEP -1
	   IF regusecopy[r].obtains
	      #ifdef DBG_PPCGEN
	      DEBUGF('   f\d loaded (use=\d) stackoffset=\d\n',
	      r-32, regusecopy[r].obtains, g_localstackoffset)
	      #endif
	      inst_pop(8,FPX,r-32)
	   ENDIF
	ENDFOR
	FOR r := 10 TO 3 STEP -1
	   IF regusecopy[r].obtains
	      #ifdef DBG_PPCGEN
	      DEBUGF('   r\d loaded (use=\d) stackoffset = \d\n',
	      r, regusecopy[r].obtains, g_localstackoffset)
	      #endif
	      inst_pop(4,RX,r)
	   ENDIF
	ENDFOR
	CopyMem(regusecopy, g_regusetab, REGUSETABSIZE * SIZEOF oreg)
ENDPROC

PROC copyObtainToDREG(o,d) OF ppcsysv IS ppcsysvcopyObtainToReg(self,o,d) -> v47
PROC copyObtainToAREG(o,d) OF ppcsysv IS ppcsysvcopyObtainToReg(self,o,d) -> v47
PROC ppcsysvcopyObtainToReg(self:PTR TO ppcsysv, o,d:PTR TO var)
	DEF r, t, v:PTR TO var, s
	#ifdef DBG_PPCGEN
	DEBUGF('ppcsysv.copyObtainToReg(\d,$\h): ', o, d)
	#endif
	IF o = VAR
	   IF d.trok AND (d.treg > 2) AND d.intreg AND (d.treg < 11)
	      r := d.treg
	      IF g_regusetab[r].write = FALSE
	         g_regusetab[r].obtains := g_regusetab[r].obtains + 1
	         #ifdef DBG_PPCGEN
	         DEBUGF('FOUND IN R\d (use=\d)\n', r, g_regusetab[r].obtains)
	         #endif
	         RETURN r, NIL
	      ENDIF
	   ENDIF
	   IF d.o = RX -> regvar 1.8.0 improvement for deref compab with 68k
	      RETURN d.d, NIL
	   ENDIF
	ELSEIF o = RX
	   IF d > 19 -> allocated
	      #ifdef DBG_PPCGEN
	      DEBUGF('FOUND IN R\d (regvar)\n', d)
	      #endif
	      RETURN d, NIL
	   ELSEIF (d < 11) AND (d > 2)
	      IF g_regusetab[d].write = FALSE
	         g_regusetab[d].obtains := g_regusetab[d].obtains + 1
	         #ifdef DBG_PPCGEN
	         DEBUGF('FOUND IN R\d (use=\d)\n', d, g_regusetab[d].obtains )
	         #endif
	         RETURN d, NIL
	      ENDIF
	   ENDIF
	ENDIF

	-> v50, try to obtain free register first
	r := self.getFreeDREG()
	IF r <> DTEMP
	   g_regusetab[r].obtains := 1
	   g_regusetab[r].write := NIL
	   inst_copy(o,d, RX, r)
	   #ifdef DBG_PPCGEN
	   DEBUGF('...copyObtain PUT IN FREE R\d\n', r)
	   #endif
	   RETURN r, FALSE
	ENDIF

	-> worst case scenario
	r, s := self.obtainDREG()
	self.toreg(o,d,r)
	#ifdef DBG_PPCGEN
	DEBUGF('...copyObtain PUT IN SPILLED R\d\n', r)
	#endif
ENDPROC r, s

PROC copyObtainToFREG(o,d:PTR TO var) OF ppcsysv
	DEF r, t, v:PTR TO var, s
	#ifdef DBG_PPCGEN
	DEBUGF('ppcsysv.copyObtainToFREG(\d,$\h):', o, d)
	#endif
	IF o = VAR64
	   IF d.trok AND d.intreg AND (d.treg < 13) AND (d.treg > 0)
	      r := d.treg
	      IF g_regusetab[r+32].write = FALSE
	         g_regusetab[r+32].obtains := g_regusetab[r+32].obtains + 1
	         #ifdef DBG_PPCGEN
	         DEBUGF('FOUND IN F\d\n (use=\d)', r, g_regusetab[r+32].obtains)
	         #endif
	         RETURN r, FALSE
	      ENDIF
	   ENDIF
	ELSEIF o = FPX
	   IF d > 19
	      #ifdef DBG_PPCGEN
	      DEBUGF('FOUND IN F\d (regvar)\n', d)
	      #endif
	      RETURN d, FALSE
	   ELSEIF (d < 13) AND (d > 0)
	      IF g_regusetab[d+32].write = FALSE
	         g_regusetab[d+32].obtains := g_regusetab[d+32].obtains + 1
	         #ifdef DBG_PPCGEN
	         DEBUGF('FOUND IN F\d (use=\d)\n', d, g_regusetab[d+32].obtains)
	         #endif
	         RETURN d, FALSE
	      ENDIF
	   ENDIF
	ENDIF

	-> v50, try to obtain free register first
	r := self.getFreeFREG()
	IF r <> FTEMP
	   g_regusetab[r+32].obtains := 1
	   g_regusetab[r+32].write := NIL
	   inst_copy(o,d, FPX,r)
	   #ifdef DBG_PPCGEN
	   DEBUGF('...copyObtain() PUT IN FREE F\d\n', r)
	   #endif
	   RETURN r, NIL
	ENDIF

	-> worst case

	r, s := self.obtainFREG()
	self.tofreg(o,d,r)
	#ifdef DBG_PPCGEN
	DEBUGF('...copyObtain() PUT IN SPILLED F\d\n', r)
	#endif
ENDPROC r, s

-> 2.3
PROC copyObtainToD64(o,d:PTR TO var) OF ppc
	DEF r, i, s
	#ifdef DBG_PPCGEN
	DEBUGF('copyObtainToD64(\d,$\h)\n', o, d)
	#endif

	IF o = D64
	   IF d <> D64TEMP
	      -> reuse the D64 if it is in shared mode
	      i := g_d64bottom - d / 8
	      IF g_regusetab[64+i].write = FALSE
	         g_regusetab[64+i].obtains := g_regusetab[64+i].obtains + 1
	         #ifdef DBG_PPCGEN
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
	   #ifdef DBG_PPCGEN
	   DEBUGF('copyObtainToD64: copied into \d, index \d\n', r, i)
	   #endif
	   RETURN r, NIL
	ENDIF

	#ifdef DBG_PPCGEN
	DEBUGF('copyObtainToD64: no free reg available!..\n')
	#endif

	r, s := self.obtainD64()

	self.tod64(o,d, r)

ENDPROC r, s

-> 2.3
PROC obtainD64() OF ppc
	DEF r, t, i, s
	#ifdef DBG_PPCGEN
	DEBUGF('obtainD64()\n')
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

	#ifdef DBG_PPCGEN
	DEBUGF('obtainD64(): locking \d\n', r)
	#endif

	s := self.lockD64(r)

ENDPROC r, s

-> 2.3
PROC releaseD64(r,retbool) OF ppc
	DEF i
	#ifdef DBG_PPCGEN
	DEBUGF('releaseD64(\d,\d) ', r, retbool)
	#endif
	i := g_d64bottom - r / 8
	IF g_regusetab[64+i].obtains = 0 THEN reportIErr('releaseD64: too many!')
	g_regusetab[64+i].obtains := g_regusetab[64+i].obtains - 1
	IF g_regusetab[64+i].write
	   g_regusetab[64+i].write := g_regusetab[64+i].write - 1
	ENDIF
	#ifdef DBG_PPCGEN
	DEBUGF(': register (with index \d) obtains became \d, writes \d\n',
	   i, g_regusetab[64+i].obtains, g_regusetab[64+i].write)
	#endif
	g_lastd64 := r + 8
ENDPROC r

-> 2.3
PROC lockD64(r) OF ppc
	DEF i
	#ifdef DBG_PPCGEN
	DEBUGF('lockD64(\d) ', r)
	#endif
	i := g_d64bottom - r / 8
	g_regusetab[64+i].obtains := g_regusetab[64+i].obtains + 1
	g_regusetab[64+i].write := g_regusetab[64+i].write + 1
	g_lastd64 := r
	#ifdef DBG_PPCGEN
	DEBUGF(': register (with index \d) obtains became \d, writes \d\n',
	   i, g_regusetab[64+i].obtains, g_regusetab[64+i].write)
	#endif
ENDPROC FALSE

PROC getFreeD64() OF ppc
	DEF r, a, i

	#ifdef DBG_PPCGEN
	DEBUGF('getFreeD64() ')
	#endif

	r := g_lastd64

	FOR a := 1 TO (D64STACKSIZE/8)
	   r := r - 8
	   IF r < (g_d64bottom - D64STACKSIZE) THEN r := g_d64bottom - 8
	   i := g_d64bottom - r / 8
	   IF g_regusetab[64+i].obtains = NIL
	      #ifdef DBG_PPCGEN
	      DEBUGF('\d\n', r)
	      #endif
	      RETURN r
	   ENDIF
	ENDFOR

	#ifdef DBG_PPCGEN
	DEBUGF('\d (D64TEMP)\n', D64TEMP)
	#endif

	-> ok lets settle for D64TEMP then

ENDPROC D64TEMP


-> v50
PROC secureReturn(ro, rd, ruc:PTR TO oreg) OF ppc
	#ifdef DBG_PPCGEN
	DEBUGF('secureReturn(\d, $\h, $\h) -> ', ro,rd, ruc)
	#endif
	SELECT ro
	CASE RX
	   IF ruc[rd].obtains
	      ppcor(rd, ATEMP, rd, 0)
	      ppcSetReg(ATEMP, NIL,NIL)
	      RETURN RX, ATEMP
	   ENDIF
	CASE FPX
	   IF ruc[rd+32].obtains
	      ppcfmr(FTEMP, rd, 0)
	      ppcSetFReg(FTEMP, NIL,NIL)
	      RETURN FPX, FTEMP
	   ENDIF
	CASE X2R -> 1.10.0 integer register pair (64bit WIDE return)
	   IF ruc[rd].obtains OR ruc[rd+1].obtains -> 2.3
	      ppcstw(rd, 1, D64TEMP)
	      ppcstw(rd+1, 1, D64TEMP+4)
	      RETURN D64, D64TEMP
	   ENDIF
	ENDSELECT
ENDPROC ro, rd

PROC toreg(o,d:PTR TO var, reg) OF ppc
	DEF t, ix, size, scale

	SELECT o
	CASE DV    ; ppcliw(reg,d)
	CASE RX    ; IF reg <> d THEN ppcor(d,reg,d,0)
	CASE VAR   ; IF d.intreg AND d.trok
	           ;    IF d.treg <> reg THEN ppcor(d.treg,reg,d.treg,0)
	           ; ELSEIF d.link=NIL
	           ;    ppclwz(reg, d.breg, d.offset)
	           ; ELSE
	           ;    ppclwzlab(reg,d.breg,d)
	           ; ENDIF
	CASE VAR64 ; IF d.type.flags AND MEMBF_FLOAT = FALSE
	           ;   -> we should probably have a warning here.. nope
	           ;   ->reportErr('Scaling down 64bit integer')
	           ;   IF d.link = FALSE
	           ;      ppclwz(reg, d.breg, d.offset+4)
	           ;   ELSE
	           ;      t := self.obtainFREG()
	           ;      ppclfdlab(t, d.breg, d)
	           ;      ppcstw(t, 1, -8)
	           ;      ppclwz(reg, 1, -4)
	           ;      self.releaseFREG(t,0)
	           ;   ENDIF
	           ; ELSE
	           ;   IF d.intreg AND d.trok
	           ;      ppcstfs(d.treg, 1, -4)
	           ;      ppclwz(reg,1,-4)
	           ;   ELSE
	           ;      t := self.obtainFREG()
	           ;      IF d.link = FALSE THEN ppclfd(t,d.breg,d.offset) ELSE ppclfdlab(t,d.breg,d)
	           ;      ppcSetFReg(t, NIL,NIL)
	           ;      ppcstfs(t, 1, -4)
	           ;      ppclwz(reg,1,-4)
	           ;      self.releaseFREG(t,0)
	           ;   ENDIF
	           ; ENDIF
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8 ; IF ARXPOflags(d) AND MEMBF_FLOAT = FALSE
	          ;    -> we should probably have a warning here..nope
	          ->;    reportErr('Scaling down 64bit integer')
	          ;    ppclwz(reg, ARXPOarx(d), ARXPOofs(d)+4)
	          ; ELSE
	          ;    ppclwz(reg,ARXPOarx(d),ARXPOofs(d)+4)
	          ; ENDIF
	   CASE 4 ; ppclwz(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 2 ; IF ARXPOflags(d) AND MEMBF_SIGNED
	          ;    ppclha(reg,ARXPOarx(d),ARXPOofs(d))
	          ; ELSE
	          ;    ppclhz(reg,ARXPOarx(d),ARXPOofs(d))
	          ; ENDIF
	   CASE 1 ; IF ARXPOflags(d) AND MEMBF_SIGNED
	          ;    ppclbz(reg,ARXPOarx(d),ARXPOofs(d)) ; ppcextsb(reg,reg,0)
	          ; ELSE
	          ;    ppclbz(reg,ARXPOarx(d),ARXPOofs(d))
	          ; ENDIF
	   CASE 0 ; ppcaddi(reg,ARXPOarx(d),ARXPOofs(d))
	   ENDSELECT
	CASE ARXPX
	   size := ARXPXsize(d)
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1 ; ix := ARXPXidrx(d)
	   CASE 2 ; ix := 0->self.obtainDREG()
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,s[3],1
	      /*self.releaseDREG(ix,0)*/ ; ppcSetReg(ix,NIL,NIL)
	   CASE 4 ; ix := 0->self.obtainDREG()
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,s[3],2
	      /*self.releaseDREG(ix,0)*/ ; ppcSetReg(ix,NIL,NIL)
	   CASE 8 ; ix := 0->self.obtainDREG()
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,s[3],3
	      /*self.releaseDREG(ix,0)*/ ; ppcSetReg(ix,NIL,NIL)
	   ENDSELECT
	   SELECT size
	   CASE 8 ; IF ARXPXflags(d) AND MEMBF_FLOAT = FALSE
	          ;    -> we should probably have a warning here..nope
	          ->;    reportErr('Scaling down 64bit integer')
	          ;    ppcaddi(ARXPXarx(d), ARXPXarx(d), 4)
	          ;    ppclwzx(reg,ARXPXarx(d),ix)
	          ;    ppcaddi(ARXPXarx(d), ARXPXarx(d), -4)
	          ; ELSE
	          ;    t := self.obtainFREG()
	          ;    ppclfdx(t,ARXPXarx(d),ix)
	          ;    ppcSetFReg(t, NIL,NIL)
	          ;    ppcstfs(t,1,-4)
	          ;    ppclwz(reg,1,-4)
	          ;    self.releaseFREG(t,0)
	          ; ENDIF
	   CASE 4 ; ppclwzx(reg,ARXPXarx(d),ix)
	   CASE 2 ; IF ARXPXflags(d) AND MEMBF_SIGNED
	          ;    ppclhax(reg,ARXPXarx(d),ix)
	          ; ELSE
	          ;    ppclhzx(reg,ARXPXarx(d),ix)
	          ; ENDIF
	   CASE 1 ; IF ARXPXflags(d) AND MEMBF_SIGNED
	          ;    ppclbzx(reg,ARXPXarx(d),ix) ; ppcextsb(reg,reg,0)
	          ; ELSE
	          ;    ppclbzx(reg,ARXPXarx(d),ix)
	          ; ENDIF
	   CASE 0 ; ppcadd(reg,ARXPXarx(d),ix,0,0)
	   ENDSELECT
	CASE FPX
	   ppcfmr(0,d,0)
	   ppcstfs(0,1,-4)
	   ppclwz(reg,1,-4)
	   ppcSetFReg(0,NIL,NIL)
	CASE D64
	   ppclwz(reg, 1, d+4)
	CASE X2R
	   ppcor(d+1, reg, d+1, 0)
	DEFAULT
	   Throw("PPC", 'toreg() o')
	ENDSELECT

	ppcSetReg(reg, o,d) -> update table

ENDPROC


PROC regto(reg, o,d:PTR TO var) OF ppc
	DEF t, size, ix, scale

	SELECT o                                               -> v45 fix
	CASE RX        ; IF d <> reg
	               ;    ppcor(reg,d,reg,0) ; ppcSetReg(d,RX,reg)
	               ; ENDIF
	CASE VAR       ; IF d.link
	               ;    ppcstwlab(reg, d.breg, d)
	               ; ELSE
	               ;    ppcstw(reg, d.breg, d.offset)
	               ; ENDIF
	               ; IF reg THEN d.intreg := TRUE ELSE d.intreg := FALSE -> r0 is not legal
	               ; d.treg := reg
	CASE VAR64     ; IF d.type.flags AND MEMBF_FLOAT
	                  t := self.obtainFREG()
	                  ppcstw(reg,STACKREG,-4)
	                  ppclfs(t,STACKREG,-4)
	                  ppcSetFReg(t, NIL,NIL)
	                  IF d.link = FALSE THEN ppcstfd(t,d.breg,d.offset) ELSE ppcstfdlab(t,d.breg,d)
	                  d.intreg := TRUE ; d.treg := t
	                  self.releaseFREG(t,0)
	                ELSE
	                  IF d.link = FALSE
	                     ppcstw(reg,d.breg,d.offset+4)
	                     ppcsrawi(reg, 0, 31, 0)
	                     ppcstw(0, d.breg, d.offset)
	                  ELSE
	                     -> stupid solution for now
	                     t := self.obtainFREG()
	                     ppcstw(reg, 1, -4)
	                     ppcsrawi(reg, 0, 31, 0)
	                     ppcstw(0, 1, -8)
	                     ppclfd(t, 1, -8)
	                     ppcstfdlab(t,d.breg,d)
	                     self.releaseFREG(t,0)
	                  ENDIF
	                ENDIF
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8 ; IF ARXPOflags(d) AND MEMBF_FLOAT = FALSE
	          ;    ppcstw(reg, ARXPOarx(d), ARXPOofs(d)+4)
	          ;    ppcsrawi(reg,0,31,0)
	          ;    ppcstw(0, ARXPOarx(d), ARXPOofs(d))
	          ; ELSE
	          ;    ppcstw(reg,1,-4)
	          ;    t := self.obtainFREG()
	          ;    ppclfs(t,1,-4)
	          ;    ppcSetFReg(t, NIL,NIL)
	          ;    ppcstfd(t, ARXPOarx(d),ARXPOofs(d))
	          ;    self.releaseFREG(t,0)
	          ; ENDIF
	   CASE 4 ; ppcstw(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 2 ; ppcsth(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 1 ; ppcstb(reg,ARXPOarx(d),ARXPOofs(d))
	   ENDSELECT
	CASE ARXPX
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1 ; ix := ARXPXidrx(d)
	   CASE 2 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,d[3],1
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   CASE 4 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,d[3],2
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   CASE 8 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,d[3],3
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   ENDSELECT
	   size := ARXPXsize(d)
	   SELECT size
	   CASE 8 ; IF ARXPXflags(d) AND MEMBF_FLOAT = FALSE
	          ;    t:= self.obtainDREG()
	          ;    ppcsrawi(reg,t,31,0)
	          ;    ppcstwx(t, ARXPXarx(d), ix)
	          ;    ppcaddi(ARXPXarx(d), ARXPXarx(d), 4)
	          ;    ppcstwx(reg, ARXPXarx(d), ix)
	          ;    ppcaddi(ARXPXarx(d), ARXPXarx(d), -4)
	          ;    self.releaseDREG(t, 0)
	          ; ELSE
	          ;    ppcstw(reg,1,-4)
	          ;    t := self.obtainFREG()
	          ;    ppclfs(t,1,-4)
	          ;    ppcSetFReg(t, NIL,NIL)
	          ;    ppcstfd(t, ARXPXarx(d),ix)
	          ;    self.releaseFREG(t,0)
	          ; ENDIF
	   CASE 4 ; ppcstwx(reg,ARXPXarx(d),ix)
	   CASE 2 ; ppcsthx(reg,ARXPXarx(d),ix)
	   CASE 1 ; ppcstbx(reg,ARXPXarx(d),ix)
	   ENDSELECT
	CASE FPX
	   ppcstw(reg,1,-4)
	   ppclfs(d,1,-4)
	   ppcSetFReg(d, RX,reg)
	CASE D64
	   ppcstw(reg, STACKREG, d+4)
	   ppcsrawi(reg, 0, 31, 0)
	   ppcstw(0, 1, d)
	   ppcSetReg(0,NIL,NIL)
	DEFAULT
	   Throw("PPC", 'regto() o')
	ENDSELECT

	-> set regcontent to destination
	->ppcSetReg(reg, o,d)
	-> see above instead
ENDPROC

PROC tofreg(o,d:PTR TO var, reg) OF ppc
	DEF t, ix, size, scale, h, l

	SELECT o
	CASE DV    ; t := self.obtainDREG()
	           ; ppcliw(t, d)
	           ; ppcSetReg(t, NIL,NIL)
	           ; ppcstw(t,STACKREG,-4)
	           ; ppclfs(reg,STACKREG,-4)
	           ; self.releaseDREG(t,0)
	CASE RX    ; ppcstw(d,STACKREG,-4)
	           ; ppclfs(reg,STACKREG,-4)
	CASE VAR   ; IF d.link
	           ;    ppclfslab(reg,d.breg,d)
	           ; ELSE
	           ;    ppclfs(reg,d.breg,d.offset)
	           ; ENDIF
	CASE VAR64 ; IF d.intreg AND d.trok
	           ;    IF d.treg <> reg THEN ppcfmr(reg,d.treg,0)
	           ; ELSE
	           ;    IF d.link THEN ppclfdlab(reg,d.breg,d) ELSE ppclfd(reg,d.breg,d.offset)
	           ; ENDIF
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 4 ; ppclfs(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 8 ; ppclfd(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 0 ; reportErr('cannot assign array to float')
	   DEFAULT
	      reportErr('floats are atleast 32 bit')->Throw("PPC", 'tofreg arxpo size')
	   ENDSELECT
	CASE ARXPX
	   size := ARXPXsize(d)
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1 ; ix := ARXPXidrx(d)
	   CASE 2 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,s[3],1
	      ppcSetReg(ix,NIL,NIL)
	   CASE 4 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,s[3],2
	      ppcSetReg(ix,NIL,NIL)
	   CASE 8 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,s[3],3
	      ppcSetReg(ix,NIL,NIL)
	   ENDSELECT
	   SELECT size
	   CASE 4 ; ppclfsx(reg,ARXPXarx(d),ix)
	   CASE 8 ; ppclfdx(reg,ARXPXarx(d),ix)
	   CASE 0 ; reportErr('cannot assign array to float')
	   DEFAULT
	      reportErr('floats are atleast 32 bit') -> Throw("PPC", 'tofreg arxpx size')
	   ENDSELECT
	CASE FPX
	   IF d <> reg THEN ppcfmr(reg,d,0)
	CASE D64
	   ppclfd(reg,1,d)
	DEFAULT
	   Throw("PPC", 'tofreg() o')
	ENDSELECT

	ppcSetFReg(reg, o,d) -> update table

ENDPROC


PROC fregto(reg, o,d:PTR TO var) OF ppc
	DEF t, size, ix, scale

	SELECT o
	CASE RX        ; ppcstfs(reg,1,-4)
	               ; ppclwz(d,1,-4)
	               ; ppcSetReg(d,NIL,NIL)
	CASE VAR       ; IF d.link
	               ;    ppcstfslab(reg, d.breg, d)
	               ;    d.intreg := FALSE
	               ; ELSE
	               ;    ppcstfs(reg,d.breg,d.offset)
	               ;    d.intreg := FALSE
	               ; ENDIF
	CASE VAR64     ; IF d.link THEN ppcstfdlab(reg,d.breg,d) ELSE ppcstfd(reg,d.breg,d.offset)
	               ; d.intreg := TRUE
	               ; d.treg := reg
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 4 ; ppcstfs(reg,ARXPOarx(d),ARXPOofs(d))
	   CASE 8 ; ppcstfd(reg,ARXPOarx(d),ARXPOofs(d))
	   DEFAULT
	      reportErr('storing float in <32bit')->Throw("PPC", 'fregto sarxpo size')
	   ENDSELECT
	CASE ARXPX
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1 ; ix := ARXPXidrx(d)
	   CASE 2 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,d[3],1
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   CASE 4 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,d[3],2
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   CASE 8 ; ix := 0->self.obtainDREG()
	          ; ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,d[3],3
	          ; ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	          ->; self.releaseDREG(ix,0)
	   ENDSELECT
	   size := ARXPXsize(d)
	   SELECT size
	   CASE 4 ; ppcstfsx(reg,ARXPXarx(d),ix)
	   CASE 8 ; ppcstfdx(reg,ARXPXarx(d),ix)
	   DEFAULT
	      reportErr('storing float in <32bit')->Throw("PPC", 'fregto arxpx size')
	   ENDSELECT
	CASE FPX
	   IF d <> reg THEN ppcfmr(d,reg,0) BUT ppcSetFReg(d,NIL,NIL) -> fix 1.10.0
	CASE D64
	   ppcstfd(reg,1,d)
	DEFAULT
	   Throw("PPC", 'regto() o')
	ENDSELECT

ENDPROC

-> 1.10.0
PROC tod64(o,d:PTR TO var, reg) OF ppc
	DEF t, ix, size, scale


	SELECT o
	CASE DV
	   t := self.obtainDREG()
	   ppcliw(t, d)
	   ppcstw(t, STACKREG, reg+4)
	   ppcliw(t, IF d AND $80000000 THEN -1 ELSE 0)
	   ppcstw(t, STACKREG, reg)
	   ppcSetReg(t, NIL,NIL)
	   self.releaseDREG(t, 0)
	CASE RX
	   ppcstw(d, STACKREG, reg+4)
	   ppcsrawi(d, 0, 31, 0)
	   ppcstw(0, STACKREG, reg)
	   ppcSetReg(0, NIL,NIL)
	CASE VAR
	   IF d.intreg AND d.trok
	      IF d.treg <> 0 THEN ppcor(d.treg,0,d.treg,0)
	   ELSEIF d.link=NIL
	      ppclwz(0, d.breg, d.offset)
	   ELSE
	      ppclwzlab(0,d.breg,d)
	   ENDIF
	   ppcstw(0, STACKREG, reg+4)
	   ppcsrawi(0, 0, 31, 0)
	   ppcstw(0, STACKREG, reg)
	   ppcSetReg(0, NIL,NIL)
	CASE VAR64
	   t := self.obtainFREG()
	   IF d.link THEN ppclfdlab(t,d.breg,d) ELSE ppclfd(t,d.breg,d.offset)
	   ppcstfd(t, STACKREG, reg)
	   self.releaseFREG(t, 0)
	   ppcSetFReg(t, NIL,NIL)
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8
	      t := self.obtainFREG()
	      ppclfd(t, ARXPOarx(d), ARXPOofs(d))
	      ppcstfd(t, 1, reg)
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t, 0)
	   CASE 4
	      ppclwz(0,ARXPOarx(d),ARXPOofs(d))
	      ppcstw(0, STACKREG, reg+4)
	      IF ARXPOflags(d) AND MEMBF_SIGNED THEN ppcsrawi(0, 0, 31, 0) ELSE ppcaddi(0,0,0)
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 2
	      IF ARXPOflags(d) AND MEMBF_SIGNED
	         ppclha(0,ARXPOarx(d),ARXPOofs(d))
	         ppcstw(0, STACKREG, reg+4)
	         ppcsrawi(0, 0, 31, 0)
	      ELSE
	         ppclhz(0,ARXPOarx(d),ARXPOofs(d))
	         ppcstw(0, STACKREG, reg+4)
	         ppcaddi(0, 0, 0)
	      ENDIF
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 1
	      IF ARXPOflags(d) AND MEMBF_SIGNED
	         ppclbz(0,ARXPOarx(d),ARXPOofs(d))
	         ppcextsb(0,0,0)
	         ppcstw(0, STACKREG, reg+4)
	         ppcsrawi(0, 0, 31, 0)
	      ELSE
	         ppclbz(0,ARXPOarx(d),ARXPOofs(d))
	         ppcstw(0, STACKREG, reg+4)
	         ppcaddi(0, 0, 0)
	      ENDIF
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 0
	      ppcaddi(0,ARXPOarx(d),ARXPOofs(d))
	      ppcstw(0, STACKREG, reg+4)
	      ppcsrawi(0, 0, 31, 0)
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   ENDSELECT
	CASE ARXPX
	   size := ARXPXsize(d)
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1
	      ix := ARXPXidrx(d)
	   CASE 2
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,s[3],1
	      ppcSetReg(ix,NIL,NIL)
	   CASE 4
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,s[3],2
	      ppcSetReg(ix,NIL,NIL)
	   CASE 8
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,s[3],3
	      ppcSetReg(ix,NIL,NIL)
	   ENDSELECT
	   SELECT size
	   CASE 8
	      t := self.obtainFREG()
	      ppclfdx(t,ARXPXarx(d),ix)
	      ppcstfd(t, 1, reg)
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t,0)
	   CASE 4
	      ppclwzx(0,ARXPXarx(d),ix)
	      ppcstw(0, STACKREG, reg+4)
	      IF d.type.flags AND MEMBF_SIGNED THEN ppcsrawi(0, 0, 31, 0) ELSE ppcaddi(0,0,0)
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 2
	      IF ARXPXflags(d) AND MEMBF_SIGNED
	         ppclhax(0,ARXPXarx(d),ix)
	         ppcstw(0, STACKREG, reg+4)
	         ppcsrawi(0, 0, 31, 0)
	      ELSE
	         ppclhzx(0,ARXPXarx(d),ix)
	         ppcstw(0, STACKREG, reg+4)
	         ppcaddi(0, 0, 0)
	      ENDIF
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 1
	      IF ARXPXflags(d) AND MEMBF_SIGNED
	         ppclbzx(0,ARXPXarx(d),ix)
	         ppcextsb(0,0,0)
	         ppcstw(0, STACKREG, reg+4)
	         ppcsrawi(0, 0, 31, 0)
	      ELSE
	         ppclbzx(0,ARXPXarx(d),ix)
	         ppcstw(0, STACKREG, reg+4)
	         ppcaddi(0, 0, 0)
	      ENDIF
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   CASE 0
	      ppcadd(0,ARXPXarx(d),ix,0,0)
	      ppcstw(0, STACKREG, reg+4)
	      ppcsrawi(0, 0, 31, 0)
	      ppcstw(0, STACKREG, reg)
	      ppcSetReg(0, NIL,NIL)
	   ENDSELECT
	CASE FPX
	   ppcstfd(d, 1, reg)
	CASE D64
	   IF d <> reg
	      t := self.obtainFREG()
	      ppclfd(t, 1, d)
	      ppcstfd(t, 1, reg)
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t, 0)
	   ENDIF
	CASE X2R
	   ppcstw(d, 1, reg)
	   ppcstw(d+1, 1, reg+4)
	DEFAULT
	   Throw("PPC", 'tod64() o')
	ENDSELECT

ENDPROC

-> 1.10.0
PROC d64to(reg, o,d:PTR TO var) OF ppc
	DEF t, size, ix, scale

	#ifdef DBG_PPCGEN
	DEBUGF('d64to(\d, \d, $\h)\n', reg, o, d)
	#endif

	SELECT o
	CASE RX
	   ppclwz(d, STACKREG, reg+4)
	   ppcSetReg(d,NIL,NIL)
	CASE VAR
	   ppclwz(0, STACKREG, reg+4)
	   IF d.link
	      ppcstwlab(0, d.breg, d)
	   ELSE
	      ppcstw(0, d.breg, d.offset)
	   ENDIF
	   d.intreg := FALSE
	   ppcSetReg(0, NIL,NIL)
	CASE VAR64
	   t := self.obtainFREG()
	   ppclfd(t, 1, reg)
	   IF d.link
	      ppcstfdlab(t, d.breg, d)
	   ELSE
	      ppcstfd(t, d.breg, d.offset)
	   ENDIF
	   ppcSetFReg(t, NIL,NIL)
	   self.releaseFREG(t, 0)
	   d.intreg := FALSE
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8
	      t := self.obtainFREG()
	      ppclfd(t, 1, reg)
	      ppcstfd(t, ARXPOarx(d), ARXPOofs(d))
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t,0)
	   CASE 4
	      ppclwz(0, STACKREG, reg+4)
	      ppcstw(0, ARXPOarx(d), ARXPOofs(d))
	      ppcSetReg(0, NIL,NIL)
	   CASE 2
	      ppclwz(0, STACKREG, reg+4)
	      ppcsth(0,ARXPOarx(d),ARXPOofs(d))
	      ppcSetReg(0, NIL,NIL)
	   CASE 1
	      ppclwz(0, STACKREG, reg+4)
	      ppcstb(0,ARXPOarx(d),ARXPOofs(d))
	      ppcSetReg(0, NIL,NIL)
	   ENDSELECT
	CASE ARXPX
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1
	      ix := ARXPXidrx(d)
	   CASE 2
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,d[3],1
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   CASE 4
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,d[3],2
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   CASE 8
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,d[3],3
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   ENDSELECT
	   size := ARXPXsize(d)
	   SELECT size
	   CASE 8
	      t := self.obtainFREG()
	      ppclfd(t, 1, reg)
	      ppcstfdx(t, ARXPXarx(d), ix)
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t,0)
	   CASE 4
	      t := self.obtainDREG()
	      ppclwz(t, STACKREG, reg+4)
	      ppcstwx(t,ARXPXarx(d),ix)
	      ppcSetReg(t, NIL,NIL)
	      self.releaseDREG(t,0)
	   CASE 2
	      t := self.obtainDREG()
	      ppclwz(t, STACKREG, reg+4)
	      ppcsthx(t,ARXPXarx(d),ix)
	      ppcSetReg(t, NIL,NIL)
	      self.releaseDREG(t,0)
	   CASE 1
	      t := self.obtainDREG()
	      ppclwz(t, STACKREG, reg+4)
	      ppcstbx(t,ARXPXarx(d),ix)
	      ppcSetReg(t, NIL,NIL)
	      self.releaseDREG(t,0)
	   ENDSELECT
	CASE FPX
	   ppclfd(d, 1, reg)
	   ppcSetFReg(d,NIL,NIL)
	CASE D64
	   IF d <> reg
	      t := self.obtainFREG()
	      ppclfd(t, 1, reg)
	      ppcstfd(t, 1, d)
	      ppcSetFReg(t, NIL,NIL)
	      self.releaseFREG(t, 0)
	   ENDIF
	DEFAULT
	   Throw("PPC", 'd64to() o')
	ENDSELECT

ENDPROC


->v44. o2:RX/DV only !
-> v55: o1: any, o2: RX/DV/D64
PROC compareReg(o1,d1:PTR TO var, o2, d2, cond) OF ppc
	DEF treg1=-1, treg2=-1, reverse=FALSE, t

	IF o1 = DV
	   IF (Abs(d1) > $7FFF) OR (d1 = $80000000) -> 1.10.0 fix
	      treg1 := self.obtainDREG()
	      self.toreg(o1,d1, treg1)
	      o1 := RX
	      d1 := treg1
	   ENDIF
	ENDIF

	IF o2 = DV
	   IF (Abs(d2) > $7FFF) OR (d2 = $80000000) -> 1.10.0 fix
	      treg2 := self.obtainDREG()
	      self.toreg(o2,d2, treg2)
	      o2 := RX
	      d2 := treg2
	   ENDIF
	ENDIF

	-> now, only 16 bit DV is used

	IF o2 = RX
	   SELECT o1
	   CASE RX   ; ppccmp(0,0,d2,d1)
	   CASE DV   ; ppccmpi(0,0,d2,d1)
	   CASE VAR
	      IF d1.intreg AND d1.trok
	         ppccmp(0,0,d2,d1.treg)
	      ELSE
	         self.toreg(o1,d1, 0)
	         ppccmp(0,0,d2,0)
	      ENDIF
	   DEFAULT
	      treg1 := self.obtainDREG()
	      self.toreg(o1,d1, treg1)
	      ppccmp(0,0,d2,treg1)
	   ENDSELECT
	ELSEIF o2 = DV
	   SELECT o1
	   CASE RX   ; ppccmpi(0,0,d1,d2)
	   CASE DV   ; treg1 := self.obtainDREG()
	             ; self.toreg(o1,d1,treg1)
	             ; ppccmpi(0,0,treg1,d2)
	   CASE VAR
	      IF d1.intreg AND d1.trok
	         ppccmpi(0,0,d1.treg,d2)
	      ELSE
	         self.toreg(o1,d1, 0)
	         ppccmpi(0,0,0,d2)
	      ENDIF
	   CASE D64 -> 1.10.0
	      IF d2 = NIL  -> 2.2.2
	         ppcTest64(d1)
	      ELSE
	         ppcaddi(0, 0, d2)
	         ppcstw(0, STACKREG, -4)
	         ppcaddi(0, 0, IF d2 AND $80000000 THEN -1 ELSE 0)
	         ppcstw(0, STACKREG, -8)
	         ppcCompare64(d1, -8)
	         ppcSetReg(0, NIL,NIL)
	      ENDIF
	   CASE X2R -> 2.3
	      IF d2 = NIL
	         ppcstw(d1, STACKREG, -8)
	         ppcstw(d1+1, STACKREG, -4)
	         ppcTest64(-8)
	      ELSE
	         ppcaddi(0, 0, d2)
	         ppcstw(0, STACKREG, -4)
	         ppcaddi(0, 0, IF d2 AND $80000000 THEN -1 ELSE 0)
	         ppcstw(0, STACKREG, -8)
	         ppcstw(d1, STACKREG, -16)
	         ppcstw(d1+1, STACKREG, -12)
	         ppcCompare64(-16, -8)
	         ppcSetReg(0, NIL,NIL)
	      ENDIF
	   CASE VAR64 -> 1.10.0
	      IF d2 = NIL  -> 2.2.2
	         IF d1.link = FALSE
	            ppcTest64(d1.offset)
	         ELSE
	            ppclfdlab(0, d1.breg, d1)
	            ppcstfd(0, 1, -8)
	            ppcTest64(-8)
	            ppcSetFReg(0, NIL,NIL)
	         ENDIF
	      ELSE
	         ppcaddi(0, 0, d2)
	         ppcstw(0, STACKREG, -4)
	         ppcaddi(0, 0, IF d2 AND $80000000 THEN -1 ELSE 0)
	         ppcstw(0, STACKREG, -8)
	         IF d1.link = FALSE
	            ppcCompare64(d1.offset, -8)
	            ppcSetReg(0, NIL,NIL)
	         ELSE
	            ppclfdlab(0, d1.breg, d1)
	            ppcstfd(0, 1, -16)
	            ppcCompare64(-16, -8)
	            ppcSetReg(0, NIL,NIL)
	            ppcSetFReg(0, NIL,NIL)
	         ENDIF
	      ENDIF
	   DEFAULT
	      treg1 := self.obtainDREG()
	      self.toreg(o1,d1, treg1)
	      ppccmpi(0,0,treg1,d2)
	   ENDSELECT
	   reverse := TRUE
	ELSEIF o2 = D64 -> 1.10.0, used by SELECT 64bit
	   t := self.obtainD64()
	   self.tod64(o1,d1, t)
	   ppcCompare64(t, d2)
	   self.releaseD64(t, 0)
	ELSE
	   Throw("PPC", 'priv_cmp o2')
	ENDIF

	IF treg1 > -1 THEN self.releaseDREG(treg1,0)
	IF treg2 > -1 THEN self.releaseDREG(treg2,0)

ENDPROC reverse

-> 1.10.0
PROC ppcCompare64(d64a, d64b)
	ppclwz(11, 1, d64b)
	ppclwz(0, 1, d64a)
	ppccmp(0, 0, 11,0)
	ppcbc(PPCNE,4,0,0)
	ppclwz(11, 1, d64b+4)
	ppclwz(0, 1, d64a+4)
	ppccmpl(0, 0, 11,0)   -> 2.2.1 fix
	ppcSetReg(11,NIL,NIL)
	ppcSetReg(0,NIL,NIL)
ENDPROC

-> used by ppc.compareReg()
PROC ppcTest64(d64) -> 2.2.2
	ppclwz(11, 1, d64)
	ppccmpi(0, 0, 11, 0) -> compare upper against zero
	ppcbc(PPCNE,3,0,0) -> we are done if not zero
	ppclwz(11, 1, d64+4)
	ppccmpli(0, 0, 11, 0) -> compare lower against zero
	ppcSetReg(11,NIL,NIL)
ENDPROC

-> 2.2
PROC ppc_abs(d, s)
	ppcsrawi(s, 0, 31, 0)   -> srawi r0,s,31
	ppcadd(12, 0, s, 0, 0)  -> add r12,r0,s
	ppcxor(12,d,0,0)        -> xor d,r12,r0
	ppcSetReg(12,NIL,NIL)
	ppcSetReg(0,NIL,NIL)

ENDPROC

->v57
PROC tox2r(o,d:PTR TO var, reg) OF ppc
	DEF t, ix, size, scale

	SELECT o
	CASE DV
	   ppcliw(reg+1,d)
	   ppcliw(reg, IF d AND $80000000 THEN -1 ELSE 0)
	CASE RX
	   IF (reg+1) <> d THEN ppcor(d,reg+1,d,0)
	   ppcsrawi(reg+1, reg, 31, 0)
	CASE VAR
	   IF d.intreg AND d.trok
	      IF d.treg <> (reg+1) THEN ppcor(d.treg,reg+1,d.treg,0)
	   ELSEIF d.link=NIL
	      ppclwz(reg+1, d.breg, d.offset)
	   ELSE
	      ppclwzlab(reg+1,d.breg,d)
	   ENDIF
	   ppcsrawi(reg+1, reg, 31, 0)
	CASE VAR64
	   IF d.link = NIL
	      ppclwz(reg, d.breg, d.offset)
	      ppclwz(reg+1, d.breg, d.offset+4)
	   ELSE
	      t := self.obtainFREG()
	      IF d.link THEN ppclfdlab(t,d.breg,d) ELSE ppclfd(t,d.breg,d.offset)
	      ppcstfd(t, FRAMEREG, -8)
	      ppclwz(reg, FRAMEREG, -8)
	      ppclwz(reg+1, FRAMEREG, -4)
	      self.releaseFREG(t,0)
	      ppcSetFReg(t, NIL,NIL)
	   ENDIF
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8
	      ppclwz(reg, ARXPOarx(d), ARXPOofs(d))
	      ppclwz(reg+1, ARXPOarx(d), ARXPOofs(d)+4)
	   CASE 4
	      ppclwz(reg+1,ARXPOarx(d),ARXPOofs(d))
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 2
	      IF ARXPOflags(d) AND MEMBF_SIGNED
	         ppclha(reg+1,ARXPOarx(d),ARXPOofs(d))
	      ELSE
	         ppclhz(reg+1,ARXPOarx(d),ARXPOofs(d))
	      ENDIF
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 1
	      IF ARXPOflags(d) AND MEMBF_SIGNED
	         ppclbz(reg+1,ARXPOarx(d),ARXPOofs(d)) ; ppcextsb(reg+1,reg+1,0)
	      ELSE
	         ppclbz(reg+1,ARXPOarx(d),ARXPOofs(d))
	      ENDIF
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 0
	      ppcaddi(reg+1,ARXPOarx(d),ARXPOofs(d))
	      ppcsrawi(reg+1, reg, 31, 0)
	   ENDSELECT
	CASE ARXPX
	   size := ARXPXsize(d)
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1 ; ix := ARXPXidrx(d)
	   CASE 2 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,s[3],1
	      ppcSetReg(ix,NIL,NIL)
	   CASE 4 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,s[3],2
	      ppcSetReg(ix,NIL,NIL)
	   CASE 8 ; ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,s[3],3
	      ppcSetReg(ix,NIL,NIL)
	   ENDSELECT
	   SELECT size
	   CASE 8
	      ppclwz(reg,ARXPXarx(d),ix)
	      ppcaddi(ARXPXarx(d), ARXPXarx(d), 4)
	      ppclwzx(reg+1,ARXPXarx(d),ix)
	      ppcaddi(ARXPXarx(d), ARXPXarx(d), -4)
	   CASE 4
	      ppclwzx(reg+1,ARXPXarx(d),ix)
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 2
	      IF ARXPXflags(d) AND MEMBF_SIGNED
	         ppclhax(reg+1,ARXPXarx(d),ix)
	      ELSE
	         ppclhzx(reg+1,ARXPXarx(d),ix)
	      ENDIF
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 1
	      IF ARXPXflags(d) AND MEMBF_SIGNED
	         ppclbzx(reg+1,ARXPXarx(d),ix) ; ppcextsb(reg+1,reg+1,0)
	      ELSE
	         ppclbzx(reg+1,ARXPXarx(d),ix)
	      ENDIF
	      ppcsrawi(reg+1, reg, 31, 0)
	   CASE 0
	      ppcadd(reg+1,ARXPXarx(d),ix,0,0)
	      ppcsrawi(reg+1, reg, 31, 0)
	   ENDSELECT
	CASE FPX
	   ppcfmr(0,d,0)
	   ppcstfd(0,1,-8)
	   ppclwz(reg,1,-8)
	   ppclwz(reg+1,1,-4)
	   ppcSetFReg(0,NIL,NIL)
	CASE D64
	   ppclwz(reg, 1, d)
	   ppclwz(reg+1, 1, d+4)
	CASE X2R
	   IF reg <> d
	      ppcstw(d,FRAMEREG,-8)
	      ppcstw(d+1,FRAMEREG,-4)
	      ppclwz(reg,FRAMEREG,-8)
	      ppclwz(reg+1,FRAMEREG,-4)
	   ENDIF
	DEFAULT
	   Throw("PPC", 'tox2r() o')
	ENDSELECT

	ppcSetReg(reg, NIL,NIL) -> update table
	ppcSetReg(reg+1, NIL,NIL) -> update table

ENDPROC

-> v57
PROC x2rto(reg, o,d:PTR TO var) OF ppc
	DEF t, size, ix, scale

	#ifdef DBG_PPCGEN
	DEBUGF('x2rto(\d, \d, $\h)\n', reg, o, d)
	#endif

	SELECT o
	CASE RX
	   ppcor(reg+1,d,reg+1,0)
	   ppcSetReg(d,NIL,NIL)
	CASE VAR
	   IF d.link
	      ppcstwlab(reg+1, d.breg, d)
	   ELSE
	      ppcstw(reg+1, d.breg, d.offset)
	   ENDIF
	   d.intreg := FALSE
	CASE VAR64
	   IF d.link
	      ppcstw(reg, 1, -8)
	      ppcstw(reg+1, 1, -4)
	      ppclfd(0, 1, -8)
	      ppcstfdlab(0, d.breg, d)
	      ppcSetFReg(0,NIL,NIL)
	   ELSE
	      ppcstw(reg, d.breg, d.offset)
	      ppcstw(reg+1, d.breg, d.offset+4)
	   ENDIF
	   d.intreg := FALSE
	CASE ARXPO
	   size := ARXPOsize(d)
	   SELECT size
	   CASE 8
	      ppcstw(reg, ARXPOarx(d), ARXPOofs(d))
	      ppcstw(reg+1, ARXPOarx(d), ARXPOofs(d)+4)
	   CASE 4
	      ppcstw(reg+1, ARXPOarx(d), ARXPOofs(d))
	   CASE 2
	      ppcsth(reg+1,ARXPOarx(d),ARXPOofs(d))
	   CASE 1
	      ppcstb(reg+1,ARXPOarx(d),ARXPOofs(d))
	   ENDSELECT
	CASE ARXPX
	   scale := ARXPXscale(d)
	   SELECT scale
	   CASE 1
	      ix := ARXPXidrx(d)
	   CASE 2
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,1,0,31-1,0) -> slwi ix,d[3],1
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   CASE 4
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,2,0,31-2,0) -> slwi ix,d[3],2
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   CASE 8
	      ix := 0
	      ppcrlwinm(ARXPXidrx(d),ix,3,0,31-3,0) -> slwi ix,d[3],3
	      ppcSetReg(ix, NIL,NIL) -> invalidate temp reg
	   ENDSELECT
	   size := ARXPXsize(d)
	   SELECT size
	   CASE 8
	      ppcstwx(reg, ARXPXarx(d),ix)
	      ppcaddi(ARXPXarx(d),ARXPXarx(d),4)
	      ppcstwx(reg+1, ARXPXarx(d),ix)
	      ppcaddi(ARXPXarx(d),ARXPXarx(d),-4)
	   CASE 4
	      ppcstwx(reg+1, ARXPXarx(d),ix)
	   CASE 2
	      ppcsthx(reg+1,ARXPXarx(d),ix)
	   CASE 1
	      ppcstbx(reg+1,ARXPXarx(d),ix)
	   ENDSELECT
	CASE FPX
	   ppcstw(reg, 1, -8)
	   ppcstw(reg+1, 1, -4)
	   ppclfd(d, 1, -8)
	   ppcSetFReg(d,NIL,NIL)
	CASE D64
	   ppcstw(reg, FRAMEREG, d)
	   ppcstw(reg+1, FRAMEREG, d+4)
	CASE X2R
	   IF reg <> d
	      ppcstw(reg,FRAMEREG,-8)
	      ppcstw(reg+1,FRAMEREG,-4)
	      ppclwz(d,FRAMEREG,-8)
	      ppclwz(d+1,FRAMEREG,-4)
	      ppcSetReg(d,RX,reg)
	      ppcSetReg(d+1,RX,reg+1)
	   ENDIF
	DEFAULT
	   Throw("PPC", 'x2rto() o')
	ENDSELECT

ENDPROC

-> v58
PROC doMethod(n:PTR TO item, as, as2, postkey, type:PTR TO member) OF ppc
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
	      inst_copy(as,as2, AREG,meth.selfreg)
	      IF meth.offset
	         inst_goslab(meth)
	      ELSE -> module -> module
	         inst_labadr(type.object, ATEMP)
	         inst_copy(ARXPO,AxSizeOfs(ATEMP,4,mid*4), AREG, ATEMP)
	         inst_gosarx(ATEMP)
	      ENDIF
	      ro, rd := self.secureReturn(ro, rd, regusecopy)
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

	   -> v50 ppc bugfix, save as,as2 on stack. doProcfunc might trash basereg of as
	   -> v51 68k bugfix, saving onstack is not needed and does not work for 68k
	   IF as <> VAR
	      inst_push(4,as,as2)
	   ENDIF

	   n := self.doProcfunc(t, meth)

	   IF as <> VAR
	      inst_pop(4, AREG,meth.selfreg)
	   ELSE
	      inst_copy(as,as2, AREG,meth.selfreg)
	   ENDIF

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

	   ro, rd := self.secureReturn(ro, rd, regusecopy)

	   self.loadObtained(regusecopy)
	   clearRegs()

	#ifdef DBG_CODEGEN
	DEBUGF('dvs_method done\n')
	#endif



ENDPROC n, ro, rd



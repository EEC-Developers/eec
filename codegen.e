
-> ECX/codegen.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */


OPT MODULE
OPT PREPROCESS
OPT LARGE
OPT EXPORT

-> Sept 2007: ecx/codegen.e created. Moved all the codegen stuff into this module.

->#define DBG_CODEGEN

MODULE 'exec/lists'

MODULE '*compiler'
MODULE '*common'
MODULE '*binary'
MODULE '*runtime'
MODULE '*support'
MODULE '*opcodes68'
MODULE '*opcodesppc'
MODULE '*assembler' -> v55

-> v58
CONST D64STACKSIZE = 8*8

#define Put32(v) g_codeptr[]++ := v

OBJECT derefargs
   pk -> copy of it_varexp.postkey
   eo
   ed
   nd -> "nodest"
   am -> copy of it_varexp.assignmod
ENDOBJECT

-> v49
OBJECT lastx
   iid -> new IID_XXX
   start
   end
   o
   d
   cond
   rx
ENDOBJECT

-> v49, g_regusetable point to array of this now, isntead of CHAR
OBJECT oreg -> obtainable register
   obtains:CHAR -> count of obtains (totals read and write)
   write:CHAR -> number of write-obtains, or 0.  was: 1 if obtained for write, else 0
ENDOBJECT


DEF g_codeptr:PTR TO LONG, g_codebuf, g_databuf
DEF g_gvarlist:PTR TO gvar
DEF g_currentproc:PTR TO proc
DEF link_codesize
DEF g_sizeofptr
DEF g_linelist:PTR TO linedef, g_codelablist:PTR TO codelab, g_multireturn:PTR TO multireturn
DEF g_optpowerpc, g_optmodule
DEF g_globalsize, g_rwreflist:PTR TO rwref, g_databufsize
DEF g_nilcheck, g_linedebug, g_stepdebug,g_stepdebug50
DEF link_reloc32list, link_nrofreloc32s
DEF g_linenum, g_numregalloc, g_numfregalloc
DEF g_stacksize, g_modulelist:PTR TO mlh
DEF g_objectlist:PTR TO object, g_symbolhunk
DEF g_naturalalign -> 1.8.2

DEF g_regusetab:PTR TO oreg
DEF g_lastx:PTR TO lastx

DEF g_dreg -> RX/DRX
DEF g_areg -> RX/ARX
DEF g_freg -> FPX
DEF g_vreg -> VX
DEF g_d64reg -> D64 v55
DEF g_ireg0  -> R3/D0   (non obtainable)
DEF g_ireg1  -> R4/D1
DEF g_freg0  -> FP1, F1
DEF g_stackreg -> R1/A7  (non obtainable, dedicated)
DEF g_selfreg  -> R12/A0
DEF g_globreg  -> R13/A4 (non obtainable, dedicated)
DEF g_framereg -> R1/A5  (non obtainable, dedicated)
DEF g_atemp   -> R11/A6 (non obtainable)
DEF g_dtemp   -> R12/D3 (non obtain)
DEF g_ftemp   -> FP0/F0 (non obtain)
DEF g_d64temp -> v55

DEF g_safeimmlists -> v57, imported from main and used by doList()

CONST REGUSETABSIZE=96

#define PTRSIZE g_sizeofptr
#define REALSIZE 8

ENUM ISEQ, ISNE, ISGT, ISLT, ISGE, ISLE

ENUM IID_DUMMY,
     IID_COPY, -> o1,d1, o2,d2
     IID_RET, ->
     IID_BIC,

     IID_NEGREG, -> v42
     IID_MULREG,
     IID_DIVREG,
     IID_SHRREG,
     IID_SHLREG,
     IID_ADDREG,
     IID_SUBREG,
     IID_ORREG,
     IID_ANDREG,
     IID_SICREG, -> v41
     IID_XORREG, -> v55
     IID_NOTREG, -> v55
     IID_ASRREG, -> v55
     IID_ABSREG, -> 2.2

      -> v55
     IID_NEGD64,
     IID_MULD64,
     IID_DIVD64,
     IID_SHRD64,
     IID_SHLD64,
     IID_ADDD64,
     IID_SUBD64,
     IID_ORD64,
     IID_ANDD64,
     IID_SICD64,
     IID_D642F,
     IID_F2D64,
     IID_D642I,
     IID_I2D64,
     IID_XORD64,
     IID_NOTD64,
     IID_ASRD64,
     IID_ABSD64, -> 2.2

     IID_FADDREG,
     IID_FSUBREG,
     IID_FMULREG,
     IID_FDIVREG,
     IID_I2FREG,
     IID_F2IREG,
     IID_FSICREG, -> v41
     IID_FNEGREG, -> v44
     IID_FABSREG, -> 2.2

     IID_GOLAB,
     IID_GOARX,
     IID_VARADR, -> new
     IID_GETRWD, -> new v34
     IID_LABADR, -> v44
     IID_GOSLAB,
     IID_GOSARX,
     IID_GETIMMSTR, -> v49 (str, arx)
     IID_INCVAR, -> 1.5.3 (var,val)
     IID_INCARX, -> 1.5.3 (arx,val)
     IID_PUSH,    -> 1.8.1 (size,o,d)
     IID_POP      -> 1.8.1 (size,o,d)




-> ofs:16, size:5, ax:5, flags:5, rsrvd:1
#define AxSizeOfsF(ax,size,ofs,flags) __axsizeofsf(ax,size,ofs,flags)
PROC __axsizeofsf(ax,size,ofs,flags) IS (Shl(ofs AND $FFFF,16) OR
                                        Shl(size,11) OR
                                        Shl(ax,6) OR
                                        flags)

-> scale:5, idrx:5, size:5, ax:5, flags:5
#define AxSizeIdrxScaleF(ax,size,idrx,scale,flags) __axsizeidrxscalef(ax,size,idrx,scale,flags)
PROC __axsizeidrxscalef(ax,size,idrx,scale,flags) IS (Shl(scale,27) OR
                                                     Shl(idrx,22) OR
                                                     Shl(size,17) OR
                                                     Shl(ax,12) OR
                                                     Shl(flags,7))

#define AxSizeOfs(ax,size,ofs) AxSizeOfsF(ax,size,ofs,NIL)
#define AxSizeIdrxScale(ax,size,idrx,scale) AxSizeIdrxScaleF(ax,size,idrx,scale,NIL)

#define ARXPOarx(x) (Shr(x,6) AND $1F)
#define ARXPOsize(x) (Shr(x,11) AND $1F)
#define ARXPOofs(x) Shr(x,16)
#define ARXPOflags(x) (x AND $1F)

#define ARXPXarx(x) (Shr(x,12) AND $1F)
#define ARXPXsize(x) (Shr(x,17) AND $1F)
#define ARXPXidrx(x) (Shr(x,22) AND $1F)
#define ARXPXscale(x) (Shr(x,27) AND $1F)
#define ARXPXflags(x) (Shr(x,7) AND $1F)

-> 1.5.3
-> hintargs might be NIL, if so ignore it.
-> it IS safe to ignore hintargs completely. (unless you are dvs_#?())
-> A copy of result is always done by caller anyway.
OBJECT hintargs
   -> type of register we would _prefer_ to have result in.
   -> DREG/AREG/FREG/VREG
   hregop:INT
   -> if > -1 then we would _prefer_ result in this specific register #
   hregnum:INT
   -> if TRUE, hreg is register variable
   -> getlist2() and mathlogic() checks this and avoids using if TRUE
   hregisvar:INT
ENDOBJECT

OBJECT codegen
   -> 1.6.1
   fastnew_lif:PTR TO lif
   fastdispose_lif:PTR TO lif
   throw_lif:PTR TO lif
   raise_lif:PTR TO lif
   -> 1.8.0
   ivar_exception:PTR TO gvar
   pvar_exceptstruct:PTR TO gvar
ENDOBJECT

#define GLOBREG g_globreg
#define STACKREG g_stackreg
#define FRAMEREG g_framereg
#define IREG0 g_ireg0
#define IREG1 g_ireg1
#define IREG2 g_ireg2
#define FREG0 g_freg0
#define VREG0 g_vreg0
#define SELFREG g_selfreg

-> theese 3 are NOT obtainable !
#define ATEMP g_atemp
#define DTEMP g_dtemp
#define FTEMP g_ftemp
#define VTEMP g_vtemp
#define D64TEMP g_d64temp

#define EMPTYMETHOD(xxx,codegen) PROC xxx OF codegen IS WriteF('codegen.xxx is EMPTY !\n') BUT NIL

EMPTYMETHOD(doAsm(n), codegen)
EMPTYMETHOD(dispatcher(iid,o1,d1,o2,d2,e1,e2,e3), codegen)
EMPTYMETHOD(prochead(tab,tabsize, ftab,ftabsize), codegen)
EMPTYMETHOD(exceptblock(do), codegen)
EMPTYMETHOD(endproc1(), codegen)
EMPTYMETHOD(endproc2(), codegen)
EMPTYMETHOD(obtainDREG(), codegen)
EMPTYMETHOD(releaseDREG(r,b), codegen)
EMPTYMETHOD(obtainAREG(), codegen)
EMPTYMETHOD(releaseAREG(r,b), codegen)
EMPTYMETHOD(obtainFREG(), codegen)
EMPTYMETHOD(releaseFREG(r,b), codegen)
EMPTYMETHOD(obtainVREG(), codegen)
EMPTYMETHOD(releaseVREG(r,b), codegen)
EMPTYMETHOD(saveObtained(regusecopy), codegen)
EMPTYMETHOD(loadObtained(regusecopy), codegen)
EMPTYMETHOD(doProcfunc(n,meth), codegen)
EMPTYMETHOD(doLibfunc(n), codegen)
EMPTYMETHOD(doVarfunc(n), codegen)
EMPTYMETHOD(doIntfunc(n), codegen)
EMPTYMETHOD(newMemory(sizeo,sized), codegen)
EMPTYMETHOD(endMemory(sizeo,sized,varo,vard), codegen)
EMPTYMETHOD(newClassObject(obj), codegen)
EMPTYMETHOD(endClassObject(obj,varo,vard), codegen)
-> v47
EMPTYMETHOD(nilCheck(arx, line), codegen)
EMPTYMETHOD(copyObtainToDREG(o,d), codegen)
EMPTYMETHOD(copyObtainToAREG(o,d), codegen)
EMPTYMETHOD(copyObtainToFREG(o,d), codegen)
EMPTYMETHOD(copyObtainToVREG(o,d), codegen)
-> V48
EMPTYMETHOD(abiArgs(array,numargs,flags), codegen)
-> v49
EMPTYMETHOD(doReturn(n,r), codegen)
EMPTYMETHOD(abiReturns(mret), codegen)
-> v50
EMPTYMETHOD(secureReturn(ro, rd, regusecopy), codegen)
EMPTYMETHOD(resetObtainStart(), codegen)
-> V50 (implemented 1.5.4)
EMPTYMETHOD(mathlogichead(n, float, neg, ha), codegen)
-> v50 (1.5.3)
EMPTYMETHOD(quotehead(), codegen)
EMPTYMETHOD(quotetail(qr1,qr2,o,d,qarx), codegen)
EMPTYMETHOD(lockDREG(dreg), codegen)
EMPTYMETHOD(lockAREG(areg), codegen)
EMPTYMETHOD(lockFREG(freg), codegen)
-> v50 (1.5.4)
EMPTYMETHOD(getFreeDREG(), codegen)
EMPTYMETHOD(getFreeAREG(), codegen)
EMPTYMETHOD(getFreeFREG(), codegen)
-> v55 (1.10.0)
EMPTYMETHOD(getFreeD64(), codegen)
EMPTYMETHOD(lockD64(reg), codegen)
EMPTYMETHOD(copyObtainToD64(o,d), codegen)
EMPTYMETHOD(obtainD64(), codegen)
EMPTYMETHOD(releaseD64(r,b), codegen)
EMPTYMETHOD(copy(o1,d1,o2,d2), codegen)
EMPTYMETHOD(dispatcher2(iid,o1,d1,o2,d2,e1,e2,e3), codegen)
EMPTYMETHOD(dispatcher3(iid,o1,d1,o2,d2,e1,e2,e3), codegen)
-> v57
EMPTYMETHOD(putInit(globsize,ivarsize), codegen)
EMPTYMETHOD(putGlobInit(), codegen)
-> v58
EMPTYMETHOD(doMethod(n,as,as2,postkey,type), codegen)


PROC initCodegen() OF codegen
   #ifdef DBG_CODEGEN
   DEBUGF('initCodegen()\n')
   #endif
   NEW g_lastx
   NEW g_regusetab[REGUSETABSIZE]
ENDPROC

PROC checkmemlist()
   DEF s, a
   DEBUGF('FreeStack=$\h AvailMem=$\h\n', s := FreeStack(), a := AvailMem($20000))
   IF s < 1000 THEN Raise("STCK")
   IF a < 1000000 THEN Raise("MEM")
ENDPROC





PROC clearRegs()  -> for 68k and ppc, at label entry, inline asm, etc-
   DEF a, t, var:REG PTR TO var

   #ifdef DBG_CODEGEN
   DEBUGF('clearRegs(): ')
   #endif

      #ifdef DBG_CODEGEN
      DEBUGF('\n')
      #endif

   var := g_gvarlist
   WHILE var
      #ifdef DBG_CODEGEN
      IF var.intreg THEN DEBUGF('   global \s:\d cleared\n', var.hln.name, var.treg)
      #endif
      var.intreg := FALSE -> clear
      var := var.next
   ENDWHILE


   IF g_currentproc -> changed v50 (1.5.1)
      var := g_currentproc.varchain
      WHILE var
         #ifdef DBG_CODEGEN
         IF var.intreg THEN DEBUGF('   local \s:\d cleared\n', var.hln.name, var.treg)
         #endif
         var.intreg := FALSE -> clear
         var := var.varchain
      ENDWHILE
   ENDIF


ENDPROC

PROC def_label(lab:PTR TO codelab)
   clearRegs()
   g_lastx.iid := NIL -> 1.6.0
   g_lastx.end := NIL
   lab.offset := currentOffset()
ENDPROC

PROC reverseCondition(cond) -> used also by ecxmain for RAISE functionality
   SELECT cond
   CASE ISEQ ; cond := ISNE
   CASE ISNE ; cond := ISEQ
   CASE ISGT ; cond := ISLE
   CASE ISLT ; cond := ISGE
   CASE ISLE ; cond := ISGT
   CASE ISGE ; cond := ISLT
   ENDSELECT
ENDPROC cond

#define inst_copy(o1,d1,o2,d2) self.copy(o1,d1,o2,d2)

#define inst_varadr(var,arx) self.dispatcher(IID_VARADR,NIL,var,NIL,arx,0,0,0)
#define inst_incvar(var,val) self.dispatcher(IID_INCVAR,NIL,var,NIL,val,0,0,0)
#define inst_decvar(var,val) self.dispatcher(IID_INCVAR,NIL,var,NIL,0-(val),0,0,0)
#define inst_incarx(arx,val) self.dispatcher(IID_INCARX,NIL,arx,NIL,val,0,0,0)
#define inst_decarx(arx,val) self.dispatcher(IID_INCARX,NIL,arx,NIL,0-(val),0,0,0)
#define inst_getrwd(rwdofs, ax) self.dispatcher(IID_GETRWD,NIL,rwdofs,NIL,ax,0,0,0)
#define inst_ret(rtsbool) self.dispatcher(IID_RET,NIL,rtsbool,0,0,0,0,0)
#define inst_negreg(d1) self.dispatcher(IID_NEGREG,NIL,d1,0,0,0,0,0)
#define inst_mulreg(o1,d1,d2) self.dispatcher(IID_MULREG,o1,d1,NIL,d2,0,0,0)
#define inst_divreg(o1,d1,d2) self.dispatcher(IID_DIVREG,o1,d1,NIL,d2,0,0,0)
#define inst_bic(o1,d1,c,o2,d2,label) self.dispatcher(IID_BIC,o1,d1,o2,d2,c,label,0)
#define inst_shlreg(o1,d1,d2) self.dispatcher(IID_SHLREG,o1,d1,NIL,d2,0,0,0)
#define inst_shrreg(o1,d1,d2) self.dispatcher(IID_SHRREG,o1,d1,NIL,d2,0,0,0)
#define inst_orreg(o1,d1,d2) self.dispatcher(IID_ORREG,o1,d1,NIL,d2,0,0,0)
#define inst_andreg(o1,d1,d2) self.dispatcher(IID_ANDREG,o1,d1,NIL,d2,0,0,0)
#define inst_golab(lab) self.dispatcher(IID_GOLAB,NIL,lab,0,0,0,0,0)
#define inst_goarx(lab) self.dispatcher(IID_GOARX,NIL,lab,0,0,0,0,0)
#define inst_addreg(o1,d1,d2) self.dispatcher(IID_ADDREG,o1,d1,NIL,d2,0,0,0)
#define inst_subreg(o1,d1,d2) self.dispatcher(IID_SUBREG,o1,d1,NIL,d2,0,0,0)
#define inst_sicreg(o1,d1,cond,reg) self.dispatcher(IID_SICREG,o1,d1,NIL,NIL,cond,reg,0)
#define inst_labadr(lab,reg) self.dispatcher(IID_LABADR,NIL,lab,NIL,reg,0,0,0)
#define inst_goslab(l) self.dispatcher(IID_GOSLAB,NIL,l,0,0,0,0,0)
#define inst_gosarx(a) self.dispatcher(IID_GOSARX,NIL,a,0,0,0,0,0)
#define inst_getimmstr(str, arx) self.dispatcher(IID_GETIMMSTR,0,0,0,0,str,arx,0)
#define inst_push(size,o,d) self.dispatcher(IID_PUSH,o,d,NIL,NIL,size,0,0)
#define inst_pop(size,o,d) self.dispatcher(IID_POP,o,d,NIL,NIL,size,0,0)
#define inst_asrreg(o1,d1,d2) self.dispatcher(IID_ASRREG,o1,d1,NIL,d2,0,0,0)
#define inst_xorreg(o1,d1,d2) self.dispatcher(IID_XORREG,o1,d1,NIL,d2,0,0,0)
#define inst_notreg(d1) self.dispatcher(IID_NOTREG,NIL,d1,0,0,0,0,0)
#define inst_absreg(d1) self.dispatcher(IID_ABSREG,NIL,d1,0,0,0,0,0)

#define inst_faddreg(o1,d1,d2) self.dispatcher2(IID_FADDREG,o1,d1,NIL,d2,0,0,0)
#define inst_fsubreg(o1,d1,d2) self.dispatcher2(IID_FSUBREG,o1,d1,NIL,d2,0,0,0)
#define inst_fmulreg(o1,d1,d2) self.dispatcher2(IID_FMULREG,o1,d1,NIL,d2,0,0,0)
#define inst_fdivreg(o1,d1,d2) self.dispatcher2(IID_FDIVREG,o1,d1,NIL,d2,0,0,0)
#define inst_fnegreg(d1) self.dispatcher2(IID_FNEGREG,NIL,d1,0,0,0,0,0)
#define inst_fsicreg(o1,d1,cond,freg,reg) self.dispatcher2(IID_FSICREG,o1,d1,NIL,NIL,cond,freg,reg)
#define inst_i2f(r,f) self.dispatcher2(IID_I2FREG,NIL,r,0,f,0,0,0)
#define inst_f2i(f,r) self.dispatcher2(IID_F2IREG,NIL,f,0,r,0,0,0)
#define inst_fabsreg(d1) self.dispatcher2(IID_FABSREG,NIL,d1,0,0,0,0,0)


-> 1.10.0
#define inst_addd64(o1,d1,d2) self.dispatcher3(IID_ADDD64,o1,d1,NIL,d2,0,0,0)
#define inst_subd64(o1,d1,d2) self.dispatcher3(IID_SUBD64,o1,d1,NIL,d2,0,0,0)
#define inst_i2d64(r,f) self.dispatcher3(IID_I2D64,NIL,r,0,f,0,0,0)
#define inst_f2d64(f,r) self.dispatcher3(IID_F2D64,NIL,f,0,r,0,0,0)
#define inst_d642f(f,r) self.dispatcher3(IID_D642F,NIL,f,0,r,0,0,0)
#define inst_d642i(f,r) self.dispatcher3(IID_D642I,NIL,f,0,r,0,0,0)
#define inst_sicd64(o1,d1,cond,reg64,reg) self.dispatcher3(IID_SICD64,o1,d1,NIL,NIL,cond,reg64,reg)
#define inst_shld64(o1,d1,d2) self.dispatcher3(IID_SHLD64,o1,d1,NIL,d2,0,0,0)
#define inst_shrd64(o1,d1,d2) self.dispatcher3(IID_SHRD64,o1,d1,NIL,d2,0,0,0)
#define inst_ord64(o1,d1,d2) self.dispatcher3(IID_ORD64,o1,d1,NIL,d2,0,0,0)
#define inst_andd64(o1,d1,d2) self.dispatcher3(IID_ANDD64,o1,d1,NIL,d2,0,0,0)
#define inst_negd64(d1) self.dispatcher3(IID_NEGD64,NIL,d1,0,0,0,0,0)
#define inst_muld64(o1,d1,d2) self.dispatcher3(IID_MULD64,o1,d1,NIL,d2,0,0,0)
#define inst_divd64(o1,d1,d2) self.dispatcher3(IID_DIVD64,o1,d1,NIL,d2,0,0,0)
#define inst_asrd64(o1,d1,d2) self.dispatcher3(IID_ASRD64,o1,d1,NIL,d2,0,0,0)
#define inst_xord64(o1,d1,d2) self.dispatcher3(IID_XORD64,o1,d1,NIL,d2,0,0,0)
#define inst_notd64(d1) self.dispatcher3(IID_NOTD64,NIL,d1,0,0,0,0,0)
#define inst_absd64(d1) self.dispatcher3(IID_ABSD64,NIL,d1,0,0,0,0,0)

PROC def_line(l)
   IF l <= 0 THEN RETURN
   IF g_linelist
      IF g_linelist.line = l THEN RETURN
      IF g_linelist.offset = (currentOffset()) THEN RETURN
   ENDIF
   g_linelist := NEW [g_linelist, l, currentOffset()]:linedef
   IF g_stepdebug AND (g_optpowerpc=CPU_M68)
     nop()
   ENDIF
   IF g_stepdebug50 AND (g_optpowerpc=CPU_M68)
     nop()
     nop()
     nop()
   ENDIF
ENDPROC







/*************************************************************
*************************** methods of codegen ***************
*************************************************************/

PROC endLocalClasses(proc:PTR TO proc) OF codegen
   DEF va:PTR TO var, m:PTR TO proc

   va := proc.locals00 -> v46, call .end() on classes
   WHILE va
      IF va.type.object
      IF va.type.numes = 1
      IF va.type.object.nrofmethods
      IF va.type.object.destofs > -1  -> call .end() ?
         m := va.type.object.methodtable[va.type.object.destofs/4]
         IF m.flags AND PROCF_CLMETH = FALSE
            inst_copy(va.o,va.d, AREG, SELFREG)
            inst_copy(ARXPO,AxSizeOfs(SELFREG,PTRSIZE,va.type.object.classofs), AREG,ATEMP)
            inst_copy(ARXPO,AxSizeOfs(ATEMP,PTRSIZE,va.type.object.destofs), AREG, ATEMP)
            inst_gosarx(ATEMP)
         ELSE -> 1.8.0
            inst_copy(va.o,va.d, AREG,m.selfreg)
            IF m.offset
               inst_goslab(m)
            ELSE
               inst_labadr(va.type.object, ATEMP)
               inst_copy(ARXPO,AxSizeOfs(ATEMP,PTRSIZE,va.type.object.destofs), AREG, ATEMP)
               inst_gosarx(ATEMP)
            ENDIF
         ENDIF
         clearRegs()
      ENDIF ; ENDIF ; ENDIF ; ENDIF
      va := va.next
   ENDWHILE

ENDPROC

PROC sortRegAlloc(buf:PTR TO LONG, count)
   DEF exit, va:PTR TO var, va2:PTR TO var, a
   REPEAT
      exit := TRUE
      FOR a := 1 TO count-1
         va := buf[a]
         va2 := buf[a-1]
         IF va2.usage < va.usage
            buf[a] := buf[a-1]
            buf[a-1] := va
            exit := FALSE
         ENDIF
      ENDFOR
   UNTIL exit
ENDPROC

PROC collectRegVars(buf:PTR TO LONG, firstvar, count, op, min)
   DEF va:PTR TO var
   va := firstvar
   WHILE va
      IF va.usage >= min
         IF va.o <> op
            IF va.trok THEN buf[count++] := va
         ENDIF
      ENDIF
      va := va.next
   ENDWHILE
ENDPROC count



-> v43: moved arg/local/frame init into DEF_PROC
PROC doProc(n:PTR TO item) OF codegen
   DEF proc:PTR TO proc, va:REG PTR TO var, a
   DEF o, d, buf:PTR TO item
   DEF t1, t2
   DEF count=0, fcount=0, exit, t
   DEF returnssaved=FALSE, va2:PTR TO var
   DEF allocregsbuf[256]:ARRAY OF LONG
   DEF allocfregsbuf[256]:ARRAY OF LONG


   proc := n.info -> !!
   n++

   #ifdef DBG_CODEGEN
   DEBUGF('doproc() "\s"\n', proc.name)
   #endif

   /* auto reg alloc */
   IF proc.handle = FALSE
      IF g_numregalloc
         count := collectRegVars(allocregsbuf, proc.args32, count, DREG, IF g_numregalloc < 0 THEN MINALLOCUSAGE ELSE 0)
         count := collectRegVars(allocregsbuf, proc.locals32, count, DREG, IF g_numregalloc < 0 THEN MINALLOCUSAGE ELSE 0)
         sortRegAlloc(allocregsbuf, count)
         #ifdef DBG_CODEGEN
         DEBUGF('doproc() "\s" found \d vars suitable for regalloc\n', proc.name, count)
         #endif
      ENDIF
      IF g_numfregalloc
         ->IF proc.maxcalldepth = NIL -> no chance of exception happening ?
            fcount := collectRegVars(allocfregsbuf, proc.args64, fcount, FREG, IF g_numfregalloc < 0 THEN MINALLOCUSAGE ELSE 0)
            fcount := collectRegVars(allocfregsbuf, proc.locals64, fcount, FREG, IF g_numfregalloc < 0 THEN MINALLOCUSAGE ELSE 0)
         ->ENDIF
          sortRegAlloc(allocfregsbuf, fcount)
         #ifdef DBG_CODEGEN
         DEBUGF('doproc() "\s" found \d vars suitable for fregalloc\n', proc.name, fcount)
         #endif
      ENDIF

   ELSE -> v58

      IF proc.except = FALSE THEN reportErr('HANDLE without EXCEPT')

   ENDIF


   IF g_linedebug THEN def_line(g_linenum)

   g_currentproc := proc

  #ifdef DBG_CODEGEN
  DEBUGF('doproc() "\s" alloc and init locals\n', proc.name)
  #endif


   self.prochead(allocregsbuf, count, allocfregsbuf, fcount) -> v37, v45, 46, 48

   ->IF g_symbolhunk
   ->   IF proc.object
   ->      addMethDbgSym(proc.object.name, proc.name, proc.offset)
   ->   ENDIF
   ->ENDIF

   g_stacksize := g_stacksize + proc.framesize -> v58

     -> v50
   self.resetObtainStart()


    n := self.compileCode(n)


    IF n.data <> KW_ENDPROC
       IF n.data <> KW_IS THEN reportErr('unexpected end of source')
    ENDIF


       #ifdef DBG_CODEGEN
       DEBUGF('doproc() "\s" final self=$\h, classinfo=$\h\n', proc.name, self, Long(self-4))
       #endif

    IF CtrlC() THEN reportErr('CtrlC')

    IF g_linedebug THEN def_line(g_linenum)


    self.endproc1()


   -> now BEFORE returning values (v49) !!
   -> means we CAN NOT used autoinit classes at ENDPROC

   n := self.doReturn(n, FALSE)


   self.endproc2()

   ->#ifdef DBG_CODEGEN
   t := FALSE
    FOR a := 0 TO REGUSETABSIZE-1
       IF (g_regusetab[a].obtains) OR (g_regusetab[a].write)
          DEBUGF(' !!! ENDPROC \s regusetab[\d] = \d,\d\n', proc.name, a, g_regusetab[a].obtains, g_regusetab[a].write)
          t := TRUE
       ENDIF
    ENDFOR
    IF t THEN reportIErr('REGUSETAB is inconsistent')


   #ifdef DBG_CODEGEN
    DEBUGF('doproc() "\s" DONE\n', proc.name)
   #endif

    -> v50

    IF proc.framesize > (32*1024) THEN
      reportErr('local stack usage exceeds 32k for procedure', proc.name)

   g_currentproc := NIL


ENDPROC n

-> v48, now returns "end-statement" bool in second return value.
-> NIL = ENDIF,ENDFOR,ENDSELECT, etc...,  <> NIL = n.data
PROC compileCode(n:PTR TO item, oneliner=FALSE) OF codegen
   DEF t, as, as2, hln:PTR TO hln, var:PTR TO var, asm:PTR TO asm
   DEF lab:PTR TO codelab, reg:PTR TO reg, e:PTR TO entry, a
   DEF dh, dl -> v50

   #ifdef DBG_CODEGEN
   DEBUGF('compileCode($\h): line=\d \n', n, g_linenum)
   #endif

   IF FreeStack() < 4000 THEN Raise("STCK") -> 2.0
   

   WHILE (t := n.data)
      as := NIL
      SELECT 256 OF t
      CASE IT_SUBSTATEMENT
         self.compileCode(n.info)
         n++
      CASE 10
         g_linenum := n.info
         IF g_linedebug THEN def_line(g_linenum)
         n++
         IF oneliner THEN RETURN n--, n.data
      CASE IT_PROC
         n := self.doProc(n)
      CASE KW_EXCEPT
         n++
         IF n.data = KW_DO
            n++
            self.exceptblock(TRUE)
         ELSE
            self.exceptblock(FALSE)
         ENDIF
      CASE IT_LABEL
         def_label(n.info.ident)
         n++
      CASE IT_ASM                ; n := self.doAsm(n)
      CASE KW_FOR                ; n := self.doFor(n)
      CASE KW_WHILE, KW_WHILEN   ; n := self.doWhile(n)
      CASE KW_LOOP               ; n := self.doLoop(n)
      CASE KW_REPEAT             ; n := self.doRepeat(n)
      CASE KW_SELECT
         IF n[2].data = KW_OF
            n := self.doSelectOf(n)
         ELSE
            n := self.doSelect(n)
         ENDIF
      CASE KW_IF
         IF n[2].data = KW_THEN THEN n := self.doIfThenStat(n) ELSE n := self.doIfEndif(n)
      CASE KW_RETURN  ; n := self.doReturn(n,TRUE)
      CASE KW_JUMP
         n++
         lab := n.info.ident
         IF lab = NIL THEN reportErr('unknown dentifier', n.info.name)
         IF lab.identID <> IDENT_LABEL THEN reportErr('label expected', n.info.name)
         lab.referenced := 1
         inst_golab(lab)
         n++
      CASE KW_INC
         n++ ; var := n.info ; inst_incvar(var, 1) ; n++
      CASE KW_DEC
         n++ ; var := n.info ; inst_decvar(var, 1) ; n++
      CASE IT_PFUNC   ; n := self.doProcfunc(n,NIL)
      CASE IT_IFUNC    ; n := self.doIntfunc(n)
      CASE IT_LFUNC    ; n := self.doLibfunc(n)
      CASE IT_VFUNC   ; n := self.doVarfunc(n)
      CASE IT_MASSIGN ; n := self.doMAssign(n) -> v49
      CASE IT_VAREXP  ; n := self.doVarstuff(n, TRUE, NIL)
      CASE IT_UNIEXP  ; n := self.doListUni(n)
      CASE KW_INCBIN   ; n := self.doIncbin(n)
      CASE KW_CHAR  -> also BYTE
         putAlign(2)
         n++
         WHILE (t := n.data) <> 10
            SELECT t
            CASE IT_VALUE
               IF n.num THEN reportErr('illegal value for CHAR/BYTE')
               PutChar(g_codeptr, n.info)
               g_codeptr := g_codeptr + 1
            CASE IT_STRING
               t := n.info
               a := EstrLen(t) + 1
               WHILE a-- DO PutChar(g_codeptr, t[]++) BUT g_codeptr := g_codeptr + 1
            ENDSELECT
            n++
         ENDWHILE
         putAlign(2) -> important for CHAR/BYTE
         clearRegs()
      CASE KW_INT -> also WORD
         putAlign(2)
         n++
         WHILE n.data = IT_VALUE
            IF n.num THEN reportErr('illegal value for INT/WORD')
            PutInt(g_codeptr, n.info)
            g_codeptr := g_codeptr + 2
            n++
         ENDWHILE
         IF n.data <> 10 THEN reportErr('integer value expected')
         clearRegs()
      CASE KW_LONG -> also PTR
         putAlign(2)
         n++
         WHILE (t := n.data) <> 10
            SELECT t
            CASE IT_LABEL
               lab := n.info.ident
               IF lab = NIL THEN reportErr('unknown inline label', n.info.name)
               IF lab.identID <> IDENT_LABEL
                  reportErr('wrong type of label', n.info.name)
               ENDIF
               lab.referenced := 1
               putReloc(lab)
            CASE IT_VALUE
               Put32(n.info)
            CASE IT_STRING
               lab := addRelStr(n.info)
               putReloc(lab)
            ENDSELECT
            n++
         ENDWHILE
         clearRegs()
      CASE KW_FLOAT  -> 1.5.1
         putAlign(4)
         n++
         WHILE n.data = IT_VALUE
            Put32(n.info)
            n++
         ENDWHILE
         clearRegs()
      CASE KW_DOUBLE -> v50
         putAlign(4)
         n++
         WHILE n.data = IT_VALUE
            dh, dl := singToDoub(n.info)
            PutLong(g_codeptr, dh)
            PutLong(g_codeptr + 4, dl)
            g_codeptr := g_codeptr + 8
            n++
         ENDWHILE
         IF n.data <> 10 THEN reportErr('float value expected')
         clearRegs()
      CASE KW_WIDE -> v56
         putAlign(4)
         n++
         WHILE n.data = IT_VALUE
            PutLong(g_codeptr + 4, n.info)
            PutLong(g_codeptr, IF n.info AND $80000000 THEN $FFFFFFFF ELSE NIL)
            g_codeptr := g_codeptr + 8
            n++
         ENDWHILE
         IF n.data <> 10 THEN reportErr('constant value expected')
         clearRegs()
      CASE KW_ELSE, KW_ELSEIF, KW_ENDIF, KW_CASE, KW_DEFAULT, KW_ENDSELECT,
           KW_ENDFOR, KW_ENDWHILE, KW_ENDLOOP, KW_UNTIL, KW_ENDPROC, KW_IS
         RETURN n, NIL
      CASE KW_EXIT
         RETURN n, n.data
      CASE KW_BUT ; n++-> just ignore
      CASE KW_VOID  -> V47
         n++
         n := self.doExpression(n)
      CASE KW_NOP
         n++
      CASE KW_STATIC -> 1.9.0
         n := doStatic(n[1])
      DEFAULT
         #ifdef DBG_CODEGEN
         DEBUGF('n.data=\d\n', n.data)
         #endif
         reportIErr(' statement does not compute')
      ENDSELECT



   ENDWHILE
   #ifdef DBG_CODEGEN
   DEBUGF('compileCode() DONE\n')
   #endif


ENDPROC n, n.data

-> 1.9.0
PROC doStatic(n:PTR TO item)
   DEF lab:PTR TO statlab, hln:PTR TO hln, ofs
   WHILE n.data = IT_LABEL
      hln := n.info
      lab := hln.ident
      n++
      IF n.data = IT_STRING
         n, ofs := makeStaticString(n)
         lab.offset := ofs
      ELSE
         n, ofs := makeStaticList(n)
         lab.offset := ofs
      ENDIF
   ENDWHILE
ENDPROC n

PROC makeStaticString(n:PTR TO item)
   DEF ofs, t
   putAlign(4)
   ofs := currentOffset()
   ->WHILE TRUE
      CopyMem(n.info, g_codeptr, t := EstrLen(n.info))
      g_codeptr := g_codeptr + t
      n++
   ->   EXIT n.data <> "+"
   ->   n++
   ->ENDWHILE
   put8(NIL)
   putAlign(4)
ENDPROC n, ofs

PROC makeStaticList(n:PTR TO item)
   ->DEF flags, esize, len, object:PTR TO object
   ->DEF start, size
   DEF offset, ptr:PTR TO LONG
   DEF sl:PTR TO it_statlist

   sl := n.info

   ->flags := n.num
   ->len := n.info
   ->esize := n.info2 AND $FF
   ->object := IF n.info2 > 1000 THEN n.info2 ELSE NIL


   -> do some alignment
   IF sl.object
      putAlign(sl.object.alignment)
   ELSE
      putAlign(sl.esize)
   ENDIF
   -> list len ?
   IF sl.cplx
      Put32(sl.len)
   ENDIF
   -> label of data at this offset
   offset := currentOffset()
   ptr := g_codeptr

   -> compute size needed
   ->size := IF object = NIL THEN Mul(esize, len) ELSE (object.sizeof *
   ->                                       (Div(len, object.nrofmembers) +
   ->
   ->                                        IF Mod(len, object.nrofmembers) THEN 1 ELSE 0))

   -> alloc space
   g_codeptr := g_codeptr + sl.sizeof

   n := evalStaticList(n, ptr, offset)

ENDPROC n, offset

PROC evalStaticList(n:PTR TO item, ptr:PTR TO LONG, oldoffset)
   ->DEF flags, esize, len, object:PTR TO object
   DEF memb:PTR TO member
   DEF start, ofs, size
   DEF offset, a, dh, dl, t, hln:PTR TO hln
   DEF lab:PTR TO codelab, tstr[50]:STRING
   DEF sl:PTR TO it_statlist, len, esize

   ->flags := n.num
   ->len := n.info
   ->esize := n.info2 AND $FF
   ->object := IF n.info2 > 1000 THEN n.info2 ELSE NIL
   sl := n.info
   len := sl.len
   esize := sl.esize
   n++


   start := ptr

   -> eval and put data
   IF esize = 4
      WHILE len
         t := n.data
         IF     t = IT_VALUE
            ptr[]++ := n.info
            n++
         ELSEIF t = IT_STRING
            n, ofs := makeStaticString(n)
            addReloc(oldoffset + ptr - start)
            ptr[]++ := ofs
         ELSEIF t = IT_LABEL
            hln := n.info
            lab := hln.ident
            IF lab = NIL THEN reportErr('unknown label', hln.name)
            lab.referenced := 1
            IF lab.offset
               addReloc(oldoffset + ptr - start)
               ptr[]++ := lab.offset
            ELSE
               lab.labrefs := NEW [lab.labrefs, oldoffset + ptr - start, REFADR]:LONG
               ptr++
            ENDIF
            n++
         ELSEIF t = "["
            n, ofs := makeStaticList(n)
            addReloc(oldoffset + ptr - start)
            ptr[]++ := ofs
         ELSE
            reportIErr('evalStaticList ', StringF(tstr, 't=\d', t))
         ENDIF
         len--
      ENDWHILE
   ELSEIF sl.object <> NIL
      offset := 0
      WHILE len > 0
         FOR a := 0 TO sl.object.nrofmembers-1
            EXIT len = 0
            memb := sl.object.membertable[a]
            esize := memb.size
            t := n.data
            IF esize = 0 -> we support embedded objects !!
               IF t <> "[" THEN reportErr('"[" expected for embedded array')
               n, size := evalStaticList(n, ptr + memb.offset + offset, oldoffset + memb.offset + offset)
               IF Mul(memb.numes, IF memb.esize = 255 THEN memb.object.sizeof ELSE memb.esize) < size
                  reportErr('immediate array bigger than member', memb.name)
               ENDIF
            ELSEIF t = IT_VALUE
               SELECT esize
               CASE 8 -> 1.8.2, was missing!
                  IF memb.flags AND MEMBF_FLOAT
                     dh, dl := singToDoub(n.info)
                  ELSE -> 2.3, was missing
                     dh := IF n.info AND $80000000 THEN -1 ELSE 0
                     dl := n.info
                  ENDIF
                  PutLong(ptr + memb.offset + offset, dh)
                  PutLong(ptr + memb.offset + offset + 4, dl)
               CASE 4
                  PutLong(ptr + memb.offset + offset, n.info)
               CASE 2
                  PutInt(ptr + memb.offset + offset, n.info)
               CASE 1
                  PutChar(ptr + memb.offset + offset, n.info)
               DEFAULT
                  reportIErr('evalStaticList object IT_VALUE esize ?')
               ENDSELECT
               n++
            ELSEIF t = IT_LABEL
               hln := n.info
               IF esize <> 4 THEN reportErr('cannot fit relocation in list element', hln.name)
               lab := hln.ident
               IF lab = NIL THEN reportErr('unknown label', hln.name)
               lab.referenced := 1  -> 1.10.0
               IF lab.offset
                  addReloc(oldoffset + ptr - start + memb.offset + offset)
                  PutLong(ptr + memb.offset + offset, lab.offset)
               ELSE
                  lab.labrefs := NEW [lab.labrefs, oldoffset + ptr - start + memb.offset + offset, REFADR]:LONG
               ENDIF
               n++
            ELSEIF t = IT_STRING
               IF esize <> 4 THEN reportErr('cannot fit relocation in list element')
               n, ofs := makeStaticString(n)
               addReloc(oldoffset + ptr - start + memb.offset + offset)
               PutLong(ptr + memb.offset + offset, ofs)
            ELSEIF t = "["
               IF esize <> 4 THEN reportErr('cannot fit relocation in list element')
               n, ofs := makeStaticList(n)
               addReloc(oldoffset + ptr - start + memb.offset + offset)
               PutLong(ptr + memb.offset + offset, ofs)
            ELSE
               reportIErr('evalStaticList object', StringF(tstr, 't=\d', t))
            ENDIF
            len--
         ENDFOR
         offset := offset + sl.object.sizeof
      ENDWHILE
      ptr := ptr + offset
   ELSE
      WHILE len
         t := n.data
         IF     t = IT_VALUE
            SELECT esize
            CASE 1   ; PutChar(ptr, n.info) ; ptr := ptr + 1
            CASE 2   ; PutInt(ptr, n.info)  ; ptr := ptr + 2
            CASE 8
               IF sl.flags AND MEMBF_FLOAT
                  dh, dl := singToDoub(n.info)
               ELSE
                  dh := IF n.info AND $80000000 THEN -1 ELSE 0
                  dl := n.info
               ENDIF
               PutLong(ptr, dh)
               PutLong(ptr + 4, dl)
               ptr := ptr + 8
            DEFAULT    ; reportIErr('evalStaticList', StringF(tstr, 'esize=\d', esize))
            ENDSELECT
            n++
         ELSE
            reportErr('ilegal value for STATIC list')
         ENDIF
         len--
      ENDWHILE
   ENDIF

ENDPROC n, ptr - start

-> 1.9.0: always as expression, never statement
PROC doLabDeref(it:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF hln:PTR TO hln, n:PTR TO item, lab:PTR TO statlab, memb:PTR TO member
   DEF type:member, arx, size, offset, flags, irx=-1, scale, o, d, t, exit=FALSE, i

   #ifdef DBG_CODEGEN
   DEBUGF('doLabDeref($\h, $\h)\n', it, ha)
   #endif

   n := it.info
   hln := n.info
   lab := hln.ident

   arx := self.obtainAREG()

   inst_labadr(lab, arx)
   type.size := lab.deref.size
   type.esize := lab.deref.esize
   type.flags := lab.deref.flags
   type.object := lab.deref.object
   IF type.object < 0  -> 1.10.0 needed for statics from modules
      hln := -type.object
      type.object := hln.ident2
      IF type.object = NIL THEN reportErr('unknown object', hln.name)
   ENDIF


   n++
   REPEAT
      #ifdef DBG_CODEGEN
      DEBUGF('doLabDeref(): size=\d esize=\d flags=$\h object=$\h ',
      type.size,type.esize, type.flags, type.object)
      #endif
      t := n.data
      SELECT t
      CASE IT_MEMBER
         #ifdef DBG_CODEGEN
         DEBUGF('IT_MEMBER\n')
         #endif
         memb := n.info
         n++
         size := memb.size
         flags := memb.flags
         i := `inst_copy(ARXPO,AxSizeOfsF(arx,size,memb.offset,flags), o,d)
         type.size := memb.size
         type.esize := memb.esize
         type.flags := memb.flags
         type.object := memb.object
      CASE "["
         #ifdef DBG_CODEGEN
         DEBUGF('"["\n')
         #endif
         n++
         size := IF type.esize = 255 THEN 0 ELSE type.esize
         flags := type.flags
         IF n.data <> "]"
            IF n.data = IT_VALUE
               IF type.esize = 255
                  offset := Mul(n.info, type.object.sizeof)
               ELSE
                  offset := Mul(n.info, size)
                  type.esize := 0
                  type.object := NIL
               ENDIF
               i := `inst_copy(ARXPO,AxSizeOfsF(arx,size,offset,flags), o,d)
               n++
            ELSE
               n, o, d := self.doExpression(n)
               irx := self.copyObtainToDREG(o,d)
               scale := size
               IF type.esize = 255
                  scale := 1
                  inst_mulreg(DV,type.object.sizeof, irx)
               ELSE
                  type.esize := 0
                  type.object := NIL
               ENDIF
               i := `inst_copy(ARXPX,AxSizeIdrxScaleF(arx,size,irx,scale,flags), o,d)
            ENDIF
         ELSE -> []
            i := `inst_copy(ARXPO,AxSizeOfsF(arx,size,0,flags), o,d)
         ENDIF
         type.size := size
         n++ -> skip "]"
      ENDSELECT
      IF n.data
         o := AREG
         d := arx
      ELSE
         exit := TRUE
         IF ha
            IF ha.hregop
               o := ha.hregop
               IF ha.hregnum <> -1
                  d := ha.hregnum
               ELSEIF ha.hregop = AREG
                  d := self.getFreeAREG()
               ELSEIF ha.hregop = DREG
                  d := self.getFreeDREG()
               ELSEIF ha.hregop = FREG
                  d := self.getFreeFREG()
               ELSEIF ha.hregop = D64REG
                  d := self.getFreeD64()
               ENDIF
            ELSE
               o := NIL
            ENDIF
         ELSE
            o := NIL
         ENDIF
         IF o = NIL
            IF size = 0
               o := AREG
               d := self.getFreeAREG()
            ELSEIF flags AND MEMBF_FLOAT
               o := FREG
               d := self.getFreeFREG()
            ELSE
               o := DREG
               d := self.getFreeDREG()
            ENDIF
         ENDIF
      ENDIF
      Eval(i)
      IF irx > -1
         self.releaseDREG(irx,0)
         irx := -1
      ENDIF
   UNTIL exit

   self.releaseAREG(arx,0)


   #ifdef DBG_CODEGEN
   DEBUGF('doLabDeref() DONE o=\d, d=$\h\n', o, d)
   #endif

ENDPROC it[1], o, d

-> changed 1.8.0
PROC doIncbin(n:PTR TO item) OF codegen HANDLE
   DEF fh=NIL, name, flen

   flen := n.info2 -> set in syntax (1.8.0)
   name := n.info -> set in syntax
   fh := Open(name, OLDFILE)
   IF fh = NIL THEN Throw("OPEN", name) -> should not happen as we have checked it in pass1
   IF Read(fh, g_codeptr, flen) <> flen THEN Throw("READ", name)
   g_codeptr := g_codeptr + (flen + 3 AND $FFFFFC)
   n++

EXCEPT DO

   IF fh THEN Close(fh)
   ReThrow()

ENDPROC n

-> 2.3
PROC doSingleExp(n:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF o, d, t, var:PTR TO var

   #ifdef DBG_CODEGEN
   DEBUGF('doSingleExp($\h, $\h) ', n, ha)
   IF ha
      DEBUGF('hregop \d hregnum \d hregisvar \d\n', ha.hregop, ha.hregnum, ha.hregisvar)
   ELSE
      DEBUGF('\n')
   ENDIF
   #endif

   t := n.data
   SELECT 256 OF t
   CASE IT_VARIABLE
      var := n.info
      n++
      o := var.o
      d := var.d
   CASE IT_VALUE
      o := DV
      d := n.info
      n++
   CASE IT_REG
      o := n.info::reg.type
      d := n.info::reg.num
      n++
   CASE IT_VARADR     ; n, o, d := self.doVarAdr(n, ha)
   CASE IT_LABADR     ; n, o, d := self.doLabAdr(n, ha)
   CASE IT_STRING     ; n, o, d := self.doString(n, ha)
   CASE IT_NEWSTRING  ; n, o, d := self.doNewString(n, ha)
   CASE IT_IMMEDLIST  ; n, o, d := self.doList(n, ha)
   CASE IT_EXPRESSION ; n, o, d := self.cplx_expression(n, ha)
   CASE IT_PFUNC      ; n, o, d := self.doProcfunc(n, NIL)
   CASE IT_IFUNC      ; n, o, d := self.doIntfunc(n)
   CASE IT_LFUNC      ; n, o, d := self.doLibfunc(n)
   CASE IT_VFUNC      ; n, o, d := self.doVarfunc(n)
   CASE IT_VAREXP     ; n, o, d := self.doVarstuff(n, FALSE, ha)
   CASE IT_IFEXP      ; n, o, d := self.doIfThenExp(n, ha)
   CASE IT_UNIEXP     ; n, o, d := self.doListUni(n)
   CASE IT_LABDEREF   ; n, o, d := self.doLabDeref(n, ha) -> 1.9.0
   DEFAULT            ; reportIErr('doSingleExp unknown IT_')
   ENDSELECT
   IF o = NIL THEN reportIErr('doSingleExp : o=NIL')
ENDPROC n, o, d

-> used by self.doExpression() only.
-> v58: needs improvements for mathlogic stuff, some done..
-> now let mathlogichead evaluate starting expression with doSingleExp..
PROC cplx_expression(buf:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF o:REG, d:REG
   DEF mode=MODE_DEFAULT, quote=FALSE
   DEF unary=FALSE:REG
   DEF t
   DEF hln:PTR TO hln, var:PTR TO var
   DEF n:REG PTR TO item
   DEF qr1, qr2

   n := buf.info

   #ifdef DBG_CODEGEN
   DEBUGF('cmplx_expression($\h, $\h)\n', n, ha)
   #endif

   IF n.data = "`"
      quote := TRUE
      n++
      qr1, qr2 := self.quotehead()
      ha := NIL -> so that not quote epilouge craps on reg from math
   ENDIF

   IF n.data = "!"
      n++
      mode := MODE_FLOAT ->float := TRUE
   ELSEIF n.data = "@" -> 1.10.0
      n++
      mode := MODE_D64
   ENDIF

   IF n.data = "-"
      n++
      unary := "-"
   ELSEIF n.data = "~"
      n++
      unary := "~"
   ELSEIF n.data = KW_ABS
      n++
      unary := KW_ABS
   ENDIF

   t := n[1].data
   SELECT 256 OF t
   CASE KW_BUT
      n, o, d := self.doSingleExp(n, NIL)
      n++
      n, o, d := self.doExpression(n)
   CASE "=", "<", ">", KW_ISLE, KW_ISGE, KW_ISNE, "@",
        "+", "-", "*", "/", KW_AND, KW_OR, "!", KW_SHL, KW_SHR, KW_ASR, KW_XOR
      ->n, o, d := self.doSingleExp(n, ha)
      ->n, o, d := self.mathlogichead(n,o,d,mode,unary,ha)
      n, o, d := self.mathlogichead(n,mode,unary, ha) -> 2.3
   DEFAULT
      IF (t = NIL) AND (n.data = IT_VALUE) -> 2.3
         -> ecxmain/exp_constexp now leaves a "!" in front of exp if float mode
         -> lets just return the it_value and be done. otherwise
         -> mathlogichead would get involved and do unneeded computation..
         o := DV
         d := n.info
         n++
      ELSE
         -> 1.5.3: moved negate into default, not needed for others
         -> mathlogic handles neg too now
         ->n, o, d := self.doSingleExp(n, ha)
         ->IF unary THEN n, o, d := self.mathlogichead(n,o,d,mode,unary,ha)
         IF unary
            n, o, d := self.mathlogichead(n,mode,unary,ha) -> 2.3
         ELSE
            n, o, d := self.doSingleExp(n, ha)
         ENDIF
      ENDIF
   ENDSELECT

   IF o = NIL THEN reportIErr('cmplx_expression/2 : o=NIL')

   IF n.data <> NIL THEN reportIErr(' cplx_exp not completed!')

   IF quote
      self.quotetail(qr1, qr2, o, d, ATEMP)
      o := AREG ; d := ATEMP
   ENDIF


ENDPROC buf[1], o, d



-> 1.5.3: added "ha"
PROC doExpression(n:PTR TO item, ha=NIL:PTR TO hintargs) OF codegen
   DEF o, d
   DEF t, var:PTR TO var

   #ifdef DBG_CODEGEN
   DEBUGF('self.doExpression($\h, $\h)\n', n, ha)
   #endif

   IF FreeStack() < 4000 THEN Raise("STCK") -> 2.0

   n, o, d := self.doSingleExp(n, ha)

   -> 1.8.0
   IF n.data = 10
      g_linenum := n.info
      IF g_linedebug THEN def_line(g_linenum)
      n++
   ENDIF

   #ifdef DBG_CODEGEN
   DEBUGF('self.doExpression() DONE\n',n)
   #endif


ENDPROC n, o, d


-> v31: loop-reg reinstated ! :)
-> v44: loop-reg removed...;]
-> 1.10.0 added support for EXITN
PROC doFor(buf:PTR TO item) OF codegen
   DEF n:PTR TO item
   DEF o, d
   DEF t, r
   DEF for, endfor
   DEF step=1, id, var:PTR TO var
   DEF cond

   n := buf

   #ifdef DBG_CODEGEN
   DEBUGF('dofor($\h)\n', buf)
   #endif

   n++

   for := newLabel()
   endfor := newLabel()

   /* get variable */
   var := n.info

   n++

   /* evaluate expression for the labels startingvalue */
   n, o, d := self.doExpression(n)
   inst_copy(o,d, var.o,var.d)

   def_label(for)

   n, o,d := self.doExpression(n)

   IF n.data = KW_STEP
      n++
      IF n.data <> IT_VALUE THEN reportErr('value expected')
      IF n.num THEN reportErr('illegal STEP value')
      step := n.info
      n++
      IF step = NIL THEN reportErr('illegal STEP value')
   ENDIF

   IF o <> DV
      d := self.copyObtainToDREG(o,d)
      o := DREG
      t := TRUE
   ELSE
      t := FALSE
   ENDIF
   inst_bic(var.o,var.d, IF step > 0 THEN ISLT ELSE ISGT, o, d, endfor)

   IF t THEN self.releaseDREG(d, 0)

   IF n.data = KW_DO
      n++
      n := self.compileCode(n,1) ; n++ ->dostatement2(n)
   ELSE -> can be any number of expressions
      REPEAT
         n, t := self.compileCode(n)
         cond := -1
         IF n.data = KW_EXIT
            cond := IF n.num THEN ISEQ ELSE ISNE
            n++
            n, o,d := self.doExpression(n)
            inst_bic(o,d, cond, DV,NIL, endfor)
         ENDIF
      UNTIL t = NIL
      IF n.data <> KW_ENDFOR THEN reportErr('"ENDFOR" expected')
      n++
   ENDIF

   /* inc/decrease FOR-variable */
   IF step > 0
      inst_incvar(var, step)
   ELSE
      inst_decvar(var, -step)
   ENDIF

   /* branch back */
   inst_golab(for)
   /* here we end up when finnished */
   def_label(endfor)

   #ifdef DBG_CODEGEN
   DEBUGF('dofor DONE\n')
   #endif

ENDPROC n, NIL


/* 000804 */
-> 1.10.0 added support for whilen/exitn
PROC doWhile(n:PTR TO item) OF codegen
   DEF r, as, as2
   DEF while, endwhile
   DEF cond

   cond := IF n.num THEN ISNE ELSE ISEQ

   #ifdef DBG_CODEGEN
   DEBUGF('dowhile($\h), cond=\d\n', n, cond)
   #endif

   n++

   while := newLabel()
   endwhile := newLabel()


   /* define whilelabel */
   def_label(while)


   n, as,as2 := self.doExpression(n)
   inst_bic(as,as2, cond, DV,NIL, endwhile)

   IF n.data = KW_DO
      n++
      /* do ONE expression */
      n := self.compileCode(n,1) ; n++->dostatement2(n)
   ELSE -> can be any number of expressions
      REPEAT
         n, r := self.compileCode(n)
         IF n.data = KW_EXIT
            cond := IF n.num THEN ISEQ ELSE ISNE
            n++
            n, as,as2 := self.doExpression(n)
            inst_bic(as,as2, cond, DV,NIL, endwhile)
         ENDIF
      UNTIL r = NIL
      IF n.data <> KW_ENDWHILE THEN reportErr('"ENDWHILE" expected')
      n++ -> skip ENDWHILE
   ENDIF

   inst_golab(while)
   def_label(endwhile)

   #ifdef DBG_CODEGEN
   DEBUGF('dowhile() DONE\n')
   #endif

ENDPROC n, NIL


-> v43: soooo optimised.
-> v40: now writes directly to g_databuf !!
-> v50: :DOUBLE
-> V50: makes use of the OBJECT litem
-> v56: WIDE
-> v57: now respects g_safeimmlists
PROC doList(buf:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF n:PTR TO litem
   DEF len, size, as, as2
   DEF offset=0
   DEF object=NIL:PTR TO object
   DEF esize, eflags, cmplx
   DEF memb:PTR TO member
   DEF a, new=FALSE
   DEF arx, rwdofs, rwsize, ptr,align
   DEF it_immedlist:PTR TO it_immedlist
   DEF tr
   DEF dl, dh  -> v50 :DOUBLE
   DEF t

   #ifdef DBG_CODEGEN
   DEBUGF('getlist2($\h)\n', buf)
   #endif

   it_immedlist := buf.info
   n := it_immedlist.litems

   len := it_immedlist.numelems
   rwsize := it_immedlist.sizeof
   new := it_immedlist.newed

   esize := it_immedlist.esize
   eflags := it_immedlist.eflags
   cmplx := it_immedlist.cmplx
   IF esize = 0
      object := it_immedlist.object
   ENDIF


   IF new
      SELECT esize
      CASE 0 -> object
         tr := self.newMemory(DV,rwsize)
         arx := self.copyObtainToAREG(DREG,tr)
      CASE 4 -> list/larray
         IF cmplx
            tr := self.newMemory(DV,rwsize+4)
            arx := self.obtainAREG()
            inst_copy(DREG,tr, AREG,arx)
            inst_copy(DV,len, ARXPO,AxSizeOfs(arx,4,0))
            inst_incarx(arx, 4)
         ELSE
            tr := self.newMemory(DV,rwsize)
            arx := self.copyObtainToAREG(DREG,tr)
         ENDIF
      CASE 8 -> double/wide array
         tr := self.newMemory(DV,rwsize)
         arx := self.copyObtainToAREG(DREG,tr)
      CASE 2 -> iarray
         tr := self.newMemory(DV,rwsize)
         arx := self.copyObtainToAREG(DREG,tr)
      CASE 1 -> carray
         tr := self.newMemory(DV,rwsize)
         arx := self.copyObtainToAREG(DREG,tr)
      ENDSELECT
      IF object -> typed object list
         WHILE len > 0
            FOR a := 0 TO object.nrofmembers-1
               EXIT len = 0
               memb := object.membertable[a]
               IF memb.size = 0 THEN reportErr('object is not suitable for typed list', object.name)
               IF n.data = IT_VALUE
                  IF n.info <> NIL
                     inst_copy(DV,n.info,ARXPO, AxSizeOfsF(arx,memb.size,memb.offset + offset, memb.flags))
                  ENDIF
                  n := n.next
               ELSE
                  t, as, as2 := self.doExpression(n)
                  inst_copy(as,as2, ARXPO, AxSizeOfsF(arx,memb.size,memb.offset + offset, memb.flags))
                  n := n.next
               ENDIF
               len--
            ENDFOR
            offset := offset + object.sizeof
         ENDWHILE
      ELSE  -> DARRAY/LARRAY/IARRAY/CARRAY/LIST
         FOR a := 0 TO len-1
            IF n.data = IT_VALUE
               IF n.info <> NIL
                  inst_copy(DV,n.info, ARXPO,AxSizeOfsF(arx,esize,offset,eflags))
               ENDIF
               n := n.next ->n++
            ELSE
               t, as, as2 := self.doExpression(n)
               inst_copy(as,as2, ARXPO,AxSizeOfsF(arx,esize,offset,eflags))
               n := n.next
            ENDIF
            offset := offset + esize
         ENDFOR
      ENDIF
   ELSE -> not NEW

      -> 1.5.4
      IF ha
         IF ha.hregisvar = FALSE
            IF ha.hregop = AREG
               IF ha.hregnum > -1
                  arx := ha.hregnum
                  self.lockAREG(arx)
               ELSE
                  arx := self.obtainAREG()
               ENDIF
            ELSE
               arx := self.obtainAREG()
            ENDIF
         ELSE
            arx := self.obtainAREG()
         ENDIF
      ELSE
         arx := self.obtainAREG()
      ENDIF

      rwdofs := it_immedlist.offset
      inst_getrwd(rwdofs, arx)
      ptr := g_databuf + rwdofs
      IF rwdofs + rwsize > g_databufsize THEN reportIErr('DBUF overflow!')

      SELECT esize
      CASE 0 -> object
         WHILE len > 0
            FOR a := 0 TO object.nrofmembers-1
               EXIT len = 0
               memb := object.membertable[a]
               esize := memb.size
               IF esize = 0 THEN reportErr('object is not suitable for typed list', object.name)
               IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
                  SELECT esize
                  CASE 8 -> 1.8.2, was missing! fixed 2.2.3
                     IF memb.flags AND MEMBF_FLOAT
                        dh, dl := singToDoub(n.info)
                        PutLong(ptr + memb.offset + offset, dh)
                        PutLong(ptr + memb.offset + offset + 4, dl)
                     ELSE -> v56
                        PutLong(ptr + memb.offset + offset + 4, n.info)
                        PutLong(ptr + memb.offset + offset + 4,
                              IF n.info AND $80000000 THEN $FFFFFFFF ELSE NIL)
                     ENDIF
                  CASE 4
                     PutLong(ptr + memb.offset + offset, n.info)
                  CASE 2
                     PutInt(ptr + memb.offset + offset, n.info)
                  CASE 1
                     PutChar(ptr + memb.offset + offset, n.info)
                  ENDSELECT
                  n := n.next
               ELSE
                  t, as, as2 := self.doExpression(n)
                  inst_copy(as,as2, ARXPO,AxSizeOfsF(arx,esize,memb.offset + offset, memb.flags))
                  n := n.next
               ENDIF
               len--
            ENDFOR
            offset := offset + object.sizeof
         ENDWHILE
      CASE 4 -> list/larray
         IF cmplx THEN PutLong(ptr-4, len)
         FOR a := 0 TO len-1
            IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
               PutLong(ptr + offset, n.info)
               n := n.next ->n++
            ELSE
               t, as, as2 := self.doExpression(n)
               inst_copy(as,as2, ARXPO,AxSizeOfs(arx,4,offset))
               n := n.next
            ENDIF
            offset := offset + 4
         ENDFOR
      CASE 8 -> double/wide array  v50, 56
         IF eflags AND MEMBF_FLOAT
            FOR a := 0 TO len-1
               IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
                  dh, dl := singToDoub(n.info)
                  PutLong(ptr + offset, dh)
                  PutLong(ptr + offset + 4, dl)
                  n := n.next
               ELSE
                  t, as, as2 := self.doExpression(n)
                  inst_copy(as,as2, ARXPO,AxSizeOfsF(arx,8,offset,MEMBF_FLOAT))
                  n := n.next
               ENDIF
               offset := offset + 8
            ENDFOR
         ELSE   -> v56
            FOR a := 0 TO len-1
               IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
                  PutLong(ptr + offset + 4, n.info)
                  PutLong(ptr + offset, IF n.info AND $80000000 THEN $FFFFFFFF ELSE NIL)
                  n := n.next
               ELSE
                  t, as, as2 := self.doExpression(n)
                  inst_copy(as,as2, ARXPO,AxSizeOfsF(arx,8,offset,NIL))
                  n := n.next
               ENDIF
               offset := offset + 8
            ENDFOR
         ENDIF
      CASE 2 -> iarray
         FOR a := 0 TO len-1
            IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
               PutInt(ptr + offset, n.info)
               n := n.next->n++
            ELSE
               t, as, as2 := self.doExpression(n)
               inst_copy(as,as2, ARXPO,AxSizeOfs(arx,2,offset))
               n := n.next
            ENDIF
            offset := offset + 2
         ENDFOR
      CASE 1 -> carray
         FOR a := 0 TO len-1
            IF (n.data = IT_VALUE) AND (g_safeimmlists = FALSE)
               PutChar(ptr + offset, n.info)
               n := n.next->n++
            ELSE
               t, as, as2 := self.doExpression(n)
               inst_copy(as,as2, ARXPO,AxSizeOfs(arx,1,offset))
               n := n.next
            ENDIF
            offset := offset + 1
         ENDFOR
      ENDSELECT
   ENDIF

   arx := self.releaseAREG(arx,1)

   #ifdef DBG_CODEGEN
   DEBUGF('getlist2 DONE()\n')
   #endif


ENDPROC buf[1], AREG, arx

-> 1.10.0 added support for IFN
PROC doIfThenExp(buf:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF n:PTR TO item
   DEF as, as2
   DEF ifthen, ifthen2
   DEF ro=NIL, rd
   DEF cond, var:PTR TO var

   #ifdef DBG_CODEGEN
   DEBUGF('doifthenexp($\h)\n', buf)
   #endif

   n := buf.info
   cond := IF n.num THEN ISNE ELSE ISEQ

   #ifdef DBG_CODEGEN
   DEBUGF('doifthenexp($\h), n=$\h, cond=\d\n', buf, n,cond)
   #endif

   n++ -> skip IF

   ifthen := newLabel()

   /* do first exp */
   n, as,as2 := self.doExpression(n)
   inst_bic(as,as2, cond, DV,NIL, ifthen)

   IF n.data <> KW_THEN THEN reportErr('"THEN" expected')

   n++ -> skip THEN

   /* do exp after THEN */

   n, as,as2 := self.doExpression(n)

   IF ha
      ro := ha.hregop
      IF ha.hregnum = -1
         SELECT ro
         CASE DREG ; rd := self.getFreeDREG()->DTEMP
         CASE AREG ; rd := self.getFreeAREG()->ATEMP
         CASE FREG ; rd := self.getFreeFREG()->FTEMP
         CASE D64REG ; rd := self.getFreeD64()->FTEMP
         ENDSELECT
      ELSE
         rd := ha.hregnum
      ENDIF
   ELSE
      ro := DREG
      rd := self.getFreeDREG()->DTEMP
      SELECT as
      CASE FREG
         ro := FREG
         rd := self.getFreeFREG()->FTEMP
      CASE VAR64
         var := as2
         IF var.type.flags AND MEMBF_FLOAT
            ro := FREG
            rd := self.getFreeFREG()->FTEMP
         ELSE
            ro := D64REG
            rd := self.getFreeD64()
         ENDIF
      CASE D64REG
         ro := D64REG
         rd := self.getFreeD64()
      CASE X2R -> 2.3
         ro := D64REG
         rd := self.getFreeD64()
      ENDSELECT
   ENDIF

   inst_copy(as,as2, ro,rd)

   IF n.data = KW_ELSE
      n++

      ifthen2 := newLabel()
      inst_golab(ifthen2)
      def_label(ifthen)

      n, as,as2 := self.doExpression(n)

      inst_copy(as,as2, ro,rd)

      def_label(ifthen2)

   ELSE

      reportErr('"ELSE" expected')

   ENDIF



   #ifdef DBG_CODEGEN
   DEBUGF('doifthenexp DONE()\n')
   #endif

ENDPROC buf[1], ro, rd


-> 1.10.0 addedsupport for IFN
PROC doIfThenStat(n:PTR TO item) OF codegen
   DEF as, as2
   DEF ifthen, ifthen2
   DEF cond

   cond := IF n.num THEN ISNE ELSE ISEQ

   #ifdef DBG_CODEGEN
   DEBUGF('doifthenstat($\h), cond=\d\n', n,cond)
   #endif


   n++ -> skip IF

   ifthen := newLabel()

   /* do first exp */
   n, as,as2 := self.doExpression(n)
   inst_bic(as,as2, cond, DV,NIL, ifthen)

   IF n.data <> KW_THEN THEN reportErr('"THEN" expected')

   n++ -> skip THEN

   /* do exp after THEN */

   n := self.compileCode(n, 1)

   IF n.data = KW_ELSE
      n++

      ifthen2 := newLabel()
      inst_golab(ifthen2)
      def_label(ifthen)

      n := self.compileCode(n, 1) ; n++

      def_label(ifthen2)

   ELSE

      def_label(ifthen)
      n++ -> skip nl v46

   ENDIF



   #ifdef DBG_CODEGEN
   DEBUGF('doifthen DONE()\n')
   #endif

ENDPROC n, NIL, NIL




-> v44. removed LOOP xxx
-> 1.10.0 added support for EXITN
PROC doLoop(buf) OF codegen
   DEF n:PTR TO item, t
   DEF explen, as,as2
   DEF loop, endloop=NIL
   DEF cond

   #ifdef DBG_CODEGEN
   DEBUGF('doloop($\h)\n', buf)
   #endif

   n := buf

   n++

   loop := newLabel()

   def_label(loop)

   IF n.data = KW_DO
      n++
      /* do ONE expression */
      n := self.compileCode(n,1) ; n++->dostatement2(n)
   ELSE
      REPEAT
         n, t := self.compileCode(n)
         IF n.data = KW_EXIT
            cond := IF n.num THEN ISEQ ELSE ISNE
            IF endloop=NIL THEN endloop := newLabel()
            n++
            n, as,as2 := self.doExpression(n)
            inst_bic(as,as2, cond, DV,NIL, endloop)
         ENDIF
      UNTIL t = NIL
      IF n.data <> KW_ENDLOOP THEN reportErr('"ENDLOOP" expected')
      n++
   ENDIF

   inst_golab(loop)
   IF endloop THEN def_label(endloop)


   #ifdef DBG_CODEGEN
   DEBUGF('doloop DONE\n')
   #endif

ENDPROC n, NIL

-> 1.10.0 added support for untiln/exitn
PROC doRepeat(n:PTR TO item) OF codegen
   DEF t
   DEF as,as2
   DEF repeat, endrepeat=NIL
   DEF cond

   #ifdef DBG_CODEGEN
   DEBUGF('dorepeat($\h)\n', n)
   #endif

   n++

   repeat := newLabel()


   /* define repetlabel */
   def_label(repeat)

   REPEAT
      n, t := self.compileCode(n)
      IF n.data = KW_EXIT
         cond := IF n.num THEN ISEQ ELSE ISNE
         n++
         n, as,as2 := self.doExpression(n)
         IF endrepeat = NIL THEN endrepeat := newLabel()
         inst_bic(as,as2, cond, DV,NIL, endrepeat)
      ENDIF
   UNTIL t = NIL

   IF n.data = KW_UNTIL
      cond := IF n.num THEN ISNE ELSE ISEQ
   ELSE
      reportErr('"UNTIL[N]" expected')
   ENDIF

   n++

   n, as,as2 := self.doExpression(n)

   inst_bic(as,as2, cond, DV,NIL, repeat)

   /* for EXIT */
   IF endrepeat THEN def_label(endrepeat)

   #ifdef DBG_CODEGEN
   DEBUGF('dorepeat DONE\n')
   #endif

ENDPROC n, NIL, NIL

-> v55: 64bit support.
-> v44. no more optireg. now uses IREG0 instead !
-> no functions allowed as CASE exp.
-> v48. FIX: CASE func() now works.
-> v49, opbtains register nnow
PROC doSelect(buf:PTR TO item) OF codegen
   DEF n:PTR TO item
   DEF as, as2
   DEF selectend, nextcase, nextcasecontent
   DEF t:PTR TO item
   DEF selo -> v55
   DEF selreg -> v48
   DEF mode -> v55

   #ifdef DBG_CODEGEN
   DEBUGF('doselect($\h)\n', buf)
   #endif

   n := buf
   n++

   selectend := newLabel()

   n, as,as2 := self.doExpression(n)
   SELECT NROFOPS OF as
   CASE VAR64, D64, X2R
      mode := MODE_D64
      selo := D64REG
      selreg := self.copyObtainToD64(as,as2)
      ->self.releaseD64(selreg,0)
   DEFAULT
      mode := MODE_DEFAULT
      selo := DREG
      selreg := self.copyObtainToDREG(as,as2)
      self.releaseDREG(selreg,0)
   ENDSELECT



   /* do cases */
   WHILE n.data = KW_CASE
      n++
      nextcase := newLabel()
      nextcasecontent := newLabel()

      IF mode = MODE_DEFAULT
         self.copyObtainToDREG(selo,selreg) -> v48,v49: results in no-op but we get the obtain
      ELSE
         ->self.copyObtainToD64(selo,selreg)
      ENDIF

sel_case_123:

      n, as, as2 := self.doExpression(n)
      /* 001111 : support for CASE 1, 2, 3,... */
      IF n.data = "," -> 1.8.0 change, now requires commaseparation
         n++
         inst_bic(as,as2, ISEQ, selo,selreg, nextcasecontent)
         JUMP sel_case_123
      ELSE
         inst_bic(as,as2, ISNE, selo,selreg, nextcase)
      ENDIF

      IF mode = MODE_DEFAULT
         self.releaseDREG(selreg,0) -> v48
      ELSE
         ->self.releaseD64(selreg,0)
      ENDIF

      def_label(nextcasecontent)

      /* do stuff in CASE */
      n := self.compileCode(n)

      inst_golab(selectend)

      /* def label for next case */

      def_label(nextcase)

   ENDWHILE

   IF mode = MODE_D64 THEN self.releaseD64(selreg,0)

   IF n.data = KW_DEFAULT
      n++
      n := self.compileCode(n)
   ENDIF

   IF n.data <> KW_ENDSELECT THEN reportErr('"ENDSELECT" expected')
   n++
   /* put endselectlabel */

   def_label(selectend)


   #ifdef DBG_CODEGEN
   DEBUGF('doselect DONE()\n')
   #endif

ENDPROC n, NIL,NIL

-> v45: now uses non-reloc approach! :)
-> 1.5.4: now has a SELECT-ENDSELECT range of 256K!
-> v58 now uses real 32bit relocs..  so no more limits! execp 1024 entries table.
PROC doSelectOf(buf) OF codegen
   DEF n:PTR TO item
   DEF as:LONG, as2
   DEF casetable[256]:ARRAY OF LONG
   DEF end, default:PTR TO genlab, table:PTR TO genlab, code:PTR TO genlab
   DEF t:PTR TO item
   DEF ofval, a, b, exit
   DEF arx
   DEF default_stub:PTR TO genlab
   DEF oreg

   n := buf
   n++

   #ifdef DBG_CODEGEN
   DEBUGF('selectof($\h)\n', buf)
   #endif

   -> nr of adresses in table
   IF n.data <> IT_VALUE THEN reportErr('value expected for SELECT OF')
   IF n.num THEN reportErr('illegal value for SELECT OF')
   ofval := n.info
   n++
   IF ofval > 1024 THEN reportErr('too big value for SELECT OF')
   IF ofval <= 0 THEN reportErr('too big value for SELECT OF')

   -> get some initial labels
   table := newLabel()
   default := newLabel()
   end := newLabel()
   default_stub := newLabel()

   n++ -> skip OF

   -> initialise casetable
   FOR a := 0 TO ofval-1 DO casetable[a] := default

   -> get expression to select in
   n, as, as2 := self.doExpression(n)
   oreg := self.copyObtainToDREG(as,as2)
   self.releaseDREG(oreg,0)

   -> bound check expression
   inst_bic(DV,NIL, ISLT, DREG,oreg, default_stub)
   inst_bic(DV,ofval-1, ISGT, DREG,oreg, default_stub)

   -> get table adr
   inst_labadr(table, ATEMP)

   -> get code ptr
   inst_copy(ARXPX,AxSizeIdrxScaleF(ATEMP,4,oreg,4,NIL), AREG,ATEMP)

   -> jump
   inst_goarx(ATEMP)

   -> we jump here to trampoline ourselves away
   -> with a full range BRA.L / B instruction
   -> 1.5.4
   def_label(default_stub)
   inst_golab(default)


   -> do cases

   WHILE n.data = KW_CASE
      n++
      code := newLabel()
      exit := FALSE
      REPEAT
         IF n.data = "," THEN n++ -> 1.8.0 change
         IF n[1].data = KW_TO
            IF n.data <> IT_VALUE THEN reportErr('value expected')
            a := n.info
            n++
            IF a >= ofval THEN reportErr('incorrect size of value')
            IF a < 0 THEN reportErr('incorrect size of value')
            IF casetable[a] <> default THEN reportErr('value declared twice')
            casetable[a] := code
            n++ -> skip TO
            IF n.data <> IT_VALUE THEN reportErr('value expected')
            b := n.info
            IF b <= a THEN reportErr('incorrect size of value')
            IF b >= ofval THEN reportErr('incorrect size of value')
            WHILE a < b
               a++
               IF casetable[a] <> default THEN reportErr('value declared twice')
               casetable[a] := code
            ENDWHILE
            n++
         ELSEIF n.data = IT_VALUE
            a := n.info
            IF a >= ofval THEN reportErr('incorrect size of value')
            IF a < 0 THEN reportErr('incorrect size of value')
            IF casetable[a] <> default THEN reportErr('value declared twice')
            casetable[a] := code
            n++
         ELSEIF n.data = 10
            exit := TRUE
         ELSE
            reportErr('CASE syntax')
         ENDIF
      UNTIL exit
      g_linenum := n.info
      IF g_linedebug THEN def_line(g_linenum)
      n++ -> skip 10
      putAlign(4)
      def_label(code)

      n := self.compileCode(n)
      inst_golab(end)
   ENDWHILE

   putAlign(4)
   def_label(default)

   IF n.data = KW_DEFAULT
      n++
      n := self.compileCode(n)
   ENDIF

   IF n.data <> KW_ENDSELECT THEN reportErr('"ENDSELECT" expected')
   n++

   inst_golab(end) -> jump over table

   putAlign(4)
   def_label(table)
   -> allocate codetable
   FOR a := 0 TO ofval-1
      code := casetable[a]
      putReloc(code)
   ENDFOR

   def_label(end)

   #ifdef DBG_CODEGEN
   DEBUGF('selectof DONE\n')
   #endif

ENDPROC n, NIL, NIL

-> 1.10.0 added support for IFN, ELSEIFN
PROC doIfEndif(n:PTR TO item) OF codegen
   DEF else, end
   DEF exit, as, as2
   DEF t:PTR TO item
   DEF cond

   cond := IF n.num THEN ISNE ELSE ISEQ

   #ifdef DBG_CODEGEN
   DEBUGF('doifendif($\h), cond=\d\n', n, cond)
   #endif

   n++

   end := newLabel()
   else := newLabel()

   n, as, as2 := self.doExpression(n)
   inst_bic(as,as2, cond, DV,NIL, else)

   /* do stuff in IF */
   n := self.compileCode(n)


   WHILE n.data = KW_ELSEIF
      cond := IF n.num THEN ISNE ELSE ISEQ
      n++

      inst_golab(end)

      def_label(else)

      else := newLabel()

      n, as,as2 := self.doExpression(n)
      inst_bic(as,as2, cond, DV,NIL, else)


      /* do stuff in ELSEIF */
      n := self.compileCode(n)


   ENDWHILE

   IF n.data = KW_ELSE
      n++

      inst_golab(end)

      def_label(else)

      /* do stuff in ELSE */
      n := self.compileCode(n)

   ELSE
      def_label(else)
   ENDIF

   IF n.data <> KW_ENDIF THEN reportErr('"ENDIF" expected')
   n++

   def_label(end)

   #ifdef DBG_CODEGEN
   DEBUGF('leaving IF-ENDIF\n')
   #endif

ENDPROC n, NIL,NIL

-> v44. no more relocs or crap.
-> v45. hehe, relocs for 68k strings :)
PROC doString(n:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF areg

   IF ha
      IF ha.hregop = AREG
         IF ha.hregnum > -1
            areg := ha.hregnum
         ELSE
            areg := self.getFreeAREG()->ATEMP
         ENDIF
      ELSE
        areg := self.getFreeAREG()->ATEMP
      ENDIF
   ELSE
      areg := self.getFreeAREG()->ATEMP
   ENDIF

   #ifdef DBG_CODEGEN
   DEBUGF('getstring2(n=$\h ha=$\h): \a\s\a => areg:\d\n', n, ha, n.info, areg)
   #endif

   inst_getimmstr(n.info, areg)


ENDPROC n[1], AREG, areg

PROC doNewString(n:PTR TO item, ha:PTR TO hintargs) OF codegen
   DEF str:PTR TO CHAR
   DEF a
   DEF strlen, size
   DEF drx
   DEF arx

   IF ha
      IF ha.hregop = AREG
         IF ha.hregnum > -1
            arx := ha.hregnum
         ELSE
            arx := self.getFreeAREG()->ATEMP
         ENDIF
      ELSE
         arx := self.getFreeAREG()->ATEMP
      ENDIF
   ELSE
      arx := self.getFreeAREG()->ATEMP
   ENDIF


   #ifdef DBG_CODEGEN
   DEBUGF('getnewstring($\h)\n', n)
   #endif

   str := n.info
   strlen := EstrLen(str) + 1

   drx := self.newMemory(DV,strlen)
   inst_copy(DREG,drx, AREG,arx)
   FOR a := 0 TO strlen-1 STEP 4
      inst_copy(DV,Long(str+a), DREG,DTEMP)
      inst_copy(DREG,DTEMP, ARXPO,AxSizeOfs(arx,4,a))
   ENDFOR


   #ifdef DBG_CODEGEN
   DEBUGF('getnewstring DONE\n')
   #endif

ENDPROC n[1], AREG, arx


-> v49
PROC doMAssign(n:PTR TO item) OF codegen
   DEF var:PTR TO var, var2:PTR TO var, var3=NIL:PTR TO var, o, d
   DEF var4=NIL:PTR TO var

   #ifdef DBG_CODEGEN
   DEBUGF('domassign($\h)\n', n)
   #endif

   var := n.info
   n++
   var2 := n.info
   n++
   IF n.data = IT_VARIABLE
      var3 := n.info
      n++
   ENDIF
   IF n.data = IT_VARIABLE
      var4 := n.info
      n++
   ENDIF
   n++ -> skip KW_ASSIGN
   n, o, d := self.doExpression(n)

   IF g_multireturn = NIL THEN reportErr('multiple assign error')

   #ifdef DBG_CODEGEN
   DEBUGF('domassign: multireturn= [\d,\d] [\d,\d] [\d,\d] [\d,\d]\n',
   g_multireturn.ros[0],g_multireturn.rds[0],
   g_multireturn.ros[1],g_multireturn.rds[1],
   g_multireturn.ros[2],g_multireturn.rds[2],
   g_multireturn.ros[3],g_multireturn.rds[3])
   #endif

   inst_copy(o,d, var.o, var.d)
   inst_copy(g_multireturn.ros[1],g_multireturn.rds[1], var2.o, var2.d)
   IF var3 THEN inst_copy(g_multireturn.ros[2],g_multireturn.rds[2], var3.o, var3.d)
   IF var4 THEN inst_copy(g_multireturn.ros[3],g_multireturn.rds[3], var4.o, var4.d)

ENDPROC n, NIL, NIL


-> v44. now requires varexp !
-> v45: now does assignment exp directly if used..
-> v45. trying to do first-level dereference in here now = better code.
-> v49. support for vector_of_x[] UNDER CONSTRUCTION
-> v49. moved mult assign out of here, into domassign()
-> v58, now handles calling codegen.doMethod()
PROC doVarstuff(buf:PTR TO item, nodest, ha:PTR TO hintargs) OF codegen
   DEF n:PTR TO item
   DEF o=NIL, d
   DEF ptrsize=NIL, t
   DEF hln:PTR TO hln
   DEF drx, arx=NIL
   DEF sarx
   DEF var:PTR TO var->, var2:PTR TO var
   DEF art
   DEF postkey=NIL, eo=NIL, ed, da:derefargs
   DEF eha:hintargs -> hintargs for := exp
   DEF dha:hintargs -> hintargs for "." / "["
   DEF it_varexp:PTR TO it_varexp


   #ifdef DBG_CODEGEN
   DEBUGF('dovarstuff($\h,\d, $\h) ', buf, nodest, ha)
   #endif


   it_varexp := buf.info

   n := it_varexp + SIZEOF it_varexp

   postkey := it_varexp.postkey

   var := it_varexp.var
   #ifdef DBG_CODEGEN
   DEBUGF('\s ', var.hln.name)
   #endif


   t := n.data

   eha.hregop := type2reg(var.type)
   eha.hregnum := -1
   eha.hregisvar := FALSE

   IF (t = ".") OR (t = "[")
      art := preDeref(var.type,
                        n,
                        postkey,
                        it_varexp.assignmod,
                        var.hln.name) -> v50
      dha.hregisvar := FALSE
      IF ha
         dha.hregop := ha.hregop
         IF ha.hregnum <> -1
            dha.hregnum := ha.hregnum
         ELSE
            dha.hregnum := -1
         ENDIF
      ELSE
         dha.hregop := art
         dha.hregnum := -1
      ENDIF
      eha.hregisvar := FALSE
      eha.hregop := art
      eha.hregnum := -1
   ELSEIF t = KW_PLUSPLUS   -> var++
      art := eha.hregop
   ELSEIF t = KW_MINMIN     -> var--

   ELSEIF postkey = KW_NEW  -> NEW var

   ELSEIF postkey = KW_END  -> END var

   ELSE -> var
      IF it_varexp.assignmod = KW_ASSIGN -> var := ?

         IF var.type.size = 0 THEN reportErr('cannot assign to array', var.hln.name) -> 1.5.5
         IF var.type.numes THEN reportErr('cannot assign to array', var.hln.name) -> 1.5.5

         IF var.o = DREG
            eha.hregnum := var.d
            eha.hregisvar := TRUE
         ELSEIF var.o = FREG
            eha.hregnum := var.d
            eha.hregisvar := TRUE
         ELSE
            eha.hregnum := -1
            eha.hregisvar := FALSE
         ENDIF
         art := NIL

      ELSEIF it_varexp.assignmod -> modification ?

         IF var.type.size = 0 THEN reportErr('cannot modify array', var.hln.name) -> 1.5.5
         IF var.type.numes <> 0 THEN reportErr('cannot modify array', var.hln.name) -> 1.5.5

         art := NIL

      ENDIF
   ENDIF

   IF it_varexp.assignmod -> assign used ?
      t, eo, ed := self.doExpression(n[it_varexp.index], eha)
      n[it_varexp.index].data := NIL -> rem it out
      IF (eo = IT_VALUE) OR (eo = IT_VARIABLE)   -> 2.2
         IF art = NIL
            IF it_varexp.assignmod = KW_ASSIGN
               inst_copy(eo, ed, var.o, var.d)
               o := var.o ; d := var.d
            ELSE
               da.eo := eo
               da.ed := ed
               da.am := it_varexp.assignmod
               o, d := self.dvs_modify(da, var.o,var.d, var.type)
            ENDIF
            eo := NIL
            ed := NIL
         ENDIF
      ELSE /* end of 2.2 improvement */
         SELECT art
         CASE NIL
            IF it_varexp.assignmod = KW_ASSIGN
               inst_copy(eo, ed, var.o, var.d)
               o := var.o ; d := var.d
            ELSE
               da.eo := eo
               da.ed := ed
               da.am := it_varexp.assignmod
               o, d := self.dvs_modify(da, var.o,var.d, var.type)
            ENDIF
            eo := NIL
            ed := NIL
         CASE DREG
            ed := self.copyObtainToDREG(eo,ed)
            eo := DREG
         CASE D64REG
           ed := self.copyObtainToD64(eo,ed)
           eo := D64REG
         CASE AREG
            ed := self.copyObtainToAREG(eo,ed)
            eo := AREG
         CASE FREG
            ed := self.copyObtainToFREG(eo,ed)
            eo := FREG
         CASE VREG
            ed := self.copyObtainToVREG(eo,ed)
            eo := VREG
         ENDSELECT
      ENDIF
   ENDIF

   t := n.data
   IF t = "."
      #ifdef DBG_CODEGEN
      DEBUGF('. \n')
      #endif
      n++
      IF n.data = IT_PFUNC -> method ?
         n, o, d := self.doMethod(n, var.o,var.d, postkey, var.type)
      ELSE
         da.pk := postkey
         da.eo := eo
         da.ed := ed
         da.nd := nodest
         da.am := it_varexp.assignmod
         IF var.type.size = 0
            arx := self.obtainAREG()
            n, o, d := self.dvs_member(var.type, var.breg, var.offset, n, arx, da,dha)
         ELSE
            sarx := self.copyObtainToAREG(var.o, var.d) -> v48
            arx := self.obtainAREG()
            n, o, d := self.dvs_member(var.type, sarx, 0, n, arx, da, dha)
         ENDIF
         IF (o = AREG) AND (d = arx)
            d := self.releaseAREG(arx,1)
         ELSE
            self.releaseAREG(arx,0)
         ENDIF
         IF var.type.size <> 0 THEN self.releaseAREG(sarx,0)
      ENDIF
      IF n.data <> NIL THEN reportIErr(' vexp. not completed')
   ELSEIF t = "["
      #ifdef DBG_CODEGEN
      DEBUGF('[ \n')
      #endif
      n++
      da.pk := postkey
      da.eo := eo
      da.ed := ed
      da.nd := nodest
      da.am := it_varexp.assignmod
      sarx := self.copyObtainToAREG(var.o,var.d)
      n, o, d := self.dvs_index(n, var.type, sarx, var.o, var.d, da,dha)
      IF (o = AREG) AND (d = sarx)
         d := self.releaseAREG(sarx,1)
      ELSE
         self.releaseAREG(sarx,0)
      ENDIF
      IF n.data <> NIL THEN reportIErr(' vexp[ not completed')
   ELSEIF postkey = KW_NEW
      IF var.type.numes THEN reportErr('cannot NEW this', var.hln.name) -> back 1.8.0
      IF var.type.esize = 255 -> OPTR
         IF var.type.object.nrofmethods
            -> 2.0, err if new() exists but wasnt called
            hln := getLabelHLN('new')
            IF findMethod(var.type.object, hln.name)
               addWarning('new() constructor is not used at NEW')
            ENDIF
            arx := self.newClassObject(var.type.object)
            inst_copy(AREG, arx, var.o, var.d)
         ELSE
            drx := self.newMemory(DV,var.type.object.sizeof)
            inst_copy(DREG,drx, var.o,var.d)
         ENDIF
      ELSE
         drx := self.newMemory(DV,var.type.esize) -> LONG/CPTR/IPTR/LPTR
         inst_copy(DREG,drx, var.o,var.d)
      ENDIF
      o := var.o ; d := var.d
   ELSEIF postkey = KW_END
      IF var.type.numes THEN reportErr('cannot END this', var.hln.name) -> back 1.8.0
      IF var.type.esize = 255 -> OPTR
         IF var.type.object.nrofmethods
            self.endClassObject(var.type.object, var.o, var.d)
         ELSE
            self.endMemory(DV,var.type.object.sizeof, var.o, var.d)
         ENDIF
      ELSE
         self.endMemory(DV,var.type.esize, var.o, var.d) -> LONG/CPTR/IPTR/LPTR
      ENDIF
      o := NIL ; d := NIL
   ELSEIF t = KW_PLUSPLUS
      #ifdef DBG_CODEGEN
      DEBUGF('++ \n')
      #endif
      n++
      ptrsize := var.type.esize
      IF ptrsize = 255 THEN ptrsize := var.type.object.sizeof
      IF ptrsize = 0 THEN reportErr('++/-- syntax')
      IF nodest = FALSE
         IF eo
            IF var.type.size = 8 -> 2.1
               inst_copy(eo,ed, D64REG,D64TEMP)
               inst_incvar(var, ptrsize)
               o := D64REG ; d := D64TEMP
            ELSE
               inst_copy(eo,ed, DREG,DTEMP)
               inst_addreg(DV,ptrsize, DTEMP)
               inst_copy(DREG,DTEMP, var.o,var.d)
               inst_subreg(DV,ptrsize, DTEMP)
               o := DREG ; d := DTEMP
            ENDIF
         ELSE
            IF var.type.size = 8 -> 2.1
               inst_copy(var.o,var.d, D64REG,D64TEMP)
               inst_incvar(var, ptrsize)
               o := D64REG ; d := D64TEMP
            ELSE
               inst_copy(var.o,var.d, DREG,DTEMP)
               inst_addreg(DV,ptrsize, DTEMP)
               inst_copy(DREG,DTEMP, var.o,var.d)
               inst_subreg(DV,ptrsize, DTEMP)
               o := DREG ; d := DTEMP
            ENDIF
         ENDIF
      ELSE
         inst_incvar(var, ptrsize)
         o:= var.o ; d := var.d
      ENDIF
   ELSEIF t = KW_MINMIN
      #ifdef DBG_CODEGEN
      DEBUGF('-- \n')
      #endif
      n++
      ptrsize := var.type.esize
      IF ptrsize = 255 THEN ptrsize := var.type.object.sizeof
      IF ptrsize THEN inst_decvar(var, ptrsize) ELSE reportErr('++/-- syntax')
      o := var.o ; d := var.d
   ELSEIF t = NIL
      -> nada  assign already made
   ELSE
      reportErr('varexp syntax')
   ENDIF

   SELECT eo
   CASE NIL
   CASE IT_VALUE
   CASE IT_VARIABLE
   CASE DREG ; self.releaseDREG(ed,0)
   CASE AREG ; self.releaseAREG(ed,0)
   CASE FREG ; self.releaseFREG(ed,0)
   CASE VREG ; self.releaseVREG(ed,0)
   CASE D64REG ; self.releaseD64(ed,0)
   ENDSELECT


ENDPROC buf[1], o, d



-> v44. no scratch source !
-> "arx" is numer of reg that dovarstuff() obtained for us.
-> the ssolution is to go back to some old idea.. copy to areg, when
-> dereferencing continiues.
-> v48. now takes one more arg "safereturn"
-> this arg is a bool. if TRUE, we must return result in non obtainable scratch register..
-> this is to avoid problems when spilled registers get restored by our caller.
-> TODO: Methods can not return in float register.. *FIXED*
-> TODO: move methodstuff into a separate procedure.  *DONE*
-> 1.5.3 added "sofs" source offset, always 0 for non arrays, else offfset of array
-> relative to "sarx".
-> 1.5.3: added "ha" arg. we now ALWAYS return result in a register !
-> either we return in supplied register (ha) or we return in DTEMP/ATEMP/FTEMP
PROC dvs_member(type:PTR TO member, sarx, sofs, buf:PTR TO item, arx,
                                 da:PTR TO derefargs, ha:PTR TO hintargs) OF codegen
   DEF n:PTR TO item
   DEF memb=NIL:PTR TO member
   DEF inc=FALSE, dec=FALSE
   DEF follow=NIL
   DEF t:PTR TO item
   DEF idsize=0
   DEF as, as2
   DEF x
   DEF hln:PTR TO hln


    #ifdef DBG_CODEGEN
   DEBUGF('dvs_member(<\d,\d,\d,$\h>,\d,$\h,$\h,\d,(\d,$\h)\n', type.size,type.esize,type.numes,type.object,sarx,buf,arx,da.pk,da.eo,da.ed)
   #endif

   n := buf


   -> member eh ?

   memb := n.info

   n++

   -> what follows member ?
   t := n.data
   SELECT t
   CASE KW_PLUSPLUS
      n++
      inc:=TRUE  -> we`ll take care of increment later
   CASE KW_MINMIN
      n++
      dec := TRUE -> we`ll take care of decrement later
   CASE "."
      n++
      follow := "."
   CASE "["
      n++
      follow := "["
   ENDSELECT

   IF inc OR dec
      idsize := memb.esize
      IF idsize = 0 THEN idsize := 1 -> CHAR/INT/LONG
   ENDIF

   -> nilcheck ?
   IF g_nilcheck<>NIL
      IF sofs = 0
         IF da.pk=NIL
            self.nilCheck(sarx, g_linenum)
         ENDIF
      ENDIF
   ENDIF

   IF dec
      IF memb.size < 8
         inst_copy(ARXPO, AxSizeOfsF(sarx, memb.size, memb.offset + sofs, memb.flags), DREG,DTEMP)
         inst_subreg(DV, IF idsize = 255 THEN memb.object.sizeof ELSE idsize, DTEMP)
         inst_copy(DREG,DTEMP, ARXPO, AxSizeOfsF(sarx, memb.size, memb.offset + sofs, memb.flags))
      ELSE -> 2.1
         inst_copy(ARXPO, AxSizeOfsF(sarx, 8, memb.offset + sofs, memb.flags), D64REG,D64TEMP)
         inst_subd64(DV, IF idsize = 255 THEN memb.object.sizeof ELSE idsize, D64TEMP)
         inst_copy(D64REG,D64TEMP, ARXPO, AxSizeOfsF(sarx, 8, memb.offset + sofs, memb.flags))
      ENDIF
   ENDIF

   IF follow = "["
      arx := self.obtainAREG()
      inst_copy(ARXPO,AxSizeOfsF(sarx, memb.size, memb.offset + sofs, memb.flags), AREG,arx)
      n, as, as2 := self.dvs_index(n,
                              memb,
                              arx,
                              ARXPO,
                              AxSizeOfsF(sarx, memb.size, memb.offset + sofs, memb.flags),
                              da, ha)
      IF (as = AREG) AND (as2 = arx)
         as2 := self.releaseAREG(arx,1)
      ELSE
         self.releaseAREG(arx,0)
      ENDIF
   ELSEIF follow = "."
      IF n.data = IT_PFUNC -> method ?
         IF memb.size <> PTRSIZE THEN reportErr('this member can not have methods', memb.name) -> 2.0
         n, as, as2 := self.doMethod(n, ARXPO,AxSizeOfsF(sarx,memb.size,memb.offset + sofs,memb.flags), da.pk, memb)
         -> v58, let dovarstuff() handle it
         ->inst_copy(ARXPO,AxSizeOfsF(sarx,memb.size,memb.offset + sofs,memb.flags), AREG,arx)
      ELSEIF memb.size = 0
         n, as, as2 := self.dvs_member(memb, sarx, memb.offset + sofs, n, arx, da, ha)
      ELSE
         inst_copy(ARXPO,AxSizeOfsF(sarx,memb.size,memb.offset + sofs,memb.flags), AREG,arx)
         n, as, as2 := self.dvs_member(memb, arx, 0, n, arx, da, ha)
      ENDIF
   ELSEIF da.am -> was: da.eo
      IF dec THEN reportErr('this does not make sense')
      IF da.am = KW_ASSIGN
         inst_copy(da.eo,da.ed, ARXPO, AxSizeOfs(sarx, memb.size, memb.offset + sofs))
         as := da.eo
         as2 := da.ed
      ELSE -> 1.5.4
         as, as2 := self.dvs_modify(da, ARXPO,
                               AxSizeOfsF(sarx,memb.size,memb.offset+sofs,memb.flags),
                               memb)
      ENDIF
   ELSEIF da.pk = KW_NEW
      IF inc OR dec THEN reportErr('this does not make sense')
      IF memb.esize = 255 -> OPTR ?
         IF memb.object.nrofmethods
            -> 2.0, warn if new() exists but wasnt called
            hln := getLabelHLN('new')
            IF findMethod(memb.object, hln.name)
               addWarning('new() constructor is not used at NEW')
            ENDIF
            as2 := self.newClassObject(memb.object)
            inst_copy(AREG,as2, ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
            as := AREG
         ELSE
            as2 := self.newMemory(DV,memb.object.sizeof)
            inst_copy(DREG,as2, ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
            as := DREG
         ENDIF
      ELSE
         as2 := self.newMemory(DV,idsize)
         inst_copy(DREG,as2, ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
         as := DREG
      ENDIF
   ELSEIF da.pk = KW_END
      IF inc OR dec THEN reportErr('this does not make sense')
      IF memb.esize = 255 -> OPTR ?
         IF memb.object.nrofmethods
            self.endClassObject(memb.object, ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
         ELSE
            self.endMemory(DV,memb.object.sizeof,
                  ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
         ENDIF
         as := NIL
      ELSE
         /* end automatically sets dest to NIL */
         self.endMemory(DV,idsize, ARXPO,AxSizeOfs(sarx, 4, memb.offset + sofs))
         as := NIL
      ENDIF
   ELSE

      as := ha.hregop
      IF ha.hregnum <> -1
         as2 := ha.hregnum
      ELSE
         IF ha.hregop = DREG
            as2 := self.getFreeDREG()->DTEMP
         ELSEIF ha.hregop = AREG
            as2 := self.getFreeAREG()->ATEMP
         ELSEIF ha.hregop = FREG
            as2 := self.getFreeFREG()->FTEMP
         ELSEIF ha.hregop = D64REG
            as2 := self.getFreeD64()
         ELSE
            reportIErr('dvs_member/exit wrong ha.hregop')
         ENDIF
      ENDIF

      IF (da.nd = FALSE) OR inc -> 2.3 fix
         inst_copy(ARXPO,AxSizeOfsF(sarx, memb.size, memb.offset + sofs, memb.flags), as, as2)
      ELSE
         as := NIL
      ENDIF

      IF inc
         IF memb.size < 8
            arx := DTEMP
            inst_copy(as,as2, DREG, arx)
            inst_addreg(DV,IF idsize = 255 THEN memb.object.sizeof ELSE idsize, arx)
            inst_copy(DREG,arx, ARXPO, AxSizeOfs(sarx, memb.size, memb.offset + sofs))
            IF da.nd = FALSE -> 1.5.3
               inst_subreg(DV, IF idsize = 255 THEN memb.object.sizeof ELSE idsize, arx)
            ENDIF
            as2 := DTEMP
            as := DREG
         ELSE -> 2.1
            arx := D64TEMP
            inst_copy(as,as2, D64REG, arx)
            inst_addd64(DV,IF idsize = 255 THEN memb.object.sizeof ELSE idsize, arx)
            inst_copy(D64REG,arx, ARXPO, AxSizeOfs(sarx, memb.size, memb.offset + sofs))
            IF da.nd = FALSE -> 1.5.3
               inst_subd64(DV, IF idsize = 255 THEN memb.object.sizeof ELSE idsize, arx)
            ENDIF
            as2 := D64TEMP
            as := D64REG
         ENDIF
      ENDIF

   ENDIF


ENDPROC n, as, as2

-> 1.5.4
-> note: always put result in DTEMP/FTEMP because haregnum might be same as deref reg!
PROC dvs_modify(da:PTR TO derefargs, dvso, dvsd, memb:PTR TO member) OF codegen
   DEF o, d, m, eo, ed

   #ifdef DBG_CODEGEN
   DEBUGF('dvs_modify([pk:\d,eo:\d,ed:$\h,nd:\d,am:\d], \d, $\h, $\h)\n',
   da.pk,da.eo,da.ed,da.nd,da.am)
   #endif

   m := da.am
   IF memb.flags AND MEMBF_FLOAT
      o := FREG
      d := FTEMP
      inst_copy(dvso,dvsd, o,d)
      -> make sure expression is in right type of reg
      ed := self.copyObtainToFREG(da.eo,da.ed)
      eo := FREG
      SELECT m
      CASE "+"     ; inst_faddreg(eo,ed,d)
      CASE "-"     ; inst_fsubreg(eo,ed,d)
      CASE "*"     ; inst_fmulreg(eo,ed,d)
      CASE "/"     ; inst_fdivreg(eo,ed,d)
      DEFAULT      ; reportErr('illegal modification') -> AND,OR,SHL,SHR
      ENDSELECT
      self.releaseFREG(ed,0)
   ELSEIF memb.size = 8 -> 1.10.0
      o := D64REG
      d := D64TEMP
      inst_copy(dvso,dvsd, o,d)
      -> make sure expression is in right type of reg
      SELECT m
      CASE "+"
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_addd64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE "-"
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_subd64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE "*"
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_muld64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE "/"
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_divd64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE KW_AND
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_andd64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE KW_OR
         ed := self.copyObtainToD64(da.eo,da.ed)
         eo := D64REG
         inst_ord64(eo,ed,d)
         self.releaseD64(ed,0)
      CASE KW_SHR
         ed := self.copyObtainToDREG(da.eo,da.ed) -> ! DREG
         eo := DREG
         inst_shrd64(eo,ed,d)
         self.releaseDREG(ed,0)
      CASE KW_SHL
         ed := self.copyObtainToDREG(da.eo,da.ed) -> ! DREG
         eo := DREG
         inst_shld64(eo,ed,d)
         self.releaseDREG(ed,0)
      ENDSELECT
   ELSE
      o := DREG
      d := DTEMP
      inst_copy(dvso,dvsd, o,d)
      IF da.eo = DV
         eo := DV
         ed := da.ed
      ELSE
         -> make sure expression is in right type of reg
         ed := self.copyObtainToDREG(da.eo,da.ed)
         eo := DREG
      ENDIF
      SELECT m
      CASE "+"     ; inst_addreg(eo,ed,d)
      CASE "-"     ; inst_subreg(eo,ed,d)
      CASE "*"     ; inst_mulreg(eo,ed,d)
      CASE "/"     ; inst_divreg(eo,ed,d)
      CASE KW_AND  ; inst_andreg(eo,ed,d)
      CASE KW_OR   ; inst_orreg(eo,ed,d)
      CASE KW_SHR  ; inst_shrreg(eo,ed,d)
      CASE KW_SHL  ; inst_shlreg(eo,ed,d)
      CASE KW_ASR  ; inst_asrreg(eo,ed,d)
      ENDSELECT
      IF eo = DREG THEN self.releaseDREG(ed,0)
   ENDIF
   inst_copy(o,d, dvso,dvsd)

   #ifdef DBG_CODEGEN
   DEBUGF('dvs_modify() DONE\n')
   #endif

ENDPROC o, d


-> v50, underconstruction
-> run this before dvs_member, dvs_index
-> returns destinations type of register, for assignment
PROC preDeref(type:PTR TO member, n:PTR TO item, postkey, assign, name)
   DEF memb=NIL:PTR TO member
   DEF inc=FALSE, dec=FALSE
   DEF follow=NIL
   DEF t:PTR TO item
   DEF hln:PTR TO hln
   DEF typecopy:member

   #ifdef DBG_CODEGEN
   DEBUGF('preDeref: ')
   #endif

   LOOP
      IF n.data = "."

         n++

         IF n.data = IT_LABEL
            IF type.object = NIL THEN reportErr('only objects have members/methods', name)
            memb := findMember(type.object, n.info.name)
            IF memb = NIL THEN reportErr('unknown member', n.info.name)
            n.data := IT_MEMBER
            n.info := memb

         ELSEIF n.data = IT_PFUNC
            IF type.object = NIL THEN reportErr('only objects have members/methods', name)
            #ifdef DBG_CODEGEN
            DEBUGF(' .method()\n')
            #endif
            RETURN NIL

         ELSEIF n.data = IT_MEMBER
            memb := n.info
         ELSE
            reportIErr('preDeref not member')
         ENDIF
         n++

         #ifdef DBG_CODEGEN
         DEBUGF(' .member [size=\d, esize=\d, numes=\d, object=$\h, flags=$\h\n',
         memb.size, memb.esize, memb.numes, memb.object, memb.flags)
         #endif

         -> what follows member ?

         t := n.data
         SELECT t
         CASE KW_PLUSPLUS
            inc := TRUE
         CASE KW_MINMIN
            dec := TRUE
         CASE "."
            follow := "."
         CASE "["
            follow := "["
         DEFAULT
            follow := NIL
         ENDSELECT

         -> late binding..
         IF memb.object < 0
            IF postkey OR inc OR dec OR (follow="[") OR (follow=".")
               hln := -memb.object
               t := hln.ident2
               IF t = NIL THEN reportErr('unknown object', hln.name)
               memb.object := t
            ENDIF
         ENDIF

         IF inc OR dec
            IF postkey THEN reportErr('cannot ++/-- member', memb.name)
            IF memb.size = 0 THEN reportErr('cannot ++/-- member', memb.name)
            IF memb.flags AND MEMBF_FLOAT THEN reportErr('cannot ++/-- member', memb.name)
            RETURN type2reg(memb)
         ENDIF


         IF follow = "["
            type := memb
            name := memb.name
         ELSEIF follow = "."
            type := memb
            name := memb.name
         ELSEIF assign
            IF memb.size = 0 THEN reportErr('cannot assign to array', memb.name)
            RETURN type2reg(memb)
         ELSEIF postkey = KW_NEW
            IF (memb.size <> 4) OR (memb.numes<>0) THEN reportErr('cannot NEW member', memb.name)
            RETURN AREG
         ELSEIF postkey = KW_END
            IF (memb.size <> 4) OR (memb.numes<>0) THEN reportErr('cannot END member', memb.name)
            RETURN AREG
         ELSE
            RETURN type2reg(memb)
         ENDIF

      ELSEIF n.data = "["

         typecopy.size := IF type.esize = 255 THEN 0 ELSE type.esize
         typecopy.esize:= IF type.object THEN 255 ELSE 0
         typecopy.numes := 0
         typecopy.flags := type.flags
         typecopy.object := type.object

         #ifdef DBG_CODEGEN
         DEBUGF('... index []\n')
         #endif

         IF type.size = 1 THEN reportErr('cannot index variable/member', name) -> CHAR
         IF type.size = 2 THEN reportErr('cannot index variable/member', name) -> INT
         IF type.size = 8 THEN reportErr('cannot index variable/member', name) -> DOUBLE
         n++
         IF n.data <> "]" THEN n++
         n++ -> skip "]"
         IF n.data = KW_PLUSPLUS
            IF type.size = 0 THEN reportErr('cannot ++/-- array')
            IF postkey = KW_NEW THEN reportErr('cannot NEW this')
            IF postkey = KW_END THEN reportErr('cannot END this')
            RETURN type2reg(typecopy)
         ELSEIF n.data = KW_MINMIN
            IF type.size = 0 THEN reportErr('cannot ++/-- array')
            IF postkey = KW_NEW THEN reportErr('cannot NEW this')
            IF postkey = KW_END THEN reportErr('cannot END this')
            RETURN type2reg(typecopy)
         ELSEIF n.data <> "."
            IF postkey = KW_SUPER
              -> fall through
            ELSEIF postkey = KW_NEW
               RETURN AREG
            ELSEIF postkey = KW_END
               RETURN NIL
            ELSE
               IF (type.esize = 255) AND (assign<>FALSE) THEN reportErr('cannot assign to object', name)
               RETURN type2reg(typecopy)
            ENDIF
         ENDIF
         -> remember type and typecopy might be same!
         type := typecopy
      ELSEIF n.data = KW_PLUSPLUS
         RETURN type2reg(type)
      ELSEIF n.data = KW_MINMIN
         RETURN type2reg(type)
      ELSE
         RETURN type2reg(type)
      ENDIF

  ENDLOOP

ENDPROC

PROC type2reg(type:PTR TO member)
   IF type.size = 8
      IF type.flags AND MEMBF_FLOAT
         RETURN FREG
      ELSE
         RETURN D64REG
      ENDIF
   ELSEIF type.size = 4
      IF type.esize
         RETURN DREG
      ELSE
         RETURN DREG
      ENDIF
   ELSEIF type.size = 0
      RETURN DREG
   ELSE
      RETURN DREG
   ENDIF
ENDPROC

-> v44 rewriten.. for the.. n:th time !
-> now ALWAYS returns result in DREG,IREG0
-> v45, must be fixed to NOT release in reg
-> mainly for R64 operands...
-> v45.. should take source register in sarx, source in as,as2
-> v48. new arg "safereturn".. see dvs_member notes.
-> v50: "safreturn" removed..
PROC dvs_index(n:PTR TO item, type:PTR TO member, sarx, as,as2,da:PTR TO derefargs,ha:PTR TO hintargs) OF codegen
   DEF ias=NIL,ias2=NIL,ix, iexp=FALSE
   DEF r, ax, inc=FALSE, ptrsize=0, arraysize=0
   DEF t:PTR TO item, temp:PTR TO item
   DEF ro=NIL, rd
   DEF dx -> v50
   DEF dro, drd
   DEF iha:hintargs -> 2.3, for index exp

   iha.hregop := DREG
   iha.hregnum := -1
   iha.hregisvar := FALSE

   #ifdef DBG_CODEGEN
   DEBUGF('dvs_index($\h,<\d,\d,\d,$\h>,\d,(\d,$\h),\d,(\d,$\h)\n',n,type.size,type.esize,type.numes,
   type.object,sarx,as,as2,da.pk,da.eo,da.ed)
   #endif

   IF g_nilcheck
      IF da.pk = NIL THEN self.nilCheck(sarx,g_linenum)
   ENDIF


   t := n

   IF t.data <> "]"
      iexp := TRUE
      t := t[2]
   ELSE
      t++
   ENDIF

   IF t.data = "."
      t++
      IF t.data = IT_PFUNC THEN reportErr('illegal operation')
      ax := self.obtainAREG()
      IF type.object = NIL THEN reportErr('only objects have members/methods')
      IF iexp
         temp, ias, ias2 := self.doExpression(n,iha)
         ix := DTEMP
         inst_copy(ias,ias2, DREG,ix)
         inst_mulreg(DV,type.object.sizeof,ix)
         inst_copy(ARXPX,AxSizeIdrxScale(sarx,0,ix,1), AREG,ax)
         n, as, as2 := self.dvs_member(type,ax,0, t,ax,da,ha)
      ELSE
         n, as, as2 := self.dvs_member(type,sarx,0, t,ax,da,ha)
      ENDIF
      IF (as = AREG) AND (as2 = ax)
         as2 := self.releaseAREG(ax,1)
      ELSE
         self.releaseAREG(ax,0)
      ENDIF

      RETURN n, as, as2

   ENDIF

   IF t.data = KW_PLUSPLUS
      inc := TRUE
      t++
   ELSEIF t.data = KW_MINMIN
      inst_decarx(sarx, IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
      inst_copy(AREG,sarx,as,as2)
      t++
   ENDIF


   IF da.pk = NIL
      IF da.eo = FALSE
         IF iexp = FALSE
            ro := ARXPO
            rd := AxSizeOfsF(sarx,IF type.esize = 255 THEN 0 ELSE type.esize,0,type.flags)
            IF inc
               IF type.esize = 8
                  IF type.flags AND MEMBF_FLOAT
                     inst_copy(ro, rd, FREG, FTEMP)
                     ro := FREG ; rd := FTEMP
                  ELSE
                     inst_copy(ro, rd, D64REG, D64TEMP)
                     ro := D64REG ; rd := D64TEMP
                  ENDIF
               ELSE
                  inst_copy(ro, rd, DREG, DTEMP)
                  ro := DREG ; rd := DTEMP
               ENDIF
               inst_copy(as,as2, AREG,ATEMP)
               inst_incarx(ATEMP, IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
               inst_copy(AREG,ATEMP, as,as2)
            ENDIF
            n := t
         ELSE -> index exp
            IF n.data = IT_VALUE -> constant iexp ?
               r := n.info * IF type.esize = 255 THEN type.object.sizeof ELSE type.esize
               ro := ARXPO
               rd := AxSizeOfsF(sarx, IF type.esize = 255 THEN 0 ELSE type.esize, r,type.flags)
               IF inc
                  IF type.esize = 8
                     IF type.flags AND MEMBF_FLOAT
                        inst_copy(ro, rd, FREG, FTEMP)
                        ro := FREG ; rd := FTEMP
                     ELSE
                        inst_copy(ro, rd, D64REG, D64TEMP)
                        ro := D64REG ; rd := D64TEMP
                     ENDIF
                  ELSE
                     inst_copy(ro, rd, DREG, DTEMP)
                     ro := DREG ; rd := DTEMP
                  ENDIF
                  inst_copy(as,as2, AREG,ATEMP)
                  inst_incarx(ATEMP,IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
                  inst_copy(AREG,ATEMP, as,as2)
                  ->inst_inc(as,as2, IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
               ENDIF
               n := t
            ELSE -> index
               n, ias,ias2 := self.doExpression(n, iha) -> iexp
               IF type.esize = 255
                  ix := self.obtainDREG()
                  inst_copy(ias,ias2, DREG, ix)
                  inst_mulreg(DV,type.object.sizeof,ix)
               ELSE -> 1.10.0
                  ix := self.copyObtainToDREG(ias,ias2)
               ENDIF
               ro := ARXPX
               rd := AxSizeIdrxScaleF(sarx, IF type.esize = 255 THEN 0 ELSE type.esize, ix, IF type.esize = 255 THEN 1 ELSE type.esize,type.flags)
               IF inc
                  IF type.esize = 8
                     IF type.flags AND MEMBF_FLOAT
                        inst_copy(ro, rd, FREG, FTEMP)
                        ro := FREG ; rd := FTEMP
                     ELSE
                        inst_copy(ro, rd, D64REG, D64TEMP)
                        ro := D64REG ; rd := D64TEMP
                     ENDIF
                  ELSE
                     inst_copy(ro, rd, DREG, DTEMP)
                     ro := DREG ; rd := DTEMP
                  ENDIF
                  inst_copy(as,as2, AREG,ATEMP)
                  inst_incarx(ATEMP,IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
                  inst_copy(AREG,ATEMP, as,as2)
                  ->inst_inc(as,as2, IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
               ENDIF
               self.releaseDREG(ix,0)
               n := t
            ENDIF
         ENDIF
      ELSE -> assign/modification used

         IF da.am = KW_ASSIGN
            IF iexp = FALSE
               ro := ARXPO
               rd := AxSizeOfsF(sarx,type.esize,0,type.flags)
               inst_copy(da.eo,da.ed, ro,rd)
            ELSE -> iexp used
               IF n.data = IT_VALUE -> constant iexp ?
                  inst_copy(da.eo,da.ed, ARXPO,AxSizeOfs(sarx,type.esize,n.info*type.esize))
               ELSEIF n.data = IT_VARIABLE -> variable iexp ?
                  ias := n.info::var.o
                  ias2 := n.info::var.d
                  ix := self.copyObtainToDREG(ias,ias2) -> v48
                  inst_copy(da.eo,da.ed, ARXPX, AxSizeIdrxScale(sarx,type.esize,ix,type.esize))
                  self.releaseDREG(ix,0)
               ELSE -> any iexp
                  temp, ias,ias2 := self.doExpression(n)
                  ix := self.copyObtainToDREG(ias,ias2)
                  inst_copy(da.eo,da.ed, ARXPX, AxSizeIdrxScale(sarx,type.esize,ix,type.esize))
                  self.releaseDREG(ix,0)
               ENDIF
            ENDIF

            ro := da.eo ; rd := da.ed
            IF inc
               inst_copy(as,as2, AREG,ATEMP)
               inst_incarx(ATEMP,type.esize)
               inst_copy(AREG,ATEMP, as,as2)
            ENDIF
            n := t

         ELSE -> modification

            IF iexp = FALSE
               ro, rd := self.dvs_modify(da,ARXPO,AxSizeOfsF(sarx,type.esize,0,type.flags),type)
            ELSE -> iexp used
               IF n.data = IT_VALUE -> constant iexp ?
                  ro, rd := self.dvs_modify(da,ARXPO,AxSizeOfs(sarx,type.esize,n.info*type.esize), type)
               ELSEIF n.data = IT_VARIABLE -> variable iexp ?
                  ias := n.info::var.o
                  ias2 := n.info::var.d
                  ix := self.copyObtainToDREG(ias,ias2) -> v48
                  ro, rd := self.dvs_modify(da,ARXPX,AxSizeIdrxScale(sarx,type.esize,ix,type.esize), type)
                  self.releaseDREG(ix,0)
               ELSE -> any iexp
                  temp, ias,ias2 := self.doExpression(n)
                  ix := self.copyObtainToDREG(ias,ias2)
                  ro, rd := self.dvs_modify(da,ARXPX,AxSizeIdrxScale(sarx,type.esize,ix,type.esize), type)
                  self.releaseDREG(ix,0)
               ENDIF
            ENDIF

            IF inc
               inst_copy(as,as2, AREG,ATEMP)
               inst_incarx(ATEMP,type.esize)
               inst_copy(AREG,ATEMP, as,as2)
            ENDIF
            n := t

         ENDIF

      ENDIF
   ELSEIF da.pk = KW_NEW
      IF iexp = FALSE
         dx := self.newMemory(DV,IF type.esize = 255 THEN type.object.sizeof ELSE type.esize)
         inst_copy(DREG,dx, as,as2)
      ELSE -> iexp used
         n,ias,ias2 := self.doExpression(n, iha)
         ix := self.obtainDREG()
         inst_copy(ias,ias2, DREG,ix)
         IF type.esize = 1
            -> done
         ELSEIF type.esize = 2
            inst_shlreg(DV,1, ix)
         ELSEIF type.esize = 4
            inst_shlreg(DV,2, ix)
         ELSEIF type.esize = 8
            inst_shlreg(DV,3, ix)
         ELSE -> 255
            inst_mulreg(DV,type.object.sizeof, ix)
         ENDIF
         dx := self.newMemory(DREG,ix)
         inst_copy(DREG,dx, as,as2)
         self.releaseDREG(ix,0)
         ->ro := as
         ->rd := as2
         ->n := t
         RETURN t, as, as2
      ENDIF
   ELSEIF da.pk = KW_END
      IF iexp = FALSE
         self.endMemory(DV,IF type.esize = 255 THEN type.object.sizeof ELSE type.esize,as,as2)
      ELSE -> iexp used
         n,ias,ias2 := self.doExpression(n, iha)
         ix := self.obtainDREG()
         inst_copy(ias,ias2, DREG,ix)
         IF type.esize = 1
            -> done
         ELSEIF type.esize = 2
            inst_shlreg(DV,1, ix)
         ELSEIF type.esize = 4
            inst_shlreg(DV,2, ix)
         ELSEIF type.esize = 8
            inst_shlreg(DV,3, ix)
         ELSE -> 255
            inst_mulreg(DV,type.object.sizeof, ix)
         ENDIF
         self.endMemory(DREG,ix, as, as2)
         self.releaseDREG(ix,0)
         n := t
      ENDIF
      RETURN n, NIL, NIL
   ENDIF


   IF da.nd = FALSE

      dro := ha.hregop

      IF ro <> dro

         IF ha.hregnum <> -1
            drd := ha.hregnum
         ELSE
            IF dro = DREG
               drd := self.getFreeDREG()->DTEMP
            ELSEIF dro = AREG
               drd := self.getFreeAREG()->ATEMP
            ELSEIF dro = FREG
               drd := self.getFreeFREG()->FTEMP
            ELSEIF dro = D64REG
               drd := self.getFreeD64()
            ELSE
               reportIErr('dvs_index bad ha.hregop')
            ENDIF
         ENDIF

         inst_copy(ro, rd, dro, drd)

      ELSE

         dro := ro
         drd := rd

      ENDIF

   ELSE

      dro := ro
      drd := rd

   ENDIF


   #ifdef DBG_CODEGEN
   DEBUGF('dvs_index done\n')
   #endif

ENDPROC n, dro, drd


PROC doLabAdr(n:PTR TO item,ha:PTR TO hintargs) OF codegen
   DEF lab:PTR TO codelab
   DEF hln:PTR TO hln
   DEF arx

   #ifdef DBG_CODEGEN
   DEBUGF('getlabadr($\h,\d)\n', n,arx)
   #endif

      IF ha
         IF ha.hregop = AREG
            IF ha.hregnum > -1
               arx := ha.hregnum
            ELSE
               arx := self.getFreeAREG()->ATEMP
            ENDIF
         ELSE
            arx := self.getFreeAREG()->ATEMP
         ENDIF
      ELSE
         arx := self.getFreeAREG()->ATEMP
      ENDIF


      hln := n.info
      lab := hln.ident
      IF lab = NIL THEN reportErr('unknown label', hln.name)
      lab.referenced := 1
      inst_labadr(lab,arx)

   #ifdef DBG_CODEGEN
   DEBUGF('getlabadr done\n')
   #endif

ENDPROC n[1], AREG, arx

PROC doVarAdr(n:PTR TO item,ha:PTR TO hintargs)  OF codegen
   DEF arx
   DEF var:PTR TO var
   DEF hln:PTR TO hln


   #ifdef DBG_CODEGEN
   DEBUGF('getvaradr($\h,\d)\n',n, arx)
   #endif

   IF ha
      IF ha.hregop = AREG
         IF ha.hregnum > -1
            arx := ha.hregnum
         ELSE
            arx := self.getFreeAREG()->ATEMP
         ENDIF
      ELSE
         arx := self.getFreeAREG()->ATEMP
      ENDIF
   ELSE
      arx := self.getFreeAREG()->ATEMP
   ENDIF

      var := n.info
      IF var.o = DREG THEN reportErr('not suitable for {}', var.hln.name)
      inst_varadr(var, arx)

    #ifdef DBG_CODEGEN
    DEBUGF('getvaradr done\n')
    #endif

ENDPROC n[1], AREG, arx

PROC doListUni(buf:PTR TO item) OF codegen
   DEF depth=1
   DEF falselab, endlab
   DEF vars=0
   DEF varasarray[25]:ARRAY OF LONG
   DEF r, expo, expd, a
   DEF var:PTR TO var, t:PTR TO item
   DEF listlen
   DEF modestack[16]:ARRAY OF CHAR
   DEF ofsstack[16]:ARRAY OF CHAR
   DEF aregstack[16]:ARRAY OF CHAR
   DEF hln:PTR TO hln
   DEF n:PTR TO item
   DEF arx, drx
   DEF as, as2

   #ifdef DBG_CODEGEN
   DEBUGF('listuni($\h)\n', buf)
   #endif

   n := buf.info
   n, expo, expd := self.doExpression(n)


   falselab := newLabel()
   endlab := newLabel()

   arx := self.obtainAREG()
   inst_copy(expo,expd, AREG,arx)
   aregstack[depth] := arx
   ofsstack[depth] := 0
   t := n.info
   SELECT t
   CASE 4
   CASE 2
   CASE 1
   CASE 0 -> list
      inst_bic(ARXPO,AxSizeOfs(aregstack[depth],2,-2), ISNE, DV,n.num, falselab)
   DEFAULT -> object
   ENDSELECT

   n++

   REPEAT

      t := n.data
      SELECT t
      CASE "["
         arx := self.obtainAREG()
         inst_copy(ARXPO,AxSizeOfs(aregstack[depth],4,ofsstack[depth]), AREG,arx) -> get new
         depth++
         aregstack[depth] := arx
         ofsstack[depth] := 0
         t := n.info
         SELECT t
         CASE 4
         CASE 2
         CASE 1
         CASE 0 -> list
            inst_bic(ARXPO,AxSizeOfs(aregstack[depth],2,-2), ISNE, DV,n.num, falselab)
         DEFAULT -> object
         ENDSELECT
         n++
      CASE IT_VALUE
         inst_bic(ARXPO, AxSizeOfs(aregstack[depth], 4, ofsstack[depth]), ISNE, DV,n.info, falselab)
         ofsstack[depth] := ofsstack[depth] + 4
         n++
      CASE IT_VARIABLE
         var := n.info
         n++
         inst_copy(ARXPO, AxSizeOfs(aregstack[depth], 4, ofsstack[depth]), var.o, var.d)
         ofsstack[depth] := ofsstack[depth] + 4
      CASE "*"
         n++
         ofsstack[depth] := ofsstack[depth] + 4
      CASE "]"
         self.releaseAREG(aregstack[depth],0)
         depth--
         ofsstack[depth] := ofsstack[depth] + 4
         n++
      ENDSELECT

   UNTIL depth = 0

   IF buf.num
      drx := self.obtainDREG()
      /* return TRUE */
      inst_copy(DV,TRUE, DREG,drx)
      /* end */
      inst_golab(endlab)
   ENDIF
   /* here we end up if uni was false */
   def_label(falselab)
   IF buf.num
      /* return FALSE */
      inst_copy(DV,FALSE, DREG,drx)
   ENDIF
   /* endlabel */
   def_label(endlab)

   IF buf.num THEN drx := self.releaseDREG(drx,1)

    #ifdef DBG_CODEGEN
   DEBUGF('listuni done\n')
   #endif


ENDPROC buf[1], DREG, drx

-> 1.10.0, added MODE_D64
PROC domathlogictail(buf, mode, reg) OF codegen
   DEF n:REG PTR TO item
   DEF t, q, freg, as, as2, reg64
   DEF shift
   DEF fha:hintargs, iha:hintargs, wha:hintargs -> 2.3
   
   fha.hregop := FREG
   fha.hregnum := -1
   fha.hregisvar := FALSE
   
   iha.hregop := DREG
   iha.hregnum := -1
   iha.hregisvar := FALSE

   wha.hregop := D64REG
   wha.hregnum := -1
   wha.hregisvar := FALSE

   #ifdef DBG_CODEGEN
   DEBUGF('mathlogictail($\h,\d,\d)\n',buf,mode,reg)
   #endif

   n := buf


   LOOP
      IF mode = MODE_FLOAT

         IF n.data = "!"
            t := self.obtainDREG()
            inst_f2i(reg,t)
            self.releaseFREG(reg,0)
            reg := t
            mode := MODE_DEFAULT->float := FALSE
            n++
         ELSEIF n.data = "@"   -> 1.10.0
            t := self.obtainD64()
            inst_f2d64(reg,t)
            self.releaseFREG(reg,0)
            reg := t
            mode := MODE_D64
            n++
         ENDIF

      ELSEIF mode = MODE_DEFAULT

         IF n.data = "!"
            t := self.obtainFREG()
            inst_i2f(reg,t)
            self.releaseDREG(reg,0)
            reg := t
            mode := MODE_FLOAT ->float := TRUE
            n++
         ELSEIF n.data = "@"   -> 1.10.0
            t := self.obtainD64()
            inst_i2d64(reg,t)
            self.releaseDREG(reg,0)
            reg := t
            mode := MODE_D64
            n++
         ENDIF

      ELSEIF mode = MODE_D64

         IF n.data = "!"
            t := self.obtainFREG()
            inst_d642f(reg,t)
            self.releaseD64(reg,0)
            reg := t
            mode := MODE_FLOAT
            n++
         ELSEIF n.data = "@"   -> 1.10.0
            t := self.obtainDREG()
            inst_d642i(reg,t)
            self.releaseD64(reg,0)
            reg := t
            mode := MODE_DEFAULT
            n++
         ENDIF

      ELSE

         reportIErr('domathlogictail() mode=?')

      ENDIF

      IF mode = MODE_FLOAT

         t := n.data
         SELECT 256 OF t
         CASE "+"     ; q := `inst_faddreg(as,as2, reg)
         CASE "-"     ; q := `inst_fsubreg(as,as2, reg)
         CASE "*"     ; q := `inst_fmulreg(as,as2, reg)
         CASE "/"     ; q := `inst_fdivreg(as,as2, reg)
         CASE "=", ">", "<", KW_ISGE, KW_ISLE, KW_ISNE
            freg := reg
            reg := self.obtainDREG()
            SELECT t
            CASE "=" ;     q := `inst_fsicreg(as,as2, ISEQ, freg,reg)
            CASE ">" ;     q := `inst_fsicreg(as,as2, ISGT, freg,reg)
            CASE "<" ;     q := `inst_fsicreg(as,as2, ISLT, freg,reg)
            CASE KW_ISGE ; q := `inst_fsicreg(as,as2, ISGE, freg,reg)
            CASE KW_ISLE ; q := `inst_fsicreg(as,as2, ISLE, freg,reg)
            CASE KW_ISNE ; q := `inst_fsicreg(as,as2, ISNE, freg,reg)
            ENDSELECT
            mode := MODE_DEFAULT
         CASE KW_OR, KW_AND, KW_SHR , KW_SHL ; reportErr('bitwise operation on float value')
         DEFAULT
            reg := self.releaseFREG(reg,1)
            #ifdef DBG_CODEGEN
            DEBUGF('mathlogictail DONE $\h, \d, \d\n',n, FREG, reg)
            #endif
            RETURN n, FREG, reg
         ENDSELECT
         n++

         IF n.data = "-"  -> negate ?
            n++
            n, as, as2 := self.doExpression(n, fha)
            t := self.obtainFREG()
            inst_copy(as, as2, FREG, t)
            as := FREG
            as2 := t
            inst_fnegreg(as2)
         ELSEIF n.data = KW_ABS  -> abs ? 2.2
            n++
            n, as, as2 := self.doExpression(n, fha)
            t := self.obtainFREG()
            inst_copy(as, as2, FREG, t)
            as := FREG
            as2 := t
            inst_fabsreg(as2)
         ELSE
            n, as, as2 := self.doExpression(n, fha)
            as2 := self.copyObtainToFREG(as,as2)
         ENDIF

         as := FREG
         Eval(q)
         IF mode = MODE_DEFAULT THEN self.releaseFREG(freg,0)
         self.releaseFREG(as2,0)

      ELSEIF mode = MODE_DEFAULT

         t := n.data
         SELECT 256 OF t
         CASE "+"     ; q := `inst_addreg(as,as2, reg)
         CASE "-"     ; q := `inst_subreg(as,as2, reg)
         CASE "*"     ; q := `inst_mulreg(as,as2, reg)
         CASE "/"     ; q := `inst_divreg(as,as2, reg)
         CASE KW_OR   ; q := `inst_orreg(as,as2, reg)
         CASE KW_AND  ; q := `inst_andreg(as,as2, reg)
         CASE KW_SHR  ; q := `inst_shrreg(as,as2, reg)
         CASE KW_SHL  ; q := `inst_shlreg(as,as2, reg)
         CASE KW_ASR  ; q := `inst_asrreg(as,as2, reg)
         CASE KW_XOR  ; q := `inst_xorreg(as,as2, reg)
         CASE "="     ; q := `inst_sicreg(as,as2, ISEQ, reg)
         CASE ">"     ; q := `inst_sicreg(as,as2, ISGT, reg)
         CASE "<"     ; q := `inst_sicreg(as,as2, ISLT, reg)
         CASE KW_ISGE ; q := `inst_sicreg(as,as2, ISGE, reg)
         CASE KW_ISLE ; q := `inst_sicreg(as,as2, ISLE, reg)
         CASE KW_ISNE ; q := `inst_sicreg(as,as2, ISNE, reg)
         DEFAULT
            reg := self.releaseDREG(reg,1)
            ->#ifdef DBG_CODEGEN
            ->DEBUGF('mathlogictail DONE $\h, \d, \d\n',n, DREG, reg)
            ->#endif
            RETURN n, DREG, reg
         ENDSELECT
         n++
         IF n.data = "-"  -> negate ?
            n++
            n, as, as2 := self.doExpression(n, iha)
            t := self.obtainDREG()
            inst_copy(as, as2, DREG, t)
            as := DREG
            as2 := t
            inst_negreg(as2)
         ELSEIF n.data = "~"  -> not ?
            n++
            n, as, as2 := self.doExpression(n, iha)
            t := self.obtainDREG()
            inst_copy(as, as2, DREG, t)
            as := DREG
            as2 := t
            inst_notreg(as2)
         ELSEIF n.data = KW_ABS  -> abs 2.2
            n++
            n, as, as2 := self.doExpression(n, iha)
            t := self.obtainDREG()
            inst_copy(as, as2, DREG, t)
            as := DREG
            as2 := t
            inst_absreg(as2)
         ELSE
            n, as, as2 := self.doExpression(n, iha)
            IF as <> DV
               as2 := self.copyObtainToDREG(as,as2)
               as := DREG
            ENDIF
         ENDIF

         Eval(q)

         IF as <> DV THEN self.releaseDREG(as2,0)

      ELSEIF mode = MODE_D64  -> 1.10.0
         shift := FALSE
         t := n.data
         SELECT 256 OF t
         CASE "+"     ; q := `inst_addd64(as,as2, reg)
         CASE "-"     ; q := `inst_subd64(as,as2, reg)
         CASE "*"     ; q := `inst_muld64(as,as2, reg)
         CASE "/"     ; q := `inst_divd64(as,as2, reg)
         CASE KW_OR   ; q := `inst_ord64(as,as2, reg)
         CASE KW_AND  ; q := `inst_andd64(as,as2, reg)
         CASE KW_SHR  ; q := `inst_shrd64(as,as2, reg) ; shift := TRUE
         CASE KW_SHL  ; q := `inst_shld64(as,as2, reg) ; shift := TRUE
         CASE KW_ASR  ; q := `inst_asrd64(as,as2, reg) ; shift := TRUE
         CASE KW_XOR  ; q := `inst_xord64(as,as2, reg)
         CASE "=", ">", "<", KW_ISGE, KW_ISLE, KW_ISNE
            reg64 := reg
            reg := self.obtainDREG()
            SELECT 256 OF t
            CASE "="     ; q := `inst_sicd64(as,as2, ISEQ, reg64, reg)
            CASE ">"     ; q := `inst_sicd64(as,as2, ISGT, reg64, reg)
            CASE "<"     ; q := `inst_sicd64(as,as2, ISLT, reg64, reg)
            CASE KW_ISGE ; q := `inst_sicd64(as,as2, ISGE, reg64, reg)
            CASE KW_ISLE ; q := `inst_sicd64(as,as2, ISLE, reg64, reg)
            CASE KW_ISNE ; q := `inst_sicd64(as,as2, ISNE, reg64, reg)
            ENDSELECT
            mode := MODE_DEFAULT
         DEFAULT
            reg := self.releaseD64(reg,1)
            #ifdef DBG_CODEGEN
            DEBUGF('mathlogictail DONE $\h, \d, \d\n',n, D64REG, reg)
            #endif
            RETURN n, D64REG, reg
         ENDSELECT
         n++
         IF n.data = "-"  -> negate ?
            n++
            IF shift
               n, as, as2 := self.doExpression(n, iha)
               t := self.obtainDREG()
               inst_copy(as, as2, DREG, t)
               as := DREG
               as2 := t
               inst_negreg(as2)
            ELSE
               n, as, as2 := self.doExpression(n, wha)
               t := self.obtainD64()
               inst_copy(as, as2, D64, t)
               as := D64
               as2 := t
               inst_negd64(as2)
            ENDIF
         ELSEIF n.data = "~"  -> not ?
            n++
            IF shift
               n, as, as2 := self.doExpression(n, iha)
               t := self.obtainDREG()
               inst_copy(as, as2, DREG, t)
               as := DREG
               as2 := t
               inst_notreg(as2)
            ELSE
               n, as, as2 := self.doExpression(n, wha)
               t := self.obtainD64()
               inst_copy(as, as2, D64, t)
               as := D64
               as2 := t
               inst_notd64(as2)
            ENDIF
         ELSEIF n.data = KW_ABS  -> abs ?  2.2
            n++
            IF shift
               n, as, as2 := self.doExpression(n, iha)
               t := self.obtainDREG()
               inst_copy(as, as2, DREG, t)
               as := DREG
               as2 := t
               inst_absreg(as2)
            ELSE
               n, as, as2 := self.doExpression(n, wha)
               t := self.obtainD64()
               inst_copy(as, as2, D64, t)
               as := D64
               as2 := t
               inst_absd64(as2)
            ENDIF
         ELSE
            IF shift
               n, as, as2 := self.doExpression(n, iha)
               as2 := self.copyObtainToDREG(as,as2)
               as := DREG
            ELSE
               n, as, as2 := self.doExpression(n, wha)
               as2 := self.copyObtainToD64(as,as2)
               as := D64REG
            ENDIF
         ENDIF

         Eval(q)
         IF mode = MODE_DEFAULT THEN self.releaseD64(reg64,0)
         IF as = D64REG THEN self.releaseD64(as2,0) ELSE self.releaseDREG(as2,0)

      ENDIF
   ENDLOOP

ENDPROC




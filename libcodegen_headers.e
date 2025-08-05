-> EEC/libcodegen_headers.e

/* EEC by Samuel Crow et al. [samuraileumas yahoo com] is Copyright (c)2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See CompilerLicense.md */

OPT MODULE, PREPROCESS

OBJECT lib_env_heap
    codeptr:PTR TO LONG, codebuf, databuf,
    gvarlist:PTR TO gvar,
    currentproc:PTR TO proc,
    sizeofptr,
    linelist:PTR TO linedef, codelablist:PTR TO codelab, multireturn:PTR TO multireturn,
    optpowerpc, optmodule,
    globalsize, rwreflist:PTR TO rwref, databufsize,
    nilcheck, linedebug, stepdebug,g_stepdebug50,
    linenum, numregalloc, numfregalloc,
    stacksize, modulelist:PTR TO mlh,
    objectlist:PTR TO object, symbolhunk,
    naturalalign, -> 1.8.2

    regusetab:PTR TO oreg,
    lastx:PTR TO lastx,

    dreg, -> RX/DRX
    areg, -> RX/ARX
    freg, -> FPX
    vreg, -> VX
    d64reg, -> D64 v55
    ireg0,  -> R3/D0   (non obtainable)
    ireg1,  -> R4/D1
    freg0,  -> FP1, F1
    stackreg, -> R1/A7  (non obtainable, dedicated)
    selfreg,  -> R12/A0
    globreg,  -> R13/A4 (non obtainable, dedicated)
    framereg, -> R1/A5  (non obtainable, dedicated)
    atemp,   -> R11/A6 (non obtainable)
    dtemp,   -> R12/D3 (non obtain)
    ftemp,   -> FP0/F0 (non obtain)
    d64temp, -> v55

    safeimmlists -> v57, imported from main and used by doList()

    link_codesize
    link_reloc32list, link_nrofreloc32s
ENDOBJECT

CONST REGUSETABSIZE=96

#define PTRSIZE g.sizeofptr
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

#define GLOBREG g.globreg
#define STACKREG g.stackreg
#define FRAMEREG g.framereg
#define IREG0 g.ireg0
#define IREG1 g.ireg1
#define IREG2 g.ireg2
#define FREG0 g.freg0
#define VREG0 g.vreg0
#define SELFREG g.selfreg

-> theese 3 are NOT obtainable !
#define ATEMP g.atemp
#define DTEMP g.dtemp
#define FTEMP g.ftemp
#define VTEMP g.vtemp
#define D64TEMP g.d64temp

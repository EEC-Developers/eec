-> ECX/compiler.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */


-> Sept 2007: changes to work with new codegen.e

OPT MODULE
OPT EXPORT
OPT PREPROCESS

MODULE '*binary'

#ifdef ECX_VERSION
#define DEBUGF WriteF
->#define DEBUGF DebugF
#endif

#ifndef ECX_VERSION
#define DEBUGF WriteF
#define Word Int
#endif

#define IFUNCDEF(name,args,defs,rets,f) \
   [5, 0, 0, 0, 0,0,0,0, name, 0,0, 0, 0, args, defs, rets, f, 0]:lif

#define IFUNCDEF2(name,code,codelen,args,defs,rets,f) \
   [5, 0, 0, 0, 0,0,0,0, name, 0,0, code, codelen, args, defs, rets, f, 0]:lif

CONST MEMF_CLEAR=$10000, MEMF_PUBLIC=1

CONST TEMPSTRLEN = 16000, TEMPARRAYLEN=2048 -> 1.5.3

CONST LHASHSIZE=1024

CONST IDENT_NONE   = 0,
      IDENT_CONST  = 1,
      IDENT_VARIABLE = 2,
      IDENT_LABEL = 3,  -> proc, asmlab
      IDENT_LFUNC  = 4,
      IDENT_IFUNC = 5,
      IDENT_MACRO  = 6,
      IDENT_REG    = 7,
      IDENT_KEYWORD = 8, -> v46, its back!
      IDENT2_OBJECT = 9,
      IDENT2_ASM    = 10,
      IDENT2_ASMMACRO = 11 -> v46

ENUM IT_NEWLINE=10,  -> 10 (newline),-> linenumber in info (set by .parse())
     IT_LABEL=128,   -> anycase label, string in info
     IT_VALUE,  -> integer value, value in info  v43: now is float if .num=1
     IT_STRING, -> quoted string, string in info
     ->IT_ASIZE,  -> v35: .L .W .B etc
     IT_REG, -> v43:info : PTR TO reg
     IT_ASM, -> info: PTR TO asm,
     IT_IFUNC, -> v44. its back!  info: PTR TO lif
     IT_LFUNC, -> v44. its back!  info: PTR TO lfunc

     /* special types created by syntax parser only! */
     IT_PFUNC, ->IT_FUNCTION,  -> proc
     IT_IMMEDLIST,
     IT_VAREXP, -> if .num then .num is index of assignexp
     IT_EXPRESSION,
     IT_SUBSTATEMENT, -> v38
     IT_PROC, -> v40 : created by proc_key !
     IT_VFUNC, -> v44. variable function. lex:hln sntx:var
     IT_OBJECT, -> v44. lex:hln sntx:object
     IT_VARIABLE, -> v44 lex:hln sntx:var
     IT_IFEXP, -> v44 .
     IT_UNIEXP, -> v44
     IT_CELLEXP, -> v44
     IT_NEWSTRING, -> v44
     IT_PRIVLAB, -> v48 "^x:" priveta label def for MACROs
     IT_MASSIGN, -> v49. multiassign (v1,v2,.. := exp)
     IT_LABADR, -> v49
     IT_VARADR, -> v49
     IT_MEMBER, -> v50, pass1 ptrtypeing, pass2 preDeref() creates it
     IT_LABDEREF, -> 1.9.0 v54

      KEY_BASE,
      KW_NEW,
      KW_END,
      KW_AND,
      KW_OR,
      KW_BUT,
      KW_OPT,
      KW_MODULE,
      KW_OBJECT,
      KW_ENDOBJECT,
      KW_CONST,
      KW_SET,
      KW_ENUM,
      KW_PROC,
      KW_ENDPROC,
      KW_IS,
      KW_DEF,
      KW_SUPER,
      KW_FOR,
      KW_STEP,
      KW_ENDFOR,
      KW_LOOP,
      KW_ENDLOOP,
      KW_WHILE,
      KW_ENDWHILE,
      KW_REPEAT,
      KW_UNTIL,
      KW_JUMP,
      KW_REG,
      KW_IF,
      KW_THEN,
      KW_ELSE,
      KW_ELSEIF,
      KW_ENDIF,
      KW_SELECT,
      KW_CASE,
      KW_DEFAULT,
      KW_ENDSELECT,
      KW_CHAR,
      KW_INT,
      KW_LONG,
      KW_STRING,
      KW_LIST,
      KW_ARRAY,
      KW_PTR,
      KW_TO,
      KW_DO,
      KW_OF,
      ->KW_STRLEN,
      KW_EXPORT,
      KW_SIZEOF,
      KW_RETURN,
      KW_EXCEPT,
      KW_HANDLE,
      KW_EXIT,
      KW_RAISE,
      KW_UNI, -> unification (<=>)

      /* special "keywords" */
      KW_ASSIGN, -> :=
      KW_ISNE,   -> <>
      KW_ISGE,   -> >=, =>
      KW_ISLE,   -> <=, =<
      KW_PTYPE,  -> ::
      KW_PLUSPLUS, -> ++
      KW_MINMIN,   -> --
      KW_MODIFY, -> V46: +=, *=, etc. "+", KW_OR, etc in .info.

      KW_SHL,     -> SHL
      KW_SHR,     -> SHR
      KW_NOP,
      KW_LIBRARY,
      KW_INCBIN, -> v3
      KW_INC,
      KW_DEC, -> v40

      KW_DOUBLE,
      KW_QUAD,   -> v50 reserved for future
      KW_VECTOR,
      KW_CLASS, -> v47
      KW_VOID, -> v47

      KW_BYTE, -> v49!
      KW_WORD, -> v49
      KW_FLOAT, -> v49, back AGAIN!
      KW_REAL, -> v50

      KW_WIDE, -> V50. The new 64bit "LONG"
      KW_ULONG, -> v50, future vector subtype

      KW_OFFSETOF, -> 1.5.6

      KW_UWIDE, -> 1.6.1

      KW_USES, -> 1.7.1
      KW_LINKOBJECT, -> 1.7.1
      KW_AS, -> 1.8.0

      KW_STATIC, -> 1.9.0

      /* 1.10.0 inverted versions */
      KW_IFN,
      KW_ELSEIFN,
      KW_WHILEN,
      KW_UNTILN,
      KW_EXITN,
      -> 1.10.0
      KW_XOR,
      KW_ASR, -> >>

      KW_ABS -> 2.2

OBJECT ident
   identID:LONG
ENDOBJECT /* SIZEOF = 4 */

OBJECT reg OF ident  -> item.info
   type:CHAR -> normal DRX/ARX/RX/FPX or special "V"/"P"
   num:CHAR -> register number
   referenced:INT -> 1 if used
   name:PTR TO CHAR
ENDOBJECT

OBJECT keyword OF ident
   id:LONG
ENDOBJECT

OBJECT asm OF ident -> item.info
   name:PTR TO CHAR
   data:PTR TO LONG
ENDOBJECT

OBJECT macro OF ident -> compatible with modmacro !!
   name:PTR TO CHAR
   type:CHAR -> 0:define, 1:definefunc, 2:asmmacro, 3:varargs definefunc (v44)
   nrofargs:CHAR     -> if type <> 0, else NIL.
   rsrvd:INT
   ascii:PTR TO CHAR
   ->---------------
   params:PTR TO LONG -> if type=1, else NIL. for exported macros
   next:PTR TO macro -> v44, for exported macros
ENDOBJECT

OBJECT const OF ident
   value:LONG
   -> for exported consts
   name:PTR TO CHAR
   next:PTR TO const
ENDOBJECT

OBJECT hln
   name:PTR TO CHAR
   ident:PTR TO ident -> normal idents
   ident2:PTR TO ident -> v37, objects, asm
   hnext:PTR TO hln
ENDOBJECT /* SIZEOF = 16 */

OBJECT item
   data:CHAR
   num:CHAR -> for some syntax types
   info:PTR TO hln -> or value:LONG or PTR TO ident / PTR TO item.. depending..
   /* 1.8.0 */
   info2:LONG -> lower part of 64 bit values
ENDOBJECT /* SIZEOF = 10 */


->           8     8     16    32      1     1
/* type      size  esize numes object  float signed
CHAR memb    1,    0,    0,    0,      0,    0
BYTE memb    1,    0,    0,    0,      0,    1
INT  memb    2,    0,    0,    0,      0,    1
WORD memb    2,    0,    0,    0,      0,    0
LONG memb    4,    0,    0,    0,      0,    1
LONG         4,    1,    0,    0,      0,    1
FLOAT        4,    0,    0,    0,      1,    1
DOUBLE       8,    0,    0,    0,      1,    1
VECTOR      16,    0,    0,    0,      0,    0
CVECT       16,    1,   16,    0,      0,    0
BVECT       16,    1,   16,    0,      0,    1
IVECT       16,    2,    8,    0,      0,    1
WVECT       16,    2,    8,    0,      0,    0
LVECT       16,    4,    4,    0,      0,    1
UVECT       16,    4,    4,    0,      0,    0
FVECT       16,    4,    4,    0,      1,    0
CPTR         4,    1,    0,    0,      0,    0
BPTR         4,    1,    0,    0,      0,    1
IPTR         4,    2,    0,    0,      0,    0
WPTR         4,    2,    0,    0,      0,    1
LPTR         4,    4,    0,    0,      0,    1
FPTR         4,    4,    0,    0,      1,    1
DPTR         4,    8,    0,    0,      1,    0
VPTR         4,   16,    0,    0,      0,    0
CARRAY       0,    1,    #,    0,      0,    0
BARRAY       0,    1,    #,    0,      0,    1
IARRAY       0,    2,    #,    0,      0,    1
WARRAY       0,    2,    #,    0,      0,    0
LARRAY       0,    4,    #,    0,      0,    1
FARRAY       0,    4,    #,    0,      1,    1
DARRAY       0,    8,    #,    0,      1,    1
VARRAY       0,   16,    #,    0,      0,    0
OPTR         4,  255,    0,    obj,    0,    0
OARRAY       0,  255,    #,    obj,    0,    0
OBJECT       0,  255,    1,    obj,    0,    0
*/

-> v49 absorbed "type"
OBJECT member -> compatible with modmember !
   size:CHAR  -> 0/1/2/4/8  [ARRAY/CHAR/INT/LONG/DOUBLE]
   esize:CHAR -> 0/1/2/4/8/255 [NOT_PTR/CPTR/IPTR/LPTR/DPTR/OPTR]
   numes:INT   -> 1-32767 elements, or 0.
   object:PTR TO object -> ...
   offset:INT -> offset from beginning. (v49. unused when type for variable)
   flags:INT
   name:PTR TO CHAR -> hashed
ENDOBJECT /* SIZEOF = 16 */


OBJECT genlab OF ident
   codelink:PTR TO genlab
   offset:LONG
   labrefs
ENDOBJECT

OBJECT codelab OF genlab  -> user labels and procedures
   exported:CHAR, rsrvd:CHAR, referenced:CHAR, ltype:CHAR
   name:PTR TO CHAR -> backpointer to hln.name
   next:PTR TO codelab  -> procedures/labels
   dblabnext:PTR TO codelab -> v56, link all debug symbols together
ENDOBJECT

-> 1.9.0 for STATIC data.
OBJECT statlab OF codelab
   deref:member -> size=0, esize=x, flags=x, object=x or NIL, numes=0
   sizeof:LONG -> 2.2
ENDOBJECT

-> 2.2
OBJECT loclab OF codelab  -> chained off proc.loclabs through loclab.next
   hln:hln -> "fake" hln pointing back to loclab
ENDOBJECT

OBJECT rwref -> immed lists in rw section
   next:PTR TO rwref
   offset:LONG
ENDOBJECT

OBJECT lif OF codelab ->genlab
   code:LONG -> align 2
   codelen:LONG -> align 4
   params:PTR TO LONG -> LIST. was: LONG. used by frontend
   defaults:PTR TO LONG -> LIST: used by frontend
   returns:PTR TO LONG -> was:LONG [0=Dx/Rx,1=Fx,2=RX2] used by frontend
   flags:LONG -> was: inline. 1=inline, 2=varargs. used by bridge/front
   raise:PTR TO raise -> v44 !
ENDOBJECT

CONST VTYPE_GLOB = 0,
      VTYPE_LOC = 1

-> 1.6.1
CONST GTYPE_DEF  = 0,
      GTYPE_BASE = 1,
      GTYPE_XREF = 2,
      GTYPE_INTERNAL = 3 -> 1.7.1

/* 1.9.0 extracted arg out of var */
OBJECT arg OF ident -> compatible with modarg !
   defd:LONG
   pad:CHAR
   rtype:CHAR
   rnum:INT
   ->--------------------
   type:member  -> v49 (was: type:type)
ENDOBJECT

OBJECT var OF arg
   hln:PTR TO hln -> locals need to reach hln (also inherited submodule methods -> module)
   o:CHAR, defo:CHAR
   offset:INT
   d:PTR TO var
   oldident:LONG -> for locals/args
   trok:INT -> bool. if FALSE, DO NOT use .treg !! (set to false when stymbling on {var}) v43
   breg:CHAR -> baseregister
   treg:CHAR -> used by tanslator.
   intreg:INT -> v50 BOOL
   vtype:CHAR -> VTYPE_GLOB/VTYPE_LOC
   link:CHAR -> 1 if moduleglobal, else 0. (1=needs relocation)
   cmplx:INT -> TRUE/FALSE
   next:PTR TO var -> v42
   usage:INT
   saveoffset:INT -> v46 -> save/restore regvar at this offset
   argsarray:PTR TO LONG -> 1.9.0, varfuncs
   multiret:PTR TO multireturn -> 1.9.0, varfuncs. may be NIL
->-------------------
   cbenable:INT -> v50, copyback enable, TRUE/FALSE (not used yet)
   flushme:INT -> v50, contents of .treg should be written back. (not used yet)
   nextuse:INT -> v50, pass1 fills it in with total # of accs. pass2 decrements it. (not used yet)
   varchain:PTR TO var -> v50 (1.5.1) chains ALL _locals_ together!
ENDOBJECT

OBJECT gvar OF var
   export:CHAR
   gtype:CHAR -> 1.6.1
   padd:INT
   labrefs:PTR TO labref -> [next,offset]:labref
   xname:PTR TO CHAR -> 1.8.0. only for LINKOBJECT mode.
ENDOBJECT

OBJECT lfunc OF ident -> compatible with modlfunc !
   name:PTR TO CHAR -> hashed
   nrofargs:CHAR
   type:CHAR -> NIL for now
   baseofs:INT
   basernum:CHAR -> ppc native functions only
   return:CHAR ->
   basertype:CHAR -> v46, 0:rx, 1:stack
   flags:CHAR -> LFFLAG_XXX, 1.7.1
   basehln:PTR TO hln -> librarybase hln (so that any var may be used)
   raise:PTR TO raise -> v44 !
   regs[16]:ARRAY OF lfuncarg
ENDOBJECT

OBJECT object OF codelab
   nrofmembers:INT
   nrofmethods:INT
   sizeof:LONG -> v50 ,made LONG to be able to detect larger than 32k error
   membertable:PTR TO LONG
   methodtable:PTR TO LONG
   privatesuper:INT -> not used yet
   startline:INT -> useful for errors in finnishObjects()
   super:PTR TO object -> if superobject
   classofs:INT -> v44. offset in object of classinfo.
   destofs:INT -> v44. offset of endmethod in classinfo, or -1 for no endmethod.
   addedmethods:PTR TO proc-> proc_key just adds metohods here, nothing more. v44
   alignment:CHAR -> v49. normally 2. 4 if contains float/doubles, 16 if vectors
   flags:CHAR -> 1.8.0 OFLAG_XXX
   modmethtab:PTR TO LONG -> 1.10.0
ENDOBJECT

-> v49, for functionreturns and multiple assigment
OBJECT multireturn
   ros[4]:ARRAY OF LONG
   rds[4]:ARRAY OF LONG
ENDOBJECT


OBJECT proc OF codelab
   nrofargs:CHAR
   nrofdefaults:CHAR
   cpu:INT -> v40: 0=68k, 1=ppc
   argarray:PTR TO LONG
   args32:PTR TO var -> v46
   args64:PTR TO var -> v46
   args128:PTR TO var -> v48, not used yet
   object:PTR TO object -> if method, else NIL
   locals32:PTR TO var -> v45.
   locals00:PTR TO var  -> v45.
   locals64:PTR TO var -> v45.
   locals128:PTR TO var -> v48, not used yet
   framesize:LONG -> v44 used by ppc back. v58 68k too.
   maxcallargs:INT -> v44. used by ppc back. set in pass 1.
   maxcalldepth:INT -> v44. ppc. maximum depth of nested functioncalls. 0=no functioncalls
   handle:INT -> bool
   except:INT -> v58 bool
   pad:INT
   selfvar:PTR TO var -> v47
   flags:LONG -> v47. PROCF_EMPTY, PROCF_CLMETH
   numregalloc:CHAR
   numfregalloc:CHAR
   selfreg:CHAR -> v45. for now always 12
   mret:multireturn -> v49, replaces .return
   varchain:PTR TO var -> v50 (1.5.1) chains ALL locals/params together!
   raise:PTR TO raise -> 2.2
   loclabs:PTR TO loclab -> 2.2
ENDOBJECT


OBJECT raise
   condition:INT -> "EQ"/"NE"/"LT"/"GT"/"LE"/"GE"
   pad:INT
   excval
   trigval
ENDOBJECT /* SIZEOF = 16 */

OBJECT entry OF lfunc
   prochln:PTR TO hln -> same hln as procedure
   label:codelab
ENDOBJECT



OBJECT linedef
   next
   line
   offset
ENDOBJECT



->unreferenced
OBJECT unref
   next:PTR TO unref
   name:PTR TO CHAR
   count:LONG
ENDOBJECT

OBJECT warn  -> v45. g_warnlist
   next:PTR TO warn
   message:PTR TO CHAR
   count:LONG
ENDOBJECT

OBJECT undefglob -> v50
   next:PTR TO undefglob
   name:PTR TO CHAR
ENDOBJECT


/* preprocessor */
CONST MACROBUFSIZE = 16000

-> v49
#define LoopUsageMod(usage) (usage*8)
#define SelectUsageMod(usage) (usage/8)
#define IfUsageMod(usage) (usage)
CONST SIMPLEUSAGE = 2
CONST ASSIGNUSAGE = 4
CONST MINALLOCUSAGE = 12

-> v49
OBJECT it_funccall
   data -> depends on type
   numpars -> # of params issued
   info  -> libbase for libcalls, or NIL.
ENDOBJECT
-> items follow until .data = NIL

OBJECT litem OF item -> 1.5.1 used by immedlist2 getlist
   pad:INT
   next:PTR TO litem
ENDOBJECT

OBJECT it_immedlist
   numelems:INT -> # of elements in list
   newed:INT -> TRUE if NEW []
   offset:LONG -> starting offet into g_databuf to support alignment
   sizeof:LONG -> size in bytes of list
   esize:CHAR  -> 0=object, 1, 2, 4, 8
   eflags:CHAR -> MEMBF_FLOAT or NIL
   cmplx:INT   -> TRUE if LIST
   object:PTR TO object -> if esize=0
   ->type:LONG -> -4=LIST,1=CHAR,2=INT,4=LONG,8=DOUBLE,>1000=object
   litems:PTR TO litem -> 1.5.1 linked items !!
ENDOBJECT
->-> items follow until .data = NIL -> 1.5.1: no items here!!

-> direct access to ifuncs array

/* removed 1.10.0

CONST IF68K_THROW = 100,
      IF68K_FASTNEW = 110,
      IF68K_FASTDISPOSE = 109,
      IF68K_RAISE = 82,
      IF68K_CONS = 128

-> direct access to ifuncsppc array

CONST IFPPC_FASTNEW = 0,
      IFPPC_FASTDISPOSE = 1,
      IFPPC_STRINGF_OLD = 5,
      IFPPC_RAISE = 6,
      IFPPC_THROW = 7,
      IFPPC_WRITEF_OLD = 23,
      IFPPC_PRINTF_OLD = 24,
      IFPPC_I_2_F = 40,
      IFPPC_TEXTF_OLD = 77,
      IFPPC_DEBUGF_OLD = 94,
      IFPPC_STR_FMT_OLD = 96
*/

-> 1.5.4
OBJECT it_varexp
   var:PTR TO var
   postkey:CHAR   -> KW_NEW / KW_END / KW_SUPER / NIL
   -> modification, or NIL.
   -> KW_ASSIGN / "+" / "-" / "*" / "/" / KW_AND / KW_OR / KW_SHL / KW_SHR / NIL
   assignmod:CHAR
   -> index of modification expression if any, or NIL. always NIL for NEW/END/SUPER.
   index:CHAR
   rsrvd:CHAR
ENDOBJECT
-> items follow until n.data = NIL

-> 1.5.4 not used yet
OBJECT it_expression
   quoted:INT     -> TRUE / FALSE
   prefloat:INT   -> TRUE / FALSE (IF !...)
   preneg:INT     -> TRUE / FALSE (IF -x...)
ENDOBJECT
-> items follow until n.data = NIL

-> 1.10.0
ENUM MODE_DEFAULT,
     MODE_FLOAT,
     MODE_D64



OBJECT labref
   next:PTR TO labref
   offset:LONG
   type:LONG -> if external proc/label: REF32/REF24/RE1616, else REF16/REF14/REF32/REF24
ENDOBJECT



->moved here 1.10.0
#define AREG g_areg
#define DREG g_dreg
#define FREG g_freg
#define VREG g_vreg
#define D64REG g_d64reg

-> 1.10.0 moved back in here (needed by new inline.e)
EXPORT ENUM
     NO_OP,
     RX,    -> n
     DRX,   -> n
     ARX,   -> n
     FPX,   -> n
     VX,    -> n -> v48
     VAR, -> var/gvar
     DV,  -> v
     ARXPO, -> <ax,size,ofs>:LONG
     ARXPX, -> <ax,size,idrx,scale>:LONG
     LAB,  -> codelab  (inline asm)
     VAR64, -> var (v45!)
     VAR128, -> 128bit vector v48, not used yet
     D64,      -> 1.10.0 for a virtual WIDE register
     X2R,      -> only used for WIDE returns (handled by secureReturn())
     NROFOPS


#define NEWMEM(size) New(size)
#define NEWMEMR(size) NewR(size)
#define DISPOSE(mem) Dispose(mem)
#define FASTNEW(size) FastNew(size)
#define ALLOCMEM(size,flags) AllocMem(size,flags)
#define FREEMEM(mem,size) FreeMem(mem,size)

OBJECT it_statlist
   len:LONG
   sizeof:LONG
   esize:CHAR
   flags:CHAR
   cplx:INT
   object:PTR TO object
ENDOBJECT



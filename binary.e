/*************************************************************************
** ECX/binary.e.
** Copyright (c) Leif Salomonsson 2002-2008.
** Redistribution of this source is allowed as long as:
** 1. no copyright notice is removed or altered.
** 2. altered source must be clearly marked as such.
*************************************************************************/

OPT MODULE
OPT EXPORT
OPT PREPROCESS

MODULE 'exec/semaphores'
MODULE 'exec/lists'
MODULE 'exec/nodes'

EXPORT CONST MODULEVERSION = 1

EXPORT ENUM CPU_NONE = -1, -> 2.0
            CPU_M68,-> 1.10.0
            CPU_PPC

-> v2.0  procedures/lfuncs
ENUM PPCARGRTYPE_RX,
     PPCARGRTYPE_STACKRX,
     PPCARGRTYPE_FX,
     PPCARGRTYPE_VX,
     PPCARGRTYPE_RX2,
     PPCARGRTYPE_STACKRX2

-> 2.3
ENUM M68ARGTYPE_STACK,
     M68ARGTYPE_FREG,
     M68ARGTYPE_STACK_WIDE

EXPORT OBJECT modulecache OF ss
   ident:INT -> 1.8.0 "MC" (was pad:INT)
   modlist:mlh
   rsrvd:LONG
ENDOBJECT

OBJECT modinfo
   rsrvd:LONG
   count:LONG
   misc:LONG
ENDOBJECT

OBJECT modextens -> internal structure, not actually part of moduleformat
   codelist:LONG
   rname:PTR TO CHAR
   referenced:INT
   secondary:INT
ENDOBJECT

ENUM OSID_NONE,
     OSID_AMIGAOS,
     OSID_AMIGAOS4,
     OSID_MORPHOS,
     OSID_AROS,
     OSID_LINUX

OBJECT moduleheader
   succ:PTR TO moduleheader  -> NIL on disk
   pred:PTR TO moduleheader  -> NIL on disk
   identification:LONG -> "ECXM"
   mname:PTR TO CHAR -> name of module
   mnamelen:INT     -> len of name
   headsize:INT     -> SIZEOF moduleheader
   modextens:PTR TO modextens -> v45
   rsrvd[16]:ARRAY
   checksum:LONG
   checksumstart[1]:ARRAY OF LONG
   cpu:INT        -> 0=68k, 1=ppc
   cpubits:INT -> flags, currently NIL.
   modsize:LONG        -> total size of module
   stacksize:LONG
   osversion:INT
   modversion:INT -> v45: 1
   keepnil:LONG -> WAS oschars[4]:ARRAY OF CHAR (DONTUSE THIS) -> 1.5.6 "\0\0\0\0"/"AOS3"/"ABOX"/"AOS4"/"AROS"
   dummymod:LONG -> TRUE if dummy modulehead in cache (1.10.0)
   rsrvd2:LONG
   osid:LONG -> 2.0, OSID_XXX
   strtabinfo
   macroinfo
   libiinfo
   constinfo
   procinfo
   objectinfo
   moduleinfo
   codeinfo
   ifuncinfo
   xrefginfo
   dreldinfo
   globinfo
   datainfo
   relocinfo
   lineinfo
   debuginfo
   const64info -> 1.8.0  (not used yet)
   relainfo -> 1.10.0 -> elf relocations (to support HA/LO type relocs)  (not used yet)
   globinitinfo -> 2.2
   xglobderefinfo -> 2.2
ENDOBJECT /* SIZEOF = 160 */

OBJECT modmacro -> compatible with macro !
   rsrvd:INT
   totalsize:INT -> to skip to next
   name:LONG
   type:CHAR -> 0:define macro, 1:define macro function, 2:asm macro, 3:varargs macor
   nrofargs:CHAR
   bodysize:INT -> including longalign+nilterm
   pad:LONG -> used internally
-> IF Odd(type) THEN args[nrofargs]:ARRAY OF LONG
-> body[bodysize]:ARRAY OF CHAR
ENDOBJECT

OBJECT modlibi
   basename:LONG
   totalsize:LONG
   nroflfuncs:INT
   rsrvd:INT
-> lfuncs[nroflfuncs]:ARRAY OF modlfunc
ENDOBJECT

OBJECT lfuncarg  -> v45
   name:LONG  -> v46
   rsrvd:CHAR
   rtype:CHAR
   rnum:INT
ENDOBJECT

OBJECT modlfunc  -> compatible with lfunc !
   rsrvd:INT
   totalsize:INT -> v44
   name:LONG
   nrofargs:CHAR
   type:CHAR -> 0=68k abi, 1=ppc sysv abi
   baseofs:INT
   basernum:CHAR -> for ppc sysv
   return:CHAR -> sysv: o=rx, 1=fx, 2:vx, 3:wide
   basertype:CHAR -> 0:rx, 1:stack, 255:monolithic calling
   flags:CHAR ->1.7.1 LFFLAG_XXX
   dummy[2]:ARRAY OF LONG -> we need extra space internally
-> regs[nrofparams]:ARRAY OF lfuncarg
ENDOBJECT

SET LFFLAG_KERNELFUNC,
    LFFLAG_VARARGS -> 2.0

OBJECT modconst
   name:LONG
   value:LONG
ENDOBJECT

OBJECT double
   high:LONG
   low:LONG
ENDOBJECT

OBJECT modconst64 -> not used yet
   name:LONG
   flags:LONG
   value:double
ENDOBJECT

SET PROCF_EMPTY,  -> not really used yet
    PROCF_CLMETH

OBJECT modlabel
   name:LONG
   offset:LONG
   rsrvd:CHAR
   flags:CHAR -> v46,47 PROCF_XXX
   totalsize:INT -> if 12, then we are a label, and no more data.
ENDOBJECT

OBJECT modproc OF modlabel -> also used for methods and labels
   nrofargs:CHAR
   nrofdefaults:CHAR
   return:CHAR -> 0:integer reg, 1:double reg, 2:vector, 3:wide
   selfreg:CHAR -> v45, for methods
-> params[nrofparams]:ARRAY OF modarg
ENDOBJECT

/*
OBJECT oldmodstatic OF modlabel
   -> up till now compatible with modproc
   nrofargs:CHAR -> 0, to be compatible with older tools
   statflags:CHAR -> MEMBF_XXX
   statesize:CHAR -> 1,2,4,8,255
   unused:CHAR
   -> at this point same size as modproc
   statobj:LONG -> name of object if any.
ENDOBJECT  /* SIZEOF = 20 */
*/

-> changed v2.2
OBJECT modstatic OF modlabel
   -> up till now compatible with modproc
   nrofargs:CHAR -> 0, to be compatible with older tools
   statflags:CHAR -> MEMBF_XXX
   keepnil:CHAR -> NIL, this is proc.return to old code.
   statesize:CHAR -> 1,2,4,8,255
   -> at this point same size as modproc
   statobj:LONG -> name of object if any.
   sizeof:LONG   -> yeah.. for SIZEOF support
ENDOBJECT /* SIZEOF = 24 */

-> v49
#define MODPROC_RETURN1(ret) (ret AND $3)
#define MODPROC_RETURN2(ret) (Shr(ret,2) AND $3)
#define MODPROC_RETURN3(ret) (Shr(ret,4) AND $3)
#define MODPROC_RETURN4(ret) (Shr(ret,6) AND $3)
#define MAKE_MODPROC_RETURN(r1,r2,r3,r4) r1 \
                                         OR Shl(r2,2)\
                                         OR Shl(r3,4)\
                                         OR Shl(r4,6)

OBJECT modarg
   name:LONG ->
   defval:LONG
   rsrvd:CHAR
   rtype:CHAR -> 0:rx, 1:stack, 2:fx, 3:vector
   rnum:INT  -> regnum or stackoffset or nil CHANGED v46
ENDOBJECT

SET OFLAG_NOCLASSINFO -> 1.8.1, set for object with only class methods. useful at new/end

OBJECT modobject
   name:LONG
   nrofmembers:INT
   nrofmethods:INT
   sizeof:INT
   flags:CHAR -> OFLAG_XXX (was: rsrvd)
   alignment:CHAR -> v1.5
   totalsize:LONG -> of modobject
-> members[nrofmembers]:ARRAY OF modmember
-> IF nrofmethods THEN modclass:modclass
ENDOBJECT

OBJECT modclass
   classofs:INT  -> classinfo in object
   destofs:INT   -> destructor ( .end() ), or -1
   offset:LONG -> classinfo in code
   supername:LONG -> 1.10.0 it is back. only for inheritation inside same module, otherwise see ref_inherit
   rsrvd2:LONG
-> methods[modobject.nrofmethods]:ARRAY OF modproc
ENDOBJECT

-> v48
SET MEMBF_PRIVATE, -> only used internally in ECX
    MEMBF_FLOAT,    -> set if DOUBLE, PTR TO DOUBLE, ARRAY OF DOUBLE
    MEMBF_SIGNED, -> v49. signed/unsigned (LONG/INT, PTR TO LONG/INT, ARRAY OF LONG/INT)
    MEMBF_VECTOR -> was "forgotten" added 1.7

OBJECT modmember -> same as "member" !! v44
   size:CHAR, esize:CHAR, numes:INT
   object:LONG
   offset:INT
   flags:INT -> (was: rsrvd) v48: MEMBF_XXX or NIL
   name:LONG
ENDOBJECT

OBJECT vdbghead   -> used in executables
   vdbgid:LONG -> "VDBG"
   namelongs:LONG
   strtablongs:LONG
   numdebugs:LONG
   rsrvd:LONG -> NIL
ENDOBJECT
-> sourcename[namelongs]:ARRAY OF LONG
-> strtab[strtablongs]:ARRAY OF LONG
-> debugs[numdebugs]:ARRAY OF vardbg


OBJECT vardbg -> changed again v50
   name:LONG -> name of variable
   inproc:LONG -> name of procedure
   ofobject:LONG -> name of methods object, if variable of method
   scope:CHAR -> 0:internglob, 1:relocglob, 2:xrefglob, 3:global
   varsize:CHAR ->  0/1/2/4/8/16
   varesize:CHAR -> 0/1/2/4/8/16/255
   varreg:CHAR    -> D0-D7:0-7 / A0-A7:8-15 / R0-R31:0-31
   varofs:INT     -> -1:variable is register, else ofs(reg).
   varflags:INT   -> MEMBF_SIGNED, MEMBF_FLOAT, MEMBF_VECTOR, or NIL
   varobject:LONG -> if type.esize=255, else NIL
ENDOBJECT /* SIZEOF = 24 */

ENUM VDSCOPE_INTERN,  -> modules / main
     VDSCOPE_RELOC,   -> modules only
     VDSCOPE_XREF,    -> modules only
     VDSCOPE_GLOBAL,  -> main (exe)
     VDSCOPE_PROC

OBJECT modmodule
   name:LONG -> negative names are relative !
   totalsize:LONG -> total size of this modmodule
   rsrvd:INT
   numsymbs:INT -> nr of symbols referenced
-> modrefs[numsymbs]:ARRAY OF modref
ENDOBJECT

OBJECT modref
   name:LONG  -> or "num" for ifuncs
   ltype:CHAR -> NIL for ifuncs
   info:CHAR  -> numparams for PROC, NIL for ifuncs
   numrefs:INT
-> refs[numrefs]:ARRAY OF LONG -> [type:8, offset:24]
ENDOBJECT


CONST REF32 = 0,
      REF24 = 1,
      REFADR = 2,
      REF1616 = 3,
      REF_INHERIT = 4,   ->  V47 class inheritation between modules
      /* internal types */
      REF16 = 5,
      REF14 = 6


CONST LTYPE_LAB = 0,
      LTYPE_PROC = 1,
      LTYPE_CLASS = 2,
      LTYPE_STATIC = 3-> 1.9.0

-> 2.2

OBJECT xglobderef
   totalsize:LONG -> SIZEOF xglobderef
   name:LONG
   esize:CHAR
   rsrvd1:CHAR
   rsrvd2:INT
   object:LONG
ENDOBJECT




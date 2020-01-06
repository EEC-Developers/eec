
-> ECX/runtime.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT EXPORT

OPT DIR = 'emodules:'

MODULE 'exec/libraries'
MODULE 'exec/resident'
MODULE 'exec/lists'
MODULE 'exec/nodes'


CONST JUMP68 = $4EF9


CONST FRAMEPPC_OLDFRAME = 0,
      FRAMEPPC_LINKREG = 4,
      SIZEOF_FRAMEPPC = 8,

      ES_OLDSTRUCT = 0,
      ES_STACKPOINTER = 4,
      ES_EXCEPTCODE = 8,
      ES_OLDEXCEPTION = 12  -> 2.2.3

CONST SIZEOF_ES = 16


OBJECT memnode
   next:LONG
   size:LONG
ENDOBJECT

OBJECT libentry
   inst:INT
   addr:LONG
ENDOBJECT

OBJECT publicbase OF lib
   pad2:INT
   userlist:mlh -> 1.6.0
   pubbaseid:LONG -> 1.10.0 "PUBL"
   seglist:LONG
   utilitybase:LONG -> 1.10.0
ENDOBJECT

OBJECT privatebase OF lib
   dummy1:INT
   dummy2:LONG
   dummy3:LONG
   dummy4:LONG
   privbaseid:LONG -> "PRIV" -> 1.10.0
   environment:LONG -> 1.10.0
   envmem:LONG -> 1.10.0
   publicbase:PTR TO publicbase
   usertask:LONG -> 1.6.0
   usecount:LONG
ENDOBJECT

CONST PRIVBASE_ENV = 52

OBJECT startinfo
   rwsize:LONG
   initofs:LONG
   stacksize:LONG
   mainofs:LONG
   osversion:LONG -> 1.6
ENDOBJECT

OBJECT libtag OF rt
   -> morphos extensions
   revision:INT
   tags:LONG
ENDOBJECT

OBJECT libinfo OF startinfo -> 1.10.0, lets inherit
   functabofs:PTR TO libentry -> 1.6
   closeofs:LONG
   queryofs:LONG -> v53
ENDOBJECT

->CONST SIZEOF_privatebase = 60


CONST JUMPSTARTSIZE = 8 -> 1.5.4


-> classes
CONST OBJECT_CLASSINFO = -4,
      CLASSINFO_SIZE = -4,
      CLASSINFO_NAME = -8

/* library */

CONST LN_TYPE=8, LN_PRI=9, LN_NAME=10
CONST LIB_FLAGS=14, LIB_VERSION=20, LIB_REVISION=22, LIB_IDSTRING=24


CONST PV_cleanupstack =  -4,  /* ec comp */
      IV_stdout =        -8,  /* ec comp */
      IV_conout =        -12, /* ec comp */
      IV_stdrast =       -16, /* ec comp */
      PV_memlist =       -20, /* ec comp */
      PV_exit68k =       -24, /* 68k cleanupcode */
      PV_clireturnval =  -28, /* ec */
      IV_arg =           -32, /* ec comp */
      IV_wbmessage =     -36, /* ec comp */
      IV_execbase =      -40, /* ec comp */
      IV_dosbase =       -44, /* ec comp */
      IV_intuitionbase = -48, /* ec comp */
      IV_gfxbase =       -52, /* ec comp */
      PV_mathieeesingbasbase =   -56, /* ec comp */
      PV_mathieeesingtransbase = -60, /* ec comp */
      PV_stackbottom =   -64, /* ec comp, the startaddress of stack */->  AllocVec:ed
      PV_codequal =      -68, /* [code:INT,qualifier:INT] intui support */
      PV_iaddress =      -72, /* intui support */
      PV_code_return =   -76, /* 68k exceptions */
      PV_stack_return =  -80, /* 68k exceptions */
      IV_exception =     -84, /* ec comp */
      PV_saveda5 =       -88, /* 68k exceptions */
      IV_stdin =         -92, /* ec comp */
      IV_exceptioninfo = -96, /* ec comp */
      PV_currchunk =     -100, /* FastNew() ("chopmem") */
      PV_chunkleft =     -104, /* FastNew() ("chopleft") */
      PV_cellsmem =      -108, /* ec cells */
      PV_freecells =     -112, /* ec cells */
      PV_chunksize =     -116, /* ec cells */

      /****** END, EC GLOBALS ******/

      PV_mempool =       -120, /* mempool for String(),List(),DisposeLink() */
      PV_utillib =       -124, /* CreativE "utilitybase" variable (NOT USED, KEEP NIL)*/
      PV_cleanupframe =  -128, /* 1.6.0: 68k CleanUp() startup A5 */
      PV_rwdata =        -132, -> V46: points to AllocVec:ed rwdata

      /* v45: own librarybase in library mode */
      IV_librarybase =   -136,

      -> powerpc globals
      PV_randseed =      -140,  -> Rnd()
      PV_ppccleanup =    -144,  -> adr of cleanupcode, used by CleanUp()
      PV_exceptstruct =  -148,  -> ppc exceptionhandling

      -> ppc list-quote function globals
      PV_argsave0 =      -152,  -> ppc quote+list functions..
      PV_argsave1 =      -156,
      PV_argsave2 =      -160,
      PV_argsave3 =      -164,
      PV_argsave4 =      -168,
      PV_argsave5 =      -172,
      PV_argsave6 =      -176,
      PV_argsave7 =      -180,

      PV_initcode  =  -184,  -> 1.10.0, startup sets it to initcode
      PV_unused100 =  -188,
      PV_unused101 =  -192,

      -> ppc str_fmt
      PV_ppc_str_fmt_old =    -196, -> backwards compab as of 1.5.4

      -> ppc double float support
      PV_mathieeedoubbasbase =   -200,
      PV_mathieeedoubtransbase = -204,

      /* OS4 support */
      IV_execiface = -208,
      IV_dosiface = -212,
      IV_gfxiface = -216,
      IV_intuitioniface = -220,
      PV_mathieeedoubbasiface = -224,
      PV_mathieeedoubtransiface = -228,
      PV_mathieeesingbasiface = -232,
      PV_mathieeesingtransiface = -236,


      -> memtable for fastnew
      PV_memtable =      -512, -> table compatible with EC. (260 bytes)

      IVARSSIZE = 512



->CONST ENVOFFSET = SIZEOF_privatebase + IVARSSIZE





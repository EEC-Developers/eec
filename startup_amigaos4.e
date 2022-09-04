

-> October 2008: splitted opt amigaos4

OPT MODULE, PREPROCESS, AMIGAOS4

->#define MINSTARTUP

->#define DEBUG

#ifdef DEBUG
   #define DEBUGF(str,...) DebugF(str,...,0)
#else
   #define DEBUGF(str,...)
#endif

#define MEMFLAGS MEMF_CLEAR OR MEMF_SHARED



   #ifdef MINSTARTUP
      OPT MODNAME = 'minstartup_amigaos4'
   #else
      OPT MODNAME = 'startup_amigaos4'
   #endif

   #define REG_ARG R3
   #define REG_ARGLEN R4
   #define REG_EXEC R5
   #define GLOBREG R13
   #define USE_DOUBLE_LIBS 1
   SAVEREGISTERS MACRO STMW R13, regsave
   LOADREGISTERS MACRO LMW R13, regsave
   #define LIBS_VERSION 50
   MODULE 'exec/tasks'

MODULE 'exec/execbase'
MODULE 'dos/dosextens'
MODULE 'dos/dos'
MODULE 'exec/memory'
MODULE 'powerpc/simple'
MODULE 'runtime'
MODULE 'exec/libraries'

EXPORT ___startinfo:
   LONG 0 -> rwsize
   LONG 0 -> initofs
   LONG 0 -> stacksize
   LONG 0 -> mainofs
   LONG 0 -> osversion

EXPORT PROC ___startup(_arg, _arglen, execbase:PTR TO execbase)
   DEF si:PTR TO startinfo
   DEF sss:stackswapstruct
   DEF rwmem=NIL:PTR TO LONG, ifunc(PTR), r=20, stackmem=NIL
   DEF mem:PTR TO memnode, next
   DEF process:PTR TO process
   DEF regsave[32]:ARRAY OF LONG
   DEF execiface

   SAVEREGISTERS

   execiface := execbase.maininterface

   si := {___startinfo}

   -> check osversion
   IF si.osversion > execbase.lib.version THEN RETURN 20

   rwmem := AllocVec(si.rwsize, MEMFLAGS)
   IF rwmem = NIL THEN RETURN 20

   ifunc := si + si.initofs
   ifunc(rwmem)

   /* WE NOW HAVE ACCESS TO globals ! */

   ___initcode := ifunc -> 1.10.0, save for stuff like newEnvironment()

   ___rwdatasize := si.rwsize -> 2.0

   -> we cannot reach global execbase as its defined locally, so we do it like this
   PutLong(GLOBREG + OFFSETOF ___internalglobs.execbase, execbase)

   -> same with execiface
   PutLong(GLOBREG + OFFSETOF ___internalglobs.execiface, execiface)

   DEBUGF('startup: globreg=$\h\n', GLOBREG)

   ___rwdata := rwmem

   process := FindTask(NIL)

   DEBUGF('startup: allocating stack \d bytes\n', si.stacksize)

   stackmem := AllocVec(si.stacksize, MEMFLAGS)
   IF stackmem = NIL THEN JUMP endpart
   ___stackbottom := stackmem
   -> init stack swap struct
   sss.lower := ___stackbottom
   sss.upper := ___stackbottom + si.stacksize
   sss.pointer := sss.upper - 16
   ___stackswapstruct := sss  -> fix


   -> create pool
   ___mempool := CreatePool(NIL, 4096, 256)
   IF ___mempool = NIL THEN JUMP endpart

   DEBUGF('startup: created mempool $\h\n', ___mempool)

#ifndef MINSTARTUP

   -> check for wbstartup, or arg
   IF process.cli = NIL
      WaitPort(process.msgport)
      wbmessage := GetMsg(process.msgport)
      arg := ''
   ELSE -> no wbstartup
      arg := _arg
      arg[_arglen-1] := NIL
   ENDIF

   DEBUGF('startup: done with arg\n')

   dosbase := OpenLibrary('dos.library', LIBS_VERSION)
   intuitionbase := OpenLibrary('intuition.library', LIBS_VERSION)
   gfxbase := OpenLibrary('graphics.library', LIBS_VERSION)

   dosiface := GetInterface(dosbase, 'main', 1, NIL)
   intuitioniface := GetInterface(intuitionbase, 'main', 1, NIL)
   gfxiface := GetInterface(gfxbase, 'main', 1, NIL)

   stdout := Output()
   stdin := Input()

   DEBUGF('startup: input/output done\n')


#else /* MINSTARTUP */

   arg := _arg

#endif /* MINSTARTUP */

   #ifdef USE_DOUBLE_LIBS
      -> ppc code uses double precision libs
      ___mathieeedoubbasbase := OpenLibrary('mathieeedoubbas.library', LIBS_VERSION)
      ___mathieeedoubtransbase := OpenLibrary('mathieeedoubtrans.library', LIBS_VERSION)
      ___mathieeedoubbasiface := GetInterface(___mathieeedoubbasbase, 'main', 1, NIL)
      ___mathieeedoubtransiface := GetInterface(___mathieeedoubtransbase, 'main', 1, NIL)
   #endif

   #ifdef USE_SINGLE_LIBS
      -> 68k code uses single precision libs
      ___mathieeesingbasbase := OpenLibrary('mathieeesingbas.library', LIBS_VERSION)
      ___mathieeesingtransbase := OpenLibrary('mathieeesingtrans.library', LIBS_VERSION)
      ___mathieeesingbasiface := GetInterface(___mathieeesingbasbase, 'main', 1, NIL)
      ___mathieeesingtransiface := GetInterface(___mathieeesingtransbase, 'main', 1, NIL)
   #endif

   ___os4main := si + si.mainofs

   safeCallMainSwapped()

endpart:

   DEBUGF('startup: finnishing up\n')

   IF stackmem THEN FreeVec(stackmem)

   IF rwmem

      -> free user memory [ New(), NewR(), NewM(), NEW, FastNew() ]
      mem := ___memlist
      WHILE mem
         next := mem.next
         FreeMem(mem, mem.size)
         mem := next
      ENDWHILE

      IF ___mempool THEN DeletePool(___mempool)

      #ifdef USE_SINGLE_LIBS
         DropInterface(___mathieeesingbasiface)
         DropInterface(___mathieeesingtransiface)
         CloseLibrary(___mathieeesingbasbase)
         CloseLibrary(___mathieeesingtransbase)
      #endif

      #ifdef USE_DOUBLE_LIBS
         DropInterface(___mathieeedoubbasiface)
         DropInterface(___mathieeedoubtransiface)
         CloseLibrary(___mathieeedoubbasbase)
         CloseLibrary(___mathieeedoubtransbase)
      #endif

#ifndef MINSTARTUP

      IF conout
         Read(conout,0,0) -> what ?
         Close(conout)
      ENDIF

      DropInterface(dosiface)
      DropInterface(intuitioniface)
      DropInterface(gfxiface)

      CloseLibrary(dosbase)
      CloseLibrary(intuitionbase)
      CloseLibrary(gfxbase)

      IF wbmessage
         Forbid() -> we dont want to be unloadseged until we are done
         ReplyMsg(wbmessage)
      ENDIF

#endif /* MINSTARTUP */

      r := ___clireturnval

      FreeVec(rwmem)

      /* GLOBAL ENV IS NOW GONE */

   ENDIF


   LOADREGISTERS

ENDPROC r

PROC safeCallMain()

   DEBUGF('startup: safeCallMain()\n')

   STW R1, ___cleanupstack

   ___ppccleanup := {exitcode}

   ___clireturnval := ___os4main()

exitcode:
   LWZ R1, ___cleanupstack

   DEBUGF('startup: safeCallMain DONE\n')

ENDPROC

PROC safeCallMainSwapped()
   DEBUGF('startup: safeCallMainSwapped()\n')

   StackSwap(___stackswapstruct)

   safeCallMain()

   -> danger! we have no access to local execiface until after stackswap()!
   -> so we will have to do it with asm
   LWZ R4, ___stackswapstruct
   LWZ R3, .execiface(R13:___internalglobs)
   LWZ R0, StackSwap(R3)
   MTCTR R0
   BCTRL

   DEBUGF('startup: safeCallMainSwapped() DONE\n')

ENDPROC


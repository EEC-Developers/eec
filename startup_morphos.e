
-> Feb 2007

-> Startupcode in E!
-> FINALLY..

-> NOTE: WE CAN NOT have inline lists..
-> Maybe this will change though..

-> TODO: make 68k exitcode (in PPC mode) point to stubcode.

-> October 2008: splitted out morphos version

OPT MODULE, PREPROCESS


->#define MINSTARTUP

#ifdef DEBUG
   #define DEBUGF(str,...) DebugF(str,...)
#else
   #define DEBUGF(str,...)
#endif

#define MEMFLAGS MEMF_CLEAR OR MEMF_PUBLIC

   #ifdef MINSTARTUP
      OPT MODNAME = 'minstartup_morphos'
   #else
      OPT MODNAME = 'startup_morphos'
      #define USE_SINGLE_LIBS 1
   #endif
   OPT MORPHOS
   #define REG_ARG R3
   #define REG_ARGLEN R4
   #define GLOBREG R13
   #define USE_NEW_STACKSWAP 1
   #define USE_ETASK_MEMPOOL 1
   #define USE_DOUBLE_LIBS 1
   SAVEREGISTERS MACRO STMW R13, regsave
   LOADREGISTERS MACRO LMW R13, regsave
   SAVESTACKRETURN MACRO NOP
   LOADSTACKRETURN MACRO NOP
   #define USE_PPC_STUB 1
   #define LIBS_VERSION 50
   MODULE 'exec/tasks'
   #define SHELLSTACKBOT process.task.etask.ppcsplower
   #define STACKREG R1
   SETUPEXEC MACRO
      execbase := Long(4)
   ENDM


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

EXPORT PROC ___startup()
   DEF _arg, _arglen, si:PTR TO startinfo, execbase:PTR TO execbase
   DEF sss:stackswapstruct
   DEF rwmem=NIL:PTR TO LONG, ifunc(PTR), mfunc(), r=20, stackmem=NIL
   DEF mem:PTR TO memnode, next
   DEF process:PTR TO process
   DEF ssargs[10]:ARRAY OF LONG
   DEF regsave[32]:ARRAY OF LONG

   SAVEREGISTERS

   _arg := REG_ARG
   _arglen := REG_ARGLEN

   SETUPEXEC

   si := {___startinfo}

   -> check osversion
   IF si.osversion > execbase.lib.version THEN JUMP endpart

   rwmem := AllocVec(si.rwsize, MEMFLAGS)
   IF rwmem = NIL THEN JUMP endpart

   ifunc := si + si.initofs
   ifunc(rwmem)

   /* WE NOW HAVE ACCESS TO globals ! */

   DEBUGF('startup: globreg=$\h\n', GLOBREG)

   ___initcode := ifunc -> 1.10.0, save for stuff like newEnvironment()

   ___rwdatasize := si.rwsize -> 2.0

   -> we cannot reach global execbase as its defined locally, so we do it like this
   PutLong(GLOBREG + OFFSETOF ___internalglobs.execbase, execbase)

   ___rwdata := rwmem

   process := FindTask(NIL)

   DEBUGF('startup: allocating stack \d bytes\n', si.stacksize)

   stackmem := AllocVec(si.stacksize, MEMFLAGS)
   IF stackmem = NIL THEN JUMP endpart
   ___stackbottom := stackmem
   -> init stack swap struct
   sss.lower := ___stackbottom
   sss.upper := ___stackbottom + si.stacksize
   sss.pointer := sss.upper
   ___stackswapstruct := sss  -> fix


   -> get pool from tc.etask.mempool
   ___mempool := process.task.etask.mempool

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

   dosbase := OpenLibrary('dos.library', LIBS_VERSION)
   intuitionbase := OpenLibrary('intuition.library', LIBS_VERSION)
   gfxbase := OpenLibrary('graphics.library', LIBS_VERSION)

   stdout := Output()
   stdin := Input()


#else /* MINSTARTUP */

   arg := _arg

#endif /* MINSTARTUP */

   #ifdef USE_DOUBLE_LIBS
      -> ppc code uses double precision libs
      ___mathieeedoubbasbase := OpenLibrary('mathieeedoubbas.library', LIBS_VERSION)
      ___mathieeedoubtransbase := OpenLibrary('mathieeedoubtrans.library', LIBS_VERSION)
   #endif

   #ifdef USE_SINGLE_LIBS
      -> 68k code uses single precision libs
      ___mathieeesingbasbase := OpenLibrary('mathieeesingbas.library', LIBS_VERSION)
      ___mathieeesingtransbase := OpenLibrary('mathieeesingtrans.library', LIBS_VERSION)
   #endif

   mfunc := si + si.mainofs

   ssargs[0] := mfunc
   ssargs[1] := GLOBREG
   ___clireturnval := NewPPCStackSwap(sss, {stub}, ssargs)


endpart:

   IF stackmem  THEN FreeVec(stackmem)

   IF rwmem

      -> free user memory [ New(), NewR(), NewM(), NEW, FastNew() ]
      mem := ___memlist
      WHILE mem
         next := mem.next
         FreeMem(mem, mem.size)
         mem := next
      ENDWHILE

      #ifdef USE_SINGLE_LIBS
         CloseLibrary(___mathieeesingbasbase)
         CloseLibrary(___mathieeesingtransbase)
      #endif

      #ifdef USE_DOUBLE_LIBS
         CloseLibrary(___mathieeedoubbasbase)
         CloseLibrary(___mathieeedoubtransbase)
      #endif

#ifndef MINSTARTUP

      IF conout
         Read(conout,0,0) -> what ?
         Close(conout)
      ENDIF

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

PROC stub(mainaddres, globalreg)
   DEF regs[32]:ARRAY OF LONG
   STMW R13, regs
   OR R13, R4, R4
   STW R1, ___cleanupstack
   MTCTR R3
   LA R5, exitcodeppc
   STW R5, ___ppccleanup
   BCTRL
exitcodeppc:
   LWZ R1, ___cleanupstack
   LMW R13, regs
ENDPROC R3

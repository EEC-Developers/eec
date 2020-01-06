

-> Feb 2007

-> Startupcode in E!
-> FINALLY..

-> NOTE: WE CAN NOT have inline lists..
-> Maybe this will change though..

-> October 2008: splitted out amigaos

OPT MODULE, PREPROCESS

->#define MINSTARTUP

#ifdef DEBUG
   #define DEBUGF(str,...) DebugF(str,...)
#else
   #define DEBUGF(str,...)
#endif

#define MEMFLAGS MEMF_CLEAR OR MEMF_PUBLIC

   #define USE_SINGLE_LIBS 1
   #ifdef MINSTARTUP
      OPT MODNAME = 'minstartup_amigaos'
   #else
      OPT MODNAME = 'startup_amigaos'
   #endif
   OPT AMIGAOS
   #define REG_ARG A0
   #define REG_ARGLEN D0
   #define GLOBREG A4
   SAVEREGISTERS MACRO NOP
   LOADREGISTERS MACRO NOP
   SAVESTACKRETURN MACRO
      MOVE.L A7, ___cleanupstack
      MOVE.L A5, ___cleanupframe
   ENDM
   LOADSTACKRETURN MACRO
      MOVE.L ___cleanupstack, A7
      MOVE.L ___cleanupframe, A5
   ENDM
   #define LIBS_VERSION 39
   MODULE 'exec/tasks'
   #define SHELLSTACKBOT process.task.splower
   #define STACKREG A7
   SETUPEXEC MACRO
      execbase := Long(4)
   ENDM

MODULE 'exec/execbase'
MODULE 'dos/dosextens'
MODULE 'dos/dos'
MODULE 'exec/memory'
MODULE '*runtime'
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
   DEBUGF('startup: calling init $\h\n', ifunc)

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
   StackSwap(sss)


   -> create pool
   ___mempool := CreatePool(NIL, 4096, 256)
   IF ___mempool = NIL THEN JUMP endpart

   DEBUGF('startup: ___mempool = $\h\n', ___mempool)

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

   DEBUGF('startup: arg done\n')

   dosbase := OpenLibrary('dos.library', LIBS_VERSION)
   intuitionbase := OpenLibrary('intuition.library', LIBS_VERSION)
   gfxbase := OpenLibrary('graphics.library', LIBS_VERSION)

   stdout := Output()
   stdin := Input()

   DEBUGF('input output done\n')


#else /* MINSTARTUP */

   arg := _arg

#endif /* MINSTARTUP */

   -> keep using 37 as version for these  !
   #ifdef USE_DOUBLE_LIBS
      -> ppc code uses double precision libs
      ___mathieeedoubbasbase := OpenLibrary('mathieeedoubbas.library', 37)
      ___mathieeedoubtransbase := OpenLibrary('mathieeedoubtrans.library', 37)
   #endif

   #ifdef USE_SINGLE_LIBS
      -> 68k code uses single precision libs
      ___mathieeesingbasbase := OpenLibrary('mathieeesingbas.library', 37)
      ___mathieeesingtransbase := OpenLibrary('mathieeesingtrans.library', 37)
   #endif

   SAVESTACKRETURN

   ___exit68k := {exitcode}

   mfunc := si + si.mainofs

   DEBUGF('startup: calling main $\h\n', mfunc)

   ___clireturnval := mfunc()

exitcode:
   LOADSTACKRETURN
endpart:

   StackSwap(___stackswapstruct)
   FreeVec(stackmem)

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


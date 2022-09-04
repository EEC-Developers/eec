/*

 (The zlib/libpng License)

 Copyright (c) 2007 Leif Salomonsson

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.

*/

-> Feb 2007

-> NOTE: Being startupcode, WE CAN NOT have inline lists..
-> Maybe this will change though..

-> May 2007: now returns same base if task already has openlibrary us.

-> 2008 sometime, added support for MorphOS2 query() function and resident.tags.

-> May 2008: made query() function take additional parameter: utilitybase
-> as we are not guarantied to have global environment in this function.
-> this also means we open utilitybase in init close it in expunge. store it in
-> publicbase.

-> June 2008: libraries now uses same environment layout as executables.
-> also changed the libinfo structure, no more relocs in it.
-> Environment is now allocated separately and stuffed into
-> privbase.environment.

OPT MODULE, PREPROCESS

#ifdef DEBUG
#define DEBUGF(str,...) DebugF(str,...)
#else
#define DEBUGF(str,...)
#endif


->#define CODE_MORPHOS
->#define CODE_AMIGAOS


#ifdef CODE_MORPHOS
   OPT MORPHOS
   #define RTFLAGS RTF_PPC
   #define GLOBREG R13
   #define USE_DOUBLE_LIBS 1
   #define PUBLICFUNCARRAY PTR libopen_gate, libclose_gate, libexpunge_gate, libext_gate, -1
   PROC_RTINIT_HEAD MACRO PROC rtinit(dum,seglist,execbase:PTR TO execbase)
   PROC_LIBOPEN_HEAD MACRO
      PROC libopen()
      DEF pubbase:PTR TO publicbase, version, eh:PTR TO emulhandle, globregsave
      eh := R2
      pubbase := eh.an[6]
      version := eh.dn[0]
   ENDM
   SETUPGLOBREG MACRO
      LWZ GLOBREG, \1
      LWZ GLOBREG, GLOBREG, PRIVBASE_ENV
   ENDM
   SAVEGLOBREG MACRO STW GLOBREG, globregsave
   LOADGLOBREG MACRO LWZ GLOBREG, globregsave
   PROC_LIBCLOSE_HEAD MACRO
      PROC libclose()
      DEF eh:PTR TO emulhandle, privbase:PTR TO privatebase, globregsave
      eh := R2
      privbase := eh.an[6]
   ENDM
   PROC_LIBEXPUNGE_HEAD MACRO
      PROC libexpunge()
      DEF eh:PTR TO emulhandle, pubbase:PTR TO publicbase
      eh := R2
      pubbase := eh.an[6]
   ENDM
   PROC_LIBEXT_HEAD MACRO
      PROC libext()
      DEF eh:PTR TO emulhandle, pubbase:PTR TO publicbase
      DEF data, attr
      ->DEF privbase:PTR TO privatebase
      eh := R2
      pubbase := eh.an[6]
      data := eh.an[0]
      attr := eh.dn[0]
   ENDM
   RUNPROTECT MACRO
      ADDI R3, R0, -1
      BLR
   ENDM
   #define LIBS_VERSION 50
   #define MATHLIBS_VERSION 37
   OPT MODNAME = 'multilibrary_morphos'
   #define CACHECLEARU NOP
   MODULE 'morphos/exec'
   MODULE 'morphos/exec/resident'
   MODULE 'morphos/emul/emulinterface'
   MODULE 'powerpc/simple'
   #define ALLOCVEC(size,flags) AllocVecAligned(size,flags,16,16)
#endif

#ifdef CODE_AMIGAOS
   OPT AMIGAOS
   #define USE_SINGLE_LIBS 1
   #define RTFLAGS NIL
   #define GLOBREG A4
   #define PUBLICFUNCARRAY PTR libopen, libclose, libexpunge, libext, -1
   PROC_RTINIT_HEAD MACRO
      PROC rtinit()
      DEF seglist, execbase:PTR TO execbase
      MOVE.L D0, seglist
      MOVE.L A6, execbase
   ENDM
   PROC_LIBOPEN_HEAD MACRO
      PROC libopen()
      DEF pubbase:PTR TO publicbase, version, globregsave
      MOVE.L A6, pubbase
      MOVE.L D0, version
   ENDM
   SETUPGLOBREG MACRO
      MOVE.L \1, GLOBREG
      MOVE.L PRIVBASE_ENV(GLOBREG), GLOBREG
   ENDM
   SAVEGLOBREG MACRO MOVE.L GLOBREG, globregsave
   LOADGLOBREG MACRO MOVE.L globregsave, GLOBREG
   PROC_LIBCLOSE_HEAD MACRO
      PROC libclose()
      DEF privbase:PTR TO privatebase, globregsave
      MOVE.L A6, privbase
   ENDM
   PROC_LIBEXPUNGE_HEAD MACRO
      PROC libexpunge()
      DEF pubbase:PTR TO publicbase
      MOVE.L A6, pubbase
   ENDM
   PROC_LIBEXT_HEAD MACRO
      PROC libext()
      DEF privbase:PTR TO privatebase, pubbase:PTR TO publicbase
      DEF data, attr
      MOVE.L A6, privbase
      MOVE.L A0, data
      MOVE.L D0, attr
   ENDM
   RUNPROTECT MACRO
      MOVE.L #-1, D0
      RTS
   ENDM
   #define LIBS_VERSION 39
   #define MATHLIBS_VERSION 37
   OPT MODNAME = 'multilibrary_amigaos'
   #define CACHECLEARU CacheClearU()
   MODULE 'exec/resident'
   #define ALLOCVEC(size,flags) AllocVec(size,flags)
#endif


MODULE 'morphos/exec/libraries'
MODULE 'exec/memory'
MODULE 'runtime'
MODULE 'exec/nodes'
MODULE 'exec/lists'
MODULE 'exec/execbase'

CONST INITFLAGS = LIBF_SUMUSED OR LIBF_CHANGED
CONST ALLOCMEMFLAGS = MEMF_CLEAR OR MEMF_PUBLIC

EXPORT ___startup:
   RUNPROTECT

EXPORT ___libtag:
   WORD RTC_MATCHWORD
   PTR ___libtag
   PTR endskip
   CHAR RTFLAGS /* might get or-patched with RTF_EXTENDED later */,
        0       /* version patched later */,
        NT_LIBRARY,
        0
   PTR NIL -> namestr  # (14)
   PTR NIL -> idstr    # (18)
   PTR rtinit
   WORD 0   -> v53: revision patched later
   PTR 0    -> v53: tags. for mos extended resident struct

EXPORT ___startinfo:
   LONG 0 -> rwsize
   LONG 0 -> initofs
   LONG 0 -> stacksize
   LONG 0 -> mainofs
   LONG 0 -> osversion
   LONG 0 -> functabofs
   LONG 0 -> closeofs
   LONG 0 -> queryofs

publicfunctions:
   PUBLICFUNCARRAY


PROC_RTINIT_HEAD
   DEF lib:PTR TO publicbase
   DEF libinfo:PTR TO libinfo
   DEF libtag:PTR TO libtag

   DEBUGF('LIB_rtinit\n')

   libtag := {___libtag}
   libinfo := {___startinfo}
   IF libinfo.osversion > execbase.lib.version THEN RETURN NIL

   lib := MakeLibrary({publicfunctions}, NIL, NIL, SIZEOF publicbase, seglist)
   IF lib = NIL THEN RETURN NIL

   lib.seglist := seglist
   lib.version := libtag.version
   lib.revision := libtag.revision
   lib.flags := INITFLAGS
   IF libinfo.queryofs
      lib.flags OR= LIBF_QUERYINFO -> v53
      lib.utilitybase := OpenLibrary('utility.library', 39) -> 1.10.0
      IF lib.utilitybase = NIL THEN RETURN NIL
   ELSE
      lib.utilitybase := NIL
   ENDIF
   lib.ln.name := libtag.name
   lib.idstring := libtag.idstring
   lib.ln.type := NT_LIBRARY

   lib.userlist.head := lib.userlist + 4
   lib.userlist.tail := NIL
   lib.userlist.tailpred := lib.userlist
   lib.pubbaseid := "PUBL"

   AddLibrary(lib)

   DEBUGF('LIB_rtinit DONE (lib=$\h, ver=\d, rev=\d, flags=$\h, name=\s, idstr=\s, osver=\d)\n',
   lib, lib.version, lib.revision, lib.flags, lib.ln.name, lib.idstring, libinfo.osversion)

ENDPROC lib

PROC thistask()
   DEF execbase
   execbase := Long(4)
ENDPROC FindTask(NIL)


PROC_LIBOPEN_HEAD
   DEF privbase:PTR TO privatebase
   DEF libinfo:PTR TO libinfo, libtag:PTR TO libtag
   DEF ifunc(PTR), mfunc(), r, task

   DEBUGF('LIB_open lib=$\h, ver=\d\n', pubbase, version)

   SAVEGLOBREG

   libinfo := {___startinfo}
   libtag := {___libtag}
   pubbase.opencnt++
   pubbase.flags := pubbase.flags AND Not(LIBF_DELEXP)

   task := thistask()

   privbase := pubbase.userlist.head
   WHILE privbase.ln.succ
      EXIT privbase.usertask = task
      privbase := privbase.ln.succ
   ENDWHILE

   IF privbase.ln.succ

      DEBUGF('LIB_open reused privbase = $\h, task = $\h\n', privbase, task)
      privbase.usecount++

      RETURN privbase

   ENDIF

   privbase := makePrivateBase(libinfo.functabofs + libinfo, libinfo.rwsize + libinfo.stacksize)
   IF privbase = NIL THEN RETURN NIL
   privbase.ln.name := libtag.name
   privbase.idstring := libtag.idstring
   privbase.version := version -> version _requested_!
   privbase.publicbase := pubbase
   privbase.opencnt := pubbase.opencnt -> 1.6
   privbase.usecount := 1
   privbase.usertask := task

   DEBUGF('LIB_open created privbase = $\h, task = $\h, libinfo.initofs = $\h\n',
      privbase, task, libinfo.initofs)

   -> call init (sets up GLOBREG)
   ifunc := libinfo.initofs + libinfo
   r := ifunc(privbase.envmem)
   ___stackbottom := r

   DEBUGF('LIB_open globreg = $\h\n', GLOBREG)

   privbase.environment := GLOBREG

   DEBUGF('LIB_open done calling init, ___stackbottom = $\h\n', ___stackbottom)

   execbase := Long(4)

   AddTail(pubbase.userlist, privbase)

   DEBUGF('LIB_open done adding private base to list\n')

   dosbase := OpenLibrary('dos.library', LIBS_VERSION)
   IF dosbase = NIL THEN JUMP libopen_error
   intuitionbase := OpenLibrary('intuition.library', LIBS_VERSION)
   IF intuitionbase = NIL THEN JUMP libopen_error
   gfxbase := OpenLibrary('graphics.library', LIBS_VERSION)
   IF gfxbase = NIL THEN JUMP libopen_error
   #ifdef USE_SINGLE_LIBS
   ___mathieeesingbasbase := OpenLibrary('mathieeesingbas.library', MATHLIBS_VERSION)
   IF ___mathieeesingbasbase = NIL THEN JUMP libopen_error
   ___mathieeesingtransbase := OpenLibrary('mathieeesingtrans.library', MATHLIBS_VERSION)
   IF ___mathieeesingtransbase = NIL THEN JUMP libopen_error
   #endif
   #ifdef USE_DOUBLE_LIBS
   ___mathieeedoubbasbase := OpenLibrary('mathieeedoubbas.library', MATHLIBS_VERSION)
   IF ___mathieeedoubbasbase = NIL THEN JUMP libopen_error
   ___mathieeedoubtransbase := OpenLibrary('mathieeedoubtrans.library', MATHLIBS_VERSION)
   IF ___mathieeedoubtransbase = NIL THEN JUMP libopen_error
   #endif

   DEBUGF('LIB_open opened libs ok\n')

   librarybase := privbase

   ___mempool := CreatePool(ALLOCMEMFLAGS, 4096, 256)
   IF ___mempool = NIL THEN JUMP libopen_error

   -> call main()
   mfunc := libinfo.mainofs
   IF mfunc
      mfunc += libinfo
      r := mfunc()
      IF r = FALSE THEN JUMP libopen_error
   ENDIF

   DEBUGF('LIB_open done calling main()\n')

   LOADGLOBREG

   RETURN privbase

libopen_error:

   cleanupInstance(privbase)
   freePrivbase(privbase)

   LOADGLOBREG

ENDPROC FALSE

PROC freePrivbase(base:PTR TO privatebase)
   DEBUGF('freePrivbase($\h)\n', base)
   FreeMem(base-base.negsize, base.negsize + base.possize)
ENDPROC


PROC cleanupInstance(base:PTR TO privatebase)
   DEF mem:PTR TO memnode, next
   DEBUGF('cleanUpInstance($\h)\n', base)
   CloseLibrary(dosbase)
   CloseLibrary(intuitionbase)
   CloseLibrary(gfxbase)
   #ifdef USE_SINGLE_LIBS
   CloseLibrary(___mathieeesingbasbase)
   CloseLibrary(___mathieeesingtransbase)
   #endif
   #ifdef USE_DOUBLE_LIBS
   CloseLibrary(___mathieeedoubbasbase)
   CloseLibrary(___mathieeedoubtransbase)
   #endif

   IF ___mempool THEN DeletePool(___mempool)

   mem := ___memlist
   WHILE mem
      next := mem.next
      FreeMem(mem, mem.size)
      mem := next
   ENDWHILE

   IF base.envmem THEN FreeVec(base.envmem) -> 1.10.0

ENDPROC

PROC_LIBCLOSE_HEAD
   DEF func(), pubbase:PTR TO publicbase
   DEF libinfo:PTR TO libinfo

   -> 1.10.0 valid base check
   IF privbase.privbaseid <> "PRIV" THEN RETURN NIL

   DEBUGF('LIB_close privbase=$\h, usecount=\d\n', privbase, privbase.usecount)

   pubbase := privbase.publicbase

   IF privbase.usecount > 1
      pubbase.opencnt--
      privbase.usecount--
      RETURN NIL
   ENDIF

   SAVEGLOBREG
   SETUPGLOBREG privbase

   libinfo := {___startinfo}

   func := libinfo.closeofs
   IF func
      func += libinfo
      func()
   ENDIF

   cleanupInstance(privbase)

   Remove(privbase)

   freePrivbase(privbase)

   IF pubbase.opencnt-- = 0
      IF pubbase.flags AND LIBF_DELEXP
         LOADGLOBREG
         DEBUGF('LIB_close calling expunge, we want to go home\n')
         RETURN expunge(pubbase)
      ENDIF
   ENDIF

   LOADGLOBREG

   DEBUGF('LIB_close DONE\n')

ENDPROC NIL

PROC_LIBEXPUNGE_HEAD
   DEBUGF('LIB_expunge pubbase=$\h\n', pubbase)
ENDPROC expunge(pubbase)

PROC expunge(base:PTR TO publicbase)
   DEF execbase, seglist

   IF base.opencnt > 0
      base.flags OR= LIBF_DELEXP
      RETURN NIL
   ENDIF
   seglist := base.seglist
   execbase := Long(4)
   IF base.utilitybase THEN CloseLibrary(base.utilitybase)  -> 1.10.0
   Remove(base)
   FreeMem(base - base.negsize, base.negsize + base.possize)

   DEBUGF('expunge DONE\n')

ENDPROC seglist



PROC makePrivateBase(functab, datasize)
   DEF lib:PTR TO privatebase, e:PTR TO libentry, e2:PTR TO libentry, negsize=4*SIZEOF libentry
   DEF execbase, a
   execbase := Long(4)
   e := functab
   DEBUGF('makePrivateLibrary($\h,\d) computing negsize\n', functab,datasize)
   WHILE e.inst <> -1 DO e++
   negsize := negsize + e - functab + 3 AND -4
   DEBUGF('makePrivateLibrary() allocating mem (negsize=\d)\n', negsize)
   lib := AllocMem(SIZEOF privatebase + negsize, ALLOCMEMFLAGS)
   IF lib = NIL THEN RETURN NIL
   lib := lib + negsize
   DEBUGF('makePrivateLibrary() constructing functable (lib=$\h)\n', lib)

   e2 := lib

   e := {vectable}
   FOR a := 1 TO 4
      e2--
      e2.inst := e.inst
      e2.addr := e.addr
      e++
   ENDFOR

   e := functab
   WHILE e.inst <> -1
      e2--
      e2.inst := e.inst
      e2.addr := e.addr
      e++
   ENDWHILE

   lib.negsize := negsize

   -> 1.10.0 some stuff changed
   lib.possize := SIZEOF privatebase
   lib.envmem := AllocVec(datasize, ALLOCMEMFLAGS)
   IF lib.envmem = NIL
      freePrivbase(lib)
      RETURN NIL
   ENDIF

   lib.ln.type := NT_LIBRARY
   lib.flags := LIBF_SUMUSED OR LIBF_CHANGED
   lib.privbaseid := "PRIV"
   SumLibrary(lib) -> 1.6.0
   CACHECLEARU
   DEBUGF('makePrivateLibrary() DONE\n')
ENDPROC lib

-> May 2008: setups no global environment as we might be called without OpenLibrary() !
-> we also pass pubbase.utilitybase as third arg.
-> we might get called with both public AND private libbase!
PROC_LIBEXT_HEAD
   DEF info:PTR TO libinfo, func(PTR,LONG,PTR)(PTR), r=NIL
   info := {___startinfo}
   func := info.queryofs
   IF func
      func += info
      IF pubbase.pubbaseid <> "PUBL"              -> 1.10.0
         pubbase := pubbase::privatebase.publicbase
      ENDIF
      r := func(data,attr,pubbase.utilitybase)
   ENDIF
ENDPROC r

endskip:

   FLOAT 0.0 -> ensures longword align :)

#ifdef CODE_MORPHOS

libopen_gate:
   WORD TRAP_LIB, 0
   PTR libopen
libclose_gate:
   WORD TRAP_LIB, 0
   PTR libclose
libexpunge_gate:
   WORD TRAP_LIB, 0
   PTR libexpunge
libext_gate:
   WORD TRAP_LIB, 0
   PTR libext

vectable:
   WORD JUMP68 ; PTR libopen_gate
   WORD JUMP68 ; PTR libclose_gate
   WORD JUMP68 ; PTR libexpunge_gate
   WORD JUMP68 ; PTR libext_gate

#endif /* CODE_MORPHOS */

#ifdef CODE_AMIGAOS

vectable:
   WORD JUMP68 ; PTR libopen
   WORD JUMP68 ; PTR libclose
   WORD JUMP68 ; PTR libexpunge
   WORD JUMP68 ; PTR libext

#endif /* CODE_AMIGAOS */






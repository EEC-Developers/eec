OPT PREPROCESS

#ifndef STACKSIZE
	#define STACKSIZE 262144
#endif

OPT STACK=STACKSIZE

OPT LARGE

/* EEC by Samuel D. Crow [samuraileumas yahoo com] is Copyright (c) 2019 */
/* Derived from the work listed below */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

/*
	ECX homepage: "http://blubbedev.net/ecx" 
*/


-> Apr 15 2004: v45
-> June 2004: v46
-> Aug 2004: v47
-> Nov 2004: v48
-> Feb 2005: v49pre (aug 2005, released as 1.4.5)
-> Aug 2005: v49pre (sept 2005, 1.4.6)
-> Sep 2005: v49pre (1.4.7)
-> Apr 2006: v50 (1.5)
-> Mar 2007: v51 (1.6)
-> Jun 2007: v52 (1.7)
-> Aug 2007: v52 (1.7.1)
-> sept 2007: v52 (1.7.2)
-> Sept 2007: v53 (1.8.0)  ecxmain.e
-> Feb 2008: v53 (1.8.1)
-> Apr 2008: v54 (1.9.0)
-> May 2008: v55 (1.10.0) => 2.0.0 !
-> something v56 (2.1)
-> July 2009: v57 (2.2)
-> Something: 2.2.1
-> June 2010: v57 (2.2.2)
-> June 2013: v58 (2.3.0)
-> Sept 2013: keyfile stuff removed. (2.3.1)


#define COMPILEDWITH 'Compiled with'
#define ECX_WIP ' WIP'
->#define ECX_WIP ''

#define ECX_BRANCH_VERSION_STR '58'
#define ECX_BRANCH_VERSION_NUM 58
#define ECX_COPYRIGHT_STR 'by Leif Salomonsson (c) 2002-2013'
#define ECX_VERSTR 'ECX 2.3.1' + ECX_WIP

#ifndef ECX_VERSION
	#define ECX_PLATFORM_STR 'AmigaOS,68K'
#endif
#ifdef ECX_VERSION
	#define ECX_PLATFORM_STR __TARGET__
#endif

#define ECX_VERSION_STRING '$VER: ' + \
	     ECX_VERSTR + \
	     ' (xx.08.2013) ' + \
	     '[v' + \
	     ECX_BRANCH_VERSION_STR + \
	     '] (' + \
	     ECX_PLATFORM_STR + \
	     ') ' + \
	     ECX_COPYRIGHT_STR

CONST VERSIONLONG = $FF020301 -> REPLACE FF with 00 for release version

-> #define DEBUG

#ifdef DEBUG

->#define DBG_MAIN
->#define DBG_PP
->#define DBG_EXE
->#define DBG_PASS1
->#define DBG_TRANS
->#define DBG_BMOD
->#define DBG_OOP

#endif

CONST REUSEREGS = TRUE


MODULE 'dos/dos'
MODULE 'exec/lists'
MODULE 'exec/nodes'
MODULE 'dos/dosextens'
MODULE 'dos/var' -> 1.10.0

MODULE '*binary'
MODULE '*compiler'
MODULE '*eecelf'
MODULE '*runtime' -> 1.5.6
MODULE '*support' -> 1.6.1
MODULE '*codegen' -> 1.8.0 (v53)
MODULE '*common'  -> 1.8.0 (v53)
MODULE '*assembler' -> 1.10.0 (v55)
->MODULE '*initcode'  -> 1.10.0 (v55)
MODULE '*inline68'    -> 1.10.0 (v55)
MODULE '*inlineppc'    -> 1.10.0 (v55)
MODULE '*ppcgen'    -> 1.10.0 (v55)
MODULE '*020gen'    -> 1.10.0 (v55)
MODULE '*ecmodtrans' -> 1.10.0 (v55)
MODULE '*libstubs' -> 2.2 v57

DEF g_globcheckhead="HEAD"

DEF g_modulecache:PTR TO modulecache
DEF g_optmodule=FALSE -> OPT MODULE
DEF g_optpreprocess=FALSE
DEF g_optexport=FALSE -> OPT EXPORT
DEF g_optstack=FALSE  -> OPT STACK
DEF g_optosversion=FALSE -> OPT OSVERSION
DEF g_optpowerpc=CPU_NONE -> OPT POWERPC
DEF g_moduledir=NIL -> 1.10.0 now linked estrings
DEF g_nilcheck=FALSE -> NILCHECK/S
DEF g_optimize=FALSE -> OPTI/S
DEF g_nowarn=FALSE -> OPT NOWARN, NOWARN/S
DEF g_vardebug=FALSE -> VARDEBUG/S
DEF g_linedebug=FALSE -> LINEDEBUG/S
DEF g_symbolhunk=FALSE -> SYMBOLHUNK/S
DEF g_quiet=FALSE -> v49
DEF g_showbuf=FALSE
DEF g_numregalloc=0
DEF g_numfregalloc=0 -> v48
DEF g_optnostartup=FALSE
DEF g_nodefmods=FALSE
DEF g_linkobjmode=FALSE -> 1.7.1
DEF g_optroundnear=FALSE -> 1.7.1

DEF g_sourcename[512]:STRING -> was pointer
DEF g_linenum=0
DEF g_stacksize=0
DEF g_modulelist:mlh -> shared between various
DEF g_warnlist -> v45

DEF g_emacrolist=NIL
DEF g_librarymode=FALSE, g_libraryname, g_libraryversion
DEF g_libraryrevision, g_libraryidstr
DEF g_entrytable[1024]:ARRAY OF LONG, g_nrofentries=0
DEF g_econstlist=NIL:PTR TO const
DEF g_gvarlist=NIL:PTR TO gvar
DEF g_objectlist=NIL:PTR TO object

DEF ident_STRLEN:PTR TO const -> v46

DEF g_currentproc:PTR TO proc

DEF g_unrefs=NIL

DEF g_internalfuncs=NIL:PTR TO LONG, g_internalfuncsppc=NIL:PTR TO LONG
DEF link_datasize=0
DEF g_globalsize=0:PTR TO LONG -> first global starts at 0(A4).
DEF link_globaldatasize=0
DEF link_codesize=0
DEF link_reloc32list=NIL, link_nrofreloc32s=0

DEF g_codebuf, g_codeptr:PTR TO LONG
DEF g_databuf, g_databufsize=0
DEF g_linelist:PTR TO linedef

DEF g_extracodesize=0 -> set by various in pass1
DEF g_incbinsize=0 -> v45: sum of all incbins
DEF g_codelablist:PTR TO codelab -> user labels, procedures, generated labels. .codelink
DEF g_rwreflist:PTR TO rwref -> immedlists
DEF g_proclist:PTR TO proc -> user labels and procedures .next

DEF g_currentobject=NIL:PTR TO object

DEF g_temparray[TEMPARRAYLEN]:ARRAY OF LONG -> for members, params
DEF g_tempstr[TEMPSTRLEN]:STRING -> for immedstrings
DEF g_ppstring=NIL:PTR TO CHAR -> for macro definition and macro expansion
DEF g_argbodystr=NIL:PTR TO CHAR -> for expanding body with arguments
DEF g_argprocstr=NIL:PTR TO CHAR -> 1.10.0, for processing arguments

DEF itmz_linestart=NIL:PTR TO item
DEF itmz_buffer=NIL:PTR TO item, itmz_bufend -> realloc-able
DEF itmz_stringssize=0
DEF itmz_labelhashtable:PTR TO LONG

DEF g_secondariesloaded=FALSE -> V47

DEF c_functiondepth=0 -> to keep track of depth in pass1

DEF itmz_openbrackdepth=0 -> to keep track in itemizer

DEF g_startupcodelab:PTR TO codelab -> v48

DEF g_currentloopdepth=0 -> FOR/WHILE/LOOP/REPEAT. used by autoregalloc
-> v49
DEF g_currentselectdepth=0, g_currentifdepth=0

DEF g_privatekey=FALSE -> v45


DEF g_codegen=NIL:PTR TO codegen -> v46

-> v49
DEF g_multireturn:PTR TO multireturn -> set this when calling function/method

-> v50 (1.5.1)
DEF g_ivarsadded=FALSE -> 1.7.1
DEF g_methodlist:PTR TO proc -> all defned methods in this source here. meth.next lkinks them

DEF g_optminstartup=FALSE -> 1.5.4

DEF g_optosid=OSID_NONE -> 2.0

DEF g_maxmodosvers=NIL -> gets set to maximum osversion from modules  1.6.0

DEF g_sizeofptr=4 -> v52

DEF g_xcodelist=NIL -> 1.7.1, for external code referenced from LINKOBJECT

-> v53
DEF g_forcemodsyms=FALSE ->TRUE if user set SYMBOLHUNK/S for module explicetly
DEF g_libraryrtagsname=NIL
DEF itmz_export=FALSE -> 1.8.0

DEF g_modname[128]:STRING -> only filename !
DEF g_exename[128]:STRING -> only filename !
DEF g_destdir[256]:STRING

DEF g_naturalalign=FALSE -> 1.8.12 .. OBJECTs

-> 1.10.0
DEF ident_LINENUM:PTR TO const
DEF g_elfsym_target[100]:STRING -> __abox__ etc
DEF g_elf_usephdr=FALSE -> true for os4

DEF g_opt_moduledir[1024]:STRING -> OPT DIR
DEF g_arg_moduledir[1024]:STRING -> comandline dir

DEF g_defmodnames=NIL -> linked list of estrings set by scantargetmodule()
DEF g_privoptnotargetmod=FALSE -> special opt to not load target module (from self)

DEF 
	 g_compiledwithstr,
	 g_verstr,
	 g_platformstr

DEF g_outformatstr[50]:STRING -> 2.0

DEF g_dblablist:PTR TO codelab -> 2.1, link all debugsymbols

-> 2.2
DEF g_showcondsyms=FALSE, g_condsymnames=NIL
DEF g_modglobinitlab:PTR TO genlab -> used in module mode with code

DEF g_safeimmlists=FALSE

-> 2.2.3
DEF itmz_lastbrackline -> improving error report

-> 2.3
DEF g_varfill=FALSE:PTR TO LONG

DEF g_globchecktail="TAIL"


CONST TICKS_PER_MINUTE=TICKS_PER_SECOND*60

ENUM ARG_SOURCE,
	  ARG_NILCHECK,
	  ARG_HOLD,
	  ARG_WBTOFRONT,
	  ARG_OPTIMIZE,
	  ARG_LARGE,
	  ARG_SHOWFNAME,
	  ARG_NOWARN,
	  ARG_VARDEBUG,
	  ARG_LINEDEBUG,
	  ARG_SYMBOLHUNK,
	  ARG_IGNORECACHE,
	  ARG_NODEFMODS,
	  ARG_SHOWBUF,
	  ARG_NUMREGALLOC,
	  ARG_NUMFREGALLOC, -> v48
	  ARG_TOADDR, -> v48
	  ARG_ABSOLUTE, -> v48
	  ARG_MODULEDIR, -> v48
	  ARG_DEFINE, -> v49p
	  ARG_DESTDIR, -> v49p
	  ARG_QUIET, -> v49p
	  ARG_EXENAME, -> v49
	  ARG_MODNAME, -> 1.5.6
	  ARG_STEPDEBUG, -> 1.6.0
	  ARG_POWERPC, -> v52
	  ARG_MORPHOS, -> v52
	  ARG_AMIGAOS, -> 1.10.0
	  ARG_SHOWARGS, -> 1.10.0
	  ARG_AMIGAOS4, -> 2.0.0
	  ARG_OUTFORMAT, -> 2.0
	  ARG_SHOWCONDSYMS, -> 2.2
	  ARG_ERRLINE, -> 2.2
	  ARG_VARFILL, -> 2.3
	  NROFARGS -> keep last !

PROC main() HANDLE
	DEF fbuf, flen, fib:fileinfoblock
	DEF fh=NIL, rdargs=NIL, args:PTR TO LONG
	DEF t:PTR TO item
	DEF err
	DEF mh:PTR TO moduleheader, a
	DEF tempstr[2000]:STRING
	DEF hold=FALSE, wbtofront=FALSE
	DEF codebufsize, hln:PTR TO hln
	DEF size
	DEF ds1:datestamp, ds2:datestamp -> v49
	DEF process:PTR TO process  -> v49
	DEF argstemplate, targetmacro:PTR TO macro
	DEF errline=FALSE

	DateStamp(ds1)

	NEW args[NROFARGS]

	argstemplate :=    'SOURCE/A,'+
	                   'NIL=NILCHECK/S,'+
	                   'HOLD/S,'+
	                   'WBTOFRONT/S,'+
	                   'OPTI/S,'+
	                   'LARGE/S,'+
	                   'SHOWFNAME/S,'+
	                   'NOWARN/S,'+
	                   'VAR=VARDEBUG/S,'+
	                   'LINE=LINEDEBUG/S,'+
	                   'SYM=SYMBOLHUNK/S,'+
	                   'ICA=IGNORECACHE/S,'+
	                   'NODEFMODS/S,'+
	                   'SHOWBUF/S,'+
	                   'REG=NUMREGALLOC/N,'+
	                   'FREG=NUMFREGALLOC/N,'+
	                   'TOADDR/N,'+
	                   'ABSOLUTE/N,'+
	                   'MDIR=MODULEDIR/K,'+
	                   'DEFINE/K,'+
	                   'DDIR=DESTDIR/K,'+
	                   'QUIET/S,'+
	                   'EXENAME/K,'+
	                   'MODNAME/K,'+
	                   'STEP=STEPDEBUG/S,'+
	                   'POWERPC/S,'+
	                   'MORPHOS/S,'+
	                   'AMIGAOS/S,'+
	                   'SHOWARGS/S,'+
	                   'AMIGAOS4/S,'+
	                   'OUTFORMAT/K,'+
	                   'SHOWCONDSYMS/S,'+
	                   'ERRLINE/S,'+
	                   'VARFILL/N'



	/* read agrs */
	rdargs := ReadArgs(argstemplate, args, NIL)

	IF rdargs = NIL THEN Raise("ARGS")

	initSupport() -> 1.6.1

	IF args[ARG_IGNORECACHE] = NIL
	   Forbid()
	   g_modulecache := FindSemaphore('EcxCache')
	   Permit()
	   IF g_modulecache = NIL
	      g_modulecache := ALLOCMEM(SIZEOF modulecache, MEMF_PUBLIC OR MEMF_CLEAR)
	      IF g_modulecache = NIL THEN Raise("MEM")
	      g_modulecache.modlist.head := g_modulecache.modlist + 4
	      g_modulecache.modlist.tail := NIL
	      g_modulecache.modlist.tailpred := g_modulecache.modlist
	      g_modulecache.ln.name := ALLOCMEM(9,MEMF_CLEAR OR MEMF_PUBLIC)
	      AstrCopy(g_modulecache.ln.name, 'EcxCache')
	      AddSemaphore(g_modulecache)
	      g_modulecache.ident := "MC" -> 1.8.0
	   ENDIF
	ENDIF

	IF g_modulecache
	   IF g_modulecache.ident <> "MC" THEN Raise("MCC")  -> 1.8.0
	   IF AttemptSemaphore(g_modulecache) = NIL THEN g_modulecache := NIL
	ENDIF


	g_compiledwithstr := COMPILEDWITH
	g_verstr := ECX_VERSTR
	g_platformstr := ECX_PLATFORM_STR

	NEW itmz_labelhashtable[LHASHSIZE+1]  -> hln hash

	IF args[ARG_NILCHECK]   THEN g_nilcheck := TRUE
	IF args[ARG_NOWARN]     THEN g_nowarn := TRUE
	IF args[ARG_SYMBOLHUNK]
	   g_symbolhunk := TRUE
	ENDIF
	IF args[ARG_LINEDEBUG]  THEN g_linedebug := TRUE
	IF args[ARG_VARDEBUG]   THEN g_vardebug := TRUE
	IF args[ARG_SHOWBUF]    THEN g_showbuf := TRUE
	IF args[ARG_WBTOFRONT]  THEN wbtofront := TRUE
	IF args[ARG_HOLD]       THEN hold := TRUE
	IF args[ARG_NODEFMODS]  THEN g_nodefmods := TRUE
	IF args[ARG_NUMREGALLOC] THEN g_numregalloc := Long(args[ARG_NUMREGALLOC])
	IF args[ARG_NUMFREGALLOC] THEN g_numfregalloc := Long(args[ARG_NUMFREGALLOC])
	IF args[ARG_OPTIMIZE]
	   g_optimize := TRUE -> not used yet
	   g_numregalloc := -1
	   g_numfregalloc := -1
	ENDIF

	-> 2.3
	IF args[ARG_VARFILL] THEN g_varfill := args[ARG_VARFILL]

	IF g_vardebug THEN g_linedebug := TRUE   -> 1.6.0
	IF g_linedebug THEN g_symbolhunk := TRUE -> 1.6.0

	-> 1.10.0
	IF args[ARG_MODULEDIR]
	   StrCopy(g_arg_moduledir, args[ARG_MODULEDIR])
	ENDIF

	-> 2.2
	IF args[ARG_ERRLINE] THEN errline := TRUE

	IF args[ARG_QUIET] THEN g_quiet := TRUE

	g_sizeofptr := 4 ->IF args[ARG_E64] THEN 8 ELSE 4 -> v52

	g_modulelist.head := g_modulelist + 4
	g_modulelist.tail := NIL
	g_modulelist.tailpred := g_modulelist

	StrCopy(g_sourcename, args[ARG_SOURCE])

	IF StrCmp(g_sourcename+EstrLen(g_sourcename)-2, '.e') = FALSE THEN StrAdd(g_sourcename, '.e')
	LowerStr(g_sourcename)


	-> we need these this early in case error occurs, so we delete old file

	IF args[ARG_DESTDIR]
	   StrCopy(g_destdir, args[ARG_DESTDIR])
	ELSE
	   StrCopy(g_destdir, g_sourcename, FilePart(g_sourcename) - g_sourcename)
	ENDIF

	IF args[ARG_EXENAME]
	   StrCopy(g_exename, args[ARG_EXENAME])
	ENDIF

	IF args[ARG_MODNAME]
	   StrCopy(g_modname, args[ARG_MODNAME])
	   StrAdd(g_modname, '.m')
	ENDIF

	IF g_quiet = FALSE
	   WriteF('\s (\s) \s.\n',
	   ECX_VERSTR, ECX_PLATFORM_STR, ECX_COPYRIGHT_STR)
	   WriteF('\s \s.\n')
	ENDIF

	IF args[ARG_SHOWARGS] THEN WriteF('Arguments: "\s"\n', arg)

	IF args[ARG_SHOWFNAME] THEN WriteF('-> \s\n', g_sourcename)

	IF g_showbuf
	    WriteF('Stack: \d bytes FIXED.\n', FreeStack())
	ENDIF

	-> 1.10.0
	->IF readConfig() = FALSE
	->   addModuledirs('ecxmodules:')
	->ENDIF

	fh := Open(g_sourcename, OLDFILE)
	IF fh = NIL THEN Throw("OPEN", g_sourcename)

	IF ExamineFH(fh, fib) = NIL THEN Throw("EXAM", g_sourcename)
	flen := fib.size
	fbuf := NEWMEMR(flen+4)
	fbuf[0] := 10 -> must start with newline
	IF Read(fh, fbuf+1, flen) <> flen THEN Throw("READ", g_sourcename)
	fbuf[flen+1] := 10 -> must end with newline
	IF NameFromFH(fh, g_sourcename, StrMax(g_sourcename)) = NIL THEN Throw("NFFH", g_sourcename)
	SetStr(g_sourcename, StrLen(g_sourcename))
	LowerStr(g_sourcename) -> 1.8.2 fix
	Close(fh) ; fh := NIL

	addKeywords()
	add_iconsts()

	-> v52, deprecated,obsolete v55
	->IF args[ARG_POWERPC]
	->   g_optosid := OSID_MORPHOS
	->   g_optpowerpc := CPU_PPC
	->ENDIF

	IF args[ARG_MORPHOS] OR args[ARG_POWERPC]
	   g_optosid := OSID_MORPHOS
	   g_optpowerpc := CPU_PPC
	   StrCopy(g_outformatstr, 'ELF')
	ELSEIF args[ARG_AMIGAOS]
	   g_optosid := OSID_AMIGAOS
	   g_optpowerpc := CPU_M68
	   StrCopy(g_outformatstr, 'ADOS')
	ELSEIF args[ARG_AMIGAOS4]
	   g_optosid := OSID_AMIGAOS4
	   g_optpowerpc := CPU_PPC
	   StrCopy(g_outformatstr, 'ELF')
	ENDIF

	-> 2.0
	IF args[ARG_OUTFORMAT]
	   StrCopy(g_outformatstr, args[ARG_OUTFORMAT])
	   UpperStr(g_outformatstr)
	ENDIF

	IF g_optosid <> OSID_NONE THEN loadTargetModule() -> 2.0

	-> v49, add commandline define ?
	IF args[ARG_DEFINE] THEN addCLDefine(args[ARG_DEFINE])

	-> 2.2
	IF args[ARG_SHOWCONDSYMS] THEN g_showcondsyms := TRUE


	/* internalfuncs may be un-inited ! */
	g_internalfuncs := [NIL]
	g_internalfuncsppc := [NIL]

	-> 1.5.4
	t := uncommentSource(fbuf) - fbuf
	IF t > (fbuf+flen+4) THEN Raise("UNCO")

	->WriteF(fbuf)


	t := Mul(Div(t,10), SIZEOF item) + (4000*SIZEOF item)
	itmz_buffer := NEWMEMR(t)
	itmz_linestart := itmz_buffer
	itmz_bufend := itmz_buffer + t
	IF g_quiet = FALSE THEN WriteF('Lexical and syntax analysis..\n')
	setLinenum(0)
	itmz_linestart.data := 10
	itmz_linestart.info := 0
	t := itemize(fbuf, itmz_linestart+SIZEOF item)
	t.data := 10 ; t++
	t.data := NIL ; t++
	IF t > itmz_bufend THEN Raise("IBUF")

	-> 1.8.2
	g_databufsize := g_databufsize + 15 AND (-16)

	initTarget() -> in case it has not beendone in itemize() pass ! 1.8.0
	initIvars() -> we need this too

	-> 1.8.0
	IF EstrLen(g_exename) = 0
	   StrCopy(g_exename, FilePart(g_sourcename), StrLen(FilePart(g_sourcename))-2)
	ENDIF
	IF EstrLen(g_modname) = 0
	   StrCopy(g_modname, FilePart(g_sourcename), StrLen(FilePart(g_sourcename))-2)
	   StrAdd(g_modname, '.m')
	ENDIF

	IF (g_globcheckhead <> "HEAD") OR (g_globchecktail <> "TAIL") THEN Raise("HTC1") -> 1.8.0

	IF g_showbuf
	   WriteF('Item buffer: \d of \d bytes used.\n', t-itmz_buffer, itmz_bufend-itmz_buffer)
	ENDIF

	IF g_optosversion = 0 THEN g_optosversion := g_maxmodosvers -> 1.6.0

	IF g_quiet = FALSE
	   WriteF('Targeting ')
	   IF g_proclist -> 1.8.0
	      hln := getLabelHLN('__TARGET__') -> 2.0
	      targetmacro := hln.ident
	      IF targetmacro THEN WriteF('\s ', targetmacro.ascii)
	   ENDIF
	   IF g_optmodule
	      WriteF(IF g_linkobjmode THEN 'linkobject' ELSE 'module')
	   ELSEIF g_librarymode
	      WriteF('library (\d.\d)', g_libraryversion, g_libraryrevision)
	   ELSE
	      WriteF('executable')
	   ENDIF

	   -> 1.6.0
	   WriteF(IF g_optosversion THEN ', osversion \d.\n' ELSE '.\n', g_optosversion)
	ENDIF

	DISPOSE(fbuf)


	IF g_optmodule = FALSE THEN loadSecondaryModules() -> v49, do it here

	codebufsize := (t - itmz_buffer / 2) + g_extracodesize -> functions/expressions extra code
	codebufsize := Div(codebufsize,4)
	codebufsize := Mul(codebufsize, IF g_optpowerpc THEN 3 ELSE 2)
	codebufsize := codebufsize +
	               g_incbinsize +
	               itmz_stringssize +
	               32000 + -> startup,ifuncs,glob-initcode, misc
	               g_databufsize + -> datainit
	               link_datasize -> rw-initcode


	g_codebuf := NEWMEMR(codebufsize)
	g_codeptr := g_codebuf
	g_databuf := NEWMEMR(g_databufsize) -> size computed in syntax/immedlist()


	#ifdef DBG_PASS1
	DEBUGF('finnishObjects()\n')
	#endif

	finnishObjects()

	allocateGlobals() -> allocates all globals for exe, private globals for module

	IF g_optmodule = FALSE
	   linkModules()
	   putAlign(4)
	   fixExtMethsOffsets()
	   patchMethods()
	   putStartup()
	   IF g_librarymode THEN putLibTable()
	ENDIF

	IF g_quiet = FALSE THEN WriteF('Parsing and generating..\n')

	-> v57
	hln := getLabelHLN('___SAFELISTS___')
	IF hln.ident THEN g_safeimmlists := TRUE

	t := g_codegen.compileCode(itmz_buffer)

	putAlign(4)


	#ifdef DBG_MAIN
	DEBUGF('freeing items buffer $\h\n', itmz_buffer)
	#endif
	DISPOSE(itmz_buffer)
	itmz_buffer := NIL

	#ifdef DBG_MAIN
	DEBUGF('freed items buffer OK\n')
	#endif

	IF g_codeptr > (g_codebuf + codebufsize) THEN Raise("CBUF")

	putClassinfos()

	putRelocStrings()

	putAlign(4)

	IF g_codeptr > (g_codebuf + codebufsize) THEN Raise("CBUF")

	IF g_showbuf THEN WriteF('Code buffer: \d of \d bytes used.\n', g_codeptr - g_codebuf, codebufsize)

	IF g_optmodule

	   StrCopy(tempstr, g_destdir)
	   StrAdd(tempstr, g_modname)



	   IF g_codeptr > g_codebuf -> 2.2
	      g_modglobinitlab := newLabel()
	      putLabel(g_modglobinitlab)
	      g_codegen.putGlobInit()
	      putAlign(4)
	      g_codeptr[]++ := VERSIONLONG -> 1.6.0
	   ENDIF

	   patchReferences()

	   IF g_linkobjmode -> 1.7.1
	      fbuf, size := makeLINKOBJ()

	      IF writeNewFile(fbuf, size, tempstr) = NIL THEN Throw("WRIT", tempstr)

	   ELSE

	      mh, size := makeModule()

	      setsum(mh)

	      IF writeNewFile(mh, size, tempstr) = NIL THEN Throw("WRIT", tempstr)

	   ENDIF

	ELSE

	   IF g_optosid = OSID_MORPHOS
	      StrCopy(g_elfsym_target, '__abox__')
	   ELSEIF g_optosid = OSID_AMIGAOS4
	      StrCopy(g_elfsym_target, '__amigaos4__')
	   ELSE
	      StrCopy(g_elfsym_target, '__amigaos__')
	   ENDIF

	   /* v48 */
	   IF g_librarymode
	      putAlign(4)
	      IF g_optpowerpc THEN putEntriesPPC() ELSE putEntries68K()
	      putAlign(4)
	   ENDIF

	   linkMainIfuncs()

	   putAlign(4)

	   finnishExe()

	   g_codeptr[]++ := VERSIONLONG -> 1.6.0

	   patchReferences()

	   IF StrCmp(g_outformatstr, 'RAW')
	      fbuf, size := makeRAW(
	         IF args[ARG_ABSOLUTE] THEN Long(args[ARG_ABSOLUTE]) ELSE -1,
	         IF args[ARG_TOADDR] THEN Long(args[ARG_TOADDR]) ELSE NIL) -> v48
	   ELSEIF StrCmp(g_outformatstr, 'ELF')
	      fbuf, size := makeELF()
	   ELSEIF StrCmp(g_outformatstr, 'ADOS')
	      fbuf, size := makeADOS()
	   ELSE
	      reportIErr('unknown out format')
	   ENDIF

	   IF g_librarymode

	      StrCopy(tempstr, g_destdir)
	      StrAdd(tempstr, g_modname)

	      mh, a := makeLibMod()

	      setsum(mh)

	      IF writeNewFile(mh, a, tempstr) = NIL THEN Throw("WRIT", tempstr)

	   ENDIF

	   StrCopy(tempstr, g_destdir)
	   StrAdd(tempstr, g_exename)

	   IF args[ARG_TOADDR] = FALSE   -> v48

	      IF writeNewExeFile(fbuf, size, tempstr) = NIL THEN Throw("WRIT", tempstr)

	   ENDIF

	   #ifdef DBG_EXE
	   DEBUGF('exe: \d bytes written\n', size)
	   #endif

	ENDIF

	checkUnrefs()

	printUnrefs()

	showCondSyms()

	->printUndefglobs() -> v50 removed v55

	IF (g_globcheckhead <> "HEAD") OR (g_globchecktail <> "TAIL") THEN Raise("HTC2")

	->checkHashTable() -> debug

EXCEPT DO

  -> debug
  ->IF g_modulecache
  ->   hln := getLabelHLN('createZonegroupClass')
  ->   g_modulecache.rsrvd := hln.ident
  ->ENDIF

	-> moved here from above again 1.8.0
	IF g_modulecache
	   IF EstrLen(g_modname)
	      mh := findModuleFile(g_modulecache.modlist, g_modname)
	      IF mh
	         IF mh.identification = "ECXM" -> 1.8.0
	            Remove(mh)
	            freeModule(mh)
	         ENDIF
	      ENDIF
	   ENDIF
	ENDIF

	-> 1.8.0
	IF exception
	   IF EstrLen(g_exename) = 0
	      StrCopy(g_exename, FilePart(g_sourcename), StrLen(FilePart(g_sourcename))-2)
	   ENDIF
	   IF EstrLen(g_modname) = 0
	      StrCopy(g_modname, FilePart(g_sourcename), StrLen(FilePart(g_sourcename))-2)
	      StrAdd(g_modname, '.m')
	   ENDIF
	   StrCopy(tempstr, g_destdir)
	   StrAdd(tempstr, g_exename)
	   DeleteFile(tempstr)
	   StrCopy(tempstr, g_destdir)
	   StrAdd(tempstr, g_modname)
	   DeleteFile(tempstr)
	   IF g_codebuf
	      IF g_codeptr > (g_codebuf + codebufsize) THEN WriteF('INTERNAL ERROR: CBUF OVERFLOW\n')
	   ENDIF
	ENDIF


	IF exception = NIL THEN printWarnings()

	IF g_modulecache THEN ReleaseSemaphore(g_modulecache)

	endSupport()  -> 1.6.1

	IF rdargs THEN FreeArgs(rdargs)

	->IF g_quiet = FALSE -> v49 removed 1.7.2

	   SELECT exception
	   CASE "ARGS"  ; WriteF('ERROR: Invalid arguments.\n')
	   CASE "OPEN"  ; WriteF('ERROR: Unable to open file "\s".\n', IF exceptioninfo THEN exceptioninfo ELSE '???')
	   CASE "WRIT"  ; WriteF('ERROR: Unable to write to file "\s".\n', IF exceptioninfo THEN exceptioninfo ELSE '???')
	   CASE "MEM"   ; WriteF('ERROR: Out of memory.\n')
	   CASE "NIL"   ; WriteF('INTERNAL ERROR: NIL at \d\n', exceptioninfo)
	   CASE "STCK"  ; WriteF('INTERNAL ERROR: Ran out of stack! Please notify author.\n')
	   CASE "68K"   ; WriteF('INTERNAL ERROR: 68K, \s, Line: \d\n', exceptioninfo, g_linenum)
	   CASE "PPC"   ; WriteF('INTERNAL ERROR: PPC, \s, Line: \d\n', exceptioninfo, g_linenum)
	   CASE "IDNT"  ; WriteF('ERROR: Module failed identification "\s".\n', IF exceptioninfo THEN exceptioninfo ELSE '???')
	   CASE "SUM"   ; WriteF('ERROR: Checksum failed for module "\s".\n', IF exceptioninfo THEN exceptioninfo ELSE '???')
	   CASE "ERR" -> error has already been printed out..
	   CASE "IBUF"  ; WriteF('INTERNAL ERROR: IBUF OVERFLOW\n')
	   CASE "MODS"  ; WriteF('ERROR: Corrupt module (wrong modsize) "\s"\n', exceptioninfo)
	   CASE "CBUF"  ; WriteF('INTERNAL ERROR: CBUF OVERFLOW buffer=\d, actual=\d\n', codebufsize, g_codeptr - g_codebuf)
	   ->CASE "DBUF"  ; WriteF('INTERNAL ERROR: DBUF OVERFLOW\n')
	   CASE "TRNS"  ; WriteF('INTERNAL ERROR: Module translation failed for module "\s"\n', exceptioninfo)
	   CASE "ABUF"  ; WriteF('INTERNAL ERROR: ABUF OVERFLOW\n') -> argbody
	   CASE "MBUF"  ; WriteF('INTERNAL ERROR: MBUF OVERFLOW\n') -> macrobody
	   CASE "OBUF"  ; WriteF('INTERNAL ERROR: OBUF OVERFLOW\n')
	   CASE "LIB"   ; WriteF('INTERNAL ERROR: Could not open library "\s"!\n', exceptioninfo)
	   CASE "HTC1"  ; WriteF('INTERNAL ERROR: HTC1\n')
	   CASE "HTC2"  ; WriteF('INTERNAL ERROR: HTC2\n')
	   CASE "MCC"   ; WriteF('ERROR: Modulecache is corrupt! (reboot or use IGNORECACHE)\n')
	   CASE "READ"  ; WriteF('ERROR: Unable to read file "\s".\n', exceptioninfo)
	   CASE "EXAM"  ; WriteF('ERROR: Unable to examine file "\s".\n', exceptioninfo)
	   CASE "NFFH"  ; WriteF('ERROR: Unable to get name from fh "\s".\n', exceptioninfo)
	   CASE "FORM"  ; WriteF('ERROR: Bad module format for module "\s".\n', exceptioninfo)
	   CASE "UNCO"  ; WriteF('INTERNAL ERROR: UNCO.\n')
	   DEFAULT
	      IF exception > 0 THEN WriteF('INTERNAL ERROR: UNKNOWN, \s\n', [exception,0])
	   ENDSELECT

	   IF (exception = NIL)
	      DateStamp(ds2)

	      IF g_quiet = FALSE THEN WriteF(':-) \d bytes written in \s seconds.\n', size,
	               RealF(tempstr, Mul(ds2.minute-ds1.minute,TICKS_PER_MINUTE) +
	                     (ds2.tick-ds1.tick) ! / (TICKS_PER_SECOND!), 2))
	   ENDIF

	->ENDIF

	IF wbtofront THEN WbenchToFront()

	IF hold THEN ReadStr(stdin, tempstr)

	process := FindTask(NIL)
	process.result2 := g_linenum

	IF errline
	   IF exception
	      IF g_linenum = 0 THEN RETURN -1
	      RETURN g_linenum
	   ELSE
	      RETURN NIL
	   ENDIF
	ENDIF

ENDPROC IF exception THEN 10 ELSE NIL

-> 2.0, used by initTarget()
PROC getEnvVar(estr, name)
	DEF l
	l := GetVar(name, estr, StrMax(estr), NIL)
	IF l > -1
	   SetStr(estr, l)
	ELSE
	   reportErr('could not read environment variable', name)
	ENDIF
ENDPROC

/*
-> 1.10.0
PROC readConfig()
	DEF l, buf[4096]:ARRAY, temp[100]:STRING
	l := GetVar('ecx.config', buf, 4096, GVF_BINARY_VAR)
	IF l > -1
	   IF getConfigItem(buf, '\ncache-max-size-module', temp)

	   ENDIF
	   IF getConfigItem(buf, '\ncache-max-num-modules', temp)

	   ENDIF
	   IF getConfigItem(buf, '\ncache-max-size-total', temp)

	   ENDIF
	   RETURN TRUE
	ELSE
	   RETURN FALSE
	ENDIF

ENDPROC

PROC getConfigItem(buf, item, data)
	DEF p, e
	p := InStr(buf, item)
	IF p > -1
	   buf := buf + p + StrLen(item)
	   WHILE (buf[] = " ") OR (buf[] = "\t") DO buf++
	   e := InStr(buf, '\n')
	   IF e = -1 THEN e := StrLen(buf)
	   StrCopy(data, buf, e)
	   RETURN TRUE
	ENDIF
ENDPROC FALSE
*/

-> 1.10.0
PROC addModuledirs(dirs)
	DEF p, s
	WHILE dirs[] > 32
	   p := dirs + 1
	   REPEAT
	      WHILE p[] > 32 DO p++
	      IF (p[-1] = ":") OR (p[-1] = "/")
	         s := String(p-dirs)
	         IF s = NIL THEN Raise("MEM")
	         StrCopy(s, dirs,p-dirs)
	         Link(s, g_moduledir)
	         g_moduledir := s
	      ELSEIF p[] = NIL
	         RETURN
	      ELSE
	         p++
	      ENDIF
	   UNTIL p[] < 33
	   dirs := p
	   WHILE dirs[] < 33
	      EXIT dirs[] = NIL
	      dirs++
	   ENDWHILE
	ENDWHILE
ENDPROC

-> 1.8.0
PROC addVersiondateDefine()
	DEF hln:PTR TO hln
	DEF str[100]:STRING, m:PTR TO macro

	-> deprecated
	hln := getLabelHLN('_DATE_')
	NEW m
	m.name := hln.name
	m.identID := IDENT_MACRO
	m.type := 0
	m.nrofargs := 0
	hln.ident := m
	makeVersiondateStr(str)
	m.ascii := String(EstrLen(str))
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, str)

	-> the real name
	hln := getLabelHLN('__AMIGADATE__')
	NEW m
	m.name := hln.name
	m.identID := IDENT_MACRO
	m.type := 0
	m.nrofargs := 0
	hln.ident := m
	makeVersiondateStr(str)
	m.ascii := String(EstrLen(str))
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, str)

ENDPROC

PROC addDateDefine()
	DEF hln:PTR TO hln
	DEF str[100]:STRING, m:PTR TO macro

	hln := getLabelHLN('__DATE__')
	NEW m
	m.name := hln.name
	m.identID := IDENT_MACRO
	m.type := 0
	m.nrofargs := 0
	hln.ident := m
	makeDateStr(str)
	m.ascii := String(EstrLen(str))
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, str)

ENDPROC



PROC addTimeDefine()
	DEF hln:PTR TO hln, str[100]:STRING
	DEF m:PTR TO macro

	NEW m
	hln := getLabelHLN('__TIME__')
	m.name := hln.name
	m.identID := IDENT_MACRO
	m.type := 0
	m.nrofargs := 0
	hln.ident := m

	makeTimeStr(str)

	m.ascii := String(EstrLen(str))
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, str)

ENDPROC



-> 1.8.0
PROC checkHashTable()
	DEF i, hln:PTR TO hln

	FOR i := 0 TO LHASHSIZE-1
	   DEBUGF('HASH INDEX \d:\n', i)
	   hln := itmz_labelhashtable[i]
	   WHILE hln
	      DEBUGF('HLN name="\s" strlen=\d ident1=$\h, ident2=$\h\n',
	         hln.name, StrLen(hln.name), hln.ident, hln.ident2)
	      hln := hln.hnext
	   ENDWHILE
	ENDFOR
ENDPROC

/**************************************************************
***************************************************************
******************** ITEMIZER *********************************
***************************************************************
**************************************************************/

PROC getLabelLen(str)
	DEF c:REG, i:REG
	i := 0
	WHILE (c := str[i])
	   SELECT 128 OF c
	   CASE "0" TO "9", "A" TO "Z", "a" TO "z", "_"
	      i++
	   DEFAULT
	      RETURN i
	   ENDSELECT
	ENDWHILE
ENDPROC i

PROC realloc(ptr)
	DEF len, len2, size, newbuf
	#ifdef DBG_PASS1
	DEBUGF('*** reallocating! ***\n')
	#endif
	len := ptr - itmz_buffer
	len2 := itmz_linestart - itmz_buffer
	size := len + 3 AND $FFFFFC
	newbuf := NEWMEMR(Mul(size,2))
	CopyMemQuick(itmz_buffer, newbuf, size)
	DISPOSE(itmz_buffer)
	itmz_buffer := newbuf
	itmz_bufend := itmz_buffer + Mul(size,2)
	ptr := itmz_buffer + len
	itmz_linestart := itmz_buffer + len2
ENDPROC ptr

PROC itemize(_ascii:PTR TO CHAR, _items:PTR TO item)
	DEF c:REG, l, str[256]:STRING, i, r, v, t, pos, hln:PTR TO hln
	DEF ascii:REG PTR TO CHAR, items:REG PTR TO item
	DEF ident:PTR TO ident, memb:PTR TO member
	DEF ccskip=NIL:REG, ccdepth=NIL
	DEF ccinastr=FALSE, ccinqstr=FALSE -> 1.10.0 needed to fix a bug
	DEF ccskipmode-> 0=old mode, 1=new mode (skip to #endif)
	#ifdef DBG_PASS1
	DEBUGF('itemize($\h,$\h\n', _ascii, _items)
	#endif

	ascii := _ascii
	items := _items

itmz_againhuh:

	WHILE (c := ascii[])
	   IF ccskip
	      SELECT c
	      CASE "#"
	         IF ccinastr OR ccinqstr
	            ascii++
	            JUMP itmz_againhuh
	         ENDIF
	      CASE 10
	         ascii++
	         setLinenum(g_linenum + 1)
	         JUMP itmz_againhuh
	      CASE "\q"
	         IF ccinastr = FALSE
	            ccinqstr := IF ccinqstr THEN FALSE ELSE TRUE
	         ENDIF
	         ascii++
	         JUMP itmz_againhuh
	      CASE "\a"
	         IF ccinqstr = FALSE
	            ccinastr := IF ccinastr THEN FALSE ELSE TRUE
	         ENDIF
	         ascii++
	         JUMP itmz_againhuh
	      DEFAULT
	         ascii++
	         JUMP itmz_againhuh
	      ENDSELECT
	   ENDIF

	   SELECT 128 OF c
	   CASE " ", "\t"
	      ascii++
	   CASE 10, ";"
	      items.data := NIL -> terminate eline
	      items--
	      IF itmz_linestart[1].data = IT_ASM
	         -> 2.1 : fix for 68k inline asm (Ax)+
	         items := parseELine() -> ..
	         IF c = 10 THEN setLinenum(g_linenum + 1)
	         itmz_linestart := items
	         items.data := 10
	         items.info := g_linenum
	         items++
	      ELSE
	         SELECT 256 OF items.data
	         CASE ",", "(", "[", KW_OR, KW_AND, KW_SHL, KW_SHR, "+", "-",
	           KW_DEF, KW_NEW, KW_END, "*", "/", KW_THEN, KW_IS, KW_BUT,
	           KW_CONST, KW_SET, KW_ENUM, KW_USES, KW_STATIC,
	           KW_ASSIGN, KW_MODIFY, KW_ASR, KW_XOR  -> added 1.10.0
	            IF c = 10 THEN setLinenum(g_linenum + 1)
	            items++
	            IF items > (itmz_bufend-(1000*SIZEOF item)) THEN items := realloc(items)
	            items.data := 10
	            items.info := g_linenum
	            items++
	         CASE 10
	            IF c = 10 THEN setLinenum(g_linenum + 1)
	            items.info := g_linenum
	            items++
	         DEFAULT
	            -> 1.5.4: fix for MACROs overrunning buffer
	            IF items > (itmz_bufend-(1000*SIZEOF item)) THEN items := realloc(items)
	            IF itmz_openbrackdepth
	               SELECT 256 OF items.data
	               CASE KW_PROC, KW_ENDPROC,  -> v46
	                 KW_FOR, KW_WHILE, KW_ENDFOR, KW_ENDWHILE,
	                 KW_LOOP, KW_ENDLOOP, KW_ELSEIF, KW_ENDIF,
	                 KW_SELECT, KW_CASE, KW_DEFAULT, KW_ENDSELECT,
	                 KW_OF, KW_TO, KW_STEP, KW_JUMP, KW_INC, KW_DEC,
	                 KW_REPEAT, KW_UNTIL, KW_OBJECT, KW_ENDOBJECT
	                 -> if we end up here, someones forgotten to close with ")" or "]"
	                   -> improved 2.2.3
	                   g_linenum := itmz_lastbrackline
	                   reportErr('"(" or "[" is not terminated')
	               DEFAULT
	                  IF c = 10 THEN setLinenum(g_linenum + 1)
	                  items++
	                  items.data := 10
	                  items.info := g_linenum
	                  items++
	               ENDSELECT
	            ELSE
	               -> v45 simple fix to keep linenum from drifting
	               t := g_linenum
	               items := parseELine() -> ..
	               setLinenum(t + IF c = 10 THEN 1 ELSE 0)
	               itmz_linestart := items
	               items.data := 10
	               items.info := g_linenum
	               items++
	            ENDIF
	         ENDSELECT
	      ENDIF
	      ascii++
	   CASE "a" TO "z", "_"
	      l := getLabelLen(ascii)
	      StrCopy(str, ascii, l)
	      ascii := ascii + l
	      hln := getLabelHLN(str)
	      ident := hln.ident
	      IF ident = NIL
	         items.data := IT_LABEL  -> undefined label or proc or member
	         items.num := 0 -> lowercase
	         items.info := hln
	         items++
	      ELSEIF ident.identID = IDENT_MACRO -> macro ? (v42)
	         IF g_optpreprocess -> check for commandline macros
	            ascii, items := doMacro(hln, ascii, items, g_ppstring)
	         ELSE
	            reportErr('macro needs "OPT PREPROCESS"')
	         ENDIF
	      ELSEIF ident.identID = IDENT_LFUNC -> v46, for "lfunc-iised" proc's ..
	         items.data := IT_LFUNC
	         items.info := ident
	         items++
	      ELSE -> misc lowercase identifier
	         items.data := IT_LABEL
	         items.num := 0 -> lowercase
	         items.info := hln
	         items++
	      ENDIF
	   CASE "A" TO "Z"
	      l := getLabelLen(ascii)
	      StrCopy(str, ascii, l)
	      hln := getLabelHLN(str)
	      ident := hln.ident
	      IF hln.ident2
	         IF (itmz_linestart+SIZEOF item) = items
	            ident := hln.ident2
	            IF ident.identID = IDENT2_ASM
	               items.data := IT_ASM
	               items.info := ident
	               ascii := ascii + l
	               IF ascii[] = "." -> 2.2
	                  ascii++
	                  t := ascii[]
	                  SELECT t
	                  CASE "B"    ; items.num := "B" ; ascii++
	                  CASE "W"    ; items.num := "W" ; ascii++
	                  CASE "L"    ; items.num := "L" ; ascii++
	                  CASE "S"    ; items.num := "S" ; ascii++
	                  CASE "D"    ; items.num := "D" ; ascii++
	                  DEFAULT     ; items.num := "."  -> ppc
	                  ENDSELECT
	               ELSE
	                  items.num := 0
	               ENDIF
	               items++
	            ELSEIF ident.identID = IDENT2_ASMMACRO -> v46
	               ascii, items := doAsmMacro(hln, ascii + l, items, g_ppstring)
	            ELSE
	               reportIErr(' Itemize UC LAB ident2')
	            ENDIF
	         ELSEIF ident
	            IF ident.identID = IDENT_KEYWORD  -> OR, AND, etc
	               items.data := ident::keyword.id
	               items++
	            ELSEIF ident.identID = IDENT_CONST
	               items.data := IT_VALUE
	               items.num := 0
	               items.info := ident::const.value
	               items++
	            ELSE
	               reportIErr(' ident what ?', hln.name)
	            ENDIF
	            ascii := ascii + l
	         ELSE
	            ->reportErr('unknown identifier', hln.name)
	            items.data := IT_LABEL
	            items.num := 1
	            items.info := hln
	            items++
	            ascii := ascii + l
	         ENDIF
	      ELSEIF ident
	         t := ident.identID
	         SELECT t
	         CASE IDENT_KEYWORD -> V46
	            t := ident::keyword.id
	            SELECT t
	            CASE KW_DEF
	               initTarget()
	               initIvars()
	               items.data := t
	               items++
	            CASE KW_PROC
	               initTarget()
	               initIvars()
	               items.data := t
	               items++
	            CASE KW_RAISE
	               initTarget()
	               initIvars()
	               items.data := t
	               items++
	            CASE KW_OBJECT
	               initTarget() -> 1.7
	               items.data := t
	               items++
	            CASE KW_LIBRARY
	               g_librarymode := TRUE
	               initTarget()
	               items.data := t
	               items++
	            CASE KW_OFFSETOF -> 1.5.6
	               items, ascii := parseOffsetof(items,ascii+l) -> 1.8.0
	               l := 0
	            CASE KW_SIZEOF -> moved here 2.2
	               items, ascii := parseSizeof(items, ascii+l)
	               l := 0
	            CASE KW_EXPORT  -> 1.8.0
	               itmz_export := TRUE
	               items--
	               IF items.data = 10
	                  items++
	                  itmz_export := TRUE
	               ELSE -> OPT
	                  items++
	                  items.data := KW_EXPORT
	                  items++
	               ENDIF
	            DEFAULT
	               items.data := t
	               items++
	            ENDSELECT
	            ascii := ascii + l
	         CASE IDENT_CONST
	            items.data := IT_VALUE
	            items.num := 0
	            items.info := ident::const.value
	            items++
	            ascii := ascii + l
	         CASE IDENT_MACRO -> macro ? (v42)
	            IF g_optpreprocess -> we may have a commandline macro v49
	               ascii, items := doMacro(hln, ascii + l, items, g_ppstring)
	            ELSE
	               reportErr('macro needs "OPT PREPROCESS"')
	            ENDIF
	         CASE IDENT_LFUNC -> v44
	            items.data := IT_LFUNC
	            items.info := ident
	            items++
	            ascii := ascii + l
	         CASE IDENT_IFUNC -> v44
	            items.data := IT_IFUNC
	            items.info := ident
	            items++
	            ascii := ascii + l
	         CASE IDENT_REG -> v44
	            items.data := IT_REG
	            items.info := ident
	            ascii := ascii + l
	            IF ascii[] = "." -> 2.2
	                  ascii++
	                  t := ascii[]
	                  SELECT t
	                  CASE "B"    ; items.num := "B" ; ascii++
	                  CASE "W"    ; items.num := "W" ; ascii++
	                  CASE "L"    ; items.num := "L" ; ascii++
	                  CASE "S"    ; items.num := "S" ; ascii++
	                  CASE "D"    ; items.num := "D" ; ascii++
	                  DEFAULT     ; items.num := "."  -> ppc
	                  ENDSELECT
	               ELSE
	                  items.num := 0
	               ENDIF
	            items++
	         DEFAULT -> uppercase label (asmlab ?)
	            items.data := IT_LABEL
	            items.num := 1
	            items.info := hln
	            items++
	            ascii := ascii + l
	         ENDSELECT
	      ELSEIF StrCmp(str, 'MACRO') AND g_optpreprocess -> v45
	         items--
	         hln := items.info
	         IF items.data <> IT_LABEL THEN reportErr('MACRO definition syntax')
	         IF items.num = 0 THEN reportErr('MACRO lexical', hln.name)
	         ascii := parseMACRO(hln, ascii + l)
	      ELSEIF StrCmp(str, 'PRIVATE') -> v45
	         g_privatekey := TRUE
	         ascii := ascii + l
	      ELSEIF StrCmp(str, 'PUBLIC') -> v45
	         g_privatekey := FALSE
	         ascii := ascii + l
	      ELSE -> undefined uppercase label (own asmlab ?)
	         items.data := IT_LABEL
	         items.num := 1
	         items.info := hln
	         items++
	         ascii := ascii + l
	      ENDIF
	   CASE "1" TO "9", "0"
	      IF (c = "0") AND (ascii[1] = "x") -> 2.2.2 support for 0xABC..
	         ascii++
	         ascii[] := "$"
	      ENDIF
	      v, r := Val(ascii)
	      items.data := IT_VALUE
	      items.num := 0
	      IF ascii[r] = "."
	         v,r := RealVal(ascii)
	         items.num := 1
	         IF r = NIL THEN reportErr('illegal float value')
	         #ifndef ECX_VERSION
	         IF r > 10 THEN reportErr('too many digits for float value')
	         #endif
	      ENDIF
	      ascii := ascii + r
	      items.info := v
	      items++
	   CASE "."          -> float value/"."/"..." ?
	      v := ascii[1]
	      SELECT 128 OF v
	      CASE "0" TO "9"
	         /*get value and set type to IT_FVALUE */
	         items.data := IT_VALUE
	         items.num := 1
	         v,r := RealVal(ascii)
	         IF r = NIL THEN reportErr('illegal float value')
	         #ifndef ECX_VERSION
	         IF r > 10 THEN reportErr('too many digits for float value')
	         #endif
	         items.info := v
	         ascii := ascii + r
	         items++
	      DEFAULT
	         ascii++
	         items.data := "."
	         items++
	      ENDSELECT
	   CASE "\a" -> 1.10.0 rewritten, now slaps together strings by itself.
	      items-- -> and yes, we HAVE to look back for this.
	      IF items.data = 10 THEN items--
	      IF items.data = "+"
	         items--
	         IF items.data = IT_STRING
	            StrCopy(g_tempstr, items.info)
	            endString(items.info)
	         ELSE
	            items++
	            items++
	            SetStr(g_tempstr, 0)
	         ENDIF
	      ELSE
	         SetStr(g_tempstr, 0)
	         items++
	      ENDIF
	      ascii := lex_string(ascii+1, g_tempstr)
	      l := EstrLen(g_tempstr)
	      IF l
	         items.info := newString(l)
	         CopyMem(g_tempstr, items.info, l) -> StrCopy() doesnt handle '\0' !
	         SetStr(items.info,l)
	      ELSE
	         items.info := newString(0) -> yes
	      ENDIF
	      itmz_stringssize := itmz_stringssize + l + 3 -> add to codesize
	      items.data := IT_STRING
	      items.num := NIL -> l
	      items++
	      ident_STRLEN.value := l
	   CASE "\q"
	      ascii++
	      t, ascii := qVal(ascii)
	      IF ascii[]++ <> "\q" THEN reportErr('\q expected')
	      items.info := t
	      items.data := IT_VALUE
	      items.num := 0
	      items++
	   CASE "%","$" -> binary/hexa value
	      /*get value and set type to IT_VALUE */
	      items.data := IT_VALUE
	      items.num := 0
	      v,r := Val(ascii)
	      IF r = NIL THEN reportErr('$ or % value failed')
	      items.info := v
	      ascii := ascii + r
	      items++
	   CASE "-"
	      IF ascii[1] = "-"
	         ascii := ascii + 2
	         items.data := KW_MINMIN
	         items++
	      ELSE
	         items.data := "-"
	         items++
	         ascii++
	      ENDIF
	   CASE ":"
	      IF ascii[1] = "=" -> assign
	         ascii := ascii + 2
	         items.data := KW_ASSIGN ;
	      ELSEIF ascii[1] = ":" -> ptr-type
	         ascii := ascii + 2
	         items.data := KW_PTYPE ;
	      ELSE -> just ":"
	         ascii++
	         items.data := ":" ;
	      ENDIF
	      items++
	   CASE "<"
	      IF ascii[1] = "="
	         IF ascii[2] = ">"
	            items.data := KW_UNI ;
	            ascii := ascii + 3
	         ELSE
	           ascii := ascii + 2
	           items.data := KW_ISLE ;
	        ENDIF
	      ELSEIF ascii[1] = ">"
	         ascii := ascii + 2
	         items.data := KW_ISNE ;
	      ELSEIF ascii[1] = "<" -> 2.0
	         ascii := ascii + 2
	         items.data := KW_SHL
	      ELSE
	         ascii++
	         items.data := "<" ;
	      ENDIF
	      items++
	   CASE ">"
	      IF ascii[1] = "=" -> Greater or Equal
	         ascii := ascii + 2
	         items.data := KW_ISGE ;
	      ELSEIF ascii[1] = ">" -> 1.10.0
	         ascii := ascii + 2
	         items.data := KW_ASR
	      ELSE -> just ">"
	         ascii++
	         items.data := ">"
	      ENDIF
	      items++
	   CASE "+"
	      IF ascii[1] = "+" -> increment
	         ascii := ascii + 2
	         items.data := KW_PLUSPLUS ;
	      ELSE
	         ascii++
	         items.data := "+" ;
	      ENDIF
	      items++
	   CASE "]", ")"
	      items--
	      t := items.data
	      SELECT 256 OF t  -> ,] type errors v49
	      CASE ",", "+", "-", "/", "*", KW_SHR, KW_SHL, KW_AND, KW_OR, KW_ASR, KW_XOR, KW_MODIFY
	         reportErr('looks like missing expression')
	      ENDSELECT
	      IF t <> 10 THEN items++
	      items.data := c ;
	      items++
	      ascii++
	      itmz_openbrackdepth--
	      IF itmz_openbrackdepth < 0 THEN reportErr('")" or "]" is unmatched')
	   CASE "[", "("
	      itmz_lastbrackline := g_linenum  -> 2.2.3
	      items.data := c  ;
	      items++
	      ascii++
	      itmz_openbrackdepth++
	   CASE "{", "}", "!", ",", "@", "`", "~", "*", "/"
	      items.data := c
	      items++
	      ascii++
	   CASE "="
	      items--
	      SELECT 256 OF items.data
	      CASE "+"       ; items.data := KW_MODIFY ; items.info := "+"
	      CASE "-"       ; items.data := KW_MODIFY ; items.info := "-"
	      CASE "*"       ; items.data := KW_MODIFY ; items.info := "*"
	      CASE "/"       ; items.data := KW_MODIFY ; items.info := "/"
	      CASE KW_AND    ; items.data := KW_MODIFY ; items.info := KW_AND
	      CASE KW_OR     ; items.data := KW_MODIFY ; items.info := KW_OR
	      CASE KW_SHL    ; items.data := KW_MODIFY ; items.info := KW_SHL
	      CASE KW_SHR    ; items.data := KW_MODIFY ; items.info := KW_SHR
	      CASE KW_XOR    ; items.data := KW_MODIFY ; items.info := KW_XOR
	      CASE KW_ASR    ; items.data := KW_MODIFY ; items.info := KW_ASR
	      DEFAULT        ; items++ ; items.data := "="
	      ENDSELECT
	      items++
	      ascii++
	   CASE "#"
	      ascii++
	      IF g_optpreprocess
	         WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++ -> v45
	         IF     StrCmp(ascii, 'define', STRLEN)
	            ascii := ascii + STRLEN
	            IF ccskip = FALSE THEN ascii := parseDefine(ascii)
	         ELSEIF StrCmp(ascii, 'ifdef', STRLEN)
	            ascii := ascii + STRLEN
	            ccdepth++
	            IF ccskip = FALSE-> not inside another false if ?
	               WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	               l := getLabelLen(ascii)
	               IF l
	                  StrCopy(str, ascii, l)
	                  hln := getLabelHLN(str)
	                  recordCondSym(hln.name) -> 2.2
	                  ascii := ascii + l
	                  IF hln.ident = NIL -> not defined ?
	                     ccskip := ccdepth -> so we know what depth is skipping
	                     ccskipmode := 0
	                  ENDIF
	               ELSE
	                  reportErr('preprocessor symbol expected')
	               ENDIF
	            ENDIF
	         ELSEIF StrCmp(ascii, 'ifndef', STRLEN)
	            ascii := ascii + STRLEN
	            ccdepth++
	            IF ccskip = FALSE-> not inside another false if ?
	               WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	               l := getLabelLen(ascii)
	               IF l
	                  StrCopy(str, ascii, l)
	                  hln := getLabelHLN(str)
	                  recordCondSym(hln.name) -> 2.2
	                  ascii := ascii + l
	                  IF hln.ident -> defined ?
	                     ccskip := ccdepth
	                     ccskipmode := 0
	                  ENDIF
	               ELSE
	                  reportErr('preprocessor symbol expected')
	               ENDIF
	            ENDIF
	         ELSEIF StrCmp(ascii, 'if', STRLEN) -> 1.10.0
	            ascii := ascii + STRLEN
	            ccdepth++
	            IF ccskip = FALSE-> not inside another false if ?
	               ascii, t := cc_if(ascii)
	               IF t = FALSE
	                  ccskip := ccdepth
	                  ccskipmode := 0
	               ENDIF
	            ENDIF
	         ELSEIF StrCmp(ascii, 'elifndef', STRLEN) -> 2.0
	            ascii := ascii + STRLEN
	            IF ccdepth = FALSE THEN reportErr('#elifndef without #if[(n)def]')
	            IF ccskip
	               IF ccskipmode = 0
	                  WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	                  l := getLabelLen(ascii)
	                  IF l
	                     StrCopy(str, ascii, l)
	                     hln := getLabelHLN(str)
	                     recordCondSym(hln.name) -> 2.2
	                     ascii := ascii + l
	                     IF hln.ident = NIL
	                        IF ccskip = ccdepth
	                           ccskip := FALSE
	                        ENDIF
	                     ENDIF
	                  ELSE
	                     reportErr('preprocessor symbol expected')
	                  ENDIF
	               ENDIF
	            ELSE
	               ccskip := ccdepth
	               ccskipmode := 1
	            ENDIF
	         ELSEIF StrCmp(ascii, 'elifdef', STRLEN) -> 2.0
	            ascii := ascii + STRLEN
	            IF ccdepth = FALSE THEN reportErr('#elifdef without #if[(n)def]')
	            IF ccskip
	               IF ccskipmode = 0
	                  WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	                  l := getLabelLen(ascii)
	                  IF l
	                     StrCopy(str, ascii, l)
	                     hln := getLabelHLN(str)
	                     recordCondSym(hln.name) -> 2.2
	                     ascii := ascii + l
	                     IF hln.ident
	                        IF ccskip = ccdepth
	                           ccskip := FALSE
	                        ENDIF
	                     ENDIF
	                  ELSE
	                     reportErr('preprocessor symbol expected')
	                  ENDIF
	               ENDIF
	            ELSE
	               ccskip := ccdepth
	               ccskipmode := 1
	            ENDIF
	         ELSEIF StrCmp(ascii, 'elif', STRLEN) -> 1.10.0
	            ascii := ascii + STRLEN
	            IF ccdepth = FALSE THEN reportErr('#elif without #if[(n)def]')
	            IF ccskip
	               IF ccskipmode = 0
	                  ascii, t := cc_if(ascii)
	                  IF t
	                     IF ccskip = ccdepth
	                        ccskip := FALSE
	                     ENDIF
	                  ENDIF
	               ENDIF
	            ELSE
	               ccskip := ccdepth
	               ccskipmode := 1
	            ENDIF
	         ELSEIF StrCmp(ascii, 'else', STRLEN)
	            ascii := ascii + STRLEN
	            IF ccdepth = FALSE THEN reportErr('#else without #if[(n)def]')
	            IF ccskip
	               IF ccskipmode = 0
	                  IF ccskip = ccdepth THEN ccskip :=  FALSE
	               ENDIF
	            ELSE
	               ccskip := ccdepth
	            ENDIF
	         ELSEIF StrCmp(ascii, 'endif', STRLEN)
	            ascii := ascii + STRLEN
	            IF ccdepth = FALSE THEN reportErr('#endif without #if[(n)def]')
	            IF ccskip = ccdepth THEN ccskip := FALSE
	            ccdepth--
	         ELSEIF StrCmp(ascii, 'fmtstr', STRLEN)   -> 1.10.0
	            IF ccskip = FALSE
	               ascii := ascii + STRLEN
	               ascii := parseFmtStr(ascii)
	            ENDIF
	         ELSEIF StrCmp(ascii, 'error', STRLEN)    -> 1.10.0
	            IF ccskip = FALSE
	               ascii := ascii + STRLEN
	               WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	               IF ascii[]++ <> "\a" THEN reportErr('string expected')
	               SetStr(g_tempstr, 0)
	               ascii := lex_string(ascii, g_tempstr)
	               reportErr('#error')
	            ENDIF
	         ELSEIF StrCmp(ascii, 'warning', STRLEN) -> 1.10.0
	            IF ccskip = FALSE
	               ascii := ascii + STRLEN
	               WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	               IF ascii[]++ <> "\a" THEN reportErr('string expected')
	               ->StrCopy(g_tempstr, '#warning \a')
	               ascii := lex_string(ascii, g_tempstr)
	               WriteF('#warning at line \d: \a\s\a\n', g_linenum, g_tempstr)
	               ->StrAdd(g_tempstr, '\a')
	               ->addWarning(g_tempstr)
	            ENDIF
	         ELSEIF ccskip = FALSE
	            items.data := "#" -> 68k asm #val
	            items++
	         ENDIF
	      ELSE
	         items.data := "#" -> 68k asm #val
	         items++
	      ENDIF

	   DEFAULT
	      StringF(str, 'ascii \d', c)
	      reportErr('foreign character encountered', str)
	   ENDSELECT

	ENDWHILE

	IF ccdepth > 0 THEN reportErr('missing #endif')

	#ifdef DBG_PASS1
	DEBUGF('itemize() DONE\n')
	#endif

ENDPROC items

-> 2.2
-> SIZEOF LONG/INT/CHAR/etc
-> SIZEOF object
-> SIZEOF var[]
-> SIZEOF {var}
-> SIZEOF (var)
-> SIZEOF (static)
PROC parseSizeof(items:PTR TO item, ascii)
	DEF t, l, str[256]:STRING, obj:PTR TO object, var:PTR TO var, hln:PTR TO hln
	DEF type:PTR TO member, cmplx, lab:PTR TO statlab, macro=NIL:PTR TO macro, len, end

	#ifdef DBG_PASS1
	DEBUGF('parseSizeof($\h, $\h)\n', items, ascii)
	#endif

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	t := ascii[]
	SELECT 128 OF t
	CASE "a" TO "z", "A" TO "Z", "_"
	   -> might be preprocessor symbol
	   len := getLabelLen(ascii)
	   StrCopy(str, ascii, len)
	   hln := getLabelHLN(str)
	   macro := hln.ident
	   IF macro
	      IF macro.identID = IDENT_MACRO
	         end := ascii + len
	         ascii := macro.ascii
	         WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	      ELSE
	         macro := NIL
	      ENDIF
	   ENDIF
	ENDSELECT

	t := ascii[]
	SELECT 128 OF t
	CASE "A" TO "Z"
	   IF     StrCmp(ascii, 'VECTOR', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 16
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'DOUBLE', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 8
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'WIDE', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 8
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'REAL', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := REALSIZE
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'PTR', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := PTRSIZE
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'LONG', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 4
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'INT', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 2
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'CHAR', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 1
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'FLOAT', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 4
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'WORD', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 2
	      ascii := ascii + STRLEN
	   ELSEIF StrCmp(ascii, 'BYTE', STRLEN)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := 1
	      ascii := ascii + STRLEN
	   ELSE
	      reportErr('SIZEOF failed')
	   ENDIF
	CASE "("
	   ascii++
	   l := getLabelLen(ascii)
	   IF l = 0 THEN reportErr('SIZEOF failed')
	   StrCopy(str, ascii, l)
	   hln := getLabelHLN(str)
	   var := hln.ident
	   IF var = NIL THEN reportErr('unknown identifier for SIZEOF', hln.name)
	   IF var.identID = IDENT_VARIABLE
	      type := var.type
	      cmplx := var.cmplx
	      IF type.esize = 255
	         obj := type.object
	         items.data := IT_VALUE
	         items.num := 0
	         items.info := Mul(obj.sizeof, type.numes)
	      ELSEIF (cmplx = TRUE) AND (type.esize = 1)
	         items.data := IT_VALUE
	         items.num := 0
	         items.info := Mul(type.esize, type.numes) + 1
	      ELSE
	         items.data := IT_VALUE
	         items.num := 0
	         items.info := Mul(type.esize, type.numes)
	      ENDIF
	   ELSEIF var.identID = IDENT_LABEL
	      lab := var
	      IF lab.ltype <> LTYPE_STATIC THEN reportErr('wrong type of label for SIZEOF', hln.name)
	      items.data := IT_VALUE
	      items.num := 0
	      items.info := lab.sizeof
	   ELSE
	      reportErr('wrong type of identifier for SIZEOF', hln.name)
	   ENDIF
	   ascii := ascii + l
	   IF ascii[]++ <> ")" THEN reportErr('")" expected')
	CASE "{"
	   ascii++
	   l := getLabelLen(ascii)
	   IF l = 0 THEN reportErr('SIZEOF failed')
	   StrCopy(str, ascii, l)
	   hln := getLabelHLN(str)
	   var := hln.ident
	   IF var = NIL THEN reportErr('unknown identifier for SIZEOF', hln.name)
	   IF var.identID <> IDENT_VARIABLE THEN reportErr('variable expected for SIZEOF', hln.name)
	   items.data := IT_VALUE ; items.num := 0 ; items.info := var.type.size
	   ascii := ascii + l
	   IF ascii[]++ <> "}" THEN reportErr('"}" expected')
	CASE "a" TO "z", "_"
	   l := getLabelLen(ascii)
	   StrCopy(str, ascii, l)
	   hln := getLabelHLN(str)
	   ascii := ascii + l
	   IF ascii[] = "["
	      ascii++
	      var := hln.ident
	      IF var = NIL THEN reportErr('unknown identifier for SIZEOF', hln.name)
	      IF var.identID <> IDENT_VARIABLE THEN reportErr('variable expected for SIZEOF', hln.name)
	      IF var.type.esize <> 255
	         items.data := IT_VALUE ; items.num := 0 ; items.info := var.type.esize
	      ELSE
	         obj := var.type.object
	         items.data := IT_VALUE ; items.num := 0 ; items.info := obj.sizeof
	      ENDIF
	      IF ascii[]++ <> "]" THEN reportErr('"]" expected')
	   ELSE
	      obj := hln.ident2
	      IF obj = NIL THEN reportErr('unknown identifier for SIZEOF', hln.name)
	      IF obj.identID <> IDENT2_OBJECT THEN reportErr('object expected for SIZEOF', hln.name)
	      items.data := IT_VALUE ; items.num := 0 ; items.info := obj.sizeof
	   ENDIF
	DEFAULT
	   reportErr('SIZEOF failed')
	ENDSELECT
	items++
	IF macro
	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	   IF ascii[] THEN reportErr('unexpected characters')
	   ascii := end
	ENDIF
ENDPROC items, ascii

-> 1.10.0
PROC cc_if(ascii)
	DEF l, hln:PTR TO hln, t, pos, str[256]:STRING

	#ifdef DBG_PP
	DEBUGF('cc_if() ')
	#endif

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	l := getLabelLen(ascii)
	IF l
	   StrCopy(str, ascii, l)
	   #ifdef DBG_PP
	   DEBUGF('\s, ', str)
	   #endif
	   hln := getLabelHLN(str)
	   ascii := ascii + l
	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	   IF hln.ident = NIL THEN reportErr('unknown preprocessor symbol', str)
	   IF hln.ident.identID <> IDENT_MACRO THEN reportErr('preprocessor symbol expected', str)
	   IF StrCmp(ascii, '=', STRLEN)
	      ascii := ascii + STRLEN
	      t := 0
	   ELSEIF StrCmp(ascii, '<>', STRLEN)
	      ascii := ascii + STRLEN
	      t := 1
	   ELSE
	      reportErr('preprocessor syntax')
	   ENDIF
	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	   pos := InStr(ascii, '\n')
	   WHILE (ascii[pos-1] = " ") OR (ascii[pos-1] = "\t") DO pos--
	   StrCopy(str, ascii, pos)
	   #ifdef DBG_PP
	   DEBUGF('\s\n', str)
	   #endif
	   ascii := ascii + pos
	   IF t = 0
	      RETURN ascii, StrCmp(hln.ident::macro.ascii, str)
	   ELSE
	      RETURN ascii, Not(StrCmp(hln.ident::macro.ascii, str))
	   ENDIF
	ELSE
	   reportErr('preprocessor symbol expected')
	ENDIF
ENDPROC


-> 1.10.0
PROC parseFmtStr(ascii)
	DEF tstr[256]:STRING, fmtstr[1024]:STRING, l, hln:PTR TO hln, m:PTR TO macro, t
	DEF valuesarray[256]:ARRAY OF LONG, numvalues=0, s, pos, p:PTR TO LONG
	DEF ident:PTR TO ident
	WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++
	l := getLabelLen(ascii)
	IF l = 0 THEN reportErr('preprocessor symbol expected')
	StrCopy(tstr, ascii, l)
	hln := getLabelHLN(tstr)
	IF hln.ident THEN reportErr('symbol is already defined', hln.name)
	NEW m
	hln.ident := m
	m.name := hln.name
	m.identID := IDENT_MACRO
	IF itmz_export OR g_optexport
	   m.next := g_emacrolist
	   g_emacrolist := m
	   itmz_export := FALSE
	ENDIF
	ascii := ascii + l
	WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++
	IF ascii[]++ <> "'" THEN reportErr('string expected')
	StrCopy(fmtstr, '\a')
	ascii := lex_string(ascii, fmtstr)
	StrAdd(fmtstr, '\a')
	WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++
	WHILE (t := ascii[]) <> 10
	   EXIT t = NIL
	   IF t = "\\"
	      ascii++
	      WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++
	      IF ascii[] = 10
	         setLinenum(g_linenum + 1)
	         ascii++
	      ENDIF
	   ELSE
	      l := getLabelLen(ascii)
	      IF l = 0 THEN reportErr('#fmtstr syntax')
	      StrCopy(tstr, ascii, l)
	      ascii := ascii + l
	      hln := getLabelHLN(tstr)
	      ident := hln.ident
	      IF ident = NIL THEN reportErr('unknown identifier', hln.name)
	      IF ident.identID = IDENT_CONST
	         valuesarray[numvalues++] := ident::const.value
	      ELSEIF ident.identID = IDENT_MACRO
	         t := TrimStr(ident::macro.ascii)
	         IF t[] = "\a"
	            t++
	            pos := InStr(t, '\a')
	            IF pos < 0 THEN pos := StrLen(t)
	            StrCopy(g_tempstr, t, pos)
	         ELSE
	            StrCopy(g_tempstr, t)
	         ENDIF
	         s := String(EstrLen(g_tempstr))
	         IF s = NIL THEN Raise("MEM")
	         StrCopy(s, g_tempstr)
	         valuesarray[numvalues++] := s
	      ELSE
	         reportErr('wrong type of identifier for #fmtstr', hln.name)
	      ENDIF
	   ENDIF
	   WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++
	ENDWHILE

	p := valuesarray
	StringF(g_tempstr, fmtstr,
	   p[]++, p[]++, p[]++, p[]++,
	   p[]++, p[]++, p[]++, p[]++,
	   p[]++, p[]++, p[]++, p[]++,
	   p[]++, p[]++, p[]++, p[]++)

	s := String(EstrLen(g_tempstr))
	IF s = NIL THEN Raise("MEM")
	StrCopy(s, g_tempstr)

	m.ascii := s

ENDPROC ascii

-> 2.2
PROC recordCondSym(name)
	DEF cs
	IF g_showcondsyms = FALSE THEN RETURN
	cs := g_condsymnames
	WHILE cs
	   IF StrCmp(cs, name)
	      RETURN
	   ENDIF
	   cs := Next(cs)
	ENDWHILE
	cs := String(StrLen(name))
	StrCopy(cs, name)
	Link(cs, g_condsymnames)
	g_condsymnames := cs
ENDPROC

-> 2.2
PROC showCondSyms()
	DEF cs
	cs := g_condsymnames
	IF cs THEN WriteF('Conditional symbols: ')
	WHILE cs
	   WriteF('\s\c ', cs, IF Next(cs) THEN "," ELSE " ")
	   cs := Next(cs)
	ENDWHILE
	IF g_condsymnames THEN WriteF('\n')
ENDPROC

-> 1.8.0
PROC parseOffsetof(items:PTR TO item, ascii)
	DEF l, hln:PTR TO hln, ident:PTR TO ident
	DEF memb:PTR TO member, str[256]:STRING, offset
	-> skip keyword and white space
	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	-> get object name
	l := getLabelLen(ascii)
	IF l = 0 THEN reportErr('OFFSETOF expected object')
	StrCopy(str, ascii, l)
	-> find object
	hln := getLabelHLN(str)
	ident := hln.ident2
	IF ident = NIL THEN reportErr('unknown object for OFFSETOF', str)
	IF ident.identID <> IDENT2_OBJECT THEN reportErr('identifier is not object', str)
	-> skip object name and white space
	ascii := ascii + l
	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	-> skip "." and skip more whitespace
	IF ascii[]++ <> "." THEN reportErr('"." expected')
	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	-> get member name
	l := getLabelLen(ascii)
	IF l = 0 THEN reportErr('OFFSETOF expected member')
	StrCopy(str, ascii, l)
	ascii := ascii + l
	-> find member
	hln := getLabelHLN(str)
	memb := findMember(ident, hln.name)
	IF memb = NIL THEN reportErr('unknown member for OFFSETOF', str)
	offset := memb.offset
	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	WHILE ascii[] = "." -> embedded members ?
	   ascii++
	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	   IF memb.size <> 0 THEN reportErr('OFFSETOF cannot dereference this member', memb.name)
	   IF memb.object = NIL THEN reportErr('OFFSETOF cannot dereference this member', memb.name)
	   -> get member name
	   l := getLabelLen(ascii)
	   IF l = 0 THEN reportErr('OFFSETOF expected member')
	   StrCopy(str, ascii, l)
	   ascii := ascii + l
	   hln := getLabelHLN(str)
	   memb := findMember(memb.object, hln.name)
	   IF memb = NIL THEN reportErr('unknown member for OFFSETOF', hln.name)
	   offset := offset + memb.offset
	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	ENDWHILE
	items.info := offset
	items.num := 0
	items.data := IT_VALUE
	items++
ENDPROC items, ascii, 0

-> 1.5.4
-> removes comments from source "in place", leaves newlines.
-> 2.1: now also converts "" or '' inside strings into \q and \a
PROC uncommentSource(source:PTR TO CHAR)
	DEF incomment=FALSE:REG
	DEF dest:REG PTR TO CHAR
	DEF c:REG, linenum=1, pos
	DEF inastring=FALSE, inqstring=FALSE, astrline, qstrline, mcommentline

	#ifdef DBG_PASS1
	DEBUGF('uncommentSource($\h)\n', source)
	#endif

	dest := source

	WHILE c := source[]++

	   IF inastring OR inqstring
	      IF c = 10
	         linenum++
	      ELSEIF c = "\a"
	         IF inqstring = FALSE
	            IF source[] = "\a"  -> 2.1 !
	               source++
	               c := "a"
	               dest[]++ := "\\"
	            ELSE
	               inastring := FALSE
	            ENDIF
	         ENDIF
	      ELSEIF c = "\q"
	         IF inastring = FALSE
	            IF source[] = "\q"  -> 2.1 !
	               source++
	               c := "q"
	               dest[]++ := "\\"
	            ELSE
	               inqstring := FALSE
	            ENDIF
	         ENDIF
	      ENDIF
	      dest[]++ := c
	   ELSEIF incomment
	      SELECT c
	      CASE "/"
	         IF source[] = "*"
	            source++
	            incomment++
	            IF incomment = 1 THEN dest[]++ := " " -> 1.10.0
	         ENDIF
	      CASE "*"
	         IF source[] = "/"
	            source++
	            incomment--
	         ENDIF
	      CASE 10
	         dest[]++ := c
	         linenum++
	      ENDSELECT
	   ELSEIF c = "/"
	      IF source[] = "*"
	         source++
	         incomment := 1
	         mcommentline := linenum
	      ELSE
	         dest[]++ := c
	      ENDIF
	   ELSEIF c = "*"
	      IF source[] = "/"
	         setLinenum(linenum)
	         reportErr('end of multi line comment without start')
	      ELSE
	         dest[]++ := c
	      ENDIF
	   ELSEIF c = "-"
	      IF source[-2] = "-" -> avoid x-->y problem 1.7.1
	         dest[]++ := c
	      ELSEIF source[] = ">"
	         pos := InStr(source, '\n')
	         source := source + IF pos > -1 THEN pos ELSE StrLen(source)
	      ELSE
	         dest[]++ := c
	      ENDIF
	   ELSEIF c = "\a"
	      inastring := TRUE
	      astrline := linenum
	      dest[]++ := c
	   ELSEIF c = "\q"
	      inqstring := TRUE
	      qstrline := linenum
	      dest[]++ := c
	   ELSEIF c = 160 -> nobrake space, turn into space
	      dest[]++ := " "
	   ELSEIF c = 13 -> carriage return, merge with following newline, else error
	      c := source[]++
	      IF c <> 10 THEN reportErr('carriage return without newline')
	      dest[]++ := 10
	   ELSE
	      IF c = 10 THEN linenum++
	      dest[]++ := c
	   ENDIF

	ENDWHILE

	IF dest[] <> 10 THEN dest[]++ := 10 -> safety newline 1.8.0
	dest[]++ := NIL -> terminate

	IF incomment > 0
	   setLinenum(mcommentline)
	   reportErr('start of multi line comment without end')
	ENDIF

	IF inastring
	   setLinenum(astrline)
	   reportErr('string not terminated')
	ENDIF

	IF inqstring
	   setLinenum(qstrline)
	   reportErr('string value not terminated')
	ENDIF

ENDPROC dest

PROC qVal(str)
	DEF t,val=NIL, a, x

	FOR a := 0 TO 3

	   x := str[]
	   SELECT x
	   CASE "\\"
	      str++
	      x := str[]++
	      SELECT 128 OF x
	      CASE "t"  ; t := "\t"
	      CASE "n"  ; t := "\n"
	      CASE "q"  ; t := "\q"
	      CASE "a"  ; t := "\a"
	      CASE "0" TO "9"  ; t := x - "0"
	      CASE "\\" ; t := "\\"
	      CASE "e"  ; t := "\e"
	      CASE "b"  ; t := "\b"
	      DEFAULT   ; reportErr('unsupported formatcode inside value string', [x,0]:CHAR)
	      ENDSELECT
	   CASE "\q"
	      RETURN val, str
	   CASE 10
	      reportErr('value string syntax')
	   CASE 0
	      reportErr('value string syntax')
	   DEFAULT
	      t := str[]++
	   ENDSELECT

	   val := Shl(val, 8)
	   PutChar({val}+3, t)

	ENDFOR

ENDPROC val, str


/**************************************************************
***************************************************************
******************** INTERNALS INIT ***************************
***************************************************************
**************************************************************/

#define addKEY(name,id) _addKEY([name,[IDENT_KEYWORD,id]:keyword,NIL,NIL]:hln)

PROC _addKEY(hln:PTR TO hln)
	DEF i
	i := hashfunc(hln.name)
	hln.hnext := itmz_labelhashtable[i]
	itmz_labelhashtable[i] := hln
ENDPROC

-> keywords MUST be added before asm-instructions!
PROC addKeywords()

	addKEY('Abs', KW_ABS) -> 2.2: now operator!
	addKEY('AND', KW_AND) -> also asm instruction !
	addKEY('ARRAY', KW_ARRAY)
	addKEY('AS', KW_AS) -> 1.8.0, LINKOBJECT mode
	addKEY('BUT', KW_BUT)
	addKEY('BYTE', KW_BYTE) -> v49 8bit signed !
	addKEY('CASE', KW_CASE)
	addKEY('CHAR', KW_CHAR)
	addKEY('CLASS', KW_CLASS)
	addKEY('CONST', KW_CONST)
	addKEY('DEC', KW_DEC)
	addKEY('DEFAULT', KW_DEFAULT)
	addKEY('DO', KW_DO)
	addKEY('DOUBLE', KW_DOUBLE)
	addKEY('DEF', KW_DEF)     -> initTarget !
	addKEY('END', KW_END)
	addKEY('ENDFOR', KW_ENDFOR)
	addKEY('ENDLOOP', KW_ENDLOOP)
	addKEY('ENDWHILE', KW_ENDWHILE)
	addKEY('ELSE', KW_ELSE)
	addKEY('ELSEIF', KW_ELSEIF)
	addKEY('ELSEIFN', KW_ELSEIFN) -> 1.10.0
	addKEY('ENDIF', KW_ENDIF)
	addKEY('ENDSELECT', KW_ENDSELECT)
	addKEY('ENDOBJECT', KW_ENDOBJECT)
	addKEY('ENUM', KW_ENUM)
	addKEY('ENDPROC', KW_ENDPROC)
	addKEY('EXIT', KW_EXIT)
	addKEY('EXITN', KW_EXITN) -> 1.10.0
	addKEY('EXPORT', KW_EXPORT)
	addKEY('EXCEPT', KW_EXCEPT)
	addKEY('FLOAT', KW_FLOAT) -> v49, back AGAIN
	addKEY('FOR', KW_FOR)
	addKEY('HANDLE', KW_HANDLE)
	addKEY('IF', KW_IF)
	addKEY('IFN', KW_IFN) -> 1.10.0
	addKEY('INC', KW_INC)
	addKEY('INCBIN', KW_INCBIN)
	addKEY('INT', KW_INT)
	addKEY('IS', KW_IS)
	addKEY('JUMP', KW_JUMP)
	addKEY('LIBRARY', KW_LIBRARY)
	addKEY('LINKOBJECT', KW_LINKOBJECT) -> 1.7.1
	addKEY('LIST', KW_LIST)
	addKEY('LOOP', KW_LOOP)
	addKEY('LONG', KW_LONG)
	addKEY('MODULE', KW_MODULE)  -> initTarget in *parseELine* !
	addKEY('NEW', KW_NEW)
	addKEY('Not', "~") -> 2.2 now operator!
	addKEY('NOP', KW_NOP) -> also instruction !
	addKEY('OR', KW_OR) -> also instruction !
	addKEY('OF', KW_OF)
	addKEY('OFFSETOF', KW_OFFSETOF) -> 1.5.6
	addKEY('OPT', KW_OPT)
	addKEY('OBJECT', KW_OBJECT)
	addKEY('PTR', KW_PTR)
	addKEY('PROC', KW_PROC)    -> initTarget !
	addKEY('REPEAT', KW_REPEAT)
	addKEY('RETURN', KW_RETURN)
	addKEY('REG', KW_REG)
	addKEY('REAL', KW_REAL) -> NEW V50!
	addKEY('RAISE', KW_RAISE)    -> initTarget !
	addKEY('SIZEOF', KW_SIZEOF)
	addKEY('STEP', KW_STEP)
	addKEY('SELECT', KW_SELECT)
	addKEY('STATIC', KW_STATIC) -> 1.9.0
	addKEY('STRING', KW_STRING)
	addKEY('SET', KW_SET)
	addKEY('SUPER', KW_SUPER)
	addKEY('SHL', KW_SHL)
	addKEY('SHR', KW_SHR)
	addKEY('THEN', KW_THEN)
	addKEY('TO', KW_TO)
	addKEY('ULONG', KW_ULONG) -> v50, future vector subtype
	addKEY('USES', KW_USES) -> 1.7.1, for LINKOBJECT
	addKEY('UNTIL', KW_UNTIL)
	addKEY('UNTILN', KW_UNTILN) -> 1.10.0
	addKEY('UWIDE', KW_UWIDE) -> 1.6.1
	addKEY('VOID', KW_VOID)     -> V47
	addKEY('VECTOR', KW_VECTOR)     -> V48
	addKEY('WIDE', KW_WIDE) ->1.6.1
	addKEY('WHILE', KW_WHILE)
	addKEY('WHILEN', KW_WHILEN) -> 1.10.0
	addKEY('WORD', KW_WORD) -> v49 16bit unsigned  !
	addKEY('QUAD', KW_QUAD) -> v50 NOT USED
	addKEY('XOR', KW_XOR) -> v55

ENDPROC

-> v44
ifuncs68data:
	INCBIN 'ec68kifuncs.o'
	LONG NIL

ifuncsmosdata:
	INCBIN 'ppcifuncs.o'
	LONG NIL


ifuncsos4data:
	INCBIN 'ppcifuncs_os4.o'
	LONG NIL


PROC scanadosobject(o:PTR TO LONG, relocs=FALSE)
	DEF t, l=NIL:PTR TO LONG
	WHILE (t := o[]++)
	   ->WriteF('\h\n', t)
	   SELECT t
	   CASE $3E9 -> CODE
	      t := o[]++
	      l := o
	      o := o + Mul(t,4)
	   CASE $3EC -> RELOC32
	      WHILE (t := o[]++)
	         o++
	         IF relocs
	            WHILE t
	               PutLong(l+o[], Long(l+o[])+g_startupcodelab.offset)
	               addReloc(o[]++ + g_startupcodelab.offset)
	               t--
	            ENDWHILE
	         ELSE
	            o := o + Mul(t,4)
	         ENDIF
	      ENDWHILE
	   CASE $3F2 -> END
	      RETURN l, l[-1]
	   CASE $3E8 -> NAME
	      t := o[]++ * 4
	      o := o + t
	   CASE $3E7 -> UNIT
	      t := o[]++ * 4
	      o := o + t
	   CASE $3F0 -> SYMBOL
	      WHILE (t := o[]++)
	         o := o + (t AND $FF * 4) + 4
	      ENDWHILE
	   CASE $3EF -> EXT
	      WHILE (t := o[]++)
	         o := o + (t AND $FF * 4) + 4
	      ENDWHILE
	   DEFAULT
	      reportIErr(' scanadosobject: t')
	   ENDSELECT
	ENDWHILE
	IF l = NIL THEN reportIErr(' scanadosobject: l')
ENDPROC l , l[-1]




-> 1.10.0 rewritten to use the new ___internalglobs object from target module
-> 1.5.4: call after initTarget(), but before code
PROC initIvars()
	DEF a
	DEF gv:PTR TO gvar, hln:PTR TO hln, object:PTR TO object
	DEF memb:PTR TO member, globsobj:PTR TO object

	IF g_ivarsadded THEN RETURN -> only do this once!
	g_ivarsadded := TRUE

	#ifdef DBG_MAIN
	DEBUGF('initIvars()\n')
	#endif

	hln := getLabelHLN('___internalglobs')
	globsobj := hln.ident2

	IF globsobj = NIL THEN RETURN -> target module might not be loaded (from self)

	->WriteF('initivars on the loose!\n')

	FOR a := 0 TO globsobj.nrofmembers-1
	   memb := globsobj.membertable[a]
	   hln := getLabelHLN(memb.name)
	   ->WriteF('   global "\s"\n', hln.name)
	   -> overwrite execbase,dosbase,etc globals from modules with internal version /1.7.1)
	   IF hln.ident
	      IF hln.ident.identID <> IDENT_VARIABLE THEN reportErr('symbol collision', hln.name)
	      gv := hln.ident
	   ELSE
	      NEW gv
	      gv.next := g_gvarlist
	      g_gvarlist := gv
	   ENDIF
	   gv.hln := hln
	   hln.ident := gv
	   gv.offset := memb.offset
	   IF memb.object  -> lets type the var if object exists (1.5.4)
	      IF memb.object < 0
	         hln := -memb.object
	         object := hln.ident2
	      ELSE
	         object := memb.object
	      ENDIF
	      gv.type.esize := IF object THEN 255 ELSE 1
	      gv.type.object := object
	   ELSE
	      gv.type.esize := 1 -> 1.5.6
	   ENDIF
	   gv.type.size := memb.size
	   gv.type.flags := memb.flags
	   gv.o := VAR
	   gv.d := gv
	   gv.breg := GLOBREG -> target specific!
	   gv.identID := IDENT_VARIABLE
	   gv.vtype := VTYPE_GLOB
	   gv.trok := TRUE -> as of 1.5.1 we can clear these!
	   -> 1.5.1
	   gv.gtype := GTYPE_INTERNAL  -> 1.7.1
	   IF g_optmodule -> 1.7.1
	      gv.export := TRUE
	      gv.link := 1
	   ELSE
	      gv.export := FALSE -> gv.export might be TRUE as we might be overwriting gvar
	      gv.link := 0
	   ENDIF
	ENDFOR

	-> 1.8.0
	hln := getLabelHLN('exception')
	g_codegen.ivar_exception := hln.ident
	hln := getLabelHLN('___exceptstruct')
	g_codegen.pvar_exceptstruct := hln.ident

	#ifdef DBG_MAIN
	DEBUGF('initIvars() DONE\n')
	#endif

ENDPROC


PROC initTarget() HANDLE
	DEF t, c:PTR TO const, ident:PTR TO ident
	DEF gv:PTR TO gvar, l:PTR TO LONG, hln:PTR TO hln
	DEF ppcsysv:PTR TO ppcsysv, m68amiga:PTR TO m68amiga
	DEF object:PTR TO object, tstr[1024]:STRING

	IF g_codegen THEN RETURN

	IF g_optosid = OSID_NONE THEN reportErr('no target defined') -> v55

	IF g_optosid = OSID_AMIGAOS4 THEN g_elf_usephdr := TRUE

	#ifdef DBG_MAIN
	DEBUGF('initTarget()\n')
	#endif

	IF g_optmodule = FALSE THEN link_codesize := JUMPSTARTSIZE -> the JUMP ! (V48)

	IF g_optpowerpc = CPU_PPC
	   addPPCInstructions()
	ELSEIF g_optpowerpc = CPU_M68
	   add68KInstructions()
	ENDIF

	   -> we need both regtypes regardless of "mode" because of LIBRARY M68K..
	addPPCRegs()
	add68KRegs()

	IF g_optpowerpc = CPU_M68
	   init68KIFS()
	   initPPCIFS() -> mixed here too yo!
	ELSEIF g_optpowerpc = CPU_PPC
	   initPPCIFS()
	   init68KIFS() -> might have mixed code
	ENDIF

	#define GetIdent(name) (hln := getLabelHLN(name)) BUT hln.ident
	#define CheckIdent_ IF ident = NIL THEN Raise("IDEN")


	#ifdef DBG_MAIN
	DEBUGF('initTarget() initing godegen..\n')
	#endif

	IF g_optpowerpc = CPU_PPC

	   NEW ppcsysv
	   ppcsysv.init()
	   ->ident := GetIdent('___I2F')
	   ->CheckIdent_
	   ->ppcsysv.lif_private_i2f := ident

	   ident := GetIdent('___SHR64')
	   CheckIdent_
	   ppcsysv.lif_private_shr64 := ident
	   ident := GetIdent('___SHL64')
	   CheckIdent_
	   ppcsysv.lif_private_shl64 := ident
	   ident := GetIdent('___ASR64')
	   CheckIdent_
	   ppcsysv.lif_private_asr64 := ident
	   ident := GetIdent('___DIV64')
	   CheckIdent_
	   ppcsysv.lif_private_div64 := ident
	   ident := GetIdent('___F2D64')
	   CheckIdent_
	   ppcsysv.lif_private_f2d64 := ident
	   ident := GetIdent('___D642F')
	   CheckIdent_
	   ppcsysv.lif_private_d642f := ident

	   g_codegen := ppcsysv

	ELSEIF g_optpowerpc = CPU_M68
	   NEW m68amiga
	   m68amiga.init()
	   g_codegen := m68amiga

	ELSE   -> 2.0

	   reportIErr('no cpu defined')

	ENDIF


	ident := GetIdent('FastNew')
	CheckIdent_
	g_codegen.fastnew_lif := ident
	CheckIdent_
	ident := GetIdent('FastDispose')
	CheckIdent_
	g_codegen.fastdispose_lif := ident
	ident := GetIdent('Throw')
	CheckIdent_
	g_codegen.throw_lif := ident
	ident := GetIdent('Raise')
	CheckIdent_
	g_codegen.raise_lif := ident

	g_codegen.initCodegen()

	#ifdef DBG_MAIN
	DEBUGF('initTarget() generating module dirs\n')
	#endif

	-> 1.10.0, 2.0
	IF g_optosid = OSID_AMIGAOS4
	   getEnvVar(tstr, 'ecx-amigaos4-dir')
	   addModuledirs(tstr)
	ELSEIF g_optosid = OSID_MORPHOS
	   getEnvVar(tstr, 'ecx-morphos-dir')
	   addModuledirs(tstr)
	ELSEIF g_optosid = OSID_AMIGAOS
	   getEnvVar(tstr, 'ecx-amigaos-dir')
	   addModuledirs(tstr)
	ENDIF

	addModuledirs(g_opt_moduledir)
	addModuledirs(g_arg_moduledir)

	loadTargetModule() -> 2.0

	#ifdef DBG_MAIN
	DEBUGF('initTarget() loading default modules\n')
	#endif

	IF g_nodefmods = FALSE
	   IF g_linkobjmode = FALSE
	      -> 1.10.0
	      t := g_defmodnames
	      WHILE t
	         loadModule(t)
	         t := Next(t)
	      ENDWHILE

	   ENDIF
	ENDIF

	->IF g_optpreprocess THEN addTargetDefines() -> 1.10.0


	#ifdef DBG_MAIN
	DEBUGF('initTarget() DONE\n')
	#endif

EXCEPT

	IF exception = "IDEN" THEN reportIErr('initTarget/ident=NIL')
	ReThrow()

ENDPROC

-> 2.0, called from main(), opt_key() and initTarget()
PROC loadTargetModule()
	IF g_privoptnotargetmod = FALSE
	   g_privoptnotargetmod := TRUE
	   #ifdef DBG_MAIN
	   DEBUGF('initTarget() scanning target module')
	   #endif

	   -> 1.10.0, 2.0
	   IF g_optosid = OSID_MORPHOS
	      scantargetmodule('ecxmodules:ecx/target_morphos.m')
	   ELSEIF g_optosid = OSID_AMIGAOS4
	      scantargetmodule('ecxmodules:ecx/target_amigaos4.m')
	   ELSEIF g_optosid = OSID_AMIGAOS
	      scantargetmodule('ecxmodules:ecx/target_amigaos.m')
	   ENDIF

	ENDIF
ENDPROC

PROC add_iconsts()
	DEF hln:PTR TO hln, c:PTR TO const

	c := [IDENT_CONST,TRUE,'TRUE',NIL,
	      IDENT_CONST,FALSE,'FALSE',NIL,
	      IDENT_CONST,NIL,'NIL',NIL,
	      IDENT_CONST,ALL,'ALL',NIL,
	      IDENT_CONST,NIL,'EMPTY',NIL,
	      NIL]

	WHILE c.identID
	   hln := getLabelHLN(c.name)
	   hln.ident := c
	   c++
	ENDWHILE

	hln := getLabelHLN('STRLEN')
	hln.ident := [IDENT_CONST,NIL]:const
	ident_STRLEN := hln.ident

	-> 1.10.0
	hln := getLabelHLN('LINENUM')
	hln.ident := [IDENT_CONST,NIL]:const
	ident_LINENUM := hln.ident


ENDPROC


PROC init68KIFS()
	DEF i:PTR TO lif, l:PTR TO LONG, hln:PTR TO hln
	DEF b:PTR TO LONG, x=0
	b := scanadosobject({ifuncs68data})

	IF g_optpowerpc <> CPU_M68
	   IF g_optmodule THEN RETURN
	ENDIF

	/* do not change order of functions ! */

	 -> 10
l:= [IFUNCDEF('WriteF',         [0],[],0,2),
	  IFUNCDEF('Mul',            [0,0],[],0,0),
	  IFUNCDEF('Div',            [0,0],[],0,0),
	  IFUNCDEF('OpenW',          [0,0,0,0,0,0,0,0,0,0,0],[1,0,0],0,0),
	  IFUNCDEF('OpenS',          [0,0,0,0,0,0],[0],0,0),
	  IFUNCDEF('Mouse',          [],[],0,0),
	  IFUNCDEF('Plot',           [0,0,0],[1],0,0),
	  IFUNCDEF('Line',           [0,0,0,0,0],[1],0,0),
	  IFUNCDEF('TextF',          [0,0,0],[],0,2),
	  IFUNCDEF('Colour',         [0,0],[0],0,0),
	  -> 20
	  IFUNCDEF('SetStdRast',     [0],[],0,0),
	  IFUNCDEF('SetStdOut',      [0],[],0,0),
	  IFUNCDEF('Long',           [0],[],0,0),
	  IFUNCDEF('Int',            [0],[],0,0),
	  IFUNCDEF('Char',           [0],[],0,0),
	  IFUNCDEF('PutLong',        [0,0],[],0,0),
	  IFUNCDEF('PutInt',         [0,0],[],0,0),
	  IFUNCDEF('PutChar',        [0,0],[],0,0),
	  IFUNCDEF('New',            [0],[],0,0),
	  IFUNCDEF('CleanUp',        [0],[0],0,0),
	  -> 30
	  IFUNCDEF('CloseW',         [0],[],0,0),
	  IFUNCDEF('CloseS',         [0],[],0,0),
	  IFUNCDEF('And',            [0,0],[],0,0),
	  IFUNCDEF('Or',             [0,0],[],0,0),
	  IFUNCDEF('#Not_OLD',            [0],[],0,0),
	  IFUNCDEF('Gadget',         [0,0,0,0,0,0,0,0],[],0,0),
	  IFUNCDEF('SetTopaz',       [0],[],0,0),
	  IFUNCDEF('StrCmp',         [0,0,0],[-1],0,0),
	  IFUNCDEF('StrCopy',        [0,0,0],[-1],0,0),
	  IFUNCDEF('StrAdd',         [0,0,0],[-1],0,0),
	  -> 40
	  IFUNCDEF('StrLen',         [0],[],0,0),
	  IFUNCDEF('EstrLen',        [0],[],0,0),
	  IFUNCDEF('StrMax',         [0],[],0,0),
	  IFUNCDEF('#String_OLD',    [0],[],0,0),
	  IFUNCDEF('RightStr',       [0,0,0],[],0,0),
	  IFUNCDEF('MidStr',         [0,0,0,0],[-1],0,0),
	  IFUNCDEF('StringF',        [0,0],[],0,2),
	  IFUNCDEF('Val',            [0,0],[0],0,0),
	  IFUNCDEF('InStr',          [0,0,0],[0],0,0),
	  IFUNCDEF('TrimStr',        [0],[],0,0),
	  -> 50
	  IFUNCDEF('UpperStr',       [0],[],0,0),
	  IFUNCDEF('LowerStr',       [0],[],0,0),
	  IFUNCDEF('ReadStr',        [0,0],[],0,0),
	  IFUNCDEF('Out',            [0,0],[],0,0),
	  IFUNCDEF('Inp',            [0],[],0,0),
	  IFUNCDEF('KickVersion',    [0],[],0,0),
	  IFUNCDEF('FileLength',     [0],[],0,0),
	  IFUNCDEF('MouseX',         [0],[],0,0),
	  IFUNCDEF('MouseY',         [0],[],0,0),
	  IFUNCDEF('FreeStack',      [],[],0,0),
	  -> 60
	  IFUNCDEF('CtrlC',          [],[],0,0),
	  IFUNCDEF('#List_OLD',      [0],[],0,0),
	  IFUNCDEF('ListCopy',       [0,0,0],[-1],0,0),
	  IFUNCDEF('ListAdd',        [0,0,0],[-1],0,0),
	  IFUNCDEF('ListCmp',        [0,0,0],[-1],0,0),
	  IFUNCDEF('ListLen',        [0],[],0,0),
	  IFUNCDEF('ListMax',        [0],[],0,0),
	  IFUNCDEF('Even',           [0],[],0,0),
	  IFUNCDEF('Odd',            [0],[],0,0),
	  IFUNCDEF('Eval',           [0],[],0,0),
	  -> 70
	  IFUNCDEF('ForAll',         [0,0,0],[],0,0),
	  IFUNCDEF('Exists',         [0,0,0],[],0,0),
	  IFUNCDEF('MapList',        [0,0,0,0],[],0,0),
	  IFUNCDEF('#Abs_OLD',            [0],[],0,0),
	  IFUNCDEF('Shl',            [0,0],[],0,0),
	  IFUNCDEF('Shr',            [0,0],[],0,0),
	  IFUNCDEF('Box',            [0,0,0,0,0],[1],0,0),
	  IFUNCDEF('Dispose',        [0],[],0,0),
	  IFUNCDEF('#DisposeLink_OLD', [0],[],0,0),
	  IFUNCDEF('Link',           [0,0],[],0,0),
	  -> 80
	  IFUNCDEF('Next',           [0],[],0,0),
	  IFUNCDEF('Forward',        [0,0],[],0,0),
	  IFUNCDEF('SetStr',         [0,0],[],0,0),
	  IFUNCDEF('SetList',        [0,0],[],0,0),
	  IFUNCDEF('WaitIMessage',   [0],[],0,0),
	  IFUNCDEF('MsgCode',        [],[],0,0),
	  IFUNCDEF('MsgQualifier',   [],[],0,0),
	  IFUNCDEF('MsgIaddr',       [],[],0,0),
	  IFUNCDEF('Rnd',            [0],[],0,0),
	  IFUNCDEF('RndQ',           [0],[],0,0),
	  -> 90
	  IFUNCDEF('Mod',            [0,0],[],0,0),
	  IFUNCDEF('Eor',            [0,0],[],0,0),
	  IFUNCDEF('Raise',          [0],[0],0,0),
	  IFUNCDEF('ListItem',       [0,0],[],0,0),
	  IFUNCDEF('NewR',           [0],[],0,0),
	  IFUNCDEF('Sign',           [0],[],0,0),
	  IFUNCDEF('PrintF',         [0],[],0,2),
	  IFUNCDEF('WaitLeftMouse',  [0],[],0,0),
	  IFUNCDEF('LeftMouse',      [0],[],0,0),
	  IFUNCDEF('SetStdIn',       [0],[],0,0),
	  -> 100
	  IFUNCDEF('Throw',          [0,0],[0],0,0),
	  IFUNCDEF('ReThrow',        [],[],0,0),
	  IFUNCDEF('SelectList',     [0,0,0,0],[],0,0),
	  IFUNCDEF('SetColour',      [0,0,0,0,0],[],0,0),
	  IFUNCDEF('NewM',           [0,0],[],0,0),
	  IFUNCDEF('Bounds',         [0,0,0],[],0,0),
	  IFUNCDEF('RealF',          [0,0,0],[1],0,0),
	  IFUNCDEF('RealVal',        [0],[],0,0),
	  IFUNCDEF('Fabs',           [0],[],0,0),
	  IFUNCDEF('Ffloor',         [0],[],0,0),
	  -> 110
	  IFUNCDEF('Fceil',          [0],[],0,0),
	  IFUNCDEF('Fsin',           [0],[],0,0),
	  IFUNCDEF('Fcos',           [0],[],0,0),
	  IFUNCDEF('Ftan',           [0],[],0,0),
	  IFUNCDEF('Fexp',           [0],[],0,0),
	  IFUNCDEF('Flog',           [0],[],0,0),
	  IFUNCDEF('Fpow',           [0,0],[],0,0),
	  IFUNCDEF('Fsqrt',          [0],[],0,0),
	  IFUNCDEF('Flog10',         [0],[],0,0),
	  IFUNCDEF('FastDispose',    [0,0],[],0,0),
	  -> 120
	  IFUNCDEF('FastNew',        [0],[],0,0),
	  IFUNCDEF('Min',            [0,0],[],0,0),
	  IFUNCDEF('Max',            [0,0],[],0,0),
	  IFUNCDEF('OstrCmp',        [0,0,0],[-1],0,0),
	  IFUNCDEF('AstrCopy',       [0,0,0],[-1],0,0),
	  IFUNCDEF('#Cell_removed',  [0],[],0,0),
	  IFUNCDEF('#FreeCells',      [],[],0,0),
	  IFUNCDEF('#SetChunkSize',   [0],[],0,0),
	  IFUNCDEF('#Car',            [0],[],0,0),
	  IFUNCDEF('#Cdr',            [0],[],0,0),
	  -> 130
	  IFUNCDEF('#Cons',           [0],[],0,0),
	  IFUNCDEF('FastDisposeList',[0],[],0,0),
	  IFUNCDEF('Fatan',          [0],[],0,0),
	  IFUNCDEF('Fsincos',        [0,0],[],0,0),
	  IFUNCDEF('Fsinh',          [0],[],0,0),
	  IFUNCDEF('Fcosh',          [0],[],0,0),
	  IFUNCDEF('Ftanh',          [0],[],0,0),
	  IFUNCDEF('Ftieee',         [0],[],0,0),
	  IFUNCDEF('Ffieee',         [0],[],0,0),
	  IFUNCDEF('Fasin',          [0],[],0,0),
	  -> 140
	  IFUNCDEF('Facos',          [0],[],0,0),
	  -> ECX new internal funcs
	  IFUNCDEF('ObjName',        [0],[],0,0),
	  IFUNCDEF('ObjSize',        [0],[],0,0),
	  -> v48
	  IFUNCDEF('DebugF',         [0],[],0,2),
	  -> v50
	  IFUNCDEF('Double',         [0],[],1,0),
	  IFUNCDEF('PutDouble',      [0,1],[],1,0),
	  IFUNCDEF('Ptr',            [0],[],0,0),
	  IFUNCDEF('PutPtr',         [0,0],[],0,0),
	  IFUNCDEF('Byte',           [0],[],0,0),
	  IFUNCDEF('PutByte',        [0,0],[],0,0),
	  -> 150
	  IFUNCDEF('Word',           [0],[],0,0),
	  IFUNCDEF('PutWord',        [0,0],[],0,0),
	  IFUNCDEF('Float',          [0],[],1,0),
	  IFUNCDEF('PutFloat',       [0,1],[],1,0),
	  IFUNCDEF('Real',           [0],[],1,0),
	  IFUNCDEF('PutReal',        [0,1],[],1,0),
	  IFUNCDEF('NewList',        [0],[],0,0), -> v56
	  IFUNCDEF('String',         [0,0],[0],0,0), -> v56
	  IFUNCDEF('List',           [0,0],[0],0,0), -> v56
	  IFUNCDEF('DisposeLink',    [0,0],[0],0,0), -> v56
	  -> 160
	  IFUNCDEF('Wide',          [0],[],2,0),
	  IFUNCDEF('PutWide',       [0,2],[],0,0),
	  IFUNCDEF('UlongToWide',   [0],[],2,0),
	  NIL]


  g_internalfuncs := l

	WHILE (i := l[]++)
	   i.code := b[x] + b
	   i.codelen := b[x+1] - b[x]
	   i.dblabnext := g_dblablist  -> 2.2. was forgotten
	   g_dblablist := i           ->
	   x++
	   hln := getLabelHLN(i.name)
	   IF g_optpowerpc = CPU_M68
	      hln.ident := i
	   ENDIF
	   i.name := hln.name -> 1.7.1 fix
	ENDWHILE

ENDPROC

/***** PPC internal functions *********/

-> function arguments are passed in r3-r10, returns in r3-r5
-> some smaller functions are inline!

PROC initPPCIFS()
	DEF l:PTR TO LONG, i:PTR TO lif, hln:PTR TO hln
	DEF b:PTR TO LONG, x=0


	IF g_optpowerpc <> CPU_PPC
	   IF g_optmodule THEN RETURN
	ELSE
	   addPPCInlines()
	ENDIF

	IF g_optosid = OSID_NONE THEN RETURN

	b := scanadosobject(
	   IF g_optosid = OSID_AMIGAOS4 THEN {ifuncsos4data} ELSE {ifuncsmosdata})


	 /* do not change order of functions ! */
	 l := [
	 IFUNCDEF('FastNew',     [0], [], [0], 0),
	 IFUNCDEF('FastDispose', [0,0], [], [0], 0),
	 IFUNCDEF('New',         [0], [], [0], 0),
	 IFUNCDEF('NewR',        [0], [], [0], 0),
	 IFUNCDEF('Dispose',     [0], [], [0], 0),
	 IFUNCDEF('#StringF_OLD',[0,0], [], [0], -1),
	 IFUNCDEF('Raise',       [0], [0], [0], 0),
	 IFUNCDEF('Throw',       [0,0], [], [0], 0),
	 IFUNCDEF('ReThrow',     [], [], [0], 0),
	 IFUNCDEF('StrCopy',     [0,0,0], [-1], [0], 0),
	 IFUNCDEF('StrAdd',      [0,0,0], [-1], [0], 0),
	 IFUNCDEF('StrCmp',      [0,0,0], [-1], [0], 0),
	 IFUNCDEF('StrLen',      [0], [], [0], 0),
	 IFUNCDEF('TrimStr',     [0], [], [0], 0),
	 IFUNCDEF('AstrCopy',    [0,0,0], [-1], [0], 0),
	 IFUNCDEF('RndQ',        [0], [], [0], 0),
	 IFUNCDEF('#Bounds_OLD', [0,0,0], [], [0], 0),
	 IFUNCDEF('#Min_OLD',    [0,0], [], [0], 0),
	 IFUNCDEF('#Max_OLD',    [0,0], [], [0], 0),
	 IFUNCDEF('#Abs_OLD',    [0], [], [0], 0),
	 IFUNCDEF('#Sign_OLD',   [0], [], [0], 0),
	 IFUNCDEF('String',      [0,0], [0], [0], 0),
	 IFUNCDEF('List',        [0,0], [0], [0], 0),
	 IFUNCDEF('#WriteF_OLD', [0], [], [0], -1),
	 IFUNCDEF('#PrintF_OLD', [0], [], [0], -1),
	 IFUNCDEF('CleanUp',     [0], [0], [0], 0),
	 IFUNCDEF('FreeStack',   [], [], [0], 0),
	 IFUNCDEF('InStr',       [0,0,0], [0], [0], 0),
	 IFUNCDEF('UpperStr',    [0], [], [0], 0),
	 IFUNCDEF('LowerStr',    [0], [], [0], 0),
	 IFUNCDEF('OpenW',       [0,0,0,0,0,0,0,0,0,0,0], [0,1,0,0], [0], 0),
	 IFUNCDEF('ReadStr',     [0,0], [], [0], 0),
	 IFUNCDEF('CloseW',      [0], [], [0], 0),
	 IFUNCDEF('ListCopy',    [0,0,0], [-1], [0], 0),
	 IFUNCDEF('ListAdd',     [0,0,0], [-1], [0], 0),
	 IFUNCDEF('ListCmp',     [0,0,0], [-1], [0], 0),
	 IFUNCDEF('Val',         [0,0], [0], [0,0], 0),
	 IFUNCDEF('CtrlC',       [], [], [0], 0),
	 IFUNCDEF('Mod',         [0,0], [], [0,0], 0),
	 IFUNCDEF('Forward',     [0,0], [], [0], 0),
	 IFUNCDEF('#___I2F',      [], [], [0], 0),
	 IFUNCDEF('DisposeLink', [0,0], [0], [0], 0),
	 IFUNCDEF('FastDisposeList', [0], [], [0], 0),
	 IFUNCDEF('KickVersion', [0], [], [0], 0),
	 IFUNCDEF('RealVal',     [0], [], [1,0], 0),
	 IFUNCDEF('Inp',         [0], [], [0], 0),
	 IFUNCDEF('Out',         [0,0], [], [0], 0),
	 IFUNCDEF('ForAll',      [0,0,0], [], [0], 0),
	 IFUNCDEF('Exists',      [0,0,0], [], [0], 0),
	 IFUNCDEF('MapList',     [0,0,0,0], [], [0], 0),
	 IFUNCDEF('SelectList',  [0,0,0,0], [], [0], 0),
	 IFUNCDEF('#Fabs_OLD',   [1], [], [1], 0), -> here for backwards module compab
	 IFUNCDEF('Ffloor',      [1], [], [1], 0),
	 IFUNCDEF('Fceil',       [1], [], [1], 0),
	 IFUNCDEF('Fsin',        [1], [], [1], 0),
	 IFUNCDEF('Fcos',        [1], [], [1], 0),
	 IFUNCDEF('Ftan',        [1], [], [1], 0),
	 IFUNCDEF('Fexp',        [1], [], [1], 0),
	 IFUNCDEF('Flog',        [1], [], [1], 0),
	 IFUNCDEF('Fpow',        [1,1], [], [1], 0),
	 IFUNCDEF('Fsqrt',       [1], [], [1], 0),
	 IFUNCDEF('Flog10',      [1], [], [1], 0),
	 IFUNCDEF('Fatan',       [1], [], [1], 0),
	 IFUNCDEF('Fsincos',     [0,1], [], [1], 0),
	 IFUNCDEF('Fsinh',       [1], [], [1], 0),
	 IFUNCDEF('Fcosh',       [1], [], [1], 0),
	 IFUNCDEF('Ftanh',       [1], [], [1], 0),
	 IFUNCDEF('Ftieee',      [0], [], [1], 0),
	 IFUNCDEF('Ffieee',      [1], [], [0], 0),
	 IFUNCDEF('Fasin',       [1], [], [1], 0),
	 IFUNCDEF('Facos',       [1], [], [1], 0),
	 IFUNCDEF('LeftMouse',   [0], [], [0], 0),
	 IFUNCDEF('WaitLeftMouse', [0], [], [0], 0),
	 IFUNCDEF('WaitIMessage',  [0], [], [0], 0),
	 IFUNCDEF('SetStdIn',    [0], [], [0], 0),
	 IFUNCDEF('SetStdOut',   [0], [], [0], 0),
	 IFUNCDEF('SetStdRast',  [0], [], [0], 0),
	 IFUNCDEF('#TextF_OLD',  [0,0,0], [], [0], -1),
	 IFUNCDEF('RealF',       [0,1,0], [1], [0], 0),
	 IFUNCDEF('FileLength',  [0], [], [0], 0),
	 IFUNCDEF('MidStr',      [0,0,0,0], [], [0], 0),
	 IFUNCDEF('Plot',        [0,0,0], [1], [0], 0),
	 IFUNCDEF('Line',        [0,0,0,0,0], [1], [0], 0),
	 IFUNCDEF('Rnd',         [0], [], [0], 0),
	 IFUNCDEF('Box',         [0,0,0,0,0], [1], [0], 0),
	 IFUNCDEF('RightStr',    [0,0,0], [], [0], 0),
	 IFUNCDEF('SetChunkSize',[0], [], [0], 0),
	 IFUNCDEF('SetColour',   [0,0,0,0,0], [], [0], 0),
	 IFUNCDEF('OpenS',       [0,0,0,0,0,0], [0], [0], 0),
	 IFUNCDEF('CloseS',      [0], [], [0], 0),
	 IFUNCDEF('OstrCmp',     [0,0,0], [-1], [0], 0),
	 IFUNCDEF('NewM',        [0,0], [], [0], 0),
	 IFUNCDEF('#WtoD',        [1], [], [1], 0),
	 IFUNCDEF('#DtoW',        [1], [], [1], 0),
	 IFUNCDEF('#DebugF_OLD', [0], [], [0], -1), -> v48
	 IFUNCDEF('Colour',      [0,0], [], [0], 0), -> v49
	 IFUNCDEF('#STR_FMT_OLD',[0,0], [], [0], -1), -> v50
	 IFUNCDEF('StringF',     [0,0], [], [0], 2),  -> 1.5.4 NEW
	 IFUNCDEF('PrintF',      [0], [], [0], 2),  -> 1.5.4 NEW
	 IFUNCDEF('WriteF',      [0], [], [0], 2),  -> 1.5.4 NEW
	 IFUNCDEF('TextF',      [0], [], [0], 2),  -> 1.5.4 NEW
	 IFUNCDEF('DebugF',      [0], [], [0], 2),  -> 1.5.4 NEW

	  -> 1.10.0
	 IFUNCDEF('___SHL64',    [0], [], 0, 0),    -> 102
	 IFUNCDEF('___SHR64',    [0], [], 0, 0),
	 IFUNCDEF('___ASR64',    [0], [], 0, 0),
	 IFUNCDEF('___DIV64',    [0], [], 0, 0),
	 IFUNCDEF('Mod64',       [2,2], [], [2,2], 0),
	 IFUNCDEF('#Abs64_OLD',       [2], [], [2], 0),
	 IFUNCDEF('___F2D64',    [0], [], 0, 0),
	 IFUNCDEF('___D642F',    [0], [], 0, 0),    -> 109
	 NIL]


	g_internalfuncsppc := l

	WHILE (i := l[]++)
	   i.code := b[x] + b
	   i.codelen := b[x+1] - b[x] + 3 AND $FFFC -> correct codelen
	   i.dblabnext := g_dblablist -> 2 2. was forgotten
	   g_dblablist := i          ->
	   hln := getLabelHLN(i.name)
	   IF g_optpowerpc = CPU_PPC THEN hln.ident := i
	   i.name := hln.name -> 1.7.1 fix
	   x++
	ENDWHILE

ENDPROC

/**************************************************************
***************************************************************
******************** PREPROCESSOR *****************************
***************************************************************
**************************************************************/

/*
PROC includeFile(name, items) HANDLE -> v45,v53
	DEF fh=NIL, fbuf, flen, fib:fileinfoblock
	DEF oldline, oldinclude

	fh := Open(name,OLDFILE)
	IF fh = NIL THEN Throw("OPEN", name)
	IF ExamineFH(fh,fib) = NIL THEN Throw("EXAM", name)
	flen := fib.size
	fbuf := NEWMEMR(flen+4)
	IF Read(fh,fbuf,flen) <> flen THEN Throw("READ", name)
	Close(fh) ; fh := NIL
	oldline := g_linenum
	setLinenum(1)
	oldinclude := g_includename
	g_includename := name
	items := itemize(fbuf,items)
	DISPOSE(fbuf)
	g_includename := oldinclude
	setLinenum(oldline)

EXCEPT DO

	IF fh THEN Close(fh)
	ReThrow()

ENDPROC items
*/

-> v49, add a commandline define
PROC addCLDefine(str)
	DEF l, m:PTR TO macro, hln:PTR TO hln ,name[256]:STRING
	WHILE (l := getLabelLen(str)) -> multiple!
	   IF l = NIL THEN RETURN NIL
	   NEW m
	   StrCopy(name, str, l)
	   str := str + l
	   hln := getLabelHLN(name)
	   m.name := hln.name
	   m.identID := IDENT_MACRO
	   m.type := 0
	   m.nrofargs := 0
	   WHILE str[] = " " DO str++ -> 1.10.0
	   l := InStr(str, ';')
	   IF l = -1 THEN l := StrLen(str)
	   m.ascii := String(l)
	   IF m.ascii = NIL THEN Raise("MEM")
	   StrCopy(m.ascii, str)
	   hln.ident := m
	   str := str + l + IF str[l] = ";" THEN 1 ELSE 0
	ENDWHILE
ENDPROC

PROC parseDefine(ascii)
	DEF m:PTR TO macro, l, str[256]:STRING, hln:PTR TO hln
	DEF arguments[17]:ARRAY OF LONG, numargs=0, p
	DEF c, s

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++

	l := getLabelLen(ascii)
	IF l = NIL THEN reportErr('preprocessor symbol expected')
	StrCopy(str, ascii, l)
	hln := getLabelHLN(str)

	#ifdef DBG_PP
	DEBUGF('parseDefine: \s\n', hln.name)
	#endif

	IF hln.ident THEN reportErr('double declaration', hln.name)

	NEW m
	hln.ident := m
	m.name := hln.name
	m.identID := IDENT_MACRO

	IF itmz_export OR g_optexport
	   m.next := g_emacrolist
	   g_emacrolist := m
	   itmz_export := FALSE
	ENDIF

	ascii := ascii + l

	IF ascii[] = "("
	   m.type := 1
	   ascii++ ->skip "("
	   WHILE ascii[] <> ")"
	      IF numargs = 17 THEN reportErr('too many args for #define')
	      WHILE ascii[] = " " DO ascii++
	      l := getLabelLen(ascii)
	      IF l = NIL
	         IF StrCmp(ascii, '...', STRLEN) = FALSE THEN reportErr('#define syntax')
	         l := 3
	         m.type := 3  -> varargs
	      ELSE
	         StrCopy(str, ascii, l)
	         hln := getLabelHLN(str)
	         arguments[numargs++] := hln.name
	      ENDIF
	      ascii := ascii + l
	      WHILE ascii[] = " " DO ascii++
	      IF ascii[] = ","
	         ascii++
	         WHILE ascii[] = " " DO ascii++
	      ELSEIF ascii[] <> ")"
	         reportErr('")" or "," expected for #define')
	      ENDIF
	   ENDWHILE
	   m.nrofargs := numargs
	   IF ascii[]++ <> ")" THEN reportErr('")" expected for #define')
	ENDIF

	NEW m.params[numargs+1]
	CopyMemQuick(arguments, m.params, numargs*4)

	/* get body */

	SetStr(g_ppstring, 0)

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++  -> 1.10.0

	s := ascii
	WHILE (c := s[])
	   SELECT 128 OF c
	   CASE "a" TO "z", "A" TO "Z", "_" -> label ?
	      l := getLabelLen(s)
	      StrCopy(str, s, l)
	      hln := getLabelHLN(str)
	      FOR p := 0 TO numargs-1
	         IF StrCmp(hln.name, m.params[p])
	            StrAdd(g_ppstring, [p+1,0]:CHAR)
	            p := numargs + 1 -> exit
	         ENDIF
	      ENDFOR
	      IF p = numargs THEN StrAdd(g_ppstring, str)
	      s := s + l
	   CASE "." -> '...' ?
	      IF StrCmp(s, '...', STRLEN)
	         IF m.type <> 3 THEN reportErr('#define body syntax', '...')
	         StrAdd(g_ppstring, [m.nrofargs+1,0]:CHAR)
	         s := s + 3
	      ELSE
	         StrAdd(g_ppstring, [c,0]:CHAR) ; s++
	      ENDIF
	   CASE "\\"
	      IF s[1] = 10 -> 1.5.5
	         setLinenum(g_linenum + 1)
	         s := s + 2
	      ELSE
	         StrAdd(g_ppstring, [c,0]:CHAR) ; s++
	      ENDIF
	   CASE 10, ";"
	      IF c = 10 THEN setLinenum(g_linenum + 1)
	      /* 1.10.0 remove ending whites */
	      WHILE (l := EstrLen(g_ppstring)) BUT (g_ppstring[l-1] = " ") OR (g_ppstring[l-1] = "\t")
	         SetStr(g_ppstring, l - 1)
	      ENDWHILE
	      s[] := NIL
	   CASE "\t"
	      StrAdd(g_ppstring, [" ",0]:CHAR) ; s++
	   DEFAULT
	      StrAdd(g_ppstring, [c,0]:CHAR) ; s++
	   ENDSELECT
	ENDWHILE
	s++

	l := EstrLen(g_ppstring)
	IF l > MACROBUFSIZE THEN reportErr('too large #define body', m.name)
	IF l < 0 THEN reportErr('too large #define body', m.name)
	m.ascii := String(l)
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, g_ppstring)
	SetStr(g_ppstring, 0)

	#ifdef DBG_PP
	DEBUGF('parseDefine: \s DONE: numargs=\d, type=\d\nBODY=\s\n', hln.name, m.nrofargs, m.type,m.ascii)
	#endif

ENDPROC s


PROC doMacro(hln:PTR TO hln, ascii, items:PTR TO item, tostring)->, prevargs:PTR TO LONG)
	DEF apspos
	DEF p=0
	DEF m:PTR TO macro, t, a, margs[17]:ARRAY OF LONG
	DEF s, c

	#ifdef DBG_PP
	DEBUGF('doMacro($\h,$\h,$\h)\n', hln,ascii,items)
	#endif

	IF FreeStack() < 1000 THEN reportErr('excessive macro-recursion', hln.name)

	m := hln.ident


	t := m.type
	SELECT 4 OF t
	CASE 0

	   #ifdef DBG_PP
	   DEBUGF('\s: \s\n', hln.name, m.ascii)
	   #endif
	   hln.ident := NIL -> no endless recursion here !
	   -> 1.7.1 FIX
	   t := EstrLen(g_argbodystr)
	   ->IF (StrMax(g_argbodystr) - t) < 1000 THEN Raise("ABUF")
	   StrAdd(g_argbodystr, m.ascii)
	   processBody(g_argbodystr + t, tostring) -> was m.ascii
	   IF items  -> if called by itemizer, lets itemize
	      items := itemize(tostring, items)
	      SetStr(tostring, 0)
	   ENDIF
	   SetStr(g_argbodystr, t) -> 1.7.1 fix too
	   hln.ident := m -> restore ident

	CASE 1, 3

	   WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++ -> v46
	   IF ascii[] <> "(" THEN reportErr('"(" expected for macro', m.name)
	   ascii++ -> skip "("
	   WHILE (c := ascii[]) < 33
	      EXIT c = 0
	      IF c = 10 THEN setLinenum(g_linenum + 1)
	      ascii++
	   ENDWHILE

	   -> 1.10.0 process args before inclusion.
	   apspos := EstrLen(g_argprocstr)
	   ascii := processArgs(ascii, ")") -> into g_argprocstr
	   s := g_argprocstr + apspos

	   -> gather param-expressions
	   a := m.nrofargs-1
	   FOR p := 0 TO a
	      t := getparamexp(s)
	      IF t = NIL THEN reportErr('parameter expected for macro', m.name)
	      margs[p] := s
	      s := t + 1
	      IF (p < a)
	         IF t[] <> "," THEN reportErr('"," expected for macro', m.name)
	         WHILE (c := s[]) < 33
	            EXIT c = 0
	            IF c = 10 THEN setLinenum(g_linenum + 1)
	            s++
	         ENDWHILE
	         t[] := NIL -> terminate param exp
	      ELSE
	         IF t[] <> ")"
	            IF m.type <> 3
	               reportErr('")" expected for macro', m.name)
	            ELSEIF t[] <> ","
	               reportErr('")" or "," expected for macro', m.name)
	            ENDIF
	            t[] := NIL -> terminate param exp
	         ELSE
	            IF m.type <> 3 THEN t[] := NIL ELSE s := t
	         ENDIF
	      ENDIF
	   ENDFOR

	   IF m.type = 3-> varargs ?
	      margs[p] := s
	      WHILE (t := getparamexp(s))
	         s := t
	         IF s[] = ","
	            s++
	            WHILE (c := s[]) < 33
	               EXIT c = 0
	               IF c = 10 THEN setLinenum(g_linenum + 1)
	               s++
	            ENDWHILE
	         ENDIF
	      ENDWHILE
	      IF s[] <> ")" THEN reportErr('")" expected for macro', m.name)
	      s[]++ := NIL -> terminate varargs exp
	   ELSE
	      margs[p] := ''
	   ENDIF

	   #ifdef DBG_PP
	   DEBUGF('doMacro PARAMS DONE \s/\d: \s\n', m.name, m.nrofargs, m.ascii)
	   #endif

	   t := EstrLen(g_argbodystr)
	   IF (StrMax(g_argbodystr) - t) < 1000
	      ->THEN Raise("ABUF")
	      reportErr('Macro argument substitution buffer full (forgot ")" ?)')
	   ENDIF

	   /* lets expand body with args, appending it to g_argbodystr */
	   s := m.ascii
	   p := m.nrofargs + 1
	   WHILE (c := s[]++)
	      IF c < p
	         #ifdef DBG_PP
	         ->DEBUGF('macroparam added: \s\n', margs[c-1])
	         #endif
	         StrAdd(g_argbodystr, margs[c-1])
	      ELSEIF c = p -> varargs
	         IF StrLen(margs[c-1])
	            StrAdd(g_argbodystr, margs[c-1])
	         ELSEIF g_argbodystr[EstrLen(g_argbodystr)-1] = ","
	            SetStr(g_argbodystr, EstrLen(g_argbodystr)-1)
	         ENDIF
	      ELSE
	         StrAdd(g_argbodystr, [c,0]:CHAR)
	      ENDIF
	   ENDWHILE
	   StrAdd(g_argbodystr, [255,0]:CHAR) -> end of argbody marker !

	   #ifdef DBG_PP
	   DEBUGF('doMacro EXPAND DONE \s/\d: \s\n', m.name, m.nrofargs, m.ascii)
	   #endif


	   -> 1.10.0 give back what we used from g_argprocstr
	   SetStr(g_argprocstr, apspos)

	   processBody(g_argbodystr + t, tostring)

	   SetStr(g_argbodystr, t) -> reset

	   -> if we were called by itemizer, lets itemize
	   IF items
	      #ifdef DBG_PP
	      DEBUGF('\s\n',tostring)
	      #endif
	      items := itemize(tostring, items)
	      SetStr(tostring, 0)
	   ENDIF

	ENDSELECT

ENDPROC ascii, items

-> 1.10.0
-> processes all args into g_argprocstr
-> endchar is ")" for macro functions, "\n" for MACROs
PROC processArgs(ascii, endchar)
	DEF t, inastr=FALSE, inqstr=FALSE, l, hln:PTR TO hln, m:PTR TO macro
	DEF str[256]:STRING, debugstart, pdepth=0, run=TRUE

	#ifdef DBG_PP
	DEBUGF('processArgs(\s[16]..)\n', ascii)
	#endif

	debugstart := g_argprocstr + EstrLen(g_argprocstr)

	WHILE (t := ascii[]) AND (run=TRUE)
	   SELECT 128 OF t
	   CASE "A" TO "Z", "a" TO "z", "_"
	      IF (inastr OR inqstr) = FALSE
	         l := getLabelLen(ascii)
	         StrCopy(str, ascii, l)
	         hln := getLabelHLN(str)
	         m := hln.ident
	         IF m
	            IF m.identID = IDENT_MACRO
	               IF Odd(m.type) -> function
	                  IF ascii[l] <> "(" -> not func as defined ?
	                     StrAdd(g_argprocstr, ascii, l)
	                     ascii := ascii + l
	                  ELSE
	                     ascii := doMacro(hln, ascii + l, NIL, g_argprocstr)
	                  ENDIF
	               ELSE
	                  ascii := doMacro(hln, ascii + l, NIL, g_argprocstr)
	               ENDIF
	            ELSE
	               StrAdd(g_argprocstr, ascii, l)
	               ascii := ascii + l
	            ENDIF
	         ELSE
	            StrAdd(g_argprocstr, ascii, l)
	            ascii := ascii + l
	         ENDIF
	      ELSE
	         StrAdd(g_argprocstr, [t,0]:CHAR)
	         ascii++
	      ENDIF
	   CASE " ", "\t"
	      StrAdd(g_argprocstr, [" ",0]:CHAR)
	      ascii++
	      IF (inastr OR inqstr) = FALSE THEN WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++
	   CASE ","
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	      IF (inastr OR inqstr) = FALSE
	         WHILE (t := ascii[]) < 33
	            EXIT t = 0
	            IF t = 10
	               StrAdd(g_argprocstr, [t,0]:CHAR)
	            ENDIF
	            ascii++
	         ENDWHILE
	      ENDIF
	   CASE "\n"
	      IF endchar = t THEN run := FALSE
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   CASE "\a"
	      IF inqstr = FALSE -> 2.1
	         IF inastr
	            inastr := FALSE
	         ELSE
	            inastr := TRUE
	         ENDIF
	      ENDIF
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   CASE "\q"
	      IF inastr = FALSE -> 2.1
	         IF inqstr
	            inqstr := FALSE
	         ELSE
	            inqstr := TRUE
	         ENDIF
	      ENDIF
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   CASE "("
	      IF (inastr OR inqstr) = FALSE THEN pdepth++
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   CASE ")"
	      IF (inastr OR inqstr) = FALSE
	         IF (endchar = t) AND (pdepth = 0)
	            run := FALSE
	         ELSE
	            pdepth--
	         ENDIF
	      ENDIF
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   DEFAULT
	      StrAdd(g_argprocstr, [t,0]:CHAR)
	      ascii++
	   ENDSELECT
	ENDWHILE

	#ifdef DBG_PP
	DEBUGF('processArgs DONE:\n\s\n', debugstart)
	#endif


	IF StrMax(g_argprocstr) - EstrLen(g_argprocstr) < 1000
	   ->THEN Raise("MBUF")
	   reportErr('Macro argument processing buffer full (forgot ")" ?)')
	ENDIF
ENDPROC ascii

-> 1.10.0 now takes specified destination string as second arg
PROC processBody(ascii, tostring)
	DEF s:REG, c:REG, hln:PTR TO hln, l, str[256]:STRING, m:PTR TO macro
	DEF inastr=FALSE, inqstr=FALSE

	#ifdef DBG_PP
	DEBUGF('processBody($\h):\n\s\n', ascii, ascii)
	#endif

	s := ascii
	WHILE (c := s[])
	   SELECT 256 OF c
	   CASE "a" TO "z", "A" TO "Z", "_" -> label ?
	      l := getLabelLen(s)
	      IF (inastr OR inqstr) = FALSE
	         StrCopy(str, s, l)
	         hln := getLabelHLN(str)
	         m := hln.ident
	         IF m
	            IF m.identID = IDENT_MACRO
	               IF Odd(m.type) -> function
	                  IF s[l] <> "(" -> not func as defined ?
	                     StrAdd(tostring, str) ; s := s + l
	                  ELSE
	                     s := doMacro(hln, s + l, NIL, tostring)
	                  ENDIF
	               ELSE
	                  s := doMacro(hln, s + l, NIL, tostring)
	               ENDIF
	            ELSE
	               m := hln.ident2
	               IF m
	                  IF m.identID = IDENT2_ASMMACRO
	                     s := doAsmMacro(hln, s + l, NIL, tostring)
	                  ELSE
	                     StrAdd(tostring, str) ; s := s + l
	                  ENDIF
	               ELSE
	                  StrAdd(tostring, str) ; s := s + l
	               ENDIF
	            ENDIF
	         ELSE
	            m := hln.ident2
	            IF m
	               IF m.identID = IDENT2_ASMMACRO
	                  s := doAsmMacro(hln, s + l, NIL, tostring)
	               ELSE
	                  StrAdd(tostring, str) ; s := s + l
	               ENDIF
	            ELSE
	               StrAdd(tostring, str) ; s := s + l
	            ENDIF
	         ENDIF
	      ELSE
	         StrAdd(tostring, s, l) ; s := s + l
	      ENDIF
	   CASE "\q"
	      IF inastr = FALSE -> 2.1
	         IF inqstr
	            inqstr := FALSE
	         ELSE
	            inqstr := TRUE
	         ENDIF
	      ENDIF
	      StrAdd(tostring, '\q') ; s++
	   CASE "\a"
	      IF inqstr = FALSE -> 2.1
	         IF inastr
	            inastr := FALSE
	         ELSE
	            inastr := TRUE
	         ENDIF
	      ENDIF
	      StrAdd(tostring, '\a') ; s++
	   CASE 255 -> end of argbody marker!
	      s[] := NIL -> exit
	   DEFAULT
	      StrAdd(tostring, [c,0]:CHAR) ; s++
	   ENDSELECT
	ENDWHILE

	IF StrMax(tostring) - EstrLen(tostring) < 1000
	   ->THEN Raise("MBUF")
	   reportErr('Macro body processing buffer full (forgot ")" ?)')
	ENDIF

ENDPROC

->BLA MACRO <BODY>\n
->or
->BLA MACRO
-> <BODY>
->ENDM

PROC parseMACRO(hln:PTR TO hln, ascii)
	DEF m:PTR TO macro
	DEF p
	DEF c, s, instr, end, a

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++

	#ifdef DBG_PP
	DEBUGF('parseMACRO: \s \n', hln.name)
	#endif

	IF hln.ident2 THEN reportErr('double declaration of MACRO', hln.name)

	NEW m
	hln.ident2 := m
	m.name := hln.name
	m.identID := IDENT2_ASMMACRO

	IF itmz_export OR g_optexport
	   m.next := g_emacrolist
	   g_emacrolist := m
	   itmz_export := FALSE -> 1.8.0
	ENDIF

	WHILE (ascii[]=" ") OR (ascii[]="\t") DO ascii++

	IF ascii[] <> 10 -> one-liner ?
	   p := InStr(ascii, '\n')
	   IF p = -1 THEN reportErr('syntax for MACRO', hln.name)
	   ascii[p] := NIL
	   end := ascii + p + 1
	ELSE
	   ascii++ -> skip nl
	   a := ascii
	   setLinenum(g_linenum + 1)
	   REPEAT
	      p := InStr(a, '\n')
	      IF p = -1 THEN reportErr('missing "ENDM" for', hln.name)
	      a := a + p + 1
	      WHILE (a[]=" ") OR (a[]="\t") DO a++
	   UNTIL StrCmp(a, 'ENDM', STRLEN)
	   a[] := NIL
	   end := a + 4
	ENDIF

	#ifdef DBG_PP
	DEBUGF('BODY FOR MACRO "\s":\n\s\n', hln.name, ascii)
	#endif


	/* get body */

	SetStr(g_ppstring, 0)

	s := ascii
	instr := FALSE
	WHILE (c := s[])
	   SELECT 128 OF c
	   CASE "\\" -> arg ?
	      IF instr = FALSE
	         s++
	         c:= s[]
	         IF c <= "9"
	            IF c > "0"
	               StrAdd(g_ppstring, [c-("0")+1,0]:CHAR)
	               m.nrofargs := Max(m.nrofargs, c-"0")
	               s++
	            ELSEIF c = "0"
	               StrAdd(g_ppstring, [1,0]:CHAR)
	               s++
	            ELSE
	               StrAdd(g_ppstring, '\\')
	            ENDIF
	         ELSE
	            StrAdd(g_ppstring, '\\')
	         ENDIF
	      ELSE
	         StrAdd(g_ppstring, [c,0]:CHAR) ; s++
	      ENDIF
	   CASE 10
	      setLinenum(g_linenum + 1)
	      StrAdd(g_ppstring, ';')
	      s++
	   DEFAULT
	      StrAdd(g_ppstring, [c,0]:CHAR) ; s++
	   ENDSELECT
	ENDWHILE

	m.type := 2

	m.ascii := String(EstrLen(g_ppstring)+1)
	IF m.ascii = NIL THEN Raise("MEM")
	StrCopy(m.ascii, g_ppstring)
	StrAdd(m.ascii ,';')
	SetStr(g_ppstring, 0)

	#ifdef DBG_PP
	DEBUGF('parseMACRO BODY: \s\n', m.ascii)
	#endif

	setLinenum(g_linenum + 1)

ENDPROC end

PROC doAsmMacro(hln:PTR TO hln, ascii, items:PTR TO item, tostring)
	DEF m:PTR TO macro, pnum=1, t, c, margs[10]:ARRAY OF LONG, s, p, apspos

	IF FreeStack() < 1000 THEN reportErr('excessive macro-recursion', hln.name)

	#ifdef DBG_PP
	DEBUGF('doAsmMacro(hln.name=\s, ascii=\s, items=$\h\n', hln.name, ascii, items)
	#endif

	m := hln.ident2

	IF ascii[] = "."
	   ascii++
	   t := ascii[]
	   SELECT t
	   CASE "L"  ; margs[0] := '.L' ; ascii++
	   CASE "W"  ; margs[0] := '.W' ; ascii++
	   CASE "B"  ; margs[0] := '.B' ; ascii++
	   CASE "D"  ; margs[0] := '.D' ; ascii++
	   DEFAULT   ; margs[0] := '.'
	   ENDSELECT
	ELSE
	   margs[0] := ''
	ENDIF

	WHILE (ascii[] = " ") OR (ascii[] = "\t") DO ascii++

	-> 1.10.0 process args before inclusion.
	apspos := EstrLen(g_argprocstr)
	ascii := processArgs(ascii, "\n") -> into g_argprocstr
	s := g_argprocstr + apspos

	FOR p := 1 TO m.nrofargs

	   WHILE (s[] = " ") OR (s[] = "\t") DO s++

	   -> gather param-expressions
	   t := getparamexp2(s)
	   IF t = NIL THEN reportErr('parameter expected for MACRO', m.name)
	   margs[pnum++] := s
	   c := t[]
	   t[] := NIL
	   #ifdef DBG_PP
	   DEBUGF('MACRO arg \d IS:\n\s\n', pnum-1, s)
	   #endif
	   s := t + 1
	   IF c = ","
	      WHILE (c := s[]) < 33
	         EXIT c = NIL
	         IF c = 10 THEN setLinenum(g_linenum + 1)
	         s++
	      ENDWHILE
	   ELSEIF p < m.nrofargs
	      reportErr('parameter expected for MACRO', m.name)
	   ELSEIF (c<>10) AND (c<>";")
	      reportErr('unterminated MACRO', m.name)
	   ENDIF

	ENDFOR

	t := EstrLen(g_argbodystr)
	IF (StrMax(g_argbodystr) - t) < 1000 THEN Raise("ABUF")

	-> expand body with args..
	s := m.ascii
	p := m.nrofargs + 1
	WHILE (c := s[]++)
	   IF c <= p
	      StrAdd(g_argbodystr, margs[c-1])
	   ELSE
	      StrAdd(g_argbodystr, [c,0]:CHAR)
	   ENDIF
	ENDWHILE
	StrAdd(g_argbodystr, [255,0]:CHAR) -> end of argbody marker !

	-> 1.10.0 give back what we used from g_argprocstr
	SetStr(g_argprocstr, apspos)

	processBody(g_argbodystr + t, tostring)

	SetStr(g_argbodystr, t) -> reset

	-> if we were called by itemizer, lets itemize
	IF items
	   #ifdef DBG_PP
	   DEBUGF(tostring)
	   #endif
	   StrAdd(tostring, ';')
	   items := itemize(tostring, items)
	   SetStr(tostring, 0)
	ENDIF


ENDPROC ascii, items


PROC getparamexp(ascii_)
	DEF pdepth=0 -> ()
	DEF bdepth=0 -> []
	DEF inqstr=FALSE -> "bla"
	DEF inastr=FALSE -> 'bla'
	DEF t:REG
	DEF ascii:REG

	ascii := ascii_

	WHILE (t := ascii[])
	   SELECT 128 OF t
	   CASE "("
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         pdepth++
	      ENDIF ; ENDIF
	   CASE ")"
	      IF ascii = ascii_ THEN RETURN NIL
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         IF pdepth = 0 THEN RETURN ascii
	         pdepth--
	      ENDIF ; ENDIF
	   CASE "["
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         bdepth++
	      ENDIF ; ENDIF
	   CASE "]"
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         bdepth--
	      ENDIF ; ENDIF
	   CASE "\a"
	      IF inqstr = FALSE -> 2.1
	         IF inastr
	            inastr := FALSE
	         ELSE
	            inastr := TRUE
	         ENDIF
	      ENDIF
	   CASE "\q"
	      IF inastr = FALSE -> 2.1
	         IF inqstr
	            inqstr := FALSE
	         ELSE
	            inqstr := TRUE
	         ENDIF
	      ENDIF
	   CASE ","
	      IF ascii = ascii_ THEN RETURN NIL
	      IF pdepth=0
	      IF bdepth=0
	      IF inqstr = FALSE
	      IF inastr = FALSE
	         RETURN ascii
	      ENDIF ; ENDIF ; ENDIF ; ENDIF
	   CASE " ", "\t"
	      ascii[] := " "
	   CASE "\n"
	      setLinenum(g_linenum + 1)
	      ascii[] := " "
	   ENDSELECT
	   ascii++
	ENDWHILE

ENDPROC NIL

PROC getparamexp2(ascii_) -> for MACROs
	DEF pdepth=0 -> ()
	DEF bdepth=0 -> []
	DEF inqstr=FALSE -> "bla"
	DEF inastr=FALSE -> 'bla'
	DEF t:REG
	DEF ascii:REG

	ascii := ascii_

	WHILE (t := ascii[])
	   SELECT 128 OF t
	   CASE "("
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         pdepth++
	      ENDIF ; ENDIF
	   CASE ")"
	      IF ascii = ascii_ THEN RETURN NIL
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         IF pdepth = 0 THEN RETURN ascii
	         pdepth--
	      ENDIF ; ENDIF
	   CASE "["
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         bdepth++
	      ENDIF ; ENDIF
	   CASE "]"
	      IF inastr = FALSE
	      IF inqstr = FALSE
	         bdepth--
	      ENDIF ; ENDIF
	   CASE "\a"
	      IF inqstr = FALSE -> 2.1
	         IF inastr
	            inastr := FALSE
	         ELSE
	            inastr := TRUE
	         ENDIF
	      ENDIF
	   CASE "\q"
	      IF inastr = FALSE -> 2.1
	         IF inqstr
	            inqstr := FALSE
	         ELSE
	            inqstr := TRUE
	         ENDIF
	      ENDIF
	   CASE ","
	      IF ascii = ascii_ THEN RETURN NIL
	      IF pdepth=0
	      IF bdepth=0
	      IF inqstr = FALSE
	      IF inastr = FALSE
	         RETURN ascii
	      ENDIF ; ENDIF ; ENDIF ; ENDIF
	   CASE " ", "\t"
	      ascii[] := " "
	   CASE "\n"
	      IF bdepth = NIL
	      IF pdepth = NIL
	         RETURN ascii
	      ENDIF ; ENDIF
	      setLinenum(g_linenum + 1)
	      ascii++
	   CASE ";"
	      RETURN ascii
	   ENDSELECT
	   ascii++
	ENDWHILE

ENDPROC NIL

/**************************************************************
***************************************************************
******************** MODULE LOAD / READ ***********************
***************************************************************
**************************************************************/

-> follow this up with an codegen.abiReturns(proc)
PROC return2mret(return, mret:PTR TO multireturn) -> v49
	DEF t
	t := MODPROC_RETURN1(return)
	SELECT t
	CASE 0 ; mret.ros[0] := DREG
	CASE 1 ; mret.ros[0] := FREG
	CASE 2 ; mret.ros[0] := VREG
	CASE 3 ; mret.ros[0] := X2R
	DEFAULT   ; reportIErr('return2mret 0 t=?')
	ENDSELECT
	t := MODPROC_RETURN2(return)
	SELECT t
	CASE 0 ; mret.ros[1] := DREG
	CASE 1 ; mret.ros[1] := FREG
	CASE 2 ; mret.ros[1] := VREG
	CASE 3 ; mret.ros[1] := X2R
	DEFAULT   ; reportIErr('return2mret 1 t=?')
	ENDSELECT
	t := MODPROC_RETURN3(return)
	SELECT t
	CASE 0 ; mret.ros[2] := DREG
	CASE 1 ; mret.ros[2] := FREG
	CASE 2 ; mret.ros[2] := VREG
	CASE 3 ; mret.ros[2] := X2R
	DEFAULT   ; reportIErr('return2mret 2 t=?')
	ENDSELECT
	t := MODPROC_RETURN4(return)
	SELECT t
	CASE 0 ; mret.ros[3] := DREG
	CASE 1 ; mret.ros[3] := FREG
	CASE 2 ; mret.ros[3] := VREG
	CASE 3 ; mret.ros[3] := X2R
	DEFAULT   ; reportIErr('return2mret 3 t=?')
	ENDSELECT
ENDPROC


PROC readSecondaryInfo(m:PTR TO moduleheader)
	DEF mo:PTR TO modobject, mcl:PTR TO modclass, o:PTR TO object
	DEF mmemb:PTR TO modmember
	DEF mmeth:PTR TO modproc, meth:PTR TO proc
	DEF codelab:PTR TO codelab, table
	DEF a, b, hln:PTR TO hln, i:PTR TO modinfo,count, mproc:PTR TO modproc

	#ifdef DBG_BMOD
	DEBUGF('readSecondaryInfo()\n')
	#endif

	i := m.strtabinfo + m
	table := i + SIZEOF modinfo

	i := m.objectinfo
	IF i
	   i := i + m
	   mo := i + SIZEOF modinfo
	   count := i.count+1
	   WHILE count--
	      mmemb := mo + SIZEOF modobject
	      FOR a := 1 TO mo.nrofmembers DO mmemb++
	      mcl := mmemb
	      IF mo.nrofmethods
	         hln := getLabelHLN(table + mo.name)
	         NEW o
	         o.nrofmethods := mo.nrofmethods
	         o.name := hln.name
	         o.identID := IDENT2_OBJECT
	         o.ltype := LTYPE_CLASS
	         o.offset := mcl.offset + link_codesize -> classinfo
	         o.classofs := mcl.classofs -> in obj
	         o.codelink := m.modextens.codelist
	         o.modmethtab := m + m.codeinfo + SIZEOF modinfo + mcl.offset -> 1.10.0
	         m.modextens.codelist := o
	         mmeth := mcl + SIZEOF modclass
	         NEW o.methodtable[o.nrofmethods+1]
	         FOR a := 0 TO o.nrofmethods-1
	            NEW meth
	            o.methodtable[a] := meth
	            IF mmeth.name
	               hln := getLabelHLN(table + mmeth.name)
	               meth.name := hln.name -> name of method
	               meth.object := o
	            ENDIF
	            meth.offset := mmeth.offset + link_codesize
	            ->IF g_symbolhunk THEN addMethDbgSym(o.name, meth.name, meth.offset)
	            -> 2.1 make available to symbolhunk
	            meth.dblabnext := g_dblablist
	            g_dblablist := meth
	            mmeth := mmeth + mmeth.totalsize -> fix 1.9.0
	         ENDFOR
	      ENDIF
	      mo := mo + mo.totalsize
	   ENDWHILE
	ENDIF


	i := m.procinfo
	IF i
	   i := i + m
	   mproc := i+ SIZEOF modinfo
	   count := i.count+1
	   WHILE count--
	      NEW codelab
	      codelab.codelink := m.modextens.codelist -> v38
	      m.modextens.codelist := codelab  -> v38
	      hln := getLabelHLN(table + mproc.name)
	      codelab.name := hln.name
	      codelab.offset := mproc.offset + link_codesize
	      -> 2.1 make available to symbolhunk
	      codelab.dblabnext := g_dblablist
	      g_dblablist := codelab

	      mproc := mproc + mproc.totalsize
	   ENDWHILE
	ENDIF


ENDPROC


PROC readMacroInfo(m:PTR TO moduleheader)
	DEF hln:PTR TO hln, d:PTR TO macro, h:PTR TO modmacro, strtable
	DEF a, t, iptr:PTR TO INT, lptr:PTR TO LONG, i:PTR TO modinfo, count

	#ifdef DBG_BMOD
	DEBUGF('readMacroInfo()\n')
	#endif

	i := m.strtabinfo + m
	strtable := i + SIZEOF modinfo

	i := m.macroinfo
	IF i = NIL THEN RETURN
	i := i + m
	h := i + SIZEOF modinfo
	count := i.count+1

	WHILE count--
	   d := h
	   hln := getLabelHLN(strtable + h.name)
	   IF d.type = 2
	      IF hln.ident2 THEN reportErr('multiple definition', hln.name)
	      hln.ident2 := d
	   ELSE
	      IF hln.ident THEN reportErr('multiple definition', hln.name)
	      hln.ident := d -> add to hash
	   ENDIF
	   d.name := hln.name
	   lptr := h + SIZEOF modmacro
	   IF Odd(d.type) THEN lptr := lptr + Mul(d.nrofargs,4) -> definefunc
	   d.ascii := lptr
	   #ifdef DBG_BMOD
	   DEBUGF('macro \s,\d:\n\s\n', d.name, d.nrofargs, d.ascii)
	   #endif
	   h := h + h.totalsize
	   d.identID := IF d.type = 2 THEN IDENT2_ASMMACRO ELSE IDENT_MACRO  -> overwrites totalsize !
	ENDWHILE
ENDPROC


PROC readConstInfo(m:PTR TO moduleheader)
	DEF len, i:PTR TO modinfo, c:PTR TO const, hln:PTR TO hln, table, lptr:PTR TO LONG
	DEF str[300]:STRING

	#ifdef DBG_BMOD
	DEBUGF('readConstInfo()\n')
	#endif

	i := m.strtabinfo + m
	table := i + SIZEOF modinfo

	i := m.constinfo
	IF i = NIL THEN RETURN
	i := i + m
	lptr := i + SIZEOF modinfo
	len := i.count+1 -> nr of consts
	WHILE len--
	   #ifdef DBG_BMOD
	   DEBUGF('lptr[]=$\h, len=\d name = "\s"\n', lptr[], len, table + lptr[])
	   #endif
	   hln := getLabelHLN(table + lptr[])
	   IF hln.ident
	      IF hln.ident.identID = IDENT_CONST
	         IF hln.ident::const.value <> lptr[1]
	            StringF(str, 'multiple definitions of constant "\s"', hln.name)
	            addWarning(str)
	         ENDIF
	      ELSE
	         reportErr('multiple definition', hln.name)
	      ENDIF
	   ENDIF
	   lptr[] := IDENT_CONST
	   hln.ident := lptr
	   lptr := lptr + 8
	ENDWHILE
ENDPROC

PROC readObjectInfo(m:PTR TO moduleheader)
	DEF mo:PTR TO modobject, o:PTR TO object
	DEF mmemb:PTR TO modmember, memb:PTR TO member
	DEF mmeth:PTR TO modproc, meth:PTR TO proc
	DEF p:PTR TO var, table
	DEF a, b, hln:PTR TO hln, i:PTR TO modinfo, count
	DEF mpar:PTR TO modarg, mcl:PTR TO modclass

	#ifdef DBG_BMOD
	DEBUGF('readObjectInfo()\n')
	#endif

	i := m.strtabinfo + m
	table := i + SIZEOF modinfo


	i := m.objectinfo
	IF i = NIL THEN RETURN
	i := i + m
	mo := i + SIZEOF modinfo
	count := i.count+1

	WHILE count--
	   NEW o
	   o.identID := IDENT2_OBJECT
	   hln := getLabelHLN(table + mo.name)
	   IF hln.ident2 THEN reportErr('multiple definitions of object', hln.name)
	   o.name := hln.name
	   hln.ident2 := o
	   o.nrofmembers := mo.nrofmembers
	   o.nrofmethods := mo.nrofmethods
	   o.sizeof := mo.sizeof
	   o.flags := mo.flags -> 1.8.1
	   IF mo.alignment -> changed 1.5.6
	      o.alignment := mo.alignment
	   ELSE
	      o.alignment := 2
	   ENDIF
	   NEW o.membertable[o.nrofmembers+1]
	   NEW o.methodtable[o.nrofmethods+1]
	   mmemb := mo + SIZEOF modobject
	   FOR a := 0 TO o.nrofmembers-1
	      memb := mmemb
	      IF i.misc = 0 -> v50. if pre v50
	         IF memb.size = 8
	            memb.flags := memb.flags OR MEMBF_FLOAT
	         ELSEIF memb.size > 1 -> 2 or 4
	            IF memb.esize <> 1 THEN memb.flags := memb.flags OR MEMBF_SIGNED
	         ENDIF
	      ENDIF
	      o.membertable[a] := memb
	      hln := getLabelHLN(table + mmemb.name)
	      memb.name := hln.name
	      IF mmemb.object
	         memb.object := -getLabelHLN(table + mmemb.object) -> !! late binding !!
	      ENDIF
	      mmemb++
	   ENDFOR
	   mcl := mmemb
	   IF o.nrofmethods
	         o.ltype := LTYPE_CLASS
	         o.offset := IF g_optmodule THEN NIL ELSE mcl.offset + link_codesize
	         o.classofs := mcl.classofs
	         o.destofs := mcl.destofs
	         o.codelink := m.modextens.codelist
	         o.modmethtab := m + m.codeinfo + SIZEOF modinfo + mcl.offset -> 1.10.0
	         m.modextens.codelist := o
	         mcl := mcl + SIZEOF modclass
	   ENDIF
	   mmeth := mcl
	   FOR a := 0 TO o.nrofmethods-1
	      NEW meth
	      o.methodtable[a] := meth
	      IF mmeth.name
	         hln := getLabelHLN(table + mmeth.name)
	         meth.name := hln.name -> name of method
	         meth.nrofargs := mmeth.nrofargs
	         meth.nrofdefaults := mmeth.nrofdefaults
	         meth.offset := IF g_optmodule THEN NIL ELSE mmeth.offset + link_codesize
	         meth.selfreg := mmeth.selfreg
	         meth.flags := mmeth.flags
	         return2mret(mmeth.return, meth.mret) -> v49
	         g_codegen.abiReturns(meth.mret) -> v49
	         meth.exported := TRUE -> v47
	         meth.object := o -> fix 200805
	         NEW meth.argarray[meth.nrofargs+1]
	         mpar := mmeth + SIZEOF modproc
	         FOR b := 0 TO meth.nrofargs-1
	            meth.argarray[b] := mpar
	            mpar.name := mpar.name + table -> 1.9.0 fix for saving name for makeModule()
	            mpar++
	         ENDFOR
	         mmeth := IF mmeth.totalsize THEN mmeth + mmeth.totalsize ELSE mpar -> fixed 1.9.0
	         ->IF g_symbolhunk THEN addMethDbgSym(o.name, meth.name, meth.offset) -> 1.6.0
	         -> 2.1 make available to symbolhunk
	         meth.dblabnext := g_dblablist
	         g_dblablist := meth
	      ELSE
	         meth.name := NIL
	         mmeth := mmeth + mmeth.totalsize -> fixed 1.9.0
	      ENDIF
	   ENDFOR
	   mo := mo + mo.totalsize
	ENDWHILE

ENDPROC

PROC readProcInfo(m:PTR TO moduleheader)
	DEF modproc:PTR TO modproc
	DEF proc:PTR TO proc, lab:PTR TO codelab
	DEF param:PTR TO arg
	DEF a, table, i:PTR TO modinfo, count
	DEF hln:PTR TO hln
	DEF modparams:PTR TO modarg, static:PTR TO statlab

	#ifdef DBG_BMOD
	DEBUGF('readProcInfo()\n')
	#endif

	i := m.strtabinfo + m
	table := i + SIZEOF modinfo

	i := m.procinfo
	IF i = NIL THEN RETURN
	i := i + m
	modproc := i + SIZEOF modinfo
	count := i.count+1

	WHILE count--
	   IF modproc.totalsize = SIZEOF modlabel
	      proc := FASTNEW(SIZEOF codelab)
	      proc.identID := IDENT_LABEL
	      proc.ltype := LTYPE_LAB
	      -> 2.1 make available to symbolhunk
	      proc.dblabnext := g_dblablist
	      g_dblablist := proc
	   ELSEIF modproc.totalsize = SIZEOF modstatic  -> 1.10.0
	      NEW static
	      static.identID := IDENT_LABEL
	      static.ltype := LTYPE_STATIC
	      static.deref.flags := modproc::modstatic.statflags
	      static.deref.esize := modproc::modstatic.statesize
	      static.deref.size := 0
	      static.deref.numes := 0
	      static.sizeof := modproc::modstatic.sizeof -> v2.2
	      IF static.deref.esize = 255
	         static.deref.object := 0-getLabelHLN(modproc::modstatic.statobj + table)
	         -> we resolve it later ! (codegen)
	      ENDIF
	      proc := static
	   /*
	   ELSEIF modproc.totalsize = SIZEOF oldmodstatic  ->
	      NEW static
	      static.identID := IDENT_LABEL
	      static.ltype := LTYPE_STATIC
	      static.deref.flags := modproc::oldmodstatic.statflags
	      static.deref.esize := modproc::oldmodstatic.statesize
	      static.deref.size := 0
	      static.deref.numes := 0
	      IF static.deref.esize = 255
	         static.deref.object := 0-getLabelHLN(modproc::modstatic.statobj + table)
	         -> we resolve it later ! (codegen)
	      ENDIF
	      proc := static
	   */
	   ELSE
	      NEW proc
	      proc.identID := IDENT_LABEL
	      proc.ltype := LTYPE_PROC
	      proc.cpu := m.cpu
	      proc.nrofargs := modproc.nrofargs
	      proc.nrofdefaults := modproc.nrofdefaults
	      return2mret(modproc.return,proc.mret) -> v49
	      g_codegen.abiReturns(proc.mret) -> v49
	      NEW proc.argarray[proc.nrofargs+1]
	      modparams := modproc + SIZEOF modproc
	      FOR a := 0 TO proc.nrofargs-1
	         param := modparams
	         param.identID := modparams.name + table -> 1.10.0 store name for use in librarymode
	         proc.argarray[a] := param
	         IF (g_optpowerpc = CPU_PPC) AND (proc.cpu = CPU_M68)  -> for 68k proc's called by ppc
	            param.rtype := PPCARGRTYPE_RX
	            param.rnum := a+3
	         ENDIF
	         modparams++
	      ENDFOR
	      -> 2.1 make available to symbolhunk
	      proc.dblabnext := g_dblablist
	      g_dblablist := proc
	   ENDIF

	   proc.codelink := m.modextens.codelist -> v38
	   m.modextens.codelist := proc  -> v38

	   hln := getLabelHLN(table + modproc.name)
	   IF hln.ident
	      IF hln.ident.identID = IDENT_LFUNC
	         -> it has been "lfunc-iised"
	      ELSE
	         reportErr('multiple definition', hln.name)
	      ENDIF
	   ENDIF

	   hln.ident := proc
	   proc.name := hln.name

	   #ifdef DBG_BMOD
	   DEBUGF('   \s\n', proc.name)
	   #endif

	   proc.offset := IF g_optmodule THEN NIL ELSE modproc.offset + link_codesize

	   modproc := modproc + modproc.totalsize
	ENDWHILE

ENDPROC

-> 2.0
PROC newBaseGlob(hln:PTR TO hln)
	DEF l:PTR TO gvar
	NEW l
	l.identID := IDENT_VARIABLE
	l.vtype := VTYPE_GLOB
	l.o := VAR
	l.d := l -> !
	l.hln := hln
	l.type.size := PTRSIZE
	l.type.flags := MEMBF_SIGNED -> 1.5.1

	hln.ident := l
	l.trok := TRUE

	l.next := g_gvarlist
	g_gvarlist := l
	l.gtype := GTYPE_BASE

	IF g_optmodule
	   l.link := 1
	   l.export := 1
	ENDIF
ENDPROC l

PROC readLibiInfo(m:PTR TO moduleheader)
	DEF lh:PTR TO modlibi, l:PTR TO gvar, override=FALSE
	DEF hlf:PTR TO modlfunc, lf:PTR TO lfunc
	DEF a, hln:PTR TO hln, table, i:PTR TO modinfo, count
	DEF tstr[256]:STRING

	#ifdef DBG_BMOD
	DEBUGF('readLibiInfo().. ')
	#endif

	i := m.strtabinfo + m
	table := i + SIZEOF modinfo

	i := m.libiinfo
	IF i = NIL THEN RETURN
	i := i + m
	lh := i + SIZEOF modinfo
	count := i.count+1


	WHILE count--

	   hlf := lh + SIZEOF modlibi

	   -> cleaned up 1.10.0
	   hln := getLabelHLN(table + lh.basename)
	   #ifdef DBG_BMOD
	   DEBUGF('   base = \s\n ', hln.name)
	   #endif
	   l := hln.ident
	   IF l
	      IF l.identID <> IDENT_VARIABLE THEN reportErr('symbol collision', hln.name)
	      IF l.gtype <> GTYPE_INTERNAL -> 1.7.1
	         l.gtype := GTYPE_BASE -> it was xref before
	      ENDIF
	      override := TRUE
	   ELSE
	      l := newBaseGlob(hln)
	   ENDIF



	   FOR a := 0 TO lh.nroflfuncs-1
	      lf := hlf  -> we reuse the memory !
	      hln := getLabelHLN(table + hlf.name)
	      IF hln.ident
	         -> avoid double declaration error if we did "override" a librarymodule (exec fex) 1.7
	         IF hln.ident.identID <> IDENT_IFUNC -> v56. ifuncs might "override" lfuncs
	            IF override = FALSE THEN reportErr('multiple definition', hln.name)
	            hln.ident := lf
	         ENDIF
	      ELSE
	         hln.ident := lf
	      ENDIF

	      lf.name := hln.name
	      -> rest is just as-is
	      lf.basehln := l.hln
	      lf.raise := NIL
	      #ifdef DBG_BMOD
	      DEBUGF('   \s(/\d) @ \d. totalsize=\d\n', hln.name, lf.nrofargs, lf.baseofs, hlf.totalsize)
	      #endif
	      hlf := hlf + hlf.totalsize
	      lf.identID := IDENT_LFUNC -> overwrites hlf.totalsize
	   ENDFOR


	   #ifdef DBG_BMOD
	   DEBUGF('   \d lfuncs\n', lh.nroflfuncs)
	   #endif
	   lh := lh + lh.totalsize


	ENDWHILE



ENDPROC

-> 1.10.0: tries reading module from multiple paths
-> v58: heavily improved
PROC readModuleModuledir(s, name)
	DEF dir, err, m

	#ifdef DBG_BMOD
	DEBUGF('readModuleModuledir("\s", "\s"\n', s, name)
	#endif

	dir := g_moduledir
	WHILE dir
	   StrCopy(name, dir)
	   StrAdd(name, s)
	   StrAdd(name, '.m')
	   IF findModule(g_modulelist, name) THEN RETURN NIL, NIL
	   dir := Next(dir)
	ENDWHILE

	dir := g_moduledir
	WHILE dir
	   StrCopy(name, dir)
	   StrAdd(name, s)
	   StrAdd(name, '.m')
	   m, err := findCachedModule(name)
	   IF m = NIL
	      IF err = "DUM"
	         -> do nada
	      ELSEIF err
	         RETURN NIL, err
	      ELSE -> was not found in cache
	         m, err := readModule(name)
	         IF m = NIL
	            IF err <> "OPEN" THEN RETURN NIL, err
	         ELSE
	            RETURN m, NIL
	         ENDIF
	      ENDIF
	   ELSE
	      RETURN m, NIL
	   ENDIF
	   dir := Next(dir)
	ENDWHILE

ENDPROC NIL, "OPEN"

PROC loadModule(n)
	DEF name[512]:STRING, err, lptr:PTR TO LONG
	DEF m:PTR TO moduleheader, i:PTR TO modinfo, t
	DEF s, str[512]:STRING, tstr[200]:STRING
	DEF count, modref:PTR TO modref, gvar:PTR TO gvar, table, hln:PTR TO hln
	DEF gd:PTR TO xglobderef

	StrCopy(str, n)
	LowerStr(str)

	#ifdef DBG_BMOD
	DEBUGF('loadModule(\s)\n', n)
	#endif

	s := str

	IF s[] = "*"
	   s++
	   SetStr(name,0)
	   StrCopy(name, g_sourcename, PathPart(g_sourcename)-g_sourcename)
	   WHILE s[] = "/"
	      s++
	      SetStr(name, PathPart(name)-name)
	   ENDWHILE
	   IF name[EstrLen(name)-1] <> ":" THEN StrAdd(name, '/') -> 1.7.0 more fix
	   StrAdd(name, s)
	   StrAdd(name, '.m')
	   IF findModule(g_modulelist, name) THEN RETURN NIL
	   m, err := findCachedModule(name)
	   IF m = NIL
	      IF err THEN Throw(err, name)
	      m, err := readModule(name)
	      IF m = NIL THEN Throw(err, name)
	   ENDIF
	ELSE
	   IF InStr(s, ':') > -1 -> 1.10.0
	      StrCopy(name, s)
	      StrAdd(name, '.m')
	      IF findModule(g_modulelist, name) THEN RETURN NIL
	      m, err := findCachedModule(name)
	      IF m = NIL
	         IF err THEN Throw(err, name)
	         m, err := readModule(name)
	         IF m = NIL THEN Throw(err, name)
	      ENDIF
	   ELSE
	      m, err := readModuleModuledir(s, name)
	      IF m = NIL
	         IF err = NIL THEN RETURN -> double (already in modulelist)
	         Throw(err, name)
	      ENDIF
	   ENDIF
	ENDIF

	IF m.modversion < MODULEVERSION THEN reportErr('too old moduleformat', name)

	IF (m.cpu <> CPU_NONE) AND (g_optpowerpc <> CPU_NONE)
	   IF m.cpu <> g_optpowerpc
	      IF m.codeinfo THEN addWarning('module is compiled for different target', name)
	   ENDIF
	ENDIF

	-> 1.6
	IF g_optosversion
	   IF m.codeinfo
	      IF g_optosversion < m.osversion THEN addWarning('module has higher OS version', name)
	   ENDIF
	ENDIF

	g_maxmodosvers := Max(g_maxmodosvers, m.osversion)

	-> 1.10.0
	IF g_optosid <> OSID_NONE
	   IF m.osid <> OSID_NONE
	      IF g_optosid <> m.osid
	         IF m.codeinfo THEN addWarning('module is compiled for different target', name)
	      ENDIF
	   ENDIF
	ENDIF

	-> v2.2
	IF m.headsize > SIZEOF moduleheader
	   count := Shr(m.headsize - SIZEOF moduleheader,2) + 1
	   lptr := m + SIZEOF moduleheader
	   WHILE count--
	      IF lptr[]++ THEN addWarning('module might need newer version of ECX', name)
	   ENDWHILE
	ENDIF


	NEW m.modextens

	IF str[] = "*"
	   StrCopy(name, str+1)
	   StrAdd(name, '.m')
	   m.modextens.rname := String(EstrLen(name))
	   IF m.modextens.rname = NIL THEN Raise("MEM")
	   StrCopy(m.modextens.rname, name)
	   ->THEN m.modextens.rname := m.mname + InStr(m.mname, s) -> used by submod write
	ENDIF

	AddTail(g_modulelist, m)

	readLibiInfo(m)
	IF g_optpreprocess THEN readMacroInfo(m)
	readConstInfo(m)
	readObjectInfo(m)

	   -> v55 moved outside IF m.codeinfo
	    -> v53 moved outside "IF g_optmodule"
	    -> v49, moved here from linkModules() ,we need to allocate all globals
	    -> before linking modules.

	   i := m.xrefginfo
	   IF i
	      i := i + m
	      count := i.count + 1
	      modref := i + SIZEOF modinfo
	      table := m + m.strtabinfo + SIZEOF modinfo
	      WHILE count--
	         hln := getLabelHLN(modref.name + table)
	         gvar := hln.ident
	         #ifdef DBG_BMOD
	         DEBUGF('   xrefglob "\s", hln.ident=$\h\n', hln.name, hln.ident)
	         #endif
	         IF gvar = NIL
	            -> changed 1.6.1
	            NEW gvar
	            gvar.next := g_gvarlist
	            g_gvarlist := gvar
	            gvar.identID := IDENT_VARIABLE
	            gvar.gtype := GTYPE_XREF
	            gvar.hln := hln
	            gvar.type.esize := 1
	            gvar.type.size := IF modref.ltype THEN modref.ltype ELSE 4
	            gvar.type.flags := IF modref.ltype THEN modref.info ELSE MEMBF_SIGNED
	            gvar.vtype := VTYPE_GLOB
	            gvar.hln.ident := gvar
	            gvar.o := IF gvar.type.size = 4 THEN VAR ELSE VAR64 -> 1.6.1
	            gvar.d := gvar
	            gvar.trok := REUSEREGS
	            gvar.link := IF g_optmodule THEN 1 ELSE 0
	            gvar.export := 1
	         ELSE
	            gvar.usage := 1 -> 1.10.0
	         ENDIF
	         modref := modref + SIZEOF modref + Mul(modref.numrefs,4)
	      ENDWHILE
	   ENDIF

	   i := IF m.headsize < 160 THEN NIL ELSE m.xglobderefinfo  -> 2.2
	   IF i
	      i := i + m
	      count := i.count + 1
	      gd := i + SIZEOF modinfo
	      table := m + m.strtabinfo + SIZEOF modinfo
	      WHILE count--
	         hln := getLabelHLN(gd.name + table)
	         gvar := hln.ident
	         #ifdef DBG_BMOD
	         DEBUGF('   xglobderef "\s", hln.ident=$\h\n', hln.name, hln.ident)
	         #endif
	         gvar.type.object := NIL
	         IF gd.esize = 255
	            hln := getLabelHLN(gd.object + table)
	            IF hln.ident2
	               gvar.type.object := hln.ident2
	               gvar.type.esize := 255
	            ELSE
	               gvar.type.object := NIL
	               gvar.type.esize := 1
	            ENDIF
	         ELSE
	            gvar.type.object := NIL
	            gvar.type.esize := gd.esize
	         ENDIF
	         gd := gd + gd.totalsize
	      ENDWHILE
	   ENDIF

	IF m.codeinfo

	   IF g_linkobjmode -> 1.7.1
	      addWarning('code from module(s) skipped due to LINKOBJECT mode')
	      RETURN
	   ENDIF

	   readProcInfo(m)


	   IF g_optmodule = FALSE

	      i := m.codeinfo + m
	      i.misc := link_codesize
	      link_codesize := link_codesize + Shl(i.count,2)

	      i := m.globinfo
	      IF i
	         i := i + m
	         IF i.misc -> 1.6.1
	            t := i.misc -> new way is to save total size in i.misc
	            i.misc := g_globalsize
	            g_globalsize := g_globalsize + t
	         ELSE
	            i.misc := g_globalsize
	            g_globalsize := g_globalsize + Shl(i.count,2)
	         ENDIF
	      ENDIF

	      i := m.datainfo
	      IF i
	         i := i + m
	         i.misc := link_datasize -> needs fix later
	         link_datasize := link_datasize + Shl(i.count,2)
	      ENDIF



	      g_stacksize := g_stacksize + m.stacksize

	    ENDIF

	ENDIF

ENDPROC

->1.6.1 big changes
-> v49, before linkModules:
PROC allocateGlobals()
	DEF gvar:PTR TO gvar

	#ifdef DBG_MAIN
	  DEBUGF('allocateGlobals()\n')
	  #endif

	gvar := g_gvarlist
	WHILE gvar
	   IF gvar.gtype <> GTYPE_INTERNAL
	      gvar.breg := GLOBREG -> 1.10.0 moved here from load module
	      IF g_optmodule=FALSE
	         gvar.offset := g_globalsize
	         g_globalsize := g_globalsize + gvar.type.size -> 1.6.1
	         #ifdef DBG_EXE
	         DEBUGF('allocated global "\s",offset=\d,gtype=\d,usage=\d,breg=\d\n',
	         gvar.hln.name,gvar.offset,gvar.gtype, gvar.usage, gvar.breg)
	         #endif
	      ELSE  -> module
	         IF gvar.export = FALSE
	            gvar.offset := g_globalsize
	            g_globalsize := g_globalsize + gvar.type.size -> 1.6.1
	            #ifdef DBG_EXE
	            DEBUGF('private global "\s",offset=\d,gtype=\d,usage=\d,breg=\d\n',
	            gvar.hln.name,gvar.offset,gvar.gtype, gvar.usage, gvar.breg)
	            #endif
	         ELSE
	            #ifdef DBG_EXE
	            DEBUGF('xref global "\s",offset=\d,gtype=\d,usage=\d,breg=\d\n',
	            gvar.hln.name,gvar.offset,gvar.gtype, gvar.usage, gvar.breg)
	            #endif
	         ENDIF
	      ENDIF
	   ELSE
	      #ifdef DBG_EXE
	      DEBUGF('internal global "\s",offset=\d,gtype=\d,usage=\d,breg=\d\n',
	      gvar.hln.name,gvar.offset,gvar.gtype, gvar.usage, gvar.breg)
	      #endif
	   ENDIF
	   gvar := gvar.next
	ENDWHILE

	 #ifdef DBG_MAIN
	 DEBUGF('allocateGlobals() DONE\n')
	 #endif

ENDPROC


PROC loadSecondaryModules() -> V47
	DEF mod:PTR TO moduleheader, seclist:mlh, next

	IF g_secondariesloaded THEN RETURN
	IF g_optmodule THEN RETURN

	mod := g_modulelist.head
	seclist.head := seclist + 4
	seclist.tail := NIL
	seclist.tailpred := seclist
	WHILE mod.succ
	   loadSecondary(mod, seclist)
	   mod := mod.succ
	ENDWHILE
	mod := seclist.head
	WHILE mod.succ
	   next := mod.succ
	   Remove(mod)
	   AddTail(g_modulelist, mod)
	   mod := next
	ENDWHILE
	g_secondariesloaded := TRUE
ENDPROC

PROC loadSecondary(mod:PTR TO moduleheader, seclist:PTR TO mlh) -> V47
	DEF i:PTR TO modinfo, mm:PTR TO modmodule, submod:PTR TO moduleheader, c, t
	DEF str[700]:STRING, name[700]:STRING, strtab, err
	DEF hln:PTR TO hln, gvar:PTR TO gvar, count, modref:PTR TO modref
	DEF lptr:PTR TO LONG

	IF mod.moduleinfo

	   strtab := mod.strtabinfo + mod + SIZEOF modinfo

	   i := mod + mod.moduleinfo
	   c := i.count+1
	   mm := i + SIZEOF modinfo
	   WHILE c--

	      t := mm.name
	      IF t < 0 -> relative name ?
	         t := -t
	         SetStr(name, 0)
	         StrCopy(name, mod.mname, PathPart(mod.mname)-mod.mname)

	         t := t + strtab
	         WHILE t[] = "/"
	            t++
	            SetStr(name, PathPart(name)-name)
	         ENDWHILE
	         IF name[EstrLen(name)-1] <> ":" THEN StrAdd(name, '/') -> 1.7.1
	         StrAdd(name, t)
	         t := name
	      ELSE
	         t := t + strtab
	      ENDIF

	      #ifdef DBG_BMOD
	      DEBUGF('submodule \a\s\a referenced FROM MODULE \s\n', t, mod.mname)
	      #endif

	      submod := findModule(g_modulelist, t)
	      IF submod = FALSE THEN submod := findModule(seclist, t)

	      IF submod = FALSE

	         submod, err := findCachedModule(t)
	         IF submod = NIL
	            IF err THEN Throw(err, t)
	            submod, err := readModule(t)
	            IF submod = NIL THEN Throw(err, t)
	         ENDIF

	         NEW submod.modextens
	         AddTail(seclist, submod)

	         IF submod.modversion < MODULEVERSION THEN reportErr('too old moduleformat', t)

	         IF (submod.cpu <> CPU_NONE) AND (g_optpowerpc <> CPU_NONE)
	            IF submod.cpu <> g_optpowerpc
	               IF submod.codeinfo THEN addWarning('module is compiled for different target', t)
	            ENDIF
	         ENDIF

	         IF g_optosid <> OSID_NONE
	            IF submod.osid <> OSID_NONE
	               IF g_optosid <> submod.osid
	                  IF submod.codeinfo THEN addWarning('module is compiled for different target', t)
	               ENDIF
	            ENDIF
	         ENDIF

	         -> v2.2
	         IF submod.headsize > SIZEOF moduleheader
	            count := Shr(submod.headsize - SIZEOF moduleheader,2) + 1
	            lptr := submod + SIZEOF moduleheader
	            WHILE count--
	               IF lptr[]++ THEN addWarning('module might need newer version of ECX', t)
	            ENDWHILE
	         ENDIF

	         submod.modextens.referenced := TRUE -> prevent removal.
	         submod.modextens.secondary := TRUE

	         readSecondaryInfo(submod) -> adds proc/label/classes to submod.codelist

	         i := submod.codeinfo + submod
	         i.misc := link_codesize
	         link_codesize := link_codesize + Shl(i.count,2)

	         i := submod.globinfo
	         IF i
	            i := i + submod -> 1.10.0 fix
	            IF i.misc -> 1.7.1
	               t := i.misc -> new way is to save total size in i.misc
	               i.misc := g_globalsize
	               g_globalsize := g_globalsize + t
	            ELSE
	               i.misc := g_globalsize
	               g_globalsize := g_globalsize + Shl(i.count,2)
	            ENDIF
	         ENDIF

	         i := submod.datainfo
	         IF i
	            i := i + submod
	            i.misc := link_datasize -> needs fix later
	            link_datasize := link_datasize + Shl(i.count,2)
	         ENDIF

	         i := submod.xrefginfo
	         IF i
	            i := i + submod
	            count := i.count + 1
	            modref := i + SIZEOF modinfo
	            WHILE count--
	               hln := getLabelHLN(modref.name + submod + submod.strtabinfo + SIZEOF modinfo)
	               gvar := hln.ident
	               #ifdef DBG_BMOD
	               DEBUGF('   secondary xrefglob "\s", hln.ident=$\h\n', hln.name, hln.ident)
	               #endif
	               IF gvar = NIL
	                  -> changed 1.6.1
	                  NEW gvar
	                  gvar.next := g_gvarlist
	                  g_gvarlist := gvar
	                  gvar.identID := IDENT_VARIABLE
	                  gvar.gtype := GTYPE_XREF
	                  gvar.hln := hln
	                  gvar.type.size := IF modref.ltype THEN modref.ltype ELSE 4
	                  gvar.type.flags := IF modref.ltype THEN modref.info ELSE MEMBF_SIGNED
	                  gvar.vtype := VTYPE_GLOB
	                  gvar.hln.ident := gvar
	                  gvar.o := IF gvar.type.size = 4 THEN VAR ELSE VAR64 -> 1.6.1
	                  gvar.d := gvar
	                  gvar.trok := REUSEREGS
	                  gvar.link := IF g_optmodule THEN 1 ELSE 0
	               ELSE
	                  gvar.usage := 1 -> 1.10.0
	               ENDIF
	               modref := modref + SIZEOF modref + Mul(modref.numrefs,4)
	            ENDWHILE
	         ENDIF

	         g_stacksize := g_stacksize + submod.stacksize

	         loadSecondary(submod, seclist) -> was forgotten before

	      ENDIF

	      mm.name := submod -> !!
	      mm := mm + mm.totalsize
	   ENDWHILE

	ENDIF

ENDPROC

/**************************************************************
***************************************************************
******************** (SYNTAX) PARSER **************************
***************************************************************
**************************************************************/

-> changed v49, 50
PROC useVar(var:PTR TO var, isexp)
	DEF add
	#ifdef DBG_PASS1
	DEBUGF('useVar() "\s" ld=\d use=\d isexp=\d\n', var.hln.name, g_currentloopdepth, var.usage, isexp)
	#endif
	IF g_currentproc<>NIL
	   add := IF isexp THEN ASSIGNUSAGE ELSE SIMPLEUSAGE
	   IF g_currentloopdepth THEN add := LoopUsageMod(add)
	   IF g_currentselectdepth THEN add := SelectUsageMod(add)
	   IF g_currentifdepth THEN add := IfUsageMod(add)
	   var.usage := var.usage + add
	ENDIF
	IF var.usage = 0 THEN var.usage := 1
	var.nextuse := var.nextuse + 1 -> v50
ENDPROC

PROC parseELine()-> HANDLE  -> was parse1()
	DEF d, d2, r:PTR TO item, n:REG PTR TO item
	DEF hln:PTR TO hln, ident:PTR TO ident, dest:REG PTR TO item
	DEF lptr:PTR TO LONG, lfunc:PTR TO lfunc
	DEF fh, flen, fbuf, fib:fileinfoblock
	DEF object:PTR TO object, memb:PTR TO member, memb2:PTR TO member
	DEF incval, mnum
	DEF err
	DEF t, exit, a, asm:PTR TO asm, reg:PTR TO reg
	DEF len, size, newbuf, va:PTR TO var
	DEF codelab:PTR TO codelab
	DEF str[300]:STRING
	DEF ohln:PTR TO hln
	DEF super:PTR TO object
	DEF loclab:PTR TO loclab


	n := itmz_linestart
	dest := n
	IF n.data <> 10 THEN reportIErr('parseELine(), didnt start with newline')
	setLinenum(n.info)
	n++
	dest.data := 10
	dest.info := g_linenum
	dest++ -> decrement away this newline if nothing is written !

	#ifdef DBG_PASS1
	DEBUGF('parseELine(): n.data=\d, line=\d\n', n.data, g_linenum)
	->DEBUGF('AvailMem(MEMF_LARGEST)=$\h\n', AvailMem($20000))
	#endif

pel_uuh:

	   t := n.data
	   SELECT 256 OF t
	   CASE KW_OPT
	      IF g_proclist OR g_librarymode OR (IsListEmpty(g_modulelist)=FALSE)
	         reportErr('scope', 'OPT')
	      ENDIF
	      n := opt_key(n)
	   CASE KW_MODULE
	      IF g_proclist THEN reportErr('scope', 'MODULE')
	      initTarget()
	      REPEAT
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	         IF n.data <> IT_STRING THEN reportErr('string expected')
	         loadModule(n.info)
	         n++
	      UNTIL n.data <> ","
	   CASE KW_CONST, KW_SET, KW_ENUM
	      n := const_key(n, itmz_export OR g_optexport)
	   CASE KW_LIBRARY
	      n := library_key(n)
	   CASE KW_LINKOBJECT -> 1.7.1
	      n++
	      n := linkobject_key(n)
	   CASE KW_DEF
	      n := def_key(n, itmz_export OR g_optexport)
	   CASE KW_OBJECT
	      IF g_currentobject OR g_currentproc THEN reportErr('scope', 'OBJECT')
	      n++
	      NEW object
	      g_currentobject := object
	      object.startline := g_linenum
	      object.next := g_objectlist
	      g_objectlist := object
	      IF n.data <> IT_LABEL THEN reportErr('label expected')
	      hln := n.info
	      IF n.num = 1 THEN reportErr('lexical', hln.name) -> no upper
	      /* double declaration ? */
	      IF hln.ident2 THEN reportErr('double declaration of object', hln.name)
	      hln.ident2 := object
	      object.identID := IDENT2_OBJECT
	      object.name := hln.name
	      object.exported := itmz_export OR g_optexport
	      object.alignment := 2 -> v49
	      n++
	      /* do OF (if there) */
	      -> 1.10.0: now allows multiple inheritance (not methods)
	      IF n.data = KW_OF
	         REPEAT
	            n++
	            /* get super-object */
	            IF n.data <> IT_LABEL THEN reportErr('label expected')
	            hln := n.info
	            IF n.num = 1 THEN reportErr('lexical', hln.name) -> no upper
	            super := hln.ident2
	            IF super = NIL THEN reportErr('unknown object', hln.name)
	            n++
	            IF object.super
	               IF super.nrofmethods
	                  reportErr('multiple inheritance of methods for object', hln.name)
	               ENDIF
	               FOR a := 0 TO super.nrofmembers-1
	                  NEW memb
	                  CopyMemQuick(super.membertable[a], memb, SIZEOF member)
	                  memb.offset := memb.offset + object.sizeof
	                  g_temparray[a + object.nrofmembers] := memb
	               ENDFOR
	            ELSE
	               FOR a := 0 TO super.nrofmembers-1
	                  memb := super.membertable[a]
	                  g_temparray[a + object.nrofmembers] := memb
	               ENDFOR
	               object.super := super
	            ENDIF
	            object.nrofmembers := object.nrofmembers + a
	            object.sizeof := object.sizeof + super.sizeof
	            object.alignment := Max(object.alignment, super.alignment)
	         UNTIL n.data <> ","
	      ENDIF
	   CASE KW_ENDOBJECT
	      object := g_currentobject
	      IF object = NIL THEN reportErr('ENDOBJECT scope')
	      n++
	      -> v49 smarter aligment
	      object.sizeof := object.sizeof + (object.alignment - 1) AND (0-object.alignment)
	      -> v1.5.1
	      IF object.sizeof > $7FFE THEN reportErr('OBJECT is too large', object.name)
	      -> so members are done, lets make a copy and keep it..
	      NEW object.membertable[object.nrofmembers+1] -> add one item in case we are empty..
	      CopyMemQuick(g_temparray, object.membertable, object.nrofmembers * 4)
	      g_currentobject := NIL
	      g_privatekey := FALSE
	   CASE KW_RAISE
	      n := raise_key(n)
	   CASE KW_PROC
	      n, dest := proc_key(n, dest, itmz_export OR g_optexport)
	   CASE KW_EXCEPT
	      IF g_currentproc = NIL THEN reportErr('EXCEPT scope')
	      IF g_currentproc.handle = FALSE THEN reportErr('EXCEPT without HANDLE')
	      g_currentproc.except := TRUE -> v58
	      dest.data := KW_EXCEPT
	      dest++
	      n++
	      IF n.data = KW_DO
	         dest.data := KW_DO
	         dest++
	         n++
	      ENDIF
	   CASE KW_ENDPROC, KW_RETURN
	      #ifdef DBG_PASS1
	      DEBUGF('ENDPROC\n')
	      #endif
	      IF g_currentproc = NIL THEN reportErr('scope', 'ENDPROC')
	      r := dest
	      dest.data := t
	      dest++
	      n++
	      n, dest, t := explist(n, dest)
	      r.num := t -> v49
	       -> give back old idents..!
	      IF r.data = KW_ENDPROC
	         restoreLocalIdents(g_currentproc)
	         g_currentproc := NIL
	      ENDIF
	   CASE KW_INCBIN
	      -> changed 1.8.0, now only checks files len and existanse
	      t := n - SIZEOF item
	      n++
	      IF n.data <> IT_STRING THEN reportErr('string expected')
	      SetStr(str, 0)
	      IF InStr(n.info, ':') = -1
	         StrCopy(str, g_sourcename, FilePart(g_sourcename)-g_sourcename) -> v49
	      ENDIF
	      StrAdd(str, n.info)
	      fh := Open(str, OLDFILE)
	      IF fh = NIL THEN Throw("OPEN", str)
	      IF ExamineFH(fh, fib) = NIL THEN Throw("EXAM", str)
	      g_incbinsize := g_incbinsize + fib.size
	      Close(fh)
	      n++
	      dest.data := KW_INCBIN
	      dest.info := StrCopy(String(EstrLen(str)),str)
	      dest.info2 := fib.size -> 1.8.0
	      dest++
	      /*
	      -> 2.2.1
	      IF t.data = ":"
	         t--
	         IF t.data = IT_LABEL
	            hln := t.info
	            codelab := hln.ident
	      */
	   CASE IT_ASM
	      dest.data := IT_ASM
	      asm := n.info
	      dest.info := asm
	      dest.num := n.num -> 2.2 asize
	      #ifdef DBG_PASS1
	      DEBUGF('parseEline/IT_ASM \s\n', asm.name)
	      #endif
	      dest++
	      n++
	      -> 1.8.0, not method anymore
	      SELECT g_optpowerpc
	      CASE CPU_M68 ;  n, dest := parse68kAsm(n,dest)
	      CASE CPU_PPC ;  n, dest := parsePPCAsm(n,dest)
	      DEFAULT      ; reportIErr('parseELine/parse asm cpu')
	      ENDSELECT
	   CASE KW_STATIC -> 1.9.0
	      n, dest := static_key(n, dest, itmz_export OR g_optexport)
	   CASE IT_LABEL
	      IF g_currentobject
	         object := g_currentobject
parsemember_again:
	         NEW memb
	         n, ohln := varmembdec(n, memb, NIL) -> now reports its own errors
	         IF g_privatekey THEN memb.flags := memb.flags OR MEMBF_PRIVATE -> v48
	         SELECT 17 OF memb.size
	         CASE 0 -> array
	            t := memb.esize
	            SELECT t
	            CASE 1 -> carray ?
	               memb.offset := IF g_naturalalign THEN object.sizeof ELSE object.sizeof + 1 AND -2
	               incval := memb.numes
	            CASE 2 -> iarray ?
	               memb.offset := object.sizeof + 1 AND -2
	               incval := memb.numes * 2
	            CASE 4 -> larray ?
	               IF (memb.flags AND MEMBF_FLOAT) OR g_naturalalign
	                  memb.offset := object.sizeof + 3 AND -4 -> v49
	               ELSE
	                  memb.offset := object.sizeof + 1 AND -2
	               ENDIF
	               incval := memb.numes * 4
	            CASE 8 -> darray ?
	               IF g_naturalalign
	                  memb.offset := object.sizeof + 7 AND -8
	               ELSE
	                  memb.offset := object.sizeof + 3 AND -4
	               ENDIF
	               incval := memb.numes * 8
	               object.alignment := Max(object.alignment, IF g_naturalalign THEN 8 ELSE 4) -> v49
	            CASE 255 -> oarray ?
	               memb.object := ohln.ident2
	               IF memb.object = NIL THEN reportErr('unknown object', ohln.name)
	               memb.offset := object.sizeof + (memb.object.alignment-1) AND (0-memb.object.alignment)  -> v1.5
	               incval := memb.numes * memb.object.sizeof
	            DEFAULT
	               reportIErr(' membertype', memb.name)
	            ENDSELECT
	         CASE 1 -> char
	            memb.offset := object.sizeof
	            incval := 1
	         CASE 2 -> int
	            memb.offset := object.sizeof + 1 AND -2
	            incval := 2
	         CASE 4 -> long / ptr / float
	            IF (memb.flags AND MEMBF_FLOAT) OR g_naturalalign
	               memb.offset := object.sizeof + 3 AND -4 -> v49
	            ELSE
	               memb.offset := object.sizeof + 1 AND -2
	            ENDIF
	            incval := 4
	            IF memb.esize = 255
	               memb.object := ohln -> ! backpatch later !
	               memb.size := 255 -> ! backpatch later !
	            ENDIF
	         CASE 8 -> double/wide/ptr64
	            IF g_naturalalign
	               memb.offset := object.sizeof + 7 AND -8
	            ELSE
	               memb.offset := object.sizeof + 3 AND -4
	            ENDIF
	            incval := 8
	            IF memb.esize = 255
	               memb.object := ohln -> ! backpatch later !
	               memb.size := 255 -> ! backpatch later !
	            ENDIF
	            object.alignment := Max(object.alignment, IF g_naturalalign THEN 8 ELSE 4) -> v49
	         DEFAULT
	            reportIErr(' membertype', memb.name)
	         ENDSELECT
	         IF n.data = "@" -> union ?
	            n++
	            IF n.data = IT_LABEL
	               FOR mnum := 0 TO object.nrofmembers-1
	                  memb2 := g_temparray[mnum]
	                  IF memb2.name = n.info.name
	                     memb.offset := memb2.offset
	                     mnum := object.nrofmembers -> exits
	                  ENDIF
	               ENDFOR
	               IF mnum <= object.nrofmembers THEN reportErr('unknown member', n.info.name)
	               n++
	            ELSE -> v45
	               n, d := getconstexp(n)
	               IF n = NIL THEN reportErr('union syntax')
	               memb.offset := d
	            ENDIF
	         ELSE
	            object.sizeof := memb.offset + incval
	         ENDIF
	         -> double declaration ?
	         FOR mnum := 0 TO object.nrofmembers-1
	            memb2 := g_temparray[mnum]
	            IF memb2.name = memb.name THEN reportErr('double declaration of member', memb.name)
	         ENDFOR
	         g_temparray[object.nrofmembers] := memb
	         object.nrofmembers := object.nrofmembers + 1
	         IF n.data = ","
	            n++
	            JUMP parsemember_again
	         ENDIF
	      ELSE -> vfunc/var(exp)/proc [<=> ..]/label:
	         hln := n.info
	         ident := hln.ident
	         IF ident
	            IF ident.identID = IDENT_VARIABLE
	               SELECT 256 OF n[1].data
	               CASE ".", "[", KW_PLUSPLUS, KW_MINMIN, KW_ASSIGN, KW_PTYPE, KW_MODIFY
	                  n,dest := varexp(n,dest,0,0)
	               CASE "," -> v49pre
	                  n, dest := multiassign(n, dest)
	               CASE "("
	                  n, dest := vfunction(n, dest)
	               CASE "="
	                  reportErr('assignment spelled wrong ?') -> 2.0
	               CASE KW_UNI
	                  n, dest := uniexp(n[1], n, 0)
	               DEFAULT
	                  reportErr('variable syntax', hln.name) -> 2.0
	               ENDSELECT
	            ELSEIF n[1].data = "(" -> backward proc ?
	               n, dest := function(n, dest)
	               IF n.data = KW_UNI
	                  n, dest := uniexp(n, dest--, 0)
	               ENDIF
	            ELSEIF n[1].data = ":" -> label:
	               reportErr('double declaration of label', hln.name)
	            ELSE
	               SELECT 256 OF n[1].data
	               CASE ".", "[", KW_PTYPE
	                  n, dest := labderef(n, dest)
	               DEFAULT
	                  reportErr('label syntax', hln.name)
	               ENDSELECT
	            ENDIF
	         ELSEIF n[1].data = ":" -> label:
	            dest.data := IT_LABEL
	            hln := n.info
	            dest.info := hln
	            NEW codelab
	            codelab.identID := IDENT_LABEL
	            codelab.ltype := LTYPE_LAB
	            codelab.name := hln.name
	            codelab.exported := itmz_export OR g_optexport
	            hln.ident := codelab
	            codelab.codelink := g_codelablist
	            g_codelablist := codelab
	            codelab.next := g_proclist
	            g_proclist := codelab
	            -> 2.1 make available to symbolhunk
	            codelab.dblabnext := g_dblablist
	            g_dblablist := codelab

	            dest++
	            n := n[2]
	            IF n.data THEN JUMP pel_uuh -> LONG/INT/CHAR/ASMinstruction may follow !
	         ELSEIF n[1].data = "(" -> forward proc ?
	            n, dest := function(n, dest)
	            IF n.data = KW_UNI
	               n, dest := uniexp(n, dest--, 0)
	            ENDIF
	         ELSE
	            reportErr('unknown label', hln.name)->reportErr('label syntax', hln.name)
	         ENDIF
	      ENDIF
	   CASE IT_IFUNC
	      n, dest := ifunction(n, dest)
	   CASE IT_LFUNC
	      n, dest := lfunction(n, dest)
	   CASE "["
	      n++
	      n, dest := immedlist2(n, dest,NIL)
	      IF n.data <> KW_UNI THEN reportErr('"<=>" expected')
	      n, dest := uniexp(n,dest--, 0) -> unification ?
	   CASE KW_CASE
	      dest.data := KW_CASE
	      dest++
	      n++
	      REPEAT
	         n, dest := expression(n,dest)
	         IF n.data = ","
	            dest.data := "," -> 1.8.0 now needed
	            dest++
	            n++
	            IF n.data = 10
	               setLinenum(n.info)
	               n++
	            ENDIF
	         ELSEIF n.data = KW_TO
	            dest.data := KW_TO
	            dest++
	            n++
	         ENDIF
	      UNTIL n.data = NIL
	   CASE KW_DOUBLE, KW_REAL -> v50
	      dest.data := KW_DOUBLE
	      dest++
	      n++
	      n, dest := doublelist(n,dest)
	   CASE KW_WIDE -> v56
	      dest.data := KW_WIDE
	      dest++
	      n++
	      n, dest := widelist(n,dest)
	   CASE KW_LONG, KW_PTR, KW_ULONG
	      dest.data := KW_LONG
	      dest++
	      n++
	      n, dest := longlist(n,dest)
	   CASE KW_FLOAT -> 4byte aligned, so uses own item
	      dest.data := KW_FLOAT
	      dest++
	      n++
	      n, dest := longlist(n,dest)
	   CASE KW_INT, KW_WORD
	      dest.data := KW_INT
	      dest++
	      n++
	      n, dest := intlist(n,dest)
	   CASE KW_CHAR, KW_BYTE
	      dest.data := KW_CHAR
	      dest++
	      n++
	      n, dest := charlist(n,dest)
	   CASE KW_NEW
	      enteringFunc()
	      REPEAT
	         n++
	         IF n.data = 10  -> 1.8.1
	            setLinenum(n.info)
	            n++
	         ENDIF
	         n, dest := varexp(n,dest,KW_NEW,0)
	         IF n.data = ","
	            dest.data := 10
	            dest.info := g_linenum
	            dest++
	         ENDIF
	      UNTIL n.data <> ","
	      leavingFunc(0)
	   CASE KW_END
	      enteringFunc()
	      REPEAT
	         n++
	         IF n.data = 10  -> 1.8.1
	            setLinenum(n.info)
	            n++
	         ENDIF
	         n, dest := varexp(n,dest,KW_END,0)
	         IF n.data = ","
	            dest.data := 10
	            dest.info := g_linenum
	            dest++
	         ENDIF
	      UNTIL n.data <> ","
	      leavingFunc(0)
	   CASE KW_SUPER
	      n++
	      n, dest := varexp(n, dest,KW_SUPER,0)
	   CASE KW_ELSEIF, KW_ELSEIFN
	      dest.data := KW_ELSEIF
	      dest.num := IF t = KW_ELSEIF THEN 0 ELSE 1 -> N bool
	      dest++
	      n++
	      n, dest := expression(n,dest)
	   CASE KW_EXIT, KW_EXITN
	      dest.data := KW_EXIT
	      dest.num := IF t = KW_EXIT THEN 0 ELSE 1 -> N bool
	      dest++
	      n++
	      n, dest := expression(n,dest)
	   CASE KW_VOID
	      dest.data := KW_VOID
	      dest++
	      n++
	      n, dest := expression(n,dest)
	   CASE KW_UNTIL, KW_UNTILN
	      dest.data := KW_UNTIL
	      dest.num := IF t = KW_UNTIL THEN 0 ELSE 1 -> N bool
	      dest++
	      n++
	      n, dest := expression(n,dest)
	      g_currentloopdepth--
	   CASE KW_INC, KW_DEC
	      dest.data := n.data
	      dest++
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('variable expected')
	      hln := n.info
	      ident := hln.ident
	      IF ident = NIL THEN reportErr('unknown identifier', hln.name)
	      IF ident.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	      dest.data := IT_VARIABLE
	      dest.info := ident
	      useVar(ident,1)
	      dest++
	      n++
	   CASE KW_JUMP
	      dest.data := KW_JUMP
	      dest++
	      n++
	      IF n.data = "." -> 2.2
	         -> local label
	         n++
	         IF n.data <> IT_LABEL THEN reportErr('label expected')
	         hln := n.info
	         loclab := getLocalLabel(hln.name)
	         hln := loclab.hln
	      ELSE
	         IF n.data <> IT_LABEL THEN reportErr('label expected')
	         hln := n.info
	      ENDIF
	      dest.data := IT_LABEL
	      dest.info := hln
	      dest++
	      n++
	   CASE KW_IF, KW_IFN
	      dest.data := KW_IF
	      dest.num := IF t = KW_IF THEN 0 ELSE 1 -> N bool
	      dest++
	      n++
	      n, dest := expression(n, dest)
	      g_currentifdepth++ -> v49
	      IF n.data = KW_THEN
	         dest.data := KW_THEN
	         dest++
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	         n, dest := substatement(n, dest)
	         IF n.data = KW_ELSE
	            dest.data := KW_ELSE
	            dest++
	            n++
	            IF n.data = 10
	               setLinenum(n.info)
	               n++
	            ENDIF
	            n, dest := substatement(n, dest)
	         ENDIF
	         g_currentifdepth-- -> v49
	      ENDIF
	   CASE KW_FOR
	      g_currentloopdepth++
	      dest.data := KW_FOR
	      dest++
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('variable expected')
	      hln := n.info
	      IF hln.ident = NIL THEN reportErr('unknown identifier', hln.name)
	      IF hln.ident.identID <> IDENT_VARIABLE THEN reportErr('variable expected')
	      dest.data := IT_VARIABLE
	      dest.info := hln.ident
	      useVar(hln.ident,1)
	      dest++
	      n++
	      IF n.data <> KW_ASSIGN THEN reportErr('":=" expected')
	      n++
	      n, dest := expression(n,dest)
	      IF n.data <> KW_TO THEN reportErr('"TO" expected')
	      n++
	      n, dest := expression(n, dest)
	      IF n.data = KW_STEP
	         dest.data := KW_STEP
	         dest++
	         n++
	         n, dest := exp_constexp(n, dest)
	      ENDIF
	      IF n.data = KW_DO
	         dest.data := KW_DO
	         dest++
	         n++
	         n, dest := substatement(n, dest)
	         g_currentloopdepth--
	      ENDIF
	   CASE KW_WHILE, KW_WHILEN
	      g_currentloopdepth++
	      dest.data := KW_WHILE
	      dest.num := IF t = KW_WHILE THEN 0 ELSE 1  -> N bool
	      dest++
	      n++
	      n, dest := expression(n, dest)
	      IF n.data = KW_DO
	         dest.data := KW_DO
	         dest++
	         n++
	         n, dest := substatement(n, dest)
	         g_currentloopdepth--
	      ENDIF
	   CASE KW_LOOP
	      g_currentloopdepth++
	      dest.data := KW_LOOP
	      dest++
	      n++
	      IF n.data = KW_DO
	         dest.data := KW_DO
	         dest++
	         n++
	         n, dest := substatement(n, dest)
	         g_currentloopdepth--
	      ENDIF
	   CASE KW_SELECT
	      dest.data := KW_SELECT
	      dest++
	      n++
	      n, dest := expression(n, dest)
	      IF n.data = KW_OF
	         dest.data := KW_OF
	         dest++
	         n++
	         n, dest := expression(n, dest)
	      ENDIF
	      g_currentselectdepth++ -> v49
	   CASE KW_REPEAT
	      g_currentloopdepth++
	      dest.data := n.data
	      dest++
	      n++
	   CASE KW_ENDWHILE, KW_ENDFOR, KW_ENDLOOP
	      g_currentloopdepth--
	      dest.data := n.data
	      dest++
	      n++
	   CASE KW_ENDIF -> v49
	      g_currentifdepth--
	      dest.data := n.data
	      dest++
	      n++
	   CASE KW_ENDSELECT  -> v49
	      g_currentselectdepth--
	      dest.data := n.data
	      dest++
	      n++
	   CASE KW_DEFAULT, KW_ELSE
	      dest.data := n.data
	      dest++
	      n++
	   CASE "." -> 2.2
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('label expected')
	      IF n[1].data <> ":" THEN reportErr('":" expected')
	      hln := n.info
	      loclab := getLocalLabel(hln.name)
	      IF loclab.hln.ident THEN reportErr('double declaration of local label', hln.name)
	      loclab.hln.ident := loclab
	      dest.data := IT_LABEL
	      dest.info := loclab.hln
	      dest++
	      n := n[2]
	      IF n.data THEN JUMP pel_uuh -> LONG/INT/CHAR/ASMinstruction may follow !
	   DEFAULT
	      reportErr('statement does not compute')
	   ENDSELECT

	   IF n.data = KW_BUT     -> 1.8.2
	      dest.data := KW_BUT
	      dest++
	      n++
	      JUMP pel_uuh
	   ELSEIF n.data
	      reportErr('unexpected characters following statement')
	   ENDIF

	IF dest-- = n
	   NOP    -> remove if only a newline
	ELSE
	   dest++
	   itmz_export := FALSE -> 1.8.0
	ENDIF

ENDPROC dest

-> 2.2
-> name is hashed
PROC getLocalLabel(name)
	DEF l:PTR TO loclab
	IF g_currentproc = NIL THEN reportErr('local label scope', name)
	l := g_currentproc.loclabs
	WHILE l
	   IF l.name = name THEN RETURN l
	   l := l.next
	ENDWHILE
	NEW l
	l.hln.name := name
	l.identID := IDENT_LABEL
	l.ltype := LTYPE_LAB
	l.name := name
	l.codelink := g_codelablist
	g_codelablist := l
	l.next := g_currentproc.loclabs
	g_currentproc.loclabs := l
ENDPROC l

-> 1.9.0
PROC static_key(n:PTR TO item, dest:PTR TO item, export)
	DEF codelab:PTR TO statlab, hln:PTR TO hln, d, list:PTR TO item
	DEF sl:PTR TO it_statlist

	dest.data := KW_STATIC
	dest++
	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   IF n.data <> IT_LABEL THEN reportErr('label for STATIC expected')
	   hln := n.info
	   IF n.num = 1 THEN reportErr('STATIC lexical', hln.name)
	   IF hln.ident THEN reportErr('double declaration of label', hln.name)
	   NEW codelab
	   codelab.identID := IDENT_LABEL
	   codelab.ltype := LTYPE_STATIC
	   codelab.name := hln.name
	   codelab.exported := export
	   hln.ident := codelab
	   codelab.codelink := g_codelablist
	   g_codelablist := codelab
	   codelab.next := g_proclist
	   g_proclist := codelab
	   dest.data := IT_LABEL
	   dest.info := hln
	   dest++
	   n++
	   IF n.data <> "=" THEN reportErr('"=" expected')
	   n++
	   IF n.data = IT_STRING
	      dest.data := IT_STRING
	      dest.info := n.info
	      dest++
	      codelab.deref.esize := 1
	      codelab.deref.size := 0
	      codelab.deref.object := NIL
	      codelab.deref.numes := 0
	      codelab.deref.flags := NIL
	      codelab.sizeof := EstrLen(n.info)+1 -> 2.2
	      g_incbinsize := g_incbinsize + EstrLen(n.info)
	      n++
	   ELSEIF n.data = "["
	      list := dest
	      n, dest := statlist(n, dest)
	      sl := list.info
	      codelab.deref.flags := sl.flags
	      codelab.deref.esize := sl.esize
	      codelab.deref.size := 0
	      codelab.deref.numes := 0
	      codelab.sizeof := sl.sizeof -> 2.2
	      codelab.deref.object := sl.object
	   ELSE
	      reportErr('string or list expected for STATIC')
	   ENDIF
	UNTIL n.data <> ","

ENDPROC n, dest

PROC statlist(n:PTR TO item, dest:PTR TO item)
	DEF start:PTR TO item, len=0, hln:PTR TO hln, object:PTR TO object, t, d, sizeof
	DEF sl:PTR TO it_statlist

	NEW sl

	dest.data := "["
	start := dest++
	n++

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF
	WHILE (t := n.data) <> "]"
	   SELECT t
	   CASE IT_STRING
	      dest.data := IT_STRING
	      dest.info := n.info
	      dest++
	      n++
	   CASE "["
	      n, dest := statlist(n, dest)
	   CASE IT_LABEL
	      dest.data := IT_LABEL
	      dest.info := n.info
	      dest++
	      n++
	   DEFAULT
	      n, t := getconstexp(n)
	      IF n = NIL THEN reportErr('value expected in STATIC list')
	      dest.data := IT_VALUE
	      dest.info := t
	      dest++
	   ENDSELECT
	   IF n.data <> ","
	      IF n.data <> "]" THEN reportErr('syntax error in STATIC list')
	   ELSE
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ENDIF
	   len++
	ENDWHILE
	n++ -> skip "]"
	sl.object := NIL
	sl.cplx := FALSE
	IF n.data = ":" -> typed ?
	   n++
	   t := n.data
	   SELECT t
	   CASE KW_CHAR
	      sl.flags := 0 ; sl.esize := 1
	   CASE KW_BYTE
	      sl.flags := MEMBF_SIGNED ; sl.esize := 1
	   CASE KW_WORD
	      sl.flags := 0 ; sl.esize := 2
	   CASE KW_INT
	      sl.flags := MEMBF_SIGNED ; sl.esize := 2
	   CASE KW_ULONG
	      sl.flags := 0 ; sl.esize := 4
	   CASE KW_LONG
	      sl.flags := MEMBF_SIGNED ; sl.esize := 4
	   CASE KW_PTR
	      sl.flags := MEMBF_SIGNED ; sl.esize := PTRSIZE
	   CASE KW_FLOAT
	      sl.flags := MEMBF_FLOAT ; sl.esize := 4
	   CASE KW_DOUBLE
	      sl.flags := MEMBF_FLOAT ; sl.esize := 8
	   CASE KW_WIDE
	      sl.flags := MEMBF_SIGNED ; sl.esize := 8
	   CASE KW_REAL
	      sl.flags := MEMBF_FLOAT ; sl.esize := REALSIZE
	   CASE IT_LABEL
	      hln := n.info
	      object := hln.ident2
	      IF object = NIL THEN reportErr('unknown object', hln.name)
	      IF object.nrofmembers = 0 THEN reportErr('STATIC object must have members', hln.name)
	      sl.flags := 0
	      sl.object := object
	      sl.esize := 255
	   DEFAULT        ; reportErr('unknown type for STATIC list')
	   ENDSELECT
	   n++
	ELSE
	   sl.cplx := TRUE
	   sl.flags := MEMBF_SIGNED
	   sl.esize := PTRSIZE
	ENDIF

	sl.len := len
	sl.sizeof := IF sl.object = NIL THEN Mul(sl.esize, len) ELSE (sl.object.sizeof *
	                                       (Div(len, sl.object.nrofmembers) +
	                                        IF Mod(len, sl.object.nrofmembers) THEN 1 ELSE 0))
	start.info := sl

	-> yeah we use incbinsize, what else
	g_incbinsize := g_incbinsize + sl.sizeof

ENDPROC n, dest

PROC multiassign(n:PTR TO item, dest:PTR TO item)
	DEF va:PTR TO var, hln:PTR TO hln
	hln := n.info
	va := hln.ident
	dest.data := IT_MASSIGN
	dest.info := va
	useVar(va,1)
	n++
	dest++
	n++
	IF n.data <> IT_LABEL THEN reportErr('variable expected')
	hln := n.info
	va := hln.ident
	IF va = NIL THEN reportErr('unknown identifier', hln.name)
	IF va.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	dest.data := IT_VARIABLE
	dest.info := va
	useVar(va,1)
	dest++
	n++
	IF n.data = ","
	   n++
	   IF n.data <> IT_LABEL THEN reportErr('variable expected')
	   hln := n.info
	   va := hln.ident
	   IF va = NIL THEN reportErr('unknown identifier', hln.name)
	   IF va.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	   dest.data := IT_VARIABLE
	   dest.info := va
	   useVar(va,1)
	   dest++
	   n++
	ENDIF
	IF n.data = ","
	   n++
	   IF n.data <> IT_LABEL THEN reportErr('variable expected')
	   hln := n.info
	   va := hln.ident
	   IF va = NIL THEN reportErr('unknown identifier', hln.name)
	   IF va.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	   dest.data := IT_VARIABLE
	   dest.info := va
	   useVar(va,1)
	   dest++
	   n++
	ENDIF
	IF n.data <> KW_ASSIGN THEN reportErr('":=" expected')
	dest.data := KW_ASSIGN
	dest++
	n++
	n, dest := expression(n, dest)
ENDPROC n, dest

PROC const_key(n:PTR TO item, export)
	DEF d, val, num, str[10]:STRING, cc:PTR TO const, hln:PTR TO hln
	DEF as, neg

	#ifdef DBG_PASS1
	DEBUGF('const_key($\h,\d)\n', n, export)
	#endif

	d := n.data
	val := IF d = KW_SET THEN 1 ELSE 0
	num := 1
	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   IF n.data = IT_VALUE -> doubledeclaration (itemizer replaced with value!)
	      StringF(str, '# \d', num)
	      reportErr('double declaration of constant', str)
	   ELSEIF n.data <> IT_LABEL
	      reportErr('label expected for CONST/SET/ENUM')
	   ENDIF
	   hln := n.info
	   IF hln.ident THEN reportErr('double declaration', hln.name)
	   IF n.num <> 1 THEN reportErr('lexical', hln.name)
	   NEW cc
	   hln.ident := cc
	   cc.name := hln.name
	   cc.identID := IDENT_CONST
	   n++
	   IF d = KW_CONST
	      IF n.data <> "=" THEN reportErr('"=" expected')
	      n++
	      n,val := getconstexp(n)
	      IF n = NIL THEN reportErr('constant expression expected')
	      cc.value := val
	   ELSEIF d = KW_SET
	      IF n.data = "="
	         n++
	         IF n.data <> IT_VALUE THEN reportErr('value expected')
	         val := n.info
	         n++
	      ENDIF
	      cc.value := val
	      val := Shl(val,1)
	   ELSEIF d = KW_ENUM
	      IF n.data = "="
	         n++
	         IF n.data = "-"
	            neg := TRUE
	            n++
	         ELSE
	            neg := FALSE
	         ENDIF
	         IF n.data <> IT_VALUE THEN reportErr('value expected')
	         val := IF neg THEN (0-n.info) ELSE n.info
	         n++
	      ENDIF
	      cc.value := val++
	   ENDIF
	   IF export
	      IF g_optmodule
	         cc.next := g_econstlist
	         g_econstlist := cc
	      ENDIF
	   ENDIF
	   num++
	UNTIL n.data <> ","
ENDPROC n



PROC library_key(n:PTR TO item)
	DEF a, e:PTR TO entry, reg:PTR TO reg, t, basehln, ofs=-30
	DEF abi, str[300]:STRING, hln:PTR TO hln

	IF g_optnostartup THEN reportErr('LIBRARY mode and OPT NOSTARTUP is bad idea') -> v55

	abi := 0 ->IF g_optpowerpc THEN 1 ELSE 0

	n++

	-> changed 1.8.0
	IF n.data = IT_LABEL
	   IF StrCmp(n.info::hln.name, 'M68K')
	      -> do nothing
	   ELSEIF StrCmp(n.info::hln.name, 'SYSV')
	      abi := 1
	   ELSE
	      reportErr('syntax', n.info::hln.name)
	   ENDIF
	   n++
	ENDIF

	IF g_libraryname THEN reportErr('LIBRARY scope')
	IF n.data <> IT_STRING THEN reportErr('string expected')
	g_libraryname := n.info ; n++
	-> 1.8.0
	IF EstrLen(g_exename) = 0 THEN StrCopy(g_exename, g_libraryname)
	IF EstrLen(g_modname) = 0
	   StrCopy(g_modname, g_exename, InStr(g_exename, '.'))
	   StrAdd(g_modname, '.m')
	ENDIF

	IF n.data <> "," THEN reportErr('"," expected') ; n++
	IF n.data <> IT_VALUE THEN reportErr('value expected')
	g_libraryversion := n.info ; n++
	IF n.data <> "," THEN reportErr('"," expected') ; n++
	IF n.data <> IT_VALUE THEN reportErr('value expected')
	g_libraryrevision := n.info ; n++
	IF n.data <> "," THEN reportErr('"," expected') ; n++
	IF n.data <> IT_STRING THEN reportErr('string expected')
	g_libraryidstr := n.info
	n++
	WHILE n.data = IT_LABEL -> 1.8.0
	   IF StrCmp(n.info.name, 'TAGS')
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('unknown label', n.info.name)
	      g_libraryrtagsname := n.info.name
	      n++
	   ELSE
	      reportErr('unknown LIBRARY option', n.info.name)
	   ENDIF
	ENDWHILE
	IF n.data <> KW_IS THEN reportErr('"IS" expected')
	basehln := getLabelHLN('librarybase')
	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   g_extracodesize := g_extracodesize + 50
	   IF n.data = IT_VALUE
	      IF n.info <> NIL THEN reportErr('illegal value for entry')
	      NEW e
	      e.label.ltype := LTYPE_LAB
	      e.label.codelink := g_codelablist
	      e.type := abi
	      g_codelablist := e.label
	      ofs := ofs - 6
	      g_entrytable[g_nrofentries++] := e -> put dummy entry here
	      n++
	   ELSEIF n.data = IT_LABEL
	      NEW e
	      e.identID := IDENT_LABEL -> LFUNC
	      e.prochln := n.info
	      e.name := e.prochln.name
	      ->e.basehln := basehln -> used by main
	      e.baseofs := ofs + IF abi=1 THEN 2 ELSE 0
	      StrCopy(str, 'LIB_')
	      StrAdd(str, e.name)
	      hln := getLabelHLN(str)
	      e.label.name:= hln.name
	      hln.ident := e.label -> for symbolhunk
	      e.label.ltype := LTYPE_LAB
	      e.label.identID := IDENT_LABEL
	      e.label.codelink := g_codelablist -> v49, for reloc
	      g_codelablist := e.label -> v49
	      ofs := ofs - 6
	      g_entrytable[g_nrofentries++] := e
	      n++
	      -> default regs for 68k
	      IF abi = 0
	      CopyMem([0,0,0,0,
	               0,0,0,1,
	               0,0,1,0,
	               0,0,1,1,
	               0,0,0,2,
	               0,0,0,3,
	               0,0,1,2,
	               0,0,1,3,
	               0,0,0,4,
	               0,0,0,5,
	               0,0,1,4,
	               0,0,1,5,
	               0,0,0,6,
	               0,0,0,7]:lfuncarg, e.regs, SIZEOF lfuncarg * 14)
	      ELSE
	         /*
	         CopyMem([0,0,0,3,
	               0,0,0,4,
	               0,0,0,5,
	               0,0,0,6,
	               0,0,0,7,
	               0,0,0,8,
	               0,0,0,9,
	               0,0,0,10,
	               0,0,1,8,
	               0,0,1,12,
	               0,0,1,16,
	               0,0,1,20,
	               0,0,1,24,
	               0,0,1,28]:lfuncarg, e.regs, SIZEOF lfuncarg * 14)
	         */
	         e.type := 1 -> ppc sysv
	         e.basernum := 12 -> v48
	      ENDIF
	      IF (n.data = "(")
	         IF abi <> 0 THEN reportErr('only allowed on 68k')
	         n++
	         a := 0
	         WHILE n.data = IT_REG
	            t := n.info::reg.type
	            e.regs[a].rtype := IF t = DRX THEN 0 ELSE 1
	            e.regs[a].rnum := n.info::reg.num
	            n++
	            IF n.data = ","
	               n++
	            ELSEIF n.data <> ")"
	               reportErr('LIBRARY syntax', e.name)
	            ENDIF
	            a++
	         ENDWHILE
	         IF n.data <> ")" THEN reportErr('")" expected')
	         n++
	      ENDIF
	      /*
	      IF n.data = "[" -> define basereg ?
	         n++
	         IF n.data = IT_REG
	            e.basernum := n.info::reg.num
	            n++
	         ELSE
	            reportErr('LIBRARY syntax', e.name)
	         ENDIF
	         IF n.data <> "[" THEN reportErr('LIBRARY syntax', e.name)
	         n++
	      ENDIF
	      */
	   ELSE
	      reportErr('IS syntax')
	   ENDIF

	UNTIL n.data <> ","
ENDPROC n

PROC opt_key(n:PTR TO item)
	DEF hln:PTR TO hln, t

	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   t := n.data
	   SELECT t
	   CASE KW_EXPORT
	      g_optexport := TRUE
	      n++
	   CASE KW_MODULE
	      g_optmodule := TRUE
	      n++
	   CASE KW_REG
	      n++
	      IF n.data <> "=" THEN reportErr('= expected')
	      n++
	      IF n.data = "-"
	         n++
	         t := -1
	      ELSE
	         t := 1
	      ENDIF
	      IF n.data <> IT_VALUE THEN reportErr('value expected')
	      g_numregalloc := n.info * t
	      n++
	   CASE IT_LABEL
	      hln := n.info
	      IF StrCmp(hln.name, 'PREPROCESS')
	         IF g_optpreprocess = FALSE
	            IF g_quiet = FALSE THEN WriteF('+Preprocessing..\n')
	            g_ppstring := String(MACROBUFSIZE)
	            IF g_ppstring = NIL THEN Raise("MEM")
	            g_argbodystr := String(MACROBUFSIZE)
	            IF g_argbodystr = NIL THEN Raise("MEM")
	            g_argprocstr := String(MACROBUFSIZE) -> 1.10.0
	            IF g_argprocstr = NIL THEN Raise("MEM")
	            hln := getLabelHLN('ECX_VERSION')
	            hln.ident := [IDENT_MACRO,hln.name,0,0,0,ECX_BRANCH_VERSION_STR]:macro-> v46
	            addDateDefine() -> 1.10.0
	            addTimeDefine() -> 1.10.0
	            addVersiondateDefine() -> 1.8.0
	         ENDIF
	         g_optpreprocess := TRUE
	         n++
	      ELSEIF StrCmp(hln.name, 'LARGE')
	      ->   g_optlarge := TRUE
	         n++
	      ELSEIF StrCmp(hln.name, 'STACK')
	         IF n[1].data <> "=" THEN reportErr('= expected')
	         IF n[2].data <> IT_VALUE THEN reportErr('value expected')
	         g_optstack := n[2].info
	         n := n[3]
	      ELSEIF StrCmp(hln.name, 'DIR')
	         IF n[1].data <> "=" THEN reportErr('= expected')
	         IF n[2].data <> IT_STRING THEN reportErr('string expected')
	         StrCopy(g_opt_moduledir, n[2].info)
	         n := n[3]
	      ELSEIF StrCmp(hln.name, 'OSVERSION')
	         IF n[1].data <> "=" THEN reportErr('= expected')
	         IF n[2].data <> IT_VALUE THEN reportErr('value expected')
	         g_optosversion := n[2].info
	         n := n[3]
	      ELSEIF StrCmp(hln.name, 'NOWARN')
	         g_nowarn := TRUE
	         n++
	      ELSEIF StrCmp(hln.name, 'NODEFMODS') -> v48
	         g_nodefmods := TRUE
	         n++
	      ELSEIF StrCmp(hln.name, 'NOSTARTUP')-> v48
	         g_optnostartup := TRUE
	         n++
	      ELSEIF StrCmp(hln.name, 'MORPHOS') OR StrCmp(hln.name, 'POWERPC') -> v50
	         IF (g_optosid <> OSID_NONE) AND (g_optosid <> OSID_MORPHOS)
	            addWarning('target OPTion overridden')
	         ELSE
	            g_optosid := OSID_MORPHOS
	            g_optpowerpc := CPU_PPC
	            StrCopy(g_outformatstr, 'ELF')
	         ENDIF
	         n++
	      ELSEIF StrCmp(hln.name, 'AMIGAOS') -> v55
	         IF (g_optosid <> OSID_NONE) AND (g_optosid <> OSID_AMIGAOS)
	            addWarning('target OPTion overridden')
	         ELSE
	            g_optosid := OSID_AMIGAOS
	            g_optpowerpc := CPU_M68
	            StrCopy(g_outformatstr, 'ADOS')
	         ENDIF
	         n++
	      ELSEIF StrCmp(hln.name, 'AMIGAOS4') -> v55
	         IF (g_optosid <> OSID_NONE) AND (g_optosid <> OSID_AMIGAOS4)
	            addWarning('target OPTion overridden')
	         ELSE
	            g_optosid := OSID_AMIGAOS4
	            g_optpowerpc := CPU_PPC
	            StrCopy(g_outformatstr, 'ELF')
	         ENDIF
	         n++
	      ELSEIF StrCmp(hln.name, 'FREG') -> v49
	         n++
	         IF n.data <> "=" THEN reportErr('= expected')
	         n++
	         IF n.data = "-"
	            n++
	            t := -1
	         ELSE
	            t := 1
	         ENDIF
	         IF n.data <> IT_VALUE THEN reportErr('value expected')
	         g_numfregalloc := n.info * t
	         n++
	      ELSEIF StrCmp(hln.name, 'EXENAME')
	         n++
	         IF n.data <> "=" THEN reportErr('= expected')
	         n++
	         IF n.data <> IT_STRING THEN reportErr('string expected')
	         -> 1.8.0
	         IF EstrLen(g_exename) = 0 THEN StrCopy(g_exename, n.info)
	         n++
	      ELSEIF StrCmp(hln.name, 'MODNAME') -> 1.5.6
	         n++
	         IF n.data <> "=" THEN reportErr('= expected')
	         n++
	         IF n.data <> IT_STRING THEN reportErr('string expected')
	         -> 1.8.0
	         IF EstrLen(g_modname) = 0
	            StrCopy(g_modname, n.info)
	            StrAdd(g_modname, '.m')
	         ENDIF
	         n++
	      ELSEIF StrCmp(hln.name, 'MINSTARTUP')  -> 1.5.4
	         n++
	         g_optminstartup := TRUE
	      ELSEIF StrCmp(hln.name, 'E64')  -> 1.7
	         n++
	         g_sizeofptr := 8
	      ELSEIF StrCmp(hln.name, 'INLINE')  -> 1.7
	         n++ -> do nothing
	      ELSEIF StrCmp(hln.name, 'CODE')  -> 2.1
	         -> do nothing
	         n++
	         IF n.data = IT_LABEL
	            n++ -> skip FAST
	         ENDIF
	      ELSEIF StrCmp(hln.name, 'FPEXP')  -> 1.7.1
	         n++
	         -> 2.0 turn on ROUNDNEAR for compatibility with creative
	         g_optroundnear := TRUE
	      ELSEIF StrCmp(hln.name, 'ROUNDNEAR')  -> 1.7.1
	         n++
	         g_optroundnear := TRUE
	      ELSEIF StrCmp(hln.name, 'NATURALALIGN')  -> 1.8.1
	         n++
	         g_naturalalign := TRUE
	      ELSEIF StrCmp(hln.name, '___NOTARGETMOD')  -> 1.10.0
	         n++
	         g_privoptnotargetmod := TRUE
	      ELSEIF StrCmp(hln.name, 'OUTFORMAT') -> 2.0
	         IF g_optmodule THEN reportErr('OUTFORMAT makes no sense for module')
	         n++
	         IF n.data <> "=" THEN reportErr('"=" expected')
	         n++
	         IF n.data <> IT_STRING THEN reportErr('string expected')
	         StrCopy(g_outformatstr, n.info)
	         UpperStr(g_outformatstr)
	      ELSEIF StrCmp(hln.name, 'ECXVERSION') -> 2.2
	         n++
	         IF n.data <> "=" THEN reportErr('"=" expected')
	         n++
	         IF n.data <> IT_VALUE THEN reportErr('version expected')
	         IF n.info > ECX_BRANCH_VERSION_NUM
	            reportErr('source requires later version of ECX')
	         ENDIF
	         n++
	      ELSE
	         reportErr('unknown OPT', hln.name)
	      ENDIF
	   CASE IT_VALUE -> 2.0, 881, 882, 020, etc
	      -> just ignore crap
	      n++
	   DEFAULT
	      reportErr('unknown OPT')
	   ENDSELECT
	UNTIL n.data <> ","

	IF g_optosid <> OSID_NONE THEN loadTargetModule() -> 2.0

ENDPROC n

PROC def_key(n:PTR TO item, export)
	DEF va:PTR TO var, gvc:PTR TO gvar, g2:PTR TO gvar, ohln:PTR TO hln, reg, exit
	DEF t, xhln:PTR TO hln, d

	IF g_currentproc
	   REPEAT
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	      IF n.data <> IT_LABEL THEN reportErr('name of variable expected')
	      NEW va
	      va.identID := IDENT_VARIABLE
	      va.vtype := VTYPE_LOC
	      n, ohln, reg := varmembdec(n, va.type, va)
	      va.trok := REUSEREGS
	      va.breg := FRAMEREG
	      IF va.type.esize = 255 -> object ?
	         va.type.object := ohln.ident2
	         IF va.type.object = NIL THEN reportErr('unknown identifier', ohln.name)
	         IF va.type.object.identID <> IDENT2_OBJECT THEN reportErr('object expected', ohln.name)
	      ENDIF

	      va.d := va

	      t := va.type.size
	      SELECT t
	      CASE 4
	         va.next := g_currentproc.locals32
	         g_currentproc.locals32 := va
	         va.o := VAR
	      CASE 0
	         IF va.defo THEN reportErr('init value for array is not allowed', va.hln.name) -> 1.10.0
	         va.next := g_currentproc.locals00
	         g_currentproc.locals00 := va
	         va.o := VAR  -> gets changed by .prochead()
	      CASE 8
	         va.next := g_currentproc.locals64
	         g_currentproc.locals64 := va
	         va.o := VAR64
	         IF va.type.flags AND MEMBF_FLOAT = FALSE
	            va.trok := FALSE
	         ENDIF
	      DEFAULT
	         reportIErr(' va.type.size')
	      ENDSELECT

	      IF reg
	         IF g_currentproc.handle THEN reportErr('cannot put in register', va.hln.name)
	         IF va.type.size = 8
	            IF va.type.flags AND MEMBF_FLOAT = FALSE THEN reportErr('cannot put in register', va.hln.name)
	            va.o := FREG
	         ELSE
	            va.o := DREG
	         ENDIF
	      ENDIF
	      /* add local to "hash" */
	      IF va.hln.ident
	         IF va.hln.ident.identID = IDENT_VARIABLE
	            IF va.hln.ident::var.vtype = VTYPE_LOC THEN reportErr('double declaration of local', va.hln.name)
	         ENDIF
	      ENDIF
	      va.oldident := va.hln.ident -> save previous (if any)
	      va.hln.ident := va -> set new
	      ->v50
	      va.varchain := g_currentproc.varchain
	      g_currentproc.varchain := va

	      IF g_varfill -> 2.3
	         IF va.type.size = SIZEOF LONG
	            IF va.defo = NIL
	               va.defo := DV
	               va.defd := g_varfill[]
	            ENDIF
	         ENDIF
	      ENDIF
	   UNTIL n.data <> ","
	ELSE -> global DEF
	   REPEAT
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	      IF n.data <> IT_LABEL THEN reportErr('name of variable expected')
	      NEW gvc
	      n, ohln, reg := varmembdec(n, gvc.type, gvc)

	      IF reg THEN reportErr('cannot put in register', gvc.hln.name)

	      t := gvc.type.size
	      SELECT 20 OF t
	      CASE 0
	         IF gvc.defo THEN reportErr('init value for array is not allowed', gvc.hln.name) -> 1.10.0
	         gvc.type.size := PTRSIZE -> !
	      CASE 1, 2, 16
	         reportErr('illegal size of global', gvc.hln.name)
	      CASE 8
	      CASE 4
	      ENDSELECT

	      ->IF g_optmodule
	      ->   IF gvc.defo THEN reportErr('init value not allowed in module for global', gvc.hln.name)
	      ->ENDIF

	      IF export
	         IF gvc.type.numes THEN reportErr('exported global cannot be array', gvc.hln.name)
	         gvc.export := 1
	         IF g_linkobjmode
	            IF n.data = KW_AS
	               n++
	               IF n.data <> IT_LABEL THEN reportErr('label expected after AS')
	               xhln := n.info
	               gvc.xname := xhln.name
	               n++
	            ENDIF
	         ENDIF
	      ENDIF

	      IF gvc.type.esize = 255 -> object ?
	         gvc.type.object := ohln.ident2
	         IF gvc.type.object = NIL THEN reportErr('unknown object', ohln.name)
	      ENDIF

	      -> already declared ?
	      g2 := gvc.hln.ident
	      IF g2
	         IF g2.identID <> IDENT_VARIABLE
	            reportErr('symbol collision', gvc.hln.name)
	         ELSEIF (g2.gtype = GTYPE_DEF) OR (g2.gtype = GTYPE_INTERNAL)
	            reportErr('double declaration of global', gvc.hln.name)
	         ELSE -> xref or base
	            -> we override previous global
	            g2.type.flags := gvc.type.flags
	            g2.type.esize := gvc.type.esize
	            g2.type.numes := gvc.type.numes
	            g2.type.object := gvc.type.object
	            g2.export := gvc.export
	            g2.gtype := GTYPE_DEF
	            g2.xname := gvc.xname
	            g2.cmplx := gvc.cmplx -> 1.8.0, was forgotten
	            g2.type.size := gvc.type.size -> 1.8.0, was forgotten
	            g2.defo := gvc.defo -> 2.2 fix
	            g2.defd := gvc.defd -> 2.2 fix
	            END gvc
	         ENDIF
	      ELSE
	         gvc.next := g_gvarlist
	         g_gvarlist := gvc
	         gvc.hln.ident := gvc
	         gvc.link := IF g_optmodule THEN 1 ELSE 0
	         gvc.identID := IDENT_VARIABLE
	         gvc.vtype := VTYPE_GLOB
	         gvc.o := IF gvc.type.size = 8 THEN VAR64 ELSE VAR
	         gvc.d := gvc
	         gvc.trok := REUSEREGS
	         gvc.gtype := GTYPE_DEF -> 1.7.1, was forgotten!
	      ENDIF

	   UNTIL n.data <> ","
	ENDIF
ENDPROC n

-> 1.9.0
PROC varfuncdef(n:PTR TO item, var:PTR TO var)
	DEF a=0, b=0, ptrarray[32]:ARRAY OF LONG, multiret:PTR TO multireturn, arg:PTR TO arg

	var.type.size := PTRSIZE

	-> we know next is "("

	   REPEAT
	      n++
	      IF n.data <> ")" -> 1.10.0 fix
	         ptrarray[a++] := NEW arg
	         key2type(n.data, arg.type)
	         n++
	      ENDIF
	      IF (n.data <> ")") AND (n.data <> ",") THEN reportErr('")" or "," expected')
	   UNTIL n.data = ")"
	   n++
	   g_codegen.abiArgs(ptrarray, a, NIL)
	   var.argsarray := List(a)
	   ListCopy(var.argsarray, ptrarray, a)
	   IF n.data = "("
	      NEW multiret
	      REPEAT
	         n++
	         IF b > 3 THEN reportErr('too many return values defined')
	         multiret.ros[b++] := key2rego(n.data)
	         n++
	         IF (n.data <> ")") AND (n.data <> ",") THEN reportErr('")" or "," expected')
	      UNTIL n.data = ")"
	      n++
	      WHILE b < 4 DO multiret.ros[b++] := DREG  -> 1.10.0 fix
	      g_codegen.abiReturns(multiret)
	      var.multiret := multiret
	   ENDIF

ENDPROC n, NIL, NIL

PROC raise_key(n:PTR TO item)
	DEF exit, raise:PTR TO raise, t, as, as2
	DEF lfunc:PTR TO lfunc, ifunc:PTR TO lif, proc:PTR TO proc, hln:PTR TO hln

	#ifdef DBG_PASS1
	DEBUGF('raise_key($\h)\n', n)
	#endif

	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   NEW raise
	   n,as2 := getconstexp(n)
	   IF n = NIL THEN reportErr('value expected')
	   raise.excval := as2
	   IF n.data <> KW_IF THEN reportErr('"IF" expected')
	   n++
	   IF n.data = IT_LFUNC
	      lfunc := n.info
	      IF lfunc.raise THEN reportErr('double RAISE', lfunc.name)
	      lfunc.raise := raise
	   ELSEIF n.data = IT_IFUNC
	      ifunc := n.info
	      IF ifunc.raise THEN reportErr('double RAISE', ifunc.name)
	      ifunc.raise := raise
	   ELSEIF n.data = IT_LABEL -> 2.2
	      hln := n.info
	      proc := hln.ident
	      IF proc = NIL THEN reportErr('unknown label', hln.name)
	      IF proc.ltype <> LTYPE_PROC THEN reportErr('cannot RAISE this', hln.name)
	      proc.raise := raise
	   ELSE
	      reportErr('cannot RAISE this')
	   ENDIF

	   n++
	   /* we expect "(" here */
	   IF n.data <> "(" THEN reportErr('"(" expected')
	   n++
	   IF n.data <> ")" THEN reportErr('")" expected')
	   n++
	   /* get condition */
	   t := n.data
	   SELECT t
	   CASE "="     ; raise.condition := reverseCondition(ISEQ)
	   CASE KW_ISNE ; raise.condition := reverseCondition(ISNE)
	   CASE KW_ISLE ; raise.condition := reverseCondition(ISLE)
	   CASE KW_ISGE ; raise.condition := reverseCondition(ISGE)
	   CASE "<"     ; raise.condition := reverseCondition(ISLT)
	   CASE ">"     ; raise.condition := reverseCondition(ISGT)
	   DEFAULT      ; reportErr('condition expected')
	   ENDSELECT
	   n++
	   /* get "compare-value" */
	   n,as2 := getconstexp(n)
	   IF n = NIL THEN reportErr('constant expression expected')
	   raise.trigval := as2
	UNTIL n.data <> ","
ENDPROC n


PROC proc_key(n:PTR TO item, dest:PTR TO item, export)
	DEF va:PTR TO var, proc:PTR TO proc, str[300]:STRING, ohln:PTR TO hln, reg
	DEF t, exit, prochln:PTR TO hln, ofobj:PTR TO object
	DEF entry=NIL:PTR TO entry
	DEF dtemp:PTR TO item, a

	IF g_currentproc OR g_currentobject THEN reportErr('scope', 'PROC')
	NEW proc
	g_currentproc := proc
	proc.exported := IF export THEN 1 ELSE 0

	proc.cpu := g_optpowerpc

	n++
	IF n.data <> IT_LABEL THEN reportErr('label expected')
	prochln := n.info
	proc.name := prochln.name

	dest.data := IT_PROC
	dest.info := proc
	dest++

	#ifdef DBG_PASS1
	DEBUGF('proc_key($\h,$\h,\d) : \q\s\q\n', n,dest,export,proc.name)
	#endif


	n++

	IF n.data <> "(" THEN reportErr('"(" expected')
	n++
	WHILE n.data <> ")"
	   IF n.data = NIL THEN reportErr('unexpected end of source')
	   IF n.data <> IT_LABEL THEN reportErr('argument name expected')
	   NEW va
	   va.identID := IDENT_VARIABLE
	   va.vtype := VTYPE_LOC
	   va.trok := REUSEREGS
	   va.breg := FRAMEREG
	   g_temparray[proc.nrofargs] := va
	   proc.nrofargs := proc.nrofargs + 1
	   n, ohln, reg := varmembdec(n, va.type, va)

	   IF va.defo
	      proc.nrofdefaults := proc.nrofdefaults + 1
	   ELSEIF proc.nrofdefaults -> (x=1,y,z=5) : error !
	      reportErr('syntax')
	   ENDIF
	   IF va.type.esize = 255 -> OPTR
	      va.type.object := ohln -> needs backpatch
	   ENDIF

	   va.d := va
	   IF reg
	      IF proc.handle THEN reportErr('cannot put in register', va.hln.name)
	      IF (va.type.size = 8) AND (va.type.flags AND MEMBF_FLOAT = FALSE)
	          reportErr('cannot put in register', va.hln.name)
	      ENDIF
	   ENDIF

	   IF va.type.size = 4
	      va.next := proc.args32
	      proc.args32 := va
	      va.o := IF reg THEN DREG ELSE VAR
	   ELSEIF va.type.size = 8
	      va.next := proc.args64
	      proc.args64 := va
	      IF va.type.flags AND MEMBF_FLOAT
	         va.o := IF reg THEN FREG ELSE VAR64
	      ELSE
	         va.o := VAR64
	         va.trok := FALSE -> dont mirror in float reg!
	      ENDIF
	   ELSE
	      reportErr('illegal size of argument', va.hln.name)
	   ENDIF

	   -> v50 (1.5.1)
	   va.varchain := proc.varchain
	   proc.varchain := va

	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSEIF n.data=")"
	      ->
	   ELSE
	      reportErr('argument declaration syntax')
	   ENDIF

	ENDWHILE
	n++ -> skip )


	-> defaults
	proc.mret.ros[0] := DREG
	proc.mret.ros[1] := DREG
	proc.mret.ros[2] := DREG
	proc.mret.ros[3] := DREG

	IF n.data = "(" -> v45, simplified v50
	   a := 0
	   REPEAT
	      n++
	      t := n.data
	      n++
	      IF a > 3 THEN reportErr('too many returntypes for procedure', proc.name)
	      proc.mret.ros[a] := key2rego(t)
	      a++
	   UNTIL n.data <> ","
	   IF n.data <> ")" THEN reportErr('"(" expected')
	   n++
	ENDIF

	/* get OF objectname */
	IF n.data = KW_OF
	   IF StrCmp(proc.name, 'end')
	      IF proc.nrofargs THEN reportErr('no arguments allowed for .end() method')
	   ENDIF
	   n++
	   IF n.data = KW_CLASS  -> v47
	      n++
	      proc.flags := proc.flags OR PROCF_CLMETH
	   ENDIF
	   IF n.data <> IT_LABEL THEN reportErr('label expected')
	   IF n.num = 1 THEN reportErr('lexical', proc.name) -> no upper
	   proc.object := n.info.ident2
	   IF proc.object = NIL THEN  reportErr('unknown identifier', n.info.name)
	   IF proc.object.identID <> IDENT2_OBJECT THEN reportErr('object expected', n.info.name)
	    -> we may only add methods to objects in this source.(V47)
	   IF proc.object.startline = NIL THEN reportErr('illegal object for method', proc.name)
	   n++ -> skip OF-label
	   proc.identID := IDENT_NONE
	   NEW va
	   proc.codelink := proc.object.addedmethods -> v44
	   proc.object.addedmethods := proc -> v44
	   proc.selfvar := va -> v47
	   va.o := VAR
	   va.d := va
	   va.breg := FRAMEREG
	   va.hln := getLabelHLN('self')
	   va.oldident := va.hln.ident -> v46
	   va.hln.ident := va
	   va.type.size := 4
	   va.type.esize := 255
	   va.type.numes := 0
	   va.type.object := proc.object
	   va.identID := IDENT_VARIABLE
	   va.vtype := VTYPE_LOC
	   va.next := proc.locals32
	   proc.locals32 := va
	   va.varchain := proc.varchain -> 1.5.1
	   proc.varchain := va
	   va.trok := REUSEREGS
	   va.hln.ident2 := proc.object -> v46
	   proc.exported := TRUE -> v45
	   proc.selfreg := SELFREG -> v45
	   proc.next := g_methodlist -> 1.5.1
	   g_methodlist := proc -> v1.5.1
	ELSE
	   proc.identID := IDENT_LABEL
	   proc.ltype := LTYPE_PROC
	   IF StrCmp(proc.name, 'main') THEN proc.referenced := TRUE
	   IF prochln.ident
	      reportErr('double declaration of procedure', prochln.name)
	   ENDIF
	   prochln.ident := proc   -> v46
	   proc.codelink := g_codelablist -> v44
	   g_codelablist := proc -> v44
	   proc.next := g_proclist
	   g_proclist := proc
	ENDIF

	-> 2.1 make available to symbolhunk
	proc.dblabnext := g_dblablist
	g_dblablist := proc

	-> moved down 1.8.0
	/* make permanernt copy of params */
	NEW proc.argarray[proc.nrofargs+1]
	CopyMemQuick(g_temparray, proc.argarray, Mul(proc.nrofargs,4))
	g_codegen.abiArgs(proc.argarray, proc.nrofargs, proc.flags) -> v48

	-> also moved down 1.8.0
	g_codegen.abiReturns(proc.mret)

	-> v46 get objectptrs (moved here to reach "self")
	-> 1.8.0: now traverses all args
	FOR a := 0 TO proc.nrofargs-1
	   va := proc.argarray[a]
	   IF va.type.size = PTRSIZE
	      IF va.type.esize = 255
	         t := va.type.object::hln.ident2
	         IF t = NIL THEN reportErr('unknown identifier', va.type.object::hln.name)
	         va.type.object := t
	      ENDIF
	   ENDIF
	   IF va.hln.ident
	      IF va.hln.ident.identID = IDENT_VARIABLE
	         IF va.hln.ident::var.vtype = VTYPE_LOC THEN reportErr('double declaration of argument', va.hln.name)
	      ENDIF
	   ENDIF
	   va.oldident := va.hln.ident -> save previous (if any)
	   va.hln.ident := va -> set new
	ENDFOR

	IF n.data = KW_HANDLE
	   n++
	   proc.handle := TRUE -> v3
	ELSEIF n.data = KW_IS
	   dest.data := KW_IS
	   dtemp := dest
	   dest++
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   #ifdef DBG_PASS1
	   DEBUGF('proc_key IS\n')
	   #endif
	   n, dest, t := explist(n, dest)
	   dtemp.num := t -> v50 forgotten
	   -> give back old idents..!
	   restoreLocalIdents(proc)
	   g_currentproc := NIL
	ENDIF

	g_extracodesize := g_extracodesize + 400

ENDPROC n, dest

PROC key2rego(k) -> 1.8.2
	SELECT 256 OF k
	CASE KW_REAL, KW_DOUBLE  ; RETURN FREG
	CASE KW_PTR, KW_LONG     ; RETURN DREG
	CASE KW_WIDE             ; RETURN X2R -> 1.10.0
	ENDSELECT
ENDPROC reportErr('illegal value definition')

-> used by varfunc call with parameter values defined
PROC key2type(k, type:PTR TO member) -> 1.9.0
	SELECT 256 OF k
	CASE KW_REAL             ; type.size := REALSIZE ; type.flags := MEMBF_FLOAT
	CASE KW_DOUBLE           ; type.size := 8 ; type.flags := MEMBF_FLOAT
	CASE KW_PTR              ; type.size := PTRSIZE  ; type.flags := MEMBF_SIGNED
	CASE KW_LONG             ; type.size := 4  ; type.flags := MEMBF_SIGNED
	CASE KW_WIDE             ; type.size := 8  ; type.flags := MEMBF_SIGNED
	DEFAULT
	   reportErr('illegal value definition')
	ENDSELECT
ENDPROC NIL

PROC restoreLocalIdents(proc:PTR TO proc)
	DEF exit, va:PTR TO var, t
	va := proc.varchain
	WHILE va
	   va.hln.ident := va.oldident
	   va := va.varchain
	ENDWHILE
	IF proc.selfvar -> self used ?
	   va := proc.selfvar
	   va.hln.ident2 := NIL -> OBJECT self
	ENDIF
ENDPROC

-> 1.10.0: added OR, >>, ~
PROC getconstexp(buf)
	DEF n:PTR TO item
	DEF val, op
	DEF mode, val2
	DEF t

	    #ifdef DBG_PASS1
	    DEBUGF('getconstexp\n')
	    #endif

	n := buf

	IF n.data = "!"
	   mode := MODE_FLOAT
	   n++
	ELSE
	   mode := MODE_DEFAULT
	ENDIF

	n, val:= constVal(n, mode)
	IF n = NIL THEN RETURN NIL


	LOOP

	IF n.data = "!"
	   IF mode = MODE_DEFAULT
	      val := val!
	      mode := MODE_FLOAT
	   ELSE
	      val := !val!
	      mode := MODE_DEFAULT
	   ENDIF
	   n++
	ENDIF

	SELECT 256 OF n.data
	CASE "+"
	   n++ ; IF mode = MODE_FLOAT THEN op := `val := !val + val2 ELSE op := `val := val + val2
	CASE "-"
	   n++ ; IF mode = MODE_FLOAT THEN op := `val := !val - val2 ELSE op := `val := val - val2
	CASE "*"
	   n++ ; IF mode = MODE_FLOAT THEN op := `val := !val * val2 ELSE op := `val := Mul(val,val2)
	CASE "/"
	   n++ ; IF mode = MODE_FLOAT THEN op := `val := !val / val2 ELSE op := `val := Div(val,val2)
	CASE KW_OR  ; n++ ; op := `val := val OR val2
	CASE KW_AND ; n++ ; op := `val := val AND val2
	CASE KW_SHL ; n++ ; op := `val := Shl(val, val2)
	CASE KW_SHR ; n++ ; op := `val := shr(val, val2)
	CASE KW_ASR ; n++ ; op := `val := Shr(val, val2)
	CASE KW_XOR ; n++ ; op := `val := Eor(val, val2)
	DEFAULT
	   RETURN n, val, mode
	ENDSELECT

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	n, val2 := constVal(n, mode)
	IF n = NIL THEN reportErr('constant value expected')

	Eval(op)

	ENDLOOP

ENDPROC

PROC constVal(n:PTR TO item, mode, exp=FALSE)
	DEF unary, val, t:PTR TO item

	#ifdef DBG_PASS1
	DEBUGF('constVal($\h, \d, \d)\n', n, mode, exp)
	#endif

	IF n.data = "-"
	   unary := "-"
	   n++
	ELSEIF n.data = "~"
	   unary := "~"
	   n++
	ELSEIF n.data = KW_ABS
	   unary := KW_ABS
	   n++
	ELSE
	   unary := FALSE
	ENDIF

	IF n.data = IT_VALUE
	   IF n.num THEN mode := MODE_FLOAT -> yes,keep! (needed for stuff like -1.0)
	   val := n.info
	ELSEIF (n.data = "(")
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   IF exp = FALSE
	      n, val := getconstexp(n)
	      IF n = NIL THEN reportErr('constant expression expected')
	      IF n.data <> ")" THEN reportErr('")" expected')
	   ELSE -> 2.3
	      IF n.data <> IT_VALUE THEN RETURN NIL
	      val := n.info
	      n++
	      IF n.data <> ")" THEN RETURN NIL
	   ENDIF
	ELSE
	   RETURN NIL
	ENDIF

	IF unary = "-"
	   IF mode = MODE_FLOAT THEN val := !-val ELSE val := -val
	ELSEIF unary = "~"
	   val := Not(val)
	ELSEIF unary = KW_ABS  -> 2.2
	   IF mode = MODE_FLOAT THEN val := Fabs(val) ELSE val := Abs(val)
	ENDIF
	n++

ENDPROC n, val


-> 1.7, slapped vardec,membdec together
PROC varmembdec(n:PTR TO item, type:PTR TO member, var:PTR TO var)
	DEF t, kw, kw2, ohln=NIL:PTR TO hln, hln:PTR TO hln, reg=FALSE

	    #ifdef DBG_PASS1
	    DEBUGF('varmembdec\n')
	    #endif

	hln := n.info
	IF n.num = 1 THEN reportErr('variable/member declaration lexical', hln.name) -> no uppercase

	IF var THEN var.hln := hln ELSE type.name := hln.name

	n++

	type.esize := 1
	type.numes := 0
	type.size := 4
	type.object := NIL
	type.flags := NIL

	IF n.data = "="
	   n++
	   n, t := getconstexp(n)
	   IF n=NIL THEN reportErr('constant expression expected')
	   IF var = NIL THEN reportErr('init value not allowed for member', hln.name)
	   var.defo := DV
	   var.defd := t
	ENDIF

	IF n.data = "("  -> 1.10.0

	   IF var = NIL THEN reportErr('member cannot be function', hln.name)
	   RETURN varfuncdef(n, var)

	ELSEIF n.data = ":"
	   n++

	   /* REG keyword used ? */
	   IF n.data = KW_REG
	      IF var = NIL THEN reportErr('cannot put member in register', hln.name)
	      n++    -> skip it
	      reg := TRUE
	   ENDIF

	   kw := n.data

	   SELECT kw
	   CASE KW_PTR
	      type.size := PTRSIZE
	      n++
	      IF n.data <> KW_TO THEN reportErr('"TO" expected')
	      n++
	      kw2 := n.data
	      SELECT kw2
	      CASE KW_LONG   ; type.esize := 4 ; type.flags := MEMBF_SIGNED ; n++
	      ->CASE KW_ULONG  ; type.esize := 4 ; n++ -> 1.6.1
	      CASE KW_WIDE   ; type.esize := 8 ; type.flags := MEMBF_SIGNED ; n++
	      CASE KW_UWIDE  ; type.esize := 8 ; n++ -> 1.6.1
	      CASE KW_PTR    ; type.esize := PTRSIZE ; type.flags := MEMBF_SIGNED ; n++
	      CASE KW_INT    ; type.esize := 2 ; type.flags := MEMBF_SIGNED ; n++
	      CASE KW_CHAR   ; type.esize := 1 ; n++
	      CASE KW_WORD   ; type.esize := 2 ; n++
	      CASE KW_BYTE   ; type.esize := 1 ; type.flags := MEMBF_SIGNED ; n++
	      CASE KW_DOUBLE ; type.esize := 8 ; type.flags := MEMBF_FLOAT ; n++
	      CASE KW_REAL   ; type.esize := REALSIZE ; type.flags := MEMBF_FLOAT ; n++
	      CASE KW_FLOAT  ; type.esize := 4 ; type.flags := MEMBF_FLOAT ; n++
	      CASE KW_VECTOR ; type.esize := 16 ; type.flags := MEMBF_VECTOR ; n++
	      CASE IT_LABEL
	         ohln := n.info
	         IF n.num = 1 THEN reportErr('variable/member declaration lexical', ohln.name)  -> no upper
	         type.esize := 255
	         n++
	      DEFAULT
	         reportErr('variable/member declaration syntax', hln.name)
	      ENDSELECT
	   CASE KW_CHAR   ; type.esize := 0 ; type.size := 1 ; n++
	   CASE KW_WORD   ; type.esize := 0 ; type.size := 2 ; n++
	   CASE KW_BYTE   ; type.esize := 0 ; type.size := 1 ; type.flags := MEMBF_SIGNED ; n++
	   CASE KW_INT    ; type.esize := 0 ; type.size := 2 ; type.flags := MEMBF_SIGNED ; n++
	   CASE KW_LONG   ; type.esize := IF var THEN 1 ELSE 0 ; type.flags := MEMBF_SIGNED ; n++
	   ->CASE KW_ULONG  ; type.esize := 1 ; n++     -> 1.6.1
	   CASE KW_WIDE   ; type.esize := IF var THEN 1 ELSE 0 ; type.size := 8 ; type.flags := MEMBF_SIGNED ; n++
	   ->CASE KW_UWIDE  ; type.esize := IF var THEN 1 ELSE 0 ; type.size := 8 ; n++     -> 1.6.1
	   CASE KW_DOUBLE ; type.size := 8 ; type.esize := 0 ; n++ ; type.flags := MEMBF_FLOAT
	   CASE KW_REAL   ; type.size := REALSIZE ; type.esize := 0 ; n++ ; type.flags := MEMBF_FLOAT
	   CASE KW_FLOAT  ; type.esize := 0 ; n++ ; type.flags := MEMBF_FLOAT
	   CASE KW_VECTOR
	      n++
	      type.size := 16 ; type.esize := 0
	      IF n.data = KW_OF -> v49
	         n++
	         kw2 := n.data
	         SELECT kw2
	         CASE KW_LONG  ; type.esize := 4 ; type.numes := 4
	                       ; type.flags := MEMBF_SIGNED
	         ->CASE KW_ULONG ; type.esize := 4 ; type.numes := 4 -> 1.6.1
	         CASE KW_FLOAT ; type.esize := 4 ; type.numes := 4
	                       ; type.flags := MEMBF_FLOAT
	         CASE KW_INT   ; type.esize := 2 ; type.numes := 8
	                       ; type.flags := MEMBF_SIGNED
	         CASE KW_CHAR  ; type.esize := 1 ; type.numes := 16
	         CASE KW_WORD  ; type.esize := 2 ; type.numes := 8
	         CASE KW_BYTE  ; type.esize := 1 ; type.numes := 16
	                       ; type.flags := MEMBF_SIGNED
	         DEFAULT       ; reportErr('variable/member declaration syntax', hln.name)
	         ENDSELECT
	         n++
	      ENDIF
	      type.flags := type.flags OR MEMBF_VECTOR
	   CASE IT_LABEL
	      ohln := n.info
	      IF n.num = 1 THEN reportErr('variable/member declaration lexical', ohln.name) -> no upper
	      type.size := 0
	      type.esize := 255
	      type.numes := 1
	      n++
	   DEFAULT
	      IF (reg = 0) OR (var = NIL) THEN reportErr('variable/member declaration syntax', hln.name)
	   ENDSELECT
	/* its an array/list/string */
	ELSEIF n.data = "["
	   type.size := 0
	   n++
	   n,t := getconstexp(n)
	   IF n=NIL THEN RETURN reportErr('constant expression expected')
	   IF t < 0 THEN reportErr('incorrect size of array', hln.name)
	   IF (t=0) AND (var<>NIL) THEN reportErr('incorrect size of array', hln.name) -> 1.7.1 again
	   IF t > 32767 THEN reportErr('too large array', hln.name)
	   type.numes := t
	   IF n.data <> "]" THEN reportErr('"]" expected')
	   n++
	   IF n.data <> ":" THEN reportErr('":" expected')
	   n++
	   /* REG keyword used ? */
	   IF n.data = KW_REG
	      IF var = NIL THEN reportErr('cannot put member in register', hln.name)
	      n++    -> skip it
	      reg := 1
	   ENDIF
	   kw := n.data
	   SELECT kw
	   CASE KW_ARRAY
	      n++
	      IF n.data = KW_OF
	         n++
	         kw2 := n.data
	         SELECT kw2
	         CASE KW_LONG   ; type.esize := 4 ; n++
	                        ; type.flags := MEMBF_SIGNED
	         ->CASE KW_ULONG  ; type.esize := 4 ; n++ -> 1.6.1
	         CASE KW_WIDE   ; type.esize := 8 ; n++
	                        ; type.flags := MEMBF_SIGNED
	         ->CASE KW_UWIDE  ; type.esize := 8 ; n++ -> 1.6.1
	         CASE KW_PTR    ; type.esize := PTRSIZE ; n++ -> v49, same as LONG
	                        ; type.flags := MEMBF_SIGNED
	         CASE KW_INT    ; type.esize := 2 ; n++
	                        ; type.flags := MEMBF_SIGNED
	         CASE KW_CHAR   ; type.esize := 1 ; n++
	         CASE KW_WORD   ; type.esize := 2 ; n++  -> v49
	         CASE KW_BYTE   ; type.esize := 1 ; n++   -> v49
	                        ; type.flags := MEMBF_SIGNED
	         CASE KW_DOUBLE ; type.esize := 8 ; n++
	                        ; type.flags := MEMBF_FLOAT
	         CASE KW_REAL   ; type.esize := REALSIZE ; n++
	                        ; type.flags := MEMBF_FLOAT
	         CASE KW_FLOAT  ; type.esize := 4 ; n++
	                        ; type.flags := MEMBF_FLOAT
	         CASE KW_VECTOR ; type.esize := 16 ; type.flags := MEMBF_VECTOR ; n++
	         CASE IT_LABEL
	            ohln := n.info
	            IF n.num = 1 THEN reportErr('variable/member declaration lexical', ohln.name) -> no upper
	            type.esize := 255
	            n++
	         DEFAULT
	            reportErr('variable/member declaration syntax', hln.name)
	         ENDSELECT
	      ELSE
	         type.esize := 1
	      ENDIF
	   CASE KW_STRING
	      type.esize := 1
	      IF var = NIL THEN reportErr('illegal type for member', hln.name)
	      var.cmplx := TRUE ; n++
	   CASE KW_LIST
	      type.esize := 4
	      IF var = NIL THEN reportErr('illegal type for member',hln.name)
	      var.cmplx := TRUE ; n++
	   DEFAULT
	      reportErr('variable/member declaration syntax', hln.name)
	   ENDSELECT

	ELSE    -> 1.9.0

	   type.size := PTRSIZE
	   type.esize := IF var THEN 1 ELSE 0
	   type.flags := IF var THEN NIL ELSE MEMBF_SIGNED -> 1.10.0 fix
	   type.numes := 0
	   type.object := NIL

	ENDIF

	IF var
	   IF (type.size=1) OR (type.size=2) THEN reportErr('illegal size of variable', hln.name)
	   IF type.size = 0
	      IF type.numes = 0 THEN reportErr('illegal size of array', hln.name)
	   ENDIF
	ENDIF

ENDPROC n, ohln, reg


/***********************************************************/



-> used by some statements ("ENDPROC x,y,z" "RETURN x,y,z",...)
PROC explist(n:PTR TO item, dest:PTR TO item)
  DEF r=0
	    #ifdef DBG_PASS1
	    DEBUGF('explist($\h, $\h)\n', n, dest)
	    #endif
	IF n.data = NIL THEN RETURN n, dest, NIL
	WHILE n.data
	   n, dest := expression(n, dest)
	   r++
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest, r
	   ENDIF
	ENDWHILE
ENDPROC

PROC charlist(n:PTR TO item, dest:PTR TO item)

	    #ifdef DBG_PASS1
	    DEBUGF('charlist($\h, $\h)\n', n, dest)
	    #endif

->   LOOP
	WHILE TRUE
	   IF n.data = IT_VALUE
	      dest.data := IT_VALUE
	      dest.info := n.info
	      dest.num := n.num
	      dest++
	      n++
	   ELSEIF n.data = IT_STRING
	      dest.data := n.data
	      dest.info := n.info
	      dest++
	      n++
	   ELSE
	      reportErr('inline syntax')
	   ENDIF
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest
	   ENDIF
->   ENDLOOP
	ENDWHILE

ENDPROC


PROC intlist(n:PTR TO item, dest:PTR TO item)
	DEF neg=FALSE

	    #ifdef DBG_PASS1
	    DEBUGF('intlist($\h, $\h)\n', n, dest)
	    #endif

->   LOOP
	WHILE TRUE
	   IF n.data = "-"
	      neg := TRUE
	      n++
	   ENDIF
	   IF n.data = IT_VALUE
	      dest.data := IT_VALUE
	      dest.info := IF neg THEN -n.info ELSE n.info
	      dest.num := NIL
	      dest++
	      n++
	   ELSE
	      reportErr('inline syntax')
	   ENDIF
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest
	   ENDIF
	   neg := FALSE
->   ENDLOOP
	ENDWHILE

ENDPROC

-> v50
PROC doublelist(n:PTR TO item, dest:PTR TO item)
	    DEF d, t
	    #ifdef DBG_PASS1
	    DEBUGF('doublelist($\h, $\h)\n', n, dest)
	    #endif

->   LOOP
	WHILE TRUE
	   d := n.data
	   SELECT 256 OF d
	   CASE IT_VALUE, "-", "!", "("
	      n, d, t := getconstexp(n)
	      dest.data := IT_VALUE
	      dest.info := d
	      dest.num := t
	      dest++
	   DEFAULT
	      reportErr('inline syntax')
	   ENDSELECT
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest
	   ENDIF
->   ENDLOOP
	ENDWHILE
ENDPROC

-> v56
PROC widelist(n:PTR TO item, dest:PTR TO item)
	    DEF d, t
	    #ifdef DBG_PASS1
	    DEBUGF('widelist($\h, $\h)\n', n, dest)
	    #endif

	LOOP
	   d := n.data
	   SELECT 256 OF d
	   CASE IT_VALUE, "-", "!", "("
	      n, d, t := getconstexp(n)
	      dest.data := IT_VALUE
	      dest.info := d
	      dest.num := t
	      dest++
	   DEFAULT
	      reportErr('inline syntax')
	   ENDSELECT
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest
	   ENDIF
	ENDLOOP
ENDPROC

-> v45: 'string' support
PROC longlist(n:PTR TO item, dest:PTR TO item)
	    DEF d, t
	    #ifdef DBG_PASS1
	    DEBUGF('longlist($\h, $\h)\n', n, dest)
	    #endif

	LOOP
	   d := n.data
	   SELECT 256 OF d
	   CASE IT_VALUE, "-", "!", "("
	      n, d, t := getconstexp(n)
	      dest.data := IT_VALUE
	      dest.info := d
	      dest.num := t
	      dest++
	   CASE IT_LABEL
	      dest.data := n.data
	      dest.info := n.info
	      dest++
	      n++
	   CASE IT_STRING
	      dest.data := IT_STRING
	      dest.info := n.info
	      n++
	      dest++
	   DEFAULT
	      reportErr('inline syntax')
	   ENDSELECT
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSE
	      RETURN n, dest
	   ENDIF
	ENDLOOP
ENDPROC

PROC varexp(n:PTR TO item, r:PTR TO item,postkey,isexp)
	DEF destbuf[256]:ARRAY OF item, dest:REG PTR TO item
	DEF d, rdest, hln:PTR TO hln, size, len, var:PTR TO var
	DEF object:PTR TO object, incdec=FALSE
	DEF member:PTR TO member, method=FALSE
	DEF it_varexp:PTR TO it_varexp


	    #ifdef DBG_PASS1
	    DEBUGF('varexp($\h, $\h)\n', n, r)
	    #endif

	it_varexp := destbuf
	dest := it_varexp + SIZEOF it_varexp

	it_varexp.postkey := postkey
	it_varexp.assignmod := NIL

	IF n.data <> IT_LABEL THEN reportErr('variable expected')
	hln := n.info
	var := hln.ident
	IF var = NIL THEN reportErr('unknown identifier', hln.name)
	IF var.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	it_varexp.var := var
	n++


	WHILE (d := n.data)
	  SELECT 256 OF d
	  CASE "["
	     dest.data := "["
	     dest++ ; n++
	     IF n.data <> "]"
	        n, dest := expression(n, dest)
	     ENDIF
	     IF n.data <> "]" THEN reportErr('"]" expected')
	     dest.data := "]"
	     dest++ ; n++
	  CASE KW_PLUSPLUS, KW_MINMIN
	     dest.data := n.data
	     dest++ ; n++
	     incdec := TRUE -> v49
	  CASE "."
	     dest.data := "."
	     dest++
	     n++
	     IF n[1].data <> "("
	        IF n.data <> IT_LABEL THEN reportErr('member expected')
	        dest.data := IT_LABEL
	        dest.info := n.info
	        dest++
	        n++
	     ELSE
	        n, dest := function(n,dest)
	        method := TRUE
	        JUMP varexp_end ->v50
	     ENDIF
	  CASE KW_PTYPE -> changed v50
	     n++
	     IF n.data <> IT_LABEL THEN reportErr('object expected')
	     hln := n.info
	     object := hln.ident2
	     IF object = NIL THEN reportErr('unknown object', hln.name)
	     IF object.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	     n++
	     IF n.data <> "." THEN reportErr('"." expected') -> v50
	     dest.data := "."
	     dest++
	     n++
	     IF n.data <> IT_LABEL THEN reportErr('member expected')
	     member := findMember(object, n.info.name)
	     IF member = NIL THEN reportErr('unknown member', n.info.name)
	     dest.data := IT_MEMBER
	     dest.info := member
	     dest++
	     n++
	  CASE KW_ASSIGN
	     n++
	     IF n.data = 10 -> 1.10.0
	        setLinenum(n.info)
	        n++
	     ENDIF
	     it_varexp.assignmod := KW_ASSIGN
	     IF postkey THEN reportErr('illegal assignment for variable', var.hln.name)
	     it_varexp.index := dest - it_varexp - SIZEOF it_varexp / SIZEOF item -> index of assign exp
	     n, dest := expression(n, dest)
	     JUMP varexp_end
	  CASE KW_MODIFY
	     it_varexp.assignmod := n.info
	     n++
	     IF n.data = 10 -> 1.10.0
	        setLinenum(n.info)
	        n++
	     ENDIF
	     IF postkey THEN reportErr('illegal modification for variable', var.hln.name)
	     it_varexp.index := dest - it_varexp - SIZEOF it_varexp / SIZEOF item -> index of assign exp
	     n, dest := expression(n, dest)
	     JUMP varexp_end
	  DEFAULT
	     JUMP varexp_end
	  ENDSELECT
	ENDWHILE


varexp_end:

	IF (postkey = KW_SUPER) AND (method=FALSE) THEN reportErr('SUPER needs method') -> v50

	IF isexp = FALSE  -> 2.2.2
	   IF n.data = "="
	      reportErr('spelled ":=" wrong ?')
	   ENDIF
	ENDIF

	size := dest - it_varexp

	IF it_varexp.assignmod OR incdec
	   useVar(var,1)
	ELSE
	   useVar(var,0)
	ENDIF

	IF (size = SIZEOF it_varexp) AND (postkey = FALSE)
	   -> one item ? (variable)
	   r.data := IT_VARIABLE
	   r.info := it_varexp.var
	   r++
	   RETURN n, r
	ENDIF

	rdest := FASTNEW(size + SIZEOF item)
	CopyMem(it_varexp, rdest, size)

	r.data := IT_VAREXP
	r.info := rdest
	r++

	g_extracodesize := g_extracodesize + (size / SIZEOF item * 6) -> v42

ENDPROC n, r



-> unlimited length version v50 (1.5.1)
-> 1.8.2: added alignment support
PROC immedlist2(n:PTR TO item, r:PTR TO item,postkey)
	DEF dest:PTR TO litem, prevdest:PTR TO litem, dummy:litem
	DEF d:REG, len=0, hln:PTR TO hln, object=NIL:PTR TO object
	DEF lsize, a, b
	DEF it_immedlist:PTR TO it_immedlist
	DEF align, esize, eflags=NIL, cmplx=FALSE

	    #ifdef DBG_PASS1
	    DEBUGF('immedlist2($\h, $\h)\n', n, r)
	    #endif

	dummy.next := NIL -> safety
	prevdest := dummy

	NEW it_immedlist

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   IF d =  "]"
	      JUMP immedlist2_end
	   ELSE
	      dest := FASTNEW(SIZEOF litem)
	      prevdest.next := dest
	      n := expression(n, dest)
	      prevdest := dest
	      len++
	      IF len > $7FFF THEN reportErr('too large immediate list')
	      IF n.data = ","
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	      ELSEIF n.data <> "]" -> 1.7.2
	         reportErr('syntax error in immediate list')
	      ENDIF
	   ENDIF
	ENDWHILE

immedlist2_end:

	IF n.data <> "]" THEN reportErr('"]" expected')
	n++

	it_immedlist.litems := dummy.next

	IF n.data = ":"
	   n++
	   d := n.data
	   SELECT 256 OF d
	   CASE KW_DOUBLE, KW_REAL -> v50
	      lsize := (len * 8)
	      esize := 8
	      eflags := MEMBF_FLOAT
	      align := 8
	   CASE KW_WIDE -> v56
	      lsize := (len * 8)
	      esize := 8
	      align := 8
	   CASE KW_LONG, KW_PTR
	      lsize := (len * 4)
	      esize := 4
	      align := 4
	   CASE KW_FLOAT
	      lsize := (len * 4)
	      esize := 4
	      eflags := MEMBF_FLOAT
	      align := 4
	   CASE KW_INT,KW_WORD
	      lsize := (len * 2 + 3 AND $FFFC)
	      esize := 2
	      align := 2
	   CASE KW_CHAR,KW_BYTE
	      lsize := (len + 3 AND $FFFC)
	      esize := 1
	      align := 2
	   CASE IT_LABEL
	      hln := n.info
	      object := hln.ident2
	      IF object = NIL THEN reportErr('unknown identifier', hln.name)
	      IF object.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	      IF object.nrofmembers = 0 THEN reportErr('immediate object must have members', hln.name)
	      esize := 0
	      lsize := object.sizeof *
	               (Div(len, object.nrofmembers) +
	                IF Mod(Max(len,1), object.nrofmembers) THEN 1 ELSE 0)
	      align := Max(object.alignment, 2)
	   DEFAULT
	      reportErr('illegal type for list')
	   ENDSELECT
	   n++
	ELSE
	   lsize := Mul(len,4)
	   esize := 4
	   cmplx := TRUE
	   align := 4
	ENDIF

	IF postkey <> KW_NEW
	   g_databufsize := g_databufsize + (align-1) AND (0-align)
	   IF cmplx THEN g_databufsize := g_databufsize + 4
	   it_immedlist.offset := g_databufsize
	   g_databufsize := g_databufsize + lsize
	ENDIF

	it_immedlist.esize := esize
	it_immedlist.eflags := eflags
	it_immedlist.cmplx := cmplx
	it_immedlist.object := object
	it_immedlist.numelems := len
	it_immedlist.newed := IF postkey = KW_NEW THEN TRUE ELSE FALSE
	it_immedlist.sizeof := lsize


	r.data := IT_IMMEDLIST
	r.info := it_immedlist
	r++

	g_extracodesize := g_extracodesize + (len*4)

ENDPROC n, r



-> exp <=> [..]
-> pass "exp" as "n".
PROC uniexp(n:PTR TO item, r:PTR TO item, realexp)
	DEF destbuf[256]:ARRAY OF item, dest:PTR TO item, rdest
	DEF d:REG, hln:PTR TO hln, object:PTR TO object
	DEF var:PTR TO var


	    #ifdef DBG_PASS1
	    DEBUGF('uniexp($\h, $\h)\n', n, r)
	    #endif


	dest := destbuf

	dest.data := r.data  -> the exp before <=>
	dest.info := r.info
	dest.num := r.num
	dest++

	n++ -> skip <=>

	n, dest := unilist(n, dest)

	rdest := FASTNEW(dest - destbuf + SIZEOF item)
	CopyMem(destbuf, rdest, dest - destbuf)

	r.data := IT_UNIEXP
	r.num := realexp
	r.info := rdest
	r++

ENDPROC n, r

PROC unilist(n:PTR TO item, dest:PTR TO item)
	DEF len=0, start:PTR TO item, d, hln:PTR TO hln
	DEF var:PTR TO var, object:PTR TO object

	IF (n.data <> "[") AND (n.data <> "<") THEN reportErr('"[" or "<" expected')
	start := dest
	dest.data := n.data
	dest.info := NIL
	dest.num := 0
	dest++
	n++

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   SELECT d
	   CASE "["
	      n, dest := unilist(n, dest)
	   CASE "<"
	      n, dest := unilist(n, dest)
	   CASE IT_LABEL
	      hln := n.info
	      var := hln.ident
	      IF var = NIL THEN reportErr('unknown identifier', hln.name)
	      IF var.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	      dest.data := IT_VARIABLE
	      dest.info := var
	      useVar(var,1)
	      dest++
	      n++
	      len++
	   CASE IT_VALUE
	      dest.data := IT_VALUE
	      dest.num := n.num
	      dest.info := n.info
	      dest++
	      n++
	      len++
	   CASE "*"
	      dest.data := "*"
	      dest++
	      n++
	      len++
	   DEFAULT
	      JUMP unilist_end
	   ENDSELECT
	   IF n.data = ","
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	   ELSEIF n.data = "|"
	      dest.data := "|"
	      dest++
	      n++
	   ELSEIF (n.data <> "]") AND (n.data <> ">")
	      reportErr('unification syntax')
	   ENDIF
	ENDWHILE

unilist_end:

	IF (n.data <> "]") AND (n.data <> ">") THEN reportErr('"]" or ">" expected')
	dest.data := n.data
	dest++
	n++
	start.num := len
	IF n.data = ":"
	   n++
	   d := n.data
	   SELECT d
	   CASE KW_LONG ; start.info := 4
	   CASE KW_INT  ; start.info := 2
	   CASE KW_CHAR ; start.info := 1
	   CASE IT_LABEL
	      hln := n.info
	      object := hln.ident2
	      IF object = NIL THEN reportErr('unknown identifier', hln.name)
	      IF object.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	      start.info := object
	   DEFAULT
	      reportErr('syntax')
	   ENDSELECT
	   n++
	ENDIF
ENDPROC n, dest

-> call in pass 1 when entering function
PROC enteringFunc()

	c_functiondepth++
	IF g_currentproc
	   g_currentproc.maxcalldepth := Max(g_currentproc.maxcalldepth, c_functiondepth)
	ENDIF

ENDPROC

-> call in pass 1 when leaving function
PROC leavingFunc(numargs)

	IF g_currentproc
	   g_currentproc.maxcallargs := Max(g_currentproc.maxcallargs, numargs) -> ppc
	ENDIF
	c_functiondepth--

	g_extracodesize := g_extracodesize + 64 + (numargs*8)

ENDPROC

-> proc call
PROC function(n:PTR TO item, r:PTR TO item)
	DEF destbuf[257]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF d:REG, size, hln:PTR TO hln, len
	DEF it_funccall:PTR TO it_funccall


	enteringFunc()

	it_funccall := destbuf

	hln := n.info
	it_funccall.data := hln

	-> 1.7.1 we allow uppercase proc, useful inlinkobjmode fex.
	-> it may very well be a misspelled/undefined LFUNC/IFUNC/MACRO/ETC if UC!
	->IF n.num = 1 THEN reportErr('unknown function', hln.name)
	dest := it_funccall + SIZEOF it_funccall
	n++
	n++ -> skip "("

	 #ifdef DBG_PASS1
	 DEBUGF('function($\h, $\h) : \q\s\q\n', n, r, hln.name)
	 #endif

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   IF d = ")"
	      JUMP function_end
	   ELSE
	      n, dest := expression(n, dest)
	      IF n.data <> ")"
	         IF n.data <> "," THEN reportErr('")" or "," expected')
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	      ELSEIF n.data <> ")" -> 1.7.2
	         reportErr('parameter list syntax')
	      ENDIF
	   ENDIF
	ENDWHILE
	IF n.data <> ")" THEN reportErr('")" expected')

function_end:

	n++ -> skip ")"

	size := dest - destbuf

	len := size - SIZEOF it_funccall / SIZEOF item

	IF len > 255 THEN reportErr('too many parameters for function', hln.name)

	it_funccall.numpars := len

	rdest := FASTNEW(size + SIZEOF item)
	CopyMem(destbuf, rdest, size)

	r.data := IT_PFUNC
	r.info := rdest

	leavingFunc(len)

	r++

ENDPROC n, r

-> ifunc call
PROC ifunction(n:PTR TO item, r:PTR TO item)
	DEF destbuf[257]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF d:REG, size, ifunc:PTR TO lif
	DEF len
	DEF it_funccall:PTR TO it_funccall

	enteringFunc()

	dest := destbuf

	it_funccall := dest
	ifunc := n.info
	it_funccall.data := ifunc
	dest := it_funccall + SIZEOF it_funccall

	n++
	n++ -> skip "("

	#ifdef DBG_PASS1
	 DEBUGF('ifunction($\h, $\h) \q\s\q flags $\h numparams \d numdefs \d\n',
	   n, r,ifunc.name, ifunc.flags, ListLen(ifunc.params), ListLen(ifunc.defaults))
	 #endif

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   IF d = ")"
	      JUMP ifunction_end
	   ELSE
	      n, dest := expression(n, dest)
	      IF n.data <> ")"
	         IF n.data <> "," THEN reportErr('")" or "," expected')
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	      ELSEIF n.data <> ")" -> 1.7.2
	         reportErr('parameter list syntax')
	      ENDIF
	   ENDIF
	ENDWHILE
	IF n.data <> ")" THEN reportErr('")" expected')

ifunction_end:



	n++ -> skip ")"

	size := dest - destbuf

	len := (size - SIZEOF it_funccall) / SIZEOF item

	it_funccall.numpars := len

	IF ifunc.flags <> 2 -> no varargs ?
	   IF len > ListLen(ifunc.params) THEN reportErr('too many parameters for internal function', ifunc.name)
	ENDIF

	IF (len+ListLen(ifunc.defaults)) < ListLen(ifunc.params) THEN reportErr('too few parameters for function', ifunc.name)

	rdest := FASTNEW(size + SIZEOF item)

	CopyMem(destbuf, rdest, size)

	r.data := IT_IFUNC
	r.info := rdest

	leavingFunc(Max(ListLen(ifunc.params), len))

	r++

	#ifdef DBG_PASS1
	DEBUGF('ifunction "\s" DONE len \d\n', ifunc.name, len)
	#endif

ENDPROC n, r

-> lfunc call
PROC lfunction(n:PTR TO item, r:PTR TO item)
	DEF destbuf[257]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF d:REG, size, lfunc:PTR TO lfunc, var:PTR TO var
	DEF len, it_funccall:PTR TO it_funccall

	enteringFunc()

	it_funccall := destbuf

	lfunc := n.info
	it_funccall.data := lfunc
	dest := it_funccall + SIZEOF it_funccall

	n++
	n++ -> skip "("
	var := lfunc.basehln.ident
	IF var = NIL THEN reportErr('unknown librarybase', lfunc.basehln.name)
	IF var.identID <> IDENT_VARIABLE THEN reportErr('librarybase is not variable', lfunc.basehln.name)
	it_funccall.info := var
	useVar(var,0)

	      #ifdef DBG_PASS1
	    DEBUGF('lfunction($\h, $\h) \q\s\q\n', n, r, lfunc.name)
	    #endif


	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   IF d = ")"
	      JUMP lfunction_end
	   ELSE
	      n, dest := expression(n, dest)
	      IF n.data <> ")"
	         IF n.data <> "," THEN reportErr('")" or "," expected')
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	      ELSEIF n.data <> ")" -> 1.7.2
	         reportErr('parameter list syntax')
	      ENDIF
	   ENDIF
	ENDWHILE
	IF n.data <> ")" THEN reportErr('")" expected')

lfunction_end:

	n++ -> skip ")"

	size := dest - destbuf

	len := (size - SIZEOF it_funccall)  / SIZEOF item

	it_funccall.numpars := len

	rdest := FASTNEW(size + SIZEOF item)
	CopyMem(destbuf, rdest, size)

	r.data := IT_LFUNC
	->r.num := len -> nr of params
	r.info := rdest

	leavingFunc(lfunc.nrofargs)
	r++

ENDPROC n, r

-> vfunc call
PROC vfunction(n:PTR TO item, r:PTR TO item)
	DEF destbuf[257]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF d:REG, size, var:PTR TO var, len
	DEF it_funccall:PTR TO it_funccall
	DEF a=0, b=0, arg:PTR TO arg, multiret:PTR TO multireturn, ptrarray[32]:ARRAY OF LONG   -> 1.9.0

	enteringFunc()

	it_funccall := destbuf
	var := n.info.ident
	it_funccall.data := var

	useVar(var,0)
	dest := it_funccall + SIZEOF it_funccall
	n++
	n++ -> skip "("


	 #ifdef DBG_PASS1
	 DEBUGF('vfunction($\h, $\h) \q\s\q\n', n, r, var.hln.name)
	 #endif

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	WHILE (d := n.data)
	   IF d = ")"
	      JUMP vfunction_end
	   ELSE
	      n, dest := expression(n, dest)
	      IF n.data <> ")"
	         IF n.data <> "," THEN reportErr('")" or "," expected')
	         n++
	         IF n.data = 10
	            setLinenum(n.info)
	            n++
	         ENDIF
	      ELSEIF n.data <> ")" -> 1.7.2
	         reportErr('parameter list syntax')
	      ENDIF
	   ENDIF
	ENDWHILE
	IF n.data <> ")" THEN reportErr('")" expected')

vfunction_end:

	n++ -> skip ")"

	size := dest - destbuf

	len := (size - SIZEOF it_funccall) / SIZEOF item

	IF len > 255 THEN reportErr('too many parameters for function', var.hln.name)

	it_funccall.numpars := len

	rdest := FASTNEW(size + SIZEOF item)
	CopyMem(destbuf, rdest, size)

	r.data := IT_VFUNC
	r.info := rdest

	leavingFunc(len)

	r++

	IF var.argsarray -> 1.9.0
	   IF ListLen(var.argsarray) <> len THEN reportErr('wrong number of parameters for variable function', var.hln.name)
	ELSE
	   -> warn user: variable used as function
	   addWarning('variable used as function')
	ENDIF

ENDPROC n, r

-> "statement2" type of statements
PROC substatement(n:PTR TO item, r:PTR TO item)
	DEF destbuf[256]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF ident:PTR TO ident, hln:PTR TO hln, loclab:PTR TO loclab
	DEF d:REG, d2, size, n2:PTR TO item, len, t

	    #ifdef DBG_PASS1
	    DEBUGF('substatement($\h)\n', n)
	    #endif

	dest := destbuf

substatement_urgh__:

	d := n.data
	SELECT 256 OF d
	CASE IT_LABEL -> proc(), var() or varexp
	   hln := n.info
	   ident := hln.ident
	   IF ident
	      IF ident.identID = IDENT_VARIABLE
	         SELECT 256 OF n[1].data
	         CASE "("
	            n, dest := vfunction(n, dest)
	         CASE ".", "[", KW_ASSIGN, KW_PLUSPLUS, KW_MINMIN, KW_PTYPE, KW_MODIFY
	            n, dest := varexp(n,dest,0,0)
	         CASE ","-> v49
	            n, dest := multiassign(n, dest)
	         CASE KW_UNI
	            n, dest := uniexp(n+SIZEOF item, n, 0)
	         DEFAULT
	            reportErr('substatement syntax')
	         ENDSELECT
	      ELSEIF n[1].data = "(" -> bacward proc()
	         n, dest := function(n, dest)
	         IF n.data = KW_UNI
	            n, dest := uniexp(n, dest--, 0)
	         ENDIF
	      ELSE
	         reportErr('substatement syntax')
	      ENDIF
	   ELSEIF n[1].data = "(" -> forward proc()
	      n, dest := function(n, dest)
	      IF n.data = KW_UNI
	         n, dest := uniexp(n, dest--, 0)
	      ENDIF
	   ELSE
	      reportErr('substatement syntax')
	   ENDIF
	CASE IT_IFUNC
	   n, dest := ifunction(n, dest)
	CASE IT_LFUNC
	   n, dest := lfunction(n, dest)
	CASE "["
	   n++
	   n, dest := immedlist2(n, dest,0)
	   IF n.data <> KW_UNI THEN reportErr('"<=>" expected')
	   n, dest := uniexp(n, dest--, 0)
	CASE KW_NEW
	   n++
	   enteringFunc()
	   n, dest := varexp(n,dest,KW_NEW,0)
	   leavingFunc(0)
	CASE KW_END
	   n++
	   enteringFunc()
	   n, dest := varexp(n,dest,KW_END,0)
	   leavingFunc(0)
	CASE KW_RETURN
	   n2 := dest
	   dest.data := KW_RETURN
	   dest++
	   n++
	   t := NIL
	   IF n.data <> KW_ELSE -> v50 (1.5.2)
	      n, dest, t := explist(n, dest)
	   ENDIF
	   n2.num := t -> v49
	CASE KW_SUPER
	   n++
	   n, dest := varexp(n, dest,KW_SUPER,0)
	CASE KW_IF, KW_IFN
	   dest.data := KW_IF
	   dest.num := IF d = KW_IF THEN 0 ELSE 1 -> N bool
	   dest++
	   n++
	   n, dest := expression(n, dest)
	   IF n.data <> KW_THEN THEN reportErr('"THEN" expected')
	   dest.data := KW_THEN
	   dest.info := n.info
	   dest++
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   n, dest := substatement(n, dest)
	   IF n.data = KW_ELSE
	      dest.data := KW_ELSE
	      dest.info := n.info
	      dest++
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	      n, dest := substatement(n, dest)
	   ENDIF
	CASE KW_FOR
	   g_currentloopdepth++
	   dest.data := KW_FOR
	   dest++
	   n++
	   IF n.data <> IT_LABEL THEN reportErr('variable expected')
	   hln := n.info
	   IF hln.ident = NIL THEN reportErr('unknown identifier', hln.name)
	   IF hln.ident.identID <> IDENT_VARIABLE THEN reportErr('variable expected')
	   dest.data := IT_VARIABLE
	   dest.info := hln.ident
	   useVar(hln.ident,1)
	   dest++
	   n++
	   IF n.data <> KW_ASSIGN THEN reportErr('":=" expected')
	   n++
	   n, dest := expression(n,dest)
	   IF n.data <> KW_TO THEN reportErr('"TO" expected')
	   n++
	   n, dest := expression(n, dest)
	   IF n.data = KW_STEP
	      dest.data := KW_STEP
	      dest++
	      n++
	      n, dest := exp_constexp(n, dest)
	   ENDIF
	   IF n.data <> KW_DO THEN reportErr('"DO" expected')
	   dest.data := KW_DO
	   dest++
	   n++
	   n, dest := substatement(n, dest)
	   g_currentloopdepth--
	CASE KW_WHILE, KW_WHILEN
	   g_currentloopdepth++
	   dest.data := KW_WHILE
	   dest.num := IF d = KW_WHILE THEN 0 ELSE 1 -> N bool
	   dest++
	   n++
	   n, dest := expression(n, dest)
	   IF n.data <> KW_DO THEN reportErr('"DO" expected')
	   dest.data := KW_DO
	   dest++
	   n++
	   n, dest := substatement(n, dest)
	   g_currentloopdepth--
	CASE KW_LOOP
	   g_currentloopdepth++
	   dest.data := KW_LOOP
	   dest++
	   n++
	   IF n.data <> KW_DO THEN reportErr('"DO" expected')
	   dest.data := KW_DO
	   dest++
	   n++
	   n, dest := substatement(n, dest)
	   g_currentloopdepth--
	CASE KW_INC, KW_DEC
	   dest.data := n.data
	   dest++
	   n++
	   IF n.data <> IT_LABEL THEN reportErr('variable expected')
	   hln := n.info
	   ident := hln.ident
	   IF ident = NIL THEN reportErr('unknown identifier', hln.name)
	   IF ident.identID <> IDENT_VARIABLE THEN reportErr('variable expected', hln.name)
	   dest.data := IT_VARIABLE
	   dest.info := ident
	   useVar(ident,1)
	   dest++
	   n++
	CASE KW_JUMP
      dest.data := KW_JUMP
      dest++
      n++
      IF n.data = "." -> 2.3.1, was forgotten
         -> local label
         n++
         IF n.data <> IT_LABEL THEN reportErr('label expected')
         hln := n.info
         loclab := getLocalLabel(hln.name)
         hln := loclab.hln
      ELSE
         IF n.data <> IT_LABEL THEN reportErr('label expected')
         hln := n.info
      ENDIF
      dest.data := IT_LABEL
      dest.info := hln
      dest++
      n++
	CASE "("
	   n++
	   n, dest := substatement(n, dest)
	   IF n.data <> ")" THEN reportErr('")" expected')
	   n++
	CASE KW_NOP
	   n++
	   dest.data := KW_NOP
	   dest++
	CASE KW_VOID
	   dest.data := KW_VOID
	   dest++
	   n++
	   n, dest := expression(n, dest)
	CASE KW_ELSE   -> 1.8.1 IF x THEN \n y ELSE \n z
	   JUMP substatement_done__
	DEFAULT
	   reportErr('substatement does not compute')
	ENDSELECT

	IF n.data = KW_BUT
	   dest.data := KW_BUT
	   dest++
	   n++
	   JUMP substatement_urgh__
	ENDIF

substatement_done__: -> 1.8.1

	size := dest - destbuf
	len := size / SIZEOF item

	IF size > SIZEOF item

	   rdest := FASTNEW(size + SIZEOF item)
	   CopyMem(destbuf, rdest, size)

	   r.data := IT_SUBSTATEMENT
	   r.num := len
	   r.info := rdest

	ELSEIF size = SIZEOF item -> one item (not converted to "compound" expression)

	   r.data := destbuf.data
	   r.num := destbuf.num
	   r.info := destbuf.info

	ELSE -> 1.8.1

	   reportErr('substatement error') -> could be an "ELSE" only

	ENDIF

	g_extracodesize := g_extracodesize + (len*6)  -> v42

	r++

ENDPROC n, r

PROC exp_if(n:PTR TO item, r:PTR TO item) -> v44
	DEF destbuf[10]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF size, len

	dest := destbuf

	dest.num := IF n.data = KW_IF THEN 0 ELSE 1 -> N bool
	dest.data := KW_IF
	dest++
	n++
	n, dest := expression(n, dest)
	IF n.data <> KW_THEN THEN reportErr('"THEN" expected')
	dest.data := KW_THEN
	dest++
	n++
	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF
	n, dest := expression(n, dest)
	IF n.data = KW_ELSE
	   dest.data := KW_ELSE
	   dest++
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   n, dest := expression(n, dest)
	ENDIF

	size := dest - destbuf

	len := size / SIZEOF item

	rdest := FASTNEW(size + SIZEOF item)
	CopyMem(destbuf, rdest, size)
	r.data := IT_IFEXP
	r.num := len
	r.info := rdest

	r++
	g_extracodesize := g_extracodesize + 32

ENDPROC n, r

PROC shr(x,v)
	IF v = 0 THEN RETURN x
ENDPROC Shr(x,v) AND Not(Shr($80000000, v-1))

-> 1.10.0: added XOR, >>, ~
-> 2.3 todo: put down "!" in front of it_value if in float mode.  *DONE*
PROC exp_constexp(n:PTR TO item, dest:PTR TO item)
	DEF mode,val,val2, temp, op

	#ifdef DBG_PASS1
	DEBUGF('exp_constexp($\h, $\h)\n', n, dest)
	#endif

	temp := n

	IF n.data = "!"
	   n++
	   mode := MODE_FLOAT
	ELSE
	   mode := MODE_DEFAULT
	ENDIF

	n, val := constVal(n, mode, TRUE)
	IF n = NIL THEN RETURN temp, dest, MODE_DEFAULT

	LOOP

	IF n.data = "!"
	   IF mode = MODE_DEFAULT
	      val := val!
	      mode := MODE_FLOAT
	   ELSE
	      val := !val!
	      mode := MODE_DEFAULT
	   ENDIF
	   n++
	ENDIF

	temp := n

	SELECT 256 OF n.data
	CASE "+" ; n++ ; op := IF mode = MODE_FLOAT THEN `val := !val + val2 ELSE `val := val + val2
	CASE "-" ; n++ ; op := IF mode = MODE_FLOAT THEN `val := !val - val2 ELSE `val := val - val2
	CASE "*" ; n++ ; op := IF mode = MODE_FLOAT THEN `val := !val * val2 ELSE `val := Mul(val,val2)
	CASE "/" ; n++ ; op := IF mode = MODE_FLOAT THEN `val := !val / val2 ELSE `val := Div(val,val2)
	CASE KW_OR  ; n++ ; op := `val := val OR val2
	CASE KW_AND ; n++ ; op := `val := val AND val2
	CASE KW_SHL ; n++ ; op := `val := Shl(val,val2)
	CASE KW_SHR ; n++ ; op := `val := shr(val,val2)
	CASE KW_ASR ; n++ ; op := `val := Shr(val,val2)
	CASE KW_XOR ; n++ ; op := `val := Eor(val,val2)
	DEFAULT
	   IF mode = MODE_FLOAT -> 2.3
	      dest.data := "!"
	      dest.num := 0
	      dest.info := NIL
	      dest++
	   ENDIF
	   dest.data := IT_VALUE
	   dest.num := mode
	   dest.info := val
	   dest++
	   #ifdef DBG_PASS1
	   DEBUGF('exp_constexp DONE\n')
	   #endif
	   RETURN n, dest, mode
	ENDSELECT

	IF mode = MODE_D64 THEN reportErr('64bit constant integer math not working yet')

	IF n.data = 10
	   setLinenum(n.info)
	   n++
	ENDIF

	n, val2 := constVal(n, mode, TRUE)

	IF n = NIL
	   IF mode = MODE_FLOAT -> 2.3
	      dest.data := "!"
	      dest.num := 0
	      dest.info := NIL
	      dest++
	   ENDIF
	   dest.data := IT_VALUE
	   dest.num := mode
	   dest.info := val
	   dest++
	     #ifdef DBG_PASS1
	   DEBUGF('exp_constexp DONE\n')
	   #endif
	   RETURN temp, dest, mode
	ENDIF

	Eval(op)

	ENDLOOP

ENDPROC

-> new function v1.5.4
PROC expression(n:PTR TO item, r:PTR TO item)
	DEF destbuf[257]:ARRAY OF item, dest:REG PTR TO item, rdest
	DEF ident:PTR TO ident
	DEF d:REG, d2, size, n2:PTR TO item, o:PTR TO object, hln:PTR TO hln, mode=MODE_DEFAULT
	DEF len, quote=FALSE

	    #ifdef DBG_PASS1
	    DEBUGF('expression($\h)\n', n)
	    #endif

	IF FreeStack() < 4000 THEN Raise("STCK") -> 2.0


	dest := destbuf

	IF n.data = "`"
	   dest.data := "`"
	   dest++
	   n++
	ENDIF

	n, d, mode := exp_constexp(n, dest) -> smacks leading constants into IT_VALUE

	IF d <> dest

	   dest := d

	ELSE

	   IF n.data = "!"
	      dest.data := "!"
	      dest++
	      n++
	      mode := MODE_FLOAT
	   ELSEIF n.data = "@" -> 1.10.0
	      dest.data := "@"
	      dest++
	      n++
	      mode := MODE_D64
	   ENDIF

	   n, dest, mode := expval(n, dest, mode)

	ENDIF

	WHILE (d := n.data)

	   ->DebugF('expression / a')

	   SELECT 256 OF d
	   CASE KW_BUT
	      dest.data := d
	      dest++
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	      n, dest := expression(n, dest)
	   CASE "=", "<", ">", KW_ISGE, KW_ISNE, KW_ISLE,
	        "+", "-", "*", "/", KW_OR, KW_AND, KW_SHR, KW_SHL, KW_ASR, KW_XOR
	      SELECT 256 OF d -> 1.8.1
	      CASE "=", "<", ">", KW_ISGE, KW_ISNE, KW_ISLE
	         mode := MODE_DEFAULT
	      ENDSELECT
	      dest.data := d
	      dest++
	      n++
	      IF n.data = 10
	         setLinenum(n.info)
	         n++
	      ENDIF
	      n, dest, mode := expval(n, dest, mode)
	   DEFAULT
	      JUMP expression154_end
	   ENDSELECT
  ENDWHILE


	->reportErr('unexpected end of source')

expression154_end:

	->DebugF('expression / b')

	size := dest - destbuf

	len := size / SIZEOF item

	IF len > 256 THEN reportErr('too sizy expression')

	IF size > SIZEOF item -> several compound items

	   rdest := FASTNEW(size + SIZEOF item)
	   CopyMem(destbuf, rdest, size)

	   r.data := IT_EXPRESSION
	   r.info := rdest

	ELSEIF size = SIZEOF item -> one item (not converted to "compound" expression)

	   r.data := destbuf.data
	   r.num := destbuf.num
	   r.info := destbuf.info

	ELSE -> size = 0 -> no item ?

	   reportErr('expression expected')

	ENDIF

	r++
	g_extracodesize := g_extracodesize + (len*6) -> v42

	->DebugF('expression() DONE')

ENDPROC n, r

PROC expval(n:PTR TO item, dest:PTR TO item, mode)
	   DEF d

	   #ifdef DBG_PASS1
	   DEBUGF('expval($\h, $\h, \d)\n', n, dest, mode)
	   #endif

	   IF n.data = "-"
	      IF n[1].data = IT_VALUE -> 1.6.0
	         dest.data := IT_VALUE
	         IF mode = MODE_FLOAT
	            d := !0.0-n[1].info
	         ELSE
	            d := 0-n[1].info
	         ENDIF
	         dest.info := d
	         dest++
	         n++ ; n++
	         JUMP expval_end
	      ELSE
	         dest.data := "-"
	         dest++
	         n++
	      ENDIF
	   ELSEIF n.data = "~" -> 1.10.0
	      IF n[1].data = IT_VALUE
	         dest.data := IT_VALUE
	         IF mode = MODE_FLOAT
	            reportErr('unsupported operator for floating point mode')
	         ELSE
	            d := Not(n[1].info)
	         ENDIF
	         dest.info := d
	         dest++
	         n++ ; n++
	         JUMP expval_end
	      ELSE
	         dest.data := "~"
	         dest++
	         n++
	      ENDIF
	   ELSEIF n.data = KW_ABS -> 2.2
	      dest.data := KW_ABS
	      dest++
	      n++
	   ENDIF
	   d := n.data
	   SELECT 256 OF d
	   CASE KW_IF, KW_IFN
	      n, dest := exp_if(n, dest)
	   DEFAULT
	      n, dest := simpleexp(n, dest)
	   ENDSELECT

	   IF n.data = KW_UNI
	      n, dest := uniexp(n, dest--, TRUE)
	   ENDIF

expval_end:

	   IF n.data = "!"
	      dest.data := "!"
	      dest++
	      n++
	      mode := IF mode = MODE_FLOAT THEN MODE_DEFAULT ELSE MODE_FLOAT
	   ELSEIF n.data = "@" -> 1.10.0
	      dest.data := "@"
	      dest++
	      n++
	      mode := IF mode = MODE_D64 THEN MODE_DEFAULT ELSE MODE_D64
	   ENDIF

ENDPROC n, dest, mode

-> 1.5.4
PROC simpleexp(n:PTR TO item, dest:PTR TO item)
	   DEF d, hln:PTR TO hln, ident:PTR TO ident
	   DEF o:PTR TO object, p:PTR TO proc, a
	   DEF loclab:PTR TO loclab

	   #ifdef DBG_PASS1
	   DEBUGF('simpleexp($\h, $\h)\n', n, dest)
	   #endif

	   d := n.data
	   SELECT 256 OF d
	   CASE IT_LABEL  -> proc(), var(), varexp
	      hln := n.info
	      ident := hln.ident
	      a := n[1].data
	      IF ident
	         IF ident.identID = IDENT_VARIABLE
	            IF a = "("
	               n, dest := vfunction(n, dest)
	            ELSE
	               n, dest := varexp(n,dest,0,1)
	            ENDIF
	         ELSEIF a = "(" -> bacward proc()
	            n, dest := function(n, dest)
	         ELSE
	            n, dest := labderef(n,dest)
	         ENDIF
	      ELSEIF a = "(" -> forward proc()
	         n, dest := function(n, dest)
	      ELSE
	         -> 1.9.0, labels without {} are ok now
	         n, dest := labderef(n,dest)
	      ENDIF
	   CASE IT_IFUNC
	      n, dest := ifunction(n, dest)
	   CASE IT_LFUNC
	      n, dest := lfunction(n, dest)
	   CASE IT_REG
	      dest.data := n.data
	      dest.info := n.info
	      dest++
	      n++
	   CASE IT_VALUE
	      dest.data := n.data
	      dest.num := n.num
	      dest.info := n.info
	      dest++
	      n++
	   CASE KW_NEW
	      n++
	      enteringFunc()
	      IF n.data = IT_LABEL
	         n, dest := varexp(n,dest,KW_NEW,1)
	      ELSEIF n.data = "["
	         n++
	         n, dest := immedlist2(n, dest,KW_NEW)
	      ELSEIF n.data = IT_STRING
	         dest.data := IT_NEWSTRING
	         dest.info := n.info
	         n++
	         dest++
	      ELSE
	         reportErr('NEW syntax')
	      ENDIF

	      leavingFunc(0)
	   CASE KW_SUPER
	      n++
	      n, dest := varexp(n, dest,KW_SUPER,1)
	   CASE IT_STRING
	      dest.data := IT_STRING
	      dest.info := n.info
	      n++
	      dest++
	   CASE "("
	      n++
	      IF n.data = 10  -> v47
	         setLinenum(n.info)
	         n++
	      ENDIF
	      n, dest := expression(n, dest)
	      IF n.data <> ")" THEN reportErr('")" expected')
	      n++
	   CASE "["
	      n++
	      IF n.data = 10  -> v47
	         setLinenum(n.info)
	         n++
	      ENDIF
	      n, dest := immedlist2(n, dest,NIL)
	   CASE "{"
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('label expected')
	      hln := n.info
	      ident := hln.ident
	      IF ident
	         IF ident.identID = IDENT_VARIABLE
	            IF ident::var.type.size = 0 THEN reportErr('array is already address', hln.name)
	            dest.data := IT_VARADR
	            dest.info := ident
	            ident::var.usage := 1
	            ident::var.trok := FALSE ->!
	         ELSE
	            dest.data := IT_LABADR
	            dest.info := hln
	         ENDIF
	      ELSE
	         dest.data := IT_LABADR
	         dest.info := hln
	      ENDIF
	      dest++
	      n++
	      IF n.data <> "}" THEN reportErr('"}" expected')
	      n++
	   CASE "." -> 2.2, local label
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('label expected')
	      hln := n.info
	      n++
	      loclab := getLocalLabel(hln.name)
	      hln := loclab.hln
	      dest.data := IT_LABADR
	      dest.info := hln
	      dest++
	   DEFAULT
	      reportErr('expression expected')
	   ENDSELECT

ENDPROC n, dest

-> 1.9.0
PROC labderef(n:PTR TO item, dest:PTR TO item)
	DEF t, destarray[128]:ARRAY OF item, d:PTR TO item, exit=FALSE
	DEF hln:PTR TO hln, object:PTR TO object, member:PTR TO member, size
	DEF lab:PTR TO statlab, type:member
	d := destarray
	d.data := IT_LABEL
	hln := n.info
	d.info := hln
	d++
	n++
	lab := hln.ident
	IF lab = NIL  -> 1.10.0 forward label ?
	   dest.data := IT_LABADR
	   dest.info := hln
	   dest++
	   IF n.data THEN reportErr('unknown label', hln.name) -> 2.0
	   RETURN n, dest
	ELSEIF lab.ltype = LTYPE_STATIC
	   type.size := lab.deref.size
	   type.esize := lab.deref.esize
	   type.flags := lab.deref.flags
	   type.object := lab.deref.object
	ELSE
	   type.size := 0
	   type.esize := 0
	   type.flags := NIL
	   type.object := NIL
	ENDIF
	REPEAT
	   t := n.data
	   IF t = "["
	      d.data := "["
	      d++
	      n++
	      IF (type.size <> 0) AND (type.size <> 4) THEN reportErr('cannot index this')
	      IF type.esize = 0 THEN reportErr('cannot index this')
	      IF n.data <> "]"
	        n, d := expression(n, d)
	      ENDIF
	      IF n.data <> "]" THEN reportErr('"]" expected')
	      d.data := "]"
	      d++
	      n++
	      type.size := IF type.esize = 255 THEN 0 ELSE type.esize
	   ELSEIF t = "."
	      IF type = NIL THEN reportErr('cannot dereference this')
	      IF type.object = NIL THEN reportErr('only objects have members')
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('member expected')
	      member := findMember(type.object, n.info.name)
	      IF member = NIL THEN reportErr('unknown member', n.info.name)
	      d.data := IT_MEMBER
	      d.info := member
	      type.size := member.size
	      type.esize := member.esize
	      type.flags := member.flags
	      type.object := member.object
	      d++
	      n++
	   ELSEIF t = KW_PTYPE
	     n++
	     IF n.data <> IT_LABEL THEN reportErr('object expected')
	     hln := n.info
	     object := hln.ident2
	     IF object = NIL THEN reportErr('unknown object', hln.name)
	     IF object.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	     n++
	     IF n.data <> "." THEN reportErr('"." expected') -> v50
	     n++
	     IF n.data <> IT_LABEL THEN reportErr('member expected')
	     member := findMember(object, n.info.name)
	     IF member = NIL THEN reportErr('unknown member', n.info.name)
	     d.data := IT_MEMBER
	     d.info := member
	     d++
	     n++
	   ELSE
	      exit := TRUE
	   ENDIF

	UNTIL exit

	size := d - destarray

	IF size > SIZEOF item
	   dest.data := IT_LABDEREF
	   dest.info := FastNew(size+SIZEOF item)
	   CopyMem(destarray, dest.info, size)
	   dest++
	ELSE
	   dest.data := IT_LABADR
	   dest.info := destarray.info
	   dest++
	ENDIF

ENDPROC n, dest

-> 1.10.0: added ~, D, H codes.
PROC lex_string(b, str) -> v46
	DEF c, v, r
	DEF left=FALSE,zerofill=FALSE , tstr[50]:STRING

	#ifdef DBG_PASS1
	DEBUGF('lex_string() ...')
	#endif

	WHILE (c := b[]) <> "\a"
	   EXIT c < 9
	   IF c = "\\"
	      b++
	      c := b[]++
	         SELECT 128 OF c
	         CASE "n"  ; StrAdd(str, '\n')
	         CASE "a"  ; StrAdd(str, '\a')
	         CASE "q"  ; StrAdd(str, '\q')
	         CASE "e"  ; StrAdd(str, '\e')
	         CASE "t"  ; StrAdd(str, '\t')
	         CASE "\\" ; StrAdd(str, '\\')
	         CASE "0" TO "9"  ; str[EstrLen(str)] := c-"0" ; SetStr(str, EstrLen(str)+1)
	         CASE "b"  ; StrAdd(str, '\b')
	         CASE "!"  ; StringF(tstr, '\c', 7)  -> 1.5.4 CreativE, bell
	                   ; StrAdd(str, tstr)
	         CASE "v"  ; StringF(tstr, '\c', 11) -> 1.5.4 CreativE, vert tab
	                   ; StrAdd(str, tstr)
	         CASE "x"  ; StrCopy(tstr, '$') -> 1.5.4 CreativE, immed hexval
	                   ; StrAdd(tstr, b, 2)
	                   ; b := b + 2
	                   ; v,r := Val(tstr)
	                   ; IF r = NIL THEN reportErr('illegal formatcode inside string')
	                   ->; StringF(tstr, '\c', v)
	                   ->; StrAdd(str, tstr)
	                   ; str[EstrLen(str)] := v ; SetStr(str, EstrLen(str)+1)
	         /* codes for stringformatting functions */
	         CASE "d"  ; StrAdd(str, '%')
	                   ; IF left THEN StrAdd(str, '-')
	                   ; IF zerofill THEN StrAdd(str, '0')
	                   ; IF b[] = "["
	                   ;    b++
	                   ;    v,r := Val(b)
	                   ;    b := b + r + 1
	                   ;    StringF(tstr, '\d.\d', v,v)
	                   ;    StrAdd(str, tstr)
	                   ; ENDIF
	                   ; StrAdd(str, 'ld')
	                   ; zerofill := FALSE
	         CASE "h"  ; StrAdd(str, '%')
	                   ; IF left THEN StrAdd(str, '-')
	                   ; IF zerofill THEN StrAdd(str, '0')
	                   ; IF b[] = "["
	                   ;    b++
	                   ;    v,r := Val(b)
	                   ;    b := b + r + 1
	                   ;    StringF(tstr, '\d.\d', v,v)
	                   ;    StrAdd(str, tstr)
	                   ; ENDIF
	                   ; StrAdd(str, 'l')
	                   ; StrAdd(str, 'x')
	                   ; zerofill := FALSE
	         CASE "c"  ; StrAdd(str, '%')
	                   ; StrAdd(str, 'l')
	                   ; StrAdd(str, 'c')
	                   ; zerofill := FALSE
	         CASE "s"  ; StrAdd(str, '%')
	                   ; IF left THEN StrAdd(str, '-')
	                   ; IF b[] = "["
	                   ;    b++
	                   ;    v,r := Val(b)
	                   ;    b := b + r + 1
	                   ;    StringF(tstr, '\d.\d', v,v)
	                   ;    StrAdd(str, tstr)
	                   ; ENDIF
	                   ; StrAdd(str, 's')
	                   ; zerofill := FALSE
	         CASE "l"  ; left := TRUE  -> affects rest
	         CASE "r"  ; left := FALSE -> affects rest
	         CASE "z"  ; zerofill := TRUE -> only affects next
	         -> 1.10.0
	         CASE "~"  ; -> do nothing
	         CASE "D"  ; StrAdd(str, '%')
	                   ; IF left THEN StrAdd(str, '-')
	                   ; IF zerofill THEN StrAdd(str, '0')
	                   ; IF b[] = "["
	                   ;    b++
	                   ;    v,r := Val(b)
	                   ;    b := b + r + 1
	                   ;    StringF(tstr, '\d.\d', v,v)
	                   ;    StrAdd(str, tstr)
	                   ; ENDIF
	                   ; StrAdd(str, 'lld')
	                   ; zerofill := FALSE
	         CASE "H"  ; StrAdd(str, '%')
	                   ; IF left THEN StrAdd(str, '-')
	                   ; IF zerofill THEN StrAdd(str, '0')
	                   ; IF b[] = "["
	                   ;    b++
	                   ;    v,r := Val(b)
	                   ;    b := b + r + 1
	                   ;    StringF(tstr, '\d.\d', v,v)
	                   ;    StrAdd(str, tstr)
	                   ; ENDIF
	                   ; StrAdd(str, 'llx')
	                   ; zerofill := FALSE
	         DEFAULT
	            reportErr('illegal formatcode inside string')
	         ENDSELECT
	   ELSE
	      StrAdd(str, [b[]++,0]:CHAR)
	   ENDIF
	ENDWHILE

	#ifdef DBG_PASS1
	DEBUGF(' DONE => \a\s\a\n', str)
	#endif

	IF b[]++ <> "\a" THEN reportErr('missing "\a"')

ENDPROC b


PROC parse68kAsm_(n:PTR TO item, dest:PTR TO item)
	DEF d:REG, hln:PTR TO hln, var:PTR TO var, t
	DEF reg:PTR TO reg, mask
	DEF memb:PTR TO member, obj:PTR TO object, regitem:PTR TO item, membname, objname
	DEF loclab:PTR TO loclab

	 #ifdef DBG_PASS1
	 DEBUGF('g_ops68k($\h, $\h)\n', n, dest)
	 #endif

	WHILE (d := n.data)
	   SELECT 256 OF d
	   CASE "("
	      dest.data := "("
	      dest++
	      n++
	      IF n.data = IT_REG
	         dest.data := IT_REG
	         dest.info := n.info
	         n.info::reg.referenced := 1
	         dest++
	         n++
	      ELSE
	         reportErr('register expected')
	      ENDIF

	      IF n.data = ","
	         dest.data := ","
	         dest++
	         n++
	         IF n.data = IT_REG
	            dest.data := n.data
	            dest.info := n.info
	            n.info::reg.referenced := 1
	         ELSEIF n.data = IT_LABEL
	            var := n.info.ident
	            IF var = NIL THEN reportErr('unknown identifier', n.info.name)
	            IF var.identID <> IDENT_VARIABLE THEN reportErr('operand syntax', n.info.name)
	            dest.data := IT_VARIABLE
	            dest.info := var
	            var.usage := 1
	            var.trok := FALSE
	         ELSE
	            reportErr('register expected')
	         ENDIF
	         dest++
	         n++
	         IF n.data = "*"
	            n++
	            IF n.data <> IT_VALUE THEN reportErr('value expected')
	            dest.data := n.data
	            dest++
	            n++
	         ENDIF
	      ENDIF

	      IF n.data <> ")" THEN reportErr('")" expected')
	      dest.data := ")"
	      dest++
	      n++
	      IF n.data = "+"
	         dest.data := "+"
	         dest++
	         n++
	      ENDIF
	   CASE "#"
	      dest.data := "#"
	      dest++
	      n++
	      n, dest := exp_constexp(n,dest)
	   CASE IT_REG
	      t := n[1].data
	      IF (t = "/") OR (t = "-") -> registerlist ?
	         -> handle registerlist and pass it as "#"+IT_VALUE (!)
	         mask := NIL
	         REPEAT
	            IF n.data <> IT_REG THEN reportErr('register expected')
	            reg := n.info
	            n++
	            t := n.data
	            IF reg.type = DRX
	               mask := mask OR Shl(1, 15-reg.num)
	               IF t = "-"
	                  t := reg.num
	                  IF n[1].data <> IT_REG THEN reportErr('register expected')
	                  reg := n[1].info
	                  IF reg.type <> DRX THEN reportErr('wrong type of register')
	                  WHILE t < (reg.num-1)
	                     t++
	                     mask := mask OR Shl(1, 15-t)
	                  ENDWHILE
	               ENDIF
	            ELSEIF reg.type = ARX
	               mask := mask OR Shl(1, 7-reg.num)
	               IF t = "-"
	                  t := reg.num
	                  IF n[1].data <> IT_REG THEN reportErr('register expected')
	                  reg := n[1].info
	                  IF reg.type <> ARX THEN reportErr('wrong type of register')
	                  WHILE t < (reg.num-1)
	                     t++
	                     mask := mask OR Shl(1, 7-t)
	                  ENDWHILE
	               ENDIF
	            ELSE
	               reportErr('wrong type of register')
	            ENDIF
	            t := n.data
	            IF (t = "/") OR (t = "-") THEN n++
	         UNTIL (t <> "/") AND (t <> "-")
	         dest.data := "#"
	         dest++
	         dest.data := IT_VALUE
	         dest.info := mask
	         dest++
	      ELSEIF t = ":" -> Dh:Dl ?
	         -> lets stuff data regnums into "immediate"
	         reg := n.info
	         IF reg.type <> DRX THEN reportErr('wrong type of register')
	         n := n[2]
	         dest.data := "#"
	         dest++
	         dest.data := IT_VALUE
	         dest.info := Shl(reg.num, 3)
	         IF n.data <> IT_REG THEN reportErr('register expected')
	         reg := n.info
	         IF reg.type <> DRX THEN reportErr('wrong type of register')
	         dest.info := dest.info OR reg.num
	         dest++
	         n++
	      ELSE
	         dest.data := IT_REG
	         dest.info := n.info
	         n.info::reg.referenced := 1
	         dest++
	         n++
	      ENDIF
	   CASE "-"
	      IF n[1].data = IT_VALUE
	         n, dest:= exp_constexp(n, dest)
	      ELSEIF n[1].data = "("
	         dest.data := n.data
	         dest++
	         n++
	         dest.data := n.data
	         dest++
	         n++
	         IF n.data = IT_REG
	            dest.data := n.data
	            dest.info := n.info
	            n.info::reg.referenced := 1
	         ELSEIF n.data = IT_LABEL
	            var := n.info.ident
	            IF var = NIL THEN reportErr('unknown identifier', n.info.name)
	            IF var.identID <> IDENT_VARIABLE THEN reportErr('operand syntax', n.info.name)
	            dest.data := IT_VARIABLE
	            dest.info := var
	            var.usage := 1
	            var.trok := FALSE
	         ELSE
	            reportErr('register expected')
	         ENDIF
	         dest++
	         n++
	         IF n.data <> ")" THEN reportErr('")" expected')
	         dest.data := n.data
	         dest++
	         n++
	      ELSE
	         reportErr('operand syntax')
	      ENDIF
	   CASE IT_VALUE
	      n, dest:= exp_constexp(n, dest)
	      IF n.data = "("
	         dest.data := n.data
	         dest++
	         n++
	         IF n.data = IT_REG
	            dest.data := n.data
	            dest.info := n.info
	            n.info::reg.referenced := 1
	         ELSE
	            reportErr('register expected')
	         ENDIF

	         dest++
	         n++
	         IF n.data = ")"
	            dest.data := n.data
	            dest++
	            n++
	         ELSEIF n.data = ","
	            dest.data := n.data
	            dest++
	            n++
	            IF n.data = IT_REG
	               dest.data := n.data
	               dest.info := n.info
	               n.info::reg.referenced := 1
	            ELSEIF n.data = IT_LABEL
	               var := n.info.ident
	               IF var = NIL THEN reportErr('unknown identifier', n.info.name)
	               IF var.identID <> IDENT_VARIABLE THEN reportErr('operand syntax', n.info.name)
	               dest.data := IT_VARIABLE
	               dest.info := var
	               var.usage := 1
	               var.trok := FALSE
	            ELSE
	               reportErr('register expected')
	            ENDIF
	            dest++
	            n++
	            IF n.data = "*"
	               n++
	               IF n.data <> IT_VALUE THEN reportErr('value expected')
	               dest.data := n.data
	               dest++
	               n++
	            ENDIF
	            IF n.data <> ")" THEN reportErr('")" expected')
	            dest.data := n.data
	            dest++
	            n++
	         ELSE
	            reportErr('operand syntax')
	         ENDIF
	      ELSE
	         reportErr('operand syntax')
	      ENDIF
	   CASE IT_LABEL
	      var := n.info.ident
	      IF var
	         IF var.identID = IDENT_VARIABLE
	            dest.data := IT_VARIABLE
	            dest.info := var
	            n++
	            IF n.data = "." -> 2.2
	               n++
	               IF n.data <> IT_LABEL THEN reportErr('variable size syntax')
	               hln := n.info
	               n++
	               IF StrCmp(hln.name, 'L')
	                  dest.num := 4
	               ELSEIF StrCmp(hln.name, 'W')
	                  dest.num := 2
	               ELSEIF StrCmp(hln.name, 'B')
	                  dest.num := 1
	               ELSEIF StrCmp(hln.name, 'S')
	                  dest.num := 4
	               ELSEIF StrCmp(hln.name, 'D')
	                  dest.num := 8
	               ELSE
	                  reportErr('variable size syntax')
	               ENDIF
	            ELSE
	               dest.num := NIL
	            ENDIF
	            var.usage := 1
	            var.trok := FALSE
	            dest++
	         ELSEIF var.identID = IDENT_LABEL  -> backward proc/label
	            dest.data := IT_LABEL
	            dest.info := n.info
	            dest++
	            n++
	            IF n.data = "("  -> xxx(PC)
	               n++
	               IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	               hln := n.info
	               IF StrCmp(hln.name, 'PC') = FALSE THEN reportErr('operand syntax')
	               n++
	               IF n.data <> ")" THEN reportErr('")" expected')
	               n++
	            ENDIF
	         ELSE
	            reportIErr(' g_ops68k, it_label ?', n.info.name)
	         ENDIF
	      ELSE -> forward proc / label
	         dest.data := IT_LABEL
	         dest.info := n.info
	         dest++
	         n++
	         IF n.data = "("  -> xxx(PC)
	            n++
	            IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	            hln := n.info
	            IF StrCmp(hln.name, 'PC') = FALSE THEN reportErr('operand syntax')
	            n++
	            IF n.data <> ")" THEN reportErr('")" expected')
	            n++
	         ENDIF
	      ENDIF
	   CASE "." -> 1.6.0, forgotten before
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	      hln := n.info
	      membname := hln.name -> save for later
	      n++
	      IF n.data = "("
	         n++
	         IF n.data = IT_REG
	            regitem := n++ -> save for later
	            IF n.data <> ":" THEN reportErr('":" expected')
	            n++
	            IF n.data <> IT_LABEL THEN reportErr('object name expected')
	            hln := n.info
	            n++
	            obj := hln.ident2
	            IF obj = NIL THEN reportErr('unknown object', hln.name)
	            IF obj.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	            memb := findMember(obj, membname)
	            IF memb = NIL THEN reportErr('unknown member', membname)
	            -> now make it into offset(reg)
	            dest.data := IT_VALUE
	            dest.info := memb.offset
	            dest.num := 0
	            dest++
	            dest.data := "("
	            dest++
	            dest.data := regitem.data
	            dest.info := regitem.info
	            dest.num := regitem.num
	            dest++
	            dest.data := ")"
	            dest++
	            IF n.data <> ")" THEN reportErr('")" expected')
	            n++
	         ELSE
	            reportErr('address register expected')
	         ENDIF
	      ELSE  -> 2.2
	         -> local label
	         loclab := getLocalLabel(hln.name)
	         hln := loclab.hln
	         dest.data := IT_LABEL
	         dest.info := hln
	         dest++
	      ENDIF
	   DEFAULT
	      reportErr('operand syntax')
	   ENDSELECT
	   IF n.data = ","
	      dest.data := ","
	      dest++
	      n++
	   ELSE
	      RETURN n, dest
	   ENDIF
  ENDWHILE


ENDPROC n, dest

PROC parse68kAsm(n:PTR TO item, dest:PTR TO item)
	DEF d:REG, hln:PTR TO hln, var:PTR TO var, t
	DEF reg:PTR TO reg, mask, lfunc:PTR TO lfunc
	DEF memb:PTR TO member, obj:PTR TO object, regitem:PTR TO item, membname, objname
	DEF loclab:PTR TO loclab, value, minus

	 #ifdef DBG_PASS1
	 DEBUGF('g_ops68k($\h, $\h)\n', n, dest)
	 #endif

	WHILE (d := n.data)
	   SELECT 256 OF d
	   CASE "#"
	      n++
	      n, value := getconstexp(n)
	      IF n = NIL THEN reportErr('constant expression expected')
	      dest.data := "#"
	      dest++
	      dest.data := IT_VALUE
	      dest.info := value
	      dest++
	   CASE IT_REG
	      t := n[1].data
	      IF (t = "/") OR (t = "-") -> registerlist ?
	         -> handle registerlist and pass it as "#"+IT_VALUE (!)
	         mask := NIL
	         REPEAT
	            IF n.data <> IT_REG THEN reportErr('register expected')
	            reg := n.info
	            reg.referenced := 1
	            n++
	            t := n.data
	            IF reg.type = DRX
	               mask := mask OR Shl(1, 15-reg.num)
	               IF t = "-"
	                  t := reg.num
	                  IF n[1].data <> IT_REG THEN reportErr('register expected')
	                  reg := n[1].info
	                  reg.referenced := 1
	                  IF reg.type <> DRX THEN reportErr('wrong type of register')
	                  WHILE t < (reg.num-1)
	                     t++
	                     mask := mask OR Shl(1, 15-t)
	                  ENDWHILE
	               ENDIF
	            ELSEIF reg.type = ARX
	               mask := mask OR Shl(1, 7-reg.num)
	               IF t = "-"
	                  t := reg.num
	                  IF n[1].data <> IT_REG THEN reportErr('register expected')
	                  reg := n[1].info
	                  reg.referenced := 1
	                  IF reg.type <> ARX THEN reportErr('wrong type of register')
	                  WHILE t < (reg.num-1)
	                     t++
	                     mask := mask OR Shl(1, 7-t)
	                  ENDWHILE
	               ENDIF
	            ELSE
	               reportErr('wrong type of register')
	            ENDIF
	            t := n.data
	            IF (t = "/") OR (t = "-") THEN n++
	         UNTIL (t <> "/") AND (t <> "-")
	         dest.data := "#"
	         dest++
	         dest.data := IT_VALUE
	         dest.info := mask
	         dest++
	      ELSEIF t = ":" -> Dh:Dl ?
	         -> lets stuff data regnums into "immediate"
	         reg := n.info
	         IF reg.type <> DRX THEN reportErr('wrong type of register')
	         n := n[2]
	         dest.data := "#"
	         dest++
	         dest.data := IT_VALUE
	         dest.info := Shl(reg.num, 3)
	         IF n.data <> IT_REG THEN reportErr('register expected')
	         reg := n.info
	         reg.referenced := 1
	         IF reg.type <> DRX THEN reportErr('wrong type of register')
	         dest.info := dest.info OR reg.num
	         dest++
	         n++
	      ELSE
	         dest.data := IT_REG
	         dest.info := n.info
	         n.info::reg.referenced := 1
	         dest++
	         n++
	      ENDIF
	   CASE "." -> 1.6.0, forgotten before
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	      hln := n.info
	      membname := hln.name -> save for later
	      n++
	      IF n.data = "("
	         n++
	         IF n.data = IT_REG
	            reg := n.info
	            reg.referenced := 1
	            IF reg.type <> ARX THEN reportErr('address register expected')
	            n++
	            IF n.data <> ":" THEN reportErr('":" expected')
	            n++
	            IF n.data <> IT_LABEL THEN reportErr('object name expected')
	            hln := n.info
	            n++
	            obj := hln.ident2
	            IF obj = NIL THEN reportErr('unknown object', hln.name)
	            IF obj.identID <> IDENT2_OBJECT THEN reportErr('object expected', hln.name)
	            memb := findMember(obj, membname)
	            IF memb = NIL THEN reportErr('unknown member', membname)
	            -> now make it into offset(reg)
	            dest.data := IT_VALUE
	            dest.info := memb.offset
	            dest.num := 0
	            dest++
	            dest.data := "("
	            dest++
	            dest.data := IT_REG
	            dest.info := reg
	            dest++
	            dest.data := ")"
	            dest++
	            IF n.data <> ")" THEN reportErr('")" expected')
	            n++
	         ELSE
	            reportErr('address register expected')
	         ENDIF
	      ELSE  -> 2.2
	         -> local label
	         loclab := getLocalLabel(hln.name)
	         hln := loclab.hln
	         dest.data := IT_LABEL
	         dest.info := hln
	         dest++
	      ENDIF
	   CASE IT_LABEL
	      var := n.info.ident
	      IF var
	         IF var.identID = IDENT_VARIABLE
	            dest.data := IT_VARIABLE
	            dest.info := var
	            n++
	            IF n.data = "." -> 2.2
	               n++
	               IF n.data <> IT_LABEL THEN reportErr('variable size syntax')
	               hln := n.info
	               n++
	               IF StrCmp(hln.name, 'L')
	                  dest.num := 4
	               ELSEIF StrCmp(hln.name, 'W')
	                  dest.num := 2
	               ELSEIF StrCmp(hln.name, 'B')
	                  dest.num := 1
	               ELSEIF StrCmp(hln.name, 'S')
	                  dest.num := 4
	               ELSEIF StrCmp(hln.name, 'D')
	                  dest.num := 8
	               ELSE
	                  reportErr('variable size syntax')
	               ENDIF
	            ELSE
	               dest.num := NIL
	            ENDIF
	            var.usage := 1
	            var.trok := FALSE
	            dest++
	         ELSEIF var.identID = IDENT_LABEL  -> backward proc/label
	            dest.data := IT_LABEL
	            dest.info := n.info
	            dest++
	            n++
	            IF n.data = "("  -> xxx(PC)
	               n++
	               IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	               hln := n.info
	               IF StrCmp(hln.name, 'PC') = FALSE THEN reportErr('operand syntax')
	               n++
	               IF n.data <> ")" THEN reportErr('")" expected')
	               n++
	            ENDIF
	         ELSE
	            reportIErr(' g_ops68k, it_label ?', n.info.name)
	         ENDIF
	      ELSE -> forward proc / label
	         dest.data := IT_LABEL
	         dest.info := n.info
	         dest++
	         n++
	         IF n.data = "("  -> xxx(PC)
	            n++
	            IF n.data <> IT_LABEL THEN reportErr('operand syntax')
	            hln := n.info
	            IF StrCmp(hln.name, 'PC') = FALSE THEN reportErr('operand syntax')
	            n++
	            IF n.data <> ")" THEN reportErr('")" expected')
	            n++
	         ENDIF
	      ENDIF
	   DEFAULT
	      minus := FALSE
	      value := NIL
	      IF d = "-"
	         minus := TRUE
	         n++
	         d := n.data
	      ENDIF
	      IF d = IT_VALUE
	         value := n.info
	         n++
	         d := n.data
	      ELSEIF d = IT_LFUNC
	         lfunc := n.info
	         value := lfunc.baseofs
	         n++
	         d := n.data
	      ENDIF
	      IF d <> "(" THEN reportErr('"(" expected')
	      n++
	      IF n.data <> IT_REG THEN reportErr('address register expected')
	      reg := n.info
	      reg.referenced := 1
	      IF reg.type <> ARX THEN reportErr('address register expected')
	      n++
	      d := n.data
	      SELECT d
	      CASE ")" -> [-]ofs(Ax) (Ax) -(Ax) (Ax)+
	         n++
	         IF value
	            IF minus
	               value := -value
	            ENDIF
	            dest.data := IT_VALUE
	            dest.info := value
	            dest++
	            dest.data := "("
	            dest++
	            dest.data := IT_REG
	            dest.info := reg
	            dest++
	            dest.data := ")"
	            dest++
	         ELSEIF minus
	            dest.data := "-"
	            dest++
	            dest.data := "("
	            dest++
	            dest.data := IT_REG
	            dest.info := reg
	            dest++
	            dest.data := ")"
	            dest++
	         ELSE
	            dest.data := "("
	            dest++
	            dest.data := IT_REG
	            dest.info := reg
	            dest++
	            dest.data := ")"
	            dest++
	            IF n.data = "+"
	               dest.data := "+"
	               dest++
	               n++
	            ENDIF
	         ENDIF
	      CASE "," -> [-]ofs(Ax,Dx[.L][*scale])
	         n++
	         IF minus THEN value := -value
	         dest.data := IT_VALUE
	         dest.info := value
	         dest++
	         dest.data := "("
	         dest++
	         dest.data := IT_REG
	         dest.info := reg
	         dest++
	         IF n.data <> IT_REG THEN reportErr('data register expected')
	         reg := n.info
	         reg.referenced := 1
	         dest.data := IT_REG
	         dest.info := reg
	         dest.num := n.num -> possible .L
	         dest++
	         IF reg.type <> DRX THEN reportErr('data register expected')
	         n++
	         IF n.data = "*"
	            n++
	            IF n.data <> IT_VALUE THEN reportErr('scale value expected')
	            value := n.info
	            SELECT 9 OF value
	            CASE 1,2,4,8
	            DEFAULT
	               reportErr('ilegal scale value')
	            ENDSELECT
	            dest.data := IT_VALUE
	            dest.info := value
	            dest++
	            n++
	         ENDIF
	         IF n.data <> ")" THEN reportErr('")" expected')
	         n++
	         dest.data := ")"
	         dest++
	      DEFAULT
	         reportErr('operand syntax')
	      ENDSELECT

	   ENDSELECT
	   IF n.data = ","
	      dest.data := ","
	      dest++
	      n++
	   ELSE
	      RETURN n, dest
	   ENDIF
  ENDWHILE


ENDPROC n, dest

PROC parsePPCAsm(n:PTR TO item, dest:PTR TO item)
	DEF d:REG, hln:PTR TO hln, ident:PTR TO ident
	DEF var:PTR TO var, lfunc:PTR TO lfunc
	DEF loclab:PTR TO loclab

	 #ifdef DBG_PASS1
	 DEBUGF('g_opsppc($\h, $\h)\n', n, dest)
	 #endif

	WHILE (d := n.data)
	   SELECT 256 OF d
	   CASE IT_REG
	      dest.data := n.data
	      dest.info := n.info
	      n.info::reg.referenced := 1
	      dest++
	      n++
	   CASE "."
	      n++
	      IF n.data <> IT_LABEL THEN reportErr('member expected')
	      hln := n.info
	      n++
	      IF n.data = "("
	         dest.data := "."
	         dest++
	         dest.data := IT_LABEL
	         dest.info := hln
	         dest++
	         dest.data := "("
	         dest++
	         n++
	         IF n.data = IT_REG
	            dest.data := n.data
	            dest.info := n.info
	            n.info::reg.referenced := 1
	         ELSEIF n.data = IT_LABEL
	            var := n.info.ident
	            IF var = NIL THEN reportErr('unknown identifier', n.info.name)
	            IF var.identID <> IDENT_VARIABLE THEN reportErr('operand syntax', n.info.name)
	            dest.data := IT_VARIABLE
	            dest.info := var
	            var.usage := 1
	            var.trok := FALSE
	         ELSE
	            reportErr('register expected')
	         ENDIF
	         dest++
	         n++
	         IF n.data = ":"
	            dest.data := ":"
	            dest.info := n.info
	            dest++
	            n++
	            IF n.data <> IT_LABEL THEN reportErr('object expected')
	            hln := n.info
	            ident := hln.ident2
	            IF ident = NIL THEN reportErr('unknown identifier', hln.name)
	            dest.data := IT_OBJECT
	            dest.info := ident
	            dest++
	            n++
	         ENDIF
	         IF n.data <> ")" THEN reportErr('")" expected')
	         dest.data := ")"
	         dest++
	         n++
	      ELSE
	         -> local label
	         loclab := getLocalLabel(hln.name)
	         hln := loclab.hln
	         dest.data := IT_LABEL
	         dest.info := hln
	         dest++
	      ENDIF
	   CASE IT_VALUE, "-", IT_LFUNC
	      IF d = IT_LFUNC
	         lfunc := n.info
	         dest.data := IT_VALUE
	         dest.info := lfunc.baseofs
	         dest++
	         n++
	      ELSE
	         n, dest:= exp_constexp(n, dest)
	      ENDIF
	      IF n.data = "("
	         dest.data := "("
	         dest++
	         n++
	         IF n.data = IT_REG
	            dest.data := n.data
	            dest.info := n.info
	            n.info::reg.referenced := 1
	         ELSEIF n.data = IT_LABEL
	            var := n.info.ident
	            IF var = NIL THEN reportErr('unknown identifier', n.info.name)
	            IF var.identID <> IDENT_VARIABLE THEN reportErr('operand syntax', n.info.name)
	            dest.data := IT_VARIABLE
	            dest.info := var
	            var.usage := 1
	            var.trok := FALSE
	         ELSE
	            reportErr('register expected')
	         ENDIF
	         dest++
	         n++
	         IF n.data <> ")" THEN reportErr('")" expected')
	         dest.data := ")"
	         dest++
	         n++
	      ENDIF
	   CASE IT_LABEL
	      hln := n.info
	      ident := hln.ident
	      IF ident
	         IF ident.identID = IDENT_VARIABLE
	            dest.data := IT_VARIABLE
	            dest.info := ident
	            ident::var.usage := 1
	            ident::var.trok := FALSE
	            dest++
	            n++
	         ELSEIF ident.identID = IDENT_LABEL  -> backward proc/label
	            dest.data := IT_LABEL
	            dest.info := hln
	            dest++
	            n++
	         ELSE
	            reportIErr(' ppc.parseAsm, it_label ?', hln.name)
	         ENDIF
	      ELSE
	         dest.data := IT_LABEL
	         dest.info := hln
	         dest++
	         n++
	      ENDIF
	   DEFAULT
	      reportErr('operand syntax')
	   ENDSELECT
	   IF n.data = ","
	      dest.data := ","
	      dest++
	      n++
	   ELSE
	      RETURN n, dest
	   ENDIF
  ENDWHILE


ENDPROC n, dest

PROC finnishObjects()
	DEF o:PTR TO object, str[256]:STRING
	DEF t, meth:PTR TO proc
	DEF hln:PTR TO hln, proc:PTR TO proc
	DEF nrofmethods=0, a
	DEF memb:PTR TO member, tempo, lab:PTR TO codelab
	DEF nrofclassmethods=0

	#ifdef DBG_MAIN
	DEBUGF('finnishObjects()\n')
	#endif

	-> reverse back the order of objects (important) v45
	o := g_objectlist
	g_objectlist := NIL
	WHILE o
	   t := o.next
	   o.next := g_objectlist
	   g_objectlist := o
	   o := t
	ENDWHILE

	o := g_objectlist
	WHILE o

	   setLinenum(o.startline)

	   IF o.membertable = NIL THEN reportErr('missing "ENDOBJECT"', o.name)

	   /* backpatch TYPE_OPTRs */
	   FOR a := 0 TO o.nrofmembers-1
	      memb := o.membertable[a]
	      IF memb.size = 255 -> unresolved optr
	         hln := memb.object
	         tempo := hln.ident2
	         IF tempo = NIL THEN reportErr('unknown object', hln.name)
	         memb.size := 4
	         memb.esize := 255 -> OPTR
	         memb.object := tempo
	      ENDIF
	   ENDFOR

	   /* inherit methods from superclass */
	   IF o.super
	      nrofmethods := o.super.nrofmethods
	      IF nrofmethods
	         #ifdef DBG_OOP
	         DEBUGF('finnishObjects(): object "\s" is inheriting \d methods from "\s"\n',
	         o.name, nrofmethods, o.super.name)
	         #endif
	         CopyMemQuick(o.super.methodtable, g_temparray, nrofmethods * 4)
	         IF g_optmodule
	            IF o.super.startline = NIL  -> between modules ?
	               o.super.labrefs := NEW [o.super.labrefs,o.name,REF_INHERIT]:LONG -> V47
	            ENDIF
	         ENDIF
	      ENDIF
	      o.destofs := o.super.destofs  -> inherit destructor offset (if any)
	      o.flags := o.super.flags -> 1.8.0, inherit flags
	   ELSE
	      o.destofs := -1
	      nrofmethods := 0 -> 1,7,1 FIX
	      o.flags := OFLAG_NOCLASSINFO -> 1.8.1
	   ENDIF

	   o.classofs := OBJECT_CLASSINFO  -> 2.0 set it hard!


	   proc := o.addedmethods
	   WHILE proc
	      IF (proc.flags AND PROCF_CLMETH) = NIL THEN o.flags := o.flags AND Not(OFLAG_NOCLASSINFO) -> 1.8.1
	      -> does method exist already ?
	      a := nrofmethods
	      WHILE a > 0
	         a--
	         meth := g_temparray[a]
	         IF meth.name = proc.name -> same name ?
	            IF meth.object = o -> same object ?
	               reportErr('double declaration of method', proc.name)
	            ENDIF
	            IF meth.nrofargs <> proc.nrofargs THEN reportErr('arguments doesn\at match for redefinition of method', proc.name) -> V47
	            IF (meth.flags AND PROCF_CLMETH) <> (proc.flags AND PROCF_CLMETH) THEN reportErr('redefined method is of different type', proc.name) -> v53
	            -> okey, lets replace old method with new..
	            g_temparray[a] := proc
	            a := -1 -> ends loop and tells about the replace
	            #ifdef DBG_OOP
	            DEBUGF('finnishObjects(): redefined method "\s" \n', proc.name)
	            #endif
	         ENDIF
	      ENDWHILE
	      IF a = 0 -> if no redefine
	         IF StrCmp(proc.name, 'end')      -> v45
	            o.destofs := nrofmethods*4
	         ENDIF
	         g_temparray[nrofmethods++] := proc -> add new method
	         #ifdef DBG_OOP
	         DEBUGF('finnishObjects(): added method "\s" to object "\s"\n', proc.name, o.name)
	         #endif
	      ENDIF
	      proc := proc.codelink
	   ENDWHILE


	   -> lets make a permanent copy of method array..
	   NEW o.methodtable[nrofmethods+1]
	   IF nrofmethods
	      CopyMemQuick(g_temparray, o.methodtable, nrofmethods * 4)
	      o.codelink := g_codelablist
	      g_codelablist := o
	   ENDIF
	   o.nrofmethods := nrofmethods

	   o := o.next
	ENDWHILE

	#ifdef DBG_MAIN
	DEBUGF('finnishObjects() DONE\n')
	#endif


ENDPROC



-> V47
PROC fixExtMethsOffsets()
	DEF o:PTR TO object
	DEF mod:PTR TO moduleheader
	#ifdef DBG_EXE
	DEBUGF('fixExtMEthsOffsets()\n')
	#endif
	 /* fix external method-offsets. run resolveExtClassesSuper before this. */
	mod := g_modulelist.head
	WHILE mod.succ
	   o := mod.modextens.codelist
	   WHILE o
	      IF o.identID = IDENT2_OBJECT
	         IF o.super THEN inheritOffsets(o, o.super)
	      ENDIF
	      o := o.codelink
	   ENDWHILE
	   mod := mod.succ
	ENDWHILE
ENDPROC

-> recursively inherit method-offsets for an object
PROC inheritOffsets(o:PTR TO object, o2:PTR TO object)  -> V47
	DEF a, m1:PTR TO proc, m2:PTR TO proc
	 #ifdef DBG_EXE
	 DEBUGF('inheritOffsets: \s OF \s\n', o.name, o2.name)
	 #endif
	IF o2.super THEN inheritOffsets(o2, o2.super)
	FOR a := 0 TO o2.nrofmethods-1
	   m1 := o.methodtable[a]
	   m2 := o2.methodtable[a]
	   m1.offset := m2.offset
	ENDFOR
ENDPROC

PROC patchMethods()
	DEF mod:PTR TO moduleheader
	DEF object1:PTR TO object
	DEF object2:PTR TO object
	DEF lptr:PTR TO LONG, num, meth:PTR TO proc
	DEF code, codeofs, info:PTR TO modinfo

	#ifdef DBG_EXE
	DEBUGF('patchMethods()\n')
	#endif

	mod := g_modulelist.head
	WHILE mod.succ
	   info := mod.codeinfo
	   IF info
	      info := info + mod
	      code := info + SIZEOF modinfo
	      codeofs := info.misc
	      object1 := mod.modextens.codelist
	      WHILE object1
	         IF object1.identID = IDENT2_OBJECT
	            IF object1.super
	               object2 := object1.super
	#ifdef DBG_OOP
	DEBUGF(' OBJECT \s @ $\h from module \s inherits OBJECT \s \n',
	object1.name, object1.offset, mod.mname, object2.name)
	#endif
	               inheritMethods(object1, object2) -> 1.10.0
	            ENDIF
	         ENDIF
	         object1 := object1.codelink
	      ENDWHILE
	   ENDIF
	   mod := mod.succ
	ENDWHILE

ENDPROC

-> 1.10.0
PROC inheritMethods(object1:PTR TO object, object2:PTR TO object)
	DEF num, meth:PTR TO proc, lptr:PTR TO LONG
	-> lets inherit
	lptr := object2.methodtable
	FOR num := 0 TO object2.nrofmethods-1
	   meth := lptr[num]
	   #ifdef DBG_OOP
	   DEBUGF('   .\s () @ $\h\n', meth.name, meth.offset)
	   #endif
	   IF object1.modmethtab[num] = NIL -> not redefined ?
	      #ifdef DBG_OOP
	      DEBUGF('     ..was not redefined so patch it in!\n')
	      #endif
	      object1.modmethtab[num] := meth.offset
	      addReloc(object1.offset + (num*4))
	   ENDIF
	ENDFOR
ENDPROC



PROC findModule(list:PTR TO mlh, mname)
	DEF n:REG PTR TO moduleheader, len
	#ifdef DBG_BMOD
	DEBUGF('findModule($\h, "\s"\n', list, mname)
	#endif
	len := StrLen(mname)
	n := list.head
	WHILE n.succ
	   IF n.mnamelen = len
	      IF StrCmp(n.mname, mname) THEN RETURN n
	   ENDIF
	   n := n.succ
	ENDWHILE
ENDPROC NIL

PROC findModuleFile(list:PTR TO mlh, mname)
	DEF n:REG PTR TO moduleheader, t
	#ifdef DBG_BMOD
	DEBUGF('findModuleFile($\h, "\s"\n', list, mname)
	#endif
	n := list.head
	WHILE n.succ
	   IF StrCmp(FilePart(n.mname), mname) THEN RETURN n
	   n := n.succ
	ENDWHILE
ENDPROC NIL

-> v58: extracted code from readModule() to have
-> own procedure for looking up in cache
PROC findCachedModule(mname)
	DEF cmh:PTR TO moduleheader, mh:PTR TO moduleheader
	IF g_modulecache
	   IF g_modulecache.ident <> "MC" THEN RETURN NIL, "MCC" -> 1.8.0
	   cmh := findModule(g_modulecache.modlist, mname)
	   IF cmh
	      IF cmh.identification = "ECXM" -> 1.8.0
	         IF checksum(cmh) THEN RETURN NIL, "SUM"
	         IF cmh.dummymod THEN RETURN NIL, "DUM" ->
	         Remove(cmh)                         -> 1.5.6 move used mod to top
	         AddHead(g_modulecache.modlist, cmh) ->
	         mh := cloneModule(cmh)
	         IF mh = NIL THEN RETURN NIL, "MEM"
	         #ifdef DBG_BMOD
	         DEBUGF(' cached module found ok\n')
	         #endif
	         RETURN mh -> all okey
	      ELSE -> 1.8.0
	         #ifdef DBG_BMOD
	         DEBUGF(' cached module failed identification!\n')
	         #endif
	         RETURN NIL, "IDNT"
	      ENDIF
	   ENDIF
	ENDIF
ENDPROC NIL, NIL

-> major cleanup 1.8.0
-> v58: removed cache lookup
PROC readModule(mname) HANDLE
	DEF mh:PTR TO moduleheader, cmh:PTR TO moduleheader
	DEF fh=NIL, fbuf=NIL, flen, fib:fileinfoblock
	DEF err, amh=NIL:PTR TO moduleheader

	#ifdef DBG_BMOD
	DEBUGF('readModule(\a\s\a)\n', mname)
	#endif

	fh := Open(mname, OLDFILE)
	IF fh = NIL
	   IF g_modulecache
         -> add dummy mod to cahe that signifies non existing file
         cmh := findCachedModule(mname)
         IF cmh = NIL
            cmh := ALLOCMEM(SIZEOF moduleheader, MEMF_CLEAR)
            cmh.dummymod := TRUE
            cmh.identification := "ECXM"
            cmh.headsize := SIZEOF moduleheader
            cmh.modsize := SIZEOF moduleheader
            setModuleName(cmh, mname, TRUE)
            setsum(cmh)
            AddHead(g_modulecache.modlist, cmh)
         ENDIF
	   ENDIF
	   Raise("OPEN")
	ENDIF

	IF ExamineFH(fh, fib) = NIL THEN Raise("EXAM")
	flen := fib.size
	IF g_modulecache -> holding a lock on the semaphore ?
	   #ifdef DBG_BMOD
	   DEBUGF(' alloc,read, (trans) and place in cache\n')
	   #endif
	   amh := ALLOCMEM(flen, NIL)
	   IF amh = NIL THEN Raise("MEM")
	   IF Read(fh, amh, flen) <> flen THEN Raise("READ")
	   Close(fh) ; fh := NIL
	   IF Long(amh) = "EMOD"
	      mh := amh
	      amh, err := ecmod2ecxmod(mh, flen)
	      FREEMEM(mh, flen)
	      IF amh = NIL THEN Raise(err)
	      setsum(amh)
	      flen := amh.modsize ->for freeing on error
	   ELSEIF amh.identification = "ECXM"
	      IF amh.modsize <> flen THEN
	       Raise("MODS")
	      IF checksum(amh) THEN Raise("SUM")
	   ELSE
	      Raise("IDNT")
	   ENDIF

	   IF setModuleName(amh, mname, TRUE) = NIL THEN Raise("MEM")

	   AddHead(g_modulecache.modlist, amh) -> add filemodule as on single block
	   mh := amh
	   amh := NIL
	   mh := cloneModule(mh)
	   IF mh = NIL THEN Raise("MEM")
	ELSE
	   #ifdef DBG_BMOD
	   DEBUGF(' alloc,read, (trans) without placing in cache\n')
	   #endif
	   fbuf := NEWMEMR(flen)
	   IF Read(fh, fbuf, flen) <> flen THEN Raise("READ")
	   Close(fh) ; fh := NIL
	   mh := fbuf
	   IF Long(fbuf) = "EMOD"
	      mh, err := ecmod2ecxmod(fbuf, flen)
	      DISPOSE(fbuf) ; fbuf := NIL
	      IF mh = NIL THEN Raise(err)
	      setsum(mh)
	   ELSEIF mh.identification <> "ECXM"
	      Raise("IDNT")
	   ELSEIF mh.modsize <> flen -> 1.6.1
	      Raise("MODS")
	   ENDIF

	   IF setModuleName(mh, mname, FALSE) = NIL THEN Raise("MEM")

	ENDIF

EXCEPT
debugme:

	#ifdef DBG_BMOD
	DEBUGF(' readModule error \s\n', [exception,0])
	#endif

	IF fh THEN Close(fh)
	->IF fbuf THEN DISPOSE(fbuf)
	IF amh THEN FREEMEM(amh, flen)

	RETURN NIL, exception

ENDPROC mh

-> clone a cached module
PROC cloneModule(m:PTR TO moduleheader)
	DEF nm:PTR TO moduleheader

	nm := NEWMEM(m.modsize)
	IF nm = NIL THEN RETURN NIL
	CopyMemQuick(m, nm, m.modsize)

ENDPROC nm

-> free a cached module
PROC freeModule(m:PTR TO moduleheader)
	FREEMEM(m.mname, m.mnamelen+1)
	FREEMEM(m, m.modsize)
ENDPROC



PROC setModuleName(m:PTR TO moduleheader, name, cache)
	m.mnamelen := StrLen(name)
	m.mname := IF cache THEN ALLOCMEM(m.mnamelen+1,NIL) ELSE NEWMEM(m.mnamelen+1)
	IF m.mname = NIL THEN RETURN NIL
	AstrCopy(m.mname, name)
ENDPROC m.mname



PROC checkUnrefs()
	DEF p:PTR TO proc, v:PTR TO var, g:PTR TO gvar, a, b
	DEF o:PTR TO object

	IF g_nowarn THEN RETURN

	/* procedures, labels, args, locals */
	p := g_proclist
	WHILE p
	   IF g_optmodule
	      IF p.exported = FALSE
	         IF p.referenced = FALSE THEN addUnref(p.name)
	      ENDIF
	   ELSE
	      IF p.referenced = FALSE THEN addUnref(p.name)
	   ENDIF
	   IF p.ltype = LTYPE_PROC
	      v := p.varchain
	      WHILE v
	         IF v.usage = FALSE THEN addUnref(v.hln.name)
	         v := v.varchain
	      ENDWHILE
	   ENDIF
	   p := p.next
	ENDWHILE

	/* args and locals of methods */
	p := g_methodlist
	WHILE p
	   v := p.varchain
	   WHILE v
	      IF v.usage = FALSE THEN addUnref(v.hln.name)
	      v := v.varchain
	   ENDWHILE
	   p := p.next
	ENDWHILE

	/* globals */
	g := g_gvarlist
	WHILE g
	   -> is it not used ?
	   IF g.usage = 0
	      -> changed 1.10.0
	         IF g.gtype <> GTYPE_INTERNAL
	            IF g_optmodule
	               IF g.gtype <> GTYPE_XREF THEN addUnref(g.hln.name)
	            ELSE
	               addUnref(g.hln.name)
	            ENDIF
	         ENDIF
	   ENDIF
	   g := g.next
	ENDWHILE


ENDPROC

PROC printUnrefs()
	DEF ur:PTR TO unref, str[100]:STRING
	ur := g_unrefs
	IF ur = NIL THEN RETURN
	WriteF('Unreferenced: ')
	WHILE ur
	   StringF(str, '*\d', ur.count)
	   WriteF('\s\s\c', ur.name, IF ur.count > 1 THEN str ELSE '', IF ur.next THEN "," ELSE " ")
	   ur := ur.next
	ENDWHILE
	WriteF('\n')
ENDPROC

PROC addUnref(name)
	DEF ur:PTR TO unref

	ur := g_unrefs
	WHILE ur
	   IF ur.name = name
	      ur.count := ur.count + 1
	      RETURN
	   ENDIF
	   ur := ur.next
	ENDWHILE
	NEW ur
	ur.name := name
	ur.next := g_unrefs
	ur.count := 1
	g_unrefs := ur
ENDPROC


PROC printWarnings()
	DEF warn:PTR TO warn, str[10]:STRING
	warn := g_warnlist
	WHILE warn
	   WriteF('Warning: \s\s\n', warn.message, IF warn.count > 1 THEN StringF(str, ' (x\d)', warn.count) ELSE '')
	   warn := warn.next
	ENDWHILE
ENDPROC

/* removed 1.10.0
-> v50
PROC addUndefglob(name)
	DEF len, udg:PTR TO undefglob

	IF g_nowarn THEN RETURN
	IF g_quiet THEN RETURN

	#ifdef DBG_EXE
	DEBUGF('addUndefglob("\s")\n', name)
	#endif

	len := StrLen(name)
	udg := g_undefgloblist
	WHILE udg
	   IF StrCmp(udg.name, name) THEN RETURN
	   udg := udg.next
	ENDWHILE
	g_undefgloblist := NEW [g_undefgloblist, StrCopy(String(len), name)]:undefglob
ENDPROC

PROC printUndefglobs()
	DEF udg:PTR TO undefglob
	udg := g_undefgloblist
	IF udg THEN WriteF('UNDEFINED EXPORTED GLOBALS: ')
	WHILE udg
	   WriteF('\s\s', udg.name, IF udg.next THEN ',' ELSE '\n')
	   udg := udg.next
	ENDWHILE
ENDPROC

*/


/*
PROC checksum(m:PTR TO moduleheader)
	DEF lptr, bytesize, sum

	lptr := m.checksumstart
	bytesize := m + m.modsize - lptr
	sum := m.checksum
	->IF sum = 0 THEN RETURN NIL -> no check for ecmodules

	MOVE.L bytesize, D0
	LSR.L #2, D0 -> longs
	SUBQ.L #1, D0 -> for DBRA
	MOVE.L lptr, A0
	MOVEQ.L #0, D1
_csum:
	ADD.L (A0)+, D1
	DBRA D0, _csum
	CMP.L sum, D1
	BNE cs_err
	RETURN NIL -> no error.
cs_err:
ENDPROC TRUE -> error.
*/
PROC checksum(m:PTR TO moduleheader)
	DEF lptr:REG PTR TO LONG, count:REG, sum=0

	#ifdef DBG_BMOD
	DEBUGF('checksum($\h)\n', m)
	#endif

	lptr := m.checksumstart
	count := Shr(m + m.modsize - lptr, 2) + 1

	WHILE count-- DO sum := sum + lptr[]++

	IF sum = m.checksum THEN RETURN NIL

ENDPROC TRUE -> error.

/*
PROC setsum(m:PTR TO moduleheader)
	DEF lptr, bytesize, sum

	lptr := m.checksumstart
	bytesize := m + m.modsize - lptr

	MOVE.L bytesize, D0
	LSR.L #2, D0 -> longs
	SUBQ.L #1, D0 -> for DBRA
	MOVE.L lptr, A0
	MOVEQ.L #0, D1
_sum:
	ADD.L (A0)+, D1
	DBRA D0, _sum
	MOVE.L D1, sum
	m.checksum := sum
ENDPROC
*/

PROC setsum(m:PTR TO moduleheader)
	DEF lptr:REG PTR TO LONG, count:REG, sum=0

	#ifdef DBG_BMOD
	DEBUGF('setsum($\h)\n', m)
	#endif

	lptr := m.checksumstart
	count := Shr(m + m.modsize - lptr, 2) + 1

	WHILE count-- DO sum := sum + lptr[]++

	m.checksum := sum

ENDPROC


/**************************************************************
***************************************************************
******************** EXECUTABLE CREATE ************************
***************************************************************
**************************************************************/


PROC linkMainIfuncs()
	DEF lptr:PTR TO LONG, num, labref:PTR TO labref, b, ifunc:PTR TO lif

  /* ifunc references from main */
	#ifdef DBG_EXE
	DEBUGF('resolveMainIfuncs()\n')
	#endif

	lptr := IF g_optpowerpc = CPU_PPC THEN g_internalfuncsppc ELSE g_internalfuncs
	num := 0
	WHILE (ifunc := lptr[num])
	   labref := ifunc.labrefs
	   IF labref
	      IF ifunc.offset = 0
	         putAlign(4)  -> 2.1
	         ifunc.offset := currentOffset()
	         CopyMem(ifunc.code, g_codeptr, ifunc.codelen)
	         g_codeptr := g_codeptr + ifunc.codelen
	      ENDIF
	      IF g_optpowerpc = CPU_PPC
	         WHILE labref
	            b := ifunc.offset - labref.offset
	            put24ofs(g_codebuf + labref.offset - link_codesize, b)
	            labref := labref.next
	         ENDWHILE
	      ELSE
	         WHILE labref
	            PutLong(g_codebuf + labref.offset - link_codesize, ifunc.offset - labref.offset)
	            labref := labref.next
	         ENDWHILE
	      ENDIF
	   ENDIF
	   num++
	ENDWHILE

ENDPROC

PROC linkModules()
	DEF codelab:PTR TO codelab
	DEF a, b, t
	DEF labref:REG PTR TO labref
	DEF ifunc:PTR TO lif
	DEF mod:PTR TO moduleheader
	DEF info:PTR TO modinfo
	DEF mm:PTR TO modmodule
	DEF submod:PTR TO moduleheader
	DEF rwref:PTR TO rwref
	DEF lptr:PTR TO LONG
	DEF str[256]:STRING
	DEF err
	DEF table, num, offset
	DEF count, modref:PTR TO modref
	DEF symbofs, refofs, code, codeofs, data, dataofs, hln:PTR TO hln, glob, globofs
	DEF gvar:PTR TO gvar
	DEF vdbg:PTR TO vardbg
	->DEF meth:PTR TO proc
	DEF object1:PTR TO object, object2:PTR TO object, modobj:PTR TO modobject, modclass:PTR TO modclass

	#ifdef DBG_EXE
	DEBUGF('linkModules()\n')
	#endif

	/* link references FROM modules */

	mod := g_modulelist.head
	WHILE mod.succ
	   IF mod.codeinfo = NIL

	      t := mod.succ
	      Remove(mod)
	      ->DISPOSE(mod)
	      mod := t

	   ELSE

	      info := mod.strtabinfo + mod
	      table := info + SIZEOF modinfo

	      #ifdef DBG_EXE
	      DEBUGF('linking MODULE \s.\n', mod.mname)
	      #endif

	      info := mod.codeinfo + mod
	      code := info + SIZEOF modinfo
	      codeofs := info.misc

	      info := mod.globinfo
	      IF info
	         info := info + mod
	         glob := info + SIZEOF modinfo
	         globofs := info.misc
	         #ifdef DBG_EXE
	         DEBUGF('   globofs \d\n', globofs)
	         #endif
	      ENDIF

	      info := mod.datainfo
	      IF info
	         info := info + mod
	         data := info + SIZEOF modinfo
	         dataofs := info.misc - (link_datasize + g_databufsize + IVARSSIZE)
	         #ifdef DBG_EXE
	         DEBUGF('   dataofofs \d\n', dataofs)
	         #endif
	      ENDIF


	      info := mod.ifuncinfo -> ifuncs
	      IF info
	         #ifdef DBG_EXE
	         DEBUGF('   ifunc refs\n')
	         #endif
	         info := info + mod
	         count := info.count+1
	         lptr := info + SIZEOF modinfo
	         IF info.misc = 0 -> 68k ?
	            WHILE count--
	               offset := getIfuncOffset(lptr[]++, 0)
	               num := 1 + (lptr[]++ AND $FFFFFF)
	               WHILE num-- -> traverse offsets to be linked in
	                  PutLong(code + lptr[], offset-lptr[]-codeofs)
	                  lptr++
	               ENDWHILE
	            ENDWHILE
	         ELSEIF info.misc = 1 -> ppc ?
	            WHILE count--
	               offset := getIfuncOffset(lptr[]++, 1)
	               num := 1 + (lptr[]++ AND $FFFFFF)
	               WHILE num-- -> traverse offsets to be linked in
	                  put24ofs(code + lptr[], offset-codeofs-lptr[])
	                  lptr++
	               ENDWHILE
	            ENDWHILE
	         ENDIF
	      ENDIF

	      info := mod.moduleinfo
	      IF info
	         #ifdef DBG_EXE
	         DEBUGF('   module refs\n')
	         #endif
	         info := info + mod
	         count := info.count+1
	         mm := info + SIZEOF modinfo
	         WHILE count--
	            num := mm.numsymbs+1
	            modref := mm + SIZEOF modmodule
	            WHILE num--
	               lptr := modref + SIZEOF modref
	               #ifdef DBG_EXE
	               submod := mm.name
	               DEBUGF('   \s from "\s" x\d \n', modref.name + table, submod.mname, modref.numrefs)
	               #endif
	               codelab := getModSymb(mm.name, modref.name + table)
	               symbofs := codelab.offset
	               a := modref.numrefs+1
	               WHILE a--
	                  t := Shr(lptr[], 24) AND $FF
	                  refofs := lptr[]++ AND $FFFFFF
	                  SELECT t
	                  CASE REF32
	                     PutLong(code + refofs, symbofs-(codeofs+refofs))
	                  CASE REF24
	                     put24ofs(code + refofs, symbofs-(codeofs+refofs))
	                  CASE REFADR
	                     PutLong(code + refofs, symbofs) ; addReloc(codeofs+refofs)
	                  CASE REF1616
	                     put1616ofs(code + refofs, symbofs-(codeofs+refofs))
	                  CASE REF_INHERIT  -> V47
	                     object1 := getModSymb(mod, refofs+table)
	                     object1.super := codelab
	                     #ifdef DBG_EXE
	                     DEBUGF('REF_INHERIT: \s OF \s\n', object1.name, codelab.name)
	                     #endif
	                  DEFAULT
	                     reportIErr(' modref t')
	                  ENDSELECT
	               ENDWHILE
	               modref := lptr
	            ENDWHILE
	            mm := mm + mm.totalsize
	         ENDWHILE
	      ENDIF

	      info := mod.xrefginfo
	      IF info
	         #ifdef DBG_EXE
	         DEBUGF('   global xrefs\n')
	         #endif
	         info := info + mod
	         count := info.count + 1
	         modref := info + SIZEOF modinfo
	         WHILE count--
	            hln := getLabelHLN(modref.name + table)
	            gvar := hln.ident
	            IF gvar = NIL  -> v50
	               reportIErr('linkmodules global not allocated', hln.name)
	            ENDIF
	            symbofs := gvar.offset
	            #ifdef DBG_EXE
	            DEBUGF('    symbol "\s" @ \d\n', hln.name, symbofs)
	            #endif
	            a := modref.numrefs + 1
	            lptr := modref + SIZEOF modref
	            WHILE a--
	               #ifdef DBG_EXE
	               DEBUGF('     moduleofs: $\h\n', lptr[])
	               #endif
	               PutInt(code+lptr[]++, symbofs)
	            ENDWHILE
	            modref := lptr
	         ENDWHILE
	      ENDIF


	      info := mod.globinfo -> globals reloc
	      IF info
	         #ifdef DBG_EXE
	         DEBUGF('   global relocs (globofs \d)\n', globofs)
	         #endif
	         info := info + mod
	         count := info.count+1
	         lptr := info + SIZEOF modinfo
	         WHILE count--
	            a := lptr[]++ + 1-> numrelocs
	            WHILE a--
	               t:=(code + lptr[]++)
	               #ifdef DBG_EXE
	               DEBUGF('     \d => \d\n', Int(t), Int(t) + globofs)
	               #endif
	               PutInt(t, Int(t) + globofs)
	            ENDWHILE
	         ENDWHILE
	      ENDIF

	      info := mod.dreldinfo -> rwdata reloc
	      IF info
	         #ifdef DBG_EXE
	         DEBUGF('   dreldata relocs\n')
	         #endif
	         info := info + mod
	         count := info.count+1
	         lptr := info + SIZEOF modinfo
	         IF mod.cpu = 0
	            WHILE count--
	               #ifdef DBG_EXE
	               DEBUGF('     moduleofs: $\h: rwd: \d => \d\n', lptr[], t := Long(code+lptr[]), t + dataofs)
	               #endif
	               PutLong(t:=(code + lptr[]++), Long(t) + dataofs)
	            ENDWHILE
	         ELSE -> powerpc
	            WHILE count--
	               t := fix1616code(code + lptr[]++, dataofs)
	               #ifdef DBG_EXE
	               DEBUGF('     moduleofs: $\h: rwd: \d => \d\n', lptr[-1], t-dataofs, t)
	               #endif
	            ENDWHILE
	         ENDIF
	      ENDIF

	      info := mod.relocinfo
	      IF info
	         info := info + mod
	         count := info.count + 1
	         lptr := info + SIZEOF modinfo
	         WHILE count--
	            t := lptr[]++
	            PutLong(code + t, Long(code + t) + codeofs)
	            addReloc(t+codeofs)
	         ENDWHILE
	      ENDIF


	      IF g_linedebug -> relocate line-offsets
	         info := mod.lineinfo
	         IF info
	            info := info + mod
	            lptr := info + SIZEOF modinfo
	            a := info.count+1
	            WHILE a--
	               lptr++ -> linenum
	               lptr[] := codeofs + lptr[]
	               lptr++
	            ENDWHILE
	         ENDIF
	      ENDIF

	      IF g_vardebug -> we may need to relocate globals for vardebug
	         info := mod.debuginfo
	         IF info
	            info := info + mod
	            vdbg := info + SIZEOF modinfo
	            a := info.count+1
	            WHILE a--
	               IF vdbg.scope = VDSCOPE_RELOC -> relocglob (private) ?
	                  vdbg.varofs := vdbg.varofs + globofs
	                  vdbg.scope := VDSCOPE_GLOBAL
	               ELSEIF vdbg.scope = VDSCOPE_XREF -> xref glob ?
	                  hln := getLabelHLN(vdbg.name + table)
	                  gvar := hln.ident
	                  vdbg.varofs := gvar.offset
	                  vdbg.scope := VDSCOPE_GLOBAL
	               ENDIF
	               vdbg++
	            ENDWHILE
	         ENDIF
	      ENDIF

	      -> 1.10.0
	      -> lets setup object.super for "local" inheritings
	      info := mod.objectinfo
	      IF info
	         info := info + mod
	         modobj := info + SIZEOF modinfo
	         a := info.count+1
	         WHILE a--
	            IF modobj.nrofmethods
	               modclass := modobj + SIZEOF modobject + Mul(modobj.nrofmembers, SIZEOF modmember)
	               IF modclass.supername
	                  object2 := getModSymb(mod, modclass.supername + table)
	                  IF object2 = NIL THEN reportIErr('linkModules/supername object failed')
	                  object1 := getModSymb(mod, modobj.name + table)
	                  IF object1 = NIL THEN reportIErr('linkModules/selfname object failed')
	                  object1.super := object2
	               ENDIF
	            ENDIF
	            modobj := modobj + modobj.totalsize
	         ENDWHILE
	      ENDIF

	      -> 2.2
	      info := IF mod.headsize < 160 THEN NIL ELSE mod.globinitinfo
	      IF info
	         info := info + mod
	         info.misc := info.misc + codeofs
	         link_globaldatasize := link_globaldatasize + Mul(info.count, 4)
	      ENDIF

	      mod := mod.succ
	   ENDIF

	ENDWHILE

	 #ifdef DBG_EXE
	 DEBUGF('linkModules() DONE\n')
	 #endif

ENDPROC

-> 1.10.0 extreme cleanup and rewrite for unified init
PROC finnishExe()
	DEF codelab:PTR TO codelab
	DEF a, b, t
	DEF ifunc:PTR TO lif
	DEF hln:PTR TO hln
	-> 1.5.6
	DEF startinfo:PTR TO startinfo, startoffset
	DEF libinfo:PTR TO libinfo
	DEF libtag:PTR TO libtag


	#ifdef DBG_EXE
	DEBUGF('finnishExe()\n')
	#endif

	IF g_librarymode
	   g_stacksize := IF g_optstack THEN g_optstack ELSE 32768->16384
	ELSEIF g_optstack
	   IF g_stacksize > g_optstack THEN addWarning('stack size lowered by OPT')
	   g_stacksize := g_optstack
	ELSEIF g_optpowerpc = CPU_PPC
	   g_stacksize := g_stacksize + 16000
	ELSE
	   g_stacksize := g_stacksize + 10000
	ENDIF

	g_stacksize := g_stacksize + 15 AND -16

	putAlign(4)

	hln := getLabelHLN('___startinfo')
	codelab := hln.ident
	IF codelab
	   IF codelab.identID <> IDENT_LABEL THEN reportIErr('"___startinfo" is not label')
	   startinfo := g_codebuf + codelab.offset - link_codesize
	   startoffset := codelab.offset
	ELSE
	   startinfo := NIL
	ENDIF

	IF g_optnostartup = FALSE

	   IF startinfo = NIL THEN reportIErr('___startinfo not found')

	   hln := getLabelHLN('main')
	   codelab := hln.ident
	   IF codelab
	      IF codelab.identID <> IDENT_LABEL THEN reportErr('"main" is not label')
	      codelab.referenced := 1
	      startinfo.mainofs := codelab.offset - startoffset
	   ELSEIF g_librarymode = FALSE
	      reportErr('"main" not found')
	   ENDIF
	ENDIF


	IF g_librarymode

	   -> 1.5.6
	   hln := getLabelHLN('___libtag')
	   IF hln.ident = NIL THEN reportIErr('___libtag not found')
	   codelab := hln.ident
	   IF codelab.identID <> IDENT_LABEL THEN reportIErr('___libtag is wrong type')
	   libtag := g_codebuf + codelab.offset - link_codesize

	   libinfo := startinfo

	   addReloc(libtag - g_codebuf + link_codesize + 14)
	   libtag.name := currentOffset()
	   t := StrLen(g_libraryname) + 1
	   CopyMem(g_libraryname, g_codeptr, t)
	   g_codeptr := g_codeptr + t
	   addReloc(libtag - g_codebuf + link_codesize + 18)
	   libtag.idstring := currentOffset()
	   t := StrLen(g_libraryidstr) + 1
	   CopyMem(g_libraryidstr, g_codeptr, t)
	   g_codeptr := g_codeptr + t

	   putAlign(4)

	   IF g_libraryrtagsname -> 1.8.0
	      hln := getLabelHLN(g_libraryrtagsname)
	      codelab := hln.ident
	      IF codelab
	         IF codelab.identID <> IDENT_LABEL THEN reportErr('label expected', g_libraryrtagsname)
	         codelab.referenced := 1
	         libtag.tags := codelab.offset
	         addReloc(libtag - g_codebuf + link_codesize + 28)
	      ELSE
	         reportErr('unknown label', g_libraryrtagsname)
	      ENDIF
	      libtag.flags := libtag.flags OR 64 -> libtag.flags OR= RTF_EXTENDED
	   ENDIF


	   hln := getLabelHLN('close')
	   codelab := hln.ident
	   IF codelab
	      IF codelab.identID <> IDENT_LABEL THEN reportErr('"close" is not label')
	      codelab.referenced := 1
	      libinfo.closeofs := codelab.offset - startoffset
	   ENDIF
	   hln := getLabelHLN('query')  -> 1.8.0
	   codelab := hln.ident
	   IF codelab
	      IF codelab.identID <> IDENT_LABEL THEN reportErr('"query" is not label')
	      codelab.referenced := 1
	      libinfo.queryofs := codelab.offset - startoffset
	   ENDIF

	   hln := getLabelHLN('___usrlibtab')
	   IF hln.ident = NIL THEN reportIErr('___usrlibtab not found')
	   codelab := hln.ident
	   IF codelab.identID <> IDENT_LABEL THEN reportIErr('___usrlibtab is wrong type')
	   libinfo.functabofs := codelab.offset - startoffset


	   libtag.version := g_libraryversion
	   libtag.revision := g_libraryrevision -> 1.8.0

	ENDIF

	IF startinfo

	   startinfo.stacksize := g_stacksize
	   startinfo.osversion := g_optosversion -> may be ZERO!

	   startinfo.initofs := currentOffset() - startoffset

	   g_codegen.putInit(g_globalsize, IVARSSIZE)

	   startinfo.rwsize := link_datasize +
	                       g_databufsize +
	                       IVARSSIZE +
	                       g_globalsize +
	                       link_globaldatasize
	ENDIF

	putAlign(4)

	#ifdef DBG_EXE
	DEBUGF('link_codesize=\d\n', link_codesize)
	DEBUGF('link_datasize=\d\n', link_datasize)
	DEBUGF('g_globalsize=\d\n', g_globalsize)
	DEBUGF('link_globaldatasize=\d\n', link_globaldatasize)
	DEBUGF('link_nrofreloc32s=\d\n', link_nrofreloc32s)
	DEBUGF('g_stacksize=\d\n', g_stacksize)
	DEBUGF('g_databufsize=\d\n', g_databufsize)
	DEBUGF('g_extracodesize=\d\n', g_extracodesize)
	#endif


ENDPROC

PROC put16(v)
	PutInt(g_codeptr, v)
	g_codeptr := g_codeptr + 2
ENDPROC

-> 2.2 moved back here
PROC putLibTable()
	DEF e:PTR TO entry, a, lab:PTR TO codelab
	DEF hln:PTR TO hln
	NEW lab
	putLabel(lab)
	hln := getLabelHLN('___usrlibtab')
	hln.ident := lab
	lab.name := hln.name
	lab.ltype := LTYPE_LAB
	lab.identID := IDENT_LABEL
	FOR a := 0 TO g_nrofentries-1
	   e := g_entrytable[a]
	   put16(IF e.type = 1 THEN $4AFC ELSE $4EF9)
	   putReloc(e.label)
	ENDFOR
	put16(-1)
	putAlign(4)
ENDPROC

-> 2.2 moved back here
PROC putClassinfos()   -> v44
	DEF o:PTR TO object, t, a, meth:PTR TO proc
	#ifdef DBG_OOP
	DEBUGF('putClassinfos()\n')
	#endif
	o := g_objectlist
	WHILE o
	   IF o.nrofmethods
	      #ifdef DBG_OOP
	      DEBUGF(' object = \s\n', o.name)
	      #endif
	      t := addRelStr(o.name)
	      putReloc(t)     -> -8) name:PTR
	      Put32(o.sizeof) -> -4) sizeof:LONG
	      putLabel(o)
	      a := 0
	      #ifdef DBG_OOP
	      DEBUGF('  Putting methods into classinfo..\n')
	      #endif
	      WHILE a < o.nrofmethods
	         meth := o.methodtable[a++]
	         IF meth.offset
	            #ifdef DBG_OOP
	            DEBUGF('   [RELOC] name=\s offset=$\h tableoffset=\d\n', meth.name, meth.offset, a-1*4)
	            #endif
	            putReloc(meth)
	         ELSE
	             #ifdef DBG_OOP
	            DEBUGF('   [NIL] name=\s offset=$\h tableoffset=\d\n', meth.name, meth.offset, a-1*4)
	            #endif
	            Put32(NIL)
	         ENDIF
	      ENDWHILE
	   ENDIF
	   o := o.next
	ENDWHILE
ENDPROC

PROC putStartup() -> v48
	DEF code, len, hln:PTR TO hln, codelab:PTR TO codelab

	#ifdef DBG_EXE
	DEBUGF('putStartup()\n')
	#endif

	putAlign(4)


	IF g_optnostartup
	   -> the JUMP at beginning of exe needs this label
	   NEW codelab
	   putLabel(codelab)
	   g_startupcodelab := codelab
	   RETURN
	ENDIF

	IF g_optosid = OSID_MORPHOS
	   IF g_librarymode
	      code, len := scanstartupmodule('ecxmodules:ecx/multilibrary_morphos.m')
	   ELSEIF g_optminstartup
	      code, len := scanstartupmodule('ecxmodules:ecx/minstartup_morphos.m')
	   ELSE
	      code, len := scanstartupmodule('ecxmodules:ecx/startup_morphos.m')
	   ENDIF

	ELSEIF g_optosid = OSID_AMIGAOS

	   IF g_librarymode
	      code, len := scanstartupmodule('ecxmodules:ecx/multilibrary_amigaos.m')
	   ELSEIF g_optminstartup
	      code, len := scanstartupmodule('ecxmodules:ecx/minstartup_amigaos.m')
	   ELSE
	      code, len := scanstartupmodule('ecxmodules:ecx/startup_amigaos.m')
	   ENDIF

	ELSEIF g_optosid = OSID_AMIGAOS4

	   IF g_librarymode
	      code, len := scanstartupmodule('ecxmodules:ecx/multilibrary_amigaos4.m')
	   ELSEIF g_optminstartup
	      code, len := scanstartupmodule('ecxmodules:ecx/minstartup_amigaos4.m')
	   ELSE
	      code, len := scanstartupmodule('ecxmodules:ecx/startup_amigaos4.m')
	   ENDIF

	ELSE

	   reportIErr('putStartup()/OSID')

	ENDIF

	/* insert startupcode */
	CopyMem(code, g_codeptr, len*4)
	g_codeptr := g_codeptr + (len*4)


	hln := getLabelHLN('___startup')
	IF hln.ident = NIL THEN reportIErr('___startup is missing')
	g_startupcodelab := hln.ident -> for symbolhunk


	#ifdef DBG_EXE
	DEBUGF('putStartup() DONE\n')
	#endif

ENDPROC

-> 1.5.6
PROC scanstartupmodule(name)
	DEF info:PTR TO modinfo, code, err, len, mh:PTR TO moduleheader
	DEF fullname[1024]:STRING
	DEF lptr:PTR TO LONG, a, b, ifunc:PTR TO lif, modref:PTR TO modref
	DEF ifuncsarray:PTR TO LONG
	DEF label:PTR TO codelab, modlab:PTR TO modproc
	DEF strtable, hln:PTR TO hln
	DEF gvar:PTR TO gvar

	#ifdef DBG_EXE
	DEBUGF('scanstartupmodule(\s)..', name)
	#endif

	-> 1.10.0
	mh, err := findCachedModule(name)
	IF mh = NIL
	   IF err THEN Throw(err, name)
	   mh, err := readModule(name)
	   IF mh = NIL THEN Throw(err, name)
	ENDIF

	#ifdef DBG_EXE
	DEBUGF('scanstartupmodule got module ok\n')
	#endif

	info := mh + mh.codeinfo
	code := info + SIZEOF modinfo
	len := info.count

	/* stringtable is nice to have */
	info := mh + mh.strtabinfo
	strtable := info + SIZEOF modinfo

	/* there may be relocations that needs to be handled */
	IF mh.relocinfo
	   #ifdef DBG_EXE
	   DEBUGF('relocs..')
	   #endif
	   info := mh + mh.relocinfo
	   lptr := info + SIZEOF modinfo
	   a := info.count + 1
	   WHILE a--
	      b := lptr[]++
	      PutLong(code+b, Long(code+b) + currentOffset())
	      addReloc(currentOffset() + b)
	   ENDWHILE
	ENDIF

	/* there may be ifunc usage that wee need to handle */
	IF mh.ifuncinfo
	   #ifdef DBG_EXE
	   DEBUGF('ifuncs..')
	   #endif
	   info := mh + mh.ifuncinfo
	   ifuncsarray := IF info.misc = 0 THEN g_internalfuncs ELSE g_internalfuncsppc
	   modref := info + SIZEOF modinfo
	   a := info.count + 1
	   WHILE a--
	      lptr := modref + SIZEOF modref
	      b := modref.numrefs + 1
	      ifunc := ifuncsarray[modref.name]
	      WHILE b--
	         ifunc.labrefs := NEW [ifunc.labrefs, currentOffset() + lptr[]++]:labref
	      ENDWHILE
	      modref := lptr
	   ENDWHILE
	ENDIF

	/* there should be a couple of labels, we might use to path stuff later */
	IF mh.procinfo
	   #ifdef DBG_EXE
	   DEBUGF('procs..')
	   #endif
	   info := mh + mh.procinfo
	   modlab := info + SIZEOF modinfo
	   a := info.count + 1
	   WHILE a--
	      hln := getLabelHLN(strtable + modlab.name)
	      NEW label
	      label.ltype := LTYPE_LAB
	      label.identID := IDENT_LABEL
	      hln.ident := label
	      label.offset := currentOffset() + modlab.offset
	      label.name := hln.name
	      modlab := modlab + modlab.totalsize
	   ENDWHILE
	ENDIF

	-> 1.7.1
	info := mh.xrefginfo
	IF info
	   #ifdef DBG_EXE
	   DEBUGF('   global xrefs\n')
	   #endif
	   info := info + mh
	   b := info.count + 1
	   modref := info + SIZEOF modinfo
	   WHILE b--
	      hln := getLabelHLN(modref.name + strtable)
	      gvar := hln.ident
	      IF gvar = NIL  -> v50
	         reportIErr('linkmodules global not allocated', hln.name)
	      ENDIF
	      a := modref.numrefs + 1
	      lptr := modref + SIZEOF modref
	      WHILE a-- DO PutInt(mh + mh.codeinfo + SIZEOF modinfo + lptr[]++, gvar.offset)
	      modref := lptr
	   ENDWHILE
	ENDIF

	#ifdef DBG_EXE
	DEBUGF('..DONE\n', name)
	#endif

ENDPROC code, len

-> 1.10.0
PROC scantargetmodule(name)
	DEF info:PTR TO modinfo, code, err, len, mh:PTR TO moduleheader
	DEF fullname[1024]:STRING
	DEF lptr:PTR TO LONG, a, b, s
	DEF ifuncsarray:PTR TO LONG
	DEF modlab:PTR TO modproc
	DEF strtable, hln:PTR TO hln


	#ifdef DBG_MAIN
	DEBUGF('scantargetmodule(\s)..', name)
	#endif

	-> 1.10.0
	mh, err := findCachedModule(name)
	IF mh = NIL
	   IF err THEN Throw(err, name)
	   mh, err := readModule(name)
	   IF mh = NIL THEN Throw(err, name)
	ENDIF

	IF mh.codeinfo = NIL THEN reportIErr('target module problem 1')
	info := mh + mh.codeinfo
	code := info + SIZEOF modinfo
	len := info.count

	/* stringtable is nice to have */
	info := mh + mh.strtabinfo
	strtable := info + SIZEOF modinfo

	IF mh.procinfo
	   #ifdef DBG_MAIN
	   DEBUGF('procs..')
	   #endif
	   info := mh + mh.procinfo
	   modlab := info + SIZEOF modinfo
	   a := info.count + 1
	   WHILE a--
	      IF modlab.totalsize = SIZEOF modstatic
	         IF     StrCmp(strtable + modlab.name, '___m_default', STRLEN)
	            s := String(StrLen(code + modlab.offset))
	            IF s = NIL THEN Raise("MEM")
	            StrCopy(s, code + modlab.offset)
	            Link(s, g_defmodnames)
	            g_defmodnames := s
	         ELSE
	            -> ignore for now
	         ENDIF
	      ENDIF
	      modlab := modlab + modlab.totalsize
	   ENDWHILE
	ENDIF


	readMacroInfo(mh)
	readConstInfo(mh)
	readObjectInfo(mh)

	hln := getLabelHLN('___internalglobs')
	IF hln.ident2 = NIL THEN reportIErr('target module problem 2')


	#ifdef DBG_MAIN
	DEBUGF('..DONE\n', name)
	#endif

ENDPROC


PROC getModSymb(mod:PTR TO moduleheader, name)
	DEF codelab:PTR TO codelab, i:PTR TO modinfo, hln:PTR TO hln
	codelab := mod.modextens.codelist
	WHILE codelab
	   IF StrCmp(codelab.name, name) THEN RETURN codelab
	   codelab := codelab.codelink
	ENDWHILE
	reportErr('unknown symbol', name)
ENDPROC

PROC getIfuncOffset(num, cpu)
	DEF hln:PTR TO hln, lif:PTR TO lif, a
	#ifdef DBG_EXE
	DEBUGF('getIfuncOffset(\d,\d) ', num,cpu)
	#endif
	lif := IF cpu = 0 THEN g_internalfuncs[num] ELSE g_internalfuncsppc[num]

	#ifdef DBG_EXE
	DEBUGF('lif=$\h, lif.name=\a\s\a, lif.offset=$\h, lif.code = $\h, lif.codelen = $\h\n',
	   lif, lif.name, lif.offset, lif.code, lif.codelen)
	#endif

	-> 1.5.6
	IF lif.flags = -1 THEN reportErr('Some module is using old string formatting functions that are no longer supported')

	IF lif.offset = NIL
	   IF cpu = 1 THEN putAlign(4)
	   lif.offset := currentOffset()
	   CopyMem(lif.code, g_codeptr, lif.codelen)
	   g_codeptr := g_codeptr + lif.codelen
	ENDIF

ENDPROC lif.offset

-> fix powerpc data reloc offsets in modules
/*
PROC fix1616code(code, ofs)
	MOVEQ #0, D1
	MOVEQ #0, D0
	MOVE.L code, A0
	ADDQ.L #2, A0
	MOVE.W (A0), D0 -> low
	SWAP D0
	ADDQ.L #4, A0
	MOVE.W (A0), D0 -> high
	SWAP D0
	ADD.L ofs, D0 -> fix ofs
	BTST #15, D0
	BEQ f1616c___
	SWAP D0
	ADDQ.W #1, D0 -> we add one ?
	SWAP D0
f1616c___:
	SWAP D0
	MOVE.W D0, (A0) -> high
	SUBQ.L #4, A0
	SWAP D0
	MOVE.W D0, (A0) -> low
ENDPROC
*/
PROC fix1616code(code:PTR TO INT, ofs)
	DEF val

	val := Shl(code[3],16) OR (code[1] AND $FFFF) + ofs

	code[1] := val
	code[3] := (Shr(val, 16) AND $FFFF) + IF val AND $8000 THEN 1 ELSE 0

ENDPROC val

-> powerpc
-> FIXED v49
/*
PROC put1616ofs(code, ofs)
	MOVE.L code, A0
	MOVE.L ofs, D0
	MOVE.W D0, 6(A0) -> low
	BTST #15, D0
	BEQ p1616o___
	SWAP D0
	ADDQ.W #1, D0 -> we add one ?
	SWAP D0
p1616o___:
	SWAP D0
	MOVE.W D0, 10(A0) -> high
ENDPROC
*/
PROC put1616ofs(code:PTR TO INT, ofs)
	code[3] := ofs
	code[5] := (Shr(ofs, 16) AND $FFFF) + IF ofs AND $8000 THEN 1 ELSE 0
ENDPROC





-> 1.7.1
PROC linkobject_key(n:PTR TO item)
	DEF label:PTR TO codelab, proc:PTR TO proc, hln:PTR TO hln
	DEF va:PTR TO var, freg, ireg, t
	IF g_optmodule THEN reportErr('scope of LINKOBJECT')
	IF g_librarymode THEN reportErr('scope of LINKOBJECT')
	IF g_currentobject THEN reportErr('scope of LINKOBJECT')
	IF g_currentproc THEN reportErr('scope of LINKOBJECT')
	IF g_codegen THEN reportErr('scope of LINKOBJECT') -> important!
	g_linkobjmode := TRUE
	g_optmodule := TRUE
	IF EstrLen(g_modname)
	   g_modname[EstrLen(g_modname)-1] := "o"
	ELSE
	   StrCopy(g_modname, FilePart(g_sourcename), StrLen(FilePart(g_sourcename))-2)
	   StrAdd(g_modname, '.o')
	ENDIF
	initTarget()
	IF n.data <> KW_USES THEN RETURN n

	REPEAT
	   n++
	   IF n.data = 10
	      setLinenum(n.info)
	      n++
	   ENDIF
	   IF n.data <> IT_LABEL THEN reportErr('label expected for USES')
	   IF n[1].data = "("
	      NEW proc
	      proc.identID := IDENT_LABEL
	      hln := n.info
	      proc.name := hln.name
	      proc.ltype := LTYPE_PROC
	      proc.cpu := g_optpowerpc
	      IF hln.ident THEN reportErr('double declaration of label for USES', hln.name)
	      hln.ident := proc
	      proc.codelink := g_xcodelist
	      g_xcodelist := proc
	      n := n[2]
	      freg := 1
	      ireg := 3
	      WHILE n.data <> ")"
	         IF n.data = NIL THEN reportErr('unexpected end of source')
	         NEW va
	         va.identID := IDENT_VARIABLE
	         g_temparray[proc.nrofargs] := va
	         proc.nrofargs := proc.nrofargs + 1
	         n := varmembdec(n, va.type, va)
	         IF va.defo
	            proc.nrofdefaults := proc.nrofdefaults + 1
	         ELSEIF proc.nrofdefaults -> (x=1,y,z=5) : error !
	            reportErr('default argument syntax')
	         ENDIF

	         IF va.type.size = 4
	            IF va.type.flags AND MEMBF_FLOAT
	               va.rtype := PPCARGRTYPE_FX    -> c functions takes FLOAT in float register
	               va.rnum := freg++
	            ELSE
	               va.rtype := PPCARGRTYPE_RX
	               va.rnum := ireg++
	            ENDIF
	         ELSEIF va.type.size = 8
	            va.rtype := PPCARGRTYPE_FX
	            va.rnum := freg++
	         ELSE
	            reportErr('illegal size of argument', va.hln.name)
	         ENDIF
	         IF n.data = ","
	            n++
	            IF n.data = 10
	               setLinenum(n.info)
	               n++
	            ENDIF
	         ELSEIF n.data <> ")"
	            reportErr('USES syntax')
	         ENDIF
	      ENDWHILE
	      n++ -> skip )
	      /* make permanernt copy of params */
	      NEW proc.argarray[proc.nrofargs+1]
	      CopyMemQuick(g_temparray, proc.argarray, Mul(proc.nrofargs,4))
	      FOR t := 0 TO 3
	         proc.mret.ros[t] := DREG
	         proc.mret.rds[t] := t+3
	      ENDFOR
	      IF n.data = "(" -> returntypes defined ?
	         t := 0
	         freg := 1
	         ireg := 3
	         REPEAT
	            n++
	            IF n.data <> NIL
	            IF t <= 3
	            SELECT 256 OF n.data
	            CASE KW_FLOAT, KW_DOUBLE
	               proc.mret.ros[t] := FREG
	               proc.mret.rds[t] := freg++
	            DEFAULT
	               proc.mret.ros[t] := DREG
	               proc.mret.rds[t] := ireg++
	            ENDSELECT
	            n++
	            ENDIF ; ENDIF
	         UNTIL n.data <> ","
	         IF n.data <> ")" THEN reportErr('")" expected')
	         n++
	      ENDIF
	   ELSE
	      NEW label
	      label.identID := IDENT_LABEL
	      hln := n.info
	      label.name := hln.name
	      label.ltype := LTYPE_LAB
	      IF hln.ident THEN reportErr('double declaration of label for USES', hln.name)
	      hln.ident := label
	      label.codelink := g_xcodelist
	      g_xcodelist := label
	      n++
	   ENDIF

	UNTIL n.data <> ","

ENDPROC n

OBJECT headhunk
	id, name, numhunks, starthunk, endhunk, codesize
ENDOBJECT

OBJECT hunk
	id, longsize
ENDOBJECT


PROC countReverseLinelist()
	DEF lptr:PTR TO LONG
	DEF numlines=0
	DEF t
	lptr := g_linelist
	g_linelist := NIL
	WHILE lptr -> count and reverse lines
	   numlines++
	   t := lptr[]
	   lptr[] := g_linelist
	   g_linelist := lptr
	   lptr := t
	ENDWHILE
ENDPROC numlines



-> make ADOS
PROC makeADOS()
	DEF mod:REG PTR TO moduleheader
	DEF lptr:PTR TO LONG, info:PTR TO modinfo, i2:PTR TO modinfo
	DEF t, str[100]:STRING
	DEF a, dblab:PTR TO codelab -> 2.1
	DEF codelab:PTR TO codelab
	DEF hln:PTR TO hln
	DEF offset
	DEF buffer, wptr:REG PTR TO LONG, bufsize
	DEF nroflines=0
	DEF filename[256]:STRING, fh
	DEF labref:PTR TO labref
	DEF headhunk:PTR TO headhunk, codehunk:PTR TO hunk
	DEF linehunk:PTR TO hunk
	DEF gvar:PTR TO gvar, e:PTR TO entry
	DEF dbgstrtabsize, varhunk:PTR TO hunk

	#ifdef DBG_EXE
	DEBUGF('writeADOS()\n')
	#endif

	->IF g_optpowerpc = CPU_PPC THEN IF trap THEN addReloc(4) -> 2.1

	bufsize := 4 * 8 -> HUNK_HEADER
	bufsize := bufsize + 4 * 2 -> HUNK_CODE
	bufsize := bufsize + link_codesize + (g_codeptr - g_codebuf)-> total size of code section
	bufsize := bufsize + 4 -> HUNK_RELOC
	bufsize := bufsize + (4 * 2) + Mul(link_nrofreloc32s, 4)
	IF g_symbolhunk THEN bufsize := bufsize + g_symbolssize + 1000
	IF g_linedebug
	   nroflines := countReverseLinelist()
	   mod := g_modulelist.head
	   WHILE mod.succ
	      IF mod.lineinfo
	         info := mod.lineinfo + mod
	         nroflines := nroflines + info.count
	      ENDIF
	      mod := mod.succ
	   ENDWHILE
	   bufsize:= bufsize + Mul(nroflines, 8)
	   bufsize := bufsize + 4000 -> for debugheader and sourcenames
	ENDIF
	IF g_vardebug -> 1.6.0
	   t, dbgstrtabsize := computeExeDebugSize()
	   bufsize := bufsize + t
	ENDIF

	/* allocate buffer */

	bufsize := bufsize + 1000 -> safety


	buffer := NEWMEMR(bufsize)
	wptr := buffer

	#ifdef DBG_EXE
	DEBUGF('exe: \d bytes allocated\n', bufsize)
	#endif

	/* write to buffer */

	-> write head
	headhunk := wptr
	headhunk.id := $3F3 -> HUNK_HEADER
	headhunk.name := NIL  -> no name
	headhunk.numhunks := 1    -> two hunks
	headhunk.starthunk := 0    -> start with 0
	headhunk.endhunk := 0    -> end with 1
	headhunk.codesize := 0 -> update later !


	wptr := wptr + SIZEOF headhunk

	-> write code
	codehunk := wptr
	codehunk.id := $3E9
	codehunk.longsize := 0 -> set later !
	wptr := codehunk + 8

	IF g_optpowerpc = CPU_PPC
	   ->IF trap
	   ->   wptr[]++ := $FF000000  -> morphos 68k->ppc trap
	   ->   wptr[]++ := g_startupcodelab.offset -> a reloc has been placed over this
	   ->ELSE
	      wptr[]++ := Shl(18,26) OR g_startupcodelab.offset  -> B startup
	      wptr[]++ := NIL
	   ->ENDIF
	ELSE
	   wptr[]++ := $4E7160FF
	   wptr[]++ := g_startupcodelab.offset-4  -> BRA.L startup
	ENDIF

	-> modulecode
	mod := g_modulelist.head
	WHILE mod.succ
	   info := mod.codeinfo + mod
	   CopyMemQuick(info + SIZEOF modinfo, wptr, Shl(info.count,2))
	   wptr := wptr + Shl(info.count,2)
	   mod := mod.succ
	ENDWHILE

	-> maincode and ifunc-code, globinit, etc
	CopyMem(g_codebuf, wptr, a := (g_codeptr - g_codebuf))
	wptr := wptr + (a + 3 AND $FFFFFC)

	codehunk.longsize := Div(wptr - codehunk - SIZEOF hunk, 4)
	headhunk.codesize := codehunk.longsize

	IF link_reloc32list
	   wptr[]++ := $3EC
	   t := wptr++ -> nrofrelocs
	   wptr[]++ := 0 -> hunk 0
	   lptr := link_reloc32list
	   WHILE lptr
	      wptr[]++ := lptr[1]
	      lptr := lptr[]
	   ENDWHILE
	   PutLong(t, Div(wptr - t - 8, 4))
	   wptr[]++ := NIL -> end relocs
	ENDIF



	-> write linedebug ?
	IF g_linedebug
	   linehunk := wptr
	   wptr := wptr + SIZEOF hunk
	   linehunk.id := 1009
	   wptr := genLineDebug(wptr)
	   linehunk.longsize := Div(wptr - linehunk - 8, 4)
	ENDIF

	->variable debug
	IF g_vardebug
	   varhunk := wptr
	   wptr := wptr + SIZEOF hunk
	   varhunk.id := 1009
	   wptr := genVardebug(wptr, dbgstrtabsize)
	   varhunk.longsize := Div(wptr - varhunk - 8, 4)
	ENDIF

	-> write symbol(s)
	wptr[]++ := $3F0 -> symbol hunk id


	IF g_symbolhunk
	   dblab := g_dblablist
	   WHILE dblab
	      IF dblab.offset
	         t := StrLen(dblab.name) + 1 + 3 AND $FFFC
	         wptr[]++ := Shr(t, 2) -> len in longs
	         CopyMem(dblab.name, wptr, t)   -> name
	         wptr := wptr + t
	         wptr[]++ := dblab.offset
	      ENDIF
	      dblab := dblab.dblabnext
	   ENDWHILE
	ENDIF
	wptr[]++ := NIL -> end symbols

	wptr[]++ := $3F2 -> HUNK_END


	IF g_showbuf THEN WriteF('Out buffer: \d of \d bytes used\n', wptr-buffer, bufsize)


ENDPROC buffer, wptr-buffer

OBJECT dbsymcntx
	name
	offset
	-> internal stuff below
	type:LONG -> 0=68k ifunc, 1=ppc ifunc, 2=main proc/label, 3=module codelab
	ident:PTR TO ident -> actually index if type=0
	module:PTR TO moduleheader -> if type=3
ENDOBJECT


-> v50 (1.5.1)
-> generates linedebug (for ADOS or ELF or whatever)
-> expects g_linelist to have been "ordered" before.
-> 1.6.0, added "Numlines" to format
PROC genLineDebug(wptr:PTR TO LONG)
	DEF str[500]:STRING
	DEF info:PTR TO modinfo
	DEF line:PTR TO linedef
	DEF mod:PTR TO moduleheader
	DEF numlinesptr:PTR TO LONG, count

	 #ifdef DBG_EXE
	DEBUGF('genLineDebug($\h)\n', wptr)
	#endif

	mod := g_modulelist.head
	WHILE mod.succ
	   IF mod.lineinfo
	      info := mod.lineinfo + mod
	      StrCopy(str, mod.mname, StrLen(mod.mname)-2)
	      StrAdd(str, '.e')
	      wptr[]++ := "LDBG"
	      wptr[]++ := info.count -> 1.6.0
	      wptr[] := Shr(StrLen(str) + 1 + 3, 2)
	      CopyMem(str, wptr+4, StrLen(str)+1)
	      wptr := wptr + 4 + Shl(wptr[], 2)
	      CopyMemQuick(info + SIZEOF modinfo, wptr, Shl(info.count, 3))
	      wptr := wptr + Shl(info.count, 3)
	   ENDIF
	   mod := mod.succ
	ENDWHILE

	wptr[]++ := "LDBG"
	numlinesptr := wptr++
	wptr[]++ := Shr(StrLen(g_sourcename) + 1 + 3,2)
	CopyMem(g_sourcename, wptr, StrLen(g_sourcename)+1)
	wptr := wptr + Shl(wptr[-1], 2)
	line := g_linelist
	count := 0     -> 1.6.0
	WHILE line
	   count++       -> 1.6.0
	   wptr[]++ := line.line
	   wptr[]++ := line.offset
	   line := line.next
	ENDWHILE
	numlinesptr[] := count -> 1.6.0

	wptr[]++ := NIL
ENDPROC wptr


-> make ELF (MOS)
PROC makeELF()
	DEF ehdr:PTR TO ehdr
	DEF shdr:PTR TO shdr
	DEF phdr:PTR TO phdr
	DEF sym:PTR TO sym
	DEF rel:REG PTR TO rela
	DEF mod:PTR TO moduleheader, b, if:PTR TO lif, info:PTR TO modinfo
	DEF lptr:REG PTR TO LONG
	DEF buffer, wptr:REG PTR TO LONG, bufsize
	DEF fh, filename[256]:STRING
	DEF labref:PTR TO labref
	DEF nroflines=0, t=NIL, a=NIL
	DEF codestart:PTR TO LONG
	DEF reltable:PTR TO rela, symtable:PTR TO sym, strtable, shdrtable
	DEF shstrtable
	DEF gvar:PTR TO gvar
	DEF hln:PTR TO hln
	DEF proc:PTR TO proc, e:PTR TO entry
	DEF linetable, str[300]:STRING
	DEF dblab:PTR TO codelab -> 2.1
	DEF debugtable, dbgstrtabsize -> 1.6.0

	#ifdef DBG_EXE
	DEBUGF('makeELF()\n')
	#endif



	-> ehdr
	-> code
	-> rel table   [1+nrrelocs]:ARRAY OF rel
	-> sym table   ARRAY OF sym
	-> strtable    \0__abox__\0__amigappc__\0_start\0
	-> shstrtable  \0.text\0.rela.text\0.shstrtab\0.symtab\0.strtab\0.ldbg\0.vdbg\0
	-> shdr0 dummy
	-> shdr1 .text (code)
	-> shdr2 .rela.text
	-> shdr3 .shstrtab
	-> shdr4 .symtab
	-> shdr5 .strtab
	-> [shdr6 .line]
	-> [shdr7 .debug] 1.6.0
	-> [phdr0] 1.10.0


	bufsize := SIZEOF ehdr
	bufsize := bufsize + Mul(SIZEOF shdr,8)
	bufsize := bufsize + link_codesize + (g_codeptr - g_codebuf) -> total size of code section
	bufsize := bufsize + Mul(link_nrofreloc32s, SIZEOF rela)
	bufsize := bufsize + Mul(4, SIZEOF sym)
	IF g_symbolhunk THEN bufsize := bufsize + g_symbolssize + 1000
	IF g_linedebug
	   nroflines := countReverseLinelist()
	   mod := g_modulelist.head
	   WHILE mod.succ
	      IF mod.lineinfo
	         info := mod.lineinfo + mod
	         nroflines := nroflines + info.count
	      ENDIF
	      mod := mod.succ
	   ENDWHILE
	   bufsize:= bufsize + Mul(nroflines, 8)
	   bufsize := bufsize + 4000 -> for debugheader and sourcenames
	ENDIF

	IF g_vardebug -> 1.6.0
	   t, dbgstrtabsize := computeExeDebugSize()
	   bufsize := bufsize + t
	ENDIF

	/* allocate buffer */

	bufsize := bufsize + 1000 -> safety

	#ifdef DBG_EXE
	DEBUGF('allocating \d bytes for exe (elf)\n', bufsize)
	#endif

	buffer := NEWMEMR(bufsize)

	wptr := buffer

	/* header */
	ehdr := wptr
	ehdr.ident[EI_MAG0] := $7F
	ehdr.ident[EI_MAG1] := "E"
	ehdr.ident[EI_MAG2] := "L"
	ehdr.ident[EI_MAG3] := "F"
	ehdr.ident[EI_CLASS] := ELFCLASS32
	ehdr.ident[EI_DATA] := ELFDATA2MSB
	ehdr.ident[EI_VERSION] := $01
	ehdr.ident[EI_PAD] := $00
	ehdr.type := IF g_elf_usephdr THEN ET_EXEC ELSE ET_REL
	ehdr.machine := IF g_optpowerpc = CPU_PPC THEN EM_PPC ELSE EM_68K
	ehdr.version := 1
	ehdr.phoff := 0
	ehdr.shoff := 0 -> set later !
	ehdr.ehsize := SIZEOF ehdr
	ehdr.phentsize := 0
	ehdr.phnum := 0
	ehdr.shentsize := SIZEOF shdr
	ehdr.shnum := 0 -> set later !
	ehdr.shstrndx := 3 -> shstrtab
	wptr := wptr + SIZEOF ehdr

	codestart := wptr

	IF g_optpowerpc = CPU_PPC
	   wptr[]++ := Shl(18,26) OR g_startupcodelab.offset  -> B startup
	   wptr[]++ := NIL
	ELSE
	   wptr[]++ := $4E7160FF
	   wptr[]++ := g_startupcodelab.offset-4  -> BRA.L startup
	ENDIF

	/* modulecode */
	mod := g_modulelist.head
	WHILE mod.succ
	   info := mod.codeinfo + mod
	   CopyMemQuick(info + SIZEOF modinfo, wptr, (a := Shl(info.count,2)))
	   wptr := wptr + a
	   mod := mod.succ
	ENDWHILE

	/* maincode */
	CopyMemQuick(g_codebuf, wptr, (a := g_codeptr - g_codebuf))
	wptr := wptr + a

	 -> extra long for __amigappc__,__abox__ combined vars
	wptr[]++ := 1


	 /* rel table */
	reltable := wptr
	rel := reltable
	lptr := link_reloc32list
	t := R_INFO(1,R_PPC_ADDR32)
	WHILE lptr
	   rel.offset := lptr[1]
	   rel.info := t
	   rel.addend := Long(codestart + rel.offset) -> v45 :(
	   rel++
	   lptr := lptr[]
	ENDWHILE
	wptr := rel

	/* sym table */
	symtable := wptr
	sym := symtable

	#ifdef DBG_EXE
	DEBUGF('..symbols..\n')
	#endif

	sym++ -> emty symbol first

	sym.name := NIL
	sym.value := 0
	sym.size := 0
	sym.info := ST_INFO(STB_LOCAL, STT_SECTION)
	sym.other := 0
	sym.shndx := 1 -> code section
	sym++

	sym.name := 1 -> "__abox__" etc
	sym.value := reltable-codestart-4
	sym.size := 4
	sym.info := ST_INFO(STB_GLOBAL, STT_OBJECT)
	sym.other := 0
	sym.shndx := 1
	sym++
	t := 1 + EstrLen(g_elfsym_target) + 1

	IF g_optosid = OSID_AMIGAOS4
	   sym.name := t -> "_start"
	   sym.value := 0
	   sym.size := 0
	   sym.info := ST_INFO(STB_GLOBAL, STT_FUNC)
	   sym.other := 0
	   sym.shndx := 1
	   sym++
	   t := t + StrLen('_start') + 1
	ENDIF



	StringF(str, '\s \s (\s).', g_compiledwithstr,
	                            g_verstr,
	                            g_platformstr,
	                            )
	t := t + EstrLen(str) + 1
	IF g_symbolhunk
	   dblab := g_dblablist
	   WHILE dblab
	      IF dblab.offset -> avoid adding unused ifuncs
	         sym.name := t
	         t := t + StrLen(dblab.name) + 1
	         sym.value := dblab.offset
	         sym.size := 0
	         sym.info := ST_INFO(STB_LOCAL, STT_FUNC)
	         sym.other := 0
	         sym.shndx := 1
	         sym++
	      ENDIF
	      dblab := dblab.dblabnext
	   ENDWHILE
	ENDIF

insanestfixintheworld:
	dummy()

	wptr := sym

	/* strtable */
	strtable := wptr
	AstrCopy(wptr + 1, g_elfsym_target)
	wptr := wptr + 1 + EstrLen(g_elfsym_target) + 1

	IF g_optosid = OSID_AMIGAOS4
	   CopyMem('_start\0', wptr, STRLEN)
	   wptr := wptr + STRLEN
	ENDIF

	AstrCopy(wptr, str)
->WriteF('\s, \s\n', strtable+1, wptr)
	wptr := wptr + EstrLen(str) + 1

	IF g_symbolhunk
	   dblab := g_dblablist
	   WHILE dblab
	      IF dblab.offset
	         CopyMem(dblab.name, wptr, t := StrLen(dblab.name))
	         wptr := wptr + t + 1
	      ENDIF
	      dblab := dblab.dblabnext
	   ENDWHILE
	ENDIF

	wptr := wptr + 3 AND -4

	->WriteF('sym2.name \d sym3.name \d sym4.name \d\n',
	->   symtable[2].name, symtable[3].name, symtable[4].name)


	/* shstrtable */
	shstrtable := wptr
	CopyMem('\0.text\0.rela.text\0.shstrtab\0.symtab\0.strtab\0.ldbg\0.vdbg\0', wptr, STRLEN)
	wptr := wptr + STRLEN
	wptr := wptr + 3 AND -4

	linetable := wptr  -> v45
	IF g_linedebug
	   wptr := genLineDebug(wptr)
	ENDIF

	debugtable := wptr
	IF g_vardebug
	   wptr := genVardebug(wptr, dbgstrtabsize)
	ENDIF

	shdrtable := wptr
	shdr := shdrtable
	ehdr.shoff := shdr - ehdr


	#ifdef DBG_EXE
	DEBUGF('..sections..\n')
	#endif

	/* dummy section */

	shdr++

	/* code section (1) */

	shdr.name := 1
	shdr.type := SHT_PROGBITS
	shdr.flags := SHF_ALLOC OR SHF_EXECINSTR
	shdr.addr := 0
	shdr.offset := codestart - ehdr
	shdr.size := reltable - codestart
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 16
	shdr.entsize := 0
	shdr++

	/* rel section (2) */

	shdr.name := 7
	shdr.type := IF symtable-reltable THEN SHT_RELA ELSE SHT_NULL
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := reltable - ehdr
	shdr.size := symtable - reltable
	shdr.link := 4 -> sym section
	shdr.info := 1 -> code section
	shdr.addralign := 4
	shdr.entsize := SIZEOF rela
	shdr++


	/* shstrtab section (3) */

	shdr.name := 18
	shdr.type := SHT_STRTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := shstrtable - ehdr
	shdr.size := 56
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 1
	shdr.entsize := 0
	shdr++

	/* symtab section (4) */

	shdr.name := 28
	shdr.type := SHT_SYMTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := symtable - ehdr
	shdr.size := strtable - symtable
	shdr.link := 5 -> strtable
	shdr.info := 1
	shdr.addralign := 4
	shdr.entsize := SIZEOF sym
	shdr++

	/* strtab section (7) */

	shdr.name := 36
	shdr.type := SHT_STRTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := strtable - ehdr
	shdr.size := (IF g_linedebug THEN linetable ELSE shstrtable) - strtable
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 1
	shdr.entsize := 0
	shdr++

	IF g_linedebug
	   shdr.name := 44
	   shdr.type := SHT_PROGBITS
	   shdr.flags := NIL
	   shdr.addr := 0
	   shdr.offset := linetable - ehdr
	   shdr.size := (IF g_vardebug THEN debugtable ELSE shdrtable) - linetable
	   shdr.link := 0
	   shdr.info := 0
	   shdr.addralign := 0
	   shdr.entsize := 0
	   shdr++
	ENDIF

	IF g_vardebug
	   shdr.name := 50
	   shdr.type := SHT_PROGBITS
	   shdr.flags := NIL
	   shdr.addr := 0
	   shdr.offset := debugtable - ehdr
	   shdr.size := shdrtable - debugtable
	   shdr.link := 0
	   shdr.info := 0
	   shdr.addralign := 0
	   shdr.entsize := 0
	   shdr++
	ENDIF

	ehdr.shnum := shdr - shdrtable / SIZEOF shdr

	wptr := shdr

	IF g_elf_usephdr -> 1.10.0

	   ehdr.phoff := wptr - ehdr
	   ehdr.phnum := 1
	   ehdr.phentsize := SIZEOF phdr

	   phdr := wptr

	   phdr.type := PT_LOAD
	   phdr.offset := codestart - ehdr
	   phdr.vaddr := 0
	   phdr.paddr := 0
	   phdr.filesz := reltable - codestart
	   phdr.memsz := phdr.filesz
	   phdr.flags := PF_R OR PF_X
	   phdr.align := 4
	   phdr++

	   wptr := phdr

	ENDIF

	#ifdef DBG_EXE
	DEBUGF('writing \d bytes exe (elf)\n', wptr-buffer)
	#endif

	IF g_showbuf THEN WriteF('Out buffer: \d of \d bytes used\n', wptr-buffer, bufsize)



ENDPROC buffer, wptr-buffer

PROC dummy() IS NIL

-> 1.6.0
-> returns total debugsize for exe, strtabsize for mainsource
PROC computeExeDebugSize()
	DEF size=0, strsize=0
	DEF mod:PTR TO moduleheader
	DEF info:PTR TO modinfo
	DEF proc:PTR TO proc
	DEF g:PTR TO gvar, var:PTR TO var

	mod := g_modulelist.head
	WHILE mod.succ
	   IF mod.debuginfo
	      size := size + SIZEOF vdbghead
	      size := size + StrLen(mod.mname) + 3 AND -4
	      info := mod + mod.strtabinfo
	      size := size + info.misc
	      info := mod + mod.debuginfo
	      size := size + Mul(info.count, SIZEOF vardbg)
	   ENDIF
	   mod := mod.succ
	ENDWHILE

	proc := g_proclist
	WHILE proc
	   IF proc.ltype = LTYPE_PROC
	      strsize := strsize + StrLen(proc.name) + 1
	      var := proc.varchain
	      WHILE var
	         size := size + SIZEOF vardbg
	         strsize := strsize + StrLen(var.hln.name) + 1
	         IF var.type.object THEN strsize := strsize + StrLen(var.type.object.name) + 1
	         var := var.varchain
	      ENDWHILE
	   ENDIF
	   proc := proc.next
	ENDWHILE

	g := g_gvarlist
	WHILE g
	   size := size + SIZEOF vardbg
	   strsize := strsize + StrLen(g.hln.name) + 1
	   IF g.type.object THEN strsize := strsize + StrLen(g.type.object.name) + 1
	   g := g.next
	ENDWHILE

	 /* size of methods vardebug (1.5.1) */
	proc := g_methodlist
	WHILE proc
	   strsize := strsize + StrLen(proc.name) + 1
	   strsize := strsize + StrLen(proc.object.name) + 1
	   var := proc.varchain
	   WHILE var
	      size := size + SIZEOF vardbg
	      strsize := strsize + StrLen(var.hln.name) + 1
	      IF var.type.object THEN strsize := strsize + StrLen(var.type.object.name) + 1
	      var := var.varchain
	   ENDWHILE
	   proc := proc.next
	ENDWHILE

	size := size + g_symbolssize + 3 AND -4
	strsize := strsize + 3 AND -4

ENDPROC size, strsize

-> 1.6.0
PROC genVardebug(lptr:PTR TO LONG, strsize)
	DEF vhead:PTR TO vdbghead
	DEF vd:PTR TO vardbg
	DEF mod:PTR TO moduleheader
	DEF str[300]:STRING
	DEF t, var:PTR TO var, x, g:PTR TO gvar, proc:PTR TO proc
	DEF stc:strtabcontext, info:PTR TO modinfo

	 #ifdef DBG_EXE
	DEBUGF('genVardebug($\h, \d)\n', lptr, strsize)
	#endif

	mod := g_modulelist.head
	WHILE mod.succ
	   IF mod.debuginfo
	      vhead := lptr
	      vhead.vdbgid := "VDBG"
	      vhead.rsrvd := NIL
	      StrCopy(str, mod.mname, StrLen(mod.mname)-2)
	      StrAdd(str, '.e')
	      vhead.namelongs := Shr(EstrLen(str) + 4, 2)
	      lptr := vhead + SIZEOF vdbghead
	      CopyMemQuick(str, lptr, t := Shl(vhead.namelongs, 2))
	      lptr := lptr + t
	      info := mod + mod.strtabinfo
	      vhead.strtablongs := Shr(info.misc, 2)
	      CopyMemQuick(info + SIZEOF modinfo, lptr, t := Shl(vhead.strtablongs, 2))
	      lptr := lptr + t
	      info := mod + mod.debuginfo
	      vhead.numdebugs := info.count
	      CopyMemQuick(info + SIZEOF modinfo, lptr, t := Mul(vhead.numdebugs, SIZEOF vardbg))
	      lptr := lptr + t
	   ENDIF
	   mod := mod.succ
	ENDWHILE

	vhead := lptr
	vhead.vdbgid := "VDBG"
	vhead.rsrvd := NIL
	vhead.namelongs := Shr(EstrLen(g_sourcename) + 4, 2)
	lptr := vhead + SIZEOF vdbghead
	CopyMem(g_sourcename, lptr, t := Shl(vhead.namelongs, 2))
	lptr := lptr + t
	vhead.strtablongs := Shr(strsize, 2)
	stc.memory := lptr
	stc.count := 0
	stc.offset := 0
	lptr := lptr + Shl(vhead.strtablongs, 2)
	vd := lptr

	proc := g_proclist
	WHILE proc
	   IF proc.ltype = LTYPE_PROC
	      t := getTableStrOfs(stc,proc.name, TRUE)
	      var := proc.varchain
	      WHILE var
	         setVD(vd, stc, var, t, VDSCOPE_PROC, NIL)
	         vd++
	         var := var.varchain
	      ENDWHILE
	   ENDIF
	   proc := proc.next
	ENDWHILE

	proc := g_methodlist
	WHILE proc
	   x := getTableStrOfs(stc, proc.object.name, TRUE)
	   t := getTableStrOfs(stc, proc.name, TRUE)
	   var := proc.varchain
	   WHILE var
	      setVD(vd, stc, var, t, VDSCOPE_PROC, x)
	      vd++
	      var := var.varchain
	   ENDWHILE
	   proc := proc.next
	ENDWHILE

	g := g_gvarlist
	WHILE g
	   IF g.usage -> 1.7.1
	      IF g.identID = IDENT_VARIABLE  -> avoid dummy execbase,etc
	         IF g.gtype = GTYPE_INTERNAL
	            setVD(vd, stc, g, NIL, VDSCOPE_INTERN, NIL)
	         ELSE
	            setVD(vd, stc, g, NIL, VDSCOPE_GLOBAL, NIL)
	         ENDIF
	         vd++
	      ENDIF
	   ENDIF
	   g := g.next
	ENDWHILE

	vhead.numdebugs := Div(vd - lptr, SIZEOF vardbg)

	lptr := vd

	lptr[]++ := NIL

ENDPROC lptr

-> v48
PROC makeRAW(relocateaddr, toaddr)
	DEF mod:REG PTR TO moduleheader
	DEF lptr:PTR TO LONG, info:PTR TO modinfo, i2:PTR TO modinfo
	DEF t, str[100]:STRING
	DEF a
	DEF codelab:PTR TO codelab
	DEF buffer, wptr:REG PTR TO LONG, bufsize=0

	#ifdef DBG_EXE
	DEBUGF('makeRAW()\n')
	#endif

	bufsize := bufsize + link_codesize + (g_codeptr - g_codebuf)-> total size of code section

	/* allocate buffer */


	buffer := IF toaddr THEN toaddr ELSE NEWMEMR(bufsize)
	wptr := buffer

	#ifdef DBG_EXE
	DEBUGF('raw exe: \d bytes allocated\n', bufsize)
	#endif

	/* write to buffer */

	IF g_optpowerpc = CPU_PPC
	   wptr[]++ := Shl(18,26) OR g_startupcodelab.offset  -> B startup
	   wptr[]++ := NIL
	ELSE
	   wptr[]++ := $4E7160FF
	   wptr[]++ := g_startupcodelab.offset-4  -> BRA.L startup
	ENDIF

	-> modulecode
	mod := g_modulelist.head
	WHILE mod.succ
	   info := mod.codeinfo + mod
	   CopyMemQuick(info + SIZEOF modinfo, wptr, Shl(info.count,2))
	   wptr := wptr + Shl(info.count,2)
	   mod := mod.succ
	ENDWHILE

	-> maincode and ifunc-code, globinit, etc
	CopyMem(g_codebuf, wptr, (a := g_codeptr - g_codebuf))
	wptr := wptr + (a + 3 AND $FFFFFC)

	IF relocateaddr > -1  -> v48
	   IF relocateaddr > 0
	      lptr := link_reloc32list
	      WHILE lptr
	         PutLong(buffer + lptr[1], Long(buffer + lptr[1]) + relocateaddr)
	         lptr := lptr[]
	      ENDWHILE
	   ENDIF
	ELSE
	   IF link_reloc32list THEN reportErr('raw exe cannot have relocation data')
	ENDIF



	IF g_showbuf THEN WriteF('OUTBUF: \d of \d bytes used\n', wptr-buffer, bufsize)


ENDPROC buffer, wptr-buffer


-> v47
PROC makeLibMod()
	DEF entry:PTR TO entry
	DEF mod:PTR TO moduleheader
	DEF lib:PTR TO modlibi
	DEF lfunc:PTR TO modlfunc
	DEF libinfo:PTR TO modinfo, strinfo:PTR TO modinfo
	DEF libsize=0, strsize=0, a, b, larg:PTR TO lfuncarg, parg:PTR TO var
	DEF basename[200]:STRING
	DEF stc:strtabcontext, hln:PTR TO hln

	libsize := SIZEOF modlibi
	strsize := 1024  -> for this and that

	#ifdef DBG_BMOD
	DEBUGF('writeLibMod()\n')
	#endif



	FOR a := 0 TO g_nrofentries-1
	   entry := g_entrytable[a]
	   IF entry.prochln -> 1.6.0 may be empty
	      libsize := libsize + SIZEOF modlfunc
	      strsize := strsize + StrLen(entry.name) + 1
	      FOR b := 0 TO entry.nrofargs-1
	         libsize := libsize + SIZEOF lfuncarg
	         ->arg := entry.regs[b]
	         -> 1.10.0 fix
	         hln := entry.prochln
	         IF hln
	            parg := hln.ident::proc.argarray[b]
	            -> parg.identID is patched to point to name of arg if from module, else IDENT_VARIABLE
	            strsize := strsize + StrLen(IF parg.identID > 1000 THEN parg.identID ELSE parg.hln.name) + 1
	         ENDIF
	      ENDFOR
	   ENDIF
	ENDFOR

	libsize := libsize + 3 AND -4
	strsize := strsize + 3 AND -4

	a := SIZEOF moduleheader + SIZEOF modinfo + libsize + SIZEOF modinfo + strsize


	mod := NEWMEMR(a)

	#ifdef DBG_BMOD
	DEBUGF('   allocated \d bytes fro mod\n', a)
	#endif

	mod.identification := "ECXM"
	mod.headsize := SIZEOF moduleheader
	mod.libiinfo := SIZEOF moduleheader
	mod.strtabinfo := mod.libiinfo + SIZEOF modinfo + libsize
	libinfo := mod + mod.libiinfo
	strinfo := mod + mod.strtabinfo

	stc.memory := strinfo + SIZEOF modinfo
	stc.offset := NIL
	stc.count := 0

	getTableStrOfs(stc, '---')

	lib := libinfo + SIZEOF modinfo
	libinfo.count := 1
	a := InStr(g_libraryname, '.')
	StrCopy(basename, g_libraryname, a)
	StrAdd(basename, 'base')
	lib.basename := getTableStrOfs(stc, basename)
	lfunc := lib + SIZEOF modlibi
	FOR a := 0 TO g_nrofentries-1
	   entry := g_entrytable[a]
	   hln := entry.prochln
	   IF hln
	      entry.name[] := entry.name[] - 32 -> make uppercase
	      lfunc.name := getTableStrOfs(stc, entry.name)
	      lfunc.nrofargs := entry.nrofargs
	      lfunc.type := entry.type
	      lfunc.baseofs := entry.baseofs
	      lfunc.basernum := entry.basernum
	      lfunc.basertype := entry.basertype
	      lfunc.return := entry.return
	      lib.nroflfuncs := lib.nroflfuncs + 1 -> 1.6.0
	      larg := lfunc + SIZEOF modlfunc
	      FOR b := 0 TO entry.nrofargs-1
	         parg := hln.ident::proc.argarray[b]
	         larg.name := getTableStrOfs(stc, IF parg.identID > 1000 THEN parg.identID ELSE parg.hln.name, TRUE) -> fixed 1.10.0
	         larg.rtype := entry.regs[b].rtype
	         larg.rnum := entry.regs[b].rnum
	         larg++
	      ENDFOR
	      lfunc.totalsize := larg - lfunc
	      lfunc := larg
	   ENDIF
	ENDFOR
	lib.totalsize := larg - lib
	mod.modsize := strinfo - mod + SIZEOF modinfo + stc.offset + 3 AND -4
	mod.modversion := MODULEVERSION

	strinfo.count := stc.count
	strinfo.misc := stc.offset

	mod.osid := g_optosid -> 2.0

	#ifdef DBG_BMOD
	DEBUGF('   writing mod (\d bytes)\n', mod.modsize)
	#endif

ENDPROC mod, mod.modsize

-> 1.7.1 THE AMAZING LINKOBJECT CREATOR
PROC makeLINKOBJ()
	DEF ehdr:PTR TO ehdr
	DEF shdr:PTR TO shdr
	DEF sym:PTR TO sym
	DEF rel:REG PTR TO rela
	DEF mod:PTR TO moduleheader, b, ifunc:PTR TO lif, info:PTR TO modinfo
	DEF lptr:REG PTR TO LONG
	DEF buffer, wptr:REG PTR TO LONG, bufsize
	DEF labref:PTR TO labref
	DEF t=NIL, a=NIL
	DEF codestart:PTR TO LONG
	DEF reltable:PTR TO rel, symtable:PTR TO sym, strtable, shdrtable
	DEF shstrtable
	DEF gvar:PTR TO gvar, proc:PTR TO proc, const:PTR TO const
	DEF hln:PTR TO hln
	DEF str[300]:STRING
	DEF constsyms=0, gvarsyms=0, procsyms=0, datatable=0, ifuncsyms=0
	DEF rwref:PTR TO rwref, xcode:PTR TO codelab, xcodesyms=0



	-> ehdr
	-> code
	-> sym table  ARRAY OF sym (dummy,code,data,bss,symbols...)
	-> rela table [1+nrrelocs32+glob+drel+xrefglob+xcode]:ARRAY OF rela
	-> strtable    ...\0
	-> shstrtable  \0.text\0.rel.text\0.shstrtab\0.symtab\0.strtab\0.data\0.sbss\0
	-> data
	-> shdr0 dummy
	-> shdr1 .text (code)
	-> shdr2 .rela.text
	-> shdr3 .shstrtab
	-> shdr4 .symtab
	-> shdr5 .strtab
	-> shdr6 .data
	-> shdr7 .sbss

	bufsize := SIZEOF ehdr
	bufsize := bufsize + Mul(SIZEOF shdr,7)
	bufsize := bufsize + (g_codeptr - g_codebuf) -> total size of code section
	bufsize := bufsize + g_databufsize -> total size of rwdata (immedlists)
	bufsize := bufsize + Mul(link_nrofreloc32s, SIZEOF rela)
	bufsize := bufsize + Mul(3, SIZEOF sym)
	bufsize := bufsize + g_symbolssize + 1000

	gvar := g_gvarlist
	WHILE gvar
	   labref := gvar.labrefs
	   WHILE labref
	      bufsize := bufsize + SIZEOF rela
	      labref := labref.next
	   ENDWHILE
	   IF gvar.export THEN bufsize := bufsize + SIZEOF sym
	   gvar := gvar.next
	ENDWHILE

	proc := g_proclist
	WHILE proc
	   bufsize := bufsize + SIZEOF sym
	   proc := proc.next
	ENDWHILE

	rwref := g_rwreflist
	WHILE rwref
	   bufsize := bufsize + (2 * SIZEOF rela)
	   rwref := rwref.next
	ENDWHILE

	a := 0
	WHILE ifunc := g_internalfuncsppc[a++]
	   labref := ifunc.labrefs
	   IF labref THEN bufsize := bufsize + SIZEOF sym
	   WHILE labref
	      bufsize := bufsize + SIZEOF rela
	      labref := labref.next
	   ENDWHILE
	ENDWHILE

	xcode := g_xcodelist
	WHILE xcode
	   labref := xcode.labrefs
	   IF labref THEN bufsize := bufsize + SIZEOF sym
	   WHILE labref
	      bufsize := bufsize + SIZEOF rela
	      labref := labref.next
	   ENDWHILE
	   xcode := xcode.codelink
	ENDWHILE

	/* allocate buffer */

	bufsize := bufsize + 1000 -> safety

	#ifdef DBG_EXE
	DEBUGF('allocating \d bytes for linkobj (elf)\n', bufsize)
	#endif

	buffer := NEWMEMR(bufsize)

	wptr := buffer

	/* header */
	ehdr := wptr
	ehdr.ident[EI_MAG0] := $7F
	ehdr.ident[EI_MAG1] := "E"
	ehdr.ident[EI_MAG2] := "L"
	ehdr.ident[EI_MAG3] := "F"
	ehdr.ident[EI_CLASS] := ELFCLASS32
	ehdr.ident[EI_DATA] := ELFDATA2MSB
	ehdr.ident[EI_VERSION] := $01
	ehdr.ident[EI_PAD] := $00
	ehdr.type := ET_REL
	ehdr.machine := EM_PPC
	ehdr.version := 1
	ehdr.phoff := 0
	ehdr.shoff := 0 -> set later !
	ehdr.ehsize := SIZEOF ehdr
	ehdr.phentsize := 0
	ehdr.phnum := 0
	ehdr.shentsize := SIZEOF shdr
	ehdr.shnum := 0 -> set later !
	ehdr.shstrndx := 3 -> shstrtab
	wptr := wptr + SIZEOF ehdr

	codestart := wptr


	/* maincode */
	CopyMemQuick(g_codebuf, wptr, a := g_codeptr - g_codebuf)
	wptr := wptr + a

	/* sym table */
	symtable := wptr
	sym := symtable

	sym++ -> emty symbol first

	sym.name := NIL
	sym.value := 0
	sym.size := 0
	sym.info := ST_INFO(STB_LOCAL, STT_SECTION)
	sym.other := 0
	sym.shndx := 1 -> code section
	sym++

	sym.name := NIL
	sym.value := 0
	sym.size := 0
	sym.info := ST_INFO(STB_LOCAL, STT_SECTION)
	sym.other := 0
	sym.shndx := 6 -> data section
	sym++

	sym.name := NIL
	sym.value := 0
	sym.size := 0
	sym.info := ST_INFO(STB_LOCAL, STT_SECTION)
	sym.other := 0
	sym.shndx := 7 -> sbss section
	sym++

	StringF(str, '\s \s (\s)', g_compiledwithstr,
	                            g_verstr,
	                            g_platformstr,
	                            )
	t := t + EstrLen(str) + 1

	-> exported symbols (const/def/proc) here
	-> t := 4

	constsyms := sym
	const := g_econstlist
	WHILE const
	   sym.name := t
	   t := t + EstrLen(const.name) + 1
	   sym.value := const.value
	   sym.size := 0
	   sym.info := ST_INFO(STB_GLOBAL, STT_COMMON)
	   sym.other := 0
	   sym.shndx := SHN_ABS
	   sym++
	   const := const.next
	ENDWHILE

	gvarsyms := sym
	gvar := g_gvarlist
	WHILE gvar
	   IF gvar.export
	      IF gvar.labrefs
	         sym.name := t
	         IF gvar.xname = NIL THEN gvar.xname := gvar.hln.name
	         t := t + EstrLen(gvar.xname) + 1
	         sym.value := NIL
	         sym.size := gvar.type.size
	         sym.info := ST_INFO(STB_GLOBAL, STT_NOTYPE)
	         sym.other := 0
	         sym.shndx := 0
	         sym++
	      ENDIF
	   ENDIF
	   gvar := gvar.next
	ENDWHILE

	procsyms := sym
	proc := g_proclist
	WHILE proc
	   IF proc.exported
	      sym.name := t
	      t := t + EstrLen(proc.name) + 1
	      sym.value := proc.offset
	      sym.size := 0
	      sym.info := ST_INFO(STB_GLOBAL, STT_NOTYPE)
	      sym.other := 0
	      sym.shndx := 1
	      sym++
	   ENDIF
	   proc := proc.next
	ENDWHILE

	ifuncsyms := sym
	a := 0
	WHILE ifunc := g_internalfuncsppc[a++]
	   IF ifunc.labrefs
	      sym.name := t
	      t := t + EstrLen(ifunc.name) + 1
	      sym.value := NIL
	      sym.size := 0
	      sym.info := ST_INFO(STB_GLOBAL, STT_NOTYPE)
	      sym.other := 0
	      sym.shndx := 0
	      sym++
	   ENDIF
	ENDWHILE

	xcodesyms := sym
	xcode := g_xcodelist
	WHILE xcode
	   IF xcode.labrefs
	      sym.name := t
	      t := t + EstrLen(xcode.name) + 1
	      sym.value := NIL
	      sym.size := 0
	      sym.info := ST_INFO(STB_GLOBAL, STT_NOTYPE)
	      sym.other := 0
	      sym.shndx := 0
	      sym++
	   ENDIF
	   xcode := xcode.codelink
	ENDWHILE

	wptr := sym

	 /* rel table */
	reltable := wptr
	rel := reltable
	-> put reloc32s
	lptr := link_reloc32list
	t := R_INFO(1,R_PPC_ADDR32)
	WHILE lptr
	   rel.offset := lptr[1]
	   rel.info := t
	   rel.addend := Long(g_codebuf+lptr[1])
	   rel++
	   lptr := lptr[]
	ENDWHILE
	-> put private and xref globals reloc
	gvar := g_gvarlist
	a := 0
	WHILE gvar
	   IF (gvar.export<>NIL) AND (gvar.labrefs<>NIL)
	      t := R_INFO(a+(gvarsyms-symtable/SIZEOF sym), R_PPC_SDAREL16)
	      labref := gvar.labrefs
	      WHILE labref
	         rel.offset := labref.offset
	         rel.info := t
	         rel.addend := NIL
	         rel++
	         labref := labref.next
	      ENDWHILE
	      a++
	   ELSE
	      t := R_INFO(3, R_PPC_SDAREL16)
	      labref := gvar.labrefs
	      WHILE labref
	         rel.offset := labref.offset
	         rel.info := t
	         rel.addend := Int(g_codebuf+labref.offset)
	         rel++
	         labref := labref.next
	      ENDWHILE
	   ENDIF
	   gvar := gvar.next
	ENDWHILE

	-> put immedlists reloc

	rwref := g_rwreflist
	IF g_optosid = OSID_MORPHOS
	   b := R_PPC_MORPHOS_DREL_LO
	   t := R_PPC_MORPHOS_DREL_HA
	ELSE
	   b := R_PPC_AMIGAOS_BREL_LO
	   t := R_PPC_AMIGAOS_BREL_HA
	ENDIF

	WHILE rwref
	   -> each rwref is two elfrelocs, low + high
	   rel.offset := rwref.offset + 2
	   a := Word(g_codebuf+rel.offset) OR Shl(Word(g_codebuf+rel.offset+4), 16)
	   rel.info := R_INFO(2, b)
	   rel.addend := a
	   rel++
	   rel.offset := rwref.offset + 6
	   rel.info := R_INFO(2, t)
	   rel.addend := a
	   rel++
	   rwref := rwref.next
	ENDWHILE

	-> put ifunc xref relocs
	a := 0
	b := 0
	WHILE ifunc := g_internalfuncsppc[a]
	   labref := ifunc.labrefs
	   IF labref
	      t := R_INFO(b+(ifuncsyms-symtable/SIZEOF sym), R_PPC_REL24)
	      WHILE labref
	         rel.offset := labref.offset
	         rel.info := t
	         rel.addend := NIL
	         rel++
	         labref := labref.next
	      ENDWHILE
	      b++
	   ENDIF
	   a++
	ENDWHILE

	-> put xcode relocs
	a := 0
	xcode := g_xcodelist
	WHILE xcode
	   labref := xcode.labrefs
	   IF labref
	      t := R_INFO(a+(xcodesyms-symtable/SIZEOF sym), R_PPC_REL24)
	      WHILE labref
	         rel.offset := labref.offset
	         rel.info := t
	         rel.addend := NIL
	         rel++
	         labref := labref.next
	      ENDWHILE
	      a++
	   ENDIF
	   xcode := xcode.codelink
	ENDWHILE

	wptr := rel



	/* strtable */
	strtable := wptr

	-> exported/imported symbols to strtable here.
	wptr := wptr + 4

	-> compiled with string..
	CopyMem(str, wptr, t := EstrLen(str) + 1)
	wptr := wptr + t

	const := g_econstlist
	WHILE const
	   CopyMem(const.name, wptr, t := EstrLen(const.name) + 1)
	   wptr := wptr + t
	   const:= const.next
	ENDWHILE

	gvar := g_gvarlist
	WHILE gvar
	   IF gvar.export
	      IF gvar.labrefs
	         CopyMem(gvar.xname, wptr, t := EstrLen(gvar.xname) + 1)
	         wptr := wptr + t
	      ENDIF
	   ENDIF
	   gvar := gvar.next
	ENDWHILE

	proc := g_proclist
	WHILE proc
	   IF proc.exported
	      CopyMem(proc.name, wptr, t := EstrLen(proc.name) + 1)
	      wptr := wptr + t
	   ENDIF
	   proc := proc.next
	ENDWHILE

	a := 0
	WHILE ifunc := g_internalfuncsppc[a]
	   IF ifunc.labrefs
	      CopyMem(ifunc.name, wptr, t := EstrLen(ifunc.name) + 1)
	      wptr := wptr + t
	   ENDIF
	   a++
	ENDWHILE

	xcode := g_xcodelist
	WHILE xcode
	   IF xcode.labrefs
	      CopyMem(xcode.name, wptr, t := EstrLen(xcode.name) + 1)
	      wptr := wptr + t
	   ENDIF
	   xcode := xcode.codelink
	ENDWHILE

	wptr := wptr + 3 AND -4

	/* shstrtable */
	shstrtable := wptr
	CopyMem('\0.text\0.rela.text\0.shstrtab\0.symtab\0.strtab\0.data\0.sbss\0', wptr, STRLEN)
	wptr := wptr + STRLEN
	wptr := wptr + 3 AND -4

	datatable := wptr
	CopyMemQuick(g_databuf, datatable, t := g_databufsize)
	wptr := wptr  + t

	shdrtable := wptr
	shdr := shdrtable
	ehdr.shoff := shdr - ehdr

	/* dummy section (0) */

	shdr++

	/* code section (1) */

	shdr.name := 1
	shdr.type := SHT_PROGBITS
	shdr.flags := SHF_ALLOC OR SHF_EXECINSTR
	shdr.addr := 0
	shdr.offset := codestart - ehdr
	shdr.size := symtable - codestart
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 16
	shdr.entsize := 0
	shdr++

	/* rela section (2) */

	shdr.name := 7
	shdr.type := SHT_RELA
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := reltable - ehdr
	shdr.size := strtable - reltable
	shdr.link := 4 -> sym section
	shdr.info := 1 -> code section
	shdr.addralign := 4
	shdr.entsize := SIZEOF rela
	shdr++


	/* shstrtab section (3) */

	shdr.name := 18
	shdr.type := SHT_STRTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := shstrtable - ehdr
	shdr.size := 56
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 1
	shdr.entsize := 0
	shdr++

	/* symtab section (4) */

	shdr.name := 28
	shdr.type := SHT_SYMTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := symtable - ehdr
	shdr.size := reltable - symtable
	shdr.link := 5 -> strtable
	shdr.info := 1
	shdr.addralign := 4
	shdr.entsize := SIZEOF sym
	shdr++

	/* strtab section (5) */

	shdr.name := 36
	shdr.type := SHT_STRTAB
	shdr.flags := NIL
	shdr.addr := 0
	shdr.offset := strtable - ehdr
	shdr.size := shstrtable - strtable
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 1
	shdr.entsize := 0
	shdr++

	/* data section (6) */

	shdr.name := 44
	shdr.type := SHT_PROGBITS
	shdr.flags := SHF_ALLOC OR SHF_WRITE
	shdr.addr := 0
	shdr.offset := datatable - ehdr
	shdr.size := shdrtable - datatable
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 16
	shdr.entsize := 0
	shdr++

	/* sbss section (7) */

	shdr.name := 50
	shdr.type := NIL
	shdr.flags := SHF_ALLOC OR SHF_WRITE
	shdr.addr := 0
	shdr.offset := NIL
	shdr.size := g_globalsize
	shdr.link := 0
	shdr.info := 0
	shdr.addralign := 16
	shdr.entsize := 0
	shdr++

	ehdr.shnum := shdr - shdrtable / SIZEOF shdr

	wptr := shdr

	IF g_showbuf THEN WriteF('OUTBUF: \d of \d bytes used\n', wptr-buffer, bufsize)


ENDPROC buffer, wptr-buffer


/*************************************************************
************ MODULE CREATE ***********************************
*************************************************************/
-> v49
PROC mret2return(mret:PTR TO multireturn)
	DEF t, r1, r2, r3, r4
	t := mret.ros[0]
	SELECT t
	CASE DREG ; r1 := 0
	CASE FREG ; r1 := 1
	CASE VREG ; r1 := 2
	CASE X2R  ; r1 := 3
	DEFAULT   ; reportIErr('mret2return 0 t=?')
	ENDSELECT
	t := mret.ros[1]
	SELECT t
	CASE DREG ; r2 := 0
	CASE FREG ; r2 := 1
	CASE VREG ; r2 := 2
	CASE X2R  ; r2 := 3
	DEFAULT   ; reportIErr('mret2return 1 t=?')
	ENDSELECT
	t := mret.ros[2]
	SELECT t
	CASE DREG ; r3 := 0
	CASE FREG ; r3 := 1
	CASE VREG ; r3 := 2
	CASE X2R  ; r3 := 3
	DEFAULT   ; reportIErr('mret2return 2 t=?')
	ENDSELECT
	t := mret.ros[3]
	SELECT t
	CASE DREG ; r4 := 0
	CASE FREG ; r4 := 1
	CASE VREG ; r4 := 2
	CASE X2R  ; r4 := 3
	DEFAULT   ; reportIErr('mret2return 3 t=?')
	ENDSELECT
ENDPROC MAKE_MODPROC_RETURN(r1,r2,r3,r4)


PROC makeModule()
	DEF mod:PTR TO moduleheader -> the created module

	DEF allocsize=0

	DEF macro:PTR TO macro, mmacro:PTR TO modmacro
	DEF c:PTR TO const
	DEF obj:PTR TO object, mobj:PTR TO modobject, mcl:PTR TO modclass
	DEF memb:PTR TO member, mmemb:PTR TO modmember
	DEF meth:PTR TO proc, mmeth:PTR TO modproc
	DEF proc:PTR TO proc, mproc:PTR TO modproc
	DEF lptr:PTR TO LONG, iptr:PTR TO INT, cptr:PTR TO CHAR
	DEF a, b, t, x
	DEF var:PTR TO var
	DEF par:PTR TO var, mpar:PTR TO modarg
	DEF lfunc:PTR TO lfunc, mlfunc:PTR TO modlfunc
	DEF vd:PTR TO vardbg
	DEF hln:PTR TO hln
	DEF codelab:PTR TO codelab, labref:PTR TO labref
	DEF wptr:PTR TO LONG, rwref:PTR TO rwref, ld:PTR TO linedef
	DEF g:PTR TO gvar, ifunc:PTR TO lif
	DEF m:PTR TO moduleheader -> temp
	DEF info:PTR TO modinfo
	DEF mmodule:PTR TO modmodule, mref:PTR TO modref
	DEF stc:strtabcontext -> 1.6.0
	DEF xglobderef:PTR TO xglobderef
	DEF immglobinitused=FALSE -> 2.3.1

	#ifdef DBG_BMOD
	DEBUGF('makeModule()\n')
	#endif

	/* begin with gathering size information for allocation */

	#ifdef DBG_BMOD
	DEBUGF('sizeof macros\n')
	#endif

	/* compute size of macrohunks */
	macro := g_emacrolist
	WHILE macro
	   allocsize := allocsize +
	   SIZEOF modmacro +
	   EstrLen(macro.ascii) + 1 + 3 AND $FFFC +
	   IF Odd(macro.type) THEN Mul(4,macro.nrofargs) ELSE 0
	   macro := macro.next
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof procs\n')
	#endif

	/* compute size of prochunks */
	proc := g_proclist
	WHILE proc
	   IF proc.ltype = LTYPE_PROC
	      IF proc.object = NIL THEN allocsize := allocsize + SIZEOF modproc
	      allocsize := allocsize + Mul(SIZEOF modarg, proc.nrofargs)
	      IF g_vardebug
	         var := proc.varchain
	         WHILE var
	            allocsize := allocsize + SIZEOF vardbg
	            var := var.varchain
	         ENDWHILE
	      ENDIF
	   ELSEIF proc.ltype = LTYPE_LAB
	      allocsize := allocsize + SIZEOF modlabel
	   ELSEIF proc.ltype = LTYPE_STATIC
	      allocsize := allocsize + SIZEOF modstatic
	   ENDIF
	   proc := proc.next
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof objects\n')
	#endif

	/* compute size of objecthunks */
	obj := g_objectlist
	WHILE obj
	   IF obj.exported
	      allocsize := allocsize + SIZEOF modobject
	      FOR b := 0 TO obj.nrofmembers-1
	         memb := obj.membertable[b]
	         IF memb.flags AND MEMBF_PRIVATE = FALSE
	            allocsize := allocsize + SIZEOF modmember
	         ENDIF
	      ENDFOR
	      IF obj.nrofmethods THEN allocsize := allocsize + SIZEOF modclass
	      FOR b := 0 TO obj.nrofmethods-1
	         meth := obj.methodtable[b]
	         allocsize := allocsize + SIZEOF modproc + Mul(meth.nrofargs, SIZEOF modarg)
	      ENDFOR
	   ENDIF
	   obj := obj.next
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof consts\n')
	#endif

	/* compute size of consthunk and reverese order */
	c := g_econstlist
	g_econstlist := NIL
	WHILE c
	   t := c.next
	   allocsize := allocsize + SIZEOF const
	   c.next := g_econstlist
	   g_econstlist := c
	   c := t
	ENDWHILE


	#ifdef DBG_BMOD
	DEBUGF('sizeof submods\n')
	#endif

	/* compute size of submod(s) */
	m := g_modulelist.head
	WHILE m.succ
	   codelab := m.modextens.codelist -> procedures, labels, classes
	   WHILE codelab
	      labref := codelab.labrefs
	      IF labref
	         allocsize := allocsize + SIZEOF modref
	                                                   ->  info:CHAR, numoffsets:INT]
	         m.modextens.referenced := TRUE
	      ENDIF
	      WHILE labref
	         allocsize := allocsize + 4
	         labref := labref.next
	      ENDWHILE
	      codelab := codelab.codelink
	   ENDWHILE
	   IF m.modextens.referenced THEN allocsize := allocsize + SIZEOF modmodule ->submodsize := submodsize + SIZEOF modmodule -> [(-)name:LONG,totalsize:LONG]
	   m := m.succ
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof globals drelg + xref \n')
	#endif

	/* size of globals */
	g := g_gvarlist
	WHILE g
	   IF g.export
	      IF (g.labrefs<>NIL) OR (g.gtype = GTYPE_DEF) OR (g.gtype = GTYPE_BASE)
	         allocsize := allocsize + SIZEOF modref ->xrefgsize := xrefgsize + 8 -> [name:LONG, numoffsets:LONG]
	         labref := g.labrefs
	         WHILE labref
	            allocsize := allocsize + 4 ->xrefgsize := xrefgsize + 4 -> offset:LONG
	            labref := labref.next
	         ENDWHILE
	         IF g.type.esize > 1
	            allocsize := allocsize + SIZEOF xglobderef -> 2.2
	         ENDIF
	      ENDIF
	   ELSE
	      allocsize := allocsize + 4
	      labref := g.labrefs
	      WHILE labref
	         allocsize := allocsize + 4
	         labref := labref.next
	      ENDWHILE
	      IF (g.defo) AND (g.defd<>NIL) THEN immglobinitused := TRUE -> 2.3.1
	   ENDIF
	   g := g.next
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof rwrefs\n')
	#endif

	/* size of data relocs */
	rwref := g_rwreflist
	WHILE rwref
	   allocsize := allocsize + 4 ->dreldsize := dreldsize + 4 -> offset:LONG
	   rwref := rwref.next
	ENDWHILE

	#ifdef DBG_BMOD
	DEBUGF('sizeof ifuncs\n')
	#endif

	/* size of ifuncs */
	lptr := IF g_optpowerpc = CPU_PPC THEN g_internalfuncsppc ELSE g_internalfuncs
	IF lptr = NIL THEN lptr := [NIL]
	WHILE (ifunc := lptr[]++)
	   labref := ifunc.labrefs
	   IF labref
	      allocsize := allocsize + SIZEOF modref
	   ENDIF
	   WHILE labref
	      allocsize := allocsize + 4 -> offset:LONG
	      labref := labref.next
	   ENDWHILE
	ENDWHILE

	   #ifdef DBG_BMOD
	DEBUGF('sizeof linedebug\n')
	#endif

	/* size of line debug */
	IF g_linedebug
	   t := countReverseLinelist()
	   allocsize := allocsize + Mul(t, 8)
	ENDIF


	/* size of code */
	allocsize := allocsize + (g_codeptr - g_codebuf)

	/* size of data */
	allocsize := allocsize + g_databufsize

	IF g_vardebug
	   /* add size of global vars vardebug */
	   g := g_gvarlist
	   WHILE g
	      allocsize := allocsize + SIZEOF vardbg
	      g := g.next
	   ENDWHILE

	    /* size of methods vardebug (1.5.1) */
	   proc := g_methodlist
	   WHILE proc
	      var := proc.varchain
	      WHILE var
	         allocsize := allocsize + SIZEOF vardbg
	         var := var.varchain
	      ENDWHILE
	      proc := proc.next
	   ENDWHILE
	ENDIF

	#ifdef DBG_BMOD
	DEBUGF('resolve and relocsize\n')
	#endif

	allocsize := allocsize + Mul(4, link_nrofreloc32s)

	#ifdef DBG_BMOD
	DEBUGF('allocate\n')
	#endif

	/* allocate module */

	g_symbolssize := Mul(g_symbolssize,2) + 1000 -> some extra here..1.8.0 added Mul 2

	t := SIZEOF moduleheader + allocsize + Mul(SIZEOF modinfo,25)
	mod := NEWMEMR(t + g_symbolssize)

	#ifdef DBG_BMOD
	DEBUGF('allocated \d + \d bytes for binary\n', t, g_symbolssize)
	#endif

	mod.strtabinfo := t

	stc.memory := mod + mod.strtabinfo + SIZEOF modinfo
	stc.offset := NIL
	stc.count := 0

	getTableStrOfs(stc, '---')

	wptr := mod + SIZEOF moduleheader

	mod.modsize := t + g_symbolssize-> we correct it later..

	mod.headsize := SIZEOF moduleheader

	mod.modversion := MODULEVERSION

	/* write procedures/labels */
	#ifdef DBG_BMOD
	DEBUGF('write procinfo\n')
	#endif
	info := wptr
	wptr := info + SIZEOF modinfo
	info.count := 0
	mproc := wptr
	proc := g_proclist
	WHILE proc
	   IF proc.exported
	      IF proc.ltype = LTYPE_PROC
	         info.count := info.count + 1
	         mproc.name := getTableStrOfs(stc, proc.name)
	         mproc.nrofargs := proc.nrofargs
	         mproc.nrofdefaults := proc.nrofdefaults
	         mproc.offset := proc.offset
	         mproc.return := mret2return(proc.mret) -> v49
	         mpar := mproc + SIZEOF modproc
	         FOR a := 0 TO proc.nrofargs-1
	            par := proc.argarray[a]
	            CopyMem(par, mpar, SIZEOF modarg)
	            mpar.name := getTableStrOfs(stc, par.hln.name,TRUE)
	            mpar.rsrvd := NIL
	            mpar++
	         ENDFOR
	         mproc.totalsize := mpar - mproc
	         mproc := mproc + mproc.totalsize
	      ELSEIF proc.ltype = LTYPE_LAB
	         info.count := info.count + 1
	         mproc.totalsize := SIZEOF modlabel
	         mproc.name := getTableStrOfs(stc, proc.name)
	         mproc.offset := proc.offset
	         mproc := mproc + mproc.totalsize
	      ELSEIF proc.ltype = LTYPE_STATIC -> 1.9.0
	         info.count := info.count + 1
	         mproc.totalsize := SIZEOF modstatic
	         mproc.name := getTableStrOfs(stc, proc.name)
	         mproc.offset := proc.offset
	         mproc::modstatic.statflags := proc::statlab.deref.flags
	         mproc::modstatic.statesize := proc::statlab.deref.esize
	         mproc::modstatic.keepnil := NIL
	         mproc::modstatic.statobj := IF proc::statlab.deref.esize = 255 THEN
	            getTableStrOfs(stc, proc::statlab.deref.object.name,TRUE) ELSE NIL
	         mproc::modstatic.sizeof := proc::statlab.sizeof -> 2.2
	         mproc := mproc + mproc.totalsize
	      ENDIF
	   ELSEIF g_forcemodsyms -> 1.8.0
	      info.count := info.count + 1
	      mproc.totalsize := SIZEOF modlabel
	      mproc.name := getTableStrOfs(stc, proc.name)
	      mproc.offset := proc.offset
	      mproc := mproc + mproc.totalsize
	   ENDIF
	   proc := proc.next
	ENDWHILE
	wptr := mproc
	IF info.count
	   mod.procinfo := info - mod
	   #ifdef DBG_BMOD
	   DEBUGF('procinfo \d\n', info.count)
	   #endif
	ENDIF

	/* write objects */
	#ifdef DBG_BMOD
	DEBUGF('write objects\n')
	#endif
	info := wptr
	wptr := info + SIZEOF modinfo
	info.count := 0
	mobj := wptr
	obj := g_objectlist
	WHILE obj
	   IF obj.exported
	      info.count := info.count + 1
	      mobj.name := getTableStrOfs(stc,obj.name)
	      mobj.sizeof := obj.sizeof
	      mobj.alignment := obj.alignment
	      mobj.nrofmethods := obj.nrofmethods
	      mobj.flags := obj.flags -> 1.8.1
	      mmemb := mobj + SIZEOF modobject
	      FOR a := 0 TO obj.nrofmembers-1
	         memb := obj.membertable[a]
	         IF (memb.flags AND MEMBF_PRIVATE) = FALSE -> v48
	            mmemb.name := getTableStrOfs(stc,memb.name,TRUE)
	            mmemb.offset := memb.offset
	            mmemb.size := memb.size
	            mmemb.esize := memb.esize
	            mmemb.numes := memb.numes
	            mmemb.flags := memb.flags -> v48
	            IF memb.object < 0   -> v48, we may have unresolved objectpointers by inheritation
	               hln := -memb.object
	               mmemb.object := getTableStrOfs(stc,hln.name,TRUE)
	            ELSEIF memb.object
	               mmemb.object := getTableStrOfs(stc,memb.object.name,TRUE)
	            ENDIF
	            mobj.nrofmembers := mobj.nrofmembers + 1 -> we count *public* members.
	            mmemb++
	         ENDIF
	      ENDFOR
	      mcl := mmemb
	      IF obj.nrofmethods
	         mcl.offset := obj.offset
	         mcl.classofs := obj.classofs
	         mcl.destofs := obj.destofs
	         IF obj.super    -> 1.10.0
	            IF obj.super.startline > 0 -> if in this module
	               mcl.supername := getTableStrOfs(stc, obj.super.name,TRUE)
	            ENDIF
	         ENDIF
	         mmeth := mcl + SIZEOF modclass
	         FOR a := 0 TO obj.nrofmethods-1
	            meth := obj.methodtable[a]
	            IF meth.exported
	               mmeth.nrofargs := meth.nrofargs
	               mmeth.nrofdefaults := meth.nrofdefaults
	               mmeth.offset := meth.offset
	               mmeth.name := getTableStrOfs(stc,meth.name,TRUE)
	               mmeth.selfreg := meth.selfreg
	               mmeth.flags := meth.flags
	               mmeth.return := mret2return(meth.mret)
	               mpar := mmeth + SIZEOF modproc
	               FOR b := 0 TO meth.nrofargs-1
	                  var := meth.argarray[b]
	                  mpar.name := getTableStrOfs(stc,
	                     IF meth.offset THEN var.hln.name ELSE var.identID,  -> 1.9.0 fix
	                     TRUE)
	                  mpar.defval := var.defd
	                  mpar.rtype := var.rtype -> these two, fix v49
	                  mpar.rnum := var.rnum
	                  mpar++
	               ENDFOR
	               mmeth.totalsize := mpar - mmeth -> 1.9.0
	               mmeth := mpar
	            ELSE
	               mmeth.totalsize := SIZEOF modproc -> 1.9.0
	               mmeth := mmeth + SIZEOF modproc  -> dummy method
	            ENDIF
	         ENDFOR
	         mmemb := mmeth
	      ENDIF
	      mobj.totalsize := mmemb - mobj
	      mobj := mobj + mobj.totalsize
	      wptr := mobj
	   ENDIF
	   obj := obj.next
	ENDWHILE
	IF info.count
	   info.misc := 1  -> v49 (1.5) means we have extended information
	   mod.objectinfo := info - mod
	   #ifdef DBG_BMOD
	   DEBUGF('objectinfo \d\n', info.count)
	   #endif
	ENDIF

	   /* write macroinfo */
	   #ifdef DBG_BMOD
	   DEBUGF('write macroinfo\n')
	   #endif
	info := wptr
	wptr := info + SIZEOF modinfo
	info.count := 0
	mmacro := wptr
	macro := g_emacrolist
	WHILE macro
	   info.count := info.count + 1
	   mmacro.name := getTableStrOfs(stc, macro.name)
	   mmacro.nrofargs := macro.nrofargs
	   mmacro.bodysize := EstrLen(macro.ascii) + 1 + 3 AND $FFFC
	   mmacro.type := macro.type
	   -> params
	   wptr := mmacro + SIZEOF modmacro
	   IF Odd(macro.type)
	      FOR b := 0 TO macro.nrofargs-1
	         wptr[]++ := getTableStrOfs(stc, macro.params[b], TRUE)
	      ENDFOR
	   ENDIF
	   -> body
	   CopyMemQuick(macro.ascii, wptr, mmacro.bodysize)
	   wptr := wptr + mmacro.bodysize
	   mmacro.totalsize := wptr - mmacro
	   mmacro := wptr
	   macro := macro.next
	ENDWHILE
	IF info.count
	   ->info.size := wptr - info
	   mod.macroinfo := info - mod
	   #ifdef DBG_BMOD
	   DEBUGF('macroinfo \d\n', info.count)
	   #endif
	ENDIF


	/* write consts */
	#ifdef DBG_BMOD
	DEBUGF('write constinfo\n')
	#endif
	info := wptr
	wptr := info + SIZEOF modinfo
	c := g_econstlist
	t := 0
	WHILE c
	   wptr[]++ := getTableStrOfs(stc, c.name)
	   wptr[]++ := c.value
	   c := c.next
	   t++
	ENDWHILE
	info.count := t
	IF info.count
	   ->info.size := wptr - info
	   mod.constinfo := info - mod
	   #ifdef DBG_BMOD
	   DEBUGF('constinfo \d\n', info.count)
	   #endif
	ENDIF


	/* write submods to block */
	   #ifdef DBG_BMODD
	   DEBUGF('write submodinfos\n')
	   #endif
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   info.count := 0
	   mmodule := wptr
	   m := g_modulelist.head
	   WHILE m.succ
	      IF m.modextens.referenced -> code referenced ?
	         info.count := info.count + 1
	         IF m.modextens.rname -> relative ! ?
	            mmodule.name := -getTableStrOfs(stc, m.modextens.rname)
	         ELSE
	            mmodule.name := getTableStrOfs(stc, m.mname)
	         ENDIF
	         codelab := m.modextens.codelist -> walk threw idents
	         mref := mmodule + SIZEOF modmodule
	         WHILE codelab
	            labref := codelab.labrefs
	            IF labref  -> references ?
	               mref.name := getTableStrOfs(stc, codelab.name)
	               mref.ltype := codelab.ltype
	               IF codelab.ltype = LTYPE_PROC
	                  mref.info := codelab::proc.nrofargs
	               ENDIF
	               wptr := mref + SIZEOF modref
	               mmodule.numsymbs := mmodule.numsymbs + 1
	            ENDIF
	            WHILE labref  -> walk threw references
	               mref.numrefs := mref.numrefs + 1
	               IF labref.type = REF_INHERIT
	                  labref.offset := getTableStrOfs(stc, labref.offset, TRUE) -> V47
	               ENDIF
	               wptr[]++ := Shl(labref.type,24) OR labref.offset
	               labref := labref.next
	            ENDWHILE
	            IF codelab.labrefs
	               mref := wptr
	            ENDIF
	            codelab := codelab.codelink
	         ENDWHILE
	         mmodule.totalsize := wptr - mmodule
	         mmodule := wptr
	      ENDIF
	      m := m.succ
	   ENDWHILE
	   IF info.count
	      ->info.size := wptr - info
	      mod.moduleinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('moduleinfo \d\n', info.count)
	      #endif
	   ENDIF


	/* write vardebug to module */
	#ifdef DBG_BMOD
	DEBUGF('write vardebug\n')
	#endif
	IF g_vardebug
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   vd := wptr

	   proc := g_proclist
	   WHILE proc
	      IF proc.ltype = LTYPE_PROC
	         t := getTableStrOfs(stc,proc.name, TRUE)
	         var := proc.varchain
	         WHILE var
	            setVD(vd, stc, var, t, VDSCOPE_PROC, NIL)
	            vd++
	            var := var.varchain
	         ENDWHILE
	      ENDIF
	      proc := proc.next
	   ENDWHILE

	   proc := g_methodlist
	   WHILE proc
	      x := getTableStrOfs(stc, proc.object.name, TRUE)
	      t := getTableStrOfs(stc, proc.name, TRUE)
	      var := proc.varchain
	      WHILE var
	         setVD(vd, stc, var, t, VDSCOPE_PROC, x)
	         vd++
	         var := var.varchain
	      ENDWHILE
	      proc := proc.next
	   ENDWHILE

	   g := g_gvarlist
	   WHILE g
	      IF g.usage
	         IF g.export
	            setVD(vd, stc, g, NIL, VDSCOPE_XREF, NIL)
	         ELSEIF g.gtype = GTYPE_INTERNAL
	            setVD(vd, stc, g, NIL, VDSCOPE_INTERN, NIL)
	         ELSE
	            setVD(vd, stc, g, NIL, VDSCOPE_RELOC, NIL)
	         ENDIF
	         vd++
	      ENDIF
	      g := g.next
	   ENDWHILE


	   IF vd > wptr
	      info.count := Div(vd - info, SIZEOF vardbg)
	      mod.debuginfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('debuginfo \d\n', info.count)
	      #endif
	      wptr := vd
	   ENDIF
	ENDIF

	/* write glob */
	#ifdef DBG_BMOD
	DEBUGF('write glob\n')
	#endif
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   info.misc := 0
	   info.count := 0
	   g := g_gvarlist
	   WHILE g
	      IF g.gtype <> GTYPE_INTERNAL  -> 1.7.1
	         IF g.export = FALSE -> 1.6.1
	            labref := g.labrefs
	            -> 2.2 note: we include also globals without labref!
	            -> because it has been allocated already..
	            info.count := info.count + 1
	            info.misc := info.misc + g.type.size   -> 1.6.1
	            lptr := wptr++ -> numdrels
	            WHILE labref
	               wptr[]++ := labref.offset
	               PutInt(g_codebuf + labref.offset, g.offset)
	               labref := labref.next
	               lptr[] := lptr[] + 1
	            ENDWHILE
	         ENDIF
	      ENDIF
	      g := g.next
	   ENDWHILE
	   IF info.count
	      mod.globinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('globinfo \d\n', info.count)
	      #endif
	   ENDIF

	/* write code */
	#ifdef DBG_BMOD
	DEBUGF('write code\n')
	#endif
	IF (g_codeptr-g_codebuf) > 4
	   info := wptr
	   info.count := (g_codeptr - g_codebuf) / 4 -> longs
	   IF info.count
	      wptr := info + SIZEOF modinfo
	      CopyMemQuick(g_codebuf, wptr, t := Mul(info.count,4))
	      wptr := wptr + t
	      ->info.size := wptr - info
	      mod.codeinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('codeinfo \d\n', info.count)
	      #endif
	   ENDIF
	ENDIF

	 /* write data */
	 #ifdef DBG_BMOD
	 DEBUGF('write data\n')
	 #endif
	   info := wptr
	   info.count := Div(g_databufsize,4) -> longs
	   IF info.count
	      wptr := info + SIZEOF modinfo
	      CopyMemQuick(g_databuf, wptr, t := Mul(info.count,4))
	      wptr := wptr + t
	      ->info.size := wptr - info
	      mod.datainfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('datainfo \d\n', info.count)
	      #endif
	   ENDIF


	/* write linedebug */
	#ifdef DBG_BMOD
	DEBUGF('write linedebug\n')
	#endif
	IF mod.codeinfo -> 1.8.0, avoid the two dummy lines
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   t := wptr
	   ld := g_linelist
	   WHILE ld
	      wptr[]++ := ld.line
	      wptr[]++ := ld.offset
	      ld := ld.next
	   ENDWHILE
	   IF t < wptr
	      ->info.size := wptr - info
	      info.count := Div(wptr - t, 8)
	      mod.lineinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('lineinfo \d\n', info.count)
	      #endif
	   ENDIF
	ENDIF

	/* write dreld */
	#ifdef DBG_BMOD
	DEBUGF('write dreldinfo\n')
	#endif
	  info := wptr
	   wptr := info + SIZEOF modinfo
	   t := wptr
	   rwref := g_rwreflist
	   WHILE rwref
	      wptr[]++ := rwref.offset
	      rwref := rwref.next
	   ENDWHILE
	   IF t < wptr
	      ->info.size := wptr - info
	      info.count := Div(wptr - info - SIZEOF modinfo,4)
	      mod.dreldinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('dreldinfo \d\n', info.count)
	      #endif
	   ENDIF

	/* write ifunc info */
	#ifdef DBG_BMOD
	DEBUGF('write ifuncinfo\n')
	#endif
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   info.count := 0
	   IF g_optpowerpc = CPU_PPC
	      lptr := g_internalfuncsppc
	      info.misc := 1
	   ELSE
	      lptr := g_internalfuncs
	      info.misc := 0
	   ENDIF
	   IF lptr = NIL THEN lptr := [NIL]
	   a := 0
	   WHILE (ifunc := lptr[a])
	      labref := ifunc.labrefs
	      IF labref
	         mref := wptr
	         mref.name := a -> yep, v44
	         wptr := wptr + SIZEOF modref -> (8)
	         info.count := info.count + 1
	      ENDIF
	      WHILE labref
	         mref.numrefs := mref.numrefs + 1
	         wptr[]++ := labref.offset
	         labref := labref.next
	      ENDWHILE
	      a++
	   ENDWHILE
	   IF info.count
	      ->info.size := wptr - info
	      mod.ifuncinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('ifuncinfo \d\n', info.count)
	      #endif
	   ENDIF



	/* write xref g info */
	#ifdef DBG_BMOD
	DEBUGF('write xrefginfo\n')
	#endif
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   info.count := 0
	   info.misc := 0
	   g := g_gvarlist
	   WHILE g
	      IF g.export
	         IF (g.labrefs<>NIL) OR (g.gtype = GTYPE_DEF) OR (g.gtype = GTYPE_BASE)
	            labref := g.labrefs
	            mref := wptr
	            mref.name := getTableStrOfs(stc, g.hln.name)
	            mref.ltype := g.type.size -> 1.6.1
	            mref.info := g.type.flags -> 1.6.1
	            mref.numrefs := 0
	            wptr := wptr + SIZEOF modref -> (8)
	            info.count := info.count + 1
	            WHILE labref
	               mref.numrefs := mref.numrefs + 1
	               wptr[]++ := labref.offset
	               labref := labref.next
	            ENDWHILE
	         ENDIF
	      ENDIF
	      g := g.next
	   ENDWHILE
	   IF info.count
	      mod.xrefginfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('xrefginfo \d\n', info.count)
	      #endif
	   ENDIF

	/* write reloc32 info */
	#ifdef DBG_BMOD
	DEBUGF('write reloc32info\n')
	#endif
	   info := wptr
	   wptr := info + SIZEOF modinfo
	   t := wptr
	   lptr := link_reloc32list
	   WHILE lptr
	      wptr[]++ := lptr[1]
	      lptr := lptr[]
	   ENDWHILE
	   IF t < wptr
	      ->info.size := wptr - info
	      info.count := Div(wptr - info - SIZEOF modinfo, 4)
	      mod.relocinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('relocinfo \d\n', info.count)
	      #endif
	   ENDIF


	/* write globinit info 2.2 */
	#ifdef DBG_BMOD
	DEBUGF('write globinitinfo\n')
	#endif
	IF (mod.codeinfo <> NIL) AND ((link_globaldatasize>0) OR (immglobinitused<>FALSE))
	   info := wptr
	   info.misc := g_modglobinitlab.offset
	   info.count := Div(link_globaldatasize,4)
	   mod.globinitinfo := info - mod
	   wptr := info + SIZEOF modinfo
	ENDIF

	/* write xglobderef info 2.2 */
	#ifdef DBG_BMOD
	DEBUGF('write xglobderefinfo\n')
	#endif
	   info := wptr
	   xglobderef := info + SIZEOF modinfo
	   info.misc := 0
	   info.count := 0
	   g := g_gvarlist
	   WHILE g
	      IF g.export
	         IF (g.labrefs<>NIL) OR (g.gtype = GTYPE_DEF) OR (g.gtype = GTYPE_BASE)
	            IF g.type.esize > 1
	               xglobderef.totalsize := SIZEOF xglobderef
	               xglobderef.name := getTableStrOfs(stc, g.hln.name, TRUE)
	               xglobderef.esize := g.type.esize
	               xglobderef.rsrvd1 := NIL
	               xglobderef.rsrvd2 := NIL
	               IF g.type.esize = 255
	                  xglobderef.object := getTableStrOfs(stc, g.type.object.name, TRUE)
	               ELSE
	                  xglobderef.object := NIL
	               ENDIF
	               xglobderef++
	               info.count := info.count + 1
	            ENDIF
	         ENDIF
	      ENDIF
	      g := g.next
	   ENDWHILE
	   IF info.count
	      wptr := xglobderef
	      mod.xglobderefinfo := info - mod
	      #ifdef DBG_BMOD
	      DEBUGF('xglobderefinfo \d\n', info.count)
	      #endif
	   ENDIF

	#ifdef DBG_BMOD
	DEBUGF('constructed \d bytes for non string data in module\n', wptr - mod)
	#endif


	info := mod.strtabinfo + mod
	info.count := stc.count
	info.misc := stc.offset
	info.rsrvd := NIL

	#ifdef DBG_BMOD
	DEBUGF('strtabinfo \d\n', info.misc)
	#endif

	t := (info + stc.offset + SIZEOF modinfo) - mod
	IF t > mod.modsize THEN Raise("OBUF")

	mod.modsize := (info + stc.offset + SIZEOF modinfo) - mod  + 3 AND -4


	#ifdef DBG_BMOD
	DEBUGF('module \d\n', mod.modsize)
	#endif

	mod.identification := "ECXM"

	mod.osversion := g_optosversion -> 1.6.0

	mod.cpu := g_optpowerpc

	mod.osid := g_optosid -> 2.0

ENDPROC mod, mod.modsize


-> 1.5.1: used by makeModule()/vardebug
PROC setVD(vd:PTR TO vardbg, stc:PTR TO strtabcontext, var:PTR TO var, inproc, scope, ofobj)
	vd.inproc := inproc
	vd.scope :=  scope
	vd.name := getTableStrOfs(stc, var.hln.name, TRUE)
	vd.ofobject := ofobj
	vd.varsize := var.type.size
	vd.varesize := var.type.esize
	vd.varreg := IF var.o = DREG THEN var.d ELSE var.breg
	vd.varofs := IF var.o = DREG THEN -1 ELSE var.offset
	vd.varflags := var.type.flags
	vd.varobject := IF var.type.object THEN getTableStrOfs(stc, var.type.object.name, TRUE) ELSE NIL
ENDPROC

-> 1.10.0
PROC setLinenum(num)
	g_linenum := num
	ident_LINENUM.value := num
ENDPROC


verstring:
VOID ECX_VERSION_STRING



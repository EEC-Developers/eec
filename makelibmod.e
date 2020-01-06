
-> MakeLibMod. September 2008
-> new tool to convert FD/Protos => E module

-> 3 modes:
-> a. FD only
-> b. Proto(s) only
-> c. FD+Proto(s)

-> basically two passes are used:
-> 1. read in files and create internal representation
-> 2. traverse internal representation and create result.

-> fd is read first if used, then proto(s)

-> prototypes override fd definitions with argtypes

-> prototypes does nothing in 68k mode

-> November 2009: crash on missing argument name fixed.
-> August 2010: stupid bug in executable fixed.

MODULE 'exec/nodes'
MODULE 'dos/dos'
MODULE 'exec/lists'
MODULE 'amigalib/lists'
MODULE '*binary'
MODULE 'ls/picoxml'

RAISE "MEM" IF String()=NIL

-> custom typedefs, either through #typedef, or through interactive input
OBJECT typedef
   type  -> 0:Rx,1:FX,2:VX,3:RX2
   name  ->
   next
ENDOBJECT

OBJECT lfunction OF modlfunc
   args[16]:ARRAY OF lfuncarg
   -> local baseconfig here, set by FD
   baseconfig
ENDOBJECT

DEF g_lfuncs[2048]:ARRAY OF LONG,
    g_numlfuncs=0,
    g_lfoffset,
    g_offsetinc,
    g_baseconfig, -> 0, 1, -1, 2
    g_abiconfig,  -> 0, 1
    g_linenum,
    g_itemsarray[5000]:ARRAY OF LONG,  -> protos
    g_typedefs:PTR TO typedef,         -> protos
    g_fdused, -> bool
    g_namefix, -> bool
    g_verbose, -> bool
    g_syscall, -> bool
    g_tablestrlist:mlh,
    g_prefix[50]:STRING,
    g_basename[50]:STRING,
    g_ifacename[50]:STRING


OBJECT tablestr OF mln
   string:PTR TO CHAR -> estring
ENDOBJECT

-> allocates unique string and puts in list
PROC getTableStr(str)
   DEF s:PTR TO tablestr
   s := g_tablestrlist.head
   WHILE s.succ
      IF StrCmp(s.string, str)
         RETURN s.string
      ENDIF
      s := s.succ
   ENDWHILE
   NEW s
   s.string := String(StrLen(str))
   IF s.string = NIL THEN Raise("MEM")
   StrCopy(s.string, str)
   AddTail(g_tablestrlist, s)
ENDPROC s.string

-> get offset of string
PROC getTableStrOfs(str)
   DEF s:PTR TO tablestr, offset=0
   s := g_tablestrlist.head
   WHILE s.succ
      IF StrCmp(s.string, str)
         RETURN offset
      ENDIF
      offset := offset + EstrLen(s.string) + 1
      s := s.succ
   ENDWHILE
   -> shouldnt get here
ENDPROC NIL

-> get size of string table  for writing to disk
PROC getStrtabSize()
   DEF s:PTR TO tablestr, offset=0
   s := g_tablestrlist.head
   WHILE s.succ
      offset := offset + EstrLen(s.string) + 1
      s := s.succ
   ENDWHILE
ENDPROC offset

-> copy stringtab into module in memory
PROC copyStrtab(tomem)
   DEF s:PTR TO tablestr, len
   s := g_tablestrlist.head
   WHILE s.succ
      CopyMem(s.string, tomem, len := EstrLen(s.string) + 1)
      tomem := tomem + len
      s := s.succ
   ENDWHILE
ENDPROC tomem

PROC newLfunc(name)
   DEF lf:PTR TO lfunction, a
   NEW lf
   lf.name := getTableStr(name)
   g_lfuncs[g_numlfuncs++] := lf
ENDPROC lf

PROC findLfunc(name)
   DEF a, lf:PTR TO lfunction
   FOR a := 0 TO g_numlfuncs-1
      lf := g_lfuncs[a]
      IF StrCmp(lf.name, name) THEN RETURN lf
   ENDFOR
ENDPROC NIL

PROC getWordLen(str)
   DEF c:REG, i:REG
   i := 0
   WHILE (c := str[i])
      SELECT 128 OF c
      CASE "0" TO "9", "A" TO "Z", "a" TO "z", "_", "-"
         i++
      DEFAULT
         RETURN i
      ENDSELECT
   ENDWHILE
ENDPROC i

PROC reportErr(err, info=NIL)
   WriteF('ERROR at line \d: \s \s\s\s\n', g_linenum, err,
                                IF info THEN '"' ELSE '',
                                IF info THEN info ELSE '',
                                IF info THEN '"' ELSE '')
   Raise("ERR")
ENDPROC



-> items: values > 1000 : labels  (lowercase | uppercase | "_"), {lowercase | uppercase | digit | "_"}
->        values < 1000 : special chars: "(" ")" "*" ","

PROC scanProtos(s)
   DEF incomment=FALSE
   DEF c, t, pos, len, a
   DEF iptr:PTR TO LONG
   DEF lfunc:PTR TO lfunction, tstr[100]:STRING
   DEF lfuncarg:PTR TO lfuncarg, ireg, freg, vreg, sofs:PTR TO LONG
   DEF pnum

   g_linenum := 1

   iptr := g_itemsarray


prcs_againhuh:

   WHILE (c := s[])

      IF incomment
         SELECT c
         CASE "/"
            IF s[1] = "*"
               s := s + 2
               incomment++
            ELSE
               s++
            ENDIF
         CASE "*"
            IF s[1] = "/"
               s := s + 2
               incomment--
            ELSE
               s++
            ENDIF
         CASE 10
            s++
            g_linenum++
         DEFAULT
            s++
         ENDSELECT
         JUMP prcs_againhuh
      ELSEIF c = "/"
         IF s[1] = "*"
            s := s + 2
            incomment := 1
            JUMP prcs_againhuh
         ELSEIF s[1] = "/"
            pos := InStr(s, '\n')
            s := s + IF pos > -1 THEN pos ELSE StrLen(s)
            JUMP prcs_againhuh
         ENDIF
      ENDIF

      SELECT 128 OF c
      CASE " ", "\t"
         s++
      CASE "\n"
         g_linenum++
         s++
         IF iptr[-1] <> "," THEN iptr := g_itemsarray -> line can be broken with ","
      CASE "_", "a" TO "z", "A" TO "Z"
         len := getWordLen(s)
         iptr[]++ := StrCopy(String(len), s)
         s := s + len
      CASE ",", "(", ")", "*"
         iptr[]++ := c
         s++
      CASE "#" -> just skip lines beginning with this (preprocessor)
         pos := InStr(s, '\n')
         s := s + IF pos > -1 THEN pos ELSE StrLen(s)
      CASE "."
         s++
         IF s[] = "."
            s++
            IF s[] = "."
               s++
               iptr[]++ := "." -> "." as item signals "..."
            ENDIF
         ENDIF
      CASE ";"
         -> a prototype/typedef "line" has ended, lets parse the items
         iptr[]++ := ";" -> we need a termination
         s++
         iptr := g_itemsarray

         IF iptr[] < 1000

         ELSEIF StrCmp(iptr[], 'typedef')

            iptr++
            IF iptr[] < 256 THEN reportErr('label expected')
            IF StrCmp(iptr[], 'struct')
               iptr++
               IF iptr[] < 256 THEN reportErr('label expected')
               addTypeDef(0, iptr[])
               iptr++
            ELSE
               iptr, t := parsetype(iptr)
               IF iptr[] > 256 THEN addTypeDef(t, iptr[])
            ENDIF
         ELSE

            iptr,t := parsetype(iptr)

            IF iptr[] > 1000
               StrCopy(tstr, g_prefix)
               StrAdd(tstr, iptr[]++)

               IF g_namefix
                  IF tstr[1] < 91
                     tstr[1] := tstr[1] + 32
                     IF tstr[2] < 91
                        IF tstr[3] = "_"
                           tstr[2] := tstr[2] + 32
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF


               IF tstr[] > "Z" THEN tstr[] := tstr[] - 32


               IF g_verbose THEN WriteF(' \s\n', tstr)

               IF iptr[]++ = "("
                  lfunc := findLfunc(tstr)
                  IF g_fdused = FALSE
                     IF lfunc THEN reportErr('double declaration of function', tstr)
                     lfunc := newLfunc(tstr)
                     lfunc.type := 1
                  ELSEIF lfunc
                     IF lfunc.type = 0 THEN lfunc := NIL -> skip 68k functions
                  ENDIF
               ELSE
                  lfunc := NIL
               ENDIF
            ELSE
               lfunc := NIL -> not function
            ENDIF

            IF lfunc

               lfunc.return := IF t = -1 THEN 0 ELSE t

               lfuncarg := lfunc.args

               ireg := 3
               freg := 1
               sofs := 8
               vreg := 2
               a := 0
               pnum := 0

               WHILE iptr[] <> ")"
                  EXIT iptr[] = "." -> "." is actually "..."
                  IF iptr[] < 1000 THEN reportErr('label expected')
                  iptr, t := parsetype(iptr)
                  EXIT t = -1 -> VOID

                  IF (g_fdused <> FALSE) AND (lfunc.baseconfig = 1) AND (a=0)
                     a := 1
                     ireg++
                  ELSEIF (g_fdused=FALSE) AND (g_baseconfig = 1) AND (a=0)
                     a := 1
                     ireg++
                  ENDIF

                  IF iptr[] > 1000
                     lfuncarg.name := getTableStr(iptr[]++)
                  ENDIF

                  IF t = 0
                     lfuncarg.rtype := IF ireg > 10 THEN PPCARGRTYPE_STACKRX ELSE PPCARGRTYPE_RX
                     lfuncarg.rnum := IF ireg > 10 THEN sofs++ ELSE ireg++
                  ELSEIF t = 1
                     lfuncarg.rtype := PPCARGRTYPE_FX
                     lfuncarg.rnum := freg++
                  ELSEIF t = 2
                     lfuncarg.rtype := PPCARGRTYPE_VX
                     lfuncarg.rnum := vreg++
                  ELSEIF t = 3
                     lfuncarg.rtype := PPCARGRTYPE_RX2
                     IF ireg < 10
                        lfuncarg.rnum := ireg++
                        ireg++
                     ELSE
                        lfuncarg.rtype := PPCARGRTYPE_STACKRX2
                        lfuncarg.rnum := sofs++
                        sofs++
                     ENDIF
                  ENDIF
                  lfuncarg++
                  pnum++

                  EXIT iptr[] <> ","
                  iptr++
               ENDWHILE

               IF iptr[] = "."
                  iptr++
                  lfunc.flags OR= LFFLAG_VARARGS -> Oct 2008.
               ENDIF

               IF iptr[] <> ")" THEN reportErr('")" expected')

               IF g_fdused = FALSE
                  lfunc.baseofs := g_lfoffset
                  g_lfoffset := g_lfoffset + g_offsetinc
               ELSE
                  IF pnum <> lfunc.nrofargs THEN WriteF('Warning: parameters mismatch FD/PROTO "\s"\n', lfunc.name)
               ENDIF

               lfunc.nrofargs := pnum

            ENDIF

         ENDIF

         iptr := g_itemsarray -> reset
         iptr[] := NIL -> destroy old data incase another ";" immediately turns up

      DEFAULT
         s++
      ENDSELECT

   ENDWHILE

      IF incomment > 0 THEN reportErr('start of comment without end')


ENDPROC

PROC parsetype(iptr:PTR TO LONG)
   DEF t=0, str[100]:STRING

   StrCopy(str, iptr[])
   LowerStr(str)

   IF StrCmp(str, 'const')
      iptr++
      StrCopy(str, iptr[])
      LowerStr(str)
   ENDIF

   IF StrCmp(str, 'enum')
      iptr++
      StrCopy(str, iptr[])
      LowerStr(str)
   ENDIF

   IF StrCmp(str, 'struct')
      t := 0
      iptr++ -> skip struct
      iptr++ -> skip structname
      IF iptr[] = "*"
         iptr++
      ENDIF
   ELSEIF StrCmp(str, 'signed')
      iptr++
      iptr++ -> skip long/int/short/char
      t := 0
   ELSEIF StrCmp(str, 'unsigned')
      iptr++
      iptr++ -> skip long/int/short/char
      t := 0
   ELSEIF StrCmp(str, 'void')
      iptr++
      t := -1
   ELSEIF StrCmp(str, 'long')
      iptr++
      IF iptr[] > 256
         StrCopy(str, iptr[])
         LowerStr(str)
         IF StrCmp(str, 'long')
            iptr++
            t := 3
         ELSE
            t := 0
         ENDIF
      ELSE
         t := 0
      ENDIF
   ELSEIF StrCmp(str, 'int')
      iptr++
      t := 0
   ELSEIF StrCmp(str, 'short')
      iptr++
      t := 0
   ELSEIF StrCmp(str, 'char')
      iptr++
      t := 0
   ELSE
      -> integer or float as return
      t := customtype(iptr[]++)
   ENDIF

   WHILE iptr[] = "(" -> pointer to function stuff
      t := 0
      REPEAT
         iptr++
         IF iptr[] = ";" THEN reportErr('")" expected')
      UNTIL iptr[] = ")"
      iptr++
   ENDWHILE

   WHILE iptr[] = "*"
      iptr++
      -> pointer (integer) as return
      t := 0
   ENDWHILE

ENDPROC iptr, t

PROC customtype(str)
   DEF t=0
   DEF s[100]:STRING, rstr[10]:STRING
   DEF td:PTR TO typedef

   StrCopy(s, str)
   UpperStr(s)

   IF     StrCmp(s, 'APTR')            ; t := 0
   ELSEIF StrCmp(s, 'CONST_APTR')      ; t := 0
   ELSEIF StrCmp(s, 'ULONG')           ; t := 0
   ELSEIF StrCmp(s, 'LONGBITS')        ; t := 0
   ELSEIF StrCmp(s, 'WORD')            ; t := 0
   ELSEIF StrCmp(s, 'UWORD')           ; t := 0
   ELSEIF StrCmp(s, 'WORDBITS')        ; t := 0
   ELSEIF StrCmp(s, 'BYTE')            ; t := 0
   ELSEIF StrCmp(s, 'UBYTE')           ; t := 0
   ELSEIF StrCmp(s, 'BYTEBITS')        ; t := 0
   ELSEIF StrCmp(s, 'RPTR')            ; t := 0
   ELSEIF StrCmp(s, 'STRPTR')          ; t := 0
   ELSEIF StrCmp(s, 'CONST_STRPTR')    ; t := 0
   ELSEIF StrCmp(s, 'SHORT')           ; t := 0
   ELSEIF StrCmp(s, 'USHORT')          ; t := 0
   ELSEIF StrCmp(s, 'COUNT')           ; t := 0
   ELSEIF StrCmp(s, 'UCOUNT')          ; t := 0
   ELSEIF StrCmp(s, 'CPTR')            ; t := 0
   ELSEIF StrCmp(s, 'FLOAT')           ; t := 1
   ELSEIF StrCmp(s, 'DOUBLE')          ; t := 1
   ELSEIF StrCmp(s, 'BOOL')            ; t := 0
   ELSEIF StrCmp(s, 'TEXT')            ; t := 0
   ELSEIF StrCmp(s, 'CLASS')           ; t := 0
   ELSEIF StrCmp(s, 'MSG')             ; t := 0
   ELSEIF StrCmp(s, 'OBJECT')          ; t := 0
   ELSEIF StrCmp(s, 'ENUM')            ; t := 0

   ELSEIF StrCmp(s, 'IPTR')            ; t := 0
   ELSEIF StrCmp(s, 'BPTR')            ; t := 0

   ELSEIF StrCmp(s, 'TAG')             ; t := 0
   ELSEIF StrCmp(s, 'INT32')           ; t := 0
   ELSEIF StrCmp(s, 'INT16')           ; t := 0
   ELSEIF StrCmp(s, 'INT8')            ; t := 0
   ELSEIF StrCmp(s, 'UINT')            ; t := 0
   ELSEIF StrCmp(s, 'UINT32')          ; t := 0
   ELSEIF StrCmp(s, 'UINT16')          ; t := 0
   ELSEIF StrCmp(s, 'UINT8')           ; t := 0
   ELSEIF StrCmp(s, 'INT64')           ; t := 3
   ELSEIF StrCmp(s, 'UINT64')          ; t := 3
   ELSEIF StrCmp(s, 'UQUAD')           ; t := 3
   ELSEIF StrCmp(s, 'QUAD')            ; t := 3

   ELSE

      td := g_typedefs
      WHILE td
         t := td.type
         EXIT StrCmp(s, td.name)
         td := td.next
      ENDWHILE

      IF td = NIL
         -> oh, not recognized, lets ask user then..
         WriteF('Type "\s" is not recognized.. should it go into (i)nteger, (f)loat, (v)ector, (w)ide register ?: ', s)
         ReadStr(stdin, rstr)
         t := rstr[]
         SELECT t
         CASE "i" ; t := 0
         CASE "f" ; t := 1
         CASE "v" ; t := 2
         CASE "w" ; t := 3
         DEFAULT  ; WriteF('bad choice! Lets say you ment "i" then..\n') ; t := 0
         ENDSELECT
         addTypeDef(t, s)
      ENDIF

   ENDIF

ENDPROC t

PROC addTypeDef(type, name)
   DEF td:PTR TO typedef
   NEW td
   td.type := type
   td.name := StrCopy(String(StrLen(name)), name)
   UpperStr(td.name)
   td.next := g_typedefs
   g_typedefs := td
ENDPROC

PROC scanFD(s)
   DEF c, t, v, r, public=TRUE, iptr:PTR TO LONG
   DEF ireg, sofs:PTR TO LONG, str[300]:STRING
   DEF lfunc:PTR TO lfunction, pnum, rnum

   g_linenum := 0

   iptr := g_itemsarray

   WHILE (c := s[])
      SELECT 128 OF c
      CASE "*"
         t := InStr(s, '\n')
         IF t < 1 THEN reportErr('comment not terminated')
         IF g_verbose
            StrCopy(str, s, t+1)
            WriteF(str)
         ENDIF
         s := s + t + 1
         g_linenum++
      CASE " ", "\t", ",", "/"
         s++
      CASE "\n"
         g_linenum++
         s++
         iptr[] := NIL -> terminate line
         -> lets see what this line holds..
         iptr := g_itemsarray
         IF iptr[] > 1000  -> label ?
            IF public
               StrCopy(str, g_prefix)
               StrAdd(str, iptr[]++)
               IF g_namefix
                  IF str[1] < 91
                     str[1] := str[1] + 32
                     IF str[2] < 91
                        IF str[3] = "_"
                           str[2] := str[2] + 32
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               IF str[0] > 96 THEN str[0] := str[0]  - 32
               IF g_verbose
                  WriteF('\s\n', str)
               ENDIF
               IF findLfunc(str) THEN reportErr('double declaration of function', str)
               lfunc := newLfunc(str)
               -> parse argnames
               IF iptr[]++ <> "(" THEN reportErr('"(" expected')
               pnum := 0
               WHILE iptr[] <> ")"
                  EXIT iptr[] = "."
                  IF iptr[] < 1000 THEN reportErr('label expected')
                  lfunc.args[pnum++].name := getTableStr(iptr[]++)
               ENDWHILE
               IF iptr[] = "."
                  iptr++
                  lfunc.flags OR=LFFLAG_VARARGS
               ENDIF
               iptr++  -> skip ")"
               lfunc.nrofargs := pnum
               -> parse registers/abi
               IF iptr[]++ <> "(" THEN reportErr('"(" expected')
               rnum := 0
               ireg := 0
               sofs := 8
               WHILE iptr[] > 1000
                  LowerStr(iptr[])
                  SELECT 128 OF Char(iptr[])
                  CASE "a" -> an
                     lfunc.args[rnum].rtype := 1
                     lfunc.args[rnum].rnum := Char(iptr[]+1) - 48
                  CASE "d" -> dn
                     lfunc.args[rnum].rtype := 0
                     lfunc.args[rnum].rnum := Char(iptr[]+1) - 48
                  CASE "b" -> base
                     lfunc.type := 1
                     IF rnum = 0
                        ireg := 4
                        lfunc.basertype := 0
                        lfunc.basernum := 3
                        lfunc.baseconfig := 1
                     ELSE -> rnum=1 (and sysv@0)
                        ireg := 3
                        lfunc.basertype := IF pnum < 8 THEN 0 ELSE 1
                        lfunc.basernum := IF pnum < 8 THEN 3+pnum ELSE pnum-8*4+8
                        lfunc.baseconfig := -1
                     ENDIF
                  CASE "s" -> sysv
                     lfunc.type := 1
                     IF rnum = 0
                        ireg := 3
                        lfunc.basertype := 0
                        lfunc.basernum := 0
                        lfunc.baseconfig := 0
                     ENDIF
                  CASE "r" -> r12base
                     lfunc.type := 1
                     ireg := 3
                     lfunc.basertype := 0
                     lfunc.basernum := 12
                     lfunc.baseconfig := 2
                  ENDSELECT
                  rnum++
                  iptr++
               ENDWHILE
               IF lfunc.type = 1
                  FOR c := 0 TO pnum-1
                     IF ireg < 11
                        lfunc.args[c].rtype := 0
                        lfunc.args[c].rnum := ireg++
                     ELSE
                        lfunc.args[c].rtype := 1
                        lfunc.args[c].rnum := sofs++
                     ENDIF
                  ENDFOR
               ENDIF
               lfunc.baseofs := g_lfoffset + IF lfunc.type=1 THEN 2 ELSE 0
            ELSE
               IF g_verbose THEN WriteF('private "\s"\n', iptr[])
            ENDIF
            g_lfoffset := g_lfoffset + g_offsetinc
         ELSEIF (iptr[] = "#") AND (iptr[1] = "#")
            iptr++
            iptr++
            IF iptr[] < 1000 THEN reportErr('directive name expected')
            IF     StrCmp(iptr[], 'base')
               iptr++
               IF iptr[] < 1000 THEN reportErr('base name expected')
               IF EstrLen(g_basename) = 0
                  IF Char(iptr[]) = "_"
                     StrCopy(g_basename, iptr[]+1)
                  ELSE
                     StrCopy(g_basename, iptr[])
                  ENDIF
                  LowerStr(g_basename)
               ENDIF
               iptr++
            ELSEIF StrCmp(iptr[], 'bias')
               iptr++
               IF iptr[] < 1000 THEN reportErr('value expected')
               v,r := Val(iptr[])
               IF r = NIL THEN reportErr('value expected')
               g_lfoffset := -v
               iptr++
            ELSEIF StrCmp(iptr[], 'public')
               iptr++
               public := TRUE
            ELSEIF StrCmp(iptr[], 'private')
               iptr++
               public := FALSE
            ELSEIF StrCmp(iptr[], 'end')
               s[] := NIL
            ELSE
               reportErr('unknown directive')
            ENDIF
         ELSEIF iptr[] <> NIL
            reportErr('unknown directive')
         ENDIF
         iptr := g_itemsarray
      CASE "#", "(", ")"
         s++
         iptr[]++ := c
      CASE "_", "a" TO "z", "A" TO "Z", "0", "1" TO "9"
         t := getWordLen(s)
         iptr[]++ := StrCopy(String(t), s, t)
         s := s + t
     CASE "."
         s++
         IF s[] = "."
            s++
            IF s[] = "."
               s++
               iptr[]++ := "." -> "." as item signals "..."
            ENDIF
         ENDIF
      DEFAULT
         reportErr('unexpected characters')
      ENDSELECT

   ENDWHILE

ENDPROC



OBJECT args
   fd
   to
   namefix
   prefix
   basename
   abiconfig
   baseconfig
   verbose
   ifacename
   inc
   offset
   syscall
   interface
   protos
ENDOBJECT

PROC main()  HANDLE
   DEF args:PTR TO args, rdargs=NIL, pnames:PTR TO LONG
   DEF buf, modname[256]:STRING, mod, size

   NEW args

   WriteF('\s\n', {version})

   /* read args */
   rdargs := ReadArgs(
   'FD/K,'+
   'TO/A,'+
   'NAMEFIX/S,'+
   'PREFIX/K,'+
   'BASENAME/K,'+
   'ABICONFIG/N,'+
   'BASECONFIG/N,'+
   'VERBOSE/S,'+
   'IFACENAME/K,'+
   'INC/N,'+
   'OFFSET/N,'+
   'SYSCALL/S,'+
   'INTERFACE/K,'+
   'PROTOS/M',
    args, NIL)

   IF rdargs = NIL THEN Raise("ARGS")

   g_syscall := args.syscall

   g_verbose := args.verbose

   g_namefix := args.namefix

   IF args.prefix THEN StrCopy(g_prefix, args.prefix)

   StrCopy(modname, args.to)

   IF args.basename THEN StrCopy(g_basename, args.basename)

   IF args.ifacename THEN StrCopy(g_ifacename, args.ifacename)

   IF args.abiconfig
      g_abiconfig := Long(args.abiconfig)
   ELSE
      -> 0=68k,1=ppcsysv
      g_abiconfig := 0
   ENDIF

   IF args.inc
      g_offsetinc := Long(args.inc)
   ELSE
      g_offsetinc := -6
   ENDIF

   IF args.offset
      g_lfoffset := Long(args.offset)
   ELSE
      g_lfoffset := -30
   ENDIF

   IF args.baseconfig
   -> 0=nobase, 1=arg1, -1=argn, 2=r12
      g_baseconfig := Long(args.baseconfig)
   ELSE
      g_baseconfig := 0
   ENDIF

   newList(g_tablestrlist)

   IF args.fd
      buf := loadFile(args.fd)
      WriteF('Scanning FD "\s"..\n', args.fd)
      scanFD(buf)
      g_fdused := TRUE
   ENDIF

   IF args.protos
      pnames := args.protos
      WHILE pnames[]
         buf := loadFile(pnames[])
         WriteF('Scanning prototypes in "\s"..\n', pnames[])
         scanProtos(buf)
         pnames++
      ENDWHILE
   ELSEIF args.interface
      IF g_fdused THEN Raise("ARGS")
      buf := loadFile(args.interface)
      WriteF('Scanning interface "\s"..\n', args.interface)
      scanInterface(buf)
   ENDIF

   getTableStr(g_basename)
   getTableStr(g_ifacename)

   WriteF('Making module "\s"..\n', modname)

   mod, size := makeModule()

   writeNewFile(mod, size, modname)

   WriteF('Done! (\d bytes)\n', size)

EXCEPT DO

   SELECT exception
   CASE "ARGS" ; WriteF('error: wrong arguments\n')
   CASE "OPEN" ; WriteF('error: could not open "\s"\n', exceptioninfo)
   CASE "READ" ; WriteF('error: could not read "\s"\n', exceptioninfo)
   CASE "WRIT" ; WriteF('error: could not write "\s"\n', exceptioninfo)
   CASE "EXAM" ; WriteF('error: could not examine "\s"\n', exceptioninfo)
   CASE "MEM"  ; WriteF('error: not enough memory\n')
   CASE "ERR"
   CASE NIL
   DEFAULT     ; WriteF('error: unknown\n')
   ENDSELECT

   IF rdargs THEN FreeArgs(rdargs)

ENDPROC IF exception THEN 10 ELSE 0

CHAR 0, 0, '$VER: '
version:
CHAR 'MakeLibMod 1.1 by LS 2008-2010', 0


PROC setsum(m:PTR TO moduleheader)
   DEF lptr:REG PTR TO LONG, longsize:REG, sum=NIL:REG

   lptr := m.checksumstart
   longsize := Div(m + m.modsize - lptr,4) + 1

   WHILE longsize-- DO sum := sum + lptr[]++

   m.checksum := sum

ENDPROC


PROC makeModule()
   DEF m:PTR TO moduleheader, size=0, lf:PTR TO lfunction, arg:PTR TO lfuncarg
   DEF info:PTR TO modinfo, mref:PTR TO modref, libi:PTR TO modlibi, ptr, t, a
   DEF b

   size += SIZEOF moduleheader
   size += SIZEOF modinfo -> libi
   size += SIZEOF modlibi
   size += SIZEOF modinfo -> strtab
   IF g_ifacename[]
      size += SIZEOF modinfo
      size += SIZEOF modref
   ENDIF

   -> compute size of modlfuncs and turn name-pointers into disk-offsets
   FOR a := 0 TO g_numlfuncs-1
      lf := g_lfuncs[a]
      lf.name := getTableStrOfs(lf.name)
      lf.totalsize := SIZEOF modlfunc + (SIZEOF lfuncarg * lf.nrofargs)
      size += SIZEOF modlfunc
      FOR b := 0 TO lf.nrofargs-1
         arg := lf.args[b]
         IF arg.name THEN arg.name := getTableStrOfs(arg.name)
         size += SIZEOF lfuncarg
      ENDFOR
   ENDFOR

   size += getStrtabSize()
   size := size + 3 AND -4

   -> alloc mem
   m := NewR(size)

   -> create module

   m.modsize := size
   m.identification := "ECXM"
   m.headsize := SIZEOF moduleheader
   m.modversion := 1
   IF g_ifacename[] THEN m.osid := OSID_AMIGAOS4

   info := m + SIZEOF moduleheader
   m.libiinfo := info - m
   info.count := 1
   info.rsrvd := NIL
   info.misc := NIL
   libi := info + SIZEOF modinfo
   libi.basename := getTableStrOfs(IF g_ifacename[] THEN g_ifacename ELSE g_basename)
   libi.nroflfuncs := g_numlfuncs
   ptr := libi + SIZEOF modlibi

   FOR a := 0 TO g_numlfuncs-1
      lf := g_lfuncs[a]
      CopyMem(lf, ptr, lf.totalsize)
      ptr := ptr + lf.totalsize
   ENDFOR
   libi.totalsize := ptr - libi

   IF g_ifacename[]
      info := ptr
      m.xrefginfo := info - m
      info.count := 1
      info.rsrvd := NIL
      info.misc := NIL
      mref := info + SIZEOF modinfo
      mref.name := getTableStrOfs(g_basename)
      mref.ltype := 4
      mref.info := NIL
      mref.numrefs := 0
      ptr := mref + SIZEOF modref -> (8)
   ENDIF

   info := ptr
   info.count := NIL
   info.rsrvd := NIL
   info.misc := NIL
   m.strtabinfo := info - m
   ptr := info + SIZEOF modinfo

   copyStrtab(ptr)

   setsum(m)

ENDPROC m, size


/* FILE IO */

PROC writeNewFile(buf, size, name) HANDLE
   DEF fh=NIL

   fh := Open(name, NEWFILE)
   IF fh = NIL THEN Throw("OPEN", name)

   IF Write(fh, buf, size) <> size THEN Throw("WRIT", name)

EXCEPT DO

   IF fh THEN Close(fh)

   ReThrow()

ENDPROC size

PROC loadFile(name) HANDLE
   DEF fh=NIL, fib:fileinfoblock, buf=NIL
   fh := Open(name, OLDFILE)
   IF fh = NIL THEN Throw("OPEN", name)
   IF ExamineFH(fh, fib) = NIL THEN Throw("EXAM", name)
   buf := NewR(fib.size+1)
   IF Read(fh, buf, fib.size) <> fib.size THEN Throw("READ", name)
   Close(fh)
   fh := NIL
EXCEPT
   IF fh THEN Close(fh)
   ReThrow()
ENDPROC buf


PROC scanInterface(ascii) HANDLE
   DEF pxml:picoxml, libe:PTR TO xelem, ifacee:PTR TO xelem
   DEF methode:PTR TO xelem, arge:PTR TO xelem
   DEF nameid, opennameid, basenameid
   DEF includeid, libraryid, interfaceid, structid
   DEF methodid, resultid, argid,varargid, typeid
   DEF dum, a, b, tstr[128]:STRING, lfunc:PTR TO lfunction
   DEF pnum, ireg, freg, vreg, sofs:PTR TO LONG, t
   DEF lfuncarg:PTR TO lfuncarg, err, s , d:PTR TO xdocu, succ

   pxml.new(PXF_NOCOMMENTS)

   err, d, s := pxml.scan_document(ascii)
   IF err THEN Throw("XML", err)

   nameid := pxml.merge_name('name')
   opennameid := pxml.merge_name('openname')
   basenameid := pxml.merge_name('basename')
   includeid := pxml.merge_name('include')
   libraryid := pxml.merge_name('library')
   interfaceid := pxml.merge_name('interface')
   structid := pxml.merge_name('struct')
   methodid := pxml.merge_name('method')
   resultid := pxml.merge_name('result')
   argid := pxml.merge_name('arg')
   varargid := pxml.merge_name('vararg')
   typeid := pxml.merge_name('type')

   libe := findElement(d.data, libraryid)
   IFN libe THEN reportErr('missing library element')

   IFN libe.getAttrs([basenameid, {a}])
      reportErr('failed to get attribute(s) from element', libe.name)
   ENDIF
   IF g_basename[] = NIL
      StrCopy(g_basename, a)
      LowerStr(g_basename)
   ENDIF

   ifacee := findElement(libe.data, interfaceid)
   IFN ifacee THEN reportErr('missing interface element')

   IFN ifacee.getAttrs([structid, {a}])
      reportErr('failed to get attribute(s) from element', ifacee.name)
   ENDIF

   IF g_ifacename[] = NIL
      StrCopy(g_ifacename, a)
      LowerStr(g_ifacename)
   ENDIF

   g_lfoffset := 60
   g_offsetinc := 4

   methode := ifacee.data.head
   WHILE methode.succ
      IF methode.type = XNTYPE_ELEMENT
         IF methode.name = methodid
            IFN methode.getAttrs([nameid, {a}, resultid, {b}])
               reportErr('failed to get attribute(s) from element', methode.name)
            ENDIF
            IF (g_lfoffset < 76) OR StrCmp(a, 'Private', STRLEN) OR StrCmp(a, 'Reserved', STRLEN)
               -> prepend the first four functions with ifacename
               -> or if function is private/reserved
               StrCopy(tstr, g_ifacename)
               UpperStr(tstr)
               StrAdd(tstr, '_')
               StrAdd(tstr, a)
            ELSE
               StrCopy(tstr, a)
               IF g_namefix
                  IF tstr[1] < 91
                     tstr[1] := tstr[1] + 32
                     IF tstr[2] < 91
                        IF tstr[3] = "_"
                           tstr[2] := tstr[2] + 32
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               IF tstr[] > "Z" THEN tstr[] := tstr[] - 32
            ENDIF

            IF g_verbose THEN WriteF(' \s\n', tstr)

            lfunc := findLfunc(tstr)
            IF lfunc THEN reportErr('double declaration of function', tstr)
            lfunc := newLfunc(tstr)
            lfunc.type := 1
            lfunc.basertype := 0
            lfunc.basernum := 3

            itemizeTypeStr(b) -> to g_itemsarray
            dum, t := parsetype(g_itemsarray)
            lfunc.return := IF t = -1 THEN 0 ELSE t

            ireg := 4
            freg := 1
            vreg := 2
            sofs := 8
            lfuncarg := lfunc.args
            pnum := 0

            arge := methode.data.head
            WHILE arge.succ
               IF arge.type = XNTYPE_ELEMENT
                  IF (arge.name = argid) OR (arge.name = varargid)
                     IFN arge.getAttrs([nameid, {a}, typeid, {b}])
                        reportErr('failed to get attribute(s) from arg', methode.name)
                     ENDIF
                     ->WriteF('arg=\s\n', a)
                     lfuncarg.name := getTableStr(a)
                     itemizeTypeStr(b) -> to g_itemsarray
                     dum, t := parsetype(g_itemsarray)
                     IF t = 0
                        lfuncarg.rtype := IF ireg > 10 THEN PPCARGRTYPE_STACKRX ELSE PPCARGRTYPE_RX
                        lfuncarg.rnum := IF ireg > 10 THEN sofs++ ELSE ireg++
                     ELSEIF t = 1
                        lfuncarg.rtype := PPCARGRTYPE_FX
                        lfuncarg.rnum := freg++
                     ELSEIF t = 2
                        lfuncarg.rtype := PPCARGRTYPE_VX
                        lfuncarg.rnum := vreg++
                     ELSEIF t = 3
                        lfuncarg.rtype := PPCARGRTYPE_RX2
                        IF ireg < 10
                           lfuncarg.rnum := ireg++
                           ireg++
                        ELSE
                           lfuncarg.rtype := PPCARGRTYPE_STACKRX2
                           lfuncarg.rnum := sofs++
                           sofs++
                        ENDIF
                     ENDIF
                     lfuncarg++
                     pnum++
                     IF arge.name = varargid
                        pnum--
                        lfunc.flags OR= LFFLAG_VARARGS -> Oct 2008.
                     ENDIF
                  ENDIF
               ENDIF
               arge := arge.succ
            ENDWHILE
            lfunc.baseofs := g_lfoffset
            g_lfoffset := g_lfoffset + g_offsetinc
            lfunc.nrofargs := pnum
         ENDIF
      ENDIF
      methode := methode.succ
   ENDWHILE


EXCEPT DO

   END d

   SELECT exception
   CASE "XML"     ; WriteF('XML error: \s\n', pxmlErr2Str(exceptioninfo))
   ENDSELECT

   ReThrow()

ENDPROC

PROC itemizeTypeStr(str)
   DEF iptr:PTR TO PTR, t, c
   iptr := g_itemsarray
   WHILE c := str[]
      SELECT 128 OF c
      CASE " ", "\t", 10, 12, 13
         str++
      CASE "A" TO "Z", "a" TO "z", "_"
         t := getWordLen(str)
         iptr[] := String(t)
         IFN iptr[] THEN Raise("MEM")
         StrCopy(iptr[]++, str, t)
         str += t
      CASE "*", "(", ")", ","
         iptr[]++ := c
         str++
      DEFAULT
         reportErr('unknown character in type str')
      ENDSELECT
   ENDWHILE
   iptr[]++ := NIL
ENDPROC


PROC findElement(list:PTR TO mlh, nameid)
   DEF e:PTR TO xelem
   e := list.head
   WHILE e.succ
      IF e.type = XNTYPE_ELEMENT
         IF e.name = nameid THEN RETURN e
      ENDIF
      e := e.succ
   ENDWHILE
ENDPROC NIL




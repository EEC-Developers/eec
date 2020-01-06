
/* ModuleFromProto by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2006-2007 */
/* Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT */

-> April 2006.
-> C PROTO TO ECXMODULE CONVERTER
-> This should be final solution, no more over-extended FD crap.

-> 20060416: added typedef support, made "int" buildin type
-> 2007: 1.2: removed some built in AROS STACK_XXX types, why did I add them ?

-> Aug 2007: 1.3: added arg SYSCALL/S.

-> March 2008 1.4: added ability to ask user when stumbling on unknown type and
-> automatically add it. Also now Uppercases first letter of function name.

-> Sept 2008: support for long long, (u)int64 types.. and APICALL.
-> October 2008: added IFACENAME/K

-> October 2008: modleformat now records varargs. LFFLAG_VARARGS

MODULE '*binary'

DEF g_namefix

/* automatic errorchecking */
RAISE "ARGS" IF ReadArgs()=NIL
RAISE "OPEN" IF Open()=NIL
RAISE "FILE" IF FileLength() < 1
RAISE "MEM"  IF New()=NIL

OBJECT typedef
   type
   name
   next
ENDOBJECT


DEF mod:PTR TO moduleheader
DEF g_labelsarray[1024]:ARRAY OF LONG, g_numlabs=0
DEF g_linenum=1
DEF g_itemsarray[500]:ARRAY OF LONG
DEF g_bias, g_inc
DEF g_verbose
DEF g_basereg
DEF g_typedefs:PTR TO typedef
DEF g_kernel=FALSE

PROC getStrOfs(str)
   DEF a, len, offset=0, newstr
   FOR a := 0 TO g_numlabs-1
      IF StrCmp(str, g_labelsarray[a]) THEN RETURN offset
      len := EstrLen(g_labelsarray[a])
      offset := offset + len + 1
   ENDFOR
   len := EstrLen(str)
   newstr := String(len)
   IF newstr = NIL THEN Raise("MEM")
   StrCopy(newstr, str)
   g_labelsarray[g_numlabs++] := newstr
ENDPROC offset

PROC getWordLen(str)
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

PROC reportErr(err, info=NIL)
   WriteF('ERROR at line \d: \s \s\s\s\n', g_linenum, err,
                                IF info THEN '"' ELSE '',
                                IF info THEN info ELSE '',
                                IF info THEN '"' ELSE '')
   Raise("ERR")
ENDPROC



ENUM ARG_PROTO,
     ARG_MODDIR,
     ARG_BASENAME,
     ARG_BASECONFIG,
     ARG_VERBOSE,
     ARG_INC,
     ARG_BIAS,
     ARG_IFACENAME,
     ARG_NAMEFIX,
     ARG_SYSCALL,
     NROFARGS


PROC main() HANDLE
   DEF temp:PTR TO LONG, rdargs
   DEF ffh=NIL, mfh=NIL
   DEF info:PTR TO modinfo
   DEF libi:PTR TO modlibi, lfunc:PTR TO modlfunc
   DEF ffile[256]:STRING
   DEF mdir[200]:STRING
   DEF count=NIL
   DEF str[1000]:STRING
   DEF a, t, wptr:PTR TO LONG
   DEF fbuf, fsize
   DEF r, s
   DEF basename[100]:STRING
   DEF ifacename[100]:STRING, mref:PTR TO modref

   WriteF('\s\n', {version})

   NEW temp[NROFARGS]

   /* read args */
   rdargs := ReadArgs(
   'PROTOTYPE/A,'+
   'MODULEDIR/A,'+
   'BASENAME/A,'+
   'BASECONFIG/A,'+
   'VERBOSE/S,'+
   'INC/N,'+
   'BIAS/N,'+
   'IFACENAME/K,'+
   'NAMEFIX/S,'+
   'SYSCALL/S',
    temp, NIL)

   /* get args */
   StrCopy(ffile, temp[ARG_PROTO])

   WriteF('   .. processing "\s" ..\n', ffile)

   StrCopy(mdir, temp[ARG_MODDIR])

   StrCopy(basename, temp[ARG_BASENAME])

   g_inc := -6
   g_bias := -28

   IF temp[ARG_IFACENAME]
      StrCopy(ifacename, temp[ARG_IFACENAME])
      g_inc := 4
      g_bias := 76
   ENDIF

   g_namefix := temp[ARG_NAMEFIX]

   g_verbose := temp[ARG_VERBOSE]

   IF temp[ARG_INC] THEN g_inc := Long(temp[ARG_INC])

   IF temp[ARG_BIAS] THEN g_bias := Long(temp[ARG_BIAS])

   StrCopy(str, temp[ARG_BASECONFIG])
   UpperStr(str)
   IF     StrCmp(str, 'NOBASE')
      g_basereg := NIL
   ELSEIF StrCmp(str, 'BASESYSV')
      g_basereg := 3
   ELSEIF StrCmp(str, 'SYSVBASE')
      g_basereg := -1
   ELSEIF StrCmp(str, 'R12BASE')
      g_basereg := 12
   ELSE
      reportErr('unsupported BASECONFIG', str)
   ENDIF

   IF temp[ARG_SYSCALL] THEN g_kernel := TRUE

   /* free args */
   FreeArgs(rdargs)

   ffh := Open(ffile, OLDFILE)

   fsize := FileLength(ffile)
   fbuf := NewR(fsize+8) + 4
   a := NewR(Mul(fsize,2)+4)
   IF Read(ffh,fbuf,fsize) <> fsize THEN Raise("READ")
   Close(ffh) ; ffh := NIL

   mod := NewR(SIZEOF moduleheader + (fsize*3) + 1000)
   info := mod + SIZEOF moduleheader
   info.count := 1
   mod.libiinfo := info - mod
   libi := info + SIZEOF modinfo
   lfunc := libi + SIZEOF modlibi

   ->StrCopy(str, basename)
   libi.basename := getStrOfs(IF ifacename[] THEN ifacename ELSE basename)

   wptr := process(fbuf, libi, lfunc)

   IF ifacename[]
      info := wptr
      mod.xrefginfo := info - mod
      info.count := 1
      info.rsrvd := NIL
      info.misc := NIL
      mref := info + SIZEOF modinfo
      mref.name := getStrOfs(basename)
      mref.ltype := 4
      mref.info := NIL
      mref.numrefs := 0
      wptr := mref + SIZEOF modref -> (8)
   ENDIF

   info := wptr
   mod.identification := "ECXM"
   mod.headsize := SIZEOF moduleheader
   mod.strtabinfo := info - mod
   wptr := info + SIZEOF modinfo
   FOR a := 0 TO g_numlabs-1
      s := g_labelsarray[a]
      CopyMem(s, wptr, EstrLen(s))
      wptr := wptr + EstrLen(s) + 1
   ENDFOR
   mod.modsize := (wptr - mod) + 3 AND -4

   StrCopy(str, mdir)
   t := InStr(basename, 'base')
   IF t = -1
      t := InStr(basename, 'device')
      IF t = -1
         t := InStr(basename, 'resource')
         IF t = -1
            t := ALL
         ENDIF
      ENDIF
   ENDIF
   StrAdd(str, basename, t)
   StrAdd(str, '.m')

   mod.modversion := 1

   setsum(mod)

   mfh := Open(str, NEWFILE)

   Write(mfh, mod, mod.modsize)

   IF g_verbose
      WriteF('\e[31m - processed \d functions\n', libi.nroflfuncs)
      WriteF(' - base : \s\n', basename)
      IF EstrLen(ifacename) THEN WriteF(' - iface: \s\n', ifacename)
      WriteF(' - ecxmodule : \s\n', str)
   ENDIF

EXCEPT DO
   SELECT exception
   CASE "ARGS" ; PutStr('Bad Args!\n')
   CASE "OPEN" ; PutStr('Open file error!\n')
   CASE "READ" ; PutStr('Read file error!\n')
   CASE "ERR"  ; PutStr('Aborted.\n')
   CASE "MEM"  ; PutStr('Not enough memory!\n')
   CASE "^C"   ; PutStr('User aborted!\n')
   DEFAULT
      IF exception > 0 THEN PutStr('unknown exception occoured!\n')  ELSE PutStr('Done.\n')
   ENDSELECT
   IF ffh THEN Close(ffh)
   IF mfh THEN Close(mfh)
ENDPROC

PROC setsum(m:PTR TO moduleheader)
   DEF lptr:REG PTR TO LONG, longsize:REG, sum=NIL:REG

   lptr := m.checksumstart
   longsize := Div(m + m.modsize - lptr,4) + 1

   WHILE longsize-- DO sum := sum + lptr[]++

   m.checksum := sum

ENDPROC

-> items: values > 1000 : labels  (lowercase | uppercase | "_"), {lowercase | uppercase | digit | "_"}
->        values < 1000 : special chars: "(" ")" "*" ","

PROC process(s, libi:PTR TO modlibi, lfunc:PTR TO modlfunc)
   DEF incomment=FALSE
   DEF c, t, pos, len, a
   DEF iptr:PTR TO LONG
   DEF ireg, freg, sofs:PTR TO LONG, vreg
   DEF lfuncarg:PTR TO lfuncarg, tstr[100]:STRING

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

         IF iptr[] < 256 THEN reportErr('label expected')

         IF StrCmp(iptr[], 'typedef')

            iptr++
            IF iptr[] < 256 THEN reportErr('label expected')
            iptr, t := parsetype(iptr)
            IF iptr[] < 256 THEN reportErr('alias expected')
            addTypeDef(t, iptr[])

         ELSE

            iptr,t := parsetype(iptr)
            lfunc.return := IF t = -1 THEN 0 ELSE t

            IF iptr[] < 256 THEN reportErr('function name expected')
            IF StrCmp(iptr[], 'APICALL')
               iptr++
               IF iptr[]++ <> "(" THEN reportErr('"(" exected')
               IF iptr[]++ <> "*" THEN reportErr('"*" exected')
               IF iptr[] < 256 THEN reportErr('label expected')
               StrCopy(tstr, iptr[]++)
               IF iptr[]++ <> ")" THEN reportErr('")" exected')
            ELSE
               StrCopy(tstr, iptr[]++)
            ENDIF

            IF g_namefix
               IF tstr[0] > 96 THEN tstr[0] := tstr[0]  - 32
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
            lfunc.name := getStrOfs(tstr)

            IF iptr[]++ <> "(" THEN reportErr('"(" expected')

            lfunc.type := 1

            lfuncarg := lfunc + SIZEOF modlfunc

            ireg := IF g_basereg = 3 THEN 4 ELSE 3
            freg := 1
            sofs := 8
            vreg := 2
            a := 0

            WHILE iptr[] > 256 -> while label
               EXIT iptr[] = "." -> "." is actually "..."
               iptr, t := parsetype(iptr)
               EXIT t = -1 -> VOID
               IF (g_basereg = 3) AND (a=0)
                  -> skip first arg if basesysv
                  a := 1
                  iptr++
               ELSE
                  lfunc.nrofargs := lfunc.nrofargs + 1
                  IF iptr[] > 256  -> argname ?
                     lfuncarg.name := getStrOfs(iptr[]++)
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
               ENDIF
               EXIT iptr[] <> ","
               iptr++
            ENDWHILE

            IF iptr[] = "."
               iptr++
               lfunc.flags := lfunc.flags OR LFFLAG_VARARGS -> Oct 2008.
            ENDIF

            IF iptr[] <> ")" THEN reportErr('")" expected')

            IF g_basereg = -1
               lfunc.nrofargs := lfunc.nrofargs - 1
               lfuncarg--
            ENDIF

            lfunc.basertype := IF g_basereg <> -1 THEN 0 ELSE (IF ireg > 10 THEN 1 ELSE 0)
            lfunc.basernum := IF g_basereg <> -1 THEN g_basereg ELSE (IF ireg > 10 THEN sofs ELSE ireg)
            -> 1.3
            IF g_kernel
               lfunc.flags := LFFLAG_KERNELFUNC
               lfunc.basertype := 0
               lfunc.basernum := 0
            ENDIF
            lfunc.baseofs := g_bias
            g_bias := g_bias + g_inc
            lfunc.totalsize := lfuncarg - lfunc
            lfunc := lfuncarg
            libi.nroflfuncs := libi.nroflfuncs + 1

         ENDIF

         iptr := g_itemsarray -> reset
         iptr[] := NIL -> destroy old data incase another ";" immediately turns up

      DEFAULT
         s++
         ->reportErr('unexpected characters')
      ENDSELECT

   ENDWHILE

      IF incomment > 0 THEN reportErr('start of comment without end')


ENDPROC lfunc

PROC parsetype(iptr:PTR TO LONG)
   DEF t=0, str[100]:STRING

   StrCopy(str, iptr[])
   LowerStr(str)

   IF StrCmp(str, 'const')
      iptr++
      StrCopy(str, iptr[])
      LowerStr(str)
   ENDIF

   IF StrCmp(str, 'struct')
      t := 0
      iptr++ -> skip struct
      iptr++ -> skip structname
      IF iptr[]++ <> "*" THEN reportErr('function returns object by *value*, WTF ?!?')
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

   ELSEIF StrCmp(s, 'IPTR')            ; t := 0

   ELSEIF StrCmp(s, 'TAG')             ; t := 0
   ELSEIF StrCmp(s, 'INT64')           ; t := 3
   ELSEIF StrCmp(s, 'INT32')           ; t := 0
   ELSEIF StrCmp(s, 'INT16')           ; t := 0
   ELSEIF StrCmp(s, 'INT8')            ; t := 0
   ELSEIF StrCmp(s, 'UINT')            ; t := 0
   ELSEIF StrCmp(s, 'UINT64')          ; t := 3
   ELSEIF StrCmp(s, 'UINT32')          ; t := 0
   ELSEIF StrCmp(s, 'UINT16')          ; t := 0
   ELSEIF StrCmp(s, 'UINT8')           ; t := 0
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
         DEFAULT  ; WriteF('Bad choice! Lets say you ment "i" then..\n') ; t := 0
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

CHAR '$VER: '
version:
CHAR 'ModuleFromProto 1.5 (XX.10.2008) Copyright (c) Leif Salomonsson 2006-2008',0



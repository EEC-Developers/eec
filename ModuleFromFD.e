/* ModuleFromFD by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2004-2007 .*/
/* Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT */

-> July 2004
-> produces ecx binary module.

-> Dec 2004: SYSV offset fix. add 2 to the offset!

-> Jan 2005. FDs using no base (sysv) didnt work.. *FIXING*
-> also added support for (sysv,r12base)

-> Jan 2005. v49. new arg: INC/N. WIth this offset increment can be
-> set to something else than -6, could be useful for "OS4-FDs".
-> new arg: BIAS/N

-> Sept 2005: >10 args bugfix. made the parsing of functions a bit less picky.
-> bugfix: did not longword align sizeof resulting module.

-> July 2008: added prefix option. prefixes names of functions.


MODULE '*binary'

/* automatic errorchecking */
RAISE "ARGS" IF ReadArgs()=NIL
RAISE "OPEN" IF Open()=NIL
RAISE "FILE" IF FileLength() < 1
RAISE "MEM"  IF New()=NIL


DEF mod:PTR TO moduleheader, labelsarray[1024]:ARRAY OF LONG, numlabs=0, g_linenum=1

PROC getStrOfs(str)
   DEF a, len, offset=0, newstr
   FOR a := 0 TO numlabs-1
      IF StrCmp(str, labelsarray[a]) THEN RETURN offset
      len := EstrLen(labelsarray[a])
      offset := offset + len + 1
   ENDFOR
   len := EstrLen(str)
   newstr := String(len)
   IF newstr = NIL THEN Raise("MEM")
   StrCopy(newstr, str)
   labelsarray[numlabs++] := newstr
ENDPROC offset

PROC getWordLen(str)
   DEF c:REG, i:REG
   i := 0
   WHILE (c := str[i])
      SELECT 128 OF c
      CASE "0" TO "9", "A" TO "Z", "a" TO "z", "_", "*", "."
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



ENUM ARG_FD, ARG_MODDIR, ARG_BASE, ARG_VERB, ARG_NAMEFIX, ARG_INC, ARG_BIAS,
     ARG_PREFIX,  NROFARGS

PROC main() HANDLE
   DEF temp:PTR TO LONG, rdargs
   DEF ffh=NIL, mfh=NIL
   DEF info:PTR TO modinfo
   DEF libi:PTR TO modlibi, lfunc:PTR TO modlfunc, lfuncarg:PTR TO lfuncarg
   DEF ffile[256]:STRING
   DEF mdir[200]:STRING
   DEF bias, public=TRUE, basename[50]:STRING
   DEF verbose=FALSE, showprivate=FALSE, namefix=TRUE, lowerbase=TRUE
   DEF nrofparams
   DEF count=NIL
   DEF str[1000]:STRING
   DEF pnum, rnum, params[16]:ARRAY OF LONG, regs[16]:ARRAY OF LONG
   DEF a, t, line, s, wptr:PTR TO LONG
   DEF fbuf, fsize
   DEF c, base=NIL, v, r
   DEF inc, prefix[20]:STRING

   WriteF(' - \s\n', {version})

   NEW temp[NROFARGS]

   /* read args */
   rdargs := ReadArgs(
   'FD/A,MODULEDIR/A,BASE/K,VERBOSE/S,NAMEFIX/S,INC/N,BIAS/N,PREFIX/K',
    temp, NIL)

   /* get args */
   StrCopy(ffile, temp[ARG_FD])

   WriteF('   .. processing "\s" ..\n', ffile)

   StrCopy(mdir, temp[ARG_MODDIR])
   IF temp[ARG_BASE] THEN base := StrCopy(String(StrLen(temp[ARG_BASE])),temp[ARG_BASE])

   verbose := temp[ARG_VERB]

   namefix := temp[ARG_NAMEFIX]

   IF temp[ARG_INC] THEN inc := Long(temp[ARG_INC]) ELSE inc := -6
   IF temp[ARG_BIAS] THEN bias := Long(temp[ARG_BIAS]) ELSE bias := -30

   IF temp[ARG_PREFIX] THEN StrCopy(prefix, temp[ARG_PREFIX])

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

   s := fbuf

   WHILE (c := s[])
      SELECT 128 OF c
      CASE "*"
         t := InStr(s, '\n')
         IF t < 1 THEN reportErr('comment not terminated')
         IF verbose
            StrCopy(str, s, t+1)
            WriteF(str)
         ENDIF
         s := s + t
      CASE " ", "\t"
         s++
      CASE "\n"
         g_linenum++
         s++
      CASE "#"
         s++
         IF s[]++ <> "#" THEN reportErr('"#" expected')
         IF     StrCmp(s, 'base ', STRLEN)
            s := s + STRLEN
            t := getWordLen(s)
            IF t = NIL THEN reportErr('basename expected')
            IF s[] = "_"
               StrCopy(basename, s+1, t-1)
            ELSE
               StrCopy(basename, s, t)
            ENDIF
            LowerStr(basename)
            IF base = NIL THEN libi.basename := getStrOfs(basename)
            s := s + t
         ELSEIF StrCmp(s, 'bias ', STRLEN)
            s := s + STRLEN
            v,r := Val(s)
            IF r = NIL THEN reportErr('bias expected')
            bias := -v
            s := s + r
         ELSEIF StrCmp(s, 'public', STRLEN)
            s := s + STRLEN
            public := TRUE
         ELSEIF StrCmp(s, 'private', STRLEN)
            s := s + STRLEN
            public := FALSE
         ELSEIF StrCmp(s, 'end', STRLEN)
            s[] := NIL
         ELSE
            reportErr('unknown directive')
         ENDIF
      CASE "_", "a" TO "z", "A" TO "Z"
         IF public
            IF base
               StrCopy(basename, base)
               libi.basename := getStrOfs(basename)
               base := NIL
            ENDIF
            StrCopy(str, prefix)
            t := getWordLen(s)
            StrAdd(str, s, t)
            IF namefix
               IF str[0] > 96 THEN str[0] := str[0]  - 32
               IF str[1] < 91
                  str[1] := str[1] + 32
                  IF str[2] < 91
                     IF str[3] = "_"
                        str[2] := str[2] + 32
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            IF verbose
               WriteF('\s\n', str)
            ENDIF
            lfunc.name := getStrOfs(str)
            s := s + t
            IF s[]++ <> "(" THEN reportErr('"(" expected')
            s := TrimStr(s)
            pnum := 0
            WHILE (t := getWordLen(s))
               StrCopy(str, s, t)
               params[pnum++] := getStrOfs(str)
               s := TrimStr(s + t)
               EXIT s[]=")"
               IF s[]++ <> "," THEN reportErr('"," expected')
               WHILE s[] = " " DO s++
            ENDWHILE
            s++
            lfunc.nrofargs := pnum
            IF s[]++ <> "(" THEN reportErr('"(" expected')
            rnum := 0
            WHILE (t := getWordLen(s))
               StrCopy(str, s, t)
               s := s + t
               t := Long(str)
               IF t = "base"
                  lfunc.basernum := rnum + 3
               ELSEIF t = "sysv"
                  lfunc.type := 1
               ELSEIF t = "r12b"
                  lfunc.basernum := 12
               ENDIF
               regs[rnum++] := t
               s := TrimStr(s)
               EXIT s[] = ")"
               IF s[] <> "/"
                  IF s[] <> "," THEN reportErr('"," or "/" expected')
               ENDIF
               s++
               WHILE s[] = " " DO s++
            ENDWHILE
            s++
            IF lfunc.type = 0
               IF rnum < pnum THEN reportErr('registers dont match arguments')
            ELSE
               IF lfunc.basernum = 3 THEN t := 4 ELSE t := 3
            ENDIF
            lfuncarg := lfunc + SIZEOF modlfunc
            FOR a := 0 TO pnum-1
               lfuncarg.name := params[a]
               IF lfunc.type = 0
                  lfuncarg.rtype := IF Shr(regs[a],24) = "d" THEN 0 ELSE 1
                  lfuncarg.rnum := Shr(regs[a],16) AND $FF - 48
               ELSE
                  lfuncarg.rtype := IF t > 10 THEN PPCARGRTYPE_STACKRX ELSE PPCARGRTYPE_RX
                  lfuncarg.rnum := IF t > 10 THEN t++ - 11 * 4 + 8 ELSE t++
               ENDIF
               lfuncarg++
            ENDFOR
            lfunc.baseofs := bias + IF lfunc.type = 1 THEN 2 ELSE 0
            lfunc.totalsize := lfuncarg - lfunc
            lfunc := lfuncarg
            libi.nroflfuncs := libi.nroflfuncs + 1
         ELSE
            t := InStr(s, '\n')
            s := s + IF t = -1 THEN StrLen(s) ELSE t
         ENDIF
         bias := bias + inc
      DEFAULT
         reportErr('unexpected characters')
      ENDSELECT

   ENDWHILE

   mod.identification := "ECXM"
   mod.headsize := SIZEOF moduleheader
   info := lfunc
   mod.strtabinfo := info - mod
   wptr := info + SIZEOF modinfo
   FOR a := 0 TO numlabs-1
      s := labelsarray[a]
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

   IF verbose
      WriteF('\e[31m - processed \d functions\n', libi.nroflfuncs)
      WriteF(' - base : \s\n', basename)
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
      IF exception > 0 THEN PutStr('unknown exception occoured!\n')
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

CHAR '$VER: '
version:
CHAR 'ModuleFromFD by LS 2004-08',0




/* ViewModule.e by Leif Salomonsson is Copyright (c) 2003-2008 */
/* Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT */



MODULE '*binary'
MODULE 'dos/dos'

-> v44: now prepends filename with "ecxmodules:" if it cannot be opened.
-> v45: compiled with fixed bin2   '
-> v47: now displays class methods too.
-> v48: vector args for libfuncs supported.
-> v50: supports new types in objects
-> 1.9: valstr() didnt handle the value $80000000 right, fixed.
-> Mar 2007: 1.10: now prints out OS if defined. see moduleheader.oschars. FU:this=suck
-> May 2007: 1.11: now checks mod.modsize to match with flen.
-> May 2007: 1.11: now checks mod.modversion to be atleast 1.
-> May 2008: 1.12: added support for static labels
-> Jul 2008: 1.13: now soesnt show globals if using ___ prefix, unless DEBUG.
-> Sep 2008: 1.14: now prints out head.osid in human format
-> Nov 2008: 1.15: now gives clue about static method
-> Mar 2009: 1.16: removed stupid /* */ stuff.
-> Sep 2009: 1.17: support for new modstatic
-> Feb 2011: 1.18: increased space for offset

PROC main() HANDLE
   DEF fh=NIL, str[256]:STRING, rdargs=NIL, args[2]:ARRAY OF LONG
   DEF fib:fileinfoblock, flen, fbuf, filename[300]:STRING
   DEF a, b, c, lptr:PTR TO LONG, e
   DEF head:PTR TO moduleheader
   DEF strtable
   DEF info:PTR TO modinfo
   DEF proc:PTR TO modproc
   DEF object:PTR TO modobject
   DEF class:PTR TO modclass
   DEF libi:PTR TO modlibi
   DEF lfunc:PTR TO modlfunc
   DEF macro:PTR TO modmacro
   DEF param:PTR TO modarg
   DEF member:PTR TO modmember
   DEF method:PTR TO modproc
   DEF submod:PTR TO modmodule
   DEF modref:PTR TO modref
   DEF vd:PTR TO vardbg
   DEF debug=FALSE
   DEF lparam:PTR TO lfuncarg


   /* read agrs */
   rdargs := ReadArgs('MODULE/A, DUMMY/M',
                      args, NIL)

   IF rdargs = NIL THEN Raise("ARGS")

   IF args[1]
      IF StrCmp(Long(args[1]), 'DEBUG', STRLEN) THEN debug:=TRUE
   ENDIF


   StrCopy(str, args[])

   IF StrCmp(str+EstrLen(str)-2, '.m') = FALSE THEN StrAdd(str, '.m')
   StrCopy(filename, str)

   fh := Open(filename, OLDFILE)
   IF fh = NIL
      StrCopy(str, 'ECXMODULES:')
      StrAdd(str, filename)
      StrCopy(filename, str)
      fh := Open(filename, OLDFILE)
      IF fh = NIL THEN Raise("OPEN")
   ENDIF

   PrintF('ViewModule 1.17 by LS 2003-09 -> \q\s\q\n\n', filename)

   ExamineFH(fh, fib)
   flen := fib.size
   fbuf := NewR(flen+4)
   Read(fh, fbuf, flen)
   head := fbuf

   IF head.identification <> "ECXM" THEN Raise("FORM")

   IF head.modversion < 1 THEN Raise("TOLD")  -> 1.11

   IF head.modsize <> flen THEN Raise("SIZE") -> 1.11

   IF head.codeinfo
      IF head.cpu = CPU_NONE
         -> do nothing
      ELSEIF head.cpu = CPU_PPC
         PrintF('CPU: PowerPC \n\n')
      ELSEIF head.cpu = CPU_M68
         PrintF('CPU: M68020+FPU \n\n')
      ELSE
         PrintF('CPU: ??? (\d) \n\n', head.cpu)
      ENDIF
   ENDIF

   -> 2.00
   IF head.osid THEN PrintF('Operating System: \s \n\n', ListItem(['',
                                                                        'AmigaOS',
                                                                        'AmigaOS4',
                                                                        'MorphOS',
                                                                        'AROS',
                                                                        'Linux',
                                                                        '???'], head.osid))

   IF head.osversion THEN PrintF('OS Version: \d \n\n', head.osversion)

   info := head + head.strtabinfo
   strtable := info + SIZEOF modinfo

   info := head.libiinfo
   IF info
      info := info + head
      libi := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', libi-head)
      c := info.count + 1
      WHILE c--
         PrintF('LIBRARY \s\n', libi.basename + strtable)
         lfunc := libi + SIZEOF modlibi
         FOR a := 0 TO libi.nroflfuncs-1
            PrintF('   \s(', lfunc.name + strtable)
            lparam := lfunc + SIZEOF modlfunc
            FOR b := 0 TO lfunc.nrofargs-1
               IF lfunc.type = 0
                  PrintF('\c\d', IF lparam.rtype = 0 THEN "D" ELSE "A", lparam.rnum)
               ELSEIF lfunc.type = 1
                  IF     lparam.rtype = PPCARGRTYPE_RX
                     PrintF('R\d', lparam.rnum)
                  ELSEIF lparam.rtype = PPCARGRTYPE_STACKRX
                     PrintF('S\d', lparam.rnum)
                  ELSEIF lparam.rtype = PPCARGRTYPE_FX
                     PrintF('F\d', lparam.rnum)
                  ELSEIF lparam.rtype = PPCARGRTYPE_VX
                     PrintF('V\d', lparam.rnum)
                  ELSEIF lparam.rtype = PPCARGRTYPE_RX2
                     PrintF('R\d+R\d', lparam.rnum, lparam.rnum+1)
                  ELSEIF lparam.rtype = PPCARGRTYPE_STACKRX2
                     PrintF('S\d+S\d', lparam.rnum, lparam.rnum+4)
                  ELSE
                     PrintF('<\d>\d', lparam.rtype, lparam.rnum)
                  ENDIF
               ENDIF
               lparam++
               IF b < (lfunc.nrofargs-1) THEN PrintF(',')
            ENDFOR
            IF lfunc.flags AND LFFLAG_VARARGS -> 2.0
               PrintF(',...')
            ENDIF
            PrintF(')')
            IF lfunc.basernum
               IF lfunc.type = 0
               ELSE
                  IF lfunc.basertype = 0
                     PrintF(' [R\d]',  lfunc.basernum)
                  ELSE
                     PrintF(' [S\d]',  lfunc.basernum)
                  ENDIF
               ENDIF
            ENDIF
            IF lfunc.type = 1 THEN PrintF(' = \s', ListItem(['R3','F1','V2','R3+R4'], lfunc.return))
            PrintF(' @ \d \n', lfunc.baseofs)
            lfunc := lfunc + lfunc.totalsize
         ENDFOR
         PrintF('ENDLIBRARY\n\n')
         libi := libi + libi.totalsize
      ENDWHILE
   ENDIF

   info := head.xrefginfo
   IF info
      a := FALSE
      info := info + head
      c := info.count + 1
      modref := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', modref-head)
      WHILE c--
         IF (StrCmp(modref.name + strtable, '___', STRLEN) = FALSE) OR (debug <> FALSE)
            IF a = FALSE
               PrintF('DEF')
               a := TRUE
            ELSE
               PrintF('   ')
            ENDIF
            PrintF(' \s (x\d)', modref.name + strtable, modref.numrefs)
            IF c = 1 THEN PrintF('\n') ELSE PrintF(',\n')
         ENDIF
         modref := modref + SIZEOF modref + Mul(modref.numrefs,4)
      ENDWHILE
      PrintF('\n')
   ENDIF

   info := head.procinfo
   IF info
      info := info + head
      proc := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ PROCOFFSET $\h]\n', proc-head)
      c := info.count + 1
      WHILE c--
         IF debug
            WriteF('[@ CODEOFFSET $\h]\n', proc.offset)
         ENDIF
         IF proc.totalsize = SIZEOF modlabel -> label ?
            PrintF('\s:', proc.name + strtable)
            PrintF('\n')
         ELSEIF proc.totalsize = SIZEOF modstatic
            PrintF('\s []:', proc.name + strtable)
            a := proc::modstatic.statesize
            SELECT a
            CASE 1      ; a := ['CHAR','BYTE','ERR']
            CASE 2      ; a := ['WORD','INT','ERR']
            CASE 4      ; a := ['ULONG','LONG','FLOAT']
            CASE 8      ; a := ['UWIDE','WIDE','DOUBLE']
            CASE 255    ; a := [proc::modstatic.statobj + strtable,'ERR','ERR']
            DEFAULT     ; a := NIL
            ENDSELECT
            e := proc::modstatic.statflags
            PrintF(IF a THEN '\s /* SIZEOF = \d */\n' ELSE '??? /* SIZEOF = \d */\n', ListItem(a,
               IF e AND MEMBF_FLOAT THEN 2 ELSE IF e AND MEMBF_SIGNED THEN 1 ELSE 0),
               proc::modstatic.sizeof)
         /*
         ELSEIF proc.totalsize = SIZEOF oldmodstatic
            PrintF('\s []:', proc.name + strtable)
            a := proc::oldmodstatic.statesize
            SELECT a
            CASE 1      ; a := ['CHAR','BYTE','ERR']
            CASE 2      ; a := ['WORD','INT','ERR']
            CASE 4      ; a := ['ULONG','LONG','FLOAT']
            CASE 8      ; a := ['UWIDE','WIDE','DOUBLE']
            CASE 255    ; a := [proc::oldmodstatic.statobj + strtable,'ERR','ERR']
            DEFAULT     ; a := NIL
            ENDSELECT
            e := proc::oldmodstatic.statflags
            PrintF(IF a THEN '\s\n' ELSE '???', ListItem(a,
               IF e AND MEMBF_FLOAT THEN 2 ELSE IF e AND MEMBF_SIGNED THEN 1 ELSE 0))
         */
         ELSE
            PrintF('PROC \s(', proc.name + strtable)
            param := proc + SIZEOF modproc
            FOR b := 0 TO proc.nrofargs-1
               PrintF('\s', param.name + strtable)
               IF b >= (proc.nrofargs-proc.nrofdefaults)
                  PrintF('=\s', valstr(param.defval, str))
               ENDIF
               IF debug THEN PrintF(':\d|\d', param.rtype, param.rnum)
               IF b < (proc.nrofargs-1) THEN PrintF(',')
               param++
            ENDFOR
            PrintF(')\n')
         ENDIF
         IF c = 1 THEN PrintF('\n')
         proc := proc + proc.totalsize
      ENDWHILE
   ENDIF

   info := head.objectinfo
   IF info
      info := info + head
      object := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', object-head)
      c := info.count + 1
      WHILE c--
         PrintF('(----) OBJECT \s\n', object.name + strtable)
         IF debug THEN PrintF('flags=$\h\n', object.flags)
         member := object + SIZEOF modobject
         a := object.nrofmembers + 1
         WHILE a--
            PrintF('(\r\d[6])    \s\s\n',
                   member.offset,
                   member.name + strtable,
                   IF info.misc = 1 THEN
                   typestr50(member.size, member.esize, member.numes, member.object, member.flags, strtable, str) ELSE
                   typestr(member.size, member.esize, member.numes, member.object, member.flags, strtable, str))
            member++
         ENDWHILE
         class := member
         method := member
         a := object.nrofmethods + 1
         IF a > 1 THEN method := class + SIZEOF modclass
         WHILE a--
            IF method.name
               PrintF('(   \c)    \s (',
                  IF method.flags AND PROCF_CLMETH THEN "*" ELSE " ",
                  method.name + strtable)
               param := method + SIZEOF modproc
               FOR b := 0 TO method.nrofargs-1
                  PrintF('\s', param.name + strtable)
                  IF b >= (method.nrofargs-method.nrofdefaults)
                     PrintF('=\s', valstr(param.defval, str))
                  ENDIF
                  IF b < (method.nrofargs-1) THEN PrintF(',')
                  param++
               ENDFOR
               PrintF(')')
               IF debug
                  PrintF(' codeofs=\d, indx=\d',
                  method.offset, object.nrofmethods - a)
               ENDIF
               PrintF('\n')
               method := param
            ELSE
               method := method + SIZEOF modproc
            ENDIF
         ENDWHILE
         IF (debug<>NIL) AND object.nrofmethods
            PrintF('classofs=\d, destindx=\d, offset=\d\n',
            class.classofs, class.destofs, class.offset)
         ENDIF
         PrintF('(----) ENDOBJECT /* SIZEOF = \d */\n\n', object.sizeof)
         object := object + object.totalsize
      ENDWHILE
   ENDIF

   info := head.constinfo
   IF info
      info := info + head
      lptr := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', lptr-head)
      b := info.count + 1
      WHILE b--
         PrintF(IF b = info.count THEN 'CONST ' ELSE '      ')
         PrintF('\s = \s', lptr[]++ + strtable, valstr(lptr[]++, str))
         IF b > 1 THEN PrintF(',\n')
      ENDWHILE
      PrintF('\n\n')
   ENDIF

   info := head.debuginfo
   IF info <> 0 AND debug
      info := info + head
      PrintF('DEBUG\n')
      vd := info + SIZEOF modinfo
      WriteF('[@ OFFSET $\h]\n', vd-head)
      c := info.count + 1
      WHILE c--
         PrintF('   \s', vd.name + strtable)
         IF vd.inproc
            PrintF(' in PROC \s', vd.inproc + strtable)
         ENDIF
         IF vd.ofobject
            PrintF(' OF \s ', vd.ofobject + strtable)
         ENDIF
         PrintF(' IS \s,', typestr50(vd.varsize, vd.varesize, 0, vd.varobject, vd.varflags, strtable,str))
         PrintF('\d,\d\n', vd.varreg,vd.varofs)
         vd++
      ENDWHILE
      PrintF('\n')
   ENDIF

   info := head.macroinfo
   IF info
      info := info + head
      macro := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', macro-head)
      c := info.count + 1
      WHILE c--
         IF macro.type = 0
            PrintF('#define \s', macro.name + strtable)
            lptr := macro + SIZEOF modmacro
         ELSEIF Odd(macro.type)
            PrintF('#define \s'+'(', macro.name + strtable)
            lptr := macro + SIZEOF modmacro
            FOR a := 0 TO macro.nrofargs-1
               PrintF('\s', lptr[]++ + strtable)
               IF a < (macro.nrofargs-1) THEN PrintF(',')
            ENDFOR
            IF macro.type = 3 THEN PrintF(',...')
            PrintF(')')
         ELSEIF macro.type = 2
            PrintF('MACRO \s \s', macro.name + strtable,
               IF macro.nrofargs THEN StringF(str, '/\d', macro.nrofargs) ELSE '')
            lptr := macro + SIZEOF modmacro
         ENDIF
         macro := macro + macro.totalsize
         IF debug THEN PrintF(' \s', lptr) -> body
         IF c > 1 THEN PrintF('\n') ELSE PrintF('\n\n')
      ENDWHILE
   ENDIF

   info := head.moduleinfo
   IF info
      info := info + head
      submod := info + SIZEOF modinfo
      IF debug THEN WriteF('[@ OFFSET $\h]\n', submod-head)
      c := info.count + 1
      WHILE c--
         PrintF('Code from module \q\s\s\q used:\n', IF submod.name < 0 THEN '*' ELSE '',
                                                  Abs(submod.name) + strtable)
         modref := submod + SIZEOF modmodule
         a := submod.numsymbs + 1
         WHILE a--
            IF modref.ltype = LTYPE_PROC
               PrintF('   \s/\d (x\d)\n', modref.name + strtable, modref.info, modref.numrefs)
            ELSEIF modref.ltype = LTYPE_LAB
               PrintF('   \s: (x\d)\n', modref.name + strtable, modref.numrefs)
            ELSEIF modref.ltype = LTYPE_CLASS
               PrintF('   OBJECT \s (x\d)\n', modref.name + strtable, modref.numrefs)
            ELSE
               PrintF('   ???\n')
            ENDIF
            IF debug
               lptr := modref + SIZEOF modref
               b := modref.numrefs+1
               WHILE b--
                  PrintF('      \d, $\h\n', Shr(lptr[], 24) AND $FF, lptr[] AND $FFFFFF)
                  lptr++
               ENDWHILE
            ENDIF
            modref := modref + SIZEOF modref + Mul(modref.numrefs, 4)
         ENDWHILE
         submod := submod + submod.totalsize
         IF c > 1 THEN PrintF('\n')
      ENDWHILE
      PrintF('\n')
   ENDIF

   IF head.codeinfo OR
      head.datainfo OR
      head.globinfo OR
      head.relocinfo OR
      head.lineinfo OR
      head.debuginfo OR
      (head.globinitinfo<>NIL AND (head.headsize >=160))

      PrintF('Module contains:\n')
      info := head.codeinfo
      IF info
         info := info + head
         PrintF('   \d bytes code\n', Mul(info.count,4))
      ENDIF
      info := head.datainfo
      IF info
         info := info + head
         PrintF('   \d bytes data\n', Mul(info.count,4))
      ENDIF
      info := head.globinfo
      IF info
         info := info + head
         PrintF('   \d private globals (\d bytes)\n', info.count, info.misc)
      ENDIF
      info := head.relocinfo
      IF info
         info := info + head
         PrintF('   \d relocations\n', info.count)
      ENDIF
      info := head.lineinfo
      IF info
         info := info + head
         PrintF('   \d bytes linedebug\n', Mul(info.count,8))
      ENDIF
      info := head.debuginfo
      IF info
         info := info + head
         PrintF('   \d bytes vardebug\n', Mul(info.count,SIZEOF vardbg))
      ENDIF
      info := head.globinitinfo
      IF info
         info := info + head
         PrintF('   \d bytes private global arrays\n', Mul(info.count,4))
      ENDIF

      PrintF('\n')
   ENDIF


   PrintF('\n')

EXCEPT DO

   IF fh THEN Close(fh)
   IF rdargs THEN FreeArgs(rdargs)
   SELECT exception
   CASE "MEM"  ; PrintF('Error: out of memory.\n')
   CASE "FORM" ; PrintF('Error: not an ecx module.\n')
   CASE "ARGS" ; PrintF('Error: bad arguments.\n')
   CASE "OPEN" ; PrintF('Error: could not open file.\n')
   CASE "TOLD" ; PrintF('Error: module is too old.\n')
   CASE "SIZE" ; PrintF('Error: module is corrupt (wrong modsize).\n')
   CASE NIL
   DEFAULT     ; PrintF('Error: unknown.\n')
   ENDSELECT

ENDPROC exception



PROC valstr(val, str)
   IF val = $80000000  -> this value actually gets smaller than 255 when Abs():ed.
      StringF(str, '$\h', val)
   ELSEIF Abs(val) > 255
      StringF(str, '$\h', val)
   ELSE
      StringF(str, '\d', val)
   ENDIF
ENDPROC str


PROC typestr(size, esize, numes, object, flags, strtable, str)
   SELECT size
   CASE 0
      SELECT esize
      CASE 1   ; StringF(str, '[\d]:ARRAY OF CHAR', numes)
      CASE 2   ; StringF(str, '[\d]:ARRAY OF INT', numes)
      CASE 4   ; StringF(str, '[\d]:ARRAY OF LONG', numes)
      CASE 8   ; IF flags AND MEMBF_FLOAT
               ;    StringF(str, '[\d]:ARRAY OF DOUBLE', numes)
               ; ELSE
               ;    StringF(str, '[\d]:ARRAY OF QUAD', numes)
               ; ENDIF
      CASE 255 ; IF numes = 1
               ;    StringF(str, ':\s', object + strtable)
               ; ELSE
               ;    StringF(str, '[\d]:ARRAY OF \s', numes, object + strtable)
               ; ENDIF
      ENDSELECT
   CASE 1 ; StringF(str, ':CHAR')
   CASE 2 ; StringF(str, ':INT')
   CASE 4
      SELECT esize
      CASE 0   ;  StringF(str, ':LONG')
      CASE 1   ; StringF(str, ':PTR TO CHAR')
      CASE 2   ; StringF(str, ':PTR TO INT')
      CASE 4   ; StringF(str, ':PTR TO LONG')
      CASE 8   ; IF flags AND MEMBF_FLOAT
               ;    StringF(str, ':PTR TO DOUBLE')
               ; ELSE
               ;    StringF(str, ':PTR TO QUAD')
               ; ENDIF
      CASE 16  ; StringF(str, ':PTR TO VECTOR')
      CASE 255 ; StringF(str, ':PTR TO \s', object + strtable)
      ENDSELECT
   CASE 8
      IF flags AND MEMBF_FLOAT
         StringF(str, ':DOUBLE')
      ELSE
         StringF(str, ':QUAD')
      ENDIF
   ENDSELECT
ENDPROC str

-> v50. new FLOAT,WORD,BYTE,WIDE,VECTOR support added.
PROC typestr50(size, esize, numes, object, flags, strtable, str)
   SELECT size
   CASE 0
      StringF(str, '[\d]:ARRAY OF ', numes)
      SELECT esize
      CASE 1   ; StrAdd(str, IF flags AND MEMBF_SIGNED THEN 'BYTE' ELSE 'CHAR')
      CASE 2   ; StrAdd(str, IF flags AND MEMBF_SIGNED THEN 'INT' ELSE 'WORD')
      CASE 4   ; StrAdd(str, IF flags AND MEMBF_FLOAT THEN 'FLOAT' ELSE
                              (IF flags AND MEMBF_SIGNED THEN 'LONG' ELSE 'ULONG'))
      CASE 8   ; StrAdd(str, IF flags AND MEMBF_FLOAT THEN 'DOUBLE' ELSE 'WIDE')
      CASE 16  ; StrAdd(str, 'VECTOR')
      CASE 255 ; IF numes <> 1
               ;    StrAdd(str, object + strtable)
               ; ELSE
               ;    StringF(str, ':\s', object + strtable)
               ; ENDIF
      ENDSELECT
   CASE 1 ; StrCopy(str, IF flags AND MEMBF_SIGNED THEN ':BYTE' ELSE ':CHAR')
   CASE 2 ; StrCopy(str, IF flags AND MEMBF_SIGNED THEN ':INT' ELSE ':WORD')
   CASE 4
      SELECT esize
      CASE 0   ; StrCopy(str, IF flags AND MEMBF_SIGNED THEN ':LONG' ELSE
                              (IF flags AND MEMBF_FLOAT THEN ':FLOAT' ELSE ':ULONG'))
      CASE 1   ; StringF(str, ':PTR TO \s', IF flags AND MEMBF_SIGNED THEN 'BYTE' ELSE 'CHAR')
      CASE 2   ; StringF(str, ':PTR TO \s', IF flags AND MEMBF_SIGNED THEN 'INT' ELSE 'WORD')
      CASE 4   ; StringF(str, ':PTR TO \s', IF flags AND MEMBF_SIGNED THEN 'LONG' ELSE
                              (IF flags AND MEMBF_FLOAT THEN 'FLOAT' ELSE 'ULONG'))
      CASE 8   ; StringF(str, ':PTR TO \s', IF flags AND MEMBF_FLOAT THEN 'DOUBLE' ELSE 'WIDE')
      CASE 16  ; StrCopy(str, ':PTR TO VECTOR')
      CASE 255 ; StringF(str, ':PTR TO \s', object + strtable)
      ENDSELECT
   CASE 8
      StrCopy(str, IF flags AND MEMBF_FLOAT THEN ':DOUBLE' ELSE ':WIDE')
   CASE 16
      StrCopy(str, ':VECTOR')
      SELECT esize
      CASE 4   ; StrAdd(str, IF flags AND MEMBF_FLOAT THEN ' OF FLOAT' ELSE ' OF LONG')
      CASE 2   ; StrAdd(str, IF flags AND MEMBF_SIGNED THEN ' OF BYTE' ELSE ' OF CHAR')
      CASE 1   ; StrAdd(str, IF flags AND MEMBF_SIGNED THEN ' OF INT' ELSE ' OF WORD')
      ENDSELECT
   ENDSELECT
ENDPROC str



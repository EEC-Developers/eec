/* CeeModule.e by Leif Salomonsson is Copyright (c) 2008 */
/* Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT */

-> Prints out ECX module in C-friendlier way.

-> Libraryfunctions as either FD or Prototypes.
-> Objects as Structs.
-> Constants as #defines.

-> Requires ecx 1.10+ compiled modules to display libfunc argument names properly.

-> May 2008: 1.0
-> Sep 2009: 1.1: made types into exec/types.h style instead.
-> Mar 2010: 1.2: silly field array print bug fixed.

MODULE '*binary'
MODULE 'dos/dos'

OBJECT args
   module
   libprotos
ENDOBJECT

PROC main() HANDLE
   DEF fh=NIL, str[256]:STRING, rdargs=NIL, args:PTR TO args
   DEF fib:fileinfoblock, flen, fbuf, filename[300]:STRING
   DEF a, b, c, lptr:PTR TO LONG, e, fdofs
   DEF head:PTR TO moduleheader
   DEF strtable
   DEF info:PTR TO modinfo
   DEF object:PTR TO modobject
   DEF libi:PTR TO modlibi
   DEF lfunc:PTR TO modlfunc
   DEF param:PTR TO modarg
   DEF member:PTR TO modmember
   DEF lparam:PTR TO lfuncarg

   NEW args

   /* read agrs */
   rdargs := ReadArgs('MODULE/A,LIBPROTOS/S', args, NIL)

   IF rdargs = NIL THEN Raise("ARGS")

   StrCopy(str, args.module)

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

   PrintF('CeeModule 1.2 by LS 2008-10 -> \q\s\q\n\n', filename)

   ExamineFH(fh, fib)
   flen := fib.size
   fbuf := NewR(flen+4)
   Read(fh, fbuf, flen)
   head := fbuf

   IF head.identification <> "ECXM" THEN Raise("FORM")

   IF head.modversion < 1 THEN Raise("TOLD")  -> 1.11

   IF head.modsize <> flen THEN Raise("SIZE") -> 1.11

   IF head.osversion THEN PrintF('/* OSVERSION: \d */ \n\n', head.osversion)

   info := head + head.strtabinfo
   strtable := info + SIZEOF modinfo

   /* FD */
   info := head.libiinfo
   IF info <> NIL AND (args.libprotos = FALSE)
      info := info + head
      libi := info + SIZEOF modinfo
      c := info.count + 1
      WHILE c--
         PrintF('##base _\s\n##bias 30\n', libi.basename + strtable)
         fdofs := -30
         lfunc := libi + SIZEOF modlibi
         FOR a := 0 TO libi.nroflfuncs-1
            IF lfunc.baseofs < (fdofs + IF lfunc.type = 1 THEN 2 ELSE 0)
               PrintF('##private\n')
               WHILE lfunc.baseofs < (fdofs + IF lfunc.type = 1 THEN 2 ELSE 0)
                  PrintF('Private_\d'+'()\n', -fdofs)
                  fdofs := fdofs - 6
               ENDWHILE
               PrintF('##public\n')
            ENDIF
            PrintF('\s(', lfunc.name + strtable)
            lparam := lfunc + SIZEOF modlfunc
            FOR b := 0 TO lfunc.nrofargs-1
               PrintF('\s', lparam[b].name + strtable)
               IF b < (lfunc.nrofargs-1) THEN PrintF(',')
            ENDFOR
            PrintF(')')

            IF lfunc.type = 0
               PrintF('(')
               FOR b := 0 TO lfunc.nrofargs-1
                  PrintF('\c\d', IF lparam[b].rtype = 0 THEN "d" ELSE "a", lparam[b].rnum)
                  IF b < (lfunc.nrofargs-1) THEN PrintF(',')
               ENDFOR
               PrintF(')')
            ELSE
               IF lfunc.basertype = 0
                  IF lfunc.basernum = 12
                     PrintF('(sysv,r12base)')
                  ELSEIF lfunc.basernum = 3
                     PrintF('(base,sysv)')
                  ELSEIF lfunc.basernum
                     PrintF('(sysv,base)')
                  ELSE
                     PrintF('(sysv)')
                  ENDIF
               ELSE
                  PrintF('(sysv,base)')
               ENDIF
            ENDIF
            PrintF('\n')
            lfunc := lfunc + lfunc.totalsize
            fdofs := fdofs - 6
         ENDFOR
         PrintF('##end\n\n')
         libi := libi + libi.totalsize
      ENDWHILE
   ENDIF

   /* PROTO */
   info := head.libiinfo
   IF info <> NIL AND (args.libprotos <> FALSE )
      info := info + head
      libi := info + SIZEOF modinfo
      c := info.count + 1
      WHILE c--
         lfunc := libi + SIZEOF modlibi
         FOR a := 0 TO libi.nroflfuncs-1
            PrintF('\l\s \s(',
               ListItem(['LONG','DOUBLE','VECTOR','LONG LONG'], lfunc.return),
               lfunc.name + strtable)
            lparam := lfunc + SIZEOF modlfunc
            FOR b := 0 TO lfunc.nrofargs-1
               PrintF('\s \s',
                  ListItem(['LONG','LONG','DOUBLE','VECTOR','LONG LONG', 'LONG LONG'], lparam[b].rtype),
                  lparam[b].name + strtable)
               IF b < (lfunc.nrofargs-1) THEN PrintF(',')
            ENDFOR
            PrintF(');\n')
            lfunc := lfunc + lfunc.totalsize
         ENDFOR
         libi := libi + libi.totalsize
      ENDWHILE
   ENDIF


   /* STRUCT */
   info := head.objectinfo
   IF info
      info := info + head
      object := info + SIZEOF modinfo
      c := info.count + 1
      WHILE c--
         PrintF('struct \s {\n', object.name + strtable)
         member := object + SIZEOF modobject
         a := object.nrofmembers + 1
         WHILE a--
            PrintF('   \s;\n',
                   ctypestr50(member.size,
                              member.esize,
                              member.numes,
                              member.object + strtable,
                              member.flags,
                              member.name + strtable,
                              str))
            member++
         ENDWHILE
         PrintF('}; /* SIZEOF = \d */\n\n', object.sizeof)
         object := object + object.totalsize
      ENDWHILE
   ENDIF

   /* #DEFINE */
   info := head.constinfo
   IF info
      info := info + head
      lptr := info + SIZEOF modinfo
      b := info.count + 1
      WHILE b--
         PrintF('#define \s \s\n', lptr[]++ + strtable, valstr(lptr[]++, str))
      ENDWHILE
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
      StringF(str, '0x\h', val)
   ELSEIF Abs(val) > 255
      StringF(str, '0x\h', val)
   ELSE
      StringF(str, '\d', val)
   ENDIF
ENDPROC str

PROC ctypestr50(size, esize, numes, object, flags, name, str)
   SELECT size
   CASE 0
      SELECT esize
      CASE 1   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'BYTE \s'+'[\d]' ELSE 'CHAR \s'+'[\d]',
                  name, numes)
      CASE 2   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'WORD \s'+'[\d]' ELSE 'UWORD \s'+'[\d]',
                  name, numes)
      CASE 4   ; StringF(str,
                  IF flags AND MEMBF_FLOAT THEN 'FLOAT \s'+'[\d]' ELSE
                  (IF flags AND MEMBF_SIGNED THEN 'LONG \s'+'[\d]' ELSE 'ULONG \s'+'[\d]'),
                  name, numes)
      CASE 8   ; StringF(str,
                  IF flags AND MEMBF_FLOAT THEN 'DOUBLE \s'+'[\d]' ELSE 'QUAD \s'+'[\d]',
                  name, numes)
      CASE 16  ; StringF(str, 'vector \s'+'[\d]', name, numes)
      CASE 255 ;  IF numes = 1
                     StringF(str, 'struct \s \s', object, name)
                  ELSE
                     StringF(str, 'struct \s \s'+'[\d]', object, name, numes)
                  ENDIF
      ENDSELECT
   CASE 1 ; StringF(str, IF flags AND MEMBF_SIGNED THEN 'BYTE \s' ELSE 'CHAR \s', name)
   CASE 2 ; StringF(str, IF flags AND MEMBF_SIGNED THEN 'WORD \s' ELSE 'UWORD \s', name)
   CASE 4
      SELECT esize
      CASE 0   ; StringF(str, IF flags AND MEMBF_SIGNED THEN 'LONG \s' ELSE
                              (IF flags AND MEMBF_FLOAT THEN 'FLOAT \s' ELSE 'ULONG \s'),
                                 name)
      CASE 1   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'BYTE *\s' ELSE 'CHAR *\s', name)
      CASE 2   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'WORD *\s' ELSE 'UWORD *\s', name)
      CASE 4   ; StringF(str, IF flags AND MEMBF_SIGNED THEN 'LONG *\s' ELSE
                              (IF flags AND MEMBF_FLOAT THEN 'FLOAT *\s' ELSE 'ULONG *\s'), name)
      CASE 8   ; StringF(str,
                  IF flags AND MEMBF_FLOAT THEN 'DOUBLE *\s' ELSE 'QUAD *\s', name)
      CASE 16  ; StringF(str, 'vector *\s', name)
      CASE 255 ; StringF(str, 'struct \s *\s', object, name)
      ENDSELECT
   CASE 8
      StringF(str, IF flags AND MEMBF_FLOAT THEN 'DOUBLE \s' ELSE 'QUAD \s', name)
   CASE 16
      StringF(str, 'vector \s', name)
      SELECT esize
      CASE 4   ; StringF(str,
                  IF flags AND MEMBF_FLOAT THEN 'vector float \s' ELSE 'vector long \s', name)
      CASE 2   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'vector short \s' ELSE 'vector unsigned short \s',
                     name)
      CASE 1   ; StringF(str,
                  IF flags AND MEMBF_SIGNED THEN 'vector signed char \s' ELSE 'vector char',
                     name)
      ENDSELECT
   ENDSELECT
ENDPROC str



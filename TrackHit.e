-> TrackHit.e by Leif Salomonsson is Copyright (c) 2003-2008
-> Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT

MODULE 'dos/doshunks'

-> 2006: now expects no "NIL" long before "LINE" long.. like ELF format..
-> 2007: HAH, now it actually works good :)
-> 2007: adapted to small change in line format.

RAISE "FILE" IF FileLength()=-1,
      "OPEN" IF Open()=NIL,
      "ARGS" IF ReadArgs()=NIL

OBJECT symb
   next
   name
   offset
ENDOBJECT

DEF symblist:PTR TO symb

PROC main() HANDLE
   DEF fbuf, flen
   DEF fh=NIL, rdargs=NIL, args:PTR TO LONG
   DEF name, line, source, offset, symbol:PTR TO symb
   DEF read

   NEW args[5]

   /* read agrs */
   rdargs := ReadArgs('FILE/A,'+
                      'OFFSET/A',
                      args, NIL)

   name := args[0]
   offset, read := Val(args[1])

   IF read = 0 THEN Raise("VAL")

   flen := FileLength(name)
   fbuf := NewR(flen+4)
   fh := Open(name, OLDFILE)
   Read(fh, fbuf, flen)
   Close(fh) ; fh := NIL


   WriteF('TrackHit 1.1\nCopyright (c) 2003-2007 Leif Salomonsson\n -> \q\s\q, $\h\n', name, offset)

   line, source, symbol := trackhit(fbuf, offset)

   IF line = FALSE THEN line := -1
   IF source = NIL THEN source := '???'
   IF symbol = NIL THEN symbol := [0,'???',-1]:symb

   WriteF('SOURCE \q\s\q\nLINE \d, below symbol \q\s\q ($\h)\n', source, line, symbol.name, symbol.offset)

EXCEPT DO

   IF rdargs THEN FreeArgs(rdargs)

   IF fh THEN Close(fh)


   SELECT exception
   CASE "ARGS" ; WriteF('Error: args\n')
   CASE "FILE" ; WriteF('Error: file\n')
   CASE "OPEN" ; WriteF('Error: open\n')
   CASE "FORM" ; WriteF('Error: format -> \s\n', exceptioninfo)
   CASE "MEM"  ; WriteF('Error: mem\n')
   CASE "VAL"  ; WriteF('Error: offset argument invalid\n')
   ENDSELECT
ENDPROC

OBJECT line
   line
   offset
ENDOBJECT

PROC trackhit(ptr:PTR TO LONG, ofs)
   DEF a, b, c:PTR TO LONG, d, linenum=FALSE, source=FALSE, symbol=FALSE
   DEF symbnode:PTR TO symb, rofs, t, end
   DEF line:PTR TO line, tline:PTR TO line

   IF ptr[]++ <> HUNK_HEADER THEN Throw("FORM", 'not executable file')
   WHILE ptr[] DO ptr := ptr + 4 + Mul(4,ptr[])
   ptr++
   b := ptr[]++ -> nr of hunks
   ptr := ptr + 8 -> skip fisrt/last hunk nums
   ptr := ptr + Mul(4,b) -> skip sizes

   IF ptr[]++ <> HUNK_CODE THEN Throw("FORM", 'code hunk not found')
   ptr := ptr + Mul(4,ptr[]) + 4 -> skip code

   IF ptr[] = HUNK_ABSRELOC32
      ptr++
      WHILE (a := ptr[]++) DO ptr := ptr + Mul(a,4) + 4
   ENDIF

   IF ptr[] = HUNK_END THEN ptr++

   IF ptr[] <> HUNK_DEBUG THEN Throw("FORM", 'debug hunk not found (Compile with DEBUG)')

   WHILE (ptr[] = HUNK_DEBUG)
      ptr++
      a := ptr[]++ -> size in longs
      end := ptr + Mul(a,4)
      IF ptr[] = "LDBG"
            tline := NIL
            d := NIL
            WHILE ptr[]++ = "LDBG"
               t := ptr[]++  -> numlines
               b := ptr + 4  -> save ptr to name
               line := ptr + Shl(ptr[]++, 2) + 4-> jump over name
               IF (ofs >= line.offset) AND (ofs <= line[t-1].offset)
                  d := b
                  WHILE t
                     t--
                     IF line.offset <= ofs THEN tline := line
                     line++
                  ENDWHILE
               ELSE
                  ptr := line + Mul(t, SIZEOF line)
               ENDIF
               EXIT d
            ENDWHILE
            IF d
               source := d
               linenum := tline.line
            ENDIF

      ENDIF
      ptr := end
   ENDWHILE

   IF ptr[]++ = HUNK_SYMBOL
      WHILE (a := ptr[]++) -> name len in longs
         NEW symbnode
         symbnode.name := ptr -> name
         ptr := ptr + (a*4) -> skip name
         symbnode.offset := ptr[]++ -> offset
         addSymb(symbnode)
      ENDWHILE
      ->printSymbols()
      symbol := findSymb(ofs)

   ENDIF

ENDPROC linenum, source, symbol

PROC addSymb(s:PTR TO symb)
   DEF symb:PTR TO symb, prev=NIL:PTR TO symb


   symb := symblist
   WHILE symb
      IF s.offset < symb.offset
         IF prev
            prev.next := s
            s.next := symb
         ELSE
            symblist := s
            s.next := symb
         ENDIF
         RETURN
      ELSE
         prev := symb
         symb := symb.next
      ENDIF
   ENDWHILE

   IF prev THEN prev.next := s ELSE symblist := s

ENDPROC

PROC findSymb(ofs)
   DEF symb:PTR TO symb, prev=NIL:PTR TO symb
   symb := symblist
   WHILE symb
      EXIT symb.offset > ofs
      prev := symb
      symb := symb.next
   ENDWHILE

ENDPROC prev

PROC printSymbols()
   DEF s:PTR TO symb
   s := symblist

   WHILE s
      WriteF('\s = \d\n', s.name, s.offset)
      s := s.next
   ENDWHILE

ENDPROC NIL

CHAR '$VER: TrackHit July 2003-07 by LS',0

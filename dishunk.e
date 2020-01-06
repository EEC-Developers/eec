-> DisHunk by Leif Salomonsson is Copyright (c) 2003-2007.
-> Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT

OPT PREPROCESS

MODULE 'dos/doshunks'
MODULE '*ppcdisasm'

DEF fbuf, flen, disasm
DEF hunksizes:PTR TO LONG

-> EC uses ASR for Shr()!
PROC shr(x,v)
#ifndef ECX_VERSION
   MOVE.L x, D0
   MOVE.L v, D1
   LSR.L D1, D0
ENDPROC D0
#endif
#ifdef ECX_VERSION
ENDPROC x SHR v
#endif

PROC main() HANDLE
   DEF fh=NIL, ptr:PTR TO LONG, str[24]:STRING, rdargs=NIL, args[2]:ARRAY OF LONG

   /* read agrs */
   rdargs := ReadArgs('FILE/A,'+
                      'DIS/S',
                      args, NIL)

   IF rdargs = NIL THEN Raise("ARGS")
   flen := FileLength(args[])
   IF flen = 0 THEN Raise("FLEN")
   fbuf := NewR(flen+4)
   fh := Open(args[], OLDFILE)
   IF fh = NIL THEN Raise("OPEN")
   IF Read(fh, fbuf, flen) <> flen THEN Raise("READ")
   Close(fh) ; fh := NIL
   ptr := fbuf

   disasm := args[1]



   WHILE (ptr := dohunk(ptr))
   ENDWHILE


EXCEPT DO

   IF rdargs THEN FreeArgs(rdargs)
   IF fh THEN Close(fh)

   SELECT exception
   CASE "ARGS"  ; WriteF('Error: wrong arguments!\n')
   CASE "OPEN"  ; WriteF('Error: could not open file!\n')
   CASE "FLEN"  ; WriteF('Error: file is empty!\n')
   CASE "MEM"   ; WriteF('Error: out of memory!\n')
   CASE NIL
   DEFAULT      ; WriteF('Error: unknown!\n')
   ENDSELECT

ENDPROC exception

PROC c0(ptr)
   DEF c
   c := Char(ptr)
   IF (c > 31) AND (c < 128) THEN RETURN c
ENDPROC "."

PROC c1(ptr)
   DEF c
   c := Char(ptr+1)
   IF (c > 31) AND (c < 128) THEN RETURN c
ENDPROC "."

PROC c2(ptr)
   DEF c
   c := Char(ptr+2)
   IF (c > 31) AND (c < 128) THEN RETURN c
ENDPROC "."

PROC c3(ptr)
   DEF c
   c := Char(ptr+3)
   IF (c > 31) AND (c < 128) THEN RETURN c
ENDPROC "."


PROC dohunk(ptr:PTR TO LONG) HANDLE
   DEF long, pc:PTR TO LONG, clen, str[100]:STRING, r=NIL, a, b, c=NIL, hunknum
   long := ptr[]++
   SELECT long
   CASE HUNK_UNIT ; PrintF('HUNK_UNIT \s\n', ptr+4) ; r := ptr + 4 + Mul(ptr[],4)
   CASE HUNK_NAME ; PrintF('HUNK_NAME \s\n', ptr+4) ; r := ptr + 4 + Mul(ptr[],4)
   CASE HUNK_CODE ; PrintF('HUNK_CODE\n   size: \d bytes\n', Mul(4,ptr[]))
      clen := ptr[]++
      pc := ptr
      r := ptr + Mul(4,clen)
      IF disasm
         WHILE clen
            dodis(pc-ptr, pc[], str)
           PrintF('L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a (\d\n',
            pc-ptr, str, pc[], c0(pc), c1(pc), c2(pc), c3(pc), pc-ptr)
            clen--
            pc++
            IF CtrlC() THEN Raise(0)
         ENDWHILE
      ENDIF
   CASE $3F7 ; PrintF('HUNK_PPC_CODE\n   size: \d bytes\n', Mul(4,ptr[]))
      clen := ptr[]++
      pc := ptr
      r := ptr + Mul(4,clen)
      IF disasm
         WHILE clen
            dodis(pc-ptr, pc[], str)
           PrintF('L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a\n',
            pc-ptr, str, pc[], c0(pc), c1(pc), c2(pc), c3(pc))
            clen--
            pc++
            IF CtrlC() THEN Raise(0)
         ENDWHILE
      ENDIF
   CASE HUNK_DATA ; PrintF('HUNK_DATA\n   size: \d bytes\n', Mul(4,ptr[]))
      clen := ptr[]++
      pc := ptr
      r := ptr + Mul(4,clen)
      IF disasm
         WHILE clen
            PrintF('L\z\h[7]: $\h[8] \a\c\c\c\c\a\n',
            pc-ptr, pc[], c0(pc), c1(pc), c2(pc), c3(pc))
            clen--
            pc++
            IF CtrlC() THEN Raise(0)
         ENDWHILE
      ENDIF
   CASE HUNK_BSS ; PrintF('HUNK_BSS\n   size: \d bytes\n', Mul(4,ptr[]++))
      r := ptr
   CASE HUNK_ABSRELOC32 ; PrintF('HUNK_(ABS)RELOC32\n')
      WHILE (a := ptr[]++)
         PrintF('   \d entrys to hunk \d\n', a, hunknum :=  ptr[]++)
         ->ptr := ptr + Mul(4,a)
         FOR b := 0 TO a-1
				IF ptr[] < 0 
					PrintF('   broken offset $\h\n',  ptr[])
				ELSEIF ptr[] > Mul(hunksizes[hunknum], 4)	
					PrintF('   broken offset $\h\n',  ptr[])
				ELSE	
					IF disasm THEN PrintF('      $\h\n', ptr[])
            ENDIF
            ptr++
         ENDFOR
      ENDWHILE
      r := ptr
   CASE HUNK_RELOC16
   CASE HUNK_RELRELOC16
   CASE HUNK_RELOC8
   CASE HUNK_RELRELOC8
   CASE HUNK_EXT ; PrintF('HUNK_EXT\n')
      WHILE ptr[]
         a := Char(ptr)   -> type
         b := Char(ptr+3) -> len of name in longs
         ptr++
         c := NIL
         SELECT 256 OF a
         CASE EXT_SYMB      ; PrintF('   SYMB     ')
         CASE EXT_DEF       ; PrintF('   DEF      ') ; c++
         CASE EXT_ABS       ; PrintF('   ABS      ')
         CASE EXT_REF32     ; PrintF('   REF32    ')
         CASE EXT_REF16     ; PrintF('   REF16    ')
         CASE EXT_DEXT32    ; PrintF('   DEXT32   ')
         CASE EXT_DEXT16    ; PrintF('   DEXT16   ')
         CASE EXT_RELREF32  ; PrintF('   RELREF32 ')
         DEFAULT            ; PrintF('   ???      ')
         ENDSELECT
         PrintF('\l\s[20] ', ptr)
         ptr := ptr + (b*4) -> skip name
         a := ptr[]++ -> nr of offsets
         IF c = NIL THEN PrintF('(\d references)', a)
         PrintF('\n')
         WHILE a
            IF disasm THEN PrintF('                                 $\h\n', ptr[])
            ptr++
            a--
         ENDWHILE
         IF CtrlC() THEN Raise(0)
     ENDWHILE
     ptr++ -> skip NIL-long
     r := ptr
   CASE HUNK_SYMBOL ; PrintF('HUNK_SYMBOL\n')
      WHILE (a := ptr[]++) -> name len in longs
         PrintF('   \s : ', ptr)
         ptr := ptr + (a*4)
         PrintF('   $\h\n', ptr[]++)
      ENDWHILE
      r := ptr
   CASE HUNK_DEBUG ; PrintF('HUNK_DEBUG\n')
      a := ptr[]++
      PrintF('   size: \d bytes\n', Mul(a,4))
      ->ptr := ptr + 2
      WHILE a
         IF disasm THEN PrintF('   $\z\h[8] \a\c\c\c\c\a\n', ptr[], c0(ptr),c1(ptr),c2(ptr),c3(ptr))
         ptr++
         a--
         IF CtrlC() THEN Raise(0)
      ENDWHILE
      r := ptr
   CASE HUNK_END ; PrintF('HUNK_END\n') ; r := ptr
   CASE HUNK_HEADER ; PrintF('HUNK_HEADER\n')
      a := 0
      WHILE ptr[]
         PrintF('   hunk \d \a\s\a\n', a, ptr+4)
         ptr := ptr + 4 + Mul(4,ptr[])
      ENDWHILE
      ptr++

      b := ptr[]++ -> nr of hunks
      PrintF('   nr of hunks: \d (\d-\d)\n', b, ptr[0], ptr[1])
      ptr := ptr + 8 -> skip fisrt/last hunk nums

		hunksizes :=  ptr  -> save for reloc debug

      FOR a := 0 TO b-1
         PrintF('   hunk \d size: \d bytes\n', a, Mul(4,ptr[]++))
      ENDFOR

      r := ptr
   CASE HUNK_OVERLAY
   CASE HUNK_BREAK
   CASE HUNK_DREL32
   CASE HUNK_DREL16
   CASE HUNK_DREL8
   CASE HUNK_LIB
   CASE HUNK_INDEX
   CASE HUNK_RELOC32SHORT
   CASE HUNK_RELRELOC32
   CASE HUNK_ABSRELOC16
   CASE NIL ; PrintF('done.\n') ; RETURN NIL
   DEFAULT
      PrintF('HUNK_unknown! ID: $\h\n', long) ; r := NIL
   ENDSELECT
EXCEPT
   RETURN NIL
ENDPROC r


-> ECX/common.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT PREPROCESS

-> common.e new file created Sept 2007. Moved some common stuff out of ecx52.e source.

->#define DBG_HASH

MODULE '*compiler'
MODULE '*binary'
MODULE 'dos/dos'

EXPORT DEF itmz_labelhashtable:PTR TO LONG -> main inits
EXPORT DEF g_symbolssize -> main reads

-> we "import" these from main (error reporting)
EXPORT DEF g_currentproc:PTR TO proc, g_linenum, g_sourcename
EXPORT DEF g_nowarn
EXPORT DEF g_warnlist, g_optmodule

EXPORT PROC newString(size) IS String(size)


EXPORT PROC endString(s:PTR TO LONG) IS DisposeLink(s)


PROC hashNewHLN(str, i)
   DEF hln:PTR TO hln, len, nstr
   #ifdef DBG_HASH
   DEBUGF('hashNewHLN(\a\s\a, \d)\n', str, i)
   #endif
   NEW hln
   len := StrLen(str)
   nstr := String(len)
   IF nstr = NIL THEN Raise("MEM")
   hln.name := nstr
   StrCopy(nstr, str)
   hln.hnext := itmz_labelhashtable[i]
   itmz_labelhashtable[i] := hln
   g_symbolssize := g_symbolssize + len  -> v40. to get idea of max strtabsize/symbolhunksize
ENDPROC hln

PROC hashFindHLN(str, i)
   DEF hln:PTR TO hln
   #ifdef DBG_HASH
   DEBUGF('hashFindHLN(\a\s\a, \d)\n ', str, i)
   #endif
   hln := itmz_labelhashtable[i]
   WHILE hln
      IF StrCmp(hln.name, str) THEN RETURN hln
      hln := hln.hnext
   ENDWHILE
ENDPROC NIL

EXPORT PROC getLabelHLN(str)
   DEF hln:PTR TO hln, i
   #ifdef DBG_HASH
   DEBUGF('getLabelHLN(\a\s\a) : ', str)
   #endif
   i := hashfunc(str)
   hln := hashFindHLN(str, i)
   IF hln = NIL THEN hln := hashNewHLN(str, i)
   #ifdef DBG_HASH
   DEBUGF('I:\d HLN:$\h = [$\h,$\h]\n', i, hln,hln.ident,hln.ident2)
   #endif
ENDPROC hln

EXPORT PROC hashfunc(s) -> v50
   DEF i, v=0, a=1, c
   #ifdef DBG_HASH
   DEBUGF('hashfunc(\a\s\a) : ', s)
   #endif
   WHILE (c := s[]++)
      v := v * a + c
      a++
   ENDWHILE
   i := Abs(Mod(v, LHASHSIZE))
   #ifdef DBG_HASH
   DEBUGF('Abs(Mod(\h, \d)) = \d\n', v, LHASHSIZE, i)
   #endif
   i := Min(i, LHASHSIZE-1) -> work around mos emu bug
ENDPROC i


EXPORT PROC reportErr(err, extra=NIL)
   DEF str[100]:STRING

   WriteF('ERROR: \s \s\s\s\n',
           err,
           IF extra THEN '"' ELSE '',
           IF extra THEN extra ELSE '',
           IF extra THEN '"' ELSE '')

   IF g_currentproc THEN WriteF('Procedure: \s\n', g_currentproc.name)

   IF g_linenum <> 0 THEN WriteF('Line \d: \s\n', Abs(g_linenum), getSourceLine(str))

   Raise("ERR")
ENDPROC

EXPORT PROC reportIErr(err, extra=NIL)   -> v48
   DEF str[100]:STRING

   WriteF('INTERNAL ERROR: \s \s\s\s\n',
           err,
           IF extra THEN '"' ELSE '',
           IF extra THEN extra ELSE '',
           IF extra THEN '"' ELSE '')

   IF g_currentproc THEN WriteF('Procedure: \s\n', g_currentproc.name)

   IF g_linenum <> 0 THEN WriteF('Line \d: \s\n', Abs(g_linenum), getSourceLine(str))

   Raise("ERR")
ENDPROC

-> 1.10.0: added "more"  arg
EXPORT PROC addWarning(err,more=NIL)
   DEF warn:PTR TO warn, errlen, str, tstr[256]:STRING
   IF g_nowarn THEN RETURN
   StringF(tstr, '\s \c\s\c', err,
      IF more THEN "\q" ELSE " ",
      IF more THEN more ELSE '',
      IF more THEN "\q" ELSE " ")

   warn := g_warnlist
   WHILE warn
      IF StrCmp(warn.message, tstr)
         warn.count := warn.count + 1
         RETURN
      ENDIF
      warn := warn.next
   ENDWHILE

   str := String(EstrLen(tstr))
   IF str = NIL THEN Raise("MEM")
   StrCopy(str, tstr)
   g_warnlist := NEW [g_warnlist, str, 1]:warn

ENDPROC

PROC getSourceLine(strbuf) HANDLE
   DEF fname[200]:STRING
   DEF fh=NIL, fbuf, mem=NIL
   DEF fib:fileinfoblock
   DEF linenr=1

   IF g_linenum = -1 THEN RETURN StrCopy(strbuf, '???')

   StrCopy(fname, g_sourcename)
   fh := Open(fname, OLDFILE)
   IF fh = NIL THEN Throw("OPEN", fname)
   IF ExamineFH(fh, fib) = NIL THEN Throw("EXAM", fname)
   mem := NewR(4+fib.size)
   fbuf := mem
   IF Read(fh, fbuf, fib.size) <> fib.size THEN Throw("READ", fname)
   /* find line */
   WHILE (fbuf[]<>NIL) AND (linenr <> g_linenum)
      IF fbuf[] = 10 THEN linenr++
      fbuf++
   ENDWHILE
   IF linenr = g_linenum  -> line found
      strCopyNL(strbuf, fbuf)
   ELSE
      StrCopy(strbuf, '???')
   ENDIF

EXCEPT DO
   IF fh THEN Close(fh)
   IF mem THEN Dispose(mem)
   ReThrow()
ENDPROC strbuf

PROC strCopyNL(estr, str)
   DEF pos=NIL
   pos := InStr(str, '\n')
   IF pos = -1
      StrCopy(estr, str)
      RETURN NIL, EstrLen(estr)
   ELSE
      StrCopy(estr, str, pos)
      RETURN str+pos+1, pos
   ENDIF
ENDPROC

-> returns ptr to member.
-> "mname": hashed name.
EXPORT PROC findMember(o:PTR TO object, mname)
   DEF m:REG PTR TO member, a:REG
   a := o.nrofmembers
   WHILE a
      a--
      m := o.membertable[a]
      IF m
         IF m.name = mname THEN RETURN m
      ENDIF
   ENDWHILE
ENDPROC NIL

-> used from expressions. returns ptr to proc, mid.
-> "mname": hashed name
EXPORT PROC findMethod(o:PTR TO object, mname)
   DEF m:REG PTR TO proc, a:REG
   a := o.nrofmethods
   WHILE a
      a--
      m := o.methodtable[a]
      IF m
         IF m.name = mname THEN RETURN m, a
      ENDIF
   ENDWHILE
ENDPROC NIL, NIL

/*
-> 1.6.0
EXPORT PROC addMethDbgSym(oname, methname, methoffset)
   DEF hln:PTR TO hln, str[300]:STRING, label:PTR TO codelab

   IF g_optmodule THEN RETURN

   StrCopy(str, oname)
   StrAdd(str, '.')
   StrAdd(str, methname)

   NEW label
   label.offset := methoffset
   label.ltype := LTYPE_LAB
   label.identID := IDENT_LABEL

   hln := getLabelHLN(str)
   label.name := hln.name
   hln.ident := label

ENDPROC
*/

-> 1.6.0
EXPORT OBJECT strtabcontext
   memory:PTR TO CHAR
   offset:LONG
   count:LONG
ENDOBJECT

-> 1.10.0 moved here
EXPORT PROC getTableStrOfs(stc:PTR TO strtabcontext, str, merge=FALSE)
   DEF len, ofs, ptr

   #ifdef DBG_BMOD
   DEBUGF('getTableStrOfs($\h, \a\s\a, \d) : ', stc, str, merge)
   #endif

   IF merge
      ptr := stc.memory
      WHILE ptr[]
         IF StrCmp(str, ptr)
            ofs := ptr - (stc.memory)
            #ifdef DBG_BMOD
            DEBUGF('\d\n', ofs)
            #endif
            RETURN ofs
         ENDIF
         ptr := ptr + StrLen(ptr) + 1
      ENDWHILE
   ENDIF

   stc.count := stc.count + 1

   ptr := stc.memory
   len := StrLen(str) + 1
   ofs := stc.offset
   CopyMem(str, ptr + ofs, len)
   stc.offset := ofs + len

   #ifdef DBG_BMOD
   DEBUGF('\d\n', ofs)
   #endif

ENDPROC ofs

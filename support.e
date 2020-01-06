
-> ECX/support.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */


OPT MODULE
OPT PREPROCESS

MODULE 'utility' -> v55
MODULE 'utility/date' -> v55
MODULE 'dos/datetime' -> 1.8.0
MODULE 'dos/dos'

DEF utilitybase

#ifndef ECX_VERSION

MODULE 'mathieeedoubtrans'

DEF mathieeedoubtransbase

EXPORT PROC initSupport()
   mathieeedoubtransbase := OpenLibrary('mathieeedoubtrans.library', 37)
   IF mathieeedoubtransbase = NIL THEN Throw("LIB", 'mathieeedoubtrans.library')

   utilitybase := OpenLibrary('utility.library', 39)
   IF utilitybase = NIL THEN Throw("LIB", 'utility.library')
ENDPROC

EXPORT PROC endSupport()
   IF utilitybase THEN CloseLibrary(utilitybase) -> v55
   IF mathieeedoubtransbase THEN CloseLibrary(mathieeedoubtransbase)
ENDPROC

EXPORT PROC singToDoub(sing)
   DEF x, y
   x, y := IeeeDPFieee(sing)
ENDPROC x, y

#endif /* ECX_VERSION */


#ifdef ECX_VERSION

#ifdef __AMIGAOS4__
   DEF utilityiface
#endif

EXPORT PROC initSupport()
   utilitybase := OpenLibrary('utility.library', 39)
   IF utilitybase = NIL THEN Throw("LIB", 'utility.library')
   #ifdef __AMIGAOS4__
      utilityiface := GetInterface(utilitybase, 'main', 1, NIL)
      IF utilityiface = NIL THEN Throw("LIB", 'utility.library->interface')
   #endif
ENDPROC

EXPORT PROC endSupport()
   #ifdef __AMIGAOS4__
      IF utilityiface THEN DropInterface(utilityiface)
   #endif
   IF utilitybase THEN CloseLibrary(utilitybase) -> v55
ENDPROC

EXPORT PROC singToDoub(sing)
   DEF d:DOUBLE
   d := sing
ENDPROC Long({d}), Long({d}+4)   -> fixed 1.8.2

#endif /* ECX_VERSION */

EXPORT PROC writeNewExeFile(buf,size, name) HANDLE
   DEF fh=NIL

   fh := Open(name, NEWFILE)
   IF fh = NIL THEN Raise("OPEN")

   IF Write(fh, buf, size) <> size THEN Raise("WRIT")

EXCEPT DO

   IF fh THEN Close(fh)

   IF exception
      RETURN NIL
   ELSE
      -> os4's JXFS does not set e bit by default
      SetProtection(name, NIL)
   ENDIF
ENDPROC size

EXPORT PROC writeNewFile(buf,size, name) HANDLE
   DEF fh=NIL

   fh := Open(name, NEWFILE)
   IF fh = NIL THEN Raise("OPEN")

   IF Write(fh, buf, size) <> size THEN Raise("WRIT")

EXCEPT DO

   IF fh THEN Close(fh)

   IF exception THEN RETURN NIL

ENDPROC size


-> 2.0
EXPORT PROC makeVersiondateStr(estr)
   DEF dt:datetime, s
   DEF buf[100]:ARRAY

   DateStamp(dt)
   dt.flags := NIL
   dt.strday := NIL
   dt.strdate := buf+2
   dt.strtime := NIL
   buf[0] := "'"
   buf[1] := "("

   dt.format := FORMAT_CDN
   IF DateToStr(dt)
      s := buf
      s := s + InStr(s, '-')
      s[]++ := "."
      s := s + InStr(s, '-')
      s[]++ := "."
      s[3] := s[1]
      s[2] := s[0]
      s[0] := "2"
      s[1] := "0"
      s[4] := ")"
      s[5] := "'"
      s[6] := NIL
      StrCopy(estr, buf)
   ELSE
      SetStr(estr, 0)
   ENDIF

ENDPROC

EXPORT PROC makeDateStr(estr)

   DEF dt:datetime, s
   DEF buf[100]:ARRAY

   DateStamp(dt)
   dt.flags := NIL
   dt.strday := NIL
   dt.strdate := buf+1
   dt.strtime := NIL
   buf[0] := "'"

   dt.format := FORMAT_DOS
   IF DateToStr(dt)
      s := buf
      s := s + InStr(s, '-') + 1
      s := s + InStr(s, '-') + 1
      s[3] := s[1]
      s[2] := s[0]
      s[0] := "2"
      s[1] := "0"
      s[4] := "'"
      s[5] := NIL
      StrCopy(estr, buf)
   ELSE
      SetStr(estr, 0)
   ENDIF

ENDPROC

EXPORT PROC makeTimeStr(estr)
   DEF secs
   DEF cd:clockdata

   secs := getSeconds()

   Amiga2Date(secs, cd)

   StringF(estr, '\a\z\d[2]:\z\d[2]:\z\d[2]\a', cd.hour, cd.min, cd.sec)

ENDPROC

PROC getSeconds()
   DEF seconds,now:datestamp
   DateStamp(now) -> datestamp datum
   seconds := Mul(now.days, 86400) + Mul(now.minute, 60) + Div(now.tick, TICKS_PER_SECOND)
ENDPROC seconds

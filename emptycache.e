-> EmptyCache by Leif Salomonsson [ecx tele2 se] is Copyrigh (c) 2003-2007.
-> Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT

-> February 2009: called InStr() with a zero rempat, FIXED.

MODULE '*binary'
MODULE 'dos/dos'
MODULE 'exec/lists'
MODULE 'exec/nodes'


PROC main() HANDLE
   DEF modulecache=NIL:PTR TO modulecache
   DEF cmh:PTR TO moduleheader, succ, rempat=NIL, rem

   IF StrLen(arg) THEN rempat := arg

   Forbid()
   modulecache := FindSemaphore('EcxCache')
   Permit()
   IF modulecache = NIL THEN Raise("NOCA")
   IF modulecache.ident <> "MC" THEN Raise("MCC")
   IF AttemptSemaphore(modulecache) = NIL THEN Raise("ASEM")
   cmh := modulecache.modlist.head

   PrintF('EmptyCache 1.4 by LS 2003-2009\n')

   WHILE cmh.succ
      ->WriteF('cmh=$\h, cmh.mnamelen=\d, cmh.mname=$\h, cmh.mname="\s"\n',
      ->cmh, cmh.mnamelen, cmh.mname, cmh.mname)
      succ := cmh.succ
      rem := FALSE
      IF rempat = 0
         rem := TRUE
      ELSEIF InStr(cmh.mname, rempat) <> -1
         rem := TRUE
      ELSE
         rem := FALSE
      ENDIF
      IF rem
         Remove(cmh)
         FreeMem(cmh.mname, cmh.mnamelen+1)
         FreeMem(cmh, cmh.modsize)
      ENDIF
      cmh := succ
   ENDWHILE

   PrintF('done.\n')

   ReleaseSemaphore(modulecache)

EXCEPT DO

   SELECT exception
   CASE "NOCA" ; PrintF('no cache available!\n')
   CASE "ASEM" ; PrintF('semaphore is blocked!\n')
   CASE "MCC"  ; PrintF('modulecache is corrupt!\n') -> 1.8.0
   ENDSELECT

ENDPROC



-> ViewCache by Leif Salomonsson is Copyright (c) 2003-2008
-> Released under ECX TOOLS license, see ECXTOOLSLICENSE.TXT

-> v50, now exits if semaphore could not be obtained.

-> v53: checks modulecache.ident

-> 1.6´(v55) : skips dummymods

-> 1.7 (v56) : added ignoreblock option

MODULE '*binary'
MODULE 'dos/dos'
MODULE 'exec/lists'
MODULE 'exec/nodes'

PROC main() HANDLE
   DEF modulecache:PTR TO modulecache
   DEF cmh:PTR TO moduleheader
   DEF totsize=0, num=0, ib=FALSE

   Forbid()
   modulecache := FindSemaphore('EcxCache')
   Permit()
   IF modulecache = NIL THEN Raise("NOCA")
   IF modulecache.ident <> "MC" THEN Raise("MCC")
   IF StrCmp(arg, 'ignoreblock')
      ib := TRUE
   ENDIF
   IF ib = FALSE
      IF AttemptSemaphore(modulecache) = FALSE THEN Raise("BLCK")
   ENDIF
   cmh := modulecache.modlist.head

   PrintF('ViewCache 1.7 by LS 2003-2009\n\n')

   WHILE cmh.succ
      IF cmh.dummymod = FALSE
         PrintF('\s (\r\d[7]) \s\n',
         IF cmh.codeinfo THEN IF cmh.cpu = 0 THEN '68K' ELSE 'PPC' ELSE '   ',
         cmh.modsize, cmh.mname)

         totsize := totsize + cmh.modsize
         num++
      ENDIF

      cmh := cmh.succ
   ENDWHILE

   PrintF('\n\d bytes used by \d modules.\n', totsize, num)

   IF ib = FALSE THEN ReleaseSemaphore(modulecache)

EXCEPT

   SELECT exception
   CASE "BLCK" ; PrintF('sempahore is blocked!\n')
   CASE "NOCA" ; PrintF('no cache available!\n')
   CASE "MCC"  ; PrintF('modulecache is corrupt!\n')
   ENDSELECT

ENDPROC




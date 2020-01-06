
->OPT POWERPC

-> private tool for dumping a cached module to disk (by LS) PUBLIC DOMAIN.

MODULE '*binary'
MODULE 'dos/dos'
MODULE 'exec/lists'
MODULE 'exec/nodes'


PROC main() HANDLE
   DEF modulecache=NIL:PTR TO modulecache
   DEF cmh:PTR TO moduleheader, t=NIL, fh
   DEF m:PTR TO moduleheader

   modulecache := FindSemaphore('EcxCache')
   IF modulecache = NIL THEN Raise("NOCA")
   cmh := modulecache.modlist.head

   PrintF('dumping \s\n', arg)

   WHILE cmh.succ
      IF StrCmp(cmh.mname, arg)
         fh := Open('t:t.m', NEWFILE)
         Write(fh, cmh, cmh.modsize)
         Close(fh)
         Raise(0)
      ENDIF
      cmh := cmh.succ
   ENDWHILE

   PrintF('not found!\n')

EXCEPT DO

   SELECT exception
   CASE "NOCA" ; PrintF('no cache available!\n')
   CASE "MEM"  ; PrintF('Out of memory\n')
   ENDSELECT
ENDPROC


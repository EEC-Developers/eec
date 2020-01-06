-> ECX/ecmodtrans.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT PREPROCESS

-> ecmodtrans.e, created May 2008, extracted from ecxmain.e

->#define DBG_TRANS

MODULE '*binary'
MODULE '*common'
MODULE '*compiler'

EXPORT DEF g_modulecache:PTR TO modulecache

-> 1.5.4: for uncommenting define bodies in ec-modules.
PROC uncommentBody(source:PTR TO CHAR)
   DEF incomment=FALSE:REG
   DEF dest:REG PTR TO CHAR
   DEF c:REG, pos

   dest := source

   WHILE c := source[]

      IF incomment
         SELECT c
         CASE "/"
            IF source[1] = "*"
               source := source + 2
               incomment++
            ELSE
               source++
            ENDIF
         CASE "*"
            IF source[1] = "/"
               source := source + 2
               incomment--
            ELSE
               source++
            ENDIF
         DEFAULT
            source++
         ENDSELECT
      ELSEIF c = "/"
         IF source[1] = "*"
            source := source + 2
            incomment := 1
         ELSE
            dest[]++ := source[]++
         ENDIF
      ELSEIF c = "-"
         IF source[1] = ">"
            source := source + StrLen(source)
         ELSE
            dest[]++ := source[]++
         ENDIF
      ELSE
         dest[]++ := source[]++
      ENDIF

   ENDWHILE

   dest[]++ := NIL -> terminate

ENDPROC

/**************************************************************
***************************************************************
******************** EC MODULE TRANSLATOR *********************
***************************************************************
**************************************************************/

ENUM JOB_DONE,JOB_CONST,JOB_OBJ,JOB_CODE,JOB_PROCS,
     JOB_SYS,JOB_LIB,JOB_RELOC,JOB_GLOBS,JOB_MODINFO,JOB_DEBUG,JOB_MACROS

CONST MODVERS = 11 -> now suports CreativE mods, but not new ifuncs,ivars !
CONST MAXIFNUM = 130
CONST SKIPMARK=$FFFF8000
CONST BSRLVALUE=$61FF ->, CLEANUPVALUE=$4FEF, DOUBLENOPVALUE=$4E714E71

EXPORT PROC ecmod2ecxmod(ecmod,flen) HANDLE
   DEF mh=NIL:PTR TO moduleheader, info:PTR TO modinfo
   DEF jc, o:PTR TO INT, end, wptr:PTR TO LONG
   DEF strsize=4
   DEF len, thisvers=0, osvers=NIL
   DEF l, val, off, priv, c
   DEF f, num
   DEF r, c2
   DEF code=NIL
   DEF darg, narg, coff, name
   DEF temp:PTR TO LONG, count, lptr=NIL:PTR TO LONG
   DEF oinfo=NIL:PTR TO modinfo, cinfo=NIL:PTR TO modinfo, constptr=NIL:PTR TO LONG
   DEF objectsize=0, constsize=0 -> June 2009, now supports mixed const/object data
   DEF objectptr=NIL:PTR TO modobject, procptr=NIL:PTR TO modproc
   DEF macroptr=NIL:PTR TO modmacro, codeptr=NIL:PTR TO LONG, globptr=NIL:PTR TO LONG
   DEF submodptr=NIL:PTR TO modmodule
   DEF a
   DEF libi=NIL:PTR TO modlibi, lfunc=NIL:PTR TO modlfunc
   DEF param=NIL:PTR TO modarg, t, member=NIL:PTR TO modmember, modref=NIL:PTR TO modref
   DEF allocsize=12 -> safety extra 12 bytes
   DEF ifuncrefs=NIL:PTR TO LONG, xrefgrefs=NIL:PTR TO LONG
   DEF lparam:PTR TO lfuncarg
   DEF lo:PTR TO LONG -> needed to remove ^ stuff 1.5.4
   DEF stc:strtabcontext

   o := ecmod + 4

   allocsize := SIZEOF moduleheader + Mul(SIZEOF modinfo,15)

   end := ecmod+flen

   /* first compute sizes */

   WHILE o < end
      jc := o[]++
      #ifdef DBG_TRANS
      DEBUGF('ECMODSUPP 1: \s\n', ListItem(['DONE', 'CONST', 'OBJ',
                                            'CODE', 'PROCS', 'SYS',
                                            'LIB', 'RELOC', 'GLOBS',
                                            'MODINFO', 'DEBUG', 'MACROS'], jc))
      #endif
      SELECT 25 OF jc
      CASE JOB_CONST
         IF thisvers>=6 THEN o:=o+4
         WHILE len := o[]++
           strsize := strsize + len
           o := o + 4
           o:=o+len
           constsize := constsize + 8
           allocsize := allocsize + 8
         ENDWHILE
      CASE JOB_OBJ
         allocsize := allocsize + SIZEOF modobject
         objectsize := objectsize + SIZEOF modobject
         IF thisvers>=6 THEN o:=o+4
         priv:=0
         l:=o[]++;
         strsize := strsize + l
         o:=o+4+l
         WHILE l:=o[]++
            val:=o[]++
            off:=o[]++
            IF l>0
               allocsize := allocsize + SIZEOF modmember
               objectsize := objectsize + SIZEOF modmember
               strsize := strsize + l
               o:=o+l
               priv:=0
            ELSE
               allocsize := allocsize + SIZEOF modmember
               objectsize := objectsize + SIZEOF modmember
               priv++
            ENDIF
            IF thisvers>=6
               IF (c:=o[]++)>=0
               ELSE
                  l:=o[]++
                  strsize := strsize + l -> member object name
                  o:=o+l
               ENDIF
            ENDIF
         ENDWHILE
         val:=o[]++
         IF thisvers>=7
            IF o[]++
               o:=o+4
               l:=o[]++
               o:=o+l+4
               WHILE (c:=o[]++)<>-1
                  o++; l:=o[]++
                  o:=o+l
                  l:=o[]++
                  l:=o[]++; o:=l*4+o
               ENDWHILE
               WHILE o[]++<>-1 DO o:=o+4
            ENDIF
         ENDIF
      CASE JOB_SYS
         #ifdef DBG_TRANS
         DEBUGF('SYS\n')
         #endif
         o:=o+4
         f:=FALSE
         IF c:=o[]++
            f:=TRUE
            osvers := c
         ENDIF
         o:=o+4
         IF c:=o[]++
         ENDIF
         IF c:=o[]++
         ENDIF
         o:=o+2
         thisvers:=o[]++
         IF thisvers > MODVERS
            ->WriteF('version=\d\n', thisvers)
            Raise("FORM")
         ENDIF
         o:=o+4
         IF thisvers > 10 THEN o := o + 4 -> 1.6.0 (creative 2.12 fix)
      CASE JOB_MACROS
         WHILE len:=o[]++
            strsize := strsize + len + 1
            #ifdef DBG_TRANS
            DEBUGF('#define \s',o)
            #endif
            o:=o+len
            #ifdef DBG_TRANS
            DEBUGF(' /\d\n',o[])
            #endif
            ->strsize := strsize + Mul(o[],2)
            allocsize := allocsize + Mul(o[]++,4)  ->PrintF('/\d\n',o[]++)
            o++ -> flags,dummy
            len := o[]++
            allocsize := allocsize + SIZEOF modmacro + len + 3 AND $FFFC
            #ifdef DBG_TRANS
            DEBUGF('\s\n',o)
            #endif
            o := len + o
         ENDWHILE
      CASE JOB_LIB
         allocsize := allocsize + SIZEOF modlibi
         c:=o
         WHILE c[]++ DO NOP
         strsize := strsize + StrLen(c) + 1
         WHILE c[]++ DO NOP
         off:=-30
         WHILE (c[]<>$FF) AND (c<end)
            IF c[] = 16
               INC c
            ELSE
               c2 := c
               WHILE c[]++ > " " DO NOP ; c--
               strsize := strsize + c - c2 + 1
               allocsize := allocsize + SIZEOF modlfunc
               r := c[]++
               IF r <> 16
                  WHILE r < 16
                     r := c[]++
                     allocsize := allocsize + SIZEOF lfuncarg
                  ENDWHILE
                  c--
               ENDIF
            ENDIF
         ENDWHILE
         o := end
      CASE JOB_CODE
         lo := o
         l := lo[]++  ->l:=^o++
         code := lo
         allocsize := allocsize + Mul(l, 4)
         o:=(l*4)+lo
      CASE JOB_PROCS
         WHILE (l:=o[]++)>0
            c:=o
            strsize := strsize + StrLen(c) + 1
            #ifdef DBG_TRANS
            DEBUGF('PROC \s\n', c)
            #endif
            o:=o+l+4
            IF o[]=1
              o++
              allocsize := allocsize + SIZEOF modproc
              narg:=o[]++
              allocsize := allocsize + Mul(SIZEOF modarg, narg)
              o++
              c2:=o[]++
              darg:=o
              o:=c2*4+o
              c:=o[]++
              o:=o+c
            ELSEIF o[]=2 -> label v47 fixed
              o++
              allocsize := allocsize + 12
            ENDIF
         ENDWHILE
      CASE JOB_RELOC
         IF code = NIL THEN Raise("NIL")
         -> bit 31 in offset indicates ifunc reloc (jsr abs.l [lea args*4(a7), a7]) -> weird.
         lo := o
         len := lo[]++ ; o := lo ->len := ^o++
         WHILE len
            len--
            r := Long(o)
            #ifdef DBG_TRANS
            DEBUGF('reloc: $\h: $\h <$\h> $\h\n', r,
            Long(r AND $FFFFFF + code - 4),
            Long(r AND $FFFFFF + code),
            Long(r AND $FFFFFF + code + 4))
            #endif
            IF r AND $80000000 -> ifunc ?
               r := r AND $FFFFFF
               PutInt(code+r-2, BSRLVALUE) -> patch jsr.l TO bsr.l
               c := Char(code+r+3) - 10 -> ifunc num (WriteF=10)
               IF c > MAXIFNUM
                  ->WriteF('ifnum=\d\n', c)
                  Raise("FORM") -> no cec ifuncs
               ENDIF
               #ifdef DBG_TRANS
               DEBUGF('->ifunc: \d\n', c)
               #endif
               ifuncrefs := NEW [ifuncrefs, c, r]:LONG
               allocsize := allocsize + 12 -> [num,numrefs=1,offset]:LONG  !!
               ->strsize := strsize + StrLen(c) + 1
            ELSE
               allocsize := allocsize + 4
            ENDIF
            o := o + 4
         ENDWHILE
      CASE JOB_GLOBS
         IF o[]=SKIPMARK THEN o:=o+6
         WHILE (len:=o[]++)>=0
            IF len -> xrefglob ?
               allocsize := allocsize + SIZEOF modref
               strsize := strsize + len
               name := o
               lo:=o+len
               WHILE (coff := lo[]++)
                  IF thisvers>=10 THEN lo := lo + 2 -> we only support LONG !
                  allocsize := allocsize + 12
                  xrefgrefs := NEW [xrefgrefs, name, coff]:LONG
               ENDWHILE
               o := lo
            ELSE
               lo := o
               allocsize := allocsize + 4 -> private drel glob
               WHILE (coff := lo[]++)
                  IF thisvers>=10 THEN lo := lo + 2 -> we only support LONG !
                  allocsize := allocsize + 4 ->adddrelg(Mul(nrofglobals,4))
               ENDWHILE
               o := lo
            ENDIF
         ENDWHILE
      CASE JOB_DEBUG
         lo := o
         WHILE lo[]++=$3F1
            len:=lo[]++
            lo++
            c:=lo[]++
            lo:=len*4+lo-8
         ENDWHILE
         o := lo
      CASE JOB_MODINFO
         o:=o+4
         ->g_esize := g_esize + SIZEOF vardbghunk
         WHILE len:=o[]++
            allocsize := allocsize + SIZEOF modmodule
            strsize := strsize + len
      #ifdef DBG_TRANS
      DEBUGF('modinfo: \s\n', o)
      #endif
            o:=o+len
            WHILE c:=o[]++ -> type
               allocsize := allocsize + SIZEOF modref
               len:=o[]++  -> id len
               strsize := strsize + len
               c2:=o       -> id
      #ifdef DBG_TRANS
      DEBUGF('-> \s\n', o)
      #endif
               o:=o+len
               IF c=2 -> proc/lab
                  f:=o[]++ -> narg (-1=lab)
                  c:=o[]++ -> num accesses
                  allocsize := allocsize + (4*c)
                  ->modrefs := NEW [modrefs, c2, c, NIL]:xref
                  o:=c*4+o -> ARRAY OF LONG
               ELSE -> class
                  c:=o[]++ -> num accesses
                  allocsize := allocsize + (4*c)
                  o:=c*6+o -> ARRAY OF LONG+INT
               ENDIF
            ENDWHILE
         ENDWHILE
      CASE JOB_DONE
         o := end
      DEFAULT
         ->WriteF('unknown JOB \d\n', jc)
         Raise("FORM")
      ENDSELECT
   ENDWHILE

   #ifdef DBG_TRANS
   DEBUGF('translation pass 1 done..\n')
   #endif

   strsize := strsize + SIZEOF modinfo + 3 AND $FFFFFC


   /* create new module */

   IF g_modulecache

      mh := ALLOCMEM(t := (allocsize + strsize), MEMF_CLEAR OR MEMF_PUBLIC)
      IF mh = NIL THEN Raise("MEM")

   ELSE

      mh := NEWMEMR(t := (allocsize + strsize))

   ENDIF

   mh.osversion := osvers
   mh.identification := "ECXM"

   mh.modsize := t

   mh.headsize := SIZEOF moduleheader
   mh.modversion := MODULEVERSION

   mh.strtabinfo := allocsize
   info := mh + mh.strtabinfo
   info.misc := NIL
   info.rsrvd := NIL

   stc.memory := info + SIZEOF modinfo
   stc.offset := NIL
   stc.count := 0

   getTableStrOfs(stc, '---')


   wptr := mh + SIZEOF moduleheader

   /* make ifuncinfo now */


   info := wptr
   info.misc := 0
   wptr := info + SIZEOF modinfo
   WHILE ifuncrefs
      #ifdef DBG_TRANS
      DEBUGF('trans: ifuncref \d \d\n', ifuncrefs[1], ifuncrefs[2])
      #endif
      info.count := info.count + 1
      wptr[]++ := ifuncrefs[1]  -> num
      wptr[]++ := 1 -> one offset
      wptr[]++ := ifuncrefs[2]
      t := ifuncrefs[]
      END ifuncrefs[3]
      ifuncrefs := t
   ENDWHILE
   IF info.count
      ->info.size := wptr - info
      mh.ifuncinfo := info - mh
   ENDIF

   info := wptr
   wptr := info + SIZEOF modinfo
   WHILE xrefgrefs
      #ifdef DBG_TRANS
      DEBUGF('trans: xrefgref \s \d\n', xrefgrefs[1], xrefgrefs[2])
      #endif
      info.count := info.count + 1
      wptr[]++ := getTableStrOfs(stc, xrefgrefs[1], TRUE)  -> name
      wptr[]++ := 1 -> one offset
      wptr[]++ := xrefgrefs[2]
      t := xrefgrefs[]
      END xrefgrefs[3]
      xrefgrefs := t
   ENDWHILE
   IF info.count
      ->info.size := wptr - info
      mh.xrefginfo := info - mh
   ENDIF



   o := ecmod + 4

   WHILE o < end
      jc := o[]++
      #ifdef DBG_TRANS
            DEBUGF('ECMODSUPP 2: \s\n', ListItem(['DONE', 'CONST', 'OBJ',
                                            'CODE', 'PROCS', 'SYS',
                                            'LIB', 'RELOC', 'GLOBS',
                                            'MODINFO', 'DEBUG', 'MACROS'], jc))
      #endif
      SELECT 25 OF jc
      CASE JOB_CONST
         IF cinfo = NIL
            cinfo := wptr
            constptr := cinfo + SIZEOF modinfo
            wptr := constptr + constsize
            mh.constinfo := cinfo - mh
         ENDIF
         IF thisvers>=6 THEN o:=o+4
         WHILE len := o[]++
            lo := o
            val:=lo[]++
            constptr[]++ := getTableStrOfs(stc, lo)
            constptr[]++ := val
            o:=lo+len
         ENDWHILE
         cinfo.count := Div(constsize, 8)
         ->DEBUGF('\d constants created.\n', cinfo.count)
      CASE JOB_OBJ
         IF oinfo = NIL
            oinfo := wptr
            objectptr := oinfo + SIZEOF modinfo
            mh.objectinfo := oinfo - mh
            wptr := objectptr + objectsize
         ENDIF
         member := objectptr + SIZEOF modobject
         IF thisvers>=6 THEN o:=o+4
         priv:=0
         l:=o[]++;
         objectptr.name := getTableStrOfs(stc, o+4)
         #ifdef DBG_TRANS
         DEBUGF('OBJECT \s\n', o+4)
         #endif
         o:=o+4+l
         WHILE l:=o[]++
            member.flags := NIL -> 1.8.0 fix
            val:=o[]++
            off:=o[]++
            IF l>0
               member.offset := off
               member.name := getTableStrOfs(stc, o)
               objectptr.nrofmembers := objectptr.nrofmembers + 1
               o:=o+l
               priv:=0
            ELSE
               member.offset := off
               member.name := NIL
               objectptr.nrofmembers := objectptr.nrofmembers + 1
               priv++
            ENDIF
            IF thisvers>=6
               IF (c:=o[]++)>=0
                  IF c=0
                     member.size := val -> CHAR/INT/LONG
                     member.esize := 0
                     member.numes := 0
                     IF val > 1 THEN member.flags := MEMBF_SIGNED -> v49
                  ELSE
                     IF val               -> CPTR/IPTR/LPTR
                        member.size := 4
                        member.esize := c
                        member.numes := 0
                        IF c > 1 THEN member.flags := MEMBF_SIGNED -> v49
                     ELSE
                        member.size := 0  -> CARRAY/IARRAY/LARRAY
                        member.esize := c
                        member.numes := Int(o+IF o[] THEN 4 ELSE 2)-off/c
                        IF c > 1 THEN member.flags := MEMBF_SIGNED -> v49
                     ENDIF
                 ENDIF
               ELSE
                  member.esize := 255
                  l:=o[]++
                  member.size := IF val THEN 4 ELSE 0 -> OPTR/OARRAY
                  IF val = NIL THEN member.numes := 1
                  member.object := getTableStrOfs(stc, o)
                  o:=o+l
                  member.numes := 0
               ENDIF
            ELSE
               member.size := val -> CHAR/INT/LONG
               IF val > 1 THEN member.flags := MEMBF_SIGNED -> v49
               member.esize := 0
               member.numes := 0
            ENDIF

            ->IF priv = 0 THEN member++
            member++

         ENDWHILE
         val:=o[]++
         IF thisvers>=7
            IF o[]++
               addWarning('module translator skipping methods for object')
               o:=o+4
               l:=o[]++
               o:=o+l+4
               WHILE (c:=o[]++)<>-1
                  o++; l:=o[]++
                  ->PrintF('         \s(',o)
                  o:=o+l
                  IF l:=o[]++ THEN FOR off:=1 TO l DO NOP ->PrintF(IF off=l THEN '\c' ELSE '\c,',off+96)
                  l:=o[]++; o:=l*4+o
               ENDWHILE
               WHILE o[]++<>-1 DO o:=o+4
            ENDIF
         ENDIF
         objectptr.sizeof := val
         objectptr.alignment := 2 -> v1.5
         objectptr.nrofmethods := 0
         objectptr.totalsize := member - objectptr
         objectptr := member
         oinfo.count := oinfo.count + 1
      CASE JOB_SYS
         o:=o+4
         o++
         o:=o+4
         o++
         o++
         o:=o+2
         o++
         o:=o+4
         IF thisvers > 10 THEN o := o + 4 -> 1.6.0 (creative 2.12 fix)
      CASE JOB_MACROS
         info := wptr
         macroptr := info + SIZEOF modinfo
         WHILE len:=o[]++
            macroptr.name := getTableStrOfs(stc, o)
            #ifdef DBG_TRANS
            DEBUGF('#define \s',o)
            #endif
            o:=o+len
            c := o[]++ -> nrofparams
            macroptr.nrofargs := c
            IF c
               macroptr.type := 1
               #ifdef DBG_TRANS
               DEBUGF('(')
               #endif
            ENDIF
            lptr := macroptr + SIZEOF modmacro
            FOR a := 0 TO c-1
            lptr[]++ := NIL -> we do not read arg names anyway
            ENDFOR
            #ifdef DBG_TRANS
            DEBUGF(IF c THEN ') ' ELSE ' ')
            #endif
            o++ -> flags,dummy
            len := o[]++ -> bodysize
            macroptr.bodysize := len + 3 AND $FFC
            #ifdef DBG_TRANS
            DEBUGF('\s\n', o)
            #endif
            CopyMemQuick(o, lptr, macroptr.bodysize)
            uncommentBody(lptr) -> 1.5.4
            lptr := lptr + macroptr.bodysize
            macroptr.totalsize := lptr - macroptr
            macroptr := lptr
            o:=len+ o
            info.count := info.count + 1
         ENDWHILE
         IF info.count
            wptr := lptr
            mh.macroinfo := info - mh
         ENDIF
      CASE JOB_LIB
         info := wptr
         libi := info + SIZEOF modinfo
         lfunc := libi + SIZEOF modlibi
         c:=o
         WHILE c[]++ DO NOP
         libi.basename := getTableStrOfs(stc, c)
         WHILE c[]++ DO NOP
         off:=-30
         WHILE (c[]<>$FF) AND (c<end)
            IF c[] = 16
               INC c
            ELSE
               c2 := c
               WHILE c[]++ > " " DO NOP ; c--
               r := c[]
               c[]++ := NIL
               IF StrCmp(c2, 'Dum') = FALSE
                  lfunc.name := getTableStrOfs(stc, c2)
                  lparam := lfunc + SIZEOF modlfunc
                  c2 := 0
                  IF r <> 16
                     WHILE r < 16
                        lparam.rtype := IF r > 7 THEN 1 ELSE 0
                        lparam.rnum := IF r > 7 THEN r - 8 ELSE r
                        lparam++
                        c2++
                        r := c[]++
                     ENDWHILE
                     c--
                  ENDIF
                  lfunc.baseofs := off
                  lfunc.nrofargs := c2
                  lfunc.totalsize := lparam - lfunc
                  libi.nroflfuncs := libi.nroflfuncs + 1
                  lfunc := lparam
               ENDIF
            ENDIF
            off:=off-6
         ENDWHILE
         libi.totalsize := lfunc - libi
         libi := lfunc
         info.count := 1
         mh.libiinfo := info - mh
         o := end
         wptr := lfunc
      CASE JOB_CODE
         info := wptr
         wptr := info + SIZEOF modinfo
         l:=Mul(Long(o),4) ; o := o + 4
         CopyMem(o, wptr, l)
         wptr := wptr + l
         o:=l+o
         info.count := Div(l,4)
         IF info.count
            mh.codeinfo := info - mh
         ENDIF
      CASE JOB_RELOC
         info := wptr
         wptr := info + SIZEOF modinfo
         c:=Long(o) ; o := o + 4
         WHILE c
            c--
            r := Long(o)
            #ifdef DBG_TRANS
            DEBUGF('reloc $\h\n', r)
            #endif
            IF (r AND $80000000) = FALSE
               wptr[]++ := r
               info.count := info.count + 1
            ENDIF
            o := o + 4
         ENDWHILE
         IF info.count
            mh.relocinfo := info - mh
         ENDIF
      CASE JOB_PROCS
         info := wptr
         wptr := info + SIZEOF modinfo
         procptr := wptr
         off := 0 -> counts nr of procedures
         WHILE (l:=o[]++)>0
            c:=o
            o:=o+l+4
            IF o[]=1
               o++
               off++  -> counter
               procptr.name := getTableStrOfs(stc, c)
               procptr.offset := Long(o-6)
               narg:=o[]++
               procptr.nrofargs := narg
               #ifdef DBG_TRANS
               DEBUGF('proc \s'+'(/\d) @ \d\n', c, narg, procptr.offset)
               #endif
               o++
               c2:=o[]++
               procptr.nrofdefaults := c2
               darg:=o
               param := procptr + SIZEOF modproc
               IF c2
                  FOR c := 0 TO narg-1
                     IF c >= (narg-c2)
                        param.defval := Long(darg+Mul(c-(narg-c2),4))
                     ENDIF
                     param++
                  ENDFOR
               ELSE
                  param := param + Mul(SIZEOF modarg,narg)
               ENDIF
               o:=c2*4+o
               c:=o[]++
               o:=o+c
               procptr.totalsize := param - procptr
               procptr := procptr + procptr.totalsize
            ELSEIF o[] = 2 -> label
               o++
               off++
               procptr.name := getTableStrOfs(stc, c)
               procptr.offset := Long(o-6)
               param := procptr + 12
               procptr.totalsize := param - procptr
               #ifdef DBG_TRANS
               DEBUGF('label \s\n', c)
               #endif
               procptr := procptr + procptr.totalsize
            ENDIF
         ENDWHILE
         info.count := off
         IF info.count
            mh.procinfo := info - mh
            wptr := param
         ENDIF
      CASE JOB_DEBUG
         lo := o
         WHILE lo[]++=$3F1
            len:=lo[]++
            lo:=lo+4
            c:=lo[]++
            lo:=len*4+lo-8
         ENDWHILE
         o := lo
      CASE JOB_MODINFO
         num := 0
         info := wptr
         wptr := info + SIZEOF modinfo
         submodptr := wptr
         o:=o+4
         WHILE len:=o[]++
            #ifdef DBG_TRANS
            DEBUGF('modname: \s\n', o)
            #endif
            submodptr.name := getTableStrOfs(stc, o)
            modref := submodptr + SIZEOF modmodule
            num++
            o:=o+len
            WHILE c:=o[]++
               len:=o[]++
               c2:=o
               o:=o+len
               IF c=2 -> proc/lab
                  submodptr.numsymbs := submodptr.numsymbs + 1
                  modref.name := getTableStrOfs(stc, c2)
                  f:=o[]++
                  c:=o[]++
                  modref.numrefs := c
                  lptr := modref + SIZEOF modref
                  WHILE c
                     c--
                     lptr[]++ := Shl(REFADR,24) OR Long(o) -> put xref offset
                     o := o + 4
                  ENDWHILE
                  modref := lptr
               ELSE
                  c:=o[]++
                  o:=c*6+o
               ENDIF
            ENDWHILE
            submodptr.totalsize := modref - submodptr
            submodptr := modref
         ENDWHILE
         info.count := num
         IF info.count
            mh.moduleinfo := info - mh
            wptr := modref
         ENDIF
      CASE JOB_DONE
         o := end
      CASE JOB_GLOBS
         info := wptr
         wptr := info + SIZEOF modinfo
         IF o[]=SKIPMARK THEN o:=o+6
         WHILE (len:=o[]++)>=0
           IF len -> xrefglob ?
               lo:=o+len
               WHILE (coff := lo[]++)
                  IF thisvers>=10 THEN lo := lo + 2
               ENDWHILE
               o := lo
           ELSE
              #ifdef DBG_TRANS
              DEBUGF('drelglob:\n')
              #endif
              info.count := info.count + 1
              lptr := wptr++ -> numdrels
              lo := o
              WHILE (coff := lo[]++)
                 IF thisvers>=10 THEN lo := lo + 2 -> we only support LONG !
                 wptr[]++ := coff
                 #ifdef DBG_TRANS
                 DEBUGF('   \d\n', coff)
                 #endif
                 lptr[] := lptr[] + 1
              ENDWHILE
              o := lo
           ENDIF
         ENDWHILE
         IF info.count
            mh.globinfo := info - mh
         ENDIF
      DEFAULT          ; Raise("FORM")
      ENDSELECT
   ENDWHILE

   info := mh.strtabinfo + mh
   info.count := stc.count

   IF mh.codeinfo THEN mh.cpu := CPU_M68 -> 2.0 fix

   #ifdef DBG_TRANS
   DEBUGF('allocated modsize=\d\n', mh.modsize)
   #endif

   t := (info + stc.offset + SIZEOF modinfo) - mh
   IF t > mh.modsize
      DEBUGF('!!module buffer overflow!! \d\n', t-mh.modsize)
      RETURN NIL, "TRNS"
   ENDIF

   IF wptr > (mh.strtabinfo + mh) -> 1.8.0
      DEBUGF('!!wptr overflowed into strtable!!')
      RETURN NIL, "TRNS"
   ENDIF

   #ifdef DBG_TRANS
      DEBUGF('actual modsize=\d\n', mh.modsize)
   #endif


EXCEPT DO

   IF exception
      #ifdef DBG_TRANS
      DEBUGF('translation error: $\h\n', exception)
      #endif
      IF mh
         IF g_modulecache THEN FREEMEM(mh, mh.modsize) ELSE DISPOSE(mh)
      ENDIF
   ENDIF


ENDPROC IF exception THEN NIL ELSE mh, exception


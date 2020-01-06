-> DisELF by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2003-2007.
-> Released under ECX TOOLS LICENSE, see ECXTOOLSLICENSE.TXT

OPT PREPROCESS

MODULE '*ecxelf'
MODULE '*ppcdisasm'

-> Jan, Apr, Jul 2003
-> Jan 2004: added HIT option.
->           fixed mtspr, mfspr
-> Apr 2004: added SECT option: SECT sectname.

-> todo: show symbols within code disasm.

-> May 2004: now interprets .line sections.

-> fixed fabs in ppcdisasm.e

-> April 2007: fixes to HIT information..
-> April 2007: adjstments for sligthly changed .line format (numlines added)

-> Aug 2007: - fixed some STT_XXX names that was wrong.
->           - added bunch of reloc types missing
->           - added getrelocname(id,str), added mos specific relocs
->           - replaced cx() functions with char() func and fixed a bug, thx Stefan H.

-> June 2008: some improvements on page header display.

-> June 2011: adding symbol disassembly. also needs ecx now.

-> bug: only shows first smbol in a section that
-> matches a hit..

RAISE "FILE" IF FileLength()=-1,
      "OPEN" IF Open()=NIL,
      "ARGS" IF ReadArgs()=NIL,
      "^C"   IF CtrlC()=TRUE

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


OBJECT args
	file
	dis
	hit
	sect
	label
ENDOBJECT	

STATIC phdrtypes = ['NULL',
                 'LOAD',
                 'DYNAMIC',
                 'INTERP',
                 'NOTE',
                 'SHLIB',
                 'PHDR',
                 '???']
	
STATIC shdrtypes =   ['',
                   'PROGBITS',
                   'SYMTAB',
                   'STRTAB',
                   'RELA',
                   'HASH',
                   'DYNAMIC',
                   '7',
                   'NOBITS',
                   'REL',
                   '10',
                   'DYNSYM',
                   '???']



STATIC symbolbinds = ['LOCAL',
                   'GLOBAL',
                   'WEAK',
                   '???']

STATIC symboltypes = ['NOTYPE',
                   'OBJECT',
                   'FUNC',
                   'SECTION',
                   'FILE',
                   'COMMON',
                   'TLS',
                   '???']

DEF args:args
DEF fbuf, flen

PROC main() HANDLE
   DEF fh=NIL, rdargs=NIL
   DEF ehdr:PTR TO ehdr, phdr:PTR TO phdr, phnum
   DEF shdr:PTR TO shdr, shnum
   DEF shstrtable, a, b, t
   DEF sym:PTR TO sym
   DEF shdr2:PTR TO shdr, strtable
   DEF rel:PTR TO rel, rela:PTR TO rela
   DEF str[100]:STRING
   DEF c, hit, sectname[12]:STRING
   

   /* read agrs */
   rdargs := ReadArgs('FILE/A,'+
                      'DIS/S,'+
                      'HIT/N,'+
                      'SECT/K,'+
                      'LABEL/K',
                      args, NIL)

   flen := FileLength(args.file)
   fbuf := NewR(flen+4)
   fh := Open(args.file, OLDFILE)
   Read(fh, fbuf, flen)
   Close(fh) ; fh := NIL

   WriteF('\s -> \q\s\q\n', {verstr}, args.file)

   hit := IF args.hit THEN Long(args.hit) ELSE -1
   IF args.sect THEN StrCopy(sectname, args.sect)

   ehdr := fbuf

   IF ehdr.ident[EI_MAG0] <> 127 THEN Throw("FORM", 'elf id')
   IF ehdr.ident[EI_MAG1] <> "E" THEN Throw("FORM", 'elf id')
   IF ehdr.ident[EI_MAG2] <> "L" THEN Throw("FORM", 'elf id')
   IF ehdr.ident[EI_MAG3] <> "F" THEN Throw("FORM", 'elf id')

   IF ehdr.ident[EI_CLASS] <> ELFCLASS32 THEN Raise("BITS")
   IF ehdr.ident[EI_DATA] <> ELFDATA2MSB THEN Raise("ENDI")

   IF hit > -1
      showhit(hit, ehdr)
      Raise(0)
   ENDIF
   
   IF args.label
		show_label(ehdr, args.label)
		Raise(0)
	ENDIF
		
   IF EstrLen(sectname) = NIL

      WriteF('EHDR\n')
      WriteF('   BITS=\d, ENDIAN=\s, IDENT[8-11]=\z$\h[8] IDENT[12-15]=\z$\h[8]\n',
      IF Long(4+ehdr.ident) = 2 THEN 64 ELSE 32, IF Long(8+ehdr.ident) = 1 THEN 'LITTLE' ELSE 'BIG',Long(8+ehdr.ident) ,Long(12+ehdr.ident))
      WriteF('   TYPE=\s, MACHINE=\d, VERSION=\d, FLAGS=$\h\n', ListItem(['0','REL','EXEC','DYN','???'], ehdr.type), ehdr.machine, ehdr.version, ehdr.flags)
      WriteF('   ENTRY=\d, PHOFF=$\h, SHOFF=$\h\n', ehdr.entry, ehdr.phoff, ehdr.shoff)
      WriteF('   EHSIZE=\d, PHENTSIZE=\d, PHNUM=\d\n', ehdr.ehsize, ehdr.phentsize, ehdr.phnum)
      WriteF('   SHENTSIZE=\d, SHNUM=\d, SHSTRNDX=\d\n', ehdr.shentsize, ehdr.shnum, ehdr.shstrndx)

      a := 0
      WHILE phdr := get_phdr(ehdr, a)

         WriteF('PHDR \d[3], \s[6], $\h[6], \d[6], \d[6], \d[6], \d[6], ', a,
         IF phdr.type <= ListLen(phdrtypes) THEN ListItem(phdrtypes,phdr.type) ELSE StringF(str, '\d', phdr.type),
         phdr.offset, phdr.vaddr, phdr.paddr, phdr.filesz, phdr.memsz)

         t := phdr.flags
         SetStr(str, 0)
         IF t AND PF_R
            StrAdd(str, 'R ')
            t := t AND Not(PF_R)
         ENDIF
         IF t AND PF_W
            StrAdd(str, 'W ')
            t := t AND Not(PF_W)
         ENDIF
         IF t AND PF_X
            StrAdd(str, 'X ')
            t := t AND Not(PF_X)
         ENDIF
         IF t
            WriteF('\h[8], \d\n', phdr.flags, phdr.align)
         ELSE
            WriteF('\s[6], \d\n', str, phdr.align)
         ENDIF

			a++
         CtrlC()

      ENDWHILE

   ENDIF

   shdr := get_shdr(ehdr, ehdr.shstrndx)
   shstrtable := ehdr + shdr.offset
  
   a := 0
   WHILE shdr := get_shdr(ehdr, a)

      IF EstrLen(sectname)
         IF shdr.name < 0 THEN JUMP _sectionskip
         IF StrCmp(shdr.name + shstrtable, sectname) = FALSE
            JUMP _sectionskip
         ENDIF
      ENDIF

      WriteF('SHDR \z\d[3] \l\s[12] \l\s[10] ', a,
      IF shdr.name > -1 THEN shstrtable + shdr.name ELSE StringF(str, '$\h', shdr.name),
      IF shdr.type <= ListLen(shdrtypes) THEN ListItem(shdrtypes,shdr.type) ELSE StringF(str, '\d', shdr.type))

      t := shdr.flags
      SetStr(str, 0)
      IF t AND SHF_WRITE
         StrAdd(str, 'WRITE ')
         t := t AND Not(SHF_WRITE)
      ENDIF
      IF t AND SHF_ALLOC
         StrAdd(str, 'ALLOC ')
         t := t AND Not(SHF_ALLOC)
      ENDIF
      IF t AND SHF_EXECINSTR
         StrAdd(str, 'EXECINSTR ')
         t := t AND Not(SHF_EXECINSTR)
      ENDIF
      IF t THEN StringF(str, '$\h ', shdr.flags)
      WriteF('\s[20]', str)

      WriteF('\r\d[2] \d[2] (\z\d[3]) \d, $\h, \d, \d\n',
      shdr.addralign, shdr.entsize, shdr.link,
      shdr.size, shdr.offset, shdr.info, shdr.addr)

      IF (shdr.type = SHT_SYMTAB) OR (shdr.type = SHT_DYNSYM)
         shdr2 := get_shdr(ehdr, shdr.link)
         strtable := ehdr + shdr2.offset
         b := 0
         WHILE sym := get_shent(ehdr, shdr, b)
            WriteF('   SYM \r\z\d[6] (\z\d[3]) L\z\r\h[8] \l\s[7] \l\s[7] \a\s\a \d, \d\n', b++,
            sym.shndx,
            sym.value,
            IF ST_BIND(sym.info) <= ListLen(symbolbinds) THEN ListItem(symbolbinds,
            ST_BIND(sym.info)) ELSE StringF(str, '\d', ST_BIND(sym.info)),
            IF ST_TYPE(sym.info) <= ListLen(symboltypes) THEN ListItem(symboltypes,
            ST_TYPE(sym.info)) ELSE StringF(str, '\d', ST_TYPE(sym.info)),
            strtable + sym.name,
            sym.size,
            sym.other)
            CtrlC()
         ENDWHILE
      ELSEIF (shdr.type = SHT_REL) AND (args.dis<>FALSE)
         shdr2 := get_shdr(ehdr, shdr.link) -> symbol shdr
         sym := ehdr + shdr2.offset -> symboltable
         shdr2 := get_shdr(ehdr, shdr2.link) -> str shdr
         strtable := ehdr + shdr2.offset
         b := 0
         WHILE rel := get_shent(ehdr, shdr, b)
            WriteF('   REL \r\z\d[6] (\z\d[3]) L\z\h[7] \a\s\a \s = L\z\h[7]\n',
            b++,
            R_SYM(rel.info),
            rel.offset,
            sym[R_SYM(rel.info)].name + strtable,
            getrelocname(R_TYPE(rel.info), str),
            peekSym(ehdr, sym[R_SYM(rel.info)], rel.offset))
            CtrlC()
         ENDWHILE
      ELSEIF (shdr.type = SHT_RELA) AND (args.dis <> FALSE)
         shdr2 := get_shdr(ehdr, shdr.link) -> symbol shdr
         sym := ehdr + shdr2.offset -> symboltable
         shdr2 := get_shdr(ehdr, shdr2.link) -> str shdr
         strtable := ehdr + shdr2.offset
         b := 0
         WHILE rela := get_shent(ehdr, shdr, b)
            WriteF('   RELA \r\z\d[6] (\z\d[3]) L\z\h[7] \a\s\a \s, $\h\n',
            b++,
            R_SYM(rela.info),
            rela.offset,
            sym[R_SYM(rela.info)].name + strtable,
            getrelocname(R_TYPE(rela.info), str),
            rela.addend)
            CtrlC()
         ENDWHILE
      ELSEIF (shdr.type = SHT_PROGBITS) AND (args.dis<>FALSE)
         b := 0
         c := ehdr + shdr.offset
         WHILE b < shdr.size
            IF shdr.flags AND SHF_EXECINSTR
               dodis(b, Long(c), str)
               WriteF('   L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a  (\d\n',
                  b, str, Long(c), char(c), char(c+1), char(c+2), char(c+3), b)
            ELSE
               WriteF('   L\z\h[7]: [\z\r\h[8]] \a\c\c\c\c\a  (\d\n',
                  b, Long(c), char(c), char(c+1), char(c+2), char(c+3), b)
            ENDIF
            b := b + 4
            c := c + 4
            CtrlC()
         ENDWHILE
      ENDIF

_sectionskip:

      a++
      CtrlC()

   ENDWHILE

   WriteF('<END>\n')

EXCEPT DO
   IF fh THEN Close(fh)

	IF rdargs THEN FreeArgs(rdargs) 

   SELECT exception
   CASE "FILE" ; WriteF('unable to open file.\n')
   CASE "OPEN" ; WriteF('unable to open file.\n')
   CASE "ARGS" ; WriteF('bad args.\n')
   CASE "FORM" ; WriteF('format is not understood: \s.\n', exceptioninfo)
   CASE "^C"   ; WriteF('user aborted.\n')
   CASE "BITS" ; WriteF('elf is not of 32bit type')
   CASE "ENDI" ; WriteF('elf is not big endian')
   ENDSELECT
ENDPROC

PROC getrelocname(id, str)
   DEF relocnames, relocnames200

relocnames :=  ['NONE',
                   'ADDR32',
                   'ADDR24',
                   'ADDR16',
                   'ADDR16_LO',
                   'ADDR16_HI',
                   'ADDR16_HA',
                   'ADDR14',
                   'ADDR14_BRTAKEN',
                   'ADDR14_BRNTAKEN',
                   'REL24',
                   'REL14',
                   'REL14_BRTAKEN',
                   'REL14_BRNTAKEN',
      'GOT16',
      'GOT16_LO',
      'GOT16_HI',
      'GOT16_HA',
      'PLTREL24',
      'COPY',
      'GLOB_DAT',
      'JMP_SLOT',
      'RELATIVE',
      'LOCAL24PC',
      'UADDR32',
      'UADDR16',
      'REL32',
      'PLT32',
      'PLTREL32',
      'PLT16_LO',
      'PLT16_HI',
      'PLT16_HA',
      'SDAREL16',
      'SECTOFF',
      'SECTOFF_LO',
      'SECTOFF_HI',
      'SECTOFF_HA']

   relocnames200 := ['MORPHOS_DREL',
                     'MORPHOS_DREL_LO',
                     'MORPHOS_DREL_HI',
                     'MORPHOS_DREL_HA']

   IF id < ListLen(relocnames)
      StrCopy(str, ListItem(relocnames,id))
   ELSEIF id < (ListLen(relocnames200)+200)
      StrCopy(str, ListItem(relocnames200, id-200))
   ELSE
      StringF(str, '\d', id)
   ENDIF

ENDPROC str

PROC peekSym(ehdr:PTR TO ehdr, sym:PTR TO sym, offset)
   DEF shdr:PTR TO shdr
   shdr := get_shdr(ehdr, sym.shndx)
ENDPROC Long(ehdr + shdr.offset + offset)

PROC char(ptr)
   DEF c
   c := Char(ptr)
   IF (c > 31) AND (c < 127) THEN RETURN c
ENDPROC "."



   CHAR 0, '$VER:'
verstr:
   CHAR 'DisELF 1.10 by LS 2003-11', 0


OBJECT line
   line
   offset
ENDOBJECT

PROC showhit(ofs:PTR TO LONG, ehdr:PTR TO ehdr)
   DEF shdr:PTR TO shdr, shnum
   DEF shstrtable, a, b, t, c:PTR TO LONG
   DEF shdr2:PTR TO shdr, strtable
   DEF str[100]:STRING
   DEF sym:PTR TO sym, offsetsym:PTR TO sym
   DEF lptr:PTR TO LONG, d, line:PTR TO line, tline:PTR TO line

   ofs := ofs AND $FFFFFC

   shdr := get_shdr(ehdr, ehdr.shstrndx)
   shstrtable := ehdr + shdr.offset

   a := 0
   WHILE shdr := get_shdr(ehdr, a)

      IF shdr.type = SHT_PROGBITS
      IF shdr.flags AND SHF_EXECINSTR
      IF shdr.size > ofs
         WriteF('\n   HIT at offset $\h in section \d:\n\n', ofs, a)

         IF ofs > 3
            c := ehdr + shdr.offset + ofs - 4
            dodis(ofs-4, c[], str)
            WriteF('   L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a\n',
                  ofs-4, str, c[], char(c), char(c+1), char(c+2), char(c+3))
         ENDIF

         c := ehdr + shdr.offset + ofs
         dodis(ofs, c[], str)
         WriteF('-->L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a\n',
               ofs, str, c[], char(c), char(c+1), char(c+2), char(c+3))

         c := ehdr + shdr.offset + ofs + 4
         dodis(ofs+4, c[], str)
         WriteF('   L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a\n',
               ofs+4, str, c[], char(c), char(c+1), char(c+2), char(c+3))
      ENDIF ; ENDIF ; ENDIF

      IF (shdr.type = SHT_SYMTAB) OR (shdr.type = SHT_DYNSYM)
         sym := ehdr + shdr.offset -> symboltable
         shdr2 := get_shdr(ehdr,  shdr.link) ->
         strtable := ehdr + shdr2.offset
         offsetsym := NIL
         FOR b := 0 TO (shdr.size / shdr.entsize) -1
            IF sym.shndx > 0
               IF sym.value <= ofs
                  IF offsetsym
                     IF sym.value > offsetsym.value THEN offsetsym := sym
                  ELSE
                     offsetsym := sym
                  ENDIF
               ENDIF
            ENDIF
            sym := sym + shdr.entsize
            CtrlC()
         ENDFOR
         IF offsetsym
            IF StrLen(offsetsym.name + strtable)
               WriteF('\n   Below symbol \a\s\a ($\h) in section \d\n',
               strtable + offsetsym.name,
               offsetsym.value,
               offsetsym.shndx)
            ENDIF
         ENDIF
      ENDIF

      IF (shdr.type = SHT_PROGBITS) AND (shdr.flags = NIL) AND (shdr.name > 0)
         IF StrCmp(shdr.name + shstrtable, '.ldbg')
            tline := NIL
            d := NIL
            lptr := shdr.offset + ehdr
            WHILE lptr[]++ = "LDBG"
            ->WriteF('"LINE" numlines=\d name=\s\n', lptr[], lptr + 8)
               t := lptr[]++  -> numlines
               b := lptr + 4  -> save ptr to name
               line := lptr + Shl(lptr[]++, 2) + 4-> jump over name
               ->WriteF('low offset=\d, highoffset=\d\n', line.offset, line[t-1].offset)
               IF (ofs >= line.offset) AND (ofs <= line[t-1].offset)
                  d := b
                  WHILE t
                     t--
                     IF line.offset <= ofs THEN tline := line
                     line++
                  ENDWHILE
                  lptr := line
               ELSE
                  lptr := line + Mul(t, SIZEOF line)
               ENDIF
               EXIT d
            ENDWHILE
            IF d
               WriteF('\n   Source: "\s"\n   Line: \d\n', d, tline.line)
            ENDIF
         ENDIF
      ENDIF

      a++

      CtrlC()

   ENDWHILE

   WriteF('<END>\n')

ENDPROC

-> 1.10
PROC get_phdr(ehdr:PTR TO ehdr, i)
	IF i >= ehdr.phnum THEN RETURN NIL
ENDPROC ehdr + ehdr.phoff + (i * ehdr.phentsize)

PROC get_shdr(ehdr:PTR TO ehdr, i)
	IF i >= ehdr.shnum THEN RETURN NIL
ENDPROC ehdr + (ehdr.shoff) + (ehdr.shentsize * i)

PROC get_shent(ehdr:PTR TO ehdr, shdr:PTR TO shdr, i)
	IF (i * shdr.entsize) >= shdr.size THEN RETURN NIL
ENDPROC ehdr + shdr.offset + (shdr.entsize * i)

PROC find_sym(ehdr:PTR TO ehdr, name)
	DEF a, shdr:PTR TO shdr, sym:PTR TO sym, shdr2:PTR TO shdr, b, strtable	
	a := 0
   WHILE shdr := get_shdr(ehdr, a)
      IF (shdr.type = SHT_SYMTAB) OR (shdr.type = SHT_DYNSYM)
         shdr2 := get_shdr(ehdr,  shdr.link) ->
         strtable := ehdr + shdr2.offset
			b := 0
         WHILE sym := get_shent(ehdr, shdr, b++)
				IF StrCmp(name, strtable + sym.name) THEN RETURN sym
         ENDWHILE
      ENDIF
      a++
   ENDWHILE
ENDPROC NIL
   
PROC show_label(ehdr, name)
	DEF sym:PTR TO sym, shdr:PTR TO shdr, ofs:PTR TO LONG, endofs, c:PTR TO LONG
	DEF str[128]:STRING
	
	sym := find_sym(ehdr, name)
	IFN sym THEN RETURN NIL
	IF ST_TYPE(sym.info) = NIL THEN RETURN NIL
	shdr := get_shdr(ehdr, sym.shndx)
	IFN shdr THEN RETURN NIL
	IF shdr.type <> SHT_PROGBITS THEN RETURN NIL
	
	WriteF('\nLabel "\s" in section \d @ offset $\h with size \d and other \d:\n\n', 
            name,
            sym.shndx,
            sym.value,
            sym.size,
            sym.other)
	
	ofs := sym.value
	endofs := ofs + sym.size
	c := ehdr + shdr.offset + ofs
	
	WHILE ofs < endofs
	
				dodis(ofs, c[], str)
		WriteF('   L\z\h[7]: \l\s[24] [\z\r\h[8]] \a\c\c\c\c\a\n',
         ofs, str, c[], char(c), char(c+1), char(c+2), char(c+3))
		ofs++
		c++
	ENDWHILE
	
	WriteF('\n')
   
ENDPROC TRUE
   
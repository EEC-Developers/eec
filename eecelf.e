/**********************************************************************
** EEC/eecelf.e
** derived from
** ECX/ecxelf.e .
** Copyright (c) Leif Salomonsson 2002-2007.
** Redistribution of this source is allowed as long as:
** 1. no copyright notice is removed or altered.
** 2. altered source must be clearly marked as such.
**********************************************************************/

OPT MODULE
OPT EXPORT
OPT PREPROCESS


ENUM EI_MAG0,
     EI_MAG1,
     EI_MAG2,
     EI_MAG3,
     EI_CLASS,
     EI_DATA,
     EI_VERSION,
     EI_PAD,
     EI_NIDENT=16

ENUM ELFCLASSNONE,
     ELFCLASS32,
     ELFCLASS64

ENUM ELFDATANONE,
     ELFDATA2LSB,
     ELFDATA2MSB

CONST ET_REL = 1,
      ET_EXEC = 2,
      ET_DYN = 3

CONST EM_68K=4, EM_PPC = 20

CONST EV_CURRENT = 1

OBJECT ehdr
   ident[EI_NIDENT]:ARRAY OF CHAR -> ["\$7F\~ELF", $01020100, NIL, NIL]:LONG
   type:INT      -> ET_REL
   machine:INT   -> EM_PPC
   version:LONG  -> 1
   entry:LONG    -> 0
   phoff:LONG    -> usually SIZEOF ehdr
   shoff:LONG    ->
   flags:LONG    -> NIL
   ehsize:INT    -> SIZEOF ehdr
   phentsize:INT -> SIZEOF phdr
   phnum:INT     -> number of phdr`s
   shentsize:INT -> SIZEOF shdr
   shnum:INT     -> number of shdr`s
   shstrndx:INT  ->
ENDOBJECT

ENUM PT_NULL,
     PT_LOAD,
     PT_DYNAMIC,
     PT_INTERP,
     PT_NOTE,
     PT_SHLIB,
     PT_PHDR

SET PF_X, -> executable
    PF_W, -> write
    PF_R  -> read

OBJECT phdr
   type:LONG     -> PT_xxx, normally PT_LOAD
   offset:LONG   -> offset in file of code/data
   vaddr:LONG    -> 0 for current targets
   paddr:LONG    -> 0 for current targets
   filesz:LONG   -> size on disk
   memsz:LONG    -> same as filesz for code
   flags:LONG    -> PF_R OR PF_X for code
   align:LONG    -> 4 for code
ENDOBJECT

CONST SHN_ABS = $FFF1,
      SHN_COMMON = $FFF2

CONST SHT_NULL = 0,
      SHT_PROGBITS = 1,
      SHT_SYMTAB = 2,
      SHT_STRTAB = 3,
      SHT_RELA = 4,
      SHT_HASH = 5,
      SHT_DYNAMIC = 6,
      SHT_NOBITS = 8,
      SHT_REL = 9,
      SHT_DYNSYM = 11

SET SHF_WRITE,
    SHF_ALLOC,
    SHF_EXECINSTR

OBJECT shdr
   name:LONG
   type:LONG
   flags:LONG
   addr:LONG
   offset:LONG
   size:LONG
   link:LONG
   info:LONG
   addralign:LONG
   entsize:LONG
ENDOBJECT

ENUM STB_LOCAL,
     STB_GLOBAL,
     STB_WEAK,
     STB_NUM

CONST STB_LOOS = 10
CONST STB_HIOS = 12
CONST STB_LOPROC = 13
CONST STB_HIPROC = 15

ENUM STT_NOTYPE,
     STT_OBJECT,
     STT_FUNC,
     STT_SECTION,
     STT_FILE, -> 1.7.1: was missing
     STT_COMMON, -> 1.7.1: was missing
     STT_TLS     -> 1.7.1 was missing

#define ST_BIND(val)        (Shr(val,4) AND $FFFFFFF)
#define ST_TYPE(val)        ((val) AND $F)
#define ST_INFO(bind,type) (Shl(bind, 4) + (type))

OBJECT sym
   name:LONG
   value:LONG
   size:LONG
   info:CHAR  -> ST_INFO
   other:CHAR
   shndx:INT
ENDOBJECT

ENUM  R_PPC_NONE,
      R_PPC_ADDR32,
      R_PPC_ADDR24,
      R_PPC_ADDR16,
      R_PPC_ADDR16_LO,
      R_PPC_ADDR16_HI,
      R_PPC_ADDR16_HA,
      R_PPC_ADDR14,
      R_PPC_ADDR14_BRTAKEN,
      R_PPC_ADDR14_BRNTAKEN,
      R_PPC_REL24,
      R_PPC_REL14,
      R_PPC_REL14_BRTAKEN,
      R_PPC_REL14_BRNTAKEN,
      R_PPC_GOT16,
      R_PPC_GOT16_LO,
      R_PPC_GOT16_HI,
      R_PPC_GOT16_HA,
      R_PPC_PLTREL24,
      R_PPC_COPY,
      R_PPC_GLOB_DAT,
      R_PPC_JMP_SLOT,
      R_PPC_RELATIVE,
      R_PPC_LOCAL24PC,
      R_PPC_UADDR32,
      R_PPC_UADDR16,
      R_PPC_REL32,
      R_PPC_PLT32,
      R_PPC_PLTREL32,
      R_PPC_PLT16_LO,
      R_PPC_PLT16_HI,
      R_PPC_PLT16_HA,
      R_PPC_SDAREL16,
      R_PPC_SECTOFF,
      R_PPC_SECTOFF_LO,
      R_PPC_SECTOFF_HI,
      R_PPC_SECTOFF_HA

      /* MorphOS additions */
CONST R_PPC_MORPHOS_DREL    =  200,
      R_PPC_MORPHOS_DREL_LO =  201,
      R_PPC_MORPHOS_DREL_HI =  202,
      R_PPC_MORPHOS_DREL_HA =  203

      /* AmigaOS4 additions */
CONST R_PPC_AMIGAOS_BREL =    210,
      R_PPC_AMIGAOS_BREL_LO = 211,
      R_PPC_AMIGAOS_BREL_HI = 212,
      R_PPC_AMIGAOS_BREL_HA = 213


ENUM R_68K_NONE,
     R_68K_32,
     R_68K_16,
     R_68K_8,
     R_68K_PC32

#define R_SYM(val)        Shr(val,8)
#define R_TYPE(val)       ((val) AND $FF)
#define R_INFO(sym,type)  (Shl(sym, 8) OR (type))

OBJECT rel
   offset:LONG
   info:LONG
ENDOBJECT

OBJECT rela OF rel
   addend:LONG
ENDOBJECT



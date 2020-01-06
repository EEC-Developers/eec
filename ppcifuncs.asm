
# The zlib/libpng License
#
# Copyright (c) 2003-2008 Leif Salomonsson
#
# This software is provided 'as-is', without any express or implied warranty.
# In no event will the authors be held liable for any damages arising from
# the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
#
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
#
# 3. This notice may not be removed or altered from any source distribution.
#
#
# ppcifuncs.asm

   ##################################
   # ECX POWERPC INTERNAL FUNCTIONS #
   ##################################
   #    FOR SYSV / MORPHOS ABI      #
   ##################################
   #  By Leif Salomonsson 2003-08   #
   ##################################
   #     ecx AT tele2 DOT se        #
   ##################################

# Sept 2008, added os4 support for functions:
# stringf, writef, printf, inp, out, readstr, list, string,
# disposelink, new, newr, newm, dispose
# (and indirectly fastnew/fastdispose/fastdisposelist)
# Left todo: debugf .. and a bunch of Fxxx() funcs.
# Enable _AMIGAOS4_ to use.

 .TEXT

.WORD P_FASTNEW         # 0
.WORD P_FASTDISPOSE     # 1
.WORD P_NEW             # 2
.WORD P_NEWR            # 3
.WORD P_DISPOSE         # 4
.WORD P_STRINGF_OLD     # 5
.WORD P_RAISE           # 6
.WORD P_THROW           # 7
.WORD P_RETHROW         # 8
.WORD P_STRCOPY         # 9
.WORD P_STRADD          # 10
.WORD P_STRCMP          # 11
.WORD P_STRLEN          # 12
.WORD P_TRIMSTR         # 13
.WORD P_ASTRCOPY        # 14
.WORD P_RNDQ            # 15
.WORD P_BOUNDS          # 16
.WORD P_MIN             # 17
.WORD P_MAX             # 18
.WORD P_ABS             # 19
.WORD P_SIGN            # 20
.WORD P_STRING          # 21
.WORD P_LIST            # 22
.WORD P_WRITEF_OLD      # 23
.WORD P_PRINTF_OLD      # 24
.WORD P_CLEANUP         # 25
.WORD P_FREESTACK       # 26
.WORD P_INSTR           # 27
.WORD P_UPPERSTR        # 28
.WORD P_LOWERSTR        # 29
.WORD P_OPENW           # 30
.WORD P_READSTR         # 31
.WORD P_CLOSEW          # 32
.WORD P_LISTCOPY        # 33
.WORD P_LISTADD         # 34
.WORD P_LISTCMP         # 35
.WORD P_VAL             # 36
.WORD P_CTRLC           # 37
.WORD P_MOD             # 38
.WORD P_FORWARD         # 39
.WORD P_PRIVATE_I2F     # 40 - private float support
.WORD P_DISPOSELINK     # 41
.WORD P_FASTDISPOSELIST # 42
.WORD P_KICKVERSION     # 43
.WORD P_REALVAL         # 44
.WORD P_INP             # 45
.WORD P_OUT             # 46
.WORD P_FORALL          # 47
.WORD P_EXISTS          # 48
.WORD P_MAPLIST         # 49
.WORD P_SELECTLIST      # 50
.WORD P_FABS            # 51 (OLD)
.WORD P_FFLOOR          # 52
.WORD P_FCEIL           # 53
.WORD P_FSIN            # 54
.WORD P_FCOS            # 55
.WORD P_FTAN            # 56
.WORD P_FEXP            # 57
.WORD P_FLOG            # 58
.WORD P_FPOW            # 59
.WORD P_FSQRT           # 60
.WORD P_FLOG10          # 61
.WORD P_FATAN           # 62
.WORD P_FSINCOS         # 63
.WORD P_FSINH           # 64
.WORD P_FCOSH           # 65
.WORD P_FTANH           # 66
.WORD P_FTIEEE          # 67
.WORD P_FFIEEE          # 68
.WORD P_FASIN           # 69
.WORD P_FACOS           # 70
.WORD P_LEFTMOUSE       # 71
.WORD P_WAITLEFTMOUSE   # 72
.WORD P_WAITIMESSAGE    # 73
.WORD P_SETSTDIN        # 74
.WORD P_SETSTDOUT       # 75
.WORD P_SETSTDRAST      # 76
.WORD P_TEXTF_OLD       # 77
.WORD P_REALF           # 78
.WORD P_FILELENGTH      # 79
.WORD P_MIDSTR          # 80
.WORD P_PLOT            # 81
.WORD P_LINE            # 82
.WORD P_RND             # 83
.WORD P_BOX             # 84
.WORD P_RIGHTSTR        # 85
.WORD P_SETCHUNKSIZE    # 86
.WORD P_SETCOLOUR       # 87
.WORD P_OPENS           # 88
.WORD P_CLOSES          # 89
.WORD P_OSTRCMP         # 90
.WORD P_NEWM            # 91
.WORD P_XTOD            # 92 DEPRECATED BUT KEPT
.WORD P_DTOX            # 93 DEPRECATED BUT KEPT
.WORD P_DEBUGF_OLD      # 94
.WORD P_COLOUR          # 95 (1.4.6)
.WORD P_PRIVATE_STRFMT_OLD  # 96 (1.5) PRIVATE, OBSOLETE

.WORD P_STRINGF         # 97 (1.5.4) New version
.WORD P_PRINTF          # 98 (1.5.4) New version
.WORD P_WRITEF          # 99 (1.5.4) New version
.WORD P_TEXTF           # 100 (1.5.4) New version
.WORD P_DEBUGF          # 101 (1.5.4) New version

# 1.10.0 May 2008 v55
.WORD P_PRIVATE_SHL64      # 102
.WORD P_PRIVATE_SHR64      # 103
.WORD P_PRIVATE_ASR64      # 104
.WORD P_PRIVATE_DIV64      # 105
.WORD P_MOD64              # 106
.WORD P_ABS64              # 107
.WORD P_PRIVATE_F2D64      # 108
.WORD P_PRIVATE_D642F      # 109

.WORD P_CODEEND

# constants ######

.SET FCHUNKSIZE, 4096

.SET ALLOCMEM, -198 # D0, D1
.SET FREEMEM, -210  # A1, D0
.SET ALLOCPOOLED, -708
.SET FREEPOOLED, -714
.SET EMULCALLDIRECTOS, 100
.SET RAWPUTCHAR, -516
.SET VNEWRAWDOFMT, -820  # R3:base, R4:fmtstr,R5:putproc,R6:putdata,R7:va_list
.SET RAWDOFMT, -522 # A0, A1, A2, A3

.SET FPUTS, -342
.SET WRITE, -48

.SET FRAME, 1
.SET STACK, 1
.SET GLOB, 13
.SET SYS, 2

.SET OLDFRAME, 0
.SET LINKREG, 4

.SET OLDSTRUCT, 0
.SET HANDLESTACK, 4
.SET EXCEPTCODE, 8

# the brainfucked va_list
#OBJECT va_list
.SET VA_GPR, 0 # gpr:CHAR
.SET VA_FPR, 1 # fpr:CHAR
   #pad:WORD
.SET VA_OVERFLOW, 4 # overflow_arg_area:PTR TO CHAR
.SET VA_REG_SAVE, 8 # reg_save_area:PTR TO CHAR
#ENDOBJECT


# WE USE UPPERCASE REGS :)

.SET R0, 0
.SET R1, 1
.SET R2, 2
.SET R3, 3
.SET R4, 4
.SET R5, 5
.SET R6, 6
.SET R7, 7
.SET R8, 8
.SET R9, 9
.SET R10, 10
.SET R11, 11
.SET R12, 12
.SET R13, 13
.SET R14, 14
.SET R15, 15
.SET R16, 16
.SET R17, 17
.SET R18, 18
.SET R19, 19
.SET R20, 20
.SET R21, 21
.SET R22, 22
.SET R23, 23
.SET R24, 24
.SET R25, 25
.SET R26, 26
.SET R27, 27
.SET R28, 28
.SET R29, 29
.SET R30, 30
.SET R31, 31

.SET F0, 0
.SET F1, 1
.SET F2, 2
.SET F3, 3
.SET F4, 4
.SET F5, 5
.SET F6, 6
.SET F7, 7
.SET F8, 8
.SET F9, 9
.SET F10, 10
.SET F11, 11
.SET F12, 12
.SET F13, 13


# GLOBALS ########

.SET MEMTABLE, -512
.SET MEMLIST, -20
.SET CURRFCHUNK, -100
.SET FCHUNKLEFT, -104
.SET EXCEPTSTRUCT, -148
.SET STDOUT, -8
.SET CONOUT, -12
.SET STDIN, -92
.SET STACKBOTTOM, -64
.SET CLIRETURNVAL, -28
.SET STDRAST, -16
.SET MEMPOOL, -120
.SET PV_STR_FMT, -196


.SET EXECBASE, -40
.SET DOSBASE, -44
.SET INTUITIONBASE, -48
.SET GFXBASE, -52
.SET EXCEPTION, -84
.SET EXCEPTIONINFO, -96
.SET PPCCLEANUP, -144
.SET QLARGSAVE, -152
.SET MATHBAS, -200
.SET MATHTRANS, -204

.SET CELLSMEM, -108
.SET FREECELLS, -112
.SET CHUNKSIZE, -116

.SET RANDSEED, -140
.SET FRNDSEEDH, -208
.SET FRNDSEEDL, -212

.SET SETAPEN, -342
.SET SETBPEN, -348

.SET VFPRINTF, -354


# os4
.SET IRAWDOFMT, 204
.SET IALLOCMEM, 104
.SET IFREEMEM, 164
.SET IALLOCPOOLED, 108
.SET IFREEPOOLED, 168

.SET IOPEN, 76
.SET ICLOSE, 80
.SET IREAD, 84
.SET IWRITE, 88
.SET ISETSIGNAL, 288
.SET IVFPRINTF, 324
.SET IMODIFYIDCMP, 156
.SET IREPLYMSG, 336
.SET IWAITPORT, 340
.SET IGETMSG, 324
.SET IDEBUGPRINTF, 748

.SET ILOCK, 112
.SET IEXAMINE, 124
.SET IUNLOCK, 116

.SET ISETAPEN, 284
.SET IMOVE, 216
.SET IDRAW, 220
.SET IWRITEPIXEL, 272
.SET IRECTFILL, 260
.SET ISETRGB32, 640
.SET ISETBPEN, 288
.SET ITEXT, 96

.SET IOPENWINDOWTAGLIST, 472
.SET ICLOSEWINDOW, 104
.SET IOPENSCREENTAGLIST, 480
.SET ICLOSESCREEN, 100

.SET IEXEC, -208
.SET IDOS, -212
.SET IGFX, -216
.SET IINTUI, -220



# Note: seems float values in .set does not work either.

#################################################################
################# MACROS ########################################
#################################################################

.MACRO M_INLINE_NEW
   ADDI R3, R3, 8  # add memheader size
   STW R3, 12(R1) # save size
.IFDEF _AMIGAOS4_
   OR R4, R3, R3
   LWZ R3, IEXEC(GLOB)
   LWZ R0, IALLOCMEM(R3)
   MTCTR R0          # to ctr
   ADDI R5, 0, 1<<12 # memf_shared
   ORIS R5, R5, 1    # memf_clear
.ELSE # morphos
   LWZ R0, EMULCALLDIRECTOS(SYS)
   MTCTR R0         # to ctr
   STW R3, 0(SYS) # emulhandle.dregs[0] := size
   ADDI R3, 0, 1  # memf_public
   ORIS R3, R3, 1 # memf_clear
   STW R3, 4(SYS)   # emulhandle.dregs[1] := flags
   LWZ R3, EXECBASE(GLOB)
   STW R3, 56(SYS)  # emulhandle.aregs[6] := execbase
   ADDI R3, 0, ALLOCMEM
.ENDIF
   BCTRL            # call AllocMem()
   OR. R3, R3, R3
   BEQ _\@ipnew_end
   LWZ R0, MEMLIST(GLOB)
   STW R0, 0(R3)   # set next
   STW R3, MEMLIST(GLOB) # add to list
   LWZ R0, 12(R1)
   STW R0, 4(R3)   # set size
   ADDI R3, R3, 8  # skip head
_\@ipnew_end:
.ENDM


.MACRO M_INLINE_DISPOSE
   ADDI R3, R3, -8 # get memheader
   ADDI R4, GLOB, MEMLIST # adr of memlist var
_\@disposefind:
   LWZ R5, 0(R4) # mem
   OR. R5, R5, R5
   BEQ _\@dispose_end # not found ?
   CMPW 0, R5, R3
   BEQ _\@disposefound
   OR R4, R5, R5
   B _\@disposefind
_\@disposefound: # R4:previous, R3, our mem
   LWZ R0, 0(R3)
   STW R0, 0(R4)    # prev.next := this.next
.IFDEF _AMIGAOS4_
   OR R4, R3, R3
   LWZ R3, IEXEC(GLOB)
   LWZ R0, IFREEMEM(R3)
   MTCTR R0          # to ctr
   LWZ R5, 4(R4)     # get size
.ELSE # morphos
   LWZ R0, EMULCALLDIRECTOS(SYS)
   MTCTR R0          # to linkreg
   STW R3, 36(SYS)   # emulhandle.a1 := mem
   LWZ R3, 4(R3)
   STW R3, 0(SYS)    # emulhandle.d0 := size
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)   # emulhandle.a6 := execbase
   ADDI R3, 0, FREEMEM
.ENDIF
   BCTRL             # call FreeMem()
_\@dispose_end:
.ENDM


.MACRO M_INLINE_RAISE # R3
   STW R3, EXCEPTION(GLOB)    # set exception global
   LWZ R4, EXCEPTSTRUCT(GLOB) # get last exceptstruct
   OR. R4, R4, R4
   BEQ \@raise_cleanup
   LWZ R0, OLDSTRUCT(R4)
   STW R0, EXCEPTSTRUCT(GLOB) # restore old exceptstruct
   LWZ R1, HANDLESTACK(R4)    # restore stackpointer
   LWZ R0, EXCEPTCODE(R4)     # get exceptioncode
   MTCTR R0
   BCTR                        # jump to handler
\@raise_cleanup:
   STW R3, CLIRETURNVAL(GLOB)
   LWZ R0, PPCCLEANUP(GLOB)
   MTCTR R0
   BCTR                        # jump to exitcode
.ENDM



.MACRO M_FASTNEW
# we try and alloc from table first, then we try chunk, then we try sys.
P_FASTNEW: # (size:R3) ###############################################
   CMPWI R3, 256
   BGT _sysalloc # size was bigger than 256, alloc from system instead
   ADDI R4, R3, 3
   ANDI. R4, R4, 0xFFC
   BEQ _fnewerror # size was nil0 = error
_alloc1:
   ADDI R7, GLOB, MEMTABLE
   LWZX R3, R7, R4
   OR. R3, R3, R3 # something in bucket ?
   BEQ _alloc2 # no, try chunkspace
   LWZ R0, 0(R3) # next
   STWX R0, R7, R4 # set to next mem
   ADDI R0, 0, 0
   STW R0, 0(R3) # clear nextfield
   BLR # return
_alloc2:
   LWZ R5, FCHUNKLEFT(GLOB)
   SUB. R6, R5, R4
   BLT _alloc3
   STW R6, FCHUNKLEFT(GLOB) # update
   LWZ R3, CURRFCHUNK(GLOB)
   ADD R0, R3, R4
   STW R0, CURRFCHUNK(GLOB)
   BLR # return
_alloc3: # need new chunk
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R4, 8(R1) # save size
   ADDI R3, 0, FCHUNKSIZE
   M_INLINE_NEW
   LWZ R4, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   OR. R3, R3, R3
   BEQ _fnewerror
   ADDI R0, 0, FCHUNKSIZE
   SUB R0, R0, R4
   STW R0, FCHUNKLEFT(GLOB) # set new chunkleft
   ADD R0, R3, R4
   STW R0, CURRFCHUNK(GLOB) # set new chunk
   BLR # return
_fnewerror:
   ADDI R3, 0, "EM"
   ORIS R3, R3, "M"
   M_INLINE_RAISE
_sysalloc:
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   M_INLINE_NEW
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   OR. R3, R3, R3
   BEQ _fnewerror
   BLR # return
.ENDM

.MACRO M_FASTDISPOSE
P_FASTDISPOSE: # (mem:R3, size:R4) ####################################
   OR. R3, R3, R3
   BEQ _fd_done # zero pointer !?!
   CMPWI R4, 256
   BGT _fd_sysfree # size was bigger than 256, free via system instead
   ADDI R4, R4, 3
   ANDI. R4, R4, 0xFFC
   BEQ _fd_done # zero size !?!
   ADDI R5, GLOB, MEMTABLE
   LWZX R7, R5, R4 # R7 is first mem in bucket, or NIL
   STW R7, 0(R3) # set next to the old mem
   STWX R3, R5, R4 # install our mem in bucket
   SRWI R4, R4, 2 # size in longs
   ADDI R4, R4, -2 # sub two longs
   OR. R4, R4, R4
   BLE _fd_done
   MTCTR R4
   ADDI R5, 0, 0 # load zero in R5
_fd_clearmem:
   STWU R5, 4(R3) # clear long
   BDNZ _fd_clearmem
_fd_done:
   BLR # return
_fd_sysfree:
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   M_INLINE_DISPOSE
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR # return
.ENDM

.MACRO M_NEW
P_NEW: # (size:R3) ################################################
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   M_INLINE_NEW
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR                # return
.ENDM

.MACRO M_NEWR
P_NEWR: # (size:R3) ################################################
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   M_INLINE_NEW
   OR. R3, R3, R3
   BNE _ipnrr_end
   ADDI R3, 0, "EM"
   ORIS R3, R3, "M"
   M_INLINE_RAISE
_ipnrr_end:
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR                # return
.ENDM

.MACRO M_DISPOSE
P_DISPOSE: # (mem:R3) #################################################
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   M_INLINE_DISPOSE
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR              # return
.ENDM


.MACRO M_STRINGF_OLD
# v44: NEW CALLING CONVENTION !!!
# string:R3, fmtstr:R4, array:R5
# "array" is a perfectly normal longword-array !
# v50: now acts as wrapper for P_STR_FMT
P_STRINGF_OLD: # (estr,fmtstr,varray) ( r3,r4,r5 )
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 8(R1) # save
   # call P_STR_FMT
   OR R3, R4, R4
   OR R4, R5, R5
   LWZ R0, PV_STR_FMT(GLOB)
   MTCTR R0
   BCTRL
   # copy string from stackbottom into estr
   # check maxlen against len
   LWZ R4, 8(R1) # estr
   LHZ R5, -4(R4) # maxlen
   CMPW 0, R5, R3
   BGE $+8
   OR R3, R5, R5
   OR. R3, R3, R3
   BEQ _stringf_finnish

   LWZ R5, STACKBOTTOM(GLOB)
   ADDI R5, R5, -1
   ADDI R4, R4, -1
   MTCTR R3
_stringf_copy:
   LBZU R0, 1(R5)
   STBU R0, 1(R4)
   BDNZ _stringf_copy
_stringf_finnish:
   OR R4, R3, R3 # return len in r4
   LWZ R3, 8(R1) # estr
   STH R4, -2(R3) # currlen
   ADDI R0, 0, 0
   STBX R0, R3, R4 # nilbyte
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM


.MACRO M_RAISE
P_RAISE: # exception:R3
   M_INLINE_RAISE
.ENDM

.MACRO M_THROW
P_THROW: # exception:R3, exceptioninfo:R4
   STW R3, EXCEPTION(GLOB) # set exception global
   STW R4, EXCEPTIONINFO(GLOB) # ..
   LWZ R4, EXCEPTSTRUCT(GLOB) # get last exceptstruct
   OR. R4, R4, R4
   BEQ _throw_cleanup
   LWZ R0, OLDSTRUCT(R4)
   STW R0, EXCEPTSTRUCT(GLOB) # restore old exceptstruct
   LWZ R1, HANDLESTACK(R4) # restore stackpointer
   LWZ R0, EXCEPTCODE(R4) # get exceptcode
   MTCTR R0
   BCTR # jump to handler
_throw_cleanup:
   STW R3, CLIRETURNVAL(GLOB)
   LWZ R0, PPCCLEANUP(GLOB) # get exitcode
   MTCTR R0
   BCTR # jump to exitcode
.ENDM

.MACRO M_RETHROW
P_RETHROW:
   LWZ R3, EXCEPTION(GLOB) # is exception set ?
   OR. R3, R3, R3
   BEQ _reth_no_
   LWZ R4, EXCEPTSTRUCT(GLOB) # get last exceptstruct
   OR. R4, R4, R4
   BEQ _rethrow_cleanup
   LWZ R0, OLDSTRUCT(R4)
   STW R0, EXCEPTSTRUCT(GLOB) # restore old exceptstruct
   LWZ R1, HANDLESTACK(R4) # restore stackpointer
   LWZ R0, EXCEPTCODE(R4) # get exceptcode
   MTCTR R0
   BCTR # jump to handler
_reth_no_:
   BLR # return
_rethrow_cleanup:
   STW R3, CLIRETURNVAL(GLOB)
   LWZ R0, PPCCLEANUP(GLOB) # get exitcode
   MTCTR R0
   BCTR # jump to exitcode
.ENDM

.MACRO M_STRCOPY
P_STRCOPY: # estr, str, len=ALL
   or r6, r3, r3
   rlwinm r5, r5, 0, 1, 31
   lhz r7, -4(r3)
   cmpw r5, r7
   blt _ips_lendone # blt
   or r5, r7, r7
_ips_lendone:
   addi r3, r3, -1
   addi r4, r4, -1
   cmpwi r5, 0
   beq _ips_nil # beq nil
   mtspr 9, r5
_ips_copy:
   lbzu r5, 1(r4)
   stbu r5, 1(r3)
   or. r5, r5, r5
   beq _ips_end # beq end
   bdnz _ips_copy # bdnz copy
_ips_nil:
   addi r5, r0, 0
   stbu r5, 1(r3) # nilbyte
_ips_end:
   subf r5, r6, r3 # get copylen
   sth r5, -2(r6) # set copylen
   or r3, r6, r6 # return dest
   bclr 20, 0
.ENDM

.MACRO M_STRADD
P_STRADD: # estr, str, len=ALL
   or r7, r3, r3 # save dest
   rlwinm r5, r5, 0, 1, 31 # mask away high bit
   lhz r6, -4(r3) # maxlen
   lhz r8, -2(r3) # currlen
   subf r6, r8, r6 #maxlen-currlen
   cmpw r5, r6
   blt _ipsa_lendone # blt
   or r5, r6, r6 # trunc len
_ipsa_lendone:
   add r6, r3, r8 # dest + currlen
   addi r6, r6, -1
   addi r4, r4, -1
   cmpwi r5, 0
   beq _ipsa_nil # beq
   mtspr 9, r5
_ipsa_copy:
   lbzu r5, 1(r4)
   stbu r5, 1(r6)
   or. r5, r5, r5
   beq _ipsa_end # beq
   bdnz _ipsa_copy # bdnz
_ipsa_nil:
   addi r5, r0, 0
   stbu r5, 1(r6) # nilbyte
_ipsa_end:
   subf r5, r7, r6 # get tot len
   sth r5, -2(r7) # set tot len
   or r3, r7, r7 # return dest
   bclr 20, 0
.ENDM

.MACRO M_STRCMP
P_STRCMP: # str1, str2, len=ALL
   rlwinm. r5,r5,0,1,31
   beq _ipsc_cmp_true
   addi r3, r3, -1
   addi r4, r4, -1
   #addi r5, r5, -1
   mtspr 9, r5
_ipsc_cmp:
   lbzu r6, 1(r3)
   lbzu r7, 1(r4)
   cmpw r6, r7
   bne _ipsc_cmp_false
   or. r6, r6, r7
   beq _ipsc_cmp_true
   bdnz _ipsc_cmp
_ipsc_cmp_true:
   addi r3, r0, -1
   bclr 20, 0
_ipsc_cmp_false:
   addi r3, r0, 0
   bclr 20, 0
.ENDM

.MACRO M_STRLEN
P_STRLEN: # str
   addi r4, r3, -1
_ipslen_len:
   lbzu r5, 1(r4)
   or. r5, r5, r5
   bne _ipslen_len
   subf r3, r3, r4 # compute len
   bclr 20, 0
.ENDM

.MACRO M_TRIMSTR
P_TRIMSTR: # str
   lbz  r4, 0(r3)
   cmpwi r4, 32
   bgt _trs_done # bgt done
   cmpwi r4, 0
   beq _trs_done # beq done
   addi r3, r3, 1
   b    P_TRIMSTR
_trs_done:
   bclr 20, 0 # done
.ENDM

.MACRO M_ASTRCOPY
P_ASTRCOPY: # a, s, l=ALL
   or r7, r3, r3
   cmpwi r5, 0
   beq _astr_end # beq end
   addi r3, r3, -1
   addi r4, r4, -1
   addi r5, r5, -1
   cmpwi r5, 0
   beq _astr_nil # beq nil
   mtspr 9, r5
_astr_copy:
   lbzu r6, 1(r4)
   stbu r6, 1(r3)
   cmpwi 0, r6, 0
   beq _astr_end # beq end
   bdnz _astr_copy # bdnz
_astr_nil:
   addi r6, r0, 0
   stb r6, 1(r3)
_astr_end:
   or r3, r7, r7 # return dest array
   bclr 20, 0
.ENDM

.MACRO M_RNDQ
P_RNDQ: # seed
   add. r3, r3, r3
   bgt _rndq_end
   xori r3, r3, 0x2b41
   xoris r3, r3, 0x1d87
_rndq_end:
   bclr 20, 0
.ENDM

.MACRO M_BOUNDS
P_BOUNDS: # x, a, b
   subf  r6, r4, r3
   srawi r7, r6, 31
   neg   r6, r6
   and   r8, r6, r7
   add   r3, r3, r8
   subf  r6, r3, r5
   srawi r7, r6, 31
   andc   r8, r6, r7
   subf  r3, r8, r5
   bclr  20, 0
.ENDM

.MACRO M_MIN
P_MIN: # x, y
   xoris r5, r4, 0x8000
   xoris r6, r3, 0x8000
   subfc r6, r6, r5
   subfe r5, r5, r5
   and r6, r6, r5
   add r3, r6, r3
   blr
.ENDM

.MACRO M_MAX
P_MAX: # x, y
   xoris r5, r4, 0x8000
   xoris r6, r3, 0x8000
   subfc r6, r6, r5
   subfe r5, r5, r5
   andc r6, r6, r5
   add r3, r6, r3
   blr
.ENDM

.MACRO M_ABS
P_ABS: # v
   srawi r4,r3,31
   add r3,r4,r3
   xor r3,r3,r4
   bclr 20, 0
.ENDM

.MACRO M_SIGN
P_SIGN: # v
   xoris r4, r3, 0x8000
   srawi  r5, r4, 31
   subfze r3, r5
   bclr   20, 0
.ENDM

.MACRO M_STRING
# V46: now uses mempool global
#      or.. may take a mempool as second arg !
# V49: assumes mempool(r13) exists!
P_STRING: # len,mempool=NIL
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   stw r3, 8(r1)
   or. r4, r4, r4
   bne _string_1
   lwz r4, MEMPOOL(GLOB)
_string_1:
   addi r3, r3, 20 # size:32,next:32,max:16,len:16,nil:8
   andi. r3, r3, 0xfff8
.IFDEF _AMIGAOS4_
   or r5, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IALLOCPOOLED(R3)
.ELSE # morphos
   stw r3, 12(r1)
   stw r3, 0(r2)
   stw r4, 32(r2) # pool
   lwz r0, EXECBASE(GLOB)
   stw r0, 56(r2)
   lwz r0, 100(r2)
   addi r3, 0, ALLOCPOOLED
.ENDIF
   mtctr r0
   bctrl
   or. r3, r3, r3
   beq _string_end
   lwz r4, 12(r1)
   stw r4, 0(r3)
   addi r0, 0, 0
   stw r0, 4(r3) # clear link
   lwz r4, 8(r1) # len
   slwi r4, r4, 16
   stw r4, 8(r3) # maxlen,currlen
   addi r3, r3, 12
   stb r0, 0(r3) # nilterm
_string_end:
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr
.ENDM


.MACRO M_LIST
# V46: now uses mempool global
#      or.. may take a mempool as second arg !
P_LIST: # len:r3, mempool=NIL:r4
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   stw r3, 8(r1)
   or. r4, r4, r4
   bne _list_1
   lwz r4, MEMPOOL(GLOB)
_list_1:
   slwi r3, r3, 2 # * 4
   addi r3, r3, 12 # size:32,next:32,max:16,len:16
.IFDEF _AMIGAOS4_
   or r5, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IALLOCPOOLED(R3)
.ELSE # morphos
   stw r3, 12(r1)
   stw r3, 0(r2)
   stw r4, 32(r2) # pool
   lwz r0, EXECBASE(GLOB)
   stw r0, 56(r2)
   lwz r0, 100(r2)
   addi r3, 0, ALLOCPOOLED
.ENDIF
   mtctr r0
   bctrl
   or. r3, r3, r3
   beq _list_end
   lwz r4, 12(r1)
   stw r4, 0(r3)
   addi r0, 0, 0
   stw r0, 4(r3) # clear link
   lwz r4, 8(r1) # len
   slwi r4, r4, 16
   stw r4, 8(r3) # maxlen,currlen
   addi r3, r3, 12
_list_end:
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr
.ENDM

.MACRO M_WRITEF_OLD
P_WRITEF_OLD: # fstr:r3, args:r4
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   lwz r5, STDOUT(GLOB)
   or. r5, r5, r5
   beq _writef_con
_writef_doit:
   lwz r0, PV_STR_FMT(GLOB)
   mtctr r0
   bctrl # Str fmt, result at stackbottom, len in r3
   stw r3, 8(r1) # save len
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   lwz r0, STACKBOTTOM(GLOB)
   stw r0, 8(SYS) # d2
   stw r3, 12(SYS) # d3
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -48
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   bctrl # Write()
   lwz r3, 8(r1) # return len
_writef_end:
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr
_writef_con:
   stw r3, 8(r1)
   stw r4, 12(r1)
   bl $+28
   .byte "CON:///100/Output/CLOSE",0 # 24 bytes
   mflr r3
   stw r3, 4(SYS) # d1
   addi r3, r0, 1006
   stw r3, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -30
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   bctrl # Open()
   stw r3, CONOUT(GLOB)
   stw r3, STDOUT(GLOB)
   or. r3, r3, r3
   lwz r3, 8(r1)
   lwz r4, 12(r1)
   bne _writef_doit
   addi r3, r0, 20
   stw r3, CLIRETURNVAL(GLOB)
   lwz r0, PPCCLEANUP(GLOB)
   mtlr r0
   blr # exit
_writef_err:
   addi r3, 0, 0
   b _writef_end
.ENDM


.MACRO M_PRINTF_OLD
P_PRINTF_OLD: # fstr:r3, args:r4
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   lwz r5, STDOUT(GLOB)
   or. r5, r5, r5
   beq _printf_con
_printf_doit:
   lwz r0, PV_STR_FMT(GLOB)
   mtctr r0
   bctrl # Str fmt
   stw r3, 8(r1) # save len
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   lwz r0, STACKBOTTOM(GLOB)
   stw r0, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -342
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtlr r0
   blrl # FPuts()
   lwz r3, 8(r1) # return len
_printf_end:
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr
_printf_con:
   stw r3, 8(r1)
   stw r4, 12(r1)
   bl $+28
   .byte "CON:///100/Output/CLOSE",0 # 24 bytes
   mflr r3
   stw r3, 4(SYS) # d1
   addi r3, r0, 1006
   stw r3, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -30
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   bctrl # Open()
   stw r3, CONOUT(GLOB)
   stw r3, STDOUT(GLOB)
   or. r3, r3, r3
   lwz r3, 8(r1)
   lwz r4, 12(r1)
   bne _printf_doit
   addi r3, r0, 20
   stw r3, CLIRETURNVAL(GLOB)
   lwz r0, PPCCLEANUP(GLOB)
   mtlr r0
   blr # exit
_printf_err:
   addi r3, 0, 0
   b _printf_end
.ENDM

.MACRO M_CLEANUP
P_CLEANUP: # (retval)
   stw r3, CLIRETURNVAL(GLOB)
   lwz r0, PPCCLEANUP(GLOB)
   mtlr r0
   blr
.ENDM

.MACRO M_FREESTACK
P_FREESTACK:
   lwz r3, STACKBOTTOM(GLOB)
   subf r3, r3, r1
   addi r3, r3, -1600
   blr
.ENDM

.MACRO M_INSTR
P_INSTR: # str1:r3, str2:r4, spos:r5
   add r6, r3, r5
   addi r6, r6, -1
   lbz r5, 0(r4) # char to find
_instr_char:
   lbzu r7, 1(r6)
   or. r7, r7, r7
   beq _instr_nope
   cmpw 0, r5, r7
   beq _instr_foundchar
   b _instr_char
_instr_foundchar:
   or r8, r6, r6 # copy of current str1
   or r9, r4, r4 # copy of current str2
_instr_str:
   lbzu r7, 1(r9) # str to find
   or. r7, r7, r7
   beq _instr_yep
   lbzu r10, 1(r8) # in this str ?
   or. r10, r10, r10
   beq _instr_nope
   cmpw 0, r7, r10
   bne _instr_char
   b _instr_str
_instr_nope:
   addi r3, r0, -1
   blr
_instr_yep:
   subf r3, r3, r6
   blr
.ENDM

.MACRO M_UPPERSTR
P_UPPERSTR: # str
   addi r4, r3, -1
_upper_loop:
   lbzu r5, 1(r4)
   or. r5, r5, r5
   beq _upper_end
   cmpwi r5, "a"
   blt _upper_loop
   cmpwi r5, "z"
   bgt _upper_loop
   addi r5, r5, -32
   stb r5, 0(r4)
   b _upper_loop
_upper_end:
   blr
.ENDM

.MACRO M_LOWERSTR
P_LOWERSTR: # str
   addi r4, r3, -1
_lower_loop:
   lbzu r5, 1(r4)
   or. r5, r5, r5
   beq _lower_end
   cmpwi r5, "A"
   blt _lower_loop
   cmpwi r5, "Z"
   bgt _lower_loop
   addi r5, r5, 32
   stb r5, 0(r4)
   b _lower_loop
_lower_end:
   blr
.ENDM

.MACRO M_OPENW
P_OPENW:
#        x:r3
#        y:r4
#        w:r5
#        h:r6
#        idcmp:r7
#        wflags:r8
#        title:r9
#        screen:r10 -> screen, or NIL for default public screen
#        sflags:104(r1) -> IGNORED!
#        gadgets:108(r1)
#        taglist:112(r1)


   STWU R1, -96(R1)
   MFLR R0
   STW R0, 100(R1)

   ADDIS R11, 0, 0x8000

   ADDI R0, R11, 0x64 # LEFT
   STW R0, 8(R1)
   STW R3, 12(R1)
   ADDI R0, R11, 0x65 # TOP
   STW R0, 16(R1)
   STW R4, 20(R1)
   ADDI R0, R11, 0x66 # WIDTH
   STW R0, 24(R1)
   STW R5, 28(R1)
   ADDI R0, R11, 0x67 # HEIGHT
   STW R0, 32(R1)
   STW R6, 36(R1)
   ADDI R0, R11, 0x6a # IDCMP
   STW R0, 40(R1)
   STW R7, 44(R1)
   ADDI R0, R11, 0x6b # FLAGS
   STW R0, 48(R1)
   STW R8, 52(R1)
   ADDI R0, R11, 0x6e # TITLE
   STW R0, 56(R1)
   STW R9, 60(R1)
   ADDI R0, R11, 0x79 # PUBSCREEN
   STW R0, 64(R1)
   STW R10, 68(R1)
   ADDI R0, R11, 0x6c # GADGETS
   STW R0, 72(R1)
   LWZ R0, 108(R1)
   STW R0, 76(R1)

   ADDI R0, R0, 0 # TAG_END
   STW R0, 80(R1)
   LWZ R0, 112(R1)
   STW R0, 84(R1)

   OR. R0, R0, R0
   BEQ $ + 12
   ADDI R0, R0, 2 # TAG_MORE
   STW R0, 80(R1)
.IFDEF _AMIGAOS4_
   lwz r3, IINTUI(GLOB)
   addi r4, 0, 0
   addi r5, r1, 8
   lwz r0, IOPENWINDOWTAGLIST(r3)
.ELSE #morphos
   ADDI R0, R0, 0
   STW R0, 32(r2) # A0
   ADDI R0, R1, 8
   STW R0, 36(R2) # A1
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(R2) # A6
   ADDI R3, R0, -606
   LWZ R0, 100(R2)
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _openw_end
   LWZ R4, 50(R3)
   STW R4, STDRAST(GLOB)
_openw_end:
   LWZ R0, 100(R1)
   MTLR R0
   ADDI R1, R1, 96
   BLR
.ENDM

.MACRO M_READSTR
P_READSTR: # fh:r3, estr:r4
   stwu r1, -32(r1)
   mflr r0
   stw r0, 36(r1)
   stw r3, 8(r1) # save fh
   stw r4, 12(r1) # save estr
   stw r31, 16(r1) # save strptr reg
   addi r31, r4, -1
   stw r30, 20(r1) # save endptr reg
   lhz r30, -4(r4) # maxlen
   add r30, r30, r4
_readstr_read:
   addi r31, r31, 1
   cmpw r31, r30
   beq _readstr_err
.IFDEF _AMIGAOS4_
   lwz r3, IDOS(GLOB)
   lwz r0, IREAD(R3)
   lwz r4, 8(r1)   # fh
   or r5, r31, r31 # strptr
   addi r6, 0, 1   # len
.ELSE # morphos
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(r2)
   lwz r0, 8(r1)
   stw r0, 4(r2)  # d1 = fh
   stw r31, 8(r2) # d2 = strptr
   addi r0, 0, 1
   stw r0, 12(r2) # d3 = 1
   addi r3, 0, -42
   lwz r0, 100(r2)
.ENDIF
   mtctr r0
   bctrl # Read()
   cmpwi r3, 1
   bne _readstr_err
   lbz r0, 0(r31)
   cmpwi r0, 10
   bne _readstr_read
   addi r3, r0, 0
_readstr_end:
   addi r0, r0, 0
   stb r0, 0(r31) # nilterm (overwrites newline)
   lwz r5, 12(r1) # estr
   subf r4, r5, r31 # len
   sth r4, -2(r5) # set it
   b _readstr_exit
_readstr_err:
   lwz r31, 12(r1) # makes len=0
   addi r3, r0, -1
   b _readstr_end
_readstr_exit:
   lwz r30, 20(r1)
   lwz r31, 16(r1)
   lwz r0, 36(r1)
   mtlr r0
   addi r1, r1, 32
   blr
.ENDM

.MACRO M_CLOSEW
P_CLOSEW: # w
   OR. R3, R3, R3
   BEQ _closew_end
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   ADDI R0, 0, 0
   STW R0, STDRAST(GLOB)
.IFDEF _AMIGAOS4_
   or r4, r3,r3 # w
   lwz r3, IINTUI(GLOB)
   lwz r0, ICLOSEWINDOW(r3)
.ELSE #morphos
   STW R3, 32(R2) # A0
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(R2) # A6
   ADDI R3, 0, -72
   LWZ R0, EMULCALLDIRECTOS(R2)
.ENDIF
   MTCTR R0
   BCTRL # CloseWindow()
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
_closew_end:
   BLR
.ENDM

.MACRO M_LISTCOPY
P_LISTCOPY: # dlist, slist, len=ALL
   OR R7, R3, R3
   CMPWI R5, -1
   BNE $+8
   LHZ R5, -2(R4)
   LHZ R6, -4(R3)
   CMPW R6, R5
   BGE $+12
   OR. R5, R6, R6 # len := Min(dlistmax,len)
   BEQ _listcopy_end
   MTCTR R5
   ADDI R3, R3, -4
   ADDI R4, R4, -4
_listcopy_loop:
   LWZU R0, 4(R4)
   STWU R0, 4(R3)
   BDNZ _listcopy_loop
_listcopy_end:
   OR R3, R7, R7
   STH R5, -2(R3) # new len
   BLR
.ENDM

.MACRO M_LISTADD
P_LISTADD: # dlist, slist, len=ALL
   OR R7, R3, R3
   CMPWI R5, -1
   BNE $+8
   LHZ R5, -2(R4)  # src currlen
   LHZ R6, -4(R3)  # dst maxlen
   LHZ R8, -2(R3)  # dst currlen
   SUBF R6, R8, R6 # - currlen
   CMPW R6, R5
   BGE $+12
   OR. R5, R6, R6 # len := Min(dlistleft,len)
   BEQ _listadd_end
   MTCTR R5
   ADDI R3, R3, -4
   SLWI R9, R8, 2 # longs=>bytes
   ADD R3, R3, R9 # + current
   ADDI R4, R4, -4
_listadd_loop:
   LWZU R0, 4(R4)
   STWU R0, 4(R3)
   BDNZ _listadd_loop
_listadd_end:
   OR R3, R7, R7
   ADD R5, R5, R8 # + oldlen
   STH R5, -2(R3) # new len
   BLR
.ENDM

.MACRO M_LISTCMP
P_LISTCMP: # list1, list2, len=ALL
   CMPWI R5, -1
   BNE $+8
   LHZ R5, -2(R4)
   LHZ R6, -2(R3)
   CMPW R6, R5
   BNE _listcmp_false
   OR. R5, R5, R5
   BEQ _listcmp_true
   MTCTR R5
   ADDI R3, R3, -4
   ADDI R4, R4, -4
_listcmp_loop:
   LWZU R5, 4(R3)
   LWZU R6, 4(R4)
   CMPW R5, R6
   BNE _listcmp_false
   BDNZ _listcmp_loop
_listcmp_true:
   ADDI R3, 0, -1
   BLR
_listcmp_false:
   ADDI R3, 0, 0
   BLR
.ENDM

.MACRO M_VAL
P_VAL: # str, readvaradr=NIL
   or r5, r3, r3
   or r9, r4, r4
   addi r6, r0, 0
   addi r7, r0, 1  # sign
   addi r3, r3, -1
_val_trim:
   lbzu r8, 1(r3)
   or. r8, r8, r8
   beq _val_empty
   cmpwi r8, 33
   blt _val_trim
   cmpwi r8, "%"
   beq _val_bin
   cmpwi r8, "$"
   beq _val_hex
   cmpwi r8, "-"
   beq _val_neg
   cmpwi r8, "0"
   blt _val_empty
   cmpwi r8, "9"
   bgt _val_empty
_val_dec:
   lbz r8, (r3)
   cmpwi r8, "0"
   blt _val_end
   cmpwi r8, "9"
   bgt _val_end
   addi r3, r3, 1
   mulli r6, r6, 10
   addi r8, r8, -48
   add r6, r6, r8
   b _val_dec
_val_bin:
   lbzu r8, 1(r3)
   cmpwi r8, "0"
   blt _val_end
   cmpwi r8, "1"
   bgt _val_end
   slwi r6, r6, 1
   addi r8, r8, -48
   add r6, r6, r8
   b _val_bin
_val_hex:
   lbzu r8, 1(r3)
   cmpwi r8, "0"
   blt _val_end
   cmpwi r8, "9"
   bgt _val_hex_u
   addi r8, r8, -48
   slwi r6, r6, 4
   add r6, r6, r8
   b _val_hex
_val_hex_u:
   cmpwi r8, "A"
   blt _val_end
   cmpwi r8, "F"
   bgt _val_hex_l
   addi r8, r8, -55
   slwi r6, r6, 4
   add r6, r6, r8
   b _val_hex
_val_hex_l:
   cmpwi r8, "a"
   blt _val_end
   cmpwi r8, "f"
   bgt _val_end
   addi r8, r8, -87
   slwi r6, r6, 4
   add r6, r6, r8
   b _val_hex
_val_neg:
   addi r7, 0, -1
   addi r3, r3, 1
   b _val_dec
_val_empty:
   addi r3, r0, 0
   addi r4, r0, 0
   or. r9, r9, r9
   beq $+8
   stw r4, 0(r9)
   blr
_val_end:
   subf r4, r5, r3
   or. r9, r9, r9
   beq $+8
   stw r4, 0(r9)
   mullw r3, r6, r7
   blr
.ENDM

.MACRO M_CTRLC
P_CTRLC:
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
.IFDEF _AMIGAOS4_
   lwz r3, IEXEC(GLOB)
   lwz r0, ISETSIGNAL(R3)
   addi r4, 0, 0
   addi r5, 0, 0
.ELSE # morphos
   ADDI R0, 0, 0
   STW R0, 0(SYS)
   STW R0, 4(SYS)
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, EMULCALLDIRECTOS(SYS)
   ADDI R3, 0, -306
.ENDIF
   MTCTR R0
   BCTRL # call setsignal
   ADDI R0, 0, 0x1000 # 68k bit 12
   AND R3, R3, R0
   SRWI R3, R3, 12
   NEG R3, R3    # TRUE/FALSE
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_MOD
P_MOD: # x,d
   DIVW R5, R3, R4
   MULLW R6, R5, R4
   SUBF R3, R6, R3 # mod
   OR R4, R5, R5   # div
   BLR
.ENDM

.MACRO M_FORWARD
P_FORWARD: # cmplx,num
   OR. R3, R3, R3
   BEQ _forward_end
   OR. R4, R4, R4
   BEQ _forward_end
_forward_loop:
   ADDI R4, R4, -1
   LWZ R3, -8(R3)
   CMPWI 1, R3, 0
   CMPWI 2, R4, 0
   CROR 0, 1, 2
   BNE _forward_end
   B _forward_loop
_forward_end:
   BLR
.ENDM

.MACRO M_PRIVATE_I2F
-> private convert 32bit integer to 64bit float
-> only trashes r0,f0,f13 !
P_PRIVATE_I2F: # integer:R0 => float:F0
   stw r3, -4(r1)
   or r3, r0, r0
   mflr r0
   stw r0, 4(r1)
   stw r4, -8(r1)

   bl $+12
   .word 0x43300000  # %0100 0011 0011
   .word 0x80000000
   mflr r4
   lfd f13, 0(r4)

   addis r0, r0, 0x4330
   stw r0, -16(r1) # upper half
   xoris r3, r3, 0x8000
   stw r3, -12(r1) # lower half
   lfd f0, -16(r1)
   fsub f0, f0, f13

   lwz r3, -4(r1)
   lwz r4, -8(r1)
   lwz r0, 4(r1)
   mtlr r0
   blr
.ENDM

.MACRO M_DISPOSELINK
# V46: now uses global mempool
#      or.. mempool as second argument !
# V49: assumes mempool(r13) exists!
P_DISPOSELINK: # cplx, pool
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R31, 8(R1)
   STW R30, 12(R1)
   OR R31, R3, R3
   OR. R30, R4, R4
   BNE _disposelink_loop
   LWZ R30, MEMPOOL(GLOB)
_disposelink_loop:
   OR. R3, R31, R31
   BLE _disposelink_end
   LWZ R31, -8(R3) # next
   ADDI R3, R3, -12
   LWZ R0, 0(R3) # totalsize
.IFDEF _AMIGAOS4_
   or r4, r30, r30
   or r5, r3, r3
   or r6, r0,r0
   lwz r3, IEXEC(GLOB)
   lwz r0, IFREEPOOLED(R3)
.ELSE # morphos
   STW R0, 0(SYS)
   STW R30, 32(SYS) # mempool
   STW R3, 36(SYS)
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, FREEPOOLED
.ENDIF
   MTCTR R0
   BCTRL
   B _disposelink_loop
_disposelink_end:
   LWZ R30, 12(R1)
   LWZ R31, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FASTDISPOSELIST
P_FASTDISPOSELIST: # list # maxlen is 63 !
   OR. R3, R3, R3
   BEQ _fdl_done # nil ptr !?!
   LHZ R8, -2(R3) # CURR!!
   ADDI R3, R3, -4 # max_curr
   SLWI R4, R8, 2 # in bytes
   ADDI R4, R4, 4 # max_curr
   ADDI R5, GLOB, MEMTABLE
   LWZX R7, R5, R4 # R7 is first mem in bucket, or NIL
   STW R7, 0(R3) # set next to the old mem
   STWX R3, R5, R4 # install our mem in bucket
   OR. R8, R8, R8
   BEQ _fdl_done
   ADDI R8, R8, -1 # sub one
   MTCTR R8
   ADDI R5, 0, 0 # load zero in R5
_fdl_clearmem:
   STWU R5, 4(R3) # clear long
   BDNZ _fdl_clearmem
_fdl_done:
   BLR # return
.ENDM

.MACRO M_KICKVERSION
P_KICKVERSION: # ver
   LWZ R4, EXECBASE(GLOB)
   LHZ R4, 20(R4)
   CMPW R4, R3
   BLT _vers_false
   ADDI R3, 0, -1
   BLR
_vers_false:
   ADDI R3, 0, 0
   BLR
.ENDM

.MACRO M_REALVAL
# 2007.06: rewrote for 64bit precision!
P_REALVAL: # str:R3 => val:F1, read:R3
   # f13: .double 0x4330000080000000
   # f12: .double 1.0
   # f11: .double 10.0
   # f10: .double -1.0 (sign)
   # F9:  .double 0.0
   # r4: temp value
   # F0: temp
   # F1: int val
   # F2: frac val
   # R8: start of string (for len comp)

   STWU R1,-16(R1)
   MFLR R0
   STW R0, 20(R1)

   OR R8, R3, R3

   BL $+40+4
   .word 0x43300000
   .word 0x80000000
   .uadouble 1.0
   .uadouble 10.0
   .uadouble 0.0
   .uadouble -1.0
   MFLR R4

   LFD F13, 0(R4)
   LFD F12, 8(R4)   # 1.0
   LFD F11, 16(R4)  # 10.0
   LFD F1, 24(R4)   # 0.0
   FMR F2, F1       # 0.0
   FMR F9, F12      # 1.0
   LFD F10, 32(R4)  # -1.0

   ADDIS R0, R0, 0x4330
   STW R0, -8(R1) # upper half

   # trim string

   ADDI R3 , R3, -1
_rval_trimit:
   LBZU R0, 1(R3)
   OR. R0, R0, R0
   BEQ _rval_ending
   CMPWI R0, 33
   BLT _rval_trimit

   # check if negative
   LBZ R4, 0(R3)
   CMPWI R4, "-"
   BEQ _rval_int_loop
   FMR F10, F9 # sign = +1.0
   ADDI R3, R3, -1
_rval_int_loop:
   LBZU R4, 1(R3)
   CMPWI R4, "0"
   BLT _rval_point
   CMPWI R4, "9"
   BGT _rval_point
   ADDI  R4, R4, -48  # char->val
   # make into float
   XORIS R4, R4, 0x8000
   STW R4, -4(R1) # lower half
   LFD F0, -8(R1)
   FSUB F0, F0, F13
   # mul old by 10 and add result
   FMADD F1, F1, F11, F0
   B _rval_int_loop
_rval_point:
   CMPWI R4, "."
   BNE _rval_ending
_rval_frac_loop:
   LBZU R4, 1(R3)
   CMPWI R4, "0"
   BLT _rval_frac_fin
   CMPWI R4, "9"
   BGT _rval_frac_fin
   ADDI  R4, R4, -48  # char->val
   # make into float
   XORIS R4, R4, 0x8000
   STW R4, -4(R1) # lower half
   LFD F0, -8(R1)
   FSUB F0, F0, F13
   # mul old by 10 and add result
   FMADD F2, F2, F11, F0
   FMUL F12, F12, F11 # divval := divval * 10.0
   B _rval_frac_loop
_rval_frac_fin:
   FDIV F2, F2, F12 # frac := frac / divval
   FADD F1, F1, F2 # add fraction
_rval_ending:
   FMUL F1, F1, F10 # apply sign
   SUB R3, R3, R8 # len read
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_INP
P_INP: # fh
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
.IFDEF _AMIGAOS4_
   or r4,r3, r3 # fh
   lwz r3, IDOS(GLOB)
   lwz r0, IREAD(r3)
   addi r5, r1, 8 # buf
   addi r6, 0, 1 # len
.ELSE # morphos
   LWZ R0, DOSBASE(GLOB)
   STW R0, 56(SYS)
   STW R3, 4(SYS)
   ADDI R0, R1, 8
   STW R0, 8(SYS)
   ADDI R0, 0, 1
   STW R0, 12(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -42
.ENDIF
   MTCTR R0
   BCTRL  # call read()
   CMPWI R3, 1
   BNE _inp_err
   LBZ R3, 8(R1)
_inp_end:
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
_inp_err:
   ADDI R3, R3, -1
   B _inp_end
.ENDM

.MACRO M_OUT
P_OUT: # fh, char
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   stb r4, 8(r1) # char
.IFDEF _AMIGAOS4_
   or r4, r3, r3 # fh
   addi r5, r1, 8 # buf
   addi r6, 0, 1 # len
   lwz r3, IDOS(GLOB)
   lwz r0, IWRITE(r3)
.ELSE # morphos
   LWZ R0, DOSBASE(GLOB)
   STW R0, 56(SYS)
   STW R3, 4(SYS)
   ADDI R0, R1, 8
   STW R0, 8(SYS)
   ADDI R0, 0, 1
   STW R0, 12(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -48
.ENDIF
   MTCTR R0
   BCTRL           # call write()
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

# note (all 4 functions): "code" may NOT raise exception!

.MACRO M_FORALL
P_FORALL: # var,list,code
   STW R14, QLARGSAVE+0(GLOB)
   STW R15, QLARGSAVE+4(GLOB)
   STW R16, QLARGSAVE+8(GLOB)
   STW R17, QLARGSAVE+12(GLOB)
   STW R18, QLARGSAVE+16(GLOB)
   MFLR R0
   STW R0, QLARGSAVE+20(GLOB)
_forall_init:
   OR R14, R3, R3
   ADDI R15, R4, -4
   OR R16, R5, R5
   ADDI R17, 0, -1
   LHZ R18, -2(R4)
   SLWI R18, R18, 2
   ADD R18, R18, R15
_forall_loop:
   CMPW R15, R18
   BEQ _forall_end
   LWZU R0, 4(R15)
   STW R0, 0(R14)
   MTCTR R16
   BCTRL
   AND R17, R17, R3
   B _forall_loop
_forall_end:
   OR R3, R17, R17
   LWZ R14, QLARGSAVE+0(GLOB)
   LWZ R15, QLARGSAVE+4(GLOB)
   LWZ R16, QLARGSAVE+8(GLOB)
   LWZ R17, QLARGSAVE+12(GLOB)
   LWZ R18, QLARGSAVE+16(GLOB)
   LWZ R0, QLARGSAVE+20(GLOB)
   MTLR R0
   BLR
.ENDM

.MACRO M_EXISTS
P_EXISTS: # var,list,code
   STW R14, QLARGSAVE+0(GLOB)
   STW R15, QLARGSAVE+4(GLOB)
   STW R16, QLARGSAVE+8(GLOB)
   STW R17, QLARGSAVE+12(GLOB)
   STW R18, QLARGSAVE+16(GLOB)
   MFLR R0
   STW R0, QLARGSAVE+20(GLOB)
_exists_init:
   OR R14, R3, R3
   ADDI R15, R4, -4
   OR R16, R5, R5
   ADDI R17, 0, 0
   LHZ R18, -2(R4)
   SLWI R18, R18, 2
   ADD R18, R18, R15
_exists_loop:
   CMPW R15, R18
   BEQ _exists_end
   LWZU R0, 4(R15)
   STW R0, 0(R14)
   MTCTR R16
   BCTRL
   OR. R17, R17, R3
   BNE _exists_end
   B _exists_loop
_exists_end:
   OR R3, R17, R17
   LWZ R14, QLARGSAVE+0(GLOB)
   LWZ R15, QLARGSAVE+4(GLOB)
   LWZ R16, QLARGSAVE+8(GLOB)
   LWZ R17, QLARGSAVE+12(GLOB)
   LWZ R18, QLARGSAVE+16(GLOB)
   LWZ R0, QLARGSAVE+20(GLOB)
   MTLR R0
   BLR
.ENDM

.MACRO M_MAPLIST
# note: listvar MUST be big enough!
P_MAPLIST: # var:R3,list:R4,listvar:R5,code:R6
   STW R14, QLARGSAVE+0(GLOB)
   STW R15, QLARGSAVE+4(GLOB)
   STW R16, QLARGSAVE+8(GLOB)
   STW R17, QLARGSAVE+12(GLOB)
   STW R18, QLARGSAVE+16(GLOB)
   STW R5, QLARGSAVE+20(GLOB)
   MFLR R0
   STW R0, QLARGSAVE+24(GLOB)
_maplist_init:
   OR R14, R3, R3
   ADDI R15, R4, -4
   ADDI R16, R5, -4
   OR R17, R6, R6
   LHZ R18, -2(R4)
   SLWI R18, R18, 2
   ADD R18, R18, R15
_maplist_loop:
   CMPW R15, R18
   BEQ _maplist_end
   LWZU R0, 4(R15)
   STW R0, 0(R14)
   MTCTR R17
   BCTRL
   STWU R3, 4(R16)
   B _maplist_loop
_maplist_end:
   LWZ R3, QLARGSAVE+20(GLOB)
   SUB R4, R16, R3
   ADDI R4, R4, 4
   SRWI R4, R4, 2
   STH R4, -2(R3)
   LWZ R14, QLARGSAVE+0(GLOB)
   LWZ R15, QLARGSAVE+4(GLOB)
   LWZ R16, QLARGSAVE+8(GLOB)
   LWZ R17, QLARGSAVE+12(GLOB)
   LWZ R18, QLARGSAVE+16(GLOB)
   LWZ R0, QLARGSAVE+24(GLOB)
   MTLR R0
   BLR
.ENDM

.MACRO M_SELECTLIST
# note: lisvar MUST be big enough!
P_SELECTLIST: # var,list,listvar,code
   STW R14, QLARGSAVE+0(GLOB)
   STW R15, QLARGSAVE+4(GLOB)
   STW R16, QLARGSAVE+8(GLOB)
   STW R17, QLARGSAVE+12(GLOB)
   STW R18, QLARGSAVE+16(GLOB)
   STW R5, QLARGSAVE+20(GLOB)
   MFLR R0
   STW R0, QLARGSAVE+24(GLOB)
_sellist_init:
   OR R14, R3, R3
   ADDI R15, R4, -4
   ADDI R16, R5, -4
   OR R17, R6, R6
   LHZ R18, -2(R4)
   SLWI R18, R18, 2
   ADD R18, R18, R15
_sellist_loop:
   CMPW R15, R18
   BEQ _sellist_end
   LWZU R0, 4(R15)
   STW R0, 0(R14)
   MTCTR R17
   BCTRL
   CMPWI R3, -1
   BNE $ + 12
   LWZ R0, 0(R15)
   STWU R0, 4(R16)
   B _sellist_loop
_sellist_end:
   LWZ R4, QLARGSAVE+20(GLOB)
   SUB R3, R16, R4
   ADDI R3, R3, 4
   SRWI R3, R3, 2
   STH R3, -2(R4)
   LWZ R14, QLARGSAVE+0(GLOB)
   LWZ R15, QLARGSAVE+4(GLOB)
   LWZ R16, QLARGSAVE+8(GLOB)
   LWZ R17, QLARGSAVE+12(GLOB)
   LWZ R18, QLARGSAVE+16(GLOB)
   LWZ R0, QLARGSAVE+24(GLOB)
   MTLR R0
   BLR
.ENDM

.MACRO M_FABS
P_FABS: # f1 => f1
   FABS F1,F1
   BLR
.ENDM

.MACRO M_FFLOOR
P_FFLOOR: # f1 => f1
   MFLR R12

   BL $ + 36
   .UADOUBLE 0.0 # F5 = 0.0
   .WORD 0x43300000 ; .WORD 0x00000000 # F3 = 0x43300000 = 252
   .WORD 0xC3300000 ; .WORD 0x00000000 # F4 = 0xC3300000 = -252
   .UADOUBLE 0.499999999999999999
   MFLR R3
   LFD F5, 0(R3)
   LFD F3, 8(R3)
   LFD F4, 16(R3)
   LFD F6, 24(R3)

   FSUB F1, F1, F6 # sub 0.5

   FCMPU 0,F1,F5
   BLT _FL_ROUND_LAB     # BRANCH IF VALUE < 0.0
   FCMPU 0,F1,F3
   BGT _FL_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F2,F1,F3   # ADD 252
   FSUB F1,F2,F3   # SUBTRACT 252
   B _FL_ROUND_EXIT
_FL_ROUND_LAB:
   FCMPU 0,F1,F4
   BLT _FL_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F2,F1,F4   # ADD -252
   FSUB F1,F2,F4   # SUBTRACT -252
_FL_ROUND_EXIT:

   # return
   MTLR R12
   BLR
.ENDM

.MACRO M_FCEIL
P_FCEIL: # f1 => f1
   MFLR R12

   BL $ + 36
   .UADOUBLE 0.0 # F5 = 0.0
   .WORD 0x43300000 ; .WORD 0x00000000 # F3 = 0x43300000 = 252
   .WORD 0xC3300000 ; .WORD 0x00000000 # F4 = 0xC3300000 = -252
   .UADOUBLE 0.499999999999999999
   MFLR R3
   LFD F5, 0(R3)
   LFD F3, 8(R3)
   LFD F4, 16(R3)
   LFD F6, 24(R3)

   FADD F1, F1, F6 # add 0.5

   FCMPU 0,F1,F5
   BLT _CE_ROUND_LAB     # BRANCH IF VALUE < 0.0
   FCMPU 0,F1,F3
   BGT _CE_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F2,F1,F3   # ADD 252
   FSUB F1,F2,F3   # SUBTRACT 252
   B _CE_ROUND_EXIT
_CE_ROUND_LAB:
   FCMPU 0,F1,F4
   BLT _CE_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F2,F1,F4   # ADD -252
   FSUB F1,F2,F4   # SUBTRACT -252
_CE_ROUND_EXIT:

   # return
   MTLR R12
   BLR
.ENDM

.MACRO M_FSIN
# Sept 2007: fixed
P_FSIN: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)

   BL _fsin_endtab
   .UADOUBLE 0.166666666666667    # 1/fac 3
   .UADOUBLE 0.008333333333333    # 1/fac 5
   .UADOUBLE 0.000198412698412    # 1/fac 7
   .WORD 0x3EC71DE3 ; .WORD 0xA556C734  # 1/fac 9 (bug in pasm doesnt handle float math exps)
   .UADOUBLE 1.570796326794897 # PI/2
   .UADOUBLE 0.636619772367581 # 2/PI
   .WORD 0x43300000  # %0100 0011 0011
   .WORD 0x80000000
   .UADOUBLE 1.0
_fsin_endtab:
   MFLR R3
   LFD F10, 0(R3)
   LFD F11, 8(R3)
   LFD F12, 16(R3)
   LFD F13, 24(R3)
   LFD F4,  32(R3) # PI/2
   LFD F6,  40(R3) # 2/PI
   LFD F8,  48(R3) # I2F subval
   LFD F0,  56(R3) # sign

   FMUL F2, F1, F6 # / (HALF PI)
   FCTIWZ F2, F2
   STFD F2, -8(R1)
   LWZ R4, -4(R1) # integer of div result
   ADDI R0, R0, -4
   AND R0, R4, R0 # compute p
   SUB R8, R4, R0 # p done
   # i back into float
   ADDIS R5, R0, 0x4330
   STW R5, -8(R1) # upper half
   XORIS R5, R4, 0x8000
   STW R5, -4(R1) # lower half
   LFD F2, -8(R1)
   FSUB F2, F2, F8
   # done convert to float (F2)
   FMUL F2, F2, F4 # * (PI/2)
   FSUB F1, F1, F2 # sub from original value (we create a modulo, +-0..HALF_PI)
   FABS F1, F1

   # now if p was odd, we reverse modulo in phase 1/3
   ANDI. R6, R8, 1 # r6 is trash
   BEQ $+4+4
   FSUB F1, F4, F1
   # change sign to negative if we are on the phase 2/3 on the scaled input
   ANDI. R6, R8, 2
   BEQ $+4+4
   FNEG F0, F0

   FMUL F2, F1, F1  # x2 := ! x*x
   FMUL F3, F2, F1  # x3 := ! x2*x
   FMUL F5, F3, F2  # x5 := ! x3*x2
   FMUL F7, F5, F2  # x7 := ! x5*x2
   FMUL F9, F7, F2  # x9 := ! x7*x2

   FNMSUB F1, F3, F10, F1 # x := ! x - (!x3/6.0)
   FMADD  F1, F5, F11, F1 # x := ! x + (!x5/120.0)
   FNMSUB F1, F7, F12, F1 # x := ! x - (!x7/5040.0)
   FMADD  F1, F9, F13, F1 # x := ! x + (!x9/362880.0)

   # fix sign
   FMUL F1, F1, F0

   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FCOS
# Sept 2007: fixed
P_FCOS: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)

   BL _fcos_endtab
   .UADOUBLE 0.5 # 1 / 2!
   .UADOUBLE 0.04166666666667 # 1 / 4!
   .UADOUBLE 0.00138888888888 # 1 / 6!
   .WORD 0x3EFA01A0 ; .WORD 0x20000000 # 1 / 8!
   .UADOUBLE 1.0
   .UADOUBLE 1.570796326794897 # PI/2
   .UADOUBLE 0.636619772367581 # 2/PI
   .WORD 0x43300000  # %0100 0011 0011
   .WORD 0x80000000
_fcos_endtab:
   MFLR R3
   LFD F10, 0(R3)
   LFD F11, 8(R3)
   LFD F12, 16(R3)
   LFD F13, 24(R3)
   LFD F4,  40(R3) # PI/2
   LFD F6,  48(R3) # 2/PI
   LFD F8,  56(R3) # I2F subval
   LFD F0,  32(R3)

   FMUL F2, F1, F6 # / (HALF PI)
   FCTIWZ F2, F2
   STFD F2, -8(R1)
   LWZ R4, -4(R1) # integer of div result
   ADDI R0, R0, -4
   AND R0, R4, R0 # compute p
   SUB R8, R4, R0 # p done
   # i back into float
   ADDIS R5, R0, 0x4330
   STW R5, -8(R1) # upper half
   XORIS R5, R4, 0x8000
   STW R5, -4(R1) # lower half
   LFD F2, -8(R1)
   FSUB F2, F2, F8
   # done convert to float (F2)
   FMUL F2, F2, F4 # * (PI/2)
   FSUB F1, F1, F2 # sub from original value (we create a modulo, +-0..HALF_PI)
   FABS F1, F1

   # now if p was odd, we reverse modulo in phase 1/3
   ANDI. R6, R8, 1 # r6 is trash
   BEQ $+4+4
   FSUB F1, F4, F1
   # change sign to negative if we are on the phase 1/2 on the scaled input
   ANDI. R7, R8, 2
   SRWI R7, R7, 1
   XOR. R0, R7, R6
   BEQ $+4+4
   FNEG F0, F0

   FMUL F2, F1, F1 # x2 := ! x*x
   FMUL F4, F2, F2 # x4 := ! x2*x2
   FMUL F6, F2, F4 # x6 := ! x2*x4
   FMUL F8, F6, F2 # x8 := ! x6*x2

   LFD F1, 32(R3) # x := 1.0

   FNMSUB F1, F2, F10, F1 # x := ! x - (!x2 / 2.0)
   FMADD  F1, F4, F11, F1 # x := ! x + (!x4 / 24.0)
   FNMSUB F1, F6, F12, F1 # x := ! x - (!x6 / 720.0)
   FMADD  F1, F8, F13, F1 # x := ! x + (!x8 / 40320.0)

   # fix sign
   FMUL F1, F1, F0

   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FTAN
P_FTAN: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -48
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FEXP
P_FEXP: # f1 => f1
   # seems to need atleast twice as many aproximations as sin/cos to be precise..
   # erors are amplified exponentially ?
   MFLR R0

   BL $ + 76
   .UADOUBLE 0.5 # 1 / 2!
   .UADOUBLE 0.166666666666667    # 1/fac 3
   .UADOUBLE 0.04166666666667 # 1 / 4!
   .UADOUBLE 0.008333333333333    # 1/fac 5
   .UADOUBLE 0.00138888888888 # 1 / 6!
   .UADOUBLE 0.000198412698412    # 1/fac 7
   .WORD 0x3EFA01A0 ; .WORD 0x20000000 # 1 / 8!
   .WORD 0x3EC71DE3 ; .WORD 0xA556C734  # 1/fac 9 (bug in pasm doesnt handle float math exps)
   .UADOUBLE 1.0
   MFLR R3

   FMUL F2, F1, F1  # x2 := ! x*x
   FMUL F3, F1, F2  # x3 := ! x*x2
   FMUL F4, F2, F2  # x4 := ! x2*x2
   FMUL F5, F1, F4  # x5 := ! x*x4
   FMUL F6, F2, F4  # x6 := ! x2*x4
   FMUL F7, F3, F4  # x7 := ! x3*x4
   FMUL F8, F4, F4  # x8 := ! x4*x4
   FMUL F9, F4, F5  # x9 := ! x4*x5

   LFD F0, 64(R3)
   FADD F1, F1, F0       # x := ! 1.0 + x
   LFD F10, 0(R3)
   FMADD F1, F2, F10, F1 # x := ! x + (!x2 / 2.0)
   LFD F10, 8(R3)
   FMADD F1, F3, F10, F1 # x := ! x + (!x3 / 6.0)
   LFD F10, 16(R3)
   FMADD F1, F4, F10, F1 # x := ! x + (!x4 / 24.0)
   LFD F10, 24(R3)
   FMADD F1, F5, F10, F1 # x := ! x + (!x5 / 120.0)
   LFD F10, 32(R3)
   FMADD F1, F6, F10, F1 # x := ! x + (!x6 / 720.0)
   LFD F10, 40(R1)
   FMADD F1, F7, F10, F1 # x := ! x + (!x7 / 5040.0)
   LFD F10, 48(R3)
   FMADD F1, F8, F10, F1 # x := ! x + (!x8 / 40320.0)
   LFD F10, 52(R3)
   FMADD F1, F9, F10, F1 # x := ! x + (!x9 / 362880.0)

   MTLR R0
   BLR
.ENDM

.MACRO M_FLOG
P_FLOG: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -84
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FPOW
P_FPOW: # f1,f2 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 8(SYS)
   STFD F2, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -90
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FSQRT
# square root routine from http:#www.mactech.com/articles/mactech/Vol.14/14.01/FastSquareRootCalc/
P_FSQRT: # x:F1
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   stfd f1, 8(r1)
   bl _fsqrt_endtable
.float   0.353553390593, 0.707106781187, 0.364434493428, 0.685994340570
.float   0.375000000000, 0.666666666667, 0.385275875186, 0.648885684523
.float   0.395284707521, 0.632455532034, 0.405046293650, 0.617213399848
.float   0.414578098794, 0.603022689156, 0.423895623945, 0.589767824620
.float   0.433012701892, 0.577350269190, 0.441941738242, 0.565685424949
.float   0.450693909433, 0.554700196225, 0.459279326772, 0.544331053952
.float   0.467707173347, 0.534522483825, 0.475985819116, 0.525225731439
.float   0.484122918276, 0.516397779494, 0.492125492126, 0.508000508001
.float   1.414213562373, 0.000000000000, 0.00000000000
_fsqrt_endtable:
   mflr r3                   # address of Table[]
   lhz   r4, 8(r1)           # load Sign(1)+Exponent(11)+Mantissa(4)
   andi.   r5,r4,0xF         # keep only Mantissa(4)
   ori   r5,r5,0x3FE0        # exponent = -1+BIAS = 1022
   sth   r5,8(r1)            # save reduced number

   rlwinm   r5,r5,3,25,28    # take 8*Mantissa(4) as index
   lfd   f1, 8(r1)           # load reduced number
   lfsux   f4,r5,r3          # load coefficient A
   lfs   f5,4(r5)            # load coefficient B
   lfs   f3,128(r3)          # load SQRT(2)
   fmr   f2,f1               # copy reduced number
   rlwinm.   r5,r4,31,18,28  # divide exponent by 2
   beq   _fsqrt2             # if (exponent == 0) then done

   fmadd   f2,f2,f5,f4       # approximation SQRT(x) = A + B*x
   andi.   r4,r4,0x10        # check if exponent even
   beq   _fsqrt1             # if (exponent even) do iteration
   fmul   f2,f2,f3           # multiply reduced number by SQRT(2)
   fadd   f1,f1,f1           # adjust exponent of original number

_fsqrt1: fadd f3,f2,f2       # 2*x
   fmul   f5,f2,f1           # x*n
   fadd   f3,f3,f3           # 4*x
   fmadd   f4,f2,f2,f1       # x*x + n
   fmul   f5,f3,f5           # 4*x*x*n
   fmul   f6,f2,f4           # denominator = x*(x*x + n)
   fmadd   f5,f4,f4,f5       # numerator = (x*x + n)*(x*x + n) +
                             # 4*x*x*n
   fdiv   f1,f5,f6           # double precision division
   andi.   r5,r5,0x7FF0      # mask exponent
   addi   r5,r5,0x1FE0       # rectify new exponent

_fsqrt2: sth r5,132(r3)      # save constant C (power of 2)
   lfd   f2,132(r3)          # load constant C
   fmul   f1,f1,f2           # multiply by C to replace exponent

   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr                       # done, the result is in f1
.ENDM

.MACRO M_FLOG10
P_FLOG10: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -126
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FATAN
P_FATAN: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -30
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FSINCOS
P_FSINCOS: # r3, f2 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STW R3, 32(SYS)
   STFD F2, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -54
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FSINH
P_FSINH: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -60
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FCOSH
# Sept 2007: back to libcall..
P_FCOSH: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -66
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FTANH
P_FTANH: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -72
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FTIEEE
P_FTIEEE: # f1 => R3
   FRSP F1, F1
   STFS F1, -4(R1)
   LWZ R3, -4(R1)
   BLR
.ENDM

.MACRO M_FFIEEE
P_FFIEEE: # R3 => f1
   STW R3, -4(R1)
   LFS F1, -4(R1)
   BLR
.ENDM

.MACRO M_FASIN
P_FASIN: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   MTLR R0
   ADDI R3, 0, -114
   BLRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_FACOS
P_FACOS: # f1 => f1
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R0, MATHTRANS(GLOB)
   STW R0, 56(SYS)
   STFD F1, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -120
   MTCTR R0
   BCTRL
   STW R3, 8(R1)
   STW R4, 12(R1)
   LFD F1, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_LEFTMOUSE
P_LEFTMOUSE: # win
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 8(R1) # save win
   LWZ R4, 82(R3)
   ANDI. R0, R4, 8 # BTST #3
   BNE _leftm_getmsg
   ORI R4, R4, 8 #BSET # 3
.IFDEF _AMIGAOS4_
   or r5, r4, r4
   or r4, r3, r3
   lwz r3, IINTUI(GLOB)
   lwz r0, IMODIFYIDCMP(r3)
.ELSE # morphos
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(SYS)
   STW R4, 0(SYS)
   STW R3, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -150 # modifyidcmp
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 8(R1)
_leftm_getmsg:
   LWZ R4, 0x56(R3) # port
.IFDEF _AMIGAOS4_
   lwz r3, IEXEC(GLOB)
   lwz r0, IGETMSG(r3)
.ELSE # morphos
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   STW R4, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -372 # getmsg
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _leftm_false
   LWZ R0, 20(R3) # class
   STW R0, 8(R1) # save it
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IREPLYMSG(r3)
.ELSE # morphos
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -378 # replymsg
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 8(R1)
   ANDI. R3, R3, 8 # BTST #3
   BEQ _leftm_false
   ADDI R3, 0, -1
_leftm_end:
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
_leftm_false:
   ADDI R3, 0, 0
   B _leftm_end
.ENDM

.MACRO M_WAITLEFTMOUSE
P_WAITLEFTMOUSE: # win
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 8(R1) # save win
   LWZ R4, 82(R3)
   ANDI. R0, R4, 8 # BTST #3
   BNE _wleftm_getmsg
   ORI R4, R4, 8 #BSET # 3
.IFDEF _AMIGAOS4_
   or r5, r4, r4
   or r4, r3, r3
   lwz r3, IDOS(GLOB)
   lwz r0, IMODIFYIDCMP(r3)
.ELSE # morphos
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(SYS)
   STW R4, 0(SYS)
   STW R3, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -150 # modifyidcmp
.ENDIF
   MTLR R0
   BLRL
   LWZ R3, 8(R1)
_wleftm_getmsg:
   LWZ R4, 0x56(R3) # port
   STW R4, 12(R1) # save it
.IFDEF _AMIGAOS4_
   lwz r3, IEXEC(GLOB)
   lwz r0, IGETMSG(r3)
.ELSE # morphos
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   STW R4, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -372 # getmsg
.ENDIF
   MTLR R0
   BLRL
   OR. R3, R3, R3
   BNE _wleftm_reply
_wleftm_wait:
.IFDEF _AMIGAOS4_
   lwz r4, 12(R1)
   lwz r3, IEXEC(GLOB)
   lwz r0, IWAITPORT(R3)
.ELSE # morphos
   LWZ R0, 12(R1) # port
   STW R0, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -384 # waitport
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r4, 12(r1)
   lwz r3, IEXEC(GLOB)
   lwz r0, IGETMSG(r3)
.ELSE # morphos
   LWZ R0, 12(R1) # port
   STW R0, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -372 # getmsg
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _wleftm_wait
_wleftm_reply:
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IREPLYMSG(r3)
.ELSE # morphos
   LWZ R0, 20(R3) # class
   STW R0, 8(R1) # save it
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -378 # replymsg
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 8(R1)
   ANDI. R3, R3, 8 # BTST #3
   BEQ _wleftm_wait
   ADDI R3, 0, -1
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_WAITIMESSAGE
P_WAITIMESSAGE: # win
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   LWZ R4, 0x56(R3) # port
   STW R4, 12(R1) # save it
.IFDEF _AMIGAOS4_
   lwz r3, IEXEC(GLOB)
   lwz r0, IGETMSG(r3)
.ELSE # morphos
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   STW R4, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -372 # getmsg
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BNE _wimsg_reply
_wimsg_wait:
.IFDEF _AMIGAOS4_
   lwz r4, 12(r1)
   lwz r3, IEXEC(GLOB)
   lwz r0, IWAITPORT(R3)
.ELSE # morphos
   LWZ R0, 12(R1) # port
   STW R0, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -384 # waitport
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r3, IEXEC(GLOB)
   lwz r4, 12(r1)
   lwz r0, IGETMSG(r3)
.ELSE # morphos
   LWZ R0, 12(R1) # port
   STW R0, 32(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -372 # getmsg
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _wimsg_wait
_wimsg_reply:
   LWZ R0, 28(R3)
   STW R0, -72(GLOB)
   LWZ R0, 24(R3)
   STW R0, -68(GLOB)
   LWZ R0, 20(R3) # class
   STW R0, 8(R1) # save it
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IREPLYMSG(r3)
.ELSE # morphos
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -378 # replymsg
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 8(R1)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_SETSTDIN
P_SETSTDIN: # newin => oldin
   LWZ R4, STDIN(GLOB)
   STW R3, STDIN(GLOB)
   OR R3, R4, R4
   BLR
.ENDM

.MACRO M_SETSTDOUT
P_SETSTDOUT: # newout => oldout
   LWZ R4, STDOUT(GLOB)
   STW R3, STDOUT(GLOB)
   OR R3, R4, R4
   BLR
.ENDM

.MACRO M_SETSTDRAST
P_SETSTDRAST: # newrast => oldrast
   LWZ R4, STDRAST(GLOB)
   STW R3, STDRAST(GLOB)
   OR R3, R4, R4
   BLR
.ENDM

.MACRO M_TEXTF_OLD
P_TEXTF_OLD: # x:R3,y:R4,fmt:R5,args:R6
   STWU R1, -32(R1)
   MFLR R0
   STW R0, 36(R1)
   STW R3, 8(R1)
   STW R4, 12(R1)
   OR R4, R6, R6
   OR R3, R5, R5
   LWZ R0, PV_STR_FMT(GLOB)
   MTCTR R0
   BCTRL # STR FMT
   STW R3, 16(R1) # save len
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 8(R1)  # x
   STW R0, 0(SYS)
   LWZ R0, 12(R1) # y
   STW R0, 4(SYS)
   LWZ R3, STDRAST(GLOB)
   OR. R3, R3, R3
   BEQ _textf_end
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, -240 # Move
   BCTRL
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R3, STACKBOTTOM(GLOB) # string
   STW R3, 32(SYS)
   LWZ R0, 16(R1) # get len
   STW R0, 0(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, -60 # Text
   BCTRL
   LWZ R3, 16(R1) # return len
_textf_end:
   LWZ R0, 36(R1)
   ADDI R1, R1, 32
   MTLR R0
   BLR
.ENDM

.MACRO M_REALF
# examples:
# ~ = round to closest integer
# RealF('9.92', 1)
# * 10.0 = 99.2
# ~      = 99.0
# / 10.0 = 9.9 => floor: 9
# - 9.0  = 0.9
# * 10.0 = 9.0 => 9
# RealF('9.99', 1)
# * 10.0 = 99.9
# ~      = 100.0
# / 10.0 = 10.0 => floor: 10
# - 10.0 = 0.0
# * 10.0 = 0.0  => 0
# RealF('3.33', 1)
# * 10.0 = 33.3
# ~      = 33.0
# / 10.0 = 3.3 => floor: 3
# - 3.0  = 0.3
# * 10.0 = 3.0 => 3
# RealF('9.99', 3)
# * 1000.0 = 9990.0
# ~        = 9990.0
# / 1000.0 = 9.99  => floor: 9
# - 9.0    = 0.99
# * 1000.0 = 990.0 => 990
# RealF('3.03', 3)
# * 1000.0 = 3030.0
# ~        = 3030.0
# / 1000.0 = 3.03 => floor: 3
# - 3.0    = 0.03
# * 1000.0 = 30.0 => 030
# RealF('0.0555', 3)
# * 1000.0 = 55.500
# ~        = 55.000
# / 1000.0 = 0.0555 => 0
# - 0.0    = 0.0555
# * 1000.0 = 55.500 => 056

# TODO: leave integers in floatregisters. gives much more precision
# 2.0.0 now takes float in F1 and fracdigs in R4 (sysv conform) !
P_REALF: # estr:R3, double:F1, fracdigs=1:R4
   STWU R1, -48(R1)
   MFLR R0
   STW R0, 52(R1)

   FMR F2, F1
   OR R5, R4, R4

   # integer:R6
   # fraction:R7
   # scratch:R4
   # fracmuldiv:F0
   # tempfloat:F1
   # tempfloat:F3
   # tempfloat:F4
   # 0x4330000000000000:F5
   # 0x3CC0000000000000:F6
   # 0.0:F7
   # ..part 2..
   # 10:R8
   # buf:R9
   # div_val:R10
   # chars:R11

   # get chars

   BL $+16
   .BYTE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 0, 0 # 12
   MFLR R11

   # get buffer

   ADDI R9, R1, 8

   # sign
   STFD F2, -8(R1)
   LWZ R4, -8(R1)
   SRAWI R4, R4, 31
   OR. R4, R4, R4
   BEQ $ + 20
   ADDI R0, 0, "-"
   STB R0, (R9)
   ADDI R9, R9, 1
   FNEG F2, F2

   # multiply float by (10^fracdigs)

   BL $+32
   .FLOAT 1.0
   .FLOAT 10.0
   .FLOAT 0.0
   .WORD 0x43300000 ; .WORD 0x00000000
   .WORD 0x3CC00000 ; .WORD 0x00000000
   MFLR R4
   LFS F0, 0(R4)
   LFS F1, 4(R4)
   LFS F7, 8(R4)
   LFD F5, 12(R4)
   LFD F6, 20(R4)

   OR. R5, R5, R5
   BEQ $ + 16
   MTCTR R5
_realf_loop1:
   FMUL F0, F0, F1
   BDNZ _realf_loop1

   FMUL F1, F2, F0

   # round temp float to nearest integer

   # F7 = 0.0
   # F5 = 0x43300000 = 252
   # F6 = 0xC3300000 = -252
   fcmpu 0,F1,F7
   blt _rf_round_lab     # branch if value < 0.0
   fcmpu 0,F1,F5
   bgt _rf_round_exit    # input was floating-point integer
   fadd F4,F1,F5   # add 252
   fsub F1,F4,F5   # subtract 252
   b _rf_round_exit
_rf_round_lab:
   fcmpu 0,F1,F6
   blt _rf_round_exit    # input was floating-point integer
   fadd F4,F1,F6   # add -252
   fsub F1,F4,F6   # subtract -252
_rf_round_exit:

   FMR F3, F1



   # divide temp float by (10^fracdigs)

   FDIV F1, F3, F0

   # make into integer without rounding and stuff in R6 => this is base

   FCTIWZ F3, F1
   STFD F3, -8(R1)
   LWZ R6, -4(R1)

   # back to float in F3

   ADDIS R0, 0, 0x4330
   STW R0, -8(R1)
   ADDI R0, 0, 0
   STW R0, -4(R1)
   LFD F4, -8(R1)

   ADDIS R0, 0, 0x4330
   STW R0, -8(R1) # UPPER HALF
   STW R6, -4(R1) # LOWER HALF
   LFD F3, -8(R1)
   FSUB F3, F3, F4

   # subtract

   FSUB F3, F1, F3

   # multiply again with fracmuldiv

   FMUL F3, F3, F0

   # make into integer in R7 = this is frac *

   FABS F3, F3
   FCTIW F3, F3
   STFD F3, -8(R1)
   LWZ R7, -4(R1)

   # create base part

   ADDI R8, 0, 10

   OR. R6, R6, R6
   BNE _realf_yo
   ADDI R0, 0, "0"
   STB R0, 0(R9)
   ADDI R9, R9, 1
   B _realf_createfrac
_realf_yo:
#   BGT _realf_yo2
#   # so it was negative ?
#   ADDI R0, 0, "-"
#   STB R0, 0(R9)
#   ADDI R9, R9, 1
#   ADDI R0, 0, 0
#   ADD R6, R0, R6 # abs
_realf_yo2:
   ADDI R10, 0, 1000
   MULLI R10, R10, 1000
   MULLI R10, R10, 1000
_realf_base:
   DIVW. R4, R6, R10
   BNE _realf_base2 # first encounter
   DIVW R10, R10, R8 # dividend / 10
   B _realf_base
_realf_base1:
   DIVW R4, R6, R10
_realf_base2:
   LBZX R0, R11, R4 # get char
   STB R0, 0(R9)     # write char
   ADDI R9, R9, 1
   MULLW R0, R10, R4  # result * dividend
   SUB R6, R6, R0  # subtract from value
   DIVW. R10, R10, R8 # divide dividend by 10
   BEQ _realf_createfrac
   B _realf_base1

   # create frac part

_realf_createfrac:

   OR. R5, R5, R5
   BEQ _realf_copy

   ADDI R0, 0, "."
   STB R0, 0(R9)
   ADDI R9, R9, 1

   # set div_val (R10) to (10^fracdigs) / 10
   ADDI R10, 0, 1
   MTCTR R5
   MULLI R10, R10, 10
   BDNZ $ - 4
   DIVW R10, R10, R8

   MTCTR R5
_realf_fracloop:
   DIVW R4, R7, R10
   LBZX R0, R11, R4 # get char
   STB R0, 0(R9)    # write char
   ADDI R9, R9, 1
   MULLW R0, R10, R4 # result * dividend
   SUB R7, R7, R0    # subtract from value
   DIVW R10, R10, R8 # divide dividend by 10
   BDNZ _realf_fracloop

   # copy to string
_realf_copy:

   ADDI R5, R1, 8
   SUB R4, R9, R5

   LHZ R0, -4(R3)
   CMPW R0, R4
   BGE $ + 8
   OR R4, R0, R0

   STH R4, -2(R3)
   ADDI R0, 0, 0
   STBX R0, R3, R4

   OR. R4, R4, R4
   BEQ _realf_done

   MTCTR R4
   ADDI R5, R5, -1
   ADDI R6, R3, -1
_realf_copyloop:
   LBZU R0, 1(R5)
   STBU R0, 1(R6)
   BDNZ _realf_copyloop

_realf_done:
   # we're done
   LWZ R0, 52(R1)
   MTLR R0
   ADDI R1, R1, 48
   BLR
.ENDM

.MACRO M_FILELENGTH
P_FILELENGTH: # name
   STWU R1, -272(R1)
   MFLR R0
   STW R0, 276(R1)
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   addi r5, 0, -2
   lwz r3, IDOS(GLOB)
   lwz r0, ILOCK(r3)
.ELSE # morphos
   LWZ R0, DOSBASE(GLOB)
   STW R0, 56(SYS)
   STW R3, 4(SYS)
   ADDI R0, 0, -2
   STW R0, 8(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -84 # Lock
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _flen_fail
   STW R3, 8(R1) # save lock
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   addi r5, r1, 12
   lwz r3, IDOS(GLOB)
   lwz r0, IEXAMINE(r3)
.ELSE # morphos
   LWZ R0, DOSBASE(GLOB)
   STW R0, 56(SYS)
   STW R3, 4(SYS)
   ADDI R0, R1, 12
   STW R0, 8(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -102 # Examine
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r4, 8(r1)
   lwz r3, IDOS(GLOB)
   lwz r0, IUNLOCK(r3)
.ELSE # morphos
   LWZ R0, DOSBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 8(R1)
   STW R0, 4(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -90 # UnLock
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 136(R1) # size
_flen_end:
   LWZ R0, 276(R1)
   MTLR R0
   ADDI R1, R1, 272
   BLR
_flen_fail:
   ADDI R3, 0, -1
   B _flen_end
.ENDM

.MACRO M_MIDSTR
P_MIDSTR: # estr:R3,str:R4,ofs:R5,len:R6
   or r7, r3, r3
   rlwinm r6, r6, 0, 1, 31
   lhz r8, -4(r3)
   cmpw r6, r8
   blt _mids_lendone # blt
   or r6, r8, r8
_mids_lendone:
   addi r3, r3, -1
   addi r4, r4, -1
   add r4, r4, r5
   cmpwi r6, 0
   beq _mids_nil # beq nil
   mtspr 9, r6
_mids_copy:
   lbzu r6, 1(r4)
   stbu r6, 1(r3)
   or. r6, r6, r6
   beq _mids_end # beq end
   bdnz _mids_copy # bdnz copy
_mids_nil:
   addi r6, r0, 0
   stbu r6, 1(r3) # nilbyte
_mids_end:
   subf r6, r7, r3 # get copylen
   sth r6, -2(r7) # set copylen
   or r3, r7, r7 # return dest
   bclr 20, 0
.ENDM

.MACRO M_PLOT
P_PLOT: # x,y,c
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 8(R1)
   STW R4, 12(R1)
   LWZ R4, STDRAST(GLOB)
   OR. R4, R4, R4
   BEQ _plot_end
.IFDEF _AMIGAOS4_
   or r5, r0, r0
   lwz r3, IGFX(GLOB)
   lwz r0, ISETAPEN(r3)
.ELSE #morphos
   STW R4, 36(SYS)
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   STW R5, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, SETAPEN # SetAPen()
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r3, IGFX(GLOB)
   lwz r4, STDRAST(GLOB)
   lwz r5, 8(r1)  # x
   lwz r6, 12(r1) # y
   lwz r0, IWRITEPIXEL(r3)
.ELSE # morphos
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R0, 8(R1)
   STW R0, 0(SYS)
   LWZ R0, 12(R1)
   STW R0, 4(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -324 # WritePixel()
.ENDIF
   MTCTR R0
   BCTRL
_plot_end:
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_LINE
P_LINE: # x1,y1,x2,y2,c
   STWU R1, -32(R1)
   MFLR R0
   STW R0, 36(R1)
   STW R3, 8(R1)
   STW R4, 12(R1)
   STW R5, 16(R1)
   STW R6, 20(R1)
   LWZ R4, STDRAST(GLOB)
   OR. R4, R4, R4
   BEQ _line_end
.IFDEF _AMIGAOS4_
   LWZ R3, IGFX(GLOB)
   OR R5, R7, R7 # c
   LWZ R0, ISETAPEN(R3)
.ELSE # morphos
   STW R4, 36(SYS)
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   STW R7, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, SETAPEN # SetAPen()
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   LWZ R3, IGFX(GLOB)
   LWZ R4, STDRAST(GLOB)
   LWZ R5, 16(R1) # x2
   LWZ R6, 20(R1) # y2
   LWZ R0, IMOVE(R3)
.ELSE # morphos
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R0, 16(R1)
   STW R0, 0(SYS)
   LWZ R0, 20(R1)
   STW R0, 4(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -240 # Move()
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   LWZ R3, IGFX(GLOB)
   LWZ R4, STDRAST(GLOB)
   LWZ R5, 8(R1) # x
   LWZ R6, 12(R1) # y
   LWZ R0, IDRAW(R3)
.ELSE # morphos
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R0, 8(R1)
   STW R0, 0(SYS)
   LWZ R0, 12(R1)
   STW R0, 4(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -246 # Draw()
.ENDIF
   MTCTR R0
   BCTRL
_line_end:
   LWZ R0, 36(R1)
   MTLR R0
   ADDI R1, R1, 32
   BLR
.ENDM

.MACRO M_RND
P_RND: # max:R3
   # val:R4
   # div:R5
   OR. R3, R3, R3
   BLT  _rnd_set
   LWZ R4, RANDSEED(GLOB)
   ADD R4, R4, R3
   ADD. R4, R4, R4
   BGT _rnd_bla
   XORI R4, R4, 0x2B41
   XORIS R4, R4, 0x1D87
_rnd_bla:
   STW R4, RANDSEED(GLOB)
   SRWI R4, R4, 1
   OR. R3, R3, R3
   BEQ _rnd_end
   DIVW R5, R4, R3
   MULLW R6, R5, R3
   SUBF R3, R6, R4 # mod
_rnd_end:
   BLR
_rnd_set:
   NEG R3, R3
   STW R3, RANDSEED(GLOB)
   BLR
.ENDM

.MACRO M_BOX
P_BOX: # x1,y1,x2,y2,c
   STWU R1, -32(R1)
   MFLR R0
   STW R0, 36(R1)
   STW R3, 8(R1)
   STW R4, 12(R1)
   STW R5, 16(R1)
   STW R6, 20(R1)
   LWZ R0, STDRAST(GLOB)
   OR. R0, R0, R0
   BEQ _box_end
.IFDEF _AMIGAOS4_
   lwz r3, IGFX(GLOB)
   or r4, r0, r0
   lwz r0, ISETAPEN(r3)
.ELSE #morphos
   STW R0, 36(SYS)
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   STW R7, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, SETAPEN # SetAPen()
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r3, IGFX(GLOB)
   lwz r4, STDRAST(GLOB)
   lwz r5, 8(r1)
   lwz r6, 12(r1)
   lwz r7, 16(r1)
   lwz r8, 20(r1)
   lwz r0, IRECTFILL(r3)
.ELSE # morphos
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R0, 8(R1)
   STW R0, 0(SYS)
   LWZ R0, 12(R1)
   STW R0, 4(SYS)
   LWZ R0, 16(R1)
   STW R0, 8(SYS)
   LWZ R0, 20(R1)
   STW R0, 12(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -306 # RectFill()
.ENDIF
   MTCTR R0
   BCTRL
_box_end:
   LWZ R0, 36(R1)
   MTLR R0
   ADDI R1, R1, 32
   BLR
.ENDM

.MACRO M_RIGHTSTR
P_RIGHTSTR: # estr:R3,estr:R4,n:R5
   ADDI R8, R3, -1
   LHZ R6, -4(R3)
   LHZ R7, -2(R4)
   SUB. R7, R7, R5
   BGE $ + 8
   ADDI R7, R7, 0
   ADD R4, R4, R7
   ADDI R4, R4, -1
   MTCTR R6
_rstr_copy:
   LBZU R0, 1(R4)
   STBU R0, 1(R8)
   OR. R0, R0, R0
   BEQ _rstr_end
   BDNZ _rstr_copy
_rstr_end:
   SUB R4, R8, R3
   ADDI R4, R4, 1
   STH R4, -2(R3)
   BLR
.ENDM

.MACRO M_SETCHUNKSIZE
P_SETCHUNKSIZE: # size
   LWZ R0, -116(R13)
   OR. R0, R0, R0
   BNE $ + 8
   STW R3, -116(R13)
   BLR
.ENDM

.MACRO M_SETCOLOUR
P_SETCOLOUR: # scr,c,r,g,b
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
.IFDEF _AMIGAOS4_
   slwi r8, r7, 24
   slwi r7, r6, 24
   slwi r6, r5, 24
   or r5, r4, r4
   addi r4, r3, 44
   lwz r3, IGFX(GLOB)
   lwz r0, ISETRGB32(r3)
.ELSE # morphos
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(R2)
   ADDI R3, R3, 44 # viewport
   STW R3, 32(R2)
   STW R4, 0(R2)
   SLWI R5, R5, 24
   SLWI R6, R6, 24
   SLWI R7, R7, 24
   STW R5, 4(R2)
   STW R6, 8(R2)
   STW R7, 12(R2)
   LWZ R0, 100(R2)
   ADDI R3, 0, -852 # SetRGB32()
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_OPENS
.SET OPENSCREENTAGLIST, -612
.SET CLOSESCREEN, -66
.SET SA_TITLE, 0x28
.SET SA_LIKEWORKBENCH, 0x47
.SET SA_DEPTH, 0x25
.SET SA_WIDTH, 0x23
.SET SA_HEIGHT, 0x24
P_OPENS: # w:R3,h:R4,d:R5,f:R6,t:R7,tags:R8=NIL
   STWU R1, -64(R1)
   MFLR R0
   STW R0, 68(R1)
   ADDIS R9, 0, 0x8000
   ORI R0, R9, SA_LIKEWORKBENCH
   STW R0, 8(R1)
   ADDI R0, 0, -1
   STW R0, 12(R1)
   ORI R0, R9, SA_WIDTH
   STW R0, 16(R1)
   STW R3, 20(R1)
   ORI R0, R9, SA_HEIGHT
   STW R0, 24(R1)
   STW R4, 28(R1)
   ORI R0, R9, SA_DEPTH
   STW R0, 32(R1)
   STW R5, 36(R1)
   ORI R0, R9, SA_TITLE
   STW R0, 40(R1)
   STW R7, 44(R1)
   ADDI R0, 0, 2 # tag_more
   CMPWI R8, 0
   BNE $ + 8
   ADDI R0, 0, 4 # tag_skip
   STW R0, 48(R1)
   STW R8, 52(R1)
   ADDI R0, 0, 0
   STW R0, 56(R1) # tag_end
.IFDEF _AMIGAOS4_
   addi r4, 0, 0
   lwz r3, IINTUI(GLOB)
   addi r5, r1, 8
   lwz r0, IOPENSCREENTAGLIST(r3)
.ELSE #morphos
   STW R0, 32(R2) # A0
   ADDI R0, R1, 8
   STW R0, 36(R2) # A1
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(R2)
   LWZ R0, 100(R2)
   ADDI R3, 0, OPENSCREENTAGLIST
.ENDIF
   MTCTR R0
   BCTRL
   OR. R3, R3, R3
   BEQ _opens_end
   ADDI R0, R3, 84
   STW R0, STDRAST(GLOB)
_opens_end:
   LWZ R0, 68(R1)
   MTLR R0
   ADDI R1, R1, 64
   BLR
.ENDM

.MACRO M_CLOSES
P_CLOSES: # scr
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
.IFDEF _AMIGAOS4_
   or r4, r3, r3
   lwz r3, IINTUI(GLOB)
   lwz r0, ICLOSESCREEN(r3)
.ELSE # morphos
   STW R3, 32(R2)
   LWZ R0, INTUITIONBASE(GLOB)
   STW R0, 56(R2)
   LWZ R0, 100(R2)
   ADDI R3, 0, CLOSESCREEN
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_OSTRCMP
P_OSTRCMP: # str1, str2, len=ALL
   RLWINM R5, R5, 0, 1, 31
   MTCTR R5
   ADDI R3, R3, -1
   ADDI R4, R4, -1
_ostrcmp_loop:
   LBZU R6, 1(R3)
   LBZU R7, 1(R4)
   CMPW R6, R7
   BGT _ostrcmp_gt
   BLT _ostrcmp_lt
   OR. R6, R6, R6
   BEQ _ostrcmp_eq
   BDNZ _ostrcmp_loop
_ostrcmp_eq:
   ADDI R3, 0, 0
   BLR
_ostrcmp_gt:
   ADDI R3, 0, -1
   BLR
_ostrcmp_lt:
   ADDI R3, 0, 1
   BLR
.ENDM

.MACRO M_NEWM
P_NEWM: # size, flags
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1) # save linkreg
   ADDI R3, R3, 8  # add memheader size
   STW R3, 8(R1) # save size
.IFDEF _AMIGAOS4_
   or r5, r4, r4
   or r4, r3, r3
   lwz r3, IEXEC(GLOB)
   lwz r0, IALLOCMEM(r3)
.ELSE #morphos
   LWZ R0, EMULCALLDIRECTOS(SYS)
   STW R3, 0(SYS) # emulhandle.dregs[0] := size
   STW R4, 4(SYS)   # emulhandle.dregs[1] := flags
   LWZ R3, EXECBASE(GLOB)
   STW R3, 56(SYS)  # emulhandle.aregs[6] := execbase
   ADDI R3, 0, ALLOCMEM
.ENDIF
   MTCTR R0
   BCTRL            # call AllocMem()
   OR. R3, R3, R3
   BEQ _newm_end
   LWZ R0, MEMLIST(GLOB)
   STW R0, 0(R3)   # set next
   STW R3, MEMLIST(GLOB) # add to list
   ADDI R0, 0, 0
   STW R0, 0(R3) # clear next (memory may be uncleared!)
   LWZ R0, 8(R1)
   STW R0, 4(R3)   # set size
   ADDI R3, R3, 8  # skip head
_newm_end:
   LWZ R0, 20(R1)
   ADDI R1, R1, 16
   MTLR R0         # restore linkreg
   BLR
.ENDM

.MACRO M_XTOD
P_XTOD: # x:F1 => d:F1
   MFLR R12
   STFD F1, -8(R1)
   BL $ + 12
   .WORD 0x43380000
   .WORD 0x00000000
   MFSPR R3, 8
   LFD F0, 0(R3)
   LWZ R3, -8(R1)
   XORIS R3, R3, 0x8
   ADDI R4, R0, -1
   ADDI R0, R0, 12
   SRW R4, R4, R0
   AND R3, R3, R4
   ORIS R3, R3, 0x4330
   STW R3, -8(R1)
   LFD F1, -8(R1)
   FSUB F1, F1, F0
   MTLR R12
   BLR
.ENDM

.MACRO M_DTOX
P_DTOX: # d:F1 => x:F1
   MFLR R12
   STFD F1, -8(R1)
   BL $ + 12
   .WORD 0x43380000
   .WORD 0x00000000
   MFSPR R3, 8
   LFD F0, 0(R3)
   LWZ R3, -8(R1)
   SRAWI R3, R3, 12
   ANDIS. R3, R3, 0xFFF8
   FADD F1, F1, F0
   STFD F1, -8(R1)
   LWZ R4, -8(R1)
   ADDI R5, R0, -1
   ADDI R0, R0, 13
   SRW R5, R5, R0
   AND R4, R4, R5 # mask away crap
   OR R4, R4, R3  # put in signbits
   STW R4, -8(R1)
   LFD F1, -8(R1)
   MTLR R12
   BLR
.ENDM

.MACRO M_DEBUGF_OLD
P_DEBUGF_OLD: # fmtstr:R3, values:R4
   # save1: 8(R1) => R31
   # save2: 12(R1) => R30
   STWU R1, -32(R1)
   MFLR R0
   STW R0, 36(R1)
   # call str fmt
   LWZ R0, PV_STR_FMT(GLOB)
   MTCTR R0
   BCTRL
   STW R3, 16(R1) # SAVE LEN
   # call rawputchar for every char in the string
   STW R31, 8(R1) # our counter reg
   STW R30, 12(R1) # strptr
   LWZ R30, STACKBOTTOM(GLOB)
   LWZ R31, 16(R1)
debugf_putloop:
   OR. R31, R31, R31
   BEQ _debugf_done
   ADDI R31, R31, -1
   LBZ R0, 0(R30)
   STW R0, 0(SYS) # char in d0
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, RAWPUTCHAR
   BCTRL
   ADDI R30, R30, 1
   B debugf_putloop
_debugf_done:
   LWZ R31, 8(R1)
   LWZ R30, 12(R1)
   LWZ R3, 16(R1) # return len
   LWZ R0, 36(R1)
   MTLR R0
   ADDI R1, R1, 32
   BLR
.ENDM

.MACRO M_COLOUR
P_COLOUR: # fg:R3,bg:R4
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R4, 8(R1)

   LWZ R4, STDRAST(GLOB)
   OR. R4, R4, R4
   BEQ _colour_end
.IFDEF _AMIGAOS4_
   or r5, r3, r3
   lwz r3, IGFX(GLOB)
   lwz r0, ISETAPEN(r3)
.ELSE # morphos
   STW R4, 36(SYS)
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   STW R3, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, SETAPEN
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r5, 8(r1)
   lwz r4, STDRAST(GLOB)
   lwz r3, IGFX(GLOB)
   lwz r0, ISETBPEN(r3)
.ELSE # morphos
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 8(R1)
   STW R0, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, SETBPEN
.ENDIF
   MTCTR R0
   BCTRL
_colour_end:
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDM

.MACRO M_PRIVATE_STRFMT_OLD
P_PRIVATE_STRFMT_OLD:
# removed..
.ENDM


## V 1.5.4 New versions of stringf etc..

.MACRO M_STRINGF_VNEW
# VNEWRAWDOFMT R3:base, R4:fmtstr,R5:putproc,R6:putdata,R7:va_list
P_STRINGF: # R3:estr, R4:fmtstr, R5:ARRAY OF VALUES
   STWU R1, -48(R1)
   MFLR R0
   STW R0, 52(R1)
   STW R3, 8(R1) # save estr for now
   ADDI R7, R1, 12 # address of va_list in R7
   ADDI R0, 0, 8
   STB R0, VA_GPR(R7) # va_list.gpr := 8
   STW R5, VA_OVERFLOW(R7) # va_list.overflow_arg_area := value array in R5
   ADDI R5, 0, 0 # putproc arg for VNewRawDoFmt is NIL
   LWZ R6, STACKBOTTOM(GLOB) # putdata arg for VNewRawDoFmt is stackbottom
   LWZ R3, EXECBASE(GLOB) # base arg for VNewRawDoFmt
   LWZ R0, VNEWRAWDOFMT(R3)
   MTCTR R0
   BCTRL # call VNewRawDoFmt
   # ok result string is on stackbottom now
   # lets copy it into estr
   LWZ R7, 8(R1) # estr
   LHZ R5, -4(R7) # estrlen
   ADDI R7, R7, -1
   LWZ R8, STACKBOTTOM(GLOB)
   ADDI R8, R8, -1
stringf2_loop:
   OR. R5, R5, R5
   BEQ stringf2_done
   LBZU R0, 1(R8)
   STBU R0, 1(R7)
   OR. R0, R0, R0
   BEQ stringf2_done
   ADDI R5, R5, -1
   B stringf2_loop
stringf2_done:
   LWZ R3, 8(R1) # estr
   SUB R4, R7, R3 # compute len copied
   STH R4, -2(R3)
   LWZ R0, 52(R1)
   MTLR R0
   ADDI R1, R1, 48
   BLR # returns estr,len
.ENDM

.MACRO M_PRINTF_VNEW
P_PRINTF: # fstr:r3, args:r4
   stwu r1, -48(r1)
   mflr r0
   stw r0, 52(r1)
   ADDI R7, R1, 12 # address of va_list in R7
   ADDI R0, 0, 8
   STB R0, VA_GPR(R7) # va_list.gpr := 8
   STW R4, VA_OVERFLOW(R7) # va_list.overflow_arg_area := value array in R4
   BL _printf2_endofputproc
   STB R4, 0(R3)
   ADDI R3, R3, 1
   BLR
_printf2_endofputproc:
   OR R4, R3, R3 # fmt arg
   MFLR R5 # putproc arg for VNewRawDoFmt
   LWZ R6, STACKBOTTOM(GLOB) # putdata arg for VNewRawDoFmt is stackbottom
   LWZ R3, EXECBASE(GLOB) # base arg for VNewRawDoFmt
   LWZ R0, VNEWRAWDOFMT(R3)
   MTCTR R0
   BCTRL # call VNewRawDoFmt
   LWZ R4, STACKBOTTOM(GLOB)
   SUB R3, R3, R4
   STW R3, 8(R1) # save len of data written to stackbottom
   # ok result string is on stackbottom now
   # lets print it with FPuts()
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   lwz r0, STACKBOTTOM(GLOB)
   stw r0, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   addi r3, 0, FPUTS
   bctrl # FPuts()  # d1:fh, d2:str
   # FPuts returns 0 for sucess, -1 for failure
   lwz r4, 8(r1) # len from rawdofmt
   nor r3, r3, r3
   and r3, r3, r4 # failure to FPuts will make length printed zero
_printf2_end:
   lwz r0, 52(r1)
   mtlr r0
   addi r1, r1, 48
   blr # returns printed len in R3
_printf2_err:
   addi r3, 0, 0
   b _printf2_end
.ENDM

.MACRO M_WRITEF_VNEW
P_WRITEF: # fstr:r3, args:r4
   stwu r1, -48(r1)
   mflr r0
   stw r0, 52(r1)
   lwz r0, STDOUT(GLOB)
   or. r0, r0, r0
   beq _writef2_getcon
_writef2_hasfh:
   ADDI R7, R1, 12 # address of va_list in R7
   ADDI R0, 0, 8
   STB R0, VA_GPR(R7) # va_list.gpr := 8
   STW R4, VA_OVERFLOW(R7) # va_list.overflow_arg_area := value array in R4
   BL _writef2_endofputproc
   STB R4, 0(R3)
   ADDI R3, R3, 1
   BLR
_writef2_endofputproc:
   OR R4, R3, R3 # fmt arg to VNew..
   MFLR R5 # putproc arg for VNewRawDoFmt
   LWZ R6, STACKBOTTOM(GLOB) # putdata arg for VNewRawDoFmt is stackbottom
   LWZ R3, EXECBASE(GLOB) # base arg for VNewRawDoFmt
   LWZ R0, VNEWRAWDOFMT(R3)
   MTCTR R0
   BCTRL # call VNewRawDoFmt
   LWZ R4, STACKBOTTOM(GLOB)
   SUB R3, R3, R4
   STW R3, 8(R1) # save lenof data written to stackbottom
   # ok result string is on stackbottom now
   # lets print it with Write()
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   lwz r0, STACKBOTTOM(GLOB)
   stw r0, 8(SYS) # d2
   stw r3, 12(SYS) # d3
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   addi r3, 0, WRITE
   bctrl # Write()  # d1:fh, d2:str, d3:len
   # Write returns len, or -1 for failure
   lwz r4, 8(r1) # len from rawdofmt
   cmpw 0, r3, r4
   bne _writef2_err
_writef2_end:
   lwz r0, 52(r1)
   mtlr r0
   addi r1, r1, 48
   blr # returns written len in R3
_writef2_getcon:
   stw r3, 8(r1)
   stw r4, 12(r1)
   bl $+28
   .byte "CON:///100/Output/CLOSE",0 # 24 bytes
   mflr r3
   stw r3, 4(SYS) # d1
   addi r3, r0, 1006
   stw r3, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -30
   lwz r0, EMULCALLDIRECTOS(SYS)
   mtctr r0
   bctrl # Open()
   stw r3, CONOUT(GLOB)
   stw r3, STDOUT(GLOB)
   or. r3, r3, r3
   lwz r3, 8(r1)
   lwz r4, 12(r1)
   bne _writef2_hasfh
   addi r3, r0, 20
   stw r3, CLIRETURNVAL(GLOB)
   lwz r0, PPCCLEANUP(GLOB)
   mtlr r0
   blr # exit
_writef2_err:
   addi r3, 0, 0
   b _writef2_end
.ENDM

.MACRO M_TEXTF_VNEW
P_TEXTF: # x:R3,y:R4,fmt:R5,args:R6
   STWU R1, -48(R1)
   MFLR R0
   STW R0, 52(R1)
   STW R3, 36(R1) # x
   STW R4, 40(R1) # y
   # call vnewrawdofmt
   ADDI R7, R1, 8 # address of va_list in R7
   ADDI R0, 0, 8
   STB R0, VA_GPR(R7) # va_list.gpr := 8
   STW R6, VA_OVERFLOW(R7) # va_list.overflow_arg_area := value array in R6
   OR R4, R5, R5 # fmt to arg
   BL _textf2_endofputproc
   STB R4, 0(R3)
   ADDI R3, R3, 1
   BLR
_textf2_endofputproc:
   MFLR R5 # putproc arg for VNewRawDoFmt
   LWZ R6, STACKBOTTOM(GLOB) # putdata arg for VNewRawDoFmt is stackbottom
   LWZ R3, EXECBASE(GLOB) # base arg for VNewRawDoFmt
   LWZ R0, VNEWRAWDOFMT(R3)
   MTCTR R0
   BCTRL # call VNewRawDoFmt
   LWZ R4, STACKBOTTOM(GLOB)
   SUB R3, R3, R4
   STW R3, 44(R1)  # save len
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 36(R1)  # x
   STW R0, 0(SYS)
   LWZ R0, 40(R1)  # y
   STW R0, 4(SYS)
   LWZ R3, STDRAST(GLOB)
   OR. R3, R3, R3
   BEQ _textf2_end
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, -240 # Move
   BCTRL
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R3, STACKBOTTOM(GLOB) # string
   STW R3, 32(SYS)
   LWZ R0, 44(R1)  # get len
   STW R0, 0(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, -60 # Text
   BCTRL
   LWZ R3, 44(R1)  # return len
_textf2_end:
   LWZ R0, 52(R1)
   ADDI R1, R1, 48
   MTLR R0
   BLR
.ENDM

.MACRO M_DEBUGF_VNEW
P_DEBUGF: # fmtstr:R3, values:R4
   STWU R1, -48(R1)
   MFLR R0
   STW R0, 52(R1)
   BL _debugf2_doit
_debugf2_putproc: # R4:char
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R4, 0(SYS) # char in d0
   LWZ R0, 4(0) # execbase
   STW R0, 56(SYS)
   LWZ R0, 100(SYS)
   MTCTR R0
   ADDI R3, 0, RAWPUTCHAR
   BCTRL
   LWZ R0, 20(R1)
   ADDI R1, R1, 16
   MTLR R0
   BLR
_debugf2_doit:
   MFLR R5 # address of putproc as arg in R5
   ADDI R7, R1, 8 # address of va_list in R7
   ADDI R0, 0, 8
   STB R0, VA_GPR(R7) # va_list.gpr := 8
   STW R4, VA_OVERFLOW(R7) # va_list.overflow_arg_area := value array in R4
   OR R4, R3, R3 # fmt to arg
   LWZ R3, 4(0) # execbase arg for VNewRawDoFmt
   LWZ R0, VNEWRAWDOFMT(R3)
   MTCTR R0
   BCTRL # call VNewRawDoFmt
   LWZ R0, 52(R1)
   ADDI R1, R1, 48
   MTLR R0
   BLR
.ENDM

.MACRO M_STRINGF
# sept 2007, back to using non bugged stringformatting (rawdofmt)
P_STRINGF: # R3:estr, R4:fmtstr, R5:ARRAY OF VALUES
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 8(R1) # save estr for now
.IFDEF _AMIGAOS4_
   addi r6, 0, 0              # putchproc
   lwz r7, STACKBOTTOM(GLOB)  # putchdata
   lwz r3, IEXEC(GLOB)
   lwz r0, IRAWDOFMT(r3)
.ELSE # morphos
   STW R4, 32(SYS) # a0=fstr
   STW R5, 36(SYS) # a1=values
   LWZ R0, STACKBOTTOM(GLOB) # putdata arg for RawDoFmt is stackbottom
   STW R0, 44(SYS) # a3=stackbottom
   ADDI R0, 0, 0
   STW R0, 40(SYS) # a2=NIL
   LWZ R0, EXECBASE(GLOB)
   STW R0, 56(SYS) # a6=execbase
   ADDI R3, 0, RAWDOFMT
   LWZ R0, EMULCALLDIRECTOS(SYS)
.ENDIF
   MTCTR R0
   BCTRL # call RawDoFmt
   # ok result string is on stackbottom now
   # lets copy it into estr
   LWZ R7, 8(R1) # estr
   LHZ R5, -4(R7) # estrlen
   ADDI R7, R7, -1
   LWZ R8, STACKBOTTOM(GLOB)
   ADDI R8, R8, -1
stringf2_loop:
   OR. R5, R5, R5
   BEQ stringf2_done
   LBZU R0, 1(R8)
   STBU R0, 1(R7)
   OR. R0, R0, R0
   BEQ stringf2_done
   ADDI R5, R5, -1
   B stringf2_loop
stringf2_done:
   LWZ R3, 8(R1) # estr
   SUB R4, R7, R3 # compute len copied
   STH R4, -2(R3)
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR # returns estr,len
.ENDM

# HMM! why not use VFPrintf here ? much simpler, * 2.0.0 DONE*
.MACRO M_PRINTF
P_PRINTF:  # fstr:r3, args:r4
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
.IFDEF _AMIGAOS4_
   or r6, r4, r4
   lwz r4, STDOUT(GLOB)
   or r5, r3, r3
   lwz r3, IDOS(GLOB)
   lwz r0, IVFPRINTF(R3)
.ELSE # morphos
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   stw r3, 8(SYS) # d2
   stw r4, 12(SYS) # d3
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   lwz r0, EMULCALLDIRECTOS(SYS)
   addi r3, 0, VFPRINTF
.ENDIF
   mtctr r0
   bctrl # FVPRINTF()
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr # returns printed len in R3
.ENDM

.MACRO M_WRITEF
P_WRITEF:  # fstr:r3, args:r4
   stwu r1, -16(r1)
   mflr r0
   stw r0, 20(r1)
   lwz r0, STDOUT(GLOB)
   or. r0, r0, r0
   beq _writef2_getcon
_writef2_hasfh:
.IFDEF _AMIGAOS4_
   lwz r7, STACKBOTTOM(GLOB)  # PutChData
   addi r6, 0, 0              # PuChProc
   or r5, r4, r4              # datastream
   or r4, r3, r3              # fmtstr
   lwz r3, IEXEC(GLOB)
   lwz r0, IRAWDOFMT(r3)
.ELSE # morphos
   STW R3, 32(SYS) # a0=fmtstr
   STW R4, 36(SYS) # a1=datastream
   ADDI R0, 0, 0
   STW R0, 40(SYS) # a2=0
   LWZ R0, STACKBOTTOM(GLOB) # putdata arg for RawDoFmt is stackbottom
   STW R0, 44(SYS) # a3=stackbottom
   LWZ R0, EXECBASE(GLOB) # base arg for RawDoFmt
   STW R0, 56(SYS) # a6=execbase
   ADDI R3, 0, RAWDOFMT
   LWZ R0, EMULCALLDIRECTOS(SYS)
.ENDIF
   MTCTR R0
   BCTRL # call RawDoFmt
   # bug in mos 1.4.5, returnvalue of rawdofmt is bogus
   # we have to compute len, luckily rawdofmt puts nilbyte at end so we can find it
   # otoh, this means we cannot put nilbytes in output..
   LWZ R3, STACKBOTTOM(GLOB)
   ADDI R4, R3, -1
_writef_get_len:
   LBZU R0, 1(R4)
   OR. R0, R0, R0
   BNE _writef_get_len
   SUB R3, R4, R3
   STW R3, 8(R1) # save lenof data written to stackbottom
   # ok result string is on stackbottom now
   # lets print it with Write()
.IFDEF _AMIGAOS4_
   lwz r4,STDOUT(GLOB)
   lwz r5, STACKBOTTOM(GLOB)
   or r6, r3, r3
   lwz r3, IDOS(GLOB)
   lwz r0, IWRITE(r3)
.ELSE # morphos
   lwz r0, STDOUT(GLOB)
   stw r0, 4(SYS) # d1
   lwz r0, STACKBOTTOM(GLOB)
   stw r0, 8(SYS) # d2
   stw r3, 12(SYS) # d3
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   lwz r0, EMULCALLDIRECTOS(SYS)
   addi r3, 0, WRITE
.ENDIF
   mtctr r0
   bctrl # Write()  # d1:fh, d2:str, d3:len
   # Write returns len, or -1 for failure
   lwz r4, 8(r1) # len from rawdofmt
   cmpw 0, r3, r4
   bne _writef2_err
_writef2_end:
   lwz r0, 20(r1)
   mtlr r0
   addi r1, r1, 16
   blr # returns written len in R3
_writef2_getcon:
   stw r3, 8(r1)
   stw r4, 12(r1)
   bl $+28
   .byte "CON:///100/Output/CLOSE",0 # 24 bytes
   mflr r3
.IFDEF _AMIGAOS4_
   or r4, r3, r3     # name
   addi r5, 0, 1006  # mode
   lwz r3, IDOS(GLOB)
   lwz r0, IOPEN(r3)
.ELSE # morphos
   stw r3, 4(SYS) # d1
   addi r3, r0, 1006
   stw r3, 8(SYS) # d2
   lwz r0, DOSBASE(GLOB)
   stw r0, 56(SYS) # a6
   addi r3, r0, -30
   lwz r0, EMULCALLDIRECTOS(SYS)
.ENDIF
   mtctr r0
   bctrl # Open()
   stw r3, CONOUT(GLOB)
   stw r3, STDOUT(GLOB)
   or. r3, r3, r3
   lwz r3, 8(r1)
   lwz r4, 12(r1)
   bne _writef2_hasfh
   addi r3, r0, 20
   stw r3, CLIRETURNVAL(GLOB)
   lwz r0, PPCCLEANUP(GLOB)
   mtlr r0
   blr # exit
_writef2_err:
   addi r3, 0, 0
   b _writef2_end
.ENDM

.MACRO M_TEXTF
P_TEXTF: # x:R3,y:R4,fmt:R5,args:R6
   STWU R1, -32(R1)
   MFLR R0
   STW R0, 36(R1)
   STW R3, 8(R1) # x
   STW R4, 12(R1) # y
.IFDEF _AMIGAOS4_
   lwz r7, STACKBOTTOM(GLOB)  # PutChData
   or r4, r5, r5              # fmtstr
   or r5, r6, r6              # datastream
   addi r6, 0, 0              # PuChProc
   lwz r3, IEXEC(GLOB)
   lwz r0, IRAWDOFMT(r3)
.ELSE # morphos
   STW R5, 32(SYS) # a0=fmtstr
   STW R6, 36(SYS) # a1=datastream
   ADDI R0, 0, 0
   STW R0, 40(SYS) # a2=0
   LWZ R0, STACKBOTTOM(GLOB) # putdata arg for RawDoFmt is stackbottom
   STW R0, 44(SYS) # a3=stackbottom
   LWZ R0, EXECBASE(GLOB) # base arg for RawDoFmt
   STW R0, 56(SYS) # a6=execbase
   ADDI R3, 0, RAWDOFMT
   LWZ R0, EMULCALLDIRECTOS(SYS)
.ENDIF
   MTCTR R0
   BCTRL # call RawDoFmt
   # bug in mos 1.4.5, returnvalue of rawdofmt is bogus
   # we have to compute len, luckily rawdofmt puts nilbyte at end so we can find it
   # otoh, this means we cannot put nilbytes in output..
   LWZ R3, STACKBOTTOM(GLOB)
   ADDI R4, R3, -1
_textf_get_len:
   LBZU R0, 1(R4)
   OR. R0, R0, R0
   BNE _textf_get_len
   SUB R3, R4, R3
   STW R3, 16(R1)  # save len
.IFDEF _AMIGAOS4_
   lwz r3, IGFX(GLOB)
   lwz r4, STDRAST(GLOB)
   lwz r5, 8(r1)
   lwz r6, 12(r1)
   lwz r0, IMOVE(r3)
.ELSE # morphos
   LWZ R0, GFXBASE(GLOB)
   STW R0, 56(SYS)
   LWZ R0, 8(R1)  # x
   STW R0, 0(SYS)
   LWZ R0, 12(R1)  # y
   STW R0, 4(SYS)
   LWZ R3, STDRAST(GLOB)
   OR. R3, R3, R3
   BEQ _textf2_end
   STW R3, 36(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -240 # Move
.ENDIF
   MTCTR R0
   BCTRL
.IFDEF _AMIGAOS4_
   lwz r3, IGFX(GLOB)
   lwz r4, STDRAST(GLOB)
   lwz r5, STACKBOTTOM(GLOB)
   lwz r6, 16(r1)
   lwz r0, ITEXT(r3)
.ELSE # morphos
   LWZ R0, STDRAST(GLOB)
   STW R0, 36(SYS)
   LWZ R3, STACKBOTTOM(GLOB) # string
   STW R3, 32(SYS)
   LWZ R0, 16(R1)  # get len
   STW R0, 0(SYS)
   LWZ R0, 100(SYS)
   ADDI R3, 0, -60 # Text
.ENDIF
   MTCTR R0
   BCTRL
   LWZ R3, 44(R1)  # return len
_textf2_end:
   LWZ R0, 36(R1)
   ADDI R1, R1, 32
   MTLR R0
   BLR
.ENDM

.MACRO M_DEBUGF
P_DEBUGF: # R3:fmtstr, R4:ARRAY OF VALUES
.IFDEF _AMIGAOS4_
   STWU R1, -96(R1)
   MFLR R0
   STW R0, 100(R1)
   or r5, r4, r4
   or r4, r3, r3
   lwz r0, 0(r5)
   stw r0, 8(r1)
   lwz r0, 4(r5)
   stw r0, 12(r1)
   lwz r0, 8(r5)
   stw r0, 16(r1)
   lwz r0, 12(r5)
   stw r0, 20(r1)
   lwz r3, IEXEC(GLOB)
   lwz r0, IDEBUGPRINTF(r3)
   mtctr r0
   bctrl
   LWZ R0, 100(R1)
   MTLR R0
   ADDI R1, R1, 96
   BLR
.ELSE # morphos
   STWU R1, -16(R1)
   MFLR R0
   STW R0, 20(R1)
   STW R3, 32(SYS) # a0=fstr
   STW R4, 36(SYS) # a1=values
   ADDI R0, 0, 1
   STW R0, 40(SYS) # a2=1
   LWZ R0, 4(0) # execbase
   STW R0, 56(SYS) # a6=execbase
   ADDI R3, 0, RAWDOFMT
   LWZ R0, EMULCALLDIRECTOS(SYS)
   MTCTR R0
   BCTRL # call RawDoFmt
   LWZ R0, 20(R1)
   MTLR R0
   ADDI R1, R1, 16
   BLR
.ENDIF
.ENDM


.MACRO M_PRIVATE_SHL64
P_PRIVATE_SHL64:
   # if shift = 0
   #    return
   # elseif shift > 31
   #    copy lower into upper
   #    clear lower
   #    shift upper by shift-32
   # else
   #    rotate lower
   #    shift upper
   #    extract "shifted out" bits from lower and OR into upper
   #    AND away "shifted out" bits in lower
   # endif

   STW R3, -32(R1)
   STW R4, -36(R1)

   OR. R3, R12, R12  # shift
   BEQ _shl64_done

   CMPWI 0, R3, 31
   BGT _shl64_bigshift

   STW R5, -40(R1)
   STW R6, -44(R1)

   LWZ R4, 0(R11)
   SLW R4, R4, R3       # shift upper
   LWZ R5, 4(R11)
   ROTLW R5, R5, R3     # rotate lower
   ADDI R6, 0, 1        # make mask
   SLW R6, R6, R3       # ..
   ADDI R6, R6, -1      # mask done
   AND R0, R5, R6       # extract
   OR R4, R4, R0        # OR into upper
   ANDC R5, R5, R0      # AND away with inverted mask
   STW R4, 0(R11)       # store upper
   STW R5, 4(R11)       # store lower

   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   LWZ R5, -40(R1)
   LWZ R6, -44(R1)
   BLR
_shl64_bigshift:
   LWZ R4, 4(R11)       # load lower
   ADDI R0, 0, 0        # clear lower
   STW R0, 4(R11)       # store lower
   ADDI R3, R3, -32
   SLW R0, R4, R3       # shift lower as upper
   STW R0, 0(R11)       # store upper
_shl64_done:
   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   BLR

.ENDM

.MACRO M_PRIVATE_SHR64
P_PRIVATE_SHR64:
   # if shift = 0
   #    return
   # elseif shift > 31
   #    copy upper into lower
   #    clear upper
   #    shift lower by shift-32
   # else
   #    rotate upper
   #    shift lower
   #    extract "shifted out" bits from upper and OR into lower
   #    AND away "shifted out" bits in upper
   # endif

   STW R3, -32(R1)
   STW R4, -36(R1)

   OR. R3, R12, R12  # shift
   BEQ _shr64_done

   CMPWI 0, R3, 31
   BGT _shr64_bigshift

   STW R5, -40(R1)
   STW R6, -44(R1)

   LWZ R4, 0(R11)
   ADDI R0, 0, 32
   SUB R0, R0, R3       # make rotation go right  (no rotrw ?!)
   ROTLW R4, R4, R0     # rotate upper
   LWZ R5, 4(R11)
   SRW R5, R5, R3       # shift lower
   ADDIS R6, 0, 0x8000  # make mask
   SRAW R6, R6, R3      # mask done
   AND R0, R4, R6       # extract
   OR R5, R5, R0        # OR into lower
   ANDC R4, R4, R0      # AND away with inverted mask
   STW R4, 0(R11)       # store upper
   STW R5, 4(R11)       # store lower

   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   LWZ R5, -40(R1)
   LWZ R6, -44(R1)
   BLR
_shr64_bigshift:
   LWZ R4, 0(R11)       # load upper
   ADDI R0, 0, 0        #
   STW R0, 0(R11)       # store cleared upper
   ADDI R3 ,R3, -32
   LWZ R0, 4(R11)
   SRW R0, R4, R3       # shift upper as lower
   STW R0, 4(R11)       # store lower
_shr64_done:
   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   BLR
.ENDM

.MACRO M_PRIVATE_ASR64
P_PRIVATE_ASR64:     # UNDER CONSTRUCTION

.ENDM

# used by P_PRIVATE_DIV64 and P_MOD64
.MACRO DIV64_MACRO
    # currently no support for signed values !

    # (R3:R4) = (R3:R4) / (R5:R6) (64b) = (64b / 64b)
    # quo dvd dvs
    #
    # Remainder is returned in R5:R6.
    #
    # Code comment notation:
    # msw = most-significant (high-order) word, i.e. bits 0..31
    # lsw = least-significant (low-order) word, i.e. bits 32..63
    # LZ = Leading Zeroes
    # SD = Significant Digits
    #
    # R3:R4 = dvd (input dividend); quo (output quotient)
    # R5:R6 = dvs (input divisor); rem (output remainder)
    #
    # R7:R8 = tmp

    # count the number of leading 0s in the dividend
    cmpwi     cr0,R3,0        # dvd.msw == 0?
    cntlzw     R0,R3          # R0 = dvd.msw.LZ
    cntlzw     R9,R4          # R9 = dvd.lsw.LZ
    bne     cr0,_div64_lab1\@ # if(dvd.msw == 0) dvd.LZ = dvd.msw.LZ
    addi     R0,R9,32         # dvd.LZ = dvd.lsw.LZ + 32

_div64_lab1\@:
    # count the number of leading 0s in the divisor
    cmpwi     cr0,R5,0        # dvd.msw == 0?
    cntlzw     R9,R5          # R9 = dvs.msw.LZ
    cntlzw     R10,R6         # R10 = dvs.lsw.LZ
    bne     cr0,_div64_lab2\@ # if(dvs.msw == 0) dvs.LZ = dvs.msw.LZ
    addi     R9,R10,32        # dvs.LZ = dvs.lsw.LZ + 32

_div64_lab2\@:
    # determine shift amounts to minimize the number of iterations
    cmpw     cr0,R0,R9        # compare dvd.LZ to dvs.LZ
    subfic     R10,R0,64      # R10 = dvd.SD
    bgt     cr0,_div64_lab9\@ # if(dvs > dvd) quotient = 0
    addi     R9,R9,1          # ++dvs.LZ (or --dvs.SD)
    subfic     R9,R9,64       # R9 = dvs.SD
    add     R0,R0,R9          # (dvd.LZ + dvs.SD) = left shift of dvd for
                              # initial dvd
    subf     R9,R9,R10        # (dvd.SD - dvs.SD) = right shift of dvd for
                              # initial tmp
    mtctr     R9              # number of iterations = dvd.SD - dvs.SD

    # R7:R8 = R3:R4 >> R9
    cmpwi     cr0,R9,32       # compare R9 to 32
    addi     R7,R9,-32
    blt     cr0,_div64_lab3\@ # if(R9 < 32) jump to lab3
    srw     R8,R3,R7          # tmp.lsw = dvd.msw >> (R9 - 32)
    li     R7,0               # tmp.msw = 0
    b     _div64_lab4\@
_div64_lab3\@:
    srw     R8,R4,R9          # R8 = dvd.lsw >> R9
    subfic  R7,R9,32
    slw     R7,R3,R7          # R7 = dvd.msw << 32 - R9
    or      R8,R8,R7          # tmp.lsw = R8 | R7
    srw     R7,R3,R9          # tmp.msw = dvd.msw >> R9

_div64_lab4\@:
    # R3:R4 = R3:R4 << R0
    cmpwi     cr0,R0,32       # compare R0 to 32
    addic     R9,R0,-32
    blt     cr0,_div64_lab5\@ # if(R0 < 32) jump to lab5
    slw     R3,R4,R9          # dvd.msw = dvd.lsw << R9
    li     R4,0               # dvd.lsw = 0
    b     _div64_lab6\@
_div64_lab5\@:
    slw     R3,R3,R0       # R3 = dvd.msw << R0
    subfic     R9,R0,32
    srw     R9,R4,R9       # R9 = dvd.lsw >> 32 - R0
    or     R3,R3,R9        # dvd.msw = R3 | R9
    slw     R4,R4,R0       # dvd.lsw = dvd.lsw << R0

_div64_lab6\@:
    # restoring division shift and subtract loop
    li     R10,-1          # R10 = -1
    addic     R7,R7,0      # clear carry bit before loop starts
_div64_lab7\@:
    # tmp:dvd is considered one large register
    # each portion is shifted left 1 bit by adding it to itself
    # adde sums the carry from the previous and creates a new carry
    adde     R4,R4,R4         # shift dvd.lsw left 1 bit
    adde     R3,R3,R3         # shift dvd.msw to left 1 bit
    adde     R8,R8,R8         # shift tmp.lsw to left 1 bit
    adde     R7,R7,R7         # shift tmp.msw to left 1 bit
    subfc     R0,R6,R8        # tmp.lsw - dvs.lsw
    subfe.     R9,R5,R7       # tmp.msw - dvs.msw
    blt     cr0,_div64_lab8\@ # if(result < 0) clear carry bit
    mr     R8,R0              # move lsw
    mr     R7,R9              # move msw
    addic     R0,R10,1        # set carry bit
_div64_lab8\@:
    bdnz     _div64_lab7\@

    # write quotient and remainder
    adde     R4,R4,R4     # quo.lsw (lsb = CA)
    adde     R3,R3,R3     # quo.msw (lsb from lsw)
    mr     R6,R8     # rem.lsw
    mr     R5,R7     # rem.msw
    B _div64_end\@   #blr              # return
_div64_lab9\@:
    # Quotient is 0 (dvs > dvd)
    mr     R6,R4     # rmd.lsw = dvd.lsw
    mr     R5,R3     # rmd.msw = dvd.msw
    li     R4,0      # dvd.lsw = 0
    li     R3,0      # dvd.msw = 0

_div64_end\@:

.ENDM

.MACRO M_PRIVATE_DIV64
P_PRIVATE_DIV64:
   STW R3, -32(R1)
   STW R4, -36(R1)
   STW R5, -40(R1)
   STW R6, -44(R1)
   STW R7, -48(R1)
   STW R8, -56(R1)
   STW R9, -60(R1)
   STW R10, -64(R1)

   LWZ R3, 0(R11)   #h1
   LWZ R4, 4(R11)   #l1
   LWZ R5, 0(R12)   #h2
   LWZ R6, 4(R12)   #l2

   DIV64_MACRO

   STW R3, 0(R11)  # return high
   STW R4, 4(R11)  # return low

   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   LWZ R5, -40(R1)
   LWZ R6, -44(R1)
   LWZ R7, -48(R1)
   LWZ R8, -56(R1)
   LWZ R9, -60(R1)
   LWZ R10, -64(R1)

   BLR
.ENDM

.MACRO M_MOD64
P_MOD64: # x:R3-R4 y:R5-R6

   DIV64_MACRO

   # swap returns
   OR R7, R3, R3
   OR R8, R4, R4
   OR R3, R5, R5
   OR R4, R6, R6
   OR R5, R7, R7
   OR R6, R8, R8

   BLR

.ENDM




.MACRO M_ABS64
P_ABS64: # R3:R4 => R3:R4
   # if negative then negate by inverting and adding 1
   ANDIS. R0, R3, 0x8000
   BEQ _abs64_done
   NOT R3, R3
   NOT R4, R4
   ADDIC R4, R4, 1
   ADDZE R3, R3
_abs64_done:
   BLR
.ENDM

.MACRO M_PRIVATE_F2D64
P_PRIVATE_F2D64:

   STW R3, -32(R1)
   STW R4, -36(R1)
   STW R5, -40(R1)
   STW R6, -44(R1)
   STFD F1, -52(R1)

   MFLR R6

   BL $ + 12
   .WORD 0x43380000
   .WORD 0x00000000
   MFSPR R3, 8
   LFD F0, 0(R3)
   LWZ R3, 0(R11)
   SRAWI R3, R3, 12
   ANDIS. R3, R3, 0xFFF8
   FADD F1, F1, F0
   STFD F1, 0(R11)
   LWZ R4, 0(R11)
   ADDI R5, R0, -1
   ADDI R0, R0, 13
   SRW R5, R5, R0
   AND R4, R4, R5 # mask away crap
   OR R4, R4, R3  # put in signbits
   STW R4, 0(R11)

   MTLR R6

   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   LWZ R5, -40(R1)
   LWZ R6, -44(R1)
   LFD F1, -52(R1)

   BLR

.ENDM

.MACRO M_PRIVATE_D642F
P_PRIVATE_D642F:

   STW R3, -32(R1)
   STW R4, -36(R1)
   STW R5, -40(R1)
   STFD F1, -48(R1)

   MFLR R5

   BL $ + 12
   .WORD 0x43380000
   .WORD 0x00000000
   MFSPR R3, 8
   LFD F0, 0(R3)
   LWZ R3, 0(R11)
   XORIS R3, R3, 0x8
   ADDI R4, R0, -1
   ADDI R0, R0, 12
   SRW R4, R4, R0
   AND R3, R3, R4
   ORIS R3, R3, 0x4330
   STW R3, 0(R11)
   LFD F1, 0(R11)
   FSUB F1, F1, F0
   STFD F1, 0(R11)

   MTLR R5

   LWZ R3, -32(R1)
   LWZ R4, -36(R1)
   LWZ R5, -40(R1)
   LFD F1, -48(R1)

   BLR

.ENDM


#############################################################################
#############################################################################

M_FASTNEW
M_FASTDISPOSE
M_NEW
M_NEWR
M_DISPOSE
M_STRINGF_OLD
M_RAISE
M_THROW
M_RETHROW
M_STRCOPY
M_STRADD
M_STRCMP
M_STRLEN
M_TRIMSTR
M_ASTRCOPY
M_RNDQ
M_BOUNDS
M_MIN
M_MAX
M_ABS
M_SIGN
M_STRING
M_LIST
M_WRITEF_OLD
M_PRINTF_OLD
M_CLEANUP
M_FREESTACK
M_INSTR
M_UPPERSTR
M_LOWERSTR
M_OPENW
M_READSTR
M_CLOSEW
M_LISTCOPY
M_LISTADD
M_LISTCMP
M_VAL
M_CTRLC
M_MOD
M_FORWARD
M_PRIVATE_I2F
M_DISPOSELINK
M_FASTDISPOSELIST
M_KICKVERSION
M_REALVAL
M_INP
M_OUT
M_FORALL
M_EXISTS
M_MAPLIST
M_SELECTLIST
M_FABS
M_FFLOOR
M_FCEIL
M_FSIN
M_FCOS
M_FTAN
M_FEXP
M_FLOG
M_FPOW
M_FSQRT
M_FLOG10
M_FATAN
M_FSINCOS
M_FSINH
M_FCOSH
M_FTANH
M_FTIEEE
M_FFIEEE
M_FASIN
M_FACOS
M_LEFTMOUSE
M_WAITLEFTMOUSE
M_WAITIMESSAGE
M_SETSTDIN
M_SETSTDOUT
M_SETSTDRAST
M_TEXTF_OLD
M_REALF
M_FILELENGTH
M_MIDSTR
M_PLOT
M_LINE
M_RND
M_BOX
M_RIGHTSTR
M_SETCHUNKSIZE
M_SETCOLOUR
M_OPENS
M_CLOSES
M_OSTRCMP
M_NEWM
M_XTOD
M_DTOX
M_DEBUGF_OLD
M_COLOUR
M_PRIVATE_STRFMT_OLD
.ifndef USEVNEWRAWDOFMT
M_STRINGF
M_PRINTF
M_WRITEF
M_TEXTF
M_DEBUGF
.else
M_STRINGF_VNEW
M_PRINTF_VNEW
M_WRITEF_VNEW
M_TEXTF_VNEW
M_DEBUGF_VNEW
.endif

M_PRIVATE_SHL64
M_PRIVATE_SHR64
M_PRIVATE_ASR64
M_PRIVATE_DIV64
M_MOD64
M_ABS64
M_PRIVATE_F2D64
M_PRIVATE_D642F

P_CODEEND:





# UNDER CONSTRUCTION
# new version 1.7.x for double precision and sysv conform.
P_REALF_NEW: # estr:R3, double:F1, fracdigs:R4
# F13: 10^fracdigs
# F12: 10.0
# F11: 0.0
# F10: 252:DOUBLE INT
# F9:  -252:DOUBLE INT
# F8:  divisor
# F7:  dev result temp
# F6: 1.0
# R5: temp

   # lets init some vars
   BL $ + 36 + 4
   .FLOAT 1.0
   .FLOAT 10.0
   .FLOAT 0.0
   .WORD 0x43300000
   .WORD 0x00000000 # 0x4330000000000000 = 252
   .WORD 0xC3300000
   .WORD 0x00000000 # 0xC330000000000000 = -252
   .UADOUBLE 1000000000000000.0
   MFLR R12
   LFS F13, 0(R12)
   FMR F6, F13
   LFS F12, 4(R12)
   LFS F11, 8(R12)
   LFD F10, 12(R12)
   LFD F9,  20(R12)
   LFD F8,  28(R12)

   OR R5,R4,R4

_realf_exp_loop:
   OR. R5,R5,R5
   BEQ _realf_exp_done
   FMUL F13,F13,F12 # * 10.0
   ADDI R5,R5,-1
   B _realf_exp_loop
_realf_exp_done:
   # lets round
   FMUL F1,F1,F13 # * 10^fracdiv

   FCMPU 0,F1,F11
   BLT _realf_ROUND_LAB     # BRANCH IF VALUE < 0.0

   FCMPU 0,F1,F10
   BGT _realf_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F1,F1,F10   # ADD 252
   FSUB F1,F1,F10   # SUBTRACT 252
   B _realf_ROUND_EXIT
_realf_ROUND_LAB:
   FCMPU 0,F1,F9
   BLT _realf_ROUND_EXIT    # INPUT WAS FLOATING-POINT INTEGER
   FADD F1,F1,F9   # ADD -252
   FSUB F1,F1,F9   # SUBTRACT -252
_realf_ROUND_EXIT:

   FDIV F1, F1, F13 # / 10^fracdiv

   # ROUNDING IS NOW DONE
   # LETS CONSTRUCT INTEGER PART
   # FIRST DIVIDE UNTIL RESULT > INT ZERO
_realf_skipemptyzeroes:
   FDIV F7, F1, F8 # / divisor
   FDIV F8, F8, F12 # divisor := divisor / 10.0
   FCMPU 0, F8, F6 # compare divisor with 1.0
   BLT _realf_construct_int
   FCTIWZ F7, F7 # to integer, throw away frac
   OR. F7, F7, F7
   BEQ _realf_skipemptyzeroes
_realf_construct_int:



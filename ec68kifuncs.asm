;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ec68kifuncs.asm
;; Ifuncs taken from Wouters van Oortmerssen's EC_733.S source
;; Distributed with permission under zlib license
;; Wouter van Oortmerssen 1991-1997 (wouter AT fov120 DOT com)
;; Leif Salomonsson 2003-2009 (dev AT blubbedev DOT net)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; The zlib/libpng License
;;
;; Copyright (c) 1991-1997 Wouter van Oortmerssen
;; Copyright (c) 2003-2009 Leif Salomonsson
;;
;; This software is provided 'as-is', without any express or implied warranty.
;; In no event will the authors be held liable for any damages arising from
;; the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely, subject to the following restrictions:
;;
;;
;; 1. The origin of this software must not be misrepresented; you must not
;;    claim that you wrote the original software. If you use this software
;;    in a product, an acknowledgment in the product documentation would be
;;    appreciated but is not required.
;;
;; 2. Altered source versions must be plainly marked as such, and must not be
;;    misrepresented as being the original software.
;;
;; 3. This notice may not be removed or altered from any source distribution.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; do not change order of functions !

 DC.L I_WRITEF,I_MUL,I_DIV,I_OPENW,I_OPENS
 DC.L I_MOUSE,I_PLOT,I_LINE,I_TEXTF,I_COLOR
 ; 10
 DC.L I_SETRAST,I_SETOUT,I_LONG,I_INT,I_CHAR
 DC.L I_PUTLONG,I_PUTINT,I_PUTCHAR,I_NEW,I_EXIT
 ; 20
 DC.L I_CLOSEW,I_CLOSES,I_AND,I_OR,I_NOT
 DC.L I_BOOLGAD,I_SETTOPAZ,I_STRCMP,I_STRCOPY,I_STRADD
 ; 30
 DC.L I_STRLEN,I_ESTRLEN,I_ESTRMAX,I_STRING_OLD,I_RIGHTSTR
 DC.L I_MIDSTR,I_STRINGF,I_VAL,I_INSTR,I_TRIMSTR
 ; 40
 DC.L I_UPPERSTR,I_LOWERSTR,I_READSTR,I_OUT,I_INP
 DC.L I_VERSION,I_FILELENGTH,I_MOUSEX,I_MOUSEY,I_FREESTACK
 ; 50
 DC.L I_CTRLC,I_LIST_OLD,I_LISTCOPY,I_LISTADD,I_LISTCMP
 DC.L I_LISTLEN,I_LISTMAX,I_EVEN,I_ODD,I_EVAL
 ; 60
 DC.L I_FORALL,I_EXISTS,I_MAPLIST,I_ABS,I_SHL
 DC.L I_SHR,I_BOX,I_DISP,I_DISPL_OLD,I_LINK
 ; 70
 DC.L I_NEXT,I_FORWARD,I_SETSTR,I_SETLIST,I_WAITMSG
 DC.L I_MSGCODE,I_MSGQUAL,I_MSGIADR,I_RND,I_RNDQ
 ; 80
 DC.L I_MOD,I_EOR,I_CAUSE,I_LISTITEM,I_NEWR
 DC.L I_SIGN,I_PRINTF,I_WAITLEFTMOUSE,I_LEFTMOUSE,I_SETIN
 ; 90
 DC.L I_THROW,I_RETHROW,I_SELECTLIST,I_SETCOLOUR,I_NEWM
 DC.L I_BOUNDS,I_REALF,I_REALVAL,I_FABS,I_FFLOOR
 ; 100
 DC.L I_FCEIL,I_FSIN,I_FCOS,I_FTAN,I_FEXP
 DC.L I_FLOG,I_FPOW,I_FSQRT,I_FLOG10,I_FASTDISPOSE
 ; 110
 DC.L I_FASTNEW,I_MIN,I_MAX,I_OSTRCMP,I_ASTRCOPY
 DC.L I_CELL,I_FREECELLS,I_SETCHUNKSIZE,I_CAR,I_CDR
 ; 120
 DC.L I_CONS,I_FASTDISPOSELIST,I_FATAN,I_FSINCOS,I_FSINH
 DC.L I_FCOSH,I_FTANH,I_FTIEEE,I_FFIEEE,I_FASIN
 ; 130
 DC.L I_FACOS,I_OBJNAME,I_OBJSIZE,I_DEBUGF,I_DOUBLE
 DC.L I_PUTDOUBLE,I_PTR,I_PUTPTR,I_BYTE,I_PUTBYTE
 ; 140
 DC.L I_WORD,I_PUTWORD,I_FLOAT,I_PUTFLOAT,I_REAL
 DC.L I_PUTREAL,I_NEWLIST,I_STRING,I_LIST,I_DISPL
 ; 150
 DC.L I_WIDE,I_PUTWIDE,I_ULONGTOWIDE,I_CODEEND


GLOBOFF     = -512
GLOBOFFNEWTAB   = GLOBOFF        ; 4..256+4
NEWTABMAX   = 256

CHOPMEM     = -100
CHOPLEFT    = -104
FMEMSIZE    = 4096

      I_WRITEF:
   TST.L   -8(A4)         ;STDIO
   BEQ   .2
.3:   LEA   8(A7),A1
   MOVE.L   4(A7),D0
   MOVE.L   8(A7,D0.L),A0
   LEA     .1(PC),A2
   MOVE.L  4.W,A6
   MOVE.L  -64(A4),A3
   MOVE.L  A3,D2
   JSR     -522(A6)
   MOVE.L  -8(A4),D1
   MOVE.L  D2,A0
.5:   TST.B   (A0)+
   BNE   .5
   SUB.L   D2,A0
   MOVE.L   D3,A3         ; BACKUP
   MOVE.L   A0,D3
   SUBQ.L   #1,D3
   MOVE.L  -44(A4),A6
   JSR     -48(A6)
   MOVE.L   D3,D0
   MOVE.L   A3,D3
   RTS
.1:   MOVE.B  D0,(A3)+      ; RDF DUMP
   RTS
.2:   LEA   .4(PC),A0
   MOVE.L   A0,D1         ;OPEN CON
   MOVE.L   #1006,D2
   MOVE.L   -44(A4),A6
   JSR   -30(A6)
   MOVE.L   D0,-12(A4)
   MOVE.L   D0,-8(A4)
   TST.L   D0
   BNE.S   .3
   MOVEQ   #20,D0
   MOVE.L   D0,-28(A4)
   MOVE.L   -24(A4),A0
   JMP   (A0)
.4:   DC.B   "CON:///80/Output/CLOSE",0
   EVEN

      I_MUL:  ; 020 version by LS
   MOVE.L 8(A7),D0
   MOVE.L 4(A7),D1
   MULS.L D1,D0
   RTS

      I_DIV: ; 020 version by LS
   MOVE.L 8(A7),D0
   MOVE.L 4(A7),D1
   DIVS.L D1,D0
   RTS


      I_OPENW:
   LEA   .1(PC),A0
   MOVE.L   4(A7),48(A0)
   MOVE.L   8(A7),18(A0)
   MOVE.W   14(A7),46(A0)
   MOVE.L   16(A7),30(A0)
   MOVE.L   20(A7),26(A0)
   MOVE.L   24(A7),D0
   MOVEQ   #18,D1
   BSET   D1,D0
   MOVE.L   D0,14(A0)
   MOVE.L   28(A7),10(A0)
   MOVE.W   34(A7),6(A0)
   MOVE.W   38(A7),4(A0)
   MOVE.W   42(A7),2(A0)
   MOVE.W   46(A7),(A0)
   MOVE.L   -48(A4),A6
   JSR   -$CC(A6)
   MOVE.L   D0,A0
   MOVE.L   A0,D0
   BEQ.S   .2
   MOVE.L   50(A0),D1
   MOVE.L   D1,-16(A4)
.2:   RTS
.1:   DC.W   0,0,0,0,-1
   DC.L   0,0,0,0,0,0,0
   DC.W   80,25,-1,-1,0,0,0

      I_OPENS:
   LEA   .1(PC),A0
   MOVE.L   4(A7),32(A0)
   MOVE.L   8(A7),20(A0)
   MOVE.W   14(A7),12(A0)
   MOVE.W   18(A7),8(A0)
   MOVE.W   22(A7),6(A0)
   MOVE.W   26(A7),4(A0)
   MOVE.L   -48(A4),A6
   JSR   -198(A6)
   MOVE.L   D0,D1
   BEQ.S   .2
   ADD.L   #84,D1
   MOVE.L   D1,-16(A4)
.2:   RTS
.1:   DC.W   0,0,640,256,1,$0001,$8000,$100F
   DC.L   0,0,0,0,0

      I_MOUSE:
   MOVEQ   #0,D0
   BTST   #6,$BFE001
   BNE.S   .1
   MOVEQ   #1,D0
.1:   BTST   #10,$DFF016
   BNE.S   .2
   BSET   #1,D0
.2:   BTST   #8,$DFF016
   BNE.S   .3
   BSET   #2,D0
.3:   RTS

      I_PLOT:
   MOVE.L   -16(A4),A1
   MOVE.L   A1,D0
   BEQ.S   .1
   MOVE.L   -52(A4),A6
   MOVE.L   4(A7),D0
   JSR   -342(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   12(A7),D0
   MOVE.L   8(A7),D1
   JSR   -324(A6)
.1:   RTS

      I_LINE:
   MOVE.L   -16(A4),A1
   MOVE.L   A1,D0
   BEQ.S   .1
   MOVE.L   -52(A4),A6
   MOVE.L   4(A7),D0
   JSR   -342(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   12(A7),D0
   MOVE.L   8(A7),D1
   JSR   -240(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   20(A7),D0
   MOVE.L   16(A7),D1
   JSR   -246(A6)
.1:   RTS

      I_TEXTF:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7,D0.L),A0
   LEA   8(A7),A1
   LEA     .3(PC),A2
   MOVE.L  4.W,A6
   ;MOVE.L  276(A6),A3
   ;MOVE.L  58(A3),A3
   MOVE.L  -64(A4),A3      ; BETTER!
   MOVE.L  A3,D2
   JSR     -522(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   A1,D0
   BEQ.S   .1
   MOVE.L   -52(A4),A6
   MOVE.L   4(A7),D0
   MOVE.L   12(A7,D0.L),D1
   MOVE.L   16(A7,D0.L),D0
   JSR   -240(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   D2,A0
   MOVE.L   A0,A2
.2:   TST.B   (A2)+
   BNE.S   .2
   MOVE.L   A2,D0
   SUB.L   A0,D0
   SUBQ.L   #1,D0
   MOVE.L   D0,D2
   JSR   -60(A6)
   MOVE.L   D2,D0
.1:   RTS
.3:   MOVE.B  D0,(A3)+      ;RDF DUMP
   RTS

      I_COLOR:
   MOVE.L   -16(A4),A1
   MOVE.L   A1,D0
   BEQ.S   .1
   MOVE.L   -52(A4),A6
   MOVE.L   8(A7),D0
   JSR   -342(A6)
   MOVE.L   -16(A4),A1
   MOVE.L   4(A7),D0
   JSR   -348(A6)
.1:   RTS


      I_SETRAST:
   MOVE.L   -16(A4),D0
   MOVE.L   4(A7),D1
   BEQ.S   .1
   MOVE.L   D1,-16(A4)
.1:   RTS

      I_SETOUT:
   MOVE.L   -8(A4),D0
   MOVE.L   4(A7),D1
   BEQ.S   .1
   MOVE.L   D1,-8(A4)
.1:   RTS

      I_LONG:
   MOVE.L   4(A7),A0
   MOVE.L   (A0),D0
   RTS

      I_INT:
   MOVE.L   4(A7),A0
   ;MOVEQ   #0,D0
   MOVE.W   (A0),D0
   EXT.L    D0 ; EC does no signextend,but we do (v49)!
   RTS

      I_CHAR:
   MOVE.L   4(A7),A0
   MOVEQ   #0,D0
   MOVE.B   (A0),D0
   RTS

      I_PUTLONG:
   MOVE.L   8(A7),A0
   MOVE.L   4(A7),(A0)
   RTS

      I_PUTINT:
   MOVE.L   8(A7),A0
   MOVE.W   6(A7),(A0)
   RTS

      I_PUTCHAR:
   MOVE.L   8(A7),A0
   MOVE.B   7(A7),(A0)
   RTS

      I_NEW:
   MOVE.L   4(A7),D0
   MOVE.L   #$10000,D1
   ADDQ.L   #8,D0
   MOVE.L   D0,D2
   MOVE.L   4.W,A6
   JSR   -198(A6)
   TST.L   D0
   BEQ.S   .1
   MOVE.L   D0,A0
   MOVE.L   -20(A4),(A0)
   MOVE.L   D2,4(A0)
   MOVE.L   D0,-20(A4)
   ADDQ.L   #8,D0
.1:   RTS

      I_EXIT:
   MOVE.L   4(A7),-28(A4)
   MOVE.L   -24(A4),A0
   JMP   (A0)

      I_CLOSEW:
   CLR.L   -16(A4)
   MOVE.L   4(A7),A0
   MOVE.L   A0,D0
   BEQ.S   .1
   MOVE.L   -48(A4),A6
   JSR   -72(A6)
.1:   RTS

      I_CLOSES:
   CLR.L   -16(A4)
   MOVE.L   4(A7),A0
   MOVE.L   A0,D0
   BEQ.S   .1
   MOVE.L   -48(A4),A6
   JSR   -66(A6)
.1:   RTS

      I_AND:
   MOVE.L   8(A7),D0
   AND.L   4(A7),D0
   RTS

      I_OR:
   MOVE.L   8(A7),D0
   OR.L   4(A7),D0
   RTS

      I_NOT:
   MOVE.L   4(A7),D0
   NOT.L   D0
   RTS

      I_BOOLGAD:
   MOVE.L   32(A7),D2
   MOVE.L   D2,A1         ; A1,D2=GADGET
   MOVE.L   A1,A2
   MOVEQ   #28,D0
.L:   CLR.L   (A2)+
   DBRA   D0,.L
   LEA   44(A1),A0      ; A0   =BORDER1
   LEA   16(A0),A2      ; A2   =INTUITEXT
   LEA   20(A2),A3      ; A3   =20 empty bytes
   MOVE.L   24(A7),40(A1)      ; --> INIT GADGET
   MOVE.W   18(A7),4(A1)
   MOVE.W   14(A7),6(A1)
   MOVE.W   10(A7),D0
   MOVE.W   D0,8(A1)
   MOVE.W   #12,10(A1)      ; D0,D1 = W,H
   SUBQ.W   #1,D0
   MOVEQ   #11,D1
   MOVE.W   #$0,12(A1)      ; FLAGS
   MOVE.W   #$1,14(A1)      ; ACTIVATEFLAGS
   MOVE.W   #$1,16(A1)      ; TYPEFLAGS
   BTST   #0,23(A7)
   BEQ.S   .5
   BSET   #0,14(A1)
.5:   BTST   #1,23(A7)
   BEQ.S   .6
   BSET   #7,13(A1)
.6:   MOVE.L   A2,26(A1)
   MOVE.L   A0,18(A1)
   MOVE.W   D0,8(A3)      ; --> INIT VECTORZ
   MOVE.W   D0,12(A3)
   MOVE.W   D1,6(A3)
   MOVE.W   D1,10(A3)
   MOVE.L   4(A7),D0
   MOVE.L   D0,12(A2)      ; --> INIT INTUITEXT
   MOVE.L   #$01000100,(A2)
   MOVE.W   #$2,6(A2)
   MOVE.L   D0,A6
.7:   TST.B   (A6)+
   BNE.S   .7
   SUB.L   D0,A6
   SUBQ.L   #1,A6
   MOVE.L   8(A7),D1
   MOVE.L   A6,D0
   LSL.W   #3,D0
   SUB.L   D0,D1
   LSR.W   #1,D1
   MOVE.W   D1,4(A2)
   MOVEQ   #0,D0
   MOVE.L   D0,(A0)         ; --> INIT BORDER
   MOVE.L   #$01000005,4(A0)
   MOVE.L   A3,8(A0)
   LEA   .3(PC),A3      ; --> TOPAZ
   LEA   8(A3),A0
   MOVE.L   A0,(A3)
   MOVE.L   A3,8(A2)
   MOVE.L   28(A7),A0      ; --> LINK GLIST
   MOVE.L   A0,D0
   BEQ.S   .2
   MOVE.L   (A0),D0         ; NEXTNODE OF PRED
   MOVE.L   A1,(A0)         ; HANG ONTO PRED
   MOVE.L   D0,(A1)         ; CONNECT NEXTNODE
.2:   MOVE.L   D2,D0
   ADD.L   #120,D0
   RTS
.3:   DC.L   0,$80060
.4:   DC.B   "topaz.font",0,0

      I_SETTOPAZ:
   MOVE.L   -52(A4),A6
   LEA   .2(PC),A0
   LEA   .3(PC),A1
   MOVE.L   A1,(A0)
   MOVE.W   6(A7),4(A0)
   JSR   -72(A6)         ; OPENFONT
   TST.L   D0
   BEQ.S   .1
   MOVE.L   D0,A0
   MOVE.L   -16(A4),A1
   MOVE.L   A1,D0
   BEQ.S   .1
   JSR   -66(A6)         ; SETFONT
.1:   RTS
.2:   DC.L   0,$80060
.3:   DC.B   "topaz.font",0,0

      I_STRCMP:
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A1      ; STR1
   MOVE.L   12(A7),A0      ; STR2
   ADDQ.L   #1,D0
.1:   SUBQ.L   #1,D0
   BEQ.S   .3
   CMPM.B   (A0)+,(A1)+
   BNE.S   .2
   CMP.B   #0,-1(A0)
   BNE.S   .1
.3:   MOVEQ   #-1,D0
   RTS
.2:   MOVEQ   #0,D0
   RTS

      I_STRCOPY:
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A1      ; STR
   MOVE.L   12(A7),A0      ; ESTR
   MOVE.L   A0,A2
   MOVEQ   #0,D1
   MOVE.W   -4(A0),D1
   CMP.L   D0,D1
   BPL.S   .1
   MOVE.L   D1,D0
.1:
   MOVE.L   D0,D2
   BEQ.S   .5 ;  0 len fix by LS
   SUBQ.L   #1,D0         ; D0=COPYLEN
.2:
   MOVE.B   (A1)+,(A0)+
   BEQ.S   .3
   DBRA   D0,.2
   MOVE.B   #0,(A0)+
.3:
   ADDQ.W   #1,D0
   SUB.W   D0,D2
   MOVE.W   D2,-2(A2)
   MOVE.L   A2,D0
.4:
   RTS
.5: ; 0 len fix by LS
   MOVE.W D0,-2(A2)
   MOVE.B D0,(A2)
   MOVE.L A2,D0
   RTS

      I_STRADD:
   MOVE.L   D3,A3
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A1      ; STR
   MOVE.L   12(A7),A0      ; ESTR
   MOVEQ   #0,D3
   MOVE.W   -2(A0),D3
   MOVE.L   A0,A2
   MOVEQ   #0,D1
   MOVE.W   -4(A0),D1
   SUB.L   D3,D1
   ADD.L   D3,A0
   CMP.L   D0,D1
   BPL.S   .1
   MOVE.L   D1,D0
.1:   MOVE.W   D0,D2
   BEQ.S   .4
   SUBQ.L   #1,D0         ; D0=COPYLEN
.2:   MOVE.B   (A1)+,(A0)+
   BEQ.S   .3
   DBRA   D0,.2
   MOVE.B   #0,(A0)+
.3:   ADDQ.W   #1,D0
   SUB.W   D0,D2
   ADD.W   D3,D2
   MOVE.W   D2,-2(A2)
.4:   MOVE.L   A3,D3
   MOVE.L   A2,D0
   RTS

      I_STRLEN:
   MOVE.L   4(A7),A0
   MOVE.L   A0,D1
.1:   TST.B   (A0)+
   BNE.S   .1
   SUBQ.L   #1,A0
   MOVE.L   A0,D0
   SUB.L   D1,D0
   RTS

      I_ESTRLEN:
   MOVE.L   4(A7),A0
   MOVEQ   #0,D0
   MOVE.W   -2(A0),D0
   RTS

      I_ESTRMAX:
   MOVE.L   4(A7),A0
   MOVEQ   #0,D0
   MOVE.W   -4(A0),D0
   RTS

      I_STRING_OLD:
   MOVE.L 4(A7),D0
   ADD.L #16,D0
   ANDI.L #$FFFC,D0
   MOVE.L D0,-(A7)
   MOVE.L -120(A4),A0 ; mempool
   MOVE.L 4.W,A6
   JSR -708(A6)
   MOVE.L (A7)+,D1
   TST.L D0
   BEQ .3
   MOVE.L D0,A0
   MOVE.L D1,(A0)+
   CLR.L (A0)+ ; memory is NOT cleared!
   MOVE.L 4(A7),D1
   SWAP D1
   MOVE.L D1,(A0)+
   MOVE.L A0,D0
.3:
   RTS




      I_RIGHTSTR:
   MOVEQ   #0,D0
   MOVE.L   12(A7),A0
   MOVE.L   8(A7),A1
   MOVE.W   -2(A1),D0
   SUB.L   4(A7),D0
   TST.L   D0
   BPL.S   .4
   MOVEQ   #0,D0
.4:   ADD.L   D0,A1
   MOVE.L   #$FFFF,D0
   MOVE.L   A0,A2
   MOVEQ   #0,D1
   MOVE.W   -4(A0),D1
   CMP.L   D0,D1
   BPL.S   .1
   MOVE.L   D1,D0
.1:   MOVE.L   D0,D2
   SUBQ.L   #1,D0         ; D0=COPYLEN
.2:   MOVE.B   (A1)+,(A0)+
   BEQ.S   .3
   DBRA   D0,.2
   MOVE.B   #0,(A0)+
.3:   ADDQ.W   #1,D0
   SUB.W   D0,D2
   MOVE.W   D2,-2(A2)
   MOVE.L   A2,D0
   RTS

      I_MIDSTR:
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   12(A7),A1      ; STR
   ADD.L   8(A7),A1      ; +OFFSET
   MOVE.L   16(A7),A0      ; ESTR
   MOVE.L   A0,A2
   MOVEQ   #0,D1
   MOVE.W   -4(A0),D1
   CMP.L   D0,D1
   BPL.S   .1
   MOVE.L   D1,D0
.1:   MOVE.L   D0,D2
   SUBQ.L   #1,D0         ; D0=COPYLEN
.2:   MOVE.B   (A1)+,(A0)+
   BEQ.S   .3
   DBRA   D0,.2
   MOVE.B   #0,(A0)+
.3:   ADDQ.W   #1,D0
   SUB.W   D0,D2
   MOVE.W   D2,-2(A2)
   MOVE.L   A2,D0
   RTS

      I_STRINGF:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7,D0.L),A0      ; formatstr
   LEA   8(A7),A1      ; argarray
   LEA     .1(PC),A2      ; putfun
   MOVE.L  4.W,A6
   ;MOVE.L  276(A6),A3
   ;MOVE.L  58(A3),A3
   MOVE.L  -64(A4),A3      ; BETTER!
   MOVE.L  A3,D2         ; both bottom OWN stack
   JSR     -522(A6)
   MOVE.L   D2,A1
   MOVE.L   4(A7),D0
   MOVE.L   12(A7,D0.L),A0      ; ESTR
   MOVE.L   A0,A2
   MOVEQ   #0,D0
   MOVE.W   -4(A0),D0
   MOVE.L   D0,D2
   SUBQ.L   #1,D0         ; D0=COPYLEN
.2:   MOVE.B   (A1)+,(A0)+
   BEQ.S   .3
   DBRA   D0,.2
   MOVE.B   #0,(A0)+
.3:   ADDQ.W   #1,D0
   SUB.W   D0,D2
   MOVE.W   D2,-2(A2)
   MOVE.L   D2,D1
   EXT.L   D1
   MOVE.L   A2,D0
   RTS
.1:   MOVE.B  D0,(A3)+      ;RDF DUMP
   RTS

      I_VAL:
   MOVEM.L   D3-D6,-(A7)      ; 16 OFF STACK
   MOVE.L   24(A7),A2
   MOVE.L   20(A7),D5      ; D5=READLENADR
   MOVEQ   #0,D6         ; D6=MINUSFLAG
.5:   MOVE.B   (A2)+,D0      ; reg d0,a2
   BEQ   VALER
   CMP.B   #33,D0
   BMI.S   .5
   SUBQ.L   #1,A2
   CMP.B   #"-",(A2)
   BNE.S   .NM
   ADDQ.L   #1,A2
   MOVEQ   #-1,D6
.NM:   CMP.B   #'$',(A2)
   BEQ   HEX
   CMP.B   #'%',(A2)
   BEQ   BIN
   LEA   11(A2),A1      ; reg d0-d3,a0-a3
   MOVE.L   A2,A0
.4:   MOVE.B   (A2)+,D0
   CMP.B   #58,D0
   BPL.S   .6
   CMP.B   #48,D0
   BPL.S   .4
.6:   SUBQ.L   #1,A2
   MOVE.L   A2,D4         ; D4=TOREADLEN
   CMPA.L   A2,A0
   BEQ.S   VALER
   MOVEQ   #0,D0         ; RESULT
   CMPA.L   A1,A2
   BPL.S   VALER
   LEA   .2(PC),A3
.1:   MOVEQ   #0,D3
   MOVE.B   -(A2),D3
   MOVE.L   (A3)+,D2
   SUB.B   #48,D3
   MOVE.L   D3,D1
   MULU   D2,D3
   SWAP   D2
   MULU   D2,D1
   SWAP   D1
   ADD.L   D1,D3
   ADD.L   D3,D0
   BCS.S   VALER
   CMPA.L   A0,A2
   BNE.S   .1
   BRA.S   ISIND0
.2:   DC.L   1,10,100,1000,10000,100000,1000000
   DC.L   10000000,100000000,1000000000
ISIND0:   SUB.L   24(A7),D4
   TST.L   D5
   BEQ.S   .NP
   MOVE.L   D5,A0
   MOVE.L   D4,(A0)
.NP:   TST.L   D6
   BEQ.S   .NN
   NEG.L   D0
.NN:   MOVE.L   D4,D1
   MOVEM.L   (A7)+,D3-D6
   RTS
VALER:   MOVEQ   #0,D0
   MOVEQ   #0,D1
   TST.L   D5
   BEQ.S   .NP
   MOVE.L   D5,A0
   CLR.L   (A0)
.NP:   MOVEM.L   (A7)+,D3-D6
   RTS
HEX:   ADDQ.L   #1,A2
   MOVE.L   A2,A0         ; reg d0-d3,a0,a2
   MOVE.L   A0,D3
   MOVEQ   #0,D1         ; RESULT
   MOVEQ   #0,D2         ; #OF SHIFTS
.4:   MOVE.B   (A0)+,D0
   CMP.B   #"G",D0
   BPL.S   .2B
   CMP.B   #"0",D0
   BMI.S   .1
   CMP.B   #"9"+1,D0
   BPL.S   .2
   BRA.S   .4
.1:   SUBQ.L   #1,A0
   MOVE.L   A0,D4         ; SET TOREADLEN
   CMP.L   A0,D3
   BEQ.S   VALER
.3:   CMP.L   A0,D3
   BEQ.S   .5
   MOVEQ   #0,D0
   MOVE.B   -(A0),D0
   CMP.B   #"A",D0
   BPL.S   .10
   SUB.B   #"0",D0
.11:   CMP.W   #32,D2
   BEQ   VALER
   LSL.L   D2,D0
   ADD.L   D0,D1
   ADDQ.L   #4,D2
   BRA.S   .3
.5:   MOVE.L   D1,D0
   BRA.S   ISIND0
.2:   CMP.B   #"A",D0
   BPL.S   .4
   BRA.S   .1
.2B:   CMP.B   #"a",D0
   BMI.S   .1
   CMP.B   #"g",D0
   BPL.S   .1
   BRA.S   .4
.10:   CMP.B   #"a",D0
   BPL.S   .10B
   SUB.B   #55,D0
   BRA.S   .11
.10B:   SUB.B   #55+32,D0
   BRA.S   .11
BIN:   ADDQ.L   #1,A2
   MOVE.L   A2,A0         ; reg d0-d3,a0,a2
   MOVE.L   A0,D3
   MOVEQ   #0,D1         ; RESULT
   MOVEQ   #0,D2         ; BITNUM
.4:   MOVE.B   (A0)+,D0
   CMP.B   #"1",D0
   BEQ.S   .4
   CMP.B   #"0",D0
   BEQ.S   .4
.1:   SUBQ.L   #1,A0
   MOVE.L   A0,D4         ; SET TOREADLEN
   CMP.L   A0,D3
   BEQ   VALER
.3:   CMP.L   A0,D3
   BEQ.S   .5
   MOVEQ   #0,D0
   MOVE.B   -(A0),D0
   CMP.B   #"0",D0
   BEQ.S   .10
   BSET   D2,D1
.10:   CMP.W   #32,D2
   BEQ   VALER
   ADDQ.L   #1,D2
   BRA.S   .3
.5:   MOVE.L   D1,D0
   BRA   ISIND0

      I_INSTR:
   MOVE.L   8(A7),A1      ; STRING TO FIND
   MOVE.L   12(A7),A0      ; THE STRING
   MOVE.L   A0,D2
   ADD.L   4(A7),A0
   MOVE.B   (A1)+,D0      ; FIRST CHAR
.1:   MOVE.B   (A0)+,D1
   BEQ.S   .2
   CMP.B   D0,D1
   BNE.S   .1
   MOVE.L   A0,A2
   MOVE.L   A1,A3
.3:   MOVE.B   (A3)+,D1
   BEQ.S   .4
   CMP.B   (A2)+,D1
   BNE.S   .1
   BEQ.S   .3
.4:   SUBQ.L   #1,A0
   MOVE.L   A0,D0
   SUB.L   D2,D0
   RTS
.2:   MOVEQ   #-1,D0
   RTS

      I_TRIMSTR:
   MOVE.L   4(A7),A2
   MOVEQ   #0,D0
.1:   MOVE.B   (A2)+,D0
   BEQ.S   .2
   CMP.W   #33,D0
   BMI.S   .1
.2:   SUBQ.L   #1,A2
   MOVE.L   A2,D0
   RTS

      I_UPPERSTR:
   MOVE.L   4(A7),A0
   MOVE.L   A0,A1
.1:   MOVE.B   (A0)+,D0
   BEQ.S   .2
   CMP.B   #97,D0
   BMI.S   .1
   CMP.B   #123,D0
   BPL.S   .1
   SUB.B   #32,D0
   MOVE.B   D0,-1(A0)
   BRA.S   .1
.2:   MOVE.L   A1,D0
   RTS

      I_LOWERSTR:
   MOVE.L   4(A7),A0
   MOVE.L   A0,A1
.1:   MOVE.B   (A0)+,D0
   BEQ.S   .2
   CMP.B   #91,D0
   BPL.S   .1
   CMP.B   #65,D0
   BMI.S   .1
   ADD.B   #32,D0
   MOVE.B   D0,-1(A0)
   BRA.S   .1
.2:   MOVE.L   A1,D0
   RTS

      I_READSTR:
   MOVEM.L   D3/D4/D6/D7,-(A7)
   MOVE.L   20(A7),A2      ; ESTR
   MOVE.L   A2,D4
   MOVE.L   24(A7),D7      ; FH
   MOVEQ   #0,D6
   MOVE.W   -4(A2),D6      ; MAXLEN
   ADD.L   A2,D6
   MOVE.L   -44(A4),A6
   MOVEQ   #1,D3
.1:   CMP.L   D6,A2
   BEQ.S   .4
   MOVE.L   D7,D1
   MOVE.L   A2,D2
   ADDQ.L   #1,A2
   JSR   -42(A6)
   CMP.W   #1,D0
   BMI.S   .5
   CMP.B   #10,-1(A2)
   BNE.S   .1
   MOVEQ   #0,D0
   SUBQ.L   #1,A2
.4:   MOVE.L   A2,D1
   MOVE.B   #0,(A2)+
   SUB.L   D4,D1
   MOVE.L   D4,A0
   MOVE.W   D1,-2(A0)
   MOVEM.L   (A7)+,D3/D4/D6/D7
   RTS
.5:   SUBQ.L   #1,A2
   MOVEQ   #-1,D0
   BRA.S   .4

      I_OUT:
   MOVE.L   D3,A3
   MOVE.L   8(A7),D1
   LEA   7(A7),A0
   MOVE.L   A0,D2
   MOVEQ   #1,D3
   MOVE.L   -44(A4),A6
   JSR   -48(A6)
   MOVE.L   A3,D3
   RTS

      I_INP:
   MOVE.L   D3,A3
   MOVE.L   4(A7),D1
   MOVEQ   #1,D3
   LEA   4(A7),A0
   MOVE.L   A0,D2
   MOVE.L   -44(A4),A6
   JSR   -42(A6)
   MOVE.L   A3,D3
   TST.L   D0
   BEQ.S   .1
   MOVEQ   #0,D0
   MOVE.B   4(A7),D0
   RTS
.1:   MOVEQ   #-1,D0
   RTS


      I_VERSION:
   MOVE.L   4(A7),D0      ; V. REQUESTED
   MOVE.L   4.W,A6
   MOVE.W   20(A6),D1      ; V. CURRENTLY RUNNING
   CMP.W   D0,D1
   BMI.S   .1
   MOVEQ   #-1,D0         ; TRUE=RIGHT KICK
   RTS
.1:   MOVEQ   #0,D0         ; FALSE
   RTS

      I_FILELENGTH:
   MOVEM.L   D4/D6/D7,-(A7)
   MOVE.L   16(A7),D1
   MOVEQ   #-2,D2
   MOVE.L   -44(A4),A6
   JSR   -84(A6)
   MOVE.L   D0,D7         ; LOCK
   BEQ.S   .1
   MOVE.L   D0,D1
   MOVE.L   #-260,D4      ; FRAME WITH BCPL ADDRESS CONV.
   MOVE.L   A7,D0
   BTST   #1,D0
   BEQ.S   .4
   SUBQ.L   #2,D4
.4:   LEA   0(A7,D4.L),A7
   MOVE.L   A7,D2
   JSR   -102(A6)      ; EXAMINE
   MOVE.L   124(A7),D6      ; GET LENGTH FROM FIB
   NEG.L   D4
   LEA   0(A7,D4.L),A7
   TST.L   D0
   BEQ.S   .2
   BSR   .3
   MOVE.L   D6,D0
   MOVEM.L   (A7)+,D4/D6/D7
   RTS
.3:   MOVE.L   D7,D1         ; UNLOCK
   JSR   -90(A6)
   RTS
.2:   BSR.S   .3
.1:   MOVEQ   #-1,D0
   MOVEM.L   (A7)+,D4/D6/D7
   RTS

      I_MOUSEX:
   MOVEQ   #0,D0
   MOVE.L   4(A7),A0
   MOVE.W   14(A0),D0
   RTS

      I_MOUSEY:
   MOVEQ   #0,D0
   MOVE.L   4(A7),A0
   MOVE.W   12(A0),D0
   RTS

      I_FREESTACK:
   MOVE.L   A7,D0
   SUB.L   -64(A4),D0
   SUB.L   #1000,D0
   RTS

      I_CTRLC:
   MOVEQ   #0,D0
   MOVEQ   #0,D1
   MOVE.L   4.W,A6
   JSR   -306(A6)
   BTST   #12,D0         ; NOTE: 13=CTRLD ETC.
   BEQ.S   .1
   MOVEQ   #0,D0
   MOVE.L   #4096,D1
   JSR   -306(A6)
   MOVEQ   #-1,D0
   RTS
.1:   MOVEQ   #0,D0         ; D0<--TRUE/FALSE
   RTS

      I_LIST_OLD:
   MOVE.L 4(A7),D0
   LSL.L #2,D0
   ADD.L #12,D0
   MOVE.L D0,-(A7)
   MOVE.L -120(A4),A0  ; mempool
   MOVE.L 4.W,A6
   JSR -708(A6)
   MOVE.L (A7)+,D1
   TST.L D0
   BEQ .3
   MOVE.L D0,A0
   MOVE.L D1,(A0)+
   CLR.L (A0)+ ; memory is NOT cleared!
   MOVE.L 4(A7),D1
   SWAP D1
   MOVE.L D1,(A0)+
   MOVE.L A0,D0
.3:
   RTS

      I_LISTCOPY:
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A0      ; SRC
   MOVE.L   12(A7),A1      ; DEST
   MOVE.L   A1,A2
   CMP.W   #-1,D0
   BNE.S   .1
   MOVE.W   -2(A0),D0
.1:   CMP.W   -4(A1),D0
   BMI.S   .2
   MOVE.W   -4(A1),D0
.2:   CMP.W   #1,D0
   BMI.S   .3
   MOVE.W   D0,-2(A1)
   SUBQ.W   #1,D0
.L:   MOVE.L   (A0)+,(A1)+
   DBRA   D0,.L
.3:   MOVE.L   A2,D0
   RTS

      I_LISTADD:
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A0      ; SRC
   MOVE.L   12(A7),A1      ; DEST
   MOVE.L   A1,A2
   CMP.W   #-1,D0
   BNE.S   .1
   MOVE.W   -2(A0),D0
.1:   MOVE.W   -4(A1),D1
   SUB.W   -2(A1),D1
   CMP.W   D1,D0
   BMI.S   .2
   MOVE.W   D1,D0
.2:   CMP.W   #1,D0
   BMI.S   .3
   MOVE.W   -2(A1),D1
   ADD.W   D0,-2(A1)
   EXT.L   D1
   LSL.L   #2,D1
   ADD.L   D1,A1
   SUBQ.W   #1,D0
.L:   MOVE.L   (A0)+,(A1)+
   DBRA   D0,.L
.3:   MOVE.L   A2,D0
   RTS

      I_LISTCMP:
   MOVE.W   6(A7),D0      ; LEN
   MOVE.L   8(A7),A0      ; SRC
   MOVE.L   12(A7),A1      ; DEST
   CMP.W   #-1,D0
   BNE.S   .1
   MOVE.W   -2(A0),D0
.1:   CMP.W   -2(A1),D0
   BNE.S   .4         ; LENDEST<>LENSRC
   CMP.W   #1,D0
   BMI.S   .3
   SUBQ.W   #1,D0
.L:   CMPM.L   (A0)+,(A1)+
   BNE.S   .4
   DBRA   D0,.L
.3:   MOVEQ   #-1,D0
   RTS
.4:   MOVEQ   #0,D0
   RTS

      I_LISTLEN:
   MOVE.L   4(A7),A0
   MOVEQ   #0,D0
   MOVE.W   -2(A0),D0
   RTS

      I_LISTMAX:
   MOVE.L   4(A7),A0
   MOVEQ   #0,D0
   MOVE.W   -4(A0),D0
   RTS

      I_EVEN:
   BTST   #0,7(A7)
   BEQ.S   .1
   MOVEQ   #0,D0
   RTS
.1:   MOVEQ   #-1,D0
   RTS

      I_ODD:
   BTST   #0,7(A7)
   BNE.S   .1
   MOVEQ   #0,D0
   RTS
.1:   MOVEQ   #-1,D0
   RTS

      I_EVAL:
   MOVE.L   4(A7),A0
   JSR   (A0)         ; ACTIONNES DANGEREUSES !!!
   RTS

      I_FORALL:
   MOVE.L   4(A7),A0      ; code
   MOVE.L   8(A7),A1      ; list
   MOVE.L   12(A7),A2      ; VAR
   MOVEQ   #-1,D0         ; TRUTH VALUE ->D1
   MOVE.W   -2(A1),D2      ; COUNT
   BEQ.S   .1
   SUBQ.L   #1,D2
   MOVE.L   D0,D1
.L:   MOVE.L   (A1)+,(A2)
   MOVEM.L   D1-D2/A0-A2,-(A7)
   JSR   (A0)
   MOVEM.L   (A7)+,D1-D2/A0-A2
   TST.L   D0
   BEQ.S   .2
   DBRA   D2,.L
   BRA.S   .3
.2:   MOVEQ   #0,D1
   DBRA   D2,.L
.3:   MOVE.L   D1,D0
.1:   RTS

      I_EXISTS:
   MOVE.L   4(A7),A0      ; code
   MOVE.L   8(A7),A1      ; list
   MOVE.L   12(A7),A2      ; VAR
   MOVEQ   #0,D0         ; TRUTH VALUE ->D1
   MOVE.W   -2(A1),D2      ; COUNT
   BEQ.S   .1
   SUBQ.L   #1,D2
   MOVE.L   D0,D1
.L:   MOVE.L   (A1)+,(A2)
   MOVEM.L   D1-D2/A0-A2,-(A7)
   JSR   (A0)
   MOVEM.L   (A7)+,D1-D2/A0-A2
   TST.L   D0
   BNE.S   .2
   DBRA   D2,.L
   BRA.S   .3
.2:   MOVEQ   #-1,D1
   DBRA   D2,.L
.3:   MOVE.L   D1,D0
.1:   RTS

      I_MAPLIST:
   MOVE.L   4(A7),A0      ; code
   MOVE.L   8(A7),A3      ; DESTLIST
   MOVE.L   12(A7),A1      ; SRClist
   MOVE.L   16(A7),A2      ; VAR
   MOVE.L   A3,-(A7)
   MOVE.W   -4(A3),D0
   CMP.W   -2(A1),D0
   BMI.S   .1
   MOVE.W   -2(A1),-2(A3)
   MOVE.W   -2(A1),D2      ; COUNT
   BEQ.S   .1
   SUBQ.L   #1,D2
;   MOVE.L   D0,D1
.L:   MOVE.L   (A1)+,(A2)
   MOVEM.L   D2/A0-A3,-(A7)
   JSR   (A0)
   MOVEM.L   (A7)+,D2/A0-A3
   MOVE.L   D0,(A3)+
   DBRA   D2,.L
.1:   MOVE.L   (A7)+,D0
   RTS

      I_ABS:
   MOVE.L   4(A7),D0
   BPL.S   .1
   NEG.L   D0
.1:   RTS

      I_SHL:
   MOVE.L   8(A7),D0
   MOVE.L   4(A7),D1
   ASL.L   D1,D0
   RTS

      I_SHR:
   MOVE.L   8(A7),D0
   MOVE.L   4(A7),D1
   ASR.L   D1,D0
   RTS

      I_BOX:
   MOVE.L   D3,A3
   MOVE.L   -16(A4),A1      ; rast
   MOVE.L   A1,D0
   BEQ.S   .1
   MOVE.L   -52(A4),A6
   MOVE.L   4(A7),D0
   JSR   -342(A6)      ; setapen
   MOVE.L   -16(A4),A1
   MOVE.L   20(A7),D0
   MOVE.L   16(A7),D1
   MOVE.L   12(A7),D2
   MOVE.L   8(A7),D3
   JSR   -306(A6)      ; rectfill
.1:   MOVE.L   A3,D3
   RTS

      I_DISP:
   MOVE.L   4(A7),D0
   BEQ.S   .1
   SUBQ.L   #8,D0
   LEA   -20(A4),A1
.L:   MOVE.L   (A1),D1
   BEQ.S   .1
   MOVE.L   A1,A2         ; ADR TO LINK BACK TO
   MOVE.L   D1,A1
   CMP.L   D1,D0
   BNE.S   .L
   MOVE.L   4(A1),D0      ; MEMSIZE
   MOVE.L   (A1),(A2)      ; LINK BACK
   MOVE.L   4.W,A6
   JSR   -210(A6)
.1:   RTS

      I_DISPL_OLD:
   MOVE.L 4(A7),A3
.3:
   MOVE.L A3,D0
   BEQ .4
   LEA -12(A3),A1
   MOVE.L -8(A3),A3
   MOVE.L -120(A4),A0
   MOVE.L (A1),D0
   MOVE.L 4.W,A6
   JSR -714(A6)
   BRA .3
.4:
   RTS



      I_LINK:
   MOVE.L   8(A7),A0
   MOVE.L   A0,D0
   BEQ.S   .1
   MOVE.L   4(A7),-8(A0)
.1:   RTS

      I_NEXT:
   MOVE.L   4(A7),D0
   BEQ.S   .1
   MOVE.L   D0,A0
   MOVE.L   -8(A0),D0
.1:   RTS

      I_FORWARD:
   LEA   8(A7),A0      ; LIST
   MOVE.L   4(A7),D1      ; NUM
   ADDQ.L   #1,D1
.L:   MOVE.L   (A0),D0
   BEQ.S   .1
   MOVE.L   D0,A0
   SUBQ.L   #8,A0
   SUBQ.L   #1,D1
   BNE.S   .L
.1:   RTS

      I_SETSTR:
   MOVE.W   6(A7),D0
   MOVE.L   8(A7),A0
   CMP.W   -4(A0),D0
   BHI.S   .1
   MOVE.W   D0,-2(A0)
   MOVE.B   #0,0(A0,D0.W)
.1:   RTS

      I_SETLIST:
   MOVE.W   6(A7),D0
   MOVE.L   8(A7),A0
   CMP.W   -4(A0),D0
   BHI.S   .1
   MOVE.W   D0,-2(A0)
.1:   RTS

      I_WAITMSG:
   MOVE.L   4(A7),A0
   MOVE.L   $56(A0),A2      ; A2=PORT
   MOVE.L   4.W,A6
   MOVE.L   A2,A0
   JSR   -372(A6)      ; GETMSG
   MOVE.L   D0,A3         ; A3=MES
   TST.L   D0
   BNE.S   .1
.2:   MOVE.L   A2,A0
   JSR   -384(A6)      ; WAITPORT
   MOVE.L   A2,A0
   JSR   -372(A6)      ; GETMSG
   MOVE.L   D0,A3
   TST.L   D0
   BEQ.S   .2
.1:   MOVE.L   28(A3),-72(A4)
   MOVE.L   24(A3),-68(A4)
   MOVE.L   20(A3),D2
   MOVE.L   A3,A1
   JSR   -378(A6)      ; REPLY
   MOVE.L   D2,D0         ; CLASS
   RTS

      I_MSGCODE:
   MOVEQ   #0,D0
   MOVE.W   -68(A4),D0
   RTS

      I_MSGQUAL:
   MOVEQ   #0,D0
   MOVE.W   -66(A4),D0
   RTS

      I_MSGIADR:
   MOVE.L   -72(A4),D0
   RTS

      I_RND:            ; rand16:=RangeRand(max16)
   MOVE.L   4(A7),D2      ; 6(a7) was E(a7) ????
   BMI.S   .SET
   MOVE.W   D2,D1         ; RangeRand(1000) --> 0..999
   SUBQ.W   #$1,D1
   MOVE.L   .S(PC),D0      ; randnr:=Rnd(max)
.2:   ADD.L   D0,D0
   BHI.S   .3
   EORI.L   #$1D872B41,D0
.3:   LSR.W   #$1,D1
   BNE.S   .2
   LEA   .S(PC),A0
   MOVE.L   D0,(A0)
   TST.W   D2
   BNE.S   .4
   SWAP   D0
   BRA.S   .5
.4:   MULU   D2,D0
.5:   CLR.W   D0
   SWAP   D0
   RTS
.SET:   NEG.L   D2
   LEA   .S(PC),A0
   MOVE.L   D2,(A0)
   RTS
.S:   DC.L   0

      I_RNDQ:            ;   seed32:=FastRand(seed32)
   MOVE.L   $0004(A7),D0
   ADD.L   D0,D0         ;   seed:=RndQ(seed)
   BHI.S   .1
   EORI.L   #$1D872B41,D0
.1:   RTS

      I_MOD:
   ;MOVE.L   8(A7),D0
   ;DIVS   6(A7),D0
   ;MOVE.L   D0,D1
   ;SWAP   D0
   ;EXT.L   D0
   ;EXT.L   D1

   ; 020 version by LS
   MOVE.L 8(A7),D1
   MOVE.L 4(A7),D0
   DIVSL.L D0,D0:D1
   RTS

      I_EOR:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7),D1
   EOR.L   D1,D0
   RTS

; see also NEWR(), Throw() and ReThrow()

      I_CAUSE:
   MOVE.L   4(A7),-84(A4)      ; FILL "EXCEPTION" VAR
   MOVE.L   -76(A4),D0
   BEQ.S   .1
   MOVE.L   D0,A0         ; A0=CODE TO JUMP TO
   MOVE.L   -80(A4),A7      ; STACK BACK
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)      ; (A5)
   MOVE.L   (A7)+,-76(A4)      ; PUT PREVIOUS HANDLER BACK (CODE)
   MOVE.L   (A7)+,-80(A4)      ; (STACK)
   JMP   (A0)
.1:   MOVE.L   -24(A4),A0      ; PERFORM CLEANUP(0)
   JMP   (A0)

      I_LISTITEM:         ; SIMPLE,BUT EFFECTIVE !
   MOVE.L   4(A7),D0
   LSL.L   #2,D0
   MOVE.L   8(A7),A0
   MOVE.L   0(A0,D0.L),D0
   RTS

      I_NEWR:
   MOVE.L   4(A7),D0      ; COPY OF New()
   MOVE.L   #$10000,D1
   ADDQ.L   #8,D0
   MOVE.L   D0,D2
   MOVE.L   4.W,A6
   JSR   -198(A6)
   TST.L   D0
   BEQ.S   .1
   MOVE.L   D0,A0
   MOVE.L   -20(A4),(A0)
   MOVE.L   D2,4(A0)
   MOVE.L   D0,-20(A4)
   ADDQ.L   #8,D0
   RTS
.1:   MOVE.L   #"MEM",-84(A4)      ; COPY OF Raise()
   MOVE.L   -76(A4),D0
   BEQ.S   .2
   MOVE.L   D0,A0
   MOVE.L   -80(A4),A7
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)
   MOVE.L   (A7)+,-76(A4)
   MOVE.L   (A7)+,-80(A4)
   JMP   (A0)
.2:   MOVE.L   -24(A4),A0
   JMP   (A0)
   RTS

      I_SIGN:
   MOVE.L   4(A7),D0
   BMI.S   .1
   BEQ.S   .2
   MOVEQ   #1,D0
   RTS
.1:   MOVEQ   #-1,D0
   RTS
.2:   MOVEQ   #0,D0
   RTS

      I_PRINTF:
   TST.L   -8(A4)         ;STDIO
   BEQ   .2
.3:   MOVE.L   4(A7),D0
   MOVE.L   8(A7,D0.L),A0
   LEA   8(A7),A1
   LEA   .1(PC),A2
   MOVE.L  4.W,A6
   MOVE.L   -64(A4),A3
   MOVE.L   A3,D2
   JSR   -522(A6)
   MOVE.L   -8(A4),D1
   MOVE.L   -44(A4),A6
   JSR   -342(A6)
   MOVE.L   D2,A0         ; JUST FOR LEN!
.C:   TST.B   (A0)+
   BNE.S   .C
   SUB.L   D2,A0
   SUBQ.L   #1,A0
   MOVE.L   A0,D0
   RTS
.1:   MOVE.B  D0,(A3)+      ; RDF DUMP
   RTS
.2:   LEA   .4(PC),A0
   MOVE.L   A0,D1         ; OPEN CON
   MOVE.L   #1006,D2
   MOVE.L   -44(A4),A6
   JSR   -30(A6)
   MOVE.L   D0,-12(A4)
   MOVE.L   D0,-8(A4)
   TST.L   D0
   BNE.S   .3
   MOVEQ   #20,D0
   MOVE.L   D0,-28(A4)
   MOVE.L   -24(A4),A0
   JMP   (A0)
.4:   DC.B   "CON:///80/Output/CLOSE",0
   EVEN

      I_WAITLEFTMOUSE:
   MOVE.L   4(A7),A3      ; A3=WIN
   MOVE.L   82(A3),D0
   BTST   #3,D0
   BNE.S   .3         ; CHECK IDCMP CONTAINS MOUSE
   BSET   #3,D0
   MOVE.L   -48(A4),A6
   MOVE.L   A3,A0
   JSR   -150(A6)      ; MODIFYIDCMP
.3:   MOVE.L   $56(A3),A2      ; A2=PORT
   MOVE.L   4.W,A6
   MOVE.L   A2,A0
   JSR   -372(A6)      ; GETMSG
   MOVE.L   D0,A3         ; A3=MES
   TST.L   D0
   BNE.S   .1
.2:   MOVE.L   A2,A0
   JSR   -384(A6)      ; WAITPORT
   MOVE.L   A2,A0
   JSR   -372(A6)      ; GETMSG
   MOVE.L   D0,A3
   TST.L   D0
   BEQ.S   .2
.1:   MOVE.L   A3,A1         ; MES
   MOVE.L   20(A1),D2
   JSR   -378(A6)      ; REPLY
   MOVE.L   D2,D0         ; CLASS
   BTST   #3,D0
   BEQ.S   .2
   RTS

      I_LEFTMOUSE:
   MOVE.L   4(A7),A3      ; A3=WIN
   MOVE.L   82(A3),D0
   BTST   #3,D0
   BNE.S   .3         ; CHECK IDCMP CONTAINS MOUSE
   BSET   #3,D0
   MOVE.L   -48(A4),A6
   MOVE.L   A3,A0
   JSR   -150(A6)      ; MODIFYIDCMP
.3:   MOVE.L   $56(A3),A2      ; A2=PORT
   MOVE.L   4.W,A6
   MOVE.L   A2,A0
   JSR   -372(A6)      ; GETMSG
   TST.L   D0
   BNE.S   .1
   RTS
.1:   MOVE.L   D0,A1         ; MES
   MOVE.L   20(A1),D2
   JSR   -378(A6)      ; REPLY
   MOVE.L   D2,D0         ; CLASS
   BTST   #3,D0
   BEQ.S   .2
   MOVEQ   #-1,D0
   RTS
.2:   MOVEQ   #0,D0
   RTS

      I_SETIN:
   MOVE.L   -8(A4),D0
   MOVE.L   4(A7),D1
   BEQ.S   .1
   MOVE.L   D1,-92(A4)
.1:   RTS

      I_THROW:
   MOVE.L   8(A7),-84(A4)      ; FILL "EXCEPTION" VAR
   MOVE.L   4(A7),-96(A4)      ; FILL "EXCEPTIONINFO" VAR
   MOVE.L   -76(A4),D0
   BEQ.S   .1
   MOVE.L   D0,A0         ; A0=CODE TO JUMP TO
   MOVE.L   -80(A4),A7      ; STACK BACK
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)      ; (A5)
   MOVE.L   (A7)+,-76(A4)      ; PUT PREVIOUS HANDLER BACK (CODE)
   MOVE.L   (A7)+,-80(A4)      ; (STACK)
   JMP   (A0)
.1:   MOVE.L   -24(A4),A0      ; PERFORM CLEANUP(0)
   JMP   (A0)
   RTS

      I_RETHROW:
   TST.L   -84(A4)         ; NO RETHROW() IF EXC=0
   BEQ.S   .2
   MOVE.L   -76(A4),D0
   BEQ.S   .1
   MOVE.L   D0,A0         ; A0=CODE TO JUMP TO
   MOVE.L   -80(A4),A7      ; STACK BACK
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)      ; (A5)
   MOVE.L   (A7)+,-76(A4)      ; PUT PREVIOUS HANDLER BACK (CODE)
   MOVE.L   (A7)+,-80(A4)      ; (STACK)
   JMP   (A0)
.1:   MOVE.L   -24(A4),A0      ; PERFORM CLEANUP(0)
   JMP   (A0)
.2:   RTS

      I_SELECTLIST:
   MOVE.L   4(A7),A0      ; code
   MOVE.L   8(A7),A3      ; DESTLIST
   MOVE.L   A3,A6
   MOVE.L   12(A7),A1      ; SRClist
   MOVE.L   16(A7),A2      ; VAR
   MOVE.W   -2(A1),D2      ; COUNT
   BEQ.S   .1
   SUBQ.L   #1,D2
.L:   MOVE.L   (A1)+,(A2)
   MOVEM.L   D2/A0-A3/A6,-(A7)
   JSR   (A0)
   MOVEM.L   (A7)+,D2/A0-A3/A6
   TST.L   D0
   BEQ.S   .2
   MOVE.L   (A2),(A3)+
   ADDQ.W   #1,-2(A6)
   MOVE.W   -4(A6),D0
   CMP.W   -2(A6),D0
   BEQ.S   .1
.2:   DBRA   D2,.L
.1:   MOVE.W   -2(A6),D0
   EXT.L   D0
   RTS

      I_SETCOLOUR:
   MOVE.L   D3,A3
   MOVE.L   D4,A2
   MOVE.L   4(A7),D3
   MOVE.L   8(A7),D2
   MOVE.L   12(A7),D1
   MOVE.L   16(A7),D0
   MOVE.L   20(A7),A0
   ADD.W   #44,A0
   MOVE.L   4.W,A6
   MOVE.W   20(A6),D4
   MOVE.L   -52(A4),A6
   CMP.W   #39,D4
   BPL.S   .39
   LSR.L   #4,D1
   LSR.L   #4,D2
   LSR.L   #4,D3
   JSR   -$120(A6)
   MOVE.L   A3,D3
   MOVE.L   A2,D4
   RTS
.39:   MOVEQ   #24,D4
   LSL.L   D4,D1
   LSL.L   D4,D2
   LSL.L   D4,D3
   JSR   -852(A6)
   MOVE.L   A3,D3
   MOVE.L   A2,D4
   RTS

      I_NEWM:
   MOVE.L   8(A7),D0      ; COPY OF New()
   MOVE.L   4(A7),D1
   ADDQ.L   #8,D0
   MOVE.L   D0,D2
   MOVE.L   4.W,A6
   JSR   -198(A6)
   TST.L   D0
   BEQ.S   .1
   MOVE.L   D0,A0
   MOVE.L   -20(A4),(A0)
   MOVE.L   D2,4(A0)
   MOVE.L   D0,-20(A4)
   ADDQ.L   #8,D0
   RTS
.1:   MOVE.L   #"MEM",-84(A4)      ; COPY OF Raise()
   MOVE.L   -76(A4),D0
   BEQ.S   .2
   MOVE.L   D0,A0
   MOVE.L   -80(A4),A7
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)
   MOVE.L   (A7)+,-76(A4)
   MOVE.L   (A7)+,-80(A4)
   JMP   (A0)
.2:   MOVE.L   -24(A4),A0
   JMP   (A0)
   RTS

      I_BOUNDS:
   MOVE.L   12(A7),D0      ; D0=VALUE
   MOVE.L   4(A7),D1      ; D1=HIGHERBOUND FIRST
   CMP.L   D1,D0
   BMI.S   .1
   MOVE.L   D1,D0
   BRA.S   .X
.1:   MOVE.L   8(A7),D1      ; NOW TRY LOWERBOUND
   CMP.L   D1,D0
   BPL.S   .X
   MOVE.L   D1,D0
.X:   RTS

      I_REALF:
   MOVE.L   12(A7),A2
   CLR.W   -2(A2)
   MOVE.L   8(A7),D2      ; D2=FLOAT
   MOVE.L   -56(A4),A6
   MOVE.L   D2,D0
   JSR   -48(A6)
   BPL.S   .3
   MOVE.B   #"-",D0
   BSR.S   .ADDS
   MOVE.L   D2,D0
   JSR   -54(A6)         ; ABS
   MOVE.L   D0,D2
.3:   MOVE.L   4(A7),D1
   LSL.L   #2,D1
   MOVE.L   .RTAB(PC,D1.L),D1
   MOVE.L   D2,D0
   JSR   -66(A6)
   MOVE.L   D0,D2
   MOVEQ   #-1,D1
   BSR.W   .ADD
   CMP.L   #1,4(A7)
   BMI.S   .DONE
   MOVE.L   -56(A4),A6
   MOVE.B   #".",D0
   BSR.S   .ADDS
   MOVE.L   D2,D0
   JSR   -90(A6)         ; floor
   MOVE.L   D0,D1
   MOVE.L   D2,D0
   JSR   -72(A6)         ; sub
   MOVE.L   4(A7),D1
   SUBQ.L   #1,D1
   LSL.L   #2,D1
   MOVE.L   .TAB(PC,D1.L),D1
   JSR   -78(A6)         ; mul
   BSR.S   .ADD
.DONE:   MOVE.L   12(A7),D0
   RTS
.PROC:   MOVE.B   D0,(A3)+
   RTS
.ADDS:   MOVE.W   -2(A2),D1
   CMP.W   -4(A2),D1
   BPL.S   .1
   MOVE.B   D0,0(A2,D1.W)
   CLR.B   1(A2,D1.W)
   ADDQ.W   #1,-2(A2)
.1:   RTS
.RTAB:   DC.L   $3f000000,$3d4ccccd,$3ba3d70a   ; okay for 8 digits
   DC.L   $3a03126f,$3851b717,$36a7c5ac
   DC.L   $350637bd,$3356bf95,$31abcc77
.TAB:   DC.L   $41200000,$42c80000,$447a0000   ; same here
   DC.L   $461c4000,$47c35000,$49742400
   DC.L   $4b189680,$4cbebc20   ; ,$4cbebc20
.ADD:   MOVE.L   D1,-(A7)
   JSR   -90(A6)         ; FLOOR
   JSR   -30(A6)         ; FIX, D0=INT
   MOVE.L   (A7)+,D1
   LEA   -32(A7),A7
   MOVE.L   A7,A3
   LEA   .PROC(PC),A2
   LEA   16(A3),A0
   MOVE.B   #"%",(A0)+
   TST.L   D1
   BMI.S   .2
   MOVE.L   4+4+32(A7),D1
   ADD.W   #$30,D1
   MOVE.B   #"0",(A0)+
   MOVE.B   D1,(A0)+
   MOVE.B   #".",(A0)+
   MOVE.B   D1,(A0)+
.2:   MOVE.B   #"l",(A0)+
   MOVE.B   #"d",(A0)+
   CLR.B   (A0)+
   LEA   16(A3),A0
   LEA   28(A3),A1
   MOVE.L   D0,(A1)
   MOVE.L   4.W,A6
   JSR   -522(A6)
   MOVE.L   4+12+32(A7),A2      ; A2=STRING
   MOVE.L   A7,A3
.L:   MOVE.B   (A3)+,D0
   BEQ.S   .O
   BSR.W   .ADDS
   BRA.S   .L
.O:   LEA   32(A7),A7
   RTS

      I_REALVAL:
   MOVE.L   D3,A3
   MOVE.L   4(A7),A0      ; A0=STR
.L:   MOVE.B   (A0)+,D0
   CMP.B   #33,D0
   BMI.S   .L
   MOVEQ   #0,D1
   CMP.B   #"-",D0
   BNE.S   .NNEG
   MOVEQ   #-1,D1
   MOVE.B   (A0)+,D0
.NNEG:   MOVE.W   D1,A6         ; A6=SIGN
   MOVEQ   #0,D2         ; D2=DOTFLAG
   MOVEQ   #1,D1         ; D1=DIVDOT
   MOVEQ   #0,D3         ; D3=RESULT SOFAR
   SUB.L   A1,A1         ; A1=COUNT
.L2:   CMP.B   #"9"+1,D0
   BPL.S   .D
   CMP.B   #".",D0
   BEQ.S   .DOT
   CMP.B   #"0",D0
   BMI.S   .D
   EXT.W   D0
   EXT.L   D0
   SUB.W   #"0",D0
   MOVE.L   D3,A2
   LSL.L   #2,D3
   ADD.L   A2,D3
   LSL.L   #1,D3         ; D3*10
   ADD.L   D0,D3
   TST.L   D2
   BEQ.S   .1
   MOVE.L   D1,A2
   LSL.L   #2,D1
   ADD.L   A2,D1
   LSL.L   #1,D1         ; D1*10
.1:   ADDQ.L   #1,A1
.N:   MOVE.B   (A0)+,D0
   BRA.S   .L2
.D:   MOVE.L   A1,D0
   BEQ.S   .FAIL
   MOVE.L   A0,-(A7)
   MOVE.L   A6,A2
   MOVE.L   D1,D0
   MOVE.L   -56(A4),A6
   JSR   -36(A6)
   MOVE.L   D0,D2
   MOVE.L   D3,D0
   JSR   -36(A6)
   MOVE.L   D2,D1
   JSR   -84(A6)
   MOVE.L   A2,D1
   BEQ.S   .2
   JSR   -60(A6)
.2:   MOVE.L   A3,D3
   MOVE.L   (A7)+,D1
   SUBQ.L   #1,D1
   SUB.L   4(A7),D1
   RTS
.DOT:   TST.L   D2
   BNE.S   .D
   MOVEQ   #1,D2
   BRA.S   .N
.FAIL:   MOVEQ   #0,D1
   MOVEQ   #0,D0
   RTS

      I_FABS:
   MOVE.L   4(A7),D0
   MOVE.L   -56(A4),A6
   JSR   -54(A6)
   RTS

      I_FFLOOR:
   MOVE.L   4(A7),D0
   MOVE.L   -56(A4),A6
   JSR   -90(A6)
   RTS

      I_FCEIL:
   MOVE.L   4(A7),D0
   MOVE.L   -56(A4),A6
   JSR   -96(A6)
   RTS

      I_FSIN:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -36(A6)
   RTS

      I_FCOS:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -42(A6)
   RTS

      I_FTAN:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -48(A6)
   RTS

      I_FEXP:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -78(A6)
   RTS

      I_FLOG:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -84(A6)
   RTS

      I_FPOW:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7),D1
   MOVE.L   -60(A4),A6
   JSR   -90(A6)
   RTS

      I_FSQRT:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -96(A6)
   RTS

      I_FLOG10:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -126(A6)
   RTS

      I_FASTDISPOSE:         ; SEE ALSO: FASTDISPOSELIST!!!!!
   MOVE.L   8(A7),D0      ; parms(ptr,size)
   BEQ.S   .oute
   MOVE.L   D0,A0
   MOVE.L   4(A7),D0
   CMP.L   #257,D0
   BPL.S   .free
   ADDQ.L   #3,D0
   AND.W   #%1111111100,D0
   LEA   GLOBOFFNEWTAB(A4),A1
   ADD.L   D0,A1
   MOVE.L   (A1),(A0)
   MOVE.L   A0,(A1)
.oute:   RTS
.free:   SUBQ.L   #8,A0
   LEA   -20(A4),A1
.loop:   MOVE.L   (A1),D1
   BEQ.S   .out
   MOVE.L   A1,A2         ; ADR TO LINK BACK TO
   MOVE.L   D1,A1
   CMPA.L   D1,A0
   BNE.S   .loop
   MOVE.L   4(A1),D0      ; MEMSIZE
   MOVE.L   (A1),(A2)      ; LINK BACK
   MOVE.L   $4.W,A6
   JSR   -210(A6)      ; FREEMEM
.out:   RTS

      I_FASTNEW:
   MOVE.L   4(A7),D0      ; 1st arg = size 0..
   CMP.L   #257,D0
   BPL.S   .mem
   ADDQ.L   #3,D0
   AND.W   #%1111111100,D0
   BEQ.S   .outn
   MOVE.L   D0,A3
   LEA   GLOBOFFNEWTAB(A4),A0
   ADD.L   A3,A0
   MOVE.L   (A0),D0
   BEQ.S   .chop
   MOVE.L   D0,A1
   MOVE.L   (A1),(A0)
   MOVE.L   A3,D1
   LSR.W   #2,D1
   SUBQ.L   #1,D1
   MOVEQ   #0,D2
.clrl:   MOVE.L   D2,(A1)+
   DBRA   D1,.clrl
.outn:   RTS
.chop:   MOVE.L   CHOPMEM(A4),D0
   BEQ.S   .alloc
   MOVE.L   A3,D1
   SUB.L   D1,CHOPLEFT(A4)
   BMI.S   .alloc
   ADD.L   D1,CHOPMEM(A4)
   RTS
.alloc:   MOVE.L   #FMEMSIZE+8,D0
   BSR.S   .al
   MOVE.L   D0,CHOPMEM(A4)
   MOVE.L   #FMEMSIZE,CHOPLEFT(A4)
   BRA.S   .chop
.mem:   ADDQ.L   #8,D0
.al:   MOVE.L   D0,D2         ; COPY OF New()
   MOVE.L   #$10000,D1
   MOVE.L   $4.W,A6
   JSR   -198(A6)
   TST.L   D0
   BEQ.S   .raise
   MOVE.L   D0,A0
   MOVE.L   -20(A4),(A0)
   MOVE.L   D2,4(A0)
   MOVE.L   D0,-20(A4)
   ADDQ.L   #8,D0
   RTS
.raise:   MOVE.L   #"MEM",-84(A4)      ; COPY OF Raise()
   MOVE.L   -76(A4),D0
   BEQ.S   .clean
   MOVE.L   D0,A0
   MOVE.L   -80(A4),A7
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)
   MOVE.L   (A7)+,-76(A4)
   MOVE.L   (A7)+,-80(A4)
   JMP   (A0)
.clean:   MOVE.L   -24(A4),A0
   JMP   (A0)

      I_MIN:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7),D1
   CMP.L   D0,D1
   BMI.S   .1
   RTS
.1:   MOVE.L   D1,D0
   RTS

      I_MAX:
   MOVE.L   4(A7),D0
   MOVE.L   8(A7),D1
   CMP.L   D0,D1
   BPL.S   .1
   RTS
.1:   MOVE.L   D1,D0
   RTS

      I_OSTRCMP:
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0
   MOVE.L   8(A7),A1
   MOVE.L   12(A7),A0
   ADDQ.L   #1,D0
.1:   SUBQ.L   #1,D0
   BEQ.S   .3
   CMPM.B   (A0)+,(A1)+
   BGT.S   .2
   BMI.S   .4
   CMP.B   #0,-1(A0)
   BNE.S   .1
.3:   MOVEQ   #0,D0
   RTS
.2:   MOVEQ   #1,D0
   RTS
.4:   MOVEQ   #-1,D0
   RTS

      I_ASTRCOPY:
   MOVEQ   #0,D0
   MOVE.W   6(A7),D0
   BEQ.S   .x2
   MOVE.L   8(A7),A1
   MOVE.L   12(A7),A0
   ADDQ.L   #1,D0
.al:   SUBQ.L   #1,D0
   BEQ.S   .x
   MOVE.B   (A1)+,(A0)+
   BNE.S   .al
   BRA.S   .x2
.x:   CLR.B   -(A0)
.x2:   RTS


; Yet Another MileStone:
;
; Conservative Mark-Sweep Garbage-Collected Lisp-Cells in E
;
; speed of nrev500_10 test on 128k space (includes 1 collection):
;
; 4.5 x BinProLog, 15.5 x SBP,26 x Gofer
;
; TODO:
; + stack checking?
; + chunk changable -> own chunksize var. only change if <>0
; + DBRA limit? -> now DBL. should be able to do >2meg
; + collect delegates? -> no.
; + request non-empty mem.
; + previous freelist is added ok now in new.
; + Cell() function
; - multiple args
; - special syntax + pattern matching
; - Root(x) etc.
; - 25% not ideal with small spaces and growing cell usage.


CELLSMEM   = -108
CELLSFREE   = -112
CHUNKSIZE   = -116
CHUNK      = 128*1024      ; must be multiple of 256, >1024

; mem layout:
;
; OBJECT cellmem
;   next:PTR TO cellmem         ; 0
;   end:PTR TO cellmem+SIZEOF cellmem   ; 4
;   cells[n]:ARRAY OF cell      ; 8
; ENDOBJECT
;   bits[n]:ARRAY OF BIT

      I_CELL:
   MOVE.L   4(A7),D0
   BEQ.S   .false         ; NIL
   MOVE.L   D0,D1
   AND.W   #%111,D1
   BNE.S   .false         ; not cell-aligned
   MOVE.L   CELLSMEM(A4),A0
.cloop:   CMP.L   A0,D0
   BMI.S   .cnext         ; lower than bottom
   CMP.L   4(A0),D0
   BPL.S   .cnext         ; higher than top
   MOVEQ   #-1,D0
   RTS
.cnext:   MOVE.L   (A0),A0
   MOVE.L   A0,D1
   BNE.S   .cloop
.false:   MOVEQ   #0,D0
   RTS

      I_FREECELLS:
   LEA   CELLSFREE(A4),A0
   MOVEQ   #0,D0
.ccl:   MOVE.L   (A0),D1
   BEQ.S   .cco
   ADDQ.L   #1,D0
   MOVE.L   D1,A0
   BRA.S   .ccl
.cco:   RTS

      I_SETCHUNKSIZE:
   TST.L   CHUNKSIZE(A4)
   BNE.S   .chd
   MOVE.L   4(A7),D0
   MOVEQ   #10,D1
   LSL.L   D1,D0
   MOVE.L   D0,CHUNKSIZE(A4)
.chd:   RTS

      I_CAR:
   MOVE.L   4(A7),A0
   MOVE.L   (A0),D0
   RTS

      I_CDR:
   MOVE.L   4(A7),A0
   MOVE.L   4(A0),D0
   RTS

      I_CONS:
   MOVE.L   CELLSFREE(A4),D0   ; Yep, this is IT!
   BEQ.S   .gc
   MOVE.L   D0,A0
   MOVE.L   (A0),A2
   MOVE.L   12(A7),(A0)
   MOVE.L   8(A7),4(A0)
   MOVE.W   6(A7),D1
   BEQ.S   .1
   LSR.W   #2,D1
   SUBQ.W   #1,D1
   LEA   16(A7),A1
.2:   MOVE.L   A2,D2
   BEQ.S   .gc
   EXG   D0,D2
   MOVE.L   (A2),A3
   MOVE.L   D2,4(A2)
   MOVE.L   (A1)+,(A2)
   MOVE.L   A3,A2
   DBRA   D1,.2
.1:   MOVE.L   A2,CELLSFREE(A4)
   RTS

.gc:   CLR.L   CELLSFREE(A4)
   MOVE.L   CELLSMEM(A4),D0
   BEQ   .new
   MOVEM.L   D3-D7,-(A7)      ; roots too
   MOVE.L   D0,D7         ; D7=space
   MOVE.L   A7,A0         ; A0=roots
.grab:   CMPA.L   A0,A4         ; grab roots until A4
   BLE   .sweep
   MOVE.L   (A0)+,D0
   PEA   .grab(PC)
.trace:   BEQ.S   .ex         ; NIL
   MOVE.L   D0,D1
   AND.W   #%111,D1
   BNE.S   .ex         ; not cell-aligned
   MOVE.L   D7,A2
.tloop:   CMP.L   A2,D0
   BMI.S   .tnext         ; lower than bottom
   CMP.L   4(A2),D0
   BPL.S   .tnext         ; higher than top
   MOVE.L   D0,A1
   SUB.L   A2,D0
   LSR.L   #3,D0
   MOVE.L   D0,D1
   LSR.L   #3,D0
   MOVE.L   4(A2),A3      ; mark the sucker
   BSET   D1,0(A3,D0.L)      ; already marked? great!
   BNE.S   .ex
   MOVE.L   4(A1),-(A7)      ; save cdr for later
   MOVE.L   A7,D0
   SUBQ.L   #8,D0
   CMP.L   -64(A4),D0
   BMI   .raises
   MOVE.L   (A1),D0         ; go do car now
   BSR.S   .trace         ; OOPS! stack!!!
   MOVE.L   (A7)+,D0
   BRA.S   .trace
.tnext:   MOVE.L   (A2),A2
   MOVE.L   A2,D1
   BNE.S   .tloop
.ex:   RTS

.sweep:   MOVEQ   #0,D0         ; D0=num cells collected
   MOVE.L   CELLSFREE(A4),A6   ; A6=freelist
.sl:   MOVE.L   D7,A0         ; A0=space
   MOVE.L   4(A0),A1      ; A1=endspace
   MOVE.L   (A0),D5         ; D5=next
   MOVEQ   #0,D1         ; D1=current bit
.sloop:   ADDQ.L   #8,A0
   ADDQ.L   #1,D1
   CMPA.L   A0,A1
   BEQ.S   .snext
   MOVE.L   D1,D2
   LSR.L   #3,D2
   BCLR   D1,0(A1,D2.L)
   BNE.S   .sloop
   ADDQ.L   #1,D0
   MOVE.L   A6,(A0)         ; sweep the sucker!
   MOVE.L   A0,A6
   BRA.S   .sloop
.snext:   MOVE.L   D5,D7
   BNE.S   .sl
   MOVE.L   A6,CELLSFREE(A4)
   MOVEM.L   (A7)+,D3-D7
   MOVE.L   CHUNKSIZE(A4),D1
   LSR.L   #5,D1         ; /4 = 25% must be empty
   CMP.L   D1,D0
   BMI.S   .new
   BRA   I_CONS

.new:   MOVE.L   CHUNKSIZE(A4),D0
   BNE.S   .cs
   MOVE.L   #CHUNK,D0
   MOVE.L   D0,CHUNKSIZE(A4)
.cs:   MOVE.L   D0,D1
   LSR.L   #6,D1         ; markspace
   ADD.L   D1,D0
   BSR   .alloc
   MOVE.L   D0,A0
   MOVE.L   CELLSMEM(A4),(A0)   ; previous cellspace
   ADD.L   CHUNKSIZE(A4),D0
   MOVE.L   D0,4(A0)      ; end_of_cells
   MOVE.L   D0,A2         ; markspace
   MOVE.L   A0,CELLSMEM(A4)
   MOVE.L   CHUNKSIZE(A4),D0   ; now chop free cells
   LSR.L   #5,D0
   SUBQ.L   #2,D0         ; numcells
   MOVE.L   CELLSFREE(A4),A1
   ADDQ.L   #8,A0
   MOVE.L   A1,(A0)
.chop:   LEA   8(A0),A1      ; freelist in new cell
   MOVE.L   A0,(A1)         ; loop unrolled!!!
   LEA   8(A1),A0      ; 20 cycles per cell!!!
   MOVE.L   A1,(A0)         ; 4 cells in one loop
   LEA   8(A0),A1
   MOVE.L   A0,(A1)
   LEA   8(A1),A0
   MOVE.L   A1,(A0)
   DBRA   D0,.chop
   SUB.L   #$10000,D0
   BCC.S   .chop
   MOVE.L   A0,CELLSFREE(A4)
   MOVE.L   CHUNKSIZE(A4),D0   ; now clear mark space
   LSR.L   #8,D0
   SUBQ.L   #1,D0
   MOVEQ   #0,D1
.clmark:MOVE.L   D1,(A2)+
   DBRA   D0,.clmark
   SUB.L   #$10000,D0
   BCC.S   .clmark
   BRA   I_CONS         ; try again!

.alloc:   ADDQ.L   #8,D0
   MOVE.L   D0,D2         ; COPY OF New()
   MOVEQ   #0,D1         ; MOVE.L   #$10000,D1
   MOVE.L   $4.W,A6
   JSR   -198(A6)
   TST.L   D0
   BEQ.S   .raise
   MOVE.L   D0,A0
   MOVE.L   -20(A4),(A0)
   MOVE.L   D2,4(A0)
   MOVE.L   D0,-20(A4)
   ADDQ.L   #8,D0
   RTS

.raises:MOVE.L   #"STCK",-84(A4)
   BRA.S   .rraise
.raise:   MOVE.L   #"MEM",-84(A4)      ; COPY OF Raise()
.rraise:MOVE.L   -76(A4),D0
   BEQ.S   .clean
   MOVE.L   D0,A0
   MOVE.L   -80(A4),A7
   MOVE.L   -88(A4),A5
   MOVE.L   (A7)+,-88(A4)
   MOVE.L   (A7)+,-76(A4)
   MOVE.L   (A7)+,-80(A4)
   JMP   (A0)
.clean:   MOVE.L   -24(A4),A0
   JMP   (A0)

      I_FASTDISPOSELIST:      ; ALMOST SAME AS FASTDIPOSE!!!!!
   MOVE.L   4(A7),D0      ; parms(ptr)
   BEQ.S   .oute
   MOVE.L   D0,A0
   MOVE.L   -(A0),D0
   EXT.L   D0
   LSL.L   #2,D0
   ADDQ.L   #4,D0
   CMP.L   #257,D0
   BPL.S   .free
   ADDQ.L   #3,D0
   AND.W   #%1111111100,D0
   LEA   GLOBOFFNEWTAB(A4),A1
   ADD.L   D0,A1
   MOVE.L   (A1),(A0)
   MOVE.L   A0,(A1)
.oute:   RTS
.free:   SUBQ.L   #8,A0
   LEA   -20(A4),A1
.loop:   MOVE.L   (A1),D1
   BEQ.S   .out
   MOVE.L   A1,A2         ; ADR TO LINK BACK TO
   MOVE.L   D1,A1
   CMPA.L   D1,A0
   BNE.S   .loop
   MOVE.L   4(A1),D0      ; MEMSIZE
   MOVE.L   (A1),(A2)      ; LINK BACK
   MOVE.L   $4.W,A6
   JSR   -210(A6)      ; FREEMEM
.out:   RTS

      I_FATAN:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -30(A6)
   RTS

      I_FSINCOS:
   MOVE.L   8(A7),A0
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -54(A6)
   RTS

      I_FSINH:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -60(A6)
   RTS

      I_FCOSH:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -66(A6)
   RTS

      I_FTANH:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -72(A6)
   RTS

      I_FTIEEE:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -102(A6)
   RTS

      I_FFIEEE:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -108(A6)
   RTS

      I_FASIN:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -114(A6)
   RTS

      I_FACOS:
   MOVE.L   4(A7),D0
   MOVE.L   -60(A4),A6
   JSR   -120(A6)
   RTS

   ; new ECX internal functions v48

      I_OBJNAME:
   MOVE.L 4(A7),A0
   MOVE.L -(A0),A0
   MOVE.L -8(A0),D0
   RTS

      I_OBJSIZE:
   MOVE.L 4(A7),A0
   MOVE.L -(A0),A0
   MOVE.L -(A0),D0
   RTS

   ; v49

      I_DEBUGF:
.3:LEA   8(A7),A1
   MOVE.L   4(A7),D0
   MOVE.L   8(A7,D0.L),A0
   LEA     .1(PC),A2
   MOVE.L  4.W,A6
   JSR     -522(A6)
   RTS
.1:
   MOVE.L 4.W,A6
   JSR -516(A6) ; RawPutChar
   RTS

   ; v50

   I_DOUBLE: ; (adr) = fp1
   MOVE.L 4(A7),A0
   FMOVE.D (A0),FP1
   RTS

   I_PUTDOUBLE: ; (adr,fp1) = adr
   MOVE.L 4(A7),A0
   FMOVE.D FP1,(A0)
   RTS

   I_PTR:
   MOVE.L   4(A7),A0
   MOVE.L   (A0),D0
   RTS

   I_PUTPTR:
   MOVE.L   8(A7),A0
   MOVE.L   4(A7),(A0)
   RTS

   I_BYTE:
   MOVE.L   4(A7),A0
   MOVE.B   (A0),D0
   EXTB.L   D0
   RTS

   I_PUTBYTE:
   MOVE.L   8(A7),A0
   MOVE.B   7(A7),(A0)
   RTS

   I_WORD:
   MOVE.L   4(A7),A0
   MOVEQ    #0,D0
   MOVE.W   (A0),D0
   RTS

   I_PUTWORD:
   MOVE.L   8(A7),A0
   MOVE.W   6(A7),(A0)
   RTS

   I_FLOAT: ; (adr) = fp1
   MOVE.L 4(A7),A0
   FMOVE.S (A0),FP1
   RTS

   I_PUTFLOAT: ; (adr,fp1) = adr
   MOVE.L 4(A7),A0
   FMOVE.S FP1,(A0)
   RTS

   ; v51

   I_REAL: ; (adr) = fp1
   MOVE.L 4(A7),A0
   FMOVE.D (A0),FP1
   RTS

   I_PUTREAL: ; (adr,fp1) = adr
   MOVE.L 4(A7),A0
   FMOVE.D FP1,(A0)
   RTS

   ; v56

   I_NEWLIST: ; (list)
   MOVE.L 4(A7),D0
   MOVE.L D0,A0
   ADDQ.L #4,D0
   MOVE.L D0,(A0)  ; list.head := list+4
   CLR.L 4(A0)      ; list.tail := NIL
   MOVE.L A0,8(A0) ; list.tailpred := list
   RTS

   I_STRING: ; new version that takes mempool as optional arg
   MOVE.L 4(A7),D0
   BNE .2
   MOVE.L -120(a4),D0 ;mempool
.2:
   MOVE.L D0,A0
   MOVE.L 8(A7),D0
   ADD.L #16,D0
   ANDI.L #$FFFC,D0
   MOVE.L D0,-(A7)
   MOVE.L 4.W,A6
   JSR -708(A6)
   MOVE.L (A7)+,D1
   TST.L D0
   BEQ .3
   MOVE.L D0,A0
   MOVE.L D1,(A0)+
   CLR.L (A0)+ ; memory is NOT cleared!
   MOVE.L 8(A7),D1
   SWAP D1
   MOVE.L D1,(A0)+
   MOVE.L A0,D0
.3:
   RTS

   I_LIST:            ; new version that takes mempool as optional arg
   MOVE.L 4(A7),A0
   MOVE.L A0,D0
   BNE .2
   MOVE.L -120(A4),A0  ; mempool
.2:
   MOVE.L 8(A7),D0
   LSL.L #2,D0
   ADD.L #12,D0
   MOVE.L D0,-(A7)
   MOVE.L 4.W,A6
   JSR -708(A6)
   MOVE.L (A7)+,D1
   TST.L D0
   BEQ .3
   MOVE.L D0,A0
   MOVE.L D1,(A0)+
   CLR.L (A0)+ ; memory is NOT cleared!
   MOVE.L 8(A7),D1
   SWAP D1
   MOVE.L D1,(A0)+
   MOVE.L A0,D0
.3:
   RTS


   I_DISPL:           ; new version that takes mempool as optional arg
   MOVE.L 8(A7),A3
.3:
   MOVE.L A3,D0
   BEQ .4
   LEA -12(A3),A1
   MOVE.L -8(A3),A3
   MOVE.L 4(A7),A0
   MOVE.L A0,D0
   BNE .2
   MOVE.L -120(A4),A0
.2:
   MOVE.L (A1),D0
   MOVE.L 4.W,A6
   JSR -714(A6)
   BRA .3
.4:
   RTS

   ; v58,20101212

I_WIDE: ; (adr) = fp1
   MOVE.L 4(A7),A0
   FMOVE.D (A0),FP1
   RTS

I_PUTWIDE: ; (adr,wide) = adr
   MOVE.L 12(A7),A0
   FMOVE.D 4(A7),FP1
   FMOVE.D FP1,(A0)
   MOVE.L A0,D0
   RTS

I_ULONGTOWIDE: ;
   MOVE.L 4(A7),D0
   MOVE.L D0,-4(A7)
   CLR.L D0
   MOVE.L D0,-8(A7)
   FMOVE.D -8(A7),FP1
   RTS


   I_CODEEND:

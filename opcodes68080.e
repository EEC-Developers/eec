-> EEC/opcodes68080.e

/* EEC by Samuel D. Crow [samuraileumas yahoo com] is Copyright (c) 2019-2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

EXPORT ENUM M0,M1,M2,M3,M4,M5,M6,M7
EXPORT ENUM SIZE_B,SIZE_W,SIZE_L   -> was B,W,L
EXPORT ENUM T,F,HI,LS,CC,CS,NE,EQ,VC,VS,PL,MI,GE,LT,GT,LE

EXPORT ENUM FL,FS,FX,FP,FW,FD,FB
EXPORT ENUM FCF,FCEQ,FCOGT,FCOGE,FCOLT,FCOLE,FCOGL,
            FCOR,FCUN,FCUEQ,FCUGT,FCUGE,FCULT,FCULE,
            FCNE,FCT,FCSF,FCSEQ,FCGT,FCGE,FCLT,FCLE,
            FCGL,FCGLE,FCNGLE,FCNGL,FCNLE,FCNLT,FCNGE,FCNGT

EXPORT ENUM ADDRERR,NOP,DXDX,DXAX,DXAXP,DXAXPI,DXAXPD,DXAXPOFS,
  DXAXPX,AXDX,AXAX,IMMAX,IMMQAX,LEAAXPAX,ABSWAX,XDXDX,XAXPDAXPD

-> Abstract base class
OBJECT m68k
  codeptr:PTR TO WORD, -> NOTE: Unsigned 16-bit members
  me:CHAR, -> Enumerated addressing mode identifier
  size:CHAR, -> operand size in number of bytes
  op:CHAR, -> base operand is 4 bits but allow for psuedo ops
  mode:CHAR -> addressing mode encoding is 3 bits but allow for psuedo modes
ENDOBJECT

-> instruction length in bytes
PROC getLength() OF m68k IS EMPTY

-> support to generate one operation
PROC output(buf) OF m68k
  buf := buf OR Shl(self.op AND 15,12) OR Shl(self.mode AND 7,3)
  self.codeptr[] := buf AND $FFFF
  self.codeptr := self.codeptr + self.getLength()
ENDPROC

PROC extW(da,reg,wl,scale,bd)
   DEF t=NIL
   t := Shl(Shl(Shl(Shl(da,3) OR reg, 1) OR wl,2) OR scale, 9) OR (bd AND $FF)
ENDPROC t

-> 1/2/4/8 to 0/1/2/3
#define scDwn(sc) ListItem([0,0,1,0,2,0,0,0,3],sc)

->#define BD32EXT extW(0,0,0,0,0,1,3,0)
#define IxExt(idrx,scale,bd) extW(0,idrx,1,scDwn(scale), bd)

PROC axSize(s) IS ListItem([0,3,7],s)

/**********************************
******** Addressing modes *********
**********************************/
OBJECT dxdx OF m68k
  dx1:CHAR,
  dx2:CHAR
ENDOBJECT

PROC getLength() OF dxdx IS 2

-> constructor
PROC make(s,dx1,dx2) OF dxdx
  self.size := s
  self.me := DXDX
  self.mode := M0
  self.dx1 := dx1
  self.dx2 := dx2
ENDPROC

PROC output() OF dxdx
  SUPER output(Shl(self.dx2,9) OR Shl(self.size,6) OR self.dx1)
ENDPROC

OBJECT dxax OF m68k
  dx:CHAR,
  ax:CHAR
ENDOBJECT

PROC getLength() OF dxax IS 2

-> constructor
PROC make(s,dx,ax) OF dxax
  self.size := s
  self.me := DXAX
  self.mode := M0
  self.dx := dx
  self.ax := ax
ENDPROC

PROC output() OF dxax
  SUPER output(Shl(self.ax,9) OR Shl(self.size,6) OR self.dx)
ENDPROC

OBJECT dxaxp OF m68k
  dx:CHAR,
  ax:CHAR
ENDOBJECT

PROC getLength() OF dxaxp IS 2

-> constructor
PROC make(s,dx,ax) OF dxaxp
  self.size := s
  self.me := DXAXP
  self.mode := M2
  self.dx := dx
  self.ax := ax
ENDPROC

PROC output() OF dxaxp
  SUPER output(Shl(self.ax,9) OR Shl(self.size OR 4,6) OR self.dx)
ENDPROC

OBJECT dxaxpi OF m68k
  dx:CHAR,
  ax:CHAR
ENDOBJECT

PROC getLength() OF dxaxpi IS 2

-> constructor
PROC make(s,dx,ax) OF dxaxpi
  self.size := s
  self.me := DXAXPI
  self.mode := M3
  self.dx := dx
  self.ax := ax
ENDPROC

PROC output() OF dxaxpi
  SUPER output(Shl(self.ax,9) OR Shl(self.size OR 4,6) OR self.dx)
ENDPROC

OBJECT dxaxpd OF m68k
  dx:CHAR,
  ax:CHAR
ENDOBJECT

PROC getLength() OF dxaxpd IS 2

-> constructor
PROC make(s,dx,ax) OF dxaxpd
  self.size := s
  self.me := DXAXPD
  self.mode := M4
  self.dx := dx
  self.ax := ax
ENDPROC

PROC output() OF dxaxpd
  SUPER output(Shl(self.ax,9) OR Shl(self.size OR 4,6) OR self.dx)
ENDPROC

OBJECT dxaxpofs OF m68k
  dx:CHAR,
  ax:CHAR,
  ofs:INT
ENDOBJECT

PROC getLength() OF dxaxpofs IS 4

-> constructor
PROC make(s,dx,ax,ofs) OF dxaxpofs
  self.size := s
  self.me := DXAXPOFS
  self.mode := M5
  self.dx := dx
  self.ax := ax
  self.ofs := ofs
ENDPROC

PROC output() OF dxaxpofs
  self.codeptr[1] := ofs
  SUPER output(Shl(self.ax,9) OR Shl(self.size OR 4,6) OR self.dx)
ENDPROC

OBJECT dxaxpx OF m68k
  dx:CHAR,
  ax:CHAR,
  idrx:CHAR,
  scale:CHAR,
  d:INT
ENDOBJECT

PROC getLength() OF dxaxpx IS 4

-> constructor
PROC make(s,dx,ax,idrx,scale,d) OF dxaxpx
  self.size := s
  self.me := DXAXPX
  self.mode := M6
  self.dx := dx
  self.ax := ax
  self.idrx := idrx
  self.scale := scale
  self.d := d
ENDPROC

PROC output() OF dxaxpx
  self.codeptr[1] := IxExt(self.idrx, self.scale, self.d)
  SUPER output(Shl(self.ax,9) OR Shl(self.size OR 4,6) OR self.dx)
ENDPROC

OBJECT axdx OF m68k
  ax:CHAR,
  dx:CHAR
ENDOBJECT

PROC getLength() OF axdx IS 2

-> constructor
PROC make(s,ax,dx) OF axdx
  self.size := s
  self.mode := M1
  self.ax := ax
  self.dx := dx
ENDPROC

PROC output() OF axdx
  SUPER output(Shl(self.dx,9) OR Shl(self.size,6) OR self.ax)
ENDPROC

OBJECT axax OF m68k
  ax1:CHAR,
  ax2:CHAR
ENDOBJECT

PROC getLength() OF axax IS 2

-> constructor
PROC make(s,ax1,ax2) OF axax
  self.size := s
  self.mode := M1
  self.ax1 := ax1
  self.ax2 := ax2
ENDPROC

PROC output() OF axax
  SUPER output(Shl(self.ax2,9) OR Shl(axSize(self.size),6) OR self.ax1)
ENDPROC

OBJECT immax OF m68k
  imm,
  ax:CHAR,
  len:CHAR
ENDOBJECT

-> constructor
PROC make(s,imm,ax) OF immax
  self.ax := ax
  self.imm := imm
  IF s = SIZE_L
    self.size := SIZE_L
    SELECT imm
      CASE 0
        -> NOP for add
        -> SUBA ax,ax for load
        self.me := ADDRERR
      CASE 1 TO 8
        self.len := 2
        -> ADDQ.L #imm,ax for add
        -> MOVEQ.L #imm,ax for load
        self.me := IMMQAX
      CASE 9 TO 32767
        self.len := 4
        -> LEA (#imm,ax),ax for add
        -> MOVEA.L #imm.W,ax for load
        self.me := ABSWAX
      CASE -32768 TO -1
        SELF.len := 4
        -> LEA (#imm,ax),ax for add
        -> MOVEA.L #imm.W,ax for load
        self.me := ABSWAX
      DEFAULT
        self.len := 6
        -> ADDA.L #imm,ax for add
        -> MOVEA.L #imm,ax for load
        self.me := IMMAX
    ENDSELECT
  ELSE
    self.size := SIZE_W
    IF (imm>0) AND (imm<9)
      self.len := 2
      -> MOVEQ.W #imm,ax
      self.me := IMMQAX
    ELSE
      self.len := 4
      -> LEA (#imm,ax),ax for add
      -> MOVEA.W #imm,ax for load
      self.me := ABSWAX
    ENDIF
  ENDIF
ENDPROC

PROC getLength() OF immax IS self.len

-> bounce psuedo op to the next level
PROC output(buf) OF immax IS SUPER.output(buf)

OBJECT xdxdx OF m68k
  dx1:CHAR,
  dx2:CHAR
ENDOBJECT

PROC getLength() OF xdxdx IS 2

-> constructor
PROC make(s,dx1,dx2) OF xdxdx
  self.size := s
  self.me := XDXDX
  self.mode := M0
  self.dx1 := dx1
  self.dx2 := dx2
ENDPROC

PROC output() OF xdxdx
  SUPER output(Shl(self.dx2,9) OR Shl(self.size OR 4,6) OR self.dx1)
ENDPROC

OBJECT xaxpdaxpd OF m68k
  ax1:CHAR,
  ax2:CHAR
ENDOBJECT

PROC getLength() OF xaxpdaxpd IS 2

-> constructor
PROC make(s,ax1,ax2) OF xaxpdaxpd
  self.size := s
  self.me := XAXPDAXPD
  self.mode := M1
  self.ax1 := ax1
  self.ax2 := ax2
ENDPROC

PROC output() OF xaxpdaxpd
  SUPER output(Shl(self.dx2,9) OR Shl(self.size OR 4,6) OR self.dx1)
ENDPROC


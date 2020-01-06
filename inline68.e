OPT MODULE
OPT PREPROCESS
OPT LARGE

MODULE '*common'
MODULE '*compiler'
MODULE '*opcodes68'
MODULE '*assembler'

#define A_ g_od[0]
#define B_ g_od[1]
#define C_ g_od[2]
#define D_ g_od[3]
#define E_ g_od[4]
#define F_ g_od[5]
#define G_ g_od[6]
#define H_ g_od[7]

#define GLOBREG g_globreg

EXPORT OBJECT inst68
   nrofops:LONG
   longops:PTR TO LONG
   wordops:PTR TO LONG
   byteops:PTR TO LONG
   singleops:PTR TO LONG
   doubleops:PTR TO LONG
   defasize:LONG -> 0=.L, 1=.W, 2=.B, 3=.S, 4=.D
   flags:LONG
ENDOBJECT

EXPORT DEF g_od:PTR TO LONG, g_punct -> shared with codegen.e
EXPORT DEF g_globreg -> shared with codegen.e


EXPORT PROC add68KRegs()

   DEF hln:PTR TO hln, r:PTR TO reg

r:=[IDENT_REG,DRX,0,0,'D0',
    IDENT_REG,DRX,1,0,'D1',
    IDENT_REG,DRX,2,0,'D2',
    IDENT_REG,DRX,3,0,'D3',
    IDENT_REG,DRX,4,0,'D4',
    IDENT_REG,DRX,5,0,'D5',
    IDENT_REG,DRX,6,0,'D6',
    IDENT_REG,DRX,7,0,'D7',

    IDENT_REG,ARX,0,0,'A0',
    IDENT_REG,ARX,1,0,'A1',
    IDENT_REG,ARX,2,0,'A2',
    IDENT_REG,ARX,3,0,'A3',
    IDENT_REG,ARX,4,0,'A4',
    IDENT_REG,ARX,5,0,'A5',
    IDENT_REG,ARX,6,0,'A6',
    IDENT_REG,ARX,7,0,'A7',

    IDENT_REG,FPX,0,0,'FP0',
    IDENT_REG,FPX,1,0,'FP1',
    IDENT_REG,FPX,2,0,'FP2',
    IDENT_REG,FPX,3,0,'FP3',
    IDENT_REG,FPX,4,0,'FP4',
    IDENT_REG,FPX,5,0,'FP5',
    IDENT_REG,FPX,6,0,'FP6',
    IDENT_REG,FPX,7,0,'FP7',
    NIL]:reg


   WHILE r.identID
      hln := getLabelHLN(r.name)
      hln.ident := r
      r++
   ENDWHILE

ENDPROC

#define DEFBUNCHOFVARS DEF hln:PTR TO hln, inst:PTR TO inst68, \
       l, w, b, dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp

EXPORT PROC add68KInstructions()

   addmoveinst()
   addmoveainst()
   addleainst()
   addnopinst()
   addrtsinst()
   addaddinst()
   addaddainst()
   addaddiinst()
   addsubinst()
   addsubainst()
   addsubiinst()
   addaddqinst()
   addsubqinst()
   addclrinst()
   addcmpinst()
   addcmpainst()
   addcmpiinst()
   addextinst()
   addextbinst()
   addneginst()
   addandinst()
   addandiinst()
   addorinst()
   addoriinst()
   addeorinst()
   addeoriinst()
   addnotinst()
   addmoveqinst()
   addpeainst()
   addunlkinst()
   addrtdinst()
   addtstinst()
   addaslinst()
   addasrinst()
   addlslinst()
   addlsrinst()
   addrolinst()
   addrorinst()
   addroxlinst()
   addroxrinst()
   addswapinst()
   addexginst()
   addlinkinst()
   addbsrinst()
   addbccinst()
   adddbccinst()
   addmoveminst()
   addmulsinst()
   addmuluinst()
   adddivsinst()
   adddivuinst()
   addsccinst()
   addjsrinst()
   addjmpinst()
   addaddxinst()
   addsubxinst()
   addrtrinst()
   addrteinst()
   addnegxinst()
   addcmpminst()

   /*
      fxxx
   */

ENDPROC

PROC addmoveinst()

   DEFBUNCHOFVARS

    dx:=[`movedxdx(2,A_,B_),
           `movedxax(2,A_,B_),
           `movedxaxp(2,A_,B_),
           `movedxaxpi(2,A_,B_),
           `movedxaxpd(2,A_,B_),
           `movedxaxpofs(2,A_,B_,C_),
           `moveldxaxplab(A_,B_,C_),
           `movedxaxpx(2,A_,B_,C_,D_,E_)]

      ax:=[`moveaxdx(2,A_,B_),
           `moveaxax(2,A_,B_),
           `moveaxaxp(2,A_,B_),
           `moveaxaxpi(2,A_,B_),
           `moveaxaxpd(2,A_,B_),
           `moveaxaxpofs(2,A_,B_,C_),
           `movelaxaxplab(A_,B_,C_),
           `moveaxaxpx(2,A_,B_,C_,D_,E_)]

     axp:=[`moveaxpdx(2,A_,B_),
           `moveaxpax(2,A_,B_),
           `moveaxpaxp(2,A_,B_),
           `moveaxpaxpi(2,A_,B_),
           `moveaxpaxpd(2,A_,B_),
           `moveaxpaxpofs(2,A_,B_,C_),
           `movelaxpaxplab(A_,B_,C_),
           `moveaxpaxpx(2,A_,B_,C_,D_,E_)] -> axpx

    axpi:=[`moveaxpidx(2,A_,B_),
           `moveaxpiax(2,A_,B_),
           `moveaxpiaxp(2,A_,B_),
           `moveaxpiaxpi(2,A_,B_),
           `moveaxpiaxpd(2,A_,B_),
           `moveaxpiaxpofs(2,A_,B_,C_),
           `movelaxpiaxplab(A_,B_,C_),
           `moveaxpiaxpx(2,A_,B_,C_,D_,E_)]

    axpd:=[`moveaxpddx(2,A_,B_),
           `moveaxpdax(2,A_,B_),
           `moveaxpdaxp(2,A_,B_),
           `moveaxpdaxpi(2,A_,B_),
           `moveaxpdaxpd(2,A_,B_),
           `moveaxpdaxpofs(2,A_,B_,C_),
           `movelaxpdaxplab(A_,B_,C_),
           `moveaxpdaxpx(2,A_,B_,C_,D_,E_)]

    axpo:=[`moveaxpofsdx(2,A_,B_,C_),
           `moveaxpofsax(2,A_,B_,C_),
           `moveaxpofsaxp(2,A_,B_,C_),
           `moveaxpofsaxpi(2,A_,B_,C_),
           `moveaxpofsaxpd(2,A_,B_,C_),
           `moveaxpofsaxpofs(2,A_,B_,C_,D_),
           `movelaxpofsaxplab(A_,B_,C_,D_),
           `moveaxpofsaxpx(2,A_,B_,C_,D_,E_,F_)]

    axpl:=[`movelaxplabdx(A_,B_,C_),
           `movelaxplabax(A_,B_,C_),
           `moveaxplabaxp(2,A_,B_,C_),
           `movelaxplabaxpi(A_,B_,C_),
           `movelaxplabaxpd(A_,B_,C_),
           `moveaxplabaxpofs(2,A_,B_,C_,D_),
           `movelaxplabaxplab(A_,B_,C_,D_),
           `moveaxplabaxpx(2,A_,B_,C_,D_,E_,F_)]

    axpx:=[`moveaxpxdx(2,A_,B_,C_,D_,E_),
           `moveaxpxax(2,A_,B_,C_,D_,E_),
           `moveaxpxaxp(2,A_,B_,C_,D_,E_),
           `moveaxpxaxpi(2,A_,B_,C_,D_,E_),
           `moveaxpxaxpd(2,A_,B_,C_,D_,E_),
           `moveaxpxaxpofs(2,A_,B_,C_,D_,E_,F_),
           `movelaxpxaxplab(A_,B_,C_,D_,E_,F_),
           `moveaxpxaxpx(2,A_,B_,C_,D_,E_,F_,G_,H_)]

     imm:=[`movelimmdx(A_,B_),
           `movelimmax(A_,B_),
           `movelimmaxp(A_,B_),
           `movelimmaxpi(A_,B_),
           `movelimmaxpd(A_,B_),
           `movelimmaxpofs(A_,B_,C_),
           `movelimmaxplab(A_,B_,C_),
           `movelimmaxpx(A_,B_,C_,D_,E_)]

     pcp:=[`movelpcplabdx(A_,B_),
           `movelpcplabax(A_,B_),
           `movelpcplabaxp(A_,B_),
           `movelpcplabaxpi(A_,B_),
           `movelpcplabaxpd(A_,B_),
           `movelpcplabaxpofs(A_,B_,C_),
           `movelpcplabaxplab(A_,B_,C_),
           NIL]

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


      dx:=[`movedxdx(1,A_,B_),
           `movedxax(1,A_,B_),
           `movedxaxp(1,A_,B_),
           `movedxaxpi(1,A_,B_),
           `movedxaxpd(1,A_,B_),
           `movedxaxpofs(1,A_,B_,C_),
           NIL,
           `movedxaxpx(1,A_,B_,C_,D_,E_)]

      ax:=NIL

     axp:=[`moveaxpdx(1,A_,B_),
           `moveaxpax(1,A_,B_),
           `moveaxpaxp(1,A_,B_),
           `moveaxpaxpi(1,A_,B_),
           `moveaxpaxpd(1,A_,B_),
           `moveaxpaxpofs(1,A_,B_,C_),
           NIL,
           `moveaxpaxpx(1,A_,B_,C_,D_,E_)]

    axpi:=[`moveaxpidx(1,A_,B_),
           `moveaxpiax(1,A_,B_),
           `moveaxpiaxp(1,A_,B_),
           `moveaxpiaxpi(1,A_,B_),
           `moveaxpiaxpd(1,A_,B_),
           `moveaxpiaxpofs(1,A_,B_,C_),
           NIL,
           `moveaxpiaxpx(1,A_,B_,C_,D_,E_)]

    axpd:=[`moveaxpddx(1,A_,B_),
           `moveaxpdax(1,A_,B_),
           `moveaxpdaxp(1,A_,B_),
           `moveaxpdaxpi(1,A_,B_),
           `moveaxpdaxpd(1,A_,B_),
           `moveaxpdaxpofs(1,A_,B_,C_),
           NIL,
           `moveaxpdaxpx(1,A_,B_,C_,D_,E_)]

    axpo:=[`moveaxpofsdx(1,A_,B_,C_),
           `moveaxpofsax(1,A_,B_,C_),
           `moveaxpofsaxp(1,A_,B_,C_),
           `moveaxpofsaxpi(1,A_,B_,C_),
           `moveaxpofsaxpd(1,A_,B_,C_),
           `moveaxpofsaxpofs(1,A_,B_,C_,D_),
           NIL,
           `moveaxpofsaxpx(1,A_,B_,C_,D_,E_,F_)]

    axpl:=[NIL,
           NIL,
           `moveaxplabaxp(1,A_,B_,C_),
           NIL,
           NIL,
           `moveaxplabaxpofs(1,A_,B_,C_,D_),
           NIL,
           `moveaxplabaxpx(1,A_,B_,C_,D_,E_,F_)]

    axpx:=[`moveaxpxdx(1,A_,B_,C_,D_,E_),
           `moveaxpxax(1,A_,B_,C_,D_,E_),
           `moveaxpxaxp(1,A_,B_,C_,D_,E_),
           `moveaxpxaxpi(1,A_,B_,C_,D_,E_),
           `moveaxpxaxpd(1,A_,B_,C_,D_,E_),
           `moveaxpxaxpofs(1,A_,B_,C_,D_,E_,F_),
           NIL,
           `moveaxpxaxpx(1,A_,B_,C_,D_,E_,F_,G_,H_)]

     imm:=[`movewimmdx(A_,B_),
           `movewimmax(A_,B_),
           `movewimmaxp(A_,B_),
           `movewimmaxpi(A_,B_),
           `movewimmaxpd(A_,B_),
           `movewimmaxpofs(A_,B_,C_),
           NIL,
           `movewimmaxpx(A_,B_,C_,D_,E_)]

     pcp:=NIL


  w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


      dx:=[`movedxdx(0,A_,B_),
           NIL,
           `movedxaxp(0,A_,B_),
           `movedxaxpi(0,A_,B_),
           `movedxaxpd(0,A_,B_),
           `movedxaxpofs(0,A_,B_,C_),
           NIL,
           `movedxaxpx(0,A_,B_,C_,D_,E_)]

      ax:=NIL

     axp:=[`moveaxpdx(0,A_,B_),
           NIL,
           `moveaxpaxp(0,A_,B_),
           `moveaxpaxpi(0,A_,B_),
           `moveaxpaxpd(0,A_,B_),
           `moveaxpaxpofs(0,A_,B_,C_),
           NIL,
           `moveaxpaxpx(0,A_,B_,C_,D_,E_)]

    axpi:=[`moveaxpidx(0,A_,B_),
           NIL,
           `moveaxpiaxp(0,A_,B_),
           `moveaxpiaxpi(0,A_,B_),
           `moveaxpiaxpd(0,A_,B_),
           `moveaxpiaxpofs(0,A_,B_,C_),
           NIL,
           `moveaxpiaxpx(0,A_,B_,C_,D_,E_)]

    axpd:=[`moveaxpddx(0,A_,B_),
           NIL,
           `moveaxpdaxp(0,A_,B_),
           `moveaxpdaxpi(0,A_,B_),
           `moveaxpdaxpd(0,A_,B_),
           `moveaxpdaxpofs(0,A_,B_,C_),
           NIL,
           `moveaxpdaxpx(0,A_,B_,C_,D_,E_)]

    axpo:=[`moveaxpofsdx(0,A_,B_,C_),
           NIL,
           `moveaxpofsaxp(0,A_,B_,C_),
           `moveaxpofsaxpi(0,A_,B_,C_),
           `moveaxpofsaxpd(0,A_,B_,C_),
           `moveaxpofsaxpofs(0,A_,B_,C_,D_),
           NIL,
           `moveaxpofsaxpx(0,A_,B_,C_,D_,E_,F_)]

    axpl:=[NIL,
           NIL,
           `moveaxplabaxp(0,A_,B_,C_),
           NIL,
           NIL,
           `moveaxplabaxpofs(0,A_,B_,C_,D_),
           `movelaxplabaxplab(A_,B_,C_,D_),
           `moveaxplabaxpx(0,A_,B_,C_,D_,E_,F_)]

    axpx:=[`moveaxpxdx(0,A_,B_,C_,D_,E_),
           NIL,
           `moveaxpxaxp(0,A_,B_,C_,D_,E_),
           `moveaxpxaxpi(0,A_,B_,C_,D_,E_),
           `moveaxpxaxpd(0,A_,B_,C_,D_,E_),
           `moveaxpxaxpofs(0,A_,B_,C_,D_,E_,F_),
           NIL,
           `moveaxpxaxpx(0,A_,B_,C_,D_,E_,F_,G_,H_)]

     imm:=[`movebimmdx(A_,B_),
           NIL,
           `movebimmaxp(A_,B_),
           NIL,
           NIL,
           `movebimmaxpofs(A_,B_,C_),
           NIL,
           `movebimmaxpx(A_,B_,C_,D_,E_)]

      pcp:=NIL

   b := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68


   hln := getLabelHLN('MOVE')
   hln.ident2 := [IDENT2_ASM,hln.name,inst]

ENDPROC

PROC addmoveainst()

   DEFBUNCHOFVARS

    dx:=[NIL,`movedxax(2,A_,B_)]

      ax:=[NIL,`moveaxax(2,A_,B_)]

     axp:=[NIL,`moveaxpax(2,A_,B_)]

    axpi:=[NIL,`moveaxpiax(2,A_,B_)]

    axpd:=[NIL,`moveaxpdax(2,A_,B_)]

    axpo:=[NIL,`moveaxpofsax(2,A_,B_,C_)]

    axpl:=[NIL,`movelaxplabax(A_,B_,C_)]

    axpx:=[NIL,`moveaxpxax(2,A_,B_,C_,D_,E_)]

     imm:=[NIL,`movelimmax(A_,B_)]

     pcp:=[NIL,`movelpcplabax(A_,B_)]

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

    dx:=[NIL,`movedxax(1,A_,B_)]

      ax:=[NIL,`moveaxax(1,A_,B_)]

     axp:=[NIL,`moveaxpax(1,A_,B_)]

    axpi:=[NIL,`moveaxpiax(1,A_,B_)]

    axpd:=[NIL,`moveaxpdax(1,A_,B_)]

    axpo:=[NIL,`moveaxpofsax(1,A_,B_,C_)]

    axpl:= NIL

    axpx:=[NIL,`moveaxpxax(1,A_,B_,C_,D_,E_)]

     imm:=[NIL,`movewimmax(A_,B_)]

     pcp:=NIL

     w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, w, NIL, NIL, NIL, 0, NIL]:inst68


   hln := getLabelHLN('MOVEA')
   hln.ident2 := [IDENT2_ASM,hln.name,inst]

ENDPROC

PROC addleainst()

   DEFBUNCHOFVARS

      dx:=NIL

      ax:=NIL

    axp:=[NIL,
          `leaaxpax(A_,B_)]

    axpi:=NIL

    axpd:=NIL

   axpo:=[NIL,
          `leaaxpofsax(A_,B_,C_)]

   axpl:=[NIL,
          `leaaxplabax(A_,B_,C_)]

   axpx:=NIL

    imm:=NIL

    pcp:=[NIL,
          `leapcplabax(A_,B_)]

   l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('LEA')
   hln.ident2 := [IDENT2_ASM,hln.name,inst]

ENDPROC

PROC addnopinst()

   DEFBUNCHOFVARS

   inst := [0, `nop(), NIL, NIL, NIL, NIL, 0, NIL]:inst68

hln := getLabelHLN('NOP')
hln.ident2 := [IDENT2_ASM,hln.name,inst]

ENDPROC

PROC addrtsinst()

   DEFBUNCHOFVARS

   inst := [0, `rts_(), NIL, NIL, NIL, NIL, 0, NIL]:inst68

hln := getLabelHLN('RTS')
hln.ident2 := [IDENT2_ASM,hln.name,inst]

ENDPROC


PROC addaddinst()

   DEFBUNCHOFVARS


      dx:=[`adddxdx(2,A_,B_),
           `adddxax(2,A_,B_),
           `adddxaxp(2,A_,B_),
           `adddxaxpi(2,A_,B_),
           `adddxaxpd(2,A_,B_),
           `adddxaxpofs(2,A_,B_,C_),
           `addldxaxplab(A_,B_,C_),
           `adddxaxpx(2,A_,B_,C_,D_,E_),
           NIL,
           NIL]

      ax:=[`addaxdx(2,A_,B_),
           `addaxax(2,A_,B_)]

     axp:=[`addaxpdx(2,A_,B_),
           `addaxpax(2,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`addaxpofsdx(2,A_,B_,C_),
           `addaxpofsax(2,A_,B_,C_)]

    axpl:=[`addlaxplabdx(A_,B_,C_),`addlaxplabax(A_,B_,C_)]

    axpx:=[`addaxpxdx(2,A_,B_,C_,D_,E_),
           `addaxpxax(2,A_,B_,C_,D_,E_)]

     imm:=[`addlimmdx(A_,B_),
           `addlimmax(A_,B_),
           `addlimmaxp(A_,B_),
           `addlimmaxpi(A_,B_),
           `addlimmaxpd(A_,B_),
           `addlimmaxpofs(A_,B_,C_),
           `addlimmaxplab(A_,B_,C_),
           `addlimmaxpx(A_,B_,C_,D_,E_)]

     pcp:=NIL

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

      dx:=[`adddxdx(1,A_,B_),
           `adddxax(1,A_,B_),
           `adddxaxp(1,A_,B_),
           `adddxaxpi(1,A_,B_),
           `adddxaxpd(1,A_,B_),
           `adddxaxpofs(1,A_,B_,C_),
           `addldxaxplab(A_,B_,C_),
           `adddxaxpx(1,A_,B_,C_,D_,E_)]

      ax:=[`addaxdx(1,A_,B_),
           `addaxax(1,A_,B_)]

     axp:=[`addaxpdx(1,A_,B_),
           `addaxpax(1,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`addaxpofsdx(1,A_,B_,C_),
           `addaxpofsax(1,A_,B_,C_)]

    axpl:=NIL

    axpx:=[`addaxpxdx(1,A_,B_,C_,D_,E_),
           `addaxpxax(1,A_,B_,C_,D_,E_)]

     imm:=[`addwimmdx(A_,B_),
           `addwimmax(A_,B_),
           `addwimmaxp(A_,B_),
           `addwimmaxpi(A_,B_),
           `addwimmaxpd(A_,B_),
           `addwimmaxpofs(A_,B_,C_),
           NIL,
           `addwimmaxpx(A_,B_,C_,D_,E_)]

     pcp:=NIL

     w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

      dx:=[`adddxdx(0,A_,B_),
           `adddxax(0,A_,B_),
           `adddxaxp(0,A_,B_),
           `adddxaxpi(0,A_,B_),
           `adddxaxpd(0,A_,B_),
           `adddxaxpofs(0,A_,B_,C_),
           NIL,
           `adddxaxpx(0,A_,B_,C_,D_,E_)]

      ax:=[`addaxdx(0,A_,B_),
           `addaxax(0,A_,B_)]

     axp:=[`addaxpdx(0,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`addaxpofsdx(0,A_,B_,C_),
           `addaxpofsax(0,A_,B_,C_)]

    axpl:=NIL

    axpx:=[`addaxpxdx(0,A_,B_,C_,D_,E_),
           `addaxpxax(0,A_,B_,C_,D_,E_)]

     imm:=[`addbimmdx(A_,B_),
           NIL,
           `addbimmaxp(A_,B_),
           NIL,
           NIL,
           `addbimmaxpofs(A_,B_,C_),
           NIL,
           `addbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL]

     pcp:=NIL

     b := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ADD')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addaddainst()

   DEFBUNCHOFVARS


      dx:=[NIL,`adddxax(2,A_,B_)]

      ax:=[NIL,`addaxax(2,A_,B_)]

     axp:=[NIL,`addaxpax(2,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[NIL,`addaxpofsax(2,A_,B_,C_)]

    axpl:= axpl:=[NIL,`addlaxplabax(A_,B_,C_)]

    axpx:=[NIL,`addaxpxax(2,A_,B_,C_,D_,E_)]

     imm:=[NIL,`addlimmax(A_,B_)]

     pcp:=NIL

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


  dx:=[NIL,`adddxax(1,A_,B_)]

      ax:=[NIL,`addaxax(1,A_,B_)]

     axp:=[NIL,`addaxpax(1,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[NIL,`addaxpofsax(1,A_,B_,C_)]

    axpl:=NIL

    axpx:=[NIL,`addaxpxax(1,A_,B_,C_,D_,E_)]

     imm:=[NIL,`addwimmax(A_,B_)]

     pcp:=NIL

     w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, w, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ADDA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addaddiinst()

   DEFBUNCHOFVARS


     imm:=[`addlimmdx(A_,B_),
           `addlimmax(A_,B_),
           `addlimmaxp(A_,B_),
           `addlimmaxpi(A_,B_),
           `addlimmaxpd(A_,B_),
           `addlimmaxpofs(A_,B_,C_),
           `addlimmaxplab(A_,B_,C_),
           `addlimmaxpx(A_,B_,C_,D_,E_)]

     l := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

     imm:=[`addwimmdx(A_,B_),
           `addwimmax(A_,B_),
           `addwimmaxp(A_,B_),
           `addwimmaxpi(A_,B_),
           `addwimmaxpd(A_,B_),
           `addwimmaxpofs(A_,B_,C_),
           NIL,
           `addwimmaxpx(A_,B_,C_,D_,E_)]

     w := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

      imm:=[`addbimmdx(A_,B_),
           NIL,
           `addbimmaxp(A_,B_),
           NIL,
           NIL,
           `addbimmaxpofs(A_,B_,C_),
           NIL,
           `addbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL]

    b := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ADDI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC



PROC addsubinst()

   DEFBUNCHOFVARS


      dx:=[`subdxdx(2,A_,B_),
           `subdxax(2,A_,B_),
           `subdxaxp(2,A_,B_),
           `subdxaxpi(2,A_,B_),
           `subdxaxpd(2,A_,B_),
           `subdxaxpofs(2,A_,B_,C_),
           `subldxaxplab(A_,B_,C_),
           `subdxaxpx(2,A_,B_,C_,D_,E_),
           NIL,
           NIL]

      ax:=[`subaxdx(2,A_,B_),
           `subaxax(2,A_,B_)]

     axp:=[`subaxpdx(2,A_,B_),
           `subaxpax(2,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`subaxpofsdx(2,A_,B_,C_),
           `subaxpofsax(2,A_,B_,C_)]

    axpl:=[`sublaxplabdx(A_,B_,C_),`sublaxplabax(A_,B_,C_)]

    axpx:=[`subaxpxdx(2,A_,B_,C_,D_,E_),
           `subaxpxax(2,A_,B_,C_,D_,E_)]

     imm:=[`sublimmdx(A_,B_),
           `sublimmax(A_,B_),
           `sublimmaxp(A_,B_),
           `sublimmaxpi(A_,B_),
           `sublimmaxpd(A_,B_),
           `sublimmaxpofs(A_,B_,C_),
           `sublimmaxplab(A_,B_,C_),
           `sublimmaxpx(A_,B_,C_,D_,E_)]

     pcp:=NIL

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

      dx:=[`subdxdx(1,A_,B_),
           `subdxax(1,A_,B_),
           `subdxaxp(1,A_,B_),
           `subdxaxpi(1,A_,B_),
           `subdxaxpd(1,A_,B_),
           `subdxaxpofs(1,A_,B_,C_),
           `subldxaxplab(A_,B_,C_),
           `subdxaxpx(1,A_,B_,C_,D_,E_),
           NIL,
           NIL]

      ax:=[`subaxdx(1,A_,B_),
           `subaxax(1,A_,B_)]

     axp:=[`subaxpdx(1,A_,B_),
           `subaxpax(1,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`subaxpofsdx(1,A_,B_,C_),
           `subaxpofsax(1,A_,B_,C_)]

    axpl:=NIL

    axpx:=[`subaxpxdx(1,A_,B_,C_,D_,E_),
           `subaxpxax(1,A_,B_,C_,D_,E_)]

     imm:=[`subwimmdx(A_,B_),
           `subwimmax(A_,B_),
           `subwimmaxp(A_,B_),
           `subwimmaxpi(A_,B_),
           `subwimmaxpd(A_,B_),
           `subwimmaxpofs(A_,B_,C_),
           NIL,
           `subwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL]

     pcp:=NIL

     w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]

      dx:=[`subdxdx(0,A_,B_),
           `subdxax(0,A_,B_),
           `subdxaxp(0,A_,B_),
           `subdxaxpi(0,A_,B_),
           `subdxaxpd(0,A_,B_),
           `subdxaxpofs(0,A_,B_,C_),
           NIL,
           `subdxaxpx(0,A_,B_,C_,D_,E_),
           NIL,
           NIL]

      ax:=[`subaxdx(0,A_,B_),
           `subaxax(0,A_,B_)]

     axp:=[`subaxpdx(0,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[`subaxpofsdx(0,A_,B_,C_),
           `subaxpofsax(0,A_,B_,C_)]

    axpl:=NIL

    axpx:=[`subaxpxdx(0,A_,B_,C_,D_,E_),
           `subaxpxax(0,A_,B_,C_,D_,E_)]

     imm:=[`subbimmdx(A_,B_),
           NIL,
           `subbimmaxp(A_,B_),
           NIL,
           NIL,
           `subbimmaxpofs(A_,B_,C_),
           NIL,
           `subbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL]

     pcp:=NIL

     b := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('SUB')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addsubainst()

   DEFBUNCHOFVARS


      dx:=[NIL,`subdxax(2,A_,B_)]

      ax:=[NIL,`subaxax(2,A_,B_)]

     axp:=[NIL,`subaxpax(2,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[NIL,`subaxpofsax(2,A_,B_,C_)]

    axpl:=[NIL,`sublaxplabax(A_,B_,C_)]

    axpx:=[NIL,`subaxpxax(2,A_,B_,C_,D_,E_)]

     imm:=[NIL,`sublimmax(A_,B_)]

     pcp:=NIL

     l := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


  dx:=[NIL,`subdxax(1,A_,B_)]

      ax:=[NIL,`subaxax(1,A_,B_)]

     axp:=[NIL,`subaxpax(1,A_,B_)]

    axpi:=NIL

    axpd:=NIL

    axpo:=[NIL,`subaxpofsax(1,A_,B_,C_)]

    axpl:=NIL

    axpx:=[NIL,`subaxpxax(1,A_,B_,C_,D_,E_)]

     imm:=[NIL,`subwimmax(A_,B_)]

     pcp:=NIL

     w := [dx,ax,axp,axpi,axpd,axpo,axpl,axpx,imm,pcp]


   inst := [2, l, w, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('SUBA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addsubiinst()

   DEFBUNCHOFVARS


     imm:=[`sublimmdx(A_,B_),
           `sublimmax(A_,B_),
           `sublimmaxp(A_,B_),
           `sublimmaxpi(A_,B_),
           `sublimmaxpd(A_,B_),
           `sublimmaxpofs(A_,B_,C_),
           `sublimmaxplab(A_,B_,C_),
           `sublimmaxpx(A_,B_,C_,D_,E_)]

     l := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

     imm:=[`subwimmdx(A_,B_),
           `subwimmax(A_,B_),
           `subwimmaxp(A_,B_),
           `subwimmaxpi(A_,B_),
           `subwimmaxpd(A_,B_),
           `subwimmaxpofs(A_,B_,C_),
           NIL,
           `subwimmaxpx(A_,B_,C_,D_,E_)]

     w := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

      imm:=[`subbimmdx(A_,B_),
           NIL,
           `subbimmaxp(A_,B_),
           NIL,
           NIL,
           `subbimmaxpofs(A_,B_,C_),
           NIL,
           `subbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL]

    b := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('SUBI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC


PROC addaddqinst()

   DEFBUNCHOFVARS

   imm:=[`addqdx(2,A_,B_),
         `addqax(2,A_,B_),
         `addqaxp(2,A_,B_),
         `addqaxpi(2,A_,B_),
         `addqaxpd(2,A_,B_),
         `addqaxpofs(2,A_,B_,C_),
         `addqlaxplab(A_,B_,C_),
         `addqaxpx(2,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   l := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   imm:=[`addqdx(1,A_,B_),
         `addqax(1,A_,B_),
         `addqaxp(1,A_,B_),
         `addqaxpi(1,A_,B_),
         `addqaxpd(1,A_,B_),
         `addqaxpofs(1,A_,B_,C_),
         NIL,
         `addqaxpx(1,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   w := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   imm:=[`addqdx(0,A_,B_),
         `addqax(0,A_,B_),
         `addqaxp(0,A_,B_),
         `addqaxpi(0,A_,B_),
         `addqaxpd(0,A_,B_),
         `addqaxpofs(0,A_,B_,C_),
         NIL,
         `addqaxpx(0,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   b := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ADDQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addsubqinst()

   DEFBUNCHOFVARS


   imm:=[`subqdx(2,A_,B_),
         `subqax(2,A_,B_),
         `subqaxp(2,A_,B_),
         `subqaxpi(2,A_,B_),
         `subqaxpd(2,A_,B_),
         `subqaxpofs(2,A_,B_,C_),
         `subqlaxplab(A_,B_,C_),
         `subqaxpx(2,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   l := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   imm:=[`subqdx(1,A_,B_),
         `subqax(1,A_,B_),
         `subqaxp(1,A_,B_),
         `subqaxpi(1,A_,B_),
         `subqaxpd(1,A_,B_),
         `subqaxpofs(1,A_,B_,C_),
         NIL,
         `subqaxpx(1,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   w := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   imm:=[`subqdx(0,A_,B_),
         `subqax(0,A_,B_),
         `subqaxp(0,A_,B_),
         `subqaxpi(0,A_,B_),
         `subqaxpd(0,A_,B_),
         `subqaxpofs(0,A_,B_,C_),
         NIL,
         `subqaxpx(0,A_,B_,C_,D_,E_),
         NIL,
         NIL]

   b := [NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,imm,NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('SUBQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addclrinst()

   DEFBUNCHOFVARS

   l := [`clrdx(SIZE_L,A_),
         `clrax(SIZE_L,A_),
         `clraxp(SIZE_L,A_),
         `clraxpi(SIZE_L,A_),
         `clraxpd(SIZE_L,A_),
         `clraxpofs(SIZE_L,A_,B_),
         NIL,
         `clraxpx(SIZE_L,A_,B_,C_,D_),
         NIL,
         NIL]

   w := [`clrdx(SIZE_W,A_),
         `clrax(SIZE_W,A_),
         `clraxp(SIZE_W,A_),
         `clraxpi(SIZE_W,A_),
         `clraxpd(SIZE_W,A_),
         `clraxpofs(SIZE_W,A_,B_),
         NIL,
         `clraxpx(SIZE_W,A_,B_,C_,D_),
         NIL,
         NIL]

   b := [`clrdx(SIZE_B,A_),
         `clrax(SIZE_B,A_),
         `clraxp(SIZE_B,A_),
         `clraxpi(SIZE_B,A_),
         `clraxpd(SIZE_B,A_),
         `clraxpofs(SIZE_B,A_,B_),
         NIL,
         `clraxpx(SIZE_B,A_,B_,C_,D_),
         NIL,
         NIL]

   inst := [1, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('CLR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC


PROC addcmpinst()

   DEFBUNCHOFVARS

   dx  :=   [`cmpdxdx(SIZE_L, A_,B_),`cmpdxax(SIZE_L, A_,B_)]
   ax  :=   [`cmpaxdx(SIZE_L, A_,B_),`cmpaxax(SIZE_L, A_,B_)]
   axp  :=  [`cmpaxpdx(SIZE_L, A_,B_),`cmpaxpax(SIZE_L, A_,B_)]
   axpi  := [`cmpaxpidx(SIZE_L, A_,B_),`cmpaxpiax(SIZE_L, A_,B_)]
   axpd  := [`cmpaxpddx(SIZE_L, A_,B_),`cmpaxpdax(SIZE_L, A_,B_)]
   axpo  := [`cmpaxpofsdx(SIZE_L, A_,B_,C_),`cmpaxpofsax(SIZE_L, A_,B_,C_)]
   axpl :=  NIL
   axpx  := [`cmpaxpxdx(SIZE_L, A_,B_,C_,D_,E_),`cmpaxpxax(SIZE_L, A_,B_,C_,D_,E_)]
   imm   := [`cmplimmdx(A_,B_),
             `cmplimmax(A_,B_),
             `cmplimmaxp(A_,B_),
             `cmplimmaxpi(A_,B_),
             `cmplimmaxpd(A_,B_),
             `cmplimmaxpofs(A_,B_,C_),
             `cmplimmaxplab(A_,B_,C_),
             `cmplimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]
   pcp   := NIL

   l := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx  :=   [`cmpdxdx(SIZE_W, A_,B_)]
   ax  :=   [`cmpaxdx(SIZE_W, A_,B_),`cmpaxax(SIZE_W, A_,B_)]
   axp  :=  [`cmpaxpdx(SIZE_W, A_,B_)]
   axpi  := [`cmpaxpidx(SIZE_W, A_,B_)]
   axpd  := [`cmpaxpddx(SIZE_W, A_,B_)]
   axpo  := [`cmpaxpofsdx(SIZE_W, A_,B_,C_)]
   axpl :=  [`cmplaxplabdx(A_,B_,C_),
             `cmplaxplabax(A_,B_,C_)]
   axpx  := [`cmpaxpxdx(SIZE_W, A_,B_,C_,D_,E_)]
   imm   := [`cmpwimmdx(A_,B_),
             `cmpwimmax(A_,B_),
             `cmpwimmaxp(A_,B_),
             `cmpwimmaxpi(A_,B_),
             `cmpwimmaxpd(A_,B_),
             `cmpwimmaxpofs(A_,B_,C_),
             NIL,
             `cmpwimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]
   pcp   := NIL

   w := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx  :=   [`cmpdxdx(SIZE_B, A_,B_)]
   ax  :=   [`cmpaxdx(SIZE_B, A_,B_)]
   axp  :=  [`cmpaxpdx(SIZE_B, A_,B_)]
   axpi  := [`cmpaxpidx(SIZE_B, A_,B_)]
   axpd  := [`cmpaxpddx(SIZE_B, A_,B_)]
   axpo  := [`cmpaxpofsdx(SIZE_B, A_,B_,C_)]
   axpl :=  NIL
   axpx  := [`cmpaxpxdx(SIZE_B, A_,B_,C_,D_,E_)]
   imm   := [`cmpbimmdx(A_,B_),
             NIL,
             `cmpbimmaxp(A_,B_),
             NIL,
             NIL,
             `cmpbimmaxpofs(A_,B_,C_),
             NIL,
             `cmpbimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]
   pcp   := NIL

   b := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('CMP')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addcmpainst()

   DEFBUNCHOFVARS

   dx  :=   [NIL,`cmpdxax(SIZE_L, A_,B_)]
   ax  :=   [NIL,`cmpaxax(SIZE_L, A_,B_)]
   axp  :=  [NIL,`cmpaxpax(SIZE_L, A_,B_)]
   axpi  := [NIL,`cmpaxpiax(SIZE_L, A_,B_)]
   axpd  := [NIL,`cmpaxpdax(SIZE_L, A_,B_)]
   axpo  := [NIL,`cmpaxpofsax(SIZE_L, A_,B_,C_)]
   axpl :=  NIL
   axpx  := [NIL,`cmpaxpxax(SIZE_L, A_,B_,C_,D_,E_)]
   imm   := [NIL,`cmplimmax(A_,B_)]
   pcp   := NIL

   l := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]


   dx  :=   [NIL,`cmpdxax(SIZE_W, A_,B_)]
   ax  :=   [NIL,`cmpaxax(SIZE_W, A_,B_)]
   axp  :=  [NIL,`cmpaxpax(SIZE_W, A_,B_)]
   axpi  := [NIL,`cmpaxpiax(SIZE_W, A_,B_)]
   axpd  := [NIL,`cmpaxpdax(SIZE_W, A_,B_)]
   axpo  := [NIL,`cmpaxpofsax(SIZE_W, A_,B_,C_)]
   axpl :=  NIL
   axpx  := [NIL,`cmpaxpxax(SIZE_W, A_,B_,C_,D_,E_)]
   imm   := [NIL,`cmpwimmax(A_,B_)]
   pcp   := NIL

   w := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]


   inst := [2, l, w, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('CMPA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addcmpiinst()

   DEFBUNCHOFVARS

   imm   := [`cmplimmdx(A_,B_),
             `cmplimmax(A_,B_),
             `cmplimmaxp(A_,B_),
             `cmplimmaxpi(A_,B_),
             `cmplimmaxpd(A_,B_),
             `cmplimmaxpofs(A_,B_,C_),
             `cmplimmaxplab(A_,B_,C_),
             `cmplimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   imm   := [`cmpwimmdx(A_,B_),
             `cmpwimmax(A_,B_),
             `cmpwimmaxp(A_,B_),
             `cmpwimmaxpi(A_,B_),
             `cmpwimmaxpd(A_,B_),
             `cmpwimmaxpofs(A_,B_,C_),
             NIL,
             `cmpwimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   imm   := [`cmpbimmdx(A_,B_),
             NIL,
             `cmpbimmaxp(A_,B_),
             NIL,
             NIL,
             `cmpbimmaxpofs(A_,B_,C_),
             NIL,
             `cmpbimmaxpx(A_,B_,C_,D_,E_),
             NIL,
             NIL]

   b := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('CMPI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC


PROC addextinst()

   DEFBUNCHOFVARS

   l := [`extl(A_)]

   w := [`extw(A_)]

   inst := [1, l, w, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('EXT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addextbinst()

   DEFBUNCHOFVARS

   l := [`extbl(A_)]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('EXTB')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addneginst()

   DEFBUNCHOFVARS

   l := [`negdx(SIZE_L,A_),
         NIL,
         `negaxp(SIZE_L,A_),
         `negaxpi(SIZE_L,A_),
         `negaxpd(SIZE_L,A_),
         `negaxpofs(SIZE_L,A_,B_),
         NIL,
         `negaxpx(SIZE_L,A_,B_,C_,D_),
         NIL,
         NIL]

   w := [`negdx(SIZE_W,A_),
         NIL,
         `negaxp(SIZE_W,A_),
         `negaxpi(SIZE_W,A_),
         `negaxpd(SIZE_W,A_),
         `negaxpofs(SIZE_W,A_,B_),
         NIL,
         `negaxpx(SIZE_W,A_,B_,C_,D_),
         NIL,
         NIL]

   b := [`negdx(SIZE_B,A_),
         NIL,
         `negaxp(SIZE_B,A_),
         `negaxpi(SIZE_B,A_),
         `negaxpd(SIZE_B,A_),
         `negaxpofs(SIZE_B,A_,B_),
         NIL,
         `negaxpx(SIZE_B,A_,B_,C_,D_),
         NIL,
         NIL]

   inst := [1, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('NEG')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addandinst()

   DEFBUNCHOFVARS

   dx := [`anddxdx(SIZE_L, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`andaxpdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`andaxpidx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`andaxpddx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`andaxpofsdx(SIZE_L,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`andaxpxdx(SIZE_L,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`andlimmdx(A_,B_),
           NIL,
           `andlimmaxp(A_,B_),
           `andlimmaxpi(A_,B_),
           `andlimmaxpd(A_,B_),
           `andlimmaxpofs(A_,B_,C_),
           NIL,
           `andlimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   l := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx := [`anddxdx(SIZE_W, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`andaxpdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`andaxpidx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`andaxpddx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`andaxpofsdx(SIZE_W,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`andaxpxdx(SIZE_W,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`andwimmdx(A_,B_),
           NIL,
           `andwimmaxp(A_,B_),
           NIL,
           NIL,
           `andwimmaxpofs(A_,B_,C_),
           NIL,
           `andwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   w := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx := [`anddxdx(SIZE_B, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`andaxpdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`andaxpidx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`andaxpddx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`andaxpofsdx(SIZE_B,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`andaxpxdx(SIZE_B,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`andbimmdx(A_,B_),
           NIL,
           `andbimmaxp(A_,B_),
           NIL,
           NIL,
           `andbimmaxpofs(A_,B_,C_),
           NIL,
           `andbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   b := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('AND')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addandiinst()

   DEFBUNCHOFVARS


   imm := [`andlimmdx(A_,B_),
           NIL,
           `andlimmaxp(A_,B_),
           `andlimmaxpi(A_,B_),
           `andlimmaxpd(A_,B_),
           `andlimmaxpofs(A_,B_,C_),
           NIL,
           `andlimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]


   imm := [`andwimmdx(A_,B_),
           NIL,
           `andwimmaxp(A_,B_),
           NIL,
           NIL,
           `andwimmaxpofs(A_,B_,C_),
           NIL,
           `andwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   imm := [`andbimmdx(A_,B_),
           NIL,
           `andbimmaxp(A_,B_),
           NIL,
           NIL,
           `andbimmaxpofs(A_,B_,C_),
           NIL,
           `andbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   b := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ANDI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addorinst()

   DEFBUNCHOFVARS


   dx := [`ordxdx(SIZE_L, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`oraxpdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`oraxpidx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`oraxpddx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`oraxpofsdx(SIZE_L,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`oraxpxdx(SIZE_L,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`orlimmdx(A_,B_),
           NIL,
           `orlimmaxp(A_,B_),
           `orlimmaxpi(A_,B_),
           `orlimmaxpd(A_,B_),
           `orlimmaxpofs(A_,B_,C_),
           NIL,
           `orlimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   l := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx := [`ordxdx(SIZE_W, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`oraxpdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`oraxpidx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`oraxpddx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`oraxpofsdx(SIZE_W,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`oraxpxdx(SIZE_W,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`orwimmdx(A_,B_),
           NIL,
           `orwimmaxp(A_,B_),
           NIL,
           NIL,
           `orwimmaxpofs(A_,B_,C_),
           NIL,
           `orwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   w := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   dx := [`ordxdx(SIZE_B, A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := NIL
   axp := [`oraxpdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [`oraxpidx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [`oraxpddx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpo := [`oraxpofsdx(SIZE_B,A_,B_,C_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpl := NIL
   axpx := [`oraxpxdx(SIZE_B,A_,B_,C_,D_,E_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`orbimmdx(A_,B_),
           NIL,
           `orbimmaxp(A_,B_),
           NIL,
           NIL,
           `orbimmaxpofs(A_,B_,C_),
           NIL,
           `orbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]
   pcp := NIL

   b := [dx, ax, axp, axpi, axpd, axpo, axpl, axpx, imm, pcp]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('OR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]


ENDPROC

PROC addoriinst()

   DEFBUNCHOFVARS


   imm := [`orlimmdx(A_,B_),
           NIL,
           `orlimmaxp(A_,B_),
           `orlimmaxpi(A_,B_),
           `orlimmaxpd(A_,B_),
           `orlimmaxpofs(A_,B_,C_),
           NIL,
           `orlimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]


   imm := [`orwimmdx(A_,B_),
           NIL,
           `orwimmaxp(A_,B_),
           NIL,
           NIL,
           `orwimmaxpofs(A_,B_,C_),
           NIL,
           `orwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   imm := [`orbimmdx(A_,B_),
           NIL,
           `orbimmaxp(A_,B_),
           NIL,
           NIL,
           `orbimmaxpofs(A_,B_,C_),
           NIL,
           `orbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   b := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ORI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addeorinst()

   DEFBUNCHOFVARS

   dx := [`eordxdx(SIZE_L,A_,B_),
          NIL,
          `eordxaxp(SIZE_L,A_,B_),
          `eordxaxpi(SIZE_L,A_,B_),
          `eordxaxpd(SIZE_L,A_,B_),
          `eordxaxpofs(SIZE_L,A_,B_,C_),
          NIL,
          `eordxaxpx(SIZE_L,A_,B_,C_,D_,E_),
          NIL,
          NIL]

  imm := [`eorlimmdx(A_,B_),
          NIL,
          `eorlimmaxp(A_,B_),
          `eorlimmaxpi(A_,B_),
          `eorlimmaxpd(A_,B_),
          `eorlimmaxpofs(A_,B_,C_),
          NIL,
          `eorlimmaxpx(A_,B_,C_,D_,E_),
          NIL,
          NIL]

   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`eordxdx(SIZE_W,A_,B_),
          NIL,
          `eordxaxp(SIZE_W,A_,B_),
          `eordxaxpi(SIZE_W,A_,B_),
          `eordxaxpd(SIZE_W,A_,B_),
          `eordxaxpofs(SIZE_W,A_,B_,C_),
          NIL,
          `eordxaxpx(SIZE_W,A_,B_,C_,D_,E_),
          NIL,
          NIL]

  imm := [`eorwimmdx(A_,B_),
          NIL,
          `eorwimmaxp(A_,B_),
          `eorwimmaxpi(A_,B_),
          `eorwimmaxpd(A_,B_),
          `eorwimmaxpofs(A_,B_,C_),
          NIL,
          `eorwimmaxpx(A_,B_,C_,D_,E_),
          NIL,
          NIL]

   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`eordxdx(SIZE_B,A_,B_),
          NIL,
          `eordxaxp(SIZE_B,A_,B_),
          NIL,
          NIL,
          `eordxaxpofs(SIZE_B,A_,B_,C_),
          NIL,
          `eordxaxpx(SIZE_B,A_,B_,C_,D_,E_),
          NIL,
          NIL]

  imm := [`eorbimmdx(A_,B_),
          NIL,
          `eorbimmaxp(A_,B_),
          NIL,
          NIL,
          `eorbimmaxpofs(A_,B_,C_),
          NIL,
          `eorbimmaxpx(A_,B_,C_,D_,E_),
          NIL,
          NIL]

   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('EOR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addeoriinst()

   DEFBUNCHOFVARS


   imm := [`eorlimmdx(A_,B_),
           NIL,
           `eorlimmaxp(A_,B_),
           `eorlimmaxpi(A_,B_),
           `eorlimmaxpd(A_,B_),
           `eorlimmaxpofs(A_,B_,C_),
           NIL,
           `eorlimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]


   imm := [`eorwimmdx(A_,B_),
           NIL,
           `eorwimmaxp(A_,B_),
           NIL,
           NIL,
           `eorwimmaxpofs(A_,B_,C_),
           NIL,
           `eorwimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   imm := [`eorbimmdx(A_,B_),
           NIL,
           `eorbimmaxp(A_,B_),
           NIL,
           NIL,
           `eorbimmaxpofs(A_,B_,C_),
           NIL,
           `eorbimmaxpx(A_,B_,C_,D_,E_),
           NIL,
           NIL,
           NIL]

   b := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('EORI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addnotinst()

   DEFBUNCHOFVARS

   l := [`notdx(SIZE_L,A_),
         NIL,
         `notaxp(SIZE_L,A_),
         `notaxpi(SIZE_L,A_),
         `notaxpd(SIZE_L,A_),
         `notaxpofs(SIZE_L,A_,B_),
         NIL,
         `notaxpx(SIZE_L,A_,B_,C_,D_),
         NIL,
         NIL]

   w := [`notdx(SIZE_W,A_),
         NIL,
         `notaxp(SIZE_W,A_),
         `notaxpi(SIZE_W,A_),
         `notaxpd(SIZE_W,A_),
         `notaxpofs(SIZE_W,A_,B_),
         NIL,
         `notaxpx(SIZE_W,A_,B_,C_,D_),
         NIL,
         NIL]

   b := [`notdx(SIZE_B,A_),
         NIL,
         `notaxp(SIZE_B,A_),
         NIL,
         NIL,
         `notaxpofs(SIZE_B,A_,B_),
         NIL,
         `notaxpx(SIZE_B,A_,B_,C_,D_),
         NIL,
         NIL]

   inst := [1, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('NOT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addmoveqinst()

   DEFBUNCHOFVARS

   imm := [`moveqdx(A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('MOVEQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addpeainst()

   DEFBUNCHOFVARS

   l := [NIL, `peaaxp(A_), NIL, NIL, NIL, `peaaxpofs(A_,B_), NIL, NIL, NIL, NIL]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('PEA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addunlkinst()

   DEFBUNCHOFVARS

   l := [NIL, `unlkax(A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('UNLK')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addrtdinst()

   DEFBUNCHOFVARS

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `rtdofs(A_), NIL]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('RTD')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addtstinst()

   DEFBUNCHOFVARS

   l := [`tstdx(SIZE_L,A_),
         `tstax(SIZE_L,A_),
         `tstaxp(SIZE_L,A_),
         `tstaxpi(SIZE_L,A_),
         `tstaxpd(SIZE_L,A_),
         `tstaxpofs(SIZE_L,A_,B_),
         `tstlaxplab(A_,B_),
         `tstaxpx(SIZE_L,A_,B_,C_,D_),
         NIL,
         NIL]

   w := [`tstdx(SIZE_W,A_),
         NIL,
         `tstaxp(SIZE_W,A_),
         `tstaxpi(SIZE_W,A_),
         `tstaxpd(SIZE_W,A_),
         `tstaxpofs(SIZE_W,A_,B_),
         NIL,
         `tstaxpx(SIZE_W,A_,B_,C_,D_),
         NIL,
         NIL]

   b := [`tstdx(SIZE_B,A_),
         NIL,
         `tstaxp(SIZE_B,A_),
         NIL,
         NIL,
         `tstaxpofs(SIZE_B,A_,B_),
         NIL,
         `tstaxpx(SIZE_B,A_,B_,C_,D_),
         NIL,
         NIL]

   inst := [1, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('TST')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addaslinst()

   DEFBUNCHOFVARS

   dx := [`asldxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`aslimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`asldxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`aslimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`asldxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`aslimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ASL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addasrinst()

   DEFBUNCHOFVARS

   dx := [`asrdxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`asrimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`asrdxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`asrimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`asrdxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`asrimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ASR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addlslinst()

   DEFBUNCHOFVARS

   dx := [`lsldxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lslimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`lsldxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lslimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`lsldxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lslimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('LSL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addlsrinst()

   DEFBUNCHOFVARS

   dx := [`lsrdxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lsrimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`lsrdxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lsrimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`lsrdxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`lsrimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('LSR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addrolinst()

   DEFBUNCHOFVARS

   dx := [`roldxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rolimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roldxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rolimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roldxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rolimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ROL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addrorinst()

   DEFBUNCHOFVARS

   dx := [`rordxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rorimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`rordxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rorimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`rordxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`rorimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ROR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addroxlinst()

   DEFBUNCHOFVARS

   dx := [`roxldxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxlimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roxldxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxlimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roxldxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxlimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ROXL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addroxrinst()

   DEFBUNCHOFVARS

   dx := [`roxrdxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxrimmdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roxrdxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxrimmdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   dx := [`roxrdxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   imm := [`roxrimmdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ROXR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addswapinst()

   DEFBUNCHOFVARS

   w := [`swapdx(A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('SWAP')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addexginst()

   DEFBUNCHOFVARS

   dx := [`exgdxdx(A_,B_), `exgdxax(A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   ax := [NIL, `exgaxax(A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   l := [dx, ax, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   inst := [2, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('EXG')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addlinkinst()

   DEFBUNCHOFVARS

   ax := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `linkl(A_,B_), NIL]
   l := [NIL, ax, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   ax := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `linkw(A_,B_), NIL]
   w := [NIL, ax, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('LINK')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addbsrinst()

   DEFBUNCHOFVARS

   l := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bsrlab(A_)]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('BSR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addbccinst()

   DEFBUNCHOFVARS

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(F,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BF')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(T,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BRA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   hln := getLabelHLN('BT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(EQ,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BEQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(NE,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BNE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(LT,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BLT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(LE,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BLE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(GT,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BGT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(GE,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BGE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(HI,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BHI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(LS,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BLS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(CC,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BCC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(CS,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BCS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(VC,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BVC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(VS,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BVS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(PL,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BPL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   w := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `bcclab(MI,A_)]
   inst := [1, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('BMI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC


PROC adddbccinst()

   DEFBUNCHOFVARS

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(F,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBF')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   hln := getLabelHLN('DBRA')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(T,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(HI,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBHI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(LS,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBLS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(CC,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBCC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(CS,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBCS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(NE,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBNE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(EQ,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBEQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(VC,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBVC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(VS,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBVS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(PL,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBPL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(MI,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBMI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(GE,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBGE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(LT,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBLT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(GT,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBGT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   dx := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `dbcclab(LE,A_,B_)]
   w := [dx, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [2, NIL, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DBLE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC


PROC addmoveminst()

   DEFBUNCHOFVARS

   -> note: we take registerlist as immediate !

   axp := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `movemaxpregs(SIZE_L,A_,reverseregmask(B_)), NIL]
   axpi := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `movemaxpiregs(SIZE_L,A_,reverseregmask(B_)), NIL]
   imm := [NIL, NIL, `movemregsaxp(SIZE_L,reverseregmask(A_),B_), NIL, `movemregsaxpd(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, `dbcclab(LE,A_,B_)]
   l := [NIL, NIL, axp, axpi, NIL, NIL, NIL, NIL, imm, NIL]

   axp := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `movemaxpregs(SIZE_W,A_,reverseregmask(B_)), NIL]
   axpi := [NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, `movemaxpiregs(SIZE_W,A_,reverseregmask(B_)), NIL]
   imm := [NIL, NIL, `movemregsaxp(SIZE_W,reverseregmask(A_),B_), NIL, `movemregsaxpd(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, `dbcclab(LE,A_,B_)]
   w := [NIL, NIL, axp, axpi, NIL, NIL, NIL, NIL, imm, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('MOVEM')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC reverseregmask(mask)
   DEF mask2=NIL, a
   FOR a := 0 TO 15 DO IF Shl(1, a) AND mask THEN mask2 := mask2 OR Shl(1, 15-a)
ENDPROC mask2


PROC addmulsinst()

    DEFBUNCHOFVARS

   -> note: imm as destination means dr:dq !

   dx := [`mulsldxdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mulsldxdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

  axp := [`mulslaxpdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mulslaxpdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

 axpo := [`mulslaxpofsdrdq(0,A_,B_,0,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mulslaxpofsdrdq(1, A_,B_,Shr(C_,3),C_ AND 7),
          NIL]

  imm := [`mulslimmdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mulslimmdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

   l := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   dx := [`mulswdxdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  axp := [`mulswaxpdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

 axpo := [`mulswaxpofsdx(A_,B_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  imm := [`mulswimmdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

   w := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('MULS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addmuluinst()

    DEFBUNCHOFVARS

   -> note: imm as destination means dr:dq !

   dx := [`mululdxdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mululdxdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

  axp := [`mululaxpdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mululaxpdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

 axpo := [`mululaxpofsdrdq(0,A_,B_,0,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mululaxpofsdrdq(1, A_,B_,Shr(C_,3),C_ AND 7),
          NIL]

  imm := [`mululimmdrdq(0,A_,0,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `mululimmdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

   l := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   dx := [`muluwdxdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  axp := [`muluwaxpdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

 axpo := [`muluwaxpofsdx(A_,B_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  imm := [`muluwimmdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

   w := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('MULU')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC adddivsinst()

    DEFBUNCHOFVARS

   -> note: imm as destination means dr:dq !

   dx := [`divsldxdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divsldxdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

  axp := [`divslaxpdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divslaxpdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

 axpo := [`divslaxpofsdrdq(0,A_,B_,C_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divslaxpofsdrdq(1, A_,B_,Shr(C_,3),C_ AND 7),
          NIL]

  imm := [`divslimmdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divslimmdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

   l := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   dx := [`divswdxdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  axp := [`divswaxpdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

 axpo := [`divswaxpofsdx(A_,B_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  imm := [`divswimmdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

   w := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DIVS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC adddivuinst()

    DEFBUNCHOFVARS

   -> note: imm as destination means dr:dq !

   dx := [`divuldxdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divuldxdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

  axp := [`divulaxpdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divulaxpdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

 axpo := [`divulaxpofsdrdq(0,A_,B_,C_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divulaxpofsdrdq(1, A_,B_,Shr(C_,3),C_ AND 7),
          NIL]

  imm := [`divulimmdrdq(0,A_,B_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          `divulimmdrdq(1, A_,Shr(B_,3),B_ AND 7),
          NIL]

   l := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   dx := [`divuwdxdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  axp := [`divuwaxpdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

 axpo := [`divuwaxpofsdx(A_,B_,C_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

  imm := [`divuwimmdx(A_,B_),
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL,
          NIL]

   w := [dx, NIL, axp, NIL, NIL, axpo, NIL, NIL, imm, NIL]

   inst := [2, l, w, NIL, NIL, NIL, 1, NIL]:inst68

   hln := getLabelHLN('DIVU')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addsccinst()

   DEFBUNCHOFVARS

   b := [`sccdx(F,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SF')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(T,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('ST')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(HI,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SHI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(LS,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SLS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(CC,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SCC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(CS,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SCS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(NE,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SNE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(EQ,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SEQ')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(VC,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SVC')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(VS,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SVS')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(PL,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SPL')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(MI,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SMI')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(GE,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SGE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(LT,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SLT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(GT,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SGT')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

   b := [`sccdx(LE,A_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   inst := [1, NIL, NIL, b, NIL, NIL, 2, NIL]:inst68
   hln := getLabelHLN('SLE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addjsrinst()

   DEFBUNCHOFVARS

   axp   := `jsraxp(A_)
   axpo  := `jsraxpofs(A_,B_)
   axpx  := `jsraxpx(A_,B_,C_,D_)

   l := [NIL, NIL, axp, NIL, NIL, axpo, NIL, axpx, NIL, NIL]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('JSR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addjmpinst()

   DEFBUNCHOFVARS

   axp   := `jmpaxp(A_)
   axpo  := `jmpaxpofs(A_,B_)
   axpx  := `jmpaxpx(A_,B_,C_,D_)

   l := [NIL, NIL, axp, NIL, NIL, axpo, NIL, axpx, NIL, NIL]

   inst := [1, l, NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('JMP')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC


PROC addaddxinst()

   DEFBUNCHOFVARS

   dx   := [`addxdxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `addxaxpdaxpd(SIZE_L,A_,B_),NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   dx   := [`addxdxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `addxaxpdaxpd(SIZE_W,A_,B_),NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   dx   := [`addxdxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `addxaxpdaxpd(SIZE_B,A_,B_),NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('ADDX')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addsubxinst()

   DEFBUNCHOFVARS

   dx   := [`subxdxdx(SIZE_L,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `subxaxpdaxpd(SIZE_L,A_,B_),NIL, NIL, NIL, NIL, NIL]
   l := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   dx   := [`subxdxdx(SIZE_W,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `subxaxpdaxpd(SIZE_W,A_,B_),NIL, NIL, NIL, NIL, NIL]
   w := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   dx   := [`subxdxdx(SIZE_B,A_,B_), NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL]
   axpd := [NIL, NIL, NIL, NIL, `subxaxpdaxpd(SIZE_B,A_,B_),NIL, NIL, NIL, NIL, NIL]
   b := [dx, NIL, NIL, NIL, axpd, NIL, NIL, NIL, NIL, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('SUBX')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addrtrinst()

   DEFBUNCHOFVARS

   inst := [0, `rtr_(), NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('RTR')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addrteinst()

   DEFBUNCHOFVARS

   inst := [0, `rte_(), NIL, NIL, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('RTE')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC

PROC addnegxinst()

   DEFBUNCHOFVARS

   l := [`negxdx(SIZE_L,A_),
         NIL,
         `negxaxp(SIZE_L,A_),
         `negxaxpi(SIZE_L,A_),
         `negxaxpd(SIZE_L,A_),
         `negxaxpofs(SIZE_L,A_,B_),
         NIL,
         `negxaxpx(SIZE_L,A_,B_,C_,D_),
         NIL,
         NIL]

   w := [`negxdx(SIZE_W,A_),
         NIL,
         `negxaxp(SIZE_W,A_),
         `negxaxpi(SIZE_W,A_),
         `negxaxpd(SIZE_W,A_),
         `negxaxpofs(SIZE_W,A_,B_),
         NIL,
         `negxaxpx(SIZE_W,A_,B_,C_,D_),
         NIL,
         NIL]

   b := [`negxdx(SIZE_B,A_),
         NIL,
         `negxaxp(SIZE_B,A_),
         `negxaxpi(SIZE_B,A_),
         `negxaxpd(SIZE_B,A_),
         `negxaxpofs(SIZE_B,A_,B_),
         NIL,
         `negxaxpx(SIZE_B,A_,B_,C_,D_),
         NIL,
         NIL]

   inst := [1, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('NEGX')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]
ENDPROC

PROC addcmpminst()

   DEFBUNCHOFVARS

   axpi := [NIL, NIL, NIL, `cmpmaxpiaxpi(SIZE_L, A_, B_)]
   l := [NIL, NIL, NIL, axpi, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [NIL, NIL, NIL, `cmpmaxpiaxpi(SIZE_W, A_, B_)]
   w := [NIL, NIL, NIL, axpi, NIL, NIL, NIL, NIL, NIL, NIL]
   axpi := [NIL, NIL, NIL, `cmpmaxpiaxpi(SIZE_B, A_, B_)]
   b := [NIL, NIL, NIL, axpi, NIL, NIL, NIL, NIL, NIL, NIL]

   inst := [2, l, w, b, NIL, NIL, 0, NIL]:inst68

   hln := getLabelHLN('CMPM')
   hln.ident2 := [IDENT2_ASM, hln.name,inst]

ENDPROC


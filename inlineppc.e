-> ECX/inline.e

/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See ECXCOMPILERLICENSE.TXT */

OPT MODULE
OPT PREPROCESS
OPT LARGE

-> ECX/inline.e, created May 2008, extracted from codegen.e

MODULE '*common'
MODULE '*compiler'
MODULE '*opcodesppc'
MODULE '*assembler'

#define PPC_PATCH_EXEC

#define A_ g_od[0]
#define B_ g_od[1]
#define C_ g_od[2]
#define D_ g_od[3]
#define E_ g_od[4]
#define F_ g_od[5]
#define P_ g_punct

#define GLOBREG g_globreg

EXPORT OBJECT instppc
   operands:PTR TO LONG
   numops
   punct
   qcode
ENDOBJECT

EXPORT ENUM
      PO, -> punctation optional
      PN, -> punctation none
      PR  -> punctation required

EXPORT DEF g_od:PTR TO LONG, g_punct -> shared with codegen.e
EXPORT DEF g_globreg -> shared with codegen.e


EXPORT PROC addPPCInlines()
   DEF hln:PTR TO hln, code

      hln := getLabelHLN('Char')
      code := [$88630000] -> lbz  r3, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('Byte')  -> v50 back again
      code := [$88630000,  -> lbz  r3, 0(r3)
      $7C630774] -> extsb  r3, r3
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('Int')  -> v50, now really DOES sign extend !
      code := [$A8630000] -> lha  r3, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('Word')
      code := [$A0630000] -> lhz  r3, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('Long')
      code := [$80630000] -> lwz  r3, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('PutLong')
      code := [$90830000] -> stw  r4, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('PutInt')
      code := [$B0830000] -> sth  r4, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('PutChar')
      code := [$98830000] -> stb  r4, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('ListItem')
      code := [$5484103A,  -> rlwinm r4,r4,2,0,31-2
      $7C63202E] -> lwzx  r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0,0], [], [0], 1)

      hln := getLabelHLN('ListLen')
      code := [$A063FFFE] -> lhz  r3, -2(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('EstrLen')
      code := [$A063FFFE] -> lhz  r3, -2(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('StrMax')
      code := [$A063FFFC] -> lhz  r3, -4(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('ListMax')
      code := [$A063FFFC] -> lhz  r3, -4(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('SetStr')
      code := [$B083FFFE,  -> sth  r4, -2(r3)
      $38000000,  -> addi r0,0,0
      $7C0321AE]  -> stbx r0, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 12, [0,0], [], [0], 1)

      hln := getLabelHLN('SetList')
      code := [$B083FFFE] -> sth  r4, -2(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('ObjSize')
      code := [$8063FFFC,  -> lwz  r3, -4(r3)
      $8063FFFC] -> lwz  r3, -4(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('ObjName')
      code := [$8063FFFC,  -> lwz  r3, -4(r3)
      $8063FFF8] -> lwz  r3, -8(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('Shr')
      code := [$7C632630] -> sraw r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Shl')
      code := [$7C632030] -> slw r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      -> 2.2 now operator instead!
      ->hln := getLabelHLN('Not')
      ->code := [$7C6318F8] -> nor r3, r3, r3
      ->hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('Or')
      code := [$7C632378] -> or r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Eor')
      code := [$7C632278] -> xor r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('And')
      code := [$7C632038] -> and r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Odd')
      code := [$70630001,  -> andi. r3, r3, 1
      $7C6300D0] -> neg r3, r3
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('Even')
      code := [$70630001,  -> andi. r3, r3, 1
      $3863FFFF] -> addi r3, r3, -1
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('Link')
      code := [$9083FFF8] -> stw r4, -8(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Next')
      code := [$7C631B79,  -> or. r3, r3, r3
      $41820008,  -> beq $ + 8
      $8063FFF8] -> lwz r3, -8(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 12, [0], [], [0], 1)

      hln := getLabelHLN('Eval')
      code := [$7C6903A6,  -> mtctr r3
      $4E800421] -> bctrl
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [0], 1)

      hln := getLabelHLN('Mul')
      code := [$7C6321D6] -> mullw r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Div')
      code := [$7C6323D6] -> divw r3, r3, r4
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('MouseX')
      code := [$A063000E] -> lhz  r3, 14(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('MouseY')
      code := [$A063000C] -> lhz  r3, 12(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('MsgCode')
      code := [$A06DFFBC] -> lhz  r3, -68(r13)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [], [], [0], 1)

      hln := getLabelHLN('MsgQual')
      code := [$A06DFFBA] -> lhz  r3, -70(r13)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [], [], [0], 1)

      hln := getLabelHLN('MsgIaddr')
      code := [$806DFFB8] -> lwz  r3, -72(r13)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [], [], [0], 1)

      hln := getLabelHLN('Double') -> v45
      code := [$C8230000] -> lfd  f1, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [1], 1)

      hln := getLabelHLN('PutDouble') -> v45
      code := [$D8430000] -> stfd  f2, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,1], [], [0], 1)

      hln := getLabelHLN('Ptr') -> v49, same as Long()
      code := [$80630000] -> lwz  r3, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [0], 1)

      hln := getLabelHLN('PutPtr') -> v49, same as PutLong()
      code := [$90830000] -> stw  r4, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,0], [], [0], 1)

      hln := getLabelHLN('Float') -> v50
      code := [$C8230000] -> lfd  f1, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [1], 1)

      hln := getLabelHLN('PutFloat') -> v50
      code := [$D0230000] -> stfs  f1, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,1], [], [0], 1)

      hln := getLabelHLN('Real') -> v50 same as Double
      code := [$C8230000] -> lfd  f1, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0], [], [1], 1)

      hln := getLabelHLN('PutReal') -> v50 same as PutDouble
      code := [$D8430000] -> stfd  f2, 0(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 4, [0,1], [], [0], 1)

      hln := getLabelHLN('Fabs') -> v 1.5.4: now inlined.
      code := [$FC200A10] -> fabs f1, f1
      hln.ident := IFUNCDEF2(hln.name, code, 4, [1], [], [1], 1)

      -> 2.2: is operator now
      ->hln := getLabelHLN('Abs') -> v55
      ->code := [$7C64FE70,  -> srawi, r4, r3, 31
      ->$7CA41A14,  -> add r5, r4, r3
      ->$7CA32278] -> xor,r3, r5, r4
      ->hln.ident := IFUNCDEF2(hln.name, code, 12, [0], [], [0], 1)

      hln := getLabelHLN('Sign') -> v55
      code := [$6C648000,  -> xoris r4, r3, -32768
      $7C85FE70,  -> srawi, r5, r4, 31
      $7C650190] -> subfze r3, r5
      hln.ident := IFUNCDEF2(hln.name, code, 12, [0], [], [0], 1)

      hln := getLabelHLN('Bounds') -> v55
      code := [$7CC41850,  -> subf r6, r4, r3
      $7CC7FE70,  -> srawi r7, r6, 31
      $7CC600D0,  -> neg r6, r6
      $7CC83838,  -> and r8, r6, r7
      $7C634214,  -> add r3, r3, r8
      $7CC32850,  -> dubf r6, r3, r5
      $7CC7FE70,  -> srawi r7, r6, 31
      $7CC83878,  -> andc r8, r6, r7
      $7C682850] -> subf r3, r8, r5
      hln.ident := IFUNCDEF2(hln.name, code, 36, [0,0,0], [], [0], 1)

      hln := getLabelHLN('Min') -> v55
      code := [$6C858000,  -> xoris r5, r4, -32768
      $6C668000,  -> xorsi r5, r3, -32768
      $7CC62810,  -> subfc r6, r6, r5
      $7CA52910,  -> subfe r5, r5, r5
      $7CC62838,  -> and r6, r6, r5
      $7C661A14] -> add r3, r6, r3
      hln.ident := IFUNCDEF2(hln.name, code, 24, [0,0], [], [0], 1)

      hln := getLabelHLN('Max') -> v55
      code := [$6C858000,  -> xoris r5, r4, -32768
      $6C668000,  -> xoris r6, r3, -32768
      $7CC62810,  -> subfc r6, r6, r5
      $7CA52910,  -> subfe r5, r5, r5
      $7CC62878,  -> andc r6, r6, r5
      $7C661A14] -> add r3, r6, r3
      hln.ident := IFUNCDEF2(hln.name, code, 24, [0,0], [], [0], 1)

#ifdef PPC_PATCH_EXEC
      
      hln := getLabelHLN('AddHead') -> v56
      code := [$80A30000,  -> lwz r5, 0(r3) # prev := list.head
      $90830000,  -> stw r4, 0(r3) # list.head := node
      $90A40000,  -> stw r5, 0(r4) # node.succ := prev
      $90640004,  -> stw r3, 4(r4) # node.pred := list
      $90850004] -> stw r4, 4(r5) # list.head.succ.pred := list.head
      hln.ident := IFUNCDEF2(hln.name, code, 20, [0,0], [], [0], 1)

      hln := getLabelHLN('AddTail') -> v56
      code := [$80A30008,  -> lwz r5, 8(r3) # prev := list.tailpred
      $90830008,  -> stw r4, 8(r3) # list.tailpred := node
      $90A40004,  -> stw r5, 4(r4) # node.pred := prev
      $38630004,  -> addi r3, r3,4 # list+4
      $90640000,  -> stw r3, 0(r4) # node.succ := list+4
      $90850000] -> stw r4, 0(r5) # list.tailpred.pred.succ := list.tailpred
      hln.ident := IFUNCDEF2(hln.name, code, 24, [0,0], [], [0], 1)

      hln := getLabelHLN('Remove') -> v56
      code := [$80830000,  -> lwz r4, 0(r3) # node.succ
      $80630004,  -> lwz r3, 4(r3) # node.pred
      $90830000,  -> stw r4, 0(r3) # node.pred.succ := node.succ
      $90640004] -> stw r3, 4(r4) # node.succ.pred := node.pred
      hln.ident := IFUNCDEF2(hln.name, code, 16, [0], [], [0], 1)

#endif

      hln := getLabelHLN('NewList') -> v56
      code := [$38830004,  -> addi r4, r3, 4
      $38000000,  -> addi r0, 0, 0
      $90830000,  -> stw r4, 0(r3) # head
      $90030004,  -> stw r0, 4(r3)
      $90630008] -> stw r3, 8(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 20, [0], [], [0], 1)

      hln := getLabelHLN('PutWide') -> v56
      code := [$90830000,  -> stw r4,0(r3)
      $90A30004] -> stw r5,4(r3)
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0,2], [], [0], 1)

      hln := getLabelHLN('Wide') -> v56
      code := [$7C651B78,  -> or r5,r3,r3
      $80650000,  -> lwz r3,0(r5)
      $80850004] -> lwz r4,4(r5)
      hln.ident := IFUNCDEF2(hln.name, code, 12, [0], [], [2], 1)

      hln := getLabelHLN('UlongToWide') -> 2.1
      code := [$7C641B78, -> or r4,r3,r3
      $38600000]          -> addi r3,r0,0
      hln.ident := IFUNCDEF2(hln.name, code, 8, [0], [], [2], 1)

ENDPROC


EXPORT PROC addPPCRegs()
   DEF hln:PTR TO hln, r:PTR TO reg

r:=[IDENT_REG,RX,0,0,'R0',
    IDENT_REG,RX,1,0,'R1',
    IDENT_REG,RX,2,0,'R2',
    IDENT_REG,RX,3,0,'R3',
    IDENT_REG,RX,4,0,'R4',
    IDENT_REG,RX,5,0,'R5',
    IDENT_REG,RX,6,0,'R6',
    IDENT_REG,RX,7,0,'R7',
    IDENT_REG,RX,8,0,'R8',
    IDENT_REG,RX,9,0,'R9',
    IDENT_REG,RX,10,0,'R10',
    IDENT_REG,RX,11,0,'R11',
    IDENT_REG,RX,12,0,'R12',
    IDENT_REG,RX,13,0,'R13',
    IDENT_REG,RX,14,0,'R14',
    IDENT_REG,RX,15,0,'R15',
    IDENT_REG,RX,16,0,'R16',
    IDENT_REG,RX,17,0,'R17',
    IDENT_REG,RX,18,0,'R18',
    IDENT_REG,RX,19,0,'R19',
    IDENT_REG,RX,20,0,'R20',
    IDENT_REG,RX,21,0,'R21',
    IDENT_REG,RX,22,0,'R22',
    IDENT_REG,RX,23,0,'R23',
    IDENT_REG,RX,24,0,'R24',
    IDENT_REG,RX,25,0,'R25',
    IDENT_REG,RX,26,0,'R26',
    IDENT_REG,RX,27,0,'R27',
    IDENT_REG,RX,28,0,'R28',
    IDENT_REG,RX,29,0,'R29',
    IDENT_REG,RX,30,0,'R30',
    IDENT_REG,RX,31,0,'R31',

    IDENT_REG,FPX,0,0,'F0',
    IDENT_REG,FPX,1,0,'F1',
    IDENT_REG,FPX,2,0,'F2',
    IDENT_REG,FPX,3,0,'F3',
    IDENT_REG,FPX,4,0,'F4',
    IDENT_REG,FPX,5,0,'F5',
    IDENT_REG,FPX,6,0,'F6',
    IDENT_REG,FPX,7,0,'F7',
    IDENT_REG,FPX,8,0,'F8',
    IDENT_REG,FPX,9,0,'F9',
    IDENT_REG,FPX,10,0,'F10',
    IDENT_REG,FPX,11,0,'F11',
    IDENT_REG,FPX,12,0,'F12',
    IDENT_REG,FPX,13,0,'F13',
    IDENT_REG,FPX,14,0,'F14',
    IDENT_REG,FPX,15,0,'F15',
    IDENT_REG,FPX,16,0,'F16',
    IDENT_REG,FPX,17,0,'F17',
    IDENT_REG,FPX,18,0,'F18',
    IDENT_REG,FPX,19,0,'F19',
    IDENT_REG,FPX,20,0,'F20',
    IDENT_REG,FPX,21,0,'F21',
    IDENT_REG,FPX,22,0,'F22',
    IDENT_REG,FPX,23,0,'F23',
    IDENT_REG,FPX,24,0,'F24',
    IDENT_REG,FPX,25,0,'F25',
    IDENT_REG,FPX,26,0,'F26',
    IDENT_REG,FPX,27,0,'F27',
    IDENT_REG,FPX,28,0,'F28',
    IDENT_REG,FPX,29,0,'F29',
    IDENT_REG,FPX,30,0,'F30',
    IDENT_REG,FPX,31,0,'F31',

    IDENT_REG,VX,0,0,'V0',
    IDENT_REG,VX,1,0,'V1',
    IDENT_REG,VX,2,0,'V2',
    IDENT_REG,VX,3,0,'V3',
    IDENT_REG,VX,4,0,'V4',
    IDENT_REG,VX,5,0,'V5',
    IDENT_REG,VX,6,0,'V6',
    IDENT_REG,VX,7,0,'V7',
    IDENT_REG,VX,8,0,'V8',
    IDENT_REG,VX,9,0,'V9',
    IDENT_REG,VX,10,0,'V10',
    IDENT_REG,VX,11,0,'V11',
    IDENT_REG,VX,12,0,'V12',
    IDENT_REG,VX,13,0,'V13',
    IDENT_REG,VX,14,0,'V14',
    IDENT_REG,VX,15,0,'V15',
    IDENT_REG,VX,16,0,'V16',
    IDENT_REG,VX,17,0,'V17',
    IDENT_REG,VX,18,0,'V18',
    IDENT_REG,VX,19,0,'V19',
    IDENT_REG,VX,20,0,'V20',
    IDENT_REG,VX,21,0,'V21',
    IDENT_REG,VX,22,0,'V22',
    IDENT_REG,VX,23,0,'V23',
    IDENT_REG,VX,24,0,'V24',
    IDENT_REG,VX,25,0,'V25',
    IDENT_REG,VX,26,0,'V26',
    IDENT_REG,VX,27,0,'V27',
    IDENT_REG,VX,28,0,'V28',
    IDENT_REG,VX,29,0,'V29',
    IDENT_REG,VX,30,0,'V30',
    IDENT_REG,VX,31,0,'V31',
    NIL]:reg

   WHILE r.identID
      hln := getLabelHLN(r.name)
      hln.ident := r
      r++
   ENDWHILE

ENDPROC




EXPORT PROC addPPCInstructions()
   aZZZ()
   bZZZ()
   cZZZ()
   dZZZ()
   eZZZ()
   fZZZ()
   iZZZ()
   lZZZ()
   mZZZ()
   nZZZ()
   oZZZ()
   rZZZ()
   sZZZ()
   tZZZ()
   xZZZ()
   customZZZ()
ENDPROC

#define addZZZ(name,ops,punct,qcode) addBPPCInst([IDENT2_ASM,name,\
                                     [ops,ListLen(ops),punct,qcode]:instppc]:asm)


PROC addBPPCInst(a:PTR TO asm)
   DEF hln:PTR TO hln
   hln := getLabelHLN(a.name)
   hln.ident2 := a
ENDPROC



/*         name        ops     P  create-code */

/*
** name: name
**
** ops:  0=Rx
**       1=imm
**       2=Fx
**       3=label(pc)
** P: 0=punctation optional
**    1=no punctation allowed
**    2=punctation required
**
*/

PROC aZZZ()
   addZZZ('ADD',      [RX,RX,RX],   PO, `ppcadd(A_,B_,C_,0,P_))
   addZZZ('ADDO',     [RX,RX,RX],   PO, `ppcadd(A_,B_,C_,1,P_))
   addZZZ('ADDC',     [RX,RX,RX],   PO, `ppcaddc(A_,B_,C_,0,P_))
   addZZZ('ADDCO',    [RX,RX,RX],   PO, `ppcaddc(A_,B_,C_,1,P_))
   addZZZ('ADDE',     [RX,RX,RX],   PO, `ppcadde(A_,B_,C_,0,P_))
   addZZZ('ADDEO',    [RX,RX,RX],   PO, `ppcadde(A_,B_,C_,1,P_))
   addZZZ('ADDI',     [RX,RX,DV],   PN, `IF B_ <> -1 THEN ppcaddi(A_,B_,C_) ELSE ppcaddilab(A_,GLOBREG,C_))
   addZZZ('ADDIC',    [RX,RX,DV],   PN, `ppcaddic(A_,B_,C_))
   addZZZ('ADDIC',    [RX,RX,DV],   PR, `ppcaddic(A_,B_,C_))
   addZZZ('ADDIS',    [RX,RX,DV],   PN, `ppcaddis(A_,B_,C_))
   addZZZ('ADDME',    [RX,RX],      PO, `ppcaddme(A_,B_,0,P_))
   addZZZ('ADDMEO',   [RX,RX],      PO, `ppcaddme(A_,B_,1,P_))
   addZZZ('ADDZE',    [RX,RX],      PO, `ppcaddze(A_,B_,0,P_))
   addZZZ('ADDZEO',   [RX,RX],      PO, `ppcaddze(A_,B_,1,P_))
   addZZZ('AND',      [RX,RX,RX],   PO, `ppcand(A_,B_,C_,P_))
   addZZZ('ANDC',     [RX,RX,RX],   PO, `ppcandc(A_,B_,C_,P_))
   addZZZ('ANDI',     [RX,RX,DV],   PR, `ppcandi_(A_,B_,C_))
   addZZZ('ANDIS',    [RX,RX,DV],   PR, `ppcandis_(A_,B_,C_))
ENDPROC
PROC bZZZ()
   addZZZ('B',        [LAB],        PN, `ppcblab(A_,0,0))
 /*addZZZ('BA',       [LAB],        PN, `ppcb(A,1,0]) NOT SUPPORTED */
   addZZZ('BL',       [LAB],        PN, `ppcblab(A_,0,1))
 /*addZZZ('BLA',      [LAB],        PN, `ppcb(A,1,1]) NOT SUPPORTED */
   addZZZ('BC',       [DV,DV,LAB],  PN, `ppcbclab(A_,B_,C_,0,0))
 /*addZZZ('BCA',      [DV,DV,DV],   PN, `ppcbc(A_,B_,C_,1,0]) NOT SUPPORTED */
   addZZZ('BCL',      [DV,DV,LAB],  PN, `ppcbclab(A_,B_,C_,0,1))
 /*addZZZ('BCLA',     [DV,DV,DV],   PN, `ppcbc(A_,B_,C_,1,1]) NOT SUPPORTED */
   addZZZ('BCCTR',    [DV,DV],      PN, `ppcbcctr(A_,B_,0))
   addZZZ('BCCTRL',   [DV,DV],      PN, `ppcbcctr(A_,B_,1))
   addZZZ('BCLR',     [DV,DV],      PN, `ppcbclr(A_,B_,0))
   addZZZ('BCLRL',    [DV,DV],      PN, `ppcbclr(A_,B_,1))
ENDPROC
PROC cZZZ()
   addZZZ('CMP',  [DV,DV,RX,RX],    PN, `ppccmp(A_,B_,C_,D_))
   addZZZ('CMPI', [DV,DV,RX,DV],    PN, `ppccmpi(A_,B_,C_,D_))
   addZZZ('CMPL', [DV,DV,RX,RX],    PN, `ppccmpl(A_,B_,C_,D_))
   addZZZ('CMPLI', [DV,DV,RX,DV],   PN, `ppccmpli(A_,B_,C_,D_))
   addZZZ('CNTLZD',   [RX,RX],      PO, `ppccntlzd(A_,B_,P_)) -> 64
   addZZZ('CNTLZW',   [RX,RX],      PO, `ppccntlzw(A_,B_,P_))
   addZZZ('CRAND',    [DV,DV,DV],   PN, `ppccrand(A_,B_,C_))
   addZZZ('CRANDC',   [DV,DV,DV],   PN, `ppccrandc(A_,B_,C_))
   addZZZ('CREQV',    [DV,DV,DV],   PN, `ppccreqv(A_,B_,C_))
   addZZZ('CRNAND',   [DV,DV,DV],   PN, `ppccrnand(A_,B_,C_))
   addZZZ('CRNOR',    [DV,DV,DV],   PN, `ppccrnor(A_,B_,C_))
   addZZZ('CROR',     [DV,DV,DV],   PN, `ppccror(A_,B_,C_))
   addZZZ('CRORC',    [DV,DV,DV],   PN, `ppccrorc(A_,B_,C_))
   addZZZ('CRXOR',    [DV,DV,DV],   PN, `ppccrxor(A_,B_,C_))
ENDPROC
PROC dZZZ()
   addZZZ('DCBA',     [RX,RX],      PN, `ppcdcba(A_,B_))
   addZZZ('DCBF',     [RX,RX],      PN, `ppcdcbf(A_,B_))
   addZZZ('DCBI',     [RX,RX],      PN, `ppcdcbi(A_,B_))
   addZZZ('DCBST',    [RX,RX],      PN, `ppcdcbst(A_,B_))
   addZZZ('DCBT',     [RX,RX],      PN, `ppcdcbt(A_,B_))
   addZZZ('DCBTST',   [RX,RX],      PN, `ppcdcbtst(A_,B_))
   addZZZ('DCBZ',     [RX,RX],      PN, `ppcdcbz(A_,B_))
   addZZZ('DIVD',   [RX,RX,RX],     PO, `ppcdivd(A_,B_,C_,0,P_)) -> 64
   addZZZ('DIVDO',  [RX,RX,RX],     PO, `ppcdivd(A_,B_,C_,1,P_)) -> 64
   addZZZ('DIVDU',  [RX,RX,RX],     PO, `ppcdivdu(A_,B_,C_,0,P_)) -> 64
   addZZZ('DIVDOU', [RX,RX,RX],     PO, `ppcdivdu(A_,B_,C_,1,P_)) -> 64
   addZZZ('DIVW',   [RX,RX,RX],     PO, `ppcdivw(A_,B_,C_,0,P_))
   addZZZ('DIVWO',  [RX,RX,RX],     PO, `ppcdivw(A_,B_,C_,1,P_))
   addZZZ('DIVWU',  [RX,RX,RX],     PO, `ppcdivwu(A_,B_,C_,0,P_))
   addZZZ('DIVWUO', [RX,RX,RX],     PO, `ppcdivwu(A_,B_,C_,1,P_))
ENDPROC
PROC eZZZ()
   addZZZ('ECIWX',  [RX,RX,RX],     PN, `ppceciwx(A_,B_,C_))
   addZZZ('ECOWX',  [RX,RX,RX],     PN, `ppcecowx(A_,B_,C_))
   addZZZ('EIEIO',  [],             PN, `ppceieio())
   addZZZ('EQV',    [RX,RX,RX],     PO, `ppceqv(A_,B_,C_,P_))
   addZZZ('EXTSB',  [RX,RX],        PO, `ppcextsb(A_,B_,P_))
   addZZZ('EXTSH',  [RX,RX],        PO, `ppcextsh(A_,B_,P_))
   addZZZ('EXTSW',  [RX,RX],        PO, `ppcextsw(A_,B_,P_)) -> 64
ENDPROC
PROC fZZZ()
   addZZZ('FABS',   [FPX,FPX],      PO, `ppcfabs(A_,B_,P_))
   addZZZ('FADD',   [FPX,FPX,FPX],  PO, `ppcfadd(A_,B_,C_,P_))
   addZZZ('FADDS',  [FPX,FPX,FPX],  PO, `ppcfadds(A_,B_,C_,P_))
   addZZZ('FCFID',  [FPX,FPX],      PO, `ppcfcfid(A_,B_,P_))
   addZZZ('FCMPO',  [DV,FPX,FPX],   PN, `ppcfcmpo(A_,B_,C_))
   addZZZ('FCMPU',  [DV,FPX,FPX],   PN, `ppcfcmpu(A_,B_,C_))
   addZZZ('FCTID',  [FPX,FPX],      PO, `ppcfctid(A_,B_,P_)) -> 64
   addZZZ('FCTIDZ', [FPX,FPX],      PO, `ppcfctidz(A_,B_,P_)) -> 64
   addZZZ('FCTIW',  [FPX,FPX],      PO, `ppcfctiw(A_,B_,P_))
   addZZZ('FCTIWZ', [FPX,FPX],      PO, `ppcfctiwz(A_,B_,P_))
   addZZZ('FDIV',   [FPX,FPX,FPX],  PO, `ppcfdiv(A_,B_,C_,P_))
   addZZZ('FDIVS',  [FPX,FPX,FPX],  PO, `ppcfdivs(A_,B_,C_,P_))
   addZZZ('FMADD',  [FPX,FPX,FPX,FPX], PO, `ppcfmadd(A_,B_,C_,D_,P_))
   addZZZ('FMADDS', [FPX,FPX,FPX,FPX], PO, `ppcfmadds(A_,B_,C_,D_,P_))
   addZZZ('FMR',    [FPX,FPX],      PO, `ppcfmr(A_,B_,P_))
   addZZZ('FMSUB',  [FPX,FPX,FPX,FPX], PO, `ppcfmsub(A_,B_,C_,D_,P_))
   addZZZ('FMSUBS', [FPX,FPX,FPX,FPX], PO, `ppcfmsubs(A_,B_,C_,D_,P_))
   addZZZ('FMUL',   [FPX,FPX,FPX],  PO, `ppcfmul(A_,B_,C_,P_))
   addZZZ('FMULS',  [FPX,FPX,FPX],  PO, `ppcfmuls(A_,B_,C_,P_))
   addZZZ('FNABS',  [FPX,FPX],      PO, `ppcfnabs(A_,B_,P_))
   addZZZ('FNEG',   [FPX,FPX],      PO, `ppcfneg(A_,B_,P_))
   addZZZ('FNMADD', [FPX,FPX,FPX,FPX], PO, `ppcfnmadd(A_,B_,C_,D_,P_))
   addZZZ('FNMADDS',[FPX,FPX,FPX,FPX], PO, `ppcfnmadds(A_,B_,C_,D_,P_))
   addZZZ('FNMSUB', [FPX,FPX,FPX,FPX], PO, `ppcfnmsub(A_,B_,C_,D_,P_))
   addZZZ('FNMSUBS',[FPX,FPX,FPX,FPX], PO, `ppcfnmsubs(A_,B_,C_,D_,P_))
   addZZZ('FRES',   [FPX,FPX],      PO, `ppcfres(A_,B_,P_))
   addZZZ('FRSP',   [FPX,FPX],      PO, `ppcfrsp(A_,B_,P_))
   addZZZ('FRSQRTE', [FPX,FPX],     PO, `ppcfrsqrte(A_,B_,P_))
   addZZZ('FSEL',   [FPX,FPX,FPX,FPX], PO, `ppcfsel(A_,B_,C_,D_,P_))
   addZZZ('FSQRT',  [FPX,FPX],      PO, `ppcfsqrt(A_,B_,P_))
   addZZZ('FSQRTS', [FPX,FPX],      PO, `ppcfsqrts(A_,B_,P_))
   addZZZ('FSUB',   [FPX,FPX,FPX],  PO, `ppcfsub(A_,B_,C_,P_))
   addZZZ('FSUBS',  [FPX,FPX,FPX],  PO, `ppcfsubs(A_,B_,C_,P_))
ENDPROC
PROC iZZZ()
   addZZZ('ICBI',   [RX,RX],        PN, `ppcicbi(A_,B_))
   addZZZ('ISYNC',  [],             PN, `ppcisync())
ENDPROC
PROC lZZZ()
   addZZZ('LBZ',    [RX,RX,DV],   PN, `ppclbz(A_,B_,C_))
   addZZZ('LBZU',   [RX,RX,DV],   PN, `ppclbzu(A_,B_,C_))
   addZZZ('LBZUX',  [RX,RX,RX],   PN, `ppclbzux(A_,B_,C_))
   addZZZ('LBZX',   [RX,RX,RX],   PN, `ppclbzx(A_,B_,C_))
   addZZZ('LD',     [RX,RX,DV],   PN, `ppcld(A_,B_,C_)) -> 64
   addZZZ('LDARX',  [RX,RX,RX],   PN, `ppcldarx(A_,B_,C_)) -> 64
   addZZZ('LDU',    [RX,RX,DV],   PN, `ppcldu(A_,B_,C_)) -> 64
   addZZZ('LDUX',   [RX,RX,RX],   PN, `ppcldux(A_,B_,C_)) -> 64
   addZZZ('LDX',    [RX,RX,RX],   PN, `ppcldx(A_,B_,C_)) -> 64
   addZZZ('LFD',    [FPX,RX,DV],   PN, `ppclfd(A_,B_,C_))
   addZZZ('LFDU',   [FPX,RX,DV],   PN, `ppclfdu(A_,B_,C_))
   addZZZ('LFDUX',  [FPX,RX,RX],   PN, `ppclfdux(A_,B_,C_))
   addZZZ('LFDX',   [FPX,RX,RX],   PN, `ppclfdx(A_,B_,C_))
   addZZZ('LFS',    [FPX,RX,DV],   PN, `ppclfs(A_,B_,C_))
   addZZZ('LFSU',   [FPX,RX,DV],   PN, `ppclfsu(A_,B_,C_))
   addZZZ('LFSUX',  [FPX,RX,RX],   PN, `ppclfsux(A_,B_,C_))
   addZZZ('LFSX',   [FPX,RX,RX],   PN, `ppclfsx(A_,B_,C_))
   addZZZ('LHA',    [RX,RX,DV],   PN, `ppclha(A_,B_,C_))
   addZZZ('LHAU',   [RX,RX,DV],   PN, `ppclhau(A_,B_,C_))
   addZZZ('LHAUX',  [RX,RX,RX],   PN, `ppclhaux(A_,B_,C_))
   addZZZ('LHAX',   [RX,RX,RX],   PN, `ppclhax(A_,B_,C_))
   addZZZ('LHBRX',  [RX,RX,RX],   PN, `ppclhbrx(A_,B_,C_))
   addZZZ('LHZ',    [RX,RX,DV],   PN, `ppclhz(A_,B_,C_))
   addZZZ('LHZU',   [RX,RX,DV],   PN, `ppclhzu(A_,B_,C_))
   addZZZ('LHZUX',  [RX,RX,RX],   PN, `ppclhzux(A_,B_,C_))
   addZZZ('LHZX',   [RX,RX,RX],   PN, `ppclhzx(A_,B_,C_))
   addZZZ('LMW',    [RX,RX,DV],   PN, `ppclmw(A_,B_,C_))
   addZZZ('LSWI',   [RX,RX,DV],   PN, `ppclswi(A_,B_,C_))
   addZZZ('LSWX',   [RX,RX,RX],   PN, `ppclswx(A_,B_,C_))
   addZZZ('LWA',    [RX,RX,DV],   PN, `ppclwa(A_,B_,C_)) -> 64
   addZZZ('LWARX',  [RX,RX,RX],   PN, `ppclwarx(A_,B_,C_)) -> 64
   addZZZ('LWAUX',  [RX,RX,RX],   PN, `ppclwaux(A_,B_,C_)) -> 64
   addZZZ('LWAX',   [RX,RX,RX],   PN, `ppclwax(A_,B_,C_)) -> 64
   addZZZ('LWBRX',  [RX,RX,RX],   PN, `ppclwbrx(A_,B_,C_))
   addZZZ('LWZ',    [RX,RX,DV],   PN, `IF B_ <> -1 THEN ppclwz(A_,B_,C_) ELSE ppclwzlab(A_,GLOBREG,C_)) -> support for glob too.
   addZZZ('LWZU',   [RX,RX,DV],   PN, `ppclwzu(A_,B_,C_))
   addZZZ('LWZUX',  [RX,RX,RX],   PN, `ppclwzux(A_,B_,C_))
   addZZZ('LWZX',   [RX,RX,RX],   PN, `ppclwzx(A_,B_,C_))
ENDPROC
PROC mZZZ()
   addZZZ('MCRF',   [DV,DV],     PN, `ppcmcrf(A_,B_))
   addZZZ('MCRFS',   [DV,DV],    PN, `ppcmcrfs(A_,B_))
   addZZZ('MCRXR',   [DV],       PN, `ppcmcrxr(A_))
   addZZZ('MFCR',    [DV],       PN, `ppcmfcr(A_))
   addZZZ('MFFS',    [DV],       PO, `ppcmffs(A_,P_))
   addZZZ('MFMSR',   [RX],       PN, `ppcmfmsr(A_))
   addZZZ('MFSPR',   [RX,DV],    PN, `ppcmfspr(A_,B_))
   addZZZ('MFSR',    [RX,DV],    PN, `ppcmfsr(A_,B_))
   addZZZ('MFSRIN',  [RX,RX],    PN, `ppcmfsrin(A_,B_))
   addZZZ('MFTB',    [RX,DV],    PN, `ppcmftb(A_,B_))
   addZZZ('MTCRF',   [DV,RX],    PN, `ppcmtcrf(B_,A_))
   addZZZ('MTFSB0',  [DV],       PO, `ppcmtfsb0(A_,P_))
   addZZZ('MTFSB1',  [DV],       PO, `ppcmtfsb1(A_,P_))
   addZZZ('MTFSF',   [DV,FPX],   PO, `ppcmtfsf(A_,B_,P_))
   addZZZ('MTFSFI',  [DV,DV],    PO, `ppcmtfsfi(A_,B_,P_))
   addZZZ('MTMSR',   [RX],       PN, `ppcmtmsr(A_))
   addZZZ('MTSPR',   [DV,RX],    PN, `ppcmtspr(B_,A_))
   addZZZ('MTSR',    [DV,RX],    PN, `ppcmtsr(B_,A_))
   addZZZ('MTSRIN',  [RX,RX],    PN, `ppcmtsrin(A_,B_))
   addZZZ('MULHD',   [RX,RX,RX],  PO, `ppcmulhd(A_,B_,C_,P_)) -> 64
   addZZZ('MULHDU',  [RX,RX,RX],  PO, `ppcmulhdu(A_,B_,C_,P_)) -> 64
   addZZZ('MULHW',   [RX,RX,RX],  PO, `ppcmulhw(A_,B_,C_,P_))
   addZZZ('MULHWU',  [RX,RX,RX],  PO, `ppcmulhwu(A_,B_,C_,P_))
   addZZZ('MULLD',   [RX,RX,RX],  PO, `ppcmulld(A_,B_,C_,0,P_)) -> 64
   addZZZ('MULLDO',  [RX,RX,RX],  PO, `ppcmulld(A_,B_,C_,1,P_)) -> 64
   addZZZ('MULLI',   [RX,RX,DV],  PN, `ppcmulli(A_,B_,C_))
   addZZZ('MULLW',   [RX,RX,RX],  PO, `ppcmullw(A_,B_,C_,0,P_))
   addZZZ('MULLWO',  [RX,RX,RX],  PO, `ppcmullw(A_,B_,C_,1,P_))
ENDPROC
PROC nZZZ()
   addZZZ('NAND',    [RX,RX,RX],    PO, `ppcnand(B_,A_,C_,P_))
   addZZZ('NEG',     [RX,RX],       PO, `ppcneg(A_,B_,0,P_))
   addZZZ('NEGO',    [RX,RX],       PO, `ppcneg(A_,B_,1,P_))
   addZZZ('NOR',     [RX,RX,RX],    PO, `ppcnor(B_,A_,C_,P_))
ENDPROC
PROC oZZZ()
   addZZZ('OR',      [RX,RX,RX],  PO, `ppcor(B_,A_,C_,P_))
   addZZZ('ORC',     [RX,RX,RX],  PO, `ppcorc(B_,A_,C_,P_))
   addZZZ('ORI',     [RX,RX,DV],  PN, `ppcori(B_,A_,C_))
   addZZZ('ORIS',    [RX,RX,DV],  PN, `ppcoris(B_,A_,C_))
ENDPROC
PROC rZZZ()
   addZZZ('RFI',     [],            PN, `ppcrfi())
   addZZZ('RLDCL', [RX,RX,DV,DV],   PO, `ppcrldcl(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLDCR', [RX,RX,RX,DV],   PO, `ppcrldcr(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLDIC', [RX,RX,DV,DV],   PO, `ppcrldic(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLDICL',[RX,RX,DV,DV],   PO, `ppcrldicl(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLDICR',[RX,RX,DV,DV],   PO, `ppcrldicr(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLDIMI',[RX,RX,DV,DV],   PO, `ppcrldimi(A_,B_,C_,D_,P_)) -> 64 -> ??
   addZZZ('RLWIMI',[RX,RX,DV,DV,DV],PO, `ppcrlwimi(B_,A_,C_,D_,E_,P_))
   addZZZ('RLWINM',[RX,RX,DV,DV,DV],PO, `ppcrlwinm(B_,A_,C_,D_,E_,P_))
   addZZZ('RLWNM', [RX,RX,DV,DV,DV],PO, `ppcrlwnm(B_,A_,C_,D_,E_,P_))
ENDPROC
PROC sZZZ()
   addZZZ('SC',       [],        PO, `ppcsc())
   addZZZ('SLBIA',    [],        PO, `ppcslbia()) -> 64
   addZZZ('SLBIE',    [RX],      PO, `ppcslbie(A_)) -> 64
   addZZZ('SLD',      [RX,RX,DV], PO, `ppcsld(B_,A_,C_,P_)) -> 64
   addZZZ('SLW',      [RX,RX,RX], PO, `ppcslw(B_,A_,C_,P_))
   addZZZ('SRAD',     [RX,RX,RX], PO, `ppcsrad(B_,A_,C_,P_)) -> 64
   addZZZ('SRADI',    [RX,RX,DV], PO, `ppcsradi(B_,A_,C_,P_)) -> 64
   addZZZ('SRAW',     [RX,RX,RX], PO, `ppcsraw(B_,A_,C_,P_))
   addZZZ('SRAWI',    [RX,RX,DV], PO, `ppcsrawi(B_,A_,C_,P_))
   addZZZ('SRD',      [RX,RX,RX], PO, `ppcsrd(B_,A_,C_,P_)) -> 64
   addZZZ('SRW',      [RX,RX,RX], PO, `ppcsrw(B_,A_,C_,P_))
   addZZZ('STB',      [RX,RX,DV], PN, `ppcstb(A_,B_,C_))
   addZZZ('STBU',     [RX,RX,DV], PN, `ppcstbu(A_,B_,C_))
   addZZZ('STBUX',    [RX,RX,RX], PN, `ppcstbux(A_,B_,C_))
   addZZZ('STBX',     [RX,RX,RX], PN, `ppcstbx(A_,B_,C_))
   addZZZ('STD',      [RX,RX,DV], PN, `ppcstd(A_,B_,C_)) -> 64
   addZZZ('STDCX',    [RX,RX,RX], PR, `ppcstdcx_(A_,B_,C_)) -> 64
   addZZZ('STDU',     [RX,RX,DV], PN, `ppcstdu(A_,B_,C_)) -> 64
   addZZZ('STDUX',    [RX,RX,RX], PN, `ppcstdux(A_,B_,C_)) -> 64
   addZZZ('STDX',     [RX,RX,RX], PN, `ppcstdx(A_,B_,C_)) -> 64
   addZZZ('STFD',     [FPX,RX,DV], PN, `ppcstfd(A_,B_,C_))
   addZZZ('STFDU',    [FPX,RX,DV], PN, `ppcstfdu(A_,B_,C_))
   addZZZ('STFDUX',   [FPX,RX,RX], PN, `ppcstfdux(A_,B_,C_))
   addZZZ('STFDX',    [FPX,RX,RX], PN, `ppcstfdx(A_,B_,C_))
   addZZZ('STFIWX',   [FPX,RX,RX], PN, `ppcstfiwx(A_,B_,C_))
   addZZZ('STFS',     [FPX,RX,DV], PN, `ppcstfs(A_,B_,C_))
   addZZZ('STFSU',    [FPX,RX,DV], PN, `ppcstfsu(A_,B_,C_))
   addZZZ('STFSUX',   [FPX,RX,RX], PN, `ppcstfsux(A_,B_,C_))
   addZZZ('STFSX',    [FPX,RX,RX], PN, `ppcstfsx(A_,B_,C_))
   addZZZ('STH',      [RX,RX,DV], PN, `ppcsth(A_,B_,C_))
   addZZZ('STHBRX',   [RX,RX,RX], PN, `ppcsthbrx(A_,B_,C_))
   addZZZ('STHU',     [RX,RX,DV], PN, `ppcsthu(A_,B_,C_))
   addZZZ('STHUX',    [RX,RX,RX], PN, `ppcsthux(A_,B_,C_))
   addZZZ('STHX',     [RX,RX,RX], PN, `ppcsthx(A_,B_,C_))
   addZZZ('STMW',     [RX,RX,DV], PN, `ppcstmw(A_,B_,C_))
   addZZZ('STSWI',    [RX,RX,DV], PN, `ppcstswi(A_,B_,C_))
   addZZZ('STSWX',    [RX,RX,RX], PN, `ppcstswx(A_,B_,C_))
   addZZZ('STW',      [RX,RX,DV], PN, `IF B_ <> -1 THEN ppcstw(A_,B_,C_) ELSE ppcstwlab(A_,GLOBREG,C_)) -> supports globals too
   addZZZ('STWBRX',   [RX,RX,RX], PN, `ppcstwbrx(A_,B_,C_))
   addZZZ('STWCX',    [RX,RX,RX], PR, `ppcstwcx_(A_,B_,C_))
   addZZZ('STWU',     [RX,RX,DV], PN, `ppcstwu(A_,B_,C_))
   addZZZ('STWUX',    [RX,RX,RX], PN, `ppcstwux(A_,B_,C_))
   addZZZ('STWX',     [RX,RX,RX], PN, `ppcstwx(A_,B_,C_))
   addZZZ('SUBF',     [RX,RX,RX], PO, `ppcsubf(A_,B_,C_,0,P_))
   addZZZ('SUBFO',    [RX,RX,RX], PO, `ppcsubf(A_,B_,C_,1,P_))
   addZZZ('SUBFC',    [RX,RX,RX], PO, `ppcsubfc(A_,B_,C_,0,P_))
   addZZZ('SUBFCO',   [RX,RX,RX], PO, `ppcsubfc(A_,B_,C_,1,P_))
   addZZZ('SUBFE',    [RX,RX,RX], PO, `ppcsubfe(A_,B_,C_,0,P_))
   addZZZ('SUBFEO',   [RX,RX,RX], PO, `ppcsubfe(A_,B_,C_,1,P_))
   addZZZ('SUBFIC',   [RX,RX,DV], PN, `ppcsubfic(A_,B_,C_))
   addZZZ('SUBFME',   [RX,RX],   PO, `ppcsubfme(A_,B_,0,P_))
   addZZZ('SUBFMEO',  [RX,RX],   PO, `ppcsubfme(A_,B_,1,P_))
   addZZZ('SUBFZE',   [RX,RX],   PO, `ppcsubfze(A_,B_,0,P_))
   addZZZ('SUBFZEO',  [RX,RX],   PO, `ppcsubfze(A_,B_,1,P_))
   addZZZ('SYNC',     [],        PN, `ppcsync())
ENDPROC
PROC tZZZ()
   addZZZ('TD',       [DV,RX,RX],PN, `ppctd(A_,B_,C_)) -> 64
   addZZZ('TDI',      [DV,RX,DV],PN, `ppctdi(A_,B_,C_)) -> 64
   addZZZ('TLBIA',    [],        PN, `ppctlbia())
   addZZZ('TLBIE',    [DV],      PN, `ppctlbie(A_))
   addZZZ('TLBSYNC',  [],        PN, `ppctlbsync())
   addZZZ('TW',       [DV,RX,RX], PN, `ppctw(A_,B_,C_))
   addZZZ('TWI',      [DV,RX,DV], PN, `ppctwi(A_,B_,C_))
ENDPROC
PROC xZZZ()
   addZZZ('XOR',      [RX,RX,RX], PO, `ppcxor(B_,A_,C_,P_))
   addZZZ('XORI',     [RX,RX,DV], PN, `ppcxori(B_,A_,C_))
   addZZZ('XORIS',    [RX,RX,DV], PN, `ppcxoris(B_,A_,C_))
ENDPROC
PROC customZZZ()
   addZZZ('LA',     [RX,LAB],     PN, `ppclacode(A_,B_))
   addZZZ('LIW',    [RX,DV],      PN, `ppcliw(A_,B_))    -> v48
   addZZZ('NOP',    [],           PN, `ppcor(0,0,0,0))   -> v48
ENDPROC

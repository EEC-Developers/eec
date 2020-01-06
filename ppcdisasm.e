
/*
 The zlib/libpng License

 Copyright (c) 2002-2007 Leif Salomonsson

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.

*/

OPT MODULE, PREPROCESS

-> Jan 2004, fixed mtspr, mfspr

-> Oct 2005: fabs was wrong.


#ifndef ECX_VERSION
PROC shr(x,v)
   MOVE.L x, D0
   MOVE.L v, D1
   LSR.L D1, D0
ENDPROC D0
#endif

#ifdef ECX_VERSION
#define shr(x,v) ((x) SHR (v))
#endif

EXPORT PROC dodis(cofs, long, str)
   DEF zeroFive,           -> 6bit primary opcode field
       sixTen,             -> 5bit
       elevenFifteen,      -> 5bit
       sixteenTwenty,      -> 5bit
       twentyoneTwentyfive,-> 5bit
       twentysixThirty,    -> 5bit
       thirtyone,          -> 1bit
       thirty,             -> 1bit
       twentyone,          -> 1bit
       twentytwoThirty,    -> 9bit secondary opcode field
       sixteenThirtyone,   -> 16bit signed
       twentyoneThirty:REG,-> 10bit secondary opcode field
       elevenTwenty,       -> 10bit
       sixFifteen,         -> 10bit
       sixTwentynine,      -> 24bit signed
       sixteenTwentynine,  -> 14bit signed
       twentyoneTwentysix, -> 6bit
       twentysevenThirty   -> 4bit

   DEF t
   DEF p, l, o, a, e

   p := '.'
   l := 'l'
   o := 'o'
   a := 'a'
   e := ''


   /* upd vars */
   zeroFive := shr(long, 26)
   sixTen := shr(long, 21) AND %11111
   elevenFifteen := shr(long, 16) AND %11111
   sixteenTwenty := shr(long, 11) AND %11111
   twentyoneTwentyfive := shr(long, 6) AND %11111
   twentysixThirty := shr(long, 1) AND %11111
   thirtyone := long AND %1
   thirty := shr(long, 1) AND %1
   twentyone := shr(long, 10) AND %1
   twentytwoThirty := shr(long, 1) AND %111111111
   sixteenThirtyone := long AND $FFFF
   IF sixteenThirtyone AND $8000 THEN sixteenThirtyone := sixteenThirtyone OR $FFFF0000
   twentyoneThirty := shr(long, 1) AND %1111111111
   elevenTwenty := shr(long, 11) AND %1111111111
   sixFifteen := shr(long, 16) AND %1111111111
   sixTwentynine := shr(long, 2) AND %111111111111111111111111
   IF sixTwentynine AND $800000 THEN sixTwentynine := sixTwentynine OR $FF000000
   sixteenTwentynine := shr(long, 2) AND %11111111111111
   IF sixteenTwentynine AND $2000 THEN sixteenTwentynine := sixteenTwentynine OR $FFFFC000
   twentyoneTwentysix := shr(long, 5) AND %111111
   twentysevenThirty := shr(long, 1) AND %1111

   StrCopy(str, '-?0?-')

   SELECT 64 OF zeroFive
   CASE %000010 ; StringF(str,'tdi r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %000011 ; StringF(str,'twi r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %000111 ; StringF(str,'mulli r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %001000 ; StringF(str,'subfic r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %001010 ; StringF(str,'cmpli \d,r\d,\d',shr(sixTen,2),elevenFifteen,sixteenThirtyone)
   CASE %001011 ; StringF(str,'cmpi \d,r\d,\d',shr(sixTen,2),elevenFifteen,sixteenThirtyone)
   CASE %001100 ; StringF(str,'addic r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %001101 ; StringF(str,'addic. r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %001110 ; StringF(str,'addi r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %001111 ; StringF(str,'addis r\d,r\d,\d',sixTen,elevenFifteen,sixteenThirtyone)
   CASE %010000 ; StringF(str,'bc\s\s \d,\d,L\h',IF thirtyone THEN l ELSE e,IF thirty THEN a ELSE e,sixTen,elevenFifteen,Mul(sixteenTwentynine,4)+cofs)
   CASE %010001 ; StringF(str,'sc')
   CASE %010010 ; StringF(str,'b\s\s L\h',IF thirtyone THEN l ELSE e,IF thirty THEN a ELSE e,Mul(sixTwentynine,4)+cofs)
   CASE %010011
      SELECT twentyoneThirty
      CASE %0000000000 ; StringF(str,'mcrf \d,\d',shr(sixTen,2),shr(elevenFifteen,2))
      CASE %0000010000 ; StringF(str,'bclr\s \d,\d',IF thirtyone THEN l ELSE e,sixTen,elevenFifteen)
      CASE %0000100001 ; StringF(str,'crnor \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0000110001 ; StringF(str,'rfi')
      CASE %0010000001 ; StringF(str,'crandc \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0010010110 ; StringF(str,'isync')
      CASE %0011000001 ; StringF(str,'crxor \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0011100001 ; StringF(str,'crnand \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100000001 ; StringF(str,'crand \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100100001 ; StringF(str,'creqv \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0110100001 ; StringF(str,'crorc \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0111000001 ; StringF(str,'cror \d,\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1000010000 ; StringF(str,'bcctr\s \d,\d',IF thirtyone THEN l ELSE e, sixTen,elevenFifteen)
      DEFAULT ; ; StringF(str,'-?1?-')
      ENDSELECT
   CASE %010100 ; StringF(str,'rlwimi\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty,twentyoneTwentyfive,twentysixThirty)
   CASE %010101 ; StringF(str,'rlwinm\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty,twentyoneTwentyfive,twentysixThirty)
   CASE %010111 ; StringF(str,'rlwnm\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty,twentyoneTwentyfive,twentysixThirty)
   CASE %011000 ; StringF(str,'ori r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011001 ; StringF(str,'oris r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011010 ; StringF(str,'xori r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011011 ; StringF(str,'xoris r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011100 ; StringF(str,'andi. r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011101 ; StringF(str,'andis. r\d,r\d,\d',elevenFifteen,sixTen,sixteenThirtyone)
   CASE %011110
      t := shr(twentysevenThirty,1)
      SELECT t
      CASE %000 ; StringF(str,'rldicl\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentysix,twentysevenThirty AND %1)
      CASE %001 ; StringF(str,'rldicr\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentysix,twentysevenThirty AND %1)
      CASE %010 ; StringF(str,'rldic\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentysix,twentysevenThirty AND %1)
      CASE %011 ; StringF(str,'rldimi\s r\d,r\d,\d,\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentysix,twentysevenThirty AND %1)
      CASE %100
         IF thirty = 0
            StringF(str,'rldcl\s r\d,r\d,r\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         ELSE
            StringF(str,'rldcr\s r\d,r\d,r\d,\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentysix)
         ENDIF
      DEFAULT ; StringF(str,'-?2?-')
      ENDSELECT
   CASE %011111
      SELECT twentyoneThirty -> 10 bit sec. opcodes
      CASE %0100001010 ; StringF(str,'add\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e, sixTen,elevenFifteen,sixteenTwenty)
      CASE %1100001010 ; StringF(str,'addo\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e, sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100010110 ; StringF(str,'dcbt r\d,r\d',elevenFifteen,sixteenTwenty)
      CASE %0100010111 ; StringF(str,'lhzx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100011100 ; StringF(str,'eqv\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %0100110010 ; StringF(str,'tlbie r\d',sixteenTwenty)
      CASE %0100110110 ; StringF(str,'eciwx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100110111 ; StringF(str,'lhzux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0100111100 ; StringF(str,'xor\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %0101010011 ; StringF(str,'mfspr r\d,\d',sixTen,elevenFifteen)
      CASE %0101010101 ; StringF(str,'lwax\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0101010111 ; StringF(str,'lhax\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0101110010 ; StringF(str,'tlbia')
      CASE %0101110011 ; StringF(str,'mftb r\d,r\d', sixTen,elevenTwenty)
      CASE %0101110101 ; StringF(str,'lwaux\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0101110111 ; StringF(str,'lhaux\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0110010111 ; StringF(str,'sthx\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0110011100 ; StringF(str,'orc\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %1101111011 ; StringF(str,'sradi\s r\d,r\d,\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %0110110010 ; StringF(str,'slbie r\d',sixteenTwenty)
      CASE %0110110110 ; StringF(str,'ecowx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0110110111 ; StringF(str,'sthux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %0110111100 ; StringF(str,'or\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %0111001001 ; StringF(str,'divdu\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %1111001001 ; StringF(str,'divduo\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0111001011 ; StringF(str,'divwu\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %1111001011 ; StringF(str,'divwuo\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0111010011 ; StringF(str,'mtspr \d,r\d',elevenFifteen,sixTen)
      CASE %0111010110 ; StringF(str,'dcbi r\d,r\d',elevenFifteen,sixteenTwenty)
      CASE %0111011100 ; StringF(str,'nand\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %0111101001 ; StringF(str,'divd\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %1111101001 ; StringF(str,'divdo\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0111101011 ; StringF(str,'divw\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %1111101011 ; StringF(str,'divwo\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0111110010 ; StringF(str,'slbia')
      CASE %1000000000 ; StringF(str,'mcrxr \d', shr(sixTen,2))
      CASE %1000010101 ; StringF(str,'lswx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1000010110 ; StringF(str,'lwbrx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1000010111 ; StringF(str,'lfsx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1000011000 ; StringF(str,'srw\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %1000011011 ; StringF(str,'srd\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %1000110110 ; StringF(str,'tlbsync')
      CASE %1000110111 ; StringF(str,'lfsux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1001010011 ; StringF(str,'mfsr r\d,\d',sixTen,elevenFifteen)
      CASE %1001010101 ; StringF(str,'lswi r\d,r\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1001010110 ; StringF(str,'sync')
      CASE %1001010111 ; StringF(str,'lfdx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1001110111 ; StringF(str,'lfdux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1010010011 ; StringF(str,'mfsrin r\d,r\d',sixTen,sixteenTwenty)
      CASE %1010010101 ; StringF(str,'stswx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1010010110 ; StringF(str,'stwbrx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1010010111 ; StringF(str,'stfsx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1010110111 ; StringF(str,'stfsux r\d,r\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1011010101 ; StringF(str,'stswi r\d,r\d,\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1011010111 ; StringF(str,'stfdx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1011110111 ; StringF(str,'stfdux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1100010110 ; StringF(str,'lhbrx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1100011000 ; StringF(str,'sraw\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %1100011010 ; StringF(str,'srad\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %1100111000 ; StringF(str,'srawi\s r\d,r\d,\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
      CASE %1101010110 ; StringF(str,'eieio')
      CASE %1110010110 ; StringF(str,'sthbrx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1110011010 ; StringF(str,'extsh\s r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen)
      CASE %1110111010 ; StringF(str,'extsb\s r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen)
      CASE %1111010110 ; StringF(str,'icbi r\d,r\d',elevenFifteen,sixteenTwenty)
      CASE %1111010111 ; StringF(str,'stfiwx f\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
      CASE %1111011010 ; StringF(str,'extsw\s r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
      CASE %1111110110 ; StringF(str,'dcbz r\d,r\d',elevenFifteen,sixteenTwenty)
      DEFAULT -> 8 bit sec. opcodes
         t := twentyoneThirty AND %11111111
         SELECT 256 OF t
         CASE %00000000 ; StringF(str,'cmp \d,r\d,r\d',shr(sixTen,2),elevenFifteen,sixteenTwenty)
         CASE %00000100 ; StringF(str,'tw \d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00001000 ; StringF(str,'subfc\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00001001 ; StringF(str,'mulhdu\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00001010 ; StringF(str,'addc\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00001011 ; StringF(str,'mulhwu\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00010011 ; StringF(str,'mfcr r\d',sixTen)
         CASE %00010100 ; StringF(str,'lwarx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00010101 ; StringF(str,'ldx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00010111 ; StringF(str,'lwzx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00011000 ; StringF(str,'slw\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
         CASE %00011010 ; StringF(str,'cntlzw\s r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen)
         CASE %00011011 ; StringF(str,'sld\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00011100 ; StringF(str,'and\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
         CASE %00100000 ; StringF(str,'cmpl \d,r\d,r\d',shr(sixTen,2),elevenFifteen,sixteenTwenty)
         CASE %00101000 ; StringF(str,'subf\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %00110101 ; StringF(str,'ldux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00110110 ; StringF(str,'dcbst r\d,r\d',elevenFifteen,sixteenTwenty)
         CASE %00110111 ; StringF(str,'lwzux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %00111010 ; StringF(str,'cntlzd\s r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
         CASE %00111100 ; StringF(str,'andc\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
         CASE %01000100 ; StringF(str,'td \d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %01001001 ; StringF(str,'mulhd\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %01001011 ; StringF(str,'mulhw\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %01010011 ; StringF(str,'mfmsr r\d',sixTen)
         CASE %01010100 ; StringF(str,'ldarx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %01010110 ; StringF(str,'dcbf r\d,r\d',elevenFifteen,sixteenTwenty)
         CASE %01010111 ; StringF(str,'lbzx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %01101000 ; StringF(str,'neg\s\s r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
         CASE %01110111 ; StringF(str,'lbzux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %01111100 ; StringF(str,'nor\s r\d,r\d,r\d',IF thirtyone THEN p ELSE e,elevenFifteen,sixTen,sixteenTwenty)
         CASE %10001000 ; StringF(str,'subfe\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %10001010 ; StringF(str,'adde\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e,IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %10010000 ; StringF(str,'mtcrf \d,r\d',shr(elevenTwenty,1),sixTen)
         CASE %10010010 ; StringF(str,'mtmsr r\d',sixTen)
         CASE %10010101 ; StringF(str,'stdx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %10010110 ; StringF(str,'stwcx. r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %10010111 ; StringF(str,'stwx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %10110101 ; StringF(str,'stdux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %10110111 ; StringF(str,'stwux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %11001000 ; StringF(str,'subfze\s\s r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
         CASE %11001010 ; StringF(str,'addze\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %11010010 ; StringF(str,'mtsr r\d,\d',elevenFifteen,sixTen)
         CASE %11010110 ; StringF(str,'stdcx. r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %11010111 ; StringF(str,'stbx r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         CASE %11101000 ; StringF(str,'subfme\s\s r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
         CASE %11101001 ; StringF(str,'mulld\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %11101010 ; StringF(str,'addme\s\s r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen)
         CASE %11101011 ; StringF(str,'mullw\s\s r\d,r\d,r\d',IF shr(twentyoneThirty,9) THEN o ELSE e, IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
         CASE %11110010 ; StringF(str,'mtsrin r\d,r\d',sixTen,sixteenTwenty)
         CASE %11110110 ; StringF(str,'dcbtst r\d,r\d',elevenFifteen,sixteenTwenty)
         CASE %11110111 ; StringF(str,'stbux r\d,r\d,r\d',sixTen,elevenFifteen,sixteenTwenty)
         DEFAULT ; StringF(str,'-?3?-')
         ENDSELECT
      ENDSELECT
   CASE %100000 ; StringF(str,'lwz r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100001 ; StringF(str,'lwzu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100010 ; StringF(str,'lbz r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100011 ; StringF(str,'lbzu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100100 ; StringF(str,'stw r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100101 ; StringF(str,'stwu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100110 ; StringF(str,'stb r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %100111 ; StringF(str,'stbu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101000 ; StringF(str,'lhz r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101001 ; StringF(str,'lhzu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101010 ; StringF(str,'lha r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101011 ; StringF(str,'lhau r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101100 ; StringF(str,'sth r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101101 ; StringF(str,'sthu r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101110 ; StringF(str,'lmw r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %101111 ; StringF(str,'stmw r\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110000 ; StringF(str,'lfs f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110001 ; StringF(str,'lfsu f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110010 ; StringF(str,'lfd f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110011 ; StringF(str,'lfdu f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110100 ; StringF(str,'stfs f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110101 ; StringF(str,'stfsu f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110110 ; StringF(str,'stfd f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %110111 ; StringF(str,'stfdu f\d,\d(r\d)',sixTen,sixteenThirtyone,elevenFifteen)
   CASE %111000
      IF (thirty OR thirtyone) = NIL
         StringF(str,'ld \d,\d,\d',sixTen,Mul(sixteenTwentynine,4),elevenFifteen)
      ELSEIF (thirty=0) AND (thirtyone=1)
         StringF(str,'ldu \d,\d,\d',sixTen,Mul(sixteenTwentynine,4),elevenFifteen)
      ELSEIF (thirty=1) AND (thirtyone=0)
         StringF(str,'lwa \d,\d,\d',sixTen,Mul(sixteenTwentynine,4),elevenFifteen)
      ENDIF
   CASE %111011
      SELECT 32 OF twentysixThirty
      CASE %10010 ; StringF(str,'fdivs\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %10100 ; StringF(str,'fsubs\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %10101 ; StringF(str,'fadds\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %10110 ; StringF(str,'fsqrts\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %11000 ; StringF(str,'fres\s f\d,r\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %11001 ; StringF(str,'fmuls\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive)
      CASE %11100 ; StringF(str,'fmsubs\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive,sixteenTwenty)
      CASE %11101 ; StringF(str,'fmadds\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive,sixteenTwenty)
      CASE %11110 ; StringF(str,'fnmsubs\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive,sixteenTwenty)
      CASE %11111 ; StringF(str,'fnmadds\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive,sixteenTwenty)
      DEFAULT ; StringF(str,'-?4?-')
      ENDSELECT
   CASE %111110
      IF (thirty OR thirtyone) = NIL
         StringF(str,'std \d,\d(\d)',sixTen,Mul(sixteenTwentynine,4),elevenFifteen)
      ELSEIF (thirty=0) AND (thirtyone=1)
         StringF(str,'stdu \d,\d(\d)',sixTen,Mul(sixteenTwentynine,4),elevenFifteen)
      ENDIF
   CASE %111111
      SELECT twentyoneThirty
      CASE %0000000000 ; StringF(str,'fcmpu \d,f\d,f\d',shr(sixTen,2),elevenFifteen,sixteenTwenty)
      CASE %0000001100 ; StringF(str,'frsp\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %0000001110 ; StringF(str,'fctiw f\d,f\d',sixTen,sixteenTwenty)
      CASE %0000001111 ; StringF(str,'fctiwz\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %0000010010 ; StringF(str,'fdiv\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0000010100 ; StringF(str,'fsub\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0000010101 ; StringF(str,'fadd\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty)
      CASE %0000011010 ; StringF(str,'frsqrte\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %0000100000 ; StringF(str,'fcmpo \d,f\d,f\d',shr(sixTen,2),elevenFifteen,sixteenTwenty)
      CASE %0000100110 ; StringF(str,'mtfsb1\s \d',IF thirtyone THEN p ELSE e,sixTen)
      CASE %0000101000 ; StringF(str,'fneg\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %0001000000 ; StringF(str,'mcrfs \d,\d',Shr(sixTen,2),Shr(elevenFifteen,2))
      CASE %0001000110 ; StringF(str,'mtfsb0\s \d',IF thirtyone THEN p ELSE e,sixTen)
      CASE %0001001000 ; StringF(str,'fmr\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %0010000110 ; StringF(str,'mtfsfi\s \d,\d',IF thirtyone THEN p ELSE e,Shr(sixTen,2),Shr(sixteenTwenty,1))
      CASE %0010001000 ; StringF(str,'fnabs\s f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,sixteenTwenty)
      CASE %1001000111 ; StringF(str,'mffs\s r\d',IF thirtyone THEN p ELSE e,sixTen)
      CASE %1011000111 ; StringF(str,'mtfsf\s \d,r\d',IF thirtyone THEN p ELSE e,Shr(sixFifteen,1),sixteenTwenty)
      CASE %0100001000 ; StringF(str,'fabs\s f\d,f\d',IF thirtyone THEN p ELSE e,Shr(sixFifteen,5),sixteenTwenty)

      DEFAULT ; StringF(str,'-?5?-')
         SELECT twentysixThirty
         CASE %10111 ; StringF(str,'fsel\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         CASE %11001 ; StringF(str,'fmul\s f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,twentyoneTwentyfive)
         CASE %11100 ; StringF(str,'fmsub\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         CASE %11101 ; StringF(str,'fmadd\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         CASE %11110 ; StringF(str,'fnmsub\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         CASE %11111 ; StringF(str,'fnmadd\s f\d,f\d,f\d,f\d',IF thirtyone THEN p ELSE e,sixTen,elevenFifteen,sixteenTwenty,twentyoneTwentyfive)
         DEFAULT ; StringF(str,'-?6?-')
         ENDSELECT
      ENDSELECT
   DEFAULT ; StringF(str,'-?7?-')

   ENDSELECT


ENDPROC str
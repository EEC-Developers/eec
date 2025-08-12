-> EEC/m68kopt.e

/* EEC by Samuel D. Crow [samurailuemas yahoo com] is Copyright (c) 2019-2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See CompilerLicense.md */

OPT MODULE, PREPROCESS

MODULE  '*m68kgen',
        '*optpasses',
        '*codegen',
        '*opcodes68',
        'Queue/queue',
        'List/singleList',
        'Filter/filterBase',
        'Buffer/bufferBase'

-> TODO mark all processor status flags in order 0-max of bits
EXPORT SET CARRY, EXT_CARRY, NEGATIVE, EQUAL, OVERFLOW

-> TODO mark all addressing 68020 modes according to the encoding
EXPORT ENUM addressModes
  INDAXDXOFS

-> PATTERN MATCHING FLAGS
-> fusion: treat 2-instruction sequences as atomic if an Apollo core can fuse them
-> pipeline: arrange instructions with outputs to inputs staggered if possible
-> maxaddress: recognize sequences of opcodes that correspond to advanced addressing modes
EXPORT SET PIPELINE, FUSION, MAXADDRESS

-> ADDRESSING MODE LOWERING LEVEL
-> address000: lower addressing modes to their 68000 equivalents
-> address020: lower addressing modes to the ones that perform well on an 020
-> address040: supports all addressing modes by default (specify to reset addressing)
EXPORT ENUM ADDRESS000, ADDRESS020, ADDRESS040

-> GENERATION COMPATIBILITY LEVEL
-> M68000: lower an opcode to its 68000 equivalent even if it takes more than one
-> M68020: this compatibility level is the default and is only specified for resetting to it
-> M68060: lacks 32-bit multiply that 68020+ models had previously had
-> M68080: add instruction set enhancements not supported on Motorola 680x0 variants
EXPORT ENUM M68000, M68020, M68060, M68080

-> Option Presets
-> pattern match filters are unnecessary when psuedo ops are directly generated
/*EXPORT ENUM NORMAL = PIPELINE, FUSION, ADDRESS020
  DEFAULT020 = ADDRESS020, PIPELINE
  DEFAULT000 = ADDRESS000, M68000
  APOLLO = PIPELINE, FUSION, M68080
  APOLLOJIT = PIPELINE, M68080 
*/
OBJECT optM68k of m68amiga
  flags,
  addressing,
  generation,
  -> filter stages for optimization
  filt:PTR TO filter
ENDOBJECT

-> constructor
PROC initOptM68k(flags, addressing, generation) OF optM68k
  SUPER self.init()
  -> avoid illegal combinations of optimizations

  self.flags := flags
  NEW filt.init()
ENDPROC

PROC generate(opcode, addressing) OF optM68k IS EMPTY

PROC branch(condition, destination) OF optM68k IS EMPTY

PROC addPass(pass) OF optM68k IS filt.enqueue(pass)

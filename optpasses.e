-> EEC/optpasses.e

/* EEC by Samuel D. Crow [samurailuemas yahoo com] is Copyright (c) 2019-2025 */
/* ECX by Leif Salomonsson [ecx tele2 se] is Copyright (c) 2002-2008 */
/* Released under the ECX COMPILER LICENSE, See CompilerLicense.md */

OPT MODULE, PREPROCESS

MODULE 'Filter/filter'
MODULE 'Buffer/bufferBase'

OBJECT pipelineOpt OF filter_process
  out:PTR TO bufferBase
ENDOBJECT

-> constructor
PROC initPipelineOpt() OF pipelineOpt
  SUPER self.add(parent)
ENDPROC

-> expand low-level addressing mode instructions from higher-level ones
OBJECT lowerAddressing OF filter_process
  level
ENDOBJECT

-> constructor
PROC initLowerAddressing(level) OF lowerAddressing
  SUPER self.add(parent)
ENDPROC

-> peephole opcode substitutions for different generations of CPUs
-> may imply the use of lowerAddressing pass
OBJECT lowerInstruction OF filter_process
  SUPER self.add(parent)
ENDOBJECT

-> make psuedo-ops of atomic operations for Apollo core's opcde fusion
OBJECT fuse OF filter_process
ENDOBJECT

-> constructor
PROC InitFuse(parent:PTR TO filter) OF fuse
  SUPER self.add(parent)
ENDPROC

-> increase generational level of addressing modes for later expansion
-> should speed up generation-independent optimizations
OBJECT maxAddress OF filter_process
ENDOBJECT

PROC initMaxAddress(parent:PTR TO filter)
  SUPER self.add(parent)
ENDPROC

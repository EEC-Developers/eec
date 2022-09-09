# Enhanced E Compiler

EEC is an advanced compiler for the AmigaE programming language. It's based on ECX by Leif Salmonsson with the intention of adding optimization and new backends. 

## Build Instructions

1. Configure FS-UAE or WinUAE to have at least a full 68020 or better and 128 MiB of Z3 Fast RAM on AmigaOS 3.1 up to AmigaOS 3.9.
1. Install VAsmm68k_mot for the 68k startup code.
1. Install VAsmPPC_std for the MorphOS and AmigaOS4 PowerPC startup code.
1. Set the protection bits on the script files as follows:  spat protect #?.script srwd
1. Execute the following AmigaDOS scripts to generate the startup codes:  make68kifuncs.script, makeppcifuncs.script and makeppcifuncs_os4.script
1. Also install either [TestEC](http://blubbedev.net/ecx/download/testec.lha) or [E-VO](http://aminet.net/package/dev/e/evo) (Must be v3.5.1 or higher of E-VO)
1. If using testec you will also need the modules from [amigae33a.lha](http://blubbedev.net/ecx/download/amigae33a.lha) following the direction in docs/E.guide using AmigaGuide or MultiView. If you use E-VO this already has all of the modules needed.
1. Finally, install the latest version of [ECX](http://blubbedev.net/ecx/download/ecx-2.3.1.lha)
1. Execute the following in the ECX/bin directory: Copy ECX.68k EEC.020 clone
1. Execute makedir ecxmodules:eec to create the directory for eec modules
1. Execute makestartups.script to create the modules.
1. Bootstrap the executable by typing one of these:

      makeeec.script ec (if using TESTEC)

      makeeec.script evo (if using E-VO)

1. You now have a compiled 020 version of EEC which can be used to build the remaining executables by typing:

      makeeec.script morphos

      makeeec.script amigaos4

      makeeec.script amigaos


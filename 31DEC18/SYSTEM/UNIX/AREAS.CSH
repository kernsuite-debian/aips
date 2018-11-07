# "--------------------------------------------------------------------"
# " Usage: source AREAS.CSH                                            "
# "--------------------------------------------------------------------"
# ";  Copyright (C) 1995-2000, 2003, 2006, 2009                        "
# ";  Associated Universities, Inc. Washington DC, USA.                "
# ";                                                                   "
# ";  This program is free software; you can redistribute it and/or    "
# ";  modify it under the terms of the GNU General Public License as   "
# ";  published by the Free Software Foundation; either version 2 of   "
# ";  the License, or (at your option) any later version.              "
# ";                                                                   "
# ";  This program is distributed in the hope that it will be useful,  "
# ";  but WITHOUT ANY WARRANTY; without even the implied warranty of   "
# ";  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    "
# ";  GNU General Public License for more details.                     "
# ";                                                                   "
# ";  You should have received a copy of the GNU General Public        "
# ";  License along with this program; if not, write to the Free       "
# ";  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     "
# ";  MA 02139, USA.                                                   "
# ";                                                                   "
# ";  Correspondence concerning AIPS should be addressed as follows:   "
# ";         Internet email: aipsmail@nrao.edu.                        "
# ";         Postal address: AIPS Project Office                       "
# ";                         National Radio Astronomy Observatory      "
# ";                         520 Edgemont Road                         "
# ";                         Charlottesville, VA 22903-2475 USA        "
# "--------------------------------------------------------------------"
# " This file defines the entire AIPS directory structure (relative to "
# " a 'version' directory node) plus the 'logical' associated with     "
# " each 'area' (i.e., directory).                                     "
# "                                                                    "
# " UNIX: The script $SYSUNIX/AREAS uses $SYSAIPS/AREAS.DAT to create  "
# " $SYSLOCAL/AREAS.CSH and $SYSLOCAL/AREAS.SH, which contain C shell  "
# " and Bourne/Korn/bash/zsh shell syntax, respectively, to define the "
# " area 'logicals' in terms of environment variables.  The definitions"
# " are toggled between different versions of AIPS by executing $CDOLD,"
# " $CDNEW or $CDTST, which are defined in $AIPS_ROOT/LOGIN.CSH and/or "
# " $AIPS_ROOT/LOGIN.SH.  The AREAS.* environment variables are NOT    "
# " required for the execution of AIPS programs, just for AIPS         "
# " programming.  In fact, having all the area logicals defined takes  "
# " up such a large fraction of the available environment space on     "
# " some systems that the execution of AIPS programs, UNIX commands,   "
# " AIPS programming tools and system debuggers can be impaired.  To   "
# " avoid this, it may be necessary to comment out some or all of the  "
# " definitions that don't apply to the host implementation.           "
# "                                                                    "
# " VMS: AIPS_PROC:ASSNLOCAL.COM uses this file more or less directly  "
# " to establish the definitions of area logicals.  The definitions    "
# " are toggled between different versions of AIPS by executing the    "
# " AIPS defined 'VERSION' command (see AIPS_PROC:AIPSUSER.COM), e.g., "
# " 'VERSION NEW'.                                                     "
# "--------------------------------------------------------------------"
# " Top level directories of source code areas                         "
# "--------------------------------------------------------------------"
#
setenv AIP             $AIPS_VERSION/AIPS
setenv APL             $AIPS_VERSION/APL
setenv Q               $AIPS_VERSION/Q
setenv QY              $AIPS_VERSION/QY
setenv Y               $AIPS_VERSION/Y
#
# "--------------------------------------------------------------------"
# " AIPS program areas                                                 "
# "--------------------------------------------------------------------"
#
#                                       "Standard programs"
setenv AIPPGM          $AIPS_VERSION/AIPS/PGM
#                                       "Non-standard programs"
setenv AIPNOT          $AIPS_VERSION/AIPS/PGM/NOTST
#                                       "UNIX non-standard programs"
setenv AIPGUNIX        $AIPS_VERSION/AIPS/PGM/NOTST/UNIX
#                                       "VMS non-standard programs"
setenv AIPGVMS         $AIPS_VERSION/AIPS/PGM/NOTST/VMS
#
# "--------------------------------------------------------------------"
# " AIP subroutine areas - only referenced by AIP* programs            "
# "--------------------------------------------------------------------"
#
setenv AIPSUB          $AIPS_VERSION/AIPS/SUB
#
# "--------------------------------------------------------------------"
# " APL program areas - these reference only APL* routines             "
# "--------------------------------------------------------------------"
#
#                                       "Standard programs"
setenv APLPGM          $AIPS_VERSION/APL/PGM
#                                       "Non-standard programs"
setenv APGNOT          $AIPS_VERSION/APL/PGM/NOTST
#                                       "Object-oriented programs"
setenv APGOOP          $AIPS_VERSION/APL/PGM/OOP
#                                       "UNIX programs"
setenv APGUNIX         $AIPS_VERSION/APL/PGM/NOTST/UNIX
#                                       "VMS programs"
setenv APGVMS          $AIPS_VERSION/APL/PGM/NOTST/VMS
#                                       "Contributed tasks and HELP "
#                                       "files - not compiled or linked"
#                                       "by the standard procedures"
setenv APLCONTR        $AIPS_VERSION/APL/CONTRIBU
#
# "--------------------------------------------------------------------"
# " APL subroutine areas - nothing here references Q or Y-routines     "
# "--------------------------------------------------------------------"
#
#                                       "Standard routines"
setenv APLSUB          $AIPS_VERSION/APL/SUB
#                                       "Non-standard routines"
setenv APLNOT          $AIPS_VERSION/APL/SUB/NOTST
#                                       "Object-oriented routines"
setenv APLOOP          $AIPS_VERSION/APL/SUB/OOP
#                                       "VMS non-standard routines"
setenv APLNVMS         $AIPS_VERSION/APL/SUB/NOTST/VMS
#
# "--------------------------------------------------------------------"
# " Z-routine areas                                                    "
# "--------------------------------------------------------------------"
#
#                                       "Generic"
setenv APLGEN          $AIPS_VERSION/APL/DEV
#                                       "Generic UNIX"
setenv APLUNIX         $AIPS_VERSION/APL/DEV/UNIX
#                                       "Bell UNIX"
setenv APLBELL         $AIPS_VERSION/APL/DEV/UNIX/BELL
#                                       "SunOS 5 (Solaris 2) and later"
setenv APLSOL          $AIPS_VERSION/APL/DEV/UNIX/BELL/SOL
#                                       "SunOS 5 on Intel iA86"
setenv APLSOL86        $AIPS_VERSION/APL/DEV/UNIX/BELL/SOL/SOL86
#                                       "Hewlett Packard HP-UX"
setenv APLHP           $AIPS_VERSION/APL/DEV/UNIX/BELL/HP
#                                       "Linux (Intel port)"
setenv APLLINUX        $AIPS_VERSION/APL/DEV/UNIX/BELL/LINUX
#                                       "Linux (Intel port) 64bit"
setenv APLLNX64        $AIPS_VERSION/APL/DEV/UNIX/BELL/LNX64
#                                       "MacIntosh (Darwin OS/X)"
setenv APLMACPP        $AIPS_VERSION/APL/DEV/UNIX/BELL/MACPPC
#                                       "MacIntosh (Darwin OS/X) Intel"
setenv APLMACIN        $AIPS_VERSION/APL/DEV/UNIX/BELL/MACINT
#                                       "Linux (Alpha AXP port)"
setenv APLAXLIN        $AIPS_VERSION/APL/DEV/UNIX/BELL/AXLINUX
#                                       "Linux (Power PC port)"
setenv APLPPLIN        $AIPS_VERSION/APL/DEV/UNIX/BELL/LINUXPPC
#                                       "Silicon Graphics IRIX"
setenv APLSGI          $AIPS_VERSION/APL/DEV/UNIX/BELL/SGI
#                                       "Cray Research Inc"
setenv APLCRI          $AIPS_VERSION/APL/DEV/UNIX/BELL/CRI
#                                       "Masscomp"
setenv APLMASC         $AIPS_VERSION/APL/DEV/UNIX/BELL/MASC
#                                       "Berkeley UNIX"
setenv APLBERK         $AIPS_VERSION/APL/DEV/UNIX/BERK
#                                       "Alliant"
setenv APLALLN         $AIPS_VERSION/APL/DEV/UNIX/BERK/ALLN
#                                       "Convex"
setenv APLCVEX         $AIPS_VERSION/APL/DEV/UNIX/BERK/CVEX
#                                       "Dec Alpha (OSF/1)"
setenv APLDEC          $AIPS_VERSION/APL/DEV/UNIX/BERK/DEC
#                                       "Dec Ultrix"
setenv APLULTRX        $AIPS_VERSION/APL/DEV/UNIX/BERK/DEC/ULTRIX
#                                       "IBM RS/6000"
setenv APLIBM          $AIPS_VERSION/APL/DEV/UNIX/BERK/IBM
#                                       "IBM 3090 (AIX)"
setenv APL3090         $AIPS_VERSION/APL/DEV/UNIX/BERK/IBM/3090
#                                       "SunOS 4 (Solaris 1)"
setenv APLSUN          $AIPS_VERSION/APL/DEV/UNIX/BERK/SUN
#                                       "VAX"
setenv APLVAX          $AIPS_VERSION/APL/DEV/UNIX/BERK/VAX
#                                       "Generic VMS"
setenv APLVMS          $AIPS_VERSION/APL/DEV/VMS
#
# "--------------------------------------------------------------------"
# " Binary areas                                                       "
# "--------------------------------------------------------------------"
#
setenv AIPSBIN         $AIPS_VERSION/BIN
setenv AIPSFITS        $AIPS_VERSION/BIN/FITS
#
# "--------------------------------------------------------------------"
# " Documentation areas                                                "
# "--------------------------------------------------------------------"
#
setenv DOC             $AIPS_VERSION/DOC
setenv DOCTXT          $AIPS_VERSION/DOC/TEXT
#
# "--------------------------------------------------------------------"
# " Core dump area                                                     "
# "--------------------------------------------------------------------"
#
setenv ERRORS          $AIPS_VERSION/$ARCH/ERRORS/$HOST
#
# "--------------------------------------------------------------------"
# " HELP file area                                                     "
# "--------------------------------------------------------------------"
#
setenv HLPFIL          $AIPS_VERSION/HELP
#
# "--------------------------------------------------------------------"
# " History area                                                       "
# "--------------------------------------------------------------------"
#
setenv HIST            $AIPS_VERSION/HIST
#
# "--------------------------------------------------------------------"
# " INCLUDE file areas                                                 "
# "--------------------------------------------------------------------"
#
#                                       "Standard INCLUDEs"
setenv INC             $AIPS_VERSION/INC
#                                       "Non-standard INCLUDEs"
setenv INCNOT          $AIPS_VERSION/INC/NOTST
#                                       "GNU (readline)"
setenv INCGNU          $AIPS_VERSION/INC/NOTST/GNU
#                                       "Local"
setenv INCLOC          $AIPS_VERSION/INC/LOCAL
#                                       "Alliant"
setenv INCALN          $AIPS_VERSION/INC/NOTST/ALLN
#                                       "Alpha AXP Linux"
setenv INCAXL          $AIPS_VERSION/INC/NOTST/AXLINUX
#                                       "Power PC Linux"
setenv INCPPC          $AIPS_VERSION/INC/NOTST/LINUXPPC
#                                       "Cray Research Inc"
setenv INCCRI          $AIPS_VERSION/INC/NOTST/CRI
#                                       "Convex"
setenv INCVEX          $AIPS_VERSION/INC/NOTST/CVEX
#                                       "Digital (OSF/1, Ultrix)"
setenv INCDEC          $AIPS_VERSION/INC/NOTST/DEC
#                                       "HP-UX"
setenv INCHP           $AIPS_VERSION/INC/NOTST/HP
#                                       "HP-UX 2 (for PAPC, big AP)"
setenv INCHP2          $AIPS_VERSION/INC/NOTST/HP2
#                                       "IBM RS/6000"
setenv INCIBM          $AIPS_VERSION/INC/NOTST/IBM
#                                       "IBM 3090 (AIX)"
setenv INC3090         $AIPS_VERSION/INC/NOTST/IBM/3090
#                                       "Intel Linux"
setenv INCLIN          $AIPS_VERSION/INC/NOTST/LINUX
#                                       "Intel Linux 64bit"
setenv INCLNX64        $AIPS_VERSION/INC/NOTST/LNX64
#                                       "MacIntosh (Darwin OS/X)"
setenv INCMACPP        $AIPS_VERSION/INC/NOTST/MACPPC
#                                       "MacIntosh (Darwin OS/X) Intel"
setenv INCMACIN        $AIPS_VERSION/INC/NOTST/MACINT
#                                       "Silicon Graphics IRIX"
setenv INCSGI          $AIPS_VERSION/INC/NOTST/SGI
#                                       "SunOS 5 (Solaris 2)"
setenv INCSOL          $AIPS_VERSION/INC/NOTST/SOL
#                                       "SunOS 5 (Solaris 2) UltraII"
setenv INCSUL          $AIPS_VERSION/INC/NOTST/SUL
#                                       "SunOS 4 (Solaris 1)"
setenv INCSUN          $AIPS_VERSION/INC/NOTST/SUN
#                                       "VMS"
setenv INCVMS          $AIPS_VERSION/INC/NOTST/VMS
#
# "--------------------------------------------------------------------"
# " Object module areas                                                "
# "--------------------------------------------------------------------"
#
#                                       "Subroutine object libraries"
setenv LIBR            $AIPS_VERSION/$ARCH/LIBR
#                                       "Debug version of libraries"
setenv LIBRDBG         $AIPS_VERSION/$ARCH/LIBRDBG
#                                       "Executable modules"
setenv LOAD            $AIPS_VERSION/$ARCH/LOAD
#                                       "Alternate executable modules"
#                                       "commented out; rarely used"
#                                       "LOAD1 is Pseudo AP w/wo TV 1"
#                                       " and is LOAD.ALT1"
#                                       "LOAD2 is for TV 2 w/wo real AP"
#                                       " and is LOAD.ALT2"
#                                       "LOAD3 is for TV 2 w Pseudo AP"
#                                       " and is LOAD.ALT3"
#
# "--------------------------------------------------------------------"
# "PREP area for temporary files (unix only)                           "
# "--------------------------------------------------------------------"
#
setenv PREP            $AIPS_VERSION/$ARCH/PREP
#
# "--------------------------------------------------------------------"
# " POPS memory file area                                              "
# "--------------------------------------------------------------------"
#
setenv MEMORY          $AIPS_VERSION/$ARCH/MEMORY
#
# "--------------------------------------------------------------------"
# " Q-routine areas (real and pseudo array processor)                  "
# "--------------------------------------------------------------------"
#
#                                       "Generic"
setenv QDEV            $AIPS_VERSION/Q/DEV
#                                       "Generic FPS"
setenv QFPS            $AIPS_VERSION/Q/DEV/FPS
#                                       "16 bit FPS"
setenv QFPS16          $AIPS_VERSION/Q/DEV/FPS/16B
#                                       "Model 120B FPS"
setenv Q120B           $AIPS_VERSION/Q/DEV/FPS/16B/120B
#                                       "Models 5105, 5205 ... FPS"
setenv Q5000           $AIPS_VERSION/Q/DEV/FPS/16B/5000
#                                       "32 bit FPS"
setenv QFPS32          $AIPS_VERSION/Q/DEV/FPS/32B
#                                       "Model 190 FPS"
setenv Q190            $AIPS_VERSION/Q/DEV/FPS/32B/190
#                                       "Generic pseudo AP"
setenv QPSAP           $AIPS_VERSION/Q/DEV/PSAP
#                                       "Alliant pseudo AP"
setenv QALN            $AIPS_VERSION/Q/DEV/PSAP/ALLN
#                                       "Cray Research Inc pseudo AP"
setenv QCRI            $AIPS_VERSION/Q/DEV/PSAP/CRI
#                                       "Convex pseudo AP"
setenv QVEX            $AIPS_VERSION/Q/DEV/PSAP/CVEX
#                                       "Digital: OSF/1, Ultrix"
setenv QDEC            $AIPS_VERSION/Q/DEV/PSAP/DEC
#                                       "HP-UX"
setenv QHP             $AIPS_VERSION/Q/DEV/PSAP/HP
#                                       "IBM RS/6000"
setenv QIBM            $AIPS_VERSION/Q/DEV/PSAP/IBM
#                                       "IBM 3090 (AIX)"
setenv Q3090           $AIPS_VERSION/Q/DEV/PSAP/IBM/3090
#                                       "Silicon Graphics IRIX PS-AP"
setenv QSGI            $AIPS_VERSION/Q/DEV/PSAP/SGI
#                                       "SunOS 5 (Solaris 2) pseudo AP"
setenv QSOL            $AIPS_VERSION/Q/DEV/PSAP/SOL
#                                       "SunOS 4 (Solaris 1) pseudo AP"
setenv QSUN            $AIPS_VERSION/Q/DEV/PSAP/SUN
#                                       "VMS pseudo AP"
setenv QVMS            $AIPS_VERSION/Q/DEV/PSAP/VMS
#
# "--------------------------------------------------------------------"
# " Programs that reference Q-routines                                 "
# "--------------------------------------------------------------------"
#
#                                       "Standard programs"
setenv QPGM            $AIPS_VERSION/Q/PGM
#                                       "Q-Object Oriented programs"
setenv QPGOOP          $AIPS_VERSION/Q/PGM/OOP
#                                       "Non-standard programs"
setenv QPGNOT          $AIPS_VERSION/Q/PGM/NOTST
#                                       "VMS programs"
setenv QPGVMS          $AIPS_VERSION/Q/PGM/NOTST/VMS
#
# "--------------------------------------------------------------------"
# " Subroutines that reference Q-routines                              "
# "--------------------------------------------------------------------"
#
#                                       "Standard routines"
setenv QSUB            $AIPS_VERSION/Q/SUB
#                                       "Object-oriented Q-routines"
setenv QOOP            $AIPS_VERSION/Q/SUB/OOP
#                                       "Non-standard routines"
setenv QNOT            $AIPS_VERSION/Q/SUB/NOTST
#
# "--------------------------------------------------------------------"
# " Programs that reference both Q-routines and Y-routines             "
# "--------------------------------------------------------------------"
#
#                                       "Standard programs"
setenv QYPGM           $AIPS_VERSION/QY/PGM
#                                       "Non-standard programs"
setenv QYPGNOT         $AIPS_VERSION/QY/PGM/NOTST
#                                       "VMS programs"
setenv QYPGVMS         $AIPS_VERSION/QY/PGM/NOTST/VMS
#
# "--------------------------------------------------------------------"
# " System RUN file area - useful procedures for everyone              "
# "--------------------------------------------------------------------"
#
setenv RUNSYS          $AIPS_VERSION/RUN
#
# "--------------------------------------------------------------------"
# " System manager areas                                               "
# "--------------------------------------------------------------------"
#
#                                       "Generic"
setenv SYSAIPS         $AIPS_VERSION/SYSTEM
#                                       "Generic UNIX"
setenv SYSUNIX         $AIPS_VERSION/SYSTEM/UNIX
#                                       "Generic PERL"
setenv SYSPERL         $AIPS_VERSION/SYSTEM/UNIX/PERL
#                                       "(Local) manual pages"
setenv SYSMAN          $AIPS_VERSION/SYSTEM/UNIX/manl
#                                       "Alliant"
setenv SYSALLN         $AIPS_VERSION/ALLN/SYSTEM
#                                       "Cray Research Inc"
setenv SYSCRI          $AIPS_VERSION/CRI/SYSTEM
#                                       "Convex"
setenv SYSCVEX         $AIPS_VERSION/CVEX/SYSTEM
#                                       "Decstation (Ultrix)"
setenv SYSDEC          $AIPS_VERSION/DEC/SYSTEM
#                                       "Digital Alpha (OSF/1)"
setenv SYSALPHA        $AIPS_VERSION/ALPHA/SYSTEM
#                                       "HP-UX"
setenv SYSHP           $AIPS_VERSION/HP/SYSTEM
#                                       "HP-UX"
setenv SYSHP2          $AIPS_VERSION/HP2/SYSTEM
#                                       "IBM RS/6000"
setenv SYSIBM          $AIPS_VERSION/IBM/SYSTEM
#                                       "IBM 3090 (AIX)"
setenv SYS3090         $AIPS_VERSION/IBM3090/SYSTEM
#                                       "Silicon Graphics IRIX"
setenv SYSSGI          $AIPS_VERSION/SGI/SYSTEM
#                                       "Linux/PC"
setenv SYSLINUX        $AIPS_VERSION/LINUX/SYSTEM
#                                       "Linux/PC 64bit"
setenv SYSLNX64        $AIPS_VERSION/LNX64/SYSTEM
#                                       "MacIntosh (Darwin OS/X)"
setenv SYSMACPP        $AIPS_VERSION/MACPPC/SYSTEM
#                                       "MacIntosh (Darwin OS/X) Intel"
setenv SYSMACIN        $AIPS_VERSION/MACINT/SYSTEM
#                                       "Linux/Alpha-AXP"
setenv SYSAXLIN        $AIPS_VERSION/AXLINUX/SYSTEM
#                                       "Linux/Power-PC"
setenv SYSPPLIN        $AIPS_VERSION/LINUXPPC/SYSTEM
#                                       "SunOS 5 (Solaris 2)"
setenv SYSSOL          $AIPS_VERSION/SOL/SYSTEM
#                                       "SunOS 5 (Solaris 2) UltraII"
setenv SYSSUL          $AIPS_VERSION/SUL/SYSTEM
#                                       "SunOS 5 (Solaris 2) iA86"
setenv SYSSOL86        $AIPS_VERSION/SOL86/SYSTEM
#                                       "SunOS 4 (Solaris 1)"
setenv SYSSUN          $AIPS_VERSION/SUN4/SYSTEM
#                                       "Old 68020/68030 Suns"
setenv SYSSUN3         $AIPS_VERSION/SUN3/SYSTEM
#                                       "UNIX installation"
setenv INSUNIX         $AIPS_VERSION/SYSTEM/UNIX/INSTALL
#                                       "UNIX midnight job"
setenv UPDUNIX         $AIPS_VERSION/SYSTEM/UNIX/UPDATE
#                                       "Generic VMS"
setenv SYSVMS          $AIPS_VERSION/SYSTEM/VMS
#                                       "Local VMS"
setenv SYSLVAX         $AIPS_VERSION/SYSTEM/VMS/LOCAL
#                                       "VMS installation"
setenv INSVMS          $AIPS_VERSION/SYSTEM/VMS/INSTALL
#                                       "VMS midnight job"
setenv UPDVMS          $AIPS_VERSION/SYSTEM/VMS/UPDATE
#
# "--------------------------------------------------------------------"
# " Y-routine areas                                                    "
# "--------------------------------------------------------------------"
#
#                                       "Generic"
setenv YGEN            $AIPS_VERSION/Y/DEV
#                                       "ARGS pipe from M70"
setenv YARGS           $AIPS_VERSION/Y/DEV/ARGS
#                                       "Deanza"
setenv YDEA            $AIPS_VERSION/Y/DEV/DEA
#                                       "IIS generic"
setenv YIIS            $AIPS_VERSION/Y/DEV/IIS
#                                       "IIS Model IVAS"
setenv YIVAS           $AIPS_VERSION/Y/DEV/IVAS
#                                       "IIS Model 70"
setenv YM70            $AIPS_VERSION/Y/DEV/IIS/M70
#                                       "IIS Model 75"
setenv YM75            $AIPS_VERSION/Y/DEV/IIS/M75
#                                       "Lexidata"
setenv YLEX            $AIPS_VERSION/Y/DEV/LEX
#                                       "Lexidata C code"
setenv YLEXC           $AIPS_VERSION/Y/DEV/LEX/LEXC
#                                       "Screen Servers SSS, XAS, XVSS"
setenv YSS             $AIPS_VERSION/Y/DEV/SS
#                                       "Stubbed"
setenv YSTUB           $AIPS_VERSION/Y/DEV/STUB
#                                       "Comtal Vision 1/20"
setenv YV20            $AIPS_VERSION/Y/DEV/V20
#                                       "Virtual TV"
setenv YVTV            $AIPS_VERSION/Y/DEV/VTV
#
# "--------------------------------------------------------------------"
# " Programs that reference Y-routines                                 "
# "--------------------------------------------------------------------"
#
#                                       "Standard programs"
setenv YPGM            $AIPS_VERSION/Y/PGM
#                                       "Virtual TV program"
setenv YPGVDEV         $AIPS_VERSION/Y/PGM/VDEV
#                                       "Non-standard programs"
setenv YPGNOT          $AIPS_VERSION/Y/PGM/NOTST
#                                       "VMS programs"
setenv YPGVMS          $AIPS_VERSION/Y/PGM/NOTST/VMS
#
#
# "--------------------------------------------------------------------"
# " Screen server programs                                             "
# "--------------------------------------------------------------------"
#
#                                       "Servers"
setenv YSERV           $AIPS_VERSION/Y/SERVERS
#
# "--------------------------------------------------------------------"
# " Subroutines that reference Y-routines                              "
# "--------------------------------------------------------------------"
#
#                                       "Standard routines"
setenv YSUB            $AIPS_VERSION/Y/SUB
#                                       "Non-standard routines"
setenv YNOT            $AIPS_VERSION/Y/SUB/NOTST

# --------------------------------------------------------------------
# Usage: . AREAS.SH
# --------------------------------------------------------------------
# ;  Copyright (C) 1995-2000, 2003, 2006, 2009
# ;  Associated Universities, Inc. Washington DC, USA.
# ;
# ;  This program is free software; you can redistribute it and/or
# ;  modify it under the terms of the GNU General Public License as
# ;  published by the Free Software Foundation; either version 2 of
# ;  the License, or (at your option) any later version.
# ;
# ;  This program is distributed in the hope that it will be useful,
# ;  but WITHOUT ANY WARRANTY; without even the implied warranty of
# ;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# ;  GNU General Public License for more details.
# ;
# ;  You should have received a copy of the GNU General Public
# ;  License along with this program; if not, write to the Free
# ;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
# ;  MA 02139, USA.
# ;
# ;  Correspondence concerning AIPS should be addressed as follows:
# ;         Internet email: aipsmail@nrao.edu.
# ;         Postal address: AIPS Project Office
# ;                         National Radio Astronomy Observatory
# ;                         520 Edgemont Road
# ;                         Charlottesville, VA 22903-2475 USA
# --------------------------------------------------------------------
#  This file defines the entire AIPS directory structure (relative to
#  a 'version' directory node) plus the 'logical' associated with
#  each 'area' (i.e., directory).
#
#  UNIX: The script $SYSUNIX/AREAS uses $SYSAIPS/AREAS.DAT to create
#  $SYSLOCAL/AREAS.CSH and $SYSLOCAL/AREAS.SH, which contain C shell
#  and Bourne/Korn/bash/zsh shell syntax, respectively, to define the
#  area 'logicals' in terms of environment variables.  The definitions
#  are toggled between different versions of AIPS by executing $CDOLD,
#  $CDNEW or $CDTST, which are defined in $AIPS_ROOT/LOGIN.CSH and/or
#  $AIPS_ROOT/LOGIN.SH.  The AREAS.* environment variables are NOT
#  required for the execution of AIPS programs, just for AIPS
#  programming.  In fact, having all the area logicals defined takes
#  up such a large fraction of the available environment space on
#  some systems that the execution of AIPS programs, UNIX commands,
#  AIPS programming tools and system debuggers can be impaired.  To
#  avoid this, it may be necessary to comment out some or all of the
#  definitions that don't apply to the host implementation.
#
#  VMS: AIPS_PROC:ASSNLOCAL.COM uses this file more or less directly
#  to establish the definitions of area logicals.  The definitions
#  are toggled between different versions of AIPS by executing the
#  AIPS defined 'VERSION' command (see AIPS_PROC:AIPSUSER.COM), e.g.,
#  'VERSION NEW'.
# --------------------------------------------------------------------
#  Top level directories of source code areas
# --------------------------------------------------------------------
#
AIP=$AIPS_VERSION/AIPS
export AIP
APL=$AIPS_VERSION/APL
export APL
Q=$AIPS_VERSION/Q
export Q
QY=$AIPS_VERSION/QY
export QY
Y=$AIPS_VERSION/Y
export Y
#
# --------------------------------------------------------------------
#  AIPS program areas
# --------------------------------------------------------------------
#
#                                       Standard programs
AIPPGM=$AIPS_VERSION/AIPS/PGM
export AIPPGM
#                                       Non-standard programs
AIPNOT=$AIPS_VERSION/AIPS/PGM/NOTST
export AIPNOT
#                                       UNIX non-standard programs
AIPGUNIX=$AIPS_VERSION/AIPS/PGM/NOTST/UNIX
export AIPGUNIX
#                                       VMS non-standard programs
AIPGVMS=$AIPS_VERSION/AIPS/PGM/NOTST/VMS
export AIPGVMS
#
# --------------------------------------------------------------------
#  AIP subroutine areas - only referenced by AIP* programs
# --------------------------------------------------------------------
#
AIPSUB=$AIPS_VERSION/AIPS/SUB
export AIPSUB
#
# --------------------------------------------------------------------
#  APL program areas - these reference only APL* routines
# --------------------------------------------------------------------
#
#                                       Standard programs
APLPGM=$AIPS_VERSION/APL/PGM
export APLPGM
#                                       Non-standard programs
APGNOT=$AIPS_VERSION/APL/PGM/NOTST
export APGNOT
#                                       Object-oriented programs
APGOOP=$AIPS_VERSION/APL/PGM/OOP
export APGOOP
#                                       UNIX programs
APGUNIX=$AIPS_VERSION/APL/PGM/NOTST/UNIX
export APGUNIX
#                                       VMS programs
APGVMS=$AIPS_VERSION/APL/PGM/NOTST/VMS
export APGVMS
#                                       Contributed tasks and HELP
#                                       files - not compiled or linked
#                                       by the standard procedures
APLCONTR=$AIPS_VERSION/APL/CONTRIBU
export APLCONTR
#
# --------------------------------------------------------------------
#  APL subroutine areas - nothing here references Q or Y-routines
# --------------------------------------------------------------------
#
#                                       Standard routines
APLSUB=$AIPS_VERSION/APL/SUB
export APLSUB
#                                       Non-standard routines
APLNOT=$AIPS_VERSION/APL/SUB/NOTST
export APLNOT
#                                       Object-oriented routines
APLOOP=$AIPS_VERSION/APL/SUB/OOP
export APLOOP
#                                       VMS non-standard routines
APLNVMS=$AIPS_VERSION/APL/SUB/NOTST/VMS
export APLNVMS
#
# --------------------------------------------------------------------
#  Z-routine areas
# --------------------------------------------------------------------
#
#                                       Generic
APLGEN=$AIPS_VERSION/APL/DEV
export APLGEN
#                                       Generic UNIX
APLUNIX=$AIPS_VERSION/APL/DEV/UNIX
export APLUNIX
#                                       Bell UNIX
APLBELL=$AIPS_VERSION/APL/DEV/UNIX/BELL
export APLBELL
#                                       SunOS 5 (Solaris 2) and later
APLSOL=$AIPS_VERSION/APL/DEV/UNIX/BELL/SOL
export APLSOL
#                                       SunOS 5 on Intel iA86
APLSOL86=$AIPS_VERSION/APL/DEV/UNIX/BELL/SOL/SOL86
export APLSOL86
#                                       Hewlett Packard HP-UX
APLHP=$AIPS_VERSION/APL/DEV/UNIX/BELL/HP
export APLHP
#                                       Linux (Intel port)
APLLINUX=$AIPS_VERSION/APL/DEV/UNIX/BELL/LINUX
export APLLINUX
#                                       Linux (Intel port) 64bit
APLLNX64=$AIPS_VERSION/APL/DEV/UNIX/BELL/LNX64
export APLLNX64
#                                       MacIntosh (Darwin OS/X)
APLMACPP=$AIPS_VERSION/APL/DEV/UNIX/BELL/MACPPC
export APLMACPP
#                                       MacIntosh (Darwin OS/X) Intel
APLMACIN=$AIPS_VERSION/APL/DEV/UNIX/BELL/MACINT
export APLMACIN
#                                       Linux (Alpha AXP port)
APLAXLIN=$AIPS_VERSION/APL/DEV/UNIX/BELL/AXLINUX
export APLAXLIN
#                                       Linux (Power PC port)
APLPPLIN=$AIPS_VERSION/APL/DEV/UNIX/BELL/LINUXPPC
export APLPPLIN
#                                       Silicon Graphics IRIX
APLSGI=$AIPS_VERSION/APL/DEV/UNIX/BELL/SGI
export APLSGI
#                                       Cray Research Inc
APLCRI=$AIPS_VERSION/APL/DEV/UNIX/BELL/CRI
export APLCRI
#                                       Masscomp
APLMASC=$AIPS_VERSION/APL/DEV/UNIX/BELL/MASC
export APLMASC
#                                       Berkeley UNIX
APLBERK=$AIPS_VERSION/APL/DEV/UNIX/BERK
export APLBERK
#                                       Alliant
APLALLN=$AIPS_VERSION/APL/DEV/UNIX/BERK/ALLN
export APLALLN
#                                       Convex
APLCVEX=$AIPS_VERSION/APL/DEV/UNIX/BERK/CVEX
export APLCVEX
#                                       Dec Alpha (OSF/1)
APLDEC=$AIPS_VERSION/APL/DEV/UNIX/BERK/DEC
export APLDEC
#                                       Dec Ultrix
APLULTRX=$AIPS_VERSION/APL/DEV/UNIX/BERK/DEC/ULTRIX
export APLULTRX
#                                       IBM RS/6000
APLIBM=$AIPS_VERSION/APL/DEV/UNIX/BERK/IBM
export APLIBM
#                                       IBM 3090 (AIX)
APL3090=$AIPS_VERSION/APL/DEV/UNIX/BERK/IBM/3090
export APL3090
#                                       SunOS 4 (Solaris 1)
APLSUN=$AIPS_VERSION/APL/DEV/UNIX/BERK/SUN
export APLSUN
#                                       VAX
APLVAX=$AIPS_VERSION/APL/DEV/UNIX/BERK/VAX
export APLVAX
#                                       Generic VMS
APLVMS=$AIPS_VERSION/APL/DEV/VMS
export APLVMS
#
# --------------------------------------------------------------------
#  Binary areas
# --------------------------------------------------------------------
#
AIPSBIN=$AIPS_VERSION/BIN
export AIPSBIN
AIPSFITS=$AIPS_VERSION/BIN/FITS
export AIPSFITS
#
# --------------------------------------------------------------------
#  Documentation areas
# --------------------------------------------------------------------
#
DOC=$AIPS_VERSION/DOC
export DOC
DOCTXT=$AIPS_VERSION/DOC/TEXT
export DOCTXT
#
# --------------------------------------------------------------------
#  Core dump area
# --------------------------------------------------------------------
#
ERRORS=$AIPS_VERSION/$ARCH/ERRORS/$HOST
export ERRORS
#
# --------------------------------------------------------------------
#  HELP file area
# --------------------------------------------------------------------
#
HLPFIL=$AIPS_VERSION/HELP
export HLPFIL
#
# --------------------------------------------------------------------
#  History area
# --------------------------------------------------------------------
#
HIST=$AIPS_VERSION/HIST
export HIST
#
# --------------------------------------------------------------------
#  INCLUDE file areas
# --------------------------------------------------------------------
#
#                                       Standard INCLUDEs
INC=$AIPS_VERSION/INC
export INC
#                                       Non-standard INCLUDEs
INCNOT=$AIPS_VERSION/INC/NOTST
export INCNOT
#                                       GNU (readline)
INCGNU=$AIPS_VERSION/INC/NOTST/GNU
export INCGNU
#                                       Local
INCLOC=$AIPS_VERSION/INC/LOCAL
export INCLOC
#                                       Alliant
INCALN=$AIPS_VERSION/INC/NOTST/ALLN
export INCALN
#                                       Alpha AXP Linux
INCAXL=$AIPS_VERSION/INC/NOTST/AXLINUX
export INCAXL
#                                       Power PC Linux
INCPPC=$AIPS_VERSION/INC/NOTST/LINUXPPC
export INCPPC
#                                       Cray Research Inc
INCCRI=$AIPS_VERSION/INC/NOTST/CRI
export INCCRI
#                                       Convex
INCVEX=$AIPS_VERSION/INC/NOTST/CVEX
export INCVEX
#                                       Digital (OSF/1, Ultrix)
INCDEC=$AIPS_VERSION/INC/NOTST/DEC
export INCDEC
#                                       HP-UX
INCHP=$AIPS_VERSION/INC/NOTST/HP
export INCHP
#                                       HP-UX 2 (for PAPC, big AP)
INCHP2=$AIPS_VERSION/INC/NOTST/HP2
export INCHP2
#                                       IBM RS/6000
INCIBM=$AIPS_VERSION/INC/NOTST/IBM
export INCIBM
#                                       IBM 3090 (AIX)
INC3090=$AIPS_VERSION/INC/NOTST/IBM/3090
export INC3090
#                                       Intel Linux
INCLIN=$AIPS_VERSION/INC/NOTST/LINUX
export INCLIN
#                                       Intel Linux 64bit
INCLNX64=$AIPS_VERSION/INC/NOTST/LNX64
export INCLNX64
#                                       MacIntosh (Darwin OS/X)
INCMACPP=$AIPS_VERSION/INC/NOTST/MACPPC
export INCMACPP
#                                       MacIntosh (Darwin OS/X) Intel
INCMACIN=$AIPS_VERSION/INC/NOTST/MACINT
export INCMACIN
#                                       Silicon Graphics IRIX
INCSGI=$AIPS_VERSION/INC/NOTST/SGI
export INCSGI
#                                       SunOS 5 (Solaris 2)
INCSOL=$AIPS_VERSION/INC/NOTST/SOL
export INCSOL
#                                       SunOS 5 (Solaris 2) UltraII
INCSUL=$AIPS_VERSION/INC/NOTST/SUL
export INCSUL
#                                       SunOS 4 (Solaris 1)
INCSUN=$AIPS_VERSION/INC/NOTST/SUN
export INCSUN
#                                       VMS
INCVMS=$AIPS_VERSION/INC/NOTST/VMS
export INCVMS
#
# --------------------------------------------------------------------
#  Object module areas
# --------------------------------------------------------------------
#
#                                       Subroutine object libraries
LIBR=$AIPS_VERSION/$ARCH/LIBR
export LIBR
#                                       Debug version of libraries
LIBRDBG=$AIPS_VERSION/$ARCH/LIBRDBG
export LIBRDBG
#                                       Executable modules
LOAD=$AIPS_VERSION/$ARCH/LOAD
export LOAD
#                                       Alternate executable modules
#                                       commented out; rarely used
#                                       LOAD1 is Pseudo AP w/wo TV 1
#                                        and is LOAD.ALT1
#                                       LOAD2 is for TV 2 w/wo real AP
#                                        and is LOAD.ALT2
#                                       LOAD3 is for TV 2 w Pseudo AP
#                                        and is LOAD.ALT3
#
# --------------------------------------------------------------------
# PREP area for temporary files (unix only)
# --------------------------------------------------------------------
#
PREP=$AIPS_VERSION/$ARCH/PREP
export PREP
#
# --------------------------------------------------------------------
#  POPS memory file area
# --------------------------------------------------------------------
#
MEMORY=$AIPS_VERSION/$ARCH/MEMORY
export MEMORY
#
# --------------------------------------------------------------------
#  Q-routine areas (real and pseudo array processor)
# --------------------------------------------------------------------
#
#                                       Generic
QDEV=$AIPS_VERSION/Q/DEV
export QDEV
#                                       Generic FPS
QFPS=$AIPS_VERSION/Q/DEV/FPS
export QFPS
#                                       16 bit FPS
QFPS16=$AIPS_VERSION/Q/DEV/FPS/16B
export QFPS16
#                                       Model 120B FPS
Q120B=$AIPS_VERSION/Q/DEV/FPS/16B/120B
export Q120B
#                                       Models 5105, 5205 ... FPS
Q5000=$AIPS_VERSION/Q/DEV/FPS/16B/5000
export Q5000
#                                       32 bit FPS
QFPS32=$AIPS_VERSION/Q/DEV/FPS/32B
export QFPS32
#                                       Model 190 FPS
Q190=$AIPS_VERSION/Q/DEV/FPS/32B/190
export Q190
#                                       Generic pseudo AP
QPSAP=$AIPS_VERSION/Q/DEV/PSAP
export QPSAP
#                                       Alliant pseudo AP
QALN=$AIPS_VERSION/Q/DEV/PSAP/ALLN
export QALN
#                                       Cray Research Inc pseudo AP
QCRI=$AIPS_VERSION/Q/DEV/PSAP/CRI
export QCRI
#                                       Convex pseudo AP
QVEX=$AIPS_VERSION/Q/DEV/PSAP/CVEX
export QVEX
#                                       Digital: OSF/1, Ultrix
QDEC=$AIPS_VERSION/Q/DEV/PSAP/DEC
export QDEC
#                                       HP-UX
QHP=$AIPS_VERSION/Q/DEV/PSAP/HP
export QHP
#                                       IBM RS/6000
QIBM=$AIPS_VERSION/Q/DEV/PSAP/IBM
export QIBM
#                                       IBM 3090 (AIX)
Q3090=$AIPS_VERSION/Q/DEV/PSAP/IBM/3090
export Q3090
#                                       Silicon Graphics IRIX PS-AP
QSGI=$AIPS_VERSION/Q/DEV/PSAP/SGI
export QSGI
#                                       SunOS 5 (Solaris 2) pseudo AP
QSOL=$AIPS_VERSION/Q/DEV/PSAP/SOL
export QSOL
#                                       SunOS 4 (Solaris 1) pseudo AP
QSUN=$AIPS_VERSION/Q/DEV/PSAP/SUN
export QSUN
#                                       VMS pseudo AP
QVMS=$AIPS_VERSION/Q/DEV/PSAP/VMS
export QVMS
#
# --------------------------------------------------------------------
#  Programs that reference Q-routines
# --------------------------------------------------------------------
#
#                                       Standard programs
QPGM=$AIPS_VERSION/Q/PGM
export QPGM
#                                       Q-Object Oriented programs
QPGOOP=$AIPS_VERSION/Q/PGM/OOP
export QPGOOP
#                                       Non-standard programs
QPGNOT=$AIPS_VERSION/Q/PGM/NOTST
export QPGNOT
#                                       VMS programs
QPGVMS=$AIPS_VERSION/Q/PGM/NOTST/VMS
export QPGVMS
#
# --------------------------------------------------------------------
#  Subroutines that reference Q-routines
# --------------------------------------------------------------------
#
#                                       Standard routines
QSUB=$AIPS_VERSION/Q/SUB
export QSUB
#                                       Object-oriented Q-routines
QOOP=$AIPS_VERSION/Q/SUB/OOP
export QOOP
#                                       Non-standard routines
QNOT=$AIPS_VERSION/Q/SUB/NOTST
export QNOT
#
# --------------------------------------------------------------------
#  Programs that reference both Q-routines and Y-routines
# --------------------------------------------------------------------
#
#                                       Standard programs
QYPGM=$AIPS_VERSION/QY/PGM
export QYPGM
#                                       Non-standard programs
QYPGNOT=$AIPS_VERSION/QY/PGM/NOTST
export QYPGNOT
#                                       VMS programs
QYPGVMS=$AIPS_VERSION/QY/PGM/NOTST/VMS
export QYPGVMS
#
# --------------------------------------------------------------------
#  System RUN file area - useful procedures for everyone
# --------------------------------------------------------------------
#
RUNSYS=$AIPS_VERSION/RUN
export RUNSYS
#
# --------------------------------------------------------------------
#  System manager areas
# --------------------------------------------------------------------
#
#                                       Generic
SYSAIPS=$AIPS_VERSION/SYSTEM
export SYSAIPS
#                                       Generic UNIX
SYSUNIX=$AIPS_VERSION/SYSTEM/UNIX
export SYSUNIX
#                                       Generic PERL
SYSPERL=$AIPS_VERSION/SYSTEM/UNIX/PERL
export SYSPERL
#                                       (Local) manual pages
SYSMAN=$AIPS_VERSION/SYSTEM/UNIX/manl
export SYSMAN
#                                       Alliant
SYSALLN=$AIPS_VERSION/ALLN/SYSTEM
export SYSALLN
#                                       Cray Research Inc
SYSCRI=$AIPS_VERSION/CRI/SYSTEM
export SYSCRI
#                                       Convex
SYSCVEX=$AIPS_VERSION/CVEX/SYSTEM
export SYSCVEX
#                                       Decstation (Ultrix)
SYSDEC=$AIPS_VERSION/DEC/SYSTEM
export SYSDEC
#                                       Digital Alpha (OSF/1)
SYSALPHA=$AIPS_VERSION/ALPHA/SYSTEM
export SYSALPHA
#                                       HP-UX
SYSHP=$AIPS_VERSION/HP/SYSTEM
export SYSHP
#                                       HP-UX
SYSHP2=$AIPS_VERSION/HP2/SYSTEM
export SYSHP2
#                                       IBM RS/6000
SYSIBM=$AIPS_VERSION/IBM/SYSTEM
export SYSIBM
#                                       IBM 3090 (AIX)
SYS3090=$AIPS_VERSION/IBM3090/SYSTEM
export SYS3090
#                                       Silicon Graphics IRIX
SYSSGI=$AIPS_VERSION/SGI/SYSTEM
export SYSSGI
#                                       Linux/PC
SYSLINUX=$AIPS_VERSION/LINUX/SYSTEM
export SYSLINUX
#                                       Linux/PC 64bit
SYSLNX64=$AIPS_VERSION/LNX64/SYSTEM
export SYSLNX64
#                                       MacIntosh (Darwin OS/X)
SYSMACPP=$AIPS_VERSION/MACPPC/SYSTEM
export SYSMACPP
#                                       MacIntosh (Darwin OS/X) Intel
SYSMACIN=$AIPS_VERSION/MACINT/SYSTEM
export SYSMACIN
#                                       Linux/Alpha-AXP
SYSAXLIN=$AIPS_VERSION/AXLINUX/SYSTEM
export SYSAXLIN
#                                       Linux/Power-PC
SYSPPLIN=$AIPS_VERSION/LINUXPPC/SYSTEM
export SYSPPLIN
#                                       SunOS 5 (Solaris 2)
SYSSOL=$AIPS_VERSION/SOL/SYSTEM
export SYSSOL
#                                       SunOS 5 (Solaris 2) UltraII
SYSSUL=$AIPS_VERSION/SUL/SYSTEM
export SYSSUL
#                                       SunOS 5 (Solaris 2) iA86
SYSSOL86=$AIPS_VERSION/SOL86/SYSTEM
export SYSSOL86
#                                       SunOS 4 (Solaris 1)
SYSSUN=$AIPS_VERSION/SUN4/SYSTEM
export SYSSUN
#                                       Old 68020/68030 Suns
SYSSUN3=$AIPS_VERSION/SUN3/SYSTEM
export SYSSUN3
#                                       UNIX installation
INSUNIX=$AIPS_VERSION/SYSTEM/UNIX/INSTALL
export INSUNIX
#                                       UNIX midnight job
UPDUNIX=$AIPS_VERSION/SYSTEM/UNIX/UPDATE
export UPDUNIX
#                                       Generic VMS
SYSVMS=$AIPS_VERSION/SYSTEM/VMS
export SYSVMS
#                                       Local VMS
SYSLVAX=$AIPS_VERSION/SYSTEM/VMS/LOCAL
export SYSLVAX
#                                       VMS installation
INSVMS=$AIPS_VERSION/SYSTEM/VMS/INSTALL
export INSVMS
#                                       VMS midnight job
UPDVMS=$AIPS_VERSION/SYSTEM/VMS/UPDATE
export UPDVMS
#
# --------------------------------------------------------------------
#  Y-routine areas
# --------------------------------------------------------------------
#
#                                       Generic
YGEN=$AIPS_VERSION/Y/DEV
export YGEN
#                                       ARGS pipe from M70
YARGS=$AIPS_VERSION/Y/DEV/ARGS
export YARGS
#                                       Deanza
YDEA=$AIPS_VERSION/Y/DEV/DEA
export YDEA
#                                       IIS generic
YIIS=$AIPS_VERSION/Y/DEV/IIS
export YIIS
#                                       IIS Model IVAS
YIVAS=$AIPS_VERSION/Y/DEV/IVAS
export YIVAS
#                                       IIS Model 70
YM70=$AIPS_VERSION/Y/DEV/IIS/M70
export YM70
#                                       IIS Model 75
YM75=$AIPS_VERSION/Y/DEV/IIS/M75
export YM75
#                                       Lexidata
YLEX=$AIPS_VERSION/Y/DEV/LEX
export YLEX
#                                       Lexidata C code
YLEXC=$AIPS_VERSION/Y/DEV/LEX/LEXC
export YLEXC
#                                       Screen Servers SSS, XAS, XVSS
YSS=$AIPS_VERSION/Y/DEV/SS
export YSS
#                                       Stubbed
YSTUB=$AIPS_VERSION/Y/DEV/STUB
export YSTUB
#                                       Comtal Vision 1/20
YV20=$AIPS_VERSION/Y/DEV/V20
export YV20
#                                       Virtual TV
YVTV=$AIPS_VERSION/Y/DEV/VTV
export YVTV
#
# --------------------------------------------------------------------
#  Programs that reference Y-routines
# --------------------------------------------------------------------
#
#                                       Standard programs
YPGM=$AIPS_VERSION/Y/PGM
export YPGM
#                                       Virtual TV program
YPGVDEV=$AIPS_VERSION/Y/PGM/VDEV
export YPGVDEV
#                                       Non-standard programs
YPGNOT=$AIPS_VERSION/Y/PGM/NOTST
export YPGNOT
#                                       VMS programs
YPGVMS=$AIPS_VERSION/Y/PGM/NOTST/VMS
export YPGVMS
#
#
# --------------------------------------------------------------------
#  Screen server programs
# --------------------------------------------------------------------
#
#                                       Servers
YSERV=$AIPS_VERSION/Y/SERVERS
export YSERV
#
# --------------------------------------------------------------------
#  Subroutines that reference Y-routines
# --------------------------------------------------------------------
#
#                                       Standard routines
YSUB=$AIPS_VERSION/Y/SUB
export YSUB
#                                       Non-standard routines
YNOT=$AIPS_VERSION/Y/SUB/NOTST
export YNOT

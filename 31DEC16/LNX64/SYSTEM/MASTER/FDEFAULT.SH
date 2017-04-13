#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2003, 2005, 2007, 2009, 2015
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------
# FDEFAULT.SH
# set compiler name, compiler switches to AIPS standard names
# Input Variables
#   ARCH      = Type of CPU (Convex Sun or IBM)
# Variables returned:
#   COMPILER  = Name of fortran Compiler
#   COMPSWIT  = Switches needed to enforce AIPS coding standards
#               IMPORTANT: Within the case statement on $ARCH, the
#               setting of this variable HAS to be on ONE SINGLE LINE!
#               This is for install.pl's use.
#   OPT(0-9)  = Ten compiler optimzation levels
#   DEBUG     = Compiler Debug switch
#
#   Posix version, with allowances for most AIPS-supported OS versions.
#   (based on the 1991 draft 11.2 of Posix 1003.2)
#   LINUX-only version
#-----------------------------------------------------------------------
#                                       Set default compiler for POSIX
COMPILER="fort77"
#                                       Dirty is a carryover from VMS...
DIRTY=""; NODIRTY=""
#                                       POSIX Fortran 77 debug flag.
DEBUG="-g"; NODEBUG=""
#                                       Profile, architecture dependent.
PROFILE=""
#                                       POSIX compile-only switch
COMPSWIT="-c"
#                                       POSIX "no optimize" directive
NOOPT="-O0"
#                                       Generate source code listing?
LIST=FALSE
#                                       Remove leftover .f, .LOG files
PURGE=FALSE
#                                       Case by case differences from
#                                       the above standard definitions.
case $ARCH in
   LNX64)
#                                       --------------------------------
#                                       PC-386/486/Pent. running Linux.
#                                       Change location if needed.
# Original:       COMPILER=/opt/intel/fce/10.1.021/bin/ifort
# Changed by install.pl on 2015.11.30:10:21:58
      COMPILER=/opt/intel/fce/10.1.021/bin/ifort
# Original:       COMPSWIT="$COMPSWIT -fno-automatic -fno-globals -Wno-globals -malign-double -Wimplicit -Wall"
# Changed by install.pl on 2015.11.30:10:21:58
      COMPSWIT="-c -axWPT -w90 -w95 -cm -ip -save -align all"
      case $COMPILER in
#                                       GNU g77
         *g77*)
              OPT0="" ; OPT1="-O"
#                                       Options for GNU 2.95.3
#             OPT2="$OPT1 -fexpensive-optimizations -funroll-loops"
#             OPT2="$OPT2 -fstrength-reduce -frerun-cse-after-loop"
#             OPT2="$OPT2 -fforce-mem -fforce-addr -fcaller-saves"
#                                       These are probably dangerous...
#             OPT2="$OPT2 -ffast-math -funroll-loops"
#                                       Options for GNU 3.2+
              OPT2="-O2"
              OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
              OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
              NOOPT=$OPT0
#             DEBUG="$DEBUG -fdebug-kludge"
              ;;
#                                       GNU gfortran
         *gfortran*)
              COMPSWIT="-c -fno-automatic -Wall -Wuninitialized -finit-local-zero -m64"
              OPT0="" ;  OPT1="-O"
              OPT2="-O2"
              OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
              OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
              NOOPT=$OPT0
              ;;
#                                       Intel compiler
         *ifc)
#             COMPILER=/opt/intel/compiler60/ia32/bin/ifc
              COMPSWIT="$COMPSWIT -save -w -tpp6"
              OPT0='-O0'; OPT1='-O2 -unroll -align';
              OPT2='-O3 -unroll -align';
              OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
              OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
              ;;
#                                       New Intel compiler
         *ifort*)
              COMPSWIT="-c -axWPT -w90 -w95 -cm -ip -save -align all"
              OPT0='-O0'; OPT1='-O3'; OPT2='-O3';
              OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
              OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
              ;;
#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
         *)
#                                       These are in $SYSLINUX (CCOPTS
#                                       and LDOPTS [.SH]).  Keep them
#                                       consistent if you must use this
#                                       pseudo-compiler.  You really
#                                       should be using egcs 1.x or gcc
#                                       2.95 or better!!!!
#                                       NOTE: install.pl CANNOT change
#                                       these settings!
              COMPSWIT="-ARw8 -Nn1604 -Nx400"
              OPT0="-O0" ; OPT1="-O1" ; OPT2="-O2"
              OPT3=$OPT2 ; OPT4=$OPT2 ; OPT5=$OPT2 ; OPT6=$OPT2
              OPT7=$OPT2 ; OPT8=$OPT2 ; OPT9=$OPT2;;
      esac
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "You are using a Linux FDEFAULT.SH on " $ARCH
      OPT0="-O0" ; OPT1="-O1" ; OPT2="-O2"
      OPT3=$OPT2 ; OPT4=$OPT2 ; OPT5=$OPT2 ; OPT6=$OPT2
      OPT7=$OPT2 ; OPT8=$OPT2 ; OPT9=$OPT2
   ;;
esac
#
COMP=$COMPSWIT
#                                       export the defined values
export NODEBUG DEBUG DIRTY NODIRTY PROFILE PURGE
export COMPILER COMPSWIT NOOPT
export OPT0 OPT1 OPT2 OPT3 OPT4 OPT5 OPT6 OPT7 OPT8 OPT9

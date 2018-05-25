#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2003, 2006, 2016
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
#   Mac INTEL version with Intel compiler
#-----------------------------------------------------------------------
#                                       Set default compiler for POSIX
#                          new Intel compiler
COMPILER="ifort"
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
#                                       --------------------------------
#                                       Mac Intel box.
   MACINT)
#                                       Change location if needed
#                                       New Intel compiler.
      COMPILER=/usr/local/bin/ifort
      COMPSWIT="-c"
#
      case $COMPILER in
#                                       Check g77 or not...
#                                       GNU g77
         *g77)
              COMPILER=/sw/bin/g77
              COMPSWIT="$COMPSWIT -fno-automatic -fno-globals -Wno-globals -fomit-frame-pointer"
              OPT0=" " ; OPT1="-O"
              OPT2="-O2"; OPT3=$OPT2
              ;;
#                                       IBM compiler
         *xlf)
#             COMPILER=/opt/ibmcmp/xlf/8.1/bin/xlf
              COMPSWIT="-c -qextname"
#                                       for G5 chip
#             COMPSWIT="$COMPSWIT -qarch=ppc970 -qtune=ppc970"
              OPT0="-O1 " ; OPT1="-O3" ; OPT2="-O3" ; OPT3=$OPT2
              ;;
#                                       New Intel compiler
         *ifort*)
              COMPSWIT="-c -ip -save -align all -mmacosx-version-min=10.7"
              OPT0='-O0'; OPT1='-O3'; OPT2='-O3';
              OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
              OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
              ;;

#                                       not recognized
         *)
              echo "Unrocognized compiler" $COMPILER " on " $ARCH
              OPT0="-O0" ; OPT1="-O1" ; OPT2="-O2" ; OPT3=$OPT2
              ;;
      esac
      OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
      OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
      NOOPT=$OPT0
      ;;
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
   *)
      echo "You are using a MacINTEL FDEFAULT.SH on " $ARCH
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

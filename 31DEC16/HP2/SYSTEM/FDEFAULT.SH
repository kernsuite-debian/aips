#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2001
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
# Posix version, with allowances for most AIPS-supported OS versions.
# (based on the 1991 draft 11.2 of Posix 1003.2)
#
#    HP2 version
#
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
   HP2)
#                                       --------------------------------
#                                       Hewlett-Packard 9000/7xx HP-UXv8
      COMPILER="f77"
#     COMPILER="f90" may or may not work; untested, and +E4 not available.
#                    lack of this last item may make printout look wonky.
#                                       -a: ansi compliance
#                                       -u: implicit none checking
#                                       -v: mucho output.
#                                       +ppu: externs "_" postpended
#                                       -K: automatically SAVE variables
#                                       +A: allow real/double EQUIVALENCE
#                                           actually puts variables at
#                                           I*2 addresses (very slow)
#                                       +E4: get leading 0's in F format
      COMPSWIT="+DA2.0 +DS2.0 -c -a -u -v +ppu -K +E4"
      case $COMPILER in
#                                       These are the f90 equivalents
#                                       except for +E4.  Caveat Emptor!
        *f90) COMPSWIT="-c +langlvl +implicit_none -v +ppu -K";;
      esac
#                                       Using shared libs?  +Z => PIC.
      [ -f $SYSLOCAL/USESHARED ] && COMPSWIT="$COMPSWIT +Z"
      NOOPT=""
      OPT0=""; OPT1="+O1"
      OPT2="+O2 +Odataprefetch +Oaggressive";
      OPT3="+O2 +Odataprefetch +Oaggressive"
      OPT4="+O2 +Odataprefetch +Oaggressive"
      OPT5="+O2 +Odataprefetch +Oaggressive"
      OPT6="+O2 +Odataprefetch +Oaggressive"
      OPT7="+O2 +Odataprefetch +Oaggressive"
      OPT8="+O2 +Odataprefetch +Oaggressive"
      OPT9="+O2 +Odataprefetch +Oaggressive"
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "You are using an HP2  FDEFAULT.SH on " $ARCH
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

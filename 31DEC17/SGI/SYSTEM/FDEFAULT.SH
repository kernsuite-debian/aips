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
   SGI)
#                                       --------------------------------
#                                       Silicon Graphics IRIX
#                                       Flags assume 7.0 compilers and
#                                       R8000/R10000 CPUs
      COMPILER=f77
      NOOPT=""
#                                       -mips4 enables code generation
#                                        for R8000/R10k instruction set
#                                       -r10000 enables scheduling for
#                                        R10k processors
#                                       -n32 uses the 32-bit MIPS ABI
#                                       -bytereclen makes record lengths
#                                        bytes for FORTRAN I/O
#                                       -static is needed to prevent
#                                        stack overflows
#                                       -avoid_gp_overflow automatically
#                                        deals with offset table overflows
      COMPSWIT="-c -u -ansi -static -bytereclen -avoid_gp_overflow -n32 -mips4 -r10000 -woff 2034"
#                                       Other AIPS options follow.
      OPT0="-O0"
      OPT1="-O1 -OPT:Olimit=2500"; OPT2="-O2 -OPT:Olimit 2500";
      OPT3="-O3 -OPT:Olimit=2500"
#                                       OPT4 enables MP directives
#                                       OPT5 enables automatic
#                                            parallelization
#                                       Set both of these equal to
#                                       OPT3 if your AIPS installation
#                                       supports single-processor
#                                       machines.
      OPT4="$OPT3 -mp"
      OPT5="$OPT3 -pfa keep"
#                                       Special case for Calgary MNJ:
      if [ "$SITE" = "CALSGI" ] ; then
         OPT4="$OPT3"
         OPT5="$OPT4"
      fi
#                                       higher levels of optimization
#                                       are not yet supported for AIPS
      OPT6=$OPT5; OPT7=$OPT6; OPT8=$OPT7; OPT9=$OPT8
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "You are using an SGI  FDEFAULT.SH on " $ARCH
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

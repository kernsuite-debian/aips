#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2005
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
#    SUL version
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
   SUL)
#                                       --------------------------------
#                                       Sun4/Sparc, SunOS 5+.  NOTE!!!!!
#                                       These settings are for the Sparc
#                                       Compiler version 4.x; they may
#                                       not all be available in older
#                                       compilers.  Check the f77 manual
#                                       page in /opt/SUNWspro/man/f77.1
#                                       if you are in any doubt.
#                                       MUST BE CONSISTENT between this,
#                                       LDOPTS.SH and CCOPTS.SH
#                                       (last two in $SYSLOCAL).
#                                       -fns uses Sparc nonstandard f.p.
#                                       -fsimple=1 allows conservative
#                                         simplifications in optimizing.
#                                       -xtarget=generic forces result
#                                         to run on all Sparc arch's.
#                                       -xlibmopt includes optimized
#                                         math library
#                                       -xarch=v8plusa uses Ultra
#                                         instructions with VIS
#                                         extensions (reported to
#                                         help some vector operations);
#                                         use v8plus for non-Sun
#                                         SPARC v9 chips (eg. Fujitsu)
#                                         or expect a bad performance
#                                         hit.
#                                       Do not use -xchip=ultra or
#                                         -xtarget=ultra until further
#                                         notice: SW 4.2 generates bad
#                                         code at OPT3 for some loops
#                                         involving reduction over
#                                         arrays that are not indexed
#                                         by the loop index
      COMPSWIT="-c -fns -fsimple=1 -xarch=v8plusa -xlibmopt -dalign"
#
      COMPILER="/opt/SUNWspro/bin/f77"
#
      case $COMPILER in
#                                       SUN compiler
         *SUN*f77)
            OPT0="" ;    OPT1="-O1" ; OPT2="-O2" ; OPT3="-O3"
            OPT4="-O3" ; OPT5="-O3" ; OPT6="-O3" ; OPT7="-O3"
#                                       -O4 and up can slow some
#                                       programs down and should not be
#                                       used without evidence that they
#                                       help.
            OPT8="-O4" ; OPT9="-O5"
#                                       -e extends source to 132 columns
            DIRTY="-e"
#                                       -ansi warns of non-std. code
#                                       -u is "implicit none" equivalent
            NODIRTY="-ansi -u"
            ;;
#                                       GNU g77
#                                       these are untried!!
         *g77)
            COMPSWIT="-c -fno-automatic -fno-globals -Wno-globals"
            COMPSWIT="$COMPSWIT -Wimplicit -Wall"
#                                                   said to be Intel only
#           COMPSWIT="$COMPSWIT -malign-double"
            OPT0=" " ; OPT1="-O"
#            OPT2="$OPT1 -fexpensive-optimizations -funroll-loops"
#            OPT2="$OPT2 -fstrength-reduce -frerun-cse-after-loop"
#            OPT2="$OPT2 -fforce-mem -fforce-addr -fcaller-saves"
#                                       These are probably dangerous...
#           OPT2="$OPT2 -ffast-math -funroll-loops"
            OPT2="-O2"
            OPT3=$OPT2 ; OPT4=$OPT3 ; OPT5=$OPT4 ; OPT6=$OPT5
            OPT7=$OPT6 ; OPT8=$OPT7 ; OPT9=$OPT8
            DEBUG="$DEBUG -fdebug-kludge"
            ;;
      esac
#                                       set noopt level and default
      NOOPT=$OPT0
#                                       Using shared libs?
      [ -f $SYSLOCAL/USESHARED ] && COMPSWIT="$COMPSWIT  -PIC"
#                                       Option for profiling
      PROFILE="-p"
#                                       Exceptions for MNJ sites
      if [ "$SITE" = "NRL-RIOS" ] ; then
         COMPILER="/usr/SUNWspro/bin/f77"
         COMPSWIT="$COMPSWIT -xcache=16/32/1:2048/64/1 -xlibmil"
      fi
      COMPSWIT="$COMPSWIT $NODIRTY"
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "You are using an SUL FDEFAULT.SH on" $ARCH
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

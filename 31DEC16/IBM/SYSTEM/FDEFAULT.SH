#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2002
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
#     Only historical and uncommon systems
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
#                                       --------------------------------
#                                       IBM RS/6000 AIX 3.1 or 3.2
   IBM)
      COMPILER="xlf"
#                                       For ansi, use -fips.
#                                       For implicit none, use -u.
      COMPSWIT="-qcharlen=10000 -c -u -qfips -qextname"
      OPT0=""
#                                       -O  for basic optimization
      OPT1="-O"
#                                       -Q  for inlining
#                                       -ND sets ST_SIZE (default 2048),
#                                           number of variables;
#                                       -NA sets AUX_SIZE (default 8192)
#                                       max #bytes for variable names.
      OPT2="$OPT1 -Q -ND16384 -NA65536"
#                                       -Pv causes alternate (faster?)
#                                           preprocessor; AIX 3.2 or
#                                           higher only!
#                                       -qmaxmem needed for MX, etc.
#                                           (larger limit for OMFIT, 32768
#                                           is not enough!)
      altpp=no
      case `uname -v` in
         3) [ `uname -r` -ge 2 ] && altpp=yes;;
         4) altpp=yes;;
         *) echo "unknown version of AIX (`uname -v`.`uname -r`???)"
            echo "Please edit $SYSUNIX/FDEFAULT.SH and fix me!";;
      esac
      if [ $altpp = yes ] ; then
         OPT2="$OPT2 -qmaxmem=65536"
         if [ -f /usr/lpp/xlf/bin/fppv ] ; then
            OPT3="$OPT2 -Pv"
	 else
            OPT3=$OPT2
         fi
      else
         OPT3=$OPT2
      fi
      OPT4=$OPT3; OPT5=$OPT4; OPT6=$OPT5
      OPT7=$OPT6; OPT8=$OPT7
#                                       The HSFLT option causes single
#                                       precision expressions never to
#                                       rounded, and fp->int conversions
#                                       to be done w/o range checking.
#                                       THIS MAY GIVE BAD RESULTS.
      OPT9="$OPT8 -qhsflt"
#                                       AIX doesn't (yet) have -O0.
      NOOPT=""
#                                       Insane memory allocation scheme
      if [ "$PSALLOC" != "" ] ; then
         echo 'WARNING!  PSALLOC being reset, random processes may die!'
      fi
      PSALLOC=""; export PSALLOC
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "You are using a IBM FDEFAULT.SH on " $ARCH
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

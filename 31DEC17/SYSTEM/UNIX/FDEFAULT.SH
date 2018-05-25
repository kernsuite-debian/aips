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
#                                       Convex Unix OS 8.x, 9.x
   CVEX)
      COMPILER="/usr/convex/fc"
#                                       The -fi gets IEEE floats.
      COMPSWIT="-vn -72 -fi -c"
#                                       Various optimize settings
      OPT0="-O0" ; OPT1="-O1" ; OPT2="-O2"
      OPT3=$OPT2 ; OPT4=$OPT2 ; OPT5=$OPT2 ; OPT6=$OPT2
      OPT7=$OPT2 ; OPT8=$OPT2 ; OPT9=$OPT2
#                                       Debug, noopt different here.
      DEBUG="-db"
      NOOPT="-no"
   ;;
#                                       --------------------------------
#                                       IBM 3090.  accuracy problems due
#                                       to nature of floating point fmt.
   IBM3090)
      COMPILER="fvs"
      COMPSWIT="-f SAVE,CHARLEN(5000),NOOPTIMIZE -c"
      OPT0="OPTIMIZE(0)"
      NOOPT="OPTIMIZE(0)"
#                                       -O  for basic optimization
      OPT0="OPTIMIZE(1)" ; OPT1="OPTIMIZE(2)" ; OPT2="OPTIMIZE(3)"
      OPT3="VECTOR"
      OPT4="DC(HDRVAL,MSGCOM,MSGCHR,CLNCHR,CLNCOM,CFILES,DCHCHM,DCHCOM,FTABCM),"
      OPT5="DC(TVCHAR,TVDEV,HICOM,MAPHDR,APCTLL,INPARM,APCORE),"
      OPT6=$OPT2 ; OPT7=$OPT2 ; OPT8=$OPT2 ; OPT9=$OPT2
   ;;
   SUN*)
#                                       --------------------------------
#                                       Sun-3 and Sun4/Sparc.  Here,
#                                       -ansi warns of non-std. code
#                                       -native uses best f.p. hardware
#                                       -u is "implicit none" equivalent
#                                       -w suppress all warnings (POSIX)
      COMPSWIT="-c -native"
#                                       Use above if SunOS 4.1.2 or
#                                       later and Sun Fortran SC2.0.1 or
#                                       later. Also set it in in
#                                       $SYSLOCAL/LDOPTS.SH!  Otherwise
#                                       uncomment the next line.
#     COMPSWIT="-c"
      [ \( "$SITE" = "NRAOCV" \) -o \
        \( "$SITE" = "NRAOAOC" \) ] && COMPSWIT="-c"
#
      OPT0="" ;    OPT1="-O1" ; OPT2="-O2" ; OPT3="-O3"
      OPT4="-O4" ; OPT5="-O4" ; OPT6="-O4" ; OPT7="-O4"
#                                       levels 8 and 9 not reliable
      OPT8="-O4 -fast" ; OPT9="-O4 -fast"
#                                       set noopt level and default
      NOOPT=$OPT0
#                                       If a SUN-4 or SPARC
      case $ARCH in
         SUN4*)
            COMPILER="/usr/lang/f77"
#                                       Using shared libs?
            [ -f $SYSLOCAL/USESHARED ] && COMPSWIT="$COMPSWIT -PIC"
#                                       Option for profiling
            PROFILE="-p"
            DIRTY="-e"
            NODIRTY="-ansi -u"
            COMPSWIT="$COMPSWIT $NODIRTY"
         ;;
      esac
   ;;
   ALLN)
#                                       --------------------------------
#                                       Alliant (this is probably OLD)
      COMPILER="fortran"
      COMPSWIT="-v -w -c"
      OPT0="-Og" ; OPT1="-Ogv" ; OPT2="-Ogc"
      OPT3="-O"  ; OPT4="-DAS" ; OPT5="-alt"
      OPT6=$OPT5 ; OPT7=$OPT5  ; OPT8=$OPT5; OPT9=$OPT5
   ;;
   CRI)
#                                       --------------------------------
#                                       Cray Research Inc UNICOS version
      COMPILER="cft77"
      DEBUG="z"
      NOOPT="off"
      COMPSWIT="-V -m 0 -a static -eA -dp -A fast"
      OPT0="-o novector" ; OPT1="-o full" ; OPT2="-Zp"
      OPT3=$OPT2 ; OPT4=$OPT2  ; OPT5=$OPT2
      OPT6=$OPT5 ; OPT7=$OPT5  ; OPT8=$OPT5; OPT9=$OPT5
   ;;
   *)
#                                       --------------------------------
#                                       Other: Assume simple -O settings
#                                       In addition to the defaults.
      echo "using default settings with ARCH = " $ARCH
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

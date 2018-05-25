#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1999, 2001, 2005
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
# CCOPTS.SH
#-----------------------------------------------------------------------
# Define variables used by $SYSUNIX/AIPSCC.  These include
#    1) command name for the host C compiler, and
#    2) C compiler options and AIPS style options.
# Both can be toggled by, e.g. NODEBUG or DEBUG, NOPURGE or PURGE...
#
# NOTE: These are options for compiling ONLY (see definition of the
#       environment variable 'COMP' below).  Options applicable to the
#       loader are defined in $SYSLOCAL/LDOPTS.SH.
#
# SUN version (SunOS 5+)
#-----------------------------------------------------------------------

COMPILER="/opt/SUNWspro/bin/cc"
#COMPILER="/usr/local/bin/gcc" (make sure you include right libraries
#                               in $SYSLOCAL/LDOPTS.SH!  Otherwise you
#                               will get unknown erro

#                                       Some older AOC systems are weird
[ "$SITE" = "NRAOAOC" -a `uname -r` = "5.4" ] && \
 COMPILER=/opt/alpha/SUNWspro/bin/cc

# Define compiler oriented options in terms of the host C compiler
# command syntax.

#                                       Any options that may enhance or
#                                       may be required for the usage of
#                                       system debuggers.
DEBUG="-g"
#                                       Any of a wide variety of
#                                       optimization levels available
#                                       for UNIX C compilers and
#                                       combinations thereof.
OPT0=""
OPT1="-xO1"
OPT2="-xO2"
OPT3="-xO3"
OPT4="$OPT3"
OPT5="$OPT4"
OPT6="$OPT5"
OPT7="$OPT6"
OPT8="-xO4"
OPT9="-xO5"

#                                       Options for static libraries.
#                                       MUST BE CONSISTENT between this,
#                                       LDOPTS.SH and FDEFAULT.SH
#                                       (latter in $SYSUNIX).
#                                       -v means verbose cc comments
#                                       -Xc: strict ANSI conformance.
#                                       -I$INC gets AIPS include file.
#                                       -fns uses Sparc nonstandard f.p.
#                                       -fsimple=1 allows conservative
#                                         simplifications in optimizing.
#                                       -xtarget=generic forces result
#                                         to run on all Sparc arch's.
COMP="-v -c $OPT0 -Xc -I$INC -fns -fsimple=1 -xtarget=generic"

# pilabo serves on SPARCstation 20s so it is safe to target the
# code to the native architecture.

if [ "$SITE" = "NRAOCV" ] ; then
  COMP="-v -c $OPT0 -I$INC -fns -fsimple=1 -xtarget=generic"
elif [ "$SITE" = "NRAOAOC" -a `uname -n` = "pilabo" ] ; then
  COMP="-v -c $OPT0 -Xc -I$INC -fns -fsimple=1 -xtarget=native"
#                                       Some sites don't have SC4
elif [ \( "$SITE" = "VSOP"   \) ] ; then
   COMP="-v -c $OPT0 -Xc -I$INC -fnonstd -fsimple"
elif [ "$SITE" = "NFRA" ] ; then
#                                       NFRA uses gcc, not Sun cc.
   COMPILER=/aps3/local/bin/gcc
#                                       Extra settings from Lorant
   COMP="-c $OPT0 -I$INC -dalign"
fi

if [ -f $SYSLOCAL/USESHARED ] ; then
#                                       Options for shared libraries.
#                                       They are NOT recommended.
#                                       -K PIC: position independent
#                                            code (needed for shared)
   COMP="$COMP -K PIC"
fi
#                                       files > 2 GBytes.
COMP="$COMP -D_FILE_OFFSET_BITS=64"

# Set default values for other (i.e., non-compiler oriented) options
# recognized by AIPSCC.  These are purely a matter of preference,
# however the default settings exported with the UNIX/AIPS installation
# kits seem to make the most sense.

#                                       Generate source code listing?
LIST=FALSE
#                                       Purge, that is, delete any .c
#                                       and .LOG files associated with a
#                                       successful compilation and, if
#                                       LIST=FALSE, any extant .LIS file
#                                       associated with the program or
#                                       subroutine being compiled (i.e.
#                                       keep the AIPS source code direc-
#                                       tories free of trash files).
PURGE=TRUE

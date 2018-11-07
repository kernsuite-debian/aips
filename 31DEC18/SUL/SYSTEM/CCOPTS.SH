#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1999, 2001-2002, 2004
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
# This script is used to assign definitions to the environment variables
# used by $SYSUNIX/AIPSCC.  These include
#    1) the name of the command name for the host C compiler, and
#    2) the compiler oriented AIPSCC options in terms of the host C
#       compiler option syntax and the locally desired defaults for the
#       other non-compiler oriented options of AIPSCC.
# Both compiler and non-compiler oriented options can be toggled by the
# use of 'option' (e.g. DEBUG) or 'NOoption' (e.g., NOPURGE) command
# line arguments to AIPSCC or to any of the UNIX/AIPS programming shell
# scripts that invoke AIPSCC (i.e. COMRPL and COMLNK).
#
# NOTE: These are options for compiling ONLY (see definition of the
#       environment variable 'COMP' below).  Options applicable to the
#       loader are defined in $SYSLOCAL/LDOPTS.SH.
#
# SUN version (Solaris 2.4) with support for sharable libraries.
# Assumes some level of POSIX compliance and ANSI C.
#-----------------------------------------------------------------------

COMPILER="/opt/SUNWspro/bin/cc"
COMP="-v -fns -fsimple=1 -xarch=v8plus -dalign"

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

#                                       Options for static libraries.
#                                       MUST BE CONSISTENT between this,
#                                       LDOPTS.SH and FDEFAULT.SH
#                                       (latter in $SYSUNIX).
#                                       -v means verbose cc comments
#                                       -Xc: strict ANSI conformance.
#                                           ***** dropped for 64 ******
#                                       -I$INC gets AIPS include file.
#                                       -fns uses Sparc nonstandard f.p.
#                                       -fsimple=1 allows conservative
#                                         simplifications in optimizing.
#                                       -xtarget=generic forces result
#                                         to run on all Sparc arch's.
case $COMPILER in
#                                       GNU g77
    *gcc)
        OPT0="-O3 -fomit-frame-pointer -funroll-loops"
        OPT1="$OPT0"
        OPT2="$OPT1"
        OPT3="$OPT2"
        COMP=""
        ;;
#                                       SUN compiler
    *SUN*cc)
        OPT1="-xO1"
        OPT2="-xO2"
        OPT3="-xO3"
        OPT8="-xO4"
        OPT9="-xO5"
        ;;
#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
    *)
        COMP=""
        ;;
esac
COMP="$COMP -c -I$INC $OPT0 -D_FILE_OFFSET_BITS=64"
OPT4="$OPT3"
OPT5="$OPT4"
OPT6="$OPT5"
OPT7="$OPT6"

if [ \( "$SITE" = "VSOP"   \) ] ; then
   COMP="-v -c $OPT0 -Xc -I$INC -fnonstd -fsimple"
elif [ "$SITE" = "NRL-RIOS" ] ; then
   COMPILER="/usr/SUNWspro/bin/cc"
   COMP="$COMP -xcache=16/32/1:2048/64/1 -xlibmil"
elif [ "$SITE" = "NFRA" ] ; then
   COMPILER="/local/bin/gcc"
   COMP="-c $OPT0 -I$INC -dalign  -D_FILE_OFFSET_BITS=64"
fi

if [ -f $SYSLOCAL/USESHARED ] ; then
#                                       Options for shared libraries.
#                                       They are NOT recommended.
#                                       -K PIC: position independent
#                                            code (needed for shared)
   COMP="$COMP -K PIC"
fi

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

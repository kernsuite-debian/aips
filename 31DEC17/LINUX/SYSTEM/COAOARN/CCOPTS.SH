#!/bin/sh
# CCOPTS.SH
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1996, 1998, 2001-2002, 2005
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
# Define variables used by $SYSUNIX/AIPSCC.  These include
#    1) command name for the host C compiler, and
#    2) C compiler options and AIPS style options.
# Both can be toggled by, e.g. NODEBUG or DEBUG, NOPURGE or PURGE...
#
# NOTE: These are options for compiling C ONLY (see definition of the
#       environment variable 'COMP' below).  Options applicable to the
#       loader are defined in $SYSLOCAL/LDOPTS.SH.
#
#  NRAO-CV Linux version.
#--------------------------------------------------------------------

# Original: COMPILER=/usr/bin/gcc
# Changed by install.pl on 2016.11.30:09:44:48
COMPILER=/opt/intel/cc/9.1.047/bin/icc
#                                       System specific options.  Debug:
DEBUG="-g"
#                                       Options for static libraries.
#                                       MUST BE CONSISTENT between this,
#                                       LDOPTS.SH and FDEFAULT.SH
#                                       (latter in $SYSUNIX).
#                                       One hopes the 64 won't matter
#                                       to < 2.4.2 kernels

OPT0="-O3 -fomit-frame-pointer -funroll-loops"
# Original: COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64"
# Changed by install.pl on 2016.11.30:09:44:48
COMP="-c -O2 -I -D_FILE_OFFSET_BITS=64 -DHAVE_LINUX_GLIBC"

#                                       For glibc-based systems,
#                                       e.g. Red Hat 5.0 and later.
[ -f /lib/libc.so.6* ] && COMP="$COMP -DHAVE_LINUX_GLIBC"

case $COMPILER in
#                                       GNU g77
    *gcc*)
        ;;
#                                       Intel compiler
    *ifc)
        OPT0="-O2 -unroll"
        COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64 -DHAVE_LINUX_GLIBC"
        ;;
#                                       New Intel compiler
    *icc*)
        OPT0="-O3 -ip -axNP"
        COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64 -DHAVE_LINUX_GLIBC"
        ;;
#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
    *)
        ;;
esac

#                                       Optimizations (safe ones).
OPT1="$OPT0"
OPT2="$OPT1"
OPT3="$OPT2"
OPT4="$OPT3"
OPT5="$OPT4"
OPT6="$OPT5"
OPT7="$OPT6"
OPT8="$OPT7"
OPT9="$OPT8"
#                                       AIPS style options
#                                       Generate source code listing?
LIST=FALSE
#                                       Purge: delete .c, .LOG files.
#                                       if LIST=FALSE, also  .LIS files.
PURGE=TRUE
#                                       End

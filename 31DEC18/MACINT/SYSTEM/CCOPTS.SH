#!/bin/sh
# CCOPTS.SH
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2001, 2003, 2006, 2016, 2018
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
# NOTE: These are options for compiling ONLY (see definition of the
#       environment variable 'COMP' below).  Options applicable to the
#       loader are defined in $SYSLOCAL/LDOPTS.SH.
#
#  Mac/INT version.
#--------------------------------------------------------------------
#                                       GNU gcc/g77
#  COMPILER=/usr/bin/gcc
#                                       IBM xlc/xlf
#  COMPILER=/opt/ibmcmp/vac/6.0/bin/xlc
#					Intel ICC
#  COMPILER=/opt/intel/cc/9.1.024/bin/icc
COMPILER=/usr/local/bin/icc
#                                       System specific options.  Debug:
DEBUG="-g"
#                                       Optimizations (safe ones).
OPT0="-O1"
COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64"
case $COMPILER in
#                                       GNU g77
    *gcc*)
        OPT0="-O3 -fomit-frame-pointer -funroll-loops"
        COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64"
        ;;
#                                       IBM compiler
    *xlc)
#       COMPILER=/opt/ibmcmp/xlf/8.1/bin/xlf
        OPT0="-O3"
#                                       Add for G5 machines w xlf
#       COMP="$COMP -qarch=ppc970 -qtune=ppc970"
        ;;
#                                       Intel compiler
    *ifc)
        OPT0="-O2 -unroll"
        COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64"
        ;;
#                                       New Intel compiler
    *icc*)
        OPT0="-O3 -ip"
        COMP="-c $OPT0 -I$INC -D_FILE_OFFSET_BITS=64 -mmacosx-version-min=10.7"
        ;;

#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
    *)
        OPT0="-O1"
        ;;
esac
OPT1="$OPT0"
OPT2="$OPT1"
OPT3="$OPT2"
OPT4="$OPT3"
OPT5="$OPT4"
OPT6="$OPT5"
OPT7="$OPT6"
OPT8="$OPT7"
OPT9="$OPT8"
#
#                                       Options for static libraries.
#                                       MUST BE CONSISTENT between this,
#                                       LDOPTS.SH and FDEFAULT.SH
#                                       (latter in $SYSUNIX).
#
#                                       For glibc-based systems,
#                                       e.g. Red Hat 5.0 and later.
[ -f /lib/libc.so.6* ] && COMP="$COMP -DHAVE_LINUX_GLIBC"
#
#                                       AIPS style options
#                                       Generate source code listing?
LIST=FALSE
#                                       Purge: delete .c, .LOG files.
#                                       if LIST=FALSE, also  .LIS files.
PURGE=TRUE
#                                       End

# CCOPTS.SH
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1998, 2001
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
# Linux Alpha (AXLINUX): egcs-2.90.29 980515 (egcs-1.0.3 release)
#-----------------------------------------------------------------------

COMPILER=/opt/local/egcs/bin/gcc
#                                       Debug flag; the kludge may not
#                                       be necessary.  In fact it breaks
#                                       AIPSCC so don't use it!
#DEBUG="-g -fdebug-kludge"
DEBUG="-g"
#                                       optimization levels
OPT0="-O3 -fomit-frame-pointer -funroll-loops"
OPT1="$OPT0"
OPT2="$OPT0"
OPT3="$OPT0"
OPT4="$OPT0"
OPT5="$OPT0"
OPT6="$OPT0"
OPT7="$OPT0"
OPT8="$OPT0"
OPT9="$OPT0"
#                                       Default compiler options
#                                       The -I$INC tells the compiler to
#                                       look for header files in $INC.
#                                       This port only works with glibc
COMP="-c $OPT0 -I$INC -DHAVE_LINUX_GLIBC"
COMP="$COMP -D_FILE_OFFSET_BITS=64"
#                                       AIPS options
#                                       Generate source code listing?
LIST=FALSE
#                                       Purge .c and .LOG files?
PURGE=TRUE

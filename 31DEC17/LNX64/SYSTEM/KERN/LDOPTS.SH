#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1997-1998, 2001-2002, 2005, 2007-2009, 2015
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
#--------------------------------------------------------------------
# Usage:  . LDOPTS.SH
#--------------------------------------------------------------------
# Define variables used by $SYSUNIX/LINK.  These include:
#     1) command name for the host Fortran compiler (e.g., f77),
#     2) f77/ld options for linking, and
#     3) AIPS-style options, e.g. LIST=FALSE, SAVE=TRUE.
# See comments in LINK for more details on AIPS options.
#
# NOTE: These are options for linking ONLY (see definition of the
#       environment variable 'LINK' below).  Options for compiling are
#       defined in the $SYSLOCAL files ASOPTS.SH and CCOPTS.SH for
#       assembler and 'C', respectively, and in $SYSUNIX/FDEFAULT.SH for
#       Fortran compilations.
#
# Linux (Intel) version
# --------------------------------------------------------------------
#                                       Name of Fortran command; use it
#                                       instead of ld; it's simpler.
# Original: COMPILER=/opt/intel/fce/10.1.021/bin/ifort
# Changed by install.pl on 2016.11.30:10:27:05
# Original: COMPILER=/opt/intel/fce/10.1.021/bin/ifort
# Changed by install.pl on 2016.11.30:10:31:57
COMPILER=/opt/intel/fce/10.1.021/bin/ifort

#                                       Define AIPS-style options.
#                                       Debug and no-optimise flags.
DEBUG="-g"
NOOPT=""
#                                       Not used, but still needed :-(
DIRTY=""
#                                       f77/ld option for a link map.
MAP=""
#                                       Not used, but still needed :-(
OPT0=""
OPT1=""
OPT2=""
OPT3=""
OPT4=""
OPT5=""
OPT6=""
OPT7=""
OPT8=""
OPT9=""
#                                       execution profiling.
PROFILE=""
#                                       Link options, usually none.
LINK=""
case $COMPILER in
#                                       GNU g77
    *g77*)
        ;;
#                                       GNU gfortran
    *gfortran*)
        LINK="-m64"
        ;;
#                                       Intel compiler
    *ifc)
        LINK="-Vaxlib -i_dynamic"
        ;;
#                                       New Intel compiler
    *ifort*)
        IOPT="-Xlinker -rpath -Xlinker $LIBR/INTELCMP"
        LINK="-lsvml -Vaxlib -L/usr/local/lib $IOPT -O3 -static-libgcc -save -align all -axWPT"
        ;;
#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
    *)
        ;;
esac
#                                       Linux/glibc has marginalized
#                                       this library; it's rolled
#                                       everything that we need into
#                                       libc itself.
[ -f /lib/libc.so.6* ] || LINK="-lbsd"
#                                       Non-Red Hat systems may want this
[ "$SITE" = NFRA ] && LINK="-L/usr/lib/termcap -ltermcap"

#                                       AIPS style options.
#                                       LIST not used but still needed
LIST=FALSE
#                                       Delete any .o and .LOG files
PURGE=TRUE
#                                       Replace old .EXE in $LOAD.
REPLACE=TRUE
#                                       Remove symbol tables to save
#                                       space.
STRIP=FALSE
#                                       Save old copy of .EXE first and
#                                       then delete it; avoid bus errors
#                                       this way on running tasks.
SAVE=TRUE
#                                       End

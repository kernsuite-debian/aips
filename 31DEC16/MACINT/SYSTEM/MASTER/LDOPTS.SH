#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2000, 2003, 2005-2006
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
# Usage: . LDOPTS.SH
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
# Mac INTEL version
# --------------------------------------------------------------------
#                                       Name of Fortran command; use it
#                                       instead of ld; it's simpler.
#                            GNU Fortran
# COMPILER=/sw/bin/g77
#                            New Intel Fortran
#  COMPILER=ifort
# Original: COMPILER=/opt/intel/fc/9.1.018/bin/ifort
# Changed by install.pl on 2015.11.30:09:49:16
COMPILER=/opt/intel/fc/9.1.024/bin/ifort
#                                       Define AIPS-style options.
#                                       Debug and no-optimise flags.
DEBUG="-g"
NOOPT=""
#                                       Not used, but still needed :-(
DIRTY=""
#                                       f77/ld option for a link map.
MAP="-M"
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
PROFILE="-p"
#                                       Link options, usually none.
LINK=" "
#
case $COMPILER in
#                                       GNU g77
    *g77)
        LINK="-L/usr/lib -lSystemStubs"
        ;;
#                                       IBM compiler
    *xlf)
#       COMPILER=/opt/ibmcmp/xlf/8.1/bin/xlf
#                                       Link options, usually none.
#                                       For G5 chip
#       LINK="-O3 -qarch=ppc970 -qtune=ppc970"
        ;;
#                                       New Intel compiler
    *ifort*)
        IOPT="-lsvml -Vaxlib -L/usr/local/lib"
        LINK="$IOPT -O3 -static-libcxa -save -align all -axP"
        ;;
#                                       Some other compiler.  If f2c,
#                                       set some sensible options.
    *)
        ;;
esac
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

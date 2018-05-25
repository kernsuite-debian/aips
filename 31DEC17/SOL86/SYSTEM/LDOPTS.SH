#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2000
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
# SUN version.  (SunOS 5+)
#--------------------------------------------------------------------
# Set compiler to g77 or pgf77 (Portland Group compiler) here. If
# using the Portland Group compiler with gcc and you have enabled
# large file support then you will also need to change LINK below.

COMPILER="/usr/local/bin/g77"

case "$COMPILER" in

*pgf77)  # Portland Group compiler

         COMPILER="$COMPILER -Kieee"
         ;;

*g77)    # GNU compiler
         ;;

*)       # Unknown compiler

         echo "LDOPTS is no configured for $COMPILER"
         ;;
esac

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
#                                       Need socket and nsl libraries
#                                       for Berkeley sockets/networking.
#                                       Use generic target unless
#                                       building for a homogeneous set
#                                       of Sparc systems.
#                                       MUST BE CONSISTENT between this,
#                                       CCOPTS.SH, and FDEFAULT.SH
#                                       (last in $SYSUNIX).
LINK="-lsocket -lnsl"

# IMPORTANT
#
# If you are using gcc as your C compiler and are using a FORTRAN
# compiler other than g77 then you must uncomment the following
# line and change it to reflect your local gcc installation

# LINK="$LINK -L/usr/local/lib/gcc-lib/intel-sun-solaris-2.8/2.95.2 -lgcc"

#                                       AIPS style options.
#                                       LIST not used but still needed
LIST=FALSE
#                                       Delete any .o and .LOG files
PURGE=FALSE
#                                       Replace old .EXE in $LOAD.
REPLACE=TRUE
#                                       Remove symbol tables to save
#                                       space.
STRIP=TRUE
#                                       Save old copy of .EXE first and
#                                       then delete it; avoid bus errors
#                                       this way on running tasks.
SAVE=TRUE
#                                       End

#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1998, 2001
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
#                                       Name of Fortran command; use it
#                                       instead of ld; it's simpler.
COMPILER="/opt/SUNWspro/bin/f77"
#                                       Some AOC systems have SC4 here
[ "$SITE" = "NRAOAOC" -a `uname -r` = "5.4" ] && \
 COMPILER=/opt/alpha/SUNWspro/bin/f77
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
#                                       of Sparc systems.  Also include
#                                       the fast math libraries.
#                                       These settings are for the Sparc
#                                       Compiler version 4.x; they may
#                                       not all be available in older
#                                       compilers.  Check the f77 manual
#                                       page in /opt/SUNWspro/man/f77.1
#                                       if you are in any doubt.
#                                       MUST BE CONSISTENT between this,
#                                       CCOPTS.SH, and FDEFAULT.SH
#                                       (last in $SYSUNIX).
LINK="-lsocket -lnsl -xtarget=generic -xlibmopt"

#                                       Some sites don't have SC4
if [ "$SITE" = "NFRA" ] ; then
    LINK="-lsocket -lnsl -dalign"
elif [ "$SITE" = "VSOP" ] ; then
    LINK="-lsocket -lnsl -dalign"
    LINK="$LINK -L/local/lib/gcc-lib/sparc-sun-solaris2.6/2.7.2.3/ -lgcc"
fi
#                                       Solaris shared libraries need
#                                       this option to prevent the
#                                       compiler from giving fatal
#                                       errors with shared libraries
#                                       when unused and unresolved
#                                       references occur.  This should
#                                       no longer happen... Note AIPS
#                                       shared libs are not recommended!
#                                       Need readline stuff for all
#                                       areas if using shared libs.
[ -f $SYSLOCAL/USESHARED ] && \
 LINK="$LINK -Qoption ld -znodefs -ltermcap"

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

#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1997, 1999
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
# Bourne shell commands is used to define the environment variables
# used by $SYSUNIX/LINK.  These include 1) command name for the host
# Fortran compiler (e.g., f77), 2) compiler/loader oriented AIPS-
# style options in terms of the host compiler/loader syntax (e.g.,
# DEBUG="-g"), and 3) locally desired defaults for AIPS-style options;
# (e.g., LIST=FALSE).
#
# NOTE: These are options for linking ONLY (see definition of
#       the environment variable 'LINK' below).  Options applicable
#       to compiling are defined in the $SYSLOCAL files ASOPTS.SH and
#       CCOPTS.SH (for assembler and 'C', respectively) and in
#       $SYSUNIX/FDEFAULT.SH and OPTIMIZE.LIS (Fortran).
#
# SUN version.
#--------------------------------------------------------------------

# Fortran command.  The syntax for this command is usually simpler
# than the loader (ld).
# SunOS: This can vary, but it is usually in /usr/lang.  YMMV.

COMPILER="/usr/lang/f77"

# AIPS-style options
#                                       Debug flags
DEBUG="-g"
NOOPT=""
#                                       Deprecated from VMS days
DIRTY=""
#                                       Generate a link map.
MAP="-M"
#                                       Various optimization levels
#                                       Deprecated for LINK.
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
#                                       Profiling.
PROFILE="-p"

# Default loader options.
# SunOS 4.1.2 or later, Sun Fortran SC2.0.1 or later, NOT a Sun 4/1xx or 2xx:
LINK="-cg89 -native"

[ "$SITE" = "NRAOCV" ] && LINK=""

[ -f $SYSLOCAL/USESHARED ] && LINK="$LINK -ltermcap"
[ "$SITE" = "NRAOVLA" ] && LINK="$LINK -cg89 -native"

#Set default values for other (i.e., non-compiler/loader oriented)
#options.  These are purely a matter of preference, however the
#default settings exported with the UNIX/AIPS installation kits
#seem to make the most sense.

#                                       Generate source code listing
#                                       (this has no meaning for the
#                                       link process).
LIST=FALSE
#                                       Purge, that is, delete any .o
#                                       and .LOG files associated with
#                                       the module after a sucessful
#                                       link.
PURGE=TRUE
#                                       Replace the resulting
#                                       executable module in $LOAD.
REPLACE=TRUE
#                                       Save an old version of it to
#                                       avoid bus errors on replacement
SAVE=TRUE
#                                       Remove symbol tables to save
#                                       space.
STRIP=TRUE
#                                       End

#-----------------------------------------------------------------------
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
#  Usage: . LDOPTS.SH
#-----------------------------------------------------------------------
#  Bourne shell commands is used to define the environment variables
#  used by $SYSUNIX/LINK.  These include 1) command name for the host
#  Fortran compiler (e.g., f77), 2) compiler/loader oriented AIPS-
#  style options in terms of the host compiler/loader syntax (e.g.,
#  DEBUG="-g"), and 3) locally desired defaults for the non-compiler/
#  loader oriented AIPS-style options (e.g., LIST=FALSE).  Both
#  compiler/loader and non-compiler/loader oriented options can be
#  toggled in $SYSUNIX/LINK by the use of 'option' (e.g., DEBUG) or
#  'NOoption' (e.g., NOMAP) command line arguments to $SYSUNIX/LINK
#  or $SYSUNIX/COMLNK which invokes $SYSUNIX/LINK.
#
#  NOTE: These are options for linking ONLY (see definition of
#        the environment variable 'LINK' below).  Options applicable
#        to compiling are defined in the $SYSLOCAL files ASOPTS.SH,
#        CCOPTS.SH and FCOPTS.SH (for assembler, 'C' and Fortran
#        compilations, respectively).
#
# Decstation Mips/Ultrix version.
#-----------------------------------------------------------------------

# Define the host Fortran compiler command name (e.g., f77, fort, fc,
# etc.).  Use the compiler for linking instead of the loader (e.g.,
# 'ld').  Using the loader directly is usually a lot harder.

COMPILER="f77"

# Define the LINK AIPS-style compiler oriented options in terms of the
# host Fortran compiler command syntax.

#                                       Any loader option(s) that may
#                                       enhance or may be required for
#                                       the usage of host debuggers.
DEBUG="-g"
NOOPT=""
#                                       Host compiler option(s) for
#                                       defaulting variable/constant
#                                       declarations (this has no
#                                       meaning for the link process).
DIRTY=""
#                                       Host loader option to generate
#                                       a link map.
MAP=""
#                                       Host compiler option(s) for
#                                       various optimization levels
#                                       (this has no meaning for the
#                                       link process.
OPT0="-O0"
OPT1="-O1"
OPT2="-O2"
OPT3=""
OPT4=""
OPT5=""
OPT6=""
OPT7=""
OPT8=""
OPT9=""
#                                       Host loader option for
#                                       execution profiling.
PROFILE=""

# Set default loader oriented options applicable to all links.
# These can be either the environment variables defined above or
# hard coded host loader options.

LINK=""

# Set default values for other (i.e., non-compiler/loader oriented)
# options.  These are purely a matter of preference, however the
# default settings exported with the UNIX/AIPS installation kits
# seem to make the most sense.

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
#                                       Strip symbol table to save
#                                       space
STRIP=TRUE
#                                       Save old copy of .EXE first and
#                                       then delete it; avoid bus errors
#                                       this way on running tasks.
SAVE=TRUE
#                                       End

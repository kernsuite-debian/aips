# ASOPTS.SH
#-----------------------------------------------------------------------
#;  Copyright (C) 1995
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
#-----------------------------------------------------------------------
# This file sets values for environment variables used by $SYSUNIX/AS.
# These include the command name for the host assembler, and its options 
# as well as locally desired defaults for AIPS options such as PURGE.
# Both AIPS and assembler options can be negated via "NO", e.g. NOPURGE,
# or NOOPT0.
#
# NOTE: These are options for assembling ONLY (see definition of
#       the environment variable 'COMP' below).  Options applicable
#       to the loader are defined in $SYSLOCAL/LDOPTS.SH.
#
# Generic Unix version.  Most implementations will not need this file.
#-----------------------------------------------------------------------

# Define AIPS assembler command as the host assembler command (e.g., as).

ASSEMBLER="as"

# Define AIPS assembler options in terms of the local syntax
#
#                                       Any options that may enhance
#                                       or may be required for the
#                                       usage of system debuggers.
DEBUG=""
#                                       Any of a wide variety of
#                                       optimization levels available
#                                       for UNIX assemblers and
#                                       combinations thereof.
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

# Set default assembler oriented options .  Always specifiy the local
# assembler option for assemble ONLY.  Also specify any options you want
# to apply to all assemblies either using the environment variables
# defined above or hard coded local assembler option syntax.

COMP=""

# Set default values for other (i.e., non-assembler oriented) options
# recognized by AS.  These are purely a matter of preference, however
# the default settings exported with the UNIX/AIPS installation kits
# seem to make the most sense.
#                                       Generate source code listing?
LIST=FALSE
#                                       Purge, that is, delete any .s
#                                       and .LOG files associated with
#                                       a successful assembly and,
#                                       if LIST=FALSE, any extant .LIS
#                                       file associated with the
#                                       program/subroutine being
#                                       assembled (i.e., keep the AIPS
#                                       source code directories free
#                                       of trash files).
PURGE=TRUE

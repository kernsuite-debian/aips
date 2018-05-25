# CCOPTS.SH
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1998
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
# CCOPTS.SH
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
# NRAO-CV IBM RS/6000 (AIX 3.1/3.2) version
#--------------------------------------------------------------------
# For AIX, cc (BSD based) was traditionally used, but xlc may be
# necessary if large files are ever to be supported.  Several APLIBM
# modules will fail (e.g. ZCREA2) if you try, though.
# Also, it's best to leave the PSALLOC code unchanged as the compiler
# under 3.2.5 cannot even compile the "hello world" program if PSALLOC
# is set to "early".

COMPILER="cc"
# COMPILER="xlc"
#                                       as AIPSCC is called as a
#                                       sub-shell, this should not
#                                       affect the parent process.
if [ "$PSALLOC" = "early" ] ; then
   PSALLOC=""; export PSALLOC
fi

# Define AIPSCC compiler oriented options in terms of the host C
# compiler command syntax.

#                                       Any options that may enhance
#                                       or may be required for the
#                                       usage of system debuggers.
DEBUG="-g"
#                                       Any of a wide variety of
#                                       optimization levels available
#                                       for UNIX C compilers and
#                                       combinations thereof.
OPT0="-O"
OPT1=""
OPT2=""
OPT3=""
OPT4=""
OPT5=""
OPT6=""
OPT7=""
OPT8=""
OPT9=""

COMP="-c -DBSD -I$INC"
# COMP="-c -I$INC"

# Set default values for other (i.e., non-compiler oriented) options
# recognized by AIPSCC.  These are a matter of preference, however
# the default settings exported with the UNIX/AIPS installation kits
# seem to make the most sense.

#                                       Generate source code listing?
LIST=FALSE
#                                       Purge, that is, delete any .c
#                                       and .LOG files associated with
#                                       a successful compilation and,
#                                       if LIST=FALSE, any extant .LIS
#                                       file associated with the
#                                       program/subroutine being
#                                       compiled (i.e., keep the AIPS
#                                       source code directories free
#                                       of trash files).
PURGE=TRUE

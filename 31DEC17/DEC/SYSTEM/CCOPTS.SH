# CCOPTS.SH
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
#--------------------------------------------------------------------
# This script is used to assign definitions to the environment
# variables used by $SYSUNIX/AIPSCC.  These include 1) the name of the
# command name for the host C compiler, 2) the compiler
# oriented $SYSUNIX/AIPSCC options in terms of the host C compiler
# option syntax and the locally desired defaults for the other non-
# compiler oriented options of $SYSUNIX/AIPSCC.  Both compiler and non-
# compiler oriented options can be toggled by the use of 'option'
# (e.g. DEBUG) or 'NOoption' (e.g., NOPURGE) command line arguments
# to $SYSUNIX/AIPSCC or to any of the UNIX/AIPS programming shell
# scripts that invoke $SYSUNIX/AIPSCC (i.e., $SYSUNIX/COMRPL and
# $SYSUNIX/COMLNK).
#
# NOTE: These are options for compiling ONLY (see definition of
#       the environment variable 'COMP' below).  Options applicable
#       to the loader are defined in $SYSLOCAL/LDOPTS.SH.
#
# NRAO-CV Dec Ultrix (RISC) version
#--------------------------------------------------------------------

# Define AIPSCC compiler command name as the host C compiler command
# name (e.g., cc).

COMPILER="cc"

# Define AIPSCC compiler oriented options in terms of the host C
# compiler command syntax.

#                                       Max amount of debugging info
#                                       from the MIPS C compiler as
#                                       used in Ultrix 4.0 on DS3100
DEBUG="-g3"
#                                       Any of a wide variety of
#                                       optimization levels available
#                                       for UNIX C compilers and
#                                       combinations thereof.
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

# Set default compiler oriented options .  Always specifiy the local
# compiler option for compile ONLY.  Also specify any options you want
# to apply to all fortran compilations either using the environment
# variables defined above or hard coded local compiler option syntax
# The -I$INC is essential to enable the AIPSPROTO.H file to be included.

COMP=" -c -I$INC $DEBUG"

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

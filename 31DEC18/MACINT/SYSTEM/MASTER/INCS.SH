#--------------------------------------------------------------------
#;  Copyright (C) 2000, 2003, 2006
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
# Usage: . $SYSLOCAL/INCS.SH
#--------------------------------------------------------------------
# Bourne shell script to define the local include file search path.
# INCS.SH is invoked by the source code preprocessor (PP) prior to
# the execution of the program INCLUDE which processes VMS style
# include statements.  The search path for include files is
# installation dependent and consists of blank separated directory
# names or environment variables defined as such.
#
# MACPPC version.
#--------------------------------------------------------------------
#                                       Generic UNIX search path.
# STDINCS="$INCUNIX $INCNOT $INC"
#                                       But since $INCUNIX and
#                                       $INCNOT are currently empty,
#                                       speed up the search and skip
#                                       them.  Other directories can
#                                       be added as desired.
STDINCS="$INCLOC $INCMACIN $INC"
export STDINCS

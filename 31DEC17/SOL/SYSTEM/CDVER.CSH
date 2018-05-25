#!/bin/csh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1998, 2000-2001
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
# Usage: source CDVER.CSH
#-----------------------------------------------------------------------
# Define the AIPS programming environment variables (aka "logicals").
# The master definitions are in $SYSAIPS/AREAS.DAT, and $SYSUNIX/AREAS
# will generate $SYSUNIX/AREAS.SH and .CSH, usually via a midnight job.
# Normally, this script is only used when $CDOLD, $CDNEW or $CDTST is
# typed (by the programmer interactively, or in a login script).
# Definitions for local, and/or non-standard areas should be defined
# here, not in the other mentioned files.
#
# This script also toggles the search path (i.e., $PATH) to include the
# directories $SYSLOCAL and $SYSUNIX.
#
# Generic csh/tcsh version.
#-----------------------------------------------------------------------
#                                       Define/redefine standard names
source $AIPS_ROOT/AIPSPATH.CSH
#                                       Now get the programming areas.
source $SYSUNIX/AREAS.CSH
setenv UPDATE $AIPS_VERSION/$ARCH/UPDATE
#                                       Display AIPS version.
if ($?USER != 0 && $?prompt != 0) then
   echo AIPS_VERSION=$AIPS_VERSION
endif

#!/bin/csh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1999-2001, 2003, 2006
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
#--------------------------------------------------------------------
# This file is used to establish the AIPS programming area logicals
# (environment variables) as defined in $SYSUNIX/AREAS.CSH (created
# from $SYSAIPS/AREAS.DAT by the $SYSUNIX/AREAS script) or toggle
# these definitions between different AIPS versions (i.e., OLD, NEW
# and TST).  Normally, it is only used by the environment variables
# $CDOLD, $CDNEW and $CDTST.  Area logicals for local directories
# should be defined here, not in $SYSUNIX/AREAS.CSH.  This includes
# any directories containing local Q, Y or Z routines.  It also
# toggles the search path for executables (i.e., $PATH) to include
# the directories $SYSLOCAL and $SYSUNIX.
#
# Generic C shell version in MAC intel area
#--------------------------------------------------------------------
#                                       Define/redefine standard AIPS
#                                       programming logicals.
source $AIPS_ROOT/AIPSPATH.CSH
source $SYSUNIX/AREAS.CSH
setenv UPDATE $AIPS_VERSION/$ARCH/UPDATE
#                                       Define any local area logicals
#                                       here (e.g., Q, Y or Z-routine
#                                       directories).  For example,
#
#                                       setenv QLOCAL $QPSAP/LOCAL
#                                       setenv YLOCAL $YIVAS/LOCAL
#                                       setenv ZLOCAL $APLSUN/LOCAL
#
#                                       Be sure to include your local
#                                       area logicals in your
#                                       $SYSLOCAL/LIBR.DAT directory
#                                       search paths.
#
#                                       Delete the following line
#                                       after establishing your local
#                                       area logicals.
# echo "Define local area logicals in $SYSLOCAL/CDVER.CSH"
#                                       Display AIPS version.
echo AIPS_VERSION=$AIPS_VERSION

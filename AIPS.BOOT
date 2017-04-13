#!/bin/sh
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
# ----------------------------------------------------------------------
# Usage: AIPS.BOOT
# ----------------------------------------------------------------------
# This procedure can be called at bootup time to start various daemons
# used by AIPS, perform cleanup, and so on.  You should either make this
# script set-uid to the aips account (if you have one) or make the
# START_TPSERVERS script set-uid to aips.
#
# As shipped, it will only start TPMON daemons on systems that have
# actual tape drives defined in
#
# Any additional boot-time aips-related chores can be put here.
#
# This version should be copied to the AIPS_ROOT area on installation.
# ----------------------------------------------------------------------
#
#                                       Change for your setup
AIPS_ROOT=/opt/aips
export AIPS_ROOT
#                                       Sanity check
if [ ! -d $AIPS_ROOT/ ] ; then
  echo "Edit AIPS.BOOT to define AIPS_ROOT"
  exit 2
fi
#
. $AIPS_ROOT/LOGIN.SH
#
# If we're an AIPS tape host, start up the TPMONs
#
xxx=`grep "^$HOST" $AIPS_ROOT/DA00/TPDEVS.LIST`
#
#                                       Was anything found?
if [ "X$xxx" != "X" ]; then
   if [ -f $AIPS_ROOT/START_TPSERVERS ] ; then
#                                       Do things quietly
      $AIPS_ROOT/START_TPSERVERS
   else
      echo "AIPS.BOOT: cannot find START_TPSERVERS in $AIPS_ROOT"
      exit 2
   fi
fi
#                                       Put any other routine boot-
#                                       time tasks here.
#
#                                       End of AIPS.BOOT script

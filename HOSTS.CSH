#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 2003
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
# Usage: source HOSTS.CSH
#-----------------------------------------------------------------------
# This file gets particulars about the network setup for AIPS from the
# HOSTS.LIST file.  Do not change this file; edit the HOSTS.LIST
# instead.
# The following environment variables identify each AIPS host or TV
# server in the local network.
#
#    HOST    Name of the machine (uppercase).
#
#    SITE    The local site name (uppercase).
#
#    HOSTS   List of all hosts which run AIPS at this site.
#
#    ARCH    Differentiates executables and libraries for different
#            machine architectures.
#
#-----------------------------------------------------------------------
#                                       Set the AIPS root directory.
setenv AIPS_ROOT /opt/aips
#
# do not change anything below this line
#
#                                       Find the hosts file
if (! -d $AIPS_ROOT) then
  echo 'AIPS_ROOT is not correctly defined in the HOSTS.CSH script.'
  echo 'You need to run the AIPSROOT.DEFINE script (or get help).'
  exit 2
endif
#                                       Find the hosts file
if (! -f $AIPS_ROOT/HOSTS.LIST) then
  echo "HOSTS: Cannot find HOSTS.LIST in $AIPS_ROOT"
  echo 'HOSTS: Cannot proceed.  Help!'
  exit 2
endif
#                                       Set variable HOST.  Strip off
#                                       any domain name, use simple
#                                       hostname.  Also uppercase it.
setenv HOST `$AIPS_ROOT/SETUNAME`
#                                       Now look for this in HOSTS.LIST
set xxx = `grep "^[-+]  $HOST" $AIPS_ROOT/HOSTS.LIST`
#
if ( "$xxx" == "" ) then
  setenv ARCH "UNKNOWN"
  setenv SITE "UNKNOWN"
else
  setenv ARCH  $xxx[3]
  setenv SITE  $xxx[4]
  set xxx = `grep "^+.*$SITE" $AIPS_ROOT/HOSTS.LIST | awk '{ print $2 }'`
  setenv HOSTS "$xxx"
endif
unset xxx
exit 0

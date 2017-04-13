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
# Usage: . HOSTS.SH
#-----------------------------------------------------------------------
# This file gets particulars about the network setup for AIPS from the
# HOSTS.LIST file.  Do not change this file; edit the HOSTS.LIST
# instead.
# The following environment variables identify each AIPS host or TV
# server in the local network.
#
#    HOST    Name of the machine, uppercase.
#
#    SITE    The local site name, uppercase.
#
#    HOSTS   List of all hosts which run AIPS at this site.
#
#    ARCH    Differentiates executables and libraries for different
#            machine architectures.
#
#-----------------------------------------------------------------------
#                                       Set the AIPS root directory.
AIPS_ROOT=/DEFINE_ME
#
# do not change anything below this line
#
export AIPS_ROOT
if [ ! -d $AIPS_ROOT ] ; then
  echo 'AIPS_ROOT is not correctly defined in the HOSTS.SH script.'
  echo 'You need to run the AIPSROOT.DEFINE script (or get help).'
  exit 2
fi
#                                       Find the hosts file
if [ ! -f $AIPS_ROOT/HOSTS.LIST ] ; then
  echo "HOSTS: Cannot find HOSTS.LIST in $AIPS_ROOT"
  echo 'HOSTS: Cannot proceed.  Help!'
  exit 2
fi
#                                       Set variable HOST.  Strip off
#                                       any domain name, use simple
#                                       hostname.  Also uppercase it.
HOST=`$AIPS_ROOT/SETUNAME`
export HOST
#                                       Now look for this in HOSTS.LIST
xxx=`grep "^[-+]  $HOST" $AIPS_ROOT/HOSTS.LIST`
#
if [ "$xxx" = "" ] ; then
  ARCH="UNKNOWN"; export ARCH
  SITE="UNKNOWN"; export SITE
else
  ARCH=`echo $xxx | awk '{ print $3}'`; export ARCH
  SITE=`echo $xxx | awk '{ print $4}'`; export SITE
  xxx=`grep "^+.*$SITE" $AIPS_ROOT/HOSTS.LIST | awk '{ printf "%s ", $2 }'`
  HOSTS="$xxx"; export HOSTS
fi

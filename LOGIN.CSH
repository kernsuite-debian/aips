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
# Usage: source LOGIN.CSH
#-----------------------------------------------------------------------
# Define where the AIPS shell scripts are.  This is usually the aips
# account's login area.  Set the "AIPS_ROOT" string to the correct area.
# This script usually for AIPS installers and programmers only.
#-----------------------------------------------------------------------
setenv AIPS_ROOT /opt/aips
#  set to "YES" for local installation on portable computer
#Changed [install.pl] to yes
setenv LAPTOP "YES"
#
# Do not change anything below this line
#
if ( -d $AIPS_ROOT ) then
  if ( -f $AIPS_ROOT/HOSTS.CSH ) then
    source $AIPS_ROOT/HOSTS.CSH
  else
    echo "Error: no HOSTS.CSH file found in $AIPS_ROOT"
    echo "       Cannot proceed!"
    exit 2
  endif
  if ( -f $AIPS_ROOT/AIPSPATH.CSH ) then
    source $AIPS_ROOT/AIPSPATH.CSH
    if ( -f $AIPS_ROOT/AIPSASSN.CSH ) then
      source $AIPS_ROOT/AIPSASSN.CSH
    else
      echo "Warning: AIPSASSN.CSH not found in $AIPS_ROOT"
      echo "         Some or all of AIPS may not work"
    endif
  else
    echo "Error: AIPSPATH.CSH not found in $AIPS_ROOT"
    echo "       AIPS will not work"
  endif
else
  echo "Error: $AIPS_ROOT (\$AIPS_ROOT) is not a directory"
  echo "       (Check LOGIN.CSH and change the \$AIPS_ROOT definition there)"
endif

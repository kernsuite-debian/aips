#!/bin/bash
#-----------------------------------------------------------------------
#;  Copyright (C) 2015
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
#  Uses sudo to make links to AIPS Intel libraries and X11 libraries
#  in /usr/local/lib for MAC OS 10.11 (El Capitan) which no longer
#  allows DYLD_LIBRARY_PATH

if [ -z "${AIPS_VERSION}" ]; then
    echo "AIPS env not found - did you source your AIPS LOGIN.SH first?" >&2
    exit 1
fi

cat << EOH

This script will attempt to configure an existing AIPS installation
to work properly under OS X 10.11 "El Capitan". You will be prompted
for your password so that several commands can be run as root.

Press enter to continue...
EOH
read foo

if ! sudo mkdir -p /usr/local/lib ; then
    echo "Unable to create /usr/local/lib - do you have admin privileges?" >&2
    exit 1
fi

numlibs=0
if [ -d "${AIPS_VERSION}/MACINT" ]; then
    for lib in "${AIPS_VERSION}/"MACINT/LIBR/INTELCMP/*.dylib; do
        if sudo ln -s "$lib" "/usr/local/lib/$(basename $lib)" ; then
	    numlibs=$((numlibs+1))
	fi
    done
else
    echo "ERROR: AIPS for MACINT not found at $AIPS_VERSION" >&2
    exit 1
fi
if sudo ln -s "/usr/X11/lib/libX11.6.dylib" "/usr/local/lib/libX11.6.dylib)" ; then
    numlibs=$((numlibs+1))
fi
if sudo ln -s "/usr/X11/lib/libXext.6.dylib" "/usr/local/lib/libXext.6.dylib)" ; then
    numlibs=$((numlibs+1))
fi

if [ $numlibs -eq 5 ]; then
    cat << EOS

Three shared and two XWindows libraries were linked to
/usr/local/lib. Your installation should now be fixed
to operate under OS X 10.11.

EOS
else
    cat << EOF

$numlibs libraries were linked to /usr/local/lib instead of
the expected five. Something may have gone wrong, or your
installation may have already been fixed.

EOF
fi

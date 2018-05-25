#!/bin/bash
#-----------------------------------------------------------------------
#;  Copyright (C) 2015-2016
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

if [ "$USER" = 'root' ]; then
    echo "Please run this script as your normal (admin) user - "
    echo "do not use 'sudo'"
    exit 1
fi
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
   if sudo ln -sf "${AIPS_VERSION}/MACINT/LIBR/INTELCMP/libsvml.dylib" "/usr/local/lib/libsvml.dylib" ; then
       numlibs=$((numlibs+1))
   fi
   if sudo ln -sf "${AIPS_VERSION}/MACINT/LIBR/INTELCMP/libimf.dylib" "/usr/local/lib/libimf.dylib" ; then
       numlibs=$((numlibs+1))
   fi
   if sudo ln -sf "${AIPS_VERSION}/MACINT/LIBR/INTELCMP/libintlc.dylib" "/usr/local/lib/libintlc.dylib" ; then
       numlibs=$((numlibs+1))
   fi
   if sudo ln -sf "${AIPS_VERSION}/MACINT/LIBR/INTELCMP/libirc.dylib" "/usr/local/lib/libirc.dylib" ; then
       numlibs=$((numlibs+1))
   fi
#    for lib in "${AIPS_VERSION}/"MACINT/LIBR/INTELCMP/*.dylib; do
#        if sudo ln -sf "$lib" "/usr/local/lib/$(basename $lib)" ; then
#	    numlibs=$((numlibs+1))
#	fi
#    done
else
    echo "ERROR: AIPS for MACINT not found at $AIPS_VERSION" >&2
    exit 1
fi

#               if wrong Xquartz try
#if sudo ln -s "/usr/X11/lib/libX11.6.dylib" "/usr/local/lib/libX11.6.dylib" ; then
#    numlibs=$((numlibs+1))
#fi
#if sudo ln -s "/usr/X11/lib/libXext.6.dylib" "/usr/local/lib/libXext.6.dylib" ; then
#    numlibs=$((numlibs+1))
#fi

xversion=$(defaults read /Applications/Utilities/XQuartz.app/Contents/Info.plist CFBundleShortVersionString)
if [ $? -ne 0 ] ; then
    echo "Error reading XQuartz version. Make sure that XQuartz 2.7.8 or greater is installed" >&2
    exit 1
fi

compver=$( echo $xversion | awk -F. '{ printf("%d%03d%03d", $1,$2,$3); }' )
if [ $compver -lt 2007008 ] ; then
    echo "Error: XQuartz $compver is too old. Upgrade to 2.7.8 or later!" >&2
    exit 1
else
    # Ensure X11 and X11R6 links are present in /usr
    sudo /usr/libexec/x11-select /opt/X11
fi

if [ $numlibs -eq 4 ]; then
    cat << EOS

Four AIPS shared libraries were linked to /usr/local/lib.
Your installation should now be fixed to operate under OS X 10.11.

EOS
else
    cat << EOF

$numlibs libraries were linked to /usr/local/lib instead of
the expected four Something may have gone wrong, or your
installation may have already been fixed.

EOF
    exit 1
fi

exit 0

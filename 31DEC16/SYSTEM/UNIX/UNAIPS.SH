#!/bin/echo you_need_to_dot_this_file,_butthead
#-----------------------------------------------------------------------
#;  Copyright (C) 1999
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
#
# unaips - undefine the AIPS environment.  Useful for installs, etc.
#
if [ "$AIPS_ROOT" = "" ] ; then
  echo "AIPS_ROOT not defined, apparently no AIPS environment to undefine"
elif [ "$SYSUNIX" = "" ] ; then
  echo "SYSUNIX not defined, apparently no AIPS environment to undefine"
else
  newpath=""
  for i in `echo $PATH | sed -e 's/:/ /g'` ; do
    case $i in
      $AIPS_ROOT/*) ;;
      *) if [ "$newpath" = "" ] ; then
           newpath=$i
         else
           newpath="$newpath:$i"
         fi;;
    esac
  done
  PATH=$newpath; export PATH
  newpath=""
  for i in `echo $MANPATH | sed -e 's/:/ /g'` ; do
    case $i in
      $AIPS_ROOT/*) ;;
      *) if [ "$newpath" = "" ] ; then
           newpath=$i
         else
           newpath="$newpath:$i"
         fi;;
    esac
  done
  MANPATH=$newpath
  `cat $AIPS_VERSION/SYSTEM/AREAS.DAT | grep -v ^\! | \
   awk '{printf "unset %s\n", $1}'`
  `cat $AIPS_ROOT/AIPSASSN.SH | grep '^ *[A-Z0-9]*=' | \
   awk -F= '{printf "unset %s\n", $1}'`
  unset CDTST CDNEW CDOLD CDCVX LOAD SYSLOCAL TPATH HAVE_PERL AIPS_VERSION
  unset VERSION TST NEW OLD CVX ARCH HOSTS AIPS_ROOT HOST SITE
  unset DATA_ROOT BATCH_OUT SYSUNIX
fi


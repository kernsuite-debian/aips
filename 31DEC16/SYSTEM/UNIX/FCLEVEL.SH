#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-2000, 2003-2004
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
# FCLEVEL.SH
# --------------------------------------------------------------------
# Determines optimization and debug level for an input file
# Input Variables are:
#    FILE    - file name to find optimization
#    AREA    - directory name to find optimization
# Assumes file $SYSUNIX/OPTIMIZE.LIS exists
# Output Variables are:
#    OPTDEF  - Optimization for this file
#    DBGDEF  - Debug level  for this file
# this script must be executed in the following manner
#  OPTHIS=`(. FCLEVEL.SH)`
# then the $OPTHIS variable must be used for all further processing
# Generic version
#--------------------------------------------------------------------
DBGDEF=""
OPTDEF=""
OPTDIR=$SYSUNIX
#                                       Read in local default options ?
[ -f $SYSLOCAL/OPTIMIZE.LIS ] && \
   OPTDIR=$SYSLOCAL
#                                       For each non comment line:
grep -v '^#' $OPTDIR/OPTIMIZE.LIS | \
{
   while read line ; do
#                                       General way of getting params
      set - $line
#                                       Skip blank lines
      [ ! -n "$line" ] && continue
#                                       translate file or logical name
      LOGNAME=`eval echo $1`
      if [ -n "$LOGNAME" ] ; then
         case $LOGNAME in
#                                       default, directory or file?
         DEFAULT | $AREA | $FILE*)
#                                       if the correct Architecture
            if [ \( "$2" = "$ARCH" \) \
              -o \( "$2" = "ALL" \) ] ; then
#                                       decode the optimization
               OPTDEF=\$OPT$3
               OPTDEF=`eval echo $OPTDEF`
#                                       decode the debug level
               if [ $4 -eq 1 ] ; then
                  DBGDEF=$DEBUG
               else
                  DBGDEF=$NODEBUG
               fi
#                                        send result to upper level
               echo $OPTDEF $DBGDEF
               exit 0
#                                        end if correct architecture
            fi
         ;;
         esac
      fi
   done
}
echo $OPTDEF $DBGDEF

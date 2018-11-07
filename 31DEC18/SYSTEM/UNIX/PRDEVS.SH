#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 2004
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
#-----------------------------------------------------------------------
# PRDEVS.SH -- Define AIPS Printer devices
#-----------------------------------------------------------------------
# This file will do one of two things.  If LPDEV is set to a numeric
# value, it sets the AIPS printer variables LPDEVn, LPDESCn, LPOPTn, and
# LPTYPEn to the appropriate values for n (if it is in range).
# Otherwise, it sets all variables as with older AIPS versions for
# backwards compatibility.
#
# You should NOT need to modify this file.  If there's something wrong
# with it, let us know (email to aipsmail@nrao.edu is best).
#
# The information on the printers is now in the PRDEVS.LIST file in the
# $AIPS_ROOT/DA00 directory.  See the comments therein for details.
#-----------------------------------------------------------------------
if [ "$AIPS_ROOT" = "" ] ; then
  echo "PRDEVS: cannot proceed, AIPS_ROOT not defined"
elif [ ! -f $AIPS_ROOT/DA00/PRDEVS.LIST ] ; then
  echo "PRDEVS: cannot proceed, $AIPS_ROOT/DA00/PRDEVS.LIST not found"
else
#                          Set filename, no. of printers
  PRF=$AIPS_ROOT/DA00/PRDEVS.LIST
  LPDEVS=`grep '^[^# ]' $PRF | wc -l`
  LPDEVS=`eval echo $LPDEVS`
  if [ "$LPDEVS" = "0" ] ; then
    echo "PRDEVS: cannot proceed, no printers found in $PRF"
  elif [ "$LPDEV" != "" ] ; then
    case $LPDEV in
#                          Printer number selected.
      [0-9]*)
        if [ \( $LPDEV -ge 1 \) -a \( $LPDEV -le $LPDEVS \) ] ; then
          k=`grep '^[^# ]' $PRF | awk '(NR == '$LPDEV') {print $0}'`
          eval "LPDEV$LPDEV=`echo $k | awk '{print $1}'`"
          export LPDEV$LPDEV
          eval "LPTYPE$LPDEV=`echo $k | awk '{print $2}'`"
          export LPTYPE$LPDEV
          eval "LPOPT$LPDEV=`echo $k | awk '{print $3}'`"
          export LPOPT$LPDEV
          desc=`echo $k | awk '{for (j=4; j<=NF; j++) printf "%s ", $j}'`
          eval "LPDESC$LPDEV=\"$desc\""; export LPDESC$LPDEV
        else
          echo "PRDEVS: Printer $LPDEV not in range 1-$LPDEVS"
        fi
      ;;
      *) echo "PRDEVS: unexpected value for LPDEV: \"$LPDEV\""
      ;;
    esac
  else
#                          Compatibility mode with old way.
#                          Just set everything in sight.
    i=1
    grep '^[^# ]' $PRF | awk '
    BEGIN {printf "#!/bin/sh\n"}
    {
      printf "LPDEV%d=%s; export LPDEV%d\n",NR,$1,NR;
      printf "LPTYPE%d=%s; export LPTYPE%d\n",NR,$2,NR;
      printf "LPOPT%d=\"%s\"; export LPOPT%d\n",NR,$3,NR;
      printf "LPDESC%d=\"",NR;
      for (j=4; j<=NF; j++) {
        printf "%s ", $j;
      }
      printf "\"; export LPDESC%d\n",NR;
    }
    $3 ~ /.*DEF.*/ { printf "LPDEV=LPDEV%d; export LPDEV\n",NR; }
    END { printf "LPDEVS=%d; export LPDEVS\n",NR}' >/tmp/AIPS.PRDEV.$$
    chmod +x /tmp/AIPS.PRDEV.$$
    . /tmp/AIPS.PRDEV.$$
    rm -f /tmp/AIPS.PRDEV.$$
  fi
fi
#                                       Check for BIG printer...
BIGPRINT=`grep '^[^# ]' $PRF | awk '($3 ~ /BIG=/) { print NR}' | head -n 1`
[ "$BIGPRINT" != "" ] && export BIGPRINT
#                          End of PRDEVS.SH

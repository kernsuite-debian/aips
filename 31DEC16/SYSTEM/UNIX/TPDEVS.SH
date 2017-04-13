#!/bin/sh
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1999, 2002
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
# Usage: . TPDEVS.SH
#-----------------------------------------------------------------------
# AIPS Tape drive definitions are obtained here.  Set TPDEVS_QUIET to
# stop output.  Otherwise for each tape drive, the descriptive field in
# TPDEVS.LIST is printed, e.g. "Tape 4 is HP 9-track on HOSTNAME".
#
# Environment variables required:
#
#    HOST    The host name of the machine to run AIPS.
#    NET0    Location of TPDEVS.LIST
#
# Environment variables returned:
#
#    TAPEx   The device name of AIPS tape drive x (hex, 1-F)
#    NAIPST  The total number of tape devices, including remote.
#
# The last (NAIPST) is not yet used by AIPS but could be read by ZDCHIN
# in a similar way to NVOL for disks, in the future.
#
# To add a new tape drive, or to change existing definitions, edit the
# $NET0/TPDEVS.LIST file.
#
#-----------------------------------------------------------------------
#                                       Process tape definitions if any.
#                                       If not, just set two remote.
tapeno=1; export tapeno
quiet=0; export quiet
[ "$TPDEVS_QUIET" != "" ] && quiet=1
[ $quiet -eq 0 ] && echo "Tape assignments: "
tmpfil=/tmp/TPDEVS_${HOST}_$$; export tmpfil
[ -f $tmpfil ] && rm -f $tmpfil
if [ -f $NET0/TPDEVS.LIST ] ; then
#                                       Need to scan file to get them.
   touch $tmpfil; chmod +x $tmpfil
   if [ $quiet -eq 1 ] ; then
      grep "^$HOST " $NET0/TPDEVS.LIST | \
         awk '{printf "TAPE%d=%s; export TAPE%d\n",NR,$2,NR}
              END {printf "TAPE%d=REMOTE; export TAPE%d\n",NR+1,NR+1
	           printf "TAPE%d=REMOTE; export TAPE%d\n",NR+2,NR+2
		   printf "NAIPST=%d; export NAIPST\n",NR+2
	          }
             ' >>$tmpfil
   else
      grep "^$HOST " $NET0/TPDEVS.LIST | \
         awk '{printf "TAPE%d=%s; export TAPE%d; echo \"   Tape %d is",NR,$2,NR,NR
               if (NF > 2) {
                  for (i = 3; i <= NF; i++)
                     printf " %s",$i
                  printf "\" on %s\n",$1
                  }
               else
                  printf "\n"
              }
              END {printf "TAPE%d=REMOTE; export TAPE%d\n",NR+1,NR+1
                   printf "echo \"   Tape %d is REMOTE\"\n",NR+1
                   printf "TAPE%d=REMOTE; export TAPE%d\n",NR+2,NR+2
		   printf "echo \"   Tape %d is REMOTE\"\n",NR+2
		   printf "NAIPST=%d; export NAIPST\n",NR+2
		  }
              ' >>$tmpfil
   fi
   [ -s $tmpfil ] && . $tmpfil
else
   TAPE1=REMOTE; export TAPE1
   [ $quiet -eq 0 ] && echo "   Tape 1 is REMOTE"
   TAPE2=REMOTE; export TAPE2
   [ $quiet -eq 0 ] && echo "   Tape 2 is REMOTE"
   NAIPST=2; export NAIPST
fi
#                                       End of TPDEVS.SH, clean up.
[ -f $tmpfil ] && rm -f $tmpfil
[ $quiet -eq 0 ] && echo " "
if [ "$AIPSTP" = "" ] ; then
   AIPSTP="DONE"; export AIPSTP
fi


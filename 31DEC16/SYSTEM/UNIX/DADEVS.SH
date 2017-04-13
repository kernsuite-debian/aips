#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 1998-1999, 2010
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
# Usage: . DADEVS.SH
#-----------------------------------------------------------------------
# AIPS data disk requests are serviced here.  This script can be very
# inefficient, and is a fallback for the DADEVS.PL perl script in case a
# given site does not have perl (perl is significantly faster).
#
# Environment variables given:
#
#    DAOPT         Colon separated list of AIPS hosts whose disks are to
#                  be allocated.  If DAOPT=ALL, all disks will be
#                  allocated.
#
#    DADEVS_QUIET  YES if you want to suppress the disk listing
#
# Environment variables returned:
#
#    NVOL          The total number of data disks (decimal) - overides
#                  the value set in the SP file
#
#    DA0x          Where x is an "extended" hexadecimal digit in the
#                  range 1-Z
#
# To add a new data disk, add its name to $NET0/DADEVS.LIST in order of
# priority.  Also add an entry to $NET0/NETSP.  Note: see below for the
# order in which list files are used.
#
# Users may control disk assignment via the "da=" option to the AIPS
# startup script.  By default, a user will be assigned all "required"
# disks, plus any attached to the local machine.  Required disks are
# indicated by a "+" in column 1 of DADEVS.LIST.  All other disk entries
# must have a "-" in column 1.
#
# However, if "da=default" is specified instead, the user will be
# assigned whatever disks are set in $AIPS_ROOT/AIPSASSN.CSH or
# $AIPS_ROOT/AIPSASSN.SH instead (depending on the user's shell).  Some
# sites may wish to customize this behaviour by making the string blank
# instead of "default".  Use of the "default" route speeds up the disk
# selection process (though DADEVS.PL is fast enough, if you have perl,
# to make this not matter).
#
# DADEVS.SH searches for list files in the following sequence:
#
#             $HOME/.dadevs.$SITE
#             $HOME/.dadevs
#             $DA00/DADEVS.LIST
#             $AIPS_ROOT/DA00/DADEVS.LIST.$SITE
#             $AIPS_ROOT/DA00/DADEVS.LIST
#
# Whichever file is found first is used EXCLUSIVE OF THE OTHERS.  The
# order in which the disks are listed is significant and is preserved in
# the final disk assignments.  Those with names matching the local host
# are included first, then those indicated as required in the list file,
# and finally any selected via the DA= option are included last.
#
# The first disk is special since it contains the AIPS message,
# save/get, and tput/tget files.  Some sites may choose to ensure that
# the same disk is always "disk 1" so that users have the same message
# files etc. wherever they run AIPS.  The downside of this is reduced
# performance in message writing if this first disk is NFS mounted.
#
# The only requirement for data disk names is that the actual directory
# name be of the form HOSTNAME_n where HOSTNAME must be the name of the
# host on which the disk resides, and n is a number.  The prefix is
# usually /DATA/ but need not be.  This is so the (presumably hostname
# based) selection criteria in DAOPT can be matched against the disks.
#
# All valid AIPS data disks must contain a file called SPACE, which is
# empty.  DADEVS.SH tests for the existence of this file to see if a
# disk is available to the system at the time that AIPS starts up.
#
# The file $AIPS_ROOT/DA00/NETSP has TIMDEST and booking information.
#-----------------------------------------------------------------------
#                                       Find the appropriate list file.
quiet=no
[ "$DADEVS_QUIET" = "YES" ] && quiet=yes
if [ -f "$HOME/.dadevs.$SITE" ] ; then
   LIST="$HOME/.dadevs.$SITE"
   DESC="private $SITE-specific"
elif [ -f "$HOME/.dadevs" ] ; then
   LIST="$HOME/.dadevs"
   DESC="private"
elif [ -f "$DA00/DADEVS.LIST" ] ; then
   LIST="$DA00/DADEVS.LIST"
   DESC="$HOST-specific"
elif [ -f "$NET0/DADEVS.LIST.$SITE" ] ; then
   LIST="$NET0/DADEVS.LIST.$SITE"
   DESC="$SITE-specific default"
elif [ -f "$NET0/DADEVS.LIST" ] ; then
   LIST="$NET0/DADEVS.LIST"
   DESC="global default"
else
   echo "DADEVS.SH error - could not find DADEVS/.dadevs list file."
   echo "This is a FATAL error and NOTHING in AIPS will work."
   LIST=/dev/null
fi
if [ "$LIST" != /dev/null ] ; then
   if [ "$quiet" = no ] ; then
      echo "  (Using $DESC file $LIST)"
   fi
fi
if [ -f "$HOME/.dadevs.always" ] ; then
   LLIST="$HOME/.dadevs.always"
else
   LLIST="/dev/null"
fi
#                                       Convert to uppercase, and
#                                       strip non-alphanumerics.
daopt=`echo $DAOPT | tr '[a-z]' '[A-Z]' | sed -e 's/[^A-Z0-9:]/:/g'`
#                                       Initialize number of volumes
nvol=0
echo "Using DADEVS.SH"
#                                       If options specified, check them
if [ "$daopt" != "DEFAULT" ] ; then
#                                       Process disk requests.
#                                       Solaris awk is brain dead.
   awk=awk
   case $ARCH in
      SUN*|S[OU]L*) awk=nawk;;
   esac
#                                       Faster to use awk than shell
#                                       features.  Accumulate three sets
#                                       of disks: required, local host,
#                                       and others, and define them in
#                                       that order.  4th list always
#                                       file
    ldefs=`egrep '^[-+]' $LLIST | $awk '
    BEGIN { ilus = 0;
          }
    $1 == "+" { ilus++; lus[ilus] = $2; next }
    END {
          for (i=1; i in lus; i++) {
             amatch=0
             for (j=1; j < i; j++) {
                if (lus[j] == lus[i]) {
                   amatch=1;
                }
             }
             for (j=1; j in req; j++) {
                if (lus[i] == req[j]) {
                   amatch=1
                }
                if (lus[i] == hst[j]) {
                   amatch=1
                }
             }
             if (amatch == 0) {
                print lus[i], " "
             }
          }
    }
   '`
    kdefs=`egrep '^[-+]' $LLIST | $awk '
    BEGIN { ikus = 0;
          }
    $1 == "-" { ikus++; kus[ikus] = $2; next }
    END {
          for (i=1; i in kus; i++) {
             amatch=0
             for (j=1; j < i; j++) {
                if (kus[j] == kus[i]) {
                   amatch=1;
                }
             }
             for (j=1; j in req; j++) {
                if (kus[i] == req[j]) {
                   amatch=1
                }
                if (kus[i] == hst[j]) {
                   amatch=1
                }
             }
             if (amatch == 0) {
                print kus[i], " "
             }
          }
    }
   '`
    defs=`egrep '^[-+]' $LIST | $awk '
    BEGIN { ireq = 0; ihost = 0;
            host = "'$HOST'";
          }
    $1 == "+" { ireq++; req[ireq] = $2; next }
    $2 ~ /'$HOST'_/ { ihost++; hst[ihost] = $2; next }
    $2 ~ /'$HOST'$/ { ihost++; hst[ihost] = $2; next }
    END { for (i=1; i in req; i++) {
             amatch=0;
             for (j=1; j < i; j++) {
                if (req[j] == req[i]) {
                   amatch=1;
                }
             }
             if (amatch == 0) {
                print req[i], " "
             }
          }
          for (i=1; i in hst; i++) {
             amatch=0;
             for (j=1; j < i; j++) {
                if (hst[j] == hst[i]) {
                   amatch=1;
                }
             }
             for (j=1; j in req; j++) {
                if (hst[i] == req[j]) {
                   amatch=1;
                }
             }
             if (amatch == 0) {
                print hst[i], " "
             }
          }
    }
   '`
    odefs=`egrep '^[-+]' $LIST | $awk '
    BEGIN { ioth = 0;
            daopt = "'$daopt'";
            split (daopt, arr, ":")
          }
    "'$daopt'" == "ALL" { ioth++; oth[ioth] = $2; next }
    {
       for (i in arr) {
          if ($2 ~ arr[i]) {
             ioth++; oth[ioth] = $2; next
          }
       }
    }
    END { for (i=1; i in oth; i++) {
             amatch=0
             for (j=1; j < i; j++) {
                if (oth[j] == oth[i]) {
                   amatch=1;
                }
             }
             for (j=1; j in req; j++) {
                if (oth[i] == req[j]) {
                   amatch=1
                }
                if (oth[i] == hst[j]) {
                   amatch=1
                }
             }
             if (amatch == 0) {
                print oth[i], " "
             }
          }
    }
   '`
#                                       Assign disks.
   for ENTRY in $ldefs ; do
#                                       Check for too many requests.
      if [ $nvol -lt 35 ] ; then
#                                       Ensure that the disk is mounted.
         if [ ! -f "$ENTRY/SPACE" ] ; then
            if [ $quiet = no ] ; then
               echo "             $ENTRY is currently unavailable."
            fi
         else
#                                       Add this disk.
            nvol=`expr $nvol + 1`
            nhex=`EHEX $nvol`
            eval "DA0$nhex=$ENTRY"; export DA0$nhex
            if [ $quiet = no ] ; then
               echo "   Disk $nvol ($nhex) is $ENTRY"
            fi
         fi
      else
         if [ $quiet = no ] ; then
            echo "             $ENTRY skipped (35 disk limit)"
         fi
      fi
   done
   for ENTRY in $defs ; do
#                                       Check for too many requests.
      if [ $nvol -lt 35 ] ; then
#                                       Ensure that the disk is mounted.
         if [ ! -f "$ENTRY/SPACE" ] ; then
            if [ $quiet = no ] ; then
               echo "             $ENTRY is currently unavailable."
            fi
         else
#                                       Add this disk.
            nvol=`expr $nvol + 1`
            nhex=`EHEX $nvol`
            eval "DA0$nhex=$ENTRY"; export DA0$nhex
            if [ $quiet = no ] ; then
               echo "   Disk $nvol ($nhex) is $ENTRY"
            fi
         fi
      else
         if [ $quiet = no ] ; then
            echo "             $ENTRY skipped (35 disk limit)"
         fi
      fi
   done
   for ENTRY in $kdefs ; do
#                                       Check for too many requests.
      if [ $nvol -lt 35 ] ; then
#                                       Ensure that the disk is mounted.
         if [ ! -f "$ENTRY/SPACE" ] ; then
            if [ $quiet = no ] ; then
               echo "             $ENTRY is currently unavailable."
            fi
         else
#                                       Add this disk.
            nvol=`expr $nvol + 1`
            nhex=`EHEX $nvol`
            eval "DA0$nhex=$ENTRY"; export DA0$nhex
            if [ $quiet = no ] ; then
               echo "   Disk $nvol ($nhex) is $ENTRY"
            fi
         fi
      else
         if [ $quiet = no ] ; then
            echo "             $ENTRY skipped (35 disk limit)"
         fi
      fi
   done
   for ENTRY in $odefs ; do
#                                       Check for too many requests.
      if [ $nvol -lt 35 ] ; then
#                                       Ensure that the disk is mounted.
         if [ ! -f "$ENTRY/SPACE" ] ; then
            if [ $quiet = no ] ; then
               echo "             $ENTRY is currently unavailable."
            fi
         else
#                                       Add this disk.
            nvol=`expr $nvol + 1`
            nhex=`EHEX $nvol`
            eval "DA0$nhex=$ENTRY"; export DA0$nhex
            if [ $quiet = no ] ; then
               echo "   Disk $nvol ($nhex) is $ENTRY"
            fi
         fi
      else
         if [ $quiet = no ] ; then
            echo "             $ENTRY skipped (35 disk limit)"
         fi
      fi
   done
else
#                                       da=default, so use defaults
#                                       as set in AIPSASSN.SH/.CSH
#                                       They are NOT CHECKED HERE for
#                                       performance reasons.
   if [ $quiet = no ] ; then
      echo "   (Use da=host to select other disks)"
   fi
   nhex=0
   while [ $nhex -lt 35 ] ; do
      nhex=`expr $nhex + 1`
      ehex=`EHEX $nhex`
      dname="\$DA0$ehex"
      dname=`eval echo $dname`
      if [ "$dname" != "" ] ; then
         if [ $quiet = no ] ; then
            echo "   Disk $nhex ($ehex) is $dname"
         fi
         nvol=$nhex
      fi
   done
fi
if [ $quiet = no ] ; then
   echo " "
fi
#                                       Activate data disks via ZDCHIN
NVOL="$nvol"; export NVOL

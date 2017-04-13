#!/usr/bin/echo you_need_to_dot_this_file
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1996, 1998, 2000-2015
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
# Usage: . AIPSPATH.SH
#-----------------------------------------------------------------------
# AIPSPATH defines the version of AIPS in use and redefines $PATH to
# include the AIPS system directories.  It also defines the AIPS
# programmer version selection macro environment variables.
#
#-----------------------------------------------------------------------
#                                       Set the AIPS root directory.
AIPS_ROOT=/DEFINE_ME
#                                       Define AIPS versions.
OLD=$AIPS_ROOT/31DEC14
NEW=$AIPS_ROOT/31DEC15
#                                       TST always used for TPMONs, and
#                                       for MANPATH.
TST=$AIPS_ROOT/31DEC16
CVX=$AIPS_ROOT/31DEC16
#                                       Set default version (see
#                                       START_AIPS).
[ "$VERSION" = "" ] && VERSION=TST
#
# --------------- do not change anything below this line ---------------
#
export AIPS_ROOT OLD NEW TST CVX VERSION
#
if [ ! -d $AIPS_ROOT ] ; then
   echo 'AIPSPATH.SH: AIPS_ROOT is incorrectly defined.'
   echo 'Edit AIPSPATH.SH (and .CSH) to set it for your site.'
else
#                                       Get host info if necessary.
   [ "$ARCH" = "" ] && . $AIPS_ROOT/HOSTS.SH
#
#                                       Set AIPS_VERSION.
   case $VERSION in
      OLD) AIPS_VERSION=$OLD;;
      NEW) AIPS_VERSION=$NEW;;
      TST) AIPS_VERSION=$TST;;
      CVX) AIPS_VERSION=$CVX;;
      *)   AIPS_VERSION=$TST;;
   esac
   export AIPS_VERSION

#                                       Define AIPS procedure areas.
   SYSUNIX=$AIPS_VERSION/SYSTEM/UNIX; export SYSUNIX
   if [ -d $AIPS_VERSION/$ARCH/SYSTEM/$SITE ] ; then
      SYSLOCAL=$AIPS_VERSION/$ARCH/SYSTEM/$SITE
   else
      SYSLOCAL=$AIPS_VERSION/$ARCH/SYSTEM
   fi
   export SYSLOCAL
#                                       LIBRARY_PATH: sync'd systems
   if [ -f $SYSLOCAL/RSYNC.ME ] ; then
      if [ $ARCH = "MACPPC" ] ; then
         if [ "$DYLD_LIBRARY_PATH" = "" ] ; then
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/IBMCMP
         else
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/IBMCMP:$DYLD_LIBRARY_PATH
         fi
         export DYLD_LIBRARY_PATH
      elif [ $ARCH = "MACINT" ] ; then
         if [ "$DYLD_LIBRARY_PATH" = "" ] ; then
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP
         else
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP:$DYLD_LIBRARY_PATH
         fi
         export DYLD_LIBRARY_PATH
      elif [ $ARCH = "SUL" ] ; then
         if [ "$LD_LIBRARY_PATH" = "" ] ; then
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/SUNCMP
         else
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/SUNCMP$LD_LIBRARY_PATH
         fi
         export LD_LIBRARY_PATH
      elif [ $ARCH = "LINUX" -o $ARCH = "LNX64" ] ; then
         if [ "$LD_LIBRARY_PATH" = "" ] ; then
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP
         else
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP:$LD_LIBRARY_PATH
         fi
         export LD_LIBRARY_PATH
      fi
   fi
#                                       LIBRARY_PATH: C programs on master
   if [ -f $SYSLOCAL/RSYNC.MASTER ] ; then
      if [ $ARCH = "MACPPC" ] ; then
         if [ "$DYLD_LIBRARY_PATH" = "" ] ; then
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/IBMCMP
         else
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/IBMCMP:$DYLD_LIBRARY_PATH
         fi
         export DYLD_LIBRARY_PATH
      elif [ $ARCH = "MACINT" ] ; then
         if [ "$DYLD_LIBRARY_PATH" = "" ] ; then
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP
         else
            DYLD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP:$DYLD_LIBRARY_PATH
         fi
         export DYLD_LIBRARY_PATH
      elif [ $ARCH = "SUL" ] ; then
         if [ "$LD_LIBRARY_PATH" = "" ] ; then
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/SUNCMP
         else
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/SUNCMP:$LD_LIBRARY_PATH
         fi
         export LD_LIBRARY_PATH
      elif [ $ARCH = "LINUX" -o $ARCH = "LNX64" ] ; then
         if [ "$LD_LIBRARY_PATH" = "" ] ; then
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP
         else
            LD_LIBRARY_PATH=$AIPS_VERSION/$ARCH/LIBR/INTELCMP:$LD_LIBRARY_PATH
         fi
         export LD_LIBRARY_PATH
      fi
   fi
#                                       MANPATH to get $SYSUNIX/manl,
#                                       but always use TST version.
   if [ "$MANPATH" != "" ] ; then
      tempath="$TST/SYSTEM/UNIX:"
      for d in `echo $MANPATH | sed -e 's/:/ /g'` ; do
         case $d in
            "$TST/SYSTEM/UNIX") ;;
            *) tempath="$tempath$d:";;
         esac
      done
      MANPATH=`echo $tempath | sed -e 's/:$//'`; export MANPATH
#                                       leave empty ones for man to
#                                       handle by new rules
#  else
#     MANPATH="/usr/man:$TST/SYSTEM/UNIX"; export MANPATH
   fi
#                                       Save PATH and redefine it.
#                                       check if it starts with ":"
#                                       Don't rely on existing TPATH
#                                       definition as PATH may have
#                                       changed in the interim!
#                                       Also check if we have perl.
   TPATH=`echo $PATH | sed 's/^://' | sed 's/:$//'`
   HAVE_PERL=""
   if [ -f $SYSUNIX/DADEVS.PL ] ; then
      perlplace=`head -n 1 $SYSUNIX/DADEVS.PL | sed -e 's/^..//'`
      [ -f $perlplace ] && HAVE_PERL=$perlplace
   fi
   export TPATH HAVE_PERL
   dot=".:"
   local="$SYSLOCAL":
   unix="$SYSUNIX":
   tempath="$dot$local$unix"
   for d in `echo $TPATH | sed 's/:/ /g'` ; do
      case $d in
         ".")
             ;;
         "$SYSLOCAL")
             ;;
         "$SYSUNIX")
             ;;
         *)
             tempath="$tempath$d:"
             ;;
      esac
   done

   PATH=`echo $tempath | sed 's/:$//'`; export PATH
#                                       Define the AIPS load area.
   LOAD="$AIPS_VERSION/$ARCH/LOAD"; export LOAD
#                                       Make AIPS version changing
#                                       commands.
   if [ -d $OLD/$ARCH/SYSTEM/$SITE ] ; then
      CDOLD="eval VERSION=OLD; export VERSION; . $OLD/$ARCH/SYSTEM/$SITE/CDVER.SH"
   else
      CDOLD="eval VERSION=OLD; export VERSION; . $OLD/$ARCH/SYSTEM/CDVER.SH"
   fi
   if [ -d $NEW/$ARCH/SYSTEM/$SITE ] ; then
      CDNEW="eval VERSION=NEW; export VERSION; . $NEW/$ARCH/SYSTEM/$SITE/CDVER.SH"
   else
      CDNEW="eval VERSION=NEW; export VERSION; . $NEW/$ARCH/SYSTEM/CDVER.SH"
   fi
   if [ -d $TST/$ARCH/SYSTEM/$SITE ] ; then
      CDTST="eval VERSION=TST; export VERSION; . $TST/$ARCH/SYSTEM/$SITE/CDVER.SH"
   else
      CDTST="eval VERSION=TST; export VERSION; . $TST/$ARCH/SYSTEM/CDVER.SH"
   fi
   if [ -d $CVX/$ARCH/SYSTEM/$SITE ] ; then
      CDCVX="eval VERSION=CVX; export VERSION; . $CVX/$ARCH/SYSTEM/$SITE/CDVER.SH"
   else
      CDCVX="eval VERSION=CVX; export VERSION; . $CVX/$ARCH/SYSTEM/CDVER.SH"
   fi
   export CDOLD CDNEW CDTST CDCVX
fi

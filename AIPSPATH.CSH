#!/usr/bin/echo you_need_to_source_this_file
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1996, 1998, 2000-2017
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
# Usage: source AIPSPATH.CSH
#-----------------------------------------------------------------------
# AIPSPATH defines the version of AIPS in use and redefines $PATH to
# include the AIPS system directories.  It also defines the AIPS
# programmer version selection macro environment variables.
#
#-----------------------------------------------------------------------
#                                       Set the AIPS root directory.
setenv AIPS_ROOT /opt/aips
#                                       Define AIPS versions.
setenv OLD $AIPS_ROOT/31DEC16
setenv NEW $AIPS_ROOT/31DEC17
setenv TST $AIPS_ROOT/31DEC18
setenv CVX $AIPS_ROOT/31DEC18
#                                       Assign default version (see
#                                       START_AIPS)
if (! $?VERSION) setenv VERSION TST
#
# --------------- do not change anything below this line ---------------
#
if ( ! -d $AIPS_ROOT ) then
   echo 'AIPSPATH.CSH: AIPS_ROOT is incorrectly defined.'
   echo 'Edit AIPSPATH.CSH (and .SH) to set it for your site.'
else
#                                       Get host info if necessary.
   if (! $?ARCH) source $AIPS_ROOT/HOSTS.CSH
#
#                                       Define AIPS versions.
   switch ($VERSION)
      case OLD:
         setenv AIPS_VERSION $OLD
         breaksw
      case NEW:
         setenv AIPS_VERSION $NEW
         breaksw
      case TST:
         setenv AIPS_VERSION $TST
         breaksw
      case CVX:
         setenv AIPS_VERSION $CVX
         breaksw
      default:
         setenv AIPS_VERSION $TST
         breaksw
   endsw

#                                       Define AIPS procedure areas.
   setenv SYSUNIX $AIPS_VERSION/SYSTEM/UNIX
   if (-d $AIPS_VERSION/$ARCH/SYSTEM/$SITE) then
      setenv SYSLOCAL $AIPS_VERSION/$ARCH/SYSTEM/$SITE
   else
      setenv SYSLOCAL $AIPS_VERSION/$ARCH/SYSTEM
   endif
#                                       LIBRARY_PATH: sync'd systems
   if (-f $SYSLOCAL/RSYNC.ME) then
      if ("$ARCH" == "MACPPC") then
         if (! $?DYLD_LIBRARY_PATH) then
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/IBMCMP
         else
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/IBMCMP:${DYLD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "MACINT") then
         if (! $?DYLD_LIBRARY_PATH) then
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${DYLD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "SUL") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/SUNCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/SUNCMP:${LD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "LINUX") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${LD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "LNX64") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${LD_LIBRARY_PATH}
         endif
      endif
   endif
#                                       LIBRARY_PATH: C programs on the master
   if (-f $SYSLOCAL/RSYNC.MASTER) then
      if ("$ARCH" == "MACPPC") then
         if (! $?DYLD_LIBRARY_PATH) then
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/IBMCMP
         else
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/IBMCMP:${DYLD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "MACINT") then
         if (! $?DYLD_LIBRARY_PATH) then
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv DYLD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${DYLD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "SUL") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/SUNCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/SUNCMP:${LD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "LINUX") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${LD_LIBRARY_PATH}
         endif
      else if ("$ARCH" == "LNX64") then
         if (! $?LD_LIBRARY_PATH) then
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP
         else
            setenv LD_LIBRARY_PATH ${AIPS_VERSION}/${ARCH}/LIBR/INTELCMP:${LD_LIBRARY_PATH}
         endif
      endif
   endif
#                                       MANPATH to get $SYSUNIX/manl,
#                                       but always use TST version.
   if ( $?MANPATH ) then
      set tempath = "$TST/SYSTEM/UNIX:"
      foreach d (`echo $MANPATH | sed -e 's/:/ /g'`)
         switch ($d)
	    case "$TST/SYSTEM/UNIX":
               breaksw
            default:
               set tempath = "$tempath${d}:";;
               breaksw
         endsw
      end
      setenv MANPATH `echo $tempath | sed -e 's/:$//'`
#                                       leave empty ones for man to
#                                       handle by new rules
#  else
#     setenv MANPATH "/usr/man:$TST/SYSTEM/UNIX"
   endif
#                                       Save PATH and redefine it.
#                                       check if it starts with ":"
#                                       Don't rely on existing TPATH
#                                       definition as PATH may have
#                                       changed in the interim!
   setenv TPATH `echo $PATH | sed 's/^://' | sed 's/:$//'`
   setenv HAVE_PERL ""
   if ( -f $SYSUNIX/DADEVS.PL ) then
      set perlplace = `head -n 1 $SYSUNIX/DADEVS.PL | sed -e 's/^..//'`
     if ( -f $perlplace ) setenv HAVE_PERL $perlplace
   endif
   set dot   = ".:"
   set local = "$SYSLOCAL":
   set unix  = "$SYSUNIX":
   set tempath = "$dot$local$unix"
   foreach d (`echo $TPATH | sed 's/:/ /g'`)
      switch ($d)
         case ".":
            breaksw
         case "$SYSLOCAL":
            breaksw
         case "$SYSUNIX":
            breaksw
         default:
            set tempath = "$tempath${d}:"
            breaksw
      endsw
   end
   setenv PATH `echo $tempath | sed 's/:$//'`
   unset dot local unix tempath
#                                       Define the AIPS load area.
   setenv LOAD $AIPS_VERSION/$ARCH/LOAD
#                                       Make AIPS version changing
#                                       commands.
   if ( -d $OLD/$ARCH/SYSTEM/$SITE ) then
      setenv CDOLD "eval setenv VERSION OLD; source $OLD/$ARCH/SYSTEM/$SITE/CDVER.CSH"
   else
      setenv CDOLD "eval setenv VERSION OLD; source $OLD/$ARCH/SYSTEM/CDVER.CSH"
   endif
   if ( -d $NEW/$ARCH/SYSTEM/$SITE ) then
      setenv CDNEW "eval setenv VERSION NEW; source $NEW/$ARCH/SYSTEM/$SITE/CDVER.CSH"
   else
      setenv CDNEW "eval setenv VERSION NEW; source $NEW/$ARCH/SYSTEM/CDVER.CSH"
   endif
   if ( -d $TST/$ARCH/SYSTEM/$SITE ) then
      setenv CDTST "eval setenv VERSION TST; source $TST/$ARCH/SYSTEM/$SITE/CDVER.CSH"
   else
      setenv CDTST "eval setenv VERSION TST; source $TST/$ARCH/SYSTEM/CDVER.CSH"
   endif
   if ( -d $CVX/$ARCH/SYSTEM/$SITE ) then
      setenv CDCVX "eval setenv VERSION CVX; source $CVX/$ARCH/SYSTEM/$SITE/CDVER.CSH"
   else
      setenv CDCVX "eval setenv VERSION CVX; source $CVX/$ARCH/SYSTEM/CDVER.CSH"
   endif
endif

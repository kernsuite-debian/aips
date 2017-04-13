#-----------------------------------------------------------------------
#;  Copyright (C) 1996, 2000-2002, 2004-2006, 2008
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
# Usage: . AIPSASSN.SH
#-----------------------------------------------------------------------
# AIPSASSN defines environment variables required to run AIPS.  It
# relies on assignments made by AIPSPATH.SH, particularly $AIPS_ROOT.
# Normally AIPSASSN.SH is called by LOGIN.SH or other AIPS scripts.
#
# The TAPEn definitions are now in $NET0/TPDEVS.LIST, not here.
# Explanation of some of these variables:
#
#    NET0         Location of network system parameter file NETSP.
#                 Used by ZDCHIN for disk information.
#
#    DATA_ROOT    Root directory of all AIPS data directories
#    DA00         Host-specific directory containing AIPS system files.
#
#    LPDEV        Printer to use (see $AIPS_ROOT/PRDEVS.SH for LP* vars)
#    LPDEVn       List of printer names xx as in "lpr -Pxx"
#    LPTYPEn      Corresponding printer type: PS, QMS, TEXT, or PREVIEW.
#    LPDESCn      Human-readable description of printer (location, etc).
#
#    RUNFIL       Area for "run" files (text).  If RUNFIL is already
#                 defined, it will NOT be overridden.
#
#    RESSTTn      Terminals reserved for AIPSn (if any).  Used by ZSTRTA
#
#    TVTTn        hardware TV for AIPS number n, reserved terminal n.
#                 Restricts access to AIPS n only.  See TVDEVS.SH also.
#
#    TASKTTn      AIPSn task message terminal (if any).  Usually
#                 associated with reserved terminals.  Used by ZTTOPN.
#                 This is NOT the MSGSRV (Message Server) definition!
#
#    TEKTKn       AIPSn Tektronix 4010/4012 compatible terminal (if any)
#                 This is NOT the TEKSRV (Tektronix Server) definition!
#
#    BATCH_OUT    Batch AIPS error file/terminal.
#
#    LPRNTR       Line printer spool directory.  Used by ZLPOPN.
#
#    PLOTTER      Plotter spool directory.  Used by ZLASIO and ZLWIO.
#
#-----------------------------------------------------------------------
#                                       Misc non-version directories
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                       Ionospheric data.
AIPSIONS=$AIPS_ROOT/TEXT/IONS; export AIPSIONS
#                                       Public documents
AIPSPUBL=$AIPS_ROOT/TEXT/PUBL; export AIPSPUBL
#                                       Gripes.  Not for distribution.
AIPSGRIP=$AIPS_ROOT/TEXT/GRIP; export AIPSGRIP
#                                       AIPS user lists.  Ditto.
AIPSWHO=$AIPS_ROOT/TEXT/WHO; export AIPSWHO
#                                       AIPS Star file area
AIPSTARS=$AIPS_ROOT/TEXT/STARS; export AIPSTARS
#                                       AIPS FITS (not user) area.
AIPSFITS=$AIPS_ROOT/BIN/FITS; export AIPSFITS
#                                       Area for fig, etc. files
AIPSFIG=$AIPS_ROOT/TEXT/PUBL/FIG; export AIPSFIG
#                                       HTML files
AIPSHTML=$AIPS_ROOT/TEXT/HTML; export AIPSHTML
#                                       System OFM files.
AIPSOFM=$AIPS_ROOT/TEXT/OFM; export AIPSOFM
#                                       User OFM file area.
OFMFIL=$AIPS_ROOT/OFM; export OFMFIL
#                                       User RUN file area (no override)
if [ "$RUNFIL" = "" ] ; then
  RUNFIL=$AIPS_ROOT/RUN
  export RUNFIL
fi
#                                       PRINT spooling area.
PRTFIL=$AIPS_ROOT/PRINT; export PRTFIL
#                                       PLOT spooling directory.
PLTFIL=$AIPS_ROOT/PLOT; export PLTFIL
#
#                                       System and data areas
#                                       ~~~~~~~~~~~~~~~~~~~~~
NET0=$AIPS_ROOT/DA00; export NET0
#                                       Data root area, normally /DATA
DATA_ROOT=/DEFINE_ME; export DATA_ROOT
DA00=$AIPS_ROOT/DA00/$HOST; export DA00
#                                       These definitions don't get used
#                                       if DADEVS.SH is called (as it is
#                                       from START_AIPS and RUN) unless
#                                       you use "da=default".  To make
#                                       this option work, set these to
#                                       some sensible defaults.
# DA01=$DATA_ROOT/${HOST}_1; export DA01
# DA02=$DATA_ROOT/${HOST}_2; export DA02
# DA03=$DATA_ROOT/${HOST}_3; export DA03
# DA04=$DATA_ROOT/${HOST}_4; export DA04
#                                       FITS disk area.
FITS=$AIPS_ROOT/FITS; export FITS
#                                       Version specific directories
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                       Memory areas.
OLDMEM=$OLD/$ARCH/MEMORY; export OLDMEM
NEWMEM=$NEW/$ARCH/MEMORY; export NEWMEM
TSTMEM=$TST/$ARCH/MEMORY; export TSTMEM
CVXMEM=$CVX/$ARCH/MEMORY; export CVXMEM
#                                       Load areas.
LOAD=$AIPS_VERSION/$ARCH/LOAD; export LOAD
TVLOAD=$LOAD; export TVLOAD
#                                       LIBRDBG area, for DEBUG version
LIBRDBG=$AIPS_VERSION/$ARCH/LIBRDBG; export LIBRDBG
#                                       Pseudo AP areas.
OLDPSAP=$OLD/$ARCH/LOAD/ALT1; export OLDPSAP
NEWPSAP=$NEW/$ARCH/LOAD/ALT1; export NEWPSAP
TSTPSAP=$TST/$ARCH/LOAD/ALT1; export TSTPSAP
CVXPSAP=$CVX/$ARCH/LOAD/ALT1; export CVXPSAP
#                                       Errors area.
ERRORS=$AIPS_VERSION/$ARCH/ERRORS/$HOST; export ERRORS
#                                       System run files.
RUNSYS=$AIPS_VERSION/RUN; export RUNSYS
#
#                                       Device assignments
#                                       ~~~~~~~~~~~~~~~~~~
#                                       (Tapes done in TPDEVS.SH/LIST)
#                                       Set terminal for ZSTRTA.
TASKTT0="`tty 2>/dev/null`"; export TASKTT0
#                                       Printer and plotter definitions.
BATCH_OUT=/dev/console; export BATCH_OUT
LPRNTR=/tmp; export LPRNTR
PLOTTER=/tmp; export PLOTTER
#                                       Number of printers.  Modified by
#                                       PRDEVS.SH (called by START_AIPS)
LPDEVS=1; export LPDEVS
#                                       Host specific definitions
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~
#                                       Local host specific things
  if [ -f $AIPS_ROOT/LOCALASSN.SH ] ; then
     . $AIPS_ROOT/LOCALASSN.SH
  fi
#                                       below are NRAO spcific as example
#                                       LWPRINTER?, TASKTTn, RESSTTn,
#                                       TEKTKn, TAAC1, and modification
#                                       of any of the foregoing.
if [ "$SITE" = "NRAOCV" -o "$SITE" = VCOARN ] ; then
#                                       NRAO Charlottesville.  These
#                                       definitions are shown here only
#                                       for illustrative purposes.  You
#                                       should delete this section and
#                                       insert definitions appropriate
#                                       for your site.
#                                       --------------------------------
#                                       Architecture-specific details
#                                       For AIX 3.2.5, the following
#                                       changes the behaviour of any
#                                       malloc() calls by ANY
#                                       application.  If this interferes
#                                       with other non-AIPS applications
#                                       in an undesirable way, remove
#                                       this definition and place it in
#                                       the $SYSUNIX/AIPSEXEC file.
#                                       See /usr/lpp/bos/README.PSALLOC.
   if [ "$ARCH" = IBM ] ; then
      PSALLOC="early"; export PSALLOC
#                                       Problem w/SHM on our IBM's...
      if [ "$DISPLAY" != "" ] ; then
         displayhost=`echo $DISPLAY | awk -F: '{print $1}'`
         case $displayhost in
            ""|unix|`hostname`)
               echo "No X11 MIT shared memory (problem with IBM X servers)"
               echo 'AIPStv*useSharedMemory: 0' | xrdb -merge
               ;;
         esac
      fi
   fi
   if [ "$DATA_ROOT" = "/DEFINE_ME" ] ; then
     DATA_ROOT=/DATA; export DATA_ROOT
   fi
#                                       Different place for FITS at AOC.
   PUBFITS=$AIPS_ROOT/FITS; export PUBFITS
   if [ -d $DATA_ROOT/${HOST}_1 ] ; then
      if [ ! -d $DATA_ROOT/${HOST}_1/FITS ] ; then
         if mkdir $DATA_ROOT/${HOST}_1/FITS ; then
            FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
         else
            echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
            echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
            FITS=$DATA_ROOT/${HOST}_1; export FITS
         fi
      else
      FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
    fi
  fi
#
#
elif [ "$SITE" = NRAOAOC -o "$SITE" = COAOARN ] ; then
#                                       NRAO Array Ops Ctr, Socorro, NM.
#                                       Turn on MSGSRV unless it's set
#                                       already.  Site Preference.
   if [ "$AIPS_MSG_EMULATOR" = "" ] ; then
      AIPS_MSG_EMULATOR=xterm; export AIPS_MSG_EMULATOR
   fi
   if [ "$DATA_ROOT" = "/DEFINE_ME" ] ; then
     DATA_ROOT=/DATA; export DATA_ROOT
   fi
#                                       ONLINE logical for AOC
   ONLINE=$AIPS_ROOT/online/$ARCH; export ONLINE
#                                       Different place for FITS at AOC.
   if [ -d $DATA_ROOT/${HOST}_1 ] ; then
      if [ ! -d $DATA_ROOT/${HOST}_1/FITS ] ; then
         if mkdir $DATA_ROOT/${HOST}_1/FITS ; then
            FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
         else
            echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
            echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
            FITS=$DATA_ROOT/${HOST}_1; export FITS
         fi
      else
      FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
    fi
  fi
#
elif [ "$SITE" = NRAOVLA -o "$SITE" = ALVOARN  ] ; then
#                                       NRAO Array Ops Ctr, Socorro, NM.
#                                       Turn on MSGSRV unless it's set
#                                       already.  Site Preference.
  if [ "$AIPS_MSG_EMULATOR" = "" ] ; then
    AIPS_MSG_EMULATOR=xterm; export AIPS_MSG_EMULATOR
  fi
  if [ "$DATA_ROOT" = "/DEFINE_ME" ] ; then
    DATA_ROOT=/DATA; export DATA_ROOT
  fi
#                                       OPERATIONS area.
  OPER=/home/banshee/operations/tapelog; export OPER
#                                       ONLINE definition for VLA
  ONLINE=$AIPS_ROOT/online/${ARCH}; export ONLINE
  VISDATADIR=/home/vis-serv/vladata; export VISDATADIR
#                                       FITS definition
#                                       Different place for FITS at AOC.
  if [ -d $DATA_ROOT/${HOST}_1 ] ; then
    if [ ! -d $DATA_ROOT/${HOST}_1/FITS ] ; then
      if mkdir $DATA_ROOT/${HOST}_1/FITS ; then
         FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
      else
        echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
        echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
        FITS=$DATA_ROOT/${HOST}_1; export FITS
      fi
    else
      FITS=$DATA_ROOT/${HOST}_1/FITS; export FITS
    fi
  fi
#                                       Host-specific things.  This
#                                       particular one is left in as an
#                                       example of how to have alternate
#                                       load areas and hardwired AIPS1,
#                                       AIPS2, etc. and graphics
#                                       terminals.
  if [ "$HOST" = YUCCA ] ; then
#                                       Note LOAD4 area for CVEX only....
    LOAD4=$AIPS_VERSION/$ARCH/LOAD/ALT4; export LOAD4
#                                       Reserved terminals etc.  AIPS1:
#                                       terminal on tty0c, iis tv, tek
#                                       graphics terminal on tty0e
    RESSTT1=/dev/tty0c; export RESSTT1
       TVTT1=yucca+iis; export TVTT1
    TEKTK01=/dev/tty0e; export TEKTK01
#                                       AIPS2: terminal on tty0d, ivas
#                                       tv, tek same as AIPS1.
    RESSTT2=/dev/tty0d; export RESSTT2
       TVTT2=yucca+iis; export TVTT2
    TEKTK02=/dev/tty0e; export TEKTK02
#                                       define obsolete TVDEV1 def'n for
#                                       dicomed software.
    TVDEV1=/dev/iis; export TVDEV1
#
  fi
#                                       CfA
elif [ "$SITE" = CFA ] ; then
#                                       Turn off MSGSRV unless it's set
#                                       already.  Site Preference.
   if [ "$AIPS_MSG_EMULATOR" = "" ] ; then
      AIPS_MSG_EMULATOR=none; export AIPS_MSG_EMULATOR
   fi
#
fi
#                                       End of AIPSASSN.SH

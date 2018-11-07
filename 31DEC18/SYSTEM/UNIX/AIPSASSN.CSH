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
# Usage: source AIPSASSN.CSH
#-----------------------------------------------------------------------
# AIPSASSN defines environment variables required to run AIPS.  It
# relies on assignments made by AIPSPATH.CSH, particularly $AIPS_ROOT.
# Normally AIPSASSN.CSH is called by LOGIN.CSH or other AIPS scripts.
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
#                                       Non-version specific directories
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                       Ionospheric data.
setenv AIPSIONS $AIPS_ROOT/TEXT/IONS
#                                       Public documents.
setenv AIPSPUBL $AIPS_ROOT/TEXT/PUBL
#                                       Gripes.  Not for distribution.
setenv AIPSGRIP $AIPS_ROOT/TEXT/GRIP
#                                       AIPS user lists.  Ditto.
setenv AIPSWHO  $AIPS_ROOT/TEXT/WHO
#                                       AIPS Star file area
setenv AIPSTARS $AIPS_ROOT/TEXT/STARS
#                                       AIPS FITS (not user) area.
setenv AIPSFITS $AIPS_ROOT/BIN/FITS
#                                       Area for fig, etc. files
setenv AIPSFIG $AIPS_ROOT/TEXT/PUBL/FIG
#                                       HTML files
setenv AIPSHTML $AIPS_ROOT/TEXT/HTML
#                                       System OFM files.
setenv AIPSOFM $AIPS_ROOT/TEXT/OFM
#                                       User OFM file area.
setenv OFMFIL $AIPS_ROOT/OFM
#                                       Default user RUN file area.
if ($?RUNFIL == 0) then
   setenv RUNFIL $AIPS_ROOT/RUN
endif
#                                       PRINT spooling area.
setenv PRTFIL $AIPS_ROOT/PRINT
#                                       PLOT spooling directory.
setenv PLTFIL $AIPS_ROOT/PLOT
#
#                                       System and data areas
#                                       ~~~~~~~~~~~~~~~~~~~~~
setenv NET0 $AIPS_ROOT/DA00
#                                       Data root area, normally /DATA
setenv DATA_ROOT /DEFINE_ME
setenv DA00 $AIPS_ROOT/DA00/$HOST
#                                       These definitions don't get used
#                                       if DADEVS.SH is called (as it is
#                                       from START_AIPS and RUN) unless
#                                       you use "da=default".  To make
#                                       this option work, set these to
#                                       some sensible defaults.
# setenv DA01 $DATA_ROOT/${HOST}_1
# setenv DA02 $DATA_ROOT/${HOST}_2
# setenv DA03 $DATA_ROOT/${HOST}_3
# setenv DA04 $DATA_ROOT/${HOST}_4
#                                       FITS disk area.
setenv FITS $AIPS_ROOT/FITS
#                                       Version specific directories
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                       Memory areas.
setenv OLDMEM  $OLD/$ARCH/MEMORY
setenv NEWMEM  $NEW/$ARCH/MEMORY
setenv TSTMEM  $TST/$ARCH/MEMORY
setenv CVXMEM  $CVX/$ARCH/MEMORY
#                                       Load areas.
setenv LOAD    $AIPS_VERSION/$ARCH/LOAD
setenv TVLOAD  $LOAD
#                                       LIBRDBG area, for DEBUG version
setenv LIBRDBG $AIPS_VERSION/$ARCH/LIBRDBG
#                                       Pseudo AP areas.
setenv OLDPSAP $OLD/$ARCH/LOAD/ALT1
setenv NEWPSAP $NEW/$ARCH/LOAD/ALT1
setenv TSTPSAP $TST/$ARCH/LOAD/ALT1
setenv CVXPSAP $CVX/$ARCH/LOAD/ALT1
#                                       Errors area.
setenv ERRORS  $AIPS_VERSION/$ARCH/ERRORS/$HOST
#                                       System run files.
setenv RUNSYS  $AIPS_VERSION/RUN
#
#                                       Device assignments
#                                       ~~~~~~~~~~~~~~~~~~
#                                       (Tapes done in TPDEVS.SH/LIST)
#                                       Set terminal for ZSTRTA.
setenv TASKTT0 "`tty >&/dev/null`"
#                                       Printer and plotter definitions.
setenv BATCH_OUT /dev/console
setenv LPRNTR  /tmp
setenv PLOTTER /tmp
#                                       Number of printers.  Modified by
#                                       PRDEVS.SH (called by START_AIPS)
setenv LPDEVS 1
#                                       Host specific definitions
#                                       ~~~~~~~~~~~~~~~~~~~~~~~~~
if ( -f $AIPS_ROOT/LOCALASSN.CSH ) then
   source $AIPS_ROOT/LOCALASSN.CSH
endif
#                                       NRAO-specific examples
#                                       LWPRINTER?, TASKTTn, RESSTTn,
#                                       TEKTKn, TAAC1, and modification
#                                       of any of the foregoing.
if ("$SITE" == NRAOCV || "$SITE" == VCOARN) then
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
#                                       this definition and place the
#                                       bourne shell equivalent (see
#                                       AIPSASSN.SH) in the
#                                       $SYSUNIX/AIPSEXEC file.
#                                       See /usr/lpp/bos/README.PSALLOC.
  if ( "$ARCH" == "IBM" ) then
     setenv PSALLOC early
#                                       Problem w/SHM on our IBM's...
    if ( $?DISPLAY ) then
      set displayhost = `echo $DISPLAY | awk -F: '{print $1}'`
      if ( "$DISPLAY" != "" ) then
        if ( "$displayhost" == "" || \
             "$displayhost" == "`hostname`" || \
             "$displayhost" == "unix" ) then
          echo "No X11 MIT shared memory (problem with IBM X servers)"
          echo 'AIPStv*useSharedMemory: 0' | xrdb -merge
        endif
      endif
    endif
  endif
#                                       Different place for FITS at CV
  setenv PUBFITS $AIPS_ROOT/FITS
  if ( -d $DATA_ROOT/${HOST}_1 ) then
    if ( ! -d $DATA_ROOT/${HOST}_1/FITS ) then
      if ( mkdir $DATA_ROOT/${HOST}_1/FITS ) then
        setenv FITS $DATA_ROOT/${HOST}_1/FITS
      else
        echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
        echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
        setenv FITS $DATA_ROOT/${HOST}_1
      endif
    else
      setenv FITS $DATA_ROOT/${HOST}_1/FITS
    endif
  endif
#
else if ("$SITE" == NRAOAOC || "$SITE" == COAOARN) then
#                                       NRAO Array Ops Ctr, Socorro, NM.
#                                       Turn on MSGSRV unless it's set
#                                       already.  Site Preference.
  if ( ! $?AIPS_MSG_EMULATOR ) then
    setenv AIPS_MSG_EMULATOR xterm
  endif
  if ("$DATA_ROOT" == "/DEFINE_ME") then
     setenv DATA_ROOT /DATA
  endif
#                                       ONLINE logical for AOC
  setenv ONLINE $AIPS_ROOT/online/$ARCH
#                                       Different place for FITS at AOC.
  if ( -d $DATA_ROOT/${HOST}_1 ) then
    if ( ! -d $DATA_ROOT/${HOST}_1/FITS ) then
      if ( mkdir $DATA_ROOT/${HOST}_1/FITS ) then
        setenv FITS $DATA_ROOT/${HOST}_1/FITS
      else
        echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
        echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
        setenv FITS $DATA_ROOT/${HOST}_1
      endif
    else
      setenv FITS $DATA_ROOT/${HOST}_1/FITS
    endif
  endif
#
else if ("$SITE" == NRAOVLA || "$SITE" == ALVOARN) then
#                                       NRAO VLA site.
#                                       Turn on MSGSRV unless it's set
#                                       already.  Site Preference.
  if ( ! $?AIPS_MSG_EMULATOR ) then
    setenv AIPS_MSG_EMULATOR xterm
  endif
  if ("$DATA_ROOT" == "/DEFINE_ME") then
     setenv DATA_ROOT /DATA
  endif
#                                       OPERATIONS area.
  setenv OPER /home/banshee/operations/tapelog
#                                       ONLINE logical for VLA
  setenv ONLINE $AIPS_ROOT/online/$ARCH
  setenv VISDATADIR /home/vis-serv/vladata
#                                       FITS disk area.
#                                       Different place for FITS at AOC.
  if ( -d $DATA_ROOT/${HOST}_1 ) then
    if ( ! -d $DATA_ROOT/${HOST}_1/FITS ) then
      if ( mkdir $DATA_ROOT/${HOST}_1/FITS ) then
        setenv FITS $DATA_ROOT/${HOST}_1/FITS
      else
        echo "AIPSASSN: Tried to create $DATA_ROOT/${HOST}_1/FITS area, failed."
        echo "AIPSASSN: Using $DATA_ROOT/${HOST}_1 itself instead."
        setenv FITS $DATA_ROOT/${HOST}_1
      endif
    else
      setenv FITS $DATA_ROOT/${HOST}_1/FITS
    endif
  endif
#                                       Host-specific things.  This
#                                       particular one is left in as an
#                                       example of how to have alternate
#                                       load areas and hardwired AIPS1,
#                                       AIPS2, etc. and graphics
#                                       terminals.
  if ("$HOST" == YUCCA) then
#                                       Note LOAD4 area; for CVEX only
     setenv LOAD4 $AIPS_VERSION/$ARCH/LOAD/ALT4
#                                       Reserved terminals etc.  AIPS1:
#                                       terminal on tty0c, iis tv, tek
#                                       graphics terminal on tty0e
     setenv RESSTT1 /dev/tty0c
     setenv TVTT1   yucca+iis
     setenv TEKTK01 /dev/tty0e
#                                       AIPS2: terminal on tty0d, ivas
#                                       tv, tek same as AIPS1.
     setenv RESSTT2 /dev/tty0d
     setenv TVTT2   yucca+iis
     setenv TEKTK02 /dev/tty0e
#                                       define obsolete TVDEV1 def'n for
#                                       dicomed software.
     setenv TVDEV1 /dev/iis
#
  endif
#                                       CFA
else if ("$SITE" == CFA) then
#                                       Turn off MSGSRV unless it's set
#                                       already.  Site Preference.
  if ( ! $?AIPS_MSG_EMULATOR ) then
    setenv AIPS_MSG_EMULATOR none
  endif
#
endif
#                                       End of AIPSASSN.CSH

$! ICREDCL.COM
$!----------------------------------------------------------------------
$!;  Copyright (C) 1995
$!;  Associated Universities, Inc. Washington DC, USA.
$!;
$!;  This program is free software; you can redistribute it and/or
$!;  modify it under the terms of the GNU General Public License as
$!;  published by the Free Software Foundation; either version 2 of
$!;  the License, or (at your option) any later version.
$!;
$!;  This program is distributed in the hope that it will be useful,
$!;  but WITHOUT ANY WARRANTY; without even the implied warranty of
$!;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$!;  GNU General Public License for more details.
$!;
$!;  You should have received a copy of the GNU General Public
$!;  License along with this program; if not, write to the Free
$!;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
$!;  MA 02139, USA.
$!;
$!;  Correspondence concerning AIPS should be addressed as follows:
$!;         Internet email: aipsmail@nrao.edu.
$!;         Postal address: AIPS Project Office
$!;                         National Radio Astronomy Observatory
$!;                         520 Edgemont Road
$!;                         Charlottesville, VA 22903-2475 USA
$!----------------------------------------------------------------------
$!----------------------------------------------------------------------
$! Create the DCL file ASSNLOCAL.COM
$!----------------------------------------------------------------------
$!
$ write sys$output "Creating file ASSNSTART.NEW"
$ TheDir = f$logical("SYS$DISK") + f$directory()
$ TopDir = f$parse(TheDir,,,"device","syntax_only,no_conceal")-
       + f$parse(TheDir,,,"directory","syntax_only,no_conceal")-
       - "][" - "]" + "."
$ NewDir = TopDir + VERDATE
$ open /write assnstart ASSNSTART.NEW
$ write assnstart -
    "$! ASSNSTART - This procedure will assign logical name AIPS_STARTUP to the"
$ write assnstart -
    "$! area that will be used for all procedures that create and initialize"
$ write assnstart -
    "$! the logical name tables for AIPS."
$ write assnstart -
    "$!"
$ write assnstart -
    "$! Parameter P1 is a DEFINE/SYSTEM or possibly DEFINE/JOB command."
$ write assnstart -
    "$!"
$ write assnstart -
    "$       'P1' AIPS_STARTUP  ",NewDir,".SYSTEM.VMS]"
$ close assnstart
$!
$!
$ write sys$output "Creating file ASSNLOCAL.COM"
$ @AssnStart.new DEFINE
$ open /write assnlocal aips_startup:ASSNLOCAL.COM
$ write assnlocal -
  "$! ASSNLOCAL - Set up local AIPS logical names"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! The logical names may be defined in various tables depending upon"
$ write assnlocal -
  "$! how this procedure is invoked. P1 is a suitable DEFINE command and"
$ write assnlocal -
  "$! any DEFINE's must be derived from this."
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! P1 = a suitable DEFINE command to be used"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Two flavours of the DEFINE command"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$       DEF := 'P1'/NOLOG"
$ write assnlocal -
  "$       DEC := 'P1'/NOLOG/TRANSLATION_ATRIBUTES=CONCEALED"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! AIPS Versions - up to three allowed (OLD, NEW, TST)"
$ write assnlocal -
  "$! Note that physical device names must be used here in order that"
$ write assnlocal -
  "$! the rooted logical names work correctly."
$ write assnlocal -
  "$! WARNING! Procedure ASSNSTART also contains a hardcoded version"
$ write assnlocal -
  "$! date which must be changed when the following lines are changed."
$ write assnlocal -
  "$!"
$ if (OLDVER.nes."NONE") .and. (OLDVER.nes."") then write assnlocal -
  "$       DEC OLD       ", TopDir,OLDVER,".]"
$ if (NEWVER.nes."NONE") .and. (NEWVER.nes."") then write assnlocal -
  "$       DEC NEW       ", TopDir,NEWVER,".]"
$ if (TSTVER.nes."NONE") .and. (TSTVER.nes."") then write assnlocal -
  "$       DEC TST       ", TopDir,TSTVER,".]"
$ write assnlocal -
  "$       DEC LOCAL ", TopDir,"LOCAL.]"
$ write assnlocal -
  "$       DEC AIPS_ROOT ", TopDir,"]"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Data areas"
$ write assnlocal -
  "$!"
$ I = 0
$ DISKLOOP:
$   if (I.gt.NUMDISKS) then GOTO ENDDISK
$   write assnlocal -
      "$       DEF DA0",I,"  ",AIPSDA0'I'
$   I = I + 1
$ GOTO DISKLOOP
$ ENDDISK:
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! LOCAL areas"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$       DEF LOCRUN LOCAL:[RUN]"
$ write assnlocal -
  "$       DEF LOCLOD LOCAL:[LOAD]"
$ write assnlocal -
  "$       DEF LOCSYS LOCAL:[SYSTEM.VMS]"
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Magnetic tapes"
$ write assnlocal -
  "$!"
$ I = 0
$ TAPELOOP:
$   if (I.ge.NUMTAPES) then GOTO ENDTAPE
$   K = I + 1
$   write assnlocal -
      "$       DEF MT0", I,"  ", AIPSMT0'K'
$   I = I + 1
$ GOTO TAPELOOP
$ ENDTAPE:
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Reserved AIPS terminals"
$ write assnlocal -
  "$!"
$ I = 0
$ RESSTTLOOP:
$   I = I + 1
$   if (I.gt.NUMRESTT) then GOTO ENDRESSTT
$   write assnlocal -
      "$       DEF RESSTT", I, "  ", RTT'I'
$   if (TTT'I'.nes."") then write assnlocal -
      "$       DEF TASKTT", I ,"  ", TTT'I'
$ GOTO RESSTTLOOP
$ ENDRESSTT:
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Batch log terminal"
$ write assnlocal -
  "$!"
$ if (NUMBATCH.ne.0) then write assnlocal -
  "$       DEF BATCH_OUT ", BATOUT
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! TEK terminals"
$ write assnlocal -
  "$!"
$ I = 0
$ TEKTKLOOP:
$   I = I + 1
$   if (I.gt.NUMTEKS) then GOTO ENDTEKTK
$   write assnlocal -
      "$       DEF TEKTK",I,"  ",TKTT'I'
$ GOTO TEKTKLOOP
$ ENDTEKTK:
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! TV"
$ write assnlocal -
  "$!"
$ I = 0
$ TVLOOP:
$   I = I + 1
$   if (I.gt.NUMTVS) then GOTO ENDTV
$   write assnlocal -
      "$       DEF TVDEV",I,"  ",TV'I'
$ GOTO TVLOOP
$ ENDTV:
$ write assnlocal -
  "$!"
$ write assnlocal -
  "$! Plotters"
$ write assnlocal -
  "$!"
$ if (QMSQ.nes."") then write assnlocal -
    "$       DEF LASQUE ",QMSQ
$ if (PLOTQUE.nes."") then write assnlocal -
  "$       DEF VERPLOT ",PLOTQUE
$ write assnlocal -
    "$       DEF LPRQUE SYS$PRINT            ! line printer queue"
$ write assnlocal -
    "$       DEF PLOTTER FITS                ! temp area for plot file"
$ write assnlocal -
    "$       DEF LPRNTR FITS                 ! temp area for print file"
$ close assnlocal
$!
$ EXIT

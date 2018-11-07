$! AIPS.COM
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
$!-----------------------------------------------------------------------
$! Start up, run and shutdown AIPS.
$! Inputs:
$!   Pn  Options - following allowed
$!       OLD, NEW or TST to select version
$!       REMOTE to indicate a remote terminal i.e. no TV and integral TEK.
$!       DEBUG to run with debugger
$!       LOCAL to run a local version of AIPS (exe in current default).
$!-----------------------------------------------------------------------
$!
$! Make AIPS users one big happy family.
$!
$  Old_Protection = f$environment("Protection")
$  SET protection=(S:RWED,O:RWED,G:RWED,W:RWED) /DEFAULT
$!
$! Default options
$!
$ REMOTE = "NO"
$ DEBUG  = "NODEBUG"
$ TheDir = "AIPS_VERSION:[LOAD]"
$!
$! Check options
$!
$ COUNT = 0
$ OPTIONS:
$    COUNT = COUNT + 1
$    IF COUNT.GT.4 THEN GOTO POPS
$    OPT = P'COUNT'
$    OPT := 'OPT'
$!
$! Set options
$!
$    IF OPT.EQS."OLD" .OR. OPT.EQS."NEW" .OR. OPT.EQS."TST" THEN -
                DEFINE/NOLOG AIPS_VERSION 'OPT'
$    IF OPT.EQS."TEST" THEN DEFINE/NOLOG AIPS_VERSION TST
$    IF OPT.EQS."REMOTE" THEN REMOTE = "YES"
$    IF OPT.EQS."DEBUG" THEN DEBUG = "DEBUG"
$    IF OPT.EQS."LOCAL" THEN TheDir = ""
$ GOTO OPTIONS
$!
$ POPS:
$!                                           Type out any notices:
$ aipsnotice := aips_proc:notice.txt
$ IF F$SEARCH(aipsnotice) .NES. "" THEN TYPE 'aipsnotice'
$!
$! Set process name to AIPSn
$! Reserved terminals are specified by the logical names RESSTTn
$!
$ ON ERROR THEN GOTO FINI
$ PRCNAM = f$getjpi("", "PRCNAM")
$ RUN AIPS_VERSION:[LOAD]ZSTRTA
$!
$! Default setup is secondary user of TV #1 and TEK #1
$! The TV and TEK to use are logical names TVDEV1 and TEKTK1
$!
$ DEFINE/NOLOG TKDEV SECONDARY1
$!
$! AIPS1 is primary user of TV #1 and TEK #1
$! Could also use dedicated message terminal
$!
$ IF F$GETJPI("","PRCNAM").NES."AIPS1" THEN GOTO NOT_AIPS1
$     DEFINE/NOLOG TKDEV PRIMARY1
$!****     DEFINE/NOLOG/USER_MODE TASK_OUT TASKTT1
$ NOT_AIPS1:
$!
$! Ask the user which TV if there is more than 1
$!
$ NTV = "1"
$ IF REMOTE THEN GOTO NO_NTV
$ IF (F$TRNLNM("TVDEV2").EQS."") THEN GOTO NO_NTV
$ INQUIRE NTV "Enter number of desired TV device"
$ IF (NTV.EQS."") THEN NTV = "1"
$!                               set load area for AIPS
$ XTV = "]"
$ IF (NTV.EQS."2") THEN XTV = ".ALT2]"
$ IF (NTV.EQS."3") THEN XTV = ".ALT4]"
$ IF (NTV.EQS."4") THEN XTV = ".ALT6]"
$ IF (TheDir.NES."") THEN TheDir = "AIPS_VERSION:[LOAD" + XTV
$ NO_NTV:
$ IF (NTV.EQS."1") THEN DEF/NOLOG TVDEV PRIMARY1
$ IF (NTV.EQS."2") THEN DEF/NOLOG TVDEV PRIMARY2
$ IF (NTV.EQS."3") THEN DEF/NOLOG TVDEV PRIMARY3
$ IF (NTV.EQS."4") THEN DEF/NOLOG TVDEV PRIMARY4
$!
$! Remote terminal - no TV access, redefine TEK #1 terminal
$!
$ IF .NOT.REMOTE THEN GOTO NOT_REMOTE
$     DEF/NOLOG TVDEV NONE0
$     DEF/NOLOG TKDEV REMOTE
$     DEFINE/NOLOG/USER_MODE TEKTK11 SYS$OUTPUT
$     DEFINE/NOLOG/USER_MODE TEKTK12 SYS$OUTPUT
$     DEFINE/NOLOG/USER_MODE TEKTK13 SYS$OUTPUT
$     DEFINE/NOLOG/USER_MODE TEKTK14 SYS$OUTPUT
$     DEFINE/NOLOG/USER_MODE TEKTK15 SYS$OUTPUT
$     DEFINE/NOLOG/USER_MODE TEKTK16 SYS$OUTPUT
$ NOT_REMOTE:
$!
$! Start up AIPS
$!
$ DATE = F$TRNLOG("AIPS_VERSION")
$ DATE = F$TRNLOG(DATE)
$ INDX = f$locate("]", DATE) - 8
$ DATE = f$extract(INDX, 8, DATE)
$ WRITE SYS$OUTPUT "Starting ",DATE," version of AIPS"
$!
$ DEFINE/NOLOG/USER_MODE SYS$INPUT SYS$COMMAND
$ RUN/'DEBUG' 'TheDir'AIPS
$!
$! Clean up
$!
$ FINI:
$ SET NOON
$ SET PROCESS/NAME="''PRCNAM'"
$ IF .NOT. $STATUS THEN SET PROCESS/NAME='F$TRNLNM("TT")'
$ SET PROTECTION=('Old_protection')/DEFAULT
$ SET ON

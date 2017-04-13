$! GRIPR.COM
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
$! Start up, run and shutdown GRIPR.
$! Inputs:
$!   Pn  Options - following allowed
$!       OLD, NEW or TST to select version
$!       DEBUG to run with debugger
$!       LOCAL to run a local version of GRIPR (exe in current default).
$!-----------------------------------------------------------------------
$!
$! Make AIPS users one big happy family.
$!
$  SET protection=(S:RWED,O:RWED,G:RWED,W:RWED) /DEFAULT
$!
$! Default options
$!
$ DEBUG = "NODEBUG"
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
$    IF OPT.EQS."DEBUG" THEN DEBUG = "DEBUG"
$    IF OPT.EQS."LOCAL" THEN TheDir = ""
$ GOTO OPTIONS
$!
$ POPS:
$! Start up AIPS
$!
$ DATE = F$TRNLOG("AIPS_VERSION")
$ DATE = F$TRNLOG(DATE)
$ DATE = F$ELEMENT(1, ".", DATE)
$ WRITE SYS$OUTPUT "STARTING ",DATE," VERSION OF GRIPR"
$!
$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ RUN/'DEBUG' 'TheDir'GRIPR
$!
$! Clean up
$!
$ FINI:
$ SET PROTECTION=(S:RWED,O:RWED,G:RWED,W:RE) /DEFAULT

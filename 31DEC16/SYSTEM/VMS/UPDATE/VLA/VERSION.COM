$! VERSION.COM
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
$!  Update a version of AIPS (NEW or TST).
$!  VLA version does VAX3 also
$!
$! Modified somewhat by Pat Murphy 870112: better error control, messages
$!----------------------------------------------------------------------
$!
$ Write sys$output "*** VERSION starting, AIPS ",f$trnlnm("AIPS_VERSION")
$!
$ NumTrys  == 0                       ! Number of tries for today.
$ LinkErrs == 0                       ! Bad links are not fatal, but we
$                                     ! count them. Tested in end.
$ set def Aips_Version:[update]
$ area = f$trnlnm("AIPS_VERSION")
$ area = f$trnlnm(area)
$ area = f$parse(area,,,"DIRECTORY",)
$ VerDate == f$element(1,".",area)    ! This won't work if AIPS is not
$                                     ! the main directory.
$!
$ StartTime == f$time()               ! Time we copied control files.
$ if f$extract(0,1,StartTime).eqs." " then StartTime[0,1] :== "0"
$ datetime = F$Time()
$ date = F$Cvtime(datetime,"ABSOLUTE","DATE")
$ date = date - "-" - "-"             ! remove dashes
$ if f$extract(0,1,date) .eqs. " " then date[0,1] := "0"
$ runtime == date
$!
$ Create 'runtime'.log
$ Open /Append updlogfile 'runtime'.log
$ @UPD_PROC:cleanup                   ! Cleanup any garbage from last time.
$LOOP:
$!   Close/err=NEXT updlogfile         ! Close last logfile if open.
$NEXT:                                ! Do it quietly (no error messages)
$   @UPD_PROC:checktime               ! Check to see if too late or wrong day.
$   If (runtime .EQS. "wrongtime") Then GoTo WRONGTIME
$   Write sys$output "*** VERSION starting loop, NumTrys = ",NumTrys
$   on ERROR then GOTO LOOP
$   @UPD_PROC:control                ! Copy control files, sort, process.
$   @UPD_PROC:remove                 ! Delete all files deleted from CVAX.
$   @UPD_PROC:copy                   ! Copy all modified files from CVAX.
$!END LOOP
$!
$ Set NoOn                            ! Clear On Error.
$ Set On
$ on ERROR then GOTO QUITREP
$ @UPD_PROC:comupd                   ! Do compiles, links, etc.
$ TERMINATE :== "OKAY"
$ @UPD_PROC:end TERMINATE            ! Update control files.
$ @UPD_PROC:vax3                     ! Copy new stuff to VAX3.
$ Close updlogfile
$ EXIT
$WRONGTIME:
$ Write sys$output "*** VERSION: found wrong time, quitting!"
$QUITREP:
$ TERMINATE :== "FAIL"
$ @UPD_PROC:end TERMINATE            ! Update control files.
$ Close updlogfile
$ EXIT

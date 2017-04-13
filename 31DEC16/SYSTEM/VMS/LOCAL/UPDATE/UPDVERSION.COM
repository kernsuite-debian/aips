$! UPDVERSION.COM
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
$! Update a version of AIPS (NEW or TST).
$!
$! Modified somewhat by Pat Murphy 870112: better error control, messages
$!----------------------------------------------------------------------
$!
$ Write sys$output "*** UPDVERSION starting, AIPS ",f$trnlnm("AIPS_VERSION")
$!
$ NumTrys  == 0                       ! Number of tries for today.
$ LinkErrs == 0                       ! Bad links are not fatal, but we
$                                     ! count them. Tested in updend.
$ set def Aips_Version:[update]
$ area = f$trnlnm("AIPS_VERSION")
$ area = f$trnlnm(area)
$ area = f$parse(area,,,"DIRECTORY",)
$ VerDate == f$element(1,".",area)    ! This won't work if AIPS is not
$                                     ! the main directory.
$ @UpdVms:updcleanup                  ! Cleanup any garbage from last time.
$LOOP:
$   Close/err=NEXT updlogfile         ! Close last logfile if open.
$NEXT:                                ! Do it quietly (no error messages)
$   @UpdVms:updchecktime              ! Check to see if too late or wrong day.
$   If (runtime .EQS. "wrongtime") Then GoTo WRONGTIME
$   Write sys$output "*** UPDVERSION starting loop, NumTrys = ",NumTrys
$   Open /Write updlogfile 'runtime'.log
$   StartTime == f$time()             ! Time we copied control files.
$   if f$extract(0,1,StartTime).eqs." " then StartTime[0,1] :== "0"
$   on ERROR then GOTO LOOP
$   @UpdVms:updcontrol                ! Copy control files, sort, process.
$   @UpdVms:updremove                 ! Delete all files deleted from CVAX.
$   @UpdVms:updcopy                   ! Copy all modified files from CVAX.
$!END LOOP
$!
$ Set NoOn                            ! Clear On Error.
$ Set On
$ @UpdVms:updcomlnk                   ! Do compiles, links, etc.
$ @UpdVms:updend                      ! Update control files.
$ @UpdVms:updvax3                     ! Copy new stuff to VAX3.
$ Close updlogfile
$ EXIT
$WRONGTIME:
$ Write sys$output "*** UPDVERSION: found wrong time, quitting!"
$ EXIT

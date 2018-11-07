$! VERCOPY.COM
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
$!   Do the copy portion of the update for a version of AIPS (NEW or
$!   TST).  Called by COPYONLY.
$!----------------------------------------------------------------------
$!
$ Set Noon
$ NumTrys  == 0                       ! Number of tries for today.
$ LinkErrs == 0                       ! Bad links are not fatal, but we
$                                     ! count them. Tested in end.
$ set def Aips_Version:[update]
$ area = f$trnlnm("AIPS_VERSION")
$ area = f$trnlnm(area)
$ area = f$parse(area,,,"DIRECTORY",)
$ VerDate == f$element(1,".",area)    ! This won't work if AIPS is not
$                                     ! the main directory.
$ @UpdVla:cleanup                     ! Cleanup any garbage from last time.
$ LOOP:
$   Close updlogfile                  ! Close last logfile if open.
$   @UpdVla:checktime                 ! Check to see if too late or wrong day.
$   If (runtime .EQS. "wrongtime") Then EXIT
$   Open /Write updlogfile 'runtime'.log
$   If ($status) Then StartTime == f$time() ! Time we copied control files.
$   If ($status) Then @UpdVla:control  ! Copy control files, sort, process.
$   If ($status) Then @UpdVla:copy     ! Copy all modified files from CVAX.
$!**   If ($status) Then @UpdVla:comlnk   ! Do compiles, links, etc.
$   If ($status) Then @UpdVla:end         ! Update control files.
$ If (.Not. $status) Then GOTO LOOP       ! Retry if error.
$!** @UpdVla:vax3                      ! Copy new stuff to VAX3.
$ Close updlogfile
$ EXIT

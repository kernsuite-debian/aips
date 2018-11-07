$! UPDCHECKTIME.COM
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
$! Check to see if too late in the day to try to run or if today is a
$! Saturday or Sunday.  We don't want to break anything on a weekend.
$!-----------------------------------------------------------------------
$!
$ Write sys$output "*** UPDCHECKTIME starting"
$ runtime == "wrongtime"
$ NumTrys == NumTrys + 1
$ If (NumTrys .LT. 9) Then GOTO KEEPTRYING
$   Write sys$output "*** UPDCHECKTIME - too many retries"
$   @UpdVms:upderror
$   EXIT
$ KEEPTRYING:
$ datetime = F$Time()
$ time = F$Extract(12,11,datetime)          ! Not used here???
$ day = F$Cvtime(datetime,"ABSOLUTE","WEEKDAY")
$ If (day .EqS. "Saturday") .OR. (day .EqS. "Sunday") Then EXIT ! Error EXIT
$ date = F$Cvtime(datetime,"ABSOLUTE","DATE")
$ date = date - "-" - "-"                   ! remove dashes
$ if f$extract(0,1,date) .eqs. " " then date[0,1] := "0"
$ runtime == date
$ Write Sys$output "*** UPDCHECKTIME ending"
$ EXIT

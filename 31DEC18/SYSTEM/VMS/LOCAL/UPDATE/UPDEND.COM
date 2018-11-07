$! UPDEND.COM
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
$! Write last good date, do report.
$!----------------------------------------------------------------------
$ Write sys$output "*** UPDEND starting"
$ define/nolog sys$error updend.err
$ Write sys$output "*** Error output diverted to UPDEND.ERR"
$!
$ open /write lastgood aips_version:[update]lastgood.dat
$                        ! Round down to nearest day so we don't have
$                        ! to worry about things changed during update.
$ DateTime = f$extract(0,12,StartTime) + "00:00:00.00"
$ write lastgood DateTime
$ close lastgood
$ write sys$output "LinkErrs = ",LinkErrs
$ if LinkErrs .GT. 0 then -
        mail/subject="Link Errors = ''LinkErrs'" UpdVms:updlinkerr.txt -
        @UpdVms:updpeople.dis
$ if LinkErrs .GT. 0 then Write Sys$Output "*** Link error(s): ",LinkErrs
$ Write sys$output "*** End of UPDEND
$ EXIT

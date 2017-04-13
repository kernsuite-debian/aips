$! UPDERROR.COM
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
$! This procedure will delay 10 min. and, if it is not too late in the
$! day,  exit with an error status so that the main procedure will loop and
$! try again.  The procedure sends messages to various people warning
$! that the update encountered errors.
$!-----------------------------------------------------------------------
$!
$ Write sys$output "*** UPDERROR starting"
$ Define/nolog sys$error upderror.err
$ Write sys$output "*** Error output diverted to UPDERROR.ERR"
$!
$ Set Noon
$ Wait 00:10:00.0
$!
$! NB only need "@" sign on the distribution list (PPM 870112)
$!
$ mail/subject="update errors" UpdVms:upderror.txt @UpdVms:updpeople.dis
$!
$! The EXIT 42 will cause "%SYSTEM-E-ABORT, abort" message to appear.
$!
$ EXIT 42

$! UPDLINKONLY.COM
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
$!   Do the link part of the midnight job only.  Copy part must have
$!   finished (through UPDCOPYONLY or partly finished UPDJOBMAIN).
$!----------------------------------------------------------------------
$ Set NoOn
$ Define FPS  dba0:[FPS]
$ if (f$trnlnm("AIPS_STARTUP",,,,,).eqs."") then -
     @nrao$root:[aips]assnstart define
$ @aips_startup:aipsprog              ! Get latest process logicals.
$ Define UpdVms TST:[system.vms.local.update]     ! define proc area
$!
$ Version TST
$ @UpdVms:updverlink
$ Version NEW
$ @UpdVms:updverlink
$ EXIT

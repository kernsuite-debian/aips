$! UPDJOBMAIN.COM
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
$!   This job runs once a night to update the AIPS program files at the
$!   VLA to correspond to AIPS files on CVAX.
$!
$!   See also UPDCOPYONLY, UPDLINKONLY for partial jobs
$!
$!   Contains VLA-local directory NRAO$ROOT.
$!----------------------------------------------------------------------
$ Set NoOn
$ oldver = f$verify (0)
$ Define FPS  dba0:[FPS]
$ if (f$trnlnm("AIPS_STARTUP",,,,,).eqs."") then -
     @nrao$root:[aips]assnstart define
$ @aips_startup:aipsprog              ! Get latest process logicals.
$ Define UpdVms TST:[system.vms.local.update]     ! define proc area
$                                     ! Resubmit for same time tomorrow.
$ Submit/After="tomorrow+03:00:00.00"  - 
        /Log_File=TST:[update]updjobmain.log  -
        /NoPrint -
    UpdVms:updjobmain.com
$!
$ Version TST
$ @UpdVms:updversion
$ Version NEW
$ @UpdVms:updversion
$ Version TST
$ if (runtime .nes. "wrongtime") then @UpdVms:updreport
$ oldver = f$verify (oldver)
$ EXIT

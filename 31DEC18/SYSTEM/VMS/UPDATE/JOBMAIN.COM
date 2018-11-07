$! JOBMAIN.COM
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
$!   remote site to correspond to AIPS files on CVAX.
$!
$!   See also COPYONLY, LINKONLY for partial jobs
$!
$!   Contains  a special directory we call nrao$root which must be
$!   known if AIPS_STARTUP is not.
$!----------------------------------------------------------------------
$ Set NoOn
$ oldver = f$verify (0)
$ Define FPS  dba0:[FPS]
$ if (f$trnlnm("AIPS_STARTUP",,,,,).eqs."") then -
     @NRAO$ROOT:[aips]assnstart define
$ @aips_startup:aipsprog              ! Get latest process logicals.
$                                     ! Resubmit for same time tomorrow.
$ Submit/After="tomorrow+01:00:00.00"  -
        /Log_File=TST:[update]jobmain.log  -
        /NoPrint -
    UPD_PROC:jobmain.com
$!
$ Version TST
$ @UPD_PROC:version
$ if (runtime .nes. "wrongtime") then @UPD_PROC:report
$ Version NEW
$ @UPD_PROC:version
$ if (runtime .nes. "wrongtime") then @UPD_PROC:report
$ Version TST
$ oldver = f$verify (oldver)
$ EXIT

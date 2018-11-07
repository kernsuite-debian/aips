$! END.COM
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
$! Modified 871228 by Pat Murphy [PPM] NRAO/VLA: loop over distrib. list
$!----------------------------------------------------------------------
$ Write sys$output "*** END starting"
$ define/nolog sys$error end.err
$ Write sys$output "*** Error output diverted to END.ERR"
$!
$ if 'P1'.nes."OKAY" then GOTO NOTOK
$ copy aips_version:[update]lastgood.tmp aips_version:[update]lastgood.dat
$!
$NOTOK:
$ write sys$output "LinkErrs = ",LinkErrs
$ if LinkErrs .le. 0 then goto ENDIT
$!
$! loop over names in distribution list, in case of net problems
$!
$ open/read people_list UPD_PROC:people.dis
$ on error then goto LOOP
$LOOP:
$   read/end=ENDLOOP people_list Wizard
$   Wizard = F$Edit(Wizard, "Trim, Collapse, UnComment")   ! preserve lowercase
$   if Wizard .eqs. "" .or. Wizard .eqs. " " then goto LOOP
$   mail/subject="Link Errors = ''LinkErrs'" UPD_PROC:linkerr.txt 'Wizard'
$   goto LOOP
$ENDLOOP:
$ Set NoOn
$ Write Sys$Output "*** Link error(s): ",LinkErrs
$ENDIT:
$ Write sys$output "*** End of END [!]"
$ EXIT

$! REPORT.COM
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
$! This procedure will generate a report of what has been updated on
$! the VLA VAXs.
$!-----------------------------------------------------------------------
$                             ! write as much of the report as possible.
$ Write sys$output "*** REPORT starting"
$ set NoOn
$ define/nolog sys$error report.err
$ write sys$output "*** Error output diverted to REPORT.ERR"
$ Create rep'runtime'.doc
$ Open /Append report rep'runtime'.doc
$ write report "AIPS NIGHT TIME UPDATE REPORT FOR ",f$cvtime(,"ABSOLUTE","DATE")
$ write report "---------------------------------------------------------"
$ write report "1.  Changes to the ''P1' area"
$ write report ""
$ write report "1.1.  Copies and compiles:"
$ write report ""
$ close report
$ append 'runtime'.log rep'runtime'.doc
$ write sys$Output "*** Appended ''P1':''runtime'.LOG to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ write report "1.2.  Reasons given for file updates:"
$ write report ""
$ close report
$ append reasons.unq rep'runtime'.doc
$! @UPD_PROC:append rep'runtime'.doc  reasons.unq
$ Write sys$output "*** Appended REASONS.UNQ to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ Write report "2.  CHANGE.DOC Entries"
$ Write report ""
$                             ! Find differences in CHANGE.DOC since the
$                             ! last time.
$ on ERROR then GOTO DIFERR
$ Differ/nonumber/Output=temp.doc lastgood.doc hist:change.doc
$ Open/Read temp temp.doc
$LOOP:
$SKIP_UNTIL_STARS:
$     Read /END_OF_FILE=END_LOOP temp line
$     If line .EQS. "******" Then GOTO END_SKIP
$     GoTo SKIP_UNTIL_STARS
$END_SKIP:
$      Read /END_OF_FILE=END_LOOP temp line  ! Throw away file name.
$      Write report "************"           ! Marks start of entry.
$COPY_UNTIL_STARS:
$     Read /END_OF_FILE=END_LOOP temp line
$     If line .EQS. "************" Then GOTO LOOP
$     Write report line             ! If DEC changes the report
$     GoTo COPY_UNTIL_STARS         ! format this quits working.
$END_LOOP:
$ Close temp
$ Delete TEMP.DOC;*
$ copy hist:change.doc lastgood.doc
$ GOTO ENDREP
$DIFERR:
$ Write sys$output "*** REPORT error in differencing CHANGE.DOC files"
$ Write report ""
$ Write report "**************************************"
$ Write report ""
$ Write report "Error in differencing CHANGE.DOC files"
$ Write report ""
$ Write report "**************************************"
$ Write report ""
$ENDREP:
$ Write report ""                               ! Add a few blank lines.
$ Write report "(End of AIPS ""midnight"" job report)"
$ Close report
$ Write sys$output "*** REPORT finished."
$ EXIT

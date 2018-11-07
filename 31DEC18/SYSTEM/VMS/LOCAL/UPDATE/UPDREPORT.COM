$! UPDREPORT.COM
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
$ Write sys$output "*** UPDREPORT starting"
$ set NoOn
$                             ! one report in tst.
$ set default tst:[update]
$ define/nolog sys$error updreport.err
$ write sys$output "*** Error output diverted to UPDREPORT.ERR"
$!
$! I think all the UPDAPPEND stuff can be eliminated and the APPEND command
$! used if you do the following instead of what is done now:
$!
$!     CREATE REP'runtime'.DOC
$!     Open/Append report rep'runtime'.doc
$!     ....
$!     Append rep'runtime'.doc 'runtime'.log (or maybe the other way around)
$!
$! I've done this in my own command procedures and I know it works.
$! Sometime I'll change it.  870112 Pat Murphy.
$!
$ Open /Write report rep'runtime'.doc
$                             ! Do report header.
$ write report "NIGHT TIME UPDATE REPORT FOR ",f$time()
$ write report ""
$ write report "Changes to the TST area"
$ write report ""
$ write report "Copies and compiles:"
$ write report ""
$ close report
$ @UpdVms:updappend rep'runtime'.doc 'runtime'.log
$ write sys$Output "*** Appended TST:''runtime'.LOG to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ write report "REASONS GIVEN FOR FILE UPDATES"
$ write report ""
$ close report
$ @UpdVms:updappend rep'runtime'.doc  reasons.unq
$ Write sys$output "*** Appended TST:REASONS.UNQ to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ write report "Files copied to VAX3"
$ write report ""
$ close report
$ if f$search("VAX3HLP.LOG") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc vax3hlp.log
$ if f$search("VAX3LOAD.LOG") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc vax3load.log
$ if f$search("VAX3ALT1.LOG") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc vax3alt1.log
$ write sys$output "*** Appended TST:VAX3 stuff to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ write report "Changes to the NEW area"
$ write report ""
$ write report "Copies and compiles:"
$ write report ""
$ close report
$ @UpdVms:updappend rep'runtime'.doc NEW:[update]'runtime'.log -
     NEW:[update]reasons.unq
$ write sys$output "*** Appended NEW: stuff to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ write report "Files copied to VAX3"
$ write report ""
$ close report
$ if f$search("new:[update]vax3hlp.log") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc new:[update]vax3hlp.log
$ if f$search("new:[update]vax3load.log") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc new:[update]vax3load.log
$ if f$search("new:[update]vax3alt1.log") .nes. "" then -
      @UpdVms:updappend rep'runtime'.doc new:[update]vax3alt1.log
$ write sys$output "*** Appended NEW:VAX3 stuff to report"
$ Open /append report rep'runtime'.doc
$ write report ""
$ Write report "CHANGE.DOC Entries"
$ Write report ""
$                             ! Find differences in CHANGE.DOC since the
$                             ! last time.
$ on ERROR then GOTO DIFERR
$ Differ/nonumber/Output=temp.doc lastgood.doc tst:[hist]change.doc
$ on ERROR then GOTO DIFERR
$ Open/Read temp temp.doc
$ on ERROR then GOTO DIFERR
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
$     Write report line           ! If DEC changes the report
$     GoTo COPY_UNTIL_STARS         ! format this quits working.
$END_LOOP:
$ Close temp
$ Del TEMP.DOC;*
$ GOTO ENDREP
$DIFERR:
$ Write sys$output "*** UPDREPORT error in differencing CHANGE.DOC files"
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
$ copy hist:change.doc lastgood.doc
$ Write sys$output "*** UPDREPORT finished."
$ EXIT

$! COPY.COM
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
$!   Copy all CVAX control files containing lists of modified files
$!   and then copy all CVAX files in these lists.
$!   Signals:
$!      $status
$!-----------------------------------------------------------------------
$ Write Sys$Output "*** COPY starting"
$ @UPD_PROC:config
$ define/nolog sys$error copy.err
$ Write sys$Output "*** Error output diverted to COPY.ERR"
$ on ERROR then GOTO ERROREXIT
$                                ! Copy most files put back with
$                                ! the AIPS checkout system.
$ Scratch1 :== "''Updtempdir'COPY.LST"
$ Open/Write Scratch 'Scratch1'
$ sonc := syntax_only,no_conceal
$ Open/Read putbck  putbck.unq
$!
$LOOP_UNTIL_EOF:
$      Read/End_Of_File=END_LOOP Putbck Line
$      Line = F$Edit(Line,"Compress,Trim")
$      Area = F$Element(4," ",Line)
$      Logn = F$Trnlnm(Area)
$!                                              Normal [AIPS.date...]
$      If (F$Locate("AIPS_VERSION", Logn).Ne.0) Then Goto Next
$         Dirc = Logn - "AIPS_VERSION:["
$         Vers = F$Trnlnm(F$Trnlnm("AIPS_VERSION"))
$         Indx = F$Locate(".15", Vers) + 1
$         Date = F$Extract(Indx, 8, Vers)
$         Logn = "AIPS_ROOT:[" + Date + Dirc
$         Cvaxpath = "''Cvaxaipsroot'[''Date'''Dirc'" - "]["
$         Goto Done
$!                                              Normal [AIPS.TEXT...]
$Next:
$      If (F$Locate("AIPS_ROOT", Logn).Ne.0) Then Goto Done
$         Dirc = Logn - "AIPS_ROOT:["
$         Cvaxpath = "''Cvaxaipsroot'[''Dirc'" - "]["
$Done:
$      File = F$Element(5," ",Line)
$      Localfullname = Logn + File
$      Write Scratch  "''Cvaxpath' ''File' ''Localfullname'"
$      GoTo LOOP_UNTIL_EOF
$END_LOOP:
$ Close Putbck
$ Close Scratch
$ @UPD_PROC:Copysub 'Scratch1'
$ SET NOON
$ Delete 'Scratch1';*
$!
$ copy aips_version:[update]lastgood.tmp aips_version:[update]lastcopy.dat
$ write Sys$Output "Created new LastCopy.DAT "
$ write updlogfile "Created new LastCopy.DAT "
$!
$ Write Sys$Output "*** COPY finished."
$ EXIT
$!
$ERROREXIT:
$ close putbck
$ Write Sys$Output "*** COPY finished with errors."
$ EXIT %X1000002C

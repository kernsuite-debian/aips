$! CONTROL.COM
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
$! Copy all CVAX control files containing lists of modified files,
$! sort them and get rid of duplicates and entries already processed.
$!      Modified by DCW, NRAO-CV, 17July87.
$! Signals:
$!   $status
$!-----------------------------------------------------------------------
$ Write Sys$Output "*** CONTROL starting"
$ @UPD_PROC:Config
$ Define/Nolog SYS$ERROR 'Updtempdir'CONTROL.ERR
$ Write Sys$Output "*** Error output diverted to ''Updtempdir'CONTROL.ERR"
$                                ! Copy all control files.
$ Scratch1 :== "''Updtempdir'CONTROL.LST"
$ Open/Write Scratch 'Scratch1'
$ Write Scratch "[AIPS.''VerDate'.UPDATE] COMRPL.UPD COMRPL.UPD"
$ Write Scratch "[AIPS.''VerDate'.UPDATE] COMLNK.UPD COMLNK.UPD"
$ Write Scratch "[AIPS.''VerDate'.UPDATE] PUTBCK.UPD PUTBCK.UPD"
$ Write Scratch "[AIPS.''VerDate'.UPDATE] REASONS.UPD REASONS.UPD"
$ Write Scratch "[AIPS.''VerDate'.UPDATE] REMOVE.UPD REMOVE.UPD"
$ Close Scratch
$ @UPD_PROC:Copysub 'Scratch1'
$ Delete 'Scratch1';*
$ Write Sys$output "*** CONTROL finished copying, start sorting"
$                                ! Sort Them
$ @UPD_PROC:Srtunq COMRPL  LASTCOMRPL
$ @UPD_PROC:Srtunq COMLNK  LASTCOMLNK
$ @UPD_PROC:Srtunq PUTBCK  LASTCOPY
$ @UPD_PROC:Srtunq REASONS LASTCOPY
$ @UPD_PROC:Srtunq REMOVE  LASTREMOVE
$!
$ Write sys$output "*** CONTROL finished."
$ EXIT

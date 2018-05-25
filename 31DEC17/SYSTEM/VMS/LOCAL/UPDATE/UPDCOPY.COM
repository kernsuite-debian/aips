$! UPDCOPY.COM
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
$ Write Sys$Output "*** UPDCOPY starting"
$ define/nolog sys$error updcopy.err
$ Write sys$Output "*** Error output diverted to UPDCOPY.ERR"
$ on ERROR then GOTO ERROREXIT
$                                ! Copy most files put back with
$                                ! the AIPS checkout system.
$ sonc := syntax_only,no_conceal
$ Open/Read putbck  putbck.unq
$!
$LOOP_UNTIL_EOF:
$   Read/END_OF_FILE=END_LOOP putbck line
$   line = f$edit(line,"COMPRESS,TRIM")
$   fulname = f$element(4," ",line) + ":" + f$element(5," ",line)
$   file = f$parse(fulname,,,"directory",sonc) -
       + f$parse(fulname,,,"name",sonc)-
       + f$parse(fulname,,,"type",sonc) - "]["
$   @UpdVms:updcopysub cvax::uma0:'file' disk$res:'file'
$   GoTo LOOP_UNTIL_EOF
$END_LOOP:
$ close putbck
$ Write Sys$Output "*** UPDCOPY finished."
$ EXIT
$!
$ERROREXIT:
$ close putbck
$ Write Sys$Output "*** UPDCOPY finished with errors."
$ EXIT %X1000002C

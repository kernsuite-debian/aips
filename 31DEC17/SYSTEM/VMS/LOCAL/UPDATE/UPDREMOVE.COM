$! UPDREMOVE.COM
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
$!   Remove all files that have been deleted since last update.
$!
$!   Current version deletes the text file and tries to delete the module
$!   from the link library when appropriate.
$!-----------------------------------------------------------------------
$ Write sys$output "*** UPDREMOVE starting."
$ define/nolog sys$error updremove.err
$ Write sys$output "*** Error output diverted to UPDREMOVE.ERR"
$!
$ open/read remove remove.unq
$ on error then GOTO ERRORHANDLER
$ sonc := syntax_only,no_conceal
$!
$ READLOOP:
$   read/end_of_file=ENDREADLOOP remove line
$   line   = f$edit(line,"COMPRESS,TRIM")
$   subdir = f$element(4," ",line)
$   name   = f$element(5," ",line)
$   file   = subdir + ":" + name
$   type   = f$parse(file,,,"type")
$   if f$search(file) .eqs. "" then goto FILE_NOT_FOUND
$     delete 'file';*
$     write updlogfile "Deleted      : ", file
$     GoTo CHECK_TYPE
$FILE_NOT_FOUND:
$     write updlogfile "Can't delete : ", file, " - not found."
$CHECK_TYPE:
$!                                          remove from library
$   if ((type.nes.".FOR") .and. (type.nes.".MAR")) Then GOTO READLOOP
$     name = f$parse(file,,,"name",sonc)
$     Set Noon
$!
$! Simpler scheme entered 870112 by Pat Murphy: add "lib" to subdir name
$!  and catch exceptions.  Must check if library exists too!
$!
$     LibName = subdir
$     if  (subdir.eqs."APLGEN") then LibName = "APLVMS"
$     if  (subdir.eqs."APLNVMS") then LibName = "APLNOT"
$     if  (subdir.eqs."QPSAP")  then LibName = "QVMS"
$     if ((subdir.eqs."QFPS16") .or. -
          (subdir.eqs."QFPS"))  then LibName = "Q120B"
$     if ((subdir.eqs."YGEN") .or. -
          (subdir.eqs."YIIS"))  then LibName = "YM70"
$!
$     LibName = "Libr:" + LibName + "LIB"
$     if f$search("''LibName'.OLB") .eqs. "" then goto RESET_ERROR
$        Libr/Delete=('name')/log 'LibName'
$        Write UpdLogFile "Removed module ",name," from ",LibName
$!
$! I'm leaving the original code here for safekeeping, just commented out
$!
$!     if (subdir.eqs."AIPSUB") Then -
$!         libr/delete=('name')/log libr:aipsublib
$!     if (subdir.eqs."APLSUB") Then -
$!         libr/delete=('name')/log libr:aplsublib
$!     if (subdir.eqs."APLNOT") Then -
$!         libr/delete=('name')/log libr:aplnotlib
$!     if ((subdir.eqs."APLGEN") .or. (subdir.eqs."APLVMS")) Then -
$!         libr/delete=('name')/log libr:aplvmslib
$!     if (subdir.eqs."QSUB") Then -
$!         libr/delete=('name')/log libr:qsublib
$!     if (subdir.eqs."QNOT") Then -
$!         libr/delete=('name')/log libr:qnotlib
$!     if ((subdir.eqs."QVMS") .or. (subdir.eqs."QPSAP")) Then -
$!         libr/delete=('name')/log libr:qvmslib
$!     if ((subdir.eqs."QFPS") .or. (subdir.eqs."QFPS16") .or. -
$!          (subdir.eqs."Q120B")) Then -
$!         libr/delete=('name')/log libr:q120blib
$!     if (subdir.eqs."YSUB") Then -
$!         libr/delete=('name')/log libr:ysublib
$!     if (subdir.eqs."YNOT") Then -
$!         libr/delete=('name')/log libr:ynotlib
$!     if ((subdir.eqs."YGEN") .or. (subdir.eqs."YIIS") .or. -
$!          (subdir.eqs."YM70")) Then -
$!         libr/delete=('name')/log libr:ym70lib
$!
$RESET_ERROR:
$     Set On
$     on error then GOTO ERRORHANDLER
$     GoTo READLOOP
$!
$ERRORHANDLER:
$  write updlogfile "Could not delete : ", file
$  on error then GOTO ERRORHANDLER
$  GoTo READLOOP
$!
$ENDREADLOOP:
$ close remove
$ write sys$output "*** UPDREMOVE finished."
$!
$ EXIT

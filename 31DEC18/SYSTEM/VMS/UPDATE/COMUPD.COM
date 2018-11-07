$! COMUPD.COM
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
$!   This procedure will compile and link all routines found in the
$!   control files copied from cvax.
$!-----------------------------------------------------------------------
$ write sys$output "*** COMUPD starting"
$ define/nolog sys$error comlnk.err
$ Write sys$output "*** Error output diverted to COMUPD.ERR"
$                                            ! Do all subroutines.
$ open /read sublist comrpl.unq
$ on error then GOTO COMPILEERROR
$ READLOOP:
$   read/end_of_file=ENDREADLOOP sublist line
$   line = f$edit(line,"COMPRESS,TRIM")
$   file = f$element(4," ",line) + ":" + f$element(5," ",line)
$   comrpl 'file' dirty nolist
$   write updlogfile "Compiled  : ", file
$ GOTO READLOOP
$ ENDREADLOOP:
$ close sublist
$!
$ copy aips_version:[update]lastgood.tmp aips_version:[update]lastcomrpl.dat
$ write Sys$Output "Created new LastComrpl.DAT "
$ write updlogfile "Created new LastComrpl.DAT "
$!
$                                            ! Do all programs.
$ open /read proglist comlnk.unq
$ on error then GOTO ERRORHANDLER
$ READLOOP2:
$   read/end_of_file=ENDREADLOOP2 proglist line
$   line = f$edit(line,"COMPRESS,TRIM")
$   file = f$element(4," ",line) + ":" + f$element(5," ",line)
$   comlnk 'file' dirty nomap nolist
$   write updlogfile "Linked    : ", file
$ GOTO READLOOP2
$!
$ERRORHANDLER:
$  LinkErrs :== LinkErrs + 1
$  write updlogfile "ERROR linking : ", file
$  on error then GOTO ERRORHANDLER
$GOTO READLOOP2
$!
$COMPILEERROR:
$   write updlogfile "*** ERROR ** compiling ",file
$   write sys$output "*** COMRPL found severe compile error!"
$   close sublist
$EXIT %X1000002C
$!
$ENDREADLOOP2:
$ close proglist
$ if (LinkErrs .GT. 0) Then GOTO NOTWORK
$    copy aips_version:[update]lastgood.tmp aips_version:[update]lastcomlnk.dat
$    write Sys$Output "Created new LastComlnk.DAT "
$    write updlogfile "Created new LastComlnk.DAT "
$    write sys$output "*** COMUPD ends successfully."
$    EXIT
$NOTWORK:
$    write sys$output "*** COMUPD ends with ",LinkErrs," errors"
$    EXIT %X1000002C

$!ICREDIR.COM
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
$!---------------------------------------------------------------------
$!  This routine creates all the permanent subdirectories needed for
$!  this version of AIPS.  Version specific directories created by
$!  backup.
$!  INPUTS:
$!    ANSWER1 INT  The number of disk drives.
$!    DATA'I' STR  The data subdirectories on the available disks.
$!---------------------------------------------------------------------
$!
$  write sys$output " "
$  write sys$output "Creating subdirectory structure for AIPS."
$  write sys$output -
     "If you are already running a recent enough version of AIPS some"
$  write sys$output -
     "of these might already exist."
$  TopDir = f$logical("SYS$DISK") + f$directory() - "]"
$  NewDir = TopDir + "." + VERDATE
$  set NoOn
$                                         ! Create permanent directories.
$                                         ! Will already exist for some.
$  I = 0
$  DISKLOOP:
$    I = I + 1
$    Data = AIPSDA0'I'
$    create /directory 'Data'
$  if (I.lt.NUMDISKS) then GOTO DISKLOOP
$!
$  create /directory 'TopDir'.RUN]
$  create /directory 'TopDir'.FITS]
$  create /directory 'TopDir'.LOCAL]
$  create /directory 'TopDir'.LOCAL.SUB]
$  create /directory 'TopDir'.LOCAL.PGM]
$  create /directory 'TopDir'.LOCAL.APGM]
$  create /directory 'TopDir'.LOCAL.HELP]
$  create /directory 'TopDir'.LOCAL.LOAD]
$!
$  create /directory 'NewDir'.SYSTEM.VMS.LOCAL]
$  create /directory 'NewDir'.MEMORY]
$!
$ EXIT

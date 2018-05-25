$! APPEND.COM
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
$!   Append file 'P2' to the end of file 'P1'.  This works when the DCL
$!   APPEND doesn't work because of file format incompatibilities.
$!   Used in the midnight job to build a report file.
$!-----------------------------------------------------------------------
$!
$ open/append outfile 'p1'
$ I = 1
$                                 ! Loop for all "appendees".
$ OUTLOOP:
$   on error then GOTO OUTLOOP
$   I = I + 1
$   infilename = P'I'
$   if infilename.EQS."" then GOTO END_OUTLOOP
$   open/read infile 'infilename'
$                                       ! Loop for all lines in file.
$   INLOOP:
$     read/END_OF_FILE=END_INLOOP infile line
$     write outfile line
$   GOTO INLOOP
$   END_INLOOP:
$   close infile
$!
$ GOTO OUTLOOP
$ END_OUTLOOP:
$ close outfile
$ EXIT

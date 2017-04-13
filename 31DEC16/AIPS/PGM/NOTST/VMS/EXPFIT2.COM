$!   EXPFIT2 procedure to restart EXPFIT part of the way through.
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
$!
$!   Delete AIPSTAPEnnn.LIS;1 and EXPFITnnn.txt;1
$!   for all nnn >= last good file on the tape.
$!   Then issue the command:
$!          @EXPFIT2   tapedev     version
$!   where tapedev is MMA0:, MTA0:  and version is the version ID
$!   i.e.,   @AIPGVMS:EXPFIT2  MTA0:  15JUL88
$!
$ Set Verify
$ Ass 'P1' aipstape
$ Mount/For/Blo=32767/density=1600 AIPSTAPE:
$ Run LOAD:EXPFIT
$ dismount AIPSTAPE:
$ Set NoVerify

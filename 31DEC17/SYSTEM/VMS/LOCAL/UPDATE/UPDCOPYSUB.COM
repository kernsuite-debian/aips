$! UPDCOPYSUB.COM
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
$! Try to copy a file (usually over DECnet) 20 times before giving up.
$! Inputs:
$!   p1    STR    From file name.
$!   p2    STR    To   file name.
$!-----------------------------------------------------------------------
$ Set NoOn
$ tries = 0
$COPYLOOP:
$   tries = tries + 1
$   Copy 'p1' 'p2'
$   If ($status) Then GOTO ENDLOOP
$   If (tries.GT.20) Then GOTO ERROREXIT
$   Wait 00:00:10.0
$   GoTo COPYLOOP
$ENDLOOP:
$ Write updlogfile "Copied    : ",P1,"  To  ",P2
$ Purge 'p2'
$ EXIT
$ERROREXIT:
$ write updlogfile "** ERROR ** Could not copy ", P1
$ EXIT %X1000002C

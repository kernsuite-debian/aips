$!  GTTEX
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
$ set verify
$!-----------------------------------------------------------------------
$!  Use:  @GTTEX   p1    p2
$!  Runs the GRKEY program to produce listings of all gripes or a TEX
$!  output of all "tbc" gripes (p1 = "TEX").
$!  If p2 = "NO" then the *.bit file is kept, but not printed
$!-----------------------------------------------------------------------
$ SET NOON
$ DEASS FOR005
$ SET ON
$ IF (P1.EQS."TEX") THEN GOTO SKIP1
$ R LOAD:GRKEY


T
T
T
aipsgrip:GS%%%%,GT%%%%,GX%%%%
$ las GRKEYALL
$ GOTO SKIP2
$SKIP1:
$ R LOAD:GRKEY
Everyone

T
T
T
aipsgrip:GT%%%%,GX%%%%
$ tex grkeyeveryone
$ dviqms grkeyeveryone
$ las grkeyall.lis
$ IF (P2.EQS."NO") THEN GOTO SKIP2
$ las grkeyeveryone.bit
$SKIP2:
$ dir/size=all/date grk*.*
$ ass TT: FOR005
$ set noverify

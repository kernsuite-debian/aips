$!  GRCHANGE
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
$!  Use:  @GRCHANGE nnnn  x
$!  extracts all gripes having \changed in them in file GTnnnn.TEX and
$!  runs them through TEX with \finalrun{T} or F if x is given as F.
$!  The files must be in the default directory.
$!-----------------------------------------------------------------------
$ SET NOON
$ DEASS FOR005
$ SET ON
$ ass/user GT'p1'.TEX  grinfil
$ ass/user grjunk.TEX  groutfil
$ ass/user GC'p1'.TEX  grtbcfil
$ IF (P2.NES."F") THEN GOTO SKIP1
$ R LOAD:GETTBC



F
F
\changed
$ GOTO SKIP2
$SKIP1:
$ R LOAD:GETTBC



F
T
\changed
$SKIP2:
$ del grjunk.tex.*
$ tex GC'p1'
$ dviqms GC'p1'
$ las GC'p1'.bit
$ del GC'p1'.tex.*,gc'p1'.lis.*,gc'p1'.dvi.*
$ dir/size=all/date gc*.*
$ ass TT: FOR005
$ set noverify

$!  GXTBC
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
$!  Use:  @new:GXTBC nnnn  x
$!  Does: (1) extracts all \tbc's from GXnnnn.TEX to a new GTnnnn.TEX
$!            and all non-\tbc's to a new GSnnnn.TEX,
$!        (2) runs GRCHEK on the output GSnnnn.TEX and GTnnnn.TEX files,
$!        (3) optionally (if x = "T"), runs TEX on the new GTnnnn.TEX.
$!-----------------------------------------------------------------------
$ SET NOON
$ DEASS FOR005
$ SET ON
$!                                get the \tbc's
$ ass/user GX'p1'.TEX  grinfil
$ ass/user GS'p1'.TEX  groutfil
$ ass/user GT'p1'.TEX  grtbcfil
$ R LOAD:GETTBC



T
F
\tbc
$!                               check the files
$ ass/user GS'p1'.TEX  grinfil
$ R LOAD:GRCHEK

$ ass/user GT'p1'.TEX  grinfil
$ R LOAD:GRCHEK

$!                               TEX the new GT file
$ IF (P2.NES."T") THEN GOTO SKIP2
$    tex GT'p1'
$    dviqms GT'p1'
$    las GT'p1'.bit
$    del GT'p1'.dvi.*,GT'p1'.lis.*
$SKIP2:
$ dir/size=all/date *'p1'
$ ass TT: FOR005
$ set noverify

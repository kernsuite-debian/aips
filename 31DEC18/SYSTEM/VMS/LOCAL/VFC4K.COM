$! VFC4K.COM
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
$!  Command procedure to do AP stuff for 4.5 K TROM models.
$!  P1 = name of VFC routine
$!  P2 = name of optional link library
$!  P3 = name of optional link library
$!  P4 = name of optional link library
$!---------------------------------------------------------------------
$!                                       Create a temporay file to
$!                                       hold inputs.
$ ASS SYS$OUTPUT    FOR001
$ ASS JUNKVFC.DAT   FOR005
$ ASS SYS$OUTPUT    FOR006
$!
$ OPEN /WRITE TEMP  JUNKVFC.DAT
$ WRITE TEMP P1, ".VFC"
$ WRITE TEMP P1, ".AP"
$ CLOSE TEMP
$ ASS UMA0:[FPS.OLD] FPSOLD
$ RUN FPSOLD:VFC
$!
$ OPEN /WRITE TEMP  JUNKVFC.DAT
$ WRITE TEMP P1, ".AP"
$ WRITE TEMP P1, ".APO"
$ WRITE TEMP P1, ".LIS"
$ WRITE TEMP "N"
$ CLOSE TEMP
$ RUN FPSOLD:APAL
$ DEL JUNKVFC.DAT;*
$!
$ OPEN /WRITE TEMP  JUNKVFC.DAT
$ WRITE TEMP "L"
$ WRITE TEMP P1, ".APO"
$ IF (P2.NES."") THEN WRITE TEMP "L"
$ IF (P2.NES."") THEN WRITE TEMP P2
$ IF (P3.NES."") THEN WRITE TEMP "L"
$ IF (P3.NES."") THEN WRITE TEMP P3
$ IF (P4.NES."") THEN WRITE TEMP "L"
$ IF (P4.NES."") THEN WRITE TEMP P4
$ WRITE TEMP "L"
$ WRITE TEMP "Q120B:NRAO.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSOLD:BAALIB.4K"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSOLD:BABLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSOLD:APFLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSOLD:UTLLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSOLD:SYMLIB.4K"
$ WRITE TEMP "A"
$ WRITE TEMP P1, ".FOR"
$ WRITE TEMP "X"
$ CLOSE TEMP
$ RUN FPSOLD:APLINK
$!
$ DEL JUNKVFC.DAT;*
$ DEASS FOR005
$ EXIT

$! FLINK4K.COM
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
$!  Command procedure link routines from a library. (4.5 K TROM version
$!  P1 = name of routine
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
$!  FPSLIB needs to be assigned to the correct directory for VFC, APAL
$!  and the AP microcode libraries.
$!
$ ASS UMA0:[FPS.OLD] FPSLIB    
$!
$!
$ OPEN /WRITE TEMP  JUNKVFC.DAT
$ WRITE TEMP "F"
$ WRITE TEMP P1
$ IF (P2.NES."") THEN WRITE TEMP "L"
$ IF (P2.NES."") THEN WRITE TEMP P2
$ IF (P3.NES."") THEN WRITE TEMP "L"
$ IF (P3.NES."") THEN WRITE TEMP P3
$ IF (P4.NES."") THEN WRITE TEMP "L"
$ IF (P4.NES."") THEN WRITE TEMP P4
$ WRITE TEMP "L"
$ WRITE TEMP "FPSSUB:WDC.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSLIB:BAALIB.4K"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSLIB:BABLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSLIB:APFLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSLIB:UTLLIB.LIB"
$ WRITE TEMP "L"
$ WRITE TEMP "FPSLIB:SYMLIB.4K"
$ WRITE TEMP "A"
$ WRITE TEMP P1, ".FOR"
$ WRITE TEMP "X"
$ CLOSE TEMP
$ RUN FPSLIB:APLINK
$!
$ DEL JUNKVFC.DAT;*
$ DEASS FOR005
$ EXIT

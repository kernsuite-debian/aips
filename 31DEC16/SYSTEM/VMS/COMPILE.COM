$! COMPILE - Compile or assemble a rouitine
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
$! Error handler
$!
$       ON CONTROL_Y THEN EXIT %X1000002C
$       ON ERROR THEN EXIT %X1000002A
$!
$!                                      translate name for user
$       FILE = P1
$       LOGN = ""
$       INDX = f$locate(":", P1)
$       if (INDX.eq.f$length(P1)) then goto UDONE
$       AREA = f$extract(0, INDX, P1)
$       FILE = f$extract(INDX+1, f$length(P1), P1)
$       LOGN = f$trnlnm(AREA)
$       if (f$locate("AIPS_VERSION", LOGN).ne.0) then goto UDONE
$          DIRC = LOGN - "AIPS_VERSION:["
$          VERS = f$trnlnm(f$trnlnm("AIPS_VERSION"))
$          INDX = f$locate("]", VERS) - 8
$          DATE = f$extract(INDX, 8, VERS)
$          LOGN = "AIPS_ROOT:[" + DATE + DIRC
$ UDONE:
$       UROUTINE = LOGN + FILE
$!                                      translate name for debugger
$       FILE = P2
$       LOGN = ""
$       INDX = f$locate(":", P2)
$       if (INDX.eq.f$length(P2)) then goto DONE
$       AREA = f$extract(0, INDX, P2)
$       FILE = f$extract(INDX+1, f$length(P2), P2)
$       LOGN = f$trnlnm(AREA)
$       if (f$locate("AIPS_VERSION", LOGN).ne.0) then goto done
$          DIRC = LOGN - "AIPS_VERSION:["
$          VERS = f$trnlnm(f$trnlnm("AIPS_VERSION"))
$          INDX = f$locate("]", VERS) - 8
$          DATE = f$extract(INDX, 8, VERS)
$          LOGN = "AIPS_ROOT:[" + DATE + DIRC
$ DONE:
$       ROUTINE = LOGN + FILE
$!
$       IF P4.EQS.".MAR" THEN GOTO DO_MAR
$       WRITE SYS$OUTPUT "Compiling ",UROUTINE
$       ZZFORTRAN 'ROUTINE'/OBJECT='P3'
$       EXIT
$DO_MAR:
$       WRITE SYS$OUTPUT "Assembling ",UROUTINE
$       ZZMACRO 'ROUTINE'/OBJECT='P3'
$       EXIT

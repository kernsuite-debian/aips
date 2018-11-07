      SUBROUTINE TICCOR (TICX, TICY)
C-----------------------------------------------------------------------
C! correct tick lengths from increments in dir cosines to coordinates
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   TICCOR corrects tick lengths from Delta LL,MM to Delta RA,DEC
C   by an approximate method only.
C   In/out:
C      TICX     D       Tick length along first axis
C      TICY     D       Tick length along second axis
C   Common:
C      /LOCATI/ used heavily, but not changed
C-----------------------------------------------------------------------
      DOUBLE PRECISION TICX, TICY
C
      DOUBLE PRECISION C
      INTEGER   I, J
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IF ((CORTYP(LOCNUM).LE.0) .OR. (CORTYP(LOCNUM).GT.6) .OR.
     *   (AXTYP(LOCNUM).EQ.4)) GO TO 999
C                                       find RA-like axis
         IF ((CORTYP(LOCNUM).EQ.4) .OR. (CORTYP(LOCNUM).EQ.6)) GO TO 999
         I = 1
         IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.5)) I = 2
C                                       find DEC-like axis
         J = 3
         IF (CORTYP(LOCNUM).LT.3) J = 3 - CORTYP(LOCNUM)
C                                       do correction
         C = 3.14159265358979D0 / 180.0D0
         C = COS (C * RPVAL(J,LOCNUM))
         IF (C.LT.0.1) C = 0.1
            IF (I.EQ.1) TICX = TICX / C
            IF (I.EQ.2) TICY = TICY / C
C
 999  RETURN
      END

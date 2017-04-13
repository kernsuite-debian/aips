      SUBROUTINE IDAMAX (N, DX, INCX, IDMX)
C-----------------------------------------------------------------------
C! Finds Max. abs. element of a DP vector.
C# Math
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
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78. EWG MADE IT A SUBROUTINE
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*), DMAX
      INTEGER     I, INCX, IX, N, IDMX
C-----------------------------------------------------------------------
      IDMX = 0
      IF (N.LT.1) GO TO 999
      IDMX = 1
      IF (N.EQ.1) GO TO 999
      IF (INCX.EQ.1) GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DMAX = ABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF (ABS(DX(IX)).LE.DMAX) GO TO 5
         IDMX = I
         DMAX = ABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      GO TO 999
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 DMAX = ABS(DX(1))
      DO 30 I = 2,N
         IF (ABS(DX(I)).LE.DMAX) GO TO 30
         IDMX = I
         DMAX = ABS(DX(I))
   30 CONTINUE
C
 999  RETURN
      END

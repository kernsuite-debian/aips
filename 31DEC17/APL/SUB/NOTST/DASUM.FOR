      DOUBLE PRECISION FUNCTION DASUM (N, DX, INCX)
C-----------------------------------------------------------------------
C! Sum absolute values of a double precision vector.
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
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     JACK DONGARRA, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*), DTEMP
      INTEGER   I, INCX, M, MP1, N, NINCX
C-----------------------------------------------------------------------
      DASUM = 0.0D0
      DTEMP = 0.0D0
      IF (N.LE.0) GO TO 999
      IF (INCX.EQ.1) GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DTEMP = DTEMP + ABS(DX(I))
   10 CONTINUE
      DASUM = DTEMP
      GO TO 999
C
C        CODE FOR INCREMENT EQUAL TO 1
C        CLEAN-UP LOOP
C
   20 M = MOD (N, 6)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + ABS(DX(I))
   30 CONTINUE
      IF (N.LT.6) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        DTEMP = DTEMP + ABS(DX(I)) + ABS(DX(I + 1)) + ABS(DX(I + 2))
     *     + ABS(DX(I + 3)) + ABS(DX(I + 4)) + ABS(DX(I + 5))
   50 CONTINUE
   60 DASUM = DTEMP
C
 999  RETURN
      END

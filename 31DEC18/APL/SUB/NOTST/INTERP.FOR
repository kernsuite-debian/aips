      SUBROUTINE INTERP (X, XVAL, YVAL, N, VALUE)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C   One-dimensional linear interpolation routine
C   Inputs:
C      X       R    Abscissa value
C      N       I    Dimension of XVAL and YVAL
C   In/Out:
C      XVAL    R(*) Tabulated X values - sorted on output
C      YVAL    R(*) Tabulated Y values
C   Outputs:
C      VALUE   R    Interpolated value
C----------------------------------------------------------------------
      INTEGER   N
      REAL      X, XVAL(N), YVAL(N), VALUE
C
      REAL      RTEMP, SLOPE
      INTEGER   I, J
C----------------------------------------------------------------------
C                                       Sort array
      DO 100 I = 1,N-1
         DO 50 J = I+1,N
            IF (XVAL(J).LT.XVAL(I)) THEN
               RTEMP = XVAL(J)
               XVAL(J) = XVAL(I)
               XVAL(I) = RTEMP
               RTEMP = YVAL(J)
               YVAL(J) = YVAL(I)
               YVAL(I) = RTEMP
               END IF
50          CONTINUE
100      CONTINUE
C                                       Special cases
      IF (X.LE.XVAL(1)) THEN
         VALUE = YVAL(1)
      ELSE IF (X.GE.XVAL(N)) THEN
         VALUE = YVAL(N)
C                                       Find values on either side of
C                                       the desired x-ordinate and
C                                       interpolate.
      ELSE
         VALUE = 0.0
         DO 200 I = 1, N-1
            IF ((XVAL(I).LE.X) .AND. (XVAL(I+1).GE.X)) THEN
               IF (XVAL(I).EQ.XVAL(I+1)) THEN
                  VALUE = (YVAL(I+1) + YVAL(I)) / 2.0
               ELSE
                  SLOPE = (YVAL(I+1) - YVAL(I)) / (XVAL(I+1) - XVAL(I))
                  VALUE = (X - XVAL(I)) * SLOPE + YVAL(I)
                  END IF
               END IF
200         CONTINUE
         END IF
C                                       Exit
999   RETURN
      END

      SUBROUTINE STRLIN (X1, Y1, X2, Y2, BUF)
C-----------------------------------------------------------------------
C! computes integer array as linear interpolation between two points
C# Utility
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
C   STRLIN interpolates a straight line from (X1,Y1) to (X2,Y2) into
C   BUF(X1+1) to BUF(X2+1).
C   Inputs:
C      X1   I           First X point (0-rel, i.e. >= 0)
C      Y1   I           First Y point
C      X2   I           Second X point (0-rel, >= X1)
C      Y2   I           Second Y point
C   Output:
C      BUF  I(>X2+1)    Output line in BUF(X1+1) to BUF(X2+1)
C-----------------------------------------------------------------------
      INTEGER   BUF(*), X1, Y1, X2, Y2
C
      INTEGER   NSTEPS, I
      REAL      INC
C-----------------------------------------------------------------------
C                                       Illegal direction
      IF (X2.LT.X1) GO TO 999
C                                       First point
         BUF(X1+1) = Y1
         NSTEPS = X2 - X1
C                                       Interpolate
         IF (NSTEPS.LE.0) GO TO 999
            INC = REAL(Y2-Y1) / REAL(NSTEPS)
            DO 10 I = 1,NSTEPS
               BUF(X1+I+1) = Y1 + I * INC + 0.5
 10            CONTINUE
C
 999  RETURN
      END

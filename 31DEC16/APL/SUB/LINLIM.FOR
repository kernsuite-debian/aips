      SUBROUTINE LINLIM (BLC, TRC, X, Y, IERR)
C-----------------------------------------------------------------------
C! clips X,Y values at edges of rectangular area with interpolation
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
C   clips X,Y values at BLC, TRC with interpolation --- see also INTEDG
C   Inputs:
C      BLC  R(2)     X,Y lower left corner pixels
C      TRC  R(2)     X,Y upper right corner pixels
C   In/out:
C      X    R(2)     X pixel pair
C      Y    R(2)     Y pixel pair
C   Output:
C      IERR I        0 ok, 1 not in plot at all
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      BLC(2), TRC(2), X(2), Y(2)
C
      INTEGER   I, J
      REAL      ALPHA, BETA, XX, YY, XS(2), YS(2)
C-----------------------------------------------------------------------
      IERR = 1
      IF ((X(1).LT.BLC(1)) .AND. (X(2).LT.BLC(1))) GO TO 999
      IF ((Y(1).LT.BLC(2)) .AND. (Y(2).LT.BLC(2))) GO TO 999
      IF ((X(1).GT.TRC(1)) .AND. (X(2).GT.TRC(1))) GO TO 999
      IF ((Y(1).GT.TRC(2)) .AND. (Y(2).GT.TRC(2))) GO TO 999
      IERR = 0
C                                       Loop over points
      XS(1) = X(1)
      XS(2) = X(2)
      YS(1) = Y(1)
      YS(2) = Y(2)
      DO 20 I = 1,2
C                                       Fix point
         IF ((X(I).GE.BLC(1)) .AND. (X(I).LE.TRC(1)) .AND.
     *      (Y(I).GE.BLC(2)) .AND. (Y(I).LE.TRC(2))) GO TO 20
            J = 3 - I
            XX = MAX (BLC(1), MIN (TRC(1), XS(I)))
            YY = MAX (BLC(2), MIN (TRC(2), YS(I)))
            ALPHA = 1.0
            BETA = 1.0
            IF (XS(I).NE.XS(J)) ALPHA = (XX - XS(J)) / (XS(I) - XS(J))
            IF (YS(I).NE.YS(J)) BETA = (YY - YS(J)) / (YS(I) - YS(J))
            ALPHA = MIN (ALPHA, BETA)
            X(I) = XS(J) + ALPHA * (XS(I) - XS(J))
            Y(I) = YS(J) + ALPHA * (YS(I) - YS(J))
 20      CONTINUE
C
 999  RETURN
      END

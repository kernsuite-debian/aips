      SUBROUTINE INTPFN (APLOC, M, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C! Computes interpolation kernals and put them into "AP memory".
C# AP-util Map UV Modeling
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
C     INTPFN computes the interpolation functions and stores them in
C     the array processor.  Values are tabulated every 1/200 cell.
C     Uses the Lagrange interpolation formula, assumed value never
C     within M/2 of the edge of the table.
C    Inputs:
C     APLOC           I    AP base address of interpolation function.
C     M               I    Support size of convolving function.
C     BUFFER()        R    Work buffer.
C     BUFSZ           I    Size of BUFFER in bytes.
C    Outputs:
C     IRET            I    Return error code, 0=>OK else error.
C
C-----------------------------------------------------------------------
      INTEGER   M, LIM, I, J, IX, MM1, BUFSZ, IRET, IRNDN, IRNDP, MO2,
     *   APLOC, NWORD, LLIM
      REAL      BUFFER(*), BIAS, COEF(10), PROD, X, XX, Y
      LOGICAL   ISXPOS
C-----------------------------------------------------------------------
      IRNDN (XX) = INT (XX - 0.5001)
      IRNDP (XX) = INT (XX + 0.4999)
      IRET = 0
      LIM = M * 200 + 1
      BIAS = 100.0 * M + 1.0
      MO2 = (M / 2)
      MM1 = M - 1
C                                       Set coefficients
      DO 20 I = 1,MM1
         PROD = 1.0
         DO 10 J = 1,MM1
            IF (I.EQ.J) GO TO 10
            PROD = PROD * (I - J)
 10         CONTINUE
         COEF (I) = 1.0 / PROD
 20      CONTINUE
      BUFFER(1) = 0.0
      DO 100 I = 2,LIM
         PROD = 0.0
         Y = (I-BIAS) * 0.005
         X = (Y - IRNDN (Y))
         IF (Y.GT.0.0) X = Y - IRNDP (Y)
         ISXPOS = X.GT.-0.0001
         IX = 1 + (I-2) / 200
         IF (.NOT.ISXPOS) IX = IX - 1
         IF ((IX.LT.1) .OR. (IX.GE.M)) GO TO 70
         XX = MO2 - X
         IF (ISXPOS) XX = XX + 1.0
         PROD = COEF(IX)
         DO 50 J = 1,MM1
            IF (J.EQ.IX) GO TO 50
               PROD = PROD * (XX - J)
 50            CONTINUE
 70         BUFFER(I) = PROD
 100        CONTINUE
      GO TO 990
C                                       Load function into AP.
 990  CALL QWR
      LLIM = LIM
      CALL QPUT (BUFFER, APLOC, LLIM, 2)
      CALL QWD
C                                       Check AP roller
      NWORD = APLOC + LLIM
      CALL QROLL (NWORD, BUFFER, BUFSZ, IRET)
C
 999  RETURN
      END

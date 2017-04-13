      SUBROUTINE L1 (M, N, M2, N2, A, B, TOLER, X, E, S)
C-----------------------------------------------------------------------
C! Compute L1 solution to an overdetermined system of linear equations
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
C     This subroutine uses a modification of the simplex method
C  of linear programming to calculate an L1 solution to an
C  overdetermined system of linear equations.
C  Description of parameters:
C     M...Number of equations (adj. array dim.).
C     N...Number of unknowns (adj. array dim.; M>=N).
C     M2...Set equal to M+2 for adjustable dimensions
C     N2...Set equal to N+2 for adjustable dimensions
C     A...Two dimensional real array of size M2xN2.
C         On entry, the coefficients of the matrix must be
C         stored in the first M rows and N columns of A.
C         These values are destroyed by the subroutine.
C     B...One dimensional real array of size M.  On entry, B
C         must contain the right-hand side of the equations.
C         These values are destroyed by the subroutine.
C     TOLER...A small positive tolerance.  Empirical evidence
C         suggests TOLER=10**(-2*d/3), where d represents the
C         number of decimal digits of accuracy available.
C     X...One dimensional real array of size N.  On exit, this
C         array contains a solution to the L1 problem.
C     E...One dimensional real array of size M.  On exit, this
C         array contains the residuals in the equations.
C     S...Integer array of size M used for workspace.
C  On exit, the array A contains the following information:
C     A(M+1,N+1) -- the minimum sum of the absolute values of
C                   the residuals
C     A(M+1,N+2) -- the rank of the matrix of coefficients
C     A(M+2,N+1) -- exit code with values:
C                      0 => optimal solution which is probably
C                           non-unique
C                      1 => unique optimal solution
C                      2 => calculations terminated prematurely
C                           due to rounding errors
C     A(M+2,N+2) -- number of simplex iterations performed
C   Adapted from CACM Algorithm #478.   F. Schwab -- Dec. 1981
C-----------------------------------------------------------------------
      INTEGER M, N, M2, N2
      DOUBLE PRECISION SUM
      REAL      MN, MX, A(M2,2), X(N), E(M), B(M)
      INTEGER   OUT, S(M), M1, N1, J, I, KOUNT, KR, KL,
     *   IN, K, L
      REAL   TOLER, ZERO, ONE, TWO, BIG, D, PIVOT
      LOGICAL   STAGE, TEST, T, F
      DATA T , F /.TRUE. , .FALSE./
      DATA ZERO, ONE , TWO /0.0E0 , 1.0E0 , 2.0E0/
C  BIG must be set equal to any very large real constant.
      DATA BIG /1.0E38/
C-----------------------------------------------------------------------
C                                       Initialization:
      M1 = M + 1
      N1 = N + 1
      DO 10 J = 1,N
         A(M2,J) = J
         X(J) = ZERO
 10      CONTINUE
      DO 40 I = 1,M
         A(I,2) = N + I
         A(I,1) = B(I)
         IF (B(I).GE.ZERO) GO TO 30
         DO 20 J = 1,N2
            A(I,J) = -A(I,J)
 20         CONTINUE
 30      E(I) = ZERO
 40      CONTINUE
C                                       Compute the marginal costs:
      DO 60 J = 1,N1
         SUM = 0D0
         DO 50 I = 1,M
            SUM = SUM + A(I,J)
 50         CONTINUE
         A(M1,J) = SUM
 60      CONTINUE
C                                       Stage I. Determine the vector to
C                                       enter the basis:
      STAGE = T
      KOUNT = 0
      KR = 1
      KL = 1
 70   MX = -ONE
      DO 80 J = KR,N
         IF (ABS(A(M2,J)).GT.N) GO TO 80
            D = ABS(A(M1,J))
            IF (D.LE.MX) GO TO 80
               MX = D
               IN = J
 80      CONTINUE
      IF (A(M1,IN).GE.ZERO) GO TO 100
         DO 90 I = 1,M2
            A(I,IN) = -A(I,IN)
 90         CONTINUE
C                                       Determine the vector to leave
C                                       the basis:
 100  K = 0
      DO 110 I = KL,M
         D = A(I,IN)
         IF (D.LE.TOLER) GO TO 110
            K = K + 1
            B(K) = A(I,1) / D
            S(K) = I
            TEST = T
 110     CONTINUE
 120  IF (K.GT.0) GO TO 130
         TEST = F
         GO TO 150
 130  MN = BIG
      DO 140 I = 1,K
         IF (B(I).GE.MN) GO TO 140
            J = I
            MN = B(I)
            OUT = S(I)
 140     CONTINUE
      B(J) = B(K)
      S(J) = S(K)
      K = K - 1
C  Check for linear dependence in Stage I:
 150  IF ((TEST) .OR. (.NOT.STAGE)) GO TO 170
         DO 160 I = 1,M2
            D = A(I,KR)
            A(I,KR) = A(I,IN)
            A(I,IN) = D
 160        CONTINUE
         KR = KR + 1
         GO TO 260
 170  IF (TEST) GO TO 180
         A(M2,1) = TWO
         GO TO 350
 180  PIVOT = A(OUT,IN)
      IF (A(M1,IN)-PIVOT-PIVOT.LE.TOLER) GO TO 200
         DO 190 J = KR,N1
            D = A(OUT,J)
            A(M1,J) = A(M1,J) - D - D
            A(OUT,J) = -D
 190        CONTINUE
         A(OUT,2) = -A(OUT,2)
         GO TO 120
C                                       Pivot on A(OUT,IN):
 200  DO 210 J = KR,N1
         IF (J.EQ.IN) GO TO 210
            A(OUT,J) = A(OUT,J)/PIVOT
 210     CONTINUE
      DO 230 I = 1,M1
         IF (I.EQ.OUT) GO TO 230
            D = A(I,IN)
            DO 220 J = KR,N1
               IF (J.EQ.IN) GO TO 220
               A(I,J) = A(I,J) - D * A(OUT,J)
 220           CONTINUE
 230     CONTINUE
      DO 240 I = 1,M1
         IF (I.EQ.OUT) GO TO 240
            A(I,IN) = -A(I,IN) / PIVOT
 240     CONTINUE
      A(OUT,IN) = ONE / PIVOT
      D = A(OUT,2)
      A(OUT,2) = A(M2,IN)
      A(M2,IN) = D
      KOUNT = KOUNT + 1
C                                       Interchange rows in Stage I:
      IF (.NOT.STAGE) GO TO 270
         KL = KL + 1
         DO 250 J = KR,N2
            D = A(OUT,J)
            A(OUT,J) = A(KOUNT,J)
            A(KOUNT,J) = D
 250        CONTINUE
 260     IF (KOUNT+KR.NE.N1) GO TO 70
C                                       *** Stage II.   ***
         STAGE = F
C  Determine the vector to enter the basis:
 270  MX = -BIG
      DO 290 J = KR,N
         D = A(M1,J)
         IF (D.GE.ZERO) GO TO 280
            IF (D.GT.-TWO) GO TO 290
               D = -D - TWO
 280     IF (D.LE.MX) GO TO 290
            MX = D
            IN = J
 290     CONTINUE
      IF (MX.LE.TOLER) GO TO 310
         IF (A(M1,IN).GT.ZERO) GO TO 100
            DO 300 I = 1,M2
               A(I,IN) = -A(I,IN)
 300           CONTINUE
            A(M1,IN) = A(M1,IN)-TWO
            GO TO 100
C  Prepare output:
 310  L = KL - 1
      DO 330 I = 1,L
         IF (A(I,1).GE.ZERO) GO TO 330
            DO 320 J = KR,N2
               A(I,J) = -A(I,J)
 320           CONTINUE
 330     CONTINUE
      A(M2,1) = ZERO
      IF (KR.NE.1) GO TO 350
      DO 340 J = 1,N
         D = ABS(A(M1,J))
         IF ((D.LE.TOLER) .OR. (TWO-D.LE.TOLER)) GO TO 350
 340     CONTINUE
      A(M2,1) = ONE
 350  DO 380 I = 1,M
         K = A(I,2)
         D = A(I,1)
         IF (K.GT.0) GO TO 360
            K = -K
            D = -D
 360     IF (I.GE.KL) GO TO 370
            X(K) = D
            GO TO 380
 370     CONTINUE
            K = K - N
            E(K) = D
 380     CONTINUE
      A(M2,2) = KOUNT
      A(M1,2) = N1 - KR
      SUM = 0.D0
      DO 390 I = KL,M
         SUM = SUM + A(I,1)
 390     CONTINUE
      A(M1,1) = SUM
C
 999  RETURN
      END

      SUBROUTINE LMPAR (N, R, LDR, IPVT, DIAG, QTB, DELTA, PAR, X,
     *   SDIAG, WA1, WA2)
C-----------------------------------------------------------------------
C! completes solution of the MxN matrix least squares problem
C# Modeling
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
C   Given an M by N matrix A, an N by N nonsingular diagonal matrix D,
C   an M-vector B, and a positive number delta, the problem is to
C   determine a value for the parameter PAR such that, if X solves the
C   system
C
C           A*X = B ,     SQRT(PAR)*D*X = 0 ,
C
C   in the least squares sense, and DXNORM is the euclidean norm of
C   D*X, then either PAR is zero and
C
C           (DXNORM-DELTA) .LE. 0.1*DELTA ,
C
C   or PAR is positive and
C
C           ABS(DXNORM-DELTA) .LE. 0.1*DELTA .
C
C   This subroutine completes the solution of the problem if it is
C   provided with the necessary information from the QR factorization,
C   with column pivoting, of A. that is, if A*P = Q*R, where P is A
C   permutation matrix, Q has orthogonal columns, and R is an upper
C   triangular matrix with diagonal elements of nonincreasing magnitude,
C   then LMPAR expects the full upper triangle of R, the permutation
C   matrix P, and the first N components of (Q transpose)*B.  On output
C   LMPAR also provides an upper triangular matrix S such that
C
C            T   T                   T
C           P *(A *A + PAR*D*D)*P = S *S .
C
C   S is employed within LMPAR and may be of separate interest.
C
C   Only a few iterations are generally needed for convergence of the
C   algorithm. If, however, the limit of 10 iterations is reached, then
C   the output PAR will contain the best value obtained so far.
C   Subprograms called:
C       Minpack-supplied ... DPMPAR, ENORM, QRSOLV
C       FORTRAN-supplied ... ABS, MAX, MIN, SQRT
C   Argonne National Laboratory. Minpack project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      N      I     the order of R (adj. array dim.)
C      R      D(N,N)  on input the full upper triangle must contain the
C                   full upper triangle of the matrix R.  On output the
C                   full upper triangle is unaltered, and the strict
C                   lower triangle contains the strict upper triangle
C                   (transposed) of the upper triangular matrix S.
C      LDR    I     positive variable not less than N which specifies
C                   the leading dimension of the array r (adj. array
C                   dim.).
C      IPVT   I(N)  defines the permutation matrix P such that
C                   A*P = Q*R.  Column J of P is column IPVT(J) of the
C                   identity matrix.
C      DIAG   D(N)  must contain the diagonal elements of the matrix D.
C      QTB    D(N)  contains the first N elements of the vector
C                   (Q transpose)*B.
C      DELTA  D     positive variable which specifies an upper bound on
C                   the euclidean norm of D*X.
C      PAR    D     contains an initial estimate of the
C                   Levenberg-Marquardt parameter.
C   Outputs:
C      PAR    D     contains the final estimate of Levenberg-Marquardt
C                   parameter.
C      X      D(N)  contains the least squares solution of the system
C                   A*X = B, SQRT(PAR)*D*X = 0, for the output PAR.
C      SDIAG  D(N)  contains the diagonal elements of the upper
C                   triangular matrix S.
C      WA1    D(N)  work array.
C      WA2    D(N)  work array.
C-----------------------------------------------------------------------
      INTEGER N, LDR, IPVT(N)
      DOUBLE PRECISION DELTA, PAR, R(LDR,N), DIAG(N), QTB(N), X(N),
     *   SDIAG(N), WA1(N), WA2(N)
C
      INTEGER   I, ITER, J, JM1, JP1, K, L, NSING
      DOUBLE PRECISION    DXNORM, DWARF, FP, GNORM, PARC, PARL, PARU,
     *   P1, P001, SUM, TEMP, ZERO, DPMPAR, ENORM
      DATA P1, P001, ZERO /1.0D-1, 1.0D-3, 0.0D0/
C-----------------------------------------------------------------------
C                                       dwarf is the smallest
C                                       positive magnitude.
      DWARF = DPMPAR (2)
C                                       compute and store in X the
C                                       Gauss-Newton direction. if the
C                                       Jacobian is rank-deficient,
C                                       obtain a least squares solution
      NSING = N
      DO 10 J = 1,N
         WA1(J) = QTB(J)
         IF ((R(J,J).EQ.ZERO) .AND. (NSING.EQ.N)) NSING = J - 1
         IF (NSING.LT.N) WA1(J) = ZERO
 10      CONTINUE
      IF (NSING.GE.1) THEN
         DO 40 K = 1,NSING
            J = NSING - K + 1
            WA1(J) = WA1(J) / R(J,J)
            TEMP = WA1(J)
            JM1 = J - 1
            IF (JM1.GE.1) THEN
               DO 20 I = 1,JM1
                  WA1(I) = WA1(I) - R(I,J)*TEMP
 20               CONTINUE
               END IF
 40         CONTINUE
         END IF
      DO 60 J = 1,N
         L = IPVT(J)
         X(L) = WA1(J)
 60      CONTINUE
C                                       Initialize the iteration cntr,
C                                       evaluate the function at the
C                                       origin, and test for acceptance
C                                       of the Gauss-Newton direction.
C
      ITER = 0
      DO 70 J = 1,N
         WA2(J) = DIAG(J) * X(J)
 70      CONTINUE
      DXNORM = ENORM (N, WA2)
      FP = DXNORM - DELTA
      IF (FP.LE.P1*DELTA) GO TO 220
C                                       If the Jacobian is not rank
C                                       deficient, the Newton
C                                       step provides a lower bound,
C                                       PARL, for the zero of
C                                       the function. Otherwise
C                                       set this bound to zero.
      PARL = ZERO
      IF (NSING.GE.N) THEN
         DO 80 J = 1,N
            L = IPVT(J)
            WA1(J) = DIAG(L) * (WA2(L) / DXNORM)
 80         CONTINUE
         DO 110 J = 1,N
            SUM = ZERO
            JM1 = J - 1
            IF (JM1.GE.1) THEN
               DO 90 I = 1,JM1
                  SUM = SUM + R(I,J) * WA1(I)
 90               CONTINUE
               END IF
            WA1(J) = (WA1(J) - SUM) / R(J,J)
 110        CONTINUE
         TEMP = ENORM (N, WA1)
         PARL = ((FP/DELTA) / TEMP) / TEMP
         END IF
C                                       Calculate an upper bound, PARU
C                                       for the zero of the function.
      DO 140 J = 1,N
         SUM = ZERO
         DO 130 I = 1,J
            SUM = SUM + R(I,J) * QTB(I)
 130        CONTINUE
         L = IPVT(J)
         WA1(J) = SUM / DIAG(L)
 140     CONTINUE
      GNORM = ENORM (N, WA1)
      PARU = GNORM / DELTA
      IF (PARU.EQ.ZERO) PARU = DWARF / MIN (DELTA, P1)
C                                       If the input PAR lies outside
C                                       of the interval (PARL,PARU),
C                                       set PAR to the closer endpoint.
      PAR = MAX (PAR, PARL)
      PAR = MIN (PAR, PARU)
      IF (PAR.EQ.ZERO) PAR = GNORM / DXNORM
C                                       Beginning of an iteration.
 150  CONTINUE
         ITER = ITER + 1
C                                       Evaluate the function at the
C                                       current value of PAR.
         IF (PAR.EQ.ZERO) PAR = MAX (DWARF, P001*PARU)
         TEMP = SQRT(PAR)
         DO 160 J = 1,N
            WA1(J) = TEMP * DIAG(J)
 160        CONTINUE
         CALL QRSOLV (N, R, LDR, IPVT, WA1, QTB, X, SDIAG, WA2)
         DO 170 J = 1,N
            WA2(J) = DIAG(J) * X(J)
 170        CONTINUE
         DXNORM = ENORM (N, WA2)
         TEMP = FP
         FP = DXNORM - DELTA
C                                       If the function is small
C                                       enough, accept the current
C                                       value of PAR. Also test for
C                                       the exceptional cases where
C                                       PARL is zero or the number
C                                       of iterations has reached 10.
         IF ((ABS(FP).LE.P1*DELTA) .OR. ((PARL.EQ.ZERO) .AND.
     *      (FP.LE.TEMP) .AND. (TEMP.LT.ZERO)) .OR. (ITER.EQ.10))
     *      GO TO 220
C                                       Compute the Newton correction.
         DO 180 J = 1,N
            L = IPVT(J)
            WA1(J) = DIAG(L) * (WA2(L) / DXNORM)
 180        CONTINUE
         DO 210 J = 1,N
            WA1(J) = WA1(J) / SDIAG(J)
            TEMP = WA1(J)
            JP1 = J + 1
            IF (N.GE.JP1) THEN
               DO 190 I = JP1,N
                  WA1(I) = WA1(I) - R(I,J) * TEMP
 190              CONTINUE
               END IF
 210        CONTINUE
         TEMP = ENORM (N, WA1)
         PARC = ((FP/DELTA) / TEMP) / TEMP
C                                       Depending on the sign of the
C                                       function, update PARL or PARU.
         IF (FP.GT.ZERO) PARL = MAX (PARL, PAR)
         IF (FP.LT.ZERO) PARU = MIN (PARU, PAR)
C                                       x
C                                       Compute an improved estimate
C                                       for PAR.
         PAR = MAX (PARL, PAR+PARC)
C                                       End of an iteration.
         GO TO 150
C                                       Termination.
 220  IF (ITER.EQ.0) PAR = ZERO
C
 999  RETURN
      END

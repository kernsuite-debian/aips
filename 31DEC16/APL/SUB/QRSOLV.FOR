      SUBROUTINE QRSOLV (N, R, LDR, IPVT, DIAG, QTB, X, SDIAG, WA)
C-----------------------------------------------------------------------
C! completes the least squares matrix solution
C# Math Modeling
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
C   Given an M by N matrix A, an N by N diagonal matrix D, and an
C   M-vector B, the problem is to determine an X which solves the system
C
C           A*X = B ,     D*X = 0 ,
C
C   in the least squares sense.
C
C   This subroutine completes the solution of the problem if it is
C   provided with the necessary information from the QR factorization,
C   with column pivoting, of A. That is, if A*P = Q*R, where P is a
C   permutation matrix, Q has orthogonal columns, and R is an upper
C   triangular matrix with diagonal elements of nonincreasing magnitude,
C   then QRSOLV expects the full upper triangle of R, the permutation
C   matrix P, and the first N components of (Q transpose)*B. The system
C   A*X = B, D*X = 0, is then equivalent to
C
C                  T       T
C           R*Z = Q *B ,  P *D*P*Z = 0 ,
C
C            T   T               T
C           P *(A *A + D*D)*P = S *S .
C
C   S is computed within QRSOLV and may be of separate interest.
C   Argonne National Laboratory. Minpack Project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      N      I       the order of R (adj. array dim.).
C      R      D(N,N)  on input the full upper triangle must contain the
C                     full upper triangle of the matrix R.  On output
C                     the full upper triangle is unaltered, and the
C                     strict lower triangle contains the strict upper
C                     triangle (transposed) of the upper triangular
C                     matrix S.
C      LDR    I       positive variable not less than N which specifies
C                     the leading dimension of the array R
C      IPVT   I(N)    defines the permutation matrix P such that
C                     A*P = Q*R. Column J of P is column IPVT(J) of the
C                     identity matrix.
C      DIAG   D(n)    must contain the diagonal elements of the matrix D
C      QTB    D(N)    contains the first N elements of the vector
C                     (Q transpose)*B.
C   Outputs:
C      X      D(N)    contains the least squares solution
C                     of the system A*X = B, D*X = 0.
C      SDIAG  D(N)    contains the diagonal elements of the
C                     upper triangular matrix S.
C      WA     D(N)    work array.
C-----------------------------------------------------------------------
      INTEGER   N, LDR, IPVT(N)
      DOUBLE PRECISION R(LDR,N), DIAG(N), QTB(N), X(N), SDIAG(N), WA(N)
C
      INTEGER   I, J, JP1, K, KP1, L, NSING
      DOUBLE PRECISION P5, P25, QTBPJ, SUM, TEMP, ZERO, COS, SIN, TAN,
     *   COTAN
      DATA P5, P25, ZERO /5.0D-1, 2.5D-1, 0.0D0/
C-----------------------------------------------------------------------
C                                       Copy R and (Q transpose)*B to
C                                       preserve input and initialize S
C                                       In particular, save the diagonl
C                                       elements of R in X.
      DO 20 J = 1,N
         DO 10 I = J,N
            R(I,J) = R(J,I)
 10         CONTINUE
         X(J) = R(J,J)
         WA(J) = QTB(J)
 20      CONTINUE
C                                       Eliminate the diagonal matrix D
C                                       using a Givens Rotation.
      DO 100 J = 1,N
C                                       Prepare the row of D to be
C                                       eliminated, locating the
C                                       diagonal element using P from
C                                       the QR factorization.
         L = IPVT(J)
         IF (DIAG(L).EQ.ZERO) GO TO 90
         DO 30 K = J,N
            SDIAG(K) = ZERO
 30         CONTINUE
         SDIAG(J) = DIAG(L)
C                                       The transformations to
C                                       eliminate the row of D
C                                       modify only a single element
C                                       of (Q transpose)*B
C                                       beyond the first N, which is
C                                       initially zero.
         QTBPJ = ZERO
         DO 80 K = J,N
C                                       Determine a Givens rotation
C                                       which eliminates the
C                                       appropriate element in the
C                                       current row of D.
            IF (SDIAG(K).EQ.ZERO) GO TO 80
            IF (ABS(R(K,K)).LT.ABS(SDIAG(K))) THEN
               COTAN = R(K,K) / SDIAG(K)
               SIN = P5 / SQRT (P25+P25*COTAN**2)
               COS = SIN * COTAN
            ELSE
               TAN = SDIAG(K) / R(K,K)
               COS = P5 / SQRT (P25+P25*TAN**2)
               SIN = COS * TAN
               END IF
C                                       Compute the modified diagonal
C                                       element of R and
C                                       the modified element of
C                                       ((Q transpose)*b,0).
            R(K,K) = COS*R(K,K) + SIN*SDIAG(K)
            TEMP = COS*WA(K) + SIN*QTBPJ
            QTBPJ = -SIN*WA(K) + COS*QTBPJ
            WA(K) = TEMP
C                                       Accumulate the tranformation
C                                       in the row of S.
            KP1 = K + 1
            IF (N.GE.KP1) THEN
               DO 60 I = KP1,N
                  TEMP = COS*R(I,K) + SIN*SDIAG(I)
                  SDIAG(I) = -SIN*R(I,K) + COS*SDIAG(I)
                  R(I,K) = TEMP
 60               CONTINUE
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       Store the diagonal element
C                                       of S and restore the corres-
C                                       ponding diagonal element of R.
         SDIAG(J) = R(J,J)
         R(J,J) = X(J)
 100     CONTINUE
C                                       Solve the triangular system
C                                       for Z. If the system is
C                                       singular, then obtain a
C                                       least squares solution.
      NSING = N
      DO 110 J = 1,N
         IF (SDIAG(J).EQ.ZERO .AND. NSING.EQ.N) NSING = J - 1
         IF (NSING.LT.N) WA(J) = ZERO
 110     CONTINUE
      IF (NSING.GE.1) THEN
         DO 140 K = 1,NSING
            J = NSING - K + 1
            SUM = ZERO
            JP1 = J + 1
            IF (NSING.GE.JP1) THEN
               DO 120 I = JP1,NSING
                  SUM = SUM + R(I,J)*WA(I)
 120              CONTINUE
               END IF
            WA(J) = (WA(J) - SUM)/SDIAG(J)
 140        CONTINUE
         END IF
C                                       Permute the components of
C                                       Z back to components of X.
      DO 160 J = 1,N
         L = IPVT(J)
         X(L) = WA(J)
 160     CONTINUE
C
 999  RETURN
      END

      SUBROUTINE COVAR (N, R, LDR, IPVT, TOL, WA)
C-----------------------------------------------------------------------
C! Determines the covariance matrix of an M x N matrix
C# Modeling Math
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
C
C      Typed in by RCW   April 28,1981
C
C      Given an M by N matrix A, the problem is to determine
C      the covariance matrix corresponding to A, defined as
C
C                     T
C            Inverse(A *A) .
C
C      This subroutine completes the solution of the problem
C      if it is provided with the necessary information from the
C      QR factorization, with column pivoting, of A. That is, if
C      A*P = Q*R, where P is a permutation matrix, Q has orthogonal
C      columns, and R is an upper triangular matrix with diagonal
C      elements of nonincreasing magnitude, then COVAR expects
C      full upper triangle of R and the permutation matrix P.
C      the covariance matrix is then computed as
C
C                       T     T
C            P*INVERSE(R *R)*P   .
C
C      If A is nearly rank deficient, it may be desirable to compute
C      the covariance matrix corresponding to the linearly independent
C      columns of A.  To define the numerical rank of A, COVAR uses
C      the tolerance TOL.  If L is the largest integer such that
C
C            ABS(R(L,L)) .GT. TOL*ABS(R(1,1))  ,
C
C      then COVAR computes the covariance matrix corresponding to
C      the first L columns of R.  For K greater than L, column
C      and row IPVT(K) of the covariance matrix are set to zero.
C
C      The subroutine statement is
C
C        SUBROUTINE COVAR(N,R,LDR,IPVT,TOL,WA)
C
C      Subprograms called
C
C        FORTRAN-supplied ... DABS
C
C      Argonne National Laboratory. Minpack project. August 1980.
C       Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C
C  INPUTS:  N        I the order of R (adj. array dimension).
C
C           R        D(N,N)   On input the full upper triangle must
C                    contain the full upper triangle of the matrix R.
C
C           LDR      I A positive variable not less than N which
C                    which specifies the leading dimension of array R
C                    (adj. array dim.).
C
C           IPVT     I(N)   Defines the permutation matrix P such that
C                    A*P = Q*R.  Column J of P is column IPVT(J) of
C                    the identity matrix.
C
C           TOL      D   defines the numerical rank of A in the
C                    manner described above.
C  OUTPUT:  R        D(N,N)   Contains the square symmetric covariance
C                    matrix.
C
C           WA       D(N)   work array.
C-----------------------------------------------------------------------
C                                       Variables used as adjustable
C                                       array dimension specifiers
C                                       should be declared simply as
C                                       INTEGER
      INTEGER   N, LDR
      INTEGER   IPVT(N)
      DOUBLE PRECISION TOL
      DOUBLE PRECISION R(LDR,N), WA(N)
      INTEGER   I, II, J, JJ, K, KM1, L
      LOGICAL   SING
      DOUBLE PRECISION ONE, TEMP, TOLR, ZERO
      DATA ONE, ZERO /1.0D0,0.0D0/
C-----------------------------------------------------------------------
C                                       Form the inverse of R in the
C                                       full upper triangle of R.
      TOLR = TOL*ABS(R(1,1))
      L = 0
      DO 40 K = 1,N
         IF (ABS(R(K,K)).LE.TOLR) GO TO 50
         R(K,K) = ONE/R(K,K)
         KM1 = K - 1
         IF (KM1.LT.1) GO TO 30
            DO 20 J = 1,KM1
               TEMP = R(K,K)*R(J,K)
               R(J,K) = ZERO
               DO 10 I = 1,J
                  R(I,K) = R(I,K) - TEMP*R(I,J)
 10               CONTINUE
 20            CONTINUE
 30      L = K
 40      CONTINUE
C                                       Form the full upper triangle
C                                       of the inverse of
C                                       (R transpose)*R in the full
C                                       upper triangle of R.
 50   CONTINUE
      IF (L.LT.1) GO TO 110
         DO 100 K = 1,L
            KM1 = K-1
            IF (KM1.LT.1) GO TO 80
               DO 70 J = 1,KM1
                  TEMP = R(J,K)
                  DO 60 I = 1,J
                     R(I,J) = R(I,J) + TEMP*R(I,K)
 60                  CONTINUE
 70            CONTINUE
 80         CONTINUE
            TEMP = R(K,K)
            DO 90 I = 1,K
               R(I,K) = TEMP*R(I,K)
 90            CONTINUE
 100        CONTINUE
C                                       Form the full lower triangle of
C                                       the covariance matrix in the
C                                       strict lower triangle of R and
C                                       in WA.
 110  CONTINUE
      DO 130 J = 1,N
         JJ = IPVT(J)
         SING = J.GT.L
         DO 120 I = 1,J
            IF (SING) R(I,J) = ZERO
            II = IPVT(I)
            IF (II .GT. JJ) R(II,JJ) = R(I,J)
            IF (II .LT. JJ) R(JJ,II) = R(I,J)
 120        CONTINUE
         WA(JJ) = R(J,J)
 130     CONTINUE
C                                       Symmetrize the covariance
C                                       matrix in R.
      DO 150 J = 1,N
         DO 140 I = 1,J
            R(I,J) = R(J,I)
 140        CONTINUE
         R(J,J) = WA(J)
 150     CONTINUE
C
 999  RETURN
      END

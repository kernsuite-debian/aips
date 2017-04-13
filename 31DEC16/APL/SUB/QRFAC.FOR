      SUBROUTINE QRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, RDIAG, ACNORM,
     *   WA)
C-----------------------------------------------------------------------
C! computes a QR factorization of an MxN matrix
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
C   This subroutine uses householder transformations with column
C   pivoting (optional) to compute a QR factorization of the M by N
C   matrix A. That is, QRFAC determines an orthogonal matrix Q, a
C   permutation matrix P, and an upper trapezoidal matrix R with
C   diagonal elements of nonincreasing magnitude, such that A*P = Q*R.
C   The householder transformation for column K, K = 1,2,...,MIN(M,N),
C   is of the form
C                           T
C           I - (1/U(K))*U*U
C
C   where U has zeros in the first K-1 positions. The form of this
C   transformation and the method of pivoting first appeared in the
C   corresponding LINPACK subroutine.
C   Subprograms called
C       Minpack-supplied ... DPMPAR,ENORM
C       FORTRAN-supplied ... DMAX1,DSQRT,MIN0
C   Argonne National Laboratory. Minpack Project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      M       I       the number of rows of A (adj. array dim. passed
C                      to ENORM).
C      N       I       the number of columns of A
C      A       D(M,N)  On input A contains the matrix for
C                      which the QR factorization is to be computed.
C                      On output the strict upper trapezoidal part of A
C                      contains the strict upper trapezoidal part of R,
C                      and the lower trapezoidal part of A contains a
C                      factored form of Q (the non-trivial elements of
C                      the U vectors described above).
C      LDA     I       a variable not less than M which specifies
C                      the leading dimension of the array A
C      PIVOT   L       if pivot is set true, then column pivoting
C                      is enforced. if pivot is set false,
C                      then no column pivoting is done.
C      IPVT    I(LIPVT)  defines the permutation matrix P such that
C                                  A*P = Q*R.
C                      column J of P is column IPVT(J) of the identity
C                      matrix. if PIVOT is false, IPVT is not referenced
C      LIPVT   I       If PIVOT is false, then LIPVT may be as small
C                      as 1. If PIVOT is true, then LIPVT must be at
C                      least N (adj. array dim.).
C   Outputs:
C      RDIAG   D(N)    contains the diagonal elements of R.
C      ACNORM  D(N)    contains the norms of the corresponding columns
C                      of the input matrix A. If this information is not
C                      needed, then ACNORM can coincide with RDIAG.
C      WA      D(N)    work array. If PIVOT is false, then WA can
C                      coincide with RDIAG.
C-----------------------------------------------------------------------
      INTEGER   M, N, LDA, LIPVT, IPVT(LIPVT)
      LOGICAL   PIVOT
      DOUBLE PRECISION A(LDA,N), RDIAG(N), ACNORM(N), WA(N)
C
      INTEGER   I, J, JP1, K, KMAX, MINMN, ITMP1
      DOUBLE PRECISION AJNORM, EPSMCH, ONE, P05, SUM, TEMP, ZERO,
     *   DPMPAR, ENORM
      DATA ONE, P05, ZERO /1.0D0, 5.0D-2, 0.0D0/
C-----------------------------------------------------------------------
C                                       EPSMCH is the machine precision
      EPSMCH = DPMPAR (1)
C                                       Compute the initial column
C                                       norms and initialize several
C                                       arrays.
      DO 10 J = 1,N
         ACNORM(J) = ENORM (M, A(1,J))
         RDIAG(J) = ACNORM(J)
         WA(J) = RDIAG(J)
         IF (PIVOT) IPVT(J) = J
 10      CONTINUE
C                                       Reduce A to R with householder
C                                       transformations.
      MINMN = MIN (M, N)
      DO 110 J = 1,MINMN
C                                       Bring the column of largest
C                                       norm into the pivot position.
         IF (PIVOT) THEN
            KMAX = J
            DO 20 K = J,N
               IF (RDIAG(K).GT.RDIAG(KMAX)) KMAX = K
 20            CONTINUE
            IF (KMAX.NE.J) THEN
               DO 30 I = 1,M
                  TEMP = A(I,J)
                  A(I,J) = A(I,KMAX)
                  A(I,KMAX) = TEMP
 30               CONTINUE
               RDIAG(KMAX) = RDIAG(J)
               WA(KMAX) = WA(J)
               K = IPVT(J)
               IPVT(J) = IPVT(KMAX)
               IPVT(KMAX) = K
               END IF
            END IF
C                                       Compute the householder
C                                       transformation to reduce the
C                                       J-th column of A to a multiple
C                                       of the J-th unit vector.
         ITMP1 = M - J + 1
         AJNORM = ENORM (ITMP1, A(J,J))
         IF (AJNORM.NE.ZERO) THEN
            IF (A(J,J).LT.ZERO) AJNORM = -AJNORM
            DO 50 I = J,M
               A(I,J) = A(I,J) / AJNORM
 50            CONTINUE
            A(J,J) = A(J,J) + ONE
C                                       Apply the transformation to the
C                                       remaining columns and update
C                                       the norms.
            JP1 = J + 1
            IF (N.GE.JP1) THEN
               DO 90 K = JP1,N
                  SUM = ZERO
                  DO 60 I = J,M
                     SUM = SUM + A(I,J) * A(I,K)
 60                  CONTINUE
                  TEMP = SUM / A(J,J)
                  DO 70 I = J,M
                     A(I,K) = A(I,K) - TEMP * A(I,J)
 70                  CONTINUE
                  IF  ((PIVOT) .AND. (RDIAG(K).NE.ZERO)) THEN
                     TEMP = A(J,K) / RDIAG(K)
                     RDIAG(K) = RDIAG(K) * SQRT (MAX (ZERO,ONE-TEMP**2))
                     IF (P05*(RDIAG(K)/WA(K))**2.LE.EPSMCH) THEN
                        ITMP1 = M-J
                        RDIAG(K) = ENORM (ITMP1, A(JP1,K))
                        WA(K) = RDIAG(K)
                        END IF
                     END IF
 90               CONTINUE
               END IF
            END IF
         RDIAG(J) = -AJNORM
 110     CONTINUE
C
 999  RETURN
      END

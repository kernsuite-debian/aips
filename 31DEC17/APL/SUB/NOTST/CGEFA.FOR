      SUBROUTINE CGEFA (A, LDA, N, IPVT, INFO)
C-----------------------------------------------------------------------
C! Linpack routine: Factors complex matrix by Gaussian Elimination
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
C   CGEFA factors a complex matrix by gaussian elimination.
C   CGEFA is usually called by CGECO, but it can be called directly with
C   a saving in time if  RCOND  is not needed.
C       (Time for CGECO) = (1 + 9/N)*(time for CGEFA) .
C   Inputs:
C      A     C(LDA,N)  Matrix to be factored
C      LDA   I         The leading dimension OF the array  A .
C      N     I         The order of matrix A.
C   Outputs:
C      A     C(LDA,N)  An upper triangular matrix and the multipliers
C                      which were used to obtain it.  The factorization
C                      can be written  A = L*U  where L is a product of
C                      permutation and unit lower triangular matrices
C                      and  U  is upper triangular.
C      IPVT  I         An integer vector of pivot indices.
C      INFO  I         = 0  normal value.
C                      = K  if  U(K,K) .EQ. 0.0 .  this is not an error
C                      condition for this subroutine, but it does
C                      indicate that CGESL or CGEDI will divide by zero
C                      if called.  use  RCOND  in CGECO for a reliable
C                      indication of singularity.
C   LINPACK. This version dated 08/14/78.
C   Cleve Moler, University of New Mexico, Argonne National Lab.
C   Subroutines and Functions:
C      Blas     CAXPY, CSCAL, ICAMAX
C      Fortran  ABS, AIMAG, REAL
C-----------------------------------------------------------------------
      INTEGER    LDA, N, IPVT(*), INFO
      COMPLEX    A(LDA,*)
C
      COMPLEX   T, ZDUM
      INTEGER   J, K, KP1, L, NM1, ICAMAX
      REAL      CABS1
C-----------------------------------------------------------------------
C                                       internal function
      CABS1 (ZDUM) = ABS (REAL (ZDUM)) + ABS (AIMAG (ZDUM))
C-----------------------------------------------------------------------
C                                       Gaussian elimination with
C                                       partial Pivoting
      INFO = 0
      NM1 = N - 1
      IF (NM1.LT.1) GO TO 70
      DO 60 K = 1,NM1
         KP1 = K + 1
C                                       Find L = pivot index
         L = ICAMAX (N-K+1, A(K,K), 1) + K - 1
         IPVT(K) = L
C                                       Zero pivot implies this column
C                                       already triangularized.
         IF (CABS1(A(L,K)).EQ.0.0E0) GO TO 40
C                                       Interchange if necessary
C
            IF (L.EQ.K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C                                       Compute multipliers
            T = -(1.0E0,0.0E0) / A(K,K)
            CALL CSCAL (N-K, T, A(K+1,K), 1)
C                                       Row elimination with column
C                                       indexing.
            DO 30 J = KP1,N
               T = A(L,J)
               IF (L.EQ.K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20             CONTINUE
               CALL CAXPY (N-K, T, A(K+1,K), 1, A(K+1,J), 1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (CABS1 (A(N,N)) .EQ. 0.0E0) INFO = N
C
 999  RETURN
      END

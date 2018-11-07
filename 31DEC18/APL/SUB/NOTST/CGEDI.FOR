      SUBROUTINE CGEDI (A, LDA, N, IPVT, DET, WORK, JOB)
C-----------------------------------------------------------------------
C! Linpack routine: Determinant and inverse of a complex matrix
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
C   CGEDI computes the determinant and inverse of a matrix
C   using the factors computed by CGECO or CGEFA.
C    Inputs:
C     A(LDA,N)   C  The output from CGECO or CGEFA.
C     LDA        I  The leading dimension of the array A.
C     N          I  The order of the matrix  A.
C     IPVT(N)    I  The pivot vector from CGECO or CGEFA.
C     WORK(N)    C  Work vector
C     JOB        I  = 11   Both determinant and inverse.
C                   = 01   Inverse only.
C                   = 10   Determinant only.
C   Outputs:
C     A(LDA,N)   C  Inverse of original matrix if requested.
C                   otherwise unchanged.
C     DET        C  Determinant of original matrix if requested.
C                   otherwise not referenced.
C                   Determinant = DET(1) * 10.0**DET(2)
C                   with  1.0 .LE. CABS1(DET(1)) .LT. 10.0
C                   or  DET(1) .EQ. 0.0 .
C   Error condition:
C        A division by zero will occur if the input factor contains
C        a zero on the diagonal and the inverse is requested.
C        it will not occur if the subroutines are called correctly
C        and if CGECO has set RCOND .GT. 0.0 or CGEFA has set
C        INFO .EQ. 0 .
C     LINPACK. This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne national Lab.
C
C     Subroutines and Functions:
C     BLAS CAXPY, CSCAL, CSWAP
C     FORTRAN ABS, AIMAG, CMPLX, MOD, REAL
C-----------------------------------------------------------------------
      INTEGER   LDA, N, IPVT(*), JOB
      COMPLEX   A(LDA,1), DET(2), WORK(*)
C                                       Internal variables
      COMPLEX   T
      REAL      TEN
      INTEGER   I, J, K, KB, KP1, L, NM1
      COMPLEX ZDUM
      REAL CABS1
C-----------------------------------------------------------------------
      CABS1 (ZDUM) = ABS (REAL (ZDUM)) + ABS (AIMAG (ZDUM))
C                                       Compute determinant
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = (1.0E0,0.0E0)
         DET(2) = (0.0E0,0.0E0)
         TEN = 10.0E0
         DO 50 I = 1,N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
C                                       Exit
            IF (CABS1 (DET(1)) .EQ. 0.0E0) GO TO 60
   10       IF (CABS1 (DET(1)) .GE. 1.0E0) GO TO 20
               DET(1) = CMPLX (10.0E0,0.0E0)*DET(1)
               DET(2) = DET(2) - (1.0E0,0.0E0)
            GO TO 10
   20       CONTINUE
   30       IF (CABS1 (DET(1)) .LT. 10) GO TO 40
               DET(1) = DET(1) / CMPLX (10.0E0,0.0E0)
               DET(2) = DET(2) + (1.0E0,0.0E0)
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C                                       Compute inverse(U)
      IF (MOD (JOB, 10).EQ.0) GO TO 150
         DO 100 K = 1,N
            A(K,K) = (1.0E0,0.0E0) / A(K,K)
            T = -A(K,K)
            CALL CSCAL (K-1, T, A(1,K), 1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1,N
               T = A(K,J)
               A(K,J) = (0.0E0,0.0E0)
               CALL CAXPY (K, T, A(1,K), 1, A(1,J), 1)
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
C                                       Form inverse(U)*inverse(L)
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 140
         DO 130 KB = 1,NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = (0.0E0,0.0E0)
  110          CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL CAXPY (N, T, A(1,J), 1, A(1,K), 1)
  120          CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL CSWAP (N, A(1,K), 1, A(1,L), 1)
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
C
 999  RETURN
      END

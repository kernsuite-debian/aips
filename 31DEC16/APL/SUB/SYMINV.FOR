      SUBROUTINE SYMINV (N, A, IERR)
C-----------------------------------------------------------------------
C! Invert small symmetric real matrix.
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
C   Inverts a symmetric matrix.  The size of internal arrays limits to
C   no larger than 50x50.
C   Inputs:
C      N    I       Order of the matrix
C   Input/Output:
C      A    R(N,N)  On input the symmetric matrix,
C                   On output its inverse
C   Output:
C      IERR I       Error code, 0=OK, 
C                   1=matrix too big, 
C                   2=matrix not symmetric
C                   3=matrix singular. 
C-----------------------------------------------------------------------
      INTEGER   N, IERR
      REAL      A(N,N)
C                                       MAXSIZ = max. matrix order
      INTEGER MAXSIZ
      PARAMETER (MAXSIZ = 50)
      INTEGER   J, K, L, M
      REAL      AB, BIG, P(MAXSIZ), Q(MAXSIZ), R(MAXSIZ)
C-----------------------------------------------------------------------
C                                       Check size
      IF (N.GT.MAXSIZ) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       Make sure symmetric
      DO 10 J = 1,N
         DO 5 K = 1,N
C                                       Not symmetric
            IF (ABS (A(J,K)-A(K,J)).GT.ABS (A(J,K))*1.0E-4) THEN
               IERR = 2
               GO TO 999
               END IF
 5          CONTINUE
 10      CONTINUE
      DO 15 M = 1,N
         R(M) = 1.0
 15      CONTINUE
      DO 200 M = 1,N
         BIG = 0.0
         DO 20 L = 1,N
            AB = ABS (A(L,L))
            IF ((AB.GT.BIG) .AND. (R(L).NE.0)) THEN
               BIG = AB
               K = L
               END IF
 20         CONTINUE
C                                       Inverse indeterminant?
         IF (BIG.EQ.0.0) THEN
            IERR = 3
            GO TO 999
            END IF
         R(K) = 0.0
         Q(K) = 1.0 / A(K,K)
         P(K) = 1.0
         A(K,K) = 0.0
         DO 30 L = 1,K-1
            P(L) = A(L,K)
            IF (R(L).EQ.0.0) THEN
               Q(L) = A(L,K) * Q(K)
            ELSE
               Q(L) = -A(L,K) * Q(K)
               END IF
            A(L,K) = 0.0
 30         CONTINUE
         DO 40 L = K+1,N
            IF (R(L).NE.0.0) THEN
               P(L) = A(K,L)
            ELSE
               P(L) = -A(K,L)
               END IF
            Q(L) = -A(K,L) * Q(K)
            A(K,L) = 0.0
 40         CONTINUE
         DO 60 L = 1,N
            DO 50 K = L,N
               A(L,K) = A(L,K) + P(L)*Q(K)
 50            CONTINUE
 60         CONTINUE
 200     CONTINUE
C                                       Fill in symmetric half
      M = N + 1
      L = N
      DO 80 K = 2,N
         M = M - 1
         L = L - 1
         DO 70 J = 1,L
            A(M,J) = A(J,M)
 70         CONTINUE
 80      CONTINUE
C
 999  RETURN
      END

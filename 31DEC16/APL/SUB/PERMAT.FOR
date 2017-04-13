      SUBROUTINE PERMAT (A, LDA, N, M, IOPT, IP, WK)
C-----------------------------------------------------------------------
C! permutes rows or columns of matrix according to permutation vector
C# Sort Math
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
C   PERMAT permutes either the rows or the columns of an NxM real*4
C   matrix, A, according to a user-supplied permutation vector IP.
C   When IOPT is less than or equal to zero, the column ordering is
C   permuted, otherwise the row order.
C   Input:
C      LDA    I          the leading dimension of A
C      N      I          number of rows in A
C      M      I          number of columns in A (must be <=LDA)
C      IOPT   I          > 0 implies permutation of column ordering
C                        <=0 implies permutation of row ordering
C      IP     I(N or M)  the permutation vector.
C   In/out:
C      A      R(M,N)     the matrix
C   Output:
C      WK     R(M or N)  a work array
C-----------------------------------------------------------------------
      INTEGER   LDA, M, N, IOPT, IP(*)
      REAL      A(LDA,1), WK(1)
C
      INTEGER   IPTR, K, I, L, LN, LM
C-----------------------------------------------------------------------
      LN = N
      LM = M
      IPTR = 0
C                                       Permute rows
      IF (IOPT.GT.0) GO TO 100
 10      IPTR = IPTR + 1
            IF (IPTR.GE.LM) GO TO 999
            IF ((IP(IPTR).EQ.0) .OR. (IP(IPTR).EQ.IPTR)) GO TO 10
C                                       Save row at IPTR
               K = IPTR
               CALL RCOPY (LN, A(1,K), WK)
C                                       Move row to destination
C                                       Look for place to put row in WK
 30            L = IP(K)
               IF (L.EQ.IPTR) GO TO 50
                  CALL RCOPY (LN, A(1,L), A(1,K))
                  IP(K) = 0
                  K = L
                  GO TO 30
C                                       Have place for row in WK
 50            CALL RCOPY (LN, WK, A(1,K))
               IP(K) = 0
               GO TO 10
C-----------------------------------------------------------------------
C                                       Permute columns
 100  CONTINUE
         IPTR = IPTR + 1
            IF (IPTR.GE.LN) GO TO 999
            IF ((IP(IPTR).EQ.0) .OR. (IP(IPTR).EQ.IPTR)) GO TO 100
C                                       save col at IPTR
               K = IPTR
               DO 120 I = 1,LM
                  WK(I) = A(K,I)
 120              CONTINUE
C                                       move col to output area
C                                       look for col saved in WK
 130           L = IP(K)
               IF (L.EQ.IPTR) GO TO 150
                  DO 140 I = 1,LM
                     A(K,I) = A(L,I)
 140                 CONTINUE
                  IP(K) = 0
                  K = L
                  GO TO 130
C                                       Move col initially at IPTR
 150           DO 160 I = 1,LM
                  A(K,I) = WK(I)
 160              CONTINUE
               IP(K) = 0
               GO TO 100
C
 999  RETURN
      END

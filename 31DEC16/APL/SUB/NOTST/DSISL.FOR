      SUBROUTINE DSISL (A, LDA, N, KPVT, B)
C-----------------------------------------------------------------------
C! Solves a DP symmetric system.
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
C     DSISL SOLVES THE DOUBLE PRECISION SYMMETRIC SYSTEM
C     A * X = B
C     USING THE FACTORS COMPUTED BY DSIFA.
C
C     ON ENTRY:
C
C        A       DOUBLE PRECISION(LDA,N)
C                THE OUTPUT FROM DSIFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        KPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM DSIFA.
C
C        B       DOUBLE PRECISION(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO MAY OCCUR IF  DSICO  HAS SET RCOND.EQ.0.0
C        OR  DSIFA  HAS SET INFO .NE. 0  .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL DSIFA(A,LDA,N,KPVT,INFO)
C           IF (INFO .NE. 0) GO TO ...
C           DO 10 J = 1, P
C              CALL DSISL(A,LDA,N,KPVT,C(1,J))
C        10 CONTINUE
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     JAMES BUNCH, UNIV. CALIF. SAN DIEGO, ARGONNE NAT. LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DAXPY,DFDOT
C     FORTRAN IABS
C
C-----------------------------------------------------------------------
      INTEGER   LDA, N, KPVT(*)
      DOUBLE PRECISION A(LDA,*), B(*)
C
      DOUBLE PRECISION AK, AKM1, BK, BKM1, DFDOT, DENOM, TEMP
      INTEGER   K, KP
      INTEGER   IT1
C-----------------------------------------------------------------------
C
C     LOOP BACKWARD APPLYING THE TRANSFORMATIONS AND
C     D INVERSE TO B.
C
      K = N
   10 IF (K.EQ.0) GO TO 80
         IF (KPVT(K).LT.0) GO TO 40
C
C           1 X 1 PIVOT BLOCK.
C
            IF (K.EQ.1) GO TO 30
               KP = KPVT(K)
               IF (KP.EQ.K) GO TO 20
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
   20          CONTINUE
C
C              APPLY THE TRANSFORMATION.
C
               IT1 = K-1
               CALL DAXPY (IT1, B(K), A(1,K), 1, B(1), 1)
   30       CONTINUE
C
C           APPLY D INVERSE.
C
            B(K) = B(K)/A(K,K)
            K = K - 1
         GO TO 70
   40    CONTINUE
C
C           2 X 2 PIVOT BLOCK.
C
            IF (K.EQ.2) GO TO 60
               KP = ABS(KPVT(K))
               IF (KP.EQ.K - 1) GO TO 50
C
C                 INTERCHANGE.
C
                  TEMP = B(K-1)
                  B(K-1) = B(KP)
                  B(KP) = TEMP
   50          CONTINUE
C
C              APPLY THE TRANSFORMATION.
C
               IT1 = K-2
               CALL DAXPY (IT1, B(K), A(1,K), 1, B(1), 1)
               CALL DAXPY (IT1, B(K-1), A(1,K-1), 1, B(1), 1)
   60       CONTINUE
C
C           APPLY D INVERSE.
C
            AK = A(K,K)/A(K-1,K)
            AKM1 = A(K-1,K-1)/A(K-1,K)
            BK = B(K)/A(K-1,K)
            BKM1 = B(K-1)/A(K-1,K)
            DENOM = AK*AKM1 - 1.0D0
            B(K) = (AKM1*BK - BKM1)/DENOM
            B(K-1) = (AK*BKM1 - BK)/DENOM
            K = K - 2
   70    CONTINUE
      GO TO 10
   80 CONTINUE
C
C     LOOP FORWARD APPLYING THE TRANSFORMATIONS.
C
      K = 1
   90 IF (K.GT.N) GO TO 160
         IF (KPVT(K).LT.0) GO TO 120
C
C           1 X 1 PIVOT BLOCK.
C
            IF (K.EQ.1) GO TO 110
C
C              APPLY THE TRANSFORMATION.
C
               IT1 = K-1
               B(K) = B(K) + DFDOT (IT1, A(1,K), 1, B(1), 1)
               KP = KPVT(K)
               IF (KP.EQ.K) GO TO 100
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
  100          CONTINUE
  110       CONTINUE
            K = K + 1
         GO TO 150
  120    CONTINUE
C
C           2 X 2 PIVOT BLOCK.
C
            IF (K.EQ.1) GO TO 140
C
C              APPLY THE TRANSFORMATION.
C
               IT1 = K-1
               B(K) = B(K) + DFDOT (IT1, A(1,K), 1, B(1), 1)
               B(K+1) = B(K+1) + DFDOT (IT1, A(1,K+1), 1, B(1), 1)
               KP = ABS (KPVT(K))
               IF (KP.EQ.K) GO TO 130
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
  130          CONTINUE
  140       CONTINUE
            K = K + 2
  150    CONTINUE
      GO TO 90
  160 CONTINUE
C
      RETURN
      END

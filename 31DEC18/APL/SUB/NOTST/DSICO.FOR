      SUBROUTINE DSICO (A, LDA, N, KPVT, RCOND, Z)
C-----------------------------------------------------------------------
C! Factors a symmetric matrix.
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
C     DSICO FACTORS A DOUBLE PRECISION SYMMETRIC MATRIX BY ELIMINATION
C     WITH SYMMETRIC PIVOTING AND ESTIMATES THE CONDITION OF THE
C     MATRIX.
C
C     IF  RCOND  IS NOT NEEDED, DSIFA IS SLIGHTLY FASTER.
C     TO SOLVE  A*X = B , FOLLOW DSICO BY DSISL.
C     TO COMPUTE  INVERSE(A)*C , FOLLOW DSICO BY DSISL.
C     TO COMPUTE  INVERSE(A) , FOLLOW DSICO BY DSIDI.
C     TO COMPUTE  DETERMINANT(A) , FOLLOW DSICO BY DSIDI.
C     TO COMPUTE  INERTIA(A), FOLLOW DSICO BY DSIDI.
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE SYMMETRIC MATRIX TO BE FACTORED.
C                ONLY THE DIAGONAL AND UPPER TRIANGLE ARE USED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     OUTPUT
C
C        A       A BLOCK DIAGONAL MATRIX AND THE MULTIPLIERS WHICH
C                WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = U*D*TRANS(U)
C                WHERE  U  IS A PRODUCT OF PERMUTATION AND UNIT
C                UPPER TRIANGULAR MATRICES , TRANS(U) IS THE
C                TRANSPOSE OF  U , AND  D  IS BLOCK DIAGONAL
C                WITH 1 BY 1 AND 2 BY 2 BLOCKS.
C
C        KPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        RCOND   DOUBLE PRECISION
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
C                           1.0 + RCOND.EQ.1.0
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.
C
C        Z       DOUBLE PRECISION(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     LINPACK DSIFA
C     BLAS DAXPY,DFDOT,DSCAL,DASUM
C     FORTRAN DABS,DMAX1,IABS,DSIGN
C-----------------------------------------------------------------------
      INTEGER LDA, N
      INTEGER   KPVT(*)
      DOUBLE PRECISION A(LDA,*), Z(*)
      DOUBLE PRECISION RCOND
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION AK, AKM1, BK, BKM1, DFDOT, DENOM, EK, T
      DOUBLE PRECISION ANORM, S, DASUM, YNORM
      INTEGER   I, INFO, J, JM1, K, KP, KPS, KS
      INTEGER   IT1
C-----------------------------------------------------------------------
C     FIND NORM OF A USING ONLY UPPER HALF
C
      DO 30 J = 1, N
         Z(J) = DASUM (J, A(1,J), 1)
         JM1 = J - 1
         IF (JM1.LT.1) GO TO 20
         DO 10 I = 1, JM1
            Z(I) = Z(I) + ABS (A(I,J))
   10    CONTINUE
   20    CONTINUE
   30 CONTINUE
      ANORM = 0.0D0
      DO 40 J = 1, N
         ANORM = MAX (ANORM,Z(J))
   40 CONTINUE
C
C     FACTOR
C
      CALL DSIFA (A, LDA, N, KPVT, INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  A*Y = E .
C     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
C     GROWTH IN THE ELEMENTS OF W  WHERE  U*D*W = E .
C     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
C
C     SOLVE U*D*W = E
C
      EK = 1.0D0
      DO 50 J = 1, N
         Z(J) = 0.0D0
   50 CONTINUE
      K = N
   60 IF (K.EQ.0) GO TO 120
         KS = 1
         IF (KPVT(K).LT.0) KS = 2
         KP = ABS (KPVT(K))
         KPS = K + 1 - KS
         IF (KP.EQ.KPS) GO TO 70
            T = Z(KPS)
            Z(KPS) = Z(KP)
            Z(KP) = T
   70    CONTINUE
         IF (Z(K) .NE. 0.0D0) EK = SIGN(EK, Z(K))
         Z(K) = Z(K) + EK
         IT1 = K-KS
         CALL DAXPY (IT1, Z(K), A(1,K), 1, Z(1), 1)
         IF (KS.EQ.1) GO TO 80
            IF (Z(K-1) .NE. 0.0D0) EK = SIGN(EK,Z(K-1))
            Z(K-1) = Z(K-1) + EK
            IT1 = K-KS
            CALL DAXPY (IT1, Z(K-1), A(1,K-1), 1, Z(1), 1)
   80    CONTINUE
         IF (KS.EQ.2) GO TO 100
            IF (ABS (Z(K)).LE.ABS (A(K,K))) GO TO 90
               S = ABS (A(K,K)) / ABS (Z(K))
               CALL DSCAL (N, S, Z, 1)
               EK = S*EK
   90       CONTINUE
            IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
            IF (A(K,K).EQ.0.0D0) Z(K) = 1.0D0
         GO TO 110
  100    CONTINUE
            AK = A(K,K)/A(K-1,K)
            AKM1 = A(K-1,K-1)/A(K-1,K)
            BK = Z(K)/A(K-1,K)
            BKM1 = Z(K-1)/A(K-1,K)
            DENOM = AK*AKM1 - 1.0D0
            Z(K) = (AKM1*BK - BKM1)/DENOM
            Z(K-1) = (AK*BKM1 - BK)/DENOM
  110    CONTINUE
         K = K - KS
      GO TO 60
  120 CONTINUE
      S = 1.0D0/DASUM (N, Z, 1)
      CALL DSCAL (N, S, Z, 1)
C
C     SOLVE TRANS(U)*Y = W
C
      K = 1
  130 IF (K.GT.N) GO TO 160
         KS = 1
         IF (KPVT(K).LT.0) KS = 2
         IF (K.EQ.1) GO TO 150
            IT1 = K-1
            Z(K) = Z(K) + DFDOT (IT1, A(1,K), 1, Z(1), 1)
            IF (KS.EQ.2)
     *         Z(K+1) = Z(K+1) + DFDOT (IT1, A(1,K+1), 1, Z(1), 1)
            KP = ABS (KPVT(K))
            IF (KP.EQ.K) GO TO 140
               T = Z(K)
               Z(K) = Z(KP)
               Z(KP) = T
  140       CONTINUE
  150    CONTINUE
         K = K + KS
      GO TO 130
  160 CONTINUE
      S = 1.0D0 / DASUM (N, Z, 1)
      CALL DSCAL (N, S, Z, 1)
C
      YNORM = 1.0D0
C
C     SOLVE U*D*V = Y
C
      K = N
  170 IF (K.EQ.0) GO TO 230
         KS = 1
         IF (KPVT(K).LT.0) KS = 2
         IF (K.EQ.KS) GO TO 190
            KP = ABS (KPVT(K))
            KPS = K + 1 - KS
            IF (KP.EQ.KPS) GO TO 180
               T = Z(KPS)
               Z(KPS) = Z(KP)
               Z(KP) = T
  180       CONTINUE
            IT1 = K-KS
            CALL DAXPY (IT1, Z(K), A(1,K), 1, Z(1), 1)
            IF (KS.EQ.2)
     *         CALL DAXPY (IT1, Z(K-1), A(1,K-1), 1, Z(1), 1)
  190    CONTINUE
         IF (KS.EQ.2) GO TO 210
            IF (ABS (Z(K)).LE.ABS (A(K,K))) GO TO 200
               S = ABS (A(K,K))/ABS (Z(K))
               CALL DSCAL (N, S, Z, 1)
               YNORM = S*YNORM
  200       CONTINUE
            IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
            IF (A(K,K).EQ.0.0D0) Z(K) = 1.0D0
         GO TO 220
  210    CONTINUE
            AK = A(K,K)/A(K-1,K)
            AKM1 = A(K-1,K-1)/A(K-1,K)
            BK = Z(K)/A(K-1,K)
            BKM1 = Z(K-1)/A(K-1,K)
            DENOM = AK*AKM1 - 1.0D0
            Z(K) = (AKM1*BK - BKM1)/DENOM
            Z(K-1) = (AK*BKM1 - BK)/DENOM
  220    CONTINUE
         K = K - KS
      GO TO 170
  230 CONTINUE
      S = 1.0D0 / DASUM (N, Z, 1)
      CALL DSCAL (N, S, Z, 1)
      YNORM = S*YNORM
C
C     SOLVE TRANS(U)*Z = V
C
      K = 1
  240 IF (K.GT.N) GO TO 270
         KS = 1
         IF (KPVT(K).LT.0) KS = 2
         IF (K.EQ.1) GO TO 260
            IT1 = K-1
            Z(K) = Z(K) + DFDOT (IT1, A(1,K), 1, Z(1), 1)
            IF (KS.EQ.2)
     *         Z(K+1) = Z(K+1) + DFDOT (IT1, A(1,K+1), 1, Z(1), 1)
            KP = ABS (KPVT(K))
            IF (KP.EQ.K) GO TO 250
               T = Z(K)
               Z(K) = Z(KP)
               Z(KP) = T
  250       CONTINUE
  260    CONTINUE
         K = K + KS
      GO TO 240
  270 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0D0  / DASUM (N, Z, 1)
      CALL DSCAL (N, S, Z, 1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM.EQ.0.0D0) RCOND = 0.0D0
C
 999  RETURN
      END

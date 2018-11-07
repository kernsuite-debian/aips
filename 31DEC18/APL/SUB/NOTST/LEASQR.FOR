      SUBROUTINE LEASQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES,
     *   VARRES, VARY, FIT, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C     LEASQR does the matrix inversion and other necessary tasks
C     involved in a least squares analysis.
C
C     Given:
C          NP        I     Number of parameters.
C          N         R     The number of observations.
C          SUM       R     Error sum.
C          SSQ       R     Square error sum.
C          R(NP)     R     Results vector.
C
C     Given and returned:
C          M(NP,NP)  R     On input, the upper triangular part contains
C                          the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                          the covariance matrix.  Diagonal elements of
C                          the covariance matrix are stored in VX.
C
C     Returned:
C          X(NP)     R     Vector holding the least squares solution.
C          VX(NP)    R     Variance of the best fit parameters.
C          SSQRES    R     Sum of squares of the residuals.
C          VARRES    R     Variance of the residuals.
C          VARY      R     Variance of the error values.
C          FIT       R     Goodness of fit parameter, lies between 0
C                          and 1.
C          IERR      I     Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C
C     Called:
C          none
C
C     Algorithm:
C          LU-triangular factorization with scaled partial pivoting.
C          The sub-diagonal triangular matrix contains the scaling
C          factors used at each step in the Gaussian elimination.  Row
C          interchanges are recorded in vectors MXS and SXM.
C             During forward substitution, the pivoting and Gaussian
C          elimination operations performed on matrix M are applied to
C          vector R.  Vector X holds the intermediate result.
C             On backward substitution, successive elements of the
C          solution vector, X, are calculated by substitution of the
C          preceding elements into the equations of the upper triangular
C          factorization of the design matrix.
C
C     Notes:
C       1) Strictly speaking, the design matrix will usually contain
C          rows of zeroes and therefore be singular.  This arises if no
C          observations sensitive to a particular parameter have been
C          done.
C             In practice, any such singularities are ignored and the
C          associated parameters remain undetermined.
C
C       2) The covariance matrix is the inverse of M(i,j) multiplied by
C          the variance of the residuals.  It is obtained by forward and
C          backward substitution on the columns of the unit matrix.
C
C       3) Two statement functions, C, and SC have been employed to
C          partially alleviate the problems posed by passing arrays in
C          FORTRAN.  The design/covariance matrix m(i,j) is copied into
C          the working vector s(i).  This is addressed by using C, and
C          SC in an attempt to make it look like the matrix that it
C          actually represents.
C
C       4) The maximum size problem that LEASQR can handle is set by
C          parameter MX.= 200.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C-----------------------------------------------------------------------
C     Parameter which determines the maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 200)
C
      INTEGER   C, I, IERR, ITEMP, J, K, MXS(MX), NF, NP, PIVOT,
     *          SXM(MX)
      REAL      COLMAX, DTEMP, FIT, M(NP,NP), N, R(NP), RESIDU, RLEN,
     *          ROWMAX(MX), S(MX*MX), SC, RTEMP, SSQ, SSQRES, SUM,
     *          VARRES, VARY, VX(NP), W(MX), X(NP)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C     Statement functions for array manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C  Initialize.
C     Anticipate and return immediately on error.
      IERR = 1

C     Initialize arrays.
      DO 40 I = 1, NP
C        Vector which records row interchanges.
         MXS(I) = I

C        The solution and variance vectors.
         X(I)  = 0.0
         VX(I) = 0.0

C        Copy the design matrix and zero the covariance matrix.
         DO 10 J = 1, I-1
            M(I,J) = 0.0
            S(C(I,J)) = M(J,I)
 10      CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20      CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30      CONTINUE
 40   CONTINUE

      VARY   = 0.0
      SSQRES = 0.0
      VARRES = 0.0
      FIT    = 0.0


C     Find the number of degrees of freedom.
      NF = N
      DO 60 I = 1, NP
         IF (ROWMAX(I).NE.0.0) THEN
            NF = NF - 1
         ELSE IF (R(I).NE.0.0) THEN
C           Any row of zeroes must extend to the results vector.
            WRITE (MSGTXT,50) I
 50         FORMAT ('LEASQR: Design matrix inconsistency in row',I4)
            CALL MSGWRT (6)
         END IF
 60   CONTINUE

      IF (NF.LE.1) THEN
         WRITE (MSGTXT,70)
 70      FORMAT ('LEASQR: Insufficient degrees of freedom.')
         CALL MSGWRT (6)
         IERR = 2
         RETURN
      END IF


C  Factorize the matrix.
      DO 120 K = 1, NP
C        Check for a row of zeroes.
         IF (ROWMAX(K).EQ.0.0) GO TO 120

C        A non-zero row maximum implies non-zero diagonal element.
         IF (SC(K,K).EQ.0.0) THEN
            WRITE (MSGTXT,50) MXS(K)
            CALL MSGWRT (6)
            GO TO 120
         END IF

C        Decide whether to pivot.
         COLMAX = ABS(SC(K,K))/ROWMAX(K)
         PIVOT = K
         DO 80 I = K+1, NP
            IF (ROWMAX(I).NE.0.0) THEN
               IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                  COLMAX = ABS(SC(I,K))/ROWMAX(I)
                  PIVOT = I
               END IF
            END IF
 80      CONTINUE

         IF (PIVOT.GT.K) THEN
C           We must pivot, interchange the rows of the design matrix.
            DO 90 J = 1, NP
               DTEMP = SC(PIVOT,J)
               S(C(PIVOT,J)) = SC(K,J)
               S(C(K,J)) = DTEMP
 90         CONTINUE

C           Don't forget the vector of row maxima.
            DTEMP = ROWMAX(PIVOT)
            ROWMAX(PIVOT) = ROWMAX(K)
            ROWMAX(K) = DTEMP

C           Record the interchange for later use.
            ITEMP = MXS(PIVOT)
            MXS(PIVOT) = MXS(K)
            MXS(K) = ITEMP
         END IF

C        Gaussian elimination.
         DO 110 I = K+1, NP
C           Nothing to do if SC(i,k) is zero.
            IF (SC(I,K).NE.0.0) THEN
C              Save the scaling factor.
               S(C(I,K)) = SC(I,K)/SC(K,K)

C              Subtract rows.
               DO 100 J = K+1, NP
                  S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100           CONTINUE
            END IF
 110     CONTINUE
 120  CONTINUE

C     MXS(i) records which row of M corresponds to row i of SC.
C     SXM(i) records which row of S corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130  CONTINUE


C  Solve the normal equations.
      DO 150 I = 1, NP
C        Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1, I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140     CONTINUE
 150  CONTINUE

      DO 170 I = NP, 1, -1
C        Backward substitution.
         IF (SC(I,I).NE.0.0) THEN
            DO 160 J = I+1, NP
               W(I) = W(I) - SC(I,J)*W(J)
 160        CONTINUE
            W(I) = W(I)/SC(I,I)
         END IF
         X(I) = W(I)
 170  CONTINUE

C     Check that the solution is acceptable.
      RLEN = 0.0
      RESIDU = 0.0
      DO 200 I = 1, NP
         RTEMP = 0.0
         DO 180 J = 1, I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180     CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190     CONTINUE

         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200  CONTINUE

      IF (RESIDU.GT.0.001*RLEN) THEN
         WRITE (MSGTXT,210) RESIDU/RLEN
 210     FORMAT ('LEASQR: The solution is discrepant at',E8.1)
         CALL MSGWRT (6)
         RETURN
      END IF


C  Determine goodness-of-fit estimates, and statistical errors.
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220  CONTINUE
      IF (SSQRES.LT.0.0) SSQRES = 0.0

      VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - 1.0)
      FIT = 1.0
      IF (VARY.NE.0.0) FIT = 1.0 - SSQRES/(SSQ - SUM*SUM/N)

C     Determine the covariance matrix.
      DO 280 K = 1, NP
C        Forward substitution affects only that part of W() below the
C        first non-zero entry.
         DO 230 I = 1, SXM(K)-1
            W(I) = 0.0
 230     CONTINUE
         W(SXM(K)) = 1.0

         DO 250 I = SXM(K)+1, NP
C           Forward substitution.
            W(I) = 0.0
            DO 240 J = SXM(K), I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240        CONTINUE
 250     CONTINUE

         DO 270 I = NP, K, -1
            IF (SC(I,I).NE.0.0) THEN
C              Backward substitution.
               DO 260 J = I+1, NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260           CONTINUE
               W(I) = W(I)/SC(I,I)
            END IF

            IF (I.NE.K) THEN
C              Off diagonal elements of the covariance matrix.
               M(I,K) = VARRES*W(I)
            ELSE IF (I.EQ.K) THEN
C              Diagonal elements of the covariance matrix.
               VX(K)  = VARRES*W(I)
            END IF
 270     CONTINUE
 280  CONTINUE


C  Successful completion.
      IERR = 0


      RETURN
      END

      SUBROUTINE DLESQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES, VARRES,
     *   VARY, FIT, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2010, 2013
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
C   DLESQR does the matrix inversion and other necessary tasks involved
C   in a least squares analysis.
C   Inputs:
C      NP       I          Number of parameters.
C      N        D          The number of observations.
C      SUM      D          Error sum.
C      SSQ      D          Square error sum.
C      R        D(NP)      Results vector.
C   In/out:
C      M        D(NP,NP)   On input, the upper triangular part contains
C                            the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                            the covariance matrix.  Diagonal elements of
C                            the covariance matrix are stored in VX.
C   Output:
C      X        D(NP)      Vector holding the least squares solution.
C      VX       D(NP)      Variance of the best fit parameters.
C      SSQRES   D          Sum of squares of the residuals.
C      VARRES   D          Variance of the residuals.
C      VARY     D          Variance of the error values.
C      FIT      D          Goodness of fit parameter, lies between 0 & 1
C      IERR     I          Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C     Called:
C          none
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
C       4) The maximum size problem that DLESQR can handle is set by
C          parameter MX.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C          Leonid Kogan modified the original version to
C          double precision; 2001/May/30.
C-----------------------------------------------------------------------
      INTEGER   NP, IERR
      DOUBLE PRECISION N, SUM, SSQ, R(NP), M(NP,NP), X(NP), VX(NP),
     *   SSQRES, VARRES, VARY, FIT
C                                       Maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 500)

      INTEGER   C, I, ITEMP, J, K, MXS(MX), NF, PIVOT, SXM(MX)
      DOUBLE PRECISION   COLMAX, DTEMP, RESIDU, RLEN, ROWMAX(MX),
     *   S(MX*MX), SC, RTEMP, W(MX), ZERO, ONE
      INCLUDE 'INCS:DMSG.INC'
      DATA ZERO, ONE /0.0D0, 1.0D0/
C-----------------------------------------------------------------------
C                                       Statement functions for array
C                                       manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C                                       Initialize.
C                                       return immediately on error.
      IERR = 1
      DO 40 I = 1,NP
C                                       records row interchanges.
         MXS(I) = I
C                                       solution and variance vectors.
         X(I)  = ZERO
         VX(I) = ZERO
C                                       Copy the design matrix and zero
C                                       the covariance matrix.
         DO 10 J = 1,I-1
            M(I,J) = ZERO
            S(C(I,J)) = M(J,I)
 10         CONTINUE
         DO 20 J = I,NP
            S(C(I,J)) = M(I,J)
 20         CONTINUE
C                                       Find the maximum absolute
C                                       element in each row.
         ROWMAX(I) = ZERO
         DO 30 J = 1, NP
            ROWMAX(I) = MAX (ROWMAX(I), ABS (SC(I,J)))
 30         CONTINUE
 40      CONTINUE
      VARY   = ZERO
      SSQRES = ZERO
      VARRES = ZERO
      FIT    = ZERO
C                                       Find number of degrees freedom
      NF = N
      DO 60 I = 1,NP
         IF (ROWMAX(I).NE.ZERO) THEN
            NF = NF - 1
C                                       Any row of zeroes must extend
C                                       to the results vector.
         ELSE IF (R(I).NE.ZERO) THEN
            WRITE (MSGTXT,1050) I
            CALL MSGWRT (6)
            END IF
 60      CONTINUE
C                                       insufficient deg of freesom
      IF (NF.LE.1) THEN
         WRITE (MSGTXT,1070)
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         END IF
C                                       Factorize the matrix.
      DO 120 K = 1,NP
C                                       Check for a row of zeroes.
         IF (ROWMAX(K).NE.ZERO) THEN
C                                       A non-zero row maximum =>
C                                       non-zero diagonal element.
            IF (SC(K,K).EQ.ZERO) THEN
               WRITE (MSGTXT,1050) MXS(K)
               CALL MSGWRT (6)
               GO TO 120
               END IF
C                                       Decide whether to pivot.
            COLMAX = ABS(SC(K,K)) / ROWMAX(K)
            PIVOT = K
            DO 80 I = K+1,NP
               IF (ROWMAX(I).NE.ZERO) THEN
                  IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                     COLMAX = ABS(SC(I,K))/ROWMAX(I)
                     PIVOT = I
                     END IF
                  END IF
 80            CONTINUE
C                                       We must pivot, interchange the
C                                       rows of the design matrix.
            IF (PIVOT.GT.K) THEN
               DO 90 J = 1, NP
                  DTEMP = SC(PIVOT,J)
                  S(C(PIVOT,J)) = SC(K,J)
                  S(C(K,J)) = DTEMP
 90               CONTINUE
C                                       Don't forget the vector of
C                                       row maxima.
               DTEMP = ROWMAX(PIVOT)
               ROWMAX(PIVOT) = ROWMAX(K)
               ROWMAX(K) = DTEMP
C                                       Record the interchange for later
               ITEMP = MXS(PIVOT)
               MXS(PIVOT) = MXS(K)
               MXS(K) = ITEMP
               END IF
C                                       Gaussian elimination.
            DO 110 I = K+1, NP
C                                       Nothing to do if SC(i,k) is 0
               IF (SC(I,K).NE.ZERO) THEN
C                                       Save the scaling factor.
                  S(C(I,K)) = SC(I,K)/SC(K,K)
C                                       Subtract rows.
                  DO 100 J = K+1,NP
                     S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100                 CONTINUE
                  END IF
 110           CONTINUE
            END IF
 120     CONTINUE
C                                       MXS(i) records which row of M
C                                          corresponds to row i of SC.
C                                       SXM(i) records which row of S
C                                           corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130     CONTINUE
C                                       Solve the normal equations.
      DO 150 I = 1,NP
C                                       Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1,I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140        CONTINUE
 150     CONTINUE
      DO 170 I = NP,1,-1
C                                       Backward substitution.
         IF (SC(I,I).NE.ZERO) THEN
            DO 160 J = I+1,NP
               W(I) = W(I) - SC(I,J)*W(J)
 160           CONTINUE
            W(I) = W(I)/SC(I,I)
            END IF
         X(I) = W(I)
 170     CONTINUE
C                                       Check if solution is acceptable.
      RLEN = ZERO
      RESIDU = ZERO
      DO 200 I = 1,NP
         RTEMP = ZERO
         DO 180 J = 1,I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180        CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190        CONTINUE
         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200     CONTINUE
C                                       fails
      IF (RESIDU.GT.0.001*RLEN) THEN
         WRITE (MSGTXT,1210) RESIDU/RLEN
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Determine goodness-of-fit
C                                       estimates and statistical errors
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220     CONTINUE
      SSQRES = MAX (ZERO, SSQRES)
      VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - ONE)
      FIT = ONE
      IF (VARY.NE.ZERO) FIT = ONE - SSQRES / (SSQ - SUM*SUM/N)
C                                       Determine the covariance matrix.
      DO 280 K = 1, NP
C                                       Forward substitution affects
C                                       only that part of W() below the
C                                       first non-zero entry.
         DO 230 I = 1,SXM(K)-1
            W(I) = ZERO
 230        CONTINUE
         W(SXM(K)) = ONE
C                                       Forward substitution.
         DO 250 I = SXM(K)+1,NP
            W(I) = 0.0
            DO 240 J = SXM(K),I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240           CONTINUE
 250        CONTINUE
         DO 270 I = NP,K, -1
            IF (SC(I,I).NE.ZERO) THEN
C                                       Backward substitution.
               DO 260 J = I+1,NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260              CONTINUE
               W(I) = W(I)/SC(I,I)
               END IF
C                                       Off diagonal elements of the
C                                       covariance matrix.
            IF (I.NE.K) THEN
               M(I,K) = VARRES*W(I)
C                                       Diagonal elements of the
C                                       covariance matrix.
            ELSE IF (I.EQ.K) THEN
               VX(K)  = VARRES*W(I)
               END IF
 270        CONTINUE
 280     CONTINUE
C                                       Successful completion.
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('DLESQR: Design matrix inconsistency in row',I4)
 1070 FORMAT ('DLESQR: Insufficient degrees of freedom.')
 1210 FORMAT ('DLESQR: The solution is discrepant at',E8.1)
      END

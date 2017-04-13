      SUBROUTINE LMDER (FCN, M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL,
     *   GTOL, MAXFEV, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV, NJEV,
     *   IPVT, QTF, WA1, WA2, WA3, WA4)
C-----------------------------------------------------------------------
C! minimize the sum of squares of M nonlinear functions in N variables
C# modeling math
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
C   The purpose of LMDER is to minimize the sum of the squares of M
C   nonlinear functions in N variables by a modification of the
C   Levenberg-Marquardt algorithm. The user must provide a subroutine
C   which calculates the functions and the Jacobian.
C   Subprograms called
C      User-supplied ...... FCN
C      Minpack-supplied ... DPMPAR, ENORM, LMPAR, QRFAC
C      FORTRAN-supplied ... ABS, MAX, MIN, SQRT, MOD
C   Argonne National Laboratory. Minpack project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      FCN     EXTERNAL The name of the user-supplied subroutine
C                   which calculates the functions and the jacobian.
C                   FCN must be declared in an external statement in
C                   the user calling program, and should be written
C                   as follows:
C *********************************************************************
C         SUBROUTINE FCN (M, N, X, FVEC, FJAC, LDFJAC, IFLAG)
C                                       Variables used as adjustable
C                                       array dimension specifiers
C                                       should be declared simply as
C                                       INTEGER
C         INTEGER M,N,LDFJAC
C         INTEGER*2 IFLAG
C         REAL*8 X(N),FVEC(M),FJAC(LDFJAC,N)
C         ----------
C         If IFLAG = 1 calculate the functions at X and
C         return this vector in FVEC. Do not alter FJAC.
C         If IFLAG = 2 calculate the Jacobian at X and
C         return this matrix in FJAC. Do not alter FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless
C         the user wants to terminate execution of LMDER.
C         In this case set IFLAG to a negative INTEGER*2.
C**********************************************************************
C      M       I   The number of functions (adj. array dim.).
C      N       I   The number of variables (adj. array dim.; N
C                       must not exceed M.
C      X       D(N)   contains an initial estimate of the
C                   solution vector.
C      FTOL    D   Termination occurs when both the actual and
C                   predicted relative reductions in the sum of
C                   squares are at most FTOL. Therefore, FTOL measures
C                   the relative error desired in the sum of squares.
C      XTOL    D     Termination occurs when the relative error
C                    between two consecutive iterates is at most XTOL.
C                    therefore, XTOL measures the relative error desired
C                    in the approximate solution.
C      GTOL    D     Termination occurs when the cosine of the angle
C                    between FVEC and any column of the Jacobian is at
C                    most GTOL in absolute value. Therefore, GTOL
C                    measures the orthogonality desired between the
C                    function vector and the columns of the Jacobian.
C      MAXFEV  I     Termination occurs when the number of calls to
C                    FCN with IFLAG = 1 has reached MAXFEV.
C      DIAG    D(N)  If MODE = 1 (see below), DIAG is internally
C                    set. If MODE = 2, DIAG must contain positive
C                    entries that serve as multiplicative scale factors
C                    for the variables.
C      MODE    I     If MODE = 1, the variables will be scaled
C                    internally. If MODE = 2, the scaling is specified
C                    by the input DIAG. other values of mode are
C                    equivalent to MODE = 1.
C      FACTOR  D     Used in determining the initial step bound.
C                    This bound is set to the product of FACTOR and
C                    the Euclidean norm of DIAG*X if nonzero, or else
C                    to FACTOR itself. In most cases FACTOR should lie
C                    in the interval (.1,100.0).  100.0 is a generally
C                    recommended value.
C      NPRINT  I     enables controlled printing of iterates if it
C                    is positive. In this case, FCN is called with
C                    IFLAG = 0 at the beginning of the first iteration
C                    and every NPRINT iterations thereafter and
C                    immediately prior to return, with X, FVEC, and
C                    FJAC available for printing. FVEC and FJAC should
C                    not be altered. If NPRINT is not positive, no
C                    special calls of FCN with IFLAG = 0 are made.
C      LDFJAC  I     variable not less than M which specifies the
C                    leading dimension of the array FJAC (adj. array
C                    dim.).
C   Outputs:
C      X       D(N)  contains the final estimate of the
C                    solution vector.
C      FVEC    D(M)  contains the functions evaluated at
C                    the output X.
C      FJAC    D(M,N)  The upper N by N submatrix of FJAC
C                    contains an upper triangular matrix R with
C                    diagonal elements of nonincreasing magnitude
C                    such that
C                           T     T           T
C                          P *(JAC *JAC)*P = R *R,
C                    where P is a permutation matrix and JAC is the
C                    final calculated Jacobian. Column J of P is column
C                    IPVT(J) (see below) of the identity matrix. The
C                    lower trapezoidal part of FJAC contains information
C                    generated during the computation of R.
C      INFO    I     If the user has terminated execution, INFO is
C                    set to the (negative) value of IFLAG. SEE
C                    description of FCN. otherwise, INFO is set
C                    as follows:
C                    INFO = 0  Improper input parameters.
C                    INFO = 1  Algorithm estimates that the relative
C                    error in the sum of squares is at most FTOL.
C                    INFO = 2  Algorithm estimates that the relative
C                    error between X and the solution is at most XTOL.
C                    INFO = 3  Conditions for INFO = 1 and INFO = 2
C                    both hold.
C                    INFO = 4  The cosine of the angle between FVEC
C                    and any column of the Jacobian is at most GTOL
C                    in absolute value.
C                    INFO = 5  Number of calls to FCN with IFLAG = 1 HAS
C                    reached MAXFEV.
C                    INFO = 6  FTOL is too small. No further reduction
C                    in the sum of squares is possible.
C                    INFO = 7  XTOL is too small. No further improvement
C                    in the approximate solution X is possible.
C                    INFO = 8  GTOL is too small. FVEC is orthogonal
C                    to the columns of the Jacobian to machine
C                    precision.
C      NFEV    I     Number of calls to FCN with IFLAG = 1.
C      NJEV    I     The number of calls to FCN with IFLAG = 2.
C      IPVT    I(N)  Defines a permutation matrix P such that
C                                JAC*P = Q*R,
C                    where JAC is the final calculated Jacobian, Q is
C                    orthogonal (not stored), and R is upper triangular
C                    with diagonal elements of nonincreasing magnitude.
C                    column J of P is column IPVT(J) of the identity
C                    matrix.
C      QTF     D(N)  contains the first N elements of the
C                    vector (Q transpose)*FVEC.
C      WA1     D(N)  work array.
C      WA2     D(N)  work array.
C      WA3     D(N)  work array.
C      WA4     D(M)  work array.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
C     INTEGER   FCN
      INTEGER   M, N, LDFJAC, MAXFEV, MODE, NPRINT, INFO, NFEV,
     *   NJEV, IPVT(N)
      DOUBLE PRECISION FTOL, XTOL, GTOL, FACTOR, X(N), FVEC(M),
     *   FJAC(LDFJAC,N), DIAG(N), QTF(N), WA1(N), WA2(N), WA3(N),
     *   WA4(M)
C
      INTEGER   I, IFLAG, ITER, J, L
      DOUBLE PRECISION ACTRED, DELTA, DIRDER, EPSMCH, FNORM, FNORM1,
     *   GNORM, ONE, PAR, PNORM, PRERED, P1, P5, P25, P75, P0001, RATIO,
     *   SUM, TEMP, TEMP1, TEMP2, XNORM, ZERO
      DOUBLE PRECISION DPMPAR, ENORM
      LOGICAL   T
      DATA T /.TRUE./
      DATA ONE, P1, P5, P25, P75, P0001, ZERO
     *     /1.0D0, 1.0D-1, 5.0D-1, 2.5D-1, 7.5D-1, 1.0D-4, 0.0D0/
C-----------------------------------------------------------------------
C                                       EPSMCH is the machine precision
      EPSMCH = DPMPAR (1)
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C                                       Check the input parameters for
C                                       errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.M) .OR.
     *    (FTOL.LT.ZERO) .OR. (XTOL.LT.ZERO) .OR. (GTOL.LT.ZERO)
     *    .OR. (MAXFEV.LE.0) .OR. (FACTOR.LE.ZERO)) GO TO 300
      IF (MODE.NE.2) GO TO 20
         DO 10 J = 1, N
            IF (DIAG(J).LE.ZERO) GO TO 300
 10         CONTINUE
 20   CONTINUE
C                                 Evaluate the function at the starting
C                                 point and calculate its norm.
      IFLAG = 1
      CALL FCN (M, N, X, FVEC, FJAC, LDFJAC, IFLAG)
      NFEV = 1
      IF (IFLAG.LT.0) GO TO 300
      FNORM = ENORM (M, FVEC)
C                                       Initialize Levenberg-Marquardt
C                                       parameter and iteration counter
      PAR = ZERO
      ITER = 1
C                                       Beginning of the outer loop.
 30   CONTINUE
C                                       Calculate the Jacobian matrix.
         IFLAG = 2
         CALL FCN (M, N, X, FVEC, FJAC, LDFJAC, IFLAG)
         NJEV = NJEV + 1
         IF (IFLAG.LT.0) GO TO 300
C                                       If requested, call FCN to
C                                       enable printing of iterates.
         IF (NPRINT.LE.0) GO TO 40
         IFLAG = 0
         IF (MOD (ITER-1, NPRINT).EQ.0)
     *      CALL FCN (M, N, X, FVEC, FJAC, LDFJAC, IFLAG)
         IF (IFLAG.LT.0) GO TO 300
 40      CONTINUE
C                                       Compute the QR factorization
C                                       of the Jacobian.
         CALL QRFAC (M, N, FJAC, LDFJAC, T, IPVT, N, WA1, WA2, WA3)
C                                       On the first iteration and if
C                                       MODE is 1, scale according
C                                       to the norms of the columns of
C                                       the initial Jacobian.
         IF (ITER.NE.1) GO TO 80
         IF (MODE.EQ.2) GO TO 60
            DO 50 J = 1,N
               DIAG(J) = WA2(J)
               IF (WA2(J).EQ.ZERO) DIAG(J) = ONE
 50            CONTINUE
 60      CONTINUE
C                                       On the first iteration,
C                                       calculate the norm of the
C                                       scaled X and initialize the
C                                       step bound DELTA.
         DO 70 J = 1,N
            WA3(J) = DIAG(J) * X(J)
 70         CONTINUE
         XNORM = ENORM (N, WA3)
         DELTA = FACTOR * XNORM
         IF (DELTA.EQ.ZERO) DELTA = FACTOR
 80      CONTINUE
C                                       Form (Q transpose)*FVEC and
C                                       store the first N components in
C                                       QTF.
         DO 90 I = 1,M
            WA4(I) = FVEC(I)
 90         CONTINUE
         DO 130 J = 1,N
            IF (FJAC(J,J).EQ.ZERO) GO TO 120
               SUM = ZERO
               DO 100 I = J,M
                  SUM = SUM + FJAC(I,J) * WA4(I)
 100              CONTINUE
               TEMP = -SUM / FJAC(J,J)
               DO 110 I = J,M
                  WA4(I) = WA4(I) + FJAC(I,J) * TEMP
 110              CONTINUE
 120        FJAC(J,J) = WA1(J)
            QTF(J) = WA4(J)
 130        CONTINUE
C                                       Compute the norm of the scaled
C                                       gradient.
         GNORM = ZERO
         IF (FNORM.EQ.ZERO) GO TO 170
         DO 160 J = 1,N
            L = IPVT(J)
            IF (WA2(L).EQ.ZERO) GO TO 150
            SUM = ZERO
            DO 140 I = 1,J
               SUM = SUM + FJAC(I,J) * (QTF(I) / FNORM)
 140           CONTINUE
            GNORM = MAX(GNORM, ABS (SUM/WA2(L)))
 150        CONTINUE
 160        CONTINUE
 170     CONTINUE
C                                       Test for convergence of the
C                                       gradient norm.
         IF (GNORM.LE.GTOL) INFO = 4
         IF (INFO.NE.0) GO TO 300
C                                       Rescale if necessary.
         IF (MODE.EQ.2) GO TO 190
            DO 180 J = 1,N
               DIAG(J) = MAX (DIAG(J), WA2(J))
 180           CONTINUE
 190     CONTINUE
C                                       Beginning of the inner loop.
 200     CONTINUE
C                                       Determine the
C                                       Levenberg-Marquardt parameter.
            CALL LMPAR (N, FJAC, LDFJAC, IPVT, DIAG, QTF, DELTA, PAR,
     *         WA1, WA2, WA3, WA4)
C                                       Store the directn P and X + P.
C                                       Calculate the norm of P.
            DO 210 J = 1,N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J) * WA1(J)
 210           CONTINUE
            PNORM = ENORM (N, WA3)
C                                       On the first iteration, adjust
C                                       the initial step bound.
            IF (ITER.EQ.1) DELTA = MIN (DELTA, PNORM)
C                                       Evaluate the function at X + P
C                                       and calculate its norm.
            IFLAG = 1
            CALL FCN (M, N, WA2, WA4, FJAC, LDFJAC, IFLAG)
            NFEV = NFEV + 1
            IF (IFLAG.LT.0) GO TO 300
            FNORM1 = ENORM (M, WA4)
C                                       Compute the scaled actual
C                                       reduction.
            ACTRED = -ONE
            IF (P1*FNORM1.LT.FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C                                       Compute the scaled predicted
C                                       reduction and the scaled
C                                       directional derivative.
            DO 230 J = 1,N
               WA3(J) = ZERO
               L = IPVT(J)
               TEMP = WA1(L)
               DO 220 I = 1,J
                  WA3(I) = WA3(I) + FJAC(I,J) * TEMP
 220              CONTINUE
 230           CONTINUE
            TEMP1 = ENORM(N,WA3)/FNORM
            TEMP2 = (SQRT(PAR)*PNORM)/FNORM
            PRERED = TEMP1**2 + TEMP2**2/P5
            DIRDER = -(TEMP1**2 + TEMP2**2)
C                                       Compute the ratio of the
C                                       actual to the predicted
C                                       reduction.
            RATIO = ZERO
            IF (PRERED .NE. ZERO) RATIO = ACTRED/PRERED
C                                       Update the step bound.
            IF (RATIO .GT. P25) GO TO 240
               IF (ACTRED .GE. ZERO) TEMP = P5
               IF (ACTRED .LT. ZERO)
     *            TEMP = P5*DIRDER/(DIRDER + P5*ACTRED)
               IF (P1*FNORM1 .GE. FNORM .OR. TEMP .LT. P1) TEMP = P1
               DELTA = TEMP*MIN(DELTA,PNORM/P1)
               PAR = PAR/TEMP
               GO TO 260
 240        CONTINUE
               IF (PAR .NE. ZERO .AND. RATIO .LT. P75) GO TO 250
               DELTA = PNORM/P5
               PAR = P5*PAR
 250           CONTINUE
 260        CONTINUE
C                                       Test for successful iteration.
            IF (RATIO .LT. P0001) GO TO 290
C                                       Successful iteration. update
C                                       X, FVEC, and their norms.
            DO 270 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
 270           CONTINUE
            DO 280 I = 1, M
               FVEC(I) = WA4(I)
 280           CONTINUE
            XNORM = ENORM(N, WA2)
            FNORM = FNORM1
            ITER = ITER + 1
 290        CONTINUE
C                                       Tests for convergence.
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     *         .AND. P5*RATIO .LE. ONE) INFO = 1
            IF (DELTA .LE. XTOL*XNORM) INFO = 2
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     *         .AND. P5*RATIO .LE. ONE .AND. INFO .EQ. 2) INFO = 3
            IF (INFO .NE. 0) GO TO 300
C                                       Tests for termination and
C                                       stringent tolerances.
            IF (NFEV .GE. MAXFEV) INFO = 5
            IF (ABS(ACTRED) .LE. EPSMCH .AND. PRERED .LE. EPSMCH
     *         .AND. P5*RATIO .LE. ONE) INFO = 6
            IF (DELTA .LE. EPSMCH*XNORM) INFO = 7
            IF (GNORM .LE. EPSMCH) INFO = 8
            IF (INFO .NE. 0) GO TO 300
C                                       End of the inner loop. Repeat
C                                       if iteration unsuccessful.
            IF (RATIO .LT. P0001) GO TO 200
C                                       End of the outer loop.
         GO TO 30
 300  CONTINUE
C                                       Termination, either normal
C                                       or user imposed.
      IF (IFLAG.LT.0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT.GT.0) CALL FCN (M, N, X, FVEC, FJAC, LDFJAC, IFLAG)
      RETURN
C
      END

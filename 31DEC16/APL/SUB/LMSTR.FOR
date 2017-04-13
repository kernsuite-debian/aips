      SUBROUTINE LMSTR (FCN, M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL,
     *   GTOL, MAXFEV, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV, NJEV,
     *   IPVT, QTF, WA1, WA2, WA3, WA4)
C-----------------------------------------------------------------------
C! minimize sum of squares of M nonlinear functions in N variables
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
C   The purpose of LMSTR is to minimize the sum of the squares of M
C   nonlinear functions in N variables by a modification of the
C   Levenberg-Marquardt algorithm which uses minimum storage.  The user
C   must provide a subroutine which calculates the functions and the
C   rows of the Jacobian.  Subprograms called
C       User-supplied ...... FCN
C       Minpack-supplied ... DPMPAR, ENORM, LMPAR, QRFAC
C       FORTRAN-supplied ... ABS, MAX, MIN, SQRT, MOD
C   Argonne National Laboratory. Minpack project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      FCN     EXTERNAL The name of the user-supplied subroutine
C                    which calculates the functions and the rows of
C                    the Jacobian.  FCN must be declared in an external
C                    statement in the user calling program, and should
C                    be written as follows:
C *********************************************************************
C         SUBROUTINE FCN (M, N, X, FVEC, FJROW, IFLAG)
C         INTEGER   M, N, IFLAG
C         DOUBLE PRECISION X(N), FVEC(M), FJROW(N)
C            If IFLAG = 1 calculate the functions at X and
C               return this vector if FVEC.
C            If IFLAG <> 1 calculate the (IFLAG - 1)st row of the
C               Jacobian at X and return this vector in FJROW.
C         RETURN
C         END
C            The value of IFLAG should not be changed by FCN unless
C            the user wants to terminate execution of LMSTR.
C            In this case set IFLAG to a negative INTEGER*2.
C**********************************************************************
C      M       I     The number of functions (adj. array dim.).
C      N       I     The number of variables. N must not exceed M
C                    (adj. array dim.).
C      X       D(N)  contains an initial estimate of solution vector
C      FTOL    D     Termination occurs when both the actual and
C                    predicted relative reductions in the sum of
C                    squares are at most FTOL. Therefore, FTOL measures
C                    the relative error desired in the sum of squares.
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
C      LDFJAC  I     variable not less than N which specifies the
C                    leading dimension of the array FJAC (adj. array
C                    dim.)
C   Outputs:
C      X       D(N)  contains the final estimate of solution vector
C      FVEC    D(M)  contains the functions evaluated at the output X
C      FJAC    D(N,N)   contains an upper triangular matrix R with
C                    diagonal elements of nonincreasing magnitude
C                    such that
C                             T     T           T
C                            P *(JAC *JAC)*P = R *R,
C                    where P is a permutation matrix and JAC is the
C                    final calculated Jacobian. Column J of P is column
C                    IPVT(J) (see below) of the identity matrix. The
C                    lower trapezoidal part of FJAC contains information
C                    generated during the computation of R.
C      INFO    I     If the user has terminated execution, INFO is
C                    set to the (negative) value of IFLAG. See
C                    description of FCN. otherwise, INFO is set
C                    as follows:
C                    INFO = 0  Improper input parameters.
C                    INFO = 1  Algorithm estimates that the relative
C                       error in the sum of squares is at most FTOL.
C                    INFO = 2  Algorithm estimates that the relative
C                       error between X and the solution is at most XTOL
C                    INFO = 3  Conditions for INFO = 1 and INFO = 2
C                       both hold.
C                    INFO = 4  The cosine of the angle between FVEC
C                       and any column of the Jacobian is at most GTOL
C                       in absolute value.
C                    INFO = 5  Number of calls to FCN with IFLAG = 1 has
C                       reached MAXFEV.
C                    INFO = 6  FTOL is too small. No further reduction
C                       in the sum of squares is possible.
C                    INFO = 7  XTOL is too small. No further improvement
C                       in the approximate solution X is possible.
C                    INFO = 8  GTOL is too small. FVEC is orthogonal
C                       to the columns of the Jacobian to machine
C                       precision.
C      NFEV    I     Number of calls to FCN with IFLAG = 1.
C      NJEV    I     The number of calls to FCN with IFLAG = 2.
C      IPVT    I(N)  Defines a permutation matrix P such that
C                            JAC*P = Q*R,
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
C                                       This because SUNs are so STUPID
C     INTEGER   FCN
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, MAXFEV, MODE, NPRINT, INFO, NFEV, NJEV,
     *   IPVT(N)
      DOUBLE PRECISION FTOL, XTOL, GTOL, FACTOR, X(N), FVEC(M),
     *   FJAC(LDFJAC,N), DIAG(N), QTF(N), WA1(N), WA2(N), WA3(N), WA4(M)
C
      INTEGER   I, IFLAG, ITER, J, L, IJ
      LOGICAL   SING, T
      DOUBLE PRECISION ACTRED, DELTA, DIRDER, EPSMCH, FNORM, FNORM1,
     *   GNORM, ONE, PAR, PNORM, PRERED, P1, P5, P25, P75, P0001, RATIO,
     *   SUM, TEMP, TEMP1, TEMP2, XNORM, ZERO, DPMPAR, ENORM
      DATA T /.TRUE./
      DATA ONE, P1, P5, P25, P75, P0001, ZERO
     *   /1.0D0,1.0D-1,5.0D-1,2.5D-1,7.5D-1,1.0D-4,0.0D0/
C-----------------------------------------------------------------------
C                                       EPSMCH is the machine precision
      EPSMCH = DPMPAR (1)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C                                       Check the input parameters for
C                                       errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.N)
     *   .OR. (FTOL.LT.ZERO) .OR. (XTOL.LT.ZERO) .OR. (GTOL.LT.ZERO)
     *   .OR. (MAXFEV.LE.0) .OR. (FACTOR.LE.ZERO)) GO TO 980
      IF (MODE.EQ.2) THEN
         DO 10 J = 1,N
            IF (DIAG(J).LE.ZERO) GO TO 980
 10         CONTINUE
         END IF
C                                       Evaluate the function at the
C                                       starting point and calculate
C                                       its norm.
      IFLAG = 1
      CALL FCN (M, N, X, FVEC, WA3, IFLAG)
      NFEV = 1
      IF (IFLAG.LT.0) GO TO 980
      FNORM = ENORM (M, FVEC)
C                                       Initialize Levenberg-Marquardt
C                                       parameter and iteration counter
      PAR = ZERO
      ITER = 1
C                                       Beginning of the outer loop.
 30   CONTINUE
C                                       If requested, call FCN to
C                                       enable printing of iterates.
         IF (NPRINT.GT.0) THEN
            IFLAG = 0
            IF (MOD(ITER-1,NPRINT).EQ.0) CALL FCN (M, N, X, FVEC,
     *         WA3, IFLAG)
            IF (IFLAG.LT.0) GO TO 980
            END IF
C                                       Compute the QR factorization
C                                       of the Jacobian matrix
C                                       calculated one row at a time,
C                                       while simultaneously
C                                       forming (Q transpose)*FVEC
C                                       and storing the first
C                                       N components in QTF.
         DO 60 J = 1,N
            QTF(J) = ZERO
            DO 50 I = 1,N
               FJAC(I,J) = ZERO
 50            CONTINUE
 60         CONTINUE
         IFLAG = 2
         DO 70 I = 1,M
            CALL FCN (M, N, X, FVEC, WA3, IFLAG)
            IF (IFLAG.LT.0) GO TO 980
            TEMP = FVEC(I)
            CALL RWUPDT (N, FJAC, LDFJAC, WA3, QTF, TEMP, WA1, WA2)
            IFLAG = IFLAG + 1
 70         CONTINUE
         NJEV = NJEV + 1
C                                       If the Jacobian is rank
C                                       deficient, call QRFAC to
C                                       reorder its columns and update
C                                       the components of QTF.
         SING = .FALSE.
         DO 80 J = 1,N
            IF (FJAC(J,J).EQ.ZERO) SING = .TRUE.
            IPVT(J) = J
            IJ = J
            WA2(J) = ENORM (IJ, FJAC(1,J))
 80         CONTINUE
         IF (SING) THEN
            CALL QRFAC (N, N, FJAC, LDFJAC, T, IPVT, N, WA1, WA2, WA3)
            DO 120 J = 1,N
               IF (FJAC(J,J).NE.ZERO) THEN
                  SUM = ZERO
                  DO 90 I = J,N
                     SUM = SUM + FJAC(I,J) * QTF(I)
 90                  CONTINUE
                  TEMP = -SUM / FJAC(J,J)
                  DO 100 I = J,N
                     QTF(I) = QTF(I) + FJAC(I,J) * TEMP
 100                 CONTINUE
                  END IF
               FJAC(J,J) = WA1(J)
 120           CONTINUE
            END IF
C                                       On the first iteration and if
C                                       mode is 1, scale according
C                                       to the norms of the columns
C                                       of the initial Jacobian.
C
         IF (ITER.EQ.1) THEN
            IF (MODE.NE.2) THEN
               DO 140 J = 1,N
                  DIAG(J) = WA2(J)
                  IF (WA2(J).EQ.ZERO) DIAG(J) = ONE
 140              CONTINUE
               END IF
C                                       On the first iteration,
C                                       calculate the norm of the scaled
C                                       X and initialize the step bound
C                                       DELTA.
            DO 160 J = 1,N
               WA3(J) = DIAG(J) * X(J)
 160           CONTINUE
            XNORM = ENORM (N, WA3)
            DELTA = FACTOR * XNORM
            IF (DELTA.EQ.ZERO) DELTA = FACTOR
            END IF
C                                       Compute the norm of the scaled
C                                       gradient.
         GNORM = ZERO
         IF (FNORM.NE.ZERO) THEN
            DO 200 J = 1,N
               L = IPVT(J)
               IF (WA2(L).EQ.ZERO) GO TO 200
                  SUM = ZERO
                  DO 180 I = 1,J
                     SUM = SUM + FJAC(I,J) * (QTF(I)/FNORM)
 180                 CONTINUE
                  GNORM = MAX (GNORM, ABS (SUM/WA2(L)))
 200           CONTINUE
            END IF
C                                       Test for convergence of the
C                                       gradient norm.
         IF (GNORM.LE.GTOL) INFO = 4
         IF (INFO.NE.0) GO TO 980
C                                       Rescale if necessary.
         IF (MODE.NE.2) THEN
            DO 220 J = 1,N
               DIAG(J) = MAX (DIAG(J), WA2(J))
 220           CONTINUE
            END IF
C                                       Beginning of the inner loop.
 240     CONTINUE
C                                       Determine the Levenberg-
C                                       Marquardt parameter.
            CALL LMPAR (N, FJAC, LDFJAC, IPVT, DIAG, QTF, DELTA, PAR,
     *         WA1, WA2, WA3, WA4)
C                                       Store the direction P and
C                                       X + P. Calculate the norm of P.
            DO 250 J = 1,N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
 250           CONTINUE
            PNORM = ENORM(N,WA3)
C                                       On the first iteration, adjust
C                                       the initial step bound.
            IF (ITER.EQ.1) DELTA = MIN (DELTA, PNORM)
C                                       Evaluate the function at X + P
C                                       and calculate its norm.
            IFLAG = 1
            CALL FCN (M, N, WA2, WA4, WA3, IFLAG)
            NFEV = NFEV + 1
            IF (IFLAG.LT.0) GO TO 980
            FNORM1 = ENORM (M, WA4)
C                                       Compute the scaled actual
C                                       reduction.
            ACTRED = -ONE
            IF (P1*FNORM1.LT.FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C                                       Compute the scaled predicted
C                                       reduction and the scaled
C                                       directional derivative.
            DO 270 J = 1,N
               WA3(J) = ZERO
               L = IPVT(J)
               TEMP = WA1(L)
               DO 260 I = 1,J
                  WA3(I) = WA3(I) + FJAC(I,J) * TEMP
 260              CONTINUE
 270           CONTINUE
            TEMP1 = ENORM (N, WA3) / FNORM
            TEMP2 = (SQRT(PAR) * PNORM) / FNORM
            PRERED = TEMP1**2 + TEMP2**2 / P5
            DIRDER = -(TEMP1**2 + TEMP2**2)
C                                       Compute the ratio of the actual
C                                       to the predicted reduction.
            RATIO = ZERO
            IF (PRERED.NE.ZERO) RATIO = ACTRED / PRERED
C                                       Update the step bound.
            IF (RATIO.LE.P25) THEN
               IF (ACTRED.GE.ZERO) TEMP = P5
               IF (ACTRED.LT.ZERO)
     *            TEMP = P5 * DIRDER / (DIRDER + P5*ACTRED)
               IF ((P1*FNORM1.GE.FNORM) .OR. (TEMP.LT.P1)) TEMP = P1
               DELTA = TEMP * MIN (DELTA, PNORM/P1)
               PAR = PAR / TEMP
            ELSE
               IF ((PAR.EQ.ZERO) .OR. (RATIO.GE.P75)) THEN
                  DELTA = PNORM / P5
                  PAR = P5 * PAR
                  END IF
               END IF
C                                       Test for successful iteration.
            IF (RATIO.GE.P0001) THEN
C                                       Successful iteration. update X,
C                                       FVEC, and their norms.
               DO 310 J = 1,N
                  X(J) = WA2(J)
                  WA2(J) = DIAG(J) * X(J)
 310              CONTINUE
               DO 320 I = 1,M
                  FVEC(I) = WA4(I)
 320              CONTINUE
               XNORM = ENORM (N, WA2)
               FNORM = FNORM1
               ITER = ITER + 1
               END IF
C                                       Tests for convergence.
            IF ((ABS(ACTRED).LE.FTOL) .AND. (PRERED.LE.FTOL) .AND.
     *         (P5*RATIO.LE.ONE)) INFO = 1
            IF (DELTA.LE.XTOL*XNORM) INFO = 2
            IF ((ABS(ACTRED).LE.FTOL) .AND. (PRERED.LE.FTOL) .AND.
     *         (P5*RATIO.LE.ONE) .AND. (INFO.EQ.2)) INFO = 3
            IF (INFO.NE.0) GO TO 980
C                                       Tests for termination and
C                                       stringent tolerances.
            IF (NFEV.GE.MAXFEV) INFO = 5
            IF ((ABS(ACTRED).LE.EPSMCH) .AND. (PRERED.LE.EPSMCH)
     *         .AND. (P5*RATIO.LE.ONE)) INFO = 6
            IF (DELTA.LE.EPSMCH*XNORM) INFO = 7
            IF (GNORM.LE.EPSMCH) INFO = 8
            IF (INFO.NE.0) GO TO 980
C                                       End of the inner loop.  Repeat
C                                       if iteration unsuccessful.
            IF (RATIO.LT.P0001) GO TO 240
C                                       End of the outer loop.
         GO TO 30
C                                       Termination, either normal or
C                                       user imposed.
 980  IF (IFLAG.LT.0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT.GT.0) CALL FCN (M, N, X, FVEC, WA3, IFLAG)
C
 999  RETURN
      END

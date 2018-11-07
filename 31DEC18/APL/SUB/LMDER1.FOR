      SUBROUTINE LMDER1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT, WA, LWA)
C-----------------------------------------------------------------------
C! minimize the sum of squares of M nonlinear functions in N variables
C# Modeling Math
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
C   The purpose of LMDER1 is to minimize the sum of the squares of M
C   nonlinear functions in N variables by a modification of the
C   Levenberg-Marquardt algorithm. This is done by using the more
C   general least-squares solver LMDER. The user must provide a
C   subroutine which calculates the functions and the Jacobian.
C   Subprograms called
C      User-supplied ...... FCN
C      Minpack-supplied ... LMDER
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
C         INTEGER  M, N, LDFJAC
C         INTEGER  IFLAG
C         DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N)
C         ----------
C         IF IFLAG = 1 Calculate the functions at X and return this
C                      vector in FVEC. Do not alter FJAC.
C         IF IFLAG = 2 Calculate the Jacobian at X and return
C                      this matrix in FJAC. Do not alter FVEC.
C         ----------
C         RETURN
C         END
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of LMDER1.
C         In this case set IFLAG to a negative INTEGER.
C**********************************************************************
C      M       I     The number of functions (adj. array dim.).
C      N       I     The number of variables (adj. array dim.;
C                    N must not exceed M).
C      X       D(N)  contains an initial estimate of the
C                    solution vector.
C      TOL     D     termination occurs when the algorithm estimates
C                    either that the relative error in the sum of
C                    squares is at most TOL or that the relative error
C                    between X and the solution is at most TOL.
C   Outputs:
C      X       D(N)  contains the final estimate of the solution vector.
C      FVEC    D(M)  contains the functions evaluated at the output X.
C      FJAC    D(M,N)   The upper N by N submatrix of FJAC contains an
C                    upper triangular matrix R with diagonal elements of
C                    nonincreasing magnitude such that
C                             T     T           T
C                            P *(JAC *JAC)*P = R *R,
C                    where P is a permutation matrix and JAC is the
C                    final calculated Jacobian. Column J of P is column
C                    IPVT(J) (see below) of the identity matrix. The
C                    lower trapezoidal part of FJAC contains information
C                    generated during the computation of R.
C      LDFJAC   I    variable not less than M which specifies the
C                    leading dimension of the array FJAC
C                    (adj. array dim.).
C      INFO     I    If the user has terminated execution, INFO is
C                    set to the (negative) value of IFLAG. SEE
C                    description of FCN. otherwise, INFO is set
C                    as follows:
C
C                    INFO = 0  Improper input parameters.
C
C                    INFO = 1  Algorithm estimates that the relative
C                    error in the sum of squares is at most TOL.
C
C                    INFO = 2  Algorithm estimates that the relative
C                    error between X and the solution is at most TOL.
C
C                    INFO = 3  Conditions for INFO = 1 and INFO = 2
C                    both hold.
C
C                    INFO = 4  FVEC is orthogonal to the columns of the
C                    Jacobian to machine precision.
C
C                    INFO = 5  Number of calls to FCN with IFLAG = 1 HAS
C                    reached 100*(N+1).
C
C                    INFO = 6  TOL is too small. No further reduction in
C                    the sum of squares is possible.
C
C                    INFO = 7  TOL is too small. no further improvement
C                    in the approximate solution X is possible.
C
C      IPVT    I(N)  Defines a permutation matrix P such that
C                            JAC*P = Q*R,
C                    where JAC is the final calculated Jacobian, Q is
C                    orthogonal (not stored), and R is upper triangular
C                    with diagonal elements of nonincreasing magnitude.
C                    column J of P is column IPVT(J) of the identity
C                    matrix.
C      WA      D(LWA)  work array.
C      LWA     I     length of WA not less than 5*N+M (adj. array dim.)
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   FCN, M, N, LDFJAC, LWA, INFO, IPVT(N)
      DOUBLE PRECISION TOL, X(N), FVEC(M), FJAC(LDFJAC,N), WA(LWA)
C
      INTEGER   MAXFEV, MODE, NFEV, NJEV, NPRINT
      DOUBLE PRECISION FACTOR, FTOL, GTOL, XTOL, ZERO
      DATA FACTOR,ZERO /1.0D2,0.0D0/
C-----------------------------------------------------------------------
      INFO = 0
C                                       Check the input parameters for
C                                       errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.M) .OR. (TOL.LT.ZERO)
     *   .OR. (LWA.LT.5*N+M)) GO TO 999
C                                       Call LMDER.
      MAXFEV = 100 * (N + 1)
      FTOL = TOL
      XTOL = TOL
      GTOL = ZERO
      MODE = 1
      NPRINT = 0
      CALL LMDER (FCN, M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL, GTOL,
     *   MAXFEV, WA(1), MODE, FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT,
     *   WA(N+1), WA(2*N+1), WA(3*N+1), WA(4*N+1), WA(5*N+1))
      IF (INFO.EQ.8) INFO = 4
C
 999  RETURN
      END

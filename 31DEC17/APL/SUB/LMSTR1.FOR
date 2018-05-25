      SUBROUTINE LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT, WA, LWA)
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
C   The purpose of LMSTR1 is to minimize the sum of the squares of M
C   nonlinear functions in N variables by a modification of the
C   Levenberg-Marquardt algorithm which users minimum storage.  This is
C   done by using the more general least-squares solver LMSTR. The user
C   must provide a subroutine which calculates the functions and the
C   rows of Jacobian.    Subprograms called
C      User-supplied ...... FCN
C      Minpack-supplied ... LMSTR
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
C         ----------
C            If IFLAG = 1 calculate the functions at X and
C               return this vector if FVEC.
C            If IFLAG <> 1 calculate the (IFLAG - 1)st row of the
C               Jacobian at X and return this vector in FJROW.
C         ----------
C         RETURN
C         END
C   The value of IFLAG should not be changed by FCN unless the
C   user wants to terminate execution of LMSTR1.  In this case, set
C   IFLAG to a negative integer.
C**********************************************************************
C      M       I     The number of functions (adj. array dim.).
C      N       I     The number of variables. N must not exceed M
C                    (adj. array dim.).
C      X       D(N)  contains an initial estimate of solution vector
C      TOL     D     termination occurs when the algorithm estimates
C                    either that the relative error in the sum of
C                    squares is at most TOL or that the relative error
C                    between X and the solution is at most TOL.
C   Outputs:
C      X       D(N)  contains the final estimate of solution vector.
C      FVEC    D(M)  contains the functions evaluated at the output X
C      FJAC    D(N,N)  The upper N by N submatrix of FJAC contains an
C                    upper triangular matrix R such that
C                             T     T           T
C                            P *(JAC *JAC)*P = R *R,
C                    where P is a permutation matrix and JAC is the
C                    final calculated Jacobian. Column J of P is column
C                    IPVT(J) (see below) of the identity matrix. The
C                    lower triangular part of FJAC contains information
C                    generated during the computation of R.
C      LDFJAC  I     variable not less than N which specifies the
C                    leading dimension of the array FJAC (adj. array
C                    dim.)
C-----------------------------------------------------------------------
C      INFO    I     If the user has terminated execution, INFO is set
C                    to the (negative) value of IFLAG. See description
C                    of FCN. otherwise, INFO is set as follows:
C                    INFO = 0  Improper input parameters.
C                    INFO = 1  Algorithm estimates that the relative
C                       error in the sum of squares is at most TOL.
C                    INFO = 2  Algorithm estimates that the relative
C                       error between X and the solution is at most TOL.
C                    INFO = 3  Conditions for INFO = 1 and INFO = 2
C                       both hold.
C                    INFO = 4  FVEC is orthogonal to the columns of the
C                       Jacobian to machine precision.
C                    INFO = 5  Number of calls to FCN with IFLAG = 1 HAS
C                       reached 100*(N+1).
C                    INFO = 6  TOL is too small. No further reduction in
C                       the sum of squares is possible.
C                    INFO = 7  TOL is too small. no further improvement
C                       in the approximate solution X is possible.
C      IPVT    I(N)  Defines a permutation matrix P such that
C                               JAC*P = Q*R,
C                    where JAC is the final calculated Jacobian, Q is
C                    orthogonal (not stored), and R is upper triangular.
C                    Column J of P is column IPVT(J) of the identity
C                    matrix.
C      WA      D(LWA) work array.
C      LWA     I     length of WA not less than 5*N+M (adj. array. dim.)
C-----------------------------------------------------------------------
C                                       This for STUPID SUNS
      INTEGER FCN
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, LWA, INFO, IPVT(N)
      DOUBLE PRECISION TOL, X(N), FVEC(M), FJAC(LDFJAC,N), WA(LWA)
C
      INTEGER   MAXFEV, MODE, NFEV, NJEV, NPRINT
      DOUBLE PRECISION FACTOR, FTOL, GTOL, XTOL, ZERO
      DATA FACTOR, ZERO /1.0D2, 0.0D0/
C-----------------------------------------------------------------------
      INFO = 0
C                                       Check the input parameters for
C                                       for errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.N) .OR. (TOL.LT.ZERO)
     *   .OR. (LWA.LT.5*N+M)) GO TO 999
C                                       Call LMSTR.
      MAXFEV = 100 * (N + 1)
      FTOL = TOL
      XTOL = TOL
      GTOL = ZERO
      MODE = 1
      NPRINT = 0
      CALL LMSTR (FCN, M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL, GTOL,
     *   MAXFEV, WA(1), MODE, FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT,
     *   WA(N+1), WA(2*N+1), WA(3*N+1), WA(4*N+1), WA(5*N+1))
      IF (INFO.EQ.8) INFO = 4
C
 999  RETURN
      END

      SUBROUTINE NULB (A, B, ACC, NSIG, FUNC, MEV, IER)
C-----------------------------------------------------------------------
C! Finds a root of a function in an interval.
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
C  Finds root of FUNC in interval [A, B], provided FUNC (A) and FUNC (B)
C  are of opposite sign. Uses the algorithm described by Brent (1971,
C  The Computer Journal, 14, 422).
C   Inputs:
C    A      R       Left boundary of interval.
C    ACC    R       Machine accuracy (e.g. from subroutine MACHIN).
C    NSIG   I       Wanted number of significant digits.
C    FUNC   R       Function of which root is to be found.
C   Input/Output:
C    B      R       In: Right boundary of interval.
C                   Out: If IER=0, root of FUNC in [A,B].
C    MEV    I       In: Maximum # iterations.
C                   Out: Used # iterations.
C   Outputs:
C    IER    I       IER=0 : No errors.
C                   IER=1 : More than MEV iterations needed.
C                   IER=2 : FUNC (A) and FUNC (B) have same sign.
C-----------------------------------------------------------------------
      INTEGER   MEV, IEV, IER, NSIG
      REAL      A, B, ACC, TL, FUNC, C, D, E, FA, FB, FC, M, P, Q, R, S,
     *   TOL, DD
      EXTERNAL FUNC
C-----------------------------------------------------------------------
      IER = 0
      IEV = 0
      TOL = 10.0**(-NSIG)
      FA  = FUNC (A)
      FB  = FUNC (B)
      IF (FA*FB.LE.0.0) GO TO 10
         IER = 2
         GO TO 900
 10   C   = A
      FC  = FA
      D   = B - A
      E   = D
 20   IF (ABS (FC).GE.ABS (FB)) GO TO 30
         A  = B
         B  = C
         C  = A
         FA = FB
         FB = FC
         FC = FA
 30   TL  = 2.0 * ACC * ABS (B) + TOL
      M   = 0.5 * (C - B)
      IEV = IEV + 1
      IF (IEV.LE.MEV) GO TO 40
         IER = 1
         GO TO 900
 40   IF (ABS (M).LE.TL.OR.FB.EQ.0.0) GO TO 900
C                                       see if a bisection is forced
         IF (ABS (C).GE.TL.AND.ABS (FA).GT.ABS (FB)) GO TO 100
            D  = M
            E  = M
         GO TO 600
 100        S  = FB / FA
            IF (A.NE.C) GO TO 200
C                                       linear interpolation
               P = 2.0 * M * S
               Q = 1.0 - S
            GO TO 300
C                                       inverse quadratic
 200           Q = FA / FC
               R = FB / FC
               P = S * (2.0 * M * Q * (Q - R) - (B - A) * (R - 1.0))
               Q = (Q - 1.0) * (R - 1.0) * (S - 1.0)
 300        IF (P.GT.0.0)  Q = -Q
            P =   ABS  (P)
            S = E
            E = D
            IF (2.0*P.GE.3.0*M*Q-ABS (TL*Q)
     *         .OR.P.GE.ABS (0.5*S*Q)) GO TO 400
               D = P / Q
            GO TO 600
 400           D = M
               E = M
 600     A  = B
         FA = FB
C                                       new value for B
         DD = D
         IF (ABS (D).LT.TL)  DD = SIGN (TL, M)
         B  = B + DD
C
         FB = FUNC (B)
         IF (FB*FC) 20,10,10
C                                       the solution is B
 900  MEV = IEV
C
 999  RETURN
      END

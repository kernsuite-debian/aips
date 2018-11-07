      DOUBLE PRECISION FUNCTION DERF (X)
C-----------------------------------------------------------------------
C! Double precision erf function
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
C   Double precision computation of the error function of a real*8
C   argument, X, by means of Chebyshev expansions taken from
C   Luke:Mathematical Functions and their Approximations,
C   Acad. Pr., NY, (1975) p.119 ff.
C-----------------------------------------------------------------------
      INTEGER   NTERMS, K
      DOUBLE PRECISION X
      DOUBLE PRECISION A(22), C(16), D(44), XABS, XX, CHEB, TWOX,
     *   PREV2, PREV
C                                       Chebyshev coefficients for
C                                       |x| <= 3 :
      DATA A/
     *   1.095471299777623D00,-2.891754011269890D-1,
     *   1.104563986337951D-1,-4.125318822785655D-2,
     *   1.408283807065164D-2,-4.329295447431437D-3,
     *   1.198271901592288D-3,-2.999729623532493D-4,
     *   6.832586037887479D-5,-1.424698845486775D-5,
     *   2.73540877283989D-6, -4.8619128719754D-7,
     *   8.038727621172D-8,   -1.241841831213D-8,
     *   1.79953258879D-9,    -2.4547948775D-10,
     *   3.162508603D-11,     -3.85902200D-12,
     *   4.4720291D-13,       -4.933613D-14,
     *   5.19303D-15,         -5.2258D-16/
C                                       Chebyshev coefficients for
C                                       |x|>3 :
      DATA C/
     *   9.750834237085559D-1,-2.404939385041460D-2,
     *   8.204522408804320D-4,-4.342930813034276D-5,
     *   3.01844703403493D-6, -2.5447331925082D-7,
     *   2.485835302051D-8,   -2.73172013238D-9,
     *   3.3084722797D-10,    -4.350549080D-11,
     *   6.14121457D-12,      -9.2236928D-13,
     *   1.4635665D-13,       -2.439278D-14,
     *   4.24976D-15,         -7.7084D-16/
C-----------------------------------------------------------------------
      XABS = ABS (X)
      IF (XABS.GT.3.D0) GO TO 20
         NTERMS = 44
         XX = XABS / 3.D0
         DO 10 K = 1,22
            D(2*K-1) = 0.D0
            D(2*K) = A(K)
 10         CONTINUE
         GO TO 40
 20   CONTINUE
         NTERMS = 31
         XX = 3.D0 / XABS
         DO 30 K = 1,16
            D(2*K-1) = C(K)
            D(2*K) = 0.D0
 30         CONTINUE
C
 40   K = NTERMS
      CHEB = D(K)
      K = K-1
      TWOX = XX + XX
      PREV2 = 0.D0
 50   PREV = CHEB
      IF (K.EQ.1) GO TO 60
         CHEB = D(K) + TWOX * PREV - PREV2
         PREV2 = PREV
         K = K - 1
         GO TO 50
 60   CHEB = D(1) + XX * PREV - PREV2
      IF (XABS.GT.3.D0) GO TO 70
         CHEB = 2.D0 * CHEB * 5.641895835477563D-1
         GO TO 80
 70   CONTINUE
         CHEB = 1.D0 - EXP(-XABS**2) / XABS * CHEB *
     *      5.641895835477563D-1
 80   IF (X.LT.0D0) CHEB = -CHEB
      DERF = CHEB
C
 999  RETURN
      END

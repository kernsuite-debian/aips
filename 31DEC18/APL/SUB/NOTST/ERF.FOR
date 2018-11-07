      REAL FUNCTION ERF (X)
C-----------------------------------------------------------------------
C! error function.
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C  Single precision computation of the error function of a real
C  argument, X, by means of Chebyshev expansions taken from
C  Luke:Mathematical Functions and their Approximations,
C  Acad. Pr., NY, (1975) p.119 ff.
C-----------------------------------------------------------------------
      REAL      X
C
      INTEGER   NTERMS, K
      REAL      A(12), C(6), D(24), XABS, XX, CHEB, TWOX, PREV2, PREV
C                                       Chebyshev coefficients for
C                                       |x|<=3 :
      DATA A /1.095471E00, -2.891754E-1,
     *        1.104564E-1, -4.125319E-2,
     *        1.408284E-2, -4.329295E-3,
     *        1.198272E-3, -2.999730E-4,
     *        6.832586E-5, -1.424699E-5,
     *        2.735409E-6, -4.861913E-7/
C                                       Chebyshev coefficients for
C                                       |x|>3 :
      DATA C / 9.750834E-1, -2.404939E-2,
     *         8.204522E-4, -4.342931E-5,
     *         3.018447E-6, -2.544733E-7/
C-----------------------------------------------------------------------
      XABS = ABS (X)
      IF (XABS.LE.3.) THEN
         NTERMS = 24
         XX = XABS / 3.
         DO 10 K = 1,12
            D(2*K-1) = 0.
            D(2*K) = A(K)
 10         CONTINUE
      ELSE
         NTERMS = 11
         XX = 3. / XABS
         DO 30 K = 1,6
            D(2*K-1) = C(K)
            D(2*K) = 0.
 30         CONTINUE
         END IF
C
 40   K = NTERMS
      CHEB = D(K)
      K = K-1
      TWOX = XX+XX
      PREV2 = 0.
      DO 50 K = NTERMS-1,2,-1
         PREV = CHEB
         CHEB = D(K) + TWOX * PREV - PREV2
         PREV2 = PREV
 50      CONTINUE
      PREV = CHEB
C
 60   CHEB = D(1) + XX * PREV - PREV2
      IF (XABS.LE.3.0) THEN
         CHEB = 2. * CHEB * 5.641896E-1
      ELSE
         CHEB = 1. - EXP (-XABS**2) / XABS * CHEB * 5.641896E-1
         END IF
      IF (X.LT.0.) CHEB = -CHEB
      ERF = CHEB
C
 999  RETURN
      END

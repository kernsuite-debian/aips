      FUNCTION BESSJ0 (X)
C-----------------------------------------------------------------------
C! Computes the J0 Bessel function for argumanet X
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Compute the J0 Bessel function for a given argument
C   Inputs:
C      X     D    The argument of the J0 Bessel function desired
C   Output:
C            D    Returns the resultant J0 Bessel function
C   Method:
C      The simple version of http://gams.nist.gov...
C-----------------------------------------------------------------------
      INTEGER  NTJ0, NTM0, NTTH0
      DOUBLE PRECISION BJ0CS(13), BM0CS(21), BTH0CS(24), PI4,
     *   Y, X, Z, BESSJ0, AMPL, THETA, CSEVL
      DATA BJ0CS( 1) /    .100254161968939137 /
      DATA BJ0CS( 2) /   -.665223007764405132 /
      DATA BJ0CS( 3) /    .248983703498281314 /
      DATA BJ0CS( 4) /   -.0332527231700357697 /
      DATA BJ0CS( 5) /    .0023114179304694015 /
      DATA BJ0CS( 6) /   -.0000991127741995080 /
      DATA BJ0CS( 7) /    .0000028916708643998 /
      DATA BJ0CS( 8) /   -.0000000612108586630 /
      DATA BJ0CS( 9) /    .0000000009838650793 /
      DATA BJ0CS(10) /   -.0000000000124235515 /
      DATA BJ0CS(11) /    .0000000000001265433 /
      DATA BJ0CS(12) /   -.0000000000000010619 /
      DATA BJ0CS(13) /    .0000000000000000074 /
      DATA BM0CS( 1) /    .09284961637381644 /
      DATA BM0CS( 2) /   -.00142987707403484 /
      DATA BM0CS( 3) /    .00002830579271257 /
      DATA BM0CS( 4) /   -.00000143300611424 /
      DATA BM0CS( 5) /    .00000012028628046 /
      DATA BM0CS( 6) /   -.00000001397113013 /
      DATA BM0CS( 7) /    .00000000204076188 /
      DATA BM0CS( 8) /   -.00000000035399669 /
      DATA BM0CS( 9) /    .00000000007024759 /
      DATA BM0CS(10) /   -.00000000001554107 /
      DATA BM0CS(11) /    .00000000000376226 /
      DATA BM0CS(12) /   -.00000000000098282 /
      DATA BM0CS(13) /    .00000000000027408 /
      DATA BM0CS(14) /   -.00000000000008091 /
      DATA BM0CS(15) /    .00000000000002511 /
      DATA BM0CS(16) /   -.00000000000000814 /
      DATA BM0CS(17) /    .00000000000000275 /
      DATA BM0CS(18) /   -.00000000000000096 /
      DATA BM0CS(19) /    .00000000000000034 /
      DATA BM0CS(20) /   -.00000000000000012 /
      DATA BM0CS(21) /    .00000000000000004 /
      DATA BTH0CS( 1) /   -.24639163774300119 /
      DATA BTH0CS( 2) /    .001737098307508963 /
      DATA BTH0CS( 3) /   -.000062183633402968 /
      DATA BTH0CS( 4) /    .000004368050165742 /
      DATA BTH0CS( 5) /   -.000000456093019869 /
      DATA BTH0CS( 6) /    .000000062197400101 /
      DATA BTH0CS( 7) /   -.000000010300442889 /
      DATA BTH0CS( 8) /    .000000001979526776 /
      DATA BTH0CS( 9) /   -.000000000428198396 /
      DATA BTH0CS(10) /    .000000000102035840 /
      DATA BTH0CS(11) /   -.000000000026363898 /
      DATA BTH0CS(12) /    .000000000007297935 /
      DATA BTH0CS(13) /   -.000000000002144188 /
      DATA BTH0CS(14) /    .000000000000663693 /
      DATA BTH0CS(15) /   -.000000000000215126 /
      DATA BTH0CS(16) /    .000000000000072659 /
      DATA BTH0CS(17) /   -.000000000000025465 /
      DATA BTH0CS(18) /    .000000000000009229 /
      DATA BTH0CS(19) /   -.000000000000003448 /
      DATA BTH0CS(20) /    .000000000000001325 /
      DATA BTH0CS(21) /   -.000000000000000522 /
      DATA BTH0CS(22) /    .000000000000000210 /
      DATA BTH0CS(23) /   -.000000000000000087 /
      DATA BTH0CS(24) /    .000000000000000036 /
      DATA PI4 / 0.7853981633 9744831E0 /
C-----------------------------------------------------------------------
      NTJ0 = 13
      NTM0 = 21
      NTTH0 = 24
C
      Y = ABS(X)
      IF (Y .LE. 4.0) THEN
C
         BESSJ0 = 1.0
         IF (Y .GT. 0.01) BESSJ0 = CSEVL (.125*Y*Y-1., BJ0CS, NTJ0)
      ELSE
         Z = 32.0 / Y**2 - 1.0
         AMPL = (0.75 + CSEVL (Z, BM0CS, NTM0)) / SQRT(Y)
         THETA = Y - PI4 + CSEVL (Z, BTH0CS, NTTH0) / Y
         BESSJ0 = AMPL * COS (THETA)
         END IF
C
      RETURN
      END
C
      FUNCTION CSEVL(X, CS, N)
C-----------------------------------------------------------------------
C   Evaluate the N-term Chebyshev series CS at X.
C   Inputs:
C      X     D    Value at which the series is to be evaluated.
C      N     I    number of terms in array CS.
C   Output:
C      CS(*) D    Array of N terms of a Chebyshev series.
C   Method:
C   Adapted from R. Broucke, Algorithm 446, C.A.C.M., 16, 254 (1973).
C-----------------------------------------------------------------------
      INTEGER N, NI, I
      DOUBLE PRECISION  CS(*), B0, B1, B2, TWOX, CSEVL, X
C-----------------------------------------------------------------------
      B1=0.
      B0=0.
      TWOX=2.*X
      DO 10 I=1,N
         B2=B1
         B1=B0
         NI=N+1-I
         B0=TWOX*B1-B2+CS(NI)
   10    CONTINUE
C
      CSEVL = 0.5 * (B0-B2)
C
      RETURN
      END



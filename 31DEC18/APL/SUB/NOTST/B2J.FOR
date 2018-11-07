      SUBROUTINE B2J (R1950, D1950, BEPOCH, R2000, D2000)
C-------------------------------------------------------------------
C! Convert B1950 to J2000 positions of the source
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   Routine to convert B1950.0 coordinates to J2000
C   Input:
C      R1950,D1950 D     B1950.0 RA, Dec (rad)
C      BEPOCH      D     Besselian epoch (e.g. 1979.3D0)
C   Output:
C      R2000,D2000 D     J2000.0 RA, Dec (rad)
C
C   This subroutine is a modified verion of the SLALIB routine FK45Z
C   written by P.T.Wallace   Starlink   10 April 1990,
C   Rutherford Appleton Laboratory
C-----------------------------------------------------------------------
      DOUBLE PRECISION R1950, D1950, BEPOCH, R2000, D2000
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
      DOUBLE PRECISION W, EPB2D
      INTEGER I,J
C                                       Position and position+velocity
C                                       vectors
      DOUBLE PRECISION R0(3),A1(3),V1(3),V2(6)
C                                       Radians per year to arcsec per
C                                       century
      DOUBLE PRECISION PMF
      PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI)
C
      DOUBLE PRECISION A(3), AD(3), EM(6,3), R, COSB
      DATA A, AD/ -1.62557D-6,  -0.31919D-6, -0.13843D-6,
     *           +1.245D-3,    -1.580D-3,   -0.659D-3/
C
      DATA (EM(I,1),I=1,6) / +0.9999256782D0,
     *                       +0.0111820610D0,
     *                       +0.0048579479D0,
     *                       -0.000551D0,
     *                       +0.238514D0,
     *                       -0.435623D0 /
C
      DATA (EM(I,2),I=1,6) / -0.0111820611D0,
     *                       +0.9999374784D0,
     *                       -0.0000271474D0,
     *                       -0.238565D0,
     *                       -0.002667D0,
     *                       +0.012254D0 /
C
      DATA (EM(I,3),I=1,6) / -0.0048579477D0,
     *                       -0.0000271765D0,
     *                       +0.9999881997D0,
     *                       +0.435739D0,
     *                       -0.008541D0,
     *                       +0.002117D0 /
C-----------------------------------------------------------------------
C                                       Spherical to Cartesian
      COSB = COS(D1950)
      R0(1) = COS(R1950) * COSB
      R0(2) = SIN(R1950) * COSB
      R0(3) = SIN(D1950)
C                                       Adjust vector A to give zero
C                                       proper motion in FK5
      W=(BEPOCH - 1950D0) / PMF
      DO 10, I = 1, 3
         A1(I) = A(I) + W * AD(I)
   10    CONTINUE

C                                       Remove e-terms
      W = R0(1) * A1(1) + R0(2) * A1(2) + R0(3) * A1(3)
      DO 20 I = 1, 3
         V1(I) = R0(I) - A1(I) + W * R0(I)
   20    CONTINUE
C                                       Convert position vector to
C                                       Fricke system
      DO 40 I = 1, 6
         W = 0.0D0
         DO 30 J = 1,3
            W = W + EM(I,J) * V1(J)
   30       CONTINUE
         V2(I)=W
   40    CONTINUE
C                                       Allow for fictitious proper
C                                       motion in FK4
      EPB2D = 15019.81352D0 + (BEPOCH - 1900D0) * 365.242198781D0
C
      W = ((EPB2D - 51544.5D0) / 365.25D0) / PMF
      DO 50 I=1,3
         V2(I) =V2(I) + W * V2(I + 3)
   50    CONTINUE
C                                       Revert to spherical coordinates
      R = SQRT(V2(1) * V2(1) + V2(2) * V2(2))
      IF (R .EQ. 0D0) THEN
         W = 0D0
      ELSE
         W = ATAN2(V2(2),V2(1))
         END IF
      IF (V2(3) .EQ. 0D0) THEN
         D2000 = 0D0
      ELSE
         D2000 = ATAN2(V2(3), R)
         END IF
      R2000 = MOD(W, D2PI)
      IF (R2000 .LT. 0D0) R2000 = R2000 + D2PI
C
 999  RETURN
      END

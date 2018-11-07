      SUBROUTINE OE2VEC (SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY,
     *                   POS, VEL)
C-----------------------------------------------------------------------
C! Convert orbital elements to position and velocity vectors
C# Util Orbits
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   Convert a set of orbital elements for an Earth satellite into a
C   position and velocity vector in the intertial frame to which the
C   elements are referred.  The Cartesian coordinates are a right handed
C   set with axis 1 pointing towards RA 0, Dec 0 and with axis 2
C   pointing towards to North celestial pole.
C
C
C   Inputs:
C      SEMIMA    D          Semimajor axis (m)
C      ECCEN     D          Eccentricity
C      INCLIN    D          Inclination (radians)
C      RAANOD    D          Right ascension of ascending node (radians)
C      APERIG    D          Argument of perigee (radians)
C      MANMLY    D          Mean anomaly (radians)
C
C   Outputs:
C      POS       D(3)       Position vector (m)
C      VEL       D(3)       Velocity vector (m s**-1)
C
C   References:
C     "Orbital Motion", A.E. Roy, Institute of Physics Press, 1988
C     (Chapter 4)
C-----------------------------------------------------------------------
      DOUBLE PRECISION SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY,
     *                 POS(3), VEL(3)
C
C     Local variables
C
C     E      Eccentric anomaly
C     R      Length of radius vector (m)
C     V      Velocity (m s**-1)
C     F      True anomaly (radians)
C     PHI    Angle between velocity vector and radius vector (radians)
C
      DOUBLE PRECISION E, R, V, F, PHI
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PEARTH.INC'
C-----------------------------------------------------------------------
C
C     Solve Kepler's equation for the eccentric anomaly:
C
      CALL SLVKEP (MANMLY, ECCEN, E, 1.0D-10)
C
      R = SEMIMA * (1.0D0 - ECCEN * COS (E))
      V = SQRT (EMU * (2.0D0 / R - 1.0D0 / SEMIMA))
      F = 2.0 * ATAN (SQRT ((1.0D0 + ECCEN) / (1.0D0 - ECCEN))
     *                      * TAN (E / 2.0D0))
      PHI = ASIN (SQRT ((SEMIMA ** 2 * (1.0D0 - ECCEN ** 2)
     *                   / (R * (2.0D0 * SEMIMA - R)))))
C
C     PHI is ambiguous here but E is not and cos(PHI) must have the
C     same sign as sin(E).
C
      IF (E .GT. PI) THEN
         PHI = PI - PHI
      END IF
C
      POS(1) = R * (COS (RAANOD) * COS (APERIG + F)
     *              - SIN (RAANOD) * SIN (APERIG + F) * COS (INCLIN))
      POS(2) = R * (SIN (RAANOD) * COS (APERIG + F)
     *              + COS (RAANOD) * SIN (APERIG + F) * COS (INCLIN))
      POS(3) = R * SIN (APERIG + F) * SIN (INCLIN)
C
      VEL(1) = V * (COS (RAANOD) * COS (APERIG + F + PHI)
     *              - SIN (RAANOD) * SIN (APERIG + F + PHI )
     *                * COS (INCLIN))
      VEL(2) = V * (SIN (RAANOD) * COS (APERIG + F + PHI)
     *              + COS (RAANOD) * SIN (APERIG + F + PHI)
     *                * COS (INCLIN))
      VEL(3) = V * SIN (APERIG + F + PHI) * SIN (INCLIN)
C
      END

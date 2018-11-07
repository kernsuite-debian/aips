      SUBROUTINE PROPOE (DT, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                   MANMLY, RANODT, APERIT, MANMLT)
C-----------------------------------------------------------------------
C! Propagate orbital elements in time.
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
C   Propagate the right ascension of the ascending node, the argument
C   of perigee, and the mean anomaly of an Earth orbit through DT days
C   allowing for precession due to the oblateness of the Earth.
C
C   Inputs:
C      SEMIMA    D      Semimajor axis of orbit (m)
C      ECCEN     D      Orbital eccentricity
C      INCLIN    D      Inclination of orbit (radians)
C      RAANOD    D      Right ascension of ascending node (radians)
C      APERIG    D      Argument of perigee (radians)
C      MANMLY    D      Mean anomaly (radians)
C
C   Outputs:
C      RANODT    D      Right ascension of ascending node after time
C                       DT (radians)
C      APERIT    D      Argument of perigee after time DT (radians)
C      MANMLT    D      Mean anomaly after time DT (radians)
C
C   References:
C     "Orbital Motion", A.E. Roy, Institute of Physics Press, 1988
C     (Section 10.4.2)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DT, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                 MANMLY, RANODT, APERIT, MANMLT
C
C     Local variables:
C
C     DTSEC     Time difference in seconds
C     N0        Unperturbed mean motion (radians s**-1)
C     NBAR      Perturbed mean motion
C
      DOUBLE PRECISION P, DTSEC, N0, NBAR
C
      INCLUDE 'INCS:PEARTH.INC'
C-----------------------------------------------------------------------
      DTSEC = 24.0D0 * 3600.0D0 * DT
      P = SEMIMA * (1.0D0 - ECCEN ** 2)
      N0 = SQRT (EMU / SEMIMA ** 3)
      NBAR = N0 * (1.0D0 + 1.5D0 * (EJ2 * ERAD ** 2 / P ** 2)
     *       * (1.0D0 - 1.5D0 * (SIN (INCLIN)) ** 2)
     *       * SQRT (1.0D0 - ECCEN ** 2))
C
      RANODT = RAANOD - 1.5D0 * (EJ2 * ERAD ** 2 / P ** 2) * NBAR
     *         * COS (INCLIN) * DTSEC
      APERIT = APERIG + 1.5D0 * (EJ2 * ERAD ** 2 / P ** 2) * NBAR
     *         * (2.0D0 - 2.5D0 * (SIN (INCLIN)) ** 2) * DTSEC
      MANMLT = MANMLY + NBAR * DTSEC
C
      END

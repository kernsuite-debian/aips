      SUBROUTINE VEC2OE (POS, VEL, SEMIMA, ECCEN, INCLIN, RAANOD,
     *                   APERIG, MANMLY, IRET)
C-----------------------------------------------------------------------
C! Convert position and velocity vectors to orbital elements
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
C   Convert a Cartesian position and velocity vector for a satellite in
C   a bound Earth orbit to a set of Keplerian elements referenced to the
C   same inertial frame.  The coordinate axes should form a right handed
C   set with axis 1 defined by RA = 0, Dec = 0 and with axis 3 defined
C   by the North celestial pole.
C
C   Inputs:
C      POS       D(3)       Position vector (m)
C      VEL       D(3)       Velocity vector (m s**-1)
C
C   Outputs:
C      SEMIMA    D          Semimajor axis (m)
C      ECCEN     D          Eccentricity
C      INCLIN    D          Inclination (radians)
C      RAANOD    D          Right ascension of ascending node (radians)
C      APERIG    D          Argument of perigee (radians)
C      MANMLY    D          Mean anomaly (radians)
C      IRET      I          Status: 0 implies elements are valid
C                                   1 implies orbit is hyperbolic or
C                                             parabolic
C                                 999 implies a logic error
C
C   References:
C     "Orbital Motion", A.E. Roy, Institute of Physics Press, 1988
C     (Chapter 4)
C-----------------------------------------------------------------------
      DOUBLE PRECISION POS(3), VEL(3), SEMIMA, ECCEN, INCLIN, RAANOD,
     *                 APERIG, MANMLY
      INTEGER          IRET
C
C     Local variables
C
C     V2        The square of the magnitude of the velocity (m**2 s**-2)
C     R         The length of the radius vector (m)
C     H         The angular momentum vector
C     C         Orbital energy (Joules)
C     WF        Sum of argument of perigee and true anomaly (omega + f)
C     SINWF     Sine of WF
C     COSWF     Cosine of WF
C     F         True anomaly
C     E         Eccentric anomaly
C
      DOUBLE PRECISION V2, R, H(3), P, C, WF, SINWF, COSWF, F, E
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PEARTH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      V2 = VEL(1) ** 2 + VEL(2) ** 2 + VEL(3) ** 2
      R = SQRT (POS(1) ** 2 + POS(2) ** 2 + POS(3) ** 2)
      H(1) = POS(2) * VEL(3) - POS(3) * VEL(2)
      H(2) = POS(3) * VEL(1) - POS(1) * VEL(3)
      H(3) = POS(1) * VEL(2) - POS(2) * VEL(1)
      P = (H(1) ** 2 + H(2) ** 2 + H(3) ** 2) / EMU
      C = 0.5D0 * V2 - EMU / R
C                                       Energy negative for a bound
C                                       orbit.
      IF (C .LT. 0.0D0) THEN
         SEMIMA = 1.0D0 / (2.0D0 / R - V2 / EMU)
         CALL CHECK ('VEC2OE', 1, ((P / SEMIMA) .LE. 1.0D0), IRET)
         IF (IRET .NE. 0) GO TO 999
         ECCEN = SQRT (1.0D0 - P / SEMIMA)
         INCLIN = ACOS (H(3) / SQRT (H(1) ** 2 + H(2) ** 2 + H(3) ** 2))
         IF (H(3) .GE. 0.0D0) THEN
            RAANOD = ATAN2 (H(1), -H(2))
         ELSE
            RAANOD = ATAN2 (-H(1), H(2))
         END IF
C
C        Normalize to the range 0 to 2 pi:
C
         IF (RAANOD .LT. 0.0D0) THEN
            RAANOD = RAANOD + TWOPI
         END IF
C
         IF (INCLIN .EQ. 0.0D0) THEN
            SINWF = POS(2) * COS (RAANOD) - POS(1) * SIN (RAANOD)
         ELSE
            SINWF = POS(3) / SIN (INCLIN)
         END IF
         COSWF = POS(1) * COS (RAANOD) + POS(2) * SIN (RAANOD)
         WF = ATAN2 (SINWF, COSWF)
         IF (ECCEN .EQ. 0.0) THEN
C                                       Argument of perigee is arbitrary
C                                       for a circular orbit.
            APERIG = 0.0D0
            MANMLY = WF
         ELSE
            E = ACOS ((1.0D0 - R / SEMIMA) / ECCEN)
            F = ACOS (((H(1) ** 2 + H(2) ** 2 + H(3) ** 2) / (EMU * R)
     *                - 1.0D0) / ECCEN)
C
C           Remove ambiguities using the constraint that the cosine
C           of the angle between the velocity vector and the radius
C           vector has the same sign as the sine of the eccentric
C           anomaly:
C
            IF ((POS(1) * VEL(1) + POS(2) * VEL(2) + POS(3) * VEL(3))
     *          .LT. 0.0D0) THEN
               E = TWOPI - E
               F = TWOPI - F
            END IF
            APERIG = WF - F
            MANMLY = E - ECCEN * SIN (E)
         END IF
         IRET = 0
      ELSE
         MSGTXT = 'VEC2OE: VECTORS SPECIFY UNBOUND ORBIT'
         CALL MSGWRT (7)
      END IF
C
  999 RETURN
      END

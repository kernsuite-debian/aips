      SUBROUTINE BACOOR (IORB1, IORB2, ORBIT1, ORBIT2, XG1, YG1, ZG1,
     *   XG2, YG2, ZG2, GAST, TIME, BX, BY, BZ, VX, VY, VZ)
C-----------------------------------------------------------------------
C! Find a baseline cartesian coordinates for orbit and ground antennas
C# UV VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Routine to find a baseline coordinates in equatorial coordinate
C   system fixed at the sky for both ground based antennas and
C   orbital ones.
C   Inputs:
C      IORB1    I         The first satellite number; =0, if ground
C      IORB2    I         The second satellite number; =0, if ground
C      ORBIT1   D(6)      Parameters of the first satellite orbit
C                           1. Semimajor, meters
C                           2. Eccentricity
C                           3. Inclination of orbit plane, degrees
C                           4. RA of ascending node, degrees
C                           5. An angle in orbit plane from
C                              ascending node to peregee, degrees
C                           6. Mean anomaly at the reference time,
C                              degrees.
C      ORBIT2   D(6)      Parameters of the second satellite orbit
C                           1. Semimajor, meters
C                           2. Eccentricity
C                           3. Inclination of orbit plane, degrees
C                           4. RA of ascending node, degrees
C                           5. An angle in orbit plane from
C                              ascending node to peregee, degrees
C                           6. Mean anomaly at the reference time,
C                              degrees.
C      GAST     D         Angle of Greenwich from epinox at current
C                         time, radians
C      TIME     D         Time, UT, relatively the reference time,
C                         days
C      XG1, YG1, ZG1  D   The first ground antenna coordinates
C                         in the LH system tied with Earth
C                         (X - greenwich, Y to the west), in meters
C      XG2, YG2, ZG2  D   The second ground antenna coordinates
C                         in the LH system tied with Earth
C                         (X - greenwich, Y to the west), in meters
C   Output:
C      BX, BY, BZ     D   The baseline coordinates in sky
C                         equatorial system (X-RA..), in meters
C      VX, VY, VZ     D   The baseline velocities in sky
C                         equatorial system (X-RA..), in meters/sec
C   Programmer:  L.R. Kogan  NRAO, Socorro
C-----------------------------------------------------------------------
      INTEGER IORB1, IORB2
      DOUBLE PRECISION ORBIT1(*), ORBIT2(*), XG1, YG1, ZG1, XG2,
     *   YG2, ZG2, GAST, TIME, BX, BY, BZ, VX, VY, VZ
      DOUBLE PRECISION    A1, ECCEN1, INCLN1, RAASC1, PERIG1, MANRE1,
     *   MANRE2, A2, ECCEN2, INCLN2, RAASC2, PERIG2,  TRANOM, MAJPRO,
     *   MINPRO, VA, VB, X1, Y1, Z1, X2, Y2, Z2, VX1, VY1, VZ1, VX2,
     *   VY2, VZ2, TIMES, COEFF
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION SIDT
      DATA SIDT /1.00273790935/
C-----------------------------------------------------------------------
      TIMES = TIME * 8.64D4
      COEFF = TWOPI * SIDT / 86400.0
C                                       time in seconds
      IF (IORB1 .GT. 0) THEN
C                                       The first antenna is at orbit
         A1     = ORBIT1(1)
         ECCEN1 = ORBIT1(2)
         INCLN1 = ORBIT1(3)*DG2RAD
         RAASC1 = ORBIT1(4)*DG2RAD
         PERIG1 = ORBIT1(5)*DG2RAD
         MANRE1 = ORBIT1(6)*DG2RAD
         CALL ORBIT (A1, ECCEN1, INCLN1, RAASC1, PERIG1, MANRE1,
     *      TIMES, TRANOM, MAJPRO, MINPRO, VA, VB,
     *      X1, Y1, Z1, VX1, VY1, VZ1)
         IF (IORB2 .EQ. 0) THEN
C                                       the first antenna at orbit;
C                                       the second one on ground
            BX = XG2*DCOS(GAST) + YG2*DSIN(GAST) - X1
            BY = XG2*DSIN(GAST) - YG2*DCOS(GAST) - Y1
            BZ = ZG2 - Z1
            VX = COEFF * (-XG2*DSIN(GAST) + YG2*DCOS(GAST)) - VX1
            VY = COEFF * ( XG2*DCOS(GAST) + YG2*DSIN(GAST)) - VY1
            VZ = -VZ1
         ELSE
C                                       the first antenna at orbit;
C                                       the second one at orbit also
            A2     = ORBIT2(1)
            ECCEN2 = ORBIT2(2)
            INCLN2 = ORBIT2(3)*DG2RAD
            RAASC2 = ORBIT2(4)*DG2RAD
            PERIG2 = ORBIT2(5)*DG2RAD
            MANRE2 = ORBIT2(6)*DG2RAD
            CALL ORBIT (A2, ECCEN2, INCLN2, RAASC2, PERIG2, MANRE2,
     *         TIMES, TRANOM, MAJPRO, MINPRO, VA, VB,
     *         X2, Y2, Z2, VX2, VY2, VZ2)
            BX = X2 - X1
            BY = Y2 - Y1
            BZ = Z2 - Z1
            VX = VX2 - VX1
            VY = VY2 - VY1
            VZ = VZ2 - VZ1
            END IF
      ELSE
C                                       The first antenna is on ground
         IF (IORB2 .EQ. 0) THEN
C                                       the first antenna on ground;
C                                       the second one on ground also
            BX = (XG2-XG1)*DCOS(GAST) + (YG2-YG1)*DSIN(GAST)
            BY = (XG2-XG1)*DSIN(GAST) - (YG2-YG1)*DCOS(GAST)
            BZ = ZG2 - ZG1
            VX = COEFF * (-(XG2 - XG1) * DSIN(GAST) +
     *         (YG2 - YG1)*DCOS(GAST))
            VY = COEFF * ((XG2 - XG1) * DCOS(GAST) +
     *         (YG2 - YG1)*DSIN(GAST))
            VZ = 0

         ELSE
C                                       the first antenna on ground;
C                                       the second one at orbit
            A2     = ORBIT2(1)
            ECCEN2 = ORBIT2(2)
            INCLN2 = ORBIT2(3)*DG2RAD
            RAASC2 = ORBIT2(4)*DG2RAD
            PERIG2 = ORBIT2(5)*DG2RAD
            MANRE2 = ORBIT2(6)*DG2RAD
            CALL ORBIT (A2, ECCEN2, INCLN2, RAASC2, PERIG2, MANRE2,
     *         TIMES, TRANOM, MAJPRO, MINPRO, VA, VB,
     *         X2, Y2, Z2, VX2, VY2, VZ2)
            BX = X2 - (XG1*DCOS(GAST) + YG1*DSIN(GAST))
            BY = Y2 - (XG1*DSIN(GAST) - YG1*DCOS(GAST))
            BZ = Z2 - ZG1
            VX = VX2 - COEFF * (-XG1*DSIN(GAST) + YG1*DCOS(GAST))
            VY = VY2 - COEFF * ( XG1*DCOS(GAST) + YG1*DSIN(GAST))
            VZ = VZ2
            END IF
         END IF
      RETURN
      END

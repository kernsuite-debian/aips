      SUBROUTINE MAGDIP (GLAT, GLONG, RADIUS, H)
C-----------------------------------------------------------------------
C! Calculate Earth's magnetic field components
C# Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   Routine to compute the earth's magnetic field strength from an
C   offset dipole model.  Adapted from Handbook of Geophysics and Space
C   Envirnoments (circa 1965) S. L. Valley ed. Air Force Cambridge
C   Research Laboratories and Chapman and Bartels, 1940, GEOPHYSICS,
C   Oxford)
C      NOTE: The Gaussian coefficients from Chapman and Bartels give
C   a slightly better representation of the field than Valley so these
C   values are used here.
C      Values of H returned are probably good to better than 20%.
C   At the VLA the model is 6% low in total intensity and 11 deg W in
C   magnetic declination.
C    Inputs:
C     GLAT    R    Geocentric latitude (radians)
C     GLONG   R    Geocentric EAST longitude (radians)
C     RADIUS  R    Distance from the center of the earth (m)
C    Output:
C     H(3)    R    Magnetic field vector (gauss),
C                  (1) = positive away from earth center,
C                  (2) = positive east,
C                  (3) = positive north.
C-----------------------------------------------------------------------
      REAL    GLAT, GLONG, RADIUS, H(3),
     *   RE, FACT, GLATMP, GLONMP, PI,
     *   H02, L0, L1, L2, E, SQRT3,
     *   G10, G11, G20, G21, G22, H11, H21, H22, X0, Y0, Z0
      PARAMETER (PI = 3.14159265)
C                                       Geographic coordinates of
C                                       North magnetic pole.
      PARAMETER (GLATMP = 78.63 * PI / 180)
      PARAMETER (GLONMP = 289.85 * PI / 180)
C                                       Gaussian coefficients(gauss):
C                                       From Handbook of Geophysics...
C                                       Epoch 1960.
C                                       Modified??????
C      PARAMETER (G10 = -0.30509)
C      PARAMETER (G11 = -0.02181/2.0)
C      PARAMETER (G20 = -0.02196/2.0)
C      PARAMETER (G21 =  0.05145/3.0)
C      PARAMETER (G22 =  0.01448/4.0)
C      PARAMETER (H11 =  0.05841/2.0)
C      PARAMETER (H21 = -0.03443/3.0)
C      PARAMETER (H22 =  0.00172/4.0)
C                                       Chapman values Epoch 1922
      PARAMETER (G10 = -.3095)
      PARAMETER (G11 = -.0226)
      PARAMETER (G20 = -.0067)
      PARAMETER (G21 = 0.0292)
      PARAMETER (G22 = 0.0143)
      PARAMETER (H11 = 0.0592)
      PARAMETER (H21 = -.0122)
      PARAMETER (H22 = 0.0113)
C                                       SQRT3 = sqrt (3.0)
      PARAMETER (SQRT3 = 1.732050808)
C                                       Compute dipole center in units
C                                       of earth radius.
      PARAMETER (H02 = G10*G10 + G11*G11 + H11*H11)
      PARAMETER (L0  = 2.0*G10*G20 + (G11*G21 + H11*H21) * SQRT3)
      PARAMETER (L1  = -G11*G20 + (G10*G21+G11*G22+H11*H22) * SQRT3)
      PARAMETER (L2 = -H11*G20 + (G10*H21-H11*G22+G11*H22) * SQRT3)
      PARAMETER (E = (L0*G10 + L1*G11 + L2*H11) / (4.0*H02))
      PARAMETER (X0 = (L1 - G11*E) / (3.0*H02))
      PARAMETER (Y0 = (L2 - H11*E) / (3.0*H02))
      PARAMETER (Z0 = (L0 - G10*E) / (3.0*H02))
      REAL   X0M, Y0M, Z0M, HMAG, HD(3), CLA, SLA, CLO, SLO
      DOUBLE PRECISION POS0(3), POS1(3), POSTMP(3), POST2(3), RADDIP,
     *   COLAT,
     *   LONDIP, CA, SA, CB, SB
C                                       RE = Radius of earth (avg polar
C                                       and equitorial)
      DATA RE /6367650.0/
C-----------------------------------------------------------------------
C                                       Center of dipole
      X0M = X0 * RE
      Y0M = Y0 * RE
      Z0M = Z0 * RE
C                                       Convert to earth center x,y,z
C                                       Here y=> 90 e long.
      POS0(1) = RADIUS * COS (GLAT) * COS (GLONG)
      POS0(2) = RADIUS * COS (GLAT) * SIN (GLONG)
      POS0(3) = RADIUS * SIN (GLAT)
C                                       Translate
      POSTMP(1) = POS0(1) - X0M
      POSTMP(2) = POS0(2) - Y0M
      POSTMP(3) = POS0(3) - Z0M
C                                       Rotate to dipole coord.
      CA = COS (GLONMP)
      SA = SIN (GLONMP)
      CB = SIN (GLATMP)
      SB = -COS (GLATMP)
      POST2(1) = (POSTMP(1)*CA + POSTMP(2)*SA) * CB +
     *   POSTMP(3) * SB
      POST2(2) = POSTMP(2) * CA - POSTMP(1) * SA
      POST2(3) = POSTMP(3) * CB - SB * (POSTMP(1)*CA + POSTMP(2)*SA)
C                                       Polar coordinates in dipole.
      RADDIP = SQRT (POST2(1)*POST2(1) + POST2(2)*POST2(2) +
     *   POST2(3)*POST2(3))
      COLAT = ACOS (POST2(3) / RADDIP)
      LONDIP = ATAN2 (POST2(2), POST2(1))
      CLA = SIN (COLAT)
      SLA = COS (COLAT)
      CLO = COS (LONDIP)
      SLO = SIN (LONDIP)
C                                       Terms of dipole, local
      FACT = SQRT (H02) * ((RE / RADDIP) ** 3)
      H(1) = -2.0 * FACT * COS (COLAT)
      H(2) = 0.0
      H(3) = FACT * SIN (COLAT)
C                                       Rotate to dipole centered
      HD(1) = (H(1)*CLA - H(3)*SLA) * CLO - H(2) * SLO
      HD(2) = H(2) * CLO + (H(1)*CLA - H(3)*SLA) * SLO
      HD(3) = H(3) * CLA + H(1) * SLA
C                                       Modulus of HD
      HMAG = SQRT (HD(1)*HD(1) + HD(2)*HD(2) + HD(3)*HD(3))
C                                       Find position 1 km from
C                                       position in the direction of HD.
      POST2(1) = POST2(1) + 1000.0 * HD(1) / HMAG
      POST2(2) = POST2(2) + 1000.0 * HD(2) / HMAG
      POST2(3) = POST2(3) + 1000.0 * HD(3) / HMAG
C                                       Rotate new position to earth
C                                       system.
      POSTMP(1) = (POST2(1)*CB - POST2(3)*SB) * CA - POST2(2) * SA
      POSTMP(2) = POST2(2) * CA + (POST2(1)*CB - POST2(3)*SB) * SA
      POSTMP(3) = POST2(3) * CB + POST2(1) * SB
C                                       Translate to earth center
      POS1(1) = POSTMP(1) + X0M
      POS1(2) = POSTMP(2) + Y0M
      POS1(3) = POSTMP(3) + Z0M
C                                       Earth centered field
      HD(1) = (POS1(1) - POS0(1)) * 0.001 * HMAG
      HD(2) = (POS1(2) - POS0(2)) * 0.001 * HMAG
      HD(3) = (POS1(3) - POS0(3)) * 0.001 * HMAG
C                                       Earth local field
      CLA = COS (GLAT)
      SLA = SIN (GLAT)
      CLO = COS (GLONG)
      SLO = SIN (GLONG)
      H(1) = (HD(1)*CLO + HD(2)*SLO) * CLA + HD(3) * SLA
      H(2) = HD(2) * CLO - HD(1) * SLO
      H(3) = HD(3) * CLA - (HD(1)*CLO + HD(2)*SLO) * SLA
C
 999  RETURN
      END

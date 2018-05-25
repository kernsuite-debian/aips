      SUBROUTINE SATPRS (TEMP, PRESS, SATP)
C-----------------------------------------------------------------------
C! Calculate saturation water pressure
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   Function to calculate saturation vapor pressure over water or ice,
C   in millibar, given temperature (temp) in degrees C, and pressure
C   (press) in millibar.  Taken from:
C      Buck, A.L., New Equations for Computing Vapor Pressure and
C        Enhancement Factor, J. Appl. Met., v.20, pp. 1527-1532, 1981
C   who references:
C      Wexler, A., Vapor Pressure Formulation for Water in the Range
C         0C to 100C - A Revision, J. Res. Natl. Bur. Stand., v.80A,
C         pp. 775 ff, 1976
C   and
C      Wexler, A., Vapor Pressure Formulation for Ice, J. Res. Natl.
C        Bur. Stand., v.81A, pp. 5-20, 1977
C   for the "exact" formulations - which are reworks of the Goff-Gratch
C   formulation:
C      Goff, J.A., and S. Gratch, Low-pressure Properties of Water from
C        -160F to 212F. Trans. Am. Soc. Heat. Vent. Eng., v. 52, 95-121,
C        1946
C   note that i use the fw5 and fi5 coefficients for the "enhancement
C   factor" from Table 3 of Buck.
C   Bryan Butler, NRAO
C-----------------------------------------------------------------------
      REAL      TEMP, PRESS, SATP
C
      DOUBLE PRECISION THETA, EW, EI, FW, FI, AOW, AOI, BOW, BOI, COW,
     *   COI, DOW, DOI, EOW, EOI
      DATA AOW, BOW, COW, DOW, EOW / 4.1D-4, 3.48D-6, 7.4D-10, 30.6D0,
     *                               -3.8D-2 /
      DATA AOI, BOI, COI, DOI, EOI / 4.8D-4, 3.47D-6, 5.9D-10, 23.8D0,
     *                               -3.1D-2 /
C-----------------------------------------------------------------------
      THETA = TEMP + 273.15D0
C                                       !!! water !!!
      IF (TEMP.GT.0.01D0) THEN
         EW = (-2991.2729D0 / THETA**2) + (-6017.0128 / THETA) +
     *      (18.87643854D0) + (-0.028354721D0 * THETA) +
     *      (0.17838301D-4 * THETA**2) +
     *      (-0.84150417D-9 * THETA**3) +
     *      (0.44412543D-12 * THETA**4) +
     *      (2.858487D0 * DLOG(THETA))
         EW = 0.01D0 * DEXP (EW)
         FW = 1.0D0 + AOW +
     *      PRESS * (BOW + COW * (TEMP + DOW + EOW * PRESS)**2)
         SATP = EW * FW
C                                       !!! ice !!!
      ELSE
         EI = (-5865.3696D0 / THETA) +
     *      (22.241033D0) + (0.013749042D0 * THETA) +
     *      (-0.34031775D-4 * THETA**2) +
     *      (0.26967687D-7 * THETA**3) +
     *      (0.6918651D0 * DLOG(THETA))
         EI = 0.01D0 * DEXP (EI)
         FI = 1.0D0 + AOI +
     *      PRESS * (BOI + COI * (TEMP + DOI + EOI * PRESS)**2)
         SATP = EI * FI
         END IF
C
 999  RETURN
      END

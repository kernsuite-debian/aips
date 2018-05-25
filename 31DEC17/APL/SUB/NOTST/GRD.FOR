      SUBROUTINE GRD (JD, RA, DEC, SINDEC, COSDEC, GRDRA, GRDDEC)
C-----------------------------------------------------------------------
C! Compute the general relativity displacements in RA and DEC.
C# Math Coordinates
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
C   Compute the general relativity displacements in RA and DEC by a
C   procedure described by Cam Wade in memo dated December, 1975.
C   Input parameters:
C     JD       D    Julian day
C     RA       D    apparent right ascension, radians
C     DEC      D    apparent declination, radians
C     SINDEC   D    DSIN(DEC)
C     COSDEC   D    DCOS(DEC)
C   Output parameters:
C      GRDRA   D    right ascension displacement
C      GRDDEC  D    declination displacement
C   Both are in radians and k here is in radians.
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, RA, DEC, SINDEC, COSDEC, GRDRA, GRDDEC
      DOUBLE PRECISION K, TWOPI, DEG, L, M, D, DLAM, RLAM, SOLRA,
     *   SINSD, COSSD, RADIF, CORADF, COSD, KK
      INTEGER   I
      DATA K /1.9742D-8/
      DATA TWOPI /6.28318530717959D0/
      DATA DEG /1.74532925199433D-2/
C-----------------------------------------------------------------------
C                                       D=Julian days since Jan.0, 1900
      D = JD - 2415020.D0
C                                       L is Sun's mean longitude, less
C                                       aberration, in radians.
      L = .77692D0 + 2.737909265D-3 * D
      I = L
      L = (L-I) * TWOPI
C                                       M is Sun's mean anomaly, in
C                                       radians.
      M = .996D0 + 2.737778519D-3 * D
      I = M
      M = (M-I) * TWOPI
C                                       DLAM is Sun's apparent longitude
C                                       in degrees.  RLAM is same, but
C                                       in radians.
      DLAM = L / DEG + 1.92D0 * SIN(M) + .02D0 * SIN(M+M)
      RLAM = DLAM * DEG
C                                       SOLRA is solar RA in radians.
      SOLRA = DLAM - 2.47D0 * SIN(RLAM+RLAM) + .05D0 * SIN(4D0*RLAM)
      SOLRA = SOLRA * DEG
C                                       SINSD is DSIN(solar dec)
      SINSD = .3978D0 * SIN(RLAM)
      COSSD = SQRT(1.D0 - SINSD*SINSD)
      RADIF = RA - SOLRA
      CORADF = COS(RADIF)
      COSD = SINDEC * SINSD + COSDEC * COSSD * CORADF
      KK = K * (1.D0 / (1.D0-COSD) + .5D0*COSD)
      GRDRA = KK * SIN(RADIF) * COSSD/COSDEC
      GRDDEC = KK * (SINDEC * COSSD * CORADF - COSDEC * SINSD)
C
 999  RETURN
      END

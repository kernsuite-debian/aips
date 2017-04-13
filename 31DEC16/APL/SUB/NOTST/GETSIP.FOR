      SUBROUTINE GETSIP (LAT, AZ, ZA, ALT, DLAT, DLONG, AZION, ZAION)
C-----------------------------------------------------------------------
C! Calculate geometry of sub-ionospheric point.
C# Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-1999
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
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Given the azimuth and zenith angle of the line of sight, determine
C   the offsets of the intersection of the line of site with the
C   ionosphere in latitude and longitude relative to the observing site
C   and the azimuth and zenith angle of the line of sight at the point
C   of intersection.
C
C   The ionsphere is assumed to approximated by a thin shell at a
C   uniform altitude.
C
C   Numeric overflow will occur for extreme Northern latitudes.
C
C   Inputs:
C      LAT       R    Latitude of observer (radians)
C      AZ        R    Azimuth (radians)
C      ZA        R    Zenith angle (radians)
C      ALT       R    Altitude of ionosphere (metres)
C
C   Outputs:
C      DLAT      R    Latitude offset of subionospheric point (rad)
C      DLONG     R    Longitude offset of SIP (radians)
C      AZION     R    Azimuth at intersection (radians)
C      ZAION     R    Zenith angle at intersection (radians)
C-----------------------------------------------------------------------
      REAL    LAT, AZ, ZA, ALT, DLAT, DLONG, AZION, ZAION
C
C   Local variables:
C      RADIUS    Earth radius (m)
C      THETA     Great circle distance from observer to SIP (radians)
C      LATION    Latitude of the sub-ionospheric point (radians)
C
      REAL    RADIUS, THETA, LATION, SAZION
      PARAMETER (RADIUS = 6378000.0)
C
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C
C     The two-dimensional sine rule gives the zenith angle at the
C     intersection of the line of sight with the ionosphere:
C
      ZAION = ASIN ((RADIUS * SIN (ZA)) / (RADIUS + ALT))
C
C     Use the sum of the internal angles of a triange to determine THETA
C
      THETA = ZA - ZAION
C
C     The cosine rule for spherical triangles gives us the latitude of
C     the sub-ionospheric point.
C
      LATION = ASIN (SIN (LAT) * COS (THETA)
     *         + COS (LAT) * SIN (THETA) * COS (AZ))
      DLAT = LATION - LAT
C
C     This gives us enough information to bootstrap the remaining angles
C     using the sine rule for spherical triangles. The AZION calculation
C     can yield a sine with an absolute value greater than 1 due to
C     finite precision calculations and so needs to be protected. This
C     is not necessary for DLONG.
C
      SAZION = SIN (AZ) * COS (LAT) / COS (LATION)
      IF (SAZION .GE. 1.0) THEN
         AZION = +PI / 2.0
      ELSE IF (SAZION .LE. -1.0) THEN
         AZION = -PI / 2.0
      ELSE
         AZION = ASIN (SAZION)
      END IF
      DLONG = ASIN (SIN (AZ) * SIN (THETA) / COS (LATION))
C
C     AZION is in the principal range -PI/2 .. +PI/2; correct this if
C     the source is in the Southern sky.
C
      IF (ABS(AZ) .GT. PI/2.0) THEN
         IF (AZION .GT. 0.0) THEN
            AZION = PI - AZION
         ELSE
            AZION = -PI - AZION
         END IF
      END IF
C
      END

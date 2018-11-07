      SUBROUTINE GEOPOS (LON, LAT, ELE, X, Y, Z)
C----------------------------------------------------------------------
C   Convert geodetic (surface of the earth) to cartesian coordinates
C# Utility
C----------------------------------------------------------------------
C;  Copyright (C) 1998-1998, 2016
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
C----------------------------------------------------------------------
C   Converts geodetic coordinates to VLBI cartesian coordinates
C   Input:
C      LON    D   geodetic west longitude (degrees)
C      LAT    D   geodetic north latitude (degrees)
C      ELE    D   geodetic elevation (meters)
C   Output:
C      X      D   x coord, X-axis lies along Greenwich Longitude
C      Y      D   y coord, Y-axis lies along N.American Longitude
C      Z      D   z coord, Z-axis lies passes through North Pole
C---------------------------------------------------------------------
      DOUBLE PRECISION LON, LAT, ELE, X, Y, Z
C
      DOUBLE PRECISION SLAT, CLAT, CLON, SLON, ECC2, ELIPR
      DOUBLE PRECISION SMAXIS, FLATEN
      PARAMETER (SMAXIS = 6378136.D0)
      PARAMETER (FLATEN = 1.D0/298.257)
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
C                                       Get sine and cos of longitude
      CLON = LON * DG2RAD
      SLON = SIN (CLON)
      CLON = COS (CLON)
C                                       Get sine and cos of latitude
      CLAT = LAT * DG2RAD
      SLAT = SIN (CLAT)
      CLAT = COS (CLAT)
C                                       These formulas come from the
C                                       Astronomical Almanac 1996, pg K12
C                                       Work out earth oblateness
      ECC2 = (2.D0 - FLATEN) * FLATEN
C                                       Work out earth depth
      ELIPR = SMAXIS / (SQRT (1.D0 - ECC2 * SLAT **2))
C
      X = ( ELIPR + ELE ) * CLAT * CLON
      Y = ( ELIPR + ELE ) * CLAT * SLON
      Z = ((1.D0 - ECC2) * ELIPR + ELE) * SLAT
C                                       need RHand system
      Y = -Y
      RETURN
      END

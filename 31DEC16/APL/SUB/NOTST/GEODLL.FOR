      SUBROUTINE GEODLL (X, Y, Z, LAT, LONG)
C-----------------------------------------------------------------------
C! Calculate geodetic lat and long.
C# General
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
C  Routine to calculate the geodetic latitude and logitude of a
C  point on the earth's surface from its geocentric coordinates.
C
C  Inputs:
C      X,Y,Z      D         Coordinates of point (metres)
C
C  Outputs:
C      LAT        D         Geodetic latitude (rads)
C      LONG       D         Geodetic longitude (rads)
C-----------------------------------------------------------------------
      DOUBLE PRECISION  X, Y, Z, LONG, GCLAT, DLAT, LAT, C1, C2, C3
      DATA C1 / -3.358513D-3 /,
     *     C2 /  5.6398D-6 /,
     *     C3 / -0.01261D-6 /
C-----------------------------------------------------------------------
      LONG = ATAN2 (Y, X)
      GCLAT = ASIN ( Z / SQRT (X*X + Y*Y + Z*Z) )
      DLAT =  C1 * SIN ( 2.D0 * GCLAT )
     *      + C2 * SIN ( 4.D0 * GCLAT )
     *      + C3 * SIN ( 6.D0 * GCLAT )
      LAT = GCLAT - DLAT
      RETURN
      END

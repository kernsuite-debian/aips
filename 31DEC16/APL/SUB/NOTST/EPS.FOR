      DOUBLE PRECISION FUNCTION EPS (JD)
C-----------------------------------------------------------------------
C! Computes mean obliquity of the Ecliptic for a Julian date.
C# Math
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
C   Calculation of epsilon, the mean obliquity of the Ecliptic.
C   To obtain the true obliquity, one has to add nutation in obliquity,
C   as calculated by NUT2.
C-----------------------------------------------------------------------
      DOUBLE PRECISION T, JD
C-----------------------------------------------------------------------
      T = (JD - 2415020.D0) / 36525.D0
      EPS = ((1.81D-3 * T - 5.9D-3) * T - 46.845D0) * T  +  84428.26D0
      EPS = EPS * ATAN (1.D0) / 162000.D0
C
 999  RETURN
      END

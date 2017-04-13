      SUBROUTINE DA46 (DATJUL, A4, A5, A6)
C-----------------------------------------------------------------------
C! Computes arguments A4, A5 and A6 of the mean motion of the moon
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
C   Calculation of the basic arguments A4, A5, A6 of the mean motion of
C   the Moon.  A4 is the mean longitude of the Moon, measured from the
C   mean equinox of DATJUL along the ecliptic to mean ascending node,
C   and then along the orbit.  A5 is the longitude of mean ascending
C   node at DATJUL, referred to the mean equinox of DATJUL.  A6 is the
C   mean anomaly of the Moon at DATJUL.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, A4, A5, A6
      DOUBLE PRECISION TWOPI, CC, T
C-----------------------------------------------------------------------
      TWOPI = 8.D0 * ATAN(1.D0)
      CC = TWOPI / 1296000.D0
      T = (DATJUL - 2415020.D0) / 36525.D0
      A4 = (((.0068D0 * T - 4.08D0) * T + 1732564379.31D0) * T
     *   + 973562.99D0) * CC
      A5 = (((.008D0 * T + 7.48D0) * T - 6962911.23D0) * T
     *   + 933059.79D0) * CC
      A6 = (((.0518D0 * T + 33.09D0) * T + 1717915856.79D0)
     *   * T + 1065976.59D0) * CC
      A4 = MOD (A4, TWOPI)
      A5 = MOD (A5, TWOPI)
      A6 = MOD (A6, TWOPI)
C
 999  RETURN
      END

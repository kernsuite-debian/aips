      SUBROUTINE DA13 (DATJUL, A1, A2, A3)
C-----------------------------------------------------------------------
C! Computes arguments A1, A2 and A3 of the mean motion of the sun.
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
C   Calculation of the basic arguments A1, A2, A3 of the mean motion of
C   the Sun.  A1 is the mean longitude of the Sun at DATJUL, referred to
C   the mean equinox of DATJUL.  A2 is the mean longitude of the sun's
C   perigee at DATJUL, referred to the mean equinox of DATJUL.  A3 is
C   the mean anomaly of the Sun at DATJUL.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, A1, A2, A3
      DOUBLE PRECISION TWOPI, CC, T
C-----------------------------------------------------------------------
      TWOPI = 8.D0 * ATAN (1.D0)
      CC = TWOPI / 1296000.D0
      T = (DATJUL - 2415020.D0) / 36525.D0
      A1 = ((1.089D0 * T + 129602768.13D0) * T + 1006908.04D0) * CC
      A2 = (((.012D0 * T + 1.63D0) * T + 6189.03D0) * T
     *   + 1012395D0) * CC
      A3 = (((-.012D0 * T - .54D0) * T + 129596579.1D0) * T
     *   + 1290513.04D0) * CC
      A1 = MOD (A1, TWOPI)
      A2 = MOD (A2, TWOPI)
      A3 = MOD (A3, TWOPI)
C
 999  RETURN
      END

      SUBROUTINE DUVC (X, Y, Z, ALPHA, DELTA)
C-----------------------------------------------------------------------
C! Converts unit vector to celestial coordinates.
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
C   Calculation of the spherical coordinates which correspond to the
C   given components of a unit vector.
C-----------------------------------------------------------------------
      DOUBLE PRECISION X, Y, Z, ALPHA, DELTA
      DOUBLE PRECISION PDIST
C-----------------------------------------------------------------------
      IF ((X.EQ.0.D0) .AND. (Y.EQ.0.D0)) GO TO 10
         PDIST = SQRT (X*X + Y*Y)
         ALPHA = ATAN2 (Y, X)
         IF (ALPHA.LT.0.D0) ALPHA = ALPHA + 8.D0 * ATAN (1.D0)
         DELTA = ATAN2 (Z, PDIST)
         GO TO 999
 10   CONTINUE
         ALPHA = 0.D0
         DELTA = 0.D0
         PDIST = 0.D0
         IF (Z.NE.0.D0) DELTA = ATAN2 (Z, PDIST)
C
 999  RETURN
      END

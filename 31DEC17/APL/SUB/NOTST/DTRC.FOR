      SUBROUTINE DTRC (TRCMAT, ALPH0, DEL0, ALPHA, DELTA)
C-----------------------------------------------------------------------
C! Transforms spherical coordinates given transform matrix.
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
C   Spherical coordinates of an object are given in a coordinate
C   system C1.  The matrix is given which relates C1 to another system,
C   C2.  The procedure computes the spherical coordinates of the same
C   object in the coordinate system C2.
C-----------------------------------------------------------------------
      DOUBLE PRECISION TRCMAT(3,3), ALPH0, DEL0, ALPHA, DELTA
      INTEGER   I, J
      DOUBLE PRECISION VEC0(3), VEC(3)
C-----------------------------------------------------------------------
      CALL DCUV (ALPH0, DEL0, VEC0(1), VEC0(2), VEC0(3))
      DO 10 I = 1,3
         VEC(I) = 0.D0
         DO 10 J = 1,3
            VEC(I) = TRCMAT(I,J) * VEC0(J) + VEC(I)
 10         CONTINUE
      CALL DUVC (VEC(1), VEC(2), VEC(3), ALPHA, DELTA)
C
 999  RETURN
      END

      SUBROUTINE BASLIN (NA, X, Y, Z, FREQ, ARRLON, BX, BY, BZ,
     *   B1, B2, GH)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C   Subroutine to calculate baseline coordinates from a given set of
C   station coordinates
C   Inputs:
C      NA       I      Number of antennas
C      X        D(*)   X station coordinates
C      Y        D(*)   X station coordinates
C      X        D(*)   Z station coordinates
C      FREQ     D      frequency in Hz
C      ARRLON   D      Array longitude (rad.)
C   Outputs:
C      BX       D(*)   baseline coordinates in wavelengths X
C      BY       D(*)   baseline coordinates in wavelengths Y
C      BZ       D(*)   baseline coordinates in wavelengths Z
C      B1       D(*)   baseline coordinates in polar coordinates
C      B2       D(*)      relative Greenwich meridian
C      GH       D      hour angle
C-----------------------------------------------------------------------
      INTEGER   NA
      DOUBLE PRECISION X(*), Y(*), Z(*), FREQ, ARRLON, BX(*), BY(*),
     *   BZ(*), B1(*), B2(*), GH(*)
C
      INTEGER   I, J, NB
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       baselines
      NB = 0
      DO 20 I = 1,NA-1
         DO 10 J = I+1,NA
            NB = NB + 1
            BX(NB) = X(J) - X(I)
            BY(NB) = Y(J) - Y(I)
            BZ(NB) = Z(J) - Z(I)
C                                       Sign of B1 and B2 flipped to
C                                       agree with AIPS phase convention
            B2(NB) = -SQRT (BX(NB) * BX(NB) + BY(NB) * BY(NB)) *
     *         FREQ/VELITE
            B1(NB) = -BZ(NB) * FREQ / VELITE
            GH(NB) = -ARRLON
            IF ((BY(NB).NE.0.0D0) .OR. (BX(NB).NE.0.0D0))
     *         GH(NB)  = GH(NB) + ATAN2 (-BY(NB), BX(NB))
            IF (GH(NB).GT.PI) GH(NB) = GH(NB) - TWOPI
            IF (GH(NB).LE.-PI) GH(NB) = GH(NB) + TWOPI
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END

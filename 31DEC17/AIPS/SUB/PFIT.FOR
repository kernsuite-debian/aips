      SUBROUTINE PFIT (A, IX, S, DX, IERR)
C-----------------------------------------------------------------------
C! parabolic fit to 3x3 matrix
C# Modeling
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
C   Make a parabolic least-squares fit to a 3*3 matrix about the
C   point IX to determine strength and position of maximum.
C   Inputs:
C      A      R(16,16)   Data input array
C      IX     I(2)       Initial guess pixel position of max
C   Outputs:
C      DX     R(2)       Position of max relative to IX
C      S      R          Strength of max
C      IERR   I          0 => o.k., 1 => initial guess out of array
C-----------------------------------------------------------------------
      REAL       A(16,16), DX(2), S
      INTEGER    IX(2), IERR
C
      REAL       X, Y, MAT(3,3), TEMP(6), MOMAR(6), D
      DATA MAT /0.55555, -0.33333, -0.33333,   -0.33333, 0.5, 0.0,
     *   -0.33333, 0.0, 0.5/
C-----------------------------------------------------------------------
C                                       calculate xy-moments :
C                                       00, 01, 02, 10, 11, 20
      X = IX(1)
      Y = IX(2)
      CALL MOM (A, X, Y, 3, 3, MOMAR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       multiply matrix * even moms
C                                       yields const & quadratic terms
      TEMP(1) = MOMAR(1)
      TEMP(2) = MOMAR(3)
      TEMP(3) = MOMAR(6)
      CALL MATVMU (MAT, TEMP, TEMP(4), 3)
C                                       pick up linear & cross terms
      TEMP(1) = MOMAR(2) / 6.
      TEMP(2) = MOMAR(4) / 6.
      TEMP(3) = MOMAR(5) / 4.
C                                       offset of peak
      D = 4.* TEMP(5) * TEMP(6) - TEMP(3)**2
      IERR = 2
      IF (D.EQ.0.0) GO TO 999
      IERR = 0
      DX(1) = (TEMP(3)*TEMP(1) - 2.*TEMP(2)*TEMP(5)) / D
      DX(2) = (TEMP(3)*TEMP(2) - 2.*TEMP(1)*TEMP(6)) / D
C                                       value of peak
      S = TEMP(4) + DX(1)*(TEMP(2) + DX(1)*TEMP(6)
     *   + DX(2)*TEMP(3)) + DX(2)*(TEMP(1)+DX(2)*TEMP(5))
C
 999  RETURN
      END

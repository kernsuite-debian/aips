      FUNCTION DGNRAO (L, M, DTHETA)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C   Compute the Legendre-type functions used in the GB 140 ft gain curve
C   Inputs:
C      L,M     I     Order of the Legendre type polynomial
C      DTHETA  D     Theta (in radians)
C   Output:
C      DGNRAO  D     Value of the associated Legendre polynomial with
C                    sin(theta)=sqrt(1-x**2) and the (-1)**m term
C                    removed.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DGNRAO, DTHETA
      INTEGER L, M
C
      DOUBLE PRECISION DP0, DP1, DSINT, DCOST, DFACT, DFM1, DSINTM,
     *   DGTMP
      INTEGER I, J
C-----------------------------------------------------------------------
      DGTMP = 0.0D0
C                                       Check validity of the input
C                                       parameters
      IF ((M.LT.0).OR.(L.LT.0).OR.(M.GT.L)) GO TO 999
C
      DCOST = COS (DTHETA)
      DSINT = SIN (DTHETA)
C                                       Compute P(m,m)
      DFACT = 1.0D0
      DO 20 J = 2, M
         DFACT = DFACT * (2*J-1)
20       CONTINUE
      DFM1 = 1.0D0
C                                       Avoid raising zero to
C                                       a power directly
      IF (DSINT.EQ.0.0D0) THEN
         DP0 = 0.0D0
         IF (M.EQ.0.0D0) DP0 = 1.0D0
      ELSE
         DSINTM = DSINT ** M
         DP0 = DFM1 * DFACT * DSINTM
         END IF
C
      I = M + 2
      IF (M.NE.L) GO TO 30
         DGTMP = DP0
         GO TO 999
C                                       Recurrence loop
30    DP1 = DCOST * (2.0D0 * M + 1.0D0) * DP0
      IF (L.NE.(M+1)) GO TO 50
         DGTMP = DP1
         GO TO 999
C                                       Compute P(m,l)
C                                       While (I <= L) do:
50    IF (I.GT.L) GO TO 999
         DGTMP = (DCOST * (2.0D0 * I - 1.0D0) * DP1 -
     *      (I + M - 1) * DP0) / DBLE (I - M)
         DP0 = DP1
         DP1 = DGTMP
         I=I+1
C                                       Endwhile
         GO TO 50
C                                       Exit
999   DGNRAO = DGTMP
      RETURN
      END

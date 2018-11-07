      SUBROUTINE CHEBY (DA, DB, DX, DCHEB, N, IRET)
C----------------------------------------------------------------------
C! Evaluate Chebyshev polynomials
C# Math
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
C   Evaluate a sequence of Chebyshev polynomials.
C   Inputs:
C      DA      D       Lower limit x-ordinate range
C      DB      D       Upper limit x-ordinate range
C      DX      D       x-ordinate value
C      N       I       Order of maximum Chebyshev polynomial
C   Output:
C      DCHEB   D(N+1)  Polynomial values
C      IRET    I       Return code (0 => ok; else error)
C---------------------------------------------------------------------
      INTEGER N, IRET
      DOUBLE PRECISION DCHEB(N+1), DA, DB, DX
C
      DOUBLE PRECISION D2X, DXX
      INTEGER I
C---------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Check for valid input
C                                       parameters
      IF (((DB-DA).EQ.0.0D0).OR.(N.LT.0).OR.(DX.LT.DA).OR.
     *   (DX.GT.DB)) IRET = 1
      IF (IRET.NE.0) GO TO 999
C                                       Transform to range [A,B]
      DXX = (2.0D0 * DX - DA - DB) / (DB - DA)
      D2X = 2.0D0 * DXX
C                                       N = 0
      DCHEB(1) = 1.0D0
      IF (N.EQ.0) GO TO 999
C                                       N = 1
      DCHEB(2) = DXX
      IF (N.EQ.1) GO TO 999
C                                       Recurrence relation (N > 1)
      I = 2
 100  DCHEB(I+1) = D2X * DCHEB(I) - DCHEB(I-1)
      IF (I.EQ.N) GO TO 999
         I = I + 1
         GO TO 100
C                                       Exit
 999  RETURN
      END

      SUBROUTINE CAXPY (N, CA, CX, INCX, CY, INCY)
C-----------------------------------------------------------------------
C! Linpack routine: Complex constant times a vector plus a vector
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
C   Complex constant times a vector plus a vector.
C       CY(i*INCY+1) = CY(i*INCY+1) + CA * CX(i*INCX+1) for i=0 to N-1
C     Inputs:
C      N     I  Element count
C      CA    C  Constant
C      CX(*) C  Array
C      INCX  I  Increment of CX
C    Input/output:
C      CY(*) C  Array
C      INCY  I  Increment of CY
C     Jack Dongarra, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      COMPLEX   CX(*), CY(*), CA
      INTEGER   I, INCX, INCY, IX, IY, N
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF (ABS (REAL (CA)) + ABS (AIMAG (CA)) .EQ. 0.0 ) GO TO 999
      IF (INCX.EQ.1.AND.INCY.EQ.1) GO TO 20
C                                       Code for unequal increments or
C                                       equal increments not equal to 1.
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
         CY(IY) = CY(IY) + CA*CX(IX)
         IX = IX + INCX
         IY = IY + INCY
   10    CONTINUE
      GO TO 999
C                                       Code for both increments equal
C                                       to 1.
   20 DO 30 I = 1,N
         CY(I) = CY(I) + CA*CX(I)
   30    CONTINUE
C
 999  RETURN
      END

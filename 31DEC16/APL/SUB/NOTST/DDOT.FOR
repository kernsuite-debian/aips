      SUBROUTINE DDOT (N, DX, INCX, DY, INCY, XDDOT)
C-----------------------------------------------------------------------
C! Linpack routine: Form dot product of two vectors (DOUBLE)
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
C   Forms the dot product of two vectors.
C   uses unrolled loops for increments equal to one.
C   Jack Dongarra, Linpack, 3/11/78.
C     Inputs:
C      N       I    Element count
C      DX(*)   D    First vector
C      INCX    I    Increment of DX
C      DY(*)   D    Second vector
C      INCY    I    Increment of DY
C    Output:
C      XDDOT   D    Dot product of DX and DY
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*), DY(*), XDDOT, DTEMP
      INTEGER   I, INCX, INCY, IX, IY, M, MP1, N
C-----------------------------------------------------------------------
      XDDOT = 0.0D0
      DTEMP = 0.0D0
      IF (N.LE.0) GO TO 999
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
C                                       Code for unequal increments or
C                                       equal increments not equal to 1
C
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1) * INCX + 1
      IF (INCY.LT.0) IY = (-N+1) * INCY + 1
      DO 10 I = 1,N
         DTEMP = DTEMP + DX(IX)*DY(IY)
         IX = IX + INCX
         IY = IY + INCY
   10    CONTINUE
      XDDOT = DTEMP
      GO TO 999
C                                       Code for both increments
C                                       equal to 1.
C                                       Clean-up loop.
   20 M = MOD (N, 5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
         DTEMP = DTEMP + DX(I)*DY(I)
   30    CONTINUE
      IF (N.LT.5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     *      DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50    CONTINUE
   60 XDDOT = DTEMP
C
 999  RETURN
      END

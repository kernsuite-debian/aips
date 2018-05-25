      INTEGER FUNCTION ICAMAX (N, CX, INCX)
C-----------------------------------------------------------------------
C! Linpack routine: Index of complex element with max. abs. value
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
C   Find index of complex element having Max. absolute value.
C     Inputs:
C      N     I  Element count
C      CX(*) C  Array
C      INCX  I  Increment of CX
C     Jack Dongarra, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      COMPLEX CX(*)
      REAL    SMAX
      INTEGER I, INCX, IX, N
      COMPLEX ZDUM
      REAL CABS1
C-----------------------------------------------------------------------
      CABS1(ZDUM) = ABS (REAL (ZDUM)) + ABS (AIMAG (ZDUM))
C
      ICAMAX = 0
      IF (N.LT.1) GO TO 999
      ICAMAX = 1
      IF (N.EQ.1) GO TO 999
      IF (INCX.EQ.1) GO TO 20
C                                       Code for increment not = 1
      IX = 1
      SMAX = CABS1 (CX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF (CABS1 (CX(IX)).GT.SMAX) THEN
            ICAMAX = I
            SMAX = CABS1 (CX(IX))
            END IF
         IX = IX + INCX
   10    CONTINUE
      GO TO 999
C                                       Code for increment equal to 1
   20 SMAX = CABS1 (CX(1))
      DO 30 I = 2,N
         IF (CABS1 (CX(I)).GT.SMAX) THEN
            ICAMAX = I
            SMAX = CABS1 (CX(I))
            END IF
   30    CONTINUE
C
 999  RETURN
      END

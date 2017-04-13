      SUBROUTINE QVIDIV (A, I, D1, D2, B, J, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Divide a vector by the product of two integers.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Divides the given vector by the product of two integers:
C        B(K) = A(K) / (D1 * D2)  for K = 0,N-1
C   Inputs:
C      A   I  Source vector base address.
C      I   I  Increment for A
C      D1  I  First dividend.
C      D2  I  Second dividend.
C      B   I  Destination vector base address.
C      J   I  Increment for B
C      N   I  Element count.
C-----------------------------------------------------------------------
      INTEGER   A, I, D1, D2, B, J, N
C
      LONGINT   JA, JB
      INTEGER   LOOP
      DOUBLE PRECISION FACT
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + PSAPOF
      JB = B + PSAPOF
C                                        Determine scaling factor.
      FACT = D1
      FACT = FACT * D2
      FACT = 1.0D0 / FACT
C                                       Loop, scaling vector.
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         APCORE(JB) = FACT * APCORE(JA)
         JA = JA + I
         JB = JB + J
 100     CONTINUE
C
 999  RETURN
      END

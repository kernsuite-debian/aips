@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QVTSMU (A, I, B, C, K, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Vector table scalar multiply.
C# AP-appl
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
C   Pseudo-AP version
C   Vector Table Scalar multiply
C   IMPORTANT NOTE:   Currently only works properly for
C   Table Address (B) = 2327 (value = 0.5)
C   Also note: This routine is extremely hardware dependent - DON'T USE
C   Inputs:
C      A  I  Source vector base  address
C      I  I  A address increment
C      B  I  Table address (currently only 2327 works)
C      C  I  Destination vector base address
C      K  I  C address increment
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, B, C, K, N, LOOP, JA, JC
      REAL      FACTOR
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + 1
      JC = C + 1
      FACTOR = 1.0
      IF (B.EQ.2327) FACTOR = 0.5
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         APCORE (JC) = APCORE(JA) * FACTOR
         JA = JA + I
         JC = JC + K
 100     CONTINUE
C
 999  RETURN
      END

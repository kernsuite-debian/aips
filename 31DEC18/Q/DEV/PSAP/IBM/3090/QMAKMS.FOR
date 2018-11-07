@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QMAKMS (A, I, C, B, J, N, COUNT)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Make mask depending on vector, scalar comparison.
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
C   Makes a mask from a vector depending on a comparison
C   of the vector A and scalar C.
C             B(k) = 1.0  if A(k) < C
C             B(k) = 0.0  If A(k) >= C   k = 1,N
C  Inputs:
C     A      I  Source vector base address
C     I      I  Increment of A
C     C      I  Address of scalar.
C     B      I  Destination vector base address.
C     J      I  Increment of B
C     N      I  Element count
C     COUNT  I  Address of count of number of times A(k)>=C.
C-----------------------------------------------------------------------
      INTEGER   A, I, C, B, J, N, COUNT, JA, JC, JB, JN, JCNT, LOOP
      REAL      XCNT
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses to 1-rel.
      JA = A + 1
      JB = B + 1
      JC = C + 1
      JCNT = COUNT + 1
C                                        Initilize count
      XCNT = 0.0
C                                        Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         APCORE(JB) = 0.0
         IF (APCORE(JA).GT.APCORE(JC)) XCNT = XCNT + 1.0
C                                       Update pointers
         JA = JA + I
         JB = JB + J
 100     CONTINUE
C                                       Store count
      APCORE(JCNT) = XCNT
C
 999  RETURN
      END

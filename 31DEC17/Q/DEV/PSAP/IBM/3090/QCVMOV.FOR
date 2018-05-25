@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QCVMOV (A, I, C, K, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Complex vector move.
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
C   CVMOV copies one complex vector to another:
C     C(mK) = A(mI), C(mK+1) = A(mI+1)  for m=0 to N-1
C   Inputs:
C      A  I  Source vector base address.
C      I  I  Increment of A, should be at least 2.
C      C  I  Destination scalar address
C      K  I  Increment of C
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, C, K, N, JA, JC, L
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses to 1-rel.
      JA = A + 1
      JC = C + 1
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 L = 1,N
         APCORE (JC) = APCORE (JA)
         APCORE (JC+1) = APCORE (JA+1)
         JC = JC + K
         JA = JA + I
 100     CONTINUE
C
 999  RETURN
      END

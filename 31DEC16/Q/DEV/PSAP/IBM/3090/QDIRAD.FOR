@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QDIRAD (A, IA, B, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Directed vector add.
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
C   Does a complex directed add: adds a complex vector to a complex
C   vector whose addresses are given in the first vector.
C      B(A(mIA))   = B(A(mIA))  + A(mIA+1)   for m = 0,N-1
C      B(A(mIA)+1) = B(A(mIA)+1)+ A(mIA+2)
C   Inputs:
C      A  I  Source vector base address
C            0 => address (integer) to be added to
C                (address is zero relative)
C            1,2 => complex value (reals)
C      IA I  Increment for A (normally 3 * integer)
C      B  I  Destination vector base address
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, IA, B, N, JA, JB, LOOP, IADDR
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + 1
      JB = B + 1
C                                        Do loop
      DO 100 LOOP = 1,N
         IADDR = APCORI(JA) + JB
         APCORE(IADDR) = APCORE(IADDR) + APCORE(JA+1)
         APCORE(IADDR+1) = APCORE(IADDR+1) + APCORE(JA+2)
         JA = JA + IA
 100     CONTINUE
C
 999  RETURN
      END

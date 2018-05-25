      SUBROUTINE QDIRAD (A, IA, B, N)
C-----------------------------------------------------------------------
C! FPS AP version: Complex directed add
C# AP-util
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
C   FPS AP version
C   Does a complex directed add:
C     B(A(IA*J)) = B(A(IA*J))+A(IA*J+1)  for J = 0,N-1
C     B(A(IA*J)+1) = B(A(IA*J)+1)+A(IA*J+2)
C    Inputs:
C     A  I  Source vector base address
C             0 => address (integer) to be added to
C                  (address is zero relative)
C             1,2 => complex value (reals)
C     IA I  Increment for A
C     B  I  Destination vector base address
C     N  I  Element count
C-----------------------------------------------------------------------
      INTEGER A, IA, B, N
      INTEGER*2 JA, JIA, JB, JN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) JA = A
      IF (A.GE.32768) JA = A - 65536
      IF (IA.LT.32768) JIA = IA
      IF (IA.GE.32768) JIA = IA - 65536
      IF (B.LT.32768) JB = B
      IF (B.GE.32768) JB = B - 65536
      IF (N.LT.32768) JN = N
      IF (N.GE.32768) JN = N - 65536
C                                        Call FPS routine.
      CALL DIRADD (JA, JIA, JB, JN)
C
 999  RETURN
      END

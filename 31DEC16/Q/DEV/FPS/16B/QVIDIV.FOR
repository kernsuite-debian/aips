      SUBROUTINE QVIDIV (A, I, D1, D2, B, J, N)
C-----------------------------------------------------------------------
C! FPS AP version: Divide vector by product of two integers
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
C   Divides the given vector by the product of two integers:
C        B(K) = A(K) / (D1 * D2)  for K = 0,N-1
C    Inputs:
C      A   I  Source vector base address.
C      I   I  Increment for A
C      D1  I  First dividend.
C      D2  I  Second dividend.
C      B   I  Destination vector base address.
C      J   I  Increment for B
C      N   I  Element count.
C-----------------------------------------------------------------------
      INTEGER A, I, D1, D2, B, J, N
      INTEGER*2 IA, II, ID1, ID2, IB, IJ, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (D1.LT.32768) ID1 = D1
      IF (D1.GE.32768) ID1 = D1 - 65536
      IF (D2.LT.32768) ID2 = D2
      IF (D2.GE.32768) ID2 = D2 - 65536
      IF (B.LT.32768) IB = B
      IF (B.GE.32768) IB = B - 65536
      IF (J.LT.32768) IJ = J
      IF (J.GE.32768) IJ = J - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL VIDIV (IA, II, ID1, ID2, IB, IJ, IN)
C
 999  RETURN
      END

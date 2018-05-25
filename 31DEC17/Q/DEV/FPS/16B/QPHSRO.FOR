      SUBROUTINE QPHSRO (A, I, B, J, PHAS0, DELPHS, N)
C-----------------------------------------------------------------------
C! FPS AP version: Add phase gradient to complex vector.
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
C   Adds a phase gradient to a complex array:
C        B(j) = A(j)*EXP(-i*(PHAS0+j*DELPHS)) for j = 0,N-1
C    Inputs:
C     A       I  Source vector base address.
C     I       I  Increment of A
C     B       I  Destination base address.
C     J       I  Increment of B
C     PHAS0   I  Address of complex unit vector with
C                  phase PHAS0
C     DELPHS  I  Address of complex unit vector with
C                  phase DELPHS
C     N       I  Element count
C-----------------------------------------------------------------------
      INTEGER A, I, B, J, PHAS0, DELPHS, N
      INTEGER*2 IA, II, IB, IJ, IPHAS0, IDELPH, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (B.LT.32768) IB = B
      IF (B.GE.32768) IB = B - 65536
      IF (J.LT.32768) IJ = J
      IF (J.GE.32768) IJ = J - 65536
      IF (PHAS0.LT.32768) IPHAS0 = PHAS0
      IF (PHAS0.GE.32768) IPHAS0 = PHAS0 - 65536
      IF (DELPHS.LT.32768) IDELPH = DELPHS
      IF (DELPHS.GE.32768) IDELPH = DELPHS - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL PHSROT (IA, II, IB, IJ, IPHAS0, IDELPH, IN)
C
 999  RETURN
      END

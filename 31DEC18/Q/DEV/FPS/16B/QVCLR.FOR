      SUBROUTINE QVCLR (C, K, N)
C-----------------------------------------------------------------------
C! FPS AP version: Vector clear (zero fill)
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
C   Fill with zero routine:
C     C(mK) = 0.0  for m = 0 to N-1
C   Inputs:
C     C  I  Destination vector base address
C     K  I  C address increment
C     N  I  Element count
C-----------------------------------------------------------------------
      INTEGER C, K, N
      INTEGER*2 IC, IK, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (C.LT.32768) IC = C
      IF (C.GE.32768) IC = C - 65536
      IF (K.LT.32768) IK = K
      IF (K.GE.32768) IK = K - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL VCLR (IC, IK, IN)
C
 999  RETURN
      END

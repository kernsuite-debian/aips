      SUBROUTINE QBAKSU (B, D, E, CI, N)
C-----------------------------------------------------------------------
C! FPS AP version: Back substitution
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
C   Does the following loop:
C       DO K = N-2,1,-1
C          B(K) = (B(K)-D(K)*B(K+1)-E(K)*B(K+2))*CI(K)
C   Inputs:
C     B   I First vector base address (incr=1) also destination.
C     D   I Second vector base address (incr=1)
C     E   I Third vector base address (incr=1)
C     CI  I Fourth vector base address (incr=1)
C     N   I Element count
C-----------------------------------------------------------------------
      INTEGER B, D, E, CI, N
      INTEGER*2 IB, ID, IE, ICI, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (B.LT.32768) IB = B
      IF (B.GE.32768) IB = B - 65536
      IF (D.LT.32768) ID = D
      IF (D.GE.32768) ID = D - 65536
      IF (E.LT.32768) IE = E
      IF (E.GE.32768) IE = E - 65536
      IF (CI.LT.32768) ICI = CI
      IF (CI.GE.32768) ICI = CI - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL BAKSUB (IB, ID, IE, ICI, IN)
C
 999  RETURN
      END

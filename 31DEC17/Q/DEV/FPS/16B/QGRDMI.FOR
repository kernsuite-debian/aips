      SUBROUTINE QGRDMI (A, B, N)
C-----------------------------------------------------------------------
C! FPS AP version: Combines two complex vectors in gridding UV data
C# AP-util UV
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
C   Combines two complex vectors (A and B) to form conjugate
C   rows in a grid file to be FFTed; such that when the file is FFTed,
C   the resulting real portion is the Fourier transform of A and the
C   imaginary portion is the Fourier transform of B. (A and B assumed
C   to be symmetric).
C            A => A + iB
C            B => Conjg(A) + i * Conjg(B)
C    Inputs:
C     A   I  First complex vector base address (increment=2)
C     B   I  Second complex vector base address.
C     N   I  Element count.
C-----------------------------------------------------------------------
      INTEGER A, B, N
      INTEGER*2 IA, IB, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (B.LT.32768) IB = B
      IF (B.GE.32768) IB = B - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL GRDMIX (IA, IB, IN)
C
 999  RETURN
      END

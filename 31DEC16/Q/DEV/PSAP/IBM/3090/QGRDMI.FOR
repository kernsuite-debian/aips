@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QGRDMI (A, B, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Combined complex vector in gridding uv data.
C# AP-appl UV
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
C   QGRDMI combines two complex vectors (A and B) to form conjugate
C   rows in a grid file to be FFTed; such that when the file is FFTed,
C   the resulting real portion is the Fourier transform of A and the
C   imaginary portion is the Fourier transform of B. (A and B assumed
C   to be symmetric).
C            A => A + iB
C            B => Conjg(A) + i * Conjg(B)
C   Inputs:
C      A   I  First complex vector base address (increment=2)
C      B   I  Second complex vector base address.
C      N   I  Element count.
C-----------------------------------------------------------------------
      INTEGER   A, II, B, N, JA, JB
      REAL      AR, AI, BR, BI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses to 1-rel.
      JA = A + 1
      JB = B + 1
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 II = 1,N
         AR = APCORE(JA)
         AI = APCORE(JA+1)
         BR = APCORE(JB)
         BI = APCORE(JB+1)
         APCORE(JA) = AR - BI
         APCORE(JA+1) = AI + BR
         APCORE(JB) = AR + BI
         APCORE(JB+1) = BR - AI
         JA = JA + 2
         JB = JB + 2
 100     CONTINUE
C
 999  RETURN
      END

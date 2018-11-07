      SUBROUTINE QVTRAN (M, N, IAD, LV)
C-----------------------------------------------------------------------
C! FPS AP version: inplace transpose of matrix of vectors
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
C   Transposes a (row-stored) M X N array of row vectors
C   of length LV.  The starting address is given by IAD.  The
C   algorithm works in place.  It is adapted from Boothroyd's
C   CACM ALG.#302.  Other, probably better, algorithms, are
C   CACM #'S 380 and 467, but they're not as simple to program.
C    Inputs:
C      M   I  First dimension of the vector array
C      N   I  Second dimension of the vector array
C      IAD I  Base address of the array
C      LV  I  Length of the vectors.
C-----------------------------------------------------------------------
      INTEGER M, N, IAD, LV
      INTEGER*2 IM, IN, IIAD, ILV
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
      IF (IAD.LT.32768) IIAD = IAD
      IF (IAD.GE.32768) IIAD = IAD - 65536
      IF (LV.LT.32768) ILV = LV
      IF (LV.GE.32768) ILV = LV - 65536
C                                        Call FPS routine.
      CALL VTRANS (IM, IN, IIAD, ILV)
C
 999  RETURN
      END

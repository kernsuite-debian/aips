      SUBROUTINE QMTRAN (A, I, C, K, MC, NC)
C-----------------------------------------------------------------------
C! FPS AP version: Transpose a matrix
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
C   Transpose a matrix, doesn't work inplace.
C     C((p+qMC)K) = A((q+pNC)I)
C                 p = 0 to MC-1
C                 q = 0 to NC-1
C    Inputs:
C      A  I  Source matrix base address
C      I  I  A address increment
C      C  I  Destination  matrix base address
C      K  I  C address increment
C      MC I  Number of columns of A
C      NC I  Numbers of rows of A
C-----------------------------------------------------------------------
      INTEGER A, I, C, K, MC, NC
      INTEGER*2 IA, II, IC, IK, IMC, INC
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (C.LT.32768) IC = C
      IF (C.GE.32768) IC = C - 65536
      IF (K.LT.32768) IK = K
      IF (K.GE.32768) IK = K - 65536
      IF (MC.LT.32768) IMC = MC
      IF (MC.GE.32768) IMC = MC - 65536
      IF (NC.LT.32768) INC = NC
      IF (NC.GE.32768) INC = NC - 65536
C                                        Call FPS routine.
      CALL MTRANS (IA, II, IC, IK, IMC, INC)
C
 999  RETURN
      END

      SUBROUTINE QMTRAN (A, I, C, K, MC, NC)
C-----------------------------------------------------------------------
C! Pseudo AP routine: matrix transpose.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Pseudo-AP version
C   Transpose a matrix, doesn't work inplace.
C     C((p+qMC)K) = A((q+pNC)I)
C                 p = 0 to MC-1
C                 q = 0 to NC-1
C   Inputs:
C      A  I  Source matrix base address
C      I  I  A address increment
C      C  I  Destination  matrix base address
C      K  I  C address increment
C      MC I  Number of columns of A
C      NC I  Numbers of rows of A
C-----------------------------------------------------------------------
      INTEGER   A, I, C, K, MC, NC
C
      INTEGER   IM, IN, DJC
      LONGINT   JA, JC, JC1, JA1
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Addresses to 1-rel
      JA = A + PSAPOF
      JC = C + PSAPOF
      DJC = K * MC
C                                       Loop transposing.
      INCLUDE 'INCS:ZVND.INC'
      DO 200 IM = 1,MC
         JC1 = JC + K * (IM - 1)
         JA1 = JA + I * NC * (IM - 1)
      INCLUDE 'INCS:ZVND.INC'
         DO 100 IN = 1,NC
            APCORE(JC1) = APCORE(JA1)
            JC1 = JC1 + DJC
            JA1 = JA1 + I
 100        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END

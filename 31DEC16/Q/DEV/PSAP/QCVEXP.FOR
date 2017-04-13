      SUBROUTINE QCVEXP (A, I, C, K, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Vector complex exponential.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Complex exponential of a vector.
C      C(mK)   = COS (A(mI))         for m = 0 to N-1
C      C(mK+1) = SIN (A(mI))
C   Inputs:
C      A   I  Source vector base address.
C      I   I  Increment of A
C      C   I  Destination vector base address
C      K   I  Increment of C (normally 2 * integer)
C      N   I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, C, K, N
C
      LONGINT   JA, JC
      INTEGER   II
      DOUBLE PRECISION XX
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Save addresses (1-rel)
      JA = A + PSAPOF
      JC = C + PSAPOF
C                                       Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 10 II = 1,N
         XX = APCORE(JA)
         APCORE(JC) = COS (XX)
         APCORE(JC+1) = SIN (XX)
         JA = JA + I
         JC = JC + K
 10      CONTINUE
C
 999  RETURN
      END

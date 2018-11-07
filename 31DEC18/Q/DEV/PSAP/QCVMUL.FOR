      SUBROUTINE QCVMUL (A, I, B, J, C, K, N, F)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Complex vector multiply.
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
C   Complex vector multiply:
C      (C(mK)+iC(mK+1)) = (B(mJ)+iB(mJ+1)*(A(mI)+iA(mI+1)) if F=1
C      (C(mK)+iC(mK+1)) = (B(mJ)+iB(mJ+1)*(A(mI)-iA(mI+1)) if F=-1
C      C(mK)    = A(mI) * B(mJ)   - F * A(mI+1) * B(mJ+1)
C      C(mK+1)) = A(mI) * B(mj+1) + F * A(mI+1) * B(mJ)
C                                            for  m = 0 to N-1
C   Inputs:
C      A  I  Source vector base address
C      I  I  A address increment (normally 2)
C      B  I  Source vector base address
C      J  I  B address increment (normally 2)
C      C  I  Destination vector base address
C      K  I  C address increment (normally 2)
C      N  I  Element count
C      F  I  Conjugate flag, 1 => normal complex multiply
C                             -1 => multiply with conj of A
C-----------------------------------------------------------------------
      INTEGER   A, I, B, J, C, K, N, F
C
      LONGINT   JA, JB, JC
      INTEGER   LOOP
      DOUBLE PRECISION TEMP, TEMPR, TEMPI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + PSAPOF
      JB = B + PSAPOF
      JC = C + PSAPOF
C                                       Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         TEMP = F * APCORE(JA+1)
         TEMPR = APCORE(JA)*APCORE(JB) - TEMP*APCORE(JB+1)
         TEMPI = APCORE(JA)*APCORE(JB+1) + TEMP*APCORE(JB)
         APCORE(JC) = TEMPR
         APCORE(JC+1) = TEMPI
         JA = JA + I
         JB = JB + J
         JC = JC + K
 100     CONTINUE
C
 999  RETURN
      END

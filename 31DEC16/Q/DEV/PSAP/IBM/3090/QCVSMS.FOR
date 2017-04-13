@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QCVSMS (A, I, B, C, J, D, K, N ,FLAG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Subtract real vector*complex scalar from vector.
C# AP-appl
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
C   Pseudo-AP version.
C   CVSMS subtracts a real vector times a complex scalar from
C   a complex vector, alternately i (SQRT(-1)) times the real
C   vector times the complex scalar is subtracted from the complex
C   vector. Since the element count is expected to be small the looping
C   is not very efficient.
C   If FLAG > 0
C       D(mK)   = A(mI)   - B(1) * C(mJ)
C       D(mK+1) = A(mI+1) - B(2) * C(mJ)  for m=0, N-1
C   If FLAG < 0
C       D(mK)   = A(mI)   + B(2) * C(mJ)
C       D(mK+1) = A(mI+1) - B(1) * C(mJ)  for m=0, N-1
C   Inputs:
C      A    I  Source complex vector base address.
C      I    I  Increment of A (normally 2)
C      B    I  Source  complex scalar address.
C      C    I  Source real vector base address
C      J    I  Increment of C
C      D    I  Destination complex vector base address
C      K    I  Increment of D (normally 2)
C      N    I  Element count
C      FLAG I  Flag, if < 0 multiply complex scalar by i
C-----------------------------------------------------------------------
      INTEGER   A, I, B, J, C, D, K, N, FLAG,  JA, JB, JC, JD, LOOP
      REAL      E, F
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + 1
      JB = B + 1
      JC = C + 1
      JD = D + 1
C                                       Setup
C                                       Check FLAG
      IF (FLAG) 10, 10, 20
C                                       Multiple B times i,
C                                       E = REAL(iB), F = IMAG(iB)
 10      E = - APCORE(JB+1)
         F = APCORE(JB)
         GO TO 30
C                                       E = REAL(B), F = IMAG(B)
 20      E = APCORE(JB)
         F = APCORE(JB+1)
C                                       Loop
 30   CONTINUE
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         APCORE(JD) = APCORE(JA) - E * APCORE(JC)
         APCORE(JD+1) = APCORE(JA+1) - F * APCORE(JC)
         JA = JA + I
         JC = JC + J
         JD = JD + K
 100     CONTINUE
C
 999  RETURN
      END

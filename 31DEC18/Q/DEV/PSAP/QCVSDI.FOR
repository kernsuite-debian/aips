      SUBROUTINE QCVSDI (A, I, B, C, J, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Divide weighted complex vector by complex scalar.
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
C   Pseudo-AP version.
C   CVSDIV divides a Complex vector with weights by a complex
C   scalar.  The complex vector is expected to have data in
C   the order real, imaginary, weight.  The weight is multiplied
C   by the amplitude of the complex scalar.
C     C(mJ) = (1./(B(1)**2+B(2)**2))*(A(mI)*B(1)+A(mI+1)*B(2))
C     C(mJ+1) = (1./B(1)**2+B(2)**2))*(A(mI+1)*B(1)-A(mI)*B(2))
C     C(mJ+2) = A(mI+2) * SQRT(B(1)**2+B(2)**2)  for m = 0, N-1
C   Inputs:
C      A  I  Source vector base address.
C      I  I  Increment of A
C      B  I  Source scalar address.
C      C  I  Destination vector base address
C      J  I  Increment of C
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, B, C, J, N
C
      LONGINT   JA, JB, JC
      INTEGER   LOOP
      DOUBLE PRECISION WT, TEMP, E, F
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + PSAPOF
      JB = B + PSAPOF
      JC = C + PSAPOF
C                                        Setup
      WT = (APCORE(JB) * APCORE(JB) + APCORE(JB+1) * APCORE(JB+1))
      WT = MAX (1.0D-15, WT)
      E = APCORE(JB) / WT
      F = APCORE(JB+1) / WT
      WT = SQRT (WT)
C                                        Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         TEMP = E * APCORE(JA) + F * APCORE(JA+1)
         APCORE(JC+1) = E * APCORE(JA+1) - F * APCORE(JA)
         APCORE(JC) = TEMP
         APCORE(JC+2) = APCORE(JA+2) * WT
         JA = JA + I
         JC = JC + J
 100     CONTINUE
C
 999  RETURN
      END

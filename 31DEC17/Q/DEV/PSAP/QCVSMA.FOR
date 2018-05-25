      SUBROUTINE QCVSMA (A, I, B, C, K, D, L, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Complex vector scalar multiply and vector add.
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
C   Multiply the elements of a complex vector by a complex scalar and
C   adds to the elements of another complex vector:
C        D(mL) = (A(mI) * B) + C(mK)    for m = 0, N-1
C   Inputs:
C      A  I  First source complex vector base address
C      I  I  A address increment
C      B  I  Source multiplicative complex scalar address
C      C  I  Second source complex vector base address
C      K  I  C address increment
C      D  I  Destination complex vector base address
C      L  I  D address increment
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, B, C, K, D, L, N
C
      LONGINT   JA, JC, JD
      INTEGER   LOOP
      DOUBLE PRECISION RE, IM, TR, TI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + PSAPOF
      JC = C + PSAPOF
      JD = D + PSAPOF
      RE = APCORE(B+PSAPOF)
      IM = APCORE(B+PSAPOF+1)
C                                       Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         TR = RE * APCORE(JA) - IM * APCORE(JA+1)
         TI = RE * APCORE(JA+1) + IM * APCORE(JA)
         APCORE(JD) = APCORE(JC) + TR
         APCORE(JD+1) = APCORE(JC+1) + TI
         JA = JA + I
         JC = JC + K
         JD = JD + L
 100     CONTINUE
C
 999  RETURN
      END

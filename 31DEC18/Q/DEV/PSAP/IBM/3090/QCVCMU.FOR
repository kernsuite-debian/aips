@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QCVCMU (A, I, B, C, J, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Scalar complex times conjugate of vector to real.
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
C   Pseudo-AP version
C   QCVCMU multiplies a scalar complex value times the complex
C   conjugate of a vector producing a real vector:
C        C(mJ) = B(0) * A(mI) + B(0+1) * A(mI+1)   for m = 0 to N-1
C   Inputs:
C      A   I  Source complex vector base address.
C      I   I  Increment of A (normally 2 * integer)
C      B   I  Address of scalar (real part)
C      C   I  Destination real vector base address.
C      J   I  Increment of C
C      N   I  Element count (reals)
C-----------------------------------------------------------------------
      INTEGER   A, I, B, C, J, N, JA, JB, JC, LOOP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Addresses to 1-rel.
      JA = A + 1
      JB = B + 1
      JC = C + 1
C                                        Do loop.
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         APCORE(JC) = APCORE(JB) * APCORE(JA) + APCORE(JB+1) *
     *      APCORE(JA+1)
         JC = JC + J
         JA = JA + I
 100     CONTINUE
C
 999  RETURN
      END

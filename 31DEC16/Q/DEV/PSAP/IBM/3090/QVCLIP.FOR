@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QVCLIP (A, I, B, C, D, L, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Vector clip.
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
C   Limits the values in a vector to a specified range:
C     D(mL) = B     if A(mI) < B
C           = A(mI) if B <= A(mI) < C
C           = C     if C <= A(mI)    for m = 0 to N-1
C   Inputs:
C      A  I  First source vector base address
C      I  I  A address increment
C      B  I  Address of lower limit
C      C  I  Address of upper limit
C      D  I  Destination vector base address
C      L  I  D address increment
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, II, B, C, D, L, N, JA, JD
      REAL      VMAX, VMIN, VX
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + 1
      JD = D + 1
C                                       Get limits
      VMIN = APCORE(B+1)
      VMAX = APCORE(C+1)
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 II = 1,N
         VX = APCORE(JA)
         IF (VX.GE.VMIN) GO TO 50
            APCORE(JD) = VMIN
            GO TO 90
 50      IF (VX.LE.VMAX) GO TO 60
            APCORE(JD) = VMAX
            GO TO 90
C                                       Save result
 60      APCORE(JD) = VX
C                                       Update pointers
 90      JA = JA + I
         JD = JD + L
 100     CONTINUE
C
 999  RETURN
      END

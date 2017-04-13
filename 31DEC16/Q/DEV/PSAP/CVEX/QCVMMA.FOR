      SUBROUTINE QCVMMA (A, I, C, N)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Find max. square modulus of complex vector.
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
C   Vector version.
C   Find the maximum of the square modulus of a complex vector.
C      C = max (A(mI)**2 + A(mI+1)**2)      over m = 0 to N-1
C   Inputs:
C      A  I  Source vector base address
C      I  I  Increment of A (normally 2 * integer)
C      C  I  Destination vector.
C             0 = MAX(A ** 2) (real)
C             1  = location of max  (integer)
C      N  I  Element count
C   Also:
C            SPAD(15) = index of max.
C-----------------------------------------------------------------------
      INTEGER   A, II, I, C, N, JA, JC, JI, MAXNDX
      REAL      VMAX, AMPL
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JC = C + 1
      JA = A + 1
C                                       Init. max.
      VMAX = APCORE(JA)*APCORE(JA) + APCORE(JA+1)*APCORE(JA+1)
      JI = JA
      MAXNDX = 1
C                                       Find max.
      INCLUDE 'INCS:ZVND.INC'
      DO 100 II = 2,N
         AMPL = APCORE(JA)*APCORE(JA) + APCORE(JA+1)*APCORE(JA+1)
         VMAX = MAX (VMAX, AMPL)
         JA = JA + I
 100     CONTINUE
C                                       Find location of max
      JA = A + 1
      INCLUDE 'INCS:ZVND.INC'
      DO 200 II = 1,N
         AMPL = APCORE(JA)*APCORE(JA) + APCORE(JA+1)*APCORE(JA+1)
         IF (AMPL.GE.VMAX) THEN
            JI = JA
            MAXNDX = II
            END IF
         JA = JA + I
 200     CONTINUE
      APCORE(JC) = VMAX
      APCORI(JC+1) = JI-1
      SPAD(15) = MAXNDX
C
 999  RETURN
      END

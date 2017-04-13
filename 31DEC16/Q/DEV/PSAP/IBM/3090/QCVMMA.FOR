@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QCVMMA (A, I, C, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Max. square of modulus of complex vector.
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
C   Find the maximum of the square modulus of a complex vector.
C      max (A(mI)**2 + A(mI+1)**2)      over m = 0 to N-1
C   Inputs:
C      A  I  Source vector base address
C      I  I  Increment of A (normally 2 * integer)
C      C  I  Destination vector.
C               0 = MAX(A ** 2) (real)
C               1  = location of max  (integer)
C      N  I  Element count
C   Also:
C            SPAD(15) = index of max.
C-----------------------------------------------------------------------
      INTEGER   A, II, I, C, N, JA, JC, JI, ONE
      REAL      VMAX, AMPL(1024)
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF (N.GT.1024) THEN
         WRITE(*,*) '**************** ERROR *****************'
         WRITE(*,*) 'ARRAY AMPL IN QCVMMA TOO SMALL.  N =',N
         GOTO 999
      END IF
C                                       Addresses 1-rel
      JC = C + 1
      JA = A + 1
C                                       Init. max.
       VMAX = APCORE(JA)
       JI = JA
C                                       Find max.
C                                       Note: contains a dependency.
C      DO 100 II = 1, N
C         AMPL = APCORE(JA)*APCORE(JA) + APCORE(JA+1)*APCORE(JA+1)
C         IF (AMPL.LE.VMAX) GO TO 90
C            VMAX = AMPL
C            JI = JA
C            SPAD(15) = II
C 90      JA = JA + I
C 100     CONTINUE
      DO 10 II = 1, N
         AMPL(II) = APCORE(JA)**2 + APCORE(JA+1)**2
         JA = JA + I
 10      CONTINUE
C
      IMAX = ISMAX(N,AMPL,1)
      IF(AMPL(IMAX).GT.VMAX) THEN
        VMAX = AMPL(IMAX)
        JI = JI + (IMAX - 1) * I
        SPAD(15) = IMAX
      ENDIF
      APCORE(JC) = VMAX
      APCORI(JC+1) = JI - 1
C
 999  RETURN
      END

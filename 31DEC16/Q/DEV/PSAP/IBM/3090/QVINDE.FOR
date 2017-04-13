@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QVINDE (A, B, J, C, K, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Vector index (gather)
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
C   Vector gather:
C        C(mK) = A(r)  where r = fix(B(mJ))
C                      for m = 0,N-1
C   Inputs:
C      A  I  Source vector base address
C      B  I  Index vector base address.
C      J  I  A address increment
C      C  I  Destination vector base address
C      K  I  C address increment
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, B, J, C, K, N, JA, JB, JC, LOOP, ITEMP
      REAL      VALUE, TEMP(1024)
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF (N.GT.1024) THEN
         WRITE(*,*) '******ERROR in QVINDE *****'
         WRITE(*,*) 'N = ', N
         GOTO 999
      ENDIF
C                                       Addresses 1-rel
      JA = A + 1
      JB = B + 1
      JC = C + 1
      JBP = JB
      DO 50 LOOP = 1,N
         TEMP(LOOP) = SIGN(0.5, APCORE(JBP))
         JBP = JBP + J
 50      CONTINUE
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         ITEMP = APCORE(JB) + TEMP(LOOP)
         APCORE(JC) = APCORE(JA+ITEMP)
         JB = JB + J
         JC = JC + K
 100     CONTINUE
C
 999  RETURN
      END

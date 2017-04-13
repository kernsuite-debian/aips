@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QMAXV (A, I, C, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Find maximum value element of a vector.
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
C   Find maximum value of a vector and address of the maximum.
C   Inputs:
C      A  I  Source vector base address
C      I  I  A address increment
C      C  I  Destination  base address
C            C(0) = Max (A(mI))  m = 0 to N-1
C            C(1) = address. also in SPAD 15.
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, C, N, JA, JC, JI, II, IMAX
      REAL      VMAX
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + 1
      JC = C + 1
C                                       Find maximum.
      VMAX = APCORE(JA)
      JI = JA
CC@DIR IGNORE RECRDEPS(APCORE)
C     DO 100 II = 1,N
C        IF (APCORE(JA).GT.VMAX) THEN
C           VMAX = APCORE(JA)
C           JI = JA
C        ENDIF
C        JA = JA + I
C100     CONTINUE
      IF(I.LT.0) JA = JA + I * (N - 1)
      IMAX = (ISMAX(N,APCORE(JA),I) - 1) * ABS(I) + JA
      IF(APCORE(IMAX).GT.VMAX) THEN
        VMAX = APCORE(IMAX)
        JI = IMAX
      ENDIF
      SPAD(15) = JI-1
      APCORE(JC) = VMAX
      APCORI(JC+1) = SPAD(15)
C
 999  RETURN
      END

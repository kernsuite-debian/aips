@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QMAXMI (A,I,MAX,MIN,N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Find maximum and minimum of a vector.
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
C   QMAXMI searches the given vector for maximum and minimum values.
C   Inputs:
C      A    I  Source vector base address
C      I    I  Increment of A
C      MAX  I  Location for maximum.
C      MIN  I  Location for minimum.
C      N    I  Element count.
C-----------------------------------------------------------------------
      INTEGER   A, I, MAX, MIN, N, JA, JMAX, JMIN, LOOP
      REAL      XMAX, XMIN
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses to 1-rel.
      JA = A + 1
      JMAX = MAX + 1
      JMIN = MIN + 1
C                                       Init. extrema.
      XMAX = -1.0E20
      XMIN =  1.0E20
C                                       Loop finding extrema.
CC@DIR IGNORE RECRDEPS(APCORE)
C     DO 100 LOOP = 1,N
C        IF (XMAX.LT.APCORE(JA)) XMAX = APCORE(JA)
C        IF (XMIN.GT.APCORE(JA)) XMIN = APCORE(JA)
C        JA = JA + I
C100     CONTINUE
      II = ABS(I)
      IF(I.LT.0)  JA = JA + (N - 1) * I
      IMAX = (ISMAX(N,APCORE(JA),I) - 1) * II + JA
      IMIN = (ISMIN(N,APCORE(JA),I) - 1) * II + JA
      IF(XMAX.LT.APCORE(IMAX)) XMAX = APCORE(IMAX)
      IF(XMIN.LT.APCORE(IMIN)) XMIN = APCORE(IMIN)
C                                        Save extrema.
      APCORE(JMAX) = XMAX
      APCORE(JMIN) = XMIN
C
 999  RETURN
      END

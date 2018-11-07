      SUBROUTINE QMAXMI (A, I, IMAX, IMIN, N)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Finds max. and min. value elements.
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
C   Vector version:
C   QMAXMI searches the given vector for maximum and minimum values.
C   Inputs:
C      A    I  Source vector base address
C      I    I  Increment of A
C      IMAX I  Location to store maximum in.
C      IMIN I  Location to store minimum in.
C      N    I  Element count.
C-----------------------------------------------------------------------
      INTEGER   A, I, IMAX, IMIN, N, JA, JMAX, JMIN, LOOP
      REAL      XMAX, XMIN
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses to 1-rel.
      JA = A + 1
      JMAX = IMAX + 1
      JMIN = IMIN + 1
C                                       Init. extrema.
      XMAX = -1.0E20
      XMIN =  1.0E20
C                                       Loop finding maximum.
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         XMAX = MAX (XMAX, APCORE(JA))
         JA = JA + I
 100     CONTINUE
C                                       Loop finding minimum.
      JA = A + 1
      INCLUDE 'INCS:ZVND.INC'
      DO 200 LOOP = 1,N
         XMIN = MIN (XMIN, APCORE(JA))
         JA = JA + I
 200     CONTINUE
C                                        Save extrema.
      APCORE(JMAX) = XMAX
      APCORE(JMIN) = XMIN
C
 999  RETURN
      END

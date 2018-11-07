      SUBROUTINE PLRANG (X, N, FACTOR, LEAST, XMIN, XMAX)
C-----------------------------------------------------------------------
C! determine range of values to be plotted
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2003, 2007
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
C   Find the min. and max. axis range for an array to be plotted
C   Inputs:
C      X        R(*)   Array of data values to be plotted
C      N        I      Dimension of X
C      FACTOR   R      Factor to expand the actual range
C      LEAST    R      Least allowed difference between max and min
C                      before application of FACTOR
C   Outputs:
C      XMIN     R      Minimum axis value
C      XMAX     R      Maximum axis value
C   If actual range = Xmn to Xmx then
C      XMIN  =  Xmn - (FACTOR - 1.0) * (Xmx - Xmn) / 2
C      XMIN  =  Xmx + (FACTOR - 1.0) * (Xmx - Xmn) / 2
C----------------------------------------------------------------------
      INTEGER   N
      REAL      X(N), FACTOR, LEAST, XMIN, XMAX
C
      INTEGER   I
      REAL      SCL, TMP
C----------------------------------------------------------------------
      XMIN = 0.0
      XMAX = 0.0
      IF ((N.LE.0) .OR. (FACTOR.EQ.0.0)) GO TO 999
      IF (ABS(FACTOR).GE.1.0) THEN
         SCL = (ABS(FACTOR) - 1.0) / 2.0
      ELSE
         SCL = ABS(FACTOR) / 2.0
         END IF
C                                       Loop through data
      XMIN = X(1)
      XMAX = X(1)
      DO 50 I = 2,N
         XMIN = MIN (XMIN, X(I))
         XMAX = MAX (XMAX, X(I))
50       CONTINUE
C                                       Scale actual min., max.
      IF (XMAX-XMIN.LT.LEAST) THEN
         TMP = LEAST - (XMAX-XMIN)
         XMAX = XMAX + TMP / 2.0
         XMIN = XMIN - TMP / 2.0
         END IF
      SCL = SCL * (XMAX - XMIN)
      XMIN = XMIN - SCL
      XMAX = XMAX + SCL
C                                       Exit
999   RETURN
      END

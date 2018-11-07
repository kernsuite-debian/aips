      SUBROUTINE FMAX (A, NX, NY, ISGN, IX, S)
C-----------------------------------------------------------------------
C! returns position and value of maximum in a two-dimensional array
C#
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
C   Find the position and value of the maximum in an array
C   Inputs:
C      A      R(16,16)  Input array
C      NX     I         X-dimension of A
C      NY     I         Y-dimension of A
C      ISGN   I         If >0 look for max of A
C                       If <= 0 look for max of ABS(A)
C   Outputs:
C      IX     I(2)      X and y coordinates of max
C      S      R         Value of max
C-----------------------------------------------------------------------
      INTEGER    NX, NY, ISGN, IX(2)
      REAL       A(16,16), S
C
      INTEGER    I, J, X, Y
      REAL       TEMP, STEMP, SMAX
      LOGICAL    SABS
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SABS = ISGN.LE.0
      X = 0
      Y = 0
      SMAX = -1.E10
      DO 20 I = 1,NX
         DO 10 J = 1,NY
            TEMP = A(I,J)
            IF (TEMP.EQ.FBLANK) GO TO 10
               STEMP = TEMP
               IF (SABS) TEMP = ABS (TEMP)
               IF (TEMP.LE.SMAX) GO TO 10
                  SMAX = TEMP
                  X = I
                  Y = J
                  S = STEMP
 10         CONTINUE
 20      CONTINUE
      IX(1) = X
      IX(2) = Y
C
 999  RETURN
      END

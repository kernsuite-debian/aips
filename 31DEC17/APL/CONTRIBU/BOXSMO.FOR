      SUBROUTINE BOXSMO (WIDTH, X, Y, N, YS)
C-----------------------------------------------------------------------
C! Does boxcar smoothing of an irregularly spaced array.
C# Math
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
C   BOXSMO does a box car (running mean) smoothing of irregularly
C   spaced points.
C    Inputs:
C      WIDTH      R    Width of boxcar in same units as X
C      X(N)       R    Absissae of points to be smoothed.
C      Y(N)       R    Values to be smoothed.
C      N          I    Number of points to smooth.
C    Output:
C      YS(N)      R    Smoothed values.
C-----------------------------------------------------------------------
      INTEGER   N, FIRST, COUNT, I, CURRNT, LIMIT
      REAL      WIDTH, X(N), Y(N), YS(N), XLAST, XFIRST, SUM, HWIDTH,
     *   AVG
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF (WIDTH.LE.0.0) GO TO 900
C                                       Initialize.
      FIRST = 1
      CURRNT = 0
      XLAST = X(1) + WIDTH
      HWIDTH = WIDTH * 0.5
      SUM = 0.0
      COUNT = 0
C                                       Average window at start.
      I = 0
 50      I = I + 1
         IF (I.GT.N) GO TO 60
         IF (X(I).GT.XLAST) GO TO 60
            SUM = SUM + Y(I)
            COUNT = COUNT + 1
            GO TO 50
C                                       Finished sum over window.
 60   AVG = SUM / MAX (1, COUNT)
C                                       Fill in average value.
      XLAST = X(1) + HWIDTH
 70      CURRNT = CURRNT + 1
C                                       Check if finished
         IF (CURRNT.GT.N) GO TO 999
C                                       Check if within 1/2 width
C                                       of start.
         IF (X(CURRNT).GT.XLAST) GO TO 110
            YS(CURRNT) = AVG
            GO TO 70
C                                       Begin main loop.
 100     CURRNT = CURRNT + 1
         IF (CURRNT.GT.N) GO TO 999
 110     XFIRST = X(CURRNT) - HWIDTH
         XLAST = X(CURRNT) + HWIDTH
C                                       Check if running into window at
C                                       the end.
         IF (XLAST .GE. X(N)) GO TO 200
C                                       Init sum.
         SUM = 0.0
         COUNT = 0
         LIMIT = FIRST
         DO 150 I = LIMIT,N
C                                       Check if point too early.
            IF (X(I).GE.XFIRST) GO TO 120
C                                       Too early, reset FIRST.
               FIRST = I + 1
               GO TO 150
C                                       Check if too late.
 120        IF (X(I).GT.XLAST) GO TO 160
C                                       Sum.
               SUM = SUM + Y(I)
               COUNT = COUNT + 1
 150           CONTINUE
C                                       Average window.
 160     YS(CURRNT) = SUM / MAX (1, COUNT)
         GO TO 100
C                                       Do end window.
 200  SUM = 0.0
      COUNT = 0
      XFIRST = X(N)- WIDTH
C                                       Sum over end window.
      DO 210 I = FIRST,N
         IF (X(I).LT.XFIRST) GO TO 210
            SUM = SUM + Y(I)
            COUNT = COUNT + 1
 210        CONTINUE
      AVG = SUM / MAX (1, COUNT)
      DO 250 I = CURRNT,N
         YS(I) = AVG
 250     CONTINUE
      GO TO 999
C                                       WIDTH .le. 0, copy.
 900  CALL RCOPY (N, Y, YS)
C
 999  RETURN
      END

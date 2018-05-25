      SUBROUTINE BOXBSM (WIDTH, X, Y, S, BLANK, N, YS)
C-----------------------------------------------------------------------
C! smoothing by boxcar of irregularly spaced array with blanking
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   Does a box-car smoothing of irregularly spaced points, replacing
C   blanked and good values with the smoothed values.  Only returns
C   blanked values if no valid data found within +-WIDTH/2.  Does not
C   smooth different source numbers together.  Therefore, make S(*)
C   constant if you wish to smooth all samples.
C   Inputs:
C      WIDTH    R      Full width of window in same units as X
C      X        R(N)   Absissae of points to be smoothed - monotonically
C                      increasing
C      Y        R(N)   Values to be smoothed.
C      S        I(N)   Source numbers - do not smooth across numbers
C                      If S(1) < 0, then S(I) is not otherwise examined
C      BLANK    R      Value used for blanking.
C      N        I      Number of points to smooth.
C    Output:
C      YS       R(N)   Smoothed values.
C-----------------------------------------------------------------------
      INTEGER   N, S(N)
      REAL      WIDTH, X(N), Y(N), YS(N), BLANK
C
      INTEGER   I, J, J1, J2
      REAL      HWIDTH, WTS, V
      LOGICAL   NOSORC
C-----------------------------------------------------------------------
C                                       copy if width <= 0
      IF (WIDTH.LE.0.0) THEN
         CALL RCOPY (N, Y, YS)
      ELSE
         NOSORC = S(1).LT.0
         HWIDTH = WIDTH / 2.0
         J2 = 1
         DO 100 I = 1,N
            J1 = J2
            J2 = N + 1
            WTS = 0.0
            V   = 0.0
C                                       source not examined
            IF (NOSORC) THEN
               DO 10 J = J1,N
                  IF (X(J)-X(I).GT.HWIDTH) GO TO 90
                  IF (ABS(X(I)-X(J)).LE.HWIDTH) THEN
                     J2 = MIN (J2, J)
                     IF (Y(J).NE.BLANK) THEN
                        WTS = WTS + 1.0
                        V = V + Y(J)
                        END IF
                     END IF
 10               CONTINUE
C                                       source examined
            ELSE
               DO 20 J = J1,N
                  IF (X(J)-X(I).GT.HWIDTH) GO TO 90
                  IF (ABS(X(I)-X(J)).LE.HWIDTH) THEN
                     J2 = MIN (J2, J)
                     IF ((Y(J).NE.BLANK) .AND. (S(I).EQ.S(J))) THEN
                        WTS = WTS + 1.0
                        V = V + Y(J)
                        END IF
                     END IF
 20               CONTINUE
               END IF
 90         IF (WTS.GT.0.0) THEN
               YS(I) = V / WTS
            ELSE
               YS(I) = BLANK
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
      END

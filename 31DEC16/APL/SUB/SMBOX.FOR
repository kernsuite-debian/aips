      SUBROUTINE SMBOX (WIDTH, X, Y, W, N, YS, WS)
C-----------------------------------------------------------------------
C! Box car smoothing of an irregularly spaced array with blanking
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   SMBOX does a box car (running mean) smoothing of weighted
C   irregularly spaced points possibly with blanked values.  Only
C   returns blanked values if no valid data found.  First good value
C   used for all previous points, last good value used for all
C   subsequent points in which all data are blanked in the boxcar.  A
C   datum is blanked if its weight is <= 0 or FBLANK.
C   Inputs:
C      WIDTH   R      Width of boxcar in same units as X: 0 => replace
C                     blanks with interpolated closest 2, < 0 => replace
C                     only blanks with the b oxcar smoothed values (all
C                     others remain unchanged)
C      X       R(N)   Absissae of points to be smoothed in increasing
C                     order
C      Y       R(N)   Values to be smoothed.
C      W       R(N)   Weights of data.
C      N       I      Number of points to smooth.
C   Output:
C      YS      R(N)   Smoothed values.
C      WS      R(N)   Smoothed weights
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      WIDTH, X(N), Y(N), W(N), YS(N), WS(N)
C
      INTEGER   I , J, K, I1, I2
      REAL      HW, D, TEMP
      LOGICAL   WASB, ONLYB, BLNKD
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      D = ABS (WIDTH)
      ONLYB = WIDTH.LE.0.0
      HW = D / 2.0
      WASB = .FALSE.
C                                       0.0 => interp only
      IF (D.LE.0.0) THEN
         DO 10 I = 1,N
            BLNKD = (Y(I).EQ.FBLANK) .OR. (W(I).LE.0.0) .OR.
     *         (W(I).EQ.FBLANK)
            IF (BLNKD) THEN
               YS(I) = 0.0
               WS(I) = 0.0
               WASB = .TRUE.
            ELSE
               YS(I) = Y(I)
               WS(I) = W(I)
               END IF
 10         CONTINUE
C                                       Smooth
      ELSE
         DO 100 I = 1,N
            BLNKD = (Y(I).EQ.FBLANK) .OR. (W(I).LE.0.0) .OR.
     *         (W(I).EQ.FBLANK)
            IF (BLNKD) THEN
               YS(I) = 0.0
               WS(I) = 0.0
            ELSE
               YS(I) = Y(I) * W(I)
               WS(I) = W(I)
               END IF
            IF ((BLNKD) .OR. (.NOT.ONLYB)) THEN
               DO 20 K = 1,I-1
                  J = I - K
                  IF (ABS(X(I)-X(J)).GT.HW) THEN
                     GO TO 25
                  ELSE
                     IF ((Y(J).NE.FBLANK) .AND. (W(J).GT.0.0) .AND.
     *                  (W(J).NE.FBLANK)) THEN
                        YS(I) = YS(I) + Y(J) * W(J)
                        WS(I) = WS(I) + W(J)
                        END IF
                     END IF
 20               CONTINUE
 25            DO 30 J = I+1,N
                  IF (ABS(X(I)-X(J)).GT.HW) THEN
                     GO TO 35
                  ELSE
                     IF ((Y(J).NE.FBLANK) .AND. (W(J).GT.0.0) .AND.
     *                  (W(J).NE.FBLANK)) THEN
                        YS(I) = YS(I) + Y(J) * W(J)
                        WS(I) = WS(I) + W(J)
                        END IF
                     END IF
 30               CONTINUE
               END IF
 35         IF (WS(I).GT.0.0) THEN
               YS(I) = YS(I) / WS(I)
            ELSE
               WASB = .TRUE.
               END IF
 100        CONTINUE
         END IF
C                                       fill in remaining blanks
      IF (WASB) THEN
C                                       extrapolate to ends
         I1 = N+1
         I2 = 0
         DO 110 I = 1,N
            IF (WS(I).GT.0.0) THEN
               I1 = MIN (I, I1)
               I2 = MAX (I, I2)
               END IF
 110        CONTINUE
         IF (I1.GT.1) THEN
            J = I1 - 1
            CALL RFILL (J, YS(I1), YS(1))
            CALL RFILL (J, WS(I1), WS(1))
            END IF
         IF (I2.LT.N) THEN
            J = N - I2
            CALL RFILL (J, YS(I2), YS(I2+1))
            CALL RFILL (J, WS(I2), WS(I2+1))
            END IF
C                                       interpolate others
         DO 130  I = 1,N
            IF (WS(I).GT.0.0) THEN
               I1 = I
            ELSE
               DO 120 I2 = I+1,N
                  IF (WS(I2).GT.0.0) GO TO 125
 120              CONTINUE
 125           TEMP = X(I2) - X(I1)
               IF (TEMP.EQ.0.0) TEMP = 1.0
               YS(I) = YS(I1) + (X(I)-X(I1)) * (YS(I2)-YS(I1)) / TEMP
               WS(I) = WS(I1) + (X(I)-X(I1)) * (WS(I2)-WS(I1)) / TEMP
               END IF
 130        CONTINUE
         END IF
C
 999  RETURN
      END

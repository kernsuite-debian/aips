      SUBROUTINE SMMWF (WIDTH, ALPHA, X, Y, W, N, YS, WS, YOR, WOR)
C-----------------------------------------------------------------------
C! Median window smoothing of an irregularly spaced array with blanking
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
C   SMMWF does a median window smoothing of weighted irregularly spaced
C   points possibly with blanked values.  Only returns blanked values if
C   no valid data found.  First good value used for all previous points,
C   last good value used for all subsequent points in which all data are
C   blanked in the boxcar.  A datum is blanked if its weight is <= 0 or
C   FBLANK.
C   Inputs:
C      WIDTH   R      Width of boxcar in same units as X: 0 => replace
C                     blanks with interpolated closest 2, < 0 => replace
C                     only blanks with the b oxcar smoothed values (all
C                     others remain unchanged)
C      ALPHA   R      0 -> 1 = pure boxcar -> pure MWF (ALPHA of the
C                     data samples are discarded and the rest averaged).
C      X       R(N)   Absissae of points to be smoothed in increasing
C                     order
C      Y       R(N)   Values to be smoothed.
C      W       R(N)   Weights of data.
C      N       I      Number of points to smooth.
C    Output:
C      YS      R(N)   Smoothed values.
C      WS      R(N)   Smoothed weights
C      YOR     R(N)   Scratch
C      WOR     R(N)   Scratch
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      WIDTH, ALPHA, X(N), Y(N), W(N), YS(N), WS(N), YOR(N),
     *   WOR(N)
C
      INTEGER   I, J, K, L, I1, I2, IC
      REAL      HW, D, TEMP, BETA
      LOGICAL   WASB, ONLYB, BLNKD
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      D = ABS (WIDTH)
      ONLYB = WIDTH.LE.0.0
      HW = D / 2.0
      WASB = .FALSE.
      BETA = MAX (0.05, MIN (0.95, ALPHA)) / 2.0
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
               IC = 0
            ELSE
               YS(I) = Y(I) * W(I)
               WS(I) = W(I)
               IC = 1
               YOR(IC) = Y(I)
               WOR(IC) = W(I)
               END IF
            IF ((BLNKD) .OR. (.NOT.ONLYB)) THEN
               DO 30 K = 1,I-1
                  IF (ABS(X(I)-X(I-K)).GT.HW) THEN
                     GO TO 35
                  ELSE
                     IF ((Y(I-K).NE.FBLANK) .AND. (W(I-K).GT.0.0) .AND.
     *                  (W(I-K).NE.FBLANK)) THEN
C                                       order the datum
                        DO 15 J = 1,IC
                           L = J
                           IF (YOR(J).GT.Y(I-K)) GO TO 20
 15                        CONTINUE
                        L = IC + 1
 20                     DO 25 J = L,IC
                           I1 = IC - J + L
                           YOR(I1+1) = YOR(I1)
                           WOR(I1+1) = WOR(I1)
 25                        CONTINUE
                        YOR(L) = Y(I-K)
                        WOR(L) = W(I-K)
                        END IF
                     END IF
 30               CONTINUE
 35            DO 60 K = I+1,N
                  IF (ABS(X(I)-X(K)).GT.HW) THEN
                     GO TO 65
                  ELSE
                     IF ((Y(K).NE.FBLANK) .AND. (W(K).GT.0.0) .AND.
     *                  (W(K).NE.FBLANK)) THEN
                        DO 45 J = 1,IC
                           L = J
                           IF (YOR(J).GT.Y(K)) GO TO 50
 45                        CONTINUE
                        L = IC + 1
 50                     DO 55 J = L,IC
                           I1 = IC - J + L
                           YOR(I1+1) = YOR(I1)
                           WOR(I1+1) = WOR(I1)
 55                        CONTINUE
                        YOR(L) = Y(K)
                        WOR(L) = W(K)
                        END IF
                     END IF
 60               CONTINUE
C                                       Now average the center set
 65            YS(I) = 0.0
               WS(I) = 0.0
               IF (IC.GT.0) THEN
                  K = BETA * IC + 0.5
                  I1 = 1 + K
                  I2 = IC - K
                  IF (I2.LT.I1) THEN
                     I1 = MAX (1, I1 - 1)
                     I2 = MIN (IC, I2 + 1)
                     END IF
                  DO 70 K = I1,I2
                     YS(I) = YS(I) + YOR(K) * WOR(K)
                     WS(I) = WS(I) + WOR(K)
 70                  CONTINUE
                  END IF
               END IF
            IF (WS(I).GT.0.0) THEN
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

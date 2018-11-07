      SUBROUTINE WTFUNC (UNFBOX, NBXFUN, IAPADD, BUFF)
C-----------------------------------------------------------------------
C! Computes weighting convolution functions and puts into AP
C# UV-util AP-appl
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
C   Computes weighting convolution functions and puts into AP
C   Inputs:
C      UNFBOX   I      Support radius (cells)
C      NBXFUN   I      -4 -> 4 function shape and type (see below)
C      IAPADD   I      0-relative address in AP for func(0,0)
C   In/Out
C      BUFF     R(*)   Scratch buffer
C   Functions are
C      1 : Pill box      f(x) = 1             (x <= UVBOX)
C      2 : linear        f(x) = 1 - x / (UVBOX+1)
C      3 : exponential   f(x) = exp(-2(x/(UVBOX+1))
C      4 : Gaussian      f(x) = exp(-4(x/(UVBOX+1)**2)
C-----------------------------------------------------------------------
      INTEGER   UNFBOX, NBXFUN, IAPADD
      REAL      BUFF(*)
C
      INTEGER   LF, I, J, N, LAPADD
      REAL      X, Y, R, SCALE, EDGE, T, F
C-----------------------------------------------------------------------
C                                       null case
      IF (UNFBOX.LE.0) THEN
         BUFF(1) = 1.0
         CALL QPUT (BUFF, IAPADD, 1, 2)
C                                       some real diameter
      ELSE
         LF = ABS (NBXFUN)
         N = UNFBOX + 1
         SCALE = 1.0 / N
         LAPADD = IAPADD
C                                       Circular
         IF (LF.GE.0) THEN
            EDGE = UNFBOX + 0.05
C                                       Linear
            IF (LF.EQ.2) THEN
               DO 25 I = 1,N
                  Y = I - 1.0
                  DO 20 J = 1,N
                     X = J - 1.0
                     R = SQRT (X*X + Y*Y)
                     IF (R.LE.EDGE) THEN
                        BUFF(J) = MAX (0.0, 1.0 - R * SCALE)
                     ELSE
                        BUFF(J) = 0.0
                        END IF
 20                  CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 25               CONTINUE
C                                       Exponential
            ELSE IF (LF.EQ.3) THEN
               DO 35 I = 1,N
                  Y = I - 1.0
                  DO 30 J = 1,N
                     X = J - 1.0
                     R = SQRT (X*X + Y*Y)
                     IF (R.LE.EDGE) THEN
                        BUFF(J) = EXP (- 2.0 * R * SCALE)
                     ELSE
                        BUFF(J) = 0.0
                        END IF
 30                  CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 35               CONTINUE
C                                       Gaussian
            ELSE IF (LF.EQ.4) THEN
               DO 45 I = 1,N
                  Y = I - 1.0
                  DO 40 J = 1,N
                     X = J - 1.0
                     R = SQRT (X*X + Y*Y)
                     IF (R.LE.EDGE) THEN
                        T = 2.0 * R * SCALE
                        BUFF(J) = EXP (- T*T)
                     ELSE
                        BUFF(J) = 0.0
                        END IF
 40                  CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 45               CONTINUE
C                                       Pill-box
            ELSE
               DO 55 I = 1,N
                  Y = I - 1.0
                  DO 50 J = 1,N
                     X = J - 1.0
                     R = SQRT (X*X + Y*Y)
                     IF (R.LE.EDGE) THEN
                        BUFF(J) = 1.0
                     ELSE
                        BUFF(J) = 0.0
                        END IF
 50                  CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 55               CONTINUE
               END IF
C                                       Square
         ELSE
C                                       Linear
            IF (LF.EQ.2) THEN
               DO 125 I = 1,N
                  Y = I - 1.0
                  F = MAX (0.0, 1.0 - Y * SCALE)
                  DO 120 J = 1,N
                     X = J - 1.0
                     T = MAX (0.0, 1.0 - X * SCALE)
                     BUFF(J) = F * T
 120                 CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 125              CONTINUE
C                                       Exponential
            ELSE IF (LF.EQ.3) THEN
               DO 135 I = 1,N
                  Y = I - 1.0
                  F = EXP (- 2.0 * Y * SCALE)
                  DO 130 J = 1,N
                     X = J - 1.0
                     T = EXP (- 2.0 * X * SCALE)
                     BUFF(J) = F * T
 130                 CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 135              CONTINUE
C                                       Gaussian
            ELSE IF (LF.EQ.4) THEN
               DO 145 I = 1,N
                  Y = I - 1.0
                  F = EXP (- 4.0 * Y * Y * SCALE * SCALE)
                  DO 140 J = 1,N
                     X = J - 1.0
                     T = EXP (- 4.0 * X * X * SCALE * SCALE)
                     BUFF(J) = F * T
 140                 CONTINUE
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 145              CONTINUE
C                                       Pill-box
            ELSE
               CALL RFILL (N, 1.0, BUFF)
               DO 155 I = 1,N
                  CALL QPUT (BUFF, LAPADD, N, 2)
                  LAPADD = LAPADD + N
 155              CONTINUE
               END IF
            END IF
         END IF
C
      CALL QWD
C
 999  RETURN
      END

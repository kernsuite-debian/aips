      SUBROUTINE B2JPOS (DIR, RAIN, DECIN, RAOUT, DECOUT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Routine to convert between B1950 positions and J2000 positions.
C   Positions assumed to be at standard mean epoch (i.e. 1950 or 2000).
C   Note: Proper motions, parallax and radial velocity assumed 0.
C   Taken from the Supplement to the Astronomical Almanac 1984.
C   Inputs:
C      DIR    I  1 => B1950 to J2000; -1 => J2000 to B1950.
C      RAIN   D  Input Right ascension (radians).
C      DECIN  D  Input declination.
C   Outputs:
C      RAOUT  D  Output Right ascension
C      DECOUT D  Output declination
C-----------------------------------------------------------------------
      INTEGER DIR
      DOUBLE PRECISION RAIN, DECIN, RAOUT, DECOUT
C
      INTEGER LOOP, LOOP2
      DOUBLE PRECISION M(6,6), MR0(6), MR1(6), MA(3), MR(6), MS(6),
     *   MS1(6), X, Y, Z, R, TEMP, SUM, TWOPI, XR1, M3P(3,3)
      DATA MA /-1.62557D-6, -.31919D-6, -.13843D-6/
      DATA M /0.9999256782D0, -.0111820611D0, -.0048579477D0,
     *   0.00000242395018D0, -.00000002710663D0, -.00000001177656D0,
     *   0.0111820610D0, 0.9999374784D0, -.0000271765D0,
     *   0.00000002710663D0, 0.00000242397878D0, -.00000000006587D0,
     *   0.0048579479D0, -.0000271474D0, 0.9999881997D0,
     *   0.00000001177656D0, -.00000000006582D0, 0.00000242410173D0,
     *   -.000551D0, -.238565D0, 0.435739D0,
     *   0.99994704D0, -.01118251D0, -.00485767D0,
     *   0.238514D0, -.002667D0, -.008541D0,
     *   0.01118251D0, 0.99995883D0, -.00002718D0,
     *   -.435623D0, 0.012254D0, 0.002117D0,
     *   0.00485767D0, -.00002714D0, 1.00000956D0/
C     DATA MP /0.9999256795D0, 0.0111814828D0, 0.0048590039D0,
C    *   -.00000242389840D0, -.00000002710544D0, -.00000001177742D0,
C    *   -.0111814828D0, 0.9999374849D0, -.0000271771D0,
C    *   0.00000002710544D0, -.00000242392702D0, 0.00000000006585D0,
C    *   -.0048590040D0, -.0000271557D0, 0.9999881946D0,
C    *   0.00000001177742D0, 0.00000000006585D0, -.00000242404995D0,
C    *   -.000551D0, 0.238509D0, -.435614D0,
C    *   0.99990432D0, 0.01118145D0, 0.00485852D0,
C    *   -.238560D0, -.002667D0, 0.012254D0,
C    *   -.01118145D0, 0.99991613D0, -.00002717D0,
C    *   0.435730D0, -.008541D0, 0.002117D0,
C    *   -.00485852D0, -.00002716D0, 0.99996684D0/
C                                        M3P is the inverse of M as a
C                                        3x3 matrix, MP is the inverse
C                                        as a true 6x6 matrix.
      DATA M3P /0.999925678130111D0, 0.0111820610293105D0,
     *   0.004857947862766843D0, -0.01118206107068339D0,
     *   0.999937478463956D0, -0.00002714738732342485D0,
     *   -0.004857947737353646D0, -0.00002717648788286833D0,
     *   0.999988199766155D0/
C-----------------------------------------------------------------------
      TWOPI = 8.D0 * DATAN (1.D0)
C                                       Which direction?
      IF (DIR.GT.0) THEN
C                                       B1950 => J2000
         MR0(1) = COS (RAIN) * COS (DECIN)
         MR0(2) = SIN (RAIN) * COS (DECIN)
         MR0(3) = SIN (DECIN)
C                                       Remove abberation "E-terms"
         TEMP = MR0(1)*MA(1) + MR0(2)*MA(2) + MR0(3)*MA(3)
         MR1(1) = MR0(1) * (1.0D0 + TEMP) - MA(1)
         MR1(2) = MR0(2) * (1.0D0 + TEMP) - MA(2)
         MR1(3) = MR0(3) * (1.0D0 + TEMP) - MA(3)
C                                       Rotate
         DO 110 LOOP = 1,3
            SUM = 0.0D0
            DO 100 LOOP2 = 1,3
               SUM = SUM + M(LOOP2,LOOP) * MR1(LOOP2)
 100           CONTINUE
            MR(LOOP) = SUM
 110        CONTINUE
C                                       Convert to polar coordinates.
         X = MR(1)
         Y = MR(2)
         Z = MR(3)
         R = SQRT (X*X + Y*Y + Z*Z)
         DECOUT = ASIN (Z / R)
         RAOUT = ATAN2 (Y, X)
         IF (RAOUT.LT.0.0) RAOUT = RAOUT + TWOPI
C                                       J2000 to B1950
      ELSE
         MR0(1) = COS (RAIN) * COS (DECIN)
         MR0(2) = SIN (RAIN) * COS (DECIN)
         MR0(3) = SIN (DECIN)
C                                       Rotate
         DO 220 LOOP = 1,3
            SUM = 0.0D0
            DO 210 LOOP2 = 1,3
               SUM = SUM + M3P(LOOP2,LOOP) * MR0(LOOP2)
 210           CONTINUE
            MR1(LOOP) = SUM
 220        CONTINUE
C                                       Include abberation "E-terms"
         XR1 = 1.0D0 / SQRT (MR1(1)*MR1(1) + MR1(2)*MR1(2) +
     *      MR1(3)*MR1(3))
         MS1(1) = MR1(1) * XR1
         MS1(2) = MR1(2) * XR1
         MS1(3) = MR1(3) * XR1
         MS(1) = MS1(1)
         MS(2) = MS1(2)
         MS(3) = MS1(3)
C                                       Iterate a few times
         DO 250 LOOP = 1,5
            TEMP = MS(1)*MA(1) + MS(2)*MA(2) + MS(3)*MA(3)
            MR(1) = MS1(1) + MA(1) - (MS(1)*TEMP)
            MR(2) = MS1(2) + MA(2) - (MS(2)*TEMP)
            MR(3) = MS1(3) + MA(3) - (MS(3)*TEMP)
            XR1 = 1.0D0 / SQRT (MR(1)*MR(1) + MR(2)*MR(2) +
     *         MR(3)*MR(3))
            MS(1) = MR(1) * XR1
            MS(2) = MR(2) * XR1
            MS(3) = MR(3) * XR1
 250        CONTINUE
C                                       Convert to polar coordinates.
         X = MR(1)
         Y = MR(2)
         Z = MR(3)
         R = SQRT (X*X + Y*Y + Z*Z)
         DECOUT = ASIN (Z / R)
         RAOUT = ATAN2 (Y, X)
         IF (RAOUT.LT.0.0) RAOUT = RAOUT + TWOPI
         END IF
C
 999  RETURN
      END

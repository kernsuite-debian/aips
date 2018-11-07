      SUBROUTINE JNUT (DATJUL, DELPSI, DELEPS)
C-----------------------------------------------------------------------
C! Computes nutation from IAU 1980 series
C# Math Coordinates
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
C   Calculation of nutation, assuming a non-rigid Earth.
C   Uses IAU 1980 precession series from Supplement to the Astronomical
C   Almanac 1984.  For Epoch J2000.
C    Inputs:
C     DATJUL   D  Julian date.
C    Outputs:
C     DELPSI   D  Nutation in longitude (radians)
C     DELEPS   D  Nutation in obliquity
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, DELPSI, DELEPS
      INTEGER   I, NLONG, NOBLI
      DOUBLE PRECISION ARG(106), ALONG(106), TALONG(106), AOBLI(106),
     *   TAOBLI(106), MAMOON, MASUN, F, D, OMEGA, T, TWOPI, CC
      DATA ALONG /-17.1996D0, 0.2062D0, 0.0046D0, 0.0011D0, -.0003D0,
     *   -.0003D0, -.0002D0, 0.0001D0, -1.3187D0, 0.1426D0, -.0517D0,
     *   0.0217D0, 0.0129D0, 0.0048D0, -.0022D0, 0.0017D0, -.0015D0,
     *   -.0016D0, -.0012D0, -.0006D0, -.0005D0, 0.0004D0, 0.0004D0,
     *   -.0004D0, 0.0001D0, 0.0001D0, -.0001D0, 0.0001D0, 0.0001D0,
     *   -.0001D0, -.2274D0, 0.0712D0, -.0386D0, -.0301D0, -.0158D0,
     *   0.0123D0, 0.0063D0, 0.0063D0, -.0058D0, -.0059D0, -.0051D0,
     *   -.0038D0, 0.0029D0, 0.0029D0, -.0031D0, 0.0026D0, 0.0021D0,
     *   0.0016D0, -.0013D0, -.0010D0, -.0007D0, 0.0007D0, -.0007D0,
     *   -.0008D0, 0.0006D0, 0.0006D0, -.0006D0, -.0007D0, 0.0006D0,
     *   -.0005D0, 0.0005D0, -.0005D0, -.0004D0, 0.0004D0, -.0004D0,
     *   -.0003D0, 0.0003D0, -.0003D0, -.0003D0, -.0002D0, -.0003D0,
     *   -.0003D0, 0.0002D0, -.0002D0, 0.0002D0, -.0002D0, 0.0002D0,
     *   0.0002D0, 0.0001D0, -.0001D0, 0.0001D0, -.0002D0, -.0001D0,
     *   0.0001D0, -.0001D0, -.0001D0, 0.0001D0, 0.0001D0, 0.0001D0,
     *   -.0001D0, -.0001D0, 0.0001D0, 0.0001D0, -.0001D0, 0.0001D0,
     *   0.0001D0, -.0001D0, -.0001D0, -.0001D0, -.0001D0, -.0001D0,
     *   -.0001D0, -.0001D0, 0.0001D0, -.0001D0, 0.0001D0/
      DATA TALONG /-174.2D-4, 0.2D-4, 6*0.0D-4, -1.6D-4, -3.4D-4,
     *   1.2D-4, -.5D-4, 0.1D-4, 0.0D-4, 0.0D-4, -.1D-4, 0.0D-4, 0.1D-4,
     *   12*0.0D-4, -.2D-4, 0.1D-4, -.4D-4, 4*0.0D-4, 0.1D-4, -0.1D-4,
     *   67*0.0D-4/
      DATA AOBLI /9.2025D0, -.0895D0, -.0024D0, 0.0D0, 0.0001D0, 0.0D0,
     *   0.0001D0, 0.0D0, 0.5736D0, 0.0054D0, 0.0224D0, -.0095D0,
     *   -.0070D0, 0.0001D0, 2*0.0D0, 0.0009D0, 0.0007D0, 0.0006D0,
     *   0.0003D0, 0.0003D0, -.0002D0, -.0002D0, 7*0.0D0, 0.0977D0,
     *   -.0007D0, 0.0200D0, 0.0129D0, -.0001D0, -.0053D0, -.0002D0,
     *   -.0033D0, 0.0032D0, 0.0026D0, 0.0027D0, 0.0016D0, -.0001D0,
     *   -.0012D0, 0.0013D0, -.0001D0, -.0010D0, -.0008D0, 0.0007D0,
     *   0.0005D0, 0.0000D0, -.0003D0, 0.0003D0, 0.0003D0, 0.0000D0,
     *   -.0003D0, 0.0003D0, 0.0003D0, -.0003D0, 0.0003D0, 0.0000D0,
     *   0.0003D0, 5*0.00D0, 5*.0001D0, -.0001D0, 0.0001D0, -.0001D0,
     *   0.0001D0, 0.0000D0, -.0001D0, -.0001D0, 0.0000D0, -.0001D0,
     *   0.0001D0, 0.0000D0, -.0001D0, 0.0001D0, 0.0001D0, 0.0000D0,
     *   0.0000D0, -.0001D0, 17*0.0D0/
      DATA TAOBLI /8.9D-4, 0.5D-4, 6*0.0D-4, -3.1D-4, -.1D-4, -.6D-4,
     *   0.3D-4, 18*0.0D-4, -.5D-4, 0.0D-4, 0.0D-4, -.1D-4, 72*0.0D-4/
      DATA NLONG, NOBLI /106, 89/
C-----------------------------------------------------------------------
C                                       Time in centuries
      T = (DATJUL - 2451545.D0) / 36525.D0
      TWOPI = 8.D0 * DATAN (1.D0)
      CC = TWOPI / 1296000.D0
C                                       Arguments of the series
      MAMOON = MOD ((((((0.064D0 * T) + 31.310D0) * T + 715922.633D0
     *   + 1325.0D0 * 1296000.0D0) * T + 485866.733D0) * CC), TWOPI)
      MASUN = MOD ((((((-.012D0 * T) - 0.577D0) * T + 1292581.224D0
     *   + 99.0D0 * 1296000.0D0) * T + 1287099.804D0) * CC), TWOPI)
      F = MOD ((((((0.011D0 * T) - 13.257D0) * T + 295263.137D0
     *   + 1342.0D0 * 1296000.0D0) * T + 335778.877D0) * CC), TWOPI)
      D = MOD ((((((0.019D0 * T) - 6.891D0) * T + 1105601.328D0
     *   + 1236.0D0 * 1296000.0D0) * T + 1072261.307D0) * CC), TWOPI)
      OMEGA = MOD ((((((0.008D0 * T) + 7.455D0) * T - 482890.539D0
     *   - 5.0D0 * 1296000.0D0) * T + 450160.280D0) * CC), TWOPI)
C                                       Form final arguments
      ARG(1) = OMEGA
      ARG(2) = 2*OMEGA
      ARG(3) = -2*MAMOON + 2*F + OMEGA
      ARG(4) = 2*MAMOON - 2*F
      ARG(5) = -2*MAMOON + 2*F + 2*OMEGA
      ARG(6) = MAMOON - MASUN - D
      ARG(7) = -2*MASUN + 2*F - 2*D + OMEGA
      ARG(8) = 2*MAMOON - 2*F + OMEGA
      ARG(9) = 2*F - 2*D + 2*OMEGA
      ARG(10) = MASUN
      ARG(11) = MASUN + 2*F - 2*D + 2*OMEGA
      ARG(12) = -MASUN + 2*F - 2*D + 2*OMEGA
      ARG(13) = 2*F - 2*D + OMEGA
      ARG(14) = 2*MAMOON - 2*D
      ARG(15) = 2*F - 2*D
      ARG(16) = 2*MASUN
      ARG(17) = MASUN + OMEGA
      ARG(18) = 2*MASUN + 2*F - 2*D + 2*OMEGA
      ARG(19) = -MASUN + OMEGA
      ARG(20) = -2*MAMOON + 2*D + OMEGA
      ARG(21) = -MASUN + 2*F - 2*D + OMEGA
      ARG(22) = 2*MAMOON - 2*D + OMEGA
      ARG(23) = MASUN + 2*F - 2*D + OMEGA
      ARG(24) = MAMOON - D
      ARG(25) = 2*MAMOON + MASUN - 2*D
      ARG(26) = -2*F + 2*D + OMEGA
      ARG(27) = MASUN - 2*F + 2*D
      ARG(28) = MASUN + 2*OMEGA
      ARG(29) = -MAMOON + D + OMEGA
      ARG(30) = MASUN + 2*F - 2*D
      ARG(31) = 2*F + 2*OMEGA
      ARG(32) = MAMOON
      ARG(33) = 2*F + OMEGA
      ARG(34) = MAMOON + 2*F + 2*OMEGA
      ARG(35) = MAMOON - 2*D
      ARG(36) = -MAMOON + 2*F + 2*OMEGA
      ARG(37) = 2*D
      ARG(38) = MAMOON + OMEGA
      ARG(39) = -MAMOON + OMEGA
      ARG(40) = -MAMOON + 2*F + 2*D + 2*OMEGA
      ARG(41) = MAMOON + 2*F + OMEGA
      ARG(42) = 2*F + 2*D + 2*OMEGA
      ARG(43) = 2*MAMOON
      ARG(44) = MAMOON + 2*F - 2*D + 2*OMEGA
      ARG(45) = 2*MAMOON + 2*F + 2*OMEGA
      ARG(46) = 2*F
      ARG(47) = -MAMOON + 2*F + OMEGA
      ARG(48) = -MAMOON + 2*D + OMEGA
      ARG(49) = MAMOON - 2*D + OMEGA
      ARG(50) = -MAMOON + 2*F + 2*D + OMEGA
      ARG(51) = MAMOON + MASUN - 2*D
      ARG(52) = MASUN + 2*F + 2*OMEGA
      ARG(53) = -MASUN + 2*F + 2*OMEGA
      ARG(54) = MAMOON + 2*F + 2*D + 2*OMEGA
      ARG(55) = MAMOON + 2*D
      ARG(56) = 2*MAMOON + 2*F - 2*D + 2*OMEGA
      ARG(57) = 2*D + OMEGA
      ARG(58) = 2*F + 2*D + OMEGA
      ARG(59) = MAMOON + 2*F - 2*D + OMEGA
      ARG(60) = -2*D + OMEGA
      ARG(61) = MAMOON - MASUN
      ARG(62) = 2*MAMOON + 2*F + OMEGA
      ARG(63) = MASUN - 2*D
      ARG(64) = MAMOON - 2*F
      ARG(65) = D
      ARG(66) = MAMOON + MASUN
      ARG(67) = MAMOON + 2*F
      ARG(68) = MAMOON - MASUN + 2*F + 2*OMEGA
      ARG(69) = -MAMOON - MASUN + 2*F + 2*D + 2*OMEGA
      ARG(70) = -2*MAMOON + OMEGA
      ARG(71) = 3*MAMOON + 2*F + 2*OMEGA
      ARG(72) = -MASUN + 2*F + 2*D + 2*OMEGA
      ARG(73) = MAMOON + MASUN + 2*F + 2*OMEGA
      ARG(74) = -MAMOON + 2*F - 2*D + OMEGA
      ARG(75) = 2*MAMOON + OMEGA
      ARG(76) = MAMOON + 2*OMEGA
      ARG(77) = 3*MAMOON
      ARG(78) = 2*F + D + 2*OMEGA
      ARG(79) = -MAMOON + 2*OMEGA
      ARG(80) = MAMOON - 4*D
      ARG(81) = -2*MAMOON + 2*F + 2*D + 3*OMEGA
      ARG(82) = -MAMOON + 2*F + 4*D + 2*OMEGA
      ARG(83) = 2*MAMOON - 4*D
      ARG(84) = MAMOON + MASUN + 2*F - 2*D + 2*OMEGA
      ARG(85) = MAMOON + 2*F + 2*D + OMEGA
      ARG(86) = -2*MAMOON + 2*F + 4*D + 2*OMEGA
      ARG(87) = -MAMOON + 4*F + 2*OMEGA
      ARG(88) = MAMOON - MASUN -2*D
      ARG(89) = 2*MAMOON + 2*F - 2*D + OMEGA
      ARG(90) = 2*MAMOON + 2*F + 2*D + 2*OMEGA
      ARG(91) = MAMOON + 2*D + OMEGA
      ARG(92) = 4*F - 2*D + 2*OMEGA
      ARG(93) = 3*MAMOON + 2*F - 2*D + 2*OMEGA
      ARG(94) = MAMOON + 2*F - 2*D
      ARG(95) = MASUN + 2*F + OMEGA
      ARG(96) = -MAMOON -MASUN + 2*D + OMEGA
      ARG(97) = -2*F + OMEGA
      ARG(98) = 2*F - D + 2*OMEGA
      ARG(99) = MASUN + 2*D
      ARG(100) = MAMOON - 2*F - 2*D
      ARG(101) = -MASUN + 2*F + OMEGA
      ARG(102) = MAMOON + MASUN -2*D + OMEGA
      ARG(103) = MAMOON - 2*F + 2*D
      ARG(104) = 2*MAMOON + 2*D
      ARG(105) = 2*F + 4*D + 2*OMEGA
      ARG(106) = MASUN + D
C                                       Sum in arc sec.
C                                       Longitude:
      DELPSI = 0.0D0
      DO 100 I = 1,NLONG
         DELPSI = DELPSI + (ALONG(I) + T * TALONG(I)) * SIN (ARG(I))
 100     CONTINUE
C                                       Obliquity
      DELEPS = 0.0D0
      DO 200 I = 1,NOBLI
         DELEPS = DELEPS + (AOBLI(I) + T * TAOBLI(I)) * COS (ARG(I))
 200     CONTINUE
C                                       Convert to radians
      DELPSI = DELPSI * CC
      DELEPS = DELEPS * CC
C
 999  RETURN
      END

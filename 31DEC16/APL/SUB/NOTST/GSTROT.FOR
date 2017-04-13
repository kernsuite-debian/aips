      SUBROUTINE GSTROT (JD, GMSTM, GASTM, RATE)
C-----------------------------------------------------------------------
C! Computes GST at UT=0 and earth rotation rate.
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
C   GSTROT computes the Greenwich sideral time at UT=0 (both mean and
C   apparent) and the earth rotation rate at a given julian date.
C   Formulae taken from the supplement to the American Ephemerus and
C   Nautical Almanac.
C    Input:
C     JD      D    Julian date.
C    Output:
C     GMSTM   D    Greenwich Mean Sidereal Time at UT midnight on JD.
C                  (degrees)
C     GASTM   D    Greenwich App. Sidereal Time at UT midnight on JD.
C     RATE    D    Earth rotation rate (deg/UT day)
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, GMSTM, GASTM, RATE, TU, JD0, HR2RD, EQEQ,
     *   TC, EPS, DELPSI, DELEPS, SPSI, SEPS
      DATA JD0 /2415020.0D0/
C-----------------------------------------------------------------------
C                                       Set hours to radians constant.
      HR2RD = 15.0D0 * 3.141592653589793D0 / 180.0D0
C                                       Get equation of equinoxes.
      TC = (JD - 2433282.423D0) / 36524.21988D0
      EPS = -46.850D0 * TC - 0.0034D0 * TC * TC +
     *   0.0018D0 * TC * TC * TC
      EPS = (84404.84D0 + EPS) / 3600.0D0 / 180.0D0*3.141592658979D0
      CALL NUT4 (JD, DELPSI, DELEPS, SPSI, SEPS)
      EQEQ = DELPSI * COS (EPS)
C                                        Compute earth rotation rate.
C                                        and GMST at UT midnight.
      TU = (JD - JD0) / 36525.0D0
C                                       Mean time
      GMSTM = (8640184.542D0/3600.0D0) * TU
      GMSTM = MOD (GMSTM, 24.0D0)
      GMSTM = (6.0D0 + 38.0D0/60.0D0 + 45.836D0/3600D0) +
     *   GMSTM + (0.0929D0/3600.0D0) * TU * TU
C                                       Apparent time
      GASTM = GMSTM + (EQEQ / HR2RD)
      GMSTM = 15.0D0 * GMSTM
      GASTM = 15.0D0 * GASTM
      RATE = 1.00273790265D0 + 0.589D-10 * TU
      RATE = 360.0 * RATE
C
 999  RETURN
      END

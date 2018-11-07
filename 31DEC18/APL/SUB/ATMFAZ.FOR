      SUBROUTINE ATMFAZ (EL, HA, LAT, DEC, R, T, PRES, PPWVR, TLAP, HTP,
     *   WS, DELAY, RATE)
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
C   Computes neutral model atmosphere group delay and rate CORRECTIONS.
C   Uses the Cfa-2.2 dry maping function (Davis et al. 1985, Radio
C   Science, 20, pp 1593-1607).
C   The "dry" zenith delay is also from Davis et. al.
C   The "wet" terms are a adaptation of a routine by J. Moran
C   incorporating a more realistic temperature profile.
C    Inputs:
C     EL     R  Elevation of source (radians).
C     HA     R  Hour angle of source (radians)
C     LAT    D  latitude of observation.
C     DEC    D  App. declination of source (rad)
C     R      D  Distance of observation from center of earth (m).
C     T      R  Surface temperature (deg C)
C     PRES   R  Total surface pressure (mbar) (not sea level).
C     PPWVR  R  Partial presure of water vapor at surface (mbar).
C     TLAP   R  Tropospheric temperature lapse rate (K/km). (negative)
C     HTP    R  Height of the tropopause (km)
C     WS     R  Scale height of water vapor (km)
C    Output:
C     DELAY  R  Model group delay (seconds)
C     RATE   R  Model time derivative of DELAY (sec/sec)
C-----------------------------------------------------------------------
      REAL EL, HA, T, PRES, PPWVR, TLAP, HTP, WS, DELAY, RATE
      DOUBLE PRECISION LAT, DEC, R
      REAL ZDRY, H, A, B, C, K1, K2, K3, DELDT, MFN, DMFN, SUM1,
     *   SUM2, STEP, TERM, TH, FACT(41), PWET, FWET, DEN, FFF, CEL, RT,
     *   TK, CESET
      INTEGER NUMINT, LOOP
C                                       Coef. for Simpson's rule
C                                       integration
      DATA FACT /1.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,
     *   4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,
     *   2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,2.0,4.0,1.0/
C-----------------------------------------------------------------------
C                                       Height above geoid (km)
      H = 0.001 * (R - 6.378137E6 * (0.99827073 +
     *   0.001676438 * COS (2.0 * LAT) - 0.000003519 * COS (4.0 * LAT) +
     *   8.0E-9 * COS (6.0 * LAT)))
C                                       "Dry" zenith delay (m)
      ZDRY = 2.2768E-3 * PRES / (1.0 - 0.00266 * COS (2.0 * LAT) -
     *   0.00028 * H)
C                                       Cfa-2.2 mapping function.
      A = 0.001185 * (1.0 + 0.6071E-4 * (PRES - 1000.0)
     *   - 0.1471E-3 * PPWVR
     *   + 0.3072E-2 * (T - 20.0)
     *   + 0.1965E-1 * (TLAP + 6.5)
     *   - 0.5645E-2 * (HTP - 11.231))
      B = 0.001144 * (1.0 + 0.1164E-4 * (PRES - 1000.0)
     *   + 0.2795E-3 * PPWVR
     *   + 0.3109E-2 * (T - 20.0)
     *   + 0.3038E-1 * (TLAP + 6.5)
     *   - 0.1217E-1 * (HTP - 11.231))
      C = -0.0090
C                                       Time derivative of el (Hz)
      IF (EL.LT.1.56) THEN
         DELDT = -7.29211E-5 * SIN (HA) * COS (LAT) * COS (DEC) /
     *      SQRT (1.0 - (SIN (EL)**2))
      ELSE
C                                       At zenith
         DELDT = 0.0
         END IF
      K3 = 1.0 / (SIN (EL) + C)
      K2 = 1.0 / (TAN (EL) + B * K3)
      K1 = 1.0 / (SIN (EL) + A * K2)
      MFN = K1
      DMFN = -K1 * K1 * (COS (EL) - A * K2 * K2 *
     *   ((1.0 / (COS (EL)**2)) - B * K3 * K3 * COS (EL))) * DELDT
C                                       Delay (sec)
      DELAY = MFN * ZDRY * 3.33564E-9
C                                       Rate (sec/sec)
      RATE = DMFN * ZDRY * 3.33564E-9
C                                       "Wet" zenith delay
C                                       Numerical integration
      NUMINT = 41
      STEP = WS * 0.1
C                                       First term
C                                       Cosine elevation angle
      CEL = COS (EL)
C                                       Distance from earth center (km)
      RT = R * 0.001
      TK = T + 273.16
      SUM1 = 0.0
      SUM2 = 0.0
      DO 100 LOOP = 2,NUMINT
         H = (LOOP-1) * STEP
C                                       Temperature
         IF (H.LE.HTP) THEN
C                                       Troposphere
            TH = TK + TLAP * H
         ELSE
C                                       Isothermal stratosphere
            TH = TK + TLAP * HTP
            END IF
         TERM = EXP (-H/WS) / TH
         FFF = (RT / (RT + H))**2
         DEN = SQRT (1.0D0 - FFF * CEL * CEL)
         SUM1 = SUM1 + FACT(LOOP) * TERM / DEN
         SUM2 = SUM2 + FACT(LOOP) * TERM * FFF / (DEN**3)
 100     CONTINUE
C                                       Wet terms (sec, sec/sec)
      CESET = -COS (EL) * SIN(EL) * DELDT
      FFF = (PPWVR * 1720.0E-6 * 217.0 / TK) * (STEP * 0.33333333)
     *   *  3.33564E-6
      PWET = SUM1 * FFF
      FWET = SUM2 * FFF * CESET
C                                       Add "wet" terms
      DELAY = DELAY + PWET
      RATE = RATE + FWET
C                                       Convert to corrections
      DELAY = - DELAY
      RATE = - RATE
C
 999  RETURN
      END

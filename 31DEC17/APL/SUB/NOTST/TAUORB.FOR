      SUBROUTINE TAUORB (R, OMEGAF, V, OMEGVF, INCLIN, OMEGA, DELTA,
     *   ALPHA, TAU, RATE, DTAUDR, DTAUOF, DTAUDI, DTAUOM, DRATDV,
     *   DRATOF, DRATDI, DRATOM)
C----------------------------------------------------------------------
C! Delay and rate for an orbiting antenna, including error terms
C# Math Coordinates UV Calibration
C----------------------------------------------------------------------
C;  Copyright (C) 1997
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

C----------------------------------------------------------------------
C   Subroutine to compute delay and rate for an orbiting antenna
C   and their partial derivates wrt a transformed set of orb. parm.
C   Inputs:
C      R          D    Geocentric distance to the antenna (m)
C      OMEGAF     D    Angle in the orbit plane from the ascending node
C                      to the anten. as seen from the geocenter (rad)
C      V          D    Speed of the orbiting antenna (m/s)
C      OMEGVF     D    Angle in the orbit plane from the ascending node
C                      to the antenna velocity vector (rad)
C      INCLIN     D    Orbit plane inclination (rad)
C      OMEGA      D    Right ascension of the ascending mode (rad)
C      DELTA      D    Source declination (rad)
C      ALPHA      D    Source right ascension (rad)
C   Output:
C      TAU        D    Projection of the geocentric antenna vector
C                      in the source direction (m)
C      RATE       D    Time rate of change of TAU (m/s)
C      DTAUDR     D    Partial derivative DTAU/DR (dimensionless)
C      DTAUOF     D    Partial derivative DTAU/DOMEGAF  (m/arcsec)
C      DTAUDI     D    Partial derivative DTAU/DINCLIN, (m/arcsec)
C      DTAUOM     D    Partial derivative DTAU/DOMEGA, (m/arcsec)
C      DRATDV     D    Partial derivative DRATE/DV (m/sec/m/sec)
C      DRATOF     D    Partial derivative DRATE/DOMEGAF (m/sec/arcsec)
C      DRATDI     D    Partial derivative DRATE/DINCLIN (m/sec/arcsec)
C      DRATOM     D    Partial derivative DRATE/DOMEGA (m/sec/arcsec)
C----------------------------------------------------------------------
      DOUBLE PRECISION R, OMEGAF, V, OMEGVF, INCLIN, OMEGA, DELTA,
     *   ALPHA, TAU, RATE, DTAUDR, DTAUOF, DTAUDI, DTAUOM, DRATDV,
     *   DRATOF, DRATDI, DRATOM, OMAL
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      OMAL = OMEGA - ALPHA
C                                       Delay in m
      TAU = R * ( COS(DELTA) * (COS(OMEGAF) * COS(OMAL) - COS(INCLIN) *
     *   SIN(OMEGAF) * SIN(OMAL)) + SIN(INCLIN) * SIN(OMEGAF) *
     *   SIN(DELTA))
C                                       Partial derivative DTAU/DR,
C                                       m/m
      DTAUDR = COS(DELTA) * (COS(OMEGAF) * COS(OMAL) - COS(INCLIN) *
     *   SIN(OMEGAF) * SIN(OMAL)) + SIN(INCLIN) * SIN(OMEGAF) *
     *   SIN(DELTA)
C                                       Partial derivative
C                                       DTAU/DOMEGAF, m/arcsec
      DTAUOF = R * (COS(DELTA) * (-SIN(OMEGAF) * COS(OMAL) -
     *   COS(INCLIN) * COS(OMEGAF) * SIN(OMAL)) + SIN(INCLIN) *
     *   COS(OMEGAF) * SIN(DELTA)) * AS2RAD
C                                       Partial derivative
C                                       DTAU/DINCLIN, m/arcsec
      DTAUDI = R * ( COS(DELTA) * SIN(INCLIN) * SIN(OMEGAF) * SIN(OMAL)
     *   + COS(INCLIN) * SIN(OMEGAF) *SIN(DELTA)) * AS2RAD
C                                       Partial derivative
C                                       DTAU/DOMEGA, m/arcsec
      DTAUOM = -R * COS(DELTA) * (COS(OMEGAF) * SIN(OMAL) + COS(INCLIN)
     *   * SIN(OMEGAF) * COS(OMAL)) * AS2RAD
C
C                                       Rate in m/sec
      RATE = V * ( COS(DELTA) * (COS(OMEGVF) * COS(OMAL) - COS(INCLIN) *
     *   SIN(OMEGVF) * SIN(OMAL)) + SIN(INCLIN) * SIN(OMEGVF) *
     *   SIN(DELTA))
C                                       Partial derivative DRAT/DV,
C                                       m/sec/m/sec
      DRATDV = COS(DELTA) * (COS(OMEGVF) * COS(OMAL) - COS(INCLIN) *
     *   SIN(OMEGVF) * SIN(OMAL)) + SIN(INCLIN) * SIN(OMEGVF) *
     *   SIN(DELTA)
C                                       Partial derivative
C                                       DRAT/DOMEGVF, m/sec/arcsec
      DRATOF = V * (COS(DELTA) * (-SIN(OMEGVF) * COS(OMAL) -
     *   COS(INCLIN) * COS(OMEGVF) * SIN(OMAL)) + SIN(INCLIN) *
     *   COS(OMEGVF) * SIN(DELTA)) * AS2RAD
C                                       Partial derivative
C                                       DRAT/DINCLIN, m/arcsec
      DRATDI = V * ( COS(DELTA) * SIN(INCLIN) * SIN(OMEGVF) * SIN(OMAL)
     *   + COS(INCLIN) * SIN(OMEGVF) *SIN(DELTA)) * AS2RAD
C                                       Partial derivative
C                                       DRAT/DOMEGA, m/arcsec
      DRATOM = -V * COS(DELTA) * (COS(OMEGVF) * SIN(OMAL) + COS(INCLIN)
     *   * SIN(OMEGVF) * COS(OMAL)) * AS2RAD
C
 999  RETURN
      END

      SUBROUTINE ORBERR (AMETER, ECCEN, INCL, OMEGA, PERIG, MANREF,
     *   TIME, FREQU, DELTA, ALPHA, DR, DANGLE, DINCL, DASCEN, TAUERR,
     *   PHASER)
C----------------------------------------------------------------------
C! Delay and phase errors for an antenna in a perturbed orbit
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
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   Computes delay and phase errors for an orbiting antenna, using
C   a perturbed orbit model.
C   Inputs:
C                            Orbital elements at the reference time:
C      AMETER       D        semi-major axis (m)
C      ECCEN        D        eccentricity
C      INCL         D        Incl. of orbit plane to equator (rad)
C      OMEGA        D        RA of the ascending node (rad)
C      PERIG        D        Angle of perigee to the asc. node (rad)
C      MANREF       D        Mean anomaly at the reference time (rad)
C
C      TIME         D        Time (time in days since the ref. day)
C      FREQU        D        Frequency of observation (Hz)
C      DELTA        D        Source declination (rad)
C      ALPHA        D        Source right ascension (rad)
C
C                            Errors of four orbital parameters:
C      DR           R        Err. in distance to the geocenter (m)
C      DANGLE       R        Err. in the orbiting antenna direction
C                            wrt the orbit plane (arcsec)
C      DINCL        R        Err. in orbit plane inclination (arcsec)
C      DASCEN       R        Err. in right ascension of the orbit's
C                            ascending node (arcsec)
C   Output:
C      TAUERR       D        Residual delay wrt nominal orbit (sec)
C      PHASER       D        Residual phase wrt nominal orbit (rad)
C----------------------------------------------------------------------
      DOUBLE PRECISION TAU, RATE, DTAUDR, DTAUOF, DTAUDI, DTAUOM,
     *   DRATDV, DRATOF, DRATDI, DRATOM, INCLIN, OMEGAR, DELTA, ALPHA,
     *   TAUERR, PHASER, R, V, OMEGAF, OMEGVF, FREQU, AMETER, ECCEN,
     *   INCL, OMEGA, PERIG, MANREF, TIME, TRANOM, PROJA, PROJB, VA,
     *   VB, X, Y, Z, VX, VY, VZ
      REAL DR, DANGLE, DINCL, DASCEN
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
C                                       Find the orbiting antenna
C                                       position at the specified time
      TIME = TIME * 86400.0
      CALL ORBIT (AMETER, ECCEN, INCL, OMEGA, PERIG, MANREF, TIME,
     *   TRANOM, PROJA, PROJB, VA, VB, X, Y, Z, VX, VY, VZ)
C                                       Distance to the orbiting anten.
      R = SQRT (PROJA * PROJA + PROJB * PROJB)
C                                       Angle in the orbit plane from
C                                       the ascending node to the
C                                       antenna, as seen from the Earth
C                                       center
      OMEGAF = PERIG + TRANOM
C                                       Speed of the orbiting antenna
      V = SQRT (VA * VA + VB * VB)
C                                       Angle in the orbit plane from
C                                       the ascending node to the
C                                       antenna velocity vector.
      OMEGVF = PERIG + ATAN2 (VB, VA)
C                                       Find the relevant partial
C                                       derivatives
      INCLIN = INCL
      OMEGAR = OMEGA
      CALL TAUORB(R, OMEGAF, V, OMEGVF, INCLIN, OMEGAR, DELTA,
     *   ALPHA, TAU, RATE, DTAUDR, DTAUOF, DTAUDI, DTAUOM, DRATDV,
     *   DRATOF, DRATDI, DRATOM)
C                                       Delay error in sec
      TAUERR = (DTAUDR * DR + DTAUOF * DANGLE + DTAUDI * DINCL +
     *   DTAUOM * DASCEN) / VELITE
C                                       Phase error in radians
      PHASER = TAUERR * FREQU * TWOPI
C
 999  RETURN
      END

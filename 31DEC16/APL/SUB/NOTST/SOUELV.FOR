      SUBROUTINE SOUELV (ANTNO, TIME, HA, EL, AZ)
C-----------------------------------------------------------------------
C! Computes source hour angles and elevations and azimuths
C# Math Coordinates UV Calibration
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
C   Subroutine to compute the apparent source elevations based on source
C   and antenna coordinates in common.  The routines GETANT and GETSOU
C   should be called before this routine to but the correct values in
C   the relevant commons.
C   Inputs:
C      ANTNO      I    Antenna number
C      TIME       D    Current data time (days).
C   Input from common:
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      STNLAT     D(*) Antenna latitude (rad).
C      STNLON     D(*) Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      HA         R    Source hour angle (rad)
C      EL         R    Source elevation (rad)
C      AZ         R    Source azimuth (rad)
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME
      REAL      HA, EL, AZ
C
      DOUBLE PRECISION HRANG, ANTLST, DARG, DARG2, DRA, DDEC, DAZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
C                                       Antenna LST
      ANTLST = GSTIAT + STNLON(ANTNO) + TIME * ROTIAT
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAAPP
      DDEC = DECAPP
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAEPO
         DDEC = DECEPO
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
C                                       Limit to between 0 and 2pi
      HRANG = DMOD (HRANG, TWOPI)
C                                       translate to between -pi and pi
      IF (HRANG.GT.PI) HRANG = HRANG - TWOPI
      IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
      HA = HRANG
C                                       Elevation angle
      DARG = SIN (STNLAT(ANTNO)) * SIN (DDEC) + COS (STNLAT(ANTNO))
     *   * COS (DDEC) * COS (HRANG)
      EL = PI/2.0D0 - ACOS (DARG)
C                                       AZ = ATAN2(SD*CL - CD*SL*CH,
C                                       CD*SH)
      DARG = SIN (DDEC) * COS (STNLAT(ANTNO)) -
     *       COS (DDEC) * SIN (STNLAT(ANTNO)) * COS(HRANG)
      DARG2 = COS (DDEC) * SIN (HRANG)
      DAZ = ATAN2 (DARG, DARG2)
      DAZ = MOD (DAZ - PI/2.0D0, TWOPI)
      IF (DAZ.LT.0.0D0) DAZ = DAZ + TWOPI
      AZ = DAZ
C
 999  RETURN
      END

      SUBROUTINE PRECES (JD, EQUIN, DELDAT, RAMEAN, DECMEA, RAAPP,
     *   DECAPP, DIR, GR, DA, RHOGEO, PHIGEO, RLST, NR)
C-----------------------------------------------------------------------
C! Convert between mean and apparent positions (B1950 only)
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
C   Conversion between mean and apparent coordinates.
C    Inputs:
C      JD      Julian date corresponding to instant of observation
C              (e.g., 2443468.13472)
C      EQUIN   mean equinox (e.g., 1950.0)
C      DELDAT  interpolation interval in days.  (e.g., .1)
C      RAMEAN  mean RA in radians.
C      DECMEA  mean DEC in radians.
C      RAAPP   apparent RA in radians.
C      DECAPP  apparent DEC in radians.
C      DIR     .T. for conversion from mean to apparent
C              .F. for conversion from apparent to mean
C      GR      .T. to include (solar) general relativistic deflection
C                  in apparent position
C              .F. to not do so
C      DA      .T. to include diurnal aberration in apparent position
C              .F. to not do so
C      RHOGEO  geocentric radius to the point of observation, expressed
C              in units of Earth's equatorial radius.  This is used only
C              when diurnal aberration is applied.
C      PHIGEO  geocentric latitude of the observatory.  This is used
C              only when diurnal aberration is applied.
C      RLST    local sidereal time.  This is used only when diurnal
C              aberration is applied.
C      NR      .T. to use non-rigid Earth model in calculation of
C                 nutation
C              .F. to not do so.  i.e., to use standard rigid Earth.
C-----------------------------------------------------------------------
      LOGICAL   DIR, GR, DA, NR
      DOUBLE PRECISION JD, EQUIN, DELDAT, RAMEAN, DECMEA, RAAPP,
     *   DECAPP, RHOGEO, PHIGEO, RLST
      DOUBLE PRECISION CC, KON, HA, DRA, DDEC, RAP, DAP, SD, CD
C-----------------------------------------------------------------------
      CC = ATAN (1.D0) / 162000.D0
C                                       Conversion from mean to apparent
      IF (.NOT.DIR) GO TO 30
         CALL DMAP (JD, EQUIN, DELDAT, RAMEAN, DECMEA, RAAPP, DECAPP,
     *      NR)
         IF (.NOT.DA) GO TO 10
            KON = .3198D0 * RHOGEO * COS(PHIGEO) * CC
            HA = RLST - RAAPP
            DRA = KON * COS(HA) / COS(DECAPP)
            DDEC = KON * SIN(HA) * SIN(DECAPP)
            RAAPP = RAAPP + DRA
            DECAPP = DECAPP + DDEC
 10      IF (.NOT.GR) GO TO 20
            SD = SIN(DECAPP)
            CD = COS(DECAPP)
            CALL GRD (JD, RAAPP, DECAPP, SD, CD, DRA, DDEC)
            RAAPP = RAAPP + DRA
            DECAPP = DECAPP + DDEC
 20      GO TO 999
C                                       Conversion from apparent to mean
 30   CONTINUE
         RAP = RAAPP
         DAP = DECAPP
         IF (.NOT.DA) GO TO 40
            KON = .3198D0 * RHOGEO * COS(PHIGEO) * CC
            HA = RLST - RAP
            DRA = KON * COS(HA) / COS(DAP)
            DDEC = KON * SIN(HA) * SIN(DAP)
            RAP = RAP - DRA
            DAP = DAP - DDEC
 40      IF (.NOT.GR) GO TO 50
            SD = SIN(DAP)
            CD = COS(DAP)
            CALL GRD (JD, RAP, DAP, SD, CD, DRA, DDEC)
            RAP = RAP - DRA
            DAP = DAP - DDEC
 50      CALL DAPM (JD, EQUIN, DELDAT, RAP, DAP, RAMEAN, DECMEA, NR)
C
 999  RETURN
      END

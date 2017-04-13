      SUBROUTINE JABER (DATJUL, EQUIN, DIURN, RHOGEO, PHIGEO, RLST,
     *   POSO, VELO)
C-----------------------------------------------------------------------
C! Compute vectors needed for J2000 aberation and GR light bending.
C# Math Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Routine to compute vectors needed for corrections to true positions
C   due to aberation  and gravitational deflection of light by the sun.
C   Approximations and formulae mostly from Astronomical Almanac and
C   Starlink routine SLAEVP.
C    Inputs:
C     DATJUL      D  Julian date.
C     EQUIN       D  Epoch of mean equinox (e.g. 2000.0)
C     DIURN       L  true if diurnal aberation correction desired.
C     RHOGEO      D  geocentric radius to the point of observation,
C                    expressed in units of Earth's equatorial radius.
C                    This is used only when diurnal aberration is
C                    applied.
C     PHIGEO      D  geocentric latitude of the observatory.  This is
C                    used only when diurnal aberration is applied.
C                    (Radians).
C     RLST        D  local sidereal time.  This is used only when
C                    diurnal aberration is applied. (Radians)
C    Output:
C     POSO(3)     D  Heliocentric position of observer (AU)
C     VELO(3)     D  Barycentric velocity of observer (AU/day)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, EQUIN, RHOGEO, PHIGEO, RLST, POSO(3),
     *   VELO(3)
      LOGICAL   DIURN
      DOUBLE PRECISION DVB(3), DPB(3), DVH(3), DPH(3), CON
      DOUBLE PRECISION EQRAU, OMEGA
C                                       EQRAU = Earth's equatorial
C                                       radius in AU.
      PARAMETER (EQRAU = 6378140.0D0 / 1.49597870D11)
C                                       OMEGA = Earth's rotation rate in
C                                       rotations per day.
C      PARAMETER (OMEGA = 360.9856D0 / 360.0D0)
      PARAMETER (OMEGA = 1.002737778D0)
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Earth positions and motions.
      CALL SLAEVP (DATJUL, EQUIN, DVB, DPB, DVH, DPH)
C                                       Convert units
      VELO(1) = DVB(1) * 86400.0D0
      VELO(2) = DVB(2) * 86400.0D0
      VELO(3) = DVB(3) * 86400.0D0
      POSO(1) = DPH(1)
      POSO(2) = DPH(2)
      POSO(3) = DPH(3)
C                                       Diurnal effects
      IF (DIURN) THEN
C                                       Position
         CON = EQRAU * RHOGEO
         POSO(1) = POSO(1) + CON * COS (PHIGEO) * COS (RLST)
         POSO(2) = POSO(2) + CON * COS (PHIGEO) * SIN (RLST)
         POSO(3) = POSO(3) + CON * SIN (PHIGEO)
C                                       Velocity
C         CON = CON * OMEGA
         CON = CON * OMEGA * TWOPI
         VELO(1) = VELO(1) - CON * COS (PHIGEO) * SIN (RLST)
         VELO(2) = VELO(2) + CON * COS (PHIGEO) * COS (RLST)
         END IF
 999  RETURN
      END

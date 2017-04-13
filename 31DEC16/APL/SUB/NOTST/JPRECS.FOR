      SUBROUTINE JPRECS (JD, EQUIN, DELDAT, DIR, GR, OBSPOS, POLAR,
     *   RAMEAN, DECMEA, RAAPP, DECAPP)
C-----------------------------------------------------------------------
C! Precess between apparent and J2000 epoch positions.
C# Math Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2004
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
C   Routine to precess positions using the Julian IAU 1984 conventions,
C   (i.e. J2000 positions).  Optional corrections can be made for
C   relativistic bending of the light by the sun, diurnal aberration and
C   polar motion.  Proper motion and parallax are assumed negligible.
C   Several bugs which prevented exact returning back to the mean
C   coordinates (DIR=1 => DIR=-1) are fixed by L. Kogan August 8, 2001
C   Starting Dec 2004 the routine can work for both J2000 and B1950.
C   If input mean coordinates (DIR=1) are B1950 (EQUIN=1950D0) they are
C   converted to J2000 at the beginning by inserted routine B2J.
C   If EQUIN=1950.D0 and DIR=-1, the apparent coordinates are converted
C   to the J2000 mean coordinates and then converted to B1950 by
C   inserted routine J2B.
C   Inputs:
C     JD       D  Julian date of observation (e.g. 2446754.123445).
C     EQUIN    D  Epoch of mean equinox (e.g. 2000.0)
C     DELDAT   D  Interpolation interval; compute precession etc.
C                 parameters at this interval and do a linear
C                 interpolation (days).
C     DIR      I  1 => convert from mean to apparent;
C                -1 => convert from apparent to mean.
C     GR       L  If true correct apparent position for the general
C                 relativistic bending of light by the sun.
C     OBSPOS(3)D  Earth centered location of the observations.
C                 If non zero then diurnal aberation corrections are
C                 made for this location.
C                 1 = Latitude (radians)
C                 2 = East longitude
C                 3 = radius from earth center (meter).
C     POLAR(2) R  X and Y position of the pole (arcsec.)
C                 If non zero then the apparent position is corrected
C                 for the position of the pole.
C                 Note: this correction is not desirable if the antenna
C                 positions are corrected for polar motion.
C    Input/Output:
C     RAMEAN   D  Right ascension at the mean epoch (radians).
C     DECMEA   D  Declination at mean epoch.
C     RAAPP    D  Apparent Right ascension at JD and OBSPOS.
C     DECAPP   D  Apparent declination.
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, EQUIN, DELDAT, OBSPOS(3), RAMEAN, DECMEA,
     *   RAAPP, DECAPP
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION DR1950, DD1950, BEPOCH, EQUINN
      REAL      POLAR(2)
      INTEGER   DIR
      LOGICAL   GR
      INTEGER   ITEMP, I, J, DIROLD
      LOGICAL   DIURN
      DOUBLE PRECISION JDINT(2), PRNMAT(3,3,2), POSO(3,2), VELO(3,2),
     *   RLST, POS(3), WT1, WT2, TU, GMST, TIME, ROTI(3,3), POSOI(3),
     *   VELOI(3), CONST, V(3), BETA, PDOTV, CONST2, OUT(3), RHOGEO,
     *   GMST0, OBSOLD(3)
C
      DOUBLE PRECISION LENGTH, POSSQR, OUTSQR, VSQR, SD, CD, DRA,
     *   DDEC, AYX, AZX, BYX, BZX
      INCLUDE 'INCS:PSTD.INC'
      SAVE JDINT, PRNMAT, POSO, VELO, OBSOLD, DIROLD
      DATA JDINT, OBSOLD /5*0.0D0/
      DATA DIROLD /100/
C-----------------------------------------------------------------------
C                                       Stmt. fn. to compute GMST at
C                                       IAT=0 in radians.
      GMST0 (TU) = MOD (((((((-6.2D-6 * TU) + 0.093104D0) *
     *   TU) + 8640184.812866D0) * TU + 24110.54841D0) * TWOPI /
     *   86400.0D0), TWOPI)
C-----------------------------------------------------------------------
C                                       save input EQUIN
      EQUINN = EQUIN
      EQUIN = 2000.0D0
C                                       Convert to rectangular
C                                       coordinates.
      IF (DIR.GT.0) THEN
C                                       convert B1950 to J2000
         IF (EQUINN .EQ. 1950.0D0) THEN
            BEPOCH = 1979.3D0
            CALL B2J (RAMEAN, DECMEA, BEPOCH, RAMEAN, DECMEA)
            END IF
         POS(1) = COS (RAMEAN) * COS (DECMEA)
         POS(2) = SIN (RAMEAN) * COS (DECMEA)
         POS(3) = SIN (DECMEA)
      ELSE
         POS(1) = COS (RAAPP) * COS (DECAPP)
         POS(2) = SIN (RAAPP) * COS (DECAPP)
         POS(3) = SIN (DECAPP)
         END IF
C                                       New set of interpolation values?
      IF (((JD.LT.JDINT(1)) .OR. (JD.GT.JDINT(2))) .OR.
     *      (DIR.NE.DIROLD) .OR.
     *      ((ABS (OBSPOS(1)-OBSOLD(1)) + ABS (OBSPOS(2)-OBSOLD(2)) +
     *      ABS (OBSPOS(3)-OBSOLD(3))).GT.1.0D-5)) THEN
         JDINT(1) = JD
         JDINT(2) = JD + DELDAT
         OBSOLD(1) = OBSPOS(1)
         OBSOLD(2) = OBSPOS(2)
         OBSOLD(3) = OBSPOS(3)
         DIROLD = DIR
C                                       Beginning of interpolation
C                                       Precession and nutation
         CALL JPRENU (DIR, JDINT(1), EQUIN, .TRUE., PRNMAT(1,1,1))
C                                       Aberation and light bending
         DIURN = (ABS (OBSPOS(3)) .GT. 1.0D-5)
         RHOGEO = 0.0
         RLST = 0.0
C                                       Following for diurnal aberation.
         IF (DIURN) THEN
            RHOGEO = OBSPOS(3) / 6378140.0D0
            ITEMP = JDINT(1)
            TIME = (JDINT(1) - ITEMP - 0.5D0) * TWOPI * 1.002737778D0
            TU = (JDINT(1) - 2451545.0D0) / 36525.0D0
            GMST = GMST0 (TU) + TIME
            RLST = OBSPOS(2) + GMST
            END IF
         CALL JABER (JDINT(1), EQUIN, DIURN, RHOGEO, OBSPOS(1), RLST,
     *      POSO(1,1), VELO(1,1))
C                                       End on interpolation,
C                                       don't interpolate less than 1
C                                       sec.
         IF (DELDAT.GT.1.15741D-5) THEN
C                                       Precession and nutation
            CALL JPRENU (DIR, JDINT(2), EQUIN, .TRUE., PRNMAT(1,1,2))
C                                       Following for diurnal aberation.
            IF (DIURN) THEN
               ITEMP = JDINT(2)
               TIME = (JDINT(2) - ITEMP - 0.5D0) * TWOPI * 1.002737778D0
               TU = (JDINT(2) - 2451545.0D0) / 36525.0D0
               GMST = GMST0 (TU) + TIME
               RLST = OBSPOS(2) + GMST
               END IF
            CALL JABER (JDINT(2), EQUIN, DIURN, RHOGEO, OBSPOS(1), RLST,
     *         POSO(1,2), VELO(1,2))
            END IF
         END IF
C                                       Interpolate vectors and matrix.
      WT1 = 1.0D0
      IF (DELDAT.GT.1.15741D-5)
     *   WT1 = (JDINT(2) - JD) / (JDINT(2) - JDINT(1))
      WT2 = 1.0D0 - WT1
      DO 60 I = 1,3
         POSOI(I) = WT1 * POSO(I,1) + WT2 * POSO(I,2)
         VELOI(I) = WT1 * VELO(I,1) + WT2 * VELO(I,2)
         DO 50 J = 1,3
            ROTI(J,I) = WT1 * PRNMAT(J,I,1) + WT2 * PRNMAT(J,I,2)
 50         CONTINUE
 60      CONTINUE
C                                       Reduce position.
      IF (DIR.GT.0) THEN
C                                       Mean to apparent.
C                                       Light deflection
C                                       using the routine GRD
         IF (GR) THEN
            SD = SIN(DECMEA)
            CD = COS(DECMEA)
            CALL GRD (JD, RAMEAN, DECMEA, SD, CD, DRA, DDEC)
            RAAPP = RAMEAN + DRA
            DECAPP= DECMEA + DDEC
            POS(1) = COS (RAAPP) * COS (DECAPP)
            POS(2) = SIN (RAAPP) * COS (DECAPP)
            POS(3) = SIN (DECAPP)
            END IF
C                                       Aberation
         V(1) = VELOI(1) * 0.0057755D0
         V(2) = VELOI(2) * 0.0057755D0
         V(3) = VELOI(3) * 0.0057755D0
         BETA = SQRT (1.0D0 - (V(1)*V(1) + V(2)*V(2) + V(3)*V(3)))
         PDOTV = POS(1)* V(1) + POS(2)*V(2) + POS(3)*V(3)
C
         CONST = (1.0D0 + (PDOTV / (1.0D0 + BETA))) / (1.0D0 + PDOTV)
         CONST2 = BETA / (1.0D0 + PDOTV)
         POS(1) = POS(1)*CONST2 + CONST*V(1)
         POS(2) = POS(2)*CONST2 + CONST*V(2)
         POS(3) = POS(3)*CONST2 + CONST*V(3)
C                                       Precession and nutation
         OUT(1) = POS(1)*ROTI(1,1) + POS(2)*ROTI(2,1) + POS(3)*ROTI(3,1)
         OUT(2) = POS(1)*ROTI(1,2) + POS(2)*ROTI(2,2) + POS(3)*ROTI(3,2)
         OUT(3) = POS(1)*ROTI(1,3) + POS(2)*ROTI(2,3) + POS(3)*ROTI(3,3)
C
         LENGTH = SQRT(OUT(1)*OUT(1) + OUT(2)*OUT(2) + OUT(3)*OUT(3))
         OUT(1) = OUT(1)/LENGTH
         OUT(2) = OUT(2)/LENGTH
         OUT(3) = OUT(3)/LENGTH
      ELSE
C                                       Apparent to mean
C                                       Precession and nutation
         OUT(1) = POS(1)*ROTI(1,1) + POS(2)*ROTI(2,1) + POS(3)*ROTI(3,1)
         OUT(2) = POS(1)*ROTI(1,2) + POS(2)*ROTI(2,2) + POS(3)*ROTI(3,2)
         OUT(3) = POS(1)*ROTI(1,3) + POS(2)*ROTI(2,3) + POS(3)*ROTI(3,3)
C                                       Aberation
         V(1) = VELOI(1) * 0.0057755D0
         V(2) = VELOI(2) * 0.0057755D0
         V(3) = VELOI(3) * 0.0057755D0
C                                       let's calculate LENGTH.
C                                       The vector out*length is equal
C                                       to vector at DIR=1 before
C                                       precession+nutation
         BETA = SQRT (1.0D0 - (V(1)*V(1) + V(2)*V(2) + V(3)*V(3)))
         PDOTV = POS(1)* V(1) + POS(2)*V(2) + POS(3)*V(3)
C
         CONST = (1.0D0 + (PDOTV / (1.0D0 + BETA))) / (1.0D0 + PDOTV)
         CONST2 = BETA / (1.0D0 + PDOTV)
         POSSQR = POS(1)*POS(1) + POS(2)*POS(2) + POS(3)*POS(3)
         OUTSQR = OUT(1)*OUT(1) + OUT(2)*OUT(2) + OUT(3)*OUT(3)
         VSQR = V(1)*V(1) + V(2)*V(2) +V(3)*V(3)
C
         LENGTH = SQRT((POSSQR*CONST2*CONST2 + VSQR*CONST*CONST
     *      + 2*PDOTV*CONST2*CONST) / OUTSQR)
C
         OUT(1) = OUT(1)*LENGTH
         OUT(2) = OUT(2)*LENGTH
         OUT(3) = OUT(3)*LENGTH
C                                       Solve the system of equations
C                                       to find nonaberrated coordinates
C                                       from aberrated ones
         AYX = (OUT(2) - V(2)/(1+BETA)) / (OUT(1) - V(1)/(1+BETA))
         AZX = (OUT(3) - V(3)/(1+BETA)) / (OUT(1) - V(1)/(1+BETA))
         BYX = (AYX*V(1) - V(2)) / (1+BETA)
         BZX = (AZX*V(1) - V(3)) / (1+BETA)
C                                       Solution of the equation
C                                       for OUT(1)
         OUT(1) = ((1 + BYX*V(2) + BZX*V(3))*(OUT(1) - V(1)/(1+BETA))
     *      - (BETA/(1+BETA))*V(1)) / (BETA - (OUT(1) - V(1)/(1+BETA))
     *      * (V(1) + AYX*V(2) + AZX*V(3)))
C                                       find OUT(2) and OUT(3)
C                                       knowing OUT(1)
         OUT(2) = BYX + AYX*OUT(1)
         OUT(3) = BZX + AZX*OUT(1)
C                                       Light deflection?
C                                       using the routine GRD
         IF (GR) THEN
            RAAPP = ATAN2 (OUT(2), OUT(1))
            IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + TWOPI
            DECAPP = ASIN (OUT(3))
            SD = SIN(DECAPP)
            CD = COS(DECAPP)
            CALL GRD (JD, RAAPP, DECAPP, SD, CD, DRA, DDEC)
            RAMEAN = RAAPP - DRA
            DECMEA = DECAPP -  DDEC
            OUT(1) = COS (RAMEAN) * COS (DECMEA)
            OUT(2) = SIN (RAMEAN) * COS (DECMEA)
            OUT(3) = SIN (DECMEA)
            END IF
         END IF
C                                       Polar motion
      IF ((ABS (POLAR(1)) + ABS (POLAR(2))).GT.1.0E-20)
     *   CALL JPOLAR (DIR, POLAR(1), POLAR(2), OUT)
C                                       Convert to spherical coord.
C                                       Mean to apparent
      IF (DIR.GT.0) THEN
         RAAPP = ATAN2 (OUT(2), OUT(1))
         IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + TWOPI
         DECAPP = ASIN (OUT(3))
C                                       Apparent to mean
      ELSE
         RAMEAN = ATAN2 (OUT(2), OUT(1))
         IF (RAMEAN.LT.0.0D0) RAMEAN = RAMEAN + TWOPI
         DECMEA = ASIN (OUT(3))
C                                       convert calculated J2000
C                                       coordinates back to B1950
         IF (EQUINN .EQ. 1950.D0) THEN
            BEPOCH = 1979.3D0
            CALL J2B (RAMEAN, DECMEA, BEPOCH, RAMEAN,
     *            DECMEA, DR1950, DD1950)
            END IF
         END IF
C                                       restore input EQUIN
      EQUIN = EQUINN

C
 999  RETURN
      END

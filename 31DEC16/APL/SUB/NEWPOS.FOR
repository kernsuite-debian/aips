      SUBROUTINE NEWPOS (TYPE, RA0, DEC0, L, M, RAOUT, DECOUT, IERR)
C-----------------------------------------------------------------------
C! returns astronomical coordinates given direction cosines, projection
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2002, 2013
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
C   NEWPOS determines the coordinates (RAOUT,DECOUT) corresponding to a
C   displacement (L,M) given by the direction cosines from coordinates
C   (RA0,DEC0).  The direction cosine L is assumed to be positive to
C   the east; M is positive to the north.  The routine works for
C   4 kinds of projective geometries and for Celestial, Ecliptic, or
C   Galactic coordinate systems.
C   This subroutine always uses an accurate computation.
C   Inputs:
C      TYPE  I    2 = SIN projection, 3 = TAN projection, 4 = ARC
C                 projection, 5 = NCP, 6 = STG, 7 = GLS (SFL now),
C                 8 = MER, 9 = AIT, 10 = CAR, 11 = MOL, 12 = PAR
C      RA0   D    Coordinate reference right ascension (longitude)
C      DEC0  D    Coordinate reference declination (latitude)
C      L     D    Cosine angle of displacement to east
C      M     D    Cosine angle of displacement to north
C   Outputs:
C      RAOUT  D    right ascension or longitude at (L,M)
C      DECOUT D    declination or latitude at (L,M)
C      IERR   I    Error condition: 0 = ok, 1 = L,M crazy, 2 = bad
C                        type,  3 = answer undefined
C   ALL ANGLES IN THIS SUBROUTINE ARE IN RADIANS.
C-----------------------------------------------------------------------
      INTEGER   TYPE, IERR
      DOUBLE PRECISION RA0, DEC0, L, M, RAOUT, DECOUT
C
      DOUBLE PRECISION SINS, COSS, DECT, RAT, DT, DEPS, MG, DA, DZ,
     *   COS0, SIN0, THETA, PHI
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA DEPS /1.D-5/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.12)) THEN
         WRITE (MSGTXT,1000) TYPE
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       angle too large
      IERR = 1
      SINS = L*L + M*M
      IF (SINS.LE.1.0D0) GO TO 10
      IF (TYPE.GE.6) GO TO 10
      IF ((TYPE.NE.4) .OR. (SINS.GT.TWOPI*TWOPI/4.0D0)) GO TO 999
C                                       math errors
 10   IERR = 3
      DECOUT = 0.D0
      RAOUT = 0.D0
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
C                                       Use accurate solution
C                                       SIN projection
      IF (TYPE.EQ.2) THEN
         COSS = SQRT (1.0D0 - SINS)
         DT = SIN0 * COSS + COS0 * M
         IF (ABS(DT).GT.1.0D0) GO TO 999
         DECT = ASIN (DT)
         RAT = COS0 * COSS - SIN0 * M
         IF ((RAT.EQ.0.D0) .AND. (L.EQ.0.0D0)) GO TO 999
         RAT = ATAN2 (L, RAT) + RA0
C                                       TAN projection
      ELSE IF (TYPE.EQ.3) THEN
         DECT = COS0 - M * SIN0
         IF (DECT.EQ.0.0D0) GO TO 999
         RAT = RA0 + ATAN2 (L, DECT)
         DECT = ATAN (COS(RAT-RA0) * (M * COS0 + SIN0) / DECT)
C                                       Arc projection
      ELSE IF (TYPE.EQ.4) THEN
         SINS = SQRT(SINS)
         COSS = COS (SINS)
         IF (SINS.NE.0.0D0) THEN
            SINS = SIN (SINS) / SINS
         ELSE
            SINS = 1.0D0
            END IF
         DT = M * COS0 * SINS + SIN0 * COSS
         IF (ABS(DT).GT.1.0D0) GO TO 999
         DECT = ASIN (DT)
         DA = COSS - DT * SIN0
         DT = L * SINS * COS0
         IF ((DA.EQ.0.0D0) .AND. (DT.EQ.0.0D0)) GO TO 999
         RAT = RA0 + ATAN2 (DT, DA)
C                                       WSRT (North pole projection)
      ELSE IF (TYPE.EQ.5) THEN
         DECT = COS0 - M * SIN0
         IF (DECT.EQ.0.0D0) GO TO 999
         RAT = RA0 + ATAN2 (L, DECT)
         DT = COS (RAT-RA0)
         IF (DT.EQ.0.0D0) GO TO 999
         DECT = DECT / DT
         IF (ABS(DECT).GT.1.0D0) GO TO 999
         DECT = ACOS (DECT)
         IF (DEC0.LT.0.0D0) DECT = -DECT
C                                       Stereographic
      ELSE IF (TYPE.EQ.6) THEN
         DZ = (4.0D0 - SINS) / (4.0D0 + SINS)
         IF (ABS(DZ).GT.1.0D0) GO TO 999
         DECT = DZ * SIN0 + M * COS0 * (1.0D0+DZ) / 2.0D0
         IF (ABS(DECT).GT.1.0D0) GO TO 999
         DECT = ASIN (DECT)
         RAT = COS(DECT)
         IF (ABS(RAT).LT.DEPS) GO TO 999
         RAT = L * (1.0D0+DZ) / (2.0D0 * RAT)
         IF (ABS(RAT).GT.1.0D0) GO TO 999
         RAT = ASIN (RAT)
         MG = 1.0D0 + SIN(DECT) * SIN0 +
     *      COS(DECT) * COS0 * COS(RAT)
         IF (ABS(MG).LT.DEPS) GO TO 999
         MG = 2.0D0 * (SIN(DECT) * COS0 - COS(DECT) * SIN0
     *      * COS(RAT)) / MG
         IF (ABS(MG-M).GT.DEPS) RAT = TWOPI/2.0D0 - RAT
         RAT = RA0 + RAT
C                                       Modern WCS-II concepts
      ELSE
C                                       Global sinusoid
         IF (TYPE.EQ.7) THEN
            THETA = M
            IF (ABS(THETA).GT.TWOPI/4.0D0) GO TO 999
            COSS = COS (THETA)
            IF (ABS(L).GT.TWOPI*COSS/2.0D0) GO TO 999
            PHI = 0.0D0
            IF (COSS.GT.DEPS) PHI = PHI + L / COSS
C                                       Mercator
         ELSE IF (TYPE.EQ.8) THEN
            PHI = L
            IF (ABS(PHI).GT.TWOPI) GO TO 999
            DT = EXP (M)
            THETA = 2.0D0 * ATAN (DT) - TWOPI / 4.0D0
C                                       Hammer-Aitoff
         ELSE IF (TYPE.EQ.9) THEN
            DT = 1.0D0 - (L/4.0D0)**2 - (M/2.0D0)**2
            IF (DT.LT.0.5D0) GO TO 999
            DT = SQRT (DT)
            DA = M * DT
            IF (ABS(DA).GT.1.0D0) GO TO 999
            THETA = ASIN (DA)
            DA = 2.0D0*DT*DT - 1.0D0
            DZ = L * DT / 2.0D0
            IF ((DA.EQ.0.0D0) .AND. (DZ.EQ.0.0D0)) GO TO 999
            PHI = 2.0D0 * ATAN2 (DZ, DA)
C                                       Plate Carree (-CAR)
         ELSE IF (TYPE.EQ.10) THEN
            PHI = L
            THETA = M
C                                       Molweide's
         ELSE IF (TYPE.EQ.11) THEN
            IF (L*L/4.D0 + M*M.GT.2.0D0) GO TO 999
            PHI = PI * L / (2.0D0 * SQRT (2.0D0 - M*M))
            THETA = ASIN (2.0D0/PI * ASIN (M/SQRT(2.0D0))) +
     *         M/PI * SQRT (2.0D0 - M*M)
C                                       Parabolic
         ELSE IF (TYPE.EQ.12) THEN
            IF (PI*ABS(L)+4*M*M.GT.PI*PI) GO TO 999
            THETA = 3.0D0 * ASIN (M/PI)
            PHI = L / (1.0D0-(2.0D0*M/PI)**2)
            END IF
C                                       Native longitude/latitude
C                                       to celestial
         CALL NATCEL (PHI, THETA, GEOMD1(LOCNUM), GEOMD2(LOCNUM),
     *      GEOMD3(LOCNUM), RAT, DECT)
         END IF
C                                       Return: in range RA
 900  RAOUT = RAT
      DECOUT = DECT
      IERR = 0
      IF (RAOUT-RA0.GT.TWOPI/2.0D0) RAOUT = RAOUT - TWOPI
      IF (RAOUT-RA0.LT.-TWOPI/2.0D0) RAOUT = RAOUT + TWOPI
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NEWPOS: ILLEGAL PROJECTION TYPE',I4,' INPUT')
      END

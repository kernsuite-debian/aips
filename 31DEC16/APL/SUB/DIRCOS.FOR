      SUBROUTINE DIRCOS (TYPE, RA0, DEC0, RA, DEC, L, M, IERR)
C-----------------------------------------------------------------------
C! determines direction cosines between ref position and test position
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2002, 2013
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
C   DIRCOS determines the direction cosines (L,M) corresponding to
C   the separation from (RA0,DEC0), the reference pixel, to
C   (RA,DEC).  The direction cosine L is assumed to be positive to the
C   east; M is positive to the north.  Several projective geometries
C   are supported.
C   Inputs:
C      TYPE   I    Type of projection: 2 => SIN, 3 => TAN, 4 => Arc,
C                  5 => WSRT (rel north pole), 6 Global sinusoidal,
C                  7 Mercator, 8 Aitoff, 9 STG, 10 CAR, 11 MOL,
C                  12 PAR
C      RA0    D    Coordinate reference right ascension/longitude
C      DEC0   D    Coordinate reference declination/latitude
C      RA     D    right ascension/longitude of point
C      DEC    D    declination/latitude of point
C   Outputs:
C      L      D    Cosine angle of displacement to east
C      M      D    Cosine angle of displacement to north
C      IERR   I    0 ok, 1 out of range, 2 bad type, 3 undefined
C   ALL ANGLES ARE IN RADIANS.
C-----------------------------------------------------------------------
      INTEGER   IERR, TYPE
      DOUBLE PRECISION RA0, DEC0, RA, DEC, L, M
C
      DOUBLE PRECISION DEPS, COSS, SINS, DT, DA, DD, SINT, PHI, THETA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA DEPS /1.0D-5/
C-----------------------------------------------------------------------
C                                       Use full accuracy
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.12)) GO TO 999
      IERR = 3
      IF (ABS(DEC).GT.PI/2.0D0) GO TO 999
      IF (ABS(DEC0).GT.PI/2.0D0) GO TO 999
      IERR = 0
      COSS = COS (DEC)
      SINS = SIN (DEC)
      DT = RA - RA0
      IF (DT.GT.PI) DT = DT - TWOPI
      IF (DT.LT.-PI) DT = DT + TWOPI
      L = SIN(DT) * COSS
      SINT = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(DT)
C                                       SIN projection
      IF (TYPE.EQ.2) THEN
         IF (SINT.LT.0.0D0) IERR = 1
         M = SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(DT)
C                                       TAN projection
      ELSE IF (TYPE.EQ.3) THEN
         IF (SINT.LE.0.0D0) THEN
            IERR = 1
         ELSE
            M = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(DT)
            L = L / M
            M = (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(DT)) / M
            END IF
C                                       Arc projection
      ELSE IF (TYPE.EQ.4) THEN
         M = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(DT)
         M =  MIN (1.0D0, MAX (-1.0D0, M))
         M = ACOS (M)
         IF (M.NE.0.0D0) THEN
            M = M / SIN(M)
         ELSE
            M = 1.0D0
            END IF
         L = L * M
         M = (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(DT)) * M
         GO TO 999
C                                       WSRT projection
      ELSE IF (TYPE.EQ.5) THEN
         IF (DEC0.NE.0.0D0) THEN
            M = (COS(DEC0) - COSS * COS(DT)) / SIN(DEC0)
         ELSE
            IERR = 3
            WRITE (MSGTXT,1050)
            CALL MSGWRT (6)
            L = 0.0D0
            M = 0.0D0
            END IF
C                                       Stereographic
      ELSE IF (TYPE.EQ.6) THEN
         DD = 1.0D0 + SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(DT)
         IF (ABS(DD).LT.DEPS) THEN
            IERR = 3
         ELSE
            DD = 2.0D0 / DD
            L = L * DD
            M = DD * (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(DT))
            END IF
C                                       modern ones
      ELSE
C                                       get native long/lat
         CALL CELNAT (RA, DEC, GEOMD1(LOCNUM), GEOMD2(LOCNUM),
     *      GEOMD3(LOCNUM), PHI, THETA)
C                                       Global sinusoidal
         IF (TYPE.EQ.7) THEN
            M = THETA
            L = PHI * COS(THETA)
C                                       Mercator
         ELSE IF (TYPE.EQ.8) THEN
            L = PHI
            DT = TAN ((PI + THETA) / 2.0D0)
            IF (DT.LE.0) THEN
               IERR = 3
            ELSE
               M = LOG (DT)
               END IF
C                                       Aitoff
         ELSE IF (TYPE.EQ.9) THEN
            DT = SQRT ((1.0D0 + COS(THETA) * COS(PHI/2.D0)) / 2.0D0)
            IF (ABS(DT).LT.DEPS) THEN
               IERR = 3
            ELSE
               DT = 1.0 / DT
               L = 2.0D0 * DT * COS(THETA) * SIN(PHI/2.0)
               M = DT * SIN (THETA)
               DA = (L/4.0D0)**2 + (M/2.0D0)**2
               IF (DA.GT.0.5000009D0) IERR = 3
               END IF
C                                       Plate Carree (-CAR)
         ELSE IF (TYPE.EQ.10) THEN
            L = PHI
            M = THETA
C                                       Molweide
         ELSE IF (TYPE.EQ.11) THEN
            CALL MOLGAM (THETA, DT, IERR)
            IF (IERR.EQ.0) THEN
               L = SQRT (8.0D0) * PHI * COS (DT) / PI
               M = SQRT (2.0D0) * SIN (DT)
               END IF
C                                       parabolic
         ELSE IF (TYPE.EQ.12) THEN
            L = PHI * (2.0D0*COS(2.0D0*THETA/3.0D0) - 1.0D0)
            M = PI * SIN (THETA / 3.0D0)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('DIRCOS: -NCP COORDINATES CANNOT HAVE DEC0 = 0')
      END

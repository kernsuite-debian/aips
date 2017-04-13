      SUBROUTINE DIRRA (TYPE, DX, DEC, RA0, DEC0, ROTA, DY, RA, IERR)
C-----------------------------------------------------------------------
C! finds latitude pixel and longitude given longitude pixel and latitude
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013-2014
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
C   DIRRA will find the longitude and the latitude pixel position of a
C   point given the latitude and the longitude pixel of the point.
C   For use with four projective geometries.
C   ALL ANGLES ARE IN RADIANS.
C   Inputs:
C      TYPE  I      2 => SIN, 3 => TAN, 4 => Arc, 5 => WSRT geometry
C                   6 Global sinusoidal, 7 Mercator, 8 Aitoff
C      DX    D      X pixel value from ref pixel
C      DEC   D      declination/ latitude
C      RA0   D      RA / longitude of ref pixel
C      DEC0  D      DEC / latitude of ref pixel
C      ROTA   D      Rotation of coordinates
C   Output:
C      DY    D      Y pixel position
C      RA    D      right ascension/ longitude
C      IERR  D      0 ok, 1 out of range, 2 bad type, 3 undefined answer
C-----------------------------------------------------------------------
      INTEGER   IERR, TYPE
      DOUBLE PRECISION    DX, DEC, RA0, DEC0, ROTA, DY, RA
C
      INTEGER   NC, NCLIM, I
      DOUBLE PRECISION COSR, SINR, DA, DB, DC, COS0, SIN0, COSD, SIND,
     *   DEPS, DT, ALPHA, DELTA, M, XMIN, XMAX, DM, MVAL, DDIST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA DEPS /1.0D-5/
C-----------------------------------------------------------------------
      NCLIM = 1000
      DY = 0.0D0
      RA = 0.0D0
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.12)) GO TO 999
      IERR = 0
      COSR = COS(ROTA)
      SINR = SIN(ROTA)
      COSD = COS(DEC)
      SIND = SIN(DEC)
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
      NC = 0
      IF (ABS(COSD).LT.DEPS) THEN
         IERR = 3
C                                       SIN projection
      ELSE IF (TYPE.EQ.2) THEN
         DA = COSR
         DB = SINR * SIN0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) THEN
            IERR = 3
         ELSE
            DT = (DX - SINR*SIND*COS0) / DT / COSD
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               RA = ASIN (DT) +  ATAN2 (DB, DA)  +  RA0
               DY = SIND * COS0 * COSR  -  COSD * (COSR * SIN0 *
     *            COS(RA-RA0) + SIN(RA-RA0) * SINR)
               END IF
            END IF
C                                       TAN projection
      ELSE IF (TYPE.EQ.3) THEN
         DA = COSR
         DB = SINR * SIN0 + DX * COS0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) THEN
            IERR = 3
         ELSE
            DT = (DX*SIN0 - COS0*SINR) / DT * SIND / COSD
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               RA = ASIN (DT)  + ATAN2 (DB, DA) + RA0
               DT = SIND * SIN0 + COSD * COS0 * COS(RA-RA0)
               IF (ABS(DT).LT.DEPS) THEN
                  IERR = 3
               ELSE
                  DY = (SIND*COS0*COSR - COSD*SIN0*COS(RA-RA0)*COSR -
     *               COSD*SINR*SIN(RA-RA0)) / DT
                  END IF
               END IF
            END IF
C                                       Arc projection
      ELSE IF (TYPE.EQ.4) THEN
         IF (ABS(COSR).LT.DEPS) THEN
            IERR = 3
         ELSE
            IF (ABS(COS0).LE.DEPS) THEN
               IF (ABS(SIN0).LT.DEPS) THEN
                  IERR = 3
               ELSE
                  DY = ACOS (SIND / SIN0)
                  DY = DY*DY - DX*DX
                  IF (DY.LT.0.0D0) THEN
                     IERR = 3
                  ELSE
                     DY = -SQRT (DY)
                     DA = SQRT (DX*DX + DY*DY)
                     DB = 1.0D0
                     IF (DA.NE.0.0D0) DB = SIN(DA) / DA
                     END IF
                  END IF
            ELSE
               DY = 0.0D0
               NC = 0
 45            IF (NC.LE.NCLIM) THEN
                  DA = SQRT (DX*DX + DY*DY)
                  NC = NC + 1
                  DC = DY
                  DB = 1.0D0
                  IF (DA.NE.0.0D0) DB = SIN(DA) / DA
                  DA = COS(DA)
                  DY = (SIND - SIN0*DA - DX*SINR*COS0*DB) / (COSR * COS0
     *               * DB)
                  DA = ABS (DY - DC)
                  IF ((NC.GT.10) .AND. (NC.LE.NCLIM)) DY = (DY+DC)/2.0D0
                  IF (DA.GE.1.0D-9) GO TO 45
                  END IF
               END IF
            END IF
         IF (IERR.EQ.0) THEN
            DT = DB * (DX*COSR - DY*SINR) / COSD
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               RA = RA0 + ASIN (DT)
               END IF
            END IF
C                                       WSRT projection
      ELSE IF (TYPE.EQ.5) THEN
         IF (ABS(SIN0).LT.DEPS) THEN
            IERR = 3
         ELSE
            DA = COSR
            DB = SINR / SIN0
            DT = (DX - COS0*DB) / SQRT (DA*DA + DB*DB) / COSD
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               RA = ASIN (DT) + ATAN2 (DB, DA) + RA0
               DY = (COS0 - COSD*COS(RA-RA0)) * COSR / SIN0  -
     *            COSD * SINR * SIN(RA-RA0)
               END IF
            END IF
C                                       Stereographic
      ELSE IF (TYPE.EQ.6) THEN
         DA = 2.0D0 * COSD * COSR
         DB = 2.0D0 * COSD * SIN0 * SINR + DX * COSD * COS0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) THEN
            IERR = 3
         ELSE
            DT = (DX + DX*SIND*SIN0 - 2.0D0*SIND*COS0*SINR) / DT
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               RA = ASIN (DT) + ATAN2 (DB, DA)
               DY = 1.0D0 + SIND*SIN0 + COSD*COS0*COS(RA)
               IF (ABS(DY).LE.DEPS) THEN
                  IERR = 3
               ELSE
                  DY = 2.0D0 * ((SIND*COS0 - COSD*SIN0*COS(RA)) * COSR -
     *               COSD*SIN(RA) * SINR) / DY
                  RA = RA + RA0
                  END IF
               END IF
            END IF
         GO TO 900
C                                       modern WCS-II globals
      ELSE
         XMIN = MMIN(LOCNUM)
         XMAX = MMAX(LOCNUM)
         DM = (XMAX - XMIN) / 10.0D0
         XMIN = XMIN - DM
         XMAX = XMAX + DM
         NC = 0
         NCLIM = 25
 60      IF (NC.LT.NCLIM) THEN
            NC = NC + 1
            DM = (XMAX - XMIN) / 99.0D0
            DDIST = 1.E10
            M = XMIN
            DO 65 I = 1,100
               CALL NEWPOS (TYPE, RA0, DEC0, DX, M, ALPHA, DELTA, IERR)
               IF ((IERR.EQ.0) .AND. (ABS(DEC-DELTA).LT.DDIST)) THEN
                  DDIST = ABS (DEC-DELTA)
                  MVAL = M
                  DY = M
                  RA = ALPHA
                  END IF
               M = M + DM
 65            CONTINUE
            IF ((DDIST.LT.10.0D0) .AND. (DDIST.GT.1.D-5)) THEN
               XMIN = MVAL - DM * 5.0D0
               XMAX = MVAL + DM * 5.0D0
               GO TO 60
               END IF
            END IF
         IF (DDIST.GE.1.0D-3) IERR = 3
         END IF
C                                       RA in range:
 900  IF (IERR.EQ.0) THEN
         IF (RA-RA0.LT.-TWOPI/2.0D0) RA = RA + TWOPI
         IF (RA-RA0.GT.TWOPI/2.0D0) RA = RA - TWOPI
         IF (NC.GT.NCLIM/2) THEN
            WRITE (MSGTXT,1900) NC
            IF (NC.GT.NCLIM) WRITE (MSGTXT,1901) NC
            CALL MSGWRT (6)
            IF (NC.GT.NCLIM) IERR = 3
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('DIRRA: ITERATIONS REQUIRED = ',I5)
 1901 FORMAT ('DIRRA: FAILED TO CONVERGE AFTER',I5,' ITERATIONS')
      END

      SUBROUTINE DIRDEC (TYPE, DY, RA, RA0, DEC0, ROTA, DX, DEC, IERR)
C-----------------------------------------------------------------------
C! finds longitude pixel and latitude given latitude pixel and longitude
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
C   DIRDEC will find the latitude and longitude-like pixel position
C   of a point given the longitude and the latitude-pixel of the point.
C   ALL ANGLES ARE IN RADIANS.
C   Inputs:
C      TYPE  I      2 => SIN, 3 => TAN, 4 => Arc, 5 => WSRT geometry
C                   6 Global sinusoidal, 7 Mercator, 8 Aitoff,
C                   9 STG, 10 CAR
C      DY    D      the Y pixel pos rel to ref pixel
C      RA    D      right ascension/longitude
C      RA0   D      RA/longitude of ref pixel
C      DEC0  D      DEC/latitude of ref pixel
C      ROT   D      Rotation of coords +dec into +ra
C   Output:
C      DEC   D      declination/latitude
C      DX    D      RA-like pixel position rel to ref pixel
C      IERR  I      0 ok, 1 out of range, 2 bad type, 3 undefined
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DY, RA, RA0, DEC0, ROTA, DX, DEC
      INTEGER   IERR, TYPE
C
      INTEGER   NC, NCLIM, I
      DOUBLE PRECISION    COSR, SINR, DA, DB, DC, DD, COS0, SIN0, COSA,
     *   SINA, DEPS, DE, DT, FG, ALPHA, DELTA, L, XMIN, XMAX, DL, DDIST,
     *   LVAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA DEPS /1.D-5/
C-----------------------------------------------------------------------
      NCLIM = 1000
      DX = 0.0D0
      DEC = 0.0D0
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.12)) GO TO 999
      IERR = 0
      COSR = COS(ROTA)
      SINR = SIN(ROTA)
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
      COSA = COS(RA-RA0)
      SINA = SIN(RA-RA0)
      NC = 0
C                                       SIN projection
      IF (TYPE.EQ.2) THEN
         DA = COSR * COS0
         DB = COSR * SIN0 * COSA  +  SINR * SINA
         IF ((ABS(DA).LT.DEPS) .AND. (ABS(DB).LT.DEPS)) THEN
            IERR = 3
         ELSE
            DT = DY / SQRT(DB*DB + DA*DA)
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               DEC = ASIN (DT) + ATAN2 (DB, DA)
               DX = SINR * SIN(DEC) * COS0  -  COS(DEC) *
     *            (SINR * SIN0 * COSA - COSR * SINA)
               END IF
            END IF
C                                       TAN projection
      ELSE IF (TYPE.EQ.3) THEN
         DA = COS0 * COSR - DY * SIN0
         IF (ABS(DA).LT.DEPS) THEN
            IERR = 3
         ELSE
            DB = (SIN0 * COSA * COSR + SINA * SINR + DY * COS0 * COSA) /
     *         DA
            DEC = ATAN (DB)
            DX = (SINA*COSR + DB*COS0*SINR - SIN0*COSA*SINR) /
     *         (DB * SIN0 + COS0 * COSA)
            END IF
C                                       Arc projection
      ELSE IF (TYPE.EQ.4) THEN
         IF (ABS(COSR).LT.DEPS) THEN
            IERR = 3
         ELSE
            DD = ATAN2 (SIN0, COS0 * COSA)
            DC = SQRT (1.D0 - COS0*COS0*SINA*SINA)
            DX = (DY * SINR + COS0 * SINA) / COSR
            NC = 0
 45         IF (NC.LE.NCLIM) THEN
               DA = SQRT (DX*DX + DY*DY)
               NC = NC + 1
               DE = DX
               DB = 1.0D0
               IF (SIN(DA).NE.0.0D0) DB = DA / SIN(DA)
               DA = COS(DA) / DC
               DEC = DD
               IF (ABS(DA).LT.1.0D0) DEC = DEC + ACOS (DA)
               IF ((DY.LT.0.0D0) .AND. (DEC.GT.DEC0)) DEC = -DEC
     *            + 2.0D0 * DD
               DX = (DY * SINR + DB * COS(DEC) * SINA) / COSR
               IF ((ABS(DX-DE).GE.1.0D-9) .AND. (NC.LE.NCLIM)) THEN
                  IF (NC.GT.10) DX = (DX + DE) / 2.0D0
                  IF ((NC.GT.25) .AND. ((NC/2)*2.EQ.NC)) DX = (DX+DE) /
     *               2.0D0
                  GO TO 45
                  END IF
               END IF
            END IF
C                                       WSRT projection
      ELSE IF (TYPE.EQ.5) THEN
         IF (ABS(SIN0).LT.DEPS) THEN
            IERR = 3
         ELSE
            DA = COSA * COSR + SINA * SIN0 * SINR
            IF (ABS(DA).LT.DEPS) THEN
               IERR = 3
            ELSE
               DT = (COS0 * COSR - DY * SIN0) / DA
               IF (ABS(DT).GT.1.0D0) THEN
                  IERR = 3
               ELSE
                  DEC = ACOS (DT)
                  IF (DEC0.LT.0.0D0) DEC = -DEC
                  DX = COS(DEC) * SINA * COSR + SINR * (COS0 - COS(DEC)
     *               * COSA) / SIN0
                  END IF
               END IF
            END IF
C                                       Stereographic
      ELSE IF (TYPE.EQ.6) THEN
         DA = 2.0D0 * COS0 * COSR - DY * SIN0
         DB = COSA * (2.0D0 * SIN0 * COSR + DY * COS0) +
     *      2.0D0 * SINA * SINR
         DT = SQRT (DA * DA + DB * DB)
         IF (DT.LT.DEPS) THEN
            IERR = 3
         ELSE
            DT = DY / DT
            IF (ABS(DT).GT.1.0D0) THEN
               IERR = 3
            ELSE
               DEC = ATAN2 (DB, DA) + ASIN (DT)
C                                       check
               DC = 1.0D0 + SIN(DEC)*SIN0 + COS(DEC)*COS0*COSA
               FG = -1.0D10
               IF (ABS(DC).GT.DEPS) FG = 2.0D0 * ((SIN(DEC)*COS0 -
     *            COS(DEC)*SIN0*COSA)*COSR - COS(DEC)*SINA*SINR) / DC
               IF (ABS(FG-DY).GT.DEPS) DEC = ATAN2 (DB, DA) - ASIN (DT)
     *            + TWOPI/2.0D0
               DX = 1.0D0 + SIN(DEC) * SIN0 +  COS(DEC) * COS0 * COSA
               IF (ABS(DX).LT.DEPS) THEN
                  IERR = 3
               ELSE
                  FG = -1.0D10
                  IF (ABS(DC).GT.DEPS) FG = 2.0D0 * ((SIN(DEC)*COS0 -
     *               COS(DEC)*SIN0*COSA)*COSR - COS(DEC)*SINA*SINR) / DX
                  IF (ABS(FG-DY).GT.DEPS) THEN
                     IERR = 3
                  ELSE
                     DX = 2.0D0 * (COS(DEC) * SINA * COSR +
     *                  (SIN(DEC)*COS0 - COS(DEC)*SIN0*COSA) * SINR) /
     *                  DX
                     END IF
                  END IF
               END IF
            END IF
C                                       modern WCS-II globals
      ELSE
         XMIN = LMIN(LOCNUM)
         XMAX = LMAX(LOCNUM)
         DL = (XMAX - XMIN) / 10.0D0
         XMIN = XMIN - DL
         XMAX = XMAX + DL
         NC = 0
         NCLIM = 25
 60      IF (NC.LT.NCLIM) THEN
            NC = NC + 1
            DL = (XMAX - XMIN) / 99.0D0
            DDIST = 1.E10
            L = XMIN
            DO 65 I = 1,100
               CALL NEWPOS (TYPE, RA0, DEC0, L, DY, ALPHA, DELTA, IERR)
               IF ((IERR.EQ.0) .AND. (ABS(RA-ALPHA).LT.DDIST)) THEN
                  DDIST = ABS (RA-ALPHA)
                  LVAL = L
                  DX = L
                  DEC = DELTA
                  END IF
               L = L + DL
 65            CONTINUE
            IF ((DDIST.LT.10.0D0) .AND. (DDIST.GT.1.D-5)) THEN
               XMIN = LVAL - DL * 5.0D0
               XMAX = LVAL + DL * 5.0D0
               GO TO 60
               END IF
            END IF
         IF (DDIST.GE.1.0D-3) IERR = 3
         END IF
C                                       Check the loop
 900  IF (IERR.EQ.0) THEN
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
 1900 FORMAT ('DIRDEC: ITERATIONS REQUIRED = ',I5)
 1901 FORMAT ('DIRDEC: FAILED TO CONVERGE AFTER',I5,' ITERATIONS')
      END

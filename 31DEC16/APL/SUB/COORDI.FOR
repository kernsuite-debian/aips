      SUBROUTINE COORDI (N, RAD, CHM, HM, SEC)
C-----------------------------------------------------------------------
C! converts angles between degrees and sexagesimal format
C# Utility Coordinates
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
C   COORDI Converts RA and DEC to and from degrees under control of N.
C   Inputs:
C      N     I      OPcode: 1 degrees to RA
C                           2 degrees to DEC
C                           3 RA to degrees
C                           4 DEC to degrees
C   In/out:
C      RAD   D      The angle in degrees
C      CHM   C*1    The sign of the sexagesimal number (or a digit
C                   for hundreds or days
C      HM    I(2)   The angle in RA/dec as
C                      HM(1)   Hours or Degrees
C                      HM(2)   Minutes of time or arc
C      SEC   D      The rest of the Ra/dec : seconds of time or arc
C   Differs from COORDD only in that SEC is double precision not real
C-----------------------------------------------------------------------
      CHARACTER CHM*1
      INTEGER   HM(2), N
      DOUBLE PRECISION  RAD, SEC
C
      CHARACTER LOCHAR(10)*1
      INTEGER   I
      DOUBLE PRECISION  TEMP, CON, F15, F60
      INCLUDE 'INCS:DMSG.INC'
      DATA LOCHAR /'1','2','3','4','5','6','7','8','9','*'/
C-----------------------------------------------------------------------
C                                       Error in N
      IF ((N.LT.1) .OR. (N.GT.4)) GO TO 900
      F15 = 15.0D0
      F60 = 60.0D0
      CON = 3600.D0
C                                       From degrees
      IF (N.LE.2) THEN
         TEMP = RAD
         IF ((N.EQ.1) .AND. (TEMP.LT.0.0D0)) TEMP = TEMP + 360.0D0
         IF ((N.EQ.2) .AND. (TEMP.LT.-90.0D0)) TEMP = TEMP + 360.0D0
         CHM = ' '
         IF (N.EQ.1) TEMP = TEMP / F15
C                                       Check on sign
         IF (TEMP.LT.0.0D0) THEN
            TEMP = -TEMP
            CHM = '-'
            END IF
C                                       longitude leading digits
         IF (N.EQ.2) THEN
            I = TEMP / 100.0D0
            IF (I.GT.0) THEN
               IF (CHM.NE.' ') GO TO 910
               IF (I.LE.9) CHM = LOCHAR(I)
               IF (I.GE.10) CHM = LOCHAR(10)
               TEMP = TEMP - 100.0D0 * I
               END IF
C                                       RA: days part
         ELSE
            I = TEMP / 24.0D0
            IF (I.GT.0) THEN
               IF (CHM.NE.' ') GO TO 910
               IF (I.LE.9) CHM = LOCHAR(I)
               IF (I.GE.10) CHM = LOCHAR(10)
               TEMP = TEMP - 24.0D0 * I
               END IF
            END IF
C                                       Separate into h m s
         TEMP = TEMP * CON
         HM(1) = TEMP / CON
         TEMP = TEMP - HM(1) * CON
         HM(2) = TEMP / F60
         SEC = TEMP - HM(2) * F60
C                                       Convert to degrees
 50   ELSE
         TEMP = F60 * (F60 * HM(1) + HM(2)) + SEC
         IF (N.EQ.3) TEMP = TEMP * F15
         IF (CHM.EQ.'-') TEMP = -TEMP
         RAD = TEMP / CON
         IF ((CHM.NE.' ') .AND. (CHM.NE.'-')) THEN
            DO 55 I = 1,9
               IF (CHM.EQ.LOCHAR(I)) GO TO 60
 55            CONTINUE
            I = 0
 60         IF (N.EQ.3) RAD = RAD + I * 24.0D0
            IF (N.EQ.4) RAD = RAD + I * 100.0D0
            END IF
         END IF
      GO TO 999
C                                       Error return
 900  WRITE (MSGTXT,1900) N
      CALL MSGWRT (6)
      GO TO 999
C                                       too large negative
 910  WRITE (MSGTXT,1910) RAD
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('COORDI: IMPROPER CONTROL PARAMETER. N= ',I4)
 1910 FORMAT ('COORDI: VALUE TOO LARGE FOR FORMAT',1PE11.3)
      END

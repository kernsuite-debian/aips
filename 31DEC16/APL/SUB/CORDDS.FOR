      SUBROUTINE CORDDs (N, RAD, CHM, HM, SEC)
C-----------------------------------------------------------------------
C! converts angles between degrees and sexagesimal format
C# Utility Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2000
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
C   CORDDS Converts RA and DEC to and from degrees under control of N.
C   Inputs:
C      N    I        OPcode: 1 degrees to RA
C                            2 degrees to DEC
C                            3 RA to degrees
C                            4 DEC to degrees
C   In/out:
C      RAD   D      The angle in degrees
C      CHM   C*3    The sign of the sexagesimal number in CHM(3:)
C                     (or 3 digits for hundreds or days)
C      HM    I(2)   The angle in RA/dec as
C                      HM(1)   Hours or Degrees
C                      HM(2)   Minutes of time or arc
C      SEC   R      The rest of the Ra/dec : seconds of time or arc
C   Differs from COORDD in the number of characters in CHM
C-----------------------------------------------------------------------
      CHARACTER CHM*3
      INTEGER   HM(2), N
      REAL      SEC
      DOUBLE PRECISION  RAD
C
      INTEGER   I
      DOUBLE PRECISION  TEMP, CON, F15, F60
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      F15 = 15.0D0
      F60 = 60.0D0
      CON = 3600.D0
      CHM = ' '
C                                       From degrees
      IF ((N.EQ.1) .OR. (N.EQ.2)) THEN
         TEMP = RAD
         IF ((N.EQ.1) .AND. (TEMP.LT.0.0D0)) TEMP = TEMP + 360.0D0
         IF ((N.EQ.2) .AND. (TEMP.LT.-90.0D0)) TEMP = TEMP + 360.0D0
         IF (N.EQ.1) TEMP = TEMP / F15
C                                       Check on sign
         IF (TEMP.LT.0.0D0) THEN
            TEMP = -TEMP
            CHM = '  -'
C                                       longitude leading digits
         ELSE
            IF (N.EQ.1) THEN
               I = TEMP / 24.0D0
               TEMP = TEMP - 24.0D0 * I
            ELSE
               I = TEMP / 100.0D0
               TEMP = TEMP - 100.0D0 * I
               END IF
            IF (I.GT.0) WRITE (CHM,1000) I
            END IF
C                                       Separate into h m s
         TEMP = TEMP * CON
         HM(1) = TEMP / CON
         TEMP = TEMP - HM(1) * CON
         HM(2) = TEMP / F60
         SEC = TEMP - HM(2) * F60
C                                       Convert to degrees
      ELSE IF ((N.EQ.3) .OR. (N.EQ.4)) THEN
         TEMP = F60 * (F60 * HM(1) + HM(2)) + SEC
         IF (N.EQ.3) TEMP = TEMP * F15
         IF (CHM.EQ.'  -') TEMP = -TEMP
         RAD = TEMP / CON
         IF ((CHM.NE.' ') .AND. (CHM.NE.'  -')) THEN
            READ (CHM,1000) I
            IF (N.EQ.3) RAD = RAD + I * 24.0D0
            IF (N.EQ.4) RAD = RAD + I * 100.0D0
            END IF
      ELSE
         WRITE (MSGTXT,1900) N
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3)
 1900 FORMAT ('COORDD: IMPROPER CONTROL PARAMETER. N= ',I4)
      END

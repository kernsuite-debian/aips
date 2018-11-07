      SUBROUTINE TIMDAT (ITIME, IDATE, ATIME, ADATE)
C-----------------------------------------------------------------------
C! convert integer time and date to character form for display
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000
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
C   Convert integer array form of time and date to expanded alpha
C   form for printing.
C   Inputs:
C      ITIME  I(3)   hours, minutes, seconds.
C      IDATE  I(3)   year, month, date.  If IDATE(1) < 0, then a short
C                    (9 char) output is created using ABS(IDATE(1))
C   Outputs:
C      ATIME  C*8    Time in form  'hh:mm:ss'
C      ADATE  C*12   Date in form  'dd-MMM-yyyy' or 'dd-MMM-yy'
C-----------------------------------------------------------------------
      INTEGER   ITIME(3), IDATE(3)
      CHARACTER ATIME*(*), ADATE*(*)
C
      CHARACTER MONTH(12)*3, BADTIM*8, BADAT1*12, BADAT2*12
      INTEGER   IYEAR
      DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     *   'AUG','SEP','OCT','NOV','DEC'/
      DATA BADTIM /'BAD TIME'/
      DATA BADAT1 /'FUNNY DATE  '/
      DATA BADAT2 /'BAD DATE    '/
C-----------------------------------------------------------------------
C                                       time
      IF ((ITIME(1).LT.0) .OR. (ITIME(1).GT.24) .OR. (ITIME(2).LT.0)
     *   .OR. (ITIME(2).GT.60) .OR. (ITIME(3).LT.0) .OR.
     *   (ITIME(3).GT.60)) THEN
         ATIME = BADTIM
      ELSE
         WRITE (ATIME,1000) ITIME
         END IF
C                                       Do date - 11 char
 20   IYEAR = IDATE(1)
      IF (IYEAR.GE.0) THEN
         IF (IYEAR.LT.40) IYEAR = IYEAR + 2000
         IF (IYEAR.LT.200) IYEAR = IYEAR + 1900
         IF ((IYEAR.LE.1900) .OR. (IYEAR.GE.2100) .OR. (IDATE(3).LE.0)
     *      .OR. (IDATE(3).GT.31) .OR. (IDATE(2).LE.0) .OR.
     *      (IDATE(2).GT.12)) THEN
            ADATE = BADAT1
         ELSE
            WRITE (ADATE,1020) IDATE(3), MONTH(IDATE(2)), IYEAR
            END IF
C                                       Do date - 9 char
      ELSE
         IYEAR = -IYEAR
         IYEAR = MOD (IYEAR, 100)
         IF ((IDATE(3).LE.0) .OR. (IDATE(3).GT.31) .OR. (IDATE(2).LE.0)
     *      .OR. (IDATE(2).GT.12)) THEN
            ADATE = BADAT2
         ELSE
            WRITE (ADATE,1021) IDATE(3), MONTH(IDATE(2)), IYEAR
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2,':',I2.2,':',I2.2)
 1020 FORMAT (I2.2,'-',A3,'-',I4)
 1021 FORMAT (I2.2,'-',A3,'-',I2.2)
      END

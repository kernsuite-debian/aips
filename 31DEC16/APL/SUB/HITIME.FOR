      SUBROUTINE HITIME (TIME1, TIME2, HLUN, HBUFF, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   HITIME interprets a timerange to the history file.  Both a machine-
C   and a man-readable form are written.
C   Inputs:
C      TIME1   R        Start time in days: <= 0 => beginning
C      TIME2   R        Stop time in days: >= 1.0e4 => to end
C      HLUN    I        LUN of open history file
C   In/out:
C      HBUFF   I(256)   History writing buffer
C   Output:
C      IERR    I        Error code of HIADD : 0 => okay
C-----------------------------------------------------------------------
      REAL      TIME1, TIME2
      INTEGER   HLUN, HBUFF(256), IERR
C
      REAL      T1, T2
      INTEGER   IT1(3), IT2(3)
      CHARACTER HILINE*72
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       convert times
      IF (TIME1.GT.0.0) THEN
         T1 = TIME1
         IT1(1) = T1
         T1 = 24.0 * (T1 - IT1(1))
         IT1(2) = T1
         T1 = 60.0 * (T1 - IT1(2))
         IT1(3) = T1
         T1 = 60.0 * (T1 - IT1(3))
         END IF
      IF (TIME2.LT.1.0E4) THEN
         T2 = TIME2
         IT2(1) = T2
         T2 = 24.0 * (T2 - IT2(1))
         IT2(2) = T2
         T2 = 60.0 * (T2 - IT2(2))
         IT2(3) = T2
         T2 = 60.0 * (T2 - IT2(3))
         END IF
C                                       Man-readable form
      IF (TIME1.LE.0.0) THEN
         IF (TIME2.GE.1.0E4) THEN
            WRITE (HILINE,1000) TSKNAM
         ELSE
            WRITE (HILINE,1010) TSKNAM, IT2, T2
            IF (HILINE(43:43).EQ.' ') HILINE(43:43) = '0'
            END IF
      ELSE
         IF (TIME2.GE.1.0E4) THEN
            WRITE (HILINE,1020) TSKNAM, IT1, T1
            IF (HILINE(30:30).EQ.' ') HILINE(30:30) = '0'
         ELSE
            WRITE (HILINE,1030) TSKNAM, IT1, T1, IT2, T2
            IF (HILINE(30:30).EQ.' ') HILINE(30:30) = '0'
            IF (HILINE(50:50).EQ.' ') HILINE(50:50) = '0'
            END IF
         END IF
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       If not all times
C                                       Machine-readable form
      IF ((TIME1.GT.0.0) .OR. (TIME2.LT.1.0E4)) THEN
         T1 = MAX (0.0, TIME1)
         T2 = MIN (9999.9, TIME2)
         WRITE (HILINE,1040) TSKNAM, T1, T2
         CALL HIADD (HLUN, HILINE, HBUFF, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A5,' /TIMERANG = beginning to end')
 1010 FORMAT (A5,' /TIMERANG = beginning to',I5,'/',2(I2.2,':'),F5.2)
 1020 FORMAT (A5,' /TIMERANG =',I5,'/',2(I2.2,':'),F5.2,' to end')
 1030 FORMAT (A5,' /TIMERANG =',I5,'/',2(I2.2,':'),F5.2,' to',
     *   I5,'/',2(I2.2,':'),F5.2)
 1040 FORMAT (A5,'  TIMERANG = ',2(1PE14.6),'  / days')
      END

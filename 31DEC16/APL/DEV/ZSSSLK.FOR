      SUBROUTINE ZSSSLK (OP, IERR)
C-----------------------------------------------------------------------
C! lock SUN-Screen-Server (TV) device
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2000, 2004
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
C   Lock/unlock SUN-Screen-Server (TV) device
C   Inputs:
C      OP     C*4   'LOCK', 'UNLK'
C   Output:
C      IERR   I     Error return code: 0 = no error
C                      1 = LUN already in use
C                      2 = file not found
C                      3 = volume not found
C                      4 = excl requested but not available
C                      5 = no room for lun
C                      6 = other open errors
C   Uses TVLUN2, TVIND2 from common
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      CHARACTER PNAME*48
      HOLLERITH HNAME(12)
      LOGICAL   F
      INTEGER   ERRLUN, ERRTER, IT, IER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA ERRLUN, ERRTER /12, 6/
C-----------------------------------------------------------------------
      CALL ZPHFIL ('TV', 1, 1, 1, PNAME, IER)
C
      IERR = 6
      IF ((TVLUN2.EQ.ERRLUN) .OR. (TVLUN2.EQ.ERRTER)) GO TO 980
      IF ((TVLUN2.LE.0) .OR. (TVLUN2.GT.50)) GO TO 980
      IERR = 0
C                                       allocate table
      CALL LSERCH ('OPEN', TVLUN2, TVIND2, F, IER)
      IF (IER.NE.0) THEN
         IF (IER.EQ.2) THEN
            IERR = 1
            WRITE (MSGTXT,1000) TVLUN2
            GO TO 990
         ELSE IF (IER.EQ.3) THEN
            IERR = 5
            WRITE (MSGTXT,1005) TVLUN2
         ELSE
            IERR = 6
            WRITE (MSGTXT,1010) IER, TVLUN2
            END IF
         GO TO 990
         END IF
C                                       non-disk devices
      IT = 0
      IF (OP.EQ.'LOCK') IT = 1
      CALL CHR2H (48, PNAME, 1, HNAME)
      CALL ZSSSL2 (FTAB(TVIND2+NMOFF), HNAME, IT, IER)
      IF (IER.EQ.4) THEN
         IERR = 4
      ELSE IF (IER.NE.0) THEN
         IERR = 6
         WRITE (MSGTXT,1020) IER, OP
         CALL MSGWRT (6)
      ELSE
         IERR = 0
         END IF
C                                       remove FTAB
      CALL LSERCH ('CLOS', TVLUN2, TVIND2, F, IER)
      IF (IERR.NE.0) TVIND2 = 0
      GO TO 999
C                                       Errors
C                                       illegal lun
 980  WRITE (MSGTXT,1980) TVLUN2, PNAME(1:24)
C                                       print msg
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZSSSLK: LUN',I4,' ALREADY IN USE')
 1005 FORMAT ('ZSSSLK: LUN TABLE FULL, CANT FIT LUN',I4)
 1010 FORMAT ('ZSSSLK: ERROR',I5,' LUN',I2,' ON LSERCH')
 1020 FORMAT ('ZSSSLK: ERROR',I4,' ON ',A)
 1980 FORMAT ('ZSSSLK: LUN',I6,' ILLEGAL FOR ',A24)
      END

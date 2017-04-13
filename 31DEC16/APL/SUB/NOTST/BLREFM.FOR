      SUBROUTINE BLREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of BL table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C-----------------------------------------------------------------------
C   Routine to change the format of the BL table from one containing
C   13/9 columns to the 14/10 columns needed by the addition of the
C   FREQID column.
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to check/modify
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C
C   Note, routine will leave no trace of its operation, i.e. BL table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that number of columns has changed if that
C   is required.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4
      INTEGER   BUFFER(1024), IBLRNO, BLKOLS(MAXBLC), BLNUMV(MAXBLC),
     *   NUMANT, NUMPOL, NUMIF, OVER, OLUN, OBUFF(1024), OKOLS(MAXBLC),
     *   ONUMV(MAXBLC), NBLROW, I, SOURID, ANT1, ANT2, SUBA, FREQID,
     *   OBLRNO, MSGSAV, IDUM
      LOGICAL   CHSTAT
      REAL      TIME, FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
C-----------------------------------------------------------------------
C                                       Open BL file
      CALL BLINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IBLRNO, BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Is it new format?
      IF ((BUFFER(10).EQ.14) .OR. (BUFFER(10).EQ.10)) THEN
         CALL TABIO ('CLOS', 0, IBLRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Check is old format
      IF ((BUFFER(10).NE.13) .AND. (BUFFER(10).NE.9)) THEN
         WRITE (MSGTXT,1000) BUFFER(10)
         IRET = 1
         GO TO 990
         END IF
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, OBUFF, IRET)
      IF (STAT.EQ.'READ') THEN
C                                       Change status
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLRD'
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'WRIT'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       # rows in old table
      NBLROW = BUFFER(5)
C                                       Open up new BL table
      OVER = 0
      CALL BLINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN,
     *   OBLRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop and copy
      DO 100 I = 1, NBLROW
         CALL TABBL ('READ', BUFFER, IBLRNO, BLKOLS, BLNUMV, NUMPOL,
     *      TIME, SOURID, ANT1, ANT2, SUBA, FREQID, FACMUL, FACADD,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABBL ('WRIT', OBUFF, OBLRNO, OKOLS, ONUMV, NUMPOL,
     *      TIME, SOURID, ANT1, ANT2, SUBA, FREQID, FACMUL, FACADD,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IBLRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OBLRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'BL', VER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('BL', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CATBLK, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'BL', OVER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLWR'
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'READ'
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLREFM: UNKNOWN BL FORMAT, # COLS = ',I3)
 1010 FORMAT ('BLREFM: ERROR ',I3,' RETURNED FROM BLINI')
 1080 FORMAT ('BLREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END

      SUBROUTINE FGREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of FG table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005
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
C   Routine to change the format of the FG table from one containing
C   8 columns to the 9 columns needed by the addition of the
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
C   Note, routine will leave no trace of its operation, i.e. FG table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that number of columns has changed if that
C   is required.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, REASON*24
      INTEGER   BUFFER(1024), IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC),
     *   OVER, OLUN, OBUFF(1024), OKOLS(MAXFGC), ONUMV(MAXFGC), NFGROW,
     *   I, SOURID, ANTNO(2), SUBA, FREQID, IFS(2), CHANS(2), OFGRNO,
     *   MSGSAV, IDUM
      REAL      TIMER(2)
      LOGICAL CHSTAT, PFLAGS(4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
C-----------------------------------------------------------------------
C                                       Open existing FG file
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL FLGINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      MSGSUP = MSGSAV
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Is it new format?
      IF (BUFFER(10).EQ.9) THEN
         CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Check is old format
      IF (BUFFER(10).NE.8) THEN
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
      NFGROW = BUFFER(5)
C                                       Open up new FG table
      OVER = 0
      CALL FLGINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN,
     *   OFGRNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop and copy
      DO 100 I = 1, NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *      SOURID, SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS,
     *      REASON, IRET)
         IF (IRET.GT.0) GO TO 999
         IF (IRET.EQ.0) THEN
            CALL TABFLG ('WRIT', OBUFF, OFGRNO, OKOLS, ONUMV,
     *         SOURID, SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS,
     *         REASON, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OFGRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'FG', VER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('FG', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CATBLK, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'FG', OVER, CATBLK, BUFFER, IRET)
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
 1000 FORMAT ('FGREFM: UNKNOWN FG FORMAT, # COLS = ',I3)
 1010 FORMAT ('FGREFM: ERROR ',I3,' RETURNED FROM FGINI')
 1080 FORMAT ('FGREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END

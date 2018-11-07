      SUBROUTINE SUREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of SU table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2014
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
C   Routine to change the format of the SU table from one containing
C   19 columns to the 21 columns needed by the addition of the RAOBS
C   and DECOBS columns.
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to check/modify
C      CATBLK(256)     I       Catalog header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C
C   Note, routine will leave no trace of its operation, i.e. SU table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that number of columns has changed if that
C   is required.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4
      INTEGER   BUFFER(512), OVER, OLUN, OBUFF(512), I, OSURNO, NSUROW,
     *   OKOLS(MAXSUC), ONUMV(MAXSUC), MSGSAV, IDUM, CASAFE(256), NUMIF,
     *   ISURNO, SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      LOGICAL   CHSTAT
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
C-----------------------------------------------------------------------
C                                       Open existing SU file
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL SUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      MSGSUP = MSGSAV
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'OPEN INPUT'
         GO TO 990
         END IF
C                                       Is it new format?
      IF ((BUFFER(10).EQ.21) .AND. (SUKOLS(16).GT.0) .AND.
     *   (SUKOLS(17).GT.0)) THEN
         CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Check is old format
      IF ((BUFFER(10).GT.19) .AND. (BUFFER(10).NE.21)) THEN
         WRITE (MSGTXT,1000) BUFFER(10)
         IRET = 1
         GO TO 990
      ELSE IF (BUFFER(10).LT.19) THEN
         WRITE (MSGTXT,1001) 19-BUFFER(10)
         CALL MSGWRT (7)
         END IF
C                                       # rows in old table
      NSUROW = BUFFER(5)
C                                       input one mey be modified and
C                                       we don't dare put it back to
C                                       disk - so get safe one
      CALL CATIO ('READ', DISK, CNO, CASAFE, 'REST', OBUFF, IRET)
      IF ((IRET.NE.0) .AND. ((IRET.LT.6) .OR. (IRET.GT.8))) THEN
         WRITE (MSGTXT,1010) IRET, 'CATIO READ FOR SAFE HEADER'
         GO TO 990
         END IF
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
 20   CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, OBUFF, IRET)
C                                       Remove 1 read status: loop
      IF (STAT.EQ.'READ') THEN
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLRD'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         GO TO 20
         END IF
C                                       now add write
      IF (CHSTAT) THEN
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'WRIT'
            GO TO 990
            END IF
         END IF
C
C                                       Open up new SU table
      OVER = 0
      CALL SUINI ('WRIT', OBUFF, DISK, CNO, OVER, CASAFE, OLUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, OSURNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop and copy
      DO 100 I = 1,NSUROW
         ISURNO = I
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *      SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ INPUT'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            OSURNO = I
C                                       assumption of old routines
            RAOBS = RAEPO
            DECOBS = DECEPO
C                                       now copy
            CALL TABSOU ('WRIT', OBUFF, OSURNO, OKOLS, ONUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUTPUT'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSURNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'SU', VER, CASAFE, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'DELETE ORIGINAL'
         GO TO 990
         END IF
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('SU', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CASAFE, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'COPYING TEMP TO OLD'
         GO TO 990
         END IF
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'SU', OVER, CASAFE, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'DELETE TEMP'
         GO TO 990
         END IF
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
 1000 FORMAT ('SUREFM: UNKNOWN SU FORMAT, # COLS = ',I3)
 1001 FORMAT ('SUREFM: UNKNOWN SU FORMAT: MISSING',I3,
     *   ' COLS - try fix anyway')
 1010 FORMAT ('SUREFM: ERROR ',I3,' DOING ',A,' SU FILE')
 1080 FORMAT ('SUREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END

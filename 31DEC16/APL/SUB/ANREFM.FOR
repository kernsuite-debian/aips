      SUBROUTINE ANREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of AN table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2010
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
C   Routine to change the format of the AN table from one containing
C   12 columns to the 14 columns needed by the addition of the DIAMETER
C   and BEAMFWHM columns.  It also recognizes the handedness of the
C   coordinates and forces them to right handed.
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
C   Note, routine will leave no trace of its operation, i.e. AN table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that number of columns has changed if that
C   is required.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, BLANK*8, IFAXIS*8, CHSOLT*8
      INTEGER   BUFFER(512), OVER, OLUN, OBUFF(512), I, OANRNO, NANROW,
     *   OKOLS(MAXANC), ONUMV(MAXANC), MSGSAV, IDUM, NTYPES(3), TYPE,
     *   KEYTYP, LOCS, JERR, CASAFE(256)
      LOGICAL   CHSTAT, CHANGE
      HOLLERITH HSTNPS(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA OLUN /45/
      DATA BLANK, IFAXIS /' ', 'IF'/
      DATA CHSOLT /'POLTYPE'/
C-----------------------------------------------------------------------
C                                       Open existing AN file
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL ANINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IRET)
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
      IF ((BUFFER(10).EQ.14) .AND. (XYZHAN.EQ.'RIGHT') .AND.
     *   (ANKOLS(7).GT.0) .AND. (ANKOLS(8).GT.0)) THEN
         CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Check is old format
      IF ((BUFFER(10).GT.12) .AND. (BUFFER(10).NE.14)) THEN
         WRITE (MSGTXT,1000) BUFFER(10)
         IRET = 1
         GO TO 990
      ELSE IF (BUFFER(10).LT.12) THEN
         WRITE (MSGTXT,1001) 12-BUFFER(10)
         CALL MSGWRT (7)
         END IF
C                                       get POLTYPE
      MSGSUP = 32000
      CALL TABKEY ('READ', CHSOLT, 1, BUFFER, LOCS, HSTNPS, KEYTYP,
     *   JERR)
      MSGSUP = 0
C                                       # rows in old table
      NANROW = BUFFER(5)
C                                       Handedness of file: use center
      CALL FILL (3, 0, NTYPES)
      IF (ABS(ARRAYC(2)).GT.1.D3) THEN
         CALL ANFIND (ANAME, BLANK, ARRAYC, TYPE)
         NTYPES(TYPE) = 1
C                                       use antennas
      ELSE
         DO 20 I = 1,NANROW
            IANRNO = I
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ INPUT'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               CALL ANFIND (ANAME, ANNAME, STAXYZ, TYPE)
               NTYPES(TYPE) = NTYPES(TYPE) + 1
               END IF
 20         CONTINUE
         END IF
      CHANGE = .FALSE.
      XYZHAN = 'RIGHT'
      IF ((NTYPES(2).NE.0) .AND. (NTYPES(3).NE.0)) THEN
         WRITE (MSGTXT,1020) NTYPES(2), NTYPES(3)
         CALL MSGWRT (8)
      ELSE IF ((NTYPES(2).EQ.0) .AND. (NTYPES(3).EQ.0)) THEN
         XYZHAN = '????'
         MSGTXT = 'ANREFM: NO ANTENNA OR ARRAY RECOGNIZED'
         CALL MSGWRT (8)
      ELSE
         IF (NTYPES(2).GT.0) CHANGE = .TRUE.
         WRITE (MSGTXT,1021) NTYPES
C         IF (NTYPES(1).GT.0) CALL MSGWRT (6)
         END IF
C                                       Correct numbers
C                                       NIF was not given
C                                       NOPCAL was #Pcal parms * NIF
      IF (ANTNIF.LE.0) THEN
         CALL AXEFND (8, IFAXIS, CATBLK(KIDIM), CATBLK(KHCTP), I, IRET)
         IF (IRET.GT.0) THEN
            ANTNIF = 1
            IRET = 0
         ELSE
            ANTNIF = CATBLK(KINAX+I)
            END IF
         I = NOPCAL / ANTNIF
         IF (I.GT.0) NOPCAL = MAX (2, I)
         END IF
      IF (CHANGE) ARRAYC(2) = -ARRAYC(2)
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
C
C                                       Open up new AN table
      OVER = 0
      CALL ANINI ('WRIT', OBUFF, DISK, CNO, OVER, CASAFE, OLUN,
     *   OANRNO, OKOLS, ONUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       poltype keyword
      IF (JERR.EQ.0) CALL TABKEY ('WRIT', CHSOLT, 1, OBUFF, LOCS,
     *   HSTNPS, KEYTYP, JERR)
C                                       Loop and copy
      DO 100 I = 1,NANROW
         IANRNO = I
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ INPUT'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            OANRNO = I
            IF (CHANGE) STAXYZ(2) = -STAXYZ(2)
            CALL TABAN ('WRIT', OBUFF, OANRNO, OKOLS, ONUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUTPUT'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OANRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'AN', VER, CASAFE, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'DELETE ORIGINAL'
         GO TO 990
         END IF
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('AN', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CASAFE, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'COPYING TEMP TO OLD'
         GO TO 990
         END IF
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'AN', OVER, CASAFE, BUFFER, IRET)
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
 1000 FORMAT ('ANREFM: UNKNOWN AN FORMAT, # COLS = ',I3)
 1001 FORMAT ('ANREFM: UNKNOWN AN FORMAT: MISSING',I3,
     *   ' COLS - try fix anyway')
 1010 FORMAT ('ANREFM: ERROR ',I3,' DOING ',A,' AN FILE')
 1020 FORMAT ('ANREFM: FOUND',I3,' LEFT AND',I3,' RIGHT-HAND By VALUES')
 1021 FORMAT ('ANREFM: FOUND',3I3,' UNKNOWN, LEFT, RIGHT By VALUES')
 1080 FORMAT ('ANREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END

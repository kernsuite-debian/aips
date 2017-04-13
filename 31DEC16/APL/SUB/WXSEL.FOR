      SUBROUTINE WXSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, TB, TE, AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in a TY table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2007, 2016
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
C   Copies a subset of IFs in a TY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I        LUN to use - in
C      LUNO     I        LUN to use - out
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, AN(*), NA, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:DWXV.INC'
      INTEGER   IWXRNO, WXKOLS(MAXWXC), WXNUMV(MAXWXC), OKOLS(MAXWXC),
     *   ONUMV(MAXWXC), NWXROW, I, OVER, ANTNO,SUBA, OWXRNO, K, TABVER
      CHARACTER OBSCOD*8, OBSDAT*8
      LOGICAL   REFMT, GOTIT
      DOUBLE PRECISION TIME
      REAL      DTIME, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open WX file
      CALL WXINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IWXRNO,
     *   WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      REFMT = .FALSE.
      NWXROW = BUFFER(5)
C                                       Open up new WX table
      OVER = VER
      CALL WXINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OWXRNO, OKOLS, ONUMV, OBSCOD, OBSDAT, TABVER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NWXROW
         CALL TABWX ('READ', BUFFER, IWXRNO, WXKOLS, WXNUMV, TIME,
     *      DTIME, ANTNO, SUBA, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST,
     *      PRECIP, H2OCOL, IONCOL, IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
C                                       antenna
            IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
               GOTIT = .FALSE.
               DO 30 K = 1,NA
                  GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            END IF
C                                       This record dropped
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       This record kept
         ELSE
C                                       Write new one
            IF ((JSUB.GE.0) .AND. (SUBA.GT.0)) SUBA = JSUB
            CALL TABWX ('WRIT', OBUFF, OWXRNO, OKOLS, ONUMV, TIME,
     *         DTIME, ANTNO, SUBA, TEMP, PRESS, DEWPT, WVEL, WDIR,
     *         WGUST, PRECIP, H2OCOL, IONCOL, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IWXRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OWXRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted WX', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied WX', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WXSEL: ERROR ',I3,' RETURNED FROM WXINI')
 1020 FORMAT ('WXSEL: ERROR ',I3,' RETURNED FROM TABWX')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END

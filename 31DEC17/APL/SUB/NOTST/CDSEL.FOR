      SUBROUTINE CDSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN, NA, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in a CD table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2011, 2017
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
C   Copies a subset of IFs in a CD table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                           if <= 0 then output value unchanged.
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(*), NA, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCDC
      PARAMETER (MAXCDC=7)
C
      INTEGER   ICDRNO, CDKOLS(MAXCDC), CDNUMV(MAXCDC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXCDC), ONUMV(MAXCDC), NCDROW, I, ANTNO, SUBA,
     *   FREQID, OCDRNO, NEWNIF, IIF, JIF, IPOL, K, OVER, NEWPOL, LBPOL,
     *   LBIF
      REAL      TCAL(4,MAXIF)
      LOGICAL   GOTIT, REFMT
      CHARACTER RDATE*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open CD file
      CALL CDINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ICDRNO,
     *   CDKOLS, CDNUMV, NUMANT, NUMPOL, NUMIF, RDATE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      NEWNIF = MIN (NUMIF, EIF) - BIF + 1
      LBIF = MAX (BIF, 1)
      IF (NEWNIF.LE.0) THEN
         NEWNIF = NUMIF
         LBIF = 1
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         LBPOL = 1
         NEWPOL = NUMPOL
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NCDROW = BUFFER(5)
C                                       Open up new CD table
      OVER = VER
      CALL CDINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCDRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, RDATE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NCDROW
         CALL TABCD ('READ', BUFFER, ICDRNO, CDKOLS, CDNUMV, NUMPOL,
     *      NUMIF, ANTNO, SUBA, FREQID, TCAL, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -1
C                                       Suba selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + LBIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  TCAL(IPOL,JIF) = TCAL(K,IIF)
                  TCAL(IPOL+2,JIF) = TCAL(K+2,IIF)
 80               CONTINUE
 90            CONTINUE
C
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
            CALL TABCD ('WRIT', OBUFF, OCDRNO, OKOLS, ONUMV, NEWPOL,
     *         NEWNIF, ANTNO, SUBA, FREQID, TCAL, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICDRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OCDRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted CD', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied CD', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('CDSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CDSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CDSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CDSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END

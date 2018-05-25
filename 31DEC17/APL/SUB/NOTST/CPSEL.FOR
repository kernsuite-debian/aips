      SUBROUTINE CPSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies and renumbers the IFs, channels in a CP table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2015
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
C   Copies and renumbers the IFs and channels in an CP table; can also
C   modify the FQ ID.
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
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      BCHAN    I        First channel
C      ECHAN    I        Last channel
C      IFQID    I        FQ ID to select (output FQID will be 1)
C                          if <= 0 then output FQ ID unchanged.
C   Input/Output:
C      BUFFER   I(512)   Work buffer
C      OBUFF    I(512)   Work buffer
C   Output:
C      IRET     I        Error, 0 => OK, -1 => okay but no copy
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, BUFFER(*), OBUFF(*),
     *   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCPC
      PARAMETER (MAXCPC = 6)
C
      REAL      CFLUX(4,MAXCIF)
      INTEGER   ICPRNO, CPKOLS(MAXCPC), CPNUMV(MAXCPC), OKOLS(MAXCPC),
     *   ONUMV(MAXCPC), NCPROW, OCPRNO, FREQID, NUMIF, NUMFRQ, I,
     *   J, K, JJ, NEWIF, NEWFRQ, IFA, IFB, IFREQA, IFREQB, INX, LNX,
     *   OVER, SUID, FOPEN
      LOGICAL   REFMT
      CHARACTER SOURCE*16
      SAVE CFLUX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      REFMT = .FALSE.
      FOPEN = 0
C                                       Open CP file
      CALL CPINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ICPRNO,
     *   CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT TABLE'
         GO TO 990
         END IF
      IF (IFQID.GT.0) THEN
         IF ((FREQID.GT.0) .AND. (FREQID.NE.IFQID)) THEN
            CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, IRET)
            IRET = -1
            GO TO 999
         ELSE
            REFMT = FREQID.NE.1
            FREQID = 1
            END IF
         END IF
      FOPEN = 1
C                                       new channel numbers
      IFREQA = MAX (BCHAN, 1)
      IFREQB = MIN (NUMFRQ, ECHAN)
      NEWFRQ = IFREQB - IFREQA + 1
C                                       New CP table will cover
C                                       IF range [IFA,IFB].
      IFA = MAX (BIF, 1)
      IFB = MIN (NUMIF, EIF)
      NEWIF = IFB - IFA + 1
      IF ((NEWFRQ.NE.NUMFRQ) .OR. (NEWIF.NE.NUMIF)) REFMT = .TRUE.
C                                       # rows in old table
      NCPROW = BUFFER(5)
C                                       Open up new CP table
      OVER = VER
      CALL CPINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCPRNO, OKOLS, ONUMV, NEWIF, NEWFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TABLE'
         GO TO 990
         END IF
      FOPEN = 2
C                                       Loop and copy
      DO 300 I = 1,NCPROW
         CALL TABCP ('READ', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *      NUMFRQ, SOURCE, SUID, CFLUX, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OLD TABLE'
            GO TO 990
C                                       Copy this record
         ELSE
            DO 30 J = 1,NEWIF
               JJ = BIF + J - 1
               INX = (J - 1) * NEWFRQ
               LNX = (JJ - 1) * NUMFRQ + IFREQA - 1
               IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
               DO 20 K = 1,NEWFRQ
                  INX = INX + 1
                  LNX = LNX + 1
                  CFLUX(1,INX) = CFLUX(1,LNX)
                  CFLUX(2,INX) = CFLUX(2,LNX)
                  CFLUX(3,INX) = CFLUX(3,LNX)
                  CFLUX(4,INX) = CFLUX(4,LNX)
 20               CONTINUE
 30            CONTINUE
C                                       Write output record.
            CALL TABCP ('WRIT', OBUFF, OCPRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, SOURCE, SUID, CFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW TABLE'
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted CP', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied CP', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (6)
C                                       close
 995  IF (FOPEN.GT.0) CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, I)
      IF (FOPEN.GT.1) CALL TABIO ('CLOS', 0, OCPRNO, OBUFF, OBUFF, J)
      IF (IRET.EQ.0) IRET = MAX (I, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPSEL: ERROR ',I3,1X,A)
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END

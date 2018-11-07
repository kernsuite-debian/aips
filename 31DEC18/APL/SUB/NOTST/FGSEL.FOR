      SUBROUTINE FGSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, TB, TE, IFQID, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies and renumbers the IFs in an FG table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-2000, 2007
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
C   Copies and renumbers the IFs in an FG table, can also modify the
C   FQ ID.
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      BCHAN           I       First channel
C      ECHAN           I       Last channel
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      IFQID           I       FQ ID to select (set to 1 on output)
C                              if <= 0 then output value unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REASON*24
      INTEGER   OVER, IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC),
     *   OKOLS(MAXFGC), ONUMV(MAXFGC), NFGROW, I, OFGRNO,
     *   SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2)
      LOGICAL PFLAGS(4), DROP, REFMT
      REAL    TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open FG file
      CALL FLGINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NFGROW = BUFFER(5)
      REFMT = JSUB.GE.0
C                                       Open up new FG table
      OVER = VER
      CALL FLGINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *  OFGRNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *      SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *      REASON, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         DROP = IRET.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((IFQID.GT.0) .AND. (IFQID.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((ISUB.GT.0) .AND. (ISUB.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       timerange
         DROP = DROP .OR. (TB.GT.TIMER(2)) .OR. (TE.LT.TIMER(1))
C                                       Renumber IFs
         IF ((BIF.GT.0) .AND. (EIF.GT.0)) THEN
            IF (IFS(1).GT.0) THEN
               IF (IFS(1).GT.EIF) THEN
                  DROP = .TRUE.
               ELSE IF (BIF.GT.1) THEN
                  REFMT = .TRUE.
                  IFS(1) = MAX (1, IFS(1)-BIF+1)
                  END IF
               END IF
            IF (IFS(2).GT.0) THEN
               IF (BIF.GT.1) THEN
                  REFMT = .TRUE.
                  IFS(2) = IFS(2) - BIF + 1
                  DROP = DROP .OR. (IFS(2).LE.0)
                  END IF
               IF (IFS(2).GT.EIF-BIF+1) THEN
                  REFMT = .TRUE.
                  IFS(2) = EIF - BIF + 1
                  END IF
               END IF
            END IF
C                                       Renumber channels
         IF ((BCHAN.GT.0) .AND. (ECHAN.GT.0)) THEN
            IF (CHANS(1).GT.0) THEN
               IF (CHANS(1).GT.ECHAN) THEN
                  DROP = .TRUE.
               ELSE IF (BCHAN.GT.1) THEN
                  REFMT = .TRUE.
                  CHANS(1) = MAX (1, CHANS(1)-BCHAN+1)
                  END IF
               END IF
            IF (CHANS(2).GT.0) THEN
               IF (BCHAN.GT.1) THEN
                  REFMT = .TRUE.
                  CHANS(2) = CHANS(2) - BCHAN + 1
                  DROP = DROP .OR. (CHANS(2).LE.0)
                  END IF
               IF (CHANS(2).GT.ECHAN-BCHAN+1) THEN
                  REFMT = .TRUE.
                  CHANS(2) = ECHAN - BCHAN + 1
                  END IF
               END IF
            END IF
C                                       May ignore some records
         IF (DROP) THEN
            REFMT = .TRUE.
C                                       Write new one
         ELSE
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
            CALL TABFLG ('WRIT', OBUFF, OFGRNO, OKOLS, ONUMV,
     *         SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *         REASON, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OFGRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted FG', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied FG', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM FLGINI')
 1020 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM TABFLG')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END

      SUBROUTINE CQSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, IFQID, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies and renumbers the IFs in a CQ table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2007
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
C   Copies and renumbers the IFs in an CQ table, can also modify the
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
     *   LUNI, LUNO, BIF, EIF, IFQID, ISUB, JSUB, BUFFER(*), OBUFF(*),
     *   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCQV.INC'
      LOGICAL   REFMT
      CHARACTER LTAPCQ(MAXIF)*8
      DOUBLE PRECISION DFRQCQ(MAXIF), DCBWCQ(MAXIF)
      REAL TAVGCQ(MAXIF)
      INTEGER ICQRNO, CQKOLS(MAXCQC), CQNUMV(MAXCQC), NROW, OCQRNO,
     *   OCQKOL(MAXCQC), OCQNUM(MAXCQC), I, NOIFCQ, IFQDCQ, ISUBCQ,
     *   NFFTCQ(MAXIF), NCHCQ(MAXIF), NSAVCQ(MAXIF), NOVSCQ(MAXIF),
     *   NZPDCQ(MAXIF), NBITCQ(MAXIF), IFLTCQ(MAXIF), IOVLCQ(MAXIF),
     *   NIFO, OVER
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open CQ table
      CALL CQINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NROW = BUFFER(5)
C                                       Open new CQ table
      NIFO = EIF - BIF + 1
      REFMT = NIFO.NE.NOIFCQ
      OVER = VER
      CALL CQINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCQRNO, OCQKOL, OCQNUM, NIFO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NROW
         CALL TABCQ ('READ', BUFFER, ICQRNO, CQKOLS, CQNUMV, NOIFCQ,
     *      IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ, DFRQCQ, DCBWCQ,
     *      LTAPCQ, NOVSCQ, NZPDCQ, IFLTCQ, TAVGCQ, NBITCQ, IOVLCQ,
     *      IRET)
C                                       check FQ, suba
         IF (IRET.EQ.0) THEN
            IF ((IFQID.GT.0) .AND. (IFQID.NE.IFQDCQ) .AND.
     *         (IFQDCQ.GT.0)) IRET = -1
            IF ((ISUBCQ.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.ISUBCQ))
     *         IRET = -1
            END IF
C                                       error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       flagged
         ELSE IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Write if selected
         ELSE
C                                       Set output FQ_ID to 1
            IF (IFQID.GT.0) IFQDCQ = 1
            IF (JSUB.GE.0) ISUBCQ = JSUB
            CALL TABCQ ('WRIT', OBUFF, OCQRNO, OCQKOL, OCQNUM, NIFO,
     *         IFQDCQ, ISUBCQ, NFFTCQ(BIF), NCHCQ(BIF), NSAVCQ(BIF),
     *         DFRQCQ(BIF), DCBWCQ(BIF), LTAPCQ(BIF), NOVSCQ(BIF),
     *         NZPDCQ(BIF), IFLTCQ(BIF), TAVGCQ(BIF), NBITCQ(BIF),
     *         IOVLCQ(BIF), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICQRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OCQRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted CQ', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied CQ', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('CQSEL: ERROR ',I3,' RETURNED FROM CQINI')
 1020 FORMAT ('CQSEL: ERROR ',I3,' RETURNED FROM TABCQ')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END

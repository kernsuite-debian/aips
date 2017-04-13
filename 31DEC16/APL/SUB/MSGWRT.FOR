      SUBROUTINE MSGWRT (MSGLEV)
C-----------------------------------------------------------------------
C! writes messages to log file and/or terminal - a fundamental routine!
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   MSGWRT writes messages to log file and, if NPOPS = 1 and MSGLEV > 0,
C   to terminal.
C   Inputs:
C      MSGLEV   > 1   message importance (use 2 - 10)
C               = 1   message to terminal only
C               = 0   message to file only
C               < 0   force file closing
C   Common: alters MSGCNT in /MSGCOM/
C-----------------------------------------------------------------------
      INTEGER   MSGLEV
C
      CHARACTER PHNAME*48, MSGTET*80, ACTION(5)*4
      INTEGER   MINLEV, MAGIC, J1, KERR, NWPL, NLPR, NPKLIP, MSGTER,
     *   MSGFIL, SCRTCH(256), FINDF, FINDP, IER, IERR, JTRIM, IMSGPR,
     *   IPOS, IPRNT, IREC, J, JERR, K, MSGMAX, MSGINC, ITIME(6),
     *   MSGLEN, ITRIM
      LOGICAL   F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA F /.FALSE./
      DATA MSGTER, MSGFIL /6, 12/
      DATA ACTION /'OPEN','WRIT','CLOS','PRNT','XPND'/
      DATA MAGIC /32000/
      DATA MSGINC /210/
C-----------------------------------------------------------------------
C                                       optional suppression: level 6,7
      IF ((MSGSUP.EQ.MAGIC) .AND. ((MSGLEV.EQ.6) .OR.
     *   (MSGLEV.EQ.7))) GO TO 999
C                                       kill all lower messages option
      IF ((MSGKIL.EQ.MAGIC) .AND. (NPOPS.LE.NINTRN) .AND.
     *   (MSGLEV.EQ.0) .AND. (ISBTCH.NE.32000)) GO TO 999
      IF ((MSGLEV.GT.1) .AND. (MSGLEV.LE.MSGKIL-MAGIC+1)) GO TO 999
      FINDF = 0
C                                       clear nulls
      MINLEV = MSGLEV
C                                       Suppress informative messages?
      IF (MSGSUP.EQ.32005) THEN
         IF (MSGLEV.EQ.1) GO TO 999
         IF (MSGLEV.GT.1 .AND. MSGLEV.LE.5) MINLEV = 0
         IF (MSGKIL.EQ.MAGIC .AND. NPOPS.LE.NINTRN .AND.
     :    ISBTCH.NE.32000 .AND. MINLEV.EQ.0) GO TO 999
         END IF
      IF ((MSGKIL.EQ.MAGIC) .AND. (NPOPS.LE.NINTRN) .AND.
     *   (ISBTCH.NE.32000)) MINLEV = MIN (MINLEV, 1)
      KERR = 0
      IF (MINLEV.LT.0) GO TO 10
         NPKLIP = NINTRN
         IF (NBATQS.GT.0) NPKLIP = NINTRN + 1
         IF ((NPOPS.GT.NPKLIP) .AND. (MINLEV.EQ.1)) MINLEV = 2
         IF ((ISBTCH.EQ.32000) .AND. (MINLEV.EQ.1)) MINLEV = 2
         IMSGPR = JTRIM (MSGTXT)
         IMSGPR = MAX (1, IMSGPR)
C                                       check if file open
 10   IF (MINLEV.EQ.1) GO TO 20
      CALL LSERCH ('SRCH', MSGFIL, FINDF, F, IERR)
      IF (IERR.EQ.1) MSGCNT = -1
      IF ((IERR.EQ.0) .AND. (MSGCNT.LT.0)) CALL ZMSGCL (MSGFIL,
     *   FINDF, IERR)
C                                       Open file.
      NWPL = 25
      NLPR = 255 / NWPL
      IF ((MINLEV.LT.0) .OR. (MINLEV.EQ.1) .OR. (MSGCNT.GE.0))
     *   GO TO 20
         CALL ZPHFIL ('MS', 1, NLUSER, 0, PHNAME, IERR)
         JERR = 1
         CALL ZMSGOP (MSGFIL, FINDF, 1, PHNAME, IERR)
         IF (IERR.NE.0) FINDF = 0
         IF (IERR.NE.0) GO TO 90
C                                       read message count
         CALL ZMSGDK ('READ', MSGFIL, FINDF, 1, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         MSGCNT = SCRTCH(1)
         MSGREC = SCRTCH(2)
         IF (MSGCNT.LT.0) MSGCNT = 0
         MSGMAX = NLPR * MSGREC
         IF (MSGCNT.GT.MSGMAX) THEN
            WRITE (MSGTER,1010) MSGCNT, MSGREC
            MSGCNT = MSGMAX
            END IF
C                                       Message to file
 20   IF ((MINLEV.LT.0) .OR. (MINLEV.EQ.1)) GO TO 30
         JERR = 2
         MSGCNT = MSGCNT + 1
         IREC = (MSGCNT-1) / NLPR + 1
         IPOS = MOD (MSGCNT-1, NLPR) * NWPL + 2
         MSGMAX = NLPR * MSGREC
         IF ((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) MSGMAX = 5000
C                                       expand file
         IF (IREC.LE.MSGREC) GO TO 22
            J = 100 / NLPR
            K = MSGSUP
            MSGSUP = 32000
            CALL ZPHFIL ('MS', 1, NLUSER, 0, PHNAME, IERR)
            CALL ZMSGXP (MSGFIL, 1, PHNAME, J, IERR)
            JERR = 5
            MSGSUP = K
            IF (IERR.NE.0) GO TO 90
            MSGREC = MSGREC + J
            IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) MSGMAX =
     *         MSGMAX + J*NLPR
C                                       get message record
 22      CALL ZMSGDK ('READ', MSGFIL, FINDF, IREC, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       Message format: 102 bytes
C                                       LEV, YY, MM, DD, HH, MM,
C                                       SS, POPS#, NAME(3),
C                                       MSG(40)
         SCRTCH(IPOS+1) = MIN (MINLEV, 15) + 16 * NPOPS
         CALL CATIME (1, SCRTCH(IPOS+2), ITIME)
         CALL CHR2H (6, TSKNAM, 1, SCRTCH(IPOS+4))
         CALL CHR2H (80, MSGTXT, 1, SCRTCH(IPOS+6))
C                                       back to disk
         CALL ZMSGDK ('WRIT', MSGFIL, FINDF, IREC, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
 25      IF (MSGCNT.LT.MSGMAX) GO TO 30
            IF (MOD(MSGCNT,MSGINC).NE.0) GO TO 30
            IF (NLUSER.EQ.1) GO TO 30
            CALL ZMSGOP (MSGTER, FINDP, 0, PHNAME, IER)
            IF ((IER.NE.0) .AND. (IER.NE.1)) GO TO 30
               WRITE (MSGTET,1025) NLUSER
               CALL ZTTYIO ('WRIT', MSGTER, FINDP, 72, MSGTET, IERR)
               CALL ZMSGCL (MSGTER, FINDP, IER)
C                                       Close file
 30   IF ((MSGCNT.LT.0) .OR. (MINLEV.EQ.1)) GO TO 40
 35      JERR = 3
         CALL ZMSGDK ('READ', MSGFIL, FINDF, 1, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         MSGMAX = NLPR * MSGREC
         IF (MSGCNT.GT.MSGMAX) THEN
            WRITE (MSGTER,1035) MSGCNT, MSGREC
            MSGCNT = MSGMAX
            END IF
         SCRTCH(1) = MSGCNT
         SCRTCH(2) = MSGREC
         CALL ZMSGDK ('WRIT', MSGFIL, FINDF, 1, SCRTCH, IERR)
         MSGCNT = -1
         CALL ZMSGCL (MSGFIL, FINDF, IERR)
         FINDF = 0
         IF (IERR.NE.0) GO TO 90
         IF (KERR.NE.0) GO TO 999
C                                       Msg to terminal
 40   IF ((NPOPS.GT.NPKLIP) .OR. (MINLEV.LE.0)) GO TO 999
         JERR = 4
         CALL ZMSGOP (MSGTER, FINDP, 0, PHNAME, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.1)) GO TO 999
 42         IPRNT = MIN (64, IMSGPR)
            MSGTET = TSKNAM(1:6) // ': ' //  MSGTXT(1:IPRNT)
            CALL ZEHEX (NPOPS, 1, MSGTET(6:6))
C                                       Skip trailing blanks
            MSGLEN = ITRIM( MSGTET)
            CALL ZTTYIO ('WRIT', MSGTER, FINDP, MSGLEN, MSGTET, IERR)
            IF (IPRNT.GE.IMSGPR) GO TO 45
               J1 = IMSGPR - IPRNT
               IPRNT = IPRNT + 1
               MSGTET = ' '
               MSGTET(9:8+J1) = MSGTXT(IPRNT:IPRNT+J1-1)
C                                       Skip trailing blanks
               MSGLEN = ITRIM( MSGTET)
               CALL ZTTYIO ('WRIT', MSGTER, FINDP, MSGLEN, MSGTET, IERR)
 45         CALL ZMSGCL (MSGTER, FINDP, IERR)
            GO TO 999
C                                       Error
 90   IF (KERR.NE.0) GO TO 999
         KERR = 1
         CALL ZMSGOP (MSGTER, FINDP, 0, PHNAME, IER)
         IF ((IER.NE.0) .AND. (IER.NE.1)) GO TO 95
            WRITE (MSGTET,1090) IERR, ACTION(JERR)
            CALL ZTTYIO ('WRIT', MSGTER, FINDP, 72, MSGTET, IERR)
            IF (FINDF.GT.0) CALL ZMSGCL (MSGFIL, FINDF, IERR)
            GO TO 42
C                                       close disk
 95   CALL LSERCH ('SRCH', MSGFIL, FINDF, F, IERR)
      IF ((IERR.EQ.0) .AND. (JERR.EQ.2)) GO TO 35
      IF (IERR.EQ.0) CALL ZMSGCL (MSGFIL, FINDF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' MSGCOUNT IN FILE EXCEEDS MSG REC',I16,I8)
 1025 FORMAT ('LOGFILE FOR USER ',I4,' GETTING LARGE: USE PRTMSG',
     *   ' AND CLRMSG NOW!')
 1035 FORMAT (' MSGCOUNT TO FILE EXCEEDS MSG REC',I16,I8)
 1090 FORMAT ('MSGWRT ERROR',I7,' AT ',A4)
      END

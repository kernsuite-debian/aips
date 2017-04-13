      PROGRAM GRITP
C-----------------------------------------------------------------------
C! GRITP writes contents of a Gripe file on to a FITS-like tape.
C# Utility Service TApe Gripe
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004
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
C   GRITP is an AIPS system service program which writes the contents
C   of a Gripe file on to a FITS-like tape.  The tape will be solely
C   a FITS header of the form
C          SIMPLE  =                      T
C          BITPIX  =                     16
C          NAXIS   =                      0
C                  comments = Gripes in cols 9-80, blank in cols 1-8
C          END
C   End of lines are represented by '\\' rather than CrLf.
C-----------------------------------------------------------------------
      CHARACTER TEMP*4, OBUF*2880, PHNAME*48, BKSL*2, CTEMP*80, M*1,
     *   MSGBUF*72, PRGNAM*6, LF*1, CR*1, OPER*4, REMHOS*24, LMSG*80
      HOLLERITH LBUF(256), HOBUF(720)
      INTEGER   SCR(256), HBUF(256), LUN, IND, IERR,TTYLUN, JTRIM,
     *   TTYIND, TLUN, TIND, TVOL, LREC, LPOS, NCHPR, IPOS, LBYTES,
     *   OPOS, ILBUF(256), J1, J2, K1, K2, IILF, IICR, IDENS, IFILE,
     *   I2TMP1, I, LFITS, IREC, TTY(2), REMTAP, SYSERR
      LOGICAL   T, F, EQUAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      EQUIVALENCE (ILBUF, LBUF)
      DATA LFITS /2880/
      DATA LUN, TTYLUN /27, 5/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'GRITP '/
      DATA IILF, IICR /10, 13/
      DATA BKSL /'\\'/
C-----------------------------------------------------------------------
C                                       Only ASCII
      LF = CHAR(IILF)
      CR = CHAR(IICR)
C                                       AIPS init
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 980
      TIND = 0
C                                       Open gripe file
      CALL ZPHFIL ('GR', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZFIO ('READ', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (HBUF(2).LE.0) THEN
         MSGTXT = 'There are no gripes, exiting'
         CALL MSGWRT (6)
         GO TO 980
         END IF
C                                       Tape volume #
 10   MSGBUF = 'Tape drive number ?;    0 => none'
      CALL INQINT (TTY, MSGBUF, 1, TVOL, IERR)
      IF (IERR.LT.0) GO TO 10
      IF (IERR.GT.0) GO TO 970
      IF (TVOL.GT.NTAPED) TVOL = 1
      TLUN = 129 - TVOL
      IF (TVOL.EQ.0) GO TO 200
C                                       Mount the tape?
 15   MSGBUF = 'Mount at density = ????;   0 or N => don''t mount'
      CALL INQINT (TTY, MSGBUF, 1, IDENS, IERR)
      IF (IERR.GT.0) GO TO 970
      IF ((IERR.LT.0) .OR. (IDENS.LE.0)) GO TO 20
         IF ((IDENS.NE.800) .AND. (IDENS.NE.1600) .AND. (IDENS.NE.6250))
     *      GO TO 15
         MSGBUF = 'Enter remote host name - just hit return if local'
         CALL INQSTR (TTY, MSGBUF, 24, REMHOS, IERR)
         IF (IERR.GT.0) GO TO 970
         IF (REMHOS(1:1).NE.' ') THEN
            MSGBUF = 'Remote host tape drive number:'
            CALL INQINT (TTY, MSGBUF, 1, REMTAP, IERR)
            IF (IERR.GT.0) GO TO 970
            IF (IERR.LT.0) THEN
               REMHOS = ' '
               REMTAP = 0
               END IF
            END IF
C                                       Mount
         CALL ZMOUNT (.TRUE., TVOL, IDENS, REMHOS, REMTAP, LMSG, SYSERR,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
C                                       OP code: WRITE VERIFY
 20   MSGBUF = 'Write or verify? (A4)'
      CALL INQSTR (TTY, MSGBUF, 4, TEMP, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (4, TEMP)
      IF ((TEMP.NE.'WRIT') .AND. (TEMP.NE.'VERI')) GO TO 20
C                                       Open tape
      OPER = 'WRIT'
      IF (TEMP.EQ.'VERI') OPER = 'READ'
      CALL ZPHFIL ('MT', TVOL, 0, 0, PHNAME, IERR)
      CALL ZTPOPN (TLUN, TIND, TVOL, PHNAME, OPER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         TIND = 0
         GO TO 980
         END IF
C                                       Position tape at BOF
 25   MSGBUF = 'Write in file # ?;  <= 0 => current file'
      CALL INQINT (TTY, MSGBUF, 1, IFILE, IERR)
      IF (IERR.LT.0) GO TO 25
      IF (IERR.GT.0) GO TO 970
      I = 1
      IF (IFILE.LE.0) THEN
         CALL ZTAPE ('BAKF', TLUN, TIND, I, IERR)
      ELSE
         CALL ZTAPE ('REWI', TLUN, TIND, I, IERR)
         IF (IERR.NE.0) GO TO 980
         IREC = IFILE - 1
         IF (IREC.GT.0) THEN
            CALL ZTAPE ('ADVF', TLUN, TIND, IREC, IERR)
            IF ((IERR.EQ.4) .OR. (IERR.EQ.6)) THEN
               MSGTXT = 'END OF TAPE HIT: WRITING THERE'
               CALL MSGWRT (6)
               IERR = 0
               END IF
            END IF
         END IF
      IF (IERR.NE.0) GO TO 980
      LREC = HBUF(3)
      LPOS = HBUF(4)
      NCHPR = 256 * 4
      IREC = 2
      IPOS = 1
      OPOS = 1
C                                       Read first disk record
      CALL ZFIO ('READ', LUN, IND, IREC, LBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Write the tape
      IF (TEMP.EQ.'VERI') GO TO 100
C                                       FITS commands onto tape
C                                       SIMPLE = T
 30      WRITE (MSGBUF,1030)
         I = JTRIM (MSGBUF)
         OBUF(OPOS:OPOS+79) = MSGBUF
         OPOS = OPOS + 80
C                                       BITPIX = 16
         WRITE (MSGBUF,1031)
         I = JTRIM (MSGBUF)
         OBUF(OPOS:OPOS+79) = MSGBUF
         OPOS = OPOS + 80
C                                       NAXIS = 0
         WRITE (MSGBUF,1032)
         I = JTRIM (MSGBUF)
         OBUF(OPOS:OPOS+79) = MSGBUF
         OPOS = OPOS + 80
C                                       GRIPES = nnn
         WRITE (MSGBUF,1033) HBUF(2)
         I = JTRIM (MSGBUF)
         OBUF(OPOS:OPOS+79) = MSGBUF
         OPOS = OPOS + 80
C                                       Card image loop
C                                       Blank keyword field
 50      OBUF(OPOS:OPOS+7) = ' '
            OPOS = OPOS + 8
            J1 = MIN (72, NCHPR+1-IPOS)
            IF (IREC.EQ.LREC) J1 = MIN (J1, LPOS-IPOS)
            J2 = 72 - J1
            CALL H2CHR (J1, IPOS, LBUF, OBUF(OPOS:))
            IPOS = IPOS + J1
            OPOS = OPOS + J1
            IF ((IREC.GE.LREC) .AND. (IPOS.GE.LPOS)) GO TO 80
C                                       Read new record
            IF (IPOS.LE.NCHPR) GO TO 55
               IREC = IREC + 1
               CALL ZFIO ('READ', LUN, IND, IREC, LBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               IPOS = 1
C                                       More copy - finish card
 55         IF (J2.LE.0) GO TO 60
               IF ((IREC.EQ.LREC) .AND. (J2+IPOS.GT.LPOS)) J2 = LPOS -
     *            IPOS
               CALL H2CHR (J2, IPOS, LBUF, OBUF(OPOS:))
               IPOS = IPOS + J2
               OPOS = OPOS + J2
               IF ((IREC.GE.LREC) .AND. (IPOS.GE.LPOS)) GO TO 80
C                                       Write tape record
 60         IF (OPOS.LE.2880) GO TO 50
C                                       Convert CrLf -> \\
               DO 70 I = 1,2880
                  M = OBUF(I:I)
                  IF (M.EQ.CR) OBUF(I:I) = BKSL(1:1)
                  IF (M.EQ.LF) OBUF(I:I) = BKSL(1:1)
 70               CONTINUE
               CALL ZCLC8 (2880, OBUF, 1, HOBUF)
               CALL ZTPMIO ('WRIT', TLUN, TIND, LFITS, HOBUF, 1, IERR)
               IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
               IF (IERR.NE.0) GO TO 980
               OPOS = 1
               GO TO 50
C                                       Finish last record
 80      J1 = 2881 - OPOS
         OBUF(OPOS:OPOS+J1-1) = ' '
         OPOS = ((OPOS+78) / 80) * 80 + 1
         OBUF(OPOS:OPOS+3) = 'END '
C                                       Convert CrLf -> \\
         DO 90 I = 1,2880
            M = OBUF(I:I)
            IF (M.EQ.CR) OBUF(I:I) = BKSL(1:1)
            IF (M.EQ.LF) OBUF(I:I) = BKSL(1:1)
 90         CONTINUE
         CALL ZCLC8 (2880, OBUF, 1, HOBUF)
         CALL ZTPMIO ('WRIT', TLUN, TIND, LFITS, HOBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
         IF (IERR.NE.0) GO TO 980
         I = 1
         CALL ZTAPE ('WEOF', TLUN, TIND, I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Do VERIfy too - ask user
         MSGBUF = 'Do a verify pass? (A4)'
         CALL INQSTR (TTY, MSGBUF, 4, TEMP, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL CHLTOU (4, TEMP)
         IF ((TEMP.NE.'YES ') .AND. (TEMP.NE.'VERI')) GO TO 180
C                                       Reposition the tape
         I = 2
         CALL ZTAPE ('BAKF', TLUN, TIND, I, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Reposition disk
         IREC = 2
         CALL ZFIO ('READ', LUN, IND, IREC, LBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IPOS = 1
C                                       VERIfy the tape vs disk
 100  CONTINUE
C                                       Read first tape record
         CALL ZTPMIO ('READ', TLUN, TIND, LFITS, HOBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZC8CL (2880, 1, HOBUF, OBUF)
         OPOS = 321
C                                       Convert first disk record
C                                       to CrLf -> \\
         DO 110 I = 1,NCHPR
            K1 = (I-1) / 4 + 1
            DO 105 K2 = 1,4
               CALL H2CHR (1, K2, LBUF(K1), M)
               IF (M.EQ.LF) CALL CHR2H (1, BKSL(1:1), K2, LBUF(K1))
               IF (M.EQ.CR) CALL CHR2H (1, BKSL(1:1), K2, LBUF(K1))
 105           CONTINUE
 110        CONTINUE
C                                       Card image loop
C                                       Blank keyword field skip
 120     OPOS = OPOS + 8
            J1 = MIN (72, NCHPR+1-IPOS)
            IF (IREC.EQ.LREC) J1 = MIN (J1, LPOS-IPOS)
            J2 = 72 - J1
            CALL H2CHR (J1, IPOS, LBUF, CTEMP)
            EQUAL = CTEMP(1:J1) .EQ. OBUF(OPOS:OPOS+J1-1)
            IF (.NOT.EQUAL) GO TO 165
            IPOS = IPOS + J1
            OPOS = OPOS + J1
            IF ((IREC.GE.LREC) .AND. (IPOS.GE.LPOS)) GO TO 150
C                                       Read next disk record
            IF (IPOS.LE.NCHPR) GO TO 135
               IREC = IREC + 1
               CALL ZFIO ('READ', LUN, IND, IREC, LBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               IPOS = 1
C                                       CrLf -> \\
               DO 130 I = 1,NCHPR
                  K1 = (I-1) / 4 + 1
                  DO 125 K2 = 1,4
                     CALL H2CHR (1, K2, LBUF(K1), M)
                     IF (M.EQ.LF) CALL CHR2H (1, BKSL(1:1), K2,
     *                  LBUF(K1))
                     IF (M.EQ.CR) CALL CHR2H (1, BKSL(1:1), K2,
     *                  LBUF(K1))
 125                 CONTINUE
 130              CONTINUE
 135        IF (J2.LE.0) GO TO 140
               IF ((IREC.EQ.LREC) .AND. (J2+IPOS.GT.LPOS)) J2 = LPOS -
     *            IPOS
               CALL H2CHR (J2, IPOS, LBUF, CTEMP)
               EQUAL = CTEMP(1:J2) .EQ. OBUF(OPOS:OPOS+J2-1)
               IF (.NOT.EQUAL) GO TO 160
               IPOS = IPOS + J2
               OPOS = OPOS + J2
               IF ((IREC.GE.LREC) .AND. (IPOS.GE.LPOS)) GO TO 150
C                                       Read next tape record
 140        IF (OPOS.LE.2880) GO TO 120
               CALL ZTPMIO ('READ', TLUN, TIND, LFITS, HOBUF, 1, IERR)
               IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
               IF (IERR.NE.0) GO TO 980
               OPOS = 1
               CALL ZC8CL (2880, 1, HOBUF, OBUF)
               GO TO 120
C                                       Done and ok
 150     MSGTXT = 'Tape seems to be OK'
         CALL MSGWRT (4)
         GO TO 180
C                                       Error - bad compare
 160     J1 = J2
 165     WRITE (MSGTXT,1165) IREC
         CALL MSGWRT (8)
         CALL H2CHR (J1, IPOS, LBUF, MSGTXT)
         I2TMP1 = J1 + 1
         MSGTXT(I2TMP1:I2TMP1+1) = '  '
         CALL MSGWRT (6)
         MSGTXT(1:J1) = OBUF(OPOS:OPOS+J1-1)
         I2TMP1 = J1 + 1
         MSGTXT(I2TMP1:I2TMP1+1) = '  '
         CALL MSGWRT (6)
         GO TO 980
C                                       Dismount the tape?
 180  MSGBUF = 'Do you wish to dismount the tape? Yes or no (A4)'
      CALL INQSTR (TTY, MSGBUF, 4, TEMP, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (4, TEMP)
C                                       Close and dismount
      IF (TEMP.EQ.'YES ') THEN
         CALL ZTPCLS (TLUN, TIND, IERR)
         TIND = 0
         I = 1
         CALL ZTAPE ('DMNT', TLUN, TVOL, I, IERR)
         END IF
C                                       Initialize the file
 200  MSGBUF = 'Init the file? Yes or no (A4)'
      CALL INQSTR (TTY, MSGBUF, 4, TEMP, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (4, TEMP)
C                                       Go ahead
      IF (TEMP.EQ.'YES ') THEN
C                                       require password
         CALL PASWRD (SCR, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       do init
         HBUF(2) = 0
         HBUF(3) = 2
         HBUF(4) = 1
         CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
         END IF
      GO TO 980
C                                       TTY error
 970  WRITE (MSGTXT,1970) IERR
      CALL MSGWRT (8)
C                                       Close Gripe file
 980  CALL ZCLOSE (LUN, IND, IERR)
      IF (TIND.GT.0) CALL ZTPCLS (TLUN, TIND, IERR)
      IF (TTYIND.GT.0) CALL ZCLOSE (TTYLUN, TTYIND, IERR)
C                                       Close accounting
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1015 FORMAT ('ERROR',I5,' MOUNTING TAPE')
 1020 FORMAT ('TAPE OPEN ERROR',I7)
 1030 FORMAT ('SIMPLE  =',20X,'T')
 1031 FORMAT ('BITPIX  =',19X,'16')
 1032 FORMAT ('NAXIS   =',20X,'0',10X,'/ No binary image')
 1033 FORMAT ('GRIPES  =',18X,I3,10X,'/ # AIPS gripe texts')
 1165 FORMAT ('VERIFY FAILS IN DISK RECORD',I6,' DISK, TAPE =')
 1970 FORMAT ('IO ERROR TO TERMINAL',I7)
      END

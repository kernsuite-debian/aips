      PROGRAM AIPMAN
C-----------------------------------------------------------------------
C! AIPMAN reads and prints the AIPS manual from tape.
C# Service Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   AIPMAN reads and prints the AIPS manual from a tape written by
C   PRNTMN.  Line printer is logical unit 1, talks to unit 5.
C   The program assumes that the tape is written with 100 card images
C   per block in ASCII.
C-----------------------------------------------------------------------
      CHARACTER TEMP*4, PHNAME*48,  MSGBUF*80, TC(500)*6, TCNOW*6,
     *   TR(1000)*8, TRNOW*8, BUFF(100,2)*80, PRGNAM*6
      INTEGER   NTAPE, NLINES, NTC, NTR, IPAGE, TTYLUN, TTYIND, TTY(2),
     *   ILINES, IBUFF(1024), TCP(500), TRP(1000), I, J, MAXTC, MAXTR,
     *   TLUN, TIND, NBYTES, LPLUN, LPIND, I1, I2, IT, IP1, FLAG, IERR
      LOGICAL   FIRST, T, F, WASEOF, EQUAL, LEOF, LOPAGE, LRPAGE,
     *   EQUAL1, LASER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MANCOM/ TLUN, TIND, NBYTES, LPLUN, LPIND
      COMMON /MANCHR/ TCNOW, TRNOW
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA T, F /.TRUE.,.FALSE./
      DATA MAXTC, MAXTR /500,1000/
      DATA TTYLUN /5/
      DATA PRGNAM /'AIPMAN'/
C-----------------------------------------------------------------------
C                                        Initialize
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 970
      NLINES = 0
      TCNOW = ' '
      TRNOW = ' '
      I1 = 1
      I2 = 2
      IPAGE = 0
      WASEOF = F
      NTC = 0
      NTR = 0
      FLAG = 1
      FIRST = T
C                                        Ask which tape drive?
 10   WRITE (MSGBUF,1000)
      CALL INQINT (TTY, MSGBUF, 1, NTAPE, IERR)
      IF (IERR.LT.0) GO TO 10
      IF (IERR.GT.0) GO TO 970
      NTAPE = MAX (1, NTAPE)
C                                        Is it a QMS laser printer?
      WRITE (MSGBUF,1010)
      CALL INQSTR (TTY, MSGBUF, 1, TEMP, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (1, TEMP)
      LASER = TEMP(1:1).EQ.'Y'
      IF (LASER) FLAG = 2
C                                        Read first buffer.
      CALL GETREC (FIRST, NTAPE, BUFF(1,I1), IERR)
      IF (IERR.NE.0) GO TO 980
C                                        Label first page.
      CALL PAGER (FLAG, NLINES, IPAGE)
C                                        Begin main loop.
 100  CONTINUE
C                                        Read next buffer.
         CALL GETREC (FIRST, NTAPE, BUFF(1,I2), IERR)
         WASEOF = IERR.EQ.4
         IF ((IERR.NE.0) .AND. (IERR.NE.4)) GO TO 980
C                                        Loop thru first buffer.
         DO 300 I = 1,100
C                                        Check for special records.
            LOPAGE = BUFF(I,I1)(1:2).EQ.'@@'
            LRPAGE = (LOPAGE) .AND. (BUFF(I,I1)(3:4).EQ.'@@')
            LEOF   = BUFF(I,I1)(1:2) .EQ. '$$'
C                                        Check for end of file
            IF (LEOF) GO TO 500
C                                        If normal line print.
            IF (LOPAGE) GO TO 120
               WRITE (LPLUN,1100) BUFF(I,I1)
               NLINES = NLINES + 1
C                                        Check for new page.
               IF (NLINES.GT.PRTMAX-3) CALL PAGER (FLAG, NLINES, IPAGE)
               GO TO 300
C                                        New section.
 120        TCNOW = BUFF(I,I1)(5:10)
            TRNOW = BUFF(I,I1)(11:16)
C                                        Check for required page.
            IF ((.NOT.LRPAGE) .AND. (NLINES.LT.(PRTMAX-3))) GO TO 130
               CALL PAGER (FLAG, NLINES, IPAGE)
               GO TO 200
C                                        Check if next section will fit
C                                        on current page.
 130        CONTINUE
               ILINES = 6
               IP1 = I + 1
               DO 140 J = IP1,100
                  LOPAGE = BUFF(J,I1)(1:2).EQ.'@@'
                  LEOF   = BUFF(J,I1)(1:2).EQ.'$$'
                  IF (LOPAGE.OR.LEOF) GO TO 160
                     ILINES = ILINES + 1
 140              CONTINUE
C                                        Loop thru second buffer.
               IF (WASEOF) GO TO 160
                  DO 150 J = 1,100
                     LOPAGE = BUFF(J,I2)(1:2).EQ.'@@'
                     LEOF   = BUFF(J,I2)(1:2).EQ.'$$'
                     IF (LOPAGE.OR.LEOF) GO TO 160
                        ILINES = ILINES + 1
 150                 CONTINUE
C                                        Next section will fit.
 160           IF((NLINES+ILINES).GT.(PRTMAX-3)) GO TO 180
                  NLINES = NLINES + 6
                  WRITE (LPLUN,1160)
                  GO TO 200
C                                        New page.
 180           CONTINUE
                  CALL PAGER (FLAG, NLINES, IPAGE)
C                                        Chapter index entry.
 200        EQUAL = TCNOW .EQ. TC(NTC)
            EQUAL1 = TCNOW .EQ. '      '
            IF ((EQUAL1) .OR. ((NTC.NE.0) .AND. (EQUAL))) GO TO 210
            IF (NTC.GE.MAXTC) GO TO 210
               NTC = NTC + 1
               TC(NTC) = TCNOW
               TCP(NTC) = IPAGE
C                                        Section index entry.
 210        EQUAL = TRNOW .EQ. TR(NTR)
            EQUAL1 = TRNOW .EQ. '        '
            IF ((EQUAL1) .OR. ((NTR.NE.0).AND.(EQUAL))) GO TO 300
            IF (NTR.GE.MAXTR) GO TO 300
               NTR = NTR + 1
               TR(NTR) = TRNOW
               TRP(NTR) = IPAGE
 300        CONTINUE
C                                        Move BUFF2 to BUFF1
         IF (WASEOF) GO TO 500
            IT = I1
            I1 = I2
            I2 = IT
            GO TO 100
C                                        Finished - do index.
C                                        Chapter index.
 500  TCNOW = 'INDEX '
      TRNOW = ' '
      CALL PAGER (FLAG, NLINES, IPAGE)
      IF (NTC.LT.1) GO TO 525
         DO 520 I = 1,NTC
            IF (MOD (I,5).NE.0) GO TO 510
               NLINES = NLINES + 1
               IF (NLINES.GE.(PRTMAX-3))
     *            CALL PAGER (FLAG, NLINES, IPAGE)
               IF (NLINES.LT.(PRTMAX-3)) WRITE (LPLUN,1500)
 510        NLINES = NLINES + 1
            IF (NLINES.GE.(PRTMAX-3))
     *         CALL PAGER (FLAG, NLINES, IPAGE)
            WRITE (LPLUN,1510) TC(I), TCP(I)
 520        CONTINUE
C                                         Section index.
 525  NLINES = NLINES + 4
      IF ((NLINES.GT.PRTMAX-3) .AND. (NTR.GT.0)) WRITE (LPLUN,1525)
       DO 540 I = 1,NTR
         IF (MOD(I,5).NE.0) GO TO 530
            NLINES = NLINES + 1
            IF (NLINES.GE.(PRTMAX-3)) CALL PAGER (FLAG, NLINES, IPAGE)
            IF (NLINES.LT.(PRTMAX-3)) WRITE (LPLUN,1500)
 530     NLINES = NLINES + 1
         IF (NLINES.GE.(PRTMAX-3)) CALL PAGER (FLAG, NLINES, IPAGE)
         WRITE (LPLUN,1530) TR(I), TRP(I)
 540     CONTINUE
C                                        Finished - close printer.
      FLAG = -1
      IF (LASER) FLAG = -2
      CALL PAGER (FLAG, NLINES, IPAGE)
      GO TO 980
C                                       Error: TTY
 970  WRITE (MSGTXT,1970) IERR
      CALL MSGWRT (7)
C                                       Close TTY
 980  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter tape drive number (I)')
 1010 FORMAT ('Is it a QMS laser printer? Yes or no (A4)')
 1100 FORMAT (1X,A80)
 1160 FORMAT (//////)
 1500 FORMAT ('   ')
 1510 FORMAT (26X,A6,4X,I5)
 1525 FORMAT (////)
 1530 FORMAT (26X,A8,4X,I5)
 1970 FORMAT ('TTY I/O ERROR = ',I6)
      END
      SUBROUTINE PAGER (FLAG, NLINES, IPAGE)
C-----------------------------------------------------------------------
C   PAGER  opens and closes the line printer and prints page headers.
C   Inputs:
C      FLAG    I    If 2 open laser file, then write laser commands
C                   and page header.
C                   If 1 open file then write page header.
C                   If 0 write page header.
C                   If -1 close line printer.
C      IPAGE    I   Page number.
C   In/Output:
C      NLINES   I   Line count initialized to 3
C      IPAGE    I   Page count incremented.
C-----------------------------------------------------------------------
      CHARACTER LPNAME*48, OUTFIL*48, TCNOW*6, TRNOW*8, CTYP(4)*2
      INTEGER   FLAG, NLINES, IPAGE, TIND, TLUN, NBYTES, LPLUN, LPIND,
     *    IERR, I
      LOGICAL   T, F
      COMMON /MANCOM/ TLUN, TIND, NBYTES, LPLUN, LPIND
      COMMON /MANCHR/ TCNOW, TRNOW
      DATA T, F /.TRUE.,.FALSE./
      DATA CTYP /'1C','2C','3C','4C'/
      DATA OUTFIL /' '/
C-----------------------------------------------------------------------
C                                        Check FLAG
      IF (FLAG) 300, 200, 100
C                                        Open
 100  LPLUN = 1
C                                       non-QMS
      IF (FLAG.EQ.2) GO TO 120
         CALL ZPHFIL ('LP', 1, 1, 1, LPNAME, IERR)
         CALL ZOPEN (LPLUN, LPIND, 1, LPNAME, F, T, T, IERR)
         GO TO 150
C                                       QMS
 120  CONTINUE
         LPIND = 0
         CALL ZLASIO ('POPN', LPLUN, OUTFIL, 1, LPNAME, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (LPLUN, 1120)
         WRITE (LPLUN, 1121)
 150  IF (IERR.NE.0) GO TO 999
      FLAG = 0
C                                        Page header.
 200  IF (NLINES.EQ.3) GO TO 999
      IPAGE = IPAGE + 1
      NLINES = 3
      IF (IPAGE.GT.1) GO TO 210
         WRITE (LPLUN, 1200)
         GO TO 999
 210  CONTINUE
         DO 215 I = 1,4
            IF (CTYP(I).EQ.TCNOW(1:2)) GO TO 220
 215        CONTINUE
         WRITE (LPLUN,1215) TCNOW, TRNOW, IPAGE
         GO TO 999
 220  CONTINUE
         WRITE (LPLUN,1220) TCNOW, TRNOW, IPAGE
         GO TO 999
C                                        Close printer.
 300  IF (FLAG.EQ.-2) GO TO 320
         CALL ZCLOSE (LPLUN, LPIND, IERR)
         GO TO 999
 320  CONTINUE
         WRITE (LPLUN,1320)
         WRITE (LPLUN,1321)
         CALL ZLASIO ('CLOS', LPLUN, OUTFIL, 1, LPNAME, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT (' ^PY^-')
 1121 FORMAT (' ^IOP^IL0600^IS404^IC1000^IJ00450^IT00700^N``PN`-')
 1200 FORMAT ('1',16X///)
 1215 FORMAT ('1',A6,22X,A8,22X,'PAGE',I7///)
 1220 FORMAT ('1','Volume ',A1,'  Chapter ',A2,' ',A2,13X,A8,15X,
     *   'PAGE',I7///)
 1320 FORMAT (' `PY`-')
 1321 FORMAT (' `N^^PN^-')
      END
      SUBROUTINE GETREC (FIRST, NTAPE, REC, IERR)
C-----------------------------------------------------------------------
C   GETREC reads the tape and returns 8000 byte records.  Will also
C   open and close tape file if necessary.
C   Inputs:
C      FIRST     L    If true then open tape.
C      NTAPE     I    Tape drive number.
C   Output:
C      REC(8000) C    Data record from tape.
C      IERR      I    Error code, 0=>OK, 4=>EOF, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER REC*8000, TNAME*48
      INTEGER   NTAPE, IERR, TLUN, TIND, NBYTES, JERR, LPLUN, LPIND,
     *    LBYTES, LREC(4000)
      LOGICAL   T, F, FIRST
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MANCOM/ TLUN, TIND, NBYTES, LPLUN, LPIND
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize
      NBYTES = 8000
      TLUN = 30 + NTAPE
C                                        Check if need to open tape.
C                                        Open tape.
      IF (.NOT.FIRST) GO TO 100
         CALL ZPHFIL ('MT', NTAPE, 1, 1, TNAME, JERR)
         CALL ZTPOPN (TLUN, TIND, NTAPE, TNAME, 'READ', IERR)
         FIRST = F
         IF (IERR.NE.0) GO TO 999
C                                        Read record.
 100  CALL ZTPMIO ('READ', TLUN, TIND, NBYTES, LREC, 1, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (LBYTES.LT.NBYTES-2) IERR = 3
      IF (LBYTES.LT.NBYTES-2) GO TO 900
      CALL ZC8CL (NBYTES, 1, LREC, REC)
      GO TO 999
C                                       Error:
 900  WRITE (MSGTXT,1900) LBYTES, IERR
      IF (IERR.NE.4) CALL MSGWRT (8)
      CALL ZTPCLS (TLUN, TIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('GETREC: I/O ERROR: BYTES=',I8,' IERR=',I5)
      END

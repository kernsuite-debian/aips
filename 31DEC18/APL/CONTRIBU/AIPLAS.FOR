      PROGRAM AIPLAS
C-----------------------------------------------------------------------
C! AIPLAS reads AIPS manual from tape and prints on laser printer.
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C
C   PROBABLY DOES NOT WORK AT THE MOMENT - INCOMPLETE USE OF ZLASIO
C
C   AIPLAS reads and prints the AIPS manual from a tape written by
C   PRNTMN.  Line printer is logical unit 1, talks to unit 5.
C   This version is designed for the Laser printer in a small font
C   with two columns.
C   AIPMAN needs routines from the AIPS [APL] library.
C-----------------------------------------------------------------------
      CHARACTER MSGBUF*80, PHNAME*48, PRGNAM*6, PGBUF(2,65)*80,
     *   BUFF(100,2)*80, TCNOW*6, TRNOW*8, ICBEG*14, ICEND*14,
     *   TC(500)*6, TR(1000)*8
      INTEGER   NTAPE, IBUFF, NLINES, NTC, NTR, IPAGE, TTYLUN,
     *   TTYIND, I, J, I1, I2, IT, IP1, ILINES, TCP(500), TRP(1000),
     *   MAXTC, MAXTR, TLUN, TIND, NBYTES, LPLUN, LPIND,
     *   FLAG, IERR, PGCOL, TTY(2)
      LOGICAL   FIRST, T, F, WASEOF, EQUAL, LEOF, LOPAGE, LRPAGE,
     *   EQUAL1, LASER, DOIT, DOHELP, ICEDO
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MANCOM/ DOIT, DOHELP, TLUN, TIND, NBYTES, LPLUN, LPIND,
     *   PGCOL
      COMMON /MANCHR/ TCNOW, TRNOW, PGBUF
      EQUIVALENCE (TTY(1), TTYLUN),   (TTY(2), TTYIND)
      DATA T, F /.TRUE.,.FALSE./
      DATA MAXTC, MAXTR /500,1000/
      DATA TTYLUN /5/
      DATA PRGNAM /'AIPLAS'/
C-----------------------------------------------------------------------
C                                       Initialize
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       shorten page a bit !!!
      PRTMAX = PRTMAX - 3
      NLINES = 0
      TCNOW = ' '
      TRNOW = ' '
      I1 = 1
      I2 = 2
      IPAGE = 0
      WASEOF = F
      NTC = 0
      NTR = 0
      FLAG = 2
      FIRST = T
      LASER = T
C                                        Ask which tape drive?
 10   WRITE (MSGBUF,1000)
      CALL INQINT (TTY, MSGBUF, 1, NTAPE, IERR)
      IF (IERR.LT.0) GO TO 10
      IF (IERR.GT.0) GO TO 970
      NTAPE = MAX (1, NTAPE)
C                                        Is there a start point?
      WRITE (MSGBUF,1010)
      CALL INQSTR (TTY, MSGBUF, 14, ICBEG, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (14, ICBEG)
      DOIT = ICBEG.EQ.' '
C                                        Is there an end point?
      WRITE (MSGBUF,1020)
      CALL INQSTR (TTY, MSGBUF, 14, ICEND, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHLTOU (14, ICEND)
      ICEDO = ICEND.NE.' '
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
         IF ((IERR.NE.0) .AND. (IERR.NE.4)) GO TO 550
C                                        Loop thru first buffer.
         DO 300 I = 1,100
C                                        Check for special records.
            LOPAGE = BUFF(I,I1)(1:2) .EQ. '@@'
            LRPAGE = (LOPAGE) .AND. (BUFF(I,I1)(3:4).EQ.'@@')
            LEOF   = BUFF(I,I1)(1:2) .EQ. '$$'
C                                        Check for end of file
            IF (LEOF) GO TO 500
C                                        If normal line print.
            IF (LOPAGE) GO TO 120
               PGBUF(PGCOL,NLINES) = BUFF(I,I1)
C                                        Check for new page.
               NLINES = NLINES + 1
               IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
               GO TO 300
C                                        New section.
 120        TCNOW = BUFF(I,I1)(5:10)
            TRNOW = BUFF(I,I1)(11:16)
C                                       Do we start printing now?
            IF (DOIT) GO TO 121
               EQUAL = TCNOW .EQ. ICBEG(1:6)
               EQUAL1 = TRNOW .EQ. ICBEG(7:14)
               DOIT = EQUAL .AND. EQUAL1
               GO TO 125
C                                       or end?
 121        IF (.NOT.ICEDO) GO TO 125
               EQUAL = TCNOW .EQ. ICEND(1:6)
               EQUAL1 = TRNOW .EQ. ICEND (7:14)
               IF ((EQUAL) .AND. (EQUAL1)) GO TO 500
C                                        Check for required page.
 125        IF ((.NOT.LRPAGE) .AND. (NLINES.LE.PRTMAX)) GO TO 130
               CALL PAGER (FLAG, NLINES, IPAGE)
               GO TO 200
C                                        Check if next section will fit
C                                        on current page.
 130        CONTINUE
               IF (NLINES.LE.4) GO TO 200
               IF (NLINES+6.GT.PRTMAX) GO TO 180
                  ILINES = 6
                  IP1 = I + 1
                  IF (IP1.GT.100) GO TO 145
                     DO 140 J = IP1,100
                        LOPAGE = BUFF(J,I1)(1:2) .EQ. '@@'
                        LEOF   = BUFF(J,I1)(1:2) .EQ. '$$'
                        IF ((LOPAGE) .OR. (LEOF)) GO TO 160
                           ILINES = ILINES + 1
 140                    CONTINUE
C                                        Loop thru second buffer.
 145           IF (WASEOF) GO TO 160
                  DO 150 J = 1,100
                     LOPAGE = BUFF(J,I2)(1:2) .EQ. '@@'
                     LEOF   = BUFF(J,I2)(1:2) .EQ. '$$'
                     IF ((LOPAGE) .OR. (LEOF)) GO TO 160
                        ILINES = ILINES + 1
 150                 CONTINUE
C                                        Next section will fit.
 160           IF (NLINES+ILINES.GT.2*PRTMAX) GO TO 180
               IF ((NLINES+ILINES.GT.PRTMAX) .AND. (PGCOL.EQ.2))
     *            GO TO 180
                  NLINES = NLINES + 6
                  GO TO 200
C                                        New page.
 180           CONTINUE
                  CALL PAGER (FLAG, NLINES, IPAGE)
C                                        Chapter index entry.
 200        EQUAL = TCNOW .EQ. TC(NTC)
            EQUAL1 = TCNOW .EQ. ' '
            IF ((EQUAL1) .OR. ((NTC.NE.0) .AND. (EQUAL))) GO TO 210
            IF (NTC.GE.MAXTC) GO TO 210
               NTC = NTC + 1
               TC(NTC) = TCNOW
               TCP(NTC) = IPAGE
               WRITE (MSGTXT,1200) TCNOW, IPAGE
               CALL MSGWRT (2)
               EQUAL1 = TC(NTC)(1:4) .EQ. '1C04'
               EQUAL = TC(NTC-1)(1:4) .EQ. 'IC04'
C                                       Force to odd page at start helps
               IF ((EQUAL) .OR. (.NOT.EQUAL1)) GO TO 205
                  IF (IPAGE.LE.0) GO TO 205
                  IF (((IPAGE/2)*2.NE.IPAGE) .AND. (NLINES.LE.4))
     *               GO TO 205
                  NLINES = MAX (5, NLINES)
                  DOHELP = EQUAL1
                  CALL PAGER (FLAG, NLINES, IPAGE)
                  IF (((IPAGE/2)*2.NE.IPAGE) .AND. (NLINES.LE.4))
     *               GO TO 205
                  NLINES = MAX (5, NLINES)
                  CALL PAGER (FLAG, NLINES, IPAGE)
 205           DOHELP = EQUAL1
C                                        Section index entry.
 210        EQUAL = TRNOW .EQ. TR(NTR)
            EQUAL1 = TRNOW .EQ. ' '
            IF ((EQUAL1) .OR. ((NTR.NE.0).AND.(EQUAL))) GO TO 300
            IF (NTR.GE.MAXTR) GO TO 300
               NTR = NTR + 1
               TR(NTR) = TRNOW
               TRP(NTR) = IPAGE
               WRITE (MSGTXT,1210) TRNOW, IPAGE
               CALL MSGWRT (1)
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
               IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
 510        WRITE (MSGBUF,1510) TC(I), TCP(I)
            PGBUF(PGCOL,NLINES)(1:) = MSGBUF(1:)
            NLINES = NLINES + 1
            IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
 520        CONTINUE
C                                         Section index.
 525  IF (NTR.LE.0) GO TO 550
         IF ((NTC.GT.0) .AND. (NLINES.NE.4)) NLINES = NLINES + 4
         IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
         DO 540 I = 1,NTR
            IF (MOD(I,5).NE.0) GO TO 530
               NLINES = NLINES + 1
               IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
 530        WRITE (MSGBUF,1530) TR, TRP(I)
            PGBUF(PGCOL,NLINES)(1:) = MSGBUF(1:)
            NLINES = NLINES + 1
            IF (NLINES.GT.PRTMAX) CALL PAGER (FLAG, NLINES, IPAGE)
 540        CONTINUE
C                                        Finished - close printer.
 550  FLAG = -1
      IF (LASER) FLAG = -2
      CALL PAGER (FLAG, NLINES, IPAGE)
      GO TO 980
C                                       Error: TTY
 970  WRITE (MSGTXT,1970) IERR
      CALL MSGWRT (7)
C                                       Close TTY
 980  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
C
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter tape drive number (I)')
 1010 FORMAT ('Start chapter, member names (A6,A8)')
 1020 FORMAT ('End chapter, member names (A6,A8)')
 1200 FORMAT ('At chapter ',A6,'  Page',I5)
 1210 FORMAT ('At section ',A8,'  Page',I5)
 1510 FORMAT (25X,A6,4X,I5)
 1530 FORMAT (25X,A8,4X,I5)
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
      CHARACTER LPNAME*48,  OUTFIL*48, XC*80,
     *   CTYP(4)*2, PGBUF(2,65)*80, TCNOW*6, TRNOW*8
      INTEGER   FLAG, NLINES, IPAGE, TIND, TLUN, NBYTES,
     *   LPLUN, LPIND, IERR, I, IT, J, PGCOL, ISIZE
      LOGICAL   T, F, DOIT, DOHELP
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MANCOM/ DOIT, DOHELP, TLUN, TIND, NBYTES, LPLUN, LPIND,
     *   PGCOL
      COMMON /MANCHR/ TCNOW, TRNOW, PGBUF
      DATA T, F /.TRUE.,.FALSE./
      DATA CTYP /'1C','2C','3C','4C'/
      DATA OUTFIL /' '/
C-----------------------------------------------------------------------
      ISIZE = 40 * 2 * 65
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
      NLINES = 0
      IPAGE = 0
      GO TO 250
C                                       Page header.
 200  IF ((NLINES.EQ.4) .OR. ((IPAGE/2)*2.NE.IPAGE) .OR. (.NOT.DOIT))
     *   GO TO 250
C                                       Write it out
 210  CONTINUE
         IT = MOD (IPAGE-1, 4)
         IF ((IT.LT.2) .AND. (.NOT.DOHELP)) WRITE (LPLUN,1210)
     *      PGBUF(1,1), PGBUF(2,1)
         IF ((IT.LT.2) .AND. (DOHELP)) WRITE (LPLUN,1211)
     *      PGBUF(1,1), PGBUF(2,1)
         IF ((IT.GE.2) .AND. (.NOT.DOHELP)) WRITE (LPLUN,1212)
     *      PGBUF(1,1), PGBUF(2,1)
         IF ((IT.GE.2) .AND. (DOHELP)) WRITE (LPLUN,1213)
     *      PGBUF(1,1), PGBUF(2,1)
         DO 220 J = 2,PRTMAX
            IF (.NOT.DOHELP) WRITE (LPLUN,1214) PGBUF(1,J), PGBUF(2,J)
            IF (DOHELP) WRITE (LPLUN,1215) PGBUF(1,J), PGBUF(2,J)
 220        CONTINUE
         IF (FLAG.LT.0) GO TO 310
C                                       advance page
 250  IF ((IPAGE/2)*2.EQ.IPAGE) PGBUF(1,1) = ' '
      IF (NLINES.NE.4) IPAGE = IPAGE + 1
      NLINES = 4
      PGCOL = MOD (IPAGE-1, 2) + 1
      IF (IPAGE.EQ.1) GO TO 999
      DO 260 I = 1,4
         IF (CTYP(I).EQ.TCNOW(1:2)) GO TO 270
 260     CONTINUE
      IF (DOHELP) THEN
         WRITE (XC,1261) TCNOW, TRNOW, IPAGE
      ELSE
         WRITE (XC,1260) TCNOW, TRNOW, IPAGE
         END IF
      GO TO 280
 270  IF (DOHELP) THEN
         WRITE (XC,1271) TCNOW(1:1), TCNOW(2:3), TCNOW(4:5), TRNOW,
     *      IPAGE
      ELSE
         WRITE (XC,1270) TCNOW(1:1), TCNOW(2:3), TCNOW(4:5), TRNOW,
     *      IPAGE
         END IF
 280  PGBUF(PGCOL,1) = XC
      GO TO 999
C                                       Do last print ?
 300  IF ((IPAGE.GT.0) .AND. (DOIT) .AND. (NLINES.GT.4) .OR.
     *   (PGCOL.GT.1)) GO TO 210
C                                       Close printer.
 310  IF (FLAG.EQ.-2) GO TO 320
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
 1121 FORMAT (' ^IOL^IL0810^IS010^IC1400^IJ00560^IT00350^N``PN`-')
 1210 FORMAT ('1'///1X,A72,6X,A72)
 1211 FORMAT ('1'///7X,A64,10X,A64)
 1212 FORMAT ('1',A72,6X,A72)
 1213 FORMAT ('1',6X,A64,10X,A64)
 1214 FORMAT (1X,A72,6X,A72)
 1215 FORMAT (7X,A64,10X,A64)
 1260 FORMAT (A6,22X,A8,22X,'PAGE',I5)
 1261 FORMAT (A6,19X,A8,19X,'PAGE',I5)
 1270 FORMAT ('Volume ',A1,' Chapter ',A2,' ',A2,15X,A8,15X,
     *   'Page',I5)
 1271 FORMAT ('Volume ',A1,' Chapter ',A2,' ',A2,12X,A8,12X,
     *   'Page',I5)
 1320 FORMAT (' `PY`-')
 1321 FORMAT (' `N^^IOP^IL0600^IS404^IC1000^IJ00450^IT00700^PN^-')
      END
      SUBROUTINE GETREC (FIRST, NTAPE, REC, IERR)
C-----------------------------------------------------------------------
C   GETREC reads the tape and returns 8000 byte records.  Will also
C   open and close tape file if necessary.
C   Inputs:
C      FIRST     L         If true then open tape.
C      NTAPE     I         Tape drive number.
C   Output:
C      REC       C*8000    Data record from tape.
C      IERR      I         Error code, 0=>OK, 4=>EOF, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER  REC*8000, TNAME*48
      INTEGER   NTAPE, IERR, TLUN, TIND, NBYTES, JERR, LPLUN, LPIND,
     *   PGCOL
      LOGICAL   T, F, FIRST, DOIT, DOHELP
      REAL      LREC(2000)
      COMMON /MANCOM/ DOIT, DOHELP, TLUN, TIND, NBYTES, LPLUN, LPIND,
     *   PGCOL
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize
      NBYTES = 8000
      TLUN = 31
C                                        Check if need to open tape.
C                                        Open tape.
      IF (.NOT.FIRST) GO TO 100
         CALL ZPHFIL ('MT', NTAPE, 1, 1, TNAME, JERR)
         CALL ZOPEN (TLUN, TIND, NTAPE, TNAME, F, T, T, IERR)
         FIRST = F
         IF (IERR.NE.0) GO TO 999
C                                        Read record.
 100  CALL ZFIO ('READ', TLUN, TIND, NBYTES, LREC, IERR)
C                                        If EOF close file.
      IF (IERR.EQ.4) CALL ZCLOSE (TLUN, TIND, JERR)
      IF (IERR.EQ.0) CALL H2CHR (NBYTES, 1, LREC, REC)
C
 999  RETURN
      END

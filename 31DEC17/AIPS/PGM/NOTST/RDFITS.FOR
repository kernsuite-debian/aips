      PROGRAM RDFITS
C-----------------------------------------------------------------------
C! RDFITS performs various operations on a FITS-like tape.
C# FITS Utility Service Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2015
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
C   RDFITS is a service routine to play with FITS-like tapes.  It
C   performs operations to move tapes:
C       Rewind
C       Advance record and file
C       Back record and file
C       Read
C       Index (list first card image of each file, count records)
C-----------------------------------------------------------------------
      CHARACTER FITS*80, PHNAME*48, PRGNAM*6, TEMP*4, MSGBUF*80
      INTEGER   NREC, TLUN, TIND, TVOL, IPNT, I, J, TTYLUN, TTYIND,
     *   IERR, PRTLUN, PRTIND, NBL, IXJ, KREC, NFILE, IFIL, MBYTE,
     *   LBYTE, LBL, JBL, TTY(2)
      REAL      TAPBUF(7200)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA PRGNAM /'RDFITS'/
      DATA TTYLUN, PRTLUN /5, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 980
      PRTIND = 0
      TIND = 0
C                                       Tape volume #, open
 10   WRITE (MSGBUF,1000)
      CALL INQINT (TTY, MSGBUF, 1, TVOL, IERR)
      IF (IERR.LT.0) GO TO 10
      IF (IERR.GT.0) GO TO 980
      TLUN = 129 - TVOL
      CALL ZPHFIL ('MT', TVOL, 0, 0, PHNAME, IERR)
      CALL ZTPOPN (TLUN, TIND, TVOL, PHNAME, 'READ', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1002) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Open printer
 15   WRITE (MSGBUF,1015)
      CALL INQSTR (TTY, MSGBUF, 48, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL CHLTOU (48, PHNAME)
      IF (PHNAME.EQ.'TEMP') PHNAME = ' '
      CALL ZOPEN (PRTLUN, PRTIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       OP LOOP
C                                       get operation
 20   WRITE (MSGBUF,1020)
      CALL INQSTR (TTY, MSGBUF, 4, TEMP, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL CHLTOU (4, TEMP)
      IF (TEMP.EQ.'REWI') GO TO 100
      IF (TEMP.EQ.'QUIT') GO TO 990
      IF (TEMP.EQ.'ADVF') GO TO 30
      IF (TEMP.EQ.'INDX') GO TO 30
      IF (TEMP.EQ.'KEYI') GO TO 30
C                                       get number of records
 25      WRITE (MSGBUF,1025)
         CALL INQINT (TTY, MSGBUF, 1, NREC, IERR)
         IF (IERR.LT.0) GO TO 25
         IF (IERR.GT.0) GO TO 980
C                                       get number of files
 30   IF (TEMP.EQ.'ADVR') GO TO 300
         WRITE (MSGBUF,1030)
         CALL INQINT (TTY, MSGBUF, 1, NFILE, IERR)
         IF (IERR.LT.0) GO TO 30
         IF (IERR.GT.0) GO TO 980
      IF (TEMP.EQ.'ADVF') GO TO 200
      IF (TEMP.EQ.'READ') GO TO 400
      IF (TEMP.EQ.'INDX') GO TO 500
      IF (TEMP.EQ.'KEYI') GO TO 600
      GO TO 20
C-----------------------------------------------------------------------
C                                       Rewind
 100  J = 1
      CALL ZTAPE ('REWI', TLUN, TIND, J, IERR)
 110  IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (7)
         GO TO 20
C-----------------------------------------------------------------------
C                                       Advance file
 200  IF (NFILE.LE.0) GO TO 210
         CALL ZTAPE ('ADVF', TLUN, TIND, NFILE, IERR)
         GO TO 110
 210  CONTINUE
         NFILE = 1 - NFILE
         CALL ZTAPE ('BAKF', TLUN, TIND, NFILE, IERR)
         GO TO 110
C-----------------------------------------------------------------------
C                                       Advance record
 300  IF (NREC.LE.0) GO TO 310
         CALL ZTAPE ('ADVR', TLUN, TIND, NREC, IERR)
         GO TO 110
 310  CONTINUE
         NREC = 1 - NREC
         CALL ZTAPE ('BAKR', TLUN, TIND, NREC, IERR)
         GO TO 110
C-----------------------------------------------------------------------
C                                       Read and print ASCII
 400  IF (NFILE.LE.0) NFILE = 10000
      IF (NREC.LE.0) NREC = 10000
      MBYTE = 28800
      DO 430 IFIL = 1,NFILE
         IPNT = -79
         CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
         IF (IERR.EQ.4) GO TO 425
         IF (IERR.NE.0) GO TO 110
         LBL = LBYTE / 2880
         NBL = MAX (1, LBL)
         DO 420 I = 1,NREC,NBL
            IF (I.EQ.1) GO TO 405
               IPNT = -79
               CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
               IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
               IF (IERR.EQ.4) GO TO 425
               IF (IERR.NE.0) GO TO 110
               LBL = LBYTE / 2880
 405        WRITE (1,1405) I, LBL
            LBL = LBL + I - 1
            IF (LBL.GT.NREC) LBL = NREC
            DO 410 JBL = I,LBL
               DO 410 J = 1,36
                  IPNT = IPNT + 80
                  CALL ZC8CL (80, IPNT, TAPBUF, FITS)
                  WRITE (1,1410) FITS
                  IF (FITS(1:8).EQ.'END     ') GO TO 425
 410           CONTINUE
 420        CONTINUE
 425     WRITE (MSGTXT,1571) IFIL
         IF ((IERR.EQ.4) .AND. (I.EQ.1)) CALL MSGWRT (2)
         IF ((IERR.EQ.4) .AND. (I.EQ.1)) GO TO 110
         IF (IERR.EQ.4) GO TO 430
            J = 1
            CALL ZTAPE ('ADVF', TLUN, TIND, J, IERR)
 430     CONTINUE
      GO TO 110
C-----------------------------------------------------------------------
C                                       Index
 500  IF (NFILE.LE.0) NFILE = 10000
      MBYTE = 28800
      DO 550 IXJ = 1,NFILE
         KREC = 0
         CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
         IF (IERR.NE.0) GO TO 560
         CALL ZC8CL (80, 1, TAPBUF, FITS)
         KREC = LBYTE / 2880
         DO 510 I = 1,10000
            CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
            IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
            IF (IERR.EQ.4) GO TO 520
            IF (IERR.NE.0) GO TO 560
            KREC = KREC + LBYTE / 2880
 510        CONTINUE
 520     WRITE (1,1520) IXJ, KREC, FITS
 550     CONTINUE
      GO TO 20
 560  IF (IERR.EQ.4) GO TO 570
         WRITE (1,1560) IERR, KREC, IXJ
         GO TO 110
 570  IXJ = IXJ - 1
      IF (IERR.EQ.4) WRITE (1,1570) IXJ
      WRITE (MSGTXT,1571) IXJ
      CALL MSGWRT (2)
      GO TO 20
C-----------------------------------------------------------------------
C                                       Read and print ASCII keywords
 600  IF (NFILE.LE.0) NFILE = 10000
      NREC = 10000
      MBYTE = 28800
      DO 630 IFIL = 1,NFILE
         WRITE (1,1600) IFIL
         IPNT = -79
         CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
         IF (IERR.EQ.4) GO TO 625
         IF (IERR.NE.0) GO TO 110
         LBL = LBYTE / 2880
         NBL = MAX (1, LBL)
         DO 620 I = 1,NREC,NBL
            IF (I.EQ.1) GO TO 605
               IPNT = -79
               CALL ZTPMIO ('READ', TLUN, TIND, MBYTE, TAPBUF, 1, IERR)
               IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTE, IERR)
               IF (IERR.EQ.4) GO TO 625
               IF (IERR.NE.0) GO TO 110
               LBL = LBYTE / 2880
 605        LBL = LBL + I - 1
            IF (LBL.GT.NREC) LBL = NREC
            DO 610 JBL = I,LBL
               DO 610 J = 1,36
                  IPNT = IPNT + 80
                  CALL ZC8CL (80, IPNT, TAPBUF, FITS)
                  IF (FITS(1:8).EQ.'        ') GO TO 610
                     WRITE (1,1410) FITS
                     IF (FITS(1:8).EQ.'END     ') GO TO 625
 610           CONTINUE
 620        CONTINUE
 625     WRITE (MSGTXT,1571) IFIL
         IF ((IERR.EQ.4) .AND. (I.EQ.1)) CALL MSGWRT (2)
         IF ((IERR.EQ.4) .AND. (I.EQ.1)) GO TO 110
         IF (IERR.EQ.4) GO TO 630
            J = 1
            CALL ZTAPE ('ADVF', TLUN, TIND, J, IERR)
 630     CONTINUE
      GO TO 110
C-----------------------------------------------------------------------
C                                       Terminal error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close files
 990  IF (TIND.GT.0) CALL ZTPCLS (TLUN, TIND, IERR)
      IF (TTYIND.GT.0) CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      IF (PRTIND.GT.0) CALL ZCLOSE (PRTLUN, PRTIND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Which tape drive (I)')
 1002 FORMAT ('TAPE OPEN ERROR ',I8)
 1015 FORMAT ('Printer output file name (''TEMP'' => scratch)')
 1020 FORMAT ('Op: Rewi, Advr, Advf, Read, Indx, Keyi, Quit (A4)')
 1025 FORMAT ('How many recs +- (I) <= 0 -> 10000 for read')
 1030 FORMAT ('How many files +- (I) <= 0 -> 10000 for read, indx')
 1110 FORMAT ('IERR = ',I9)
 1405 FORMAT ('1   Rec = ',I6,'  Blocking factor =',I3//)
 1410 FORMAT ('   ',A80)
 1520 FORMAT (' Rel file #',I5,' # Recs',I6,' First line ',A80)
 1560 FORMAT (' ERROR',I8,' AT REC ',I5,' FILE',I5)
 1570 FORMAT ('0End of tape after',I6,' files')
 1571 FORMAT ('End of tape after',I6,' files')
 1600 FORMAT ('1   File = ',I6//)
 1980 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END

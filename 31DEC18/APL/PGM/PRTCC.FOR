LOCAL INCLUDE 'PRTCC.INC'
      REAL      XSEQ, XDISK, XVER, XBCNT, XECNT, XINC, DOCRT, DOCELL
      HOLLERITH XINNAM(3), XINCLS(2), XLPNAM(12)
      CHARACTER INNAM*12, INCLS*6, LPNAME*48
      INTEGER   BCOUNT, ECOUNT, ICOUNT, DATP(128,2), BUFFER(512),
     *   INSEQ, INDISK, INVERS, CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND,
     *   NACROS
      LOGICAL   ISUV
C
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XBCNT, XECNT,
     *   XINC, DOCRT, XLPNAM, DOCELL
      COMMON /CHPARM / INNAM, INCLS, LPNAME
      COMMON /PRTCCC/ BCOUNT, ECOUNT, ICOUNT, DATP, BUFFER, INSEQ,
     *   INDISK, INVERS, CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND, NACROS,
     *   ISUV
LOCAL END
      PROGRAM PRTCC
C-----------------------------------------------------------------------
C! Prints clean component files on printer or terminal
C# EXT-util Map-util Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2004, 2007-2009, 2016
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
C   PRTCC is an AIPS task to print the contents of clean components
C   (tables format) extension files on the line printer or the CRT
C   terminal.
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)     Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)     Image class: ditto
C     INSEQ      XSEQ (INSEQ) Image sequence number: ditto
C     INDISK     XDISK (INDISK)  Image disk number: 0 -> any
C     INVERS     XVER (INVERS)   Extension version number
C     BCOUNT     XBCNT (BCOUNT)  First row number
C     ECOUNT     XECNT (ECOUNT)  Last row number
C     XINC       XINC (ICOUNT)   Increment between printed rows
C     DOCRT      DOCRT           > 0. => CRT, else line printer
C     OUTPRINT   LPNAME          File name to keep printer output in
C     DOCELL     DOCELL          > 0. => x,y in cells
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTCC.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PRCCIN (IRET)
C                                       do printing
      IF (IRET.EQ.0) THEN
         CALL PRCCHK (IRET)
         IF (IRET.EQ.0) CALL PRCCDO (IRET)
         IRET = MAX (0, IRET)
         END IF
C                                       close down
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE PRCCIN (IRET)
C-----------------------------------------------------------------------
C   PRCCIN performs initialization for AIPS task PRTCC.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output:
C      IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   IRET, I4T, NPARM, IROUND, NREC, TABLUN, IERR,
     *   SCRBUF(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTCC.INC'
      DATA TABLUN /27/
      DATA PRGN /'PRTCC '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 25
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       restart AIPS
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       find image file
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      ICOUNT = IROUND (XINC)
      BCOUNT = IROUND (XBCNT)
      ECOUNT = IROUND (XECNT)
      IF (ICOUNT.LE.0) ICOUNT = 1
      IF (BCOUNT.LE.0) BCOUNT = 1
      IUSER = NLUSER
      CNO = 1
      INTYP = ' '
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file read
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'READ', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 0
      ISUV = INTYP.NE.'MA'
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'CC', INDISK, CNO, INVERS, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, 'CC', INVERS
         GO TO 990
         END IF
      I4T = BUFFER(5)
      IF (BCOUNT.GT.I4T) BCOUNT = 1
      IF ((ECOUNT.LT.BCOUNT) .OR. (ECOUNT.GT.I4T)) ECOUNT = I4T
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, OUTLUN, OUTIND, NACROS, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
 1030 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE PRCCHK (IRET)
C-----------------------------------------------------------------------
C   PRCCDO reads and counts lines to be printed for the CC table
C   Output:
C      IRET   I     Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CCTITL(7)*8, LABEL*5, SCRTCH*132, LONGS(4)*4, CHTM12*12,
     *   STR*4
      INTEGER   IRNO, MRNO, JCOL1, JCOL2, LRNO, RESULT(66), M1, RTYPE,
     *   IERR, INC, KOLS(7), OTYPE, IPAGE, I, J, M, N, NLINE, M2,
     *   ISCR(256), NCOUNT, TTY(2)
      LOGICAL   PFLAG, EQUAL, T
      REAL      RES4(66), BUFF4(512), SCALEX, SCALET, CCMAX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTCC.INC'
      EQUIVALENCE (BUFFER, BUFF4)
      EQUIVALENCE (RESULT, RES4)
      DATA T /.TRUE./
      DATA LONGS /'GLON','GLAT','ELON','ELAT'/
      DATA CCTITL /'DELTAX  ', 'DELTAY  ', 'MAJOR AX', 'MINOR AX',
     *   'POSANGLE', 'TYPE OBJ', 'FLUX    '/
C-----------------------------------------------------------------------
C                                       init counters, line size
      IRET = 0
      IF ((LPNAME.NE.' ') .OR. (DOCRT.GT.0.0)) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Find columns
      JCOL1 = 2
      IF (BUFFER(10).GE.7) JCOL1 = 6
      CALL FNDCOL (JCOL1, CCTITL(1), 8, T, BUFFER, KOLS(1), IERR)
      JCOL2 = JCOL1 + 1
      CALL FNDCOL (1, CCTITL(7), 8, T, BUFFER, KOLS(JCOL2), IERR)
      OTYPE = 5
      IF (NACROS.GE.80) OTYPE = OTYPE - 1
      IF (NACROS.GE.100) OTYPE = OTYPE - 1
      IF (NACROS.GE.120) OTYPE = OTYPE - 1
      IF (JCOL2.NE.7) OTYPE = 1
C                                       Primary page: heading info
      IPAGE = 0
      NLINE = 899
      MRNO = BUFFER(5)
      NCOUNT = 0
C                                       CC scaling - uv data
      IF (ISUV) THEN
         M1 = MIN (JCOL1, 4)
         LRNO = 0
         CCMAX = 0.0
         DO 220 IRNO = 1,ECOUNT
            DO 210 J = 1,M1
               I = KOLS(J)
               CALL GETCOL (IRNO, I, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            ISCR, IERR)
               IF (IERR.LT.0) GO TO 220
               CCMAX = MAX (CCMAX, ABS (RES4(1)))
 210           CONTINUE
 220        CONTINUE
         SCALET = CCMAX / 100.0 * 3600.0
         SCALEX = SCALET + 1.E-25
         CALL METSCA (SCALET, LABEL, PFLAG)
         SCALEX = 3600.0 * SCALET / SCALEX
C                                       find real RA/DEC axis : MA file
      ELSE
         INC = 2
         N = CATBLK(KIDIM)
         M1 = 0
         M2 = 0
         DO 30 I = 1,N
            J = KHCTP + (I-1) * INC
            CALL H2CHR (2, 1, CATH(J), CHTM12)
            IF (CHTM12(:2).EQ.'RA') M1 = I
            CALL H2CHR (3, 1, CATH(J), CHTM12)
            IF (CHTM12(1:3).EQ.'DEC') M2 = I
            DO 29 M = 1,4
               CALL H2CHR (4, 1, CATH(J), CHTM12)
               EQUAL = LONGS(M)(1:4).EQ. CHTM12(1:4)
               IF ((EQUAL) .AND. ((M.EQ.1) .OR. (M.EQ.3))) M1 = I
               IF ((EQUAL) .AND. ((M.EQ.2) .OR. (M.EQ.4))) M2 = I
 29            CONTINUE
            IF ((M1.GT.0) .AND. (M2.GT.0)) GO TO 35
 30         CONTINUE
         M1 = 1
 35      M = MAX (M1, M2) - 1
         SCALET = ABS(CATR(KRCIC+M)) * 3600.0
         SCALEX = SCALET + 1.E-25
         CALL METSCA (SCALET, LABEL, PFLAG)
         SCALEX = 3600.0 * SCALET / SCALEX
         END IF
      IF (ISUV) DOCELL = -1.0
      I = 100 + 1
      N = I + 13
C                                         First page
      IF (DOCRT.LE.-2.5) THEN
         NCOUNT = NCOUNT + 1
      ELSE
         NCOUNT = NCOUNT + 3
C                                       selection strings
         IF (BUFFER(61).GT.0) THEN
            N = BUFFER(61)
            NCOUNT = NCOUNT + 1 + N
            END IF
         END IF
C                                       Output the titles
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 2
      IF ((DOCRT.GT.-2.5) .AND. (DOCRT.LE.0.0)) NCOUNT = NCOUNT + 1
C                                       Output loop
      DO 140 IRNO = 1,ECOUNT
         IF (MOD(IRNO-BCOUNT,ICOUNT).EQ.0) NCOUNT =  NCOUNT + 1
 140     CONTINUE
      NCOUNT = NCOUNT + 2
C                                       ask if needed
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            NLINE = -1
            CALL LPCLOS (OUTLUN, OUTIND, NLINE, IERR)
            IRET = -1
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRTCH,1150) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            NLINE = -1
            CALL LPCLOS (OUTLUN, OUTIND, NLINE, IERR)
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRCCDO (IRET)
C-----------------------------------------------------------------------
C   PRCCDO reads, formats, and prints a table extension file
C   Output:
C      IRET   I     Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CCTITL(7)*8, LABEL*5, TITL1*132, TITL2*132, LINE*132,
     *   SCRTCH*132, ATIME*8, ADATE*12, BTIME*8, BDATE*12, CCTYPE*4,
     *   SORTOR(5)*8, LONGS(4)*4, CHTMP*132, CHTM12*12, CCCHAR*1,
     *   SORTS(4)*8, MODTYP(5)*5
      INTEGER   IRNO, MRNO, FIRSTC, JCOL1, JCOL2, LRNO, RESULT(66), II,
     *   M1, RTYPE, IERR, JCOL3, INC, KOLS(7), OTYPE, IPAGE, I, J, M, N,
     *   NLINE, JJ, M2, ISCR(256)
      LOGICAL   PFLAG, EQUAL, T, UPSUM
      REAL      CCPARM(9), RES4(66), BUFF4(512), SCALEX, SCALET, CCMAX,
     *   SCALES(7), OFFS(7), SUMDEL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTCC.INC'
      EQUIVALENCE (BUFFER, BUFF4)
      EQUIVALENCE (RESULT, RES4)
      DATA T /.TRUE./
      DATA SORTOR /'ASCEND ','ASCEAB ','DESCND ','DESCAB ',' '/
      DATA LONGS /'GLON','GLAT','ELON','ELAT'/
      DATA CCTITL /'DELTAX  ', 'DELTAY  ', 'MAJOR AX', 'MINOR AX',
     *   'POSANGLE', 'TYPE OBJ', 'FLUX    '/
      DATA MODTYP /'Point','Gauss','CGaus','Spher','?????'/
C-----------------------------------------------------------------------
C                                       init counters, line size
      IRET = 0
C                                       Find columns
      JCOL1 = 2
      IF (BUFFER(10).GE.7) JCOL1 = 6
      CALL FNDCOL (JCOL1, CCTITL(1), 8, T, BUFFER, KOLS(1), IERR)
      JCOL2 = JCOL1 + 1
      CALL FNDCOL (1, CCTITL(7), 8, T, BUFFER, KOLS(JCOL2), IERR)
      OTYPE = 5
      IF (NACROS.GE.80) OTYPE = OTYPE - 1
      IF (NACROS.GE.100) OTYPE = OTYPE - 1
      IF (NACROS.GE.120) OTYPE = OTYPE - 1
      IF (JCOL2.NE.7) OTYPE = 1
C                                       Primary page: heading info
      IPAGE = 0
      NLINE = 899
      MRNO = BUFFER(5)
      SORTS(1) = ' '
      SORTS(2) = ' '
      SORTS(3) = ' '
      SORTS(4) = ' '
      II = 5
      JJ = 5
      IF (BUFFER(43).GT.0) II = 1
      IF (BUFFER(44).GT.0) JJ = 1
      IF (BUFFER(43).LT.0) II = 3
      IF (BUFFER(44).LT.0) JJ = 3
      IF (BUFFER(43).GT.256) II = 2
      IF (BUFFER(44).GT.256) JJ = 2
      IF (BUFFER(43).LT.-256) II = 4
      IF (BUFFER(44).LT.-256) JJ = 4
      SORTS(2) = SORTOR(II)
      SORTS(4) = SORTOR(JJ)
      I = ABS(BUFFER(43))
      IF (I.GT.256) I = I - 256
      J = ABS(BUFFER(44))
      IF (J.GT.256) J = J - 256
      N = 2
      IF (I.EQ.0) GO TO 20
         I = 257 + (BUFFER(128+I) - 1) * 6
         CALL H2CHR (8, 1, BUFFER(I), SORTS(1))
 20   IF (J.EQ.0) GO TO 25
         J = 257 + (BUFFER(128+J) - 1) * 6
         CALL H2CHR (8, 1, BUFFER(J), SORTS(2))
 25   CALL TIMDAT (BUFFER(14), BUFFER(11), ATIME, ADATE)
      CALL TIMDAT (BUFFER(36), BUFFER(33), BTIME, BDATE)
C                                       CC scaling - uv data
      IF (ISUV) THEN
         M1 = MIN (JCOL1, 4)
         LRNO = 0
         CCMAX = 0.0
         DO 220 IRNO = 1,ECOUNT
            DO 210 J = 1,M1
               I = KOLS(J)
               CALL GETCOL (IRNO, I, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            ISCR, IERR)
               IF (IERR.LT.0) GO TO 220
               CCMAX = MAX (CCMAX, ABS (RES4(1)))
 210           CONTINUE
 220        CONTINUE
         SCALET = CCMAX / 100.0 * 3600.0
         SCALEX = SCALET + 1.E-25
         CALL METSCA (SCALET, LABEL, PFLAG)
         SCALEX = 3600.0 * SCALET / SCALEX
C                                       find real RA/DEC axis : MA file
      ELSE
         INC = 2
         N = CATBLK(KIDIM)
         M1 = 0
         M2 = 0
         DO 30 I = 1,N
            J = KHCTP + (I-1) * INC
            CALL H2CHR (2, 1, CATH(J), CHTM12)
            IF (CHTM12(:2).EQ.'RA') M1 = I
            CALL H2CHR (3, 1, CATH(J), CHTM12)
            IF (CHTM12(1:3).EQ.'DEC') M2 = I
            DO 29 M = 1,4
               CALL H2CHR (4, 1, CATH(J), CHTM12)
               EQUAL = LONGS(M)(1:4).EQ. CHTM12(1:4)
               IF ((EQUAL) .AND. ((M.EQ.1) .OR. (M.EQ.3))) M1 = I
               IF ((EQUAL) .AND. ((M.EQ.2) .OR. (M.EQ.4))) M2 = I
 29            CONTINUE
            IF ((M1.GT.0) .AND. (M2.GT.0)) GO TO 35
 30         CONTINUE
         M1 = 1
 35      M = MAX (M1, M2) - 1
         SCALET = ABS(CATR(KRCIC+M)) * 3600.0
         SCALEX = SCALET + 1.E-25
         CALL METSCA (SCALET, LABEL, PFLAG)
         SCALEX = 3600.0 * SCALET / SCALEX
         END IF
      OFFS(1) = 0.0
      OFFS(2) = 0.0
      OFFS(3) = 0.0
      OFFS(4) = 0.0
      OFFS(5) = 0.0
      SCALES(1) = SCALEX
      SCALES(2) = SCALEX
      SCALES(3) = SCALEX
      SCALES(4) = SCALEX
      SCALES(5) = 1.0
      IF (ISUV) DOCELL = -1.0
      IF ((DOCELL.GT.0.0) .AND. (M1.GT.0) .AND.
     *   (CATR(KRCIC+M1-1).NE.0.0)) THEN
         SCALES(1) = 1.0/CATR(KRCIC+M1-1)
         OFFS(1) = CATR(KRCRP+M1-1)
         END IF
      IF ((DOCELL.GT.0.0) .AND. (M2.GT.0) .AND.
     *   (CATR(KRCIC+M2-1).NE.0.0)) THEN
         SCALES(2) = 1.0/CATR(KRCIC+M2-1)
         OFFS(2) = CATR(KRCRP+M2-1)
         END IF
      I = 100 + 1
      N = I + 13
C                                         First page
      WRITE (TITL1,1040) INNAM, INCLS, INSEQ, INDISK, 'CC', INVERS
      CALL H2CHR (56, 1, BUFF4(I), CHTMP)
      WRITE (TITL2,1041) CHTMP
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
      ELSE
         CALL H2CHR (5, 1, BUFFER(29), CHTMP)
         WRITE (LINE,1042) CHTMP, ADATE, ATIME
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL H2CHR (5, 1, BUFFER(39), CHTMP)
         WRITE (LINE,1043) CHTMP, BDATE, BTIME
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         IF ((SORTS(3).EQ.' ') .AND. (SORTS(4).EQ.' ')) THEN
            WRITE (LINE,1044) MRNO, SORTS(1), SORTS(2)
         ELSE
            WRITE (LINE,1045) MRNO, SORTS
            END IF
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       selection strings
         IF (BUFFER(61).GT.0) THEN
            WRITE (LINE,1050)
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            N = BUFFER(61)
            DO 55 I = 1,N
               IRNO = I
               CALL TABIO ('READ', 2, IRNO, RESULT, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 960
               M = BUFFER(63+I) - BUFFER(62+I)
               IF (I.EQ.N) M = BUFFER(62) - BUFFER(62+I)
               M = MIN (M, 28)
               IF (16+4*M.GT.NACROS) M = (NACROS - 16) / 4
               M = M * 4
               CALL H2CHR (M, 1, RES4(1), CHTMP)
               WRITE (LINE,1051) CHTMP(:M)
               CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, NLINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 970
 55            CONTINUE
            END IF
         END IF
C                                       Prepare titles
      IF (OTYPE.EQ.1) WRITE (TITL1,1081)
      IF (OTYPE.EQ.2) WRITE (TITL1,1082)
      IF (OTYPE.EQ.3) WRITE (TITL1,1083)
      IF (OTYPE.EQ.4) WRITE (TITL1,1084)
      IF (OTYPE.EQ.5) WRITE (TITL1,1085)
      IF (DOCELL.LE.0.0) THEN
         IF (OTYPE.EQ.1) WRITE (TITL2,1086) LABEL, LABEL
         IF (OTYPE.EQ.2) WRITE (TITL2,1087) LABEL, LABEL, LABEL, LABEL
         IF (OTYPE.EQ.3) WRITE (TITL2,1088) LABEL, LABEL, LABEL, LABEL
         IF (OTYPE.EQ.4) WRITE (TITL2,1089) LABEL, LABEL, LABEL, LABEL
         IF (OTYPE.EQ.5) WRITE (TITL2,1090) LABEL, LABEL, LABEL, LABEL
      ELSE
         IF (OTYPE.EQ.1) WRITE (TITL2,1091)
         IF (OTYPE.EQ.2) WRITE (TITL2,1092) LABEL, LABEL
         IF (OTYPE.EQ.3) WRITE (TITL2,1093) LABEL, LABEL
         IF (OTYPE.EQ.4) WRITE (TITL2,1094) LABEL, LABEL
         IF (OTYPE.EQ.5) WRITE (TITL2,1095) LABEL, LABEL
         END IF
C                                       Output the titles
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *   TITL1, NLINE, IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *   TITL2, NLINE, IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 970
      IF ((DOCRT.GT.-2.5) .AND. (DOCRT.LE.0.0)) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Output loop
      JCOL3 = JCOL2 + 1
      FIRSTC = 0
      SUMDEL = 0.0
      UPSUM = .TRUE.
      CALL RFILL (JCOL3, 0.0, CCPARM)
      IF (JCOL1.GE.6) CCTYPE = ' '
      CCCHAR = ' '
      LRNO = 0
      DO 140 IRNO = 1,ECOUNT
         I = KOLS(JCOL2)
         CALL GETCOL (IRNO, I, DATP, LRNO, BUFFER, RTYPE, RESULT, ISCR,
     *      IERR)
         IF (IERR.LT.0) THEN
            CCCHAR = '*'
            GO TO 140
            END IF
         IF (IERR.NE.0) GO TO 960
C                                      Store sum prior to last negative.
         IF ((UPSUM) .AND. (RES4(1).LT.0.0)) THEN
               SUMDEL = CCPARM(JCOL3)
               UPSUM = .FALSE.
               ENDIF
C
         CCPARM(JCOL3) = CCPARM(JCOL3) + RES4(1)
         IF ((FIRSTC.LE.0) .AND. (RES4(1).LT.0.0)) FIRSTC = IRNO
         IF (IRNO.LT.BCOUNT) GO TO 140
         IF (MOD(IRNO-BCOUNT,ICOUNT).NE.0) GO TO 140
            CCPARM(JCOL2) = RES4(1)
            DO 130 J = 1,JCOL1
               I = KOLS(J)
               CALL GETCOL (IRNO, I, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            ISCR, IERR)
               IF (IERR.LT.0) GO TO 140
               IF (IERR.NE.0) GO TO 960
               IF (J.LE.5) THEN
                  CCPARM(J) = RES4(1) * SCALES(J) + OFFS(J)
               ELSE
                  I = RES4(1) + 1
                  IF ((I.LT.1) .OR. (I.GT.4)) I = 5
                  CCTYPE = MODTYP(I)
                  END IF
 130           CONTINUE
C                                       encode it now
            IF (OTYPE.EQ.1) WRITE (LINE,1131,ERR=135) IRNO, (CCPARM(I),
     *         I = 1,JCOL1), CCPARM(JCOL2), CCPARM(JCOL3), CCCHAR
            IF (OTYPE.EQ.2) WRITE (LINE,1132,ERR=135) IRNO, (CCPARM(I),
     *         I = 1,JCOL1-1), CCTYPE, CCPARM(JCOL2), CCPARM(JCOL3),
     *         CCCHAR
            IF (OTYPE.EQ.3) WRITE (LINE,1133,ERR=135) IRNO, (CCPARM(I),
     *         I = 1,JCOL1-1), CCTYPE, CCPARM(JCOL2), CCPARM(JCOL3),
     *         CCCHAR
            IF (OTYPE.EQ.4) WRITE (LINE,1134,ERR=135) IRNO, (CCPARM(I),
     *         I = 1,JCOL1-1), CCTYPE, CCPARM(JCOL2), CCPARM(JCOL3),
     *         CCCHAR
            IF (OTYPE.EQ.5) WRITE (LINE,1135,ERR=135) IRNO, (CCPARM(I),
     *         I = 1,JCOL1-1), CCTYPE, CCPARM(JCOL2), CCPARM(JCOL3),
     *         CCCHAR
C                                       do output finally
 135        CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.GT.0) GO TO 970
            IF (IERR.NE.0) GO TO 150
 140     CONTINUE
 150  IF (FIRSTC.GT.0) THEN
         IF (IERR.LT.0) NLINE = 5
         WRITE (LINE,1150) FIRSTC
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         WRITE (LINE,1160) SUMDEL
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         END IF
C                                       Close down
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      CALL LPCLOS (OUTLUN, OUTIND, NLINE, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) GO TO 150
         IRET = 2
         WRITE (MSGTXT,1970) IERR
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT (A12,'.',A6,'.',I4,'  Disk=',I2,4X,A2,' table version',
     *   I4)
 1041 FORMAT ('Title: ',A56)
 1042 FORMAT ('Created by      ',A5,' on ',A12,A8)
 1043 FORMAT ('Last written by ',A5,' on ',A12,A8)
 1044 FORMAT ('Ncomps',I8,4X,'sort on:',1X,A8,1X,'(',A6,')')
 1045 FORMAT ('Ncomps',I8,4X,'sort on:',2(1X,A8,1X,'(',A6,')'))
 1050 FORMAT (3X,'* After total => does not include flagged',
     *   ' components')
 1051 FORMAT (3X,'Selected on: ',A112)
 1081 FORMAT (4X,'Comp',8X,'Delta X',8X,'Delta Y',7X,'Flux',12X,
     *   'Total')
 1082 FORMAT (4X,'Comp',8X,'Delta X',8X,'Delta Y',7X,'Major ax',7X,
     *   'Minor ax',8X,'Pa',6X,'Type',7X,'Flux',12X,'Total')
 1083 FORMAT (4X,'Comp',8X,'Delta X',8X,'Delta Y',7X,'Major ax',7X,
     *   'Minor ax',5X,'Pa',3X,'Type',4X,'Flux',9X,'Total')
 1084 FORMAT (4X,'Comp',4X,'Deltax',4X,'Deltay',3X,'Majax',3X,'Minax',
     *   4X,'Pa',2X,'Type',3X,'Flux',8X,'Total')
 1085 FORMAT (3X,'Comp',3X,'Deltax',3X,'Deltay',2X,'Majax',2X,'Minax',
     *   4X,'Pa',2X,'Type',2X,'Flux',7X,'Total')
 1086 FORMAT (7X,'#',2(6X,A5,'asec'),8X,'Jy',15X,'Jy')
 1087 FORMAT (7X,'#',4(6X,A5,'asec'),7X,'Degr',17X,'Jy',14X,'Jy')
 1088 FORMAT (7X,'#',4(6X,A5,'asec'),4X,'Degr',11X,'Jy',11X,'Jy')
 1089 FORMAT (7X,'#',2(4X,A5,'S'),2(2X,A5,'S'),3X,'Degr',9X,'Jy',
     *   10X,'Jy')
 1090 FORMAT (6X,'#',2(3X,A5,'S'),2(2X,A5),3X,'Degr',8X,'Jy',9X,
     *   'Jy')
 1091 FORMAT (7X,'#',2(10X,'Cells'),8X,'Jy',15X,'Jy')
 1092 FORMAT (7X,'#',2(10X,'Cells'),2(6X,A5,'asec'),7X,'Degr',17X,
     *   'Jy',14X,'Jy')
 1093 FORMAT (7X,'#',2(10X,'Cells'),2(6X,A5,'asec'),4X,'Degr',11X,
     *   'Jy',11X,'Jy')
 1094 FORMAT (7X,'#',2(5X,'Cells'),2(2X,A5,'S'),3X,'Degr',9X,'Jy',
     *   10X,'Jy')
 1095 FORMAT (6X,'#',2(4X,'Cells'),2(2X,A5),3X,'Degr',8X,'Jy',9X,
     *   'Jy')
 1131 FORMAT (I8,2F15.2,4X,F12.7,4X,F13.7,A1)
 1132 FORMAT (I8,4F15.2,5X,F6.1,5X,A5,4X,F10.6,5X,F11.6,A1)
 1133 FORMAT (I8,4F15.2,2X,F6.1,2X,A5,1X,F10.6,2X,F11.6,A1)
 1134 FORMAT (I8,2F10.2,2F8.2,F7.1,1X,A5,F10.6,F12.6,A1)
 1135 FORMAT (I7,2F9.2,2F7.2,F7.1,1X,A5,F9.6,F10.5,A1)
 1150 FORMAT ('First negative component is number',I9)
 1160 FORMAT ('Sum of components prior to first negative = ',F12.5,
     *   ' Jy.')
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END

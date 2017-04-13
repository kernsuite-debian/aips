      SUBROUTINE AU7B (BRANCH)
C-----------------------------------------------------------------------
C! verbs to manipulate history file, coordinate values
C# POPS-appl Coordinates Header History Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2003-2004, 2007-2009, 2012, 2014-2016
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
C   AU7B verbs:
C   1. HINOTE   add 1 or more lines to the history file
C   2. HITEXT   write the history file to a text file
C   3. STALIN   delete 1 or more lines from the history file
C   4. CPUTIME  print out current cpu/real usage
C   5. COWINDOW set BLC, TRC based on IMSIZE and center COORDINA
C   6. COPIXEL  convert between COORDINA and PIXXY.
C   7. COODEFIN reset celestial coordinates
C   8  CALDIR   list calibrator models available
C   9  CODECIML convert X,Y <-> COORDINA
C  10  GETDATE  return date/time string in various forms
C
C   Inputs:   (though adverbs in common)
C      INNAME   H*12  input file name.  If blank the first match
C                     consistent with the other parameters is used.
C      INCLASS  H*6   input file class.  If blank the first match is
C                     used.
C      INSEQ    R     the old entry's sequence number. If zero the first
C                     match consistent with the other parameters is used
C      INDISK   R     the catalog resides on this disk.  If zero, all
C                     disk volumes are searched until a match is found.
C   (HINOTE)
C      COMENT   R(17)  history card to add: ' ' => read from user
C      INTEXT   H*48   File name to get text input
C   (HITEXT)
C      HISTRT   R     start record number
C      HIEND    R     end record number
C      PRTASK   H*5   only records w this at start
C      OUTTEXT  H*48  File name to save text output
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, LOCNAM*12, LOCCLS*6, LOCTYP*2, STATUS*4,
     *   PLINE*132, PRTNAM*48, PRTACK*8, LPT*1, HILINE*72, ATIME*8,
     *   ADATE*12, CDUM*1, CLSTAT*4, PHNAME*48, CHTEMP*6, XLATED*256,
     *   FILSPC*256, ANAME*10, XSIGN*1, YSIGN*1, THEDAT*24, MONTH(12)*3
      HOLLERITH NAMES(3,100)
      INTEGER   IBLK, IBUFF(256), HIBUFF(256), JTRIM, ICUR, IERR, BREC,
     *   NUMCHR, NPI, LOCSEQ, ISLOT, IUSER, IVOL, I, J, NNAM, POTERR,
     *   IHLUN, IHPTR, IB, ICARD, IHIND, II, NREC, TXLUN, TXIND, EREC,
     *   ITIME(6), OCARD, OBLK, NDEAD, JJ, IDUM, ALUN, AIND, LREC, MREC,
     *   NWPL, NLPR, NP, DEPTH(5), IMSIZE(2), IROUND, NMAX, FLEN
      REAL      RDUM, DOCONF, RBUFF(256), TC, XCOORD(6), BLC(7), TRC(7),
     *   CX, CY, XRPIX(2), XRINC(2), XROT, MEPS
      DOUBLE PRECISION JD, JD0, XIN, YIN, ZIN
      LOGICAL   EQUAL, EXCL, NOMAP, NOSAVE, SAVE, WAIT, T, F, NEG
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (T, WAIT, SAVE, EXCL)
      EQUIVALENCE (F, NOMAP, NOSAVE)
      EQUIVALENCE (IBUFF, RBUFF)
      DATA T, F /.TRUE.,.FALSE./
      DATA IHLUN, TXLUN /27, 11/
      DATA PRGNAM /'AU7B  '/
      DATA MONTH /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     *   'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C                                       Set initial values.
      POTERR = 0
      IF ((BRANCH.NE.4) .AND. (BRANCH.LT.8)) THEN
         CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         IF (BRANCH.LT.4) THEN
            CALL ADVERB ('HISTART', 'I', 1, 0, BREC, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            CALL ADVERB ('HIEND', 'I', 1, 0, EREC, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
C                                       PRTASK changed size
            CALL ADVERB ('PRTASK', 'C', 1, 8, IDUM, RDUM, PRTACK)
            IF (ERRNUM.NE.0) THEN
               ERRNUM = 0
               CALL ADVERB ('PRTASK', 'C', 1, 5, IDUM, RDUM, PRTACK)
               IF (ERRNUM.NE.0) GO TO 980
               END IF
            END IF
         IUSER = NLUSER
         LOCTYP = ' '
         ISLOT = 1
         CLSTAT = '  '
         CALL CATDIR ('SRCH', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ,
     *      LOCTYP, IUSER, STATUS, IBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020)
            CALL MSGWRT (8)
            POTERR = 101
            GO TO 980
            END IF
         END IF
C                                       Choose function.
      GO TO (100, 200, 300, 400, 500, 500, 600, 700, 800, 900), BRANCH
C-----------------------------------------------------------------------
C                                       HINOTE
C                                       user comments to HI file
C-----------------------------------------------------------------------
 100  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
         END IF
      CLSTAT = 'CLWR'
C                                       open HI (create if needed)
      CALL FNDEXT ('HI', CATBLK, II)
      IF (II.LE.0) THEN
         CALL HICREA (IHLUN, IVOL, ISLOT, CATBLK, HIBUFF, IERR)
      ELSE
         CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
         END IF
      IF (IERR.NE.0) THEN
         POTERR = 34
         GO TO 975
         END IF
C                                       batch or batch-like
      IB = 9
      CALL ADVERB ('DOHIST', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (RDUM.LE.0.0) IB = 1
      HILINE = 'HINOTE'
      XLATED = ' '
      CALL ADVERB ('COMMENT', 'C', 1, 64, IDUM, RDUM, XLATED)
      IF (ERRNUM.NE.0) GO TO 980
      II = JTRIM (XLATED)
      CALL ADVERB ('INTEXT', 'C', 1, 48, IDUM, RDUM, PRTNAM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((II.LE.0) .AND. (PRTNAM.NE.' ')) THEN
         CALL ZTXOPN ('READ', TXLUN, TXIND, PRTNAM, F, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 52
            GO TO 140
            END IF
         DO 110 I = 1,1000000
            CALL ZTXIO ('READ', TXLUN, TXIND, PLINE, IERR)
            IF (IERR.NE.0) THEN
               IF (IERR.NE.2) POTERR = 59
               GO TO 115
               END IF
            IERR = JTRIM (PLINE)
            HILINE(IB:) = PLINE
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) THEN
               POTERR = 34
               GO TO 115
               END IF
 110        CONTINUE
         MSGTXT = 'INPUT FILE TRUNCATED AT 1000000 LINES'
         CALL MSGWRT (6)
 115     CALL ZTXCLS (TXLUN, TXIND, IERR)
      ELSE IF ((ISBTCH.EQ.32000) .OR. (II.GT.0)) THEN
         NUMCHR = JTRIM (PRTACK)
         IF ((IB.EQ.1) .AND. (NUMCHR.GT.0)) THEN
            HILINE = PRTACK
            IB = NUMCHR + 2
            END IF
         DO 116 I = 1,II
            IF (XLATED(I:I).EQ.'"') XLATED(I:I) = ''''
 116        CONTINUE
         HILINE(IB:) = XLATED(:II)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) POTERR = 34
C                                       Interactive multi-line
      ELSE
         MSGTXT = 'Enter your comment 1 line at a time.  Type _END ' //
     *      'or _end to stop.'
         CALL MSGWRT (1)
         LPT = IPT
         IPT = '@'
         NUMCHR = JTRIM (PRTACK)
         IF ((IB.EQ.1) .AND. (NUMCHR.GT.0)) THEN
            HILINE = PRTACK
            IB = NUMCHR + 2
            END IF
 120     CALL PREAD (KARBUF)
            IF (ERRNUM.NE.0) GO TO 140
            NUMCHR = JTRIM (KARBUF(:64))
            HILINE(IB:) = KARBUF(:NUMCHR)
            II = INDEX (KARBUF, '_END')
            IF (II.EQ.0) II = INDEX (KARBUF, '_end')
            IF ((II.LE.0) .OR. ((II.GT.1) .AND. (KARBUF(:II-1).NE.' ')))
     *         THEN
               IF (II.GT.0) HILINE(II+IB-2:) = ' '
               CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
               IF (IERR.NE.0) POTERR = 34
               END IF
            IF ((II.LE.0) .AND. (IERR.EQ.0)) GO TO 120
            IPT = LPT
         END IF
 140  CALL HICLOS (IHLUN, SAVE, HIBUFF, IERR)
      GO TO 975
C-----------------------------------------------------------------------
C                                       HITEXT
C                                       write history file to text file
C-----------------------------------------------------------------------
 200  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'READ', IBUFF, IERR)
      POTERR = 33
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) GO TO 980
      CLSTAT = 'CLRD'
C                                       Open history file.
      CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
      POTERR = 34
      IF (IERR.NE.0) GO TO 975
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IERR)
      IF (IERR.NE.0) GO TO 250
      POTERR = 0
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
C                                       Test for erroneous no. of recs.
      IF ((NREC.LE.0) .OR. (NREC.GT.1000000)) THEN
         WRITE (MSGTXT,1200) LOCNAM, LOCCLS, LOCSEQ, NREC
         CALL MSGWRT (7)
         POTERR = 32
         GO TO 250
         END IF
C                                       open the output
      CALL ADVERB ('OUTTEXT', 'C', 1, 48, IDUM, RDUM, PRTNAM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (PRTNAM.EQ.' ') THEN
         MSGTXT = 'OUTTEXT MUST BE SPECIFIED'
         CALL MSGWRT (7)
         POTERR = 55
         GO TO 250
         END IF
      CALL ZTXOPN ('WRIT', TXLUN, TXIND, PRTNAM, F, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 55
         GO TO 250
         END IF
C                                       loop limits
      IF ((BREC.LT.1) .OR. (BREC.GT.NREC)) BREC = 1
      IF ((EREC.LT.BREC) .OR. (EREC.GT.NREC)) EREC = NREC
      IBLK = (BREC-1) / NHILPR + 1
      ICARD = BREC - (IBLK-1) * NHILPR
      CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 250
C                                       string restrictions
      NUMCHR = JTRIM (PRTACK)
      CALL ZTIME (ITIME(4))
      CALL ZDATE (ITIME(1))
      CALL TIMDAT (ITIME(4), ITIME(1), ATIME, ADATE)
      PLINE = 'HITEXT / file written ' // ATIME // '  ' // ADATE
      JJ = JTRIM (PLINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, PLINE(:JJ), IERR)
      IF (IERR.NE.0) THEN
         POTERR = 59
         GO TO 230
         END IF
      IF (NUMCHR.GT.0) THEN
         PLINE = 'HITEXT  PRTASK = ''' // PRTACK(:NUMCHR) //
     *      '''    / limited to such task names'
         JJ = JTRIM (PLINE)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, PLINE(:JJ), IERR)
         IF (IERR.NE.0) THEN
            POTERR = 59
            GO TO 230
            END IF
         END IF
      IF ((BREC.GT.1) .OR. (EREC.LT.NREC)) THEN
         WRITE (PLINE,1210) BREC, EREC, NREC
         JJ = JTRIM (PLINE)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, PLINE(:JJ), IERR)
         IF (IERR.NE.0) THEN
            POTERR = 59
            GO TO 230
            END IF
         END IF
      DO 225 ICUR = BREC,EREC
C                                       Read next buffer.
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 250
            END IF
C                                       desired task?
         II = (ICARD-1) * NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(II), PLINE)
         JJ = JTRIM (PLINE)
         IF (NUMCHR.GT.0) THEN
            CALL CHBLNK (72, 1, PLINE, NPI)
            EQUAL = PRTACK(1:NUMCHR).EQ.PLINE(NPI:NPI+NUMCHR-1)
         ELSE
            EQUAL = .TRUE.
            END IF
         IF (EQUAL) THEN
            CALL ZTXIO ('WRIT', TXLUN, TXIND, PLINE(:JJ), IERR)
            IF (IERR.NE.0) THEN
               POTERR = 59
               GO TO 230
               END IF
            END IF
         ICARD = ICARD + 1
 225     CONTINUE
      POTERR = 0
 230  CALL ZTXCLS (TXLUN, TXIND, IERR)
C                                       Close history file.
 250  CALL HICLOS (IHLUN, NOSAVE, HIBUFF, IERR)
      GO TO 975
C-----------------------------------------------------------------------
C                                       STALIN
C                                       delete lines from history file
C-----------------------------------------------------------------------
 300  CALL ADVERB ('DOCONFRM', 'R', 1, 0, IDUM, DOCONF, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (DOCONF.GT.0.0) THEN
         IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) THEN
            WRITE (MSGTXT,1300)
            CALL MSGWRT (8)
            POTERR = 53
            IF (IUNIT.EQ.3) POTERR = 60
            GO TO 980
            END IF
         END IF
      CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
         END IF
      CLSTAT = 'CLWR'
C                                       Open history file.
      CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
      POTERR = 34
      IF (IERR.NE.0) GO TO 975
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IERR)
      IF (IERR.NE.0) GO TO 360
      POTERR = 0
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
C                                       Test for erroneous no. of recs.
      IF ((NREC.LE.0) .OR. (NREC.GT.1000000)) THEN
         WRITE (MSGTXT,1200) LOCNAM, LOCCLS, LOCSEQ, NREC
         CALL MSGWRT (7)
         POTERR = 32
         GO TO 360
         END IF
C                                       loop limits
      IF (BREC.LT.1) BREC = 1
      IF (EREC.GT.NREC) EREC = NREC
C                                       Check for typos or screwups
      IF (EREC.LT.BREC) THEN
         WRITE (MSGTXT,1305) BREC, EREC, NREC
         CALL MSGWRT (7)
         POTERR = 101
         GO TO 360
         END IF
      IBLK = (BREC-1) / NHILPR + 1
      ICARD = BREC - (IBLK-1) * NHILPR
      CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 360
      OCARD = ICARD
      OBLK = IBLK
      CALL COPY (256, HIBUFF, IBUFF)
      NDEAD = 0
C                                       loop over remaining file
      DO 325 ICUR = BREC,NREC
C                                       Read next buffer.
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 360
            END IF
C                                       delete line?
C                                       Confirm deletion
         IF ((DOCONF.GT.0.0) .AND. (ICUR.LE.EREC)) THEN
            II = (ICARD-1) * NHIWPL + 5
            CALL H2CHR (72, 1, HIBUFF(II), HILINE)
            IERR = JTRIM (HILINE)
            WRITE (MSGTXT,1315) ICUR, HILINE(:IERR)
            CALL MSGWRT (2)
            MSGTXT = 'Delete above line: Y or y or N or n'
            CALL MSGWRT (1)
            CALL CONFRM (IERR)
            EQUAL = IERR.EQ.0
         ELSE
            EQUAL = ICUR.LE.EREC
            END IF
C                                       count deletions
         IF (EQUAL) THEN
            NDEAD = NDEAD + 1
C                                       copy record to save it
         ELSE
            II = (ICARD-1) * NHIWPL + 5
            JJ = (OCARD-1) * NHIWPL + 5
            CALL COPY (18, HIBUFF(II), IBUFF(JJ))
            OCARD = OCARD + 1
C                                       Write this buffer.
            IF (OCARD.GT.NHILPR) THEN
               CALL ZFIO ('WRIT', IHLUN, IHIND, OBLK, IBUFF, IERR)
               IF (IERR.NE.0) GO TO 360
               OBLK = OBLK + 1
               OCARD = 1
               END IF
            END IF
         ICARD = ICARD + 1
 325     CONTINUE
      POTERR = 0
      WRITE (MSGTXT,1325) NDEAD
      CALL MSGWRT (4)
      HITAB(IHPTR+2) = NREC - NDEAD
      IF (OCARD.EQ.1) OBLK = OBLK - 1
      HITAB(IHPTR+6) = OBLK
      CALL HICLOS (IHLUN, SAVE, IBUFF, IERR)
      IF (IERR.NE.0) POTERR = 34
      GO TO 975
C                                       error - no renumber
 360  CALL HICLOS (IHLUN, NOSAVE, HIBUFF, IERR)
      GO TO 975
C-----------------------------------------------------------------------
C                                       CPUTIME
C                                       print current usage
C-----------------------------------------------------------------------
C                                       Open file, read rec 1
 400  CALL ZPHFIL ('AC', 1, 0, 0, PHNAME, IERR)
      ALUN = 13
      CALL ZOPEN (ALUN, AIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZFIO ('READ', ALUN, AIND, 1, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 450
C                                       Pick up file descriptors
      LREC = IBUFF(1)
      MREC = IBUFF(2)
      NWPL = IBUFF(3)
      NLPR = IBUFF(4)
C                                       get record
      IF ((NACOUN.GE.1) .AND. (NACOUN.LE.LREC)) THEN
         NREC = NACOUN / NLPR + 1
         IF (NREC.NE.1) THEN
            CALL ZFIO ('READ', ALUN, AIND, NREC, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 450
            END IF
C                                       Double check identity
         NP = (NACOUN - (NREC-1)*NLPR) * NWPL + 1
         CALL H2CHR (6, 1, IBUFF(NP), CHTEMP)
         EQUAL = TSKNAM(1:6).EQ.CHTEMP(1:6)
         EQUAL = EQUAL .AND. (NPOPS+100*MSGVER.EQ.IBUFF(NP+2))
         EQUAL = EQUAL .AND. (NLUSER.EQ.IBUFF(NP+3))
      ELSE
         EQUAL = .FALSE.
         END IF
      CALL ZDATE (ITIME(1))
      CALL ZTIME (ITIME(4))
      CALL TIMDAT (ITIME(4), ITIME(1), ATIME, ADATE)
      CALL DAT2JD (ITIME, JD)
      IF (EQUAL) THEN
         CALL ZCPU (TC, I)
         TC = TC - RBUFF(NP+8)
         CALL CATIME (2, IBUFF(NP+4), ITIME)
         CALL DAT2JD (ITIME, JD0)
         JD = (JD - JD0) * 24.0D0 * 3600.0D0
         I = JD + 0.5D0
         WRITE (MSGTXT,1400) ADATE, ATIME, TC, I
      ELSE
         MSGTXT = 'CPUTIME: ACCOUNT PARAMETERS DON''T MATCH OPEN'
         CALL MSGWRT (6)
         CALL ZCPU (TC, I)
         WRITE (MSGTXT,1401) ADATE, ATIME, TC
         END IF
      CALL MSGWRT (5)
 450  CALL ZCLOSE (ALUN, AIND, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       COWINDOW
C                                       set BLC, TRC centered
C                                       COPIXEL
C                                       XYPIX <-> COORDINA
C-----------------------------------------------------------------------
 500  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'REST', IBUFF, IERR)
      POTERR = 33
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) GO TO 980
C                                       Get adverbs
      CALL ADVERB ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (BRANCH.EQ.5) THEN
         CALL ADVERB ('IMSIZE', 'I', 2, 0, IMSIZE, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (IMSIZE(1).LE.0) IMSIZE(1) = 2 * MAXIMG
         IF (IMSIZE(2).LE.0) IMSIZE(2) = 2 * MAXIMG
         CALL ADVERB ('BLC', 'R', 7, 0, IDUM, BLC, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('TRC', 'R', 7, 0, IDUM, TRC, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         STATUS = ' '
      ELSE
         CALL ADVERB ('PIXXY', 'R', 7, 0, IDUM, BLC, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('OPTYPE', 'C', 1, 4, IDUM, RDUM, STATUS)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C                                       init coordinates
      LOCNUM = 1
      DO 955 I = 1,5
         IF (I+2.LE.CATBLK(KIDIM)) THEN
            DEPTH(I) = IROUND (BLC(I+2))
            IF ((DEPTH(I).LE.0) .OR. (DEPTH(I).GT.CATBLK(KINAX+I+1)))
     *         DEPTH(I) = IROUND (CATR(KRCRP+I+1))
         ELSE
            DEPTH(I) = 1
            END IF
 955     CONTINUE
      CALL SETLOC (DEPTH, F)
C                                       coord -> pixel
      IF (STATUS.NE.'PIXE') THEN
         XIN = ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *      ABS (XCOORD(3))/3600.0D0
         IF ((XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *      (XCOORD(3).LT.0.0)) XIN = -XIN
         IF ((CTYP(1,LOCNUM)(:4).EQ.'RA  ') .OR.
     *      (CTYP(1,LOCNUM)(:4).EQ.'RA--')) XIN = XIN * 15.0D0
         YIN = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *      ABS(XCOORD(6))/3600.0D0
         IF ((XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *      (XCOORD(6).LT.0.0)) YIN = -YIN
         IF ((CTYP(2,LOCNUM)(:4).EQ.'RA  ') .OR.
     *      (CTYP(2,LOCNUM)(:4).EQ.'RA--')) YIN = YIN * 15.0D0
C                                       Calc center of star pos
         CX = -9999.0
         CY = -9999.0
         CALL XYPIX (XIN, YIN, CX, CY, IERR)
         IF ((IERR.NE.0) .AND. (BRANCH.EQ.5)) THEN
            MSGTXT = 'XYPIX RETURNS ERROR'
            CALL MSGWRT (6)
            POTERR = 101
            GO TO 980
            END IF
         BLC(1) = CX
         BLC(2) = CY
         I = IROUND (CX)
         J = IROUND (CY)
         IF (BRANCH.EQ.5) THEN
            IF ((IERR.NE.0) .OR. (I.LT.1) .OR. (J.LT.1) .OR.
     *         (I.GT.CATBLK(KINAX)) .OR. (J.GT.CATBLK(KINAX+1))) THEN
               WRITE (MSGTXT,1500) I, J
               CALL MSGWRT (6)
               POTERR = 101
               GO TO 980
               END IF
            II = MIN (I-1, IMSIZE(1)/2)
            II = MIN (II, CATBLK(KINAX)-I)
            BLC(1) = I - II
            TRC(1) = I + II
            II = MIN (J-1, IMSIZE(2)/2)
            II = MIN (II, CATBLK(KINAX+1)-J)
            BLC(2) = J - II
            TRC(2) = J + II
            CALL ADVRBS ('TRC', 'R', 7, 0, IDUM, TRC, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            CALL ADVRBS ('BLC', 'R', 7, 0, IDUM, BLC, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
         ELSE
            CX = -1.0
            IF ((IERR.NE.0) .OR. (I.LT.1) .OR. (J.LT.1) .OR.
     *         (I.GT.CATBLK(KINAX)) .OR. (J.GT.CATBLK(KINAX+1))) CX = 1.
            CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, CX, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            CALL ADVRBS ('PIXXY', 'R', 7, 0, IDUM, BLC, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
C                                       pixel -> coord
      ELSE
         CX = BLC(1)
         CY = BLC(2)
         I = IROUND (CX)
         J = IROUND (CY)
         CALL XYVAL (CX, CY, XIN, YIN, ZIN, IERR)
         CX = -1.0
         IF ((IERR.NE.0) .OR. (I.LT.1) .OR. (J.LT.1) .OR.
     *      (I.GT.CATBLK(KINAX)) .OR. (J.GT.CATBLK(KINAX+1))) CX = 1.
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, CX, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (IERR.EQ.0) THEN
            IF ((CTYP(1,LOCNUM)(:4).EQ.'RA  ') .OR.
     *         (CTYP(1,LOCNUM)(:4).EQ.'RA--')) XIN = XIN / 15.0D0
            NEG = XIN.LT.0.0D0
            IF (NEG) XIN = -XIN
            I = XIN
            XCOORD(1) = I
            XIN = (XIN - I) * 60.0D0
            I = XIN
            XCOORD(2) = I
            XCOORD(3) = (XIN - I) * 60.0D0
            IF (NEG) THEN
               XCOORD(1) = -XCOORD(1)
               XCOORD(2) = -XCOORD(2)
               XCOORD(3) = -XCOORD(3)
               END IF
            IF ((CTYP(2,LOCNUM)(:4).EQ.'RA  ') .OR.
     *         (CTYP(2,LOCNUM)(:4).EQ.'RA--')) YIN = YIN / 15.0D0
            NEG = YIN.LT.0.0D0
            IF (NEG) YIN = -YIN
            I = YIN
            XCOORD(4) = I
            YIN = (YIN - I) * 60.0D0
            I = YIN
            XCOORD(5) = I
            XCOORD(6) = (YIN - I) * 60.0D0
            IF (NEG) THEN
               XCOORD(4) = -XCOORD(4)
               XCOORD(5) = -XCOORD(5)
               XCOORD(6) = -XCOORD(6)
               END IF
            CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
         END IF
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       COODEFIN
C                                       redefine coordinates
C-----------------------------------------------------------------------
 600  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      POTERR = 33
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) GO TO 980
      CLSTAT = 'CLWR'
      POTERR = 0
C                                       Get adverbs
      CALL ADVERB ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('COOREF', 'R', 2, 0, IDUM, XRPIX, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('COOINC', 'R', 2, 0, IDUM, XRINC, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('ROTATE', 'R', 1, 0, IDUM, XROT, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      XRINC(1) = XRINC(1) / 3600.0
      XRINC(2) = XRINC(2) / 3600.0
C                                       Find latitude and longitude
      II = -1
      JJ = -1
      NPI = CATBLK(KIDIM)
      DO 610 I = 1,NPI
         CALL H2CHR (4, 1, CATH(KHCTP+2*(I-1)), STATUS)
         IF (STATUS(:2).EQ.'RA') THEN
            II = I-1
         ELSE IF (STATUS(:3).EQ.'DEC') THEN
            JJ = I-1
         ELSE IF (STATUS(2:4).EQ.'LON') THEN
            II = I-1
         ELSE IF (STATUS(2:4).EQ.'LAT') THEN
            JJ = I-1
            END IF
 610     CONTINUE
      IF ((II.LT.0) .OR. (JJ.LT.0)) THEN
         POTERR = 101
         MSGTXT = 'CELESTIAL AXES NOT FOUND'
         GO TO 970
         END IF
      CALL H2CHR (4, 1, CATH(KHCTP+2*II), STATUS)
      CALL H2CHR (4, 1, CATH(KHCTP+2*JJ), CHTEMP)
      XIN = ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *   ABS (XCOORD(3))/3600.0D0
      IF ((XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)) XIN = -XIN
      YIN = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3600.0D0
      IF ((XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)) YIN = -YIN
      IF ((STATUS(:2).EQ.'RA') .AND. (CHTEMP(:3).EQ.'DEC')) THEN
         XIN = XIN * 15.0D0
      ELSE IF (STATUS(:1).NE.CHTEMP(:1)) THEN
         POTERR = 101
         MSGTXT = 'CELESTIAL AXES ARE NOT OF SAME TYPE'
         GO TO 970
         END IF
C                                       open HI (create if needed)
      CALL FNDEXT ('HI', CATBLK, I)
      IF (I.LE.0) THEN
         CALL HICREA (IHLUN, IVOL, ISLOT, CATBLK, HIBUFF, IERR)
      ELSE
         CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
         END IF
      IF (IERR.NE.0) POTERR = 34
C                                       reference value
      IF ((POTERR.EQ.0) .AND. (CATD(KDCRV+II).NE.XIN)) THEN
         WRITE (HILINE,1610) 'X_CRVAL', CATD(KDCRV+II), XIN
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) POTERR = 34
         END IF
      CATD(KDCRV+II) = XIN
      IF ((POTERR.EQ.0) .AND. (CATD(KDCRV+JJ).NE.YIN)) THEN
         WRITE (HILINE,1610) 'Y_CRVAL', CATD(KDCRV+JJ), YIN
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) POTERR = 34
         END IF
      CATD(KDCRV+JJ) = YIN
C                                       reference pixel
      IF ((XRPIX(1).NE.0.0) .OR. (XRPIX(2).NE.0.0)) THEN
         IF ((POTERR.EQ.0) .AND. (CATR(KRCRP+II).NE.XRPIX(1))) THEN
            WRITE (HILINE,1610) 'X_CRPIX', CATR(KRCRP+II), XRPIX(1)
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) POTERR = 34
            END IF
         CATR(KRCRP+II) = XRPIX(1)
         IF ((POTERR.EQ.0) .AND. (CATR(KRCRP+JJ).NE.XRPIX(2))) THEN
            WRITE (HILINE,1610) 'Y_CRPIX', CATR(KRCRP+JJ), XRPIX(2)
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) POTERR = 34
            END IF
         CATR(KRCRP+JJ) = XRPIX(2)
         END IF
C                                       increment
      IF (XRINC(1).NE.0.0) THEN
         IF ((POTERR.EQ.0) .AND. (CATR(KRCIC+II).NE.XRINC(1))) THEN
            WRITE (HILINE,1610) 'X_CDELT', CATR(KRCIC+II), XRINC(1)
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) POTERR = 34
            END IF
         CATR(KRCIC+II) = XRINC(1)
         END IF
      IF (XRINC(2).NE.0.0) THEN
         IF ((POTERR.EQ.0) .AND. (CATR(KRCIC+JJ).NE.XRINC(2))) THEN
            WRITE (HILINE,1610) 'Y_CDELT', CATR(KRCIC+JJ), XRINC(2)
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) POTERR = 34
            END IF
         CATR(KRCIC+JJ) = XRINC(2)
         END IF
C                                       rotation
      IF (XROT.GE.-180.0) THEN
         IF ((POTERR.EQ.0) .AND. (CATR(KRCRT+JJ).NE.XROT)) THEN
            WRITE (HILINE,1610) 'Y_CROTA', CATR(KRCRT+JJ), XROT
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IF (IERR.NE.0) POTERR = 34
            END IF
         CATR(KRCRT+JJ) = XROT
         END IF
      CALL HICLOS (IHLUN, SAVE, HIBUFF, IERR)
      IF (IERR.NE.0) POTERR = 34
C                                       update header
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'REST', IBUFF, IERR)
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) POTERR = 33
      GO TO 975
C-----------------------------------------------------------------------
C                                       CALDIR
C                                       list calibrator models avalable
C-----------------------------------------------------------------------
 700  XLATED = 'AIPSTARS:*.MODEL'
      POTERR = 2
      CALL ZFULLN (XLATED, ' ', ' ', FILSPC, IERR)
      IF (IERR.NE.0) GO TO 980
      FLEN = JTRIM (FILSPC)
      NMAX = 100
      POTERR = 50
      CALL ZTXMA2 (FLEN, FILSPC, NMAX, 0, NNAM, NAMES(1,1), IERR)
      IF (IERR.GT.1) GO TO 980
      POTERR = 0
      IF ((IERR.EQ.1) .OR. (NNAM.LE.0)) GO TO 980
      MSGTXT = 'System-provided calibration models'
      CALL MSGWRT (3)
      DO 710 I = 1,NNAM
         CALL H2CHR (10, 1, NAMES(1,I), ANAME)
         J = INDEX (ANAME, '_')
         JJ = JTRIM (ANAME)
         MSGTXT = 'OBJECT = ''' // ANAME(:J-1) // ''''
         MSGTXT(21:) = 'BAND = ''' // ANAME(J+1:JJ) // ''''
         CALL MSGWRT (3)
 710     CONTINUE
      GO TO 980
C-----------------------------------------------------------------------
C                                       CODECIML
C                                       convert X,Y <-> COORDINA
C-----------------------------------------------------------------------
 800  CALL ADVERB ('DECIMAL', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      MEPS = -1.E-8
C                                       X,Y -> COORDINA
      IF (RDUM.GT.0.0) THEN
         CALL ADVERB ('X', 'R', 1, 0, IDUM, CX, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('Y', 'R', 1, 0, IDUM, CY, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (MSGTXT,1800) CX, CY
         CALL MSGWRT (2)
         I = 1
         XSIGN = ' '
         IF (CX.LT.MEPS) THEN
            I = -1
            XSIGN = '-'
            END IF
         CX = ABS (CX) / 15.0
         J = CX
         ITIME(1) = J
         XCOORD(1) = I * J
         CX = 60.0 * (CX - J)
         J = CX
         ITIME(2) = J
         XCOORD(2) = I * J
         CX = 60.0 * (CX - J)
         XCOORD(3) = I * CX
         I = 1
         YSIGN = ' '
         IF (CY.LT.MEPS) THEN
            I = -1
            YSIGN = '-'
            END IF
         CY = ABS (CY)
         J = CY
         ITIME(4) = J
         XCOORD(4) = I * J
         CY = 60.0 * (CY - J)
         J = CY
         ITIME(5) = J
         XCOORD(5) = I * J
         CY = 60.0 * (CY - J)
         XCOORD(6) = I * CY
         CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (MSGTXT,1801) XSIGN, ITIME(1), ITIME(2), CX, YSIGN,
     *      ITIME(4), ITIME(5), CY
         IF (MSGTXT(23:23).EQ.' ') MSGTXT(23:23) = '0'
         IF (MSGTXT(39:39).EQ.' ') MSGTXT(39:39) = '0'
         CALL MSGWRT (2)
C                                       COORDINA -> X,Y
      ELSE
         CALL ADVERB ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         I = 1
         XSIGN = ' '
         IF ((XCOORD(1).LT.MEPS) .OR. (XCOORD(2).LT.MEPS) .OR.
     *      (XCOORD(3).LE.MEPS)) THEN
            I = -1
            XSIGN = '-'
            END IF
         ITIME(1) = ABS (XCOORD(1)) + 0.001
         ITIME(2) = ABS (XCOORD(2)) + 0.001
         CX = I * 15.0 * (ABS(XCOORD(1)) + ABS(XCOORD(2))/60.0 +
     *      ABS(XCOORD(3))/3600.0)
         I = 1
         YSIGN = ' '
         IF ((XCOORD(4).LT.MEPS) .OR. (XCOORD(5).LT.MEPS) .OR.
     *      (XCOORD(6).LE.MEPS)) THEN
            I = -1
            YSIGN = '-'
            END IF
         ITIME(4) = ABS (XCOORD(4)) + 0.001
         ITIME(5) = ABS (XCOORD(5)) + 0.001
         CY = I * (ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0 +
     *      ABS(XCOORD(6))/3600.0)
         CALL ADVRBS ('X', 'R', 1, 0, IDUM, CX, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('Y', 'R', 1, 0, IDUM, CY, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (MSGTXT,1810) XSIGN, ITIME(1), ITIME(2), ABS(XCOORD(3)),
     *      YSIGN, ITIME(4), ITIME(5), ABS(XCOORD(6))
         IF (MSGTXT(26:26).EQ.' ') MSGTXT(26:26) = '0'
         IF (MSGTXT(42:42).EQ.' ') MSGTXT(42:42) = '0'
         CALL MSGWRT (2)
         WRITE (MSGTXT,1811) CX, CY
         CALL MSGWRT (2)
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       GETDATE
C                                       return date string
C                                       in various forms
C-----------------------------------------------------------------------
 900  CALL ADVERB ('FORMAT','I', 1, 0, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((I.LT.1) .OR. (I.GT.3)) I = 0
      CALL ZDATE (ITIME(1))
      CALL ZTIME (ITIME(4))
      IF (I.EQ.3) THEN
         WRITE (THEDAT,1903) ITIME
      ELSE IF (I.EQ.2) THEN
         WRITE (THEDAT,1902) ITIME
      ELSE IF (I.EQ.1) THEN
         WRITE (THEDAT,1901) ITIME(1), MONTH(ITIME(2)), ITIME(3),
     *      ITIME(4), ITIME(5), ITIME(6)
      ELSE
         WRITE (THEDAT,1900) ITIME(1), MONTH(ITIME(2)), ITIME(3),
     *      ITIME(4), ITIME(5), ITIME(6)
         END IF
      CALL ADVRBS ('THEDATE','C', 1, 24, IDUM, RDUM, THEDAT)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       Print error, clear catalog.
 970  CALL MSGWRT (6)
 975  IF (CLSTAT.NE.' ') CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM,
     *   LOCCLS, LOCSEQ, LOCTYP, IUSER, CLSTAT, IBUFF, IERR)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('CATALOG ENTRY NOT FOUND')
 1100 FORMAT ('CATALOG READ ERROR',I5)
 1101 FORMAT ('CATALOG FILE TOO BUSY')
 1200 FORMAT ('HIFILE FOR ',A12,'.',A6,'.',I5,' HAS',I8,' RECORDS!!!')
 1210 FORMAT ('HITEXT BREC=',I8,' EREC=',I8,'/ line range < 1 - ',I8)
 1300 FORMAT ('CONFIRMATION ALLOWED ONLY IN INTERACTIVE MODE')
 1305 FORMAT ('CONDEMNED RECORDS',I8,' THRU ',I8,' OUTSIDE 1 -',I8)
 1315 FORMAT (I7,1X,A)
 1325 FORMAT (I8,' messages have been shipped to Siberian salt mine')
 1400 FORMAT ('AT ',A,A,'  Cpu=',F8.1,'  Real=',I10)
 1401 FORMAT ('AT ',A,A,'  Cpu=',F8.1)
 1500 FORMAT ('PIXEL',2I10,' NOT IN IMAGE')
 1610 FORMAT ('COODEFIN / ',A,' OLD',1PE17.10,' NEW',1PE17.10)
 1800 FORMAT ('Convert X,Y =',F12.6,'    ',F12.6,'   degrees')
 1801 FORMAT ('           to',2X,A1,I2.2,':',I2.2,':',F6.3,3X,A1,I2.2,
     *   ':',I2.2,':',F5.2)
 1810 FORMAT ('Convert COORDINA',2X,A1,I2.2,':',I2.2,':',F6.3,3X,A1,
     *   I2.2,':',I2.2,':',F5.2)
 1811 FORMAT ('        to X,Y =',F12.6,'    ',F12.6,'   degrees')
 1900 FORMAT (I4.4,'-',A3,'-',I2.2,1X,2(I2.2,':'),I2.2)
 1901 FORMAT (I4.4,A3,I2.2,'.',3I2.2)
 1902 FORMAT (I4.4,2I2.2,'.',3I2.2)
 1903 FORMAT (I4.4,5I2.2)
      END

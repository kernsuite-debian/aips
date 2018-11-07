      SUBROUTINE AU7 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to print history, rescale image, alter axis descriptions
C# POPS-appl Coordinates Header History Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-2002, 2004-2007, 2009, 2012, 2015-2016
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
C   AU7 verbs:
C   1. PRTHI    prints the history file for a given catalog entry.
C   2. RENAME   renames a catalog entry.
C   3. RESCALE  changes the scale factor and offset of a catalog entry.
C   4. CLRSTAT  clears a write and one read for a catalog entry.
C   5. AXDEFINE add or changes an axis description
C   6. ALTDEF   add alternative axis description for velocity.
C   7. ALTSWTCH switch to velocity or back to frequency on that axis.
C   8. CELGAL   switch between Celestial and Galactic coordinates
C   9. EPOSWTCH switch between B1950 and J2000 coordinates
C    **** COWINDOW now in AU7B ******
C  10. EPOCONV  switch coordinate adverbs between B1950 and J2000
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
C   (PRTHI only)
C      HISTRT   R     start record number
C      HIEND    R     end record number
C      PRTASK   H*5   only records w this at start
C      OUTPRINT H*48  File name to save printed output
C   (RENAME only)
C      OUTNAME  H*12  the new name for the catalog entry.  If blank, the
C                     old name will be used.
C      OUTCLASS H*6   the new class.  If blank the old class is used.
C      OUTSEQ   R     the new sequence number.  If zero, the old
C                     sequence number will be used.
C   (RESCALE only)
C      FACTOR   R     factor to multiply current map by
C      OFFSET   R     offset to add to current map
C   (AXDEFINE only)
C      NAXIS    R     axis number to alter (<= NDIM+1)
C      AXTYPE   H     axis type (char(8)): blank illegal
C      AXREF    R     axis reference pixel - no default
C      AXVAL    R(2)  axis value at ref pixel no default
C      RAXINC   R     axis coord incr. (0 => no change)
C   (ALTDEF only)
C      AXVAL    R(2)   velocity at (new) reference pixel.
C      RESTFR   R(2)   rest frequency.
C      AXTYPE   R(2)   'OPT' or 'RAD' plus 'HEL','LSR', or 'OBS'
C      AXREF    R      new ref. pixel for velocity
C   (ALTSWTCH, CELGAL, EPOSWTCH : no special ones)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER  PRGNAM*6, LOCNAM*12, LOCCLS*6, LOCTYP*2, STATUS*4,
     *   VRBNAM*6, RESCA*6, RENAM*6, TITL1*132, TITL2*132, PLINE*132,
     *   SCRTCH*132, PRTNAM*48, PRTACK*8, CRRTYP*8, NEWNAM*12, NEWCLS*6,
     *   HILINE*72, DEFCLS*6, AGECOM(2)*4, VELREF(5)*3, REAXIS*6,
     *   FRAXIS(3)*4, XAXTY(4)*4, YAXTY(4)*4, CGEOM(3)*4, TMPTYP*2,
     *   CDUM*1, LAXTYP*8, TABEP1*8, TABEP2*8, TABEP3*8, CRD1*40,
     *   CRD2*40, CRD3*40, LABEP1*80, LABEP2*80, LABEP3*80, OPTYPE*4,
     *   DHC*1, RHC*1, CSAVE*1
      DOUBLE PRECISION DVZERO, FRLINE, DELNU, REFNU, VSIGN, RAGP, DECGP,
     *   LONCP, RA, DEC, GLAT, GLON, RHO, INFACT, DOFFS, JD, JD50, JD00,
     *   XIN, XOU, YIN, YOU, CRD13(11), ROTN13, X13, Y13
      REAL      PRINTR, R1BUF(MABFSS), R2BUF(MABFSS), RDUM, LAXREF,
     *   LAXINC, LAXVAL(2), RESTFR(2), EPOCIN, EPOCOU, EPOCOB,
     *   XCOORD(6), DH, RH
      INTEGER   IREC, IBLK, IBUFF(256), HIBUFF(256), JTRIM, ICUR, IEND,
     *   IERR, BREC, NUMCHR, IERR2, IFIND, ILUN, IMAX, IMOD, IOFFSQ,
     *   NPI, NACROS, ISEQN, LOCSEQ, ISLOT, IUSER, IVOL, IWORD, J, PAGE,
     *   POTERR, IHLUN, IHPTR, I, ICARD, IERH, IHIND, II, ILINE, NDIM,
     *   NLPR, NREC, NWPL, IAXCOD, IXA, IYA, ITA, NGEOM, NCR, NCW, PLUN,
     *   TMPVOL, TMPSLT, EREC, JJ, IDUM, DEPTH(5), IT(6), IROUND,
     *   NCOL, RHM(2), DHM(2)
      LOGICAL   EQUAL, EXCL, NOMAP, NOSAVE, SAVE, WAIT, T, F, QUIT,
     *   FOPEN, POPEN
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DIO.INC'
      COMMON /AIPSCR/ R1BUF, R2BUF
      EQUIVALENCE (T, WAIT, SAVE, EXCL)
      EQUIVALENCE (F, NOMAP, NOSAVE)
      DATA T, F /.TRUE.,.FALSE./
      DATA IHLUN, ILUN, IOFFSQ /27, 15, 4/
      DATA PRGNAM /'AU7   '/
      DATA RENAM, RESCA /'RENAM ', 'RESCA '/
      DATA REAXIS, AGECOM /'AXDEF ', 'OLD ','NEW '/
      DATA VELREF /'LSR','HEL','OBS','OPT','RAD'/
      DATA FRAXIS /'FREQ','FELO','VELO'/
      DATA XAXTY /'LL  ','RA  ','RA--','GLON'/
      DATA YAXTY /'MM  ','DEC ','DEC-','GLAT'/
      DATA NGEOM, CGEOM /3, '-SIN','-TAN','-ARC'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C                                       Set initial values.
      POTERR = 0
      IF (BRANCH.NE.10) THEN
         NWPL = 10
         NLPR = 256 / NWPL
         CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         IUSER = NLUSER
         LOCTYP = ' '
         IF (BRANCH.EQ.3) LOCTYP = 'MA'
         ISLOT = 1
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
      VRBNAM = RESCA
      GO TO (100, 200, 300, 400, 500, 600, 700, 800, 900, 950), BRANCH
C-----------------------------------------------------------------------
C                                       PRTHI
C                                       write history file for image.
C-----------------------------------------------------------------------
 100  CALL ADVERB ('HISTART', 'I', 1, 0, BREC, RDUM, CDUM)
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
      CALL ADVERB ('DOCRT', 'R', 1, 0, IDUM, PRINTR, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) PRINTR =
     *      MIN (-1.0, PRINTR)
      FOPEN = .FALSE.
      POPEN = .FALSE.
C                                       Open history file.
      CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
      POTERR = 34
      IF (IERR.NE.0) GO TO 980
      PAGE = 0
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
C                                       loop limits
      IF ((BREC.LT.1) .OR. (BREC.GT.NREC)) BREC = 1
      IF ((EREC.LT.BREC) .OR. (EREC.GT.NREC)) EREC = NREC
C                                       Open the line printer.
      IF (PRINTR.LE.0.0) THEN
         CALL ADVERB ('OUTPRINT', 'C', 1, 48, IDUM, RDUM, PRTNAM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (PRTNAM.EQ.' ') THEN
            PRINTR = -1.0
C                                       do we really mean it?
            IF ((EREC-BREC.GE.400) .AND. ((IUNIT.EQ.1) .OR.
     *         (IUNIT.EQ.4))) THEN
C                                       Loop for no. of hist. recs.
               POTERR = 50
               IBLK = (BREC-1) / NHILPR + 1
               ICARD = BREC - (IBLK-1) * NHILPR
               CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
               IF (IERR.NE.0) GO TO 150
               ILINE = 0
               NUMCHR = JTRIM (PRTACK)
               DO 110 ICUR = BREC,EREC
C                                       Read next buffer.
                  IF (ICARD.GT.NHILPR) THEN
                     IBLK = IBLK + 1
                     ICARD = 1
                     CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 150
                     END IF
C                                       desired task?
                  II = (ICARD-1) * NHIWPL + 5
                  CALL H2CHR (72, 1, HIBUFF(II), PLINE(ITA:))
                  IF (NUMCHR.GT.0) THEN
                     CALL CHBLNK (72, 1, PLINE(ITA:), NPI)
                     NPI = NPI + ITA - 1
                     EQUAL = PRTACK(1:NUMCHR).EQ.PLINE(NPI:NPI+NUMCHR-1)
                  ELSE
                     EQUAL = .TRUE.
                     END IF
                  IF (EQUAL) ILINE = ILINE + 1
                  ICARD = ICARD + 1
 110              CONTINUE
               IF (ILINE.GE.400) THEN
                  WRITE (MSGTXT,1100) ILINE
                  CALL MSGWRT (3)
                  CALL CONFRM (J)
                  IF (J.GT.0) GO TO 150
                  END IF
               END IF
            END IF
         CALL LPOPEN (PRTNAM, PRINTR, PLUN, IFIND, NACROS, IBUFF, IERR)
         POPEN = IERR.EQ.0
C                                       Set values for crt.
      ELSE
         PLUN = 5
         PRTNAM = 'TTY'
         CALL LSERCH ('SRCH', PLUN, IFIND, NOMAP, IERR)
         CALL ZWINC (NCOL)
         NACROS = NCOL
         END IF
      POTERR = 57
      IF (IERR.NE.0) GO TO 150
      IF (NACROS.GE.80.) THEN
         NACROS = 80
         ITA = 9
      ELSE
         NACROS = 72
         ITA = 1
         END IF
C                                       open file
      CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'READ', IBUFF, IERR)
      POTERR = 33
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) GO TO 150
      FOPEN = .TRUE.
C                                       Test for erroneous no. of recs.
      IF ((NREC.LE.0) .OR. (NREC.GT.10000)) THEN
         WRITE (MSGTXT,1105) LOCNAM, LOCCLS, LOCSEQ, NREC
         CALL MSGWRT (7)
         IF ((NREC.LE.0) .OR. ((PRTNAM.EQ.' ') .AND. (NREC.GT.15000)))
     *      THEN
            POTERR = 32
            IERR = 101
            GO TO 150
            END IF
         END IF
C                                       Loop for no. of hist. recs.
      ILINE = 900
      POTERR = 50
      WRITE (TITL1,1110) LOCNAM, LOCCLS, LOCSEQ, NREC
      TITL2 = ' '
      PLINE = ' '
      IBLK = (BREC-1) / NHILPR + 1
      ICARD = BREC - (IBLK-1) * NHILPR
      CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       once for -3
      IF (PRINTR.LE.-2.5) THEN
         CALL PRTALN (PLUN, IFIND, PRINTR, NACROS, TITL1, TITL2,
     *      TITL1, ILINE, PAGE, SCRTCH, QUIT)
         IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 150
         END IF
C                                       string restrictions
      NUMCHR = JTRIM (PRTACK)
      DO 125 ICUR = BREC,EREC
C                                       Read next buffer.
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 150
            END IF
C                                       desired task?
         II = (ICARD-1) * NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(II), PLINE(ITA:))
         IF (NUMCHR.GT.0) THEN
            CALL CHBLNK (72, 1, PLINE(ITA:), NPI)
            NPI = NPI + ITA - 1
            EQUAL = PRTACK(1:NUMCHR).EQ.PLINE(NPI:NPI+NUMCHR-1)
         ELSE
            EQUAL = .TRUE.
            END IF
         IF (EQUAL) THEN
            IF (ITA.GT.1) WRITE (PLINE(1:ITA-1),1115) ICUR
            JJ = JTRIM (PLINE)
            CALL PRTALN (PLUN, IFIND, PRINTR, NACROS, TITL1, TITL2,
     *         PLINE, ILINE, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 150
            END IF
         ICARD = ICARD + 1
 125     CONTINUE
C                                       Close history file.
 150  CALL HICLOS (IHLUN, NOSAVE, HIBUFF, IERR2)
C                                       Close printer.
      IF (POPEN) CALL LPCLOS (PLUN, IFIND, ILINE, IERR2)
C                                       Clear read.
      IF (FOPEN) CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS,
     *   LOCSEQ, LOCTYP, IUSER, 'CLRD', IBUFF, IERR2)
      POTERR = 33
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       RENAME
C                                       rename catalog entry.
C-----------------------------------------------------------------------
C                                       Initial values for RENAME.
 200  VRBNAM = RENAM
      CALL ADVERB ('OUTNAME', 'C', 1, 12, IDUM, RDUM, NEWNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('OUTCLASS', 'C', 1, 6, IDUM, RDUM, NEWCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('OUTSEQ', 'I', 1, 0, ISEQN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      DEFCLS = LOCCLS
      CALL MAKOUT (LOCNAM, LOCCLS, LOCSEQ, DEFCLS, NEWNAM, NEWCLS,
     *   ISEQN)
C                                       PPM 93.01.19: was IBUFF(IWORD)
C                                       but that's a relic.
      IUSER = NLUSER
C                                       Check the name.
      TMPVOL = 0
      TMPSLT = 0
      TMPTYP = '  '
C                                       Does a duplicate exist?  Don't
C                                       include type in search.
      IF (ISEQN.NE.0) THEN
         CALL CATDIR ('SRNN', TMPVOL, TMPSLT, NEWNAM, NEWCLS, ISEQN,
     *      TMPTYP, IUSER, STATUS, IBUFF, IERR)
C                                       Duplicate name found.
         IF (IERR.EQ.0) THEN
            WRITE (MSGTXT, 1210)
            CALL MSGWRT (8)
            POTERR = 101
            GO TO 980
            END IF
C                                       Find next available sequence #
      ELSE
         CALL CATDIR ('SRCH', TMPVOL, TMPSLT, NEWNAM, NEWCLS, ISEQN,
     *      TMPTYP, IUSER, STATUS, IBUFF, IERR)
         IF (IERR.EQ.0) THEN
            ISEQN = ISEQN + 1
         ELSE
            ISEQN = 1
            END IF
         END IF
C                                       Open file excl.
      CALL CATOPN (IVOL, IFIND, IBUFF, IMAX, IERR)
      POTERR = 33
      IF (IERR.NE.0) GO TO 980
C                                       Calculate catalog entry locatn.
      IMOD = (ISLOT - 1) / NLPR
      IREC = 2 + IMOD
      IWORD = 1 + NWPL * (ISLOT - NLPR*IMOD - 1)
C                                       Load proper catalog record.
      CALL ZFIO ('READ', ILUN, IFIND, IREC, IBUFF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 240
      IF (IBUFF(IWORD+1).NE.0) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         IERR = 99
         POTERR = 33
C                                       fill values for the name
      ELSE
         CALL CHR2H (12, NEWNAM, 1, IBUFF(IWORD+5))
         CALL CHR2H (6, NEWCLS, 13, IBUFF(IWORD+5))
         IBUFF(IWORD+IOFFSQ) = ISEQN
C                                       Resave record.
         CALL ZFIO ('WRIT', ILUN, IFIND, IREC, IBUFF, IERR)
         POTERR = 50
         END IF
C                                       Close catalog file.
 240  CALL ZCLOSE (ILUN, IFIND, IERR2)
      IF ((IERR.EQ.0) .AND. (IERR2.NE.0)) POTERR = 50
      IF ((IERR.NE.0) .OR. (IERR2.NE.0)) GO TO 980
C-----------------------------------------------------------------------
C                                       RESCALE and RENAME
C                                       update the cat header
C-----------------------------------------------------------------------
C                                       Load catalog header.
 300  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IERH = 0
C                                       Catalog read error.
      IF (IERR.EQ.0) GO TO 310
         WRITE (MSGTXT,1300) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1301)
         POTERR = 33
         GO TO 970
C                                       Common history file stuff.
 310  CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR2)
      IF (IERR2.EQ.0) CALL HENCO1 (VRBNAM, LOCNAM, LOCCLS, LOCSEQ,
     *   IVOL, IHLUN, HIBUFF, IERR)
      IERH = IERR
C                                       Change appropriate values.
C                                       RENAME
      IF (BRANCH.EQ.2) THEN
         CALL CHR2H (12, NEWNAM, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, NEWCLS, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = ISEQN
         WRITE (MSGTXT,1320) LOCNAM, LOCCLS, LOCSEQ, LOCTYP, IVOL,
     *      ISLOT
         CALL MSGWRT (2)
         WRITE (MSGTXT,1321) NEWNAM, NEWCLS, ISEQN, LOCTYP, IVOL,
     *      ISLOT
         CALL MSGWRT (2)
         IF (IERR2.EQ.0) THEN
            CALL HENCOO (RENAM, NEWNAM, NEWCLS, ISEQN, IVOL, IHLUN,
     *         HIBUFF, IERR)
            IERH = IERH + IERR
            END IF
C                                       RESCALE
      ELSE
         CALL ADVERB ('OFFSET', 'R', 1, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         DOFFS = RDUM
         CALL ADVERB ('FACTOR', 'R', 1, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         INFACT = RDUM
         IF (INFACT.EQ.0.0) INFACT = 1.0
         JJ = MABFSS * 2
         CALL RESCAL (INFACT, DOFFS, IVOL, ISLOT, 17, 18, R1BUF, JJ,
     *      R2BUF, JJ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1330) IERR
            POTERR = 50
            GO TO 960
            END IF
         CALL RESCSL (IVOL, ISLOT, INFACT, DOFFS, 18, R1BUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1331) IERR
            POTERR = 50
            GO TO 960
            END IF
         IF (IERR2.EQ.0) THEN
            WRITE (HILINE,1335) VRBNAM, INFACT, DOFFS
            CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
            IERH = IERH + IERR
            END IF
         END IF
C                                       Close history file
 340  IF (IERR2.EQ.0) THEN
         CALL HICLOS (IHLUN, SAVE, HIBUFF, IERR)
         IERH = IERH + IERR
         END IF
C                                       Write updated catalog.
 350  CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF, IERR)
      POTERR = 0
      IF (IERH.GT.0) POTERR = 34
C                                       Catalog write error.
      IF ((IERR.EQ.0) .OR. (IERR.EQ.9)) GO TO 980
         WRITE (MSGTXT,1350) IERR
         POTERR = 33
         GO TO 970
C-----------------------------------------------------------------------
C                                       CLRSTAT
C                                       Clear catalog read or write.
C-----------------------------------------------------------------------
 400  POTERR = 33
      NCW = 0
      NCR = 0
      CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   IUSER, 'CLWR', IBUFF, IERR)
      IF (IERR.EQ.0) NCW = NCW + 1
      IF ((IERR.NE.10) .AND. (IERR.GT.0)) GO TO 980
      DO 410 I = 1,1000
         CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ,
     *       LOCTYP, IUSER, 'CLRD', IBUFF, IERR)
         IF (IERR.EQ.0) NCR = NCR + 1
         IF (IERR.GT.0) GO TO 420
 410     CONTINUE
 420  IF (IERR.EQ.10) POTERR = 0
      IF ((NCW.LE.0) .AND. (NCR.LE.0)) GO TO 980
         WRITE (MSGTXT,1420) NCW, NCR
         CALL MSGWRT (2)
         GO TO 980
C-----------------------------------------------------------------------
C                                       AXDEFINE
C                                       reset axis parms
C-----------------------------------------------------------------------
 500  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 510
         POTERR = 33
         GO TO 970
C                                       check parms
 510  NDIM = CATBLK(KIDIM)
      CALL ADVERB ('AXTYPE', 'C', 1, 8, IDUM, RDUM, LAXTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('AXREF', 'R', 1, 0, IDUM, LAXREF, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('AXINC', 'R', 1, 0, IDUM, LAXINC, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('AXVAL', 'R', 2, 0, IDUM, LAXVAL, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('NAXIS', 'I', 1, 0, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (I.EQ.0) I = NDIM + 1
      IF ((I.GT.0) .AND. (I.LE.NDIM+1) .AND. (I.LE.7) .AND.
     *   (LAXTYP.NE.' ')) GO TO 520
C                                       allow remove last axis
         IF ((I.EQ.NDIM) .AND. (CATBLK(KINAX+I-1).EQ.1) .AND.
     *      (LAXTYP.EQ.' ')) GO TO 520
         WRITE (MSGTXT,1510)
         POTERR = 101
         GO TO 970
C                                       Old to history
 520  II = 1
      CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR2)
      IF (IERR2.EQ.0) THEN
         CALL HENCO1 (REAXIS, LOCNAM, LOCCLS, LOCSEQ, IVOL, IHLUN,
     *      HIBUFF, IERR)
         IERH = IERR + IERR2
         END IF
C                                       Axis parms to history
 530  IF (IERR2.NE.0) GO TO 540
         IF (I.GT.NDIM) GO TO 540
         CALL H2CHR (8, 1, CATH(KHCTP+(I-1)*2), CRRTYP)
         WRITE (HILINE,1530) I, CRRTYP, AGECOM(II)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IERH = IERH + IERR
         IF (IERR.NE.0) GO TO 540
         WRITE (HILINE,1531) I, CATD(KDCRV+I-1), AGECOM(II)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IERH = IERH + IERR
         IF (IERR.NE.0) GO TO 540
         WRITE (HILINE,1532) I, CATR(KRCIC+I-1), AGECOM(II)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IERH = IERH + IERR
         IF (IERR.NE.0) GO TO 540
         WRITE (HILINE,1533) I, CATR(KRCRP+I-1), AGECOM(II)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IERH = IERH + IERR
         IF (IERR.NE.0) GO TO 540
C                                       Update axis parms
 540  IF (II.NE.1) GO TO 340
      IF (LAXTYP.EQ.' ') GO TO 550
         CALL CHR2H (8, LAXTYP, 1, CATH(KHCTP+2*I-2))
         CATR(KRCRP+I-1) = LAXREF
         IF ((LAXINC.NE.0.0) .OR. (I.GT.NDIM)) CATR(KRCIC+I-1) = LAXINC
         CALL POPSRD ('R2D', LAXVAL, CATD(KDCRV+I-1))
         IF (I.GT.NDIM) CATBLK(KINAX+I-1) = 1
         NDIM = MAX (I, NDIM)
         CATBLK(KIDIM) = NDIM
         II = 2
         GO TO 530
C                                       Blank last null axis
 550  CONTINUE
         CATH(KHCTP+2*I-2) = HBLANK
         CATH(KHCTP+2*I-1) = HBLANK
         CATR(KRCRP+I-1) = 0.0
         CATR(KRCIC+I-1) = 0.0
         CATD(KDCRV+I-1) = 0.0D0
         NDIM = NDIM - 1
         CATBLK(KIDIM) = NDIM
         WRITE (HILINE,1550) I
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IERH = IERH + IERR
         GO TO 340
C-----------------------------------------------------------------------
C                                       ALTDEF
C                                       define alternate velocity axis
C-----------------------------------------------------------------------
C                                       Pick up parameters
 600  CALL ADVERB ('AXTYPE', 'C', 1, 8, IDUM, RDUM, LAXTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('AXREF', 'R', 1, 0, IDUM, LAXREF, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('AXVAL', 'R', 2, 0, IDUM, LAXVAL, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('RESTFREQ', 'R', 2, 0, IDUM, RESTFR, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL POPSRD ('R2D', LAXVAL, DVZERO)
      CALL POPSRD ('R2D', RESTFR, FRLINE)
      IAXCOD = 1
      DO 605 J = 1,3
         I = INDEX (LAXTYP, VELREF(J))
         IF (I.GT.0) IAXCOD = J
 605     CONTINUE
      I = INDEX (LAXTYP, VELREF(5))
      IF (I.GT.0) IAXCOD = 256 + IAXCOD
C                                       Check
      IF (FRLINE.GT.1.E6) GO TO 610
         WRITE (MSGTXT,1600) FRLINE
         CALL MSGWRT (8)
         POTERR = 101
         GO TO 980
C                                       Get header
 610  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 615
         WRITE (MSGTXT,1300) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1301)
         POTERR = 33
         GO TO 970
C                                       Find needed axis
 615  IMAX = CATBLK(KIDIM)
      DO 620 I = 1,IMAX
         IEND = KHCTP + (I-1)*2
         CALL H2CHR (8, 1, CATH(IEND), LAXTYP)
         DO 620 J = 1,3
            IF (FRAXIS(J).EQ.LAXTYP(1:4)) GO TO 625
 620     CONTINUE
      WRITE (MSGTXT,1615)
      POTERR = 101
      GO TO 970
C                                       Got a FREQuency axis
 625  IF (J.NE.1) GO TO 630
         CATD(KDRST) = FRLINE
         CATBLK(KIALT) = IAXCOD
         CATD(KDARV) = DVZERO
         CATR(KRARP) = LAXREF
         GO TO 640
C                                       Got a VELO or FELO axis
C                                       but can't recover frequency
 630  IF ((CATBLK(KIALT).NE.0) .AND. (CATD(KDARV).GT.1.E6) .AND.
     *   (CATD(KDRST).GT.1.E6)) GO TO 635
         WRITE (MSGTXT,1630)
         POTERR = 101
         GO TO 970
C                                       Can recover old freq info
 635  CONTINUE
         VSIGN = 1.0D0
         IF (J.EQ.3) VSIGN = -VSIGN
         DELNU = -CATR(KRCIC+I-1) * CATD(KDARV) / (VELITE +
     *      VSIGN * CATD(KDCRV+I-1))
         REFNU = CATD(KDARV) - DELNU * (CATR(KRCRP+I-1) - CATR(KRARP))
         CATD(KDRST) = FRLINE
         CATBLK(KIALT) = IAXCOD
         CATD(KDARV) = REFNU + DELNU * (LAXREF - CATR(KRARP))
         CATD(KDCRV+I-1) = DVZERO
         CATR(KRCRP+I-1) = LAXREF
         J = IAXCOD / 256 + 2
         CATR(KRCIC+I-1) = -DELNU * (VELITE + VSIGN * DVZERO) /
     *      CATD(KDARV)
         LAXTYP = FRAXIS(J)
         LAXTYP(5:5) = '-'
         J = MOD (IAXCOD, 256)
         LAXTYP(6:8) = VELREF(J)
         CALL CHR2H (8, LAXTYP, 1, CATH(IEND))
C                                       Add to history
 640  CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERH)
      IF (IERH.NE.0) GO TO 350
      WRITE (HILINE,1640) FRLINE
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IERH = IERH + IERR
      IF (IERR.NE.0) GO TO 340
      WRITE (HILINE,1641) I, DVZERO, I, LAXREF
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IERH = IERH + IERR
      IF (IERR.NE.0) GO TO 340
      I = IAXCOD/256 + 4
      J = MOD (IAXCOD, 256)
      WRITE (HILINE,1642) VELREF(J), VELREF(I)
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IERH = IERH + IERR
      GO TO 340
C-----------------------------------------------------------------------
C                                       ALTSWTCH
C                                       switch freq/velo to alternate
C-----------------------------------------------------------------------
 700  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 710
         WRITE (MSGTXT,1300) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1301)
         POTERR = 33
         GO TO 970
C                                       Find needed axis
 710  IMAX = CATBLK(KIDIM)
      DO 720 I = 1,IMAX
         IEND = KHCTP + (I-1)*2
         CALL H2CHR (8, 1, CATH(IEND), LAXTYP)
         DO 720 J = 1,3
            IF (FRAXIS(J).EQ.LAXTYP(1:4)) GO TO 730
 720     CONTINUE
      WRITE (MSGTXT,1720)
      POTERR = 101
      GO TO 970
C                                       Is it ok?
 730  IF ((CATBLK(KIALT).GT.0) .AND. (CATD(KDRST).GT.1.E6)) GO TO 740
         WRITE (MSGTXT,1730)
         POTERR = 101
         GO TO 970
C                                       Pick up values
 740  VSIGN = 1.0D0
      IF (J.EQ.3) VSIGN = -VSIGN
      IF (J.EQ.1) THEN
         DELNU = CATR(KRCIC+I-1)
         REFNU = CATD(KDCRV+I-1)
      ELSE
         DELNU = -CATR(KRCIC+I-1) * CATD(KDARV) /
     *      (VELITE + VSIGN * CATD(KDCRV+I-1) + CATR(KRCIC+I-1) *
     *      (CATR(KRCRP+I-1) - CATR(KRARP)))
         REFNU = CATD(KDARV)
         END IF
      FRLINE = CATR(KRARP)
      DVZERO = CATD(KDARV)
      CATR(KRARP) = CATR(KRCRP+I-1)
      CATR(KRCRP+I-1) = FRLINE
      IERH = 0
C                                       To velocity
       IF (J.NE.1) GO TO 750
C                                       Warning for UV data
         IF (LOCTYP.EQ.'UV') THEN
            MSGTXT = 'Some tasks (PRTUV, UVPLT) may fail on a FELO-HEL'
            CALL MSGWRT (1)
            MSGTXT = 'axis. It is recommended to ALTSWTCH back to a'
            CALL MSGWRT (1)
            MSGTXT = 'FREQUENCY axis before proceeding. Use ALTSWTCH'
            CALL MSGWRT (1)
            MSGTXT = 'on MA (map) data, NOT on UV data.'
            CALL MSGWRT (1)
            END IF
         CATD(KDARV) = REFNU
         CATD(KDCRV+I-1) = DVZERO
         J = CATBLK(KIALT)/256 + 2
         IF (J.EQ.3) VSIGN = -VSIGN
         CATR(KRCIC+I-1) = -DELNU * (VELITE + VSIGN * DVZERO) /
     *      (REFNU + DELNU * (FRLINE - CATR(KRARP)))
         LAXTYP = FRAXIS(J)
         LAXTYP(5:5) = '-'
         J = MOD (CATBLK(KIALT), 256)
         LAXTYP(6:8) = VELREF(J)
         CALL CHR2H (8, LAXTYP, 1, CATH(IEND))
         GO TO 350
C                                       To frequency
 750  CONTINUE
         CATD(KDARV) = CATD(KDCRV+I-1)
         CATD(KDCRV+I-1) = REFNU
         CATR(KRCIC+I-1) = DELNU
         LAXTYP = FRAXIS(1)
         CALL CHR2H (8, LAXTYP, 1, CATH(IEND))
         GO TO 350
C-----------------------------------------------------------------------
C                                       CELGAL
C                                       switch Celestial/Galactic coord
C-----------------------------------------------------------------------
 800  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.EQ.0) GO TO 805
         WRITE (MSGTXT,1300) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1301)
         POTERR = 33
         GO TO 970
C                                       Find needed axis
 805  IXA = 0
      IYA = 0
      ITA = 0
      II = CATBLK(KIDIM)
      CSAVE = '+'
      DO 820 I = 1,II
         CALL H2CHR (8, 1, CATH(KHCTP+(I-1)*2), LAXTYP)
         DO 815 J = 1,4
            IF (XAXTY(J).EQ.LAXTYP(1:4)) THEN
               IF (IXA.GT.0) GO TO 825
               IF ((ITA.GT.0) .AND. (ITA.NE.(J+2)/3)) GO TO 825
                  IXA = I
                  ITA = (J+2) / 3
                  CSAVE = LAXTYP(5:5)
            ELSE IF (YAXTY(J).EQ.LAXTYP(1:4)) THEN
               IF (IYA.GT.0) GO TO 825
               IF ((ITA.GT.0) .AND. (ITA.NE.(J+2)/3)) GO TO 825
                  IYA = I
                  ITA = (J+2) / 3
                  CSAVE = LAXTYP(5:5)
               END IF
 815        CONTINUE
 820     CONTINUE
      IF ((IXA.GT.0) .OR. (IYA.GT.0)) GO TO 830
C                                       Axes inconsistant somehow
 825  WRITE (MSGTXT,1825)
      POTERR = 101
      GO TO 970
C                                       projected or not
 830  IF ((ITA.EQ.2) .AND. (CSAVE.EQ.' ')) ITA = 3
C                                       fix up old LL, MM
      II = KHCTP + (IYA-1) * 2
      CALL H2CHR (8, 1, CATH(II), LAXTYP)
      EQUAL = LAXTYP.EQ.(YAXTY(1) // '    ')
      IF (EQUAL) CALL CHR2H (4, CGEOM(1), 5, CATH(II))
      II = KHCTP + (IXA-1) * 2
      CALL H2CHR (8, 1, CATH(II), LAXTYP)
      EQUAL = LAXTYP.EQ.(XAXTY(1) // '    ')
      IF (EQUAL) CALL CHR2H (4, CGEOM(1), 5, CATH(II))
C                                       Is it ok geometry ?
C                                       Only care if > 1 point on axis
      IF (CATH(II+1).NE.CATH(KHCTP+2*IYA-1)) GO TO 825
      IF ((CATBLK(KINAX+IYA-1).EQ.1) .AND. (CATBLK(KINAX+IXA-1).EQ.1))
     *   GO TO 840
         CALL H2CHR (8, 1, CATH(II), LAXTYP)
         DO 835 I = 1,NGEOM
            IF (LAXTYP(5:8).EQ.CGEOM(I)) GO TO 840
 835     CONTINUE
         WRITE (MSGTXT,1835)
         POTERR = 101
         GO TO 970
C                                       So do it finally
 840  IXA = IXA - 1
      IYA = IYA - 1
C                                       Get galactic coords for Epoch
      CALL GALPOL( CATR(KREPO), RAGP, DECGP, LONCP)
C                                       Convert to radians
      RAGP  = RAGP * DG2RAD
      DECGP = DECGP * DG2RAD
      LONCP = LONCP * DG2RAD
C                                       Celestial to Galactic
      IF (ITA.EQ.1) THEN
         DEC = CATD(KDCRV+IYA) * DG2RAD
         RA = CATD(KDCRV+IXA) * DG2RAD
         GLAT = ASIN (SIN(DEC)*SIN(DECGP) + COS(DEC)*COS(DECGP)*
     *      COS(RA-RAGP))
         GLON = LONCP + ATAN2 (COS(DEC)*SIN(RAGP-RA), SIN(DEC)*
     *      COS(DECGP) - COS(DEC)*SIN(DECGP)*COS(RA-RAGP))
         IF (GLON.GE.TWOPI) GLON = GLON - TWOPI
         IF (GLON.LT.0.0D0) GLON = GLON + TWOPI
C                                       Galactic to Celestial
      ELSE
         GLAT = CATD(KDCRV+IYA) * DG2RAD
         GLON = CATD(KDCRV+IXA) * DG2RAD
         DEC = ASIN (SIN(GLAT)*SIN(DECGP) + COS(GLAT)*COS(DECGP)*
     *      COS(GLON-LONCP))
         RA = RAGP + ATAN2 (COS(GLAT)*SIN(LONCP-GLON), SIN(GLAT)*
     *      COS(DECGP) - COS(GLAT)*SIN(DECGP)*COS(GLON-LONCP))
         IF (RA.GE.TWOPI) RA = RA - TWOPI
         IF (RA.LT.0.0D0) RA = RA + TWOPI
         END IF
C                                       rotation
      RHO = ATAN2 (COS(DECGP)*SIN(RA-RAGP), COS(DEC)*SIN(DECGP)
     *   - SIN(DEC)*COS(DECGP)*COS(RA-RAGP))
      IF (RHO.GT.TWOPI/2.D0) RHO = RHO - TWOPI
      IF (RHO.LT.-TWOPI/2.0D0) RHO = RHO + TWOPI
C                                       Store in header
      IERH = 0
      CALL CHR2H (4, XAXTY(5-ITA), 1, CATH(KHCTP+IXA*2))
      CALL CHR2H (4, YAXTY(5-ITA), 1, CATH(KHCTP+IYA*2))
      IF (ITA.EQ.1) THEN
         CATD(KDCRV+IXA) = GLON / DG2RAD
         CATD(KDCRV+IYA) = GLAT / DG2RAD
         CATR(KRCRT+IYA) = CATR(KRCRT+IYA) - RHO / DG2RAD
      ELSE
         CATD(KDCRV+IXA) = RA / DG2RAD
         CATD(KDCRV+IYA) = DEC / DG2RAD
         CATR(KRCRT+IYA) = CATR(KRCRT+IYA) + RHO / DG2RAD
         END IF
      GO TO 350
C-----------------------------------------------------------------------
C                                       EPOSWTCH
C                                       switch B1950 to J2000
C-----------------------------------------------------------------------
 900  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 33
         GO TO 970
         END IF
C                                       initial epoch coordinates
      LOCNUM = 1
      DO 905 I = 1,5
         IF (I+2.LE.CATBLK(KIDIM)) THEN
            DEPTH(I) = IROUND (CATR(KRCRP+I+1))
         ELSE
            DEPTH(I) = 1
            END IF
 905     CONTINUE
      CALL SETLOC (DEPTH, F)
C                                       check type of coordinates
      IF (AXTYP(LOCNUM).LE.0) THEN
         MSGTXT = 'NO COORDINATE AXES FOR WHICH TO SWITCH EPOCH'
         POTERR = 101
         GO TO 970
         END IF
      CALL H2CHR (8, 1, CATH(KHCTP+2*KLOCL(LOCNUM)), CRRTYP)
      CALL H2CHR (8, 1, CATH(KHCTP+2*KLOCM(LOCNUM)), LAXTYP)
      IF (((CRRTYP(:4).NE.'RA  ') .AND. (CRRTYP(:4).NE.'RA--')) .OR.
     *   ((LAXTYP(:4).NE.'DEC ') .AND. (LAXTYP(:4).NE.'DEC-'))) THEN
         MSGTXT = 'CAN SWITCH EPOCH ONLY FOR EQUATORIAL COORDINATES'
         POTERR = 101
         GO TO 970
         END IF
C                                       get the 3 epochs
      EPOCIN = CATR(KREPO)
      IF (ABS(EPOCIN-2000.).LT.0.5) THEN
         EPOCOU = 1950.
      ELSE
         EPOCOU = 2000.
         END IF
      CALL H2CHR (8, 1, CATH(KHDOB), CRRTYP)
      CALL FILL (6, 0, IT)
      IT(1) = 2050
      IT(2) = 1
      IT(3) = 1
      CALL DAT2JD (IT, JD00)
      IT(1) = 1950
      CALL DAT2JD (IT, JD50)
      IT(1) = 0
      CALL JULDAY (CRRTYP, JD)
      IF ((JD.LE.JD50) .OR. (JD.GE.JD00)) THEN
         MSGTXT = 'OBSERVATION date missing/wrong - switch less'
     *      // ' accurate'
C         CALL MSGWRT (6)
         EPOCOB = 1950.
      ELSE
         EPOCOB = (JD - JD50) / (JD00 - JD50) * 100.0D0 + 1950.0D0
         END IF
C                                       init conversion routines
      CALL STEPCH (EPOCIN, I, TABEP1, CRD1, LABEP1)
      CALL STEPCH (EPOCOB, I, TABEP2, CRD2, LABEP2)
      CALL STEPCH (EPOCOU, I, TABEP3, CRD3, LABEP3)
      CALL CRDSET (CRD1, CRD3, CRD13, IERR)
C                                       reference pixel
      XIN = CATD(KDCRV+KLOCL(LOCNUM))
      YIN = CATD(KDCRV+KLOCM(LOCNUM))
      CALL CRDTRN (XIN, YIN, CRD13, X13, Y13, ROTN13)
C                                       average 2 methods
      XIN = XIN * DG2RAD
      YIN = YIN * DG2RAD
      I = 1
      IF (ABS(EPOCOU-1950.).LT.0.5) I = -1
      CALL B2JPOS (I, XIN, YIN, XOU, YOU)
      XOU = (XOU * RAD2DG + X13) / 2.0D0
      YOU = (YOU * RAD2DG + Y13) / 2.0D0
C                                       header
      CATD(KDCRV+KLOCL(LOCNUM)) = XOU
      CATD(KDCRV+KLOCM(LOCNUM)) = YOU
      CATR(KRCRT+KLOCM(LOCNUM)) = CATR(KRCRT+KLOCM(LOCNUM)) - ROTN13
      CATR(KREPO) = EPOCOU
C                                       pointing position
      XIN = CATD(KDORA)
      YIN = CATD(KDODE)
      CALL CRDTRN (XIN, YIN, CRD13, X13, Y13, ROTN13)
C                                       average 2 methods
      XIN = XIN * DG2RAD
      YIN = YIN * DG2RAD
      I = 1
      IF (ABS(EPOCOU-1950.).LT.0.5) I = -1
      CALL B2JPOS (I, XIN, YIN, XOU, YOU)
      XOU = (XOU * RAD2DG + X13) / 2.0D0
      YOU = (YOU * RAD2DG + Y13) / 2.0D0
C                                       header
      CATD(KDORA) = XOU
      CATD(KDODE) = YOU
C                                       accuracy warning
      RDUM = 1.0 / 3600.0 / 10.
      IF ((ABS(CATR(KRCIC+KLOCL(LOCNUM))).LT.RDUM) .OR.
     *   (ABS(CATR(KRCIC+KLOCM(LOCNUM))).LT.RDUM)) THEN
         MSGTXT = 'WARNING: coordinate conversion is probably not '
     *      // 'accurate enough for you'
         CALL MSGWRT (6)
         END IF
C
C                                       update header
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF, IERR)
      POTERR = 0
C                                       Catalog write error.
      IF ((IERR.EQ.0) .OR. (IERR.EQ.9)) GO TO 980
         WRITE (MSGTXT,1350) IERR
         POTERR = 33
         GO TO 970
C-----------------------------------------------------------------------
C                                       EPOCONV
C                                       B1950 <-> J2000
C-----------------------------------------------------------------------
C                                       Get adverbs
 950  CALL ADVERB ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('OPTYPE', 'C', 1, 4, IDUM, RDUM, OPTYPE)
      IF (ERRNUM.NE.0) GO TO 980
      XIN = (ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *   ABS (XCOORD(3))/3600.0D0) * 15.00D0
      IF ((XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)) XIN = -XIN
      YIN = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3600.0D0
      IF ((XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)) YIN = -YIN
      IF (OPTYPE.NE.'1950') THEN
         EPOCIN = 1950.
         EPOCOU = 2000.
      ELSE
         EPOCIN = 2000.
         EPOCOU = 1950.
         END IF
C                                       tell user
      CALL COORDD (1, XIN, RHC, RHM, RH)
      CALL COORDD (2, YIN, DHC, DHM, DH)
      WRITE (MSGTXT,1950) 'Input ', RHC, RHM, RH, DHC, DHM, DH, EPOCIN
      IF (MSGTXT(18:18).EQ.' ') MSGTXT(18:18) = '0'
      IF (MSGTXT(38:38).EQ.' ') MSGTXT(38:38) = '0'
      CALL MSGWRT (4)
C                                       init conversion routines
      CALL STEPCH (EPOCIN, I, TABEP1, CRD1, LABEP1)
      CALL STEPCH (EPOCOU, I, TABEP3, CRD3, LABEP3)
      CALL CRDSET (CRD1, CRD3, CRD13, IERR)
      CALL CRDTRN (XIN, YIN, CRD13, X13, Y13, ROTN13)
C                                       average 2 methods
      XIN = XIN * DG2RAD
      YIN = YIN * DG2RAD
      I = 1
      IF (ABS(EPOCOU-1950.).LT.0.5) I = -1
      CALL B2JPOS (I, XIN, YIN, XOU, YOU)
      XOU = (XOU * RAD2DG + X13) / 2.0D0
      YOU = (YOU * RAD2DG + Y13) / 2.0D0
C                                       tell user
      CALL COORDD (1, XOU, RHC, RHM, RH)
      CALL COORDD (2, YOU, DHC, DHM, DH)
      WRITE (MSGTXT,1950) 'Output', RHC, RHM, RH, DHC, DHM, DH, EPOCOU
      IF (MSGTXT(18:18).EQ.' ') MSGTXT(18:18) = '0'
      IF (MSGTXT(38:38).EQ.' ') MSGTXT(38:38) = '0'
      CALL MSGWRT (4)
C                                       save result
      XIN = ABS (XOU) / 15.0D0
      YIN = ABS (YOU)
      I = XIN
      XCOORD(1) = I
      XIN = (XIN - XCOORD(1)) * 60.0D0
      I = XIN
      XCOORD(2) = I
      XCOORD(3) = (XIN - XCOORD(2)) * 60.0D0
      IF (XOU.LT.0) THEN
         XCOORD(1) = -XCOORD(1)
         XCOORD(2) = -XCOORD(2)
         XCOORD(3) = -XCOORD(3)
         END IF
      I = YIN
      XCOORD(4) = I
      YIN = (YIN - XCOORD(4)) * 60.0D0
      I = YIN
      XCOORD(5) = I
      XCOORD(6) = (YIN - XCOORD(5)) * 60.0D0
      IF (YOU.LT.0) THEN
         XCOORD(4) = -XCOORD(4)
         XCOORD(5) = -XCOORD(5)
         XCOORD(6) = -XCOORD(6)
         END IF
      CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       Print error, clear catalog.
 960  IF (IERR2.NE.0) CALL HICLOS (IHLUN, NOSAVE, HIBUFF, IERR)
 970  CALL MSGWRT (6)
      CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   IUSER, 'CLWR', IBUFF, IERR)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('CATALOG ENTRY NOT FOUND')
 1100 FORMAT ('Printer job of',I7,' lines: do you mean it?')
 1105 FORMAT ('HIFILE FOR ',A12,'.',A6,'.',I5,' HAS',I7,
     *   ' RECORDS!!!')
 1110 FORMAT ('History file for ',A12,'.',A6,'.',I5,' with',I7,
     *   ' entries')
 1115 FORMAT (I6,2X)
 1200 FORMAT ('CANNOT RENAME A BUSY IMAGE')
 1210 FORMAT ('DUPLICATE FILE NAME FOUND')
 1300 FORMAT ('CATALOG READ ERROR',I5)
 1301 FORMAT ('MAP TOO BUSY')
 1320 FORMAT ('Rename ',A12,'.',A6,'.',I4,' (',A2,')  on disk',I2,
     *   ' cno',I5)
 1321 FORMAT ('To     ',A12,'.',A6,'.',I4,' (',A2,')  on disk',I2,
     *   ' cno',I5)
 1330 FORMAT ('RESCALE: ERROR',I6,' TRYING TO RESCALE THE VALUES')
 1331 FORMAT ('RESCALE: ERROR',I6,' TRYING TO RESCALE SLICE FILES')
 1335 FORMAT (A6,' FACTOR=',E16.8,'   OFFSET=',E16.8)
 1350 FORMAT ('CATALOG WRITE ERROR',I5)
 1420 FORMAT ('Cleared',I2,' writes and',I4,' reads')
 1510 FORMAT ('INVALID NAXIS OR BLANK AXTYPE')
 1530 FORMAT ('AXDEF CTYPE',I1,'  = ''',A8,'''',10X,'/ ',A4,
     *   'Axis type')
 1531 FORMAT ('AXDEF CRVAL',I1,'  = ',1PE16.9,4X,'/ ',A4,
     *   'Reference pixel value')
 1532 FORMAT ('AXDEF CDELT',I1,'  = ',1PE12.5,8X,'/ ',A4,
     *   'Axis increment')
 1533 FORMAT ('AXDEF CRPIX',I1,'  = ',F9.3,11X,'/ ',A4,
     *   'Reference pixel loc')
 1550 FORMAT ('AXDEF / Axis',I2,' removed')
 1600 FORMAT ('REST FREQ MUST BE IN HZ',1PE13.5,' ILLEGAL')
 1615 FORMAT ('CAN''T FIND FREQ/VELO/FELO AXIS TO WHICH TO ATTACH',
     *   ' ALTERNATE DESCRIPTION')
 1630 FORMAT ('EXISTING VELO/FELO AXIS LACKS VALID ALTERNATE FREQ',
     *   ' DESCRIPTION')
 1640 FORMAT ('ALTDEF RESTFREQ=',1PE18.10,6X,'/Rest frequency in Hz')
 1641 FORMAT ('ALTDEF CRVAL',I1,'  = ',1PE16.9,3X,'CRPIX',I1,'  = ',
     *   F8.3)
 1642 FORMAT ('ALTDEF AXREFTYPE =''',2A3,2X,'''',12X,
     *   '/Velocity ref. types')
 1720 FORMAT ('CAN''T FIND FREQ/VELO/FELO AXIS TO SWITCH WITH',
     *   ' ALTERNATE')
 1730 FORMAT ('ALTERNATE AXIS PARAMETERS ARE INVALID: USE ALTDEF')
 1825 FORMAT ('AXES INCONSISTENT: NO CONVERSION DONE')
 1835 FORMAT ('CAN SWITCH ONLY TRUE PROJECTIVE TANGENT PLANE ',
     *   'GEOMETRIES')
 1950 FORMAT (A6,' RA ',A1,2(I2.2,':'),F6.3,'   Dec ',A1,2(I2.2,':'),
     *   F5.2,'   Epoch',F6.0)
      END

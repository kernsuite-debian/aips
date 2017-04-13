      SUBROUTINE AU2A (BRANCH)
C-----------------------------------------------------------------------
C! verb functions on task save and Save/Get files: TGET, SGdestr, index
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-1999, 2002-2014, 2016
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
C   AU2A performs verb-like functions related to Task-save and Save/Get
C   files.
C   Input: BRANCH = 1  : TGET     get last adverbs for a task
C                   2  : SGDESTR  destroy 1 save file
C                   3  : TGINDEX  list tasks having saved parms
C                   4  : SGINDEX  list save/get files
C                   5  : VGINDEX  list VGET/VPUT contents
C                   6  : VGET     get last saved task adverbs in version
C                   7  : DEFAULT  reset task adverbs
C                   8  : SG2RUN   copy K array to run file
C                   9  : PLGET    get PL file adverb values
C                  10  : SETCREAT sets quick or slow file creation
C   Inputs file function codes are in column 10 as:
C      ' '   GO
C      '*'   GO  TELL
C      '?'       TELL
C      '&'   GO  TELL  GET
C      '%'       TELL  GET
C      '$'   GO        GET
C      '@'             GET
C      other -> error message
C   INPUTS, TPUT, TGET and GO only do the GO adverbs, OUTPUTS only does
C   the GET adverbs, SHOW and TELL only do the TELL adverbs
C
C   For PLGET:
C      Plot types supported: 1 Misc.   2 CNTR    3 greys   4 PROFL
C                            5 SL2PL   6 pcntr   7 IMEAN   8 UVPLT
C                          [ 9 gnplt][10 vbplt] 11 PFPLn [12 gaplt]
C                           13 PLCUB  14 IMVIM  15 TAPLT  16 POSSM
C                           17 SNPLT  18 kntr   19 UVHGM  20 ISPEC
C                           21 VPLOT  22 CLPLT  23 DFTPL  24 FRPLT
C                           25 FRMAP  26 PLOTR  27 GREYS  28 PCNTR
C                           29 KNTR   30 BPLOT  31 APCAL  32 ANBPL
C                           33 IRING  34 RSPEC  35 CAPLT  36 RFLAG
C                           37 CCNTR  38 SERCH  39 EVAUV  40 DELZN
C                           41 ELINT  42 FGPLT  43 GAL    44 SNIFS
C                           45 SOUSP  46 WETHR  47 RM2PL  48 XG2PL
C                           49 ALVAR  50 PCPLT  51 PLROW
C       pcntr, greys, kntr (6, 3, 18) are old form replacd 2002-Nov
C       vbplt, gaplt, gnplt not supported 7/7/03
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NADP1, NADM1, NADSP, NA, NPLOTS
      PARAMETER (NADP1=53, NADM1=32, NADSP=18, NA=500, NPLOTS=51)
C
      CHARACTER ATIME*8, ADATE*12, PHNAME*48, ITASK*16, LVERSN*48,
     *   AVERSN*48, JBUF*80, VERVER(10)*8, PRGNAM*6, CTEST*8, ANAME*16,
     *   OLNAME*48, CDUM*1, ADVP1(NADP1)*8, ADVM1(NADM1)*8,
     *   ADVSP(NADSP)*8, NAMES(NA)*16, PNAMES(NPLOTS)*5, NAME*12,
     *   CLASS*6, TYPE*2, XNAMES(NA)*16
      INTEGER   POTERR, I, J, IERR, LUN, FIND, NTASK, IBLK(256), LUNIN,
     *   FINDIN, IPTR, ISIZE, IEOF, ITAG, ILOC, ITYPE, NWPL, NLPR, JJ,
     *   ITIME(6,NA), COLLBN, IUSER, IVER, ILVER, VERTYP, NMATCC, IOFF,
     *   NBLOCK, COLSHO, JOFF, IREC, LREC, JTRIM, IDUM, LUNDE, ALPHA,
     *   IVERS(NA), IVOL, ISEQ, IDATA(768), CATBLK(256), ISLOT, IMAXV,
     *   IPTYPE, NPARMS, PNPARM(NPLOTS), NAMTYP, JTYPE, JSIZE, NNAMES
      REAL      PARBUF(256), RDUM, VADVSP(NADSP), RVALU
      LOGICAL   T, F, WASDEF, OLDTG
      HOLLERITH HARBUF(256), HVALU, HADNAM(3)
      DOUBLE PRECISION JDS(NA), JDM
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:PKSZ.INC'
      COMMON /AIPSCR/ IBLK, IDATA, CATBLK
      COMMON /AIPCCR/ PHNAME, LVERSN, AVERSN, JBUF
      EQUIVALENCE (IBLK(1), PARBUF(1), HARBUF(1))
      EQUIVALENCE (NAMTYP, HADNAM(3))
      DATA PRGNAM /'AU2A  '/
      DATA COLLBN, COLSHO /11, 10/
      DATA LUN, LUNIN /13, 11/
      DATA IEOF /2/
      DATA T, F /.TRUE.,.FALSE./
      DATA VERVER /'OLD ', 'NEW ', 'TST ', 'CVX', 'OLDPSAP',
     *   'NEWPSAP', 'TSTPSAP', 'CVXPSAP', 'LOCAL ', 'PRIVATE '/
      DATA ADVM1 /'FQTOL','FREQID','SELFREQ','SELBAND','BPVER','BLVER',
     *   'QUAL','DO3DIMAG','DOBAND','DOPOL','DODELAY','DOCALIB',
     *   'DOOUTPUT','DOCONCAT','DOCONFRM','DOALPHA','ERROR','DOCIRCLE',
     *   'DOARRAY','DOSTOKES','DOALL','DORESID','DOMODEL','DOHIST',
     *   'DOINVERS','DOTV','DOWAIT','DOEBAR','DOACOR','DOUVCOMP',
     *   'STVERS', 'QCREATE'/
      DATA ADVP1 /'DOWEIGHT','DELCORR','DIGICOR','UVBXFN','CBPLOT',
     *   'DOGREY','REMTAPE','DOWEDGE','SLOT','DONEWTAB',
     *   'NFIELD','DOTWO','DOTABLE','DOEOT','DOALIGN','NPOINTS',
     *   'DOSLICE','DOCAT','DOEOF','DOGRIDCR','DOCONT','DOVECT',
     *   'TVCHAN','OUTTAPE','INTAPE','OUT2DISK','OUTDISK',
     *   'DOPOS','DOMAX','DOWIDTH','CHINC','COPIES','BCOUNT','BCHAN',
     *   'XINC','YINC','ZINC','TXINC','TYINC','TZINC','ORDER',
     *   'NPLOTS','BPRINT','NCHAV','DODARK','DOBTWEEN',
     *   'VECTOR','HISTART','AXREF','DOHMS','AVGCHAN','DOROBUST',
     *   'FQCENTER'/
      DATA ADVSP /'MAXPIXEL','BLOCKING','DENSITY','DOCRT','NAXIS',
     *   'MINPATCH','DIST','ICUT','PCUT','SKEW','ZXRATIO','LTYPE',
     *   'BATQUE','GAIN','XTYPE','YTYPE','DARKLINE','LPEN'/
      DATA VADVSP /20000., 10., 6250., 132., 3., 51., 3., 0.1, 0.1, 45.,
     *   0.25, 3., 2., 0.1, 5., 5., 0.33, 3./
C                                       supported PLGET task names
C                                       must match AU8A (EXTLIST)
      DATA PNAMES /'    ', 'CNTR ', '     ', 'PROFL', 'SL2PL',
     *   '     ', 'IMEAN', 'UVPLT', '     ', '     ',
     *   '     ', '     ', 'PLCUB', 'IMVIM', 'TAPLT',
     *   'POSSM', 'SNPLT', '     ', 'UVHGM', 'ISPEC',
     *   'VPLOT', 'CLPLT', 'DFTPL', 'FRPLT', 'FRMAP',
     *   'PLOTR', 'GREYS', 'PCNTR', 'KNTR ', 'BPLOT',
     *   'APCAL', 'ANBPL', 'IRING', 'RSPEC', 'CAPLT',
     *   'RFLAG', 'CCNTR', 'SERCH', 'EVAUV', 'DELZN',
     *   'ELINT', 'FGPLT', 'GAL  ', 'SNIFS', 'SOUSP',
     *   'WETHR', 'RM2PL', 'XG2PL', 'ALVAR', 'PCPLT', 'PLROW'/
      DATA PNPARM /
     *     0,  63, 279,  39,  20,  87,  46, 303,   0,   0,
     *     0,   0,  39,  47,  82, 314, 242,  80, 271,  49,
     *   403, 320, 176, 314, 305, 126, 331, 207, 222,  85,
     *   342, 296,  62,  57, 307, 537,  70, 136, 197, 353,
     *   334, 127,  97, 210, 340, 103,  61,  62, 270,  85,  29/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.9)) GO TO 999
      NBLOCK = 8
      FINDIN = 0
      FIND = 0
      OLDTG = .TRUE.
C                                       Get immediate argument
      IF ((BRANCH.LE.2) .OR. (BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         POTERR = 7
         IF (SP.LT.4) GO TO 980
         POTERR = 8
         I = 8
         IF (BRANCH.EQ.2) I = 16
         IF ((STACK(SP).NE.2) .OR. (STACK(SP-3).NE.14) .OR.
     *      (STACK(SP-2).NE.I)) GO TO 980
         WASDEF = HBLANK.EQ.CH(STACK(SP-1))
         SP = SP - 4
         ITASK = ' '
         IF (WASDEF) THEN
            CALL ADVERB ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
            IF (ERRNUM.NE.0) GO TO 980
            IF (BRANCH.EQ.2) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (6)
               POTERR = 46
               GO TO 980
               END IF
         ELSE
            I = 8
            IF (BRANCH.EQ.2) I = 16
            CALL H2CHR (I, 1, CH(STACK(SP+3)), ITASK)
            END IF
         END IF
C                                       alphabetize?
      CALL ADVERB ('DOALPHA', 'I', 1, 1, ALPHA, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980

C                                       Skip on AIPSC
      IF ((TSKNAM(:5).EQ.'AIPSC') .AND. (BRANCH.GT.1) .AND.
     *   (BRANCH.LE.5)) GO TO 999
      IF (BRANCH.LT.7) THEN
C                                       Open required file
         IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
            CALL ZPHFIL ('SG', 1, NLUSER, 0, PHNAME, IERR)
         ELSE
            IF ((BRANCH.EQ.5) .OR. (BRANCH.EQ.6)) THEN
               CALL ADVERB ('VNUMBER', 'I', 1, 1, IVER, RDUM, CDUM)
               IF (ERRNUM.NE.0) GO TO 980
               IF ((IVER.LE.0) .OR. (IVER.GT.35)) IVER = 1
            ELSE
               IVER = 0
               END IF
            IUSER = 100 * NBLOCK
            IF ((NPOPS.GT.NINTRN) .OR. (TSKNAM(:5).EQ.'AIPSC')) THEN
               IUSER = IUSER + NPOPS
C                                       Check for old name and rename
            ELSE IF (BRANCH.LT.4) THEN
               CALL ZPHFIL ('TS', 1, NLUSER, NBLOCK, OLNAME, IERR)
               CALL ZEXIST (1, OLNAME, I, IERR)
               IF (IERR.EQ.0) THEN
                  CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
                  CALL ZEXIST (1, PHNAME, I, IERR)
                  IF (IERR.EQ.1) CALL ZRENAM (1, OLNAME, PHNAME, IERR)
                  END IF
               END IF
            CALL ZPHFIL ('TG', 1, IUSER, IVER, PHNAME, IERR)
            CALL ZEXIST (1, PHNAME, I, IERR)
            IF (IERR.EQ.1) THEN
               CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
               OLDTG = .TRUE.
            ELSE
               OLDTG = .FALSE.
               END IF
            END IF
         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
               WRITE (MSGTXT,1020) 'SG', 0, IERR
            ELSE
               WRITE (MSGTXT,1020) 'TG', IVER, IERR
               END IF
            CALL MSGWRT (6)
            POTERR = 101
            FIND = 0
            GO TO 980
            END IF
C                                       Read record 1
         POTERR = 50
         CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
            NTASK = IBLK(1)
         ELSE
            NTASK = IBLK(2)
            END IF
         END IF
C                                       Branch to OP
      GO TO (100, 200, 300, 400, 300, 100, 700, 800, 900), BRANCH
C-----------------------------------------------------------------------
C                                       TGET
C                                       get task parameters back
C-----------------------------------------------------------------------
C                                       open inputs file
 100  CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
      POTERR = 101
C                                       Allow min match
C                                       Return true value
      CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
      IF (ERRNUM.NE.0) GO TO 980
      CALL VERMAT (1, PHNAME, ITASK, LVERSN, VERTYP, AVERSN, IERROR)
      IF (IERROR(1).NE.0) GO TO 980
C                                      Set TASK adverb
      CALL ADVRBS ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ZTOPEN (LUNIN, FINDIN, 1, PHNAME, ITASK, AVERSN, T, IERROR)
      POTERR = 31
      IF (IERROR(1).NE.0) THEN
         FINDIN = 0
         GO TO 980
         END IF
C                                       Locate Task in TG file
      IF (BRANCH.NE.9) THEN
         POTERR = 50
 103     LREC = 0
         NWPL = 5
         NLPR = 256 / NWPL
         DO 110 I = 1,NTASK
            IREC = I / NLPR + 1
            IF (IREC.NE.LREC) THEN
               CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               LREC = IREC
               IF (IREC.EQ.1) NBLOCK = MAX (4, IBLK(5))
               END IF
            IOFF = MOD (I, NLPR) * NWPL + 1
            CALL H2CHR (8, 1, IBLK(IOFF), CTEST)
            ILVER = IBLK(IOFF+4)
            IF (ITASK.EQ.CTEST) GO TO 115
 110        CONTINUE
C                                       switch to old format
         IF ((.NOT.OLDTG) .AND. ((BRANCH.EQ.1) .OR. (BRANCH.EQ.6))) THEN
            CALL ZCLOSE (LUN, FIND, IERR)
            OLDTG = .TRUE.
            CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
            CALL ZEXIST (1, PHNAME, I, IERR)
            IF (IERR.EQ.0) THEN
               CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
               IF (IERR.EQ.0) THEN
                  POTERR = 50
                  CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 980
                  IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
                     NTASK = IBLK(1)
                  ELSE
                     NTASK = IBLK(2)
                     END IF
                  GO TO 103
               ELSE
                  WRITE (MSGTXT,1020) 'TG', IVER, IERR
                  CALL MSGWRT (6)
                  POTERR = 101
                  FIND = 0
                  GO TO 980
                  END IF
            ELSE
               FIND = 0
               END IF
            END IF
C                                       task not found
         POTERR = 31
         GO TO 980
         END IF
C                                       Read first data block
 115  IF (BRANCH.NE.9) THEN
         IF (VERTYP.EQ.8) WRITE (MSGTXT,1115)
         IF (VERTYP.NE.ILVER) WRITE (MSGTXT,1116)
         IF ((VERTYP.EQ.8) .OR. (ILVER.NE.VERTYP)) CALL MSGWRT (6)
         JOFF = NBLOCK * (I-1) + 11
         IF (OLDTG) JOFF = 4 * (I-1) + 7
      ELSE
         JOFF = 1
         END IF
      CALL ZFIO ('READ', LUN, FIND, JOFF, IBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Init pointer
      IPTR = 11
C                                       Old method
      IF (OLDTG) THEN
C                                       skip first 2 records
 116     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         POTERR = 59
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 116
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
 117     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.EQ.IEOF) GO TO 150
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 117
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
C                                       loop for adverbs
 120     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.NE.IEOF) THEN
            POTERR = 59
            IF (IERR.NE.0) GO TO 980
            IF (JBUF(1:1).EQ.';') GO TO 120
            IF (JBUF(1:8).EQ.'--------') GO TO 150
            I = JTRIM (JBUF)
            IF (JBUF(COLSHO:COLSHO).EQ.'?') GO TO 120
            IF (JBUF(COLSHO:COLSHO).EQ.'%') GO TO 120
            IF (JBUF(COLSHO:COLSHO).EQ.'@') GO TO 120
            JBUF(COLSHO:COLSHO) = ' '
            KARBUF = JBUF
            KBPTR = NMATCC (1, ' ', KARBUF)
C                                       comment
            IF (KBPTR.GE.COLLBN) GO TO 120
C                                       adverb
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            ITAG = TAG
            ILOC = LOCSYM
            ITYPE = SYTYPE
C                                       Type 7 is a string.
            POTERR = 8
C                                       Type 1 is a scalar, 2=array.
            IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2) .AND. (SYTYPE.NE.7))
     *         GO TO 980
            ISIZE = 1
            IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
            DO 140 I = 1,ISIZE
               IF (ITYPE.EQ.7) THEN
                  CH(ITAG) = HARBUF(IPTR)
               ELSE
                  C(ITAG) = PARBUF(IPTR)
                  END IF
               ITAG = ITAG + 1
               IPTR = IPTR + 1
               IF (IPTR.GT.256) THEN
                  IPTR = 1
                  JOFF = JOFF + 1
                  CALL ZFIO ('READ', LUN, FIND, JOFF, IBLK, IERR)
                  IF (IERR.NE.0) THEN
                     POTERR = 50
                     GO TO 980
                     END IF
                  END IF
 140           CONTINUE
C                                       Check character strings
            IF (ITYPE.EQ.7) THEN
               CALL VERSTR (ILOC)
               IF (ERRNUM.NE.0) GO TO 980
               END IF
            GO TO 120
            END IF
C                                       Done !
 150     POTERR = 0
         GO TO 980
C                                       new method
      ELSE
         NNAMES = 0
C                                       get adverb and type
 160     IF (IBLK(IPTR).NE.0) THEN
            DO 165 I = 1,3
               HADNAM(I) = HARBUF(IPTR)
               IPTR = IPTR + 1
               IF (IPTR.GT.256) THEN
                  IPTR = 1
                  JOFF = JOFF + 1
                  CALL ZFIO ('READ', LUN, FIND, JOFF, IBLK, IERR)
                  IF (IERR.NE.0) THEN
                     POTERR = 50
                     GO TO 980
                     END IF
                  END IF
 165           CONTINUE
            CALL H2CHR (8, 1, HADNAM, KARBUF)
            JTYPE = MOD (NAMTYP, 10)
            JSIZE = NAMTYP / 10
            NNAMES = NNAMES + 1
            NAMES(NNAMES) = KARBUF(:8)
            KBPTR = 1
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            ITAG = TAG
            ILOC = LOCSYM
            ITYPE = SYTYPE
C                                       Type 7 is a string.
            POTERR = 8
C                                       Type 1 is a scalar, 2=array.
            IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2) .AND. (SYTYPE.NE.7))
     *         GO TO 980
            ISIZE = 1
            IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
            IF ((JSIZE.NE.ISIZE) .OR. (JTYPE.NE.ITYPE)) THEN
               WRITE (MSGTXT,1165) KARBUF(:8), JSIZE, JTYPE, ISIZE,
     *            ITYPE
               CALL MSGWRT (7)
               END IF
            DO 170 I = 1,JSIZE
               IF (I.LE.ISIZE) THEN
                  IF (JTYPE.EQ.7) THEN
                     CH(ITAG) = HARBUF(IPTR)
                  ELSE
                     C(ITAG) = PARBUF(IPTR)
                     END IF
                  END IF
               ITAG = ITAG + 1
               IPTR = IPTR + 1
               IF (IPTR.GT.256) THEN
                  IPTR = 1
                  JOFF = JOFF + 1
                  CALL ZFIO ('READ', LUN, FIND, JOFF, IBLK, IERR)
                  IF (IERR.NE.0) THEN
                     POTERR = 50
                     GO TO 980
                     END IF
                  END IF
 170           CONTINUE
C                                       Check character strings
            IF (JTYPE.EQ.7) THEN
               CALL VERSTR (ILOC)
               IF (ERRNUM.NE.0) GO TO 980
               END IF
            GO TO 160
            END IF
C                                       Now check against the INPUTS
C                                       skip first 2 records
 171     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         POTERR = 59
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 171
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
 172     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.EQ.IEOF) GO TO 185
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 172
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
C                                       loop for adverbs
 175     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.NE.IEOF) THEN
            POTERR = 59
            IF (IERR.NE.0) GO TO 980
            IF (JBUF(1:1).EQ.';') GO TO 175
            IF (JBUF(1:8).EQ.'--------') GO TO 185
            I = JTRIM (JBUF)
            IF (JBUF(COLSHO:COLSHO).EQ.'?') GO TO 175
            IF (JBUF(COLSHO:COLSHO).EQ.'%') GO TO 175
            IF (JBUF(COLSHO:COLSHO).EQ.'@') GO TO 175
            JBUF(COLSHO:COLSHO) = ' '
            KARBUF = JBUF
            KBPTR = NMATCC (1, ' ', KARBUF)
C                                       comment
            IF (KBPTR.GE.COLLBN) GO TO 175
            DO 180 I = 1,NNAMES
               IF (KARBUF(:8).EQ.NAMES(I)) THEN
                  NAMES(I) = '--------'
                  GO TO 175
                  END IF
 180           CONTINUE
            WRITE (MSGTXT,1180) KARBUF(:8)
            CALL MSGWRT (6)
            GO TO 175
            END IF
C                                       any left"
 185     DO 190 I = 1,NNAMES
            IF ((NAMES(I).NE.' ') .AND. (NAMES(I).NE.'--------')) THEN
               WRITE (MSGTXT,1185) NAMES(I)(:8)
               CALL MSGWRT (6)
               END IF
 190        CONTINUE
         POTERR = 0
         GO TO 980
         END IF
C-----------------------------------------------------------------------
C                                       SGDESTR  < name >
C                                       destroy 1 Save/Get file
C-----------------------------------------------------------------------
C                                       Locate the file
 200  LREC = 1
      POTERR = 50
      NWPL = 7
      NLPR = 256 / NWPL
      IF (NTASK.LE.0) GO TO 225
      DO 220 I = 1,NTASK
         IREC = I/NLPR + 1
         IF (IREC.EQ.LREC) GO TO 210
            LREC = IREC
            CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
            IF (IERR.NE.0) GO TO 980
 210     IOFF = MOD (I, NLPR) * NWPL + 1
         IF (IBLK(IOFF).LE.0) GO TO 220
            CALL H2CHR (16, 1, IBLK(IOFF+3), ANAME)
            IF (ANAME.EQ.ITASK) GO TO 230
 220     CONTINUE
 225  POTERR = 101
      WRITE (MSGTXT,1220) ITASK
      CALL MSGWRT (6)
      WRITE (MSGTXT,1000)
      CALL MSGWRT (6)
      GO TO 980
C                                       Found it: do destroy
 230  CALL ZPHFIL ('SG', 1, NLUSER, I, PHNAME, IERR)
      CALL ZDESTR (1, PHNAME, IERR)
      POTERR = 46
      IF (IERR.GT.1) GO TO 980
C                                       Clear entry
      IBLK(IOFF) = 0
      POTERR = 50
      IF (LREC.EQ.1) GO TO 240
         CALL ZFIO ('WRIT', LUN, FIND, LREC, IBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Update control parms
 240  IF (I.LT.NTASK) IBLK(2) = IBLK(2) + 1
      IF (I.EQ.NTASK) IBLK(1) = IBLK(1) - 1
      CALL ZFIO ('WRIT', LUN, FIND, 1, IBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Empty: delete directory
      POTERR = 0
      IF (IBLK(1).GT.IBLK(2)) GO TO 980
         CALL ZCLOSE (LUN, FIND, IERR)
         FIND = 0
         CALL ZPHFIL ('SG', 1, NLUSER, 0, PHNAME, IERR)
         CALL ZDESTR (1, PHNAME, IERR)
         IF (IERR.LE.1) GO TO 980
            WRITE (MSGTXT,1240) IERR
            CALL MSGWRT (6)
            GO TO 980
C-----------------------------------------------------------------------
C                                       TGINDEX
C                                       index tasks w saved parms
C-----------------------------------------------------------------------
 300  NNAMES = 0
      IF (IVER.EQ.0) THEN
         MSGTXT = 'Index of TGET/TPUT file'
      ELSE
         WRITE (MSGTXT,1300) IVER
         END IF
      CALL MSGWRT (2)
 305  LREC = 1
      POTERR = 50
      IF (NTASK.GT.0) THEN
         IF (NTASK.GT.NA) THEN
            WRITE (MSGTXT,1310) NTASK, NA
            CALL MSGWRT (6)
            NTASK = NA
            END IF
         NWPL = 5
         NLPR = 256 / NWPL
         JJ = 0
         DO 310 I = 1,NTASK
            IREC = I / NLPR + 1
            IF (IREC.NE.LREC) THEN
               CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               LREC = IREC
               END IF
            IOFF = MOD (I, NLPR) * NWPL
            CALL H2CHR (8, 1, IBLK(IOFF+1), CTEST)
            JJ = JJ + 1
            NAMES(JJ) = CTEST
            CALL CATIME (2, IBLK(IOFF+3), ITIME(1,JJ))
            CALL DAT2JD (ITIME(1,JJ), JDS(JJ))
            IVERS(JJ) = IBLK(IOFF+5)
            IF (.NOT.OLDTG) THEN
               NNAMES = NNAMES + 1
               XNAMES(NNAMES) = NAMES(JJ)
            ELSE
               DO 308 J = 1,NNAMES
                  IF (NAMES(JJ).EQ.XNAMES(J)) THEN
                     JJ = JJ - 1
                     GO TO 310
                     END IF
 308              CONTINUE
               END IF
 310        CONTINUE
         NTASK = JJ
         IF (NTASK.GT.0) THEN
            IF (OLDTG) THEN
               WRITE (MSGTXT,1315) 'Old'
            ELSE
               WRITE (MSGTXT,1315) 'New'
               END IF
            CALL MSGWRT (3)
            WRITE (MSGTXT,1316)
            CALL MSGWRT (3)
            END IF
         DO 340 I = 1,NTASK
            JJ = I
C                                       sort by date
            IF (ALPHA.EQ.0) THEN
               JDM = 0.0D0
               DO 320 J = 1,NTASK
                  IF (JDS(J).GT.JDM) THEN
                     JDM = JDS(J)
                     JJ = J
                     END IF
 320              CONTINUE
C                                       sort alphabetic
            ELSE IF (ALPHA.GT.0) THEN
               ANAME = 'ZZZZZZZZZZZZZZZZ'
               DO 330 J = 1,NTASK
                  IF (NAMES(J).NE.' ') THEN
                     IF (NAMES(J).LT.ANAME) THEN
                        JJ = J
                        ANAME = NAMES(J)
                        END IF
                     END IF
 330              CONTINUE
               END IF
            CTEST = NAMES(JJ)
            ILVER = IVERS(JJ)
            CALL TIMDAT (ITIME(4,JJ), ITIME(1,JJ), ATIME, ADATE)
            WRITE (MSGTXT,1330) CTEST, ADATE, ATIME, VERVER(ILVER)
            CALL MSGWRT (2)
            JDS(JJ) = -1.0D0
            NAMES(JJ) = ' '
 340        CONTINUE
         END IF
      IF (.NOT.OLDTG) THEN
         CALL ZCLOSE (LUN, FIND, IERR)
         CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
         OLDTG = .TRUE.
         FIND = 0
         CALL ZEXIST (1, PHNAME, I, IERR)
         IF (IERR.EQ.0) THEN
            CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) 'TG', IVER, IERR
               CALL MSGWRT (6)
               POTERR = 101
               FIND = 0
               GO TO 980
            ELSE
               POTERR = 50
               CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
                  NTASK = IBLK(1)
               ELSE
                  NTASK = IBLK(2)
                  END IF
               GO TO 305
               END IF
            END IF
         END IF
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       SGINDEX
C                                       index Save/Get files
C-----------------------------------------------------------------------
 400  LREC = 1
      POTERR = 50
      NWPL = 7
      NLPR = 256 / NWPL
      IF (NTASK.GT.0) THEN
         JJ = 0
         DO 410 I = 1,NTASK
            IREC = I / NLPR + 1
            IF (IREC.NE.LREC) THEN
               CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               LREC = IREC
               END IF
            IOFF = MOD (I, NLPR) * NWPL + 1
            IF (IBLK(IOFF).GT.0) THEN
               IF (JJ.GE.NA) THEN
                  WRITE (MSGTXT,1410) NA
                  CALL MSGWRT (6)
                  GO TO 415
                  END IF
               JJ = JJ + 1
               IVERS(JJ) = IBLK(IOFF) / 32
               CALL CATIME (2, IBLK(IOFF+1), ITIME(1,JJ))
               CALL DAT2JD (ITIME(1,JJ), JDS(JJ))
               CALL H2CHR (16, 1, IBLK(IOFF+3), NAMES(JJ))
               END IF
 410        CONTINUE
 415     NTASK = JJ
         END IF
      IF (NTASK.GT.0) THEN
         WRITE (MSGTXT,1415)
         CALL MSGWRT (3)
         DO 440 I = 1,NTASK
            JJ = I
C                                       sort by date
            IF (ALPHA.EQ.0) THEN
               JDM = 0.0D0
               DO 420 J = 1,NTASK
                  IF (JDS(J).GT.JDM) THEN
                     JDM = JDS(J)
                     JJ = J
                     END IF
 420              CONTINUE
C                                       sort alphabetic
            ELSE IF (ALPHA.GT.0) THEN
               ANAME = 'ZZZZZZZZZZZZZZZZ'
               DO 430 J = 1,NTASK
                  IF (NAMES(J).NE.' ') THEN
                     IF (NAMES(J).LT.ANAME) THEN
                        JJ = J
                        ANAME = NAMES(J)
                        END IF
                     END IF
 430              CONTINUE
               END IF
            CALL TIMDAT (ITIME(4,JJ), ITIME(1,JJ), ATIME, ADATE)
            ILVER = IVERS(JJ)
            ANAME = NAMES(JJ)
            IF (ILVER.EQ.SGVERS) ITASK = 'CURRENT'
            IF (ILVER.LT.SGVERS) ITASK = 'OUT DATED'
            IF (ILVER.GT.SGVERS) ITASK = 'TOO NEW'
            WRITE (MSGTXT,1430) ANAME, ADATE, ATIME, ITASK
            CALL MSGWRT (3)
            JDS(JJ) = -1.0D0
            NAMES(JJ) = ' '
 440        CONTINUE
         END IF
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       DEFAULT
C                                       reset task parameters
C-----------------------------------------------------------------------
C                                       open inputs file
 700  CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
      POTERR = 101
C                                       Allow min match
C                                       Return true value
      CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
      IF (ERRNUM.NE.0) GO TO 980
      CALL VERMAT (1, PHNAME, ITASK, LVERSN, VERTYP, AVERSN, IERROR)
      IF (IERROR(1).NE.0) GO TO 980
C                                      Set TASK adverb
      CALL ADVRBS ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ZTOPEN (LUNIN, FINDIN, 1, PHNAME, ITASK, AVERSN, T, IERROR)
      POTERR = 31
      IF (IERROR(1).NE.0) THEN
         FINDIN = 0
         GO TO 980
         END IF
C                                       skip first 2 records
 705  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      POTERR = 59
      IF (IERR.NE.0) GO TO 980
      IF (JBUF(1:1).EQ.';') GO TO 705
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 980
         END IF
 710  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      IF (IERR.EQ.IEOF) GO TO 750
      IF (IERR.NE.0) GO TO 980
      IF (JBUF(1:1).EQ.';') GO TO 710
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 980
         END IF
C                                       loop for adverbs
 720  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.EQ.IEOF) GO TO 750
         POTERR = 59
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 720
         IF (JBUF(1:8).EQ.'--------') GO TO 750
         I = JTRIM (JBUF)
         IF (JBUF(COLSHO:COLSHO).EQ.'?') GO TO 720
         JBUF(COLSHO:COLSHO) = ' '
         KARBUF = JBUF
         KBPTR = NMATCC (1, ' ', KARBUF)
C                                       coment
         IF (KBPTR.GE.COLLBN) GO TO 720
C                                       adverb
         CTEST = JBUF(:8)
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         ITAG = TAG
         ILOC = LOCSYM
         ITYPE = SYTYPE
         IF (ITYPE.EQ.7) THEN
            HVALU = HBLANK
         ELSE
            RVALU = -1.0
            DO 725 I = 1,NADM1
               IF (CTEST.EQ.ADVM1(I)) GO TO 740
 725           CONTINUE
            RVALU = 1.0
            DO 730 I = 1,NADP1
               IF (CTEST.EQ.ADVP1(I)) GO TO 740
 730           CONTINUE
            DO 735 I = 1,NADSP
               RVALU = VADVSP(I)
               IF (CTEST.EQ.ADVSP(I)) GO TO 740
 735           CONTINUE
            RVALU = 0.0
            END IF
C                                       Type 7 is a string.
 740     POTERR = 8
C                                       Type 1 is a scalar, 2=array.
         IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2) .AND. (SYTYPE.NE.7))
     *      GO TO 980
         ISIZE = 1
         IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
         DO 745 I = 1,ISIZE
            IF (ITYPE.EQ.7) THEN
               CH(ITAG) = HVALU
            ELSE
               C(ITAG) = RVALU
               END IF
            ITAG = ITAG + 1
 745        CONTINUE
         ITAG = ITAG - ISIZE
C                                       Check character strings
         IF (ITYPE.EQ.7) THEN
            IF (CTEST.EQ.'OPTELL') CALL CHR2H (4, 'CHAN', 1, CH(ITAG))
            IF (CTEST.EQ.'BANDPOL') CALL CHR2H (8, '*(RL)', 1, CH(ITAG))
            CALL VERSTR (ILOC)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
         GO TO 720
C                                       Done !
 750  POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       SG2RUN
C-----------------------------------------------------------------------
 800  CALL ADVERB ('OUTFILE', 'C', 1, 48, IDUM, RDUM, PHNAME)
      IF (ERRNUM.NE.0) GO TO 980
      LUNDE = 10
      CALL K2TEXT (LUNDE, .FALSE., PHNAME)
      GO TO 980
C-----------------------------------------------------------------------
C                                       PLGET
C-----------------------------------------------------------------------
C                                       Find start of real*4
C                                       parms in graph file.
 900  POTERR = 101
C                                       Create name string.
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, NAME)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      TYPE = ' '
      IUSER = NLUSER
C                                       Open map file, get header.
      CALL MAPOPN ('READ', IVOL, NAME, CLASS, ISEQ, TYPE, IUSER,
     *   LUN, FIND, ISLOT, CATBLK, IDATA, IERR)
      IF (IERR.GT.1) GO TO 980
C                                       Close map file.
      CALL MAPCLS ('READ', IVOL, ISLOT, LUN, FIND, CATBLK, F, IDATA,
     *   IERR)
      CALL FNDEXT ('PL', CATBLK, IMAXV)
      IF (IMAXV.LE.0) THEN
         MSGTXT = 'NO PLOT FILES IN THIS IMAGE/UV FILE'
         CALL MSGWRT (8)
         GO TO 980
         END IF
      CALL ADVERB ('PLVER', 'I', 1, 0, IVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((IVER.LT.1) .OR. (IVER.GT.IMAXV)) IVER = 1
      CALL OPEXT ('PL', IVOL, ISLOT, IVER, LUN, F, F, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) 'PL', IVER, IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      I = 1
      CALL ZFIO ('READ', LUN, FIND, I, IDATA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, I
         CALL MSGWRT (8)
         GO TO 980
         END IF
      NPARMS = IDATA(10)
      I = (NPARMS + 9) / 256 + 2
C                                       first record of plot commands
      CALL ZFIO ('READ', LUN, FIND, I, IDATA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, I
         CALL MSGWRT (8)
         GO TO 980
         END IF
      IPTYPE = IDATA(6)
      ITASK = ' '
      IF ((IPTYPE.GE.1) .AND. (IPTYPE.LE.NPLOTS)) ITASK = PNAMES(IPTYPE)
      IF (ITASK.EQ.' ') THEN
         WRITE (MSGTXT,1910) IPTYPE
         CALL MSGWRT (8)
         GO TO 980
         END IF
      IF (NPARMS.LT.PNPARM(IPTYPE)) THEN
         MSGTXT = 'WARNING: PLOT MAY BE FROM OLDER VERSION OF ' // ITASK
         CALL MSGWRT (6)
         END IF
      POTERR = 0
      GO TO 100
C-----------------------------------------------------------------------
C                                       POPS errors
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C                                       Close downs
      IF (FINDIN.GT.0) CALL ZTCLOS (LUNIN, FINDIN, IERR)
      IF (FIND.GT.0) CALL ZCLOSE (LUN, FIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SGDESTR REQUIRES A FULL NAME: NO DEFAULTS AND NO ',
     *   'MIN-MATCH')
 1020 FORMAT ('YOU SEEM TO HAVE NO FILES OF TYPE ',A2,' VERSION',I3,
     *   ' OPEN ERR',I7)
 1115 FORMAT ('Warning: private versions being used - check adverbs',
     *   ' closely')
 1116 FORMAT ('VERSION ADVERB DOES NOT MATCH THAT',
     *   ' USED WHEN ADVERBS STORED')
 1165 FORMAT ('ADVERB ',A,' SIZE, TYPE MISMATCH',2(I6,I4))
 1180 FORMAT ('Adverb ',A,' is in INPUTS but not TGET file')
 1185 FORMAT ('Adverb ',A,' was in TGET but not INPUTS file')
 1220 FORMAT ('SG FILE ',A16,' NOT FOUND')
 1240 FORMAT ('FAILED TO DESTROY EMPTY DIRECTORY',I7)
 1300 FORMAT ('Index of VGET/VPUT file version',I3)
 1310 FORMAT ('LIST OF',I6,' NAMES LIMITED TO FIRST',I4)
 1315 FORMAT ('Reading ',A,' format TGET file')
 1316 FORMAT (3X,'Name',11X,'Last GO time',10X,'Version')
 1330 FORMAT (3X,A8,3X,A12,1X,A8,5X,A8)
 1410 FORMAT ('LIST LIMITED TO FIRST',I4,' NAMES')
 1415 FORMAT (3X,'SAVE name',14X,'Last SAVE time',9X,'version')
 1430 FORMAT (2X,'''',A16,'''',3X,A12,1X,A8,5X,A9)
 1900 FORMAT ('ERROR',I4,' READING PL FILE RECORD',I4)
 1910 FORMAT ('PLOT TYPE',I4,' NOT SUPPORTED BY PLGET')
      END

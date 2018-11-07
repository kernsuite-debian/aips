LOCAL INCLUDE 'PUVGIT.INC'
C                                       Local include for UVGIT
      INTEGER   XMXVIS, XMXPRM, XMXCMP
C                                       XMXVIS = max. no. vis records.
      PARAMETER (XMXVIS=2500000)
C                                       XMXCMP = max. no sources
      PARAMETER (XMXCMP=60)
C                                       XMXPRM = max. no parameters.
      PARAMETER (XMXPRM=6*XMXCMP+30)
LOCAL END
LOCAL INCLUDE 'UVGIT.INC'
C                                       Local include for UVGIT
      INCLUDE 'PUVGIT.INC'
C
      INTEGER   SEQIN, DISKIN, NGAUSS, NITER, CCVER, CHAN, NPR,
     *   CNO
      LOGICAL   DOAMP, DOREAL
      INTEGER   NUMVIS, MAXVIS, NANT, SCRTCH(512)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSOLM, XOPCOD,
     *   XXSTOK, XINLIS(12), XFITOU(12)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSOLMO*4, OPCODE*4,
     *   XSTOK*4, INLIST*48, FITOUT*48, COMNTS(XMXCMP)*256
      REAL      XSIN, XDISIN, XBCHAN, XECHAN, XBIF, XEIF, XBAND, XFREQ,
     *   XFQID, XANTS(50), XSUBA, XTIME(8), XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XUVRA(2),
     *   XNGAU, DMAX(4), DPOS(2,4), DWIDTH(3,4), GAINE(30), XNIT, EDROP,
     *   DDMAX(4), DDPOS(2,4), DDWID(3,4), SIZE(2), PRTL, DOCAT, XNVER,
     *   TLOW, THIGH, BADD(10)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XBCHAN, XECHAN, XBIF, XEIF, XXSTOK, XBAND, XFREQ, XFQID,
     *   XXSOUR, XANTS, XSUBA, XTIME, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XUVRA, XXSOLM, XOPCOD,
     *   XNGAU, DMAX, DPOS, DWIDTH, GAINE, XNIT, EDROP, DDMAX, DDPOS,
     *   DDWID, XINLIS, SIZE, PRTL, DOCAT, XNVER, XFITOU, BADD
      COMMON /INFO/ SCRTCH, NUMVIS, MAXVIS, NANT, DOAMP, DOREAL, SEQIN,
     *   DISKIN, NGAUSS, NITER, CCVER, CHAN, CNO, NPR, TLOW, THIGH
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XSOLMO, OPCODE, XSTOK,
     *   INLIST, FITOUT, COMNTS
LOCAL END
LOCAL INCLUDE 'DUVGIT.INC'
      INCLUDE 'PUVGIT.INC'
      DOUBLE PRECISION LPARMS(XMXPRM), CHISQ, RMSRES
      INTEGER   NVAR, MODPNT(6,XMXCMP), GAPNT(30), IANT1(XMXVIS),
     *   IANT2(XMXVIS), IVAR(XMXPRM), JVAR(XMXPRM), ITTER, NUMDAT, MINC
      REAL      GAIN(30), U(XMXVIS), V(XMXVIS), W(XMXVIS), RE(XMXVIS),
     *   IM(XMXVIS), WT(XMXVIS), GPARMS(6,XMXCMP), DOCOMP(6,XMXCMP)
      COMMON /UVDATA/ LPARMS, CHISQ, RMSRES, GAIN, U, V, W, RE, IM, WT,
     *   GPARMS, DOCOMP, MODPNT, GAPNT, IANT1, IANT2, IVAR, JVAR, NVAR,
     *   ITTER, NUMDAT, MINC
LOCAL END
      PROGRAM UVGIT
C-----------------------------------------------------------------------
C! Fits source models to uv data.
C# UV Modeling Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2016, 2018
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
C   UVGIT fits a model consisting of a collection of elliptical
C   gaussians or uniform optical spheres to uv data.
C   The results can optionally be written as a CLEAN components file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      BCHAN          BCHAN         1st channel number
C      ECHAN          ECHAN         Highest channel number
C      BIF            BIF           1st IF number
C      EIF            EIF           Highest IF number
C      SOURCES        SOURCS        Source name.
C      ANTENNAS       ANTENS        Antenna list
C      SUBARRAY       SUBA          Subarray.
C      TIMERANG       TIMER         Timerange
C      DOCALIB        DOCAL         Apply calibration?
C      GAINUSE        CLUSE         CL table to apply
C      FLAGVER        FLGVER        FG table to apply
C      DOBAND                       Apply bandpass calibration?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      UVRANGE(2)     UVRA          Range of baseline lengths to
C                                   use (kilowavelengths).
C      SOLMODE        SOLMOD(?)     Amp. or  Amp and phase, or real.
C      OPCODE         OPCODE        'GAUS','SPHE' => model
C      NGAUSS         NGAUSS        Number of comp. to fit
C      GMAX(4)        GMAX          Peak intensity
C      GPOS(2,4)      GPOS          (X,Y) Position in arcsec.
C      GPARMS(6,4)    GWIDTH        (BMAJ,BMIN,BPA) in asec, asec,
C                                     degrees
C      GAINERR        GAIN          Antenna gain (<0 => fix to abs,
C                                   0 => 1
C      NITER          NITER         Number of loop iterations
C      EDROP          EDROP         Goodness of fit cutoff
C      DOMAX(4)       DOMAX         Peak intensity variability
C      DOPOS(2,4)     DOPOS         (X,Y) Position variability
C      DOWIDTH(3,4)   DOWID         (BMAJ,BMIN,BPA) variability
C      IMSIZE(2)      SIZE          Minimum and maximum component
C                                   size in asec.
C      PRTLEV         NPR           Print level from fitting
C      DOCAT          DOCAT         0=>Don't write CC.
C                                   1=>Deconvolved components
C                                   2=>Fit components
C      INVER          CCVER         Output CC file ver no.
C                                    0=>create new.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVGIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'UVGIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL UVFTIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read data
      CALL UVFDAT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Fit model.
      CALL FITMOD (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE UVFTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVFTIN gets input parameters for UVGIT and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6   Program name
C   Output:
C      JERR    I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR, IROUND, NPARM, IERR, LOOP, LUN, I, JTRIM
      REAL      CATR(256)
      LOGICAL   T, F, TABLE, EXIST, FITASC, OK, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVGIT.INC'
      INCLUDE 'DUVGIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /25/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      TLOW = 1.E10
      THIGH = -1.E10
C                                       Set max. number vis.
      MAXVIS = XMXVIS
C                                       Get input parameters.
      NPARM = 329
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRTCH, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      NGAUSS = IROUND (XNGAU)
      NITER = IROUND (XNIT)
      CCVER = IROUND (XNVER)
      NPR = IROUND (PRTL)
C                                       Characters
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSOLM, XSOLMO)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (48, 1, XINLIS, INLIST)
      CALL H2CHR (48, 1, XFITOU, FITOUT)
      I = JTRIM (INLIST)
      I = JTRIM (FITOUT)
      DO 20 I = 1,30
        CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20     CONTINUE
C                                       Find input file.
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, SCRTCH, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Set status (WRIT => CC)
      STAT = 'REST'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, STAT, SCRTCH, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       See if multisource table
      CALL ISTAB ('SU', DISKIN, CNO, 1, LUN, SCRTCH, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST .AND. (IERR.EQ.0) .AND.
     *    (XSOUR(1).EQ.'                ')) THEN
C                                       Must name a source for
C                                       multisource file.
         WRITE (MSGTXT,1050)
         JERR = 6
         GO TO 990
         END IF
      IF (EXIST .AND. (IERR.EQ.0)) THEN
C                                       Give source name for multi
         WRITE (MSGTXT,1051) XSOUR(1)
         CALL MSGWRT (6)
         END IF
C                                       Setup for UVGET
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Only 1 source allowed.
      SOURCS(1) = XSOUR(1)
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      IF (XSTOK.EQ.'    ') XSTOK = 'I   '
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      XBCHAN = BCHAN
C                                       get initial guess
      CALL INIDAT (BCHAN, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'FAILED TO SET UP INITIAL GUESSES'
         GO TO 990
         END IF
C                                       Check defaults, limits
      IF (NITER.LE.0) NITER = 100 * NGAUSS
      IF (EDROP.LE.0) EDROP = 1.0E-10
C                                       IFs
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      DO 100 LOOP = 1,50
         ANTENS(LOOP) = IROUND (XANTS(LOOP))
 100     CONTINUE
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      BLVER = -1
      DXTIME = 0.0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Get # antennas
      CALL GETANT (DISKIN, CNO, SUBARR, CATBLK, SCRTCH, JERR)
      IF (JERR.GT.0) GO TO 999
      NANT = NSTNS
C
      DOAMP = XSOLMO.EQ.'A   '
      DOREAL = XSOLMO.EQ.'RE  '
      IF (DOAMP .OR. DOREAL) THEN
C                                       Make sure one posn. fixed
         OK = F
         DO 150 LOOP = 1,NGAUSS
            OK = OK .OR. ((DOCOMP(2,LOOP).LE.0.0) .AND.
     *         (DOCOMP(3,LOOP).LE.0.0))
 150        CONTINUE
C                                       Must fix one posn.
         IF (.NOT.OK) THEN
            JERR = 7
            IF (DOAMP) WRITE (MSGTXT,1150)
            IF (DOREAL) WRITE (MSGTXT,1160)
            GO TO 990
            END IF
         END IF
C                                       Tell Stokes' type
      MSGTXT = 'Processing Stokes type ' // XSTOK
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR: You MUST specify a SOURCE for multisource files')
 1051 FORMAT ('Fitting to source = ',4A4)
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1150 FORMAT ('ERROR: You MUST fix at least one posn. for amp. only ',
     *   'fitting')
 1160 FORMAT ('ERROR: You MUST fix at least one posn. for real only ',
     *   'fitting')
      END
      SUBROUTINE INIDAT (BCHAN, IRET)
C-----------------------------------------------------------------------
C   INIDAT reads the text file input if any or copies the user adverbs
C   to the fitting parameters
C   Inputs:
C      BCHAN   I   Desired spectral channel
C   Outputs:
C      IRET    I   Error code
C-----------------------------------------------------------------------
      INTEGER   BCHAN, IRET
C
      INCLUDE 'UVGIT.INC'
      INTEGER   TLUN, TIND, LUNTMP, I, JTRIM, J, NC, KBP
      DOUBLE PRECISION X
      CHARACTER INLINE*512
      INCLUDE 'DUVGIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       adverb input
      IF (INLIST.EQ.' ') THEN
         NGAUSS = MAX (1, MIN (4, NGAUSS))
         DO 10 I = 1,NGAUSS
            GPARMS(1,I) = DMAX(I)
            IF (GPARMS(1,I).EQ.0.0) GPARMS(1,I) = 1.0
            GPARMS(2,I) = DPOS(1,I)
            GPARMS(3,I) = DPOS(2,I)
            GPARMS(4,I) = DWIDTH(1,I)
            GPARMS(5,I) = DWIDTH(2,I)
            GPARMS(6,I) = DWIDTH(3,I)
            DOCOMP(1,I) = DDMAX(I)
            DOCOMP(2,I) = DDPOS(1,I)
            DOCOMP(3,I) = DDPOS(2,I)
            DOCOMP(4,I) = DDWID(1,I)
            DOCOMP(5,I) = DDWID(2,I)
            DOCOMP(6,I) = DDWID(3,I)
 10         CONTINUE
         IRET = 0
C                                       text file input
      ELSE
         NGAUSS = 0
         TLUN = LUNTMP (2)
         CALL ZTXOPN ('READ', TLUN, TIND, INLIST, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING TEXT FILE'
            GO TO 990
            END IF
 100     CALL ZTXIO ('READ', TLUN, TIND, INLINE, IRET)
         IF ((IRET.GT.0) .AND. (IRET.NE.2)) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            NC = JTRIM (INLINE)
            IF (NC.LE.0) GO TO 100
            IF (INLINE(:1).EQ.'#') GO TO 100
            KBP = 1
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            I = X + 0.5D0
            IF ((I.NE.BCHAN) .AND. (I.NE.0)) GO TO 100
            J = NGAUSS + 1
            IF (J.GT.XMXCMP) THEN
               WRITE (MSGTXT,1100) XMXCMP
               CALL MSGWRT (7)
               GO TO 110
               END IF
            DOCOMP(1,J) = -1
            DOCOMP(2,J) = -1
            DOCOMP(3,J) = -1
            DOCOMP(4,J) = -1
            DOCOMP(5,J) = -1
            DOCOMP(6,J) = -1
C                                       flux
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(1,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(2,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(3,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(4,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(5,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPARMS(6,J) = X
C                                       count it
            NGAUSS = J
C                                       are we fitting?
C                                       flux
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(1,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(2,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(3,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(4,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(5,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOCOMP(6,J) = X
C                                       comment?
            COMNTS(J) = ' '
            IF (KBP.LT.NC) THEN
               COMNTS(J) = INLINE(KBP:NC)
               NC = JTRIM (COMNTS(J))
               CALL CHTRIM (COMNTS(J), NC, COMNTS(J), I)
               END IF
            GO TO 100
            END IF
C                                       close file
 110     CALL ZTXCLS (TLUN, TIND, I)
         END IF
      IRET = 0
C                                       Error if none
      IF (NGAUSS.LE.0) THEN
         MSGTXT = 'NO GAUSSIANS FOUND'
         IRET = 10
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('INIDAT: ERROR',I4,' ON ',A)
 1100 FORMAT ('INIDAT: USING ONLY FIRST',I3,' COMPONENTS FOUND')
      END
      SUBROUTINE UVFDAT (IRET)
C-----------------------------------------------------------------------
C   UVFDAT reads the uv data and converts to the form expected in the
C   rest of the program. Will read up to MAXVIS visibility records.
C   Input from common:
C      CHAN        I    Channel number
C      CATBLK(256) I    Catalog header
C      MAXVIS      I    Maximum number of visibilities.
C   Output:
C      IRET        I    Return error code: 0=>OK, otherwise error.
C   Output in common:
C      NUMVIS      I    Number of visibilities read.
C      U(*)        R    U coordinates in wavelengths
C      V(*)        R    V coordinates in wavelengths
C      RE(*)       R    Real part (Jy) (Amplitude if DOAMP)
C      IM(*)       R    Imaginary part (Jy)
C      WT(*)       R    weight
C      IANT1(*)    I    first antenna number
C      IANT2(*)    I    second antenna number
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPOINT, IERR, KVIS, LUN, NIF, LOOP, VISCNT, KOOP, IIVER
      REAL      BUFFER(2048), CATR(256), SUMRE, SUMIM, SUMWT, XRE, XIM,
     *   GCOR
      DOUBLE PRECISION CATD(128), SUM, TFREQ, COUNT
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF), IA2
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'UVGIT.INC'
      INCLUDE 'DUVGIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE  (CATBLK, CATR, CATD)
      DATA LUN /29/
C-----------------------------------------------------------------------
      VISCNT = 0
C                                       Frequency correction factor
C                                       Get IF table:
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNO, IIVER, CATBLK, LUN, NIF,
     *   FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
C                                       Find average frequency
      SUM = 0.0D0
      COUNT = 0.0D0
      DO 30 LOOP = BIF,EIF
C                                       Use FINC(LOOP) here rather
C                                       than the header incr.  Would
C                                       FINC(BIF) be safer in case the
C                                       increment chnages ? Probably
C                                       better to get rubbish answers
         TFREQ = FREQ + FOFF(LOOP) + (((BCHAN+ECHAN)/2.0) -
     *      CATR(KRCRP+JLOCF)) * FINC(LOOP)
         SUM = SUM + TFREQ
         COUNT = COUNT + 1.0D0
 30      CONTINUE
      IF (COUNT.GT.1.0D0) SUM = SUM / COUNT
      GCOR = SUM / FREQ
C                                       Open file
      CALL UVGET ('INIT', BUFFER, BUFFER(1+NRPARM), IRET)
      IF (IRET.NE.0) GO TO 999
      KVIS = (LREC - NRPARM) / 3
C                                       Begin loop reading
      IF (NVIS.GT.MAXVIS) THEN
         WRITE (MSGTXT,1030) MAXVIS, NVIS
         CALL MSGWRT (7)
         END IF
      NUMVIS = MIN (NVIS, MAXVIS)
      DO 200 LOOP = 1,NUMVIS
C                                       Read datum
         CALL UVGET ('READ', BUFFER, BUFFER(1+NRPARM), IRET)
         IF (IRET.GT.0) GO TO 999
         IF (IRET.LE.-1) GO TO 210
         VISCNT = VISCNT + 1
C                                       Correct u,v,w
         U(VISCNT) = BUFFER(1+ILOCU) * GCOR
         V(VISCNT) = BUFFER(1+ILOCV) * GCOR
         W(VISCNT) = BUFFER(1+ILOCW) * GCOR
         TLOW = MIN (TLOW, BUFFER(1+ILOCT))
         THIGH = MAX (THIGH, BUFFER(1+ILOCT))
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            IA2 = BUFFER(1+ILOCB) + 0.1
            IANT1(VISCNT) = IA2 / 256
            IANT2(VISCNT) = IA2 - IANT1(VISCNT) * 256
         ELSE
            IANT1(VISCNT) = BUFFER(1+ILOCA1) + 0.1
            IANT2(VISCNT) = BUFFER(1+ILOCA2) + 0.1
            END IF
C                                       Average visibility
         SUMRE = 0.0
         SUMIM = 0.0
         SUMWT = 0.0
         COUNT = 0.0D0
         IPOINT = NRPARM + 1
         DO 150 KOOP = 1,KVIS
            XRE = BUFFER(IPOINT)
            XIM = BUFFER(IPOINT+1)
C                                       Just pass reals
            IF (DOREAL) THEN
               XIM = 0.0
               END IF
C                                       Convert to amplitude.
            IF (DOAMP) THEN
               XRE = SQRT (XRE*XRE + XIM*XIM)
               XIM = 0.0
               END IF
C                                       Sum
            IF (BUFFER(IPOINT+2).GT.0.0) THEN
               SUMRE = SUMRE + XRE
               SUMIM = SUMIM + XIM
               SUMWT = SUMWT + BUFFER(IPOINT+2)
               COUNT = COUNT + 1.0D0
               END IF
            IPOINT = IPOINT + 3
 150        CONTINUE
         IF (COUNT.LE.0.5D0) COUNT = 1.0D0
         RE(VISCNT) = SUMRE / COUNT
         IM(VISCNT) = SUMIM / COUNT
         WT(VISCNT) = SUMWT / COUNT
 200     CONTINUE
C                                       Close file
 210  CALL UVGET ('CLOS', BUFFER, BUFFER(NRPARM), IRET)
      IRET = 0
C                                       Check that some data given
      NUMVIS = VISCNT
      IF (NUMVIS.LE.1) THEN
         IRET = 8
         WRITE (MSGTXT,1220)
         GO TO 980
C                                       Tell how many vis. used.
      ELSE
         WRITE (MSGTXT,1230) NUMVIS
         END IF
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('UVFDAT: READING ONLY',I8,' OF',I12,' VIS RECORDS')
 1220 FORMAT ('UVFDAT: NO VALID DATA FOUND')
 1230 FORMAT ('UVFDAT: Using',I8,' visibilities')
      END
      SUBROUTINE FITMOD (IRET)
C-----------------------------------------------------------------------
C   Routine to fit a model to uv data.  May write results to a CC file.
C   Inputs from common:
C     NUMVIS         I    Number of visibilities.
C     U(*)           R    U coordinates in wavelengths
C     V(*)           R    V coordinates in wavelengths
C     RE(*)          R    Real part (Jy) (Amplitude if DOAMP)
C     IM(*)          R    Imaginary part (Jy)
C     WT(*)          R    weight
C     DOAMP          L    IF true fit only amplitudes.
C     DOREAL         L    If true fit only reals
C     OPCODE         C*4  Model type, 'GAUS', 'SPHE'
C                         Anything but 'SPHE' is 'GAUS'.
C     NGAUSS         I    Number of gaussians to fit (up to 4)
C     GMAX(4)        R    Estimated peak of gaussian.
C     GPOS(2,4)      R    Estimated position of gaussian, x and y
C                         offsets in arcsec from phase center.
C     GWIDTH(3,4)    R    Estimated size of gaussian.
C                          1 = Major axis (FWHM in asec)
C                          2 = Minor axis (FWHM in asec)
C                          3 = Position angle ( degrees ).
C     GAIN(*)        R    Antenna gain corrections.
C     NITER          I    Maximum number of iterations to run
C     EDROP          R    Chi squared cutoff.
C     DOMAX(4)       R    Flags to fit maximum :
C                         > 0. => vary,  <= 0. => fix.
C     DOPOS(2,4)     R    Flags to fit position.
C     DOWID(3,4)     R    Flags to fit size parameters.
C                         If DOCOMP(5,n) is .le. -1.9 then
C                         a circular gaussian is desired.
C     SIZE(2)        R    Minimum and maximum size for components.
C     DOCAT          R    > 0. => write results in CC file.
C     CCVER          I    CC table version number.
C   Output in Common:
C     GMAX(4)        R    Fitted peak of gaussian.
C     GPOS(2,4)      R    Fitted position of gaussian, x and y
C                         offsets in arcsec from phase center.
C     GWIDTH(3,4)    R    Fitted size of gaussian.
C                          1 = Major axis (FWHM in asec)
C                          2 = Minor axis (FWHM in asec)
C                          3 = Position angle ( degrees ).
C     GAIN(*)        R    Antenna gain corrections.
C     MODPNT(6,*)    I    Array of pointers in P for values varies,
C                         0 => parameter fixed.  One per model parm.
C                         MODPNT(5,n) = -1 => circular gaussian.
C     GAPNT(*)       I    Array of pointers in P for antenna gains
C                         0 => parameter fixed.  One per antenna.
C     IRET           I    Return code, 0=> OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      EXTERNAL  GAUFUN, SPHFUN
C
      CHARACTER PREPOS*5, PRESIZ*5, KEYWRD*8, MODTYP(3)*8
      INTEGER   I, J, K, NREC, LREC, LUN, IERR, IP, IMODTY, JERR, LUN2,
     *   IRNO, MXPRM, TLUN, TIND, LUNTMP, JTRIM
      REAL      RECORD(20), SCLPOS, SCLSIZ, POSMAX, SIZMAX, POS1, POS2,
     *   SIZ1, SIZ2, EPOS1, EPOS2, ESIZ1, ESIZ2
      HOLLERITH HSOUR(2)
      LOGICAL   LERR
      CHARACTER OUTLIN*512
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVGIT.INC'
      INCLUDE 'DUVGIT.INC'
      REAL      ERROR(6,XMXCMP)
      DOUBLE PRECISION EPS, VALUE(XMXPRM), ERR(XMXPRM), FVEC(2*XMXVIS),
     *   TOL, FJAC(XMXPRM,XMXPRM), SFJAC(XMXPRM,XMXPRM), FNORM, ENORM,
     *   WORK(XMXPRM)
      INTEGER   IPVT(XMXPRM), INFO, LDFJAC
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA LUN, LUN2 /27,25/
      DATA MODTYP /'Gaussian','        ','Sphere  '/
      DATA KEYWRD /'SOURCE  '/
      DATA MXPRM /XMXPRM/
      DATA LDFJAC /XMXPRM/
      DATA TOL /1.D-7/
C-----------------------------------------------------------------------
C                                       Set tolerance
      EDROP = ABS (EDROP)
      IF (EDROP.LE.1.0E-6) EDROP = 1.0
      EPS = 1.0D-12 * EDROP
C                                       Find model type
C                                       1=GAUSSIAN, 2=SPHERE
      IMODTY = 1
      IF (OPCODE.EQ.'SPHE') IMODTY = 3
C                                       Tell model type
      WRITE (MSGTXT,1000) MODTYP(IMODTY)
      CALL MSGWRT (2)
C                                       Amplitude only message
      IF (DOAMP) THEN
         MSGTXT = 'Using amplitudes of visibility data ONLY.'
         CALL MSGWRT (2)
         END IF
C                                       Real only message
      IF (DOREAL) THEN
         MSGTXT = 'Using REAL part of visibility data ONLY'
         CALL MSGWRT (2)
         END IF
C                                       complex or real
      MINC = 2
      IF ((DOAMP) .OR. (DOREAL)) MINC = 1
      NUMDAT = MINC * NUMVIS
C                                       Setup for model fitting
      NVAR = 0
      ITTER = 0
      K = 0
      DO 10 I = 1,NGAUSS
C                                       Set pointers
         CALL FILL (6, 0, MODPNT(1,I))
C                                       Make sure .NE. major axis
         IF (ABS (GPARMS(5,I)-GPARMS(4,I)).LT.(0.01*GPARMS(4,I)))
     *      GPARMS(5,I) = 0.99 * GPARMS(4,I)
C                                       Trap for circular gaussian
         IF (DOCOMP(5,I).LE.-1.9) MODPNT(5,I) = -1
C                                       Don't solve for PA on circular
         IF (MODPNT(5,I).LE.-1) DOCOMP(6,I) = -1.0
C                                       all comps
         DO 5 J = 1,6
            K = K + 1
            LPARMS(K) = GPARMS(J,I)
            IF (DOCOMP(J,I).GT.0.01) THEN
               NVAR = NVAR + 1
               VALUE(NVAR) = GPARMS(J,I)
               MODPNT(J,I) = NVAR
               IVAR(NVAR) = I
               JVAR(NVAR) = J
               END IF
 5          CONTINUE
 10      CONTINUE
C                                       Gain parameters
      DO 20 I = 1,NANT
          GAPNT(I) = 0
          K = K + 1
          GAIN(I) = GAINE(I)
          LPARMS(K) = GAIN(I)
          IF (GAIN(I).GT.0.001) THEN
             NVAR = NVAR + 1
             VALUE(NVAR) = GAIN(I)
             GAPNT(I) = NVAR
             IVAR(NVAR) = I
             JVAR(NVAR) = 7
             END IF
          IF (ABS (GAIN(I)).LE.0.001) GAIN(I) = 1.0
          IF (GAIN(I).LT.-0.001) GAIN(I) = ABS (GAIN(I))
 20       CONTINUE
C                                       Check NVAR
      IF (NVAR.GT.MXPRM) THEN
         IRET = 5
         WRITE (MSGTXT,1020) NVAR, MXPRM
         GO TO 990
         END IF
C                                       Get Chi squares
      IF (IMODTY.EQ.1) CALL GAUFUN (NUMDAT, NVAR, VALUE, FVEC, FJAC, 1)
      IF (IMODTY.EQ.3) CALL SPHFUN (NUMDAT, NVAR, VALUE, FVEC, FJAC, 1)
      WRITE (MSGTXT,1030) CHISQ, RMSRES
      CALL MSGWRT (4)
C                                       Fit model
      IF (IMODTY.EQ.1) CALL XGALMS (GAUFUN, NUMDAT, NVAR, VALUE, FVEC,
     *   FJAC, LDFJAC, TOL, INFO, IPVT)
      IF (IMODTY.EQ.3) CALL XGALMS (SPHFUN, NUMDAT, NVAR, VALUE, FVEC,
     *   FJAC, LDFJAC, TOL, INFO, IPVT)
      IF (INFO.EQ.-1) THEN
         MSGTXT = 'NUMBER OF ITERATIONS EXCEEDED WHEN TRYING TO FIT'
      ELSE IF ((INFO.EQ.0) .OR. (INFO.EQ.4)) THEN
         MSGTXT = 'LMSTR1 DOES NOT LIKE INPUT PARAMETERS'
         IF (INFO.EQ.4) MSGTXT = 'MATRIX ORTHOGONAL TO JACOBIAN'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
      ELSE IF (INFO.GT.4) THEN
         WRITE (MSGTXT,1100) INFO
      ELSE
         WRITE (MSGTXT,1101) INFO
         END IF
      CALL MSGWRT (6)
C                                       Get errors
      FNORM = ENORM (NUMDAT, FVEC)
      I = XMXPRM * XMXPRM
      CALL DPCOPY (I, FJAC, SFJAC)
      CALL GETERR (NGAUSS, NVAR, NUMDAT, XMXPRM, IPVT, SFJAC, ERR,
     *   FNORM, WORK, TOL, IVAR, JVAR)
C                                       Put results into output arrays.
C                                       Also get min size and position.
      POSMAX = -1.0E20
      SIZMAX = -1.0E20
      DO 220 I = 1,NGAUSS
C                                       Peak
         IP = MODPNT(1,I)
         IF (IP.GT.0) GPARMS(1,I) = VALUE(IP)
         ERROR(1,I) = -1.0
         IF (IP.GT.0) ERROR(1,I) = ERR(IP)
C                                       Position
         IP = MODPNT(2,I)
         IF (IP.GT.0) GPARMS(2,I) = VALUE(IP)
         IF (ABS (GPARMS(2,I)).GT.POSMAX) POSMAX = ABS (GPARMS(2,I))
         ERROR(2,I) = -1.0
         IF (IP.GT.0) ERROR(2,I) = ERR(IP)
         IP = MODPNT(3,I)
         IF (IP.GT.0) GPARMS(3,I) = VALUE(IP)
         IF (ABS (GPARMS(3,I)).GT.POSMAX) POSMAX = ABS (GPARMS(3,I))
         ERROR(3,I) = -1.0
         IF (IP.GT.0) ERROR(3,I) = ERR(IP)
C                                       Size
         IP = MODPNT(4,I)
         IF (IP.GT.0) GPARMS(4,I) = VALUE(IP)
         IF (GPARMS(4,I).GT.SIZMAX) SIZMAX = GPARMS(4,I)
         ERROR(4,I) = -1.0
         IF (IP.GT.0) ERROR(4,I) = ERR(IP)
         IP = MODPNT(5,I)
         IF (IP.GT.0) GPARMS(5,I) = VALUE(IP)
         IF ((GPARMS(5,I).GT.SIZMAX) .AND. (IMODTY.EQ.1))
     *      SIZMAX = GPARMS(5,I)
         ERROR(5,I) = -1.0
         IF (IP.GT.0) ERROR(5,I) = ERR(IP)
         IP = MODPNT(6,I)
         IF (IP.GT.0) THEN
            GPARMS(6,I) = VALUE(IP)
            IF (GPARMS(6,I).GT.180.0) GPARMS(6,I) = GPARMS(6,I) - 360.0
            IF (GPARMS(6,I).GT.180.0) GPARMS(6,I) = GPARMS(6,I) - 360.0
            IF (GPARMS(6,I).GT.180.0) GPARMS(6,I) = GPARMS(6,I) - 360.0
            IF (GPARMS(6,I).LT.-180.0) GPARMS(6,I) = GPARMS(6,I) + 360.0
            IF (GPARMS(6,I).LT.-180.0) GPARMS(6,I) = GPARMS(6,I) + 360.0
            IF (GPARMS(6,I).LT.-180.0) GPARMS(6,I) = GPARMS(6,I) + 360.0
            END IF
         ERROR(6,I) = -1.0
         IF (IP.GT.0) ERROR(6,I) = ERR(IP)
 220     CONTINUE
C                                       Get size and position scaling
      SCLPOS = POSMAX
      CALL METSCA (POSMAX, PREPOS, LERR)
      IF (ABS (SCLPOS).GT.1.0E-10) SCLPOS = POSMAX / SCLPOS
      IF (ABS (SCLPOS).LE.1.0E-10) SCLPOS = 1.0
      SCLSIZ = SIZMAX
      CALL METSCA (SIZMAX, PRESIZ, LERR)
      IF (ABS (SCLSIZ).GT.1.0E-10) SCLSIZ = SIZMAX / SCLSIZ
      IF (ABS (SCLSIZ).LE.1.0E-10) SCLSIZ = 1.0
C                                       Get Chi squares
      IF (IMODTY.EQ.1) CALL GAUFUN (NUMDAT, NVAR, VALUE, FVEC, FJAC, 1)
      IF (IMODTY.EQ.3) CALL SPHFUN (NUMDAT, NVAR, VALUE, FVEC, FJAC, 1)
      WRITE (MSGTXT,1218) CHISQ, RMSRES
      CALL MSGWRT (4)
C                                       Write results and
C                                       write CC table if requested
      NREC = 10
      LREC = 7
      IRET = 0
      IF (DOCAT.GT.0.0001)
     *    CALL CCINI (LUN, LREC, DISKIN, CNO, CCVER, CATUV, U, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1210) IRET
         GO TO 990
         END IF
C                                       Write source name as KEYWORD
C                                       (only first 8 characters used).
C                                       Assume CURSOU valid.
      IRNO = 1
      IF (DOCAT.GT.0.001) THEN
         CALL GETSOU (CURSOU, DISKIN, CNO, CATUV, LUN2, JERR)
         IF (JERR.EQ.0) THEN
            CALL CHR2H (8, SNAME, 1, HSOUR)
            CALL TABKEY ('WRIT', KEYWRD, 1, U, 1, HSOUR, 3, JERR)
            END IF
         END IF
C                                       text file too?
      IF (FITOUT.NE.' ') THEN
         TLUN = LUNTMP (2)
         CALL ZTXOPN ('WRIT', TLUN, TIND, FITOUT, .TRUE., JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR OPENING TEXT FILE'
            CALL MSGWRT (6)
            FITOUT = ' '
         ELSE
            WRITE (OUTLIN,1300) PREPOS, PRESIZ
            J = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:J), JERR)
            IF (JERR.NE.0) THEN
               MSGTXT = 'ERROR WRITING TEXT FILE'
               CALL MSGWRT (6)
               FITOUT = ' '
               END IF
            END IF
         END IF
C                                       Write results
      J = 1
      CHAN = XBCHAN + 0.1
      DO 290 I = 1,NGAUSS
C                                       Scale position and size
         POS1 = GPARMS(2,I) * SCLPOS
         POS2 = GPARMS(3,I) * SCLPOS
         SIZ1 = GPARMS(4,I) * SCLSIZ
         SIZ2 = GPARMS(5,I) * SCLSIZ
C                                       ERROR=-1 if not fitted
         EPOS1 = -1.0
         EPOS2 = -1.0
         ESIZ1 = -1.0
         ESIZ2 = -1.0
         IF (MODPNT(2,I).GT.0) EPOS1 = ERROR(2,I) * SCLPOS
         IF (MODPNT(3,I).GT.0) EPOS2 = ERROR(3,I) * SCLPOS
         IF (MODPNT(4,I).GT.0) ESIZ1 = ERROR(4,I) * SCLSIZ
         IF (MODPNT(5,I).GT.0) ESIZ2 = ERROR(5,I) * SCLSIZ
         WRITE (MSGTXT,1220,ERR=235) I, GPARMS(1,I), POS1, POS2, PREPOS
 235     CALL MSGWRT (4)
         WRITE (MSGTXT,1222,ERR=240) ERROR(1,I), EPOS1, EPOS2
 240     CALL MSGWRT (4)
         WRITE (MSGTXT,1221,ERR=245) SIZ1, SIZ2, PRESIZ, GPARMS(6,I)
 245     CALL MSGWRT (4)
         WRITE (MSGTXT,1223,ERR=250) ESIZ1, ESIZ2, ERROR(6,I)
 250     CALL MSGWRT (4)
         J = J + 6
C                                       write text file
         IF (FITOUT.NE.' ') THEN
            WRITE (OUTLIN,1310) CHAN, I, POS1, EPOS1, POS2, EPOS2,
     *         GPARMS(1,I), ERROR(1,I), SIZ1, ESIZ1, SIZ2, ESIZ2,
     *         GPARMS(6,I), ERROR(6,I), RMSRES, CHISQ, TLOW, THIGH
            J = JTRIM (OUTLIN)
            OUTLIN(J+2:) = COMNTS(I)
            J = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', TLUN, TIND, OUTLIN(:J), JERR)
            IF (JERR.NE.0) THEN
               MSGTXT = 'ERROR WRITING TEXT FILE'
               CALL MSGWRT (6)
               FITOUT = ' '
               END IF
            END IF
C                                       Write CC table.
         RECORD(1) = GPARMS(1,I)
         RECORD(2) = GPARMS(2,I) / 3600.0
         RECORD(3) = GPARMS(3,I) / 3600.0
         RECORD(4) = GPARMS(4,I) / 3600.0
         RECORD(5) = 0.0
         RECORD(6) = 0.0
         IF (IMODTY.EQ.1) RECORD(5) = GPARMS(5,I) / 3600.0
         IF (IMODTY.EQ.1) RECORD(6) = GPARMS(6,I)
         RECORD(7) = IMODTY
C                                       Write cc file
         IF (DOCAT.GT.0.0001)
     *       CALL TABIO ('WRIT', 0, IRNO, RECORD, U, IRET)
         IRNO = IRNO + 1
         IF (IRET.EQ.0) GO TO 290
            WRITE (MSGTXT,1280) IRET
            GO TO 990
 290     CONTINUE
C                                       Write antenna gains varied
      DO 300 I = 1,30
         IF (GAPNT(I).GT.0) THEN
            IP = GAPNT(I)
            WRITE (MSGTXT,1290) I, VALUE(IP), ERR(IP)
            CALL MSGWRT (4)
            END IF
 300     CONTINUE
C                                       Close CC file
      IF (DOCAT.GT.0.001) CALL TABIO ('CLOS', 0, IRNO, RECORD, U, IERR)
      IF (FITOUT.NE.' ') CALL ZTXCLS (TLUN, TIND, IERR)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Fitting model of type ',A8)
 1020 FORMAT ('TOO MANY PARAMETERS TO FIT,',I3,' > ',I4)
 1030 FORMAT ('Prefit CHI squared =',1PE12.5,' Jy^2  RMS =',1PE12.5,
     *   ' Jy')
 1100 FORMAT ('MODFIT: INFO',I3,' FULL CONVERGENCE NOT ACHIEVED')
 1101 FORMAT ('MODFIT: info',I3,' fit converged')
 1210 FORMAT ('MODFIT: CCINI ERROR',I3,' INIT THE CC FILE')
 1218 FORMAT ('Postfit Chi squared =',1PE12.5,' Jy^2  RMS =',1PE12.5,
     *   ' Jy')
 1220 FORMAT ('Comp',I3,' Max=',1PE11.4,' Jy, pos=',0PF11.6,
     *   F11.6,1X,A5,'sec')
 1221 FORMAT (7X,' Size =',2F12.6,1X,A5,'sec PA =',F8.2,' deg')
 1222 FORMAT (12X,1PE11.4,9X,0PF11.6,F11.6)
 1223 FORMAT (14X,2F12.6,14X,0PF8.2)
 1280 FORMAT ('MODFIT: ERROR',I3,' WRITING CC TABLE')
 1290 FORMAT (' Gain for antenna ',I4,' = ',F8.4,' +/-',F8.4)
 1300 FORMAT ('   ch  #',3X,'Dra',1X,A5,'asec',4X,'Ddec',4X,'+-',3X,
     *   4X,'Flux Jy  +-',2X,3X,'Bmaj',1X,A,'asec',3X,'Bmin',4X,'+-',
     *   3X,4X,'PA   +- ',4X,'RMS   Chi_sq Tstart    Tend')
 1310 FORMAT (I5,I3, F10.4,F7.3, F10.4,F7.3, F9.3,F8.3, F9.4,F7.3,
     *   F9.4,F7.3, F7.1,F5.1, F8.2, F8.2, 2F10.7)
      END
      SUBROUTINE GETERR (NGAUSS, MP, NDATA, MD, IPVT, FJAC, PARERR,
     *   FNORM, WA, TOL, IVAR, JVAR)
C-----------------------------------------------------------------------
C   This subroutine calculates the errors on the fitted parameters.
C   Inputs:
C      NGAUSS  I       Totatl number Gaussians
C      IPVT    I(MP)   Defines a permutation matrix P such that
C                      JAC*P = Q*R, where JAC is the final calculated
C                      Jacobian, Q is orthogonal (not stored), and R is
C                      upper triangular with diagonal elements of
C                      nonincreasing magnitude column J of P is column
C                      IPVT(J) of the identity matrix. (See FJAC below)
C      FJAC    D(MD,MP)   The upper MP by MP submatrix of FJAC contains
C                      an upper triangular matrix R with diagonal
C                      elements of nonincreasing magnitude such that
C                           T     T           T
C                          P *(JAC *JAC)*P = R *R,
C                      where P is a permutation matrix and JAC is the
C                      final calculated Jacobian. Column J of P is
C                      column IPVT(J) (see above) of the identity
C                      matrix.
C      MP      I       Number of parameters in fitted function.
C      NDATA   I       Number of data points fitted.
C      MD      I       Maximum no. of data points allowed for in FJAC
C      FNORM   D       Euclidian norm of solution vector.
C      WA      D(MP)   work array.
C  Output:
C      FJAC    D       modified by COVAR
C      PARERR  D(*)    error in all parameters.
C      TOL     D       tolerance used in call to LMDER1.
C-----------------------------------------------------------------------
      INTEGER   NGAUSS, MD, MP, IPVT(*), NDATA, IVAR(*), JVAR(*)
      DOUBLE PRECISION FJAC(MD,*), PARERR(*), FNORM, WA(*), TOL
C
      DOUBLE PRECISION EPSILN
      INTEGER   J, JC
C-----------------------------------------------------------------------
C                                       Calculate error following
C                                       Argonne write up
      J = NGAUSS * 6 + 30
      CALL DFILL (J, 0.0D0, PARERR)
      EPSILN = FNORM / SQRT (REAL(NDATA-MP))
      CALL COVAR (MP, FJAC, MD, IPVT, TOL, WA)
      DO 100 J = 1,MP
         IF (JVAR(J).LE.6) THEN
            JC = 6 * (IVAR(J) - 1) + JVAR(J)
         ELSE
            JC = 6 + NGAUSS + IVAR(J)
            END IF
         PARERR(J) = EPSILN * SQRT (FJAC(J,J))
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GAUFUN (M, N, VALVAR, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C        M        I      Number of data points in slice (adj array dim)
C        N        I      No. of parameters (adj. array dim.;
C                        NGAUSS * 3)
C        VALVAR   D(N)   parameters of gaussian components,
C                        GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C        IFLAG    I      1=calculate difference for current guess.
C                        2=calculate jacobian for current guess.
C    COMMON GDATA
C        RE       R(M)   real part of vis
C        IM       R(M)   imaginary part of vis
C        ITTER    I      number of calls to evaluate FVEC.
C    Outputs:
C        FVEC     D(M)   Slice data points minus data points
C                           evaluated for current guess.
C        FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION VALVAR(N), FVEC(M), FJROW(N)
C
      INCLUDE 'UVGIT.INC'
      INCLUDE 'DUVGIT.INC'
      INTEGER   IDA, IDATA, I, J, K, IP, IP1, IP2, ITYP, COUNT
      REAL      GN(30), W1(XMXCMP), W2(XMXCMP), ST(XMXCMP), CT(XMXCMP),
     *   TH, C1, C2, ANTFAC, TT, T1, T2, ALF, TC, IMMOD, REMOD
      DOUBLE PRECISION PHI, DRA, DECC, DEC0, SUMRE, SUMIM, TEMP, EFACT
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE GN, W1, W2, ST, CT, REMOD, IMMOD
      DATA C1 /8.3668894E-11/
      DATA C2 /1.7453292E-2/
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      DEC0 = DEC * DG2RAD
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITER) THEN
            IFLAG = -1
            GO TO 999
            END IF
C                                       get full parms array
C                                       must be set in COMMON
C                                       defend against blanks
         DO 5 I = 1,XMXPRM
            IF (LPARMS(I).EQ.FBLANK) LPARMS(I) = 0.0D0
 5          CONTINUE
C                                       transfer varying values
         DO 10 I = 1,N
            IF (JVAR(I).EQ.7) THEN
               K = GAPNT(IVAR(I))
            ELSE
               K = 6 * (IVAR(I) - 1) + JVAR(I)
               END IF
            IF (K.GT.0) LPARMS(K) = VALVAR(I)
 10         CONTINUE
C                                       get constants
         DO 20 I = 1,NGAUSS
            K = (I - 1) * 6 + 1
            W1(I) = C1 * LPARMS(K+3) * LPARMS(K+3)
            W2(I) = C1 * LPARMS(K+4) * LPARMS(K+4)
            TH = C2 * LPARMS(K+5)
            ST(I) = SIN (TH)
            CT(I) = COS (TH)
 20         CONTINUE
         DO 25 I = 1,NANT
            GN(I) = 1.0 / GAIN(I)
            IP = GAPNT(I)
            IF (IP.GT.0) THEN
               VALVAR(IP) = ABS (VALVAR(IP))
               GN(I) = 1.0 / VALVAR(IP)
               END IF
 25         CONTINUE
         CHISQ = 0.0D0
         RMSRES = 0.0D0
         COUNT = 0
         DO 50 IDA = 1,M,MINC
            IDATA = (IDA - 1) / MINC + 1
            SUMRE = RE(IDATA)
            SUMIM = IM(IDATA)
            IP1 = IANT1(IDATA)
            IP2 = IANT2(IDATA)
            ANTFAC = GN(IP1) * GN(IP2)
            TT = U(IDATA) * U(IDATA) + V(IDATA) * V(IDATA)
            K = -5
            DO 40 I = 1,NGAUSS
               K = K + 6
               T1 = U(IDATA)*ST(I) + V(IDATA)*CT(I)
               T2 = U(IDATA)*CT(I) - V(IDATA)*ST(I)
               ALF = W1(I)*T1*T1 + W2(I)*T2*T2
               DRA = AS2RAD * LPARMS(K+1) / COS (DEC0)
               DECC = AS2RAD * LPARMS(K+2) + DEC0
               PHI = TWOPI * ((SIN(DRA)*COS(DECC))*U(IDATA) + V(IDATA)*
     *            (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *            (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)
     *            -1.D0) * W(IDATA))
               TC = EXP (-ALF) * ANTFAC * LPARMS(K)
               IMMOD = TC * SIN (PHI)
               REMOD = TC * COS (PHI)
               SUMRE = SUMRE - REMOD
               SUMIM = SUMIM - IMMOD
 40            CONTINUE
            TEMP = SUMRE * SUMRE + SUMIM * SUMIM
            COUNT = COUNT + 1
            IF (DOAMP) THEN
               FVEC(IDA) = SQRT (TEMP)
               CHISQ = CHISQ + WT(IDATA) * TEMP
               RMSRES = RMSRES + TEMP
            ELSE IF (DOREAL) THEN
               FVEC(IDA) = SUMRE
               CHISQ = CHISQ + WT(IDATA) * SUMRE * SUMRE
               RMSRES = RMSRES + SUMRE * SUMRE
            ELSE
               FVEC(IDA) = SUMRE
               FVEC(IDA+1) = SUMIM
               CHISQ = CHISQ + WT(IDATA) * TEMP
               RMSRES = RMSRES + TEMP
               END IF
 50         CONTINUE
         RMSRES = SQRT (RMSRES / MAX (COUNT, 1))
C                                       calculate Jacobean
      ELSE
         IDA = IFLAG - 1
         IDATA = (IDA - 1) / MINC + 1
         ITYP = MOD (IDA-1, MINC)
         DO 100 I = 1,N
C                                       Gains
            IF (JVAR(I).GT.6) THEN
               IP1 = IVAR(I)
               IF (ITYP.EQ.0) THEN
                  FJROW(I) = -REMOD / GN(IP1)
               ELSE
                  FJROW(I) = -IMMOD / GN(IP1)
                  END IF
C                                       Gaussians
            ELSE
               J = IVAR(I)
               K = 6 * (J - 1) + 1
               SUMRE = RE(IDATA)
               SUMIM = IM(IDATA)
               IP1 = IANT1(IDATA)
               IP2 = IANT2(IDATA)
               ANTFAC = GN(IP1) * GN(IP2)
               TT = U(IDATA) * U(IDATA) + V(IDATA) * V(IDATA)
               T1 = U(IDATA)*ST(J) + V(IDATA)*CT(J)
               T2 = U(IDATA)*CT(J) - V(IDATA)*ST(J)
               ALF = W1(I)*T1*T1 + W2(I)*T2*T2
               DRA = AS2RAD * LPARMS(K+1) / COS (DEC0)
               DECC = AS2RAD * LPARMS(K+2) + DEC0
               PHI = TWOPI * ((SIN(DRA)*COS(DECC))*U(IDATA) + V(IDATA)*
     *            (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *            (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)
     *            -1.D0) * W(IDATA))
               EFACT = -EXP (-ALF) * ANTFAC
               TC = EXP (-ALF) * ANTFAC * LPARMS(K)
C                                       wrt amplitude
               IF (JVAR(I).EQ.1) THEN
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = EFACT * COS (PHI)
                  ELSE
                     FJROW(I) = EFACT * SIN (PHI)
                     END IF
C                                       east=west
               ELSE IF (JVAR(I).EQ.2) THEN
                  TEMP = (AS2RAD * TWOPI * COS(DEC)/COS(DEC0)) *
     *               (COS(DRA)*U(IDATA) + SIN(DEC0)*SIN(DRA)*V(IDATA) -
     *               COS(DEC0)*SIN(DRA)*W(IDATA))
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -EFACT * SIN (PHI) * TEMP
                  ELSE
                     FJROW(I) = EFACT * COS (PHI) * TEMP
                     END IF
C                                       north south
               ELSE IF (JVAR(I).EQ.3) THEN
                  TEMP = AS2RAD * TWOPI * (-SIN(DRA)*SIN(DECC)*U(IDATA)+
     *               (COS(DEC0)*COS(DEC)+SIN(DEC0)*SIN(DECC)*COS(DRA))*
     *               V(IDATA) + W(IDATA) * (COS(DECC)*SIN(DEC0) -
     *               COS(DEC0)*SIN(DECC)*COS(DRA)))
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -EFACT * SIN (PHI) * TEMP
                  ELSE
                     FJROW(I) = EFACT * COS (PHI) * TEMP
                     END IF
C                                       major axis
               ELSE IF (JVAR(I).EQ.4) THEN
                  TEMP = T1 * T1
C                                       Trap for circular gaussian
                  IF (MODPNT(5,J).LE.-1) TEMP = TT
                  TEMP = 2.0 * C1 * LPARMS(K+3) * TEMP
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -EFACT * COS (PHI) * TEMP
                  ELSE
                     FJROW(I) = -EFACT * SIN (PHI) * TEMP
                     END IF
C                                       minor axis
               ELSE IF (JVAR(I).EQ.5) THEN
                  TEMP = 2.0 * C1 * LPARMS(K+4) * T2 * T2
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -EFACT * COS (PHI) * TEMP
                  ELSE
                     FJROW(I) = -EFACT * SIN (PHI) * TEMP
                     END IF
C                                       position angle
               ELSE IF (JVAR(I).EQ.6) THEN
                  TEMP = 2.0 * C2 * T1 * T2 * (W1(J) - W2(J))
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -EFACT * COS (PHI) * TEMP
                  ELSE
                     FJROW(I) = -EFACT * SIN (PHI) * TEMP
                     END IF
                  END IF
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPHFUN (M, N, VALVAR, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.  Spherical model
C   Inputs:
C        M        I      Number of data points in slice (adj array dim)
C        N        I      No. of parameters (adj. array dim.;
C                        NGAUSS * 3)
C        VALVAR   D(N)   parameters of gaussian components,
C                        GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C        IFLAG    I      1=calculate difference for current guess.
C                        2=calculate jacobian for current guess.
C    COMMON GDATA
C        RE       R(M)   real part of vis
C        IM       R(M)   imaginary part of vis
C        ITTER    I      number of calls to evaluate FVEC.
C    Outputs:
C        FVEC     D(M)   Slice data points minus data points
C                           evaluated for current guess.
C        FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION VALVAR(N), FVEC(M), FJROW(N)
C
      INCLUDE 'UVGIT.INC'
      INCLUDE 'DUVGIT.INC'
      INTEGER   IDA, IDATA, I, J, K, IP, IP1, IP2, ITYP, COUNT
      REAL      GN(30), W1(XMXCMP), C1, ANTFAC, TT, TC, IMMOD, REMOD
      DOUBLE PRECISION PHI, DRA, DECC, DEC0, SUMRE, SUMIM, TEMP, AA, CA,
     *   SA
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE GN, W1, REMOD, IMMOD
      DATA C1 /8.3668894E-11/
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      DEC0 = DEC * DG2RAD
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITER) THEN
            IFLAG = -1
            GO TO 999
            END IF
C                                       get full parms array
C                                       must be set in COMMON
C                                       defend against blanks
         DO 5 I = 1,XMXPRM
            IF (LPARMS(I).EQ.FBLANK) LPARMS(I) = 0.0D0
 5          CONTINUE
C                                       transfer varying values
         DO 10 I = 1,N
            IF (JVAR(I).EQ.7) THEN
               K = GAPNT(IVAR(I))
            ELSE
               K = 6 * (IVAR(I) - 1) + JVAR(I)
               END IF
            IF (K.GT.0) LPARMS(K) = VALVAR(I)
 10         CONTINUE
C                                       get constants
         DO 20 I = 1,NGAUSS
            K = (I - 1) * 6 + 1
            W1(I) = C1 * LPARMS(K+4)
 20         CONTINUE
         DO 25 I = 1,NANT
            GN(I) = 1.0 / GAIN(I)
            IP = GAPNT(I)
            IF (IP.GT.0) THEN
               VALVAR(IP) = ABS (VALVAR(IP))
               GN(I) = 1.0 / VALVAR(IP)
               END IF
 25         CONTINUE
         CHISQ = 0.0D0
         RMSRES = 0.0D0
         COUNT = 0
         DO 50 IDA = 1,M,MINC
            IDATA = (IDA - 1) / MINC + 1
            SUMRE = RE(IDATA)
            SUMIM = IM(IDATA)
            IP1 = IANT1(IDATA)
            IP2 = IANT2(IDATA)
            ANTFAC = GN(IP1) * GN(IP2)
            TT = U(IDATA) * U(IDATA) + V(IDATA) * V(IDATA)
            K = -5
            DO 40 I = 1,NGAUSS
               K = K + 6
               AA = W1(I) * TT
C                                       Trap very unresolved.
C                                       This needed to prevent
C                                       serious precision loss.
               IF (AA.LT.6.28D-2) AA = 6.28D-2
               CA = COS (AA)
               SA = SIN (AA)
               DRA = AS2RAD * LPARMS(K+1) / COS (DEC0)
               DECC = AS2RAD * LPARMS(K+2) + DEC0
               PHI = TWOPI * ((SIN(DRA)*COS(DECC))*U(IDATA) + V(IDATA)*
     *            (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *            (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)
     *            -1.D0) * W(IDATA))
               TC = 3.0 * ANTFAC * LPARMS(K) * ((SA/(AA*AA*AA)) -
     *            (CA/(AA*AA)))
               IMMOD = TC * SIN (PHI)
               REMOD = TC * COS (PHI)
               SUMRE = SUMRE - REMOD
               SUMIM = SUMIM - IMMOD
 40            CONTINUE
            TEMP = SUMRE * SUMRE + SUMIM * SUMIM
            COUNT = COUNT + 1
            IF (DOAMP) THEN
               FVEC(IDA) = SQRT (TEMP)
               CHISQ = CHISQ + WT(IDATA) * TEMP
               RMSRES = RMSRES + TEMP
            ELSE IF (DOREAL) THEN
               FVEC(IDA) = SUMRE
               CHISQ = CHISQ + WT(IDATA) * SUMRE * SUMRE
               RMSRES = RMSRES + SUMRE * SUMRE
            ELSE
               FVEC(IDA) = SUMRE
               FVEC(IDA+1) = SUMIM
               CHISQ = CHISQ + WT(IDATA) * TEMP
               RMSRES = RMSRES + TEMP
               END IF
 50         CONTINUE
         RMSRES = SQRT (RMSRES / MAX (COUNT, 1))
C                                       calculate Jacobean
      ELSE
         IDA = IFLAG - 1
         IDATA = (IDA - 1) / MINC + 1
         ITYP = MOD (IDA-1, MINC)
         TT = U(IDATA) * U(IDATA) + V(IDATA) * V(IDATA)
         DO 100 I = 1,N
C                                       Gains
            IF (JVAR(I).GT.6) THEN
               IP1 = IVAR(I)
               IF (ITYP.EQ.0) THEN
                  FJROW(I) = -REMOD / GN(IP1)
               ELSE
                  FJROW(I) = -IMMOD / GN(IP1)
                  END IF
C                                       Spheres
            ELSE
               J = IVAR(I)
               K = 6 * (J - 1) + 1
               IP1 = IANT1(IDATA)
               IP2 = IANT2(IDATA)
               ANTFAC = GN(IP1) * GN(IP2)
               AA = W1(J) * TT
C                                       Trap very unresolved.
C                                       This needed to prevent
C                                       serious precision loss.
               IF (AA.LT.6.28D-2) AA = 6.28D-2
               CA = COS (AA)
               SA = SIN (AA)
               DRA = AS2RAD * LPARMS(K+1) / COS (DEC0)
               DECC = AS2RAD * LPARMS(K+2) + DEC0
               PHI = TWOPI * ((SIN(DRA)*COS(DECC))*U(IDATA) + V(IDATA)*
     *            (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *            (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)
     *            -1.D0) * W(IDATA))
               TC = 3.0 * ANTFAC * LPARMS(K) * ((SA/(AA*AA*AA)) -
     *            (CA/(AA*AA)))
               IMMOD = TC * SIN (PHI)
               REMOD = TC * COS (PHI)
C                                       wrt amplitude
               IF (JVAR(I).EQ.1) THEN
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -REMOD / (LPARMS(K) + 1.E-20)
                  ELSE
                     FJROW(I) = -IMMOD / (LPARMS(K) + 1.E-20)
                     END IF
C                                       east=west
               ELSE IF (JVAR(I).EQ.2) THEN
                  TEMP = (AS2RAD * TWOPI * COS(DEC)/COS(DEC0)) *
     *               (COS(DRA)*U(IDATA) + SIN(DEC0)*SIN(DRA)*V(IDATA) -
     *               COS(DEC0)*SIN(DRA)*W(IDATA))
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = IMMOD * TEMP
                  ELSE
                     FJROW(I) = -REMOD * TEMP
                     END IF
C                                       north south
               ELSE IF (JVAR(I).EQ.3) THEN
                  TEMP = AS2RAD * TWOPI * (-SIN(DRA)*SIN(DECC)*U(IDATA)+
     *               (COS(DEC0)*COS(DEC)+SIN(DEC0)*SIN(DECC)*COS(DRA))*
     *               V(IDATA) + W(IDATA) * (COS(DECC)*SIN(DEC0) -
     *               COS(DEC0)*SIN(DECC)*COS(DRA)))
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = IMMOD * TEMP
                  ELSE
                     FJROW(I) = -REMOD * TEMP
                     END IF
C                                       size
               ELSE IF (JVAR(I).EQ.4) THEN
                  TEMP = 3.0 * LPARMS(K) * C1 * TT / (AA*AA*AA*AA) *
     *               ANTFAC
                  TEMP = TEMP * ( SA * (AA*AA - 3.0) + 3.0 * AA * CA)
                  IF (ITYP.EQ.0) THEN
                     FJROW(I) = -COS (PHI) * TEMP
                  ELSE
                     FJROW(I) = -SIN (PHI) * TEMP
                     END IF

                  END IF
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE XGALMS (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   XGALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs:
C      FCN      EXT      Function to evaluate the model
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      LDFJAC   I        Number points on first axis of FJAC (adj.
C                           array dim.)
C      TOL      D        Tolerance desired
C   In/out:
C      X        D(N)     Initial guess/ answer
C      FVEC     D(M)     Function (Data - model) evaluation
C      FJAC     D(N,N)   Work matrix
C      INFO     I        Error code: 1 - 3 good, 0 bad input,
C                           4 orthogonal, 5 - 7 poor fit
C      IPVT     D(N)     Permutation matrix
C   See precursor remarks to LMSTR1 or LMSTR for details.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, INFO, IPVT(N)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), TOL
C
      INTEGER   LWA
      DOUBLE PRECISION WA(100000)
      DATA LWA /100000/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      CALL LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END

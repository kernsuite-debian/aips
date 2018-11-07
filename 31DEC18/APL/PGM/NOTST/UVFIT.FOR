LOCAL INCLUDE 'UVFIT.INC'
C                                       Local include for UVFIT
      INTEGER   XMXVIS, XMXPRM, XMXCMP
C                                       XMXVIS = max. no. vis records.
      PARAMETER (XMXVIS=2500000)
C                                       XMXCMP = max. no sources
      PARAMETER (XMXCMP=60)
C                                       XMXPRM = max. no parameters.
      PARAMETER (XMXPRM=6*XMXCMP+30)
C
      INTEGER   SEQIN, DISKIN, NGAUSS, NITER, CCVER, CHAN, NPR,
     *   CNO, NVAR, MODPNT(6,XMXCMP), GAPNT(30), IANT1(XMXVIS),
     *   IANT2(XMXVIS)
      LOGICAL   DOAMP, DOREAL
      INTEGER   NUMVIS, MAXVIS, NANT
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSOLM, XOPCOD,
     *   XXSTOK, XINLIS(12), XFITOU(12)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSOLMO*4, OPCODE*4,
     *   XSTOK*4, INLIST*48, FITOUT*48, COMNTS(XMXCMP)*256
      REAL      XSIN, XDISIN, XBCHAN, XECHAN, XBIF, XEIF, XBAND, XFREQ,
     *   XFQID, XANTS(50), XSUBA, XTIME(8), XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XUVRA(2),
     *   XNGAU, DMAX(4), DPOS(2,4), DWIDTH(3,4), XNIT, EDROP, DDMAX(4),
     *   DDPOS(2,4), DDWID(3,4), SIZE(2), PRTL, DOCAT, XNVER, BADD(10),
     *   GAIN(30), U(XMXVIS), V(XMXVIS), W(XMXVIS), RE(XMXVIS),
     *   IM(XMXVIS), WT(XMXVIS), GMAX(XMXCMP), GPOS(2,XMXCMP),
     *   GWIDTH(3,XMXCMP), DOMAX(XMXCMP), DOPOS(2,XMXCMP),
     *   DOWID(3,XMXCMP), TLOW, THIGH
      COMMON /UVDATA/ U, V, W, RE, IM, WT, IANT1, IANT2, NUMVIS,
     *   MAXVIS, NANT
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XBCHAN, XECHAN, XBIF, XEIF, XXSTOK, XBAND, XFREQ, XFQID,
     *   XXSOUR, XANTS, XSUBA, XTIME, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XUVRA, XXSOLM, XOPCOD,
     *   XNGAU, DMAX, DPOS, DWIDTH, GAIN, XNIT, EDROP, DDMAX, DDPOS,
     *   DDWID, XINLIS, SIZE, PRTL, DOCAT, XNVER, XFITOU, BADD
      COMMON /INFO/ DOAMP, DOREAL, SEQIN, DISKIN, NGAUSS, NITER, CCVER,
     *   CHAN, CNO, NPR, NVAR, MODPNT, GAPNT, GMAX, GPOS, GWIDTH, DOMAX,
     *   DOPOS, DOWID, TLOW, THIGH
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XSOLMO, OPCODE, XSTOK,
     *   INLIST, FITOUT, COMNTS
LOCAL END
      PROGRAM UVFIT
C-----------------------------------------------------------------------
C! Fits source models to uv data.
C# UV Modeling Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2003, 2006-2007, 2010-2012,
C;  Copyright (C) 2015-2016, 2018
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
C   UVFIT fits a model consisting of a collection of elliptical
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
C      GWIDTH(3,4)    GWIDTH        (BMAJ,BMIN,BPA) in asec, asec,
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
      INCLUDE 'UVFIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'UVFIT '/
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
 990  CALL DIE (IRET, U)
C
 999  STOP
      END
      SUBROUTINE UVFTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVFTIN gets input parameters for UVFIT and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UVFIT for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR, IROUND, NPARM, IERR, LOOP, LUN, I, JTRIM
      REAL      CATR(256)
      LOGICAL   T, F, TABLE, EXIST, FITASC, OK, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
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
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, U, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, U, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      NGAUSS = IROUND (XNGAU)
      NITER = IROUND (XNIT)
      CCVER = IROUND (XNVER)
      NPR = IROUND (PRTL)
      DO 15 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 15      CONTINUE
C                                       Characters
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
     *   NLUSER, STAT, U, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Set status (WRIT => CC)
      STAT = 'REST'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, STAT, U, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       See if multisource table
      CALL ISTAB ('SU', DISKIN, CNO, 1, LUN, U, TABLE, EXIST, FITASC,
     *   IERR)
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
      CALL GETANT (DISKIN, CNO, SUBARR, CATBLK, U, JERR)
      IF (JERR.GT.0) GO TO 999
      NANT = NSTNS
C
      DOAMP = XSOLMO.EQ.'A   '
      DOREAL = XSOLMO.EQ.'RE  '
      IF (DOAMP .OR. DOREAL) THEN
C                                       Make sure one posn. fixed
         OK = F
         DO 150 LOOP = 1,NGAUSS
            OK = OK .OR. ((DOPOS(1,LOOP).LE.0.0) .AND.
     *         (DOPOS(2,LOOP).LE.0.0))
 150        CONTINUE
         IF (.NOT.OK) THEN
C                                       Must fix one posn.
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
      INCLUDE 'UVFIT.INC'
      INTEGER   TLUN, TIND, LUNTMP, I, JTRIM, J, NC, KBP
      DOUBLE PRECISION X
      CHARACTER INLINE*512
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       adverb input
      IF (INLIST.EQ.' ') THEN
         NGAUSS = MAX (1, MIN (4, NGAUSS))
         DO 10 I = 1,NGAUSS
            GMAX(I) = DMAX(I)
            IF (GMAX(I).EQ.0.0) GMAX(I) = 1.0
            GPOS(1,I) = DPOS(1,I)
            GPOS(2,I) = DPOS(2,I)
            GWIDTH(1,I) = DWIDTH(1,I)
            GWIDTH(2,I) = DWIDTH(2,I)
            GWIDTH(3,I) = DWIDTH(3,I)
            DOMAX(I) = DDMAX(I)
            DOPOS(1,I) = DDPOS(1,I)
            DOPOS(2,I) = DDPOS(2,I)
            DOWID(1,I) = DDWID(1,I)
            DOWID(2,I) = DDWID(2,I)
            DOWID(3,I) = DDWID(3,I)
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
            DOMAX(J) = -1
            DOPOS(1,J) = -1
            DOPOS(2,J) = -1
            DOWID(1,J) = -1
            DOWID(2,J) = -1
            DOWID(3,J) = -1
C                                       flux
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GMAX(J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPOS(1,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GPOS(2,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GWIDTH(1,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GWIDTH(2,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            GWIDTH(3,J) = X
C                                       count it
            NGAUSS = J
C                                       are we fitting?
C                                       flux
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOMAX(J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOPOS(1,J) = X
C                                       position
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOPOS(2,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOWID(1,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOWID(2,J) = X
C                                       width
            CALL GETNUM (INLINE, NC, KBP, X)
            IF (X.EQ.DBLANK) GO TO 100
            DOWID(3,J) = X
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
      INCLUDE 'UVFIT.INC'
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
      IF (NUMVIS.GT.1) GO TO 230
         IRET = 8
         WRITE (MSGTXT,1220)
         GO TO 980
C                                       Tell how many vis. used.
 230  WRITE (MSGTXT,1230) NUMVIS
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('UVFDAT: READING ONLY',I8,' OF',I12,' VIS RECORDS')
 1220 FORMAT ('UVFDAT: NO VALID DATA FOUND')
 1230 FORMAT ('UVFDAT: Using ',I6,' visibilities')
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
C                         If DOWID(2,n) is .le. -1.9 then
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
      CHARACTER PREPOS*5, PRESIZ*5, KEYWRD*8, MODTYP(3)*8
      INTEGER   IRET, I, J, NREC, LREC, LUN, IERR, IP, IMODTY, JERR,
     *   LUN2
      LOGICAL   LERR
      INTEGER   IRNO, MXPRM, TLUN, TIND, LUNTMP, JTRIM
      REAL      RECORD(20), SCLPOS, SCLSIZ, POSMAX, SIZMAX, POS1, POS2,
     *   SIZ1, SIZ2, EPOS1, EPOS2, ESIZ1, ESIZ2
      DOUBLE PRECISION EPS, FOPT, GNOPT, CHISQ, FAC, RMS, RMSRES
      HOLLERITH HSOUR(2)
      CHARACTER OUTLIN*512
      EXTERNAL UVFUN1, UVFUN2, UVRMS1, UVRMS2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
      REAL      ERROR(6,XMXCMP)
      DOUBLE PRECISION VALUE(XMXPRM), ERR(XMXPRM), GRAD(XMXPRM)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA LUN, LUN2 /27,25/
      DATA MODTYP /'Gaussian','        ','Sphere  '/
      DATA KEYWRD /'SOURCE  '/
      DATA MXPRM /XMXPRM/
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
C                                       Setup for model fitting
      NVAR = 0
      DO 10 I = 1,NGAUSS
C                                       Set pointers
         CALL FILL (6, 0, MODPNT(1,I))
C                                       Maximum
         IF (DOMAX(I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GMAX(I)
            MODPNT(1,I) = NVAR
            END IF
C                                       Position
         IF (DOPOS(1,I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GPOS(1,I)
            MODPNT(2,I) = NVAR
            END IF
         IF (DOPOS(2,I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GPOS(2,I)
            MODPNT(3,I) = NVAR
            END IF
C                                       Size
         IF (DOWID(1,I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GWIDTH(1,I)
            MODPNT(4,I) = NVAR
            END IF
C                                       Make sure .NE. major axis
         IF (ABS (GWIDTH(2,I)-GWIDTH(1,I)).LT.(0.01*GWIDTH(1,I)))
     *      GWIDTH(2,I) = 0.99 * GWIDTH(1,I)
         IF (DOWID(2,I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GWIDTH(2,I)
            MODPNT(5,I) = NVAR
            END IF
C                                       Trap for circular gaussian
         IF (DOWID(2,I).LE.-1.9) MODPNT(5,I) = -1
C                                       Don't solve for PA on circular
         IF (MODPNT(5,I).LE.-1) DOWID(3,I) = -1.0
         IF (DOWID(3,I).GT.0.01) THEN
            NVAR = NVAR + 1
            VALUE(NVAR) = GWIDTH(3,I)
            MODPNT(6,I) = NVAR
            END IF
 10      CONTINUE
C                                       Gain parameters
      DO 20 I = 1,NANT
          GAPNT(I) = 0
          IF (GAIN(I).GT.0.001) THEN
             NVAR = NVAR + 1
             VALUE(NVAR) = GAIN(I)
             GAPNT(I) = NVAR
             END IF
          IF (ABS (GAIN(I)).LE.0.001) GAIN(I) = 1.0
          IF (GAIN(I).LT.-0.001) GAIN(I) = ABS (GAIN(I))
 20       CONTINUE
C                                       Check NVAR
      IF (NVAR.LE.MXPRM) GO TO 30
         IRET = 5
         WRITE (MSGTXT,1020) NVAR, MXPRM
         GO TO 990
C                                       Get Chi squares
 30   IF (IMODTY.EQ.1) CALL UVFUN1 (VALUE, CHISQ, GRAD, 0)
      IF (IMODTY.EQ.3) CALL UVFUN2 (VALUE, CHISQ, GRAD, 0)
      WRITE (MSGTXT,1030) CHISQ
      CALL MSGWRT (4)
C                                       Give RMS residual
      IF (IMODTY.EQ.1) CALL UVRMS1 (VALUE, RMSRES)
      IF (IMODTY.EQ.3) CALL UVRMS2 (VALUE, RMSRES)
      WRITE (MSGTXT,1031) RMSRES
      CALL MSGWRT (4)
C                                       Rough estimate of the square
C                                       root of the diagonal elements
C                                       of the inverse Hessian, as
C                                       required by DVDMIN. Use gradient
C                                       for approximate estimate.
      DO 40 I = 1, NVAR
         IF (ABS (GRAD(I)).GT.0.0) THEN
            ERR(I) = SQRT (1.0D0 / SQRT (ABS (GRAD(I))))
         ELSE
            ERR(I) = 1.0D0
            END IF
40       CONTINUE
C                                       Fit model
      IF (IMODTY.EQ.1) CALL DVDMIN (UVFUN1, VALUE, ERR, NVAR, EPS,
     *   NITER, FOPT, GNOPT, NPR, IRET)
      IF (IMODTY.EQ.3) CALL DVDMIN (UVFUN2, VALUE, ERR, NVAR, EPS,
     *   NITER, FOPT, GNOPT, NPR, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
      ELSE IF (IRET.EQ.1) THEN
         MSGTXT = 'ITERATION LIMIT REACHED, DID NOT CONVERGE'
         CALL MSGWRT (6)
      ELSE
         MSGTXT = 'Solution converged'
         CALL MSGWRT (4)
         END IF
C                                       Get errors
      FAC = NUMVIS
      FAC = SQRT (FAC / (NUMVIS - NVAR))
      RMS = SQRT (FOPT/NUMVIS) * FAC
      DO 210 I = 1,NVAR
         ERR(I) = ERR(I) * RMS
 210     CONTINUE
C                                       Put results into output arrays.
C                                       Also get min size and position.
      POSMAX = -1.0E20
      SIZMAX = -1.0E20
      DO 220 I = 1,NGAUSS
C                                       Peak
         IP = MODPNT(1,I)
         IF (IP.GT.0) GMAX(I) = VALUE(IP)
         ERROR(1,I) = -1.0
         IF (IP.GT.0) ERROR(1,I) = ERR(IP)
C                                       Position
         IP = MODPNT(2,I)
         IF (IP.GT.0) GPOS(1,I) = VALUE(IP)
         IF (ABS (GPOS(1,I)).GT.POSMAX) POSMAX = ABS (GPOS(1,I))
         ERROR(2,I) = -1.0
         IF (IP.GT.0) ERROR(2,I) = ERR(IP)
         IP = MODPNT(3,I)
         IF (IP.GT.0) GPOS(2,I) = VALUE(IP)
         IF (ABS (GPOS(2,I)).GT.POSMAX) POSMAX = ABS (GPOS(2,I))
         ERROR(3,I) = -1.0
         IF (IP.GT.0) ERROR(3,I) = ERR(IP)
C                                       Size
         IP = MODPNT(4,I)
         IF (IP.GT.0) GWIDTH(1,I) = VALUE(IP)
         IF (GWIDTH(1,I).GT.SIZMAX) SIZMAX = GWIDTH(1,I)
         ERROR(4,I) = -1.0
         IF (IP.GT.0) ERROR(4,I) = ERR(IP)
         IP = MODPNT(5,I)
         IF (IP.GT.0) GWIDTH(2,I) = VALUE(IP)
         IF ((GWIDTH(2,I).GT.SIZMAX) .AND. (IMODTY.EQ.1))
     *      SIZMAX = GWIDTH(2,I)
         ERROR(5,I) = -1.0
         IF (IP.GT.0) ERROR(5,I) = ERR(IP)
         IP = MODPNT(6,I)
         IF (IP.GT.0) THEN
            GWIDTH(3,I) = VALUE(IP)
            IF (GWIDTH(3,I).GT.180.0) GWIDTH(3,I) = GWIDTH(3,I) - 360.0
            IF (GWIDTH(3,I).GT.180.0) GWIDTH(3,I) = GWIDTH(3,I) - 360.0
            IF (GWIDTH(3,I).GT.180.0) GWIDTH(3,I) = GWIDTH(3,I) - 360.0
            IF (GWIDTH(3,I).LT.-180.0) GWIDTH(3,I) = GWIDTH(3,I) + 360.0
            IF (GWIDTH(3,I).LT.-180.0) GWIDTH(3,I) = GWIDTH(3,I) + 360.0
            IF (GWIDTH(3,I).LT.-180.0) GWIDTH(3,I) = GWIDTH(3,I) + 360.0
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
      IF (IMODTY.EQ.1) CALL UVFUN1 (VALUE, CHISQ, GRAD, 1)
      IF (IMODTY.EQ.3) CALL UVFUN2 (VALUE, CHISQ, GRAD, 1)
      WRITE (MSGTXT,1218) CHISQ
      CALL MSGWRT (4)
C                                       Give RMS residual
      IF (IMODTY.EQ.1) CALL UVRMS1 (VALUE, RMSRES)
      IF (IMODTY.EQ.3) CALL UVRMS2 (VALUE, RMSRES)
      WRITE (MSGTXT,1219) RMSRES
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
         POS1 = GPOS(1,I) * SCLPOS
         POS2 = GPOS(2,I) * SCLPOS
         SIZ1 = GWIDTH(1,I) * SCLSIZ
         SIZ2 = GWIDTH(2,I) * SCLSIZ
C                                       ERROR=-1 if not fitted
         EPOS1 = -1.0
         EPOS2 = -1.0
         ESIZ1 = -1.0
         ESIZ2 = -1.0
         IF (MODPNT(2,I).GT.0) EPOS1 = ERROR(2,I) * SCLPOS
         IF (MODPNT(3,I).GT.0) EPOS2 = ERROR(3,I) * SCLPOS
         IF (MODPNT(4,I).GT.0) ESIZ1 = ERROR(4,I) * SCLSIZ
         IF (MODPNT(5,I).GT.0) ESIZ2 = ERROR(5,I) * SCLSIZ
         WRITE (MSGTXT,1220,ERR=235) I, GMAX(I), POS1, POS2, PREPOS
 235     CALL MSGWRT (4)
         WRITE (MSGTXT,1222,ERR=240) ERROR(1,I), EPOS1, EPOS2
 240     CALL MSGWRT (4)
         WRITE (MSGTXT,1221,ERR=245) SIZ1, SIZ2, PRESIZ, GWIDTH(3,I)
 245     CALL MSGWRT (4)
         WRITE (MSGTXT,1223,ERR=250) ESIZ1, ESIZ2, ERROR(6,I)
 250     CALL MSGWRT (4)
         J = J + 6
C                                       write text file
         IF (FITOUT.NE.' ') THEN
            WRITE (OUTLIN,1310) CHAN, I, POS1, EPOS1, POS2, EPOS2,
     *         GMAX(I), ERROR(1,I), SIZ1, ESIZ1, SIZ2, ESIZ2,
     *         GWIDTH(3,I), ERROR(6,I), RMSRES, CHISQ, TLOW, THIGH
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
         RECORD(1) = GMAX(I)
         RECORD(2) = GPOS(1,I) / 3600.0
         RECORD(3) = GPOS(2,I) / 3600.0
         RECORD(4) = GWIDTH(1,I) / 3600.0
         RECORD(5) = 0.0
         RECORD(6) = 0.0
         IF (IMODTY.EQ.1) RECORD(5) = GWIDTH(2,I) / 3600.0
         IF (IMODTY.EQ.1) RECORD(6) = GWIDTH(3,I)
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
 1030 FORMAT ('Prefit CHI squares =',1PE12.5)
 1031 FORMAT ('Prefit RMS =',1PE12.5,' Jy')
 1100 FORMAT ('MODFIT: ERROR',I3,' FITTING MODEL')
 1210 FORMAT ('MODFIT: ERROR',I3,' CREATING/OPENING CC TABLE')
 1218 FORMAT ('Postfit Chi squares =',1PE12.5)
 1219 FORMAT ('Postfit RMS =',1PE12.5,' Jy')
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
      SUBROUTINE UVFUN1 (P, F, GRAD, IFLAG)
C-----------------------------------------------------------------------
C   Eliptical Gaussian model
C   Given the vector P of solution parameters, this subroutine computes
C   the value of the chi-squared function F (a sum of squared residuals)
C   and, optionally, the gradient, GRAD, of F w.r.t. P.  When IFLAG=1,
C   only F is computed.  Otherwise F and GRAD are both computed.  Note
C   that P is to contain only the parameters which are being solved for
C   --- not the parameters that are to be held fixed.  This subroutine
C   is called by the minimization routine DVDMIN.
C
C   The data is contained in common /UVDATA/, fixed values of the
C   parameters are in common /INPARM/.  Other information is in common
C   /INFO/.
C    Inputs:
C     P(NVAR)    D    Vector of least-squares solution pararameters.
C     IFLAG      I    IFLAG=1 ==> compute just F,
C                     IFLAG.NE.1 ==> compute both F and GRAD.
C    Inputs from common:
C     NUMVIS     I    Number of visibility points
C     U(*),V(*)  R    U and v coordinates of data
C     RE(*)      R    Real part of data (Amplitude if DOAMP)
C     IM(*)      R    Imaginary part of data
C     WT(*)      R    Weights for data
C     IANT1(*)   I    first antenna number
C     IANT2(*)   I    second antenna number
C     DOAMP      L    IF true fit only amplitudes.
C     DOREAL         L    If true fit only reals
C     NGAUSS     I    Number of Gaussians
C     GMAX(*)    R    Array of peak values to use for fixed parms.
C     GPOS(2,*)  R    Array of positions to use for fixed parms.
C     GWID(3,*)  R    Array of sizes to use for fixed parms.
C     MODPNT(6,*)I    Array of pointers in P for values varies,
C                     0 => parameter fixed.  One per model parameter.
C                     MODPNT(5,n) = -1 => circular gaussian.
C     GAPNT(*)   I    Array of pointers in P for antenna gains varied
C                     0 => parameter fixed.  One per antenna.
C     SIZE(2)    R    Lower and upper bound on component size (Asec)
C
C   Outputs:
C     P(NVAR)    D    Enforces limit on component size.
C     F          D    The value of the chi-squared function
C                     corresponding to the given P.
C     GRAD(NVAR) D    The gradient of the chi-squared function.  I.e.,
C                     GRAD(I) = derivative of F w.r.t. P(I).
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
      INTEGER   IFLAG, I, J, IP, IP1, IP2, I4
      LOGICAL   NGRAD
      REAL      W1(XMXCMP), W2(XMXCMP), TH, ST(XMXCMP), CT(XMXCMP), T1,
     *   T2, TT, TC, ALF, REMOD, IMMOD, SUMRE, SUMIM, TEMP, RESRE,
     *   RESIM, PMODEL(7,XMXCMP), GN(30), ANTFAC, AMPMOD, C1, C2
      DOUBLE PRECISION    P(*), F, GRAD(*), SUM, PHI, DRA, DECC, DEC0
      REAL      PARTRE(XMXPRM), PARTIM(XMXPRM)
      DATA C1 /8.3668894E-11/
      DATA C2 /1.7453292E-2/
C      DATA C3 /3.0461742E-5/
C      DATA C4 /1.4768269E-10/
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      NGRAD = IFLAG.EQ.1
C                                       Get total current model
      DO 10 I = 1,NGAUSS
C                                       Peak
         PMODEL(1,I) = GMAX(I)
         IP = MODPNT(1,I)
         IF (IP.GT.0) PMODEL(1,I) = P(IP)
         PMODEL(7,I) = 1.0 / (PMODEL(1,I) + 1.0E-20)
C                                       Position
         PMODEL(2,I) = GPOS(1,I)
         IP = MODPNT(2,I)
         IF (IP.GT.0) PMODEL(2,I) = P(IP)
         PMODEL(3,I) = GPOS(2,I)
         IP = MODPNT(3,I)
         IF (IP.GT.0) PMODEL(3,I) = P(IP)
C                                       Size
         PMODEL(4,I) = GWIDTH(1,I)
         IP = MODPNT(4,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(4,I) = P(IP)
C                                       Circular gaussian trap.
         IP = MODPNT(5,I)
         IF (IP.LE.-1) GWIDTH(2,I) = PMODEL(4,I)
         PMODEL(5,I) = GWIDTH(2,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(5,I) = P(IP)
         PMODEL(6,I) = GWIDTH(3,I)
         IP = MODPNT(6,I)
         IF (IP.GT.0) PMODEL(6,I) = P(IP)
C                                       Set some constants
         W1(I) = C1 * PMODEL(4,I) * PMODEL(4,I)
         W2(I) = C1 * PMODEL(5,I) * PMODEL(5,I)
         TH = C2 * PMODEL(6,I)
         ST(I) = SIN (TH)
         CT(I) = COS (TH)
 10      CONTINUE
C                                       Get gains
      DO 15 I = 1,NANT
         GN(I) = 1.0 / GAIN(I)
         IP = GAPNT(I)
C                                       Make sure gains positive
         P(IP) = ABS (P(IP))
         IF (IP.GT.0) GN(I) = 1.0 / P(IP)
 15      CONTINUE
C                                       Clear GRAD
      IF (NGRAD) GO TO 30
         DO 20 I = 1,NVAR
            GRAD(I) = 0.0D0
 20         CONTINUE
C                                       Compute Chi squares
C                                       sum (wt(res(re)**2+res(im)**2))
 30   SUM = 0.0D0
C                                       Loop over data
      DEC0 = DEC * DG2RAD
      DO 100 I4 = 1,NUMVIS
         IF (WT(I4).LE.0.0) GO TO 100
C                                       Compute model - loop over gauss.
         SUMRE = 0.0
         SUMIM = 0.0
         IP1 = IANT1(I4)
         IP2 = IANT2(I4)
         ANTFAC = GN(IP1) * GN(IP2)
         TT = U(I4) * U(I4) + V(I4) * V(I4)
         DO 80 J = 1,NGAUSS
            T1 = U(I4)*ST(J) + V(I4)*CT(J)
            T2 = U(I4)*CT(J) - V(I4)*ST(J)
            ALF = W1(J)*T1*T1 + W2(J)*T2*T2
            DRA = AS2RAD * PMODEL(2,J) / COS (DEC0)
            DECC = AS2RAD * PMODEL(3,J) + DEC0
C            PHII = C3 * (PMODEL(2,J)*U(I4) + PMODEL(3,J)*V(I4)) -
C     *         0.5 * C4 * W(I4) * (PMODEL(2,J)*PMODEL(2,J) +
C     *         PMODEL(3,J)*PMODEL(3,J))
            PHI = TWOPI * ((SIN(DRA)*COS(DECC)) * U(I4) + V(I4)*
     *         (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *         (SIN(DEC0)*SIN(DECC)+COS(DEC0)*COS(DECC)*COS(DRA)-1.D0)
     *         * W(I4))
            TC = EXP (-ALF) * ANTFAC * PMODEL(1,J)
            IMMOD = TC * SIN (PHI)
            REMOD = TC * COS (PHI)
            SUMRE = SUMRE + REMOD
            SUMIM = SUMIM + IMMOD
            IF (NGRAD) GO TO 80
C                                       Gradient wanted - do partials
C                                       Partial wrt flux
            IP = MODPNT(1,J)
            IF (IP.GT.0) THEN
               PARTRE(IP) = REMOD * PMODEL(7,J)
               PARTIM(IP) = IMMOD * PMODEL(7,J)
               END IF
C                                       Partial wrt east-west offset
            IP = MODPNT(2,J)
            IF (IP.GT.0) THEN
C              TTEMP = C3 * U(I4) - C4 * W(I4) * PMODEL(2,J)
               TEMP = (AS2RAD * TWOPI * COS(DEC)/COS(DEC0)) *
     *            (COS(DRA)*U(I4) + SIN(DEC0)*SIN(DRA)*V(I4) -
     *            COS(DEC0)*SIN(DRA)*W(I4))
               PARTRE(IP) = - IMMOD * TEMP
               PARTIM(IP) = REMOD * TEMP
               END IF
C                                       Partial wrt north-south offset
            IP = MODPNT(3,J)
            IF (IP.GT.0) THEN
C               TTEMP = C3 * V(I4) - C4 * W(I4) * PMODEL(3,J)
               TEMP = AS2RAD * TWOPI * (-SIN(DRA)*SIN(DECC)*U(I4) +
     *            (COS(DEC0)*COS(DEC)+SIN(DEC0)*SIN(DECC)*COS(DRA))*
     *            V(I4) + W(I4) *
     *            (COS(DECC)*SIN(DEC0) - COS(DEC0)*SIN(DECC)*COS(DRA)))
               PARTRE(IP) = - IMMOD * TEMP
               PARTIM(IP) = REMOD * TEMP
               END IF
C                                       Partial wrt major axis
            IP = MODPNT(4,J)
            IF (IP.GT.0) THEN
               TEMP = T1 * T1
C                                       Trap for circular gaussian
               IF (MODPNT(5,J).LE.-1) TEMP = TT
               TEMP = 2.0 * C1 * PMODEL(4,J) * TEMP
               PARTRE(IP) = -REMOD * TEMP
               PARTIM(IP) = -IMMOD * TEMP
               END IF
C                                       Partial wrt major axis
            IP = MODPNT(5,J)
            IF (IP.GT.0) THEN
               TEMP = 2.0 * C1 * PMODEL(5,J) * T2 * T2
               PARTRE(IP) = -REMOD * TEMP
               PARTIM(IP) = -IMMOD * TEMP
               END IF
C                                       Partial wrt position angle
            IP = MODPNT(6,J)
            IF (IP.GT.0) THEN
               TEMP = 2.0 * C2 * T1 * T2 * (W1(J) - W2(J))
               PARTRE(IP) = -REMOD * TEMP
               PARTIM(IP) = -IMMOD * TEMP
               END IF
 80         CONTINUE
C                                       Compute residuals
         IF (DOAMP .OR. DOREAL) THEN
C                                       Amplitude only
            AMPMOD = SQRT (SUMRE*SUMRE + SUMIM*SUMIM)
            RESRE = AMPMOD - RE(I4)
            RESIM = 0.0
            IF (AMPMOD.LT.1.0E-20) AMPMOD = 1.0E-20
         ELSE
C                                       Full complex data
            RESRE = SUMRE - RE(I4)
            RESIM = SUMIM - IM(I4)
            END IF
C                                       Sum Chi squares
         SUM = SUM + WT(I4) * (RESRE*RESRE + RESIM*RESIM)
         IF (NGRAD) GO TO 100
C                                       Do antenna gain partials
            DO 85 I = 1,30
               IF (GAPNT(I).LE.0) GO TO 85
                  IP = GAPNT(I)
                  PARTRE(IP) = 0.0
                  PARTIM(IP) = 0.0
                  IF ((I.NE.IANT1(I4)) .AND. (I.NE.IANT2(I4)))
     *               GO TO 85
                     PARTRE(IP) = - SUMRE * GN(I) * GN(I)
                     PARTIM(IP) = - SUMIM * GN(I) * GN(I)
 85                  CONTINUE
C                                       Do gradients.
            IF (DOAMP .OR. DOREAL) THEN
C                                       Amplitude only
               DO 90 I = 1,NVAR
                  GRAD(I) = GRAD(I) + WT(I4) * 2.0 *
     *               (RESRE * (SUMRE * PARTRE(I) + SUMIM * PARTIM(I)) /
     *                AMPMOD)
 90               CONTINUE
            ELSE
C                                       Full complex data
               DO 95 I = 1,NVAR
                  GRAD(I) = GRAD(I) + WT(I4) * 2.0 *
     *               (PARTRE(I)*RESRE + PARTIM(I)*RESIM)
 95               CONTINUE
               END IF
 100     CONTINUE
      F = SUM
C
 999  RETURN
      END
      SUBROUTINE UVFUN2 (P, F, GRAD, IFLAG)
C-----------------------------------------------------------------------
C   Uniform sphere model
C   Given the vector P of solution parameters, this subroutine computes
C   the value of the chi-squared function F (a sum of squared residuals)
C   and, optionally, the gradient, GRAD, of F w.r.t. P.  When IFLAG=1,
C   only F is computed.  Otherwise F and GRAD are both computed.  Note
C   that P is to contain only the parameters which are being solved for
C   --- not the parameters that are to be held fixed.  This subroutine
C   is called by the minimization routine DVDMIN.
C
C   The data is contained in common /UVDATA/, fixed values of the
C   parameters are in common /INPARM/.  Other information is in common
C   /INFO/.
C    Inputs:
C     P(NVAR)    D    Vector of least-squares solution pararameters.
C     IFLAG      I    IFLAG=1 ==> compute just F,
C                     IFLAG.NE.1 ==> compute both F and GRAD.
C    Inputs from common:
C     NUMVIS     I    Number of visibility points
C     U(*),V(*)  R    U and v coordinates of data
C     RE(*)      R    Real part of data (Amplitude if DOAMP)
C     IM(*)      R    Imaginary part of data
C     WT(*)      R    Weights for data
C     IANT1(*)   I    first antenna number
C     IANT2(*)   I    second antenna number
C     DOAMP      L    IF true fit only amplitudes.
C     DOREAL     L    If true fit only reals
C     NGAUSS     I    Number of Spheres
C     GMAX(*)    R    Array of fluxes to use for fixed parms.
C     GPOS(2,*)  R    Array of positions to use for fixed parms.
C     GWID(3,*)  R    Array of size to use for fixed parms (uses 1 only)
C     MODPNT(6,*)I    Array of pointers in P for values varies,
C                     0 => parameter fixed.
C     GAPNT(*)   I    Array of pointers in P for antenna gains varied
C                     0 => parameter fixed.  One per antenna.
C     SIZE(2)    R    Lower and upper bound on component size (Asec)
C
C   Outputs:
C     P(NVAR)    D    Enforces limit on component size.
C     F          D    The value of the chi-squared function
C                     corresponding to the given P.
C     GRAD(NVAR) D    The gradient of the chi-squared function.  I.e.,
C                     GRAD(I) = derivative of F w.r.t. P(I).
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
      INTEGER   IFLAG, I, J, IP, IP1, IP2, I4
      LOGICAL   NGRAD
      REAL      W1(XMXCMP), TT, TC, REMOD, IMMOD, SUMRE, SUMIM, TEMP,
     *   RESRE, RESIM, PMODEL(5,XMXCMP), GN(30), ANTFAC, CP, SP, AMPMOD
      DOUBLE PRECISION    P(*), F, GRAD(*), SUM, C1, AA, CA, SA, PHI,
     *   DRA, DECC, DEC0
      REAL      PARTRE(XMXPRM), PARTIM(XMXPRM)
      DATA C1 /3.046174D-5/
C      DATA C3 /3.0461742E-5/
C      DATA C4 /1.4768269E-10/
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      NGRAD = IFLAG.EQ.1
C                                       Get total current model
      DO 10 I = 1,NGAUSS
C                                       Peak
         PMODEL(1,I) = 3.0 * GMAX(I)
         IP = MODPNT(1,I)
         IF (IP.GT.0) PMODEL(1,I) = 3.0 * P(IP)
         PMODEL(5,I) = 3.0 / (PMODEL(1,I) + 1.0E-20)
C                                       Position
         PMODEL(2,I) = GPOS(1,I)
         IP = MODPNT(2,I)
         IF (IP.GT.0) PMODEL(2,I) = P(IP)
         PMODEL(3,I) = GPOS(2,I)
         IP = MODPNT(3,I)
         IF (IP.GT.0) PMODEL(3,I) = P(IP)
C                                       Size
         PMODEL(4,I) = GWIDTH(1,I)
         IP = MODPNT(4,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(4,I) = P(IP)
C                                       Set some constants
         W1(I) = C1 * PMODEL(4,I)
 10      CONTINUE
C                                       Get gains
      DO 15 I = 1,NANT
         GN(I) = 1.0 / GAIN(I)
         IP = GAPNT(I)
C                                       Make sure gains positive
         P(IP) = ABS (P(IP))
         IF (IP.GT.0) GN(I) = 1.0 / P(IP)
 15      CONTINUE
C                                       Clear GRAD
      IF (.NOT.NGRAD) THEN
         DO 20 I = 1,NVAR
            GRAD(I) = 0.0D0
 20         CONTINUE
         END IF
C                                       Compute Chi squares
C                                       sum (wt(res(re)**2+res(im)**2))
      SUM = 0.0D0
C                                       Loop over data
      DEC0 = DEC * DG2RAD
      DO 100 I4 = 1,NUMVIS
         IF (WT(I4).LE.0.0) GO TO 100
C                                       Compute model - loop over gauss.
         SUMRE = 0.0
         SUMIM = 0.0
         IP1 = IANT1(I4)
         IP2 = IANT2(I4)
         ANTFAC = GN(IP1) * GN(IP2)
         TT = SQRT (U(I4) * U(I4) + V(I4) * V(I4))
         DO 80 J = 1,NGAUSS
            AA = W1(J) * TT
C                                       Trap very unresolved.
C                                       This needed to prevent
C                                       serious precision loss.
            IF (AA.LT.6.28D-2) AA = 6.28D-2
            CA = COS (AA)
            SA = SIN (AA)
            DRA = AS2RAD * PMODEL(2,J) / COS (DEC0)
            DECC = AS2RAD * PMODEL(3,J) + DEC0
            PHI = TWOPI * ((SIN(DRA)*COS(DECC)) * U(I4) + V(I4) *
     *         (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *         (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)-1.D0)
     *         * W(I4))
C            PHI = C3 * (PMODEL(2,J)*U(I4) + PMODEL(3,J)*V(I4)) -
C     *         0.5 * C4 * W(I4) * (PMODEL(2,J)*PMODEL(2,J) +
C     *         PMODEL(3,J)*PMODEL(3,J))
            CP = COS (PHI)
            SP = SIN (PHI)
            TC = ANTFAC * PMODEL(1,J) * ((SA/(AA*AA*AA)) -
     *         (CA/(AA*AA)))
            IMMOD = TC * SP
            REMOD = TC * CP
            SUMRE = SUMRE + REMOD
            SUMIM = SUMIM + IMMOD
            IF (NGRAD) GO TO 80
C                                       Gradient wanted - do partials
C                                       Partial wrt flux
            IP = MODPNT(1,J)
            IF (IP.GT.0) THEN
               PARTRE(IP) = REMOD * PMODEL(5,J)
               PARTIM(IP) = IMMOD * PMODEL(5,J)
               END IF
C                                       Partial wrt east-west offset
            IP = MODPNT(2,J)
            IF (IP.GT.0) THEN
C               TEMP = C3 * U(I4) - C4 * W(I4) * PMODEL(2,J)
               TEMP = (AS2RAD * TWOPI * COS(DEC)/COS(DEC0)) *
     *            (COS(DRA)*U(I4) + SIN(DEC0)*SIN(DRA)*V(I4) -
     *            COS(DEC0)*SIN(DRA)*W(I4))
               PARTRE(IP) = - IMMOD * TEMP
               PARTIM(IP) = REMOD * TEMP
               END IF
C                                       Partial wrt north-south offset
            IP = MODPNT(3,J)
            IF (IP.GT.0) THEN
C               TEMP = C3 * V(I4) - C4 * W(I4) * PMODEL(3,J)
               TEMP = AS2RAD * TWOPI * (-SIN(DRA)*SIN(DECC)*U(I4) +
     *            (COS(DEC0)*COS(DEC)+SIN(DEC0)*SIN(DECC)*COS(DRA))*
     *            V(I4) + W(I4) *
     *            (COS(DECC)*SIN(DEC0) - COS(DEC0)*SIN(DECC)*COS(DRA)))
               PARTRE(IP) = - IMMOD * TEMP
               PARTIM(IP) = REMOD * TEMP
               END IF
C                                       Partial wrt size.
            IP = MODPNT(4,J)
            IF (IP.GT.0) THEN
               TEMP = PMODEL(1,J) * C1 * TT / (AA*AA*AA*AA) * ANTFAC
               TEMP = TEMP * ( SA * (AA*AA - 3.0) + 3.0 * AA * CA)
               PARTRE(IP) = CP * TEMP
               PARTIM(IP) = SP * TEMP
               END IF
 80         CONTINUE
C                                       Compute residuals
C                                       Amplitude only
         IF (DOAMP .OR. DOREAL) THEN
            AMPMOD = SQRT (SUMRE*SUMRE + SUMIM*SUMIM)
            RESRE = AMPMOD - RE(I4)
            RESIM = 0.0
            IF (AMPMOD.LT.1.0E-20) AMPMOD = 1.0E-20
C                                       Full complex data
         ELSE
            RESRE = SUMRE - RE(I4)
            RESIM = SUMIM - IM(I4)
            END IF
C                                       Sum Chi squares
         SUM = SUM + WT(I4) * (RESRE*RESRE + RESIM*RESIM)
         IF (NGRAD) GO TO 100
C                                       Do antenna gain partials
            DO 85 I = 1,30
               IF (GAPNT(I).LE.0) GO TO 85
                  IP = GAPNT(I)
                  PARTRE(IP) = 0.0
                  PARTIM(IP) = 0.0
                  IF ((I.NE.IANT1(I4)) .AND. (I.NE.IANT2(I4)))
     *               GO TO 85
                     PARTRE(IP) = - SUMRE * GN(I) * GN(I)
                     PARTIM(IP) = - SUMIM * GN(I) * GN(I)
 85                  CONTINUE
C                                       Do gradients.
            IF (DOAMP .OR. DOREAL) THEN
C                                       Amplitude only
               DO 90 I = 1,NVAR
                  GRAD(I) = GRAD(I) + WT(I4) * 2.0 *
     *               (RESRE * (SUMRE * PARTRE(I) + SUMIM * PARTIM(I)) /
     *                AMPMOD)
 90               CONTINUE
            ELSE
C                                       Full complex data
               DO 95 I = 1,NVAR
                  GRAD(I) = GRAD(I) + WT(I4) * 2.0 *
     *               (PARTRE(I)*RESRE + PARTIM(I)*RESIM)
 95               CONTINUE
               END IF
 100     CONTINUE
      F = SUM
C
 999  RETURN
      END
      SUBROUTINE UVRMS1 (P, RMS)
C-----------------------------------------------------------------------
C   Eliptical Gaussian model
C   Given the vector P of solution parameters, this subroutine computes
C   the RMS residual.
C
C   The data is contained in common /UVDATA/, fixed values of the
C   parameters are in common /INPARM/.  Other information is in common
C   /INFO/.
C    Inputs:
C     P(NVAR)    D    Vector of least-squares solution pararameters.
C    Inputs from common:
C     NUMVIS     I    Number of visibility points
C     U(*),V(*)  R    U and v coordinates of data
C     RE(*)      R    Real part of data (Amplitude if DOAMP)
C     IM(*)      R    Imaginary part of data
C     WT(*)      R    Weights for data
C     IANT1(*)   I    first antenna number
C     IANT2(*)   I    second antenna number
C     DOAMP      L    IF true fit only amplitudes.
C     DOREAL     L    If true fit only reals
C     NGAUSS     I    Number of Gaussians
C     GMAX(*)    R    Array of peak values to use for fixed parms.
C     GPOS(2,*)  R    Array of positions to use for fixed parms.
C     GWID(3,*)  R    Array of sizes to use for fixed parms.
C     MODPNT(6,*)I    Array of pointers in P for values varies,
C                     0 => parameter fixed.  One per model parameter.
C                     MODPNT(5,n) = -1 => circular gaussian.
C     GAPNT(*)   I    Array of pointers in P for antenna gains varied
C                     0 => parameter fixed.  One per antenna.
C     SIZE(2)    R    Lower and upper bound on component size (Asec)
C
C   Outputs:
C     RMS        D    RMS residual (Jy)
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(*), RMS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
C
      INTEGER   I, J, IP, IP1, IP2, I4, COUNT
      REAL      W1(XMXCMP), W2(XMXCMP), TH, ST(XMXCMP), CT(XMXCMP), T1,
     *   T2, TT, TC, ALF, REMOD, IMMOD, SUMRE, SUMIM, RESRE, RESIM,
     *   PMODEL(7,XMXCMP), GN(30), ANTFAC, AMPMOD, C1, C2
      DOUBLE PRECISION SUM, PHI, DRA, DECC, DEC0
      DATA C1 /8.3668894E-11/
      DATA C2 /1.7453292E-2/
C      DATA C3 /3.0461742E-5/
C      DATA C4 /1.4768269E-10/
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Get total current model
      DO 10 I = 1,NGAUSS
C                                       Peak
         PMODEL(1,I) = GMAX(I)
         IP = MODPNT(1,I)
         IF (IP.GT.0) PMODEL(1,I) = P(IP)
         PMODEL(7,I) = 1.0 / (PMODEL(1,I) + 1.0E-20)
C                                       Position
         PMODEL(2,I) = GPOS(1,I)
         IP = MODPNT(2,I)
         IF (IP.GT.0) PMODEL(2,I) = P(IP)
         PMODEL(3,I) = GPOS(2,I)
         IP = MODPNT(3,I)
         IF (IP.GT.0) PMODEL(3,I) = P(IP)
C                                       Size
         PMODEL(4,I) = GWIDTH(1,I)
         IP = MODPNT(4,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(4,I) = P(IP)
C                                       Circular gaussian trap.
         IP = MODPNT(5,I)
         IF (IP.LE.-1) GWIDTH(2,I) = PMODEL(4,I)
         PMODEL(5,I) = GWIDTH(2,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(5,I) = P(IP)
         PMODEL(6,I) = GWIDTH(3,I)
         IP = MODPNT(6,I)
         IF (IP.GT.0) PMODEL(6,I) = P(IP)
C                                       Set some constants
         W1(I) = C1 * PMODEL(4,I) * PMODEL(4,I)
         W2(I) = C1 * PMODEL(5,I) * PMODEL(5,I)
         TH = C2 * PMODEL(6,I)
         ST(I) = SIN (TH)
         CT(I) = COS (TH)
 10      CONTINUE
C                                       Get gains
      DO 15 I = 1,NANT
         GN(I) = 1.0 / GAIN(I)
         IP = GAPNT(I)
C                                       Make sure gains positive
         P(IP) = ABS (P(IP))
         IF (IP.GT.0) GN(I) = 1.0 / P(IP)
 15      CONTINUE
C                                       Compute RMS
C                                       sum (res(re)**2+res(im)**2)
      SUM = 0.0D0
      COUNT = 0
C                                       Loop over data
      DEC0 = DEC * DG2RAD
      DO 100 I4 = 1,NUMVIS
         IF (WT(I4).LE.0.0) GO TO 100
C                                       Compute model - loop over gauss.
         SUMRE = 0.0
         SUMIM = 0.0
         IP1 = IANT1(I4)
         IP2 = IANT2(I4)
         ANTFAC = GN(IP1) * GN(IP2)
         TT = U(I4) * U(I4) + V(I4) * V(I4)
         DO 80 J = 1,NGAUSS
            T1 = U(I4)*ST(J) + V(I4)*CT(J)
            T2 = U(I4)*CT(J) - V(I4)*ST(J)
            ALF = W1(J)*T1*T1 + W2(J)*T2*T2
            DRA = AS2RAD * PMODEL(2,J) / COS (DEC0)
            DECC = AS2RAD * PMODEL(3,J) + DEC0
C            PHII = C3 * (PMODEL(2,J)*U(I4) + PMODEL(3,J)*V(I4)) -
C     *         0.5 * C4 * W(I4) * (PMODEL(2,J)*PMODEL(2,J) +
C     *         PMODEL(3,J)*PMODEL(3,J))
            PHI = TWOPI * ((SIN(DRA)*COS(DECC)) * U(I4) + V(I4) *
     *         (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *         (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)-1.D0)
     *         * W(I4))
            TC = EXP (-ALF) * ANTFAC * PMODEL(1,J)
            IMMOD = TC * SIN (PHI)
            REMOD = TC * COS (PHI)
            SUMRE = SUMRE + REMOD
            SUMIM = SUMIM + IMMOD
 80         CONTINUE
C                                       Compute residuals
C                                       Amplitude only
         IF (DOAMP .OR. DOREAL) THEN
            AMPMOD = SQRT (SUMRE*SUMRE + SUMIM*SUMIM)
            RESRE = AMPMOD - RE(I4)
            RESIM = 0.0
            IF (AMPMOD.LT.1.0E-20) AMPMOD = 1.0E-20
C                                       Full complex data
         ELSE
            RESRE = SUMRE - RE(I4)
            RESIM = SUMIM - IM(I4)
            END IF
C                                       Sum Chi squares
         SUM = SUM + (RESRE*RESRE + RESIM*RESIM)
         COUNT = COUNT + 1
 100     CONTINUE
      IF (COUNT.GE.1) THEN
         RMS = SQRT (SUM / COUNT)
      ELSE
         RMS = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE UVRMS2 (P, RMS)
C-----------------------------------------------------------------------
C   Uniform sphere model
C   Given the vector P of solution parameters, this subroutine computes
C   the RMS residual.
C
C   The data is contained in common /UVDATA/, fixed values of the
C   parameters are in common /INPARM/.  Other information is in common
C   /INFO/.
C    Inputs:
C     P(NVAR)    D    Vector of least-squares solution pararameters.
C    Inputs from common:
C     NUMVIS     I    Number of visibility points
C     U(*),V(*)  R    U and v coordinates of data
C     RE(*)      R    Real part of data (Amplitude if DOAMP)
C     IM(*)      R    Imaginary part of data
C     WT(*)      R    Weights for data
C     IANT1(*)   I    first antenna number
C     IANT2(*)   I    second antenna number
C     DOAMP      L    IF true fit only amplitudes.
C     DOREAL     L    If true fit only reals
C     NGAUSS     I    Number of Spheres
C     GMAX(*)    R    Array of fluxes to use for fixed parms.
C     GPOS(2,*)  R    Array of positions to use for fixed parms.
C     GWID(3,*)  R    Array of size to use for fixed parms (uses 1 only)
C     MODPNT(6,*)I    Array of pointers in P for values varies,
C                     0 => parameter fixed.
C     GAPNT(*)   I    Array of pointers in P for antenna gains varied
C                     0 => parameter fixed.  One per antenna.
C     SIZE(2)    R    Lower and upper bound on component size (Asec)
C
C   Outputs:
C     RMS        D    RMS residual (Jy)
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(*), RMS
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIT.INC'
      INTEGER   I, J, IP, IP1, IP2, I4, COUNT
      REAL      W1(XMXCMP), TT, TC, REMOD, IMMOD, SUMRE, SUMIM, RESRE,
     *   RESIM, PMODEL(5,XMXCMP), GN(30), ANTFAC, CP, SP, AMPMOD
      DOUBLE PRECISION SUM, C1, AA, CA, SA, PHI, DRA, DECC, DEC0
      DATA C1 /3.046174D-5/
C      DATA C3 /3.0461742E-5/
C      DATA C4 /1.4768269E-10/
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Get total current model
      DO 10 I = 1,NGAUSS
C                                       Peak
         PMODEL(1,I) = 3.0 * GMAX(I)
         IP = MODPNT(1,I)
         IF (IP.GT.0) PMODEL(1,I) = 3.0 * P(IP)
         PMODEL(5,I) = 3.0 / (PMODEL(1,I) + 1.0E-20)
C                                       Position
         PMODEL(2,I) = GPOS(1,I)
         IP = MODPNT(2,I)
         IF (IP.GT.0) PMODEL(2,I) = P(IP)
         PMODEL(3,I) = GPOS(2,I)
         IP = MODPNT(3,I)
         IF (IP.GT.0) PMODEL(3,I) = P(IP)
C                                       Size
         PMODEL(4,I) = GWIDTH(1,I)
         IP = MODPNT(4,I)
         IF ((IP.GT.0) .AND. (P(IP).LT. SIZE(1)))
     *      P(IP) = SIZE(1)
         IF ((IP.GT.0) .AND. (P(IP).GT. SIZE(2)))
     *      P(IP) = SIZE(2)
         IF (IP.GT.0) PMODEL(4,I) = P(IP)
C                                       Set some constants
         W1(I) = C1 * PMODEL(4,I)
 10      CONTINUE
C                                       Get gains
      DO 15 I = 1,NANT
         GN(I) = 1.0 / GAIN(I)
         IP = GAPNT(I)
C                                       Make sure gains positive
         P(IP) = ABS (P(IP))
         IF (IP.GT.0) GN(I) = 1.0 / P(IP)
 15      CONTINUE
C                                       Compute RMS
C                                       sum (res(re)**2+res(im)**2)
      SUM = 0.0D0
      COUNT = 0
C                                       Loop over data
      DEC0 = DEC * DG2RAD
      DO 100 I4 = 1,NUMVIS
         IF (WT(I4).LE.0.0) GO TO 100
C                                       Compute model - loop over gauss.
         SUMRE = 0.0
         SUMIM = 0.0
         IP1 = IANT1(I4)
         IP2 = IANT2(I4)
         ANTFAC = GN(IP1) * GN(IP2)
         TT = SQRT (U(I4) * U(I4) + V(I4) * V(I4))
         DO 80 J = 1,NGAUSS
            AA = W1(J) * TT
C                                       Trap very unresolved.
C                                       This needed to prevent
C                                       serious precision loss.
            IF (AA.LT.6.28D-2) AA = 6.28D-2
            CA = COS (AA)
            SA = SIN (AA)
            DRA = AS2RAD * PMODEL(2,J) / COS (DEC0)
            DECC = AS2RAD * PMODEL(3,J) + DEC0
C           PPHI = C3 * (PMODEL(2,J)*U(I4) + PMODEL(3,J)*V(I4)) -
C     *         0.5 * C4 * W(I4) * (PMODEL(2,J)*PMODEL(2,J) +
C     *         PMODEL(3,J)*PMODEL(3,J))
            PHI = TWOPI * ((SIN(DRA)*COS(DECC)) * U(I4) + V(I4) *
     *         (COS(DEC0)*SIN(DECC) - SIN(DEC0)*COS(DECC)*COS(DRA)) +
     *         (SIN(DEC0)*SIN(DECC) + COS(DEC0)*COS(DECC)*COS(DRA)-1.D0)
     *         * W(I4))
            CP = COS (PHI)
            SP = SIN (PHI)
            TC = ANTFAC * PMODEL(1,J) * ((SA/(AA*AA*AA)) -
     *         (CA/(AA*AA)))
            IMMOD = TC * SP
            REMOD = TC * CP
            SUMRE = SUMRE + REMOD
            SUMIM = SUMIM + IMMOD
 80         CONTINUE
C                                       Compute residuals
C                                       Amplitude only
         IF (DOAMP .OR. DOREAL) THEN
            AMPMOD = SQRT (SUMRE*SUMRE + SUMIM*SUMIM)
            RESRE = AMPMOD - RE(I4)
            RESIM = 0.0
            IF (AMPMOD.LT.1.0E-20) AMPMOD = 1.0E-20
C                                       Full complex data
         ELSE
            RESRE = SUMRE - RE(I4)
            RESIM = SUMIM - IM(I4)
            END IF
C                                       Sum Chi squares
         SUM = SUM + (RESRE*RESRE + RESIM*RESIM)
         COUNT = COUNT + 1
 100     CONTINUE
      IF (COUNT.GE.1) THEN
         RMS = SQRT (SUM / COUNT)
      ELSE
         RMS = 0.0
         END IF
C
 999  RETURN
      END

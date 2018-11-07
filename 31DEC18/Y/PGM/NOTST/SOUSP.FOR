LOCAL INCLUDE 'SOUSP.INC'
C                                       Local include for SOUSP
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SEQIN, CNOIN, DISKIN, NFREQ, ORDER, GRCHAN, IQUAL,
     *   SNV1, SNV2, SRCNUM, SCRTCH(256), NPARMS, SURECN
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALC
      CHARACTER NAMEIN*12, CLAIN*6, SOURCE*16, UCALC*4
      REAL      XSIN, XDISIN, XQUAL, XORD, REFREQ, NOISE(64),
     *   FLDSIZ(2,64), DOCONF, XSNV, XINV, XSYM, FACTOR, XPIXR(2),
     *   XDOTV, XGRCH, SPFLUX, SPECIN, SPECUR(3), XTRPRM(5)
      DOUBLE PRECISION XNOISE(MAXIF), XFLUX(MAXIF), XFREQ(MAXIF), DEFREQ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XCALC,
     *   XQUAL, XORD, REFREQ, NOISE, FLDSIZ, DOCONF, XSNV, XINV, XSYM,
     *   FACTOR, XPIXR, XDOTV, XGRCH, SPFLUX, SPECIN, SPECUR, XTRPRM
      COMMON /INFO/ XFLUX, XFREQ, XNOISE, DEFREQ, SCRTCH, SEQIN, DISKIN,
     *   CNOIN, NFREQ, ORDER, GRCHAN, IQUAL, NPARMS, SURECN, SNV1, SNV2,
     *   SRCNUM
      COMMON /CHRCOM/ NAMEIN, CLAIN, SOURCE, UCALC
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM SOUSP
C-----------------------------------------------------------------------
C! Fits and plots spectral index parameters to source fluxes
C# task UV calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2012, 2015, 2017-2018
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
C   Fits input or source table fluxes to find spectral index parameters
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      SOURCES        SOURCS        Source list.
C-----------------------------------------------------------------------
      INCLUDE 'SOUSP.INC'
C
      CHARACTER PRGM*6
      INTEGER   IRET, IERR
      DOUBLE PRECISION BFIT(6)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SOUSP '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL SOUSIN (PRGM, IRET)
      IF (DOCONF.GT.0.0) RQUICK = .FALSE.
      CALL DFILL (5, 0.0D0, BFIT)
C                                       Loop over sources.
      IF (IRET.EQ.0) CALL SOUSIT (BFIT, IRET)
C                                       set adverbs for later return
C                                       and PL file header
      IF (IRET.EQ.0) THEN
         SPFLUX = 10.0 ** BFIT(1)
         SPECIN = BFIT(2)
         SPECUR(1) = BFIT(3)
         SPECUR(2) = BFIT(4)
         SPECUR(3) = BFIT(5)
         XTRPRM(2) = BFIT(1)
         XTRPRM(3) = BFIT(6)
      ELSE
         CALL RFILL (5, 0.0, SPFLUX)
         END IF
      IF (RQUICK) THEN
         CALL PTPARM (5, SPFLUX, SCRTCH, IERR)
         CALL RELPOP (IRET, SCRTCH, IERR)
         END IF
C                                       report results
      IF ((IRET.EQ.0) .AND. (XDOTV.NE.0.0)) CALL SOUSPL (BFIT, IRET)
C                                       change the SU table
      IF ((IRET.EQ.0) .AND. (DOCONF.GT.0.)) CALL SOUSDO (BFIT, IRET)
C                                       change the SN table(s)
      IF ((IRET.EQ.0) .AND. (SNV1.GT.0)) CALL SOUSSN (IRET)
C                                       return parameters
      IF (.NOT.RQUICK) CALL PTPARM (5, SPFLUX, SCRTCH, IERR)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SOUSIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   SOUSIN gets input parameters for SOUSP, finds input file and
C   prepares the list of parameters.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ input file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      INCLUDE 'SOUSP.INC'
      CHARACTER STAT*4, UTYPE*2, BNDCOD(MAXIF)*8
      INTEGER   IROUND, IERR, I, LUN, VER, JJ, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), SUBUFF(512), FREQID, ISURNO, LUNTMP, NUMIF,
     *   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION REFFRQ, FOFF(MAXIF), HFREQ(MAXIF)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARMS = 332
      CALL GTPARM (PRGN, NPARMS, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
      NPARMS = NPARMS + 9
C                                       Restart AIPS
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IQUAL = IROUND (XQUAL)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
C                                       Source
      CALL H2CHR (16, 1, XXSOUR(1,1), SOURCE)
      CALL H2CHR (4, 1, XCALC, UCALC)
      ORDER = XORD
      ORDER = MAX (1, MIN (4, ORDER))
      XORD = ORDER
C                                       Get CATBLK for input file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       OK, get the header now
      STAT = 'WRIT'
      IF (XDOTV.GT.0.0) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING CATBLK'
         GO TO 990
         END IF
C                                       OK, file available
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
      IF (XDOTV.GT.0.0) FRW(NCFILE) = 0
C                                       check FLDSIZ
      NFREQ = 0
      DEFREQ = 1.D9
      DO 10 I = 1,64
         IF ((FLDSIZ(1,I).LE.0.0) .OR. (FLDSIZ(2,I).LE.0.0)) GO TO 20
         NFREQ = NFREQ + 1
         XFLUX(NFREQ) = FLDSIZ(1,I)
         XFREQ(NFREQ) = FLDSIZ(2,I)
 10      CONTINUE
 20   SNV1 = 0
      SNV2 = 0
C                                       find from source table
      IF (NFREQ.LE.0) THEN
         VER = 1
         LUN = LUNTMP (1)
         CALL SOUINI ('READ', SUBUFF, DISKIN, CNOIN, VER, CATBLK, LUN,
     *      NFREQ, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING SOURCE TABLE'
            GO TO 990
            END IF
         JJ = SUBUFF(5)
         DO 30 I = 1,JJ
            SURECN = I
            CALL TABSOU ('READ', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING SOURCE TABLE'
               GO TO 990
               END IF
            IF ((SNAME.EQ.SOURCE) .AND. ((IQUAL.LT.0) .OR.
     *         (IQUAL.EQ.QUAL))) THEN
               IF (UCALC.EQ.CALCOD) GO TO 35
               IF (UCALC.EQ.' ') GO TO 35
               IF ((UCALC.EQ.'*') .AND. (CALCOD.NE.' ')) GO TO 35
               IF ((UCALC.EQ.'-CAL') .AND. (CALCOD.EQ.' ')) GO TO 35
               END IF
 30         CONTINUE
         MSGTXT = 'SOURCE ''' // SOURCE // ''' CALCODE ''' // UCALC
     *      // ''' NOT FOUND'
         IRET = 10
         GO TO 990
 35      CALL TABSOU ('CLOS', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *      SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
C                                       FREQ
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), I, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINDING FREQ AXIS'
            GO TO 990
            END IF
         REFFRQ = CATD(KDCRV+I)
         IF (REFREQ.LT.0.0) THEN
            DEFREQ = REFFRQ
         ELSE IF (REFREQ.EQ.0.0) THEN
            DEFREQ = 1.D9
         ELSE
            DEFREQ = 1.D9 * REFREQ
            END IF
C                                       FQ table
         CALL CHNDAT ('READ', SUBUFF, DISKIN, CNOIN, VER, CATBLK,
     *      LUN, NUMIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE'
            GO TO 990
            END IF
         CALL HIGET (DISKIN, CNOIN, NFREQ, HFREQ, IRET)
         DO 40 I = 1,NFREQ
C                                       new SETJY tells freqs in HI
            IF (HFREQ(I).GT.0.0D0) THEN
               XFREQ(I) = HFREQ(I) * 1.D9 / DEFREQ
C                                       old SETJY did ref pixel
            ELSE
               XFREQ(I) = (REFFRQ + FOFF(I) + FREQO(I)) / DEFREQ
               END IF
            XFLUX(I) = FLUX(1,I)
 40         CONTINUE
         IF (DOCONF.GT.0.0) THEN
            SNV1 = IROUND (XSNV)
            IF (SNV1.GT.0) SNV2 = IROUND (XINV)
            SNV2 = MAX (SNV1, SNV2)
            END IF
      ELSE
         SURECN = 0
         DOCONF = -1.0
         END IF
C                                       noise values
      CALL DFILL (MAXIF, 0.1D0, XNOISE)
      DO 50 I = 1,NFREQ
         IF (NOISE(I).GT.0.0) XNOISE(I) = NOISE(I)
 50      CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUSIN: ERROR',I5,1X,A)
 1010 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
      END
      SUBROUTINE HIGET (DISK, CNO, NF, HFREQ, IRET)
C-----------------------------------------------------------------------
C   HIGET tries to get the frequencies from the history file
C   Inputs:
C      DISK    I      Disk number
C      CNO     I      Catalog number
C      NF      I      Number of frequencies
C      FV      D      Ref freq
C   Output
C      HFREQ   D(*)   Frequencies
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, NF, IRET
      DOUBLE PRECISION HFREQ(*)
C
      INTEGER   IHLUN, NREC, IHPTR, HIBUFF(256), IBLK, ICARD, IP, MF,
     *   ICUR, IHIND, II
      CHARACTER LINE*72
      DOUBLE PRECISION X
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MF = 0
      CALL DFILL (NF, 0.0D0, HFREQ)
C                                       open history file
      IHLUN = 27
C                                       Open history file.
      CALL HIINIT (3)
      CALL HIOPEN (IHLUN, DISK, CNO, HIBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
      IBLK = 0
      ICARD = NHILPR
      DO 20 ICUR = 1,NREC
C                                       Read next buffer.
         ICARD = ICARD + 1
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IRET)
            IF (IRET.NE.0) GO TO 100
            END IF
C                                       desired task?
         II = (ICARD-1) * NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(II), LINE)
         IF (LINE(:11).EQ.'SETJY FREQ(') THEN
            READ (LINE,1000) IP, X
            HFREQ(IP) = X
            END IF
 20      CONTINUE
      DO 30 IP = 1,NF
         IF (HFREQ(IP).GT.0.0D0) THEN
            WRITE (MSGTXT,1001) IP, HFREQ(IP)
            CALL MSGWRT (3)
            END IF
 30      CONTINUE
C                                       Close history file.
 100  CALL HICLOS (IHLUN, .FALSE., HIBUFF, II)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (11X,I2,3X,F11.6)
 1001 FORMAT ('Found in SETJY history IF',I3,' F=',F11.6,' GHz')
      END
      SUBROUTINE SOUSIT (BFIT, IRET)
C-----------------------------------------------------------------------
C   SOUSIT solves for the requested parameters from the arrays of
C   frequency, flux, and noise.
C   Output:
C      BFIT   D(6)   Fit parameters
C      IRET   I      > 0 fails
C-----------------------------------------------------------------------
      DOUBLE PRECISION BFIT(6)
      INTEGER   IRET
C
      INCLUDE 'SOUSP.INC'
      INTEGER   I, N, J, NN
      DOUBLE PRECISION X(MAXIF), Y(MAXIF), W(MAXIF), WS, SUM, TEMP
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       convert to log units
      WS = 0.0
      I = 0
      DO 20 J = 1,NFREQ
         IF (XFLUX(J).GT.0.0) THEN
            I = I + 1
            X(I) = LOG10 (XFREQ(J))
            Y(I) = LOG10 (XFLUX(J))
            W(I) = (XFLUX(J) / XNOISE(J)) ** 2
            WS = WS + W(I)
            END IF
 20      CONTINUE
      NN = I
      IF (NN.LE.0) THEN
         MSGTXT = 'NO VALID FLUXES FOUND'
         IRET = 0
         CALL MSGWRT (8)
         GO TO 999
         END IF
      WS = WS / NN
      DO 25 I = 1,NN
         W(I) = W(I) / WS
 25      CONTINUE
C                                       polino routine
      N = ORDER + 1
      CALL DFITPN (X, Y, W, N, NN, BFIT, SUM, IRET)
C                                       our own RMS
      SUM = 0.0
      DO 60 J = 1,NN
         TEMP = BFIT(1)
         DO 50 I = 2,N
            TEMP = TEMP + BFIT(I) * X(J)**(I-1)
 50         CONTINUE
         SUM = SUM + W(J) * (Y(J) - TEMP)**2
 60      CONTINUE
      SUM = SQRT (SUM)
      MSGTXT = 'Fit log flux, spectral index, curvature parameter(s)'
      CALL MSGWRT (5)
      WRITE (MSGTXT,1060) (BFIT(I), I = 1,N)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1061) 10.0**BFIT(1), SUM
      CALL MSGWRT (5)
      BFIT(6) = SUM
      WRITE (MSGTXT,1062) DEFREQ*1.D-9
      CALL MSGWRT (5)
C                                       defaults
      DO 55 I = 1,64
         IF (NOISE(I).LE.0.0) XNOISE(I) = SUM
         NOISE(I) = XNOISE(I)
 55      CONTINUE
C                                       sigma
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Fit values:',5F9.5)
 1061 FORMAT ('Flux at REFREQ',F9.4,'   rms error',1PE11.4)
 1062 FORMAT ('Reference frequency',F9.3,' GHz')
      END
      SUBROUTINE SOUSPL (BFIT, IRET)
C-----------------------------------------------------------------------
C   SOUSPL makes 2 plots of the data and fit - one logarithmic and
C   one linear
C   Inputs:
C      BFIT   D(5)   Fit parameters
C   Outputs:
C      IRET   I      > 0 => error
C-----------------------------------------------------------------------
      DOUBLE PRECISION BFIT(5)
      INTEGER   IRET
C
      INCLUDE 'SOUSP.INC'
      INTEGER   ISYM, IROUND, BUFFER(256), VER, TVCHN, LUNPL, FINDPL,
     *   LUNTMP, INCHAR, IT(3), ID(3), IERR, TVCORN(2), I, IAPARM(5),
     *   IPSIZE, ITYPE, INP, XNP
      REAL      XN, X, Y, BLC(2), TRC(2), XSCAL, YSCAL, CHOUT(4),
     *   XYRATO, DX, DY, AX(5), AY(5), XOFF, XOFL
      DOUBLE PRECISION XMIN(2), YMIN(2), XMAX(2), YMAX(2)
      CHARACTER PFILE*48, CHTMP*20, TEXT*80, ATIME*8, ADATE*12
      LOGICAL   DOTV, UP
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       set general parameters
      IF (FACTOR.LT.0.01) FACTOR = 1.0
      ISYM = IROUND (XSYM)
      IF ((ISYM.LT.1) .OR. (ISYM.GT.22)) ISYM = 2
      DOTV = XDOTV.GT.0.0
      GRCHAN = XGRCH + 0.1
      TVCHN = 1
      TVCORN(1) = 0
      TVCORN(2) = 0
      LUNPL = LUNTMP (1)
      CALL FILL (5, 1, IAPARM)
C                                       get plot ranges
      XMIN(1) = 1.E10
      XMIN(2) = 1.E10
      XMAX(1) = -1.E10
      XMAX(2) = -1.E10
      YMIN(1) = 1.E10
      YMIN(2) = 1.E10
      YMAX(1) = -1.E10
      YMAX(2) = -1.E10
      XOFF = DEFREQ/1.D9
      XOFL = LOG10 (XOFF)
      DO 10 I = 1,NFREQ
         XMIN(2) = MIN (XMIN(2), XFREQ(I))
         XMAX(2) = MAX (XMAX(2), XFREQ(I))
         IF (XFLUX(I).GT.0.0) THEN
            YMIN(2) = MIN (YMIN(2), MAX (0.0D0, XFLUX(I)-XNOISE(I)))
            YMAX(2) = MAX (YMAX(2), XFLUX(I)+XNOISE(I))
            YMIN(1) = MIN (YMIN(1), LOG10 (MAX (1.D-6,
     *         XFLUX(I)-XNOISE(I))))
            YMAX(1) = MAX (YMAX(1), LOG10(XFLUX(I)+XNOISE(I)))
            END IF
 10      CONTINUE
      XMAX(2) = XMAX(2) * XOFF
      XMIN(2) = XMIN(2) * XOFF
      XMAX(1) = LOG10 (XMAX(2))
      XMIN(1) = LOG10 (XMIN(2))
      IF ((XPIXR(2).GT.XPIXR(1)) .AND. (XPIXR(1).GT.0.0)) THEN
         YMIN(1) = LOG10 (XPIXR(1))
         YMAX(1) = LOG10 (XPIXR(2))
         YMIN(2) = XPIXR(1)
         YMAX(2) = XPIXR(2)
         END IF
C                                       log - log plot
      X = XMAX(1) - XMIN(1)
      XMAX(1) = XMAX(1) + 0.05 * X
      XMIN(1) = XMIN(1) - 0.05 * X
      X = YMAX(1) - YMIN(1)
      YMAX(1) = YMAX(1) + 0.05 * X
      YMIN(1) = YMIN(1) - 0.05 * X
      BLC(1) = 0.0
      TRC(1) = 1000.0
      BLC(2) = 0.0
      TRC(2) = 1000.0
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = 'LOG10'
      CPREF(2,LOCNUM) = 'LOG10'
      CTYP(1,LOCNUM) = ' FREQUENCY'
      CTYP(2,LOCNUM) = ' FLUX Jy'
      XSCAL = 1000.0 / (XMAX(1) - XMIN(1))
      YSCAL = 1000.0 / (YMAX(1) - YMIN(1))
      RPLOC(1,LOCNUM) = BLC(1)
      RPLOC(2,LOCNUM) = BLC(2)
      RPVAL(1,LOCNUM) = XMIN(1) + 9.0D0
      RPVAL(2,LOCNUM) = YMIN(1)
      AXINC(1,LOCNUM) = 1.0 / XSCAL
      AXINC(2,LOCNUM) = 1.0 / YSCAL
C                                       Update catalog header.
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFFER, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 960
      IPSIZE = 0
      ITYPE = 45
      XNP = NPARMS
      XTRPRM(1) = 1.0
      CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, XNP, XNAMEI,
     *   DOTV, TVCHN, GRCHAN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 960
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
      CHOUT(1) = INP + 5.5
      CHOUT(2) = 3.333
      CHOUT(4) = 3.333
      IF (DOTV) THEN
         X = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *      + CHOUT(3))
         Y = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *      + CHOUT(4))
         XYRATO = 1.0
         IF (Y.GT.0.0) XYRATO = X / Y
      ELSE
         XYRATO = 1.0
         END IF
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1010) VER
         CALL MSGWRT (2)
         END IF
C                                       border
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Data information
      DX = 0.0
      DY = 0.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      TEXT = 'Spectral index log-log plot for ' // SOURCE
      CHTMP = NAMEIN // CLAIN
      CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(50:), INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
      TEXT = ' '
      DY = DY + 1.333
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1020) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       label axis
      CALL CLAB1 (BLC, TRC, CHOUT, 3, XYRATO, .FALSE., BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       parameter values
      CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = -15.0
      DY = -5.0
      WRITE (TEXT,1030) 'F@1', 10.0**BFIT(1)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DY = DY - 1.333
      WRITE (TEXT,1030) 'SpI', BFIT(2)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 15 I = 3,ORDER+1
         CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DY = DY - 1.333
         WRITE (TEXT,1031) 'C', I-2, BFIT(I)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
 15      CONTINUE
C                                       symbol size
      DX = 8.0 * FACTOR
      IF (DX.LT.2.5) DX = 2.5
      DY = DX
      IF (DX/XYRATO.LT.2.5) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       plot points
      DO 20 I = 1,NFREQ
         IF (XFLUX(I).GT.0.0) THEN
            X = LOG10 (XFREQ(I)*XOFF)
            Y = LOG10 (XFLUX(I))
            X = XSCAL * (X - XMIN(1))
            Y = YSCAL * (Y - YMIN(1))
            IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2))
     *         .AND. (Y.LE.TRC(2))) THEN
               AX(1) = X
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - DX
               AX(5) = AX(1) + DX
               AY(1) = Y
               AY(2) = AY(1) + DY
               AY(3) = AY(1) - DY
               AY(4) = AY(1)
               AY(5) = AY(1)
               CALL PNTPLT (ISYM, AX, AY, BLC, TRC, .FALSE., .FALSE.,
     *            BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 20      CONTINUE
C                                       plot error bars
      CALL GLTYPE (3, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 30 I = 1,NFREQ
         IF (XFLUX(I).GT.0.0) THEN
            X = LOG10 (XFREQ(I)*XOFF)
            X = XSCAL * (X - XMIN(1))
            Y = LOG10 (XFLUX(I))
            AY(1) = LOG10 (XFLUX(I)+XNOISE(I))
            AY(2) = LOG10 (MAX (1.D-6, XFLUX(I)-XNOISE(I)))
            AY(1) = YSCAL * (AY(1) - YMIN(1))
            AY(2) = YSCAL * (AY(2) - YMIN(1))
            Y = YSCAL * (Y - YMIN(1))
            IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2))
     *         .AND. (Y.LE.TRC(2))) THEN
               AY(1) = MIN (AY(1), TRC(2))
               AY(2) = MAX (AY(2), BLC(2))
               CALL GPOS (X, AY(1), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GVEC (X, AY(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 30      CONTINUE
C                                       plot fit
      CALL GLTYPE (2, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      UP = .TRUE.
      DO 40 I = 5,995,5
         X = I / XSCAL + XMIN(1) - XOFL
         Y = BFIT(1) + X * (BFIT(2) + X * (BFIT(3) + X * (BFIT(4) + X *
     *      BFIT(5))))
         X = I
         Y = YSCAL * (Y - YMIN(1))
         IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2)) .AND.
     *      (Y.LE.TRC(2))) THEN
            IF (UP) THEN
               CALL GPOS (X, Y, BUFFER, IERR)
               UP = .FALSE.
            ELSE
               CALL GVEC (X, Y, BUFFER, IERR)
               END IF
            IF (IERR.NE.0) GO TO 970
            END IF
 40      CONTINUE
      GPHPAG = .TRUE.
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, I)
         IERR = 0
         END IF
      IF (IERR.LT.0) THEN
         IRET = 0
         GO TO 999
         END IF
C                                       linear plot
      X = XMAX(2) - XMIN(2)
      XMAX(2) = XMAX(2) + 0.05 * X
      XMIN(2) = XMIN(2) - 0.05 * X
      X = YMAX(2) - YMIN(2)
      YMAX(2) = YMAX(2) + 0.05 * X
      YMIN(2) = YMIN(2) - 0.05 * X
      BLC(1) = 0.0
      TRC(1) = 1000.0
      BLC(2) = 0.0
      TRC(2) = 1000.0
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = 'FREQU'
      CPREF(2,LOCNUM) = ' '
      CTYP(1,LOCNUM) = 'ENCY GHz'
      CTYP(2,LOCNUM) = 'FLUX Jy'
      XSCAL = 1000.0 / (XMAX(2) - XMIN(2))
      YSCAL = 1000.0 / (YMAX(2) - YMIN(2))
      RPLOC(1,LOCNUM) = BLC(1)
      RPLOC(2,LOCNUM) = BLC(2)
      RPVAL(1,LOCNUM) = XMIN(2)
      RPVAL(2,LOCNUM) = YMIN(2)
      AXINC(1,LOCNUM) = 1.0 / XSCAL
      AXINC(2,LOCNUM) = 1.0 / YSCAL
C                                       Update catalog header.
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFFER, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 960
      IPSIZE = 0
      ITYPE = 45
      XTRPRM(1) = 2.0
      CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, XNP, XNAMEI,
     *   DOTV, TVCHN, GRCHAN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 960
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
      CHOUT(1) = INP + 5.5
      CHOUT(2) = 3.333
      CHOUT(4) = 3.333
      IF (DOTV) THEN
         X = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *      + CHOUT(3))
         Y = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *      + CHOUT(4))
         XYRATO = 1.0
         IF (Y.GT.0.0) XYRATO = X / Y
      ELSE
         XYRATO = 1.0
         END IF
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1010) VER
         CALL MSGWRT (2)
         END IF
C                                       border
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Data information
      DX = 0.0
      DY = 0.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      TEXT = 'Spectral index lin-lin plot for ' // SOURCE
      CHTMP = NAMEIN // CLAIN
      CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(50:), INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
      TEXT = ' '
      DY = DY + 1.333
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1020) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       label axis
      CALL CLAB1 (BLC, TRC, CHOUT, 3, XYRATO, .FALSE., BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       parameter values
      CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = -15.0
      DY = -5.0
      WRITE (TEXT,1030) 'F@1', 10.0**BFIT(1)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DY = DY - 1.333
      WRITE (TEXT,1030) 'SpI', BFIT(2)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 115 I = 3,ORDER+1
         CALL GPOS (TRC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DY = DY - 1.333
         WRITE (TEXT,1031) 'C', I-2, BFIT(I)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
 115     CONTINUE
      DX = 8.0 * FACTOR
      IF (DX.LT.2.5) DX = 2.5
      DY = DX
      IF (DX/XYRATO.LT.2.5) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       plot points
      DO 120 I = 1,NFREQ
         IF (XFLUX(I).GT.0.0) THEN
            X = XFREQ(I) * XOFF
            Y = XFLUX(I)
            X = XSCAL * (X - XMIN(2))
            Y = YSCAL * (Y - YMIN(2))
            IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2))
     *         .AND. (Y.LE.TRC(2))) THEN
               AX(1) = X
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - DX
               AX(5) = AX(1) + DX
               AY(1) = Y
               AY(2) = AY(1) + DY
               AY(3) = AY(1) - DY
               AY(4) = AY(1)
               AY(5) = AY(1)
               CALL PNTPLT (ISYM, AX, AY, BLC, TRC, .FALSE., .FALSE.,
     *            BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 120     CONTINUE
C                                       plot error bars
      CALL GLTYPE (3, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 130 I = 1,NFREQ
         IF (XFLUX(I).GT.0.0) THEN
            X = XFREQ(I) * XOFF
            X = XSCAL * (X - XMIN(2))
            Y = XFLUX(I)
            XN = XNOISE(I)
            AY(1) = YSCAL * (Y + XN - YMIN(2))
            AY(2) = YSCAL * (Y - XN - YMIN(2))
            Y = YSCAL * (Y - YMIN(2))
            IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2))
     *         .AND. (Y.LE.TRC(2))) THEN
               AY(1) = MIN (AY(1), TRC(2))
               AY(2) = MAX (AY(2), BLC(2))
               CALL GPOS (X, AY(1), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GVEC (X, AY(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 130     CONTINUE
C                                       plot fit
      CALL GLTYPE (2, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      UP = .TRUE.
      DO 140 I = 5,995,5
         X = (I / XSCAL + XMIN(2)) / XOFF
         X = LOG10 (X)
         Y = BFIT(1) + X * (BFIT(2) + X * (BFIT(3) + X * (BFIT(4) + X *
     *      BFIT(5))))
         Y = 10.0 ** Y
         X = I
         Y = YSCAL * (Y - YMIN(2))
         IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1)) .AND. (Y.GE.BLC(2)) .AND.
     *      (Y.LE.TRC(2))) THEN
            IF (UP) THEN
               CALL GPOS (X, Y, BUFFER, IERR)
               UP = .FALSE.
            ELSE
               CALL GVEC (X, Y, BUFFER, IERR)
               END IF
            IF (IERR.NE.0) GO TO 970
            END IF
 140     CONTINUE
      GPHPAG = .FALSE.
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
         IERR = 0
         END IF
      IRET = 0
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  MSGTXT = 'ERROR DURING PLOT FILE CREATION'
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      IRET = 1
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'ERROR DURING PLOTTING, WILL TRY TO FINISH'
      CALL MSGWRT (6)
      GPHPAG = .FALSE.
      IRET = 2
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.LE.0) THEN
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
         END IF
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         IRET = 3
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('PLot file version',I4,'  created.')
 1020 FORMAT ('PLot file version',I4,'__created ',A,A)
 1030 FORMAT (A,F8.4)
 1031 FORMAT (A,I2,F8.4)
      END
      SUBROUTINE SOUSDO (BFIT, IRET)
C-----------------------------------------------------------------------
C   SOUSDO asks the user if he/she wants to replace the SU table values
C   with the fit and does so if asked.
C   Inputs:
C      BFIT   D(5)   The fit results
C   Outputs:
C      IRET   I      > 0 => source table issues
C-----------------------------------------------------------------------
      DOUBLE PRECISION BFIT(5)
      INTEGER   IRET
C
      INCLUDE 'SOUSP.INC'
      INTEGER   TTY(2), IERR, LUN, VER, SUKOLS(MAXSUC), SUNUMV(MAXSUC),
     *   SUBUFF(512), FREQID, ISURNO, LUNTMP, I
      DOUBLE PRECISION X, Y
      CHARACTER MSGBUF*72, CTEMP
      LOGICAL   F, T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA F, T /.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      TTY(2) = 0
      IRET = 0
      IF ((SURECN.GT.0) .AND. (DOCONF.GT.0.0)) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, F, T, T, IRET)
         IF (IRET.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1000) IRET, 'OPENING TERMINAL TO ASK QUESTION'
            GO TO 980
            END IF
         DO 10 I = 1,NFREQ
            X = LOG10 (XFREQ(I))
            Y = BFIT(1) + X * (BFIT(2) + X * (BFIT(3) + X * (BFIT(4)
     *         + X * BFIT(5))))
            Y = 10.0 ** Y
            WRITE (MSGTXT,1010) I, XFLUX(I), Y
            CALL MSGWRT (3)
 10         CONTINUE
         MSGBUF = 'Do we replace SU table fluxes with this fit?'
         CALL INQSTR (TTY, MSGBUF, 4, CTEMP, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.10)) THEN
            WRITE (MSGTXT,1000) IRET, 'ASKING THE QUESTION'
            GO TO 980
            END IF
         CALL CHLTOU (4, CTEMP)
C                                       do it
         IF (CTEMP(1:1).EQ.'Y') THEN
            LUN = LUNTMP (1)
            VER = 1
            CALL SOUINI ('WRIT', SUBUFF, DISKIN, CNOIN, VER, CATBLK,
     *         LUN, NFREQ, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS,
     *         SUNUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING SOURCE TABLE'
               GO TO 980
               END IF
            ISURNO = SURECN
            CALL TABSOU ('READ', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING SOURCE TABLE'
               GO TO 980
               END IF
            SRCNUM = IDSOUR
            DO 20 I = 1,NFREQ
               X = LOG10 (XFREQ(I))
               Y = BFIT(1) + X * (BFIT(2) + X * (BFIT(3) + X * (BFIT(4)
     *            + X * BFIT(5))))
               Y = 10.0 ** Y
               FLUX(1,I) = Y
               XFLUX(I) = SQRT (Y / XFLUX(I))
 20            CONTINUE
            ISURNO = SURECN
            CALL TABSOU ('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING SOURCE TABLE'
               GO TO 980
               END IF
            CALL TABSOU ('CLOS', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IRET = 0
            MSGTXT = 'Replaced fluxes in SU table'
            CALL MSGWRT (3)
         ELSE
            SNV1 = 0
            SNV2 = 0
            END IF

         END IF
C
 980  IF (IRET.NE.0) CALL MSGWRT (8)
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUSDO ERROR',I5,' ON ',A)
 1010 FORMAT ('IF',I3,' SU flux',F10.5,' fit flux',F10.5)
      END
      SUBROUTINE SOUSSN (IRET)
C-----------------------------------------------------------------------
C   SOUSSN updates all SN tables correcting for new flux densities.
C   Inputs from common:
C      DISKIN     I     Disk number for first file.
C      CNOIN      I     Catalog slot number for first file
C      SOUNUM     I(*)  Source numbers wanted.
C      XSUBA      R     Subarray number
C      XFLUX      R(*)  The amplitude calibration factor for each
C                       source and IF.
C      CATBLK     I(*)  Catalog header for the first file.
C      SNVER      I     Selected SN table, 0=>all
C   Output:
C      IRET       I     Return error code  0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   IER, I, LUN, BUFFER(512), ITAB, SNKOLS(MAXSNC), IT,
     *   SNNUMV(MAXSNC), ANTKOL, TIMKOL, SUBKOL, SOUKOL, RE1KOL, RE2KOL,
     *   IM1KOL, IM2KOL, WT1KOL, WT2KOL, FRQKOL, NUMANT, NUMPOL, NUMIF,
     *   NUMNOD, RECORD(XCLRSZ), SUID, NRECIN, ISNRNO, LOOP
      LOGICAL   ISAPPL, TABLE, EXIST, FITASC, NBLNK
      REAL      GMMOD,  RANOD(25), DECNOD(25), RECR(XCLRSZ), RCR, RCI
      DOUBLE PRECISION RECD(XCLRSZ/2)
      INCLUDE 'SOUSP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA LUN /28/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Which SN tables selected?
C                                       Loop over tables
      DO 100 ITAB = SNV1,SNV2
         IT = ITAB
C                                       If not there skip.
         CALL ISTAB ('SN', DISKIN, CNOIN, IT, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IER)
         IF (.NOT.EXIST .OR. (IER.NE.0)) THEN
            WRITE (MSGTXT,1010) IT
            CALL MSGWRT (7)
            GO TO 100
         ELSE
            WRITE (MSGTXT,1011) IT
            CALL MSGWRT (3)
            END IF
C                                       reformat if needed
         CALL SNREFM (DISKIN, CNOIN, IT, CATBLK, LUN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'REFORMAT'
            CALL MSGWRT (7)
            END IF
C                                       Open Table for keywords
         CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN'
            GO TO 990
            END IF
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOS'
            GO TO 990
            END IF
C                                       Open for write
         IT = ITAB
         CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IRET)
C                                       Error check
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN'
            GO TO 990
            END IF
C                                       Set table pointers
         TIMKOL = SNKOLS(SNDTIM)
         SOUKOL = SNKOLS(SNISID)
         ANTKOL = SNKOLS(SNIANT)
         SUBKOL = SNKOLS(SNISUB)
         FRQKOL = SNKOLS(SNIFQI)
         RE1KOL = SNKOLS(SNRRE1)
         IM1KOL = SNKOLS(SNRIM1)
         WT1KOL = SNKOLS(SNRWE1)
         RE2KOL = SNKOLS(SNRRE2)
         IM2KOL = SNKOLS(SNRIM2)
         WT2KOL = SNKOLS(SNRWE2)
C                                       Get number of records.
         NRECIN = BUFFER(5)
C                                       Read through table
         DO 50 LOOP = 1,NRECIN
            ISNRNO = LOOP
            CALL TABIO ('READ', 0, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ'
               GO TO 990
               END IF
C                                       Decide if wanted:
            SUID = RECORD(SOUKOL)
            IF (SUID.EQ.SRCNUM) THEN
C                                       Correct gains
               DO 20 I = 1,NUMIF
                  RCR = RECR(RE1KOL+I-1)
                  RCI = RECR(IM1KOL+I-1)
                  NBLNK = RCR.NE.FBLANK.AND.RCI.NE.FBLANK
                  IF (RECR(WT1KOL+I-1).GT.1.0 .AND. NBLNK) THEN
                     RECR(RE1KOL+I-1) = RCR * XFLUX(I)
                     RECR(IM1KOL+I-1) = RCI * XFLUX(I)
                     END IF
 20               CONTINUE
               IF (NUMPOL.GT.1) THEN
                  DO 40 I = 1,NUMIF
                     RCR = RECR(RE2KOL+I-1)
                     RCI = RECR(IM2KOL+I-1)
                     NBLNK = RCR.NE.FBLANK.AND.RCI.NE.FBLANK
                     IF (RECR(WT2KOL+I-1).GT.1.0 .AND. NBLNK) THEN
                        RECR(RE2KOL+I-1) = RCR * XFLUX(I)
                        RECR(IM2KOL+I-1) = RCI * XFLUX(I)
                        END IF
 40                  CONTINUE
                  END IF
               END IF
            ISNRNO = LOOP
            CALL TABIO ('WRIT', 0, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT'
               GO TO 990
               END IF
 50         CONTINUE
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOS'
            GO TO 990
            END IF
C                                       End table loop
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUSSN: ERROR ',I3,2X,A4,'ING SN TABLE')
 1010 FORMAT ('SOUSSN: SN TABLE VERSION',I4,' NOT PRESENT, NOT FIXED')
 1011 FORMAT ('SOUSSN: adjusting gains in SN table version',I4)
      END

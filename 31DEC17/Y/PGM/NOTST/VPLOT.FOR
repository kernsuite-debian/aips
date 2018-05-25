LOCAL INCLUDE 'PVPLOT.INC'
C                                       Parameter include for VPLOT
C                                       MAXCC = max. no. model
C                                       components.
      INTEGER   MAXCC
      PARAMETER (MAXCC = 200000)
      INTEGER MAXSAT
      PARAMETER (MAXSAT = 5*6)
      INTEGER MXSCAN
      PARAMETER (MXSCAN = 5000)
LOCAL END
LOCAL INCLUDE 'VPLOT.INC'
C                                       Local include for VPLOT
      INCLUDE 'PVPLOT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XXSTOK,
     *   XXSOUR(4,30), XXCALC, XPPLOT(2), XOPTYP, XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, XSTOK*4,
     *   STANAM(MAXANT)*8, XSOUR(30)*16, XCALCO*4, PPLOT*8,
     *   OPTYPE*4, REASON*24, OPCOD*4
      REAL      USERID, XSIN, XDISIN, XS2IN, XDI2IN, XBCHAN, XECHAN,
     *   XAVGCH, XBIF, XEIF, XAVGIF, XCROWD, XDO3C, XINC, UVRANG(2),
     *   XTIME(8), XANT(50), XBASE(50), YTYPE, SOLINT, APARM(10),
     *   BPARM(10), RALIAS(30), XNCOUN, XINV2, XCOMP(MAXAFL), XFLUX,
     *   XNMAP, XSUBA, XDOTV, XGRCH, XQUAL, XBAND, XFREQ, XFQID, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XSYM, XFACT, XLABEL, XREFAN, XYRATO
      REAL      BUFF1(UVBFSS), CCPOS(3,MAXCC), SFLUX(MAXCC),
     *   GAUSA(MAXCC), GAUSB(MAXCC), GAUSC(MAXCC), IATUTC, UT1XXX,
     *   CHOUT(4), NSIGMA, INISCL(6)
      INTEGER   NSOLIN, JBUFSZ, NUMCC, NANT, NSRC, IDN(30), OLDCNO,
     *   CATKEP(256), NPOINT(MXBASE), LUN1, LUN2, NOSCAN
      LOGICAL   DOGAUS, DOSPHE, DOTV, MULTI, DOIAVG, AVGSCA, AMPPH,
     *   SEARCH, COPFG, ALLCH, YESCAN, DOIFRA, REQAS, DOCAVG, SPHASE
      REAL      TBEG, TFIN, XYSCL(4), XYOFF(4), TIMMIN, TIMMAX,
     *   YYMIN(MXBASE), YYMAX(MXBASE), PHMIN(MXBASE), PHMAX(MXBASE),
     *   PPMIN(MXBASE), PPMAX(MXBASE), TSCAN(MXSCAN)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX(3),
     *   NCH, VER2, TESTEM(3), POLPLT, IPOLPT, ANTS(50), BASE(50),
     *   ISUB, NBASE, NPARMS, FQID, ANT1(MXBASE), ANT2(MXBASE), GRCHN,
     *   TVCHN, TVCORN(4), NUMFRQ, DTYPE, KPLOT, LABEL, INVER, REFANT,
     *   OUTVER, KBIF, KEIF, KBCH, KECH, BUFF(512), KFGRNO,
     *   IORBIT(MAXANT), IALIAS(30+1), DOMODL, ICROWD, IPHASE
      LOGICAL   UVREV, SCALEM(3), FLOTEM, DROPEM, DOERRB, DECICL, INDECL
C
      DOUBLE PRECISION XB(MAXANT), YB(MAXANT), ZB(MAXANT),
     *   ORBITA(MAXSAT), JDREF, AFREQ, GST0, ANLONG, OBSRA,
     *   OBSDEC, UVSCAL
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XNAM2I,
     *   XCLA2I, XS2IN, XDI2IN, XINV2, XCOMP, XFLUX, XNMAP, XXSOUR,
     *   XQUAL, XXCALC, XBAND, XFREQ, XFQID, XBCHAN, XECHAN, XAVGCH,
     *   XBIF, XEIF, XAVGIF, XCROWD, XDO3C, XINC, UVRANG, XTIME, XXSTOK,
     *   XANT, XBASE, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH, XSUBA, YTYPE, SOLINT, XOPTYP, XOPCOD,
     *   APARM, BPARM, RALIAS, XREFAN, XNCOUN, XPPLOT, XSYM, XFACT,
     *   XLABEL, XDOTV, XGRCH, XYRATO
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /VBPCOM/ TBEG, TFIN, XYSCL, XYOFF, TIMMIN, TIMMAX, UVREV,
     *   SCALEM, FLOTEM, DROPEM, IAW1, IAW2, INC, SEQIN, DISKIN,
     *   LUNI, INDI, TYPEAX, NCH, VER2, TESTEM, POLPLT, IPOLPT,
     *   ANTS, BASE, NBASE, ANT1, ANT2, PHMIN, PHMAX, GRCHN, TVCHN,
     *   TVCORN, NSRC, IDN, OLDCNO, CATKEP, ISUB, LABEL, CHOUT, NUMFRQ,
     *   DTYPE, DOTV, DOERRB, MULTI, DOIAVG, AVGSCA, AMPPH, DOCAVG,
     *   DECICL, INDECL, YYMIN, YYMAX, KPLOT, NPOINT, NPARMS, SEARCH,
     *   NSIGMA, COPFG, INVER, OUTVER, LUN1, LUN2, NSOLIN, KFGRNO,
     *   IALIAS, DOMODL, ALLCH, YESCAN, DOIFRA, REFANT, FQID, KBIF,
     *   KEIF, KBCH, KECH, BUFF, REQAS, ICROWD, IPHASE, PPMAX, PPMIN,
     *   SPHASE, NOSCAN, TSCAN, INISCL
C
      COMMON /CANIN/ XB, YB, ZB, ORBITA, JDREF, AFREQ, GST0, ANLONG,
     *   OBSRA, OBSDEC, UVSCAL, IATUTC, UT1XXX, NANT, IORBIT
      COMMON /COMPS/ CCPOS, SFLUX, GAUSA, GAUSB, GAUSC, DOGAUS, DOSPHE,
     *   NUMCC
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAM2IN, CLA2IN, XSTOK, STANAM,
     *   XSOUR, XCALCO, PPLOT, OPTYPE, OPCOD, REASON
LOCAL END
      PROGRAM VPLOT
C-----------------------------------------------------------------------
C! Plots selected uv data and model values.
C# UV Plot-appl VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2012, 2014-2015
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
C   VPLOT plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     USERID                       UV data file owner # ignored
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input UV data.
C     IN2NAME        NAM2IN        CLEAN components file name.
C     IN2CLASS       CLA2IN        CLEAN components file class.
C     IN2SEQ         XS2IN         CLEAN components file sequence no.
C     IN2DISK        XDI2IN        CLEAN components file disk no.
C     IN2VERS        XINV2         CLEAN components file version no.
C     NCOMP(MAXAFL)  XCOMP(MAXFLD) Number of Clean Components.
C     BCHAN          BCHAN         Start channel for averaging
C     ECHAN          ECHAN         End channel for averaging
C     BIF            BIF           Start IF number for averaging
C     EIF            EIF           End IF number for averaging
C     XINC.......Skip this number of vis. records between plotting.
C     UVRANGE....Range of UV projected spacings to include (Klambda)
C     TIMERANG...Selection parameters:
C        1 = Start IAT day (day 0 = first day in data base)
C        2 = Start IAT hour
C        3 = Start IAT minute
C        4 = Start IAT second
C        5 = Stop IAT day (day 0 = first day in data base)
C        6 = Stop IAT hour
C        7 = Stop IAT minute
C        8 = Stop IAT second
C     STOKES....Stokes' type
C     ANTENNAS..Antenna numbers
C     BASELINE..Baselines.
C     YTYPE.....If > 0, plot error bars
C     SOLINT....pre-average time (mins)
C     OPTYPE.....Data to be plotted: 'AUTO' => autocorrelation
C                else => crosscorrelation
C     OPCOD.....'IFRA' => ratio of BIF and EIF
C     APARM......Control parameters for bad points recognition
C        1 > 0 => provide bad points recognition
C            = 0 => not provide bad points recognition
C        2 = number of sigmas in clipping bad points
C        3 = 0 => show min and max amplitude in the interval of avg.
C        4 Domain of looking for min/max amplitude
C        5 =0 => vector average
C          >0 => scalar average
C     BPARM......Control parameters:
C        1 = type of X-axis (where  1 = amplitude (Jy), 2 = phase
C           (degrees), 3 = u,v distance (klambda), 4 = u,v p.a.
C           (degrees, clockwise from v-axis), 5 = time (iat days),
C           6 = u, 7 = v, 8 = w (all in klambda), 9 = real part (Jy),
C           10 = imaginary part(Jy), and 11 = folded G.S.T.(hours),
C           12 = time (IAT hms), 13 = source no.,
C           14 = freqid, 15 = int. time, 16 = weight,
C           17 = RMS) 0 => 12
C        2 = type of Y-axis; If<0 => Amp&Phase together
C        3 = Do not autoscale if non-zero, use following values:
C        4 = Minimum of X-axis,
C        5 = Maximum of X-axis,
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis,
C        8 = Minimum of of phase at Y-axis if both amplitude
C            and phase are plotted,
C        9 = Maximum of of phase at Y-axis if both amplitude
C            and phase are plotted,
C        10= > 0  => plot missing baselines (if there's a model)
C      ALIAS.....Antenna alias parameter.  If ALIAS(1) is non-zero,
C               any antenna number matching ALIAS(2-30) is treated
C               as if it was actually the antenna number in ALIAS(1)
C      NCOUNT.. Maximum number of plots per page
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, MAXPLT, MSAMP, MPOL, MIF, MCHAN, NSAMP, MBASE
C                                       large buffer areas
      REAL      PBUFF(2), SCANV(2)
      INTEGER   NWORDS, IPBUFF(2)
      LONGINT   PPBUFF, PXVAL, PYVAL, PIBASE, PADD, PLBASE, PLLBAS,
     *   PPHVAL, PMDAVA, PMDPVA, PAMMIN, PAMMAX, LI, PSCANV, PMNMX
      EQUIVALENCE (PBUFF, IPBUFF)
      INCLUDE 'VPLOT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'VPLOT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VBPIN (PRGM, MAXPLT, MSAMP, MPOL, MCHAN, MIF, IRET)
      IF (IRET.NE.0) GO TO 995
      IPOLPT = 0
      MBASE = NBASE
C                                       allocate memory
 10   NWORDS = (9 * MBASE * MPOL * MCHAN * MIF - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SCANV, PSCANV, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'NOT ENOUGH MEMORY (1) FOR THIS JOB: QUITTING'
         CALL MSGWRT (9)
         GO TO 995
         END IF
      NWORDS = NWORDS * 1024
      PMNMX = PSCANV + 7 * (NWORDS / 9) + 1
      NWORDS = 5 + MIF*MPOL*MCHAN
      IF (AMPPH) NWORDS = NWORDS + MIF*MPOL*MCHAN
      IF (DOMODL.EQ.2) NWORDS = NWORDS + 2
      IF (APARM(3).GT.0.0) NWORDS = NWORDS + 2*MIF*MPOL*MCHAN
      NWORDS = (NWORDS * (MSAMP + 20) - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PBUFF, PPBUFF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'NOT ENOUGH MEMORY (2) FOR THIS JOB: QUITTING'
         CALL MSGWRT (9)
         GO TO 995
         END IF
      PXVAL = PPBUFF + 1
      PYVAL = PXVAL + MSAMP + 10
      PIBASE = PYVAL + (MSAMP + 10) * MIF*MPOL*MCHAN
      PADD = PIBASE + MSAMP + 10
      PLBASE = PADD + MSAMP + 10
      PLLBAS = PLBASE + MSAMP + 10
      PPHVAL = PLLBAS + MSAMP + 10
      LI = PPHVAL
      IF (AMPPH) LI = LI + (MSAMP + 10) * MIF*MPOL*MCHAN
      PMDAVA = LI
      IF (DOMODL.EQ.2) LI = LI + MSAMP + 10
      PMDPVA = LI
      IF (DOMODL.EQ.2) LI = LI + MSAMP + 10
      PAMMIN = LI
      IF (APARM(3).GT.0.0) LI = LI + (MSAMP + 10) * MIF*MPOL
      PAMMAX = LI
      NSAMP = MSAMP
C                                       determine scaling
      CALL SCALVB (MAXPLT, MBASE, MSAMP, MPOL, MCHAN, MIF, PBUFF(PXVAL),
     *   PBUFF(PYVAL), IPBUFF(PIBASE), IPBUFF(PADD), IPBUFF(PLBASE),
     *   IPBUFF(PLLBAS), PBUFF(PPHVAL), PBUFF(PMDAVA), PBUFF(PMDPVA),
     *   PBUFF(PAMMIN), PBUFF(PAMMAX), SCANV(1+PSCANV), SCANV(PMNMX),
     *   IRET)
      IF ((IRET.GT.0) .OR. SEARCH) GO TO 990
C                                       retry
      IF (IRET.LT.0) THEN
         CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, PBUFF, PPBUFF, IRET)
         WRITE (MSGTXT,1000) NSAMP, MSAMP
         CALL MSGWRT (6)
         GO TO 10
         END IF
C                                       Do plots
      CALL VBPLOT (MAXPLT, MSAMP, MPOL, MCHAN, MIF, PBUFF(PXVAL),
     *   PBUFF(PYVAL), IPBUFF(PIBASE), IPBUFF(PADD), IPBUFF(PLBASE),
     *   PBUFF(PPHVAL), PBUFF(PMDAVA), PBUFF(PMDPVA), PBUFF(PAMMIN),
     *   PBUFF(PAMMAX), IRET)
      IRET = MAX (0, IRET)
C                                       clear memory
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, PBUFF, PPBUFF, IRET)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('GUESSED',I10,' SAMPLES, BUT NEED',I10,' RETRY')
      END
      SUBROUTINE VBPIN (PRGM, MAXPLT, MSAMP, MPOL, MCHAN, MIF, JERR)
C-----------------------------------------------------------------------
C   VBPIN gets input parameters for VPLOT
C   Inputs:
C      PRGM     C*6   Program name
C   Output:
C      MAXPLT   I     Maximum number of plots per page.
C      MSAMP    I     Guess maximum size of buffers
C      MPOL     I     Number of separate polarizations
C      MCHAN    I     Number of spectral channels
C      MIF      I     Number of separate IFs
C      JERR     I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MAXPLT, MSAMP, MPOL, MCHAN, MIF, JERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VPLOT.INC'
      CHARACTER UTYPE*2, STAT*4, STNS(MAXANT)*8
      INTEGER   IERR, ITEMP, IUSER, VER, I, IROUND, LUN, IXBASL(MAXANT),
     *   N, IXANT(MAXANT), NXANT, NXBASL, BUFF2(256), BUFF3(256), SUBAR,
     *   MIDCH, LTYPE
      REAL      CATR(256), EPS
      LOGICAL   MATCH, TRYMOD, DESEL
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATR, CATBLK)
C-------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARMS = 339 + MAXAFL
      CALL GTPARM (PRGM, NPARMS, RQUICK, USERID, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      EPS = 0.1
      USERID = NLUSER
      IUSER = NLUSER
      IF (XINC.LT.1.0) XINC = 1.0
      INC = XINC + EPS
      SEQIN = XSIN + EPS
      DISKIN = XDISIN + EPS
      ISUB = IROUND (XSUBA)
      IF (ISUB.NE.-1) ISUB = MAX (1, ISUB)
      ISUB = MAX (0, ISUB)
      SUBARR = ISUB
      IF (XFACT.EQ.0.0) XFACT = 1.0
C                                       Warning about timerang
C                                       selection for the data output
C                                       of DBCON
      IF ((ISUB.GT.1) .OR. (ISUB.EQ.0)) THEN
         MSGTXT = 'If the data are DBCON output, the time is shifted'
         CALL MSGWRT (2)
         MSGTXT = 'by 5 days for each subsequent subarray.'
         CALL MSGWRT (2)
         MSGTXT = 'Pay attention to this when selecting TIMERANG.'
         CALL MSGWRT (2)
         END IF
      I = IROUND (XNMAP)
      DOMODL = 0
      IF (I.GT.0) DOMODL = 2
      IF (I.LT.0) DOMODL = 1
      REFANT = IROUND (XREFAN)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCLA2I, CLA2IN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (8, 1, XPPLOT, PPLOT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XOPCOD, OPCOD)
C
      DOIFRA = OPCOD .EQ. 'IFRA'
C                                       'cros' or 'auto'
      DOACOR = .FALSE.
      DOXCOR = .TRUE.
      IF (OPTYPE.EQ.'AUTO') THEN
         DOACOR = .TRUE.
         DOXCOR = .FALSE.
         END IF
      TRYMOD = ((NAM2IN(1:4).NE.' ') .OR. (CLA2IN.NE.' '))
C                                       Timerange
      TBEG = XTIME(1) + (XTIME(2)+(XTIME(3)+XTIME(4)/60.)/60.)/24.
      TFIN = XTIME(5) + (XTIME(6)+(XTIME(7)+XTIME(8)/60.)/60.)/24.
C                                       Test time, UV ranges
      IF (TFIN.LE.TBEG) THEN
         TFIN = 1.0E10
         IF (TBEG.LE.0.0) TBEG = -100.
         END IF
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TBEG
      XTIME(5) = TFIN
      CALL RCOPY (8, XTIME, TIMRNG)
C                                       UVRANGE to DSEL common
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.0E10
      UVRNG(1) = UVRANG(1)
      UVRNG(2) = UVRANG(2)
      IF (APARM(2).EQ.0.0) THEN
         NSIGMA = 3
      ELSE
         NSIGMA = APARM(2)
         END IF
C                                       copy over IALIAS array
      DO 10 I = 1,30
         IALIAS(I) = RALIAS(I)
 10      CONTINUE
C                                       prep IALIAS array
      CALL ALIASA (IALIAS)
C                                       Test type of plot
      IF (BPARM(1).EQ.0.0) BPARM(1) = 12.0
      IF (BPARM(2).EQ.0.0) BPARM(2) = 1.0
C                                       Parameters for bad points
C                                       recognition
      SEARCH = (APARM(1).GT.0.01) .AND. (BPARM(2).EQ.1)
      ALLCH = (APARM(6).GT.0.01)
C                                       only for SOLINT.NE.0
      IF (SEARCH .AND. SOLINT.EQ.0.0) THEN
         MSGTXT = 'Bad point recognition works only if SOLINT>0'
         JERR = 1
         GO TO 990
         END IF
C                                       Multiply by 1000 if U, V, W
      IF ((BPARM(1).EQ.3) .OR. ((BPARM(1).GE.6) .AND. (BPARM(1).LE.8)))
     *   THEN
         BPARM(4) = BPARM(4) * 1.0E3
         BPARM(5) = BPARM(5) * 1.0E3
         END IF
      IF ((BPARM(2).EQ.3) .OR. ((BPARM(2).GE.6) .AND. (BPARM(2).LE.8)))
     *   THEN
         BPARM(6) = BPARM(6) * 1.0E3
         BPARM(7) = BPARM(7) * 1.0E3
         END IF
C                                       use the fixed scale
C                                       if APARM(3).NE.0 and phase
C                                       is plotted
C      IF ((BPARM(2).EQ.2) .AND. (APARM(3).GT.0)) THEN
C         BPARM(3) = 1
C         BPARM(6) = -200
C         BPARM(7) = 200
C         END IF
      XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYSCL(3) = XYSCL(1)
      XYSCL(4) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
      XYOFF(3) = XYOFF(1)
      XYOFF(4) = XYOFF(1)
      DECICL = .FALSE.
C                                       If plotting uv only
C                                       then plot conjugate points
      ITEMP = BPARM(1) * BPARM(2) + 0.1
      UVREV = (ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56)
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      DROPEM = (BPARM(10).LE.0.0)
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
      FLOTEM = (BPARM(3).EQ.0.0)
      CALL RCOPY (5, BPARM(4), INISCL)
C                                       Get CATBLK from file.
      DOERRB = YTYPE.GT.0.0
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
      XLABEL = LABEL
      LUNI = 16
      UTYPE = 'UV'
      OLDCNO = 1
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1200) JERR
         GO TO 990
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       check sort order
      IF (ISORT(1:1).NE.'T') THEN
         WRITE (MSGTXT,1100) ISORT
         JERR = 1
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATKEP)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      IUDISK = UDISK
      IUCNO = OLDCNO
      NSRC = 0
      DO 75 I = 1,30
         SOURCS(I) = ' '
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         IF (SOURCS(I)(1:4).NE.'    ') NSRC = NSRC + 1
         CALSOU(I) = ' '
 75      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BPVER = IROUND (XBPVER)
      DOBAND = IROUND (XDOBND)
C                                        Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Multi-source
      CALL MULSDB (CATBLK, MULTI)
C                                       data for FG table
C-----------------------------------------------------------------------
      IF (SEARCH) THEN
         REASON = 'VPLOT:BIG AMP. DEVIATION'
         LUN1 = 70
         LUN2 = 71
         CALL FNDEXT ('FG', CATBLK, OUTVER)
         INVER = FGVER
         IF (INVER.EQ.0) INVER = OUTVER
         COPFG = (OUTVER.GT.0 .AND. INVER.GE.0)
         OUTVER = OUTVER + 1
C                                       copy the flag table if anyone
C                                       exits and bad points are planned
C                                       to recognize
         IF (COPFG) THEN
            CALL TABCOP ('FG', INVER, OUTVER, LUN1, LUN2, DISKIN,
     *         DISKIN, OLDCNO, OLDCNO, CATBLK, BUFF2, BUFF3, JERR)
            IF (JERR.NE.0) THEN
               IF (JERR.EQ.1) THEN
                  MSGTXT = 'INPUT AND OUTPUT TABLES ARE THE SAME.'
                  GO TO 990
               ELSE IF (JERR.EQ.6) THEN
                  MSGTXT = 'OUTPUT TABLE ALREADY EXISTS'
                  GO TO 990
               ELSE
                  WRITE (MSGTXT,2300) JERR, INVER
                  GO TO 990
                  END IF
               END IF
            END IF
         CALL FLGINI ('WRIT', BUFF, DISKIN, OLDCNO, OUTVER, CATBLK,
     *      LUN2, KFGRNO, FGKOLS, FGNUMV, JERR)
C                                       mark FG table as unsorted
         BUFF(43) = 0
         BUFF(44) = 0
         END IF
C                                       Check pre-average time
      SOLINT = SOLINT / 1440.0
      YESCAN = .TRUE.
      IF (SOLINT.LT.0) THEN
         SOLINT = -SOLINT
         YESCAN = .FALSE.
C                                       the warning about possible
C                                       averaging of different source
         MSGTXT = '!!!! WARNING !!!!'
         CALL MSGWRT (6)
         MSGTXT = 'Selecting SOLINT<0, the time averaging will be '
         CALL MSGWRT (6)
         MSGTXT = 'possibly carried out through more than 1 scan.'
         CALL MSGWRT (6)
         MSGTXT = '!!!!So different sources can be averaged!!!!'
         CALL MSGWRT (6)
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      FQID = FRQSEL
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         CALL MSGWRT (6)
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Test channel #
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.EQ.0) ECHAN = BCHAN
      IF (ECHAN.LT.BCHAN) ECHAN = BCHAN
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
C                                       number of channels and
C                                       polarizations in the data
      NUMFRQ = CATBLK(KINAX+JLOCF)
      XBCHAN = BCHAN
      XECHAN = ECHAN
      KBCH = BCHAN
      KECH = ECHAN
      DOCAVG = (ECHAN.GT.BCHAN) .AND. (XAVGCH.GT.0.0)
      MCHAN = ECHAN - BCHAN + 1
      IF ((DOCAVG) .AND. (MCHAN.GT.1)) THEN
         MCHAN = 1
         WRITE (MSGTXT,1300) BCHAN, ECHAN
         CALL MSGWRT (3)
         END IF
C                                       IF number
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOIAVG = (EIF.GT.BIF) .AND. (XAVGIF.GT.0.0)
C                                       size parameters
      MIF = EIF - BIF + 1
C
C                                       The size is 1 if OPCODE='IFRA'
      IF (DOIFRA) THEN
         DOIAVG = .FALSE.
         MIF = 1
         WRITE (MSGTXT,1350) BIF, EIF
         CALL MSGWRT (3)
C                                       The size is 1 if DOIAVG
      ELSE IF (DOIAVG) THEN
         MIF = 1
         WRITE (MSGTXT,1400) BIF, EIF
         CALL MSGWRT (3)
         END IF
      XBIF = BIF
      KBIF = BIF
      XEIF = EIF
      KEIF = EIF
      MSAMP = APARM(7)
      IF (MSAMP.LE.100) MSAMP = CATBLK(KIGCN)
C                                       Maximum number of visabilities
      MSAMP = MIN (MSAMP, 10000000)
C                                       Find baselines to plot
      CALL SETANT (50, XANT, XBASE, NXANT, NXBASL, IXANT, IXBASL, DESEL)
C                                       Fill in list of all antenna
C                                       - baseline pairs and names.
C                                       Determine the list and number
C                                       of selected antennas.
      SUBAR = SUBARR
      IF (SUBAR.LE.0) SUBAR = 1
C                                       read the first version of
C                                       antennas file if subarr=0
      CALL FILANT (DISKIN, OLDCNO, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, SUBAR, NBASE, ANT1, ANT2, STNS, JERR)
      CALL GETANT (DISKIN, OLDCNO, SUBAR, CATBLK, BUFF1, JERR)
C                                       Save results in input Parms
      TYPEAX(1) = IROUND (BPARM(1))
      TYPEAX(2) = IROUND (BPARM(2))
      IF ((TYPEAX(1).NE.5) .AND. (TYPEAX(1).NE.12)) APARM(8) = 0.0
      IF (APARM(8).GT.0.0) THEN
         CALL GETNX (LUN, DISKIN, OLDCNO, CATBLK, SUBARR, BUFF1, NOSCAN,
     *      TSCAN)
      ELSE
         NOSCAN = 0
         END IF
C                                       Maximum number of
C                                       plots per page
      ICROWD = IROUND (XCROWD)
      MAXPLT = 3
      IF (XNCOUN.GE.0.95) MAXPLT = XNCOUN + EPS
      N = NBASE
      IF ((ICROWD.NE.1) .AND. (ICROWD.NE.3)) N = N * MCHAN
      IF ((ICROWD.NE.2) .AND. (ICROWD.NE.3)) N = N * MIF
      MAXPLT = MIN (N, MAXPLT)
      AMPPH = BPARM(2).LT.0.0
C                                       do not use min/max lines
C                                       for both ampl. and phase
      IF((APARM(3).GT.0) .AND. AMPPH) THEN
         MSGTXT = 'Mode with min/max lines is not allowed for plotting '
     *      // 'both ampl. and phase'
         CALL MSGWRT (6)
         APARM(3) = 0.0
         END IF
C
      IF ((SOLINT.EQ.0.0) .AND. (APARM(3).GT.0) .AND.
     *   ((APARM(4).EQ.0) .OR. (ECHAN.EQ.BCHAN)) .AND. (BIF.EQ.EIF))
     *   THEN
         MSGTXT = 'APARM(3)=1 but SOLINT=0, APARM(4)=0 ' //
     *      'BIF=EIF so MIN=MAX.'
         CALL MSGWRT (6)
         APARM(3) = 0.0
         END IF
      IF (AMPPH) THEN
C        TYPEAX(1) = 12
         TYPEAX(2) = 1
         IF (BPARM(2).LT.-21.5) TYPEAX(2) = 22
         TYPEAX(3) = 2
         IPHASE = 3
         SCALEM(3) = (BPARM(3).LE.0.0) .OR. (BPARM(8).EQ.BPARM(9))
         TESTEM(3) = TESTEM(2)
         MAXPLT = 2*MAXPLT
      ELSE
         IPHASE = 0
         IF (TYPEAX(1).EQ.2) IPHASE = 1
         IF (TYPEAX(2).EQ.2) IPHASE = 2
         END IF
C                                       if too many plots on the page
      IF (MAXPLT.GT.40) THEN
         JERR = 1
         WRITE (MSGTXT,2060) MAXPLT
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Y axis type
      DTYPE = TYPEAX(2)
C                                       type of averaging in time
      AVGSCA = APARM(5).GT.0
C                                       Check if valid POLTYP
      CALL COPY (256, CATBLK, CATUV)
      CALL POLCHK (PPLOT, POLPLT)
      MPOL = 1
      IF (POLPLT.GT.0) THEN
         IF ((POLPLT.EQ.3) .OR. (POLPLT.EQ.6)) THEN
            STOKES='HALF'
         ELSE IF ((POLPLT.GE.7) .AND. (POLPLT.LE.9)) THEN
            STOKES = 'IQUV'
         ELSE
            STOKES='FULL'
            END IF
      ELSE
         IF (ICOR0.EQ.-2) THEN
            IF (STOKES.EQ.' ') STOKES = 'LL'
            IF (STOKES.EQ.'HALF') STOKES = 'LL'
         ELSE
            IF (STOKES.EQ.' ') STOKES = 'I'
            IF (STOKES.EQ.'RRLL') STOKES = 'HALF'
            IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'FULL')) MPOL = 2
            END IF
         END IF
C                                       Multi-plot IFs
      IF ((MIF.GT.1) .OR. (MPOL.GT.1) .OR. (MCHAN.GT.1)) THEN
         IF ((TYPEAX(1).EQ.1) .OR. (TYPEAX(1).EQ.2) .OR.
     *      (TYPEAX(1).EQ.9) .OR. (TYPEAX(1).EQ.10) .OR.
     *      (TYPEAX(1).EQ.22)) THEN
            MSGTXT = 'Multiple IFs/chans/pols allowed only on Y axis'
            JERR = 1
            GO TO 990
            END IF
         IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.9) .AND.
     *      (DTYPE.NE.10) .AND. (DTYPE.NE.16) .AND. (DTYPE.NE.17)
     *      .AND. (DTYPE.NE.22)) THEN
            MSGTXT = 'Multiple IFs not meaningful here'
            CALL MSGWRT (6)
            MIF = 1
            MPOL = 1
            MCHAN = 1
            DOCAVG = ECHAN.GT.BCHAN
            EIF = BIF
            XEIF = EIF
            KEIF = EIF
            END IF
         END IF
C                                       Block model when can't handle
      IF ((TYPEAX(1).EQ.1) .OR. (TYPEAX(1).EQ.2) .OR.
     *   (TYPEAX(1).EQ.9) .OR. (TYPEAX(1).EQ.10) .OR. (TYPEAX(1).EQ.22))
     *    DOMODL = 0
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.9) .AND.
     *   (DTYPE.NE.10) .AND. (DTYPE.NE.22)) DOMODL = 0
      IF ((DOMODL.GT.0) .AND. MULTI .AND. (NSRC.EQ.0)) THEN
         IF (TRYMOD) THEN
            MSGTXT = 'You may be trying to plot a model without ' //
     *         'specifying'
            CALL MSGWRT (6)
            MSGTXT = 'a source name - I do not allow that and am ' //
     *         'switching'
            CALL MSGWRT (6)
            MSGTXT = 'that option off.'
            CALL MSGWRT (6)
            DOMODL = 0
            END IF
         END IF
      IF (DOMODL.LE.0) THEN
         DROPEM = .TRUE.
         NUMCC = 0
         END IF
C                                       Get antenna info.
C                                       Read the first version of
C                                       antennas file if subarr=0
      VER = SUBAR
C
      IF (TRYMOD .AND. DOMODL.EQ.2) THEN
         MSGTXT = 'WARNING: Model will be plotted for the frequency'
         CALL MSGWRT (6)
         MSGTXT = '         corresponding the first selected IF'
         CALL MSGWRT (6)
         MSGTXT = '         and mean of selected channels'
         CALL MSGWRT (6)
         END IF
      MIDCH = (XBCHAN + XECHAN) / 2
      CALL ANTIN (VER, MIDCH, KBIF, DISKIN, OLDCNO, FRQSEL, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Correct freq. for channel MIDCH
C                                       and IF = XBIF
      UVSCAL = AFREQ / FREQ
C                                       Get CLEAN components.
      VER2 = XINV2 + 0.01
      IF (DOMODL.GT.0) THEN
         IF (MULTI .AND. TRYMOD) THEN
            MSGTXT = 'WARNING: You are attempting to plot a' //
     *         ' model against'
            CALL MSGWRT (6)
            MSGTXT = '         multi-source data, make sure' //
     *         ' you select the'
            CALL MSGWRT (6)
            MSGTXT = '         correct source and CC file.'
            CALL MSGWRT (6)
            CALL SOURNU (SOURCS, SELQUA, 1, DISKIN, OLDCNO, NSRC,
     *         BUFF1, IDN, JERR)
            IF (JERR.LT.0) THEN
               MSGTXT = 'REQUESTED SOURCE NOT FOUND'
               CALL MSGWRT (7)
               JERR = 5
               END IF
            IF (JERR.NE.0) GO TO 999
            CALL GETSOU (IDN(1), DISKIN, OLDCNO, CATBLK, LUN, JERR)
            RA = RAEPO * RAD2DG
            DEC = DECEPO * RAD2DG
C            RA = RAAPP * RAD2DG
C            DEC = DECAPP * RAD2DG
            END IF
         CALL REEDIN (VER2, JERR)
         END IF
      IF (NUMCC.LE.0) THEN
         DROPEM = .TRUE.
         DOMODL = 0
         END IF
      XINV2 = VER2
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VBPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1100 FORMAT ('You have sort order ',A2,
     *   '. Sort the data to TB by UVSRT')
 1200 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1300 FORMAT ('Averaging from channel',I6,'-',I6)
 1350 FORMAT ('Dividing IF ',I4,'  /',I4)
 1400 FORMAT ('Averaging from IF',I3,'-',I3)
 2060 FORMAT ('The reqired number of plots on a page',I5,
     *   ' exceeds maximum 40')
 2300 FORMAT ('TABCOP ERROR ',I4,' COPYING TABLE ',I5)
      END
      SUBROUTINE SCALVB (MAXPLT, MBASE, MSAMP, MPOL, MCHAN, MIF, XVAL,
     *   YVAL, IBASE, ADD, LBASE, LLBAS, PHVAL, MDAVAL, MDPVAL, AMMIN,
     *   AMMAX, SCANV, AMNMX, IRET)
C-----------------------------------------------------------------------
C   SCALVB stores arrays of Y and X for future plot; recognizes bad
C   points with big amplitude deviation and records them in flag table.
C   Return:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C                  < 0 => MSAMP changed to what is really needed
C   SCANV values: Re, Im, Wt, Rms, Count, work1, work2
C   AMNMX values: Min, Max
C-----------------------------------------------------------------------
      INTEGER   MAXPLT, MBASE, MSAMP, MPOL, MCHAN, MIF, IBASE(*),
     *   ADD(*), LBASE(*), LLBAS(*), IRET
      REAL      XVAL(*), YVAL(MSAMP,MPOL,MCHAN,*),
     *   PHVAL(MSAMP,MPOL,MCHAN,*), MDAVAL(*), MDPVAL(*),
     *   AMMIN(MSAMP,MPOL,MCHAN,*), AMMAX(MSAMP,MPOL,MCHAN,*),
     *   SCANV(7,MBASE,MPOL,MCHAN,*), AMNMX(2,MBASE,MPOL,MCHAN,*)
C
      INCLUDE 'VPLOT.INC'
      CHARACTER TREAS*24
      INTEGER   IARR, IARRW, I, KRET, IA1, IA2, LOOP, ISOLIN, CSOU,
     *   NUMVIS, XUMVIS, ITIME(8), SCANUM, INDEX, NAMPPH, NBAD,
     *   NLVIS, IFST(2), CHANST(2), ANTSST(2), IDSOU, LPOL, LEIF, LIF,
     *   CIF, CCH, NXLUN, LCHAN, LECH
      REAL      XY(3), RPARM(20), VISO(4), AVTIM, AVU(MXBASE),
     *   AVV(MXBASE), AVW(MXBASE), DMULT, AVT, TEMP, DTT, TIMERT(2),
     *   VMAX, VMIN, FPARM(20)
      LOGICAL   NOSCAL, NUSCAN, TFLAGS(4), DIDMSG
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
      REQAS = ((TYPEAX(1).GE.18) .AND. (TYPEAX(1).LE.21)) .OR.
     * ((TYPEAX(2).GE.18) .AND. (TYPEAX(2).LE.21))
      CSOU = -1
      NXLUN = 100
C
      DIDMSG = .FALSE.
      DO 10 LOOP = 1,NBASE
         YYMIN(LOOP) = 1.E10
         YYMAX(LOOP) = -1.E10
         PHMIN(LOOP) = 1.E10
         PHMAX(LOOP) = -1.E10
         PPMIN(LOOP) = 1.E10
         PPMAX(LOOP) = -1.E10
   10    CONTINUE
      VISO(4) = 0.0
C
      IF (DOIFRA) THEN
         LEIF = 1
      ELSE IF (DOIAVG) THEN
         LEIF = 1
      ELSE
         LEIF = EIF - BIF + 1
         END IF
      IF (DOCAVG) THEN
         LECH = 1
      ELSE
         LECH = ECHAN - BCHAN + 1
         END IF
C                                       prepare for AMP&PH
      NAMPPH = 2
      IF (AMPPH) NAMPPH = 3
      RPARM(1) = FBLANK
      NOSCAL = (.NOT.SCALEM(1)) .AND. (.NOT.SCALEM(2))
      IRET = 0
      IARRW = ISUB
C
      XUMVIS = 0
      NSOLIN = MSAMP
C                                       initiate to zero interval number
C                                       and number of found bad points
C                                       and number of baseline for the
C                                       interval
      ISOLIN = 0
      NBAD = 0
      DO 40 LOOP = 1,NSOLIN
         IBASE(LOOP) = 0
 40      CONTINUE
      DO 60 I = 1,NBASE
         NPOINT(I) = 0
 60      CONTINUE
      ADD(1) = 0
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.EQ.-1) GO TO 180
      IF (IRET.EQ.0) GO TO 80
         WRITE (MSGTXT,1000) IRET
         GO TO 990
 80   NUMVIS = 0
C
C                                       Find scales
      DMULT = 1.0
      IF (DTYPE.EQ.2) DMULT = RAD2DG
C                                       Loop
 100  CONTINUE
C                                       No time averaging
      IF (SOLINT.LE.0.0) THEN
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF1, IRET)
         IF (IRET.EQ.-1) GO TO 180
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,INC).NE.0) GO TO 100
C                                       Modify VIS as appropriate
C                                       based on POLPLT.
         IF (POLPLT.GT.0) CALL POLVIS (BUFF1, POLPLT, IPOLPT)
C                                       Pick up some points
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB) / 256. + 0.1
            IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
            IARR = (RPARM(1+ILOCB)-IA1*256.0-IA2) * 100.0 + 1.5
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            IARR = RPARM(1+ILOCSA) + 0.1
            END IF
         CALL ALIAS (IA1, IALIAS)
         CALL ALIAS (IA2, IALIAS)
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) GO TO 100
C                                       Test time range (TB sort)
         TEMP = RPARM(1+ILOCT)
         IF (TEMP.GT.TFIN) GO TO 180
         IF (TEMP.LT.TBEG) GO TO 100
C                                       Test antennas
         DO 115 LOOP = 1,NBASE
            IF ((IA1.EQ.ANT1(LOOP)) .AND. (IA2.EQ.ANT2(LOOP)))
     *          GO TO 120
 115        CONTINUE
C                                       Not wanted
         GO TO 100
C                                       store values of data
C                                       scale UV values
 120     IF (UVSCAL.NE.1.0) THEN
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            END IF
         XUMVIS = XUMVIS + 1
         NPOINT(LOOP) = NPOINT(LOOP) + 1
         IF (ISOLIN.GE.MSAMP) GO TO 180
         ISOLIN = ISOLIN + 1
         IBASE(ISOLIN) = IBASE(ISOLIN) + 1
         INDEX = ADD(ISOLIN) + 1
         IF (INDEX.GT.MSAMP) GO TO 180
         IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
            CSOU = CURSOU
            CALL GETSOU (CSOU, DISKIN, OLDCNO, CATUV, NXLUN, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'TROUBLE GETTING SOURCE INFO'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Spectral averaging
         DO 125 LPOL = 1,MPOL
         DO 125 LCHAN = 1,LECH
         DO 125 LIF = 1,LEIF
            CIF = LIF + BIF - 1
            IF (DOIAVG .OR. DOIFRA) CIF = 0
            CCH = LCHAN + BCHAN - 1
            IF (DOCAVG) CCH = 0
C                                       AFGCIF averages different
C                                       channels anyway; averages
C                                       different IFs under control
C                                       of DOIAVG
            CALL AVGCIF (BUFF1, CCH, BCHAN, ECHAN, LPOL, CIF, BIF, EIF,
     *         VMIN, VMAX, VISO)

            IF (VISO(3).LE.0.0) THEN
               YVAL(INDEX,LPOL,LCHAN,LIF) = FBLANK
               IF (AMPPH) THEN
                  PHVAL(INDEX,LPOL,LCHAN,LIF) = FBLANK
               ELSE IF (APARM(3).GT.0) THEN
                  AMMIN(INDEX,LPOL,LCHAN,LIF) = FBLANK
                  AMMAX(INDEX,LPOL,LCHAN,LIF) = FBLANK
                  END IF
            ELSE
               CALL FNDXY (VISO, RPARM, XY)
C                                       autoscale for min/max lines
               IF (.NOT.NOSCAL) THEN
                  IF (APARM(3).GT.0) THEN
                     XY(2) = VMIN * DMULT
                     CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, LOOP, IRET)
                     XY(2) = VMAX * DMULT
                     CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, LOOP, IRET)
                  ELSE
                     CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, LOOP, IRET)
                     END IF
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1250) IRET
                     GO TO 990
                     END IF
                  END IF
               YVAL(INDEX,LPOL,LCHAN,LIF) = XY(2)
               IF (AMPPH) THEN
                  PHVAL(INDEX,LPOL,LCHAN,LIF) = XY(3)
               ELSE IF (APARM(3).GT.0) THEN
                  AMMIN(INDEX,LPOL,LCHAN,LIF) = VMIN * DMULT
                  AMMAX(INDEX,LPOL,LCHAN,LIF) = VMAX * DMULT
                  END IF
               END IF
 125        CONTINUE
         XVAL(INDEX) = XY(1)
         LBASE(INDEX) = LOOP
         ADD(ISOLIN+1) = ADD(ISOLIN) + 1
         IF (DOMODL.EQ.2) CALL MODPTS (RPARM(1+ILOCU), RPARM(1+ILOCV),
     *      RPARM(1+ILOCW), TYPEAX(2), MDAVAL(INDEX), MDPVAL(INDEX))
C                                       averaging over time
      ELSE
         IF (APARM(3).GT.0) THEN
            DO 130 LPOL = 1,MPOL
            DO 130 LCHAN = 1,LECH
            DO 130 LIF = 1,LEIF
               DO 129 LOOP = 1,NBASE
                  AMNMX(1,LOOP,LPOL,LCHAN,LIF) = 1.0E10
                  AMNMX(2,LOOP,LPOL,LCHAN,LIF) = -1.0E10
 129              CONTINUE
 130           CONTINUE
            END IF
C                                       Read vis recs and form averages
         CALL BASAVG (NUMVIS, SCANV, AMNMX, ITIME, NUSCAN, SCANUM,
     *      RPARM, BUFF1, AVT, AVU, AVV, AVW, NLVIS, DTT, MBASE, MSAMP,
     *      MPOL, MCHAN, MIF, XVAL, YVAL, LLBAS, FPARM, IRET)
         KRET = IRET
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 4
            GO TO 990
            END IF
C                                       No data and end found
         IF (KRET.EQ.-1) GO TO 180
C                                       Pick up some points
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,INC).NE.0) THEN
            IF (KRET.LT.0) GO TO 180
            GO TO 100
            END IF
C                                       Get array #
         IARR = SUBARR
         AVTIM = AVT
         IF (AVTIM.GT.TFIN) GO TO 180
         IF (AVTIM.LT.TBEG) THEN
            IF (KRET.LT.0) GO TO 180
            GO TO 100
            END IF
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) THEN
            IF (KRET.LT.0) GO TO 180
            GO TO 100
            END IF
C                                       bad points recognition
         IF (SEARCH) THEN
            CALL BADREC (SCANV, NLVIS, DTT, MBASE, MSAMP, MPOL, MCHAN,
     *         MIF, XVAL, YVAL, LLBAS, NBAD)
            IF (KRET.LT.0) GO TO 180
            GO TO 100
            END IF
         IF (ISOLIN.GE.MSAMP) GO TO 180
         ISOLIN = ISOLIN + 1
         DO 160 I = 1,NBASE
            IA1 = ANT1(I)
            IA2 = ANT2(I)
C                                       any good ones?
            DO 135 LPOL = 1,MPOL
            DO 135 LCHAN = 1,LECH
            DO 135 LIF = 1,LEIF
               IF (SCANV(3,I,LPOL,LCHAN,LIF).GT.0.0) GO TO 140
 135           CONTINUE
            GO TO 160
C                                       store values of data
 140        INDEX = ADD(ISOLIN) + IBASE(ISOLIN) + 1
            IF (INDEX.GT.MSAMP) GO TO 180
            XUMVIS = XUMVIS + 1
            NPOINT(I) = NPOINT(I) + 1
            IBASE(ISOLIN) = IBASE(ISOLIN) + 1
            LBASE(INDEX) = I
            FPARM(1+ILOCU) = AVU(I)
            FPARM(1+ILOCV) = AVV(I)
            FPARM(1+ILOCW) = AVW(I)
            FPARM(1+ILOCT) = AVT
C                                       loop over IFs
            DO 150 LPOL = 1,MPOL
            DO 150 LCHAN = 1,LECH
            DO 150 LIF = 1,LEIF
               CALL RCOPY (4, SCANV(1,I,LPOL,LCHAN,LIF), VISO)
C                                       Y axis
               IF (VISO(3).LE.0.0) THEN
                  YVAL(INDEX,LPOL,LCHAN,LIF) = FBLANK
                  IF (AMPPH) THEN
                     PHVAL(INDEX,LPOL,LCHAN,LIF) = FBLANK
                  ELSE IF (APARM(3).GT.0.0) THEN
                     AMMIN(INDEX,LPOL,LCHAN,LIF) = FBLANK
                     AMMAX(INDEX,LPOL,LCHAN,LIF) = FBLANK
                     END IF
               ELSE
                  CALL FNDXY (VISO, FPARM, XY)
                  XVAL(INDEX) = XY(1)
                  YVAL(INDEX,LPOL,LCHAN,LIF) = XY(2)
                  IF (AMPPH) THEN
                     PHVAL(INDEX,LPOL,LCHAN,LIF) = XY(3)
                  ELSE IF (APARM(3).GT.0) THEN
                     AMMIN(INDEX,LPOL,LCHAN,LIF) =
     *                  AMNMX(1,I,LPOL,LCHAN,LIF) * DMULT
                     AMMAX(INDEX,LPOL,LCHAN,LIF) =
     *                  AMNMX(2,I,LPOL,LCHAN,LIF) * DMULT
                     END IF
C                                       autoscale for min/max lines
                  IF (.NOT.NOSCAL) THEN
                     IF (APARM(3).GT.0) THEN
                        XY(2) = AMMIN(INDEX,LPOL,LCHAN,LIF)
                        CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, I, IRET)
                        XY(2) = AMMAX(INDEX,LPOL,LCHAN,LIF)
                        CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, I, IRET)
                     ELSE
                        CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, I, IRET)
                        END IF
                     IF (IRET.GT.0) THEN
                        WRITE (MSGTXT,1250) IRET
                        GO TO 990
                        END IF
                     END IF
                  END IF
C                                       prepare values of the model
               IF (DOMODL.EQ.2) CALL MODPTS (AVU(I), AVV(I),
     *            AVW(I), TYPEAX(2), MDAVAL(INDEX), MDPVAL(INDEX))
 150           CONTINUE
 160        CONTINUE
         ADD(ISOLIN+1) = ADD(ISOLIN) + IBASE(ISOLIN)
C                                       last vis. has been read
         IF (KRET.LT.0) GO TO 180
         END IF
      GO TO 100
C                                       Close at end
 180  CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      NSOLIN = ISOLIN
C                                       Close FG table
      IF (SEARCH) CALL TABFLG ('CLOS', BUFF, KFGRNO, FGKOLS, FGNUMV,
     *   IDSOU, ISUB, FQID, ANTSST, TIMERT, IFST, CHANST, TFLAGS, TREAS,
     *   IRET)
C                                       Any points found
      IF (NOSCAL .OR. SEARCH) GO TO 220
         IF (XUMVIS.GT.1) GO TO 210
            IRET = 4
            WRITE (MSGTXT,1200) XUMVIS
            GO TO 990
C                                       Final call to XYSCL.
 210     NUMVIS = -1
         CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, LOOP, IRET)
         IF (IRET.LE.0) GO TO 220
            WRITE (MSGTXT,1250) IRET
            GO TO 990
 220  IRET = 0
      IF (SEARCH) THEN
         IF (NBAD.EQ.0)THEN
            WRITE (MSGTXT,1300)
            CALL MSGWRT (4)
         ELSE
            WRITE (MSGTXT,1350) NBAD, OUTVER
            CALL MSGWRT (4)
            END IF
         END IF
      CALL COPY (256, CATKEP, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCALVB: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('SCALVB: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('FOUND',I4,' POINTS: NOT ENOUGH TO SELF-SCALE')
 1250 FORMAT ('SCALVB: XYSCL ERROR',I3)
 1300 FORMAT ('No bad points have been found.')
 1350 FORMAT ( I5,' bad points have been found and written in FG',
     *  ' table ',I3 )
      END
      SUBROUTINE VBPLOT (MAXPLT, MSAMP, MPOL, MCHAN, MIF, XVAL, YVAL,
     *   IBASE, ADD, LBASE, PHVAL, MDAVAL, MDPVAL, AMMIN, AMMAX, IRET)
C-----------------------------------------------------------------------
C   VBPLOT plots the data thru calls to PLTVB.
C   Inputs:
C      MAXPLT    I    The maximum number of plots per page.
C   Outputs:
C      IRET      I     Error code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   MAXPLT, MSAMP, MPOL, MCHAN, MIF, IBASE(*), ADD(*),
     *   LBASE(*), IRET
      REAL      XVAL(*), YVAL(MSAMP,MPOL,MCHAN,*),
     *   PHVAL(MSAMP,MPOL,MCHAN,*), MDAVAL(*), MDPVAL(*),
     *   AMMIN(MSAMP,MPOL,MCHAN,*), AMMAX(MSAMP,MPOL,MCHAN,*)
C
      INTEGER   IA1, IA2, IPLOT, LOOP, NLOOP, ILOOP, HMXPLT, TYPEX, I,
     *   LCH, LCH1, LCH2, LIF, LIF1, LIF2, ICH, IIF, IP, NN, ISOLIN,
     *   INDEX, JBASE
      REAL      DMS, DMO, DMSPH, DMOPH, TEMPSC, TEMPOF
      LOGICAL   TEMP
      INCLUDE 'VPLOT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      HMXPLT = MAXPLT
      IF (AMPPH) HMXPLT = MAXPLT / 2
      IRET = 0
      IPLOT = 0
      DMS = XYSCL(2)
      DMO = XYOFF(2)
      DMSPH = XYSCL(3)
      DMOPH = XYOFF(3)
      IF ((ICROWD.EQ.1) .OR. (ICROWD.EQ.3)) THEN
         LCH1 = 0
         LCH2 = 0
      ELSE
         LCH1 = 1
         LCH2 = MCHAN
         END IF
      IF ((ICROWD.EQ.2) .OR. (ICROWD.EQ.3)) THEN
         LIF1 = 0
         LIF2 = 0
      ELSE
         LIF1 = 1
         LIF2 = MIF
         END IF
C                                       Find last plot
      NLOOP = 0
      DO 100 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA2 = ANT2(LOOP)
C                                       are there data?
         DO 90 LIF = LIF1,LIF2
            DO 85 LCH = LCH1,LCH2
               IF (NPOINT(LOOP).GT.0) THEN
                  IF ((MIF*MCHAN.EQ.1) .OR. (ICROWD.EQ.3)) THEN
                     GO TO 70
                  ELSE
                     DO 60 ISOLIN = 1,NSOLIN
                        NN = IBASE(ISOLIN)
                        DO 10 I = 1,NN
                           INDEX = ADD(ISOLIN) + I
                           JBASE = LBASE(INDEX)
                           IF (JBASE.EQ.LOOP) GO TO 20
 10                        CONTINUE
                        GO TO 60
 20                     IF (ICROWD.EQ.2) THEN
                           DO 30 IIF = 1,MIF
                              DO 25 IP = 1,MPOL
                                 IF (YVAL(INDEX,IP,LCH,IIF).NE.FBLANK)
     *                              GO TO 70
 25                              CONTINUE
 30                           CONTINUE
                        ELSE IF (ICROWD.EQ.1) THEN
                           DO 40 ICH = 1,MCHAN
                              DO 35 IP = 1,MPOL
                                 IF (YVAL(INDEX,IP,ICH,LIF).NE.FBLANK)
     *                              GO TO 70
 35                              CONTINUE
 40                           CONTINUE
                        ELSE
                           DO 50 IP = 1,MPOL
                              IF (YVAL(INDEX,IP,LCH,LIF).NE.FBLANK)
     *                           GO TO 70
 50                           CONTINUE
                           END IF
 60                     CONTINUE
                     END IF
                  END IF
               GO TO 85
C                                       do this plot
 70            NLOOP = NLOOP + 1
 85            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       No baselines
      IF (NLOOP.LE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1980)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Do plots
      ILOOP = 0
      DO 200 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA2 = ANT2(LOOP)
C                                       are there data?
         DO 190 LIF = LIF1,LIF2
            DO 185 LCH = LCH1,LCH2
               IF (NPOINT(LOOP).GT.0) THEN
                  IF ((MIF*MCHAN.EQ.1) .OR. (ICROWD.EQ.3)) THEN
                     GO TO 170
                  ELSE
                     DO 160 ISOLIN = 1,NSOLIN
                        NN = IBASE(ISOLIN)
                        DO 110 I = 1,NN
                           INDEX = ADD(ISOLIN) + I
                           JBASE = LBASE(INDEX)
                           IF (JBASE.EQ.LOOP) GO TO 120
 110                       CONTINUE
                        GO TO 160
 120                    IF (ICROWD.EQ.2) THEN
                           DO 130 IIF = 1,MIF
                              DO 125 IP = 1,MPOL
                                 IF (YVAL(INDEX,IP,LCH,IIF).NE.FBLANK)
     *                              GO TO 170
 125                             CONTINUE
 130                          CONTINUE
                        ELSE IF (ICROWD.EQ.1) THEN
                           DO 140 ICH = 1,MCHAN
                              DO 135 IP = 1,MPOL
                                 IF (YVAL(INDEX,IP,ICH,LIF).NE.FBLANK)
     *                              GO TO 170
 135                             CONTINUE
 140                          CONTINUE
                        ELSE
                           DO 150 IP = 1,MPOL
                              IF (YVAL(INDEX,IP,LCH,LIF).NE.FBLANK)
     *                           GO TO 170
 150                          CONTINUE
                           END IF
 160                     CONTINUE
                     END IF
                  END IF
               GO TO 185
C                                       do this plot
 170           ILOOP = ILOOP + 1
               IAW1 = IA1
               IAW2 = IA2
               IPLOT = MOD (IPLOT, HMXPLT) + 1
               IF (IPLOT.EQ.1) THEN
                  CALL RFILL (50, 0.0, XANT)
                  CALL RFILL (50, 0.0, XBASE)
                  DO 180 I = 1,HMXPLT
                     XANT(I) = ANT1(LOOP+I-1)
                     XBASE(I) = ANT2(LOOP+I-1)
 180                 CONTINUE
                  END IF
               IF (FLOTEM) THEN
                  XYSCL(2) = YYMAX(LOOP)
                  XYOFF(2) = YYMIN(LOOP)
                  IF (IPHASE.EQ.2) THEN
                     IF (PPMAX(LOOP).GT.YYMAX(LOOP)) THEN
                        XYSCL(2) = PPMAX(LOOP)
                        XYOFF(2) = PPMIN(LOOP)
                        SPHASE = .TRUE.
                     ELSE
                        SPHASE = .FALSE.
                        END IF
                     END IF
                  END IF
               KPLOT = LOOP
               TYPEX = TYPEAX(2)
               IF (AMPPH) THEN
                  CALL PLTVB (2*IPLOT-1, MAXPLT, TYPEX, YVAL, MDAVAL, 0,
     *               DMS, DMO, MSAMP, MPOL, MCHAN, MIF, LCH, LIF, XVAL,
     *               IBASE, ADD, LBASE, AMMIN, AMMAX, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (FLOTEM) THEN
                     XYSCL(3) = PHMAX(LOOP)
                     XYOFF(3) = PHMIN(LOOP)
                     IF (PPMAX(LOOP).GT.PHMAX(LOOP)) THEN
                        XYSCL(3) = PPMAX(LOOP)
                        XYOFF(3) = PPMIN(LOOP)
                        SPHASE = .TRUE.
                     ELSE
                        SPHASE = .FALSE.
                        END IF
                     END IF
                  IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
                  TYPEX = TYPEAX(3)
                  TEMP = SCALEM(2)
                  TEMPSC = XYSCL(2)
                  TEMPOF = XYOFF(2)
                  SCALEM(2) = SCALEM(3)
                  XYSCL(2) = XYSCL(3)
                  XYOFF(2) = XYOFF(3)
                  CALL PLTVB (2*IPLOT, MAXPLT, TYPEX, PHVAL, MDPVAL, 2,
     *               DMSPH, DMOPH, MSAMP, MPOL, MCHAN, MIF, LCH, LIF,
     *               XVAL, IBASE, ADD, LBASE, AMMIN, AMMAX, IRET)
                  IF (IRET.NE.0) GO TO 999
                  SCALEM(2) = TEMP
                  XYSCL(2) = TEMPSC
                  XYOFF(2) = TEMPOF
               ELSE
                  IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
                  IF ((TYPEX.EQ.2) .OR. (TYPEX.EQ.10)) THEN
                     CALL PLTVB (IPLOT, MAXPLT, TYPEX, YVAL, MDPVAL, 0,
     *                  DMS, DMO, MSAMP, MPOL, MCHAN, MIF, LCH, LIF,
     *                  XVAL, IBASE, ADD, LBASE, AMMIN, AMMAX, IRET)
                  ELSE
                     CALL PLTVB (IPLOT, MAXPLT, TYPEX, YVAL, MDAVAL, 0,
     *                  DMS, DMO, MSAMP, MPOL, MCHAN, MIF, LCH, LIF,
     *                  XVAL, IBASE, ADD, LBASE, AMMIN, AMMAX, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  END IF
 185           CONTINUE
 190        CONTINUE
 200    CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('VBPLOT: NO BASELINES TO BE PLOTTED')
      END
      SUBROUTINE PLTVB (IPLOT, MAXPLT, TYPEX, YYVAL, MYVAL, KBPARM,
     *   DMS, DMO, MSAMP, MPOL, MCHAN, MIF, ICH, IIF, XVAL, IBASE, ADD,
     *   LBASE, AMMIN, AMMAX, IRET)
C-----------------------------------------------------------------------
C   PLTVB actually plots uv data one panel at a time.
C   Input:
C      IPLOT    I      Plot number on current page. If negative then
C                      this the last plot.
C      MAXPLT   I      The number of plots per page.
C      TYPEX    I      TYpe of Y axes
C      YYVAL    R(*)   Array of Y-values
C      KBPARM   I      0, or 2 identifies the fix scale in AMP&PHASE
C   In/out:            (Changed to user value on .not.scalem(2))
C      DMS      I      Scaling parameter for max y range
C      DMO      I      Low value parm for max y range
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, MAXPLT, TYPEX, KBPARM, MSAMP, MPOL, MCHAN, MIF,
     *   IIF, ICH, IBASE(*), ADD(*), LBASE(*), IRET
      REAL      XVAL(*), YYVAL(MSAMP,MPOL,MCHAN,*), MYVAL(*), DMS, DMO,
     *   AMMIN(MSAMP,MPOL,MCHAN,*), AMMAX(MSAMP,MPOL,MCHAN,*)
C
      INCLUDE 'VPLOT.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, CHTMP*18, ADATE*12,
     *   AUNITS(11)*8, CHTYPE(22,2)*9, POLLAB*5, STRING*8, SAVPRE(2)*5,
     *   TXTMSG*80
      HOLLERITH CATH(256)
      INTEGER   I, SEQ2, BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL,
     *   FINDPL, IAPARM(8), INCHAR, INP, IA1, IA2, JJJ, J, IAXLAB,
     *   IAPLOT, IT(3), ID(3), SVAXTP(2), INDEX, IAXL1, IAXL2, NGOOD,
     *   NNOFIT, NUMVIS, LTYPE, JBASE, NN, ISOLIN, J2, LIF, LTY, ISYM,
     *   LPOL, LCOLR, IROUND, LCH, LIF1, LIF2, LCH1, LCH2, NCOLR
      REAL      BLC(2), TRC(2), DX, DY, TR, TI, XY(2), FACTOR,
     *   CATR(256), SIZE, GERR, XTRC(2), XBLC(2), TLC(3), PLTINC,
     *   YYOFF(2), XZY(2), XMULT(2), XMIN, XMAX, YMIN, YMAX, XTEMP,
     *   BOTT, TOPP, YLAST, TEMP, COL(3), AX(5), AY(5), AMULT(2), V
      DOUBLE PRECISION    TRA, TDEC
      LOGICAL   T, F, GOOD, CATUP, PENUP, DO3C, DOLINE
      SAVE SAVPRE, AMULT, LTYPE, BUFFER
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATR, CATH)
      DATA LUNPL /26/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Janskys ', 'Degrees ',  'Wavlngth', 'Degrees ',
     *   'IAT days', 'GST hrs ','Seconds ', '1/Jy**2 ',' ', 'Hours',
     *   'log10 Jy'/
      DATA CHTYPE /'Amplitude', 'Phase   ', 'UV dist ',
     *   'UV pa   ', 'Time    ', 'U       ', 'V      ',
     *   'W       ', 'Real    ', 'Imag    ', 'GST     ',
     *   'Time    ', 'Source #', 'Freqid  ', 'Int.time',
     *   'Weight  ', 'Rms     ', 'HourAngle', 'Elevation', 'ParAngle',
     *   'Azimuth ', 'log(amp)',
     *   'RatioAmpl', 'RatioPhas', 'UV dist ',
     *   'UV pa   ', 'Time    ', 'U       ', 'V      ',
     *   'W       ', 'RatioReal', 'RatioImag', 'GST     ',
     *   'Time    ', 'Source #', 'Freqid  ', 'Int.time',
     *   'RatioWeit', 'Rms     ', 'HourAngle', 'Elevation', 'ParAngle',
     *   'Azimuth ', 'log(a/a)'/
C-----------------------------------------------------------------------
      LTY = 1
      IF (POLPLT.GT.0) LTY = 2
      XSTOK = STOKES
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
      CATUP = T
      ISYM = IROUND (XSYM)
C                                       force symbol to be '+' if the
C                                       vertical line with min/max is
C                                       plotted
      IF (APARM(3) .GT. 0) ISYM =1
      FACTOR = XFACT
      DOLINE = FACTOR.LT.0.0
      ISYM = ABS (ISYM)
      FACTOR = ABS (FACTOR)
      IF (UVREV) DOLINE = .FALSE.
      IF (DOMODL.GT.0) DOLINE = .FALSE.
      IF ((.NOT.DOLINE) .AND. ((ISYM.LE.0) .OR. (ISYM.GT.24))) ISYM = 2
      IF (MCHAN*MIF*MPOL.EQ.1) XDO3C = -1.0
      DO3C = XDO3C.GT.0.0
      IF (ICH.LE.0) THEN
         LCH1 = 1
         LCH2 = MCHAN
      ELSE
         LCH1 = ICH
         LCH2 = ICH
         END IF
      IF (IIF.LE.0) THEN
         LIF1 = 1
         LIF2 = MIF
      ELSE
         LIF1 = IIF
         LIF2 = IIF
         END IF
C                                       User sets the scales
      PLTINC = 1000. / MAXPLT
      DO 10 I = 1,2
         IF (.NOT.SCALEM(I)) THEN
            IF (I.EQ.1) THEN
               XYSCL(I) = INISCL(2)
               XYOFF(I) = INISCL(1)
               IF ((IPHASE.EQ.1) .AND. (INISCL(1).GE.0)) SPHASE = .TRUE.
            ELSE
               XYSCL(I) = INISCL(4 + KBPARM)
               XYOFF(I) = INISCL(3 + KBPARM)
               IF ((IPHASE.EQ.2+KBPARM/2) .AND. (XYOFF(I).GE.0.0))
     *            SPHASE = .TRUE.
               END IF
            IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
            SIZE = 1000.
            IF (I.EQ.2) SIZE = PLTINC
            XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
            IF (I.EQ.1) GO TO 10
            DMS = XYSCL(I)
            DMO = XYOFF(I)
            END IF
 10      CONTINUE
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      CALL FILL (5, 1, IAPARM)
C                                       add space between pairs of
C                                       plots in AMP&PH case
      IF (AMPPH) THEN
         BOTT = ((IABS(IPLOT) - 1)/2) * 4
      ELSE
         BOTT = 0.0
         END IF
      TOPP = 1
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - IABS (IPLOT) * PLTINC - BOTT
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - TOPP
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
      TLC(3) = XTRC(1)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.12) LABTYP(LOCNUM) = 7
      IF (TYPEAX(2).EQ.12) LABTYP(LOCNUM) = 70
      CALL COPY (2, TYPEAX, SVAXTP)
      AXTYP(LOCNUM) = 0
      IF (ABS(IPLOT).EQ.1) THEN
         TR = PLTINC / DMS
         TI = TR
         SAVPRE(2) = ' '
         IF (TYPEAX(2).NE.12) THEN
            CALL METSCL (LABEL, TR, SAVPRE(2), GOOD)
            AMULT(2) = TR / TI
         ELSE
            AMULT(2) = 360.0
            END IF
         TR = 1000. / XYSCL(1)
         TI = TR
         SAVPRE(1) = ' '
         IF (TYPEAX(1).NE.12) THEN
            CALL METSCL (LABEL, TR, SAVPRE(1), GOOD)
            AMULT(1) = TR / TI
         ELSE
            AMULT(1) = 360.0
            END IF
         END IF
      CPREF(1,LOCNUM) = SAVPRE(1)
      CPREF(2,LOCNUM) = SAVPRE(2)
      XMULT(1) = AMULT(1)
      XMULT(2) = AMULT(2)
      IF ((AMPPH) .AND. (MOD(ABS(IPLOT),2).EQ.0)) THEN
         XMULT(2) = 1.0
         CPREF(2,LOCNUM) = ' '
         END IF
      DO 50 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
         IF (TYPEAX(I).NE.12) GO TO 45
            CTYP(I,LOCNUM) = AUNITS(5)
            TYPEAX(I) = 5
            GO TO 50
 45      IF (TYPEAX(I).GT.5) CTYP(I,LOCNUM) = AUNITS(3)
         IF (TYPEAX(I).LE.5) CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
         IF ((TYPEAX(I).EQ.9) .OR. (TYPEAX(I).EQ.10))
     *      CTYP(I,LOCNUM) = AUNITS(1)
         IF (TYPEAX(I).EQ.11) CTYP(I,LOCNUM) = AUNITS(6)
         IF (TYPEAX(I).GT.12) CTYP(I,LOCNUM) = AUNITS(9)
         IF (TYPEAX(I).EQ.15) CTYP(I,LOCNUM) = AUNITS(7)
         IF (TYPEAX(I).EQ.16) CTYP(I,LOCNUM) = AUNITS(8)
         IF (TYPEAX(I).EQ.17) CTYP(I,LOCNUM) = AUNITS(1)
         IF (TYPEAX(I).EQ.18) CTYP(I,LOCNUM) = AUNITS(10)
         IF (TYPEAX(I).EQ.22) CTYP(I,LOCNUM) = AUNITS(11)
         IF ((TYPEAX(I).GE.19) .AND. (TYPEAX(I).LE.21)) CTYP(I,LOCNUM) =
     *      AUNITS(2)
         IF ((CTYP(I,LOCNUM).EQ.AUNITS(1)) .AND. (POLPLT.GT.0))
     *      CTYP(I,LOCNUM) = 'Ratio'
C                                       IF(DOIFRA) then amplitude is
C                                       the ratio => vertical axis
C                                       should be in 'Ratio'
         IF ((CTYP(I,LOCNUM).EQ.AUNITS(1)) .AND. DOIFRA)
     *      CTYP(I,LOCNUM) = 'Ratio'
 50      CONTINUE
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS(IPLOT).NE.MAXPLT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Determine STOKES label
      POLLAB = XSTOK
      IF (POLPLT.GT.0) THEN
         IF (POLPLT.EQ.1) POLLAB = 'RL/RR'
         IF (POLPLT.EQ.2) POLLAB = 'LR/RR'
         IF (POLPLT.EQ.3) POLLAB = 'LL/RR'
         IF (POLPLT.EQ.4) POLLAB = 'RL/LL'
         IF (POLPLT.EQ.5) POLLAB = 'LR/LL'
         IF (POLPLT.EQ.6) POLLAB = 'RR/LL'
         IF (POLPLT.EQ.7) POLLAB = 'Q/I'
         IF (POLPLT.EQ.8) POLLAB = 'U/I'
         IF (POLPLT.EQ.9) POLLAB = 'V/I'
         END IF
      CALL CHR2H (8, POLLAB, 1, XPPLOT)
C                                       Create plot file
      IF (ABS(IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, OLDCNO, CATUV, BUFFER, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
C                                       Fill in last of actual parms
         BPARM(5) = 1000.0/XYSCL(1) + XYOFF(1)
         BPARM(7) = PLTINC/XYSCL(2) + XYOFF(2)
         BPARM(4) = XYOFF(1)
         BPARM(6) = XYOFF(2)
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 21
         CALL GINIT (DISKIN, OLDCNO, PFILE, IPSIZE, ITYPE, NPARMS,
     *      USERID, DOTV, TVCHN, GRCHN, TVCORN, CATUV, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
         LTYPE = MOD (ABS (LABEL), 100)
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 5.5
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *      CHOUT(4) = CHOUT(4) + 1.333
C                                       default XYRATIO
         IF (XYRATO.LT.0.01) THEN
            IF (DOTV) THEN
               XMIN = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *            + CHOUT(3))
               YMIN = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *            + CHOUT(4))
               XYRATO = 1.0
               IF (YMIN.GT.0.0) XYRATO = XMIN / YMIN
            ELSE
               XYRATO = 1.0
               END IF
            END IF
C                                       Init for line drawing.
         IRET = 3
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1100) VER
            CALL MSGWRT (2)
            END IF
         END IF
      IRET = 3
      CATUP = T
C                                       Set max and min of axis
      XMAX = TRC(1) / XYSCL(1) + XYOFF(1)
      XMIN = BLC(1) / XYSCL(1) + XYOFF(1)
      YMAX = PLTINC / XYSCL(2) + XYOFF(2)
      YMIN = BLC(2) / XYSCL(2) + XYOFF(2)
C                                       Draw border
      WRITE (TXTMSG,1050) IAW1, IAW2
      CALL GCOMNT (-1, TXTMSG, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      IF ((ABS(IPLOT).EQ.1) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Data information
         DX = 0.0
         DY = 0.5
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C
         INP = 1
         IF ((DOIAVG) .OR. (LIF1.NE.LIF2) .OR. (DOIFRA) .OR.
     *      (KBIF.EQ.KEIF)) THEN
            WRITE (STRING,1300) BIF
            TEXT = 'IF ' // STRING(7:8)
            INP = INP + 5
            IF (EIF.NE.BIF) THEN
               WRITE (STRING,1300) EIF
               TEXT(INP:) = ' - ' // STRING(7:8)
               IF (DOIFRA) TEXT(INP:) = ' / ' // STRING(7:8)
               INP = INP + 5
               END IF
            INP = INP + 1
            END IF
         IF ((DOCAVG) .OR. (LCH1.NE.LCH2) .OR. (KBCH.EQ.KECH)) THEN
            WRITE (STRING,1300) BCHAN
            IF (INP.GT.1) THEN
               TEXT(INP:) = '__'
               INP = INP + 2
               END IF
            TEXT(INP:) = 'CHAN' // STRING(3:8)
            INP = INP + 10
            IF (ECHAN.NE.BCHAN) THEN
               WRITE (STRING,1300) ECHAN
               TEXT(INP:) = ' -' // STRING(3:8)
               INP = INP + 8
               END IF
            INP = INP + 1
            END IF
         IF (INP.NE.1) THEN
            TEXT(INP:) = '__'
            INP = INP + 2
            END IF
         TEXT(INP:) = 'STK ' // POLLAB
         INP = INP + 9
         IF (NUMCC.NE.0) THEN
            TEXT(INP:) = ' _of_ '
            INP = INP + 6
            CHTMP = NAM2IN // CLA2IN
            SEQ2 = XS2IN + 0.1
            CALL NAMEST (CHTMP, SEQ2, TEXT(INP:), INCHAR)
            END IF
C
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Plot type and file
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = CHTYPE(TYPEAX(2),LTY)
         INP = 10
         IF (AMPPH) THEN
            TEXT(INP:) = ' and' // CHTYPE(TYPEAX(3),LTY)(1:8)
            INP = INP + 13
            END IF
         TEXT(INP:) = ' vs ' // CHTYPE(TYPEAX(1),LTY)(1:8) // ' _for_ '
         INP = INP + 20
         CHTMP = NAMEIN // CLAIN
         CALL NAMEST (CHTMP, CATUV(KIIMS), TEXT(INP:), INCHAR)
         INP = INP + 1 + INCHAR
C                                       scalar/vector
         IF ((SOLINT.GT.0) .OR. (BIF.NE.EIF) .OR. (BCHAN.NE.ECHAN)) THEN
            IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.2) .OR. (DTYPE.EQ.9) .OR.
     *         (DTYPE.EQ.10) .OR. (DTYPE.EQ.17) .OR. (DTYPE.EQ.22) .OR.
     *         (BPARM(2).LT.0)) THEN
               IF (AVGSCA) THEN
                  TEXT(INP:) = '_Scal aver.'
               ELSE
                  TEXT(INP:) = '_Vect aver.'
                  END IF
               INP = INP + 12
               END IF
            END IF
C
         IF (DOCAL) THEN
            WRITE (TEXT(INP:),1250) 'CL', CLUSE
            INP = INP + 10
            END IF
         IF ((FGVER.GT.0) .AND. (.NOT. AMPPH)) THEN
            WRITE (TEXT(INP:),1250) 'FG', FGVER
            END IF
C
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) THEN
            TEXT = ' '
            DY = DY + 1.333
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1200) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Only label Y axis once.
      IAPLOT = ABS (IPLOT)
      IF (AMPPH) THEN
         IF ((MAXPLT.GE.2) .AND. (MAXPLT.LE.4)) THEN
            IAXL1 = 1
            IAXL2 = MAXPLT
            END IF
         IF ((MAXPLT.GE.6) .AND. (MAXPLT.LE.10)) THEN
            IAXL1 = 3
            IAXL2 = 6
            END IF
         IF (MAXPLT.GT.10) THEN
            IAXL1 = 2 * INT(MAXPLT/8) + 1
            IAXL2 = 2 * INT(3*MAXPLT/8) + 2
            END IF
         IF (IAPLOT.NE.IAXL1) THEN
            IF (IAPLOT.EQ.IAXL2) THEN
               CTYP(2,LOCNUM) = AUNITS(2)
            ELSE
               CPREF(2,LOCNUM) = '-1'
               END IF
            END IF
      ELSE
         IAXLAB = MAXPLT / 2 + 1
C         IF ((TYPEAX(2).EQ.16) .OR. ((IAPLOT.NE.IAXLAB) .AND.
C     *      ((IPLOT.GE.0) .OR. (IAPLOT.GT.IAXLAB)))) CPREF(2,LOCNUM) =
C     *      '-1'
         IF (((IAPLOT.NE.IAXLAB) .AND.
     *      ((IPLOT.GE.0) .OR. (IAPLOT.GT.IAXLAB)))) CPREF(2,LOCNUM) =
     *      '-1'
         END IF
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = 8.0/SQRT(FLOAT(MAXPLT)) * FACTOR
      IF (DX.LT.2.5) DX = 2.5 * FACTOR
      DY = DX
      IF (DX/XYRATO.LT.2.5) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      NUMVIS = 0
C                                       Loop
      IA1 = ANT1(KPLOT)
      IA2 = ANT2(KPLOT)
      WRITE (TXTMSG,1055) IAW1, IAW2
      CALL GCOMNT (-1, TXTMSG, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      LCOLR = 0
      NCOLR = MPOL * (LCH2-LCH1+1) * (LIF2-LIF1+1)
      NCOLR = MAX (2, NCOLR)
      DO 105 LPOL = 1,MPOL
      DO 105 LIF = LIF1,LIF2
      DO 105 LCH = LCH1,LCH2
         IF (XDO3C.GT.0.0) THEN
            TEMP = (LCOLR) / (NCOLR - 1.0)
            TEMP = 1.0 - TEMP
            LCOLR = LCOLR + 1
            CALL COLOR3 (TEMP, .FALSE., COL)
            CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         PENUP = .TRUE.
         DO 100 ISOLIN = 1,NSOLIN
            NN = IBASE(ISOLIN)
            DO 60 I = 1,NN
               INDEX = ADD(ISOLIN) + I
               JBASE = LBASE(INDEX)
               IF (JBASE.EQ.KPLOT) GO TO 70
 60            CONTINUE
            GO TO 100
C                                       found data
 70         IF ((SPHASE) .AND. (IPHASE.EQ.1)) THEN
               V = XVAL(INDEX)
               IF (V.LT.0.0) V = V + 360.0
               XZY(1) = V
            ELSE
               XZY(1) = XVAL(INDEX)
               END IF
            J2 = 1
            IF (UVREV) J2 = 2
            IF ((SPHASE) .AND. (IPHASE.GT.1)) THEN
               V = YYVAL(INDEX,LPOL,LCH,LIF)
               IF (V.LT.0.0) V = V + 360.0
               XZY(2) = V
            ELSE
               XZY(2) = YYVAL(INDEX,LPOL,LCH,LIF)
               END IF
            IF (XZY(2).NE.FBLANK) THEN
               DO 90 JJJ = 1,J2
                  DO 75 J = 1,2
                     XY(J) = XYSCL(J) * (XZY(J)-XYOFF(J)) + YYOFF(J)
                     IF ((XY(J).LE.XBLC(J)) .OR. (XY(J).GT.XTRC(J)))
     *                  THEN
                        NNOFIT = NNOFIT + 1
                        GO TO 85
                        END IF
 75                  CONTINUE
                  NGOOD = NGOOD + 1
C                                       Mark the point
                  IF (DOERRB) THEN
                     DY = 5.0
C                                       temporarily GERR = 1
                     GERR = 1.0
                     IF ((GERR*2.0).GT.5.0) DY = GERR / 2.0
                     END IF
                  AX(1) = XY(1)
                  AY(1) = XY(2)
                  IF (DOLINE) THEN
                     IF (PENUP) THEN
                        CALL GPOS (AX, AY, BUFFER, IERR)
                     ELSE IF (XDO3C.GT.0.0) THEN
                        CALL G3VEC (AX, AY, BUFFER, IERR)
                     ELSE
                        CALL GVEC (AX, AY, BUFFER, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 970
                     PENUP = .FALSE.
                     END IF
                  IF (ISYM.GT.0) THEN
                     AX(2) = AX(1)
                     AX(3) = AX(1)
                     AX(4) = AX(1) - DX
                     AX(5) = AX(1) + DX
C                                       fix 'up' of the vertical dash
C                                       to max in unaverage points
                     XTEMP = XY(2) + DY
                     IF ((APARM(3).GT.0) .AND.
     *                  ((SOLINT.GT.0) .OR. (DOIAVG))) XTEMP = XYSCL(2)
     *                  * (AMMAX(INDEX,LPOL,LCH,LIF) - XYOFF(2))
     *                  + YYOFF(2)
                     IF (XTEMP.GT.XTRC(2)) XTEMP = XTRC(2)
                     IF (XTEMP.LT.XBLC(2)) XTEMP = XBLC(2)
                     AY(2) = XTEMP
                     AY(5) = AY(1)
C                                       fix 'down' of the vertical dash
C                                       to min in unaverage points
                     XTEMP = XY(2) - DY
                     IF ((APARM(3).GT.0) .AND.
     *                  ((SOLINT.GT.0 .OR. DOIAVG))) XTEMP = XYSCL(2)
     *                  * (AMMIN(INDEX,LPOL,LCH,LIF) - XYOFF(2))
     *                  + YYOFF(2)
                     IF (XTEMP.GT.XTRC(2)) XTEMP = XTRC(2)
                     IF (XTEMP.LT.XBLC(2)) XTEMP = XBLC(2)
                     AY(3) = XTEMP
                     AY(4) = AY(1)
                     CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *                  DO3C, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     IF (DOLINE) THEN
                        CALL GPOS (AX, AY, BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        END IF
                     END IF
C                                       Plot u,v,w ?
 85               IF (UVREV) THEN
                     XZY(1) = -XZY(1)
                     XZY(2) = -XZY(2)
                     END IF
 90               CONTINUE
               END IF
 100        CONTINUE
 105     CONTINUE
C                                       Plot model at samples
      CALL GLTYPE (3, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (DOMODL.EQ.2) THEN
         PENUP = .TRUE.
         YLAST = -100000.
         DO 150 ISOLIN = 1,NSOLIN
            NN = IBASE(ISOLIN)
            DO 110 I = 1,NN
               INDEX = ADD(ISOLIN) + I
               JBASE = LBASE(INDEX)
               IF (JBASE.EQ.KPLOT) GO TO 120
 110           CONTINUE
            GO TO 150
C                                       got data
 120        XZY(1) = XVAL(INDEX)
            V = MYVAL(INDEX)
            IF ((SPHASE) .AND. (IPHASE.GT.1) .AND. (V.LT.0.0))
     *         V = V + 360.0
            XZY(2) = V
            DO 125 J = 1,2
               XY(J) = XYSCL(J) * (XZY(J)-XYOFF(J)) + YYOFF(J)
               IF ((XY(J).LE.XBLC(J)) .OR. (XY(J).GT.XTRC(J))) THEN
                  PENUP = .TRUE.
                  GO TO 150
                  END IF
  125          CONTINUE
C                                       Mark the point
            IF ((TYPEX.EQ.2) .AND. (ABS(YLAST-XZY(2)).GT.180.))
     *         PENUP = .TRUE.
            IF (PENUP) THEN
               CALL GPOS (XY(1), XY(2), BUFFER, IERR)
            ELSE
               CALL GVEC (XY(1), XY(2), BUFFER, IERR)
               END IF
               IF (IERR.NE.0) GO TO 970
            PENUP = .FALSE.
            YLAST = XZY(2)
 150        CONTINUE
         END IF
C                                       plot scan boundaries
      IF ((APARM(8).GT.0.0) .AND. (NOSCAN.GT.0)) THEN
         WRITE (TXTMSG,1150) NOSCAN
         CALL GCOMNT (-1, TXTMSG, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         I = 3
         IF (DOMODL.EQ.2) I = 2
         CALL GLTYPE (I, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         AY(1) = XYSCL(2) * (YMIN -XYOFF(2)) + YYOFF(2)
         AY(2) = XYSCL(2) * (YMAX -XYOFF(2)) + YYOFF(2)
         IF (APARM(8).LT.1.5) AY(2) = AY(1) + 0.1*(AY(2)-AY(1))
         DO 160 I = 1,NOSCAN
            IF ((TSCAN(I).GT.XMIN) .AND. (TSCAN(I).LT.XMAX)) THEN
               AX(1) = XYSCL(1) * (TSCAN(I)-XYOFF(1)) + YYOFF(1)
               CALL GPOS (AX(1), AY(1), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GVEC (AX(1), AY(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
 160        CONTINUE
         END IF
C                                       Plot model
      TRA = RA
      TDEC = DEC
      IF ((ABS(RA-OBSRA).LE.1.5) .AND. (ABS(DEC-OBSDEC).LE.1.5)) THEN
         TRA = OBSRA
         TDEC = OBSDEC
         END IF
C                                       panel labels too
      CALL VBMDL (TRA, TDEC, XMIN, XMAX, YMIN, YMAX, YYOFF, TLC,
     *   TYPEAX(1), TYPEX, KBPARM, ICH, IIF, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL COPY (2, SVAXTP, TYPEAX)
C                                       Done: finish plot
      WRITE (MSGTXT,1400) NGOOD, IAW1, IAW2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1500) NNOFIT, IAW1, IAW2
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      IF ((IPLOT.GT.0) .AND. (ABS (IPLOT).LT.MAXPLT)) GO TO 210
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
 210  IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1600)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATUV, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1700)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1400) NGOOD, IAW1, IAW2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1500) NNOFIT, IAW1, IAW2
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATUV, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Plotting baseline ',I2,' - ',I2,' labeling')
 1055 FORMAT ('Plotting baseline ',I2,' - ',I2,' data')
 1100 FORMAT ('PLot file version',I4,'  created.')
 1150 FORMAT ('Plotting',I4,' scan breaks')
 1200 FORMAT ('PLot file version',I4,'__created ',A,A)
 1250 FORMAT ('_',A,' #',I4)
 1300 FORMAT (I8)
 1400 FORMAT ('PLTVB:',I10,' points plotted: baseline ',I2,' - ',I2)
 1500 FORMAT ('PLTVB:',I10,' points did not fit: baseline ',I2,' - ',I2)
 1600 FORMAT ('PLTVB: ERROR DURING GRAPH FILE CREATION')
 1700 FORMAT ('PLTVB: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE FNDXY (BUFR, RPARM, XY)
C-----------------------------------------------------------------------
C   FNDXY extracts the desired X and Y values from the Vis record.
C   Inputs:
C      BUFR    R(*)   Visibility record
C      RPARM   R(*)   Random parms
C   Outputs:
C      XY      R(3)   X, Y values
C-----------------------------------------------------------------------
      REAL      BUFR(*), XY(3), RPARM(*)
C
      DOUBLE PRECISION GMST, GAST, TT
      INTEGER   NV, I, J, II, IA1, IA2
      REAL      TR, TI, H1, E1, H2, E2, A1, A2, S1, S2, C1, C2
      INCLUDE 'VPLOT.INC'
      REAL      PA(MAXANT)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (ILOCB.GE.0) THEN
         IA1 = RPARM(ILOCB+1) / 256. + 0.1
         IA2 = RPARM(ILOCB+1) - 256. * IA1 + 0.1
      ELSE
         IA1 = RPARM(ILOCA1+1) + 0.1
         IA2 = RPARM(ILOCA2+1) + 0.1
         END IF
C                                       Loop over axes
      DO 900 I = 1,2
         NV = 0
         TR = BUFR(1)
         TI = BUFR(2)
         XY(I) = 0.0
         J = TYPEAX(I)
         GO TO (110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     *      210, 150, 230, 240, 250, 260, 270, 280, 280, 300, 280,
     *      320), J
C                                       amplitude
 110     XY(I) = SQRT (TR*TR + TI*TI)
         IF (.NOT.AMPPH) GO TO 900
C                                       phase
 120     II = I
         IF (AMPPH) II = 3
         IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(II) = RAD2DG *
     *      ATAN2 (TI, TR)
         GO TO 900
C                                       U, V distance
 130     XY(I) = SQRT (RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2)
         GO TO 900
C                                       U, V position angle
 140     TR = RPARM(1+ILOCU)
         TI = RPARM(1+ILOCV)
         IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(I) = RAD2DG *
     *      ATAN2 (TI, TR)
         GO TO 900
C                                       Time
 150     XY(I) = RPARM(1+ILOCT)
         GO TO 900
C                                       U projected spacing
 160     XY(I) = RPARM(1+ILOCU)
         GO TO 900
C                                       V projected spacing
 170     XY(I) = RPARM(1+ILOCV)
         GO TO 900
C                                       W projected spacing
 180     XY(I) = RPARM(1+ILOCW)
         GO TO 900
C                                       Real , Imag parts
 190     XY(I) = TR
         GO TO 900
C                                       Imag part
 200     XY(I) = TI
         GO TO 900
C                                       Folded G.S.T.
 210     CALL SIDERL (JDREF, RPARM(1+ILOCT), IATUTC, GMST, GAST)
         XY(I) = GAST
         GO TO 900
C                                       Source no.
 230     IF (ILOCSU.GE.0) THEN
            XY(I) = RPARM(1+ILOCSU)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       FREQID
 240     IF (ILOCFQ.GE.0) THEN
            XY(I) = RPARM(1+ILOCFQ)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       Int. time
 250     IF (ILOCIT.GT.0) THEN
            XY(I) = RPARM(1+ILOCIT)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       Weight
 260     XY(I) = BUFR(3)
         GO TO 900
C                                       RMS
 270     XY(I) = BUFR(4)
         GO TO 900
C                                       HA, Elevation, azimuth
 280     TT = RPARM(1+ILOCT)
         IF (REFANT.GT.0) THEN
            CALL SOUELV (REFANT, TT, H1, E1, A1)
            IF (J.EQ.18) THEN
               XY(I) = H1 * RAD2DG / 15.0
            ELSE IF (J.EQ.21) THEN
               XY(I) = A1 * RAD2DG
            ELSE
               XY(I) = E1 * RAD2DG
               END IF
         ELSE
            CALL SOUELV (IA1, TT, H1, E1, A1)
            CALL SOUELV (IA2, TT, H2, E2, A2)
            IF (J.EQ.18) THEN
               S1 = SIN (H1)
               S2 = SIN (H2)
               C1 = COS (H1)
               C2 = COS (H2)
               XY(I) = ATAN2 (S1+S2, C1+C2) * RAD2DG / 15.0
            ELSE IF (J.EQ.21) THEN
               S1 = SIN (A1)
               S2 = SIN (A2)
               C1 = COS (A1)
               C2 = COS (A2)
               XY(I) = ATAN2 (S1+S2, C1+C2) * RAD2DG
            ELSE
               S1 = SIN (E1)
               S2 = SIN (E2)
               C1 = COS (E1)
               C2 = COS (E2)
               XY(I) = ATAN2 (S1+S2, C1+C2) * RAD2DG
               END IF
            END IF
         GO TO 900
C                                       Parallactic angle
 300     H1 = RPARM(1+ILOCT)
         CALL PARANG (H1, PA)
         IF (REFANT.GT.0) THEN
            XY(I) = PA(REFANT) * RAD2DG
         ELSE
            S1 = SIN (PA(IA1))
            S2 = SIN (PA(IA2))
            C1 = COS (PA(IA1))
            C2 = COS (PA(IA2))
            XY(I) = ATAN2 (S1+S2, C1+C2) * RAD2DG
            END IF
         GO TO 900
C                                       amplitude
 320     XY(I) = SQRT (TR*TR + TI*TI)
         XY(I) = LOG10 (MAX (1.E-12, XY(I)))
         IF (AMPPH) GO TO 120
         GO TO 900
C
 900     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MODPTS (U, V, W, NPLO, ADATA, PDATA)
C-----------------------------------------------------------------------
C   Subroutine to return 1 model value for a given set of clean
C   components and for a single data sample
C   Inputs: U, V, W   R    Sample coordinate
C           NPLO      I    type of Y-axis: 1 --> amplitude
C                              2 --> phase
C                              3 --> u,v distance
C                              4 --> u,v p.a.
C                              5 --> time (iat days)
C                              6 --> u
C                              7 --> v
C                              8 --> w
C                              9 --> sine channel
C                             10 --> cosine channel
C                             11 --> GAST
C                             22 --> log10 (amplitude)
C   Input from common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X-pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      REAL      U, V, W, ADATA, PDATA
C
      DOUBLE PRECISION    FREP, GAST, RA, DEC, DRA, DDEC, AA
      REAL      CSUM, SSUM, TEMP, AMODL, FAZ, ARG, FTEMP
      INTEGER   NPLO, K, IERR, IT1, IT2, IT3
      CHARACTER OPCODE*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'VPLOT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       program constants
      OPCODE = ' '
      GAST = 0
      IERR = 1
      RA = DRA * DG2RAD
      DEC = DDEC * DG2RAD
      FREP = AFREQ
      IT1 = IAW1
      IT2 = IAW2
      IT3 = 1
      ARG = -10.0
C                                     Baseline plots here
      CSUM = 0.0
      SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
      IF (DOSPHE) THEN
         DO 125 K = 1,NUMCC
            AA = GAUSA(K) * SQRT (U*U + V*V)
            IF (AA.LT.6.28D-2) AA = 6.28D-2
            FTEMP = 3.0D0 * SFLUX(K) *
     *         ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
            TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) + W * CCPOS(3,K)
            SSUM = SSUM + FTEMP * SIN (TEMP)
            CSUM = CSUM + FTEMP * COS (TEMP)
 125        CONTINUE
C                                       Gaussian
      ELSE IF (DOGAUS) THEN
         DO 130 K = 1,NUMCC
            ARG = U*U*GAUSA(K) + U*V*GAUSB(K) + V*V*GAUSC(K)
            IF (ARG.GT.-8.0) THEN
               FTEMP = SFLUX(K) * EXP(ARG)
               TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) + W * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
               END IF
 130        CONTINUE
C                                       Point
      ELSE
         DO 135 K = 1,NUMCC
            FTEMP = SFLUX(K)
            TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) + W * CCPOS(3,K)
            SSUM = SSUM + FTEMP * SIN (TEMP)
            CSUM = CSUM + FTEMP * COS (TEMP)
 135        CONTINUE
         END IF
C                                       Finished computing model
      AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
      IF ((NPLO.EQ.9) .OR. (NPLO.EQ.10)) THEN
         ADATA = CSUM
         PDATA = SSUM
      ELSE
         ADATA = AMODL
         PDATA = 0.0
         IF (AMODL.GT.1.E-10) THEN
            FAZ = 0.0
            IF ((SSUM.NE.0.0) .OR. (CSUM.NE.0.0)) FAZ = ATAN2 (SSUM,
     *         CSUM) / DG2RAD
 140        IF (FAZ.GT.180.0E0) FAZ = FAZ - 360.0E0
               IF (FAZ.LT.-180.0E0) FAZ = FAZ + 360.0E0
               IF (FAZ.GT.180.0E0) GO TO 140
               IF (FAZ.LT.-180.0E0) GO TO 140
               PDATA = FAZ
            END IF
         END IF
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE MODSET (XMIN, XMAX, MPLO, NPLO, DRA, DDEC, XDATA,
     *   YDATA, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set up model values for a given set of clean
C   components and for a given pair of antennas, or closure triangle
C
C   Inputs: XMIN,XMAX R    min and max values of the X-axis
C           MPLO      I    type of X-axis: 1 --> amplitude
C                              2 --> phase
C                              3 --> u,v distance
C                              4 --> u,v p.a.
C                              5 --> time (iat days)
C                              6 --> u
C                              7 --> v
C                              8 --> w
C                              9 --> sine channel
C                             10 --> cosine channel
C                             11 --> GAST
C                             22 --> log10 (amplitude)
C          NPLO      I     Type of Y-axis in the same manner
C          DRA,DDEC  D     Ra and Dec of the source at the epoch of
C                          observation (in degrees, and as D).
C   Input from common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X-pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      REAL      XMIN, XMAX, XDATA(*), YDATA(2,*)
      INTEGER   MPLO, NPLO, IERR
      DOUBLE PRECISION    DRA, DDEC
C
      DOUBLE PRECISION    U(2), V(2), W(2), PI, TWOPI, DTR, HTR, FREP,
     *   BX, BY, BZ, B1, B2, GH, GMST, GAST, RA, DEC, GSEC, AA, RATE,
     *   XBL(2), YBL(2), ZBL(2)
      REAL      XXPOS, PA, XDEL, XREL, HAI, BL, CSUM, SSUM, TEMP, AMODL,
     *   FAZ, ARG, FTEMP
      INTEGER   I, K, J, NUV
      CHARACTER OPCODE*4
      INCLUDE 'INCS:DDCH.INC'
C                                       add DSEL to have SUBARR. LK
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'VPLOT.INC'
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      IF ((MPLO.LT.1) .OR. (MPLO.GT.15)) GO TO 990
C                                       program constants
      OPCODE = ' '
      PI = 3.14159265358979D0
      TWOPI = 2.D0 * PI
      DTR = PI / 180.D0
      HTR = PI / 12.D0
      U(1) = 0.0D0
      V(1) = 0.0D0
      W(1) = 0.0D0
      GAST = 0
      IERR = 1
      RA = DRA * DTR
      DEC = DDEC * DTR
      FREP = AFREQ
      XBL(1) = XB(IAW1)
      YBL(1) = YB(IAW1)
      ZBL(1) = ZB(IAW1)
      XBL(2) = XB(IAW2)
      YBL(2) = YB(IAW2)
      ZBL(2) = ZB(IAW2)
C                                     divide X-axis into 200 pixels
      XDEL = (XMAX - XMIN) / 200.0
      XXPOS = XMIN - XDEL
      IF ((MPLO.EQ.5) .OR. (MPLO.EQ.11) .OR. (MPLO.EQ.12)) THEN
         CALL GSTROT (JDREF, GMST, GAST, RATE)
         GAST = GAST * (24.D0/360.D0)
C         GAST = GAST + ((XMIN*24.D0) * GSEC)
C                                       subract (SUB-1)*5 for the DBCON
C                                       output
         GAST = GAST + (((XMIN-(SUBARR-1)*5)*24.D0) * GSEC)
         GAST = MOD (GAST, 24.D0)
         END IF
C                                     determine baseline parms
      CALL BASLIN (2, XBL, YBL, ZBL, FREP, ANLONG, BX, BY, BZ,
     *   B1, B2, GH)
C
      XREL = XDEL
      IF ((MPLO.EQ.5).OR.(MPLO.EQ.12)) XDEL = XDEL * 24.0
      GAST = GAST - GSEC*XDEL
      ARG = -10.0
C                                     Baseline plots here
      DO 200 I = 1,200
         YDATA(1,I) = FBLANK
         YDATA(2,I) = FBLANK
         XXPOS = XXPOS + XREL
         XDATA(I) = XXPOS
C                                       compute u and v from X-axis type
         GO TO (210, 210, 70, 80, 15, 100, 110, 115, 210, 210, 30, 15,
     *      210, 210, 210, 210), MPLO
C                                       IAT in days
  15        NUV = 1
            GAST = GAST + GSEC*XDEL
            GO TO 60
C                                       GST
 30         NUV = 1
            GAST = XDATA(I)
C                                       u,v,w from HA
 60         HAI = GAST*HTR - RA - GH
            U(1) = B2 * SIN(HAI)
            V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
            W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
            GO TO 120
C                                       Baseline length
 70         BL = XDATA(I)
            FTEMP = B2 * COS(DEC)
            IF (FTEMP.EQ.0.0) GO TO 200
            TEMP =  B1*B1 + B2*B2 - BL*BL
            IF (TEMP.LT.0.0) GO TO 200
            TEMP = SQRT(TEMP)
            HAI = (-B1 * SIN(DEC) + TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 75
               HAI = ACOS(HAI)
               U(1) = B2 * SIN(HAI)
               V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
               HAI = SQRT (U(1)*U(1) + V(1)*V(1))
               IF (ABS(HAI-BL).LT.1.E-4*BL) GO TO 78
 75         HAI = (-B1 * SIN(DEC) - TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 200
               HAI = ACOS(HAI)
               U(1) = B2 * SIN(HAI)
               V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
               HAI = SQRT (U(1)*U(1) + V(1)*V(1))
               IF (ABS(HAI-BL).GT.1.E-3*BL) GO TO 200
 78         W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
            U(2) = -U(1)
            V(2) = V(1)
            W(2) = W(1)
            NUV = 2
            GO TO 120
C                                       Baseline position angle
 80         PA = XDATA(I) * DTR
            FTEMP = B2 * (1.0 - (COS(DEC) * COS(PA)) ** 2)
            IF (FTEMP.EQ.0.0) GO TO 200
            TEMP = B2*B2 - (B1*B1 + B2*B2)
     *         * ((COS(DEC)*COS(PA))**2)
            IF (SIN(DEC)*COS(PA).EQ.0.0) TEMP = 0.0
            IF (TEMP.LT.0.) GO TO 200
            TEMP = SIN(DEC) * COS(PA) * SQRT (TEMP)
            NUV = 1
            HAI = (B1 * COS(DEC) * COS(PA) * SIN(PA) + TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 85
               HAI = ASIN (HAI)
               U(1) = B2 * SIN(HAI)
               V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
               W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
               BL = 0.0
               IF ((V(1).NE.0.) .OR. (U(1).NE.0.0)) BL = ATAN2 (V(1),
     *            U(1)) / DTR
               IF (ABS(XDATA(I)-BL).LT.0.25) GO TO 120
 85         HAI = (B1 * COS(DEC) * COS(PA) * SIN(PA) - TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 200
               HAI = ASIN (HAI)
               U(1) = B2 * SIN(HAI)
               V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
               W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
               BL = 0.0
               IF ((V(1).NE.0.) .OR. (U(1).NE.0.0)) BL = ATAN2 (V(1),
     *            U(1)) / DTR
               IF (ABS(XDATA(I)-BL).LT.0.5) GO TO 120
               GO TO 200
C                                       Along U-axis
 100        U(1) = XDATA(I)
            U(2) = XDATA(I)
            IF (B2.EQ.0.0D0) GO TO 200
            TEMP = XDATA(I) / B2
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ASIN (TEMP)
            V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
            W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
            HAI = PI - HAI
            V(2) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
            W(2) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
            NUV = 2
            GO TO 120
C                                       Along V-axis
 110        V(1) = XDATA(I)
            V(2) = XDATA(I)
            TEMP = B2 * SIN(DEC)
            IF (TEMP.EQ.0.0) GO TO 200
            TEMP = (B1 * COS(DEC) - V(1)) / TEMP
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ACOS (TEMP)
            U(1) = B2 * SIN(HAI)
            W(1) = B1 * SIN(DEC) + B2 * COS(DEC) * COS(HAI)
            U(2) = -U(1)
            W(2) = W(1)
            NUV = 2
            GO TO 120
C                                       Along W-axis
 115        W(1) = XDATA(I)
            W(2) = XDATA(I)
            TEMP = B2 * COS(DEC)
            IF (TEMP.EQ.0.0) GO TO 200
            TEMP = (W(1) - B1 * SIN(DEC)) / TEMP
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ACOS (TEMP)
            U(1) = B2 * SIN(HAI)
            V(1) = B1 * COS(DEC) - B2 * SIN(DEC) * COS(HAI)
            U(2) = -U(1)
            V(2) = V(1)
            NUV = 2
            GO TO 120
C                                       Compute model values
 120     DO 190 J = 1,NUV
            CSUM = 0.0
            SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
            IF (DOSPHE) THEN
               DO 125 K = 1,NUMCC
                  AA = GAUSA(K) * SQRT (U(J)*U(J) + V(J)*V(J))
                  IF (AA.LT.6.28D-2) AA = 6.28D-2
                  FTEMP = 3.0D0 * SFLUX(K) *
     *               ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
                  TEMP = U(J) * CCPOS(1,K) + V(J) * CCPOS(2,K) +
     *               W(J) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 125              CONTINUE
C                                       Gaussian
            ELSE IF (DOGAUS) THEN
               DO 130 K = 1,NUMCC
                  ARG = U(J)*U(J)*GAUSA(K) + U(J)*V(J)*GAUSB(K)
     *               + V(J)*V(J)*GAUSC(K)
                  IF (ARG.GT.-8.0) THEN
                     FTEMP = SFLUX(K) * EXP(ARG)
                     TEMP = U(J) * CCPOS(1,K) + V(J) * CCPOS(2,K) +
     *                  W(J) * CCPOS(3,K)
                     SSUM = SSUM + FTEMP * SIN (TEMP)
                     CSUM = CSUM + FTEMP * COS (TEMP)
                     END IF
 130              CONTINUE
C                                       Point
            ELSE
               DO 135 K = 1,NUMCC
                  FTEMP = SFLUX(K)
                  TEMP = U(J) * CCPOS(1,K) + V(J) * CCPOS(2,K) +
     *               W(J) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 135              CONTINUE
               END IF
C                                       Finished computing model
            AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
            IF (NPLO.EQ.1) YDATA(J,I) = AMODL
            IF (NPLO.EQ.22) YDATA(J,I) = LOG10 (MAX (1.0E-12, AMODL))
            IF (NPLO.EQ.9) YDATA(J,I) = CSUM
            IF (NPLO.EQ.10) YDATA(J,I) = SSUM
            IF (NPLO.NE.2) GO TO 190
               IF (AMODL.LE.1.E-10) GO TO 190
               FAZ = 0.0
               IF ((SSUM.NE.0.0) .OR. (CSUM.NE.0.0)) FAZ = ATAN2 (SSUM,
     *            CSUM) / DTR
 140           IF (FAZ.GT.180.0E0) FAZ = FAZ - 360.0E0
                  IF (FAZ.LT.-180.0E0) FAZ = FAZ + 360.0E0
                  IF (FAZ.GT.180.0E0) GO TO 140
                  IF (FAZ.LT.-180.0E0) GO TO 140
                  IF ((SPHASE) .AND. (FAZ.LT.0.0)) FAZ = FAZ + 360.0
                  YDATA(J,I) = FAZ
 190        CONTINUE
 200     CONTINUE
 210  IERR = 0
      GO TO 999
C                                       bad X-axis type
 990  WRITE (MSGTXT,1990) MPLO
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('MODSET: BAD X-AXIS CODE =',I6)
      END
      SUBROUTINE MODORB (XMIN, XMAX, MPLO, NPLO, DRA, DDEC, XDATA,
     *   YDATA, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set up model values for a given set of clean
C   components and for a given pair of antennas, if one or both
C   antennas is at an orbit of Earth satellite.
C
C   Inputs:
C      XMIN,XMAX      R    min and max values of the X-axis
C      MPLO           I    type of X-axis: 1 --> amplitude
C                              2 --> phase
C                              3 --> u,v distance
C                              4 --> u,v p.a.
C                              5 --> time (iat days)
C                              6 --> u
C                              7 --> v
C                              8 --> w
C                              9 --> sine channel
C                             10 --> cosine channel
C                             11 --> GAST
C      NPLO           I     Type of Y-axis in the same manner
C      DRA,DDEC       D     Ra and Dec of the source at the epoch of
C                           observation (in degrees, and as D).
C   Input from common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X,Y,Z pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C   Input from common /CANIN/
C      XB, YB, ZB     D(*)  Array of ground antennas cartesian
C                           coordinates in a system tied with Earth
C
C      IORBIT         I(*)  Array of satellites number.
C                           =0 if ground based
C      ORBITA         D(IP + (IS-1)*6)  Parameters of the orbits
C                           1. Semimajor (m)
C                           2. Eccentricity
C                           3. Inclination of orbit plane, degrees
C                           4. RA of ascending node, degrees
C                           5. An angle in orbit plane from
C                              ascending node to peregee, degrees

C                           6. The mean anomaly at the reference
C                              time, degrees
C   Output:
C
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      DOUBLE PRECISION    U, V, W, HAI, TIME, BX, BY, BZ, GMST,
     *   GAST, RA, X1, Y1, Z1, X2, Y2, Z2, DEC, DRA, DDEC, GSEC, AA,
     *   RATE, ORBIT1(6), ORBIT2(6), VX, VY, VZ
      REAL      XXPOS, XDEL, CSUM, SSUM, TEMP, AMODL, FAZ, ARG, XMIN,
     *   XMAX, XDATA(*), YDATA(2,*), FTEMP
      INTEGER   MPLO, NPLO, I, K, J, IERR
      INTEGER IORB1, IORB2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VPLOT.INC'
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      IERR = 1
      IORB1 = IORBIT(IAW1)
      IORB2 = IORBIT(IAW2)
      ORBIT1(1) = ORBITA(1 + (IORB1-1)*6)
      ORBIT1(2) = ORBITA(2 + (IORB1-1)*6)
      ORBIT1(3) = ORBITA(3 + (IORB1-1)*6)
      ORBIT1(4) = ORBITA(4 + (IORB1-1)*6)
      ORBIT1(5) = ORBITA(5 + (IORB1-1)*6)
      ORBIT1(6) = ORBITA(6 + (IORB1-1)*6)
      ORBIT2(1) = ORBITA(1 + (IORB2-1)*6)
      ORBIT2(2) = ORBITA(2 + (IORB2-1)*6)
      ORBIT2(3) = ORBITA(3 + (IORB2-1)*6)
      ORBIT2(4) = ORBITA(4 + (IORB2-1)*6)
      ORBIT2(5) = ORBITA(5 + (IORB2-1)*6)
      ORBIT2(6) = ORBITA(6 + (IORB2-1)*6)
      X1 = XB(IAW1)
      Y1 = YB(IAW1)
      Z1 = ZB(IAW1)
      X2 = XB(IAW2)
      Y2 = YB(IAW2)
      Z2 = ZB(IAW2)
C                                       X can be only time;
      IF (MPLO.NE.5) GO TO 990
C
      RA = DRA * DG2RAD
      DEC = DDEC * DG2RAD
C                                       divide X-axis into 200 pixels
C                                       XMAX, XMIN, XDEL - in days
      XDEL = (XMAX - XMIN) / 200.0
      XXPOS = XMIN - XDEL
C                                       compute GST at first time
C                                       interval
      CALL GSTROT (JDREF, GMST, GAST, RATE)
      GAST = (GAST + 360.D0*XMIN*GSEC)
      GAST = MOD (GAST, 360.D0)
C                                       GAST in degrees at the beginning
C                                       of plot
C
      TIME = XMIN - XDEL
C                                       Time at the beginning of plot
C                                       relatively reference time in days
      GAST = GAST - 360.0D0*XDEL*GSEC
C
      DO 200 I = 1,200
         YDATA(1,I) = FBLANK
         YDATA(2,I) = FBLANK
         XXPOS = XXPOS + XDEL
         XDATA(I) = XXPOS
         TIME = TIME + XDEL
C                                       reference time at the current
C                                       point, in days
         GAST = GAST + 360.0D0*XDEL*GSEC
C                                       GAST in degrees at the current
C                                       time
         HAI = GAST * DG2RAD
C                                       base line projections at the
C                                       equatorial coordinate system
         CALL BACOOR (IORB1, IORB2, ORBIT1, ORBIT2, X1, Y1, Z1,
     *      X2, Y2, Z2, HAI, TIME, BX, BY, BZ, VX, VY, VZ)
C                                       U, V, W in meters
         U = BX*SIN(RA) - BY*COS(RA)
         V = BX*COS(RA)*SIN(DEC) + BY*SIN(RA)*SIN(DEC) - BZ*COS(DEC)
         W = -BX*COS(RA)*COS(DEC)- BY*SIN(RA)*COS(DEC) - BZ*SIN(DEC)
C                                       Compute model values
         CSUM = 0.0
         SSUM = 0.0
C                                       calculate model RE, IM, Phase,
C                                       Amp. Point and gaussian.
         IF (DOSPHE) THEN
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
            DO 20 K = 1,NUMCC
               AA = GAUSA(K) * SQRT (U*U + V*V)
               IF (AA.LT.6.28D-2) AA = 6.28D-2
               FTEMP = 3.0D0 * SFLUX(K) *
     *            ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
               TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) +
     *            W * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
 20            CONTINUE
C                                       Gaussian
         ELSE IF (DOGAUS) THEN
            DO 40 K = 1,NUMCC
               ARG = U*U*GAUSA(K) + U*V*GAUSB(K)
     *            + V*V*GAUSC(K)
               IF (ARG.LE.-8.0) GO TO 40
               FTEMP = SFLUX(K) * EXP(ARG)
               TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) +
     *            W * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
 40            CONTINUE
C                                       Point
         ELSE
            DO 60 K = 1,NUMCC
               FTEMP = SFLUX(K)
               TEMP = U * CCPOS(1,K) + V * CCPOS(2,K) +
     *            W * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
 60            CONTINUE
            END IF
C                                       Finished computing model
         AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
         IF (NPLO.EQ.1) YDATA(J,I) = AMODL
         IF (NPLO.EQ.22) YDATA(J,I) = LOG10 (MAX (1.0E-12, AMODL))
         IF (NPLO.EQ.9) YDATA(J,I) = CSUM
         IF (NPLO.EQ.10) YDATA(J,I) = SSUM
         IF (NPLO.EQ.2) THEN
            IF ((SSUM.NE.0.0) .OR. (CSUM.NE.0.0)) THEN
               FAZ = ATAN2 (SSUM,CSUM) / DG2RAD
            ELSE
               FAZ = 0.0
               END IF
            FAZ = MOD(FAZ, 360.0)
            IF (FAZ.GT.180.0) THEN
               FAZ = FAZ - 360.0
            ELSE
               IF (FAZ.LT.-180.0) FAZ = FAZ + 360.0
               END IF
            YDATA(J,I) = FAZ
            END IF
 200     CONTINUE
      IERR = 0
      GO TO 999
C                                       bad X-axis type
 990  WRITE (MSGTXT,1100)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('MODORB: In orbit case I can plot only X-time')
      END
      SUBROUTINE VBMDL (RA, DEC, XMIN, XMAX, YMIN, YMAX, YYOFF, TLC,
     *   MPLO, NPLO, KBPARM, ICH, IIF, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Main subroutine for plotting of a model:  If there is no model
C   (NUMCC .le. 0) VBMDL writes the baseline name and returns with
C   IERR=0.
C   Inputs:
C      RA,DEC            D     Ra and dec of the source in radians
C      XMIN,XMAX         R     Min and max of X-axis
C      YMIN,YMAX         R     Min and max of Y-axis
C      XYSCL(2)          R     Scaling factors
C      XYOFF(2)          R     Offset in plot
C      YYOFF(2)          R     Offset of the current plot.
C      TLC(2)            R     X, Y of top left corner of plot.
C      MPLO              I     Type of X-axis plot
C      NPLO              I     Type of Y-axis plot
C      KBPARM            I     2 if PHASE plotted in AMPPH; 0 elsewhere
C      IERR              I     Standard AIPS error code.
C-----------------------------------------------------------------------
      DOUBLE PRECISION    RA, DEC
      REAL      XMIN, XMAX, YMIN, YMAX, YYOFF(2), TLC(3)
      INTEGER   MPLO, NPLO, KBPARM, ICH, IIF, BUFFER(*), IERR
C
      CHARACTER TEXT*32
      INTEGER   INCHAR, LCH, LIF
      LOGICAL   DOMDL
      REAL      XDATA(200), YDATA(2,200), XXXX(10), DX, DY
      INCLUDE 'VPLOT.INC'
      COMMON /CPLOTU/ XXXX
C-----------------------------------------------------------------------
C                                       Write baseline name on plot.
C                                       do not print antennas names
C                                       in phase plot if AMP&PH
      IF (KBPARM.EQ.0) THEN
         LCH = ICH + KBCH - 1
         LIF = IIF + KBIF - 1
         CALL GLTYPE (1, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (TEXT,1000) STANAM(IAW1), STANAM(IAW2), IAW1, IAW2
         CALL DEFRMT (TEXT, '_', INCHAR)
         DX = 1.5
         DY = -1.5
         CALL GPOS (TLC(1), TLC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (TEXT,1010) LCH, LIF
         IF ((ICH.LE.0) .OR. (KBCH.EQ.KECH)) TEXT(:10) = ' '
         IF ((IIF.LE.0) .OR. (KEIF.EQ.KBIF)) TEXT(9:) = ' '
         IF (TEXT.NE.' ') THEN
            CALL REFRMT (TEXT, '_', INCHAR)
            DX = -1.5 - INCHAR
            DY = -1.5
            CALL GPOS (TLC(3), TLC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                     Check if model exists.
      DOMDL = (NPLO.EQ.1) .OR. (NPLO.EQ.2) .OR. (NPLO.EQ.9) .OR.
     *   (NPLO.EQ.10) .OR. (NPLO.EQ.22)
      IF ((NUMCC.LE.0) .OR. (.NOT.DOMDL) .OR. (DOMODL.NE.1)) GO TO 999
C
C                                       setup model if one or both
C                                       antennas are in orbit
      IF ((IORBIT(IAW1).GT.0) .OR. (IORBIT(IAW2).GT.0)) THEN
         CALL MODORB (XMIN, XMAX, MPLO, NPLO, RA, DEC,
     *      XDATA, YDATA, IERR)
C                                       setup model if both antennas are
C                                       on Earth
      ELSE
         CALL MODSET (XMIN, XMAX, MPLO, NPLO, RA, DEC, XDATA, YDATA,
     *      IERR)
         END IF
      IF (IERR.NE.0) GO TO 999
C                                     setup plotting common
      CALL PLTSET (XMIN, XMAX, YMIN, YMAX, XYSCL, XYOFF, YYOFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                     plot model
      CALL GLTYPE (2, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL MODPLT (NPLO, 200, XDATA, YDATA, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A8,' - ',A8,'_( ',I2,' - ',I2,' )')
 1010 FORMAT ('CH',I6,' _IF',I3)
      END
      SUBROUTINE ANTIN (VER, NNCH, IIBIF, DISKI, CNOIN, FREQID, IERR)
C-----------------------------------------------------------------------
C   Selects station information and gets freq,and ref. date out of the
C   header for a given baseline.
C   Inputs:
C      VER           I    Antenna array number (AN file ver.)
C      NNCH          I    Frequency channel.
C      IIBIF         I    IF number.
C      DISKI         I    Vol number
C      CNOIN         I    CNO
C      FREQID        I    Selected FREQID
C   Outputs in common:
C      XB,YB,ZB(*)   D    Cartesian coordinates for ground based
C                         stations in meters.
C      IORBIT        I(*)  Array of satellites number.
C                           =0 if ground based
C      ORBITA        D(IP + (IS-1)*6)  Parameters of the orbits
C                           IP - parameters number;  IS - satellite #
C                           1. Semimajor (m)
C                           2. Eccentricity
C                           3. Inclination of orbit plane, degrees
C                           4. RA of ascending node, degrees
C                           5. An angle in orbit plane from
C                              ascending node to peregee, degrees
C                           6. The mean anomaly at the reference
C                              time, degrees
C      STANAM(*)     C*8  Names of stations
C      JDREF         D    Julian day# for the referens date.
C      AFREQ         D    Frequency of channel NNCH, IF IIBIF in Hz.
C      GST0          D    Greenwich sidereal time at UT=0 at ref.d
C      IATUTC        R    IAT-UTC in seconds.
C      UT1XXX        R    UT1-UTC in seconds.
C   Programmer: L.B.Baath   Onsala Space Observatory  30 october 1982
C               L.R. Kogan  NRAO, Socorro added orbital antennas
C-----------------------------------------------------------------------
      INTEGER   IERR, IA, LUNA, CNOIN, IABUF(512), VER, NIF, FREQID,
     *   NNCH, IIBIF, DISKI, IORB, I, IORPRM
      INCLUDE 'VPLOT.INC'
      CHARACTER STNAME*8, BNDCOD(MAXIF)*8
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), DX, DY, DZ
      LOGICAL   DOORB
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                     open antenna file
      LUNA = 28
      CALL ANTINI ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GST0, DEGPDY, AFREQ, RDATE,
     *   POLRXY, UT1XXX, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
C                                     check for bad freq value
 10   IF ( AFREQ.EQ.0.0D0 ) THEN
         AFREQ = CATD(KDCRV+JLOCF)
         END IF
      CALL JULDAY (RDATE, JDREF)
      DX = ARRAYC(1)
      DY = ARRAYC(2)
      DZ = ARRAYC(3)
      OBSRA = RA
      OBSDEC = DEC
      NANT = IABUF(5)
      ANLONG = 0.0D0
      IF ((DX.NE.0.0D0) .OR. (DY.NE.0.0D0)) ANLONG = ATAN2 (DY, DX)
C                                     Get antenna info.
C
C                                       IORB is an orbital antenna
C                                       number
      IORB = 0
      DO 30 IA = 1,NANT
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, STNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
            END IF
         IORBIT(NOSTA) = 0
         DOORB = MNTSTA.EQ.2
         IF (DOORB) THEN
            IORB = IORB + 1
            IORBIT(NOSTA) = IORB
            DO 25 I = 1,6
               IORPRM = I + (IORB-1)*6
               ORBITA(IORPRM) = ORBPRM(I)
   25          CONTINUE
         ELSE
            XB(NOSTA) = STAXYZ(1)
            YB(NOSTA) = STAXYZ(2)
            ZB(NOSTA) = STAXYZ(3)
            END IF
         STANAM(NOSTA) = STNAME
 30      CONTINUE
C                                     close antenna file
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         END IF
C                                       Get frequency info for correct
C                                       FREQID/IF combination from
C                                       FQ or CH table
      VER = 1
      CALL CHNDAT ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
C                                       Correct freq. for channel NNCH
      AFREQ = AFREQ + FOFF(IIBIF) +
     *         (NNCH - CATR(KRCRP+JLOCF)) * FINC(IIBIF)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTIN: ERROR IN OPEN AN-FILE IERR = ',I6)
 1010 FORMAT ('ANTIN: ERROR IN FINDING STATION',I3,' IERR=',I3)
 1030 FORMAT ('ANTIN: ERROR IN CLOSING AN-FILE IERR = ',I6)
      END
      SUBROUTINE REEDIN (VER, IERR)
C-----------------------------------------------------------------------
C   Reads in up to MAXCC components from file CC.
C   If no file is found NUMCC is set to zero and the routine return
C   IERR = 0.
C   Will accept either points or gaussians.
C   Inputs:
C      VER           I     CLEAN components file version no.
C   Outputs:  In common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X,Y,Z pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C-----------------------------------------------------------------------
      CHARACTER KEYS(7)*8
      HOLLERITH CATCLH(256)
      INTEGER   LIMIT, I, NCOUNT, IROUND
      INTEGER   IERR, IABUF(512), DISK2, SEQ2, IUSER, LUNB, VER, NREC,
     *   NKEY, KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, BMAKOL, BMIKOL,
     *   BPAKOL, IBUFF1(1024), NKOL, CATCLN(256), IPOINT, IMAP, NMAP,
     *   MODEL, METHOD
      REAL      ABUF(256), CATCLR(256), EPS, RECORD(20), XTEMPC, TA, TB,
     *   AM, AN, XYZ(3), XP(3), UMAT(3,3), PMAT(3,3), XXOFF, YYOFF,
     *   ZZOFF
      DOUBLE PRECISION    CATCLD(128), A8BUF(128), XRA, XDEC
      LOGICAL   T, F, WASOME
      INCLUDE 'VPLOT.INC'
      INTEGER   BITER(MAXFLD), NITER(MAXFLD)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATCLN, CATCLR, CATCLH, CATCLD)
      EQUIVALENCE (IABUF, ABUF, A8BUF)
      EQUIVALENCE (IBUFF1, BUFF1)
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5), BMAKOL),               (KOLS(6), BMIKOL),
     *   (KOLS(7), BPAKOL)
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ',
     *   'TYPE OBJ','MAJOR AX','MINOR AX','POSANGLE'/
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNB /18/
      DATA BITER /MAXFLD * 0/
C-----------------------------------------------------------------------
      EPS = 0.1
      DISK2 = XDI2IN + EPS
      IUSER = USERID
      SEQ2 = XS2IN + EPS
      NMAP = IROUND (XNMAP)
      NMAP = ABS (NMAP)
      NUMCC = 0
      IF ((NAM2IN.EQ.' ') .AND. (CLA2IN.EQ.' ')) GO TO 200
      IF (DOMODL.LE.0) GO TO 200
      NCOUNT = 0
      LIMFLX = XFLUX
      NONEG = F
      WASOME = F
      DO 10 IMAP = 1,NMAP
         IF (IMAP.LE.MAXAFL) THEN
            NITER(IMAP) = IROUND (ABS(XCOMP(IMAP)))
            IF (XCOMP(IMAP).LE.-0.5) NONEG = T
            IF (NITER(IMAP).GT.0) WASOME = T
         ELSE
            NITER(IMAP) = 0
            IF (WASOME) NITER(IMAP) = 1000000000
            END IF
 10      CONTINUE
      MODEL = 1
      METHOD = -1
      CALL SETGDS (DISKIN, OLDCNO, NAM2IN, CLA2IN, SEQ2, DISK2, NMAP,
     *   VER, NITER, BITER, MODEL, METHOD, CCPOS, IABUF, I, IERR)
C                                      If not found => no model plot.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      FACGRD(1) = 1.0
      CALL FACSET (DISKIN, OLDCNO, KBIF, IDN(1), 1, 1.0, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 120 IMAP = 1,NMAP
C                                       Read catalog block
         CALL CATIO ('READ', CCDISK(IMAP), CCCNO(IMAP), CATCLN, 'REST',
     *      IABUF, IERR)
         IF (IERR.NE.0) GO TO 120
         IF (IMAP.EQ.1) THEN
            XS2IN = CATCLN(KIIMS)
            XDI2IN = CCDISK(1)
            CALL H2CHR (12, KHIMNO, CATCLH(KHIMN), NAM2IN)
            CALL H2CHR (6, KHIMCO, CATCLH(KHIMC), CLA2IN)
            CALL CHR2H (12, NAM2IN, 1, XNAM2I)
            CALL CHR2H (6, CLA2IN, 1, XCLA2I)
            END IF
C                                       set rest parameters
         CALL GRDAT (.FALSE., IMAP, CATR, IABUF, IERR)
         IF (IERR.NE.0) GO TO 120
C                                       Set field center offsets.
         XXOFF = DXCG(IMAP) * CCROT + DYCG(IMAP) * SSROT
         YYOFF = DYCG(IMAP) * CCROT - DXCG(IMAP) * SSROT
         ZZOFF = DZCG(IMAP)
         IF (DO3DIM) THEN
            CALL XYSHFT (RA, DEC, XSHIFT(IMAP), YSHIFT(IMAP),
     *         MAPROT, XRA, XDEC)
            CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *         PMAT)
            END IF
C                                       Get model.
C                                       Open CLEAN component file.
         NKOL = 0
         NREC = 1
         NKEY = 0
         CALL TABINI ('READ', 'CC', CCDISK(IMAP), CCCNO(IMAP), VER,
     *      CATCLN, LUNB, NKEY, NREC, NKOL, ABUF, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         CALL RFILL (7, 0.0, RECORD)
C                                       Find columns (physical)
         TYPKOL = 7
         BMAKOL = 0
         BMIKOL = 0
         BPAKOL = 0
         NKEY = NKOL
         IF (NKEY.GT.7) NKEY = 7
         CALL FNDCOL (NKEY, KEYS, 8, F, BUFF1, KOLS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Get number of components
         LIMIT = NITER(IMAP)
         IF (LIMIT.LE.0) GO TO 100
         DOGAUS = F
         DOSPHE = F
         DO 90 I = 1,LIMIT
            CALL TABIO ('READ', 0, I, RECORD, BUFF1, IERR)
            IF (IERR.LT.0) GO TO 90
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1030) IERR, I
               GO TO 990
               END IF
            IF (ABS(RECORD(FLXKOL)).LT.LIMFLX) GO TO 100
            IF ((NONEG) .AND. (RECORD(FLXKOL).LT.0.0)) GO TO 100
C                                       Deal with component.
            NCOUNT = NCOUNT + 1
            IF (NCOUNT.GT.MAXCC) THEN
               NCOUNT = NCOUNT - 1
               WRITE (MSGTXT,1040) NCOUNT
               CALL MSGWRT (8)
               MSGTXT = 'REEDIN: LATER COMPONENTS IGNORED!!!'
               CALL MSGWRT (8)
               GO TO 100
               END IF
C                                       No need to shift the components
C                                       since this already done in the
C                                       CLEANing task.
            XP(1) = (RECORD(RAKOL) + XPOFF(IMAP)) * DG2RAD * TWOPI
            XP(2) = (RECORD(DECKOL) + YPOFF(IMAP)) * DG2RAD * TWOPI
            IF (DO3DIM) THEN
               XP(3) = 0.0
               CALL PRJMUL (2, XP, UMAT, XYZ)
            ELSE
               XYZ(1) = CCROT * XP(1) + SSROT * XP(2)
               XYZ(2) = CCROT * XP(2) - SSROT * XP(1)
               XYZ(3) = 0.0
               END IF
            CCPOS(1,NCOUNT) = XYZ(1) + XXOFF
            CCPOS(2,NCOUNT) = XYZ(2) + YYOFF
            CCPOS(3,NCOUNT) = XYZ(3) + ZZOFF
            SFLUX(NCOUNT) = RECORD(FLXKOL) * FACGRD(1)
            GAUSA(NCOUNT) = 0
            GAUSB(NCOUNT) = 0
            GAUSC(NCOUNT) = 0
C                                       See if gaussian
            IF ((ABS (RECORD(TYPKOL)-1.).GT.0.5) .AND.
     *         (ABS (RECORD(TYPKOL)-3.).GT.0.5)) GO TO 90
C                                       Convert to radians
               GAUSA(NCOUNT) = RECORD(BMAKOL) * DG2RAD
               GAUSB(NCOUNT) = RECORD(BMIKOL) * DG2RAD
               GAUSC(NCOUNT) = RECORD(BPAKOL) * DG2RAD
               IF (ABS (RECORD(TYPKOL)-1.).LT.0.5) DOGAUS = T
               IF (ABS (RECORD(TYPKOL)-3.).LT.0.5) DOSPHE = T
 90         CONTINUE
C                                       Close CLNFIL.
 100     CALL TABIO ('CLOS', 0, IPOINT, RECORD, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
         IF (NCOUNT.GT.MAXCC) GO TO 130
 120     CONTINUE
      NUMCC = NCOUNT
      GO TO 140
 130  NUMCC = MAXCC
 140  CALL UNSETG (BUFF1)
C                                       Convert gaussian parameters
      IF ((NUMCC.LE.0) .OR. (.NOT.DOGAUS)) GO TO 160
         DO 150 I = 1,NUMCC
            XTEMPC = GAUSC(I)
            AM = COS ((XTEMPC+UVROT-MAPROT)*DG2RAD)
            AN = SIN ((XTEMPC+UVROT-MAPROT)*DG2RAD)
            TA = GAUSA(I) * PI / 1.1774
            TB = GAUSB(I) * PI / 1.1774
            GAUSA(I) = -(TA*TA*AM*AM + TB*TB*AN*AN)
            GAUSB(I) = -((TB*TB-TA*TA) * AN*AM)
            GAUSC(I) = -(TA*TA*AN*AN + TB*TB*AM*AM)
 150        CONTINUE
C                                       Convert sphere parameters
 160  IF ((NUMCC.LE.0) .OR. (.NOT.DOSPHE)) GO TO 200
         DO 170 I = 1,NUMCC
            GAUSA(I) = GAUSA(I) * TWOPI
 170        CONTINUE
C
 200  IERR = 0
C      XCOMP = NCOUNT
C      IF ((NITER.LE.0) .OR. (NUMCC.LE.0)) XCOMP = 0.0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REEDIN: ERROR IN OPENING CC FILES IERR = ',I5)
 1010 FORMAT ('REEDIN: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('REEDIN: READ ERROR',I3,' RECORD ',I5)
 1040 FORMAT ('REEDIN: READ',I7,' COMPONENTS WHICH IS MY MAXIMUM')
 1060 FORMAT ('REEDIN: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE BASAVG (NUMVIS, SCANV, AMNMX, TIME, NUSCAN, SCANUM,
     *   RPARM, VIS, AVTIM, AVU, AVV, AVW, NLVIS, DTT, MBASE, MSAMP,
     *   MPOL, MCHAN, MIF, XVAL, YVAL, LLBAS, FPARM, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of amp, phase or the RMS
C   scatter for selected baselines.
C   Needs to be initialized by a call to UVGET.
C   The  order of the baselines returned in SCANV is defined by
C   the order in the array NATLAB.  All data specified  (channels, IFs)
C   are averaged but only one polarization is allowed.
C   Inputs:
C      NUMVIS   I      Current visibility number
C      MSAMP    I      Max number of points in arrays
C      MIF      I      Number of IFs
C   Input in common:
C     NBASE        I    Number of baselines to average.
C     ANT1(*)      I    The first antenna numbers of each baseline
C                       selected.
C     ANT2(*)      I    The second antenna numbers of each baseline
C                       selected.
C     AVGSCA       L    If true do ampscalar averaging else vector.
C     DTYPE        I    Type of Y-axis, 1 => amplitude, 2 => phase,
C                       9 => RE, 10 => IM, 16 => weight, 17 => RMS.
C     SOLINT       R    Averaging time in days
C     CHNSEL(3,10) I    Channel selection
C     DOIAVG       L    If TRUE spectral averaging to be done
C     AMPPH        L    If TRUE both AMP and PHASE plot together
C     SEARCH       L    If TRUE bad points recognition is provided
C   Input/Output:
C     RPARM(20)    R    Random parameter array, first record of call.
C                      (1) = 'INDE' => don't use.
C                      (2) if single baseline averaged then RPARM
C                          reflects the data output.
C     VIS(3,*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(MXBASE)R   The result for baselines in ANT1, ANT2
C     TIME(8)      I    Time range, start, stop; days, hours, min, sec.
C                       Unless NUSCAN only first 4 values are set.
C     NUSCAN       L    True IF the first record in a new scan.
C     AVTIM        R    Average time (days) of output record
C     AVU(MXBASE)  R    Average u of output record
C     AVV(MXBASE)  R    Average v of output record
C     AVW(MXBASE)  R    Average w of output record
C     COUNT(*)     I    Array of visibilities numbers
C     NLVIS        I    Number of visibilities in the given interval
C     RMS(*)       R    Array of rms
C     DTT          R    Preaverage interval
C     FPARM        R(*) rparm for averaged array (SU, FQ filled in)
C     IERR         I    Return code, 0 => OK,
C                       -1 => end of data found with no data to return
C                       -2 => end of data found with data to return
C                       > 0 => failed.
C   Output in common:
C     YVAL(*)      R    Array of amplitude
C     XVAL(*)      R    Array of times
C     LLBAS(*)     I    Array of baselines
C   Output to common in D/CSOU.INC
C     SNAME(4)     R    Source name (16 char. 4 / word.)
C     QUAL         I    Source qualifier.
C     CALCOD       R    Calibrator code 4 char.
C     FLUX(4,IF)   R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)    D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMVIS, TIME(8), SCANUM, NLVIS, MBASE, MSAMP, MPOL,
     *   MCHAN, MIF, LLBAS(*)
      REAL      SCANV(7,MBASE,MPOL,MCHAN,*), RPARM(*), VIS(3,*), AVTIM,
     *   AVU(*), AVV(*), AVW(*), DTT, XVAL(*), YVAL(MSAMP,MPOL,MCHAN,*),
     *   FPARM(*), AMNMX(2,MBASE,MPOL,MCHAN,*)
      LOGICAL   NUSCAN
C
      INCLUDE 'VPLOT.INC'
      LOGICAL   DONE1, GOTDAT, FIRST
      INTEGER   IERR, I, J, JA1, JA2, SUNUM, JERR, ISLUN, IBAS, CNTTIM,
     *   IVSCNT, LVIS, LPOL, LIF, CIF, CNT(MXBASE), LCHAN, CCH
      REAL      T1, AMP, SUMTIM, VISO(4), TLAST, CT, DTUTC, TCT, FI, RE,
     *   IM, VMAX, VMIN, WT, SCALE, VRMS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (RE, VISO(1)), (IM, VISO(2)), (WT, VISO(3)),
     *   (VRMS, VISO(4))
      SAVE TLAST, DTUTC, IVSCNT
      DATA ISLUN /26/
C-----------------------------------------------------------------------
      IF (NUMVIS.EQ.0) THEN
         TLAST = -1.0
         DTUTC = DATUTC / 86400.0
         IVSCNT = 0
         END IF
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
      IERR = 0
C                                       Clear arrays
 10   DO 20 I = 1,NBASE
         AVU(I) = 0
         AVV(I) = 0
         AVW(I) = 0
         CNT(I) = 0
 20      CONTINUE
      J = 7 * MBASE * MPOL * MCHAN * MIF
      CALL RFILL (J, 0.0, SCANV)
      CNTTIM = 0
      SUMTIM = 0.0
      AVTIM  = 0.0
C                                       Initialize time
      T1 = 1.0E10
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
      IF (DONE1) CALL RCOPY (NRPARM, RPARM, FPARM)
C                                       Loop reading data
      LVIS = 0
 100  CONTINUE
         IF (.NOT.DONE1) THEN
            CALL UVGET ('READ', RPARM, VIS, IERR)
            IVSCNT = IVSCNT + 1
            IF (UVSCAL.NE.1.0) THEN
               RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
               RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
               RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
               END IF
            IF (CNTTIM.LE.0) CALL RCOPY (NRPARM, RPARM, FPARM)
            END IF
         IF (IERR.GT.0) GO TO 999
         IF (IERR.EQ.-1) GO TO 500
         DONE1 = .FALSE.
C
         CT = RPARM(ILOCT+1) - DTUTC
C                                       Set up first time boundary
         IF (IVSCNT.EQ.1) THEN
C                                       Preaverage interval time
            IF (ILOCIT.GT.0) THEN
               DTT = RPARM(1+ILOCIT)/2.0
            ELSE
               DTT= 1.0
               END IF
            IF (CT.GE.TBEG) THEN
               TCT = CT
            ELSE
               TCT = TBEG
               END IF
            TLAST = TCT + SOLINT
            END IF
C                                       Test time range (TB sort)
         IF (CT.GT.TFIN) GO TO 500
         IF (CT.LT.TBEG) GO TO 100
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         CALL ALIAS (JA1, IALIAS)
         CALL ALIAS (JA2, IALIAS)
C                                       Find baseline
         DO 110 I = 1,NBASE
            IBAS = I
            IF ((JA1.EQ.ANT1(I) .AND. (JA2.EQ.ANT2(I))))
     *         GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
C
 120     GOTDAT = .TRUE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM .AND. YESCAN) .OR. (IERR.LT.0) .OR.
     *      (CT.GT.TLAST)) GO TO 500
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
C                                       flogging abuse of this routine
         IF (SEARCH) THEN
            LVIS = LVIS + 1
C                                       too many points in the interval
            IF (LVIS.GT.MSAMP) THEN
               IERR = 1
               WRITE (MSGTXT,1130) LVIS, MSAMP
               CALL MSGWRT (6)
               WRITE (MSGTXT,1140)
               GO TO 990
               END IF
C                                       store aplitudes and times
C                                       of points of a given interval
            XVAL(LVIS) = CT
            LLBAS(LVIS) = IBAS
            END IF
C                                       average U,V,W
         AVU(IBAS) = AVU(IBAS) + RPARM(ILOCU + 1)
         AVV(IBAS) = AVV(IBAS) + RPARM(ILOCV + 1)
         AVW(IBAS) = AVW(IBAS) + RPARM(ILOCW + 1)
         CNT(IBAS) = CNT(IBAS) + 1
C                                       Modify VIS as appropriate
C                                       based on POLPLT.
         IF (POLPLT.GT.0) CALL POLVIS (VIS, POLPLT, IPOLPT)
C                                       Loop over IF
         DO 150 LPOL = 1,MPOL
         DO 150 LCHAN = 1,MCHAN
         DO 150 LIF = 1,MIF
            FIRST = .TRUE.
C                                       Spectral averaging
            CIF = LIF + BIF - 1
            IF (DOIAVG .OR. DOIFRA) CIF = 0
            CCH = LCHAN + BCHAN - 1
            IF (DOCAVG) CCH = 0
            CALL AVGCIF (VIS, CCH, BCHAN, ECHAN, LPOL, CIF, BIF, EIF,
     *         VMIN, VMAX, VISO)
C                                       bad IF/channel
            IF (VISO(3).LE.0.0) THEN
               IF (SEARCH) YVAL(LVIS,LPOL,LCHAN,LIF) = FBLANK
C                                       good IF
            ELSE
               AMP = SQRT (RE*RE + IM*IM)
               FI = 0.0
               IF ((IM.NE.0.0) .OR. (RE.NE.0.0)) FI = ATAN2 (IM, RE)
               IF (SEARCH) THEN
                  YVAL(LVIS,LPOL,LCHAN,LIF) = AMP
               ELSE
                  IF (APARM(3).GT.0) THEN
                     AMNMX(1,IBAS,LPOL,LCHAN,LIF) = MIN (VMIN,
     *                  AMNMX(1,IBAS,LPOL,LCHAN,LIF))
                     AMNMX(2,IBAS,LPOL,LCHAN,LIF) = MAX (VMAX,
     *                  AMNMX(2,IBAS,LPOL,LCHAN,LIF))
                     END IF
                  END IF
               SCANV(1,IBAS,LPOL,LCHAN,LIF) = RE * WT +
     *            SCANV(1,IBAS,LPOL,LCHAN,LIF)
               SCANV(2,IBAS,LPOL,LCHAN,LIF) = IM * WT +
     *            SCANV(2,IBAS,LPOL,LCHAN,LIF)
               SCANV(3,IBAS,LPOL,LCHAN,LIF) = WT +
     *            SCANV(3,IBAS,LPOL,LCHAN,LIF)
               SCANV(5,IBAS,LPOL,LCHAN,LIF) = WT +
     *            SCANV(5,IBAS,LPOL,LCHAN,LIF)
C                                       Ampscalar averaging
               IF (AVGSCA) THEN
                  SCANV(6,IBAS,LPOL,LCHAN,LIF) = AMP * WT +
     *               SCANV(6,IBAS,LPOL,LCHAN,LIF)
                  SCANV(7,IBAS,LPOL,LCHAN,LIF) = AMP * AMP * WT +
     *               SCANV(7,IBAS,LPOL,LCHAN,LIF)
C                                       Vector average:
               ELSE
                  SCANV(6,IBAS,LPOL,LCHAN,LIF) = RE * RE * WT +
     *               SCANV(6,IBAS,LPOL,LCHAN,LIF)
                  SCANV(7,IBAS,LPOL,LCHAN,LIF) = IM * IM * WT +
     *               SCANV(7,IBAS,LPOL,LCHAN,LIF)
                  END IF
               END IF
 150        CONTINUE
         GO TO 100
C
C                                       do an average
C
 500  CONTINUE
      NLVIS = LVIS
C                                       See if have any data.
      IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (GOTDAT) THEN
         IF (IERR.EQ.-1) IERR = -2
         DO 550 I = 1,NBASE
C                                       U,V,W
            IF (CNT(I).GT.0) THEN
               AVU(I) = AVU(I) / CNT(I)
               AVV(I) = AVV(I) / CNT(I)
               AVW(I) = AVW(I) / CNT(I)
C                                       loop over IF
               DO 545 LPOL = 1,MPOL
                  DO 545 LCHAN = 1,MCHAN
                  DO 540 LIF = 1,MIF
                     WT = SCANV(3,I,LPOL,LCHAN,LIF)
                     IF (WT.LE.0.0) THEN
                        RE = 0.0
                        IM = 0.0
                        VRMS = 0.0
                     ELSE
                        RE =  SCANV(1,I,LPOL,LCHAN,LIF) / WT
                        IM =  SCANV(2,I,LPOL,LCHAN,LIF) / WT
                        IF (AVGSCA) THEN
                           AMP = SCANV(6,I,LPOL,LCHAN,LIF) / WT
                           IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) THEN
                              SCALE = AMP / SQRT (RE*RE + IM*IM)
                              RE = RE * SCALE
                              IM = IM * SCALE
                              END IF
                           VRMS = SCANV(7,I,LPOL,LCHAN,LIF) / WT -
     *                        AMP * AMP
                           VRMS = SQRT (MAX (0.0, VRMS))
C                                       Vector average
                        ELSE
                           VRMS = SCANV(7,I,LPOL,LCHAN,LIF) / WT +
     +                        SCANV(6,I,LPOL,LCHAN,LIF) / WT - RE * RE -
     *                        IM * IM
                           VRMS = SQRT (MAX (0.0, VRMS))
                           END IF
                        END IF
                     CALL RCOPY (4, VISO, SCANV(1,I,LPOL,LCHAN,LIF))
 540                 CONTINUE
 545              CONTINUE
               END IF
 550        CONTINUE
C                                       Get source info
         IF (NUSCAN) THEN
            CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
            IF (JERR.EQ.11) THEN
               WRITE (MSGTXT,1750) SUNUM
               CALL MSGWRT (6)
            ELSE IF (JERR.GT.0) THEN
               IERR = JERR
               WRITE (MSGTXT,1700) JERR
               GO TO 990
               END IF
            END IF
         END IF
C                                       Time
      IF (CT.GT.TFIN) THEN
         IERR = -1
         IF (GOTDAT) IERR = -2
         END IF
      T1 = 0
      IF (CNTTIM.GT.0) THEN
         T1 = SUMTIM / CNTTIM
         AVTIM = T1
         END IF
      CALL TODHMS (T1, TIME)
C                                       Set up new time boundary
      TLAST = CT + SOLINT
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('!!Number points in the solution interval',I7,
     *   ' exceeds max',I7)
 1140 FORMAT ('!!Decrease the value of SOLINT to decrease the ',
     *   'number of points')
 1700 FORMAT ('BASAVG: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('BASAVG: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE XYSC (NUMVIS, XY, NAMPPH, MAXPLT, LOOP, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         R    plotted parameters .
C      NAMPPH     I    =3 for AMP&PH; =2 elsewhere.
C      MAXPLT     I    Maximum number of plots per page.
C      LOOP       I    baseline number
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C
C   Programmer T. J. Cornwell, Oct.  1981., L.R. Kogan, Oct. 1994
C-----------------------------------------------------------------------
      INTEGER   IRET, MAXPLT, I, JJ, LOOP, NUMVIS, NAMPPH
      REAL      XY(3), SIZE, TEMP, T
      LOGICAL   POSSDG
      INCLUDE 'VPLOT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      POSSDG = (TYPEAX(2).GT.12) .AND. (TYPEAX(2).NE.22)
C                                       Are they in requested range?
      IF (NUMVIS.GE.0) THEN
         IRET = -1
         IF (IPHASE.GT.0) THEN
            TEMP = XY(IPHASE)
            END IF
         DO 10 I = 1, NAMPPH
            IF (TESTEM(I).NE.0) THEN
               T = XY(I)
               JJ = 2*I - 1
               IF (I.NE.IPHASE) THEN
                  IF ((INISCL(JJ).LT.INISCL(JJ+1)) .AND.
     *               ((T.LT.INISCL(JJ)) .OR. (T.GT.INISCL(JJ+1))))
     *               GO TO 999
                  IF ((INISCL(JJ).GT.INISCL(JJ+1)) .AND.
     *               ((T.GT.INISCL(JJ)) .OR. (T.LT.INISCL(JJ+1))))
     *               GO TO 999
               ELSE IF (INISCL(JJ).LT.INISCL(JJ+1)) THEN
                  IF ((T.LT.INISCL(JJ)) .OR. (T.GT.INISCL(JJ+1))) THEN
                     T = TEMP
                     IF ((T.LT.INISCL(JJ)) .OR. (T.GT.INISCL(JJ+1)))
     *                  GO TO 999
                     XY(I) = T
                     END IF
               ELSE IF (INISCL(JJ).GT.INISCL(JJ+1)) THEN
                  IF ((T.GT.INISCL(JJ)) .OR. (T.LT.INISCL(JJ+1))) THEN
                     T = TEMP
                     IF ((T.GT.INISCL(JJ)) .OR. (T.LT.INISCL(JJ+1)))
     *                  GO TO 999
                     XY(I) = T
                     END IF
                  END IF
               END IF
 10         CONTINUE
         IRET = 0
C                                       Find max, min from data
         DO 30 I = 1,NAMPPH
            IF (SCALEM(I)) THEN
               IF (XY(I).LT.XYOFF(I)) XYOFF(I) = XY(I)
               IF (XY(I).GT.XYSCL(I)) XYSCL(I) = XY(I)
               IF (I.EQ.IPHASE) THEN
                  IF (TEMP.LT.XYOFF(4)) XYOFF(4) = TEMP
                  IF (TEMP.GT.XYSCL(4)) XYSCL(4) = TEMP
                  END IF
               END IF
 30         CONTINUE
         IF (FLOTEM) THEN
            IF (XY(2).LT.YYMIN(LOOP)) YYMIN(LOOP) = XY(2)
            IF (XY(2).GT.YYMAX(LOOP)) YYMAX(LOOP) = XY(2)
            IF (AMPPH) THEN
               IF (XY(3).LT.PHMIN(LOOP)) PHMIN(LOOP) = XY(3)
               IF (XY(3).GT.PHMAX(LOOP)) PHMAX(LOOP) = XY(3)
               END IF
            IF (IPHASE.GT.0) THEN
               IF (TEMP.LT.PPMIN(LOOP)) PPMIN(LOOP) = TEMP
               IF (TEMP.GT.PPMAX(LOOP)) PPMAX(LOOP) = TEMP
               END IF
            END IF
C                                       Final call
C                                       Convert to scaling factors
C                                       add 20% for label.
      ELSE
         IF (FLOTEM) THEN
            IF (POSSDG) THEN
               XYSCL(2) = XYSCL(2) + (XYSCL(2) * 0.05)
               XYOFF(2) = XYOFF(2) - (XYOFF(2) * 0.05)
               END IF
            DO 115 I = 1,NBASE
               IF (YYMIN(I).GE.YYMAX(I)) THEN
                  YYMIN(I) = XYOFF(2)
                  YYMAX(I) = XYSCL(2)
                  END IF
               IF (PHMIN(I).GE.PHMAX(I)) THEN
                  PHMIN(I) = XYOFF(3)
                  PHMAX(I) = XYSCL(3)
                  END IF
               IF ((PPMIN(I).GE.PPMAX(I)) .AND. (IPHASE.GT.0)) THEN
                  PPMIN(I) = XYOFF(4)
                  PPMAX(I) = XYSCL(4)
                  END IF
               YYMAX(I) = YYMAX(I) + 0.2 * (YYMAX(I) - YYMIN(I))
               PHMAX(I) = PHMAX(I) + 0.2 * (PHMAX(I) - PHMIN(I))
               PPMAX(I) = PPMAX(I) + 0.2 * (PPMAX(I) - PPMIN(I))
 115           CONTINUE
            END IF
         XYSCL(2) = XYSCL(2) + 0.2 * (XYSCL(2) - XYOFF(2))
         IF (AMPPH) XYSCL(3) = XYSCL(3) + 0.2 * (XYSCL(3) - XYOFF(3))
         IF (IPHASE.GT.1) XYSCL(4) = XYSCL(4) + 0.2*(XYSCL(4)-XYOFF(4))
         SPHASE = .FALSE.
         IF ((IPHASE.EQ.1) .AND.
     *      (XYSCL(4)-XYOFF(4).LT.XYSCL(1)-XYOFF(1))) THEN
            XYSCL(1) = XYSCL(4)
            XYOFF(1) = XYOFF(4)
            SPHASE = .TRUE.
            END IF
         IF ((.NOT.FLOTEM) .AND. (IPHASE.GT.1)) THEN
            IF (XYSCL(4)-XYOFF(4).LT.XYSCL(IPHASE)-XYOFF(IPHASE)) THEN
               XYSCL(IPHASE) = XYSCL(4)
               XYOFF(IPHASE) = XYOFF(4)
               SPHASE = .TRUE.
               END IF
            END IF
         DO 130 I = 1,NAMPPH
            IF (SCALEM(I)) THEN
               IF (XYSCL(I).LE.XYOFF(I)) GO TO 980
C                                       Deal with U,V,W axes
               IF (UVREV) THEN
                  XYSCL(I) = MAX (ABS(XYSCL(I)), ABS(XYOFF(I)))
                  XYOFF(I) = -XYSCL(I)
                  END IF
               IF ((XYOFF(I).GT.0.0) .AND.
     *            (XYOFF(I).LT.0.1*XYSCL(I))) XYOFF(I) = 0.0
               IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.3*XYSCL(I))
     *            .AND. (TYPEAX(I).EQ.1)) XYOFF(I) = 0.0
               SIZE = 0.025 * (XYSCL(I) - XYOFF(I))
               XYSCL(I) = XYSCL(I) + SIZE
               XYOFF(I) = XYOFF(I) - SIZE
               SIZE = 1000.
               IF ((I.EQ.2) .OR. (I.EQ.3)) SIZE = 1000. / MAXPLT
               XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
               END IF
 130        CONTINUE
         IF (FLOTEM) THEN
            DO 150 I = 1, NBASE
               IF (YYMAX(I).LE.YYMIN(I)) GO TO 980
C                                       Deal with U,V,W axes
               IF (UVREV) THEN
                  YYMAX(I) = MAX (ABS(YYMAX(I)), ABS(YYMIN(I)))
                  YYMIN(I) = -YYMAX(I)
                  END IF
               IF ((YYMIN(I).GT.0.0) .AND. (YYMIN(I).LT.0.1*YYMAX(I)))
     *            YYMIN(I) = 0.0
               IF ((YYMIN(I).GT.0.0) .AND. (TYPEAX(2).EQ.1) .AND.
     *            (YYMIN(I).LT.0.3*YYMAX(I))) YYMIN(I) = 0.0
               SIZE = 0.025 * (YYMAX(I) - YYMIN(I))
               YYMAX(I) = YYMAX(I) + SIZE
               YYMIN(I) = YYMIN(I) - SIZE
               SIZE = 1000. / MAXPLT
               YYMAX(I) = SIZE / (YYMAX(I) - YYMIN(I))
               IF (AMPPH) THEN
                  IF ((PHMIN(I).GT.0.0) .AND.
     *               (PHMIN(I).LT.0.1*PHMAX(I))) PHMIN(I) = 0.0
                  IF ((PHMIN(I).GT.0.0) .AND. (TYPEAX(3).EQ.1) .AND.
     *               (PHMIN(I).LT.0.3*PHMAX(I))) PHMIN(I) = 0.0
                  SIZE = 0.025 * (PHMAX(I) - PHMIN(I))
                  PHMAX(I) = PHMAX(I) + SIZE
                  PHMIN(I) = PHMIN(I) - SIZE
                  SIZE = 1000. / MAXPLT
                  PHMAX(I) = SIZE / (PHMAX(I) - PHMIN(I))
                  END IF
               IF (IPHASE.GT.1) THEN
                  IF ((PPMIN(I).GT.0.0) .AND.
     *               (PPMIN(I).LT.0.1*PPMAX(I))) PPMIN(I) = 0.0
                  IF ((PPMIN(I).GT.0.0) .AND. (TYPEAX(3).EQ.1) .AND.
     *               (PPMIN(I).LT.0.3*PPMAX(I))) PPMIN(I) = 0.0
                  SIZE = 0.025 * (PPMAX(I) - PPMIN(I))
                  PPMAX(I) = PPMAX(I) + SIZE
                  PPMIN(I) = PPMIN(I) - SIZE
                  SIZE = 1000. / MAXPLT
                  PPMAX(I) = SIZE / (PPMAX(I) - PPMIN(I))
                  END IF
  150          CONTINUE
            END IF
         END IF
      GO TO 999
C
 980  IRET = 1
      WRITE (MSGTXT,1980) I
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('XYSC: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE FILANT (DISK, CNO, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, NSUBA, NBASE, ANT1, ANT2, STNS, IRET)
C-----------------------------------------------------------------------
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned. Also fills in 2
C   arrays with all possible cominations of antenna numbers.
C   Fills array of selected antennas for possible using in UVGET.
C   Find  number and list of selected antennas.
C-----------------------------------------------------------------------
C   Inputs:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C      IXANT    I(50)    List of user supplied antennas
C      IXBASL   I(50)    Baselines to match XANTEN
C      NXANT    I        # entries in XANTEN
C      NXBASL   I        # entries in XBASE
C      DESEL    L        True if entries are to be de-selected rather
C                        than selected
C      NSUBA    I        Subarray used
C   Output:
C      NBASE    I        Max # baselines
C      ANT1     I(*)     1st antenna number of baseline pairs selected
C      ANT2     I(*)     2nd antenna number of baseline pairs selected
C      STNS     C(*)*8   station names
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                           10 = no AN files.
C   Output in common:
C      ANTENS   I(*)     Array of selected antennas
C      NANTSL   I        Number of selected antennas
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN, IXANT(50), IXBASL(50), NXANT, NXBASL,
     *   NSUBA, NBASE, ANT1(*), ANT2(*), IRET
      LOGICAL   DESEL
      CHARACTER STNS(*)*8
C
      INTEGER   NBUFF, II, NUMREC, J, MXNSTA, I1, IERR, ICNT, K, I,
     *   SCRTCH(512), IANT
      LOGICAL   ACCEPT, REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       Set default results.
      CALL FILL (MXBASE, 0, ANT1)
      CALL FILL (MXBASE, 0, ANT2)
      NBUFF = 1024
C                                       read the antenna file
C                                       Open file
      CALL ANTINI ('READ', SCRTCH, DISK, CNO, NSUBA, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NSUBA
         GO TO 990
         END IF
C                                       Get # of antennas in subarray.
      NUMREC = SCRTCH(5)
      MXNSTA = 1
      ICNT = 0
      DO 10 II = 1,NUMREC
         CALL TABAN ('READ', SCRTCH, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', NSUBA
            GO TO 990
            END IF
         MXNSTA = MAX (NOSTA, MXNSTA)
         STNS(NOSTA) = ANNAME
         IF ((NXANT.EQ.0) .AND. (NXBASL.EQ.0)) THEN
            ICNT = ICNT + 1
            IXANT(ICNT) = NOSTA
            END IF
 10      CONTINUE
      NXANT = MAX (NXANT, ICNT)
C                                       make a baseline list
      NBASE = 0
      DO 50 I1 = 1,MXNSTA
         DO 40 J = I1,MXNSTA
            IF (((I1.LT.J) .AND. (.NOT.DOACOR)) .OR.
     *         ((DOACOR) .AND. (I1.EQ.J))) THEN
               ACCEPT = REQBAS (I1, J, DESEL, IXANT, NXANT, IXBASL,
     *            NXBASL)
               IF (ACCEPT) THEN
                  NBASE = NBASE + 1
                  ANT1(NBASE) = I1
                  ANT2(NBASE) = J
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Find number of selected antennas
C                                       and their list
      IANT = 0
      DO 70 I = 1,MAXANT
         DO 60 K = 1,NBASE
            IF (I.EQ.ANT1(K)) THEN
               IANT = IANT + 1
               ANTENS(IANT) = ANT1(K)
               GO TO 70
               END IF
            IF (I.EQ.ANT2(K)) THEN
               IANT = IANT + 1
               ANTENS(IANT) = ANT2(K)
               GO TO 70
               END IF
 60         CONTINUE
 70      CONTINUE
      NANTSL = IANT
C                                       Close
      CALL TABIO ('CLOS', 0, II, SCRTCH, SCRTCH, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILANT: ERROR',I3,1X,A4,'ING AN FILE ',I5)
      END
      SUBROUTINE BADREC (SCANV, NLVIS, DTT, MBASE, MSAMP, MPOL, MCHAN,
     *   MIF, XVAL, YVAL, LLBAS, NBAD)
C-----------------------------------------------------------------------
C   Finds the points with large deviation from the mean and
C   send them to a flag table
C   Inputs:
C      COUNT    I(*)     Number of points in the interval of averaging
C                        depending on baselines
C      RMS      R(*)     Array of rms depending on baselines
C      SCANV    R(*,*)   The result of averaging depending on baselines
C      NLVIS    I        The whole visibilities number in the interval
C      DTT      R        Preaverage interval (days)
C   Input in common:
C     LLBAS(*)     I   Array of baselines
C     ANT1(MXBASE) I   Array of the first antennas
C     ANT2(MXBASE) I   Array of the second antennas
C     XVAL(*)      R   Array of stored times
C     YVAL(*)      R   Array of stored amplitudes
C     KBIF         I   Begin IF number
C     KEIF         I   End IF number
C     KBCH         I   Begin channel number
C     KECH         I   End channel number
C     NSIGMA       I   Number of sigma in threshold estimation
C   Input/Output:
C     NBAD         I   The total number of bad found points
C-----------------------------------------------------------------------
      INCLUDE 'VPLOT.INC'
      INTEGER   NLVIS, MBASE, MSAMP, MPOL, MCHAN, MIF, LLBAS(*)
      REAL      SCANV(7,MBASE,MPOL,MCHAN,*), DTT, XVAL(*),
     *   YVAL(MSAMP,MPOL,MCHAN,*)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   IA1, IA2, IVIS, I, ANTSS(2), JERR, IFS(2), CHANS(2),
     *   NBAD, LIF, LPOL, LCHAN
      REAL      THRES, TOLT, DAMP, TIMER(2), AMP
      LOGICAL   PFLAGS(4), LFSET
      CHARACTER LSTOK(2)*4
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
      TOLT = DTT / (24.*60.*60.)
      IF (ALLCH) THEN
         CHANS(1) = 1
         CHANS(2) = 0
      ELSE
         CHANS(1) = KBCH
         CHANS(2) = KECH
         END IF
      IFS(1) = KBIF
      IFS(2) = KEIF
      IF (XSTOK.EQ.'I') THEN
         LSTOK(1) = '1111'
         LSTOK(2) = '1111'
      ELSE IF (XSTOK.EQ.'LL') THEN
         LSTOK(1) = '0111'
      ELSE
         LSTOK(1) = '1011'
         LSTOK(2) = '0111'
         END IF
      DO 200 IVIS = 1,NLVIS
         DO 100 I = 1,NBASE
            IF (I.EQ.LLBAS(IVIS)) THEN
               IA1 = ANT1(I)
               IA2 = ANT2(I)
C                                       flag all baseline with IA1
C                                       if auto correlation
               IF (DOACOR) IA2 = 0
               ANTSS(1) = IA1
               ANTSS(2) = IA2
C                                       time interval
               TIMER(1) = XVAL(IVIS) - TOLT
               TIMER(2) = XVAL(IVIS) + TOLT
C                                       loop over IFs
               DO 50 LPOL = 1,MPOL
               DO 50 LCHAN = 1,MCHAN
               DO 50 LIF = 1,MIF
                  IF ((SCANV(1,I,LPOL,LCHAN,LIF).NE.FBLANK) .AND.
     *               (SCANV(5,I,LPOL,LCHAN,LIF).GE.10.)) THEN
                     THRES = NSIGMA * SCANV(4,I,LPOL,LCHAN,LIF)
                     AMP = SQRT (SCANV(1,I,LPOL,LCHAN,LIF)**2 +
     *                 SCANV(2,I,LPOL,LCHAN,LIF)**2)
                     DAMP = ABS (YVAL(IVIS,LPOL,LCHAN,LIF)-AMP)
                     IF (DAMP.GT.THRES) THEN
                        IF (MIF.GT.1) THEN
                           IFS(1) = LIF + KBIF - 1
                           IFS(2) = IFS(1)
                           END IF
                        IF (MCHAN.GT.1) THEN
                           CHANS(1) = LCHAN + KBCH - 1
                           CHANS(2) = CHANS(1)
                           END IF
                        CALL FLGSTK (LSTOK(LPOL), 'FLAG', PFLAGS, LFSET,
     *                     JERR)
                        CALL TABFLG ('WRIT', BUFF, KFGRNO, FGKOLS,
     *                     FGNUMV, IDN(1), ISUB, FQID, ANTSS, TIMER,
     *                     IFS, CHANS, PFLAGS, REASON,JERR)
                        NBAD = NBAD + 1
                        END IF
                     END IF
 50               CONTINUE
               END IF
 100        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE AVGCIF (VIS, CCH, BCHAN, ECHAN, LPOL, CIF, BIF, EIF,
     *   VMIN, VMAX, VISOUT)
C-----------------------------------------------------------------------
C   Routine to average a spectrum or group of IFs in frequency to
C   produce a so-called pseudo-continuum channel. The CHNSEL array is
C   used to specify which channels are required in the average.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt)
C      BCHAN    I         Start channel to accept
C      ECHAN    I         Final channel to accept
C      CIF      I         Current IF: 0 -> ALL
C      BIF      I         Start IF to accept
C      EIF      I         Final IF to accept
C   inputs in common:
C      DTYPE    I         Type of Y-axis, 1 => amplitude, 2 => phase,
C                         9 => RE, 10 => IM, 16 => weight, 17 => RMS.
C   Output:
C      VMIN     R         MIN over data averaged
C      VMAX     R         MAX over data averaged
C      VISOUT   R(3)      Pseudo-continuum visibility
C-----------------------------------------------------------------------
      INTEGER   CCH, BCHAN, ECHAN, LPOL, CIF, BIF, EIF
      REAL      VIS(*), VMIN, VMAX, VISOUT(*)
C
      INCLUDE 'VPLOT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPIF, LOOPF, INDEX, INP, FCHAN, SCHAN, FIF, SIF
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM, AMP, SWT, SRE, SIM,
     *   XN, FI, RE, IM, SUMAMP, SAMP
C
      REAL      AMPBEG, AMPEND, FIBEG, FIEND
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      VMIN = 1.E10
      VMAX = -1.E10
      IF (CCH.LE.0) THEN
         FCHAN = 1
         SCHAN = FCHAN + ECHAN - BCHAN
      ELSE
         FCHAN = CCH - BCHAN + 1
         SCHAN = FCHAN
         END IF
      IF (CIF.EQ.0) THEN
         FIF = BIF
         SIF = EIF
      ELSE
         FIF = CIF
         SIF = CIF
         END IF
C                                       use the one selected polzn.
      SUMWT = 0.0
      SUMRE = 0.0
      SUMIM = 0.0
      SUMAMP = 0.0
      DO 30 LOOPIF = FIF,SIF
         SWT = 0.0
         SRE = 0.0
         SIM = 0.0
         SAMP = 0.0
         INDEX = 1 + (IPOLPT+LPOL-1)*INCS + (LOOPIF-BIF)*INCIF
         DO 20 LOOPF = FCHAN,SCHAN
            INP = INDEX + (LOOPF-1)*INCF
            WT = VIS(INP+2)
            IF (WT.LE.0.0) GO TO 20
            RE = VIS(INP)
            IM = VIS(INP+1)
            AMP = SQRT (RE*RE + IM*IM)
            FI = 0.0
            IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) FI = ATAN2 (IM, RE)
            IF ((APARM(3).GT.0) .AND. (APARM(4).GT.0)) THEN
C                                       Max and min amplitude
               IF (DTYPE.EQ.1) THEN
                  IF (AMP.GT.VMAX) VMAX = AMP
                  IF (AMP.LT.VMIN) VMIN = AMP
C                                       Max and min phase
               ELSE IF (DTYPE.EQ.2) THEN
                  IF (FI.GT.VMAX) VMAX = FI
                  IF (FI.LT.VMIN) VMIN = FI
C                                       Max and min RE
               ELSE IF (DTYPE.EQ.9) THEN
                  IF (RE.GT.VMAX) VMAX = RE
                  IF (RE.LT.VMIN) VMIN = RE
C                                       Max and min IM
               ELSE IF (DTYPE.EQ.10) THEN
                  IF (IM.GT.VMAX) VMAX = IM
                  IF (IM.LT.VMIN) VMIN = IM
                  END IF
               END IF
C                                       sum for averages
            SUMAMP = SUMAMP + AMP*WT
            SAMP = SAMP + AMP*WT
            SUMRE = SUMRE + RE*WT
            SUMIM = SUMIM + IM*WT
            SRE = SRE + RE*WT
            SIM = SIM + IM*WT
            SUMWT = SUMWT + WT
            SWT = SWT + WT
 20         CONTINUE
         XN = 1.0
         IF (SWT.GT.1.0E-10) THEN
            XN = 1.0 / SWT
C                                       normalize sums of phases
C                                       and amplitudes
            RE = SRE * XN
            IM = SIM * XN
            FI = 0.0
            IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) FI = ATAN2 (IM, RE)
            AMP = SQRT (RE*RE + IM*IM)
C

            IF (AVGSCA) AMP = SAMP * XN
C                                       store the begin IF
            IF (DOIFRA .AND. LOOPIF.EQ.FIF) THEN
               AMPBEG = AMP
               FIBEG = FI
               END IF
C                                       store the end IF
            IF (DOIFRA .AND. LOOPIF.EQ.SIF) THEN
               AMPEND = AMP
               FIEND = FI
               END IF

C                                       Max and min amplitude/phase
C                                       for the given visibility
C                                       averaged for all channels
            IF ((APARM(3).GT.0) .AND. (APARM(4).EQ.0)) THEN
C                                       Max and min amplitude
               IF (DTYPE.EQ.1) THEN
                  IF (AMP.GT.VMAX) VMAX = AMP
                  IF (AMP.LT.VMIN) VMIN = AMP
C                                       Max and min phase
               ELSE IF (DTYPE.EQ.2) THEN
                  IF (FI.GT.VMAX) VMAX = FI
                  IF (FI.LT.VMIN) VMIN = FI
C                                       Max and min RE
               ELSE IF (DTYPE.EQ.9) THEN
                  IF (RE.GT.VMAX) VMAX = RE
                  IF (RE.LT.VMIN) VMIN = RE
C                                       Max and min IM
               ELSE IF (DTYPE.EQ.10) THEN
                  IF (IM.GT.VMAX) VMAX = IM
                  IF (IM.LT.VMIN) VMIN = IM
                  END IF
               END IF
            END IF
 30      CONTINUE
      XNORM = 1.0
      IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
      RE = SUMRE * XNORM
      IM = SUMIM * XNORM
      AMP = SUMAMP * XNORM

      FI = 0.0
      IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) FI = ATAN2 (IM, RE)
      IF (AVGSCA) THEN
         VISOUT(1) = AMP * COS(FI)
         VISOUT(2) = AMP * SIN(FI)
      ELSE
         VISOUT(1) = RE
         VISOUT(2) = IM
         END IF
C
      IF (DOIFRA) THEN
         VISOUT(1) = (AMPBEG/AMPEND) * COS(FIBEG-FIEND)
         VISOUT(2) = (AMPBEG/AMPEND) * SIN(FIBEG-FIEND)
         END IF
C                                       weight of the ratio
C                                       OPCODE = 'IFRA'???????
      VISOUT(3) = SUMWT
C
 999  RETURN
      END

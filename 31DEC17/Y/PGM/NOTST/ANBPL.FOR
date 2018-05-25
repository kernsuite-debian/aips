LOCAL INCLUDE 'ANBPL.INC'
C                                       Local include for ANBPL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK,
     *   XOPCOD, XLPFIL(12)
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, STANAM(MAXANT)*8,
     *   XSOUR(30)*16, XCALCO*4, OPCODE*4, LPNAME*48, TSTOK(4)*2
      REAL      USERID, XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID,
     *   XBCHAN, XECHAN, XBIF, XEIF, XAVGIF, XINC, UVRANG(2), XTIME(8),
     *   XANT(50), XBASE(50), XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XSUBA, SOLINT, XDO3C,
     *   BPARM(10), XNCOUN, XSYM, FACTOR, XLABEL, XDOTV, XGRCH, DOCRT,
     *   XNDIG, XYRATO
      REAL      BUFF1(UVBFSS), IATUTC, UT1XXX, CHOUT(4), INISCL(6)
      INTEGER   NSOLIN, JBUFSZ, NANT, NSRC, IDN(30), OLDCNO,
     *   CATKEP(256), NPOINT(MAXANT,4,MAXIF), LUN1, LUN2, IXBAS(50),
     *   IXANT(50), NXANT, NXBAS
      LOGICAL   DOTV, MULTI, DOAVG, AVGSCA, AMPPH, COPFG, FLOTEM
      REAL      TBEG, TFIN, XYSCL(3), XYOFF(3), TIMMIN, TIMMAX,
     *   YYMIN(MAXANT,4,MAXIF), YYMAX(MAXANT,4,MAXIF),
     *   PHMIN(MAXANT,4,MAXIF), PHMAX(MAXANT,4,MAXIF)
      INTEGER   INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX(3), NCH, VER2,
     *   TESTEM(3), POLPLT, ANTS(50), BASE(50), ISUB, NPARMS,
     *   FQID, GRCHN, TVCHN, TVCORN(4), NUMFRQ, DTYPE, LABEL, INVER,
     *   OUTVER, KBIF, KEIF, KBCH, KECH, BUFF(512), KFGRNO
      LOGICAL   SCALEM(3), DECICL, INDECL, DESEL
C
      DOUBLE PRECISION JDREF, GST0, OBSRA, OBSDEC
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR,
     *   XQUAL, XXCALC, XBAND, XFREQ, XFQID, XBCHAN, XECHAN, XBIF, XEIF,
     *   XAVGIF, XINC, UVRANG, XTIME, XXSTOK, XANT, XBASE, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH,
     *   XSUBA, SOLINT, XDO3C, XOPCOD, BPARM, XNCOUN, XSYM, FACTOR,
     *   XLABEL, XDOTV, XGRCH, XYRATO, DOCRT, XLPFIL, XNDIG
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /VBPCOM/ TBEG, TFIN, XYSCL, XYOFF, TIMMIN, TIMMAX, SCALEM,
     *   INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH, VER2, TESTEM,
     *   POLPLT, ANTS, BASE, PHMIN, PHMAX, GRCHN, TVCHN, TVCORN, NSRC,
     *   IDN, OLDCNO, CATKEP, ISUB, LABEL, CHOUT, NUMFRQ, DTYPE, DOTV,
     *   MULTI, DOAVG, AVGSCA, AMPPH, DECICL, INDECL, YYMIN, YYMAX,
     *   NPOINT, NPARMS, COPFG, INVER, OUTVER, LUN1, LUN2, NSOLIN,
     *   KFGRNO, IXBAS, IXANT, NXANT, NXBAS, DESEL, FLOTEM, FQID, KBIF,
     *   KEIF, KBCH, KECH, INISCL, BUFF
      COMMON /CANIN/ JDREF, GST0, OBSRA, OBSDEC, IATUTC, UT1XXX, NANT
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSTOK, STANAM, XSOUR, XCALCO,
     *   LPNAME, TSTOK, OPCODE
LOCAL END
      PROGRAM ANBPL
C-----------------------------------------------------------------------
C! Plots selected uv data and model values.
C# UV Plot-appl VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2006-2007, 2009-2010, 2012, 2014-2016
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
C   ANBPL plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     USERID                       UV data file owner # ignored
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input UV data.
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
C     SOLINT....pre-average time (mins)
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
C      NCOUNT.. Maximum number of plots per page
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, MAXPLT, MSAMP, MIF, MPOL, NWORDS, IERR
C                                       large buffer areas
      REAL      PBUFF(2), SBUFF(2)
      LONGINT   PPBUFF, PXVAL, PYVAL, PIANT, PPHVAL, PSUM
      INTEGER   IPBUFF(2)
      INCLUDE 'ANBPL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IPBUFF, PBUFF)
      DATA PRGM /'ANBPL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL ANPIN (PRGM, MAXPLT, MSAMP, MPOL, MIF, IRET)
      IF (IRET.NE.0) GO TO 995
      NWORDS = (NANT * NANT * MPOL * MIF * 4 - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SBUFF, PSUM, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'REALLY NOT ENOUGH MEMORY FOR THIS JOB: QUITTING'
         CALL MSGWRT (9)
         GO TO 995
         END IF
C                                       allocate memory
 10   NWORDS = 2 + MIF*MPOL
      IF (AMPPH) NWORDS = NWORDS + MIF*MPOL
      NWORDS = (NWORDS * MSAMP - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PBUFF, PPBUFF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'NOT ENOUGH MEMORY FOR THIS JOB: QUITTING'
         CALL MSGWRT (9)
         GO TO 995
         END IF
      PXVAL = PPBUFF + 1
      PYVAL = PXVAL + MSAMP
      PIANT = PYVAL + MSAMP * MIF*MPOL
      PPHVAL = PIANT + MSAMP
      IF ((XDOTV.EQ.0.0) .AND. (DOCRT.EQ.0.0)) XDOTV = -1.0
C                                       determine scaling
      CALL SCALAN (NANT, MAXPLT, MSAMP, MPOL, MIF, SBUFF(1+PSUM),
     *   PBUFF(PXVAL), PBUFF(PYVAL), IPBUFF(PIANT), PBUFF(PPHVAL), IRET)
      IF (IRET.GT.0) GO TO 990
C                                       retry
      IF (NSOLIN.GT.MSAMP) THEN
         CALL ZMEMRY ('FREE', TSKNAM, NWORDS, PBUFF, PPBUFF, IRET)
         WRITE (MSGTXT,1000) MSAMP, NSOLIN
         CALL MSGWRT (6)
         MSAMP = 1.01 * NSOLIN
         GO TO 10
         END IF
C                                       check phase scaling
      IF ((FLOTEM) .AND. (XDOTV.NE.0.0)) THEN
         IF ((DTYPE.EQ.13) .OR. (DTYPE.EQ.14)) THEN
            CALL FIXPHS (MAXPLT, MSAMP, MPOL, MIF, PBUFF(PYVAL),
     *         IPBUFF(PIANT), YYMIN, YYMAX)
         ELSE
            CALL FIXOTH (MAXPLT, MSAMP, MPOL, MIF, PBUFF(PYVAL),
     *         IPBUFF(PIANT), YYMIN, YYMAX)
            END IF
         IF (AMPPH) CALL FIXPHS (MAXPLT, MSAMP, MPOL, MIF,
     *      PBUFF(PPHVAL), IPBUFF(PIANT), PHMIN, PHMAX)
         END IF
C                                       Do plots
      IF (XDOTV.NE.0.0) THEN
         CALL ANPLOT (MAXPLT, MSAMP, MPOL, MIF, PBUFF(PXVAL),
     *      PBUFF(PYVAL), IPBUFF(PIANT), PBUFF(PPHVAL), IRET)
         IRET = MAX (0, IRET)
         END IF
C                                       Do print
      IF ((DOCRT.NE.0.0) .AND. (IRET.EQ.0) .AND. (TYPEAX(1).LE.2)) THEN
         CALL ANPRCT (MSAMP, MPOL, MIF, PBUFF(PXVAL), PBUFF(PYVAL),
     *      IPBUFF(PIANT), PBUFF(PPHVAL), IRET)
         IF (IRET.EQ.0) CALL ANPRIN (MSAMP, MPOL, MIF, PBUFF(PXVAL),
     *      PBUFF(PYVAL), IPBUFF(PIANT), PBUFF(PPHVAL), IRET)
         IRET = MAX (0, IRET)
         END IF
C                                       clear memory
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, PBUFF, PPBUFF, IERR)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('GUESSED',I10,' SAMPLES, BUT NEED',I10,' RETRY')
      END
      SUBROUTINE ANPIN (PRGM, MAXPLT, MSAMP, MPOL, MIF, JERR)
C-----------------------------------------------------------------------
C   ANPIN gets input parameters for ANBPL
C   Inputs:
C      PRGM     C*6   Program name
C   Output:
C      MAXPLT   I     Maximum number of plots per page.
C      MSAMP    I     Guess maximum size of buffers
C      MPOL     I     Number polarizations
C      MIF      I     Number of separate IFs
C      JERR     I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MAXPLT, MSAMP, MPOL, MIF, JERR
C
      CHARACTER UTYPE*2, STAT*4, ASTOK(13)*2
      INTEGER   IERR, IUSER, VER, I, IROUND, LUN, SUBAR, MCHAN, IV,
     *   DROUND, LTYPE
      REAL      CATR(256), EPS, RPARM(20)
      LOGICAL   MATCH
      DOUBLE PRECISION CATD(128), DV
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'ANBPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATD, CATR, CATBLK)
      DATA ASTOK /'YX','XY','YY','XX','LR','RL','LL','RR','B ','I',
     *   'Q','U','V'/
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
      NPARMS = 296
      CALL GTPARM (PRGM, NPARMS, RQUICK, USERID, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      CALL H2CHR (48, 1, XLPFIL, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
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
      ISUB = MAX (1, ISUB)
      SUBARR = ISUB
      XSUBA = SUBARR
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       'cros' or 'auto'
      DOACOR = .FALSE.
      DOXCOR = .TRUE.
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
C                                       Test type of plot
      IF (BPARM(1).LE.0.0) BPARM(1) = 2.0
      IF (BPARM(2).LE.0.0) BPARM(2) = 12.0
      XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYSCL(3) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
      XYOFF(3) = XYOFF(1)
      DECICL = .FALSE.
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      SCALEM(3) = (BPARM(3).LE.0.0) .OR. (BPARM(8).EQ.BPARM(9))
      FLOTEM = (BPARM(3).EQ.0.0)
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((FLOTEM) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((FLOTEM) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
      TESTEM(3) = 1
      IF (BPARM(8).GT.BPARM(9)) TESTEM(3) = -1
      IF ((FLOTEM) .OR. (BPARM(8).EQ.BPARM(9))) TESTEM(3) = 0
      CALL RCOPY (6, BPARM(4), INISCL)
C                                       Get CATBLK from file.
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
C                                       Check pre-average time
      IF (SOLINT.LE.0.0) SOLINT = 1.0/60.0
      SOLINT = SOLINT / 1440.0
      IF (STOKES.EQ.' ') STOKES = 'HALF'
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
      XFQID = FRQSEL
C                                       Test channel #
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.EQ.0) ECHAN = BCHAN
      IF (ECHAN.LT.BCHAN) ECHAN = BCHAN
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (ECHAN.GT.BCHAN) THEN
         WRITE (MSGTXT,1300) BCHAN, ECHAN
         CALL MSGWRT (3)
         END IF
C                                       number of channels and
C                                       polarizations in the data
      NUMFRQ = CATBLK(KINAX+JLOCF)
      XBCHAN = BCHAN
      XECHAN = ECHAN
      KBCH = BCHAN
      KECH = ECHAN
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
      DOAVG = (EIF.GT.BIF) .AND. (XAVGIF.GT.0.0)
      IF (DOAVG) THEN
         WRITE (MSGTXT,1400) BIF, EIF
         CALL MSGWRT (3)
         END IF
      XBIF = BIF
      KBIF = BIF
      XEIF = EIF
      KEIF = EIF
C                                       size parameters
      MIF = EIF - BIF + 1
      IF (DOAVG) MIF = 1
      MSAMP = CATBLK(KIGCN)
      MSAMP = MIN (MSAMP, 2000000)
C                                       Find baselines to plot
      CALL SETANT (50, XANT, XBASE, NXANT, NXBAS, IXANT, IXBAS, DESEL)
C                                       Fill in list of all antenna
C                                       - baseline pairs and names.
C                                       Determine the list and number
C                                       of selected antennas.
      SUBAR = SUBARR
      IF (SUBAR.LE.0) SUBAR = 1
      CALL GETANT (DISKIN, OLDCNO, SUBAR, CATBLK, BUFF1, JERR)
C                                       Save results in input Parms
      TYPEAX(1) = IROUND (BPARM(1))
      TYPEAX(2) = IROUND (BPARM(2))
      AMPPH = BPARM(2).LT.0.0
      IF (AMPPH) THEN
         TYPEAX(1) = 2
         TYPEAX(2) = 12
         TYPEAX(3) = 13
         END IF
C                                       Y axis type
      IF (TYPEAX(2).LE.0) TYPEAX(2) = 17
      DTYPE = TYPEAX(2)
      IF ((DTYPE.LT.11) .OR. (DTYPE.GT.17)) THEN
         MSGTXT = 'Y axis must be amp, phase, real, imag, or weight'
         CALL MSGWRT (7)
         DTYPE = 12
         TYPEAX(2) = DTYPE
         END IF
C                                       X can be time, u, v, uv-dist
      IF (TYPEAX(1).LE.0) TYPEAX(1) = 2
      IF ((TYPEAX(1).LT.1) .OR. (TYPEAX(1).GT.7)) THEN
         MSGTXT = 'X axis can be time(s), HA, elev, PA, source'
         CALL MSGWRT (7)
         TYPEAX(1) = 2
         END IF
C                                       scalar averaging allowed?
C                                       type of averaging in time
      AVGSCA = BPARM(10).GT.0
C                                       Get antenna info.
C                                       Read the first version of
C                                       antennas file if subarr=0
      VER = SUBAR
      MCHAN = (XBCHAN + XECHAN) / 2
      CALL ANTIN (VER, DISKIN, OLDCNO, FRQSEL, JERR)
C                                       Maximum number of
C                                       plots per page
      MAXPLT = 3
      IF (XNCOUN.GE.0.95) MAXPLT = XNCOUN + EPS
      MAXPLT = MIN (NANT, MAXPLT)
      IF (AMPPH) MAXPLT = 2 * MAXPLT
      MAXPLT = MIN (20,MAXPLT)
C                                       how many Stokes are there?
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, JERR)
      IF (JERR.GT.0) THEN
         WRITE (MSGTXT,1075) JERR
         GO TO 990
      ELSE IF (JERR.LT.0) THEN
         MSGTXT = 'ANPON: NO DATA FOUND'
         JERR = 1
         GO TO 990
         END IF
      MPOL = CATBLK(KINAX+JLOCS)
      IF (MIF*MPOL.LE.1) XDO3C = -1.0
      DO 100 I = 1,MPOL
         DV = CATD(KDCRV+JLOCS) + (I - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         IV = DROUND (DV) + 9
         TSTOK(I) = ASTOK(IV)
 100     CONTINUE
      CALL UVGET ('CLOS', RPARM, BUFF1, I)
      CALL COPY (256, CATKEP, CATBLK)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1075 FORMAT ('ANPIN: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('You have sort order ',A2,
     *   '. Sort the data to TB with UVSRT')
 1200 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1300 FORMAT ('Averaging from channel ',I4,'-',I4)
 1400 FORMAT ('Averaging from IF ',I4,'-',I4)
      END
      SUBROUTINE SCALAN (CANT, MAXPLT, MSAMP, MPOL, MIF, SUMS, XVAL,
     *   YVAL, IANT, PHVAL, IRET)
C-----------------------------------------------------------------------
C   SCALAN stores arrays of Y and X for future plot
C   Return:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C                  < 0 => MSAMP changed to what is really needed
C-----------------------------------------------------------------------
      INTEGER   CANT, MAXPLT, MSAMP, MPOL, MIF, IANT(*), IRET
      REAL      SUMS(CANT,CANT,MPOL,MIF,*), XVAL(*), YVAL(MSAMP,MPOL,*),
     *   PHVAL(MSAMP,MPOL,*)
C
      INCLUDE 'ANBPL.INC'
      INTEGER   IARR, I, LOOP, ISOLIN, NUMVIS, XUMVIS, SCANUM, NAMPPH,
     *   NBAD, LIF, IERR, LP
      REAL      XY(3), RPARM(20), SCANV(MAXANT,4,MAXIF), HA, EL,
     *   PHAVG(MAXANT,4,MAXIF), DMULT, AVT, AZ
      LOGICAL   DOSCAL, NUSCAN, DIDMSG
      DOUBLE PRECISION DT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      SCANUM = -101
      DIDMSG = .FALSE.
      DO 10 LIF = 1,MIF
         DO 9 LP = 1,MPOL
            DO 8 LOOP = 1,NANT
               YYMIN(LOOP,LP,LIF) = 1.E10
               YYMAX(LOOP,LP,LIF) = -1.E10
               PHMIN(LOOP,LP,LIF) = 1.E10
               PHMAX(LOOP,LP,LIF) = -1.E10
 8             CONTINUE
 9          CONTINUE
 10      CONTINUE
C                                       prepare for AMP&PH
      NAMPPH = 2
      IF (AMPPH) NAMPPH = 3
      RPARM(1) = FBLANK
      DOSCAL = (SCALEM(1)) .OR. (SCALEM(2))
      IF (AMPPH) DOSCAL = DOSCAL .OR. SCALEM(3)
      IRET =  0
C                                       Check if valid POLTYP
      CALL COPY (256, CATBLK, CATUV)
C
      XUMVIS = 0
      NSOLIN = MSAMP
C                                       initiate to zero interval number
C                                       and number of found bad points
C                                       and number of baseline for the
C                                       interval
      ISOLIN = 0
      NBAD = 0
      DO 60 I = 1,NANT
         DO 50 LIF = 1,MIF
            DO 40 LP = I,MPOL
               NPOINT(I,LP,LIF) = 0
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
      NUMVIS = 0
C                                       Find scales
      DMULT = 1.0
      IF (DTYPE.EQ.13) DMULT = RAD2DG
      IF (DTYPE.EQ.14) DMULT = RAD2DG
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'SCALAN: NO DATA FOUND'
         CALL MSGWRT (8)
         GO TO 180
         END IF
      RPARM(1) = FBLANK
C                                       Loop
C                                       Read vis recs and form averages
 100  CALL BASAVG (NUMVIS, SCANV, PHAVG, NUSCAN, SCANUM, RPARM, BUFF1,
     *   AVT, MPOL, MIF, CANT, SUMS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         IRET = 4
         GO TO 990
C                                       Got data before end
      ELSE IF ((IRET.NE.-1) .AND. (AVT.LE.TFIN)) THEN
C                                       Pick up some points
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,INC).NE.0) GO TO 100
         IF (AVT.LT.TBEG) GO TO 100
C                                       Get array #
         IARR = SUBARR
         DO 160 I = 1,NANT
C                                       any good ones?
            DO 110 LP = 1,MPOL
               DO 105 LIF = 1,MIF
                  IF (SCANV(I,LP,LIF).NE.FBLANK) GO TO 120
 105              CONTINUE
 110           CONTINUE
            GO TO 160
C                                       store values of data
 120        ISOLIN = ISOLIN + 1
            XUMVIS = XUMVIS + 1
            IF (ISOLIN.LE.MSAMP) THEN
               IANT(ISOLIN) = I
C                                       X axis
               IF (TYPEAX(1).EQ.1) THEN
                  XY(1) = AVT
               ELSE IF (TYPEAX(1).EQ.2) THEN
                  XY(1) = AVT
               ELSE IF (TYPEAX(1).EQ.3) THEN
                  XY(1) = CURSOU
               ELSE IF (TYPEAX(1).EQ.7) THEN
                  CALL PARANG (AVT, XY(1))
                  XY(1) = XY(1) * RAD2DG
               ELSE
                  DT = AVT
Ctemporally L.K.
C                  RAAPP = 12.5*15*PI/180
C                  DECAPP = (12+23.0/60.0)*PI/180
                  CALL SOUELV (I, DT, HA, EL, AZ)
                  IF (TYPEAX(1).EQ.4) THEN
                     XY(1) = HA * RAD2DG / 15.0
                  ELSE IF (TYPEAX(1).EQ.5) THEN
                     XY(1) = EL * RAD2DG
                  ELSE
                     XY(1) = AZ * RAD2DG
                     END IF
                  END IF
               XVAL(ISOLIN) = XY(1)
C                                       loop over IFs
               DO 150 LIF = 1,MIF
                  DO 140 LP = 1,MPOL
C                                       Y axis
                     IF (SCANV(I,LP,LIF).EQ.FBLANK) THEN
                        YVAL(ISOLIN,LP,LIF) = FBLANK
                        IF (AMPPH) PHVAL(ISOLIN,LP,LIF) = FBLANK
                     ELSE
                        NPOINT(I,LP,LIF) = NPOINT(I,LP,LIF) + 1
                        XY(2) = SCANV(I,LP,LIF) * DMULT
                        YVAL(ISOLIN,LP,LIF) = XY(2)
C                                       store min and max amplitudes
                        IF (AMPPH) THEN
                           XY(3) = PHAVG(I,LP,LIF) * RAD2DG
                           PHVAL(ISOLIN,LP,LIF) = XY(3)
                           END IF
C                                       autoscale for min/max lines
                        IF (DOSCAL) CALL XYSC (NUMVIS, XY, NAMPPH,
     *                     MAXPLT, IERR)
                        END IF
 140                 CONTINUE
 150              CONTINUE
               END IF
 160        CONTINUE
         IF (IRET.EQ.0) GO TO 100
         END IF
C                                       Close at end
 180  CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      NSOLIN = ISOLIN
C                                       Any points found
      IF (DOSCAL) THEN
         IF (XUMVIS.LE.1) THEN
            IRET = 4
            WRITE (MSGTXT,1200) XUMVIS
            GO TO 990
            END IF
C                                       Final call to XYSC
         NUMVIS = -1
         CALL XYSC (NUMVIS, XY, NAMPPH, MAXPLT, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1250) IRET
            GO TO 990
            END IF
         END IF
      IRET = 0
      CALL COPY (256, CATKEP, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCALAN: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('SCALAN: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('FOUND',I4,' POINTS: NOT ENOUGH TO SELF-SCALE')
 1250 FORMAT ('SCALAN: XYSC ERROR',I3)
      END
      SUBROUTINE FIXPHS (MAXPLT, MSAMP, MPOL, MIF, YVAL, IANT, PPOFF,
     *   PPSCL)
C-----------------------------------------------------------------------
C   FIXPHS checks the phases to see if an all positive plot would
C   cover a smaller range
C   Inputs:
C      MAXPLT   I            Number plots per page
C      MSAMP    I            Number samples
C      MPOL     I            Number polarizations
C      MIF      I            Number IFs
C      IANT     I(MSAMP)     Antenna values
C   In/Out:
C      YVAL     R(MSAMP,MIF) Phase values
C      PPOFF    R(NANT)      Min phase values for plotting
C      PPSCL    R(NANT)      Phase plot scaling
C-----------------------------------------------------------------------
      INCLUDE 'ANBPL.INC'
      INTEGER   MAXPLT, MSAMP, MPOL, MIF, IANT(*)
      REAL      YVAL(MSAMP,MPOL,MIF), PPOFF(MAXANT,4,*),
     *   PPSCL(MAXANT,4,*)
C
      INTEGER   LA, LF, LS, LP, LP1, LP2, LF1, LF2, JF, JF2, JP, JP2
      REAL      PMIN, PMAX, PP, PPMAX, PPMIN
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      JP2 = MPOL
      JF2 = MIF
      IF (OPCODE.EQ.'ALIF') THEN
         JF2 = 1
      ELSE IF (OPCODE.EQ.'ALST') THEN
         JP2 = 1
      ELSE IF (OPCODE.EQ.'ALSI') THEN
         JF2 = 1
         JP2 = 1
         END IF
      DO 100 LA = 1,NANT
         DO 90 JP = 1,JP2
            DO 80 JF = 1,JF2
               LF1 = JF
               LF2 = JF
               LP1 = JP
               LP2 = JP
               IF (OPCODE.EQ.'ALIF') THEN
                  LF1 = 1
                  LF2 = MIF
               ELSE IF (OPCODE.EQ.'ALST') THEN
                  LP1 = 1
                  LP2 = MPOL
               ELSE IF (OPCODE.EQ.'ALSI') THEN
                  LF1 = 1
                  LF2 = MIF
                  LP1 = 1
                  LP2 = MPOL
                  END IF
               PMIN = 1.E10
               PMAX = -1.E10
               PPMIN = PMIN
               PPMAX = -PMAX
               DO 20 LS = 1,NSOLIN
                  IF (LA.EQ.IANT(LS)) THEN
                     DO 15 LF = LF1,LF2
                        DO 10 LP = LP1,LP2
                           PP = YVAL(LS,LP,LF)
                           IF (PP.NE.FBLANK) THEN
                              PPMIN = MIN (PP, PPMIN)
                              PPMAX = MAX (PP, PPMAX)
                              IF (PP.LT.0.0) PP = PP + 360.0
                              PMIN = MIN (PP, PMIN)
                              PMAX = MAX (PP, PMAX)
                              END IF
 10                        CONTINUE
 15                     CONTINUE
                     END IF
 20               CONTINUE
               IF ((PPMAX.GT.PPMIN) .AND. (PMAX.GT.PMIN) .AND.
     *            (PPMAX-PPMIN-0.1.GT.PMAX-PMIN)) THEN
                  PMAX = PMAX + 0.1 * (PMAX - PMIN)
                  PPMAX = PMAX + 0.025 * (PMAX - PMIN)
                  PPMIN = PMIN - 0.025 * (PMAX - PMIN)
                  PPSCL(LA,JP,JF) = (1000.0/MAXPLT) / (PPMAX - PPMIN)
                  PPOFF(LA,JP,JF) = PPMIN
                  DO 40 LS = 1,NSOLIN
                     IF (LA.EQ.IANT(LS)) THEN
                        DO 30 LF = LF1,LF2
                           DO 25 LP = LP1,LP2
                              PP = YVAL(LS,LP,LF)
                              IF ((PP.NE.FBLANK) .AND. (PP.LT.0.0))
     *                           YVAL(LS,LP,LF) = PP + 360.0
 25                           CONTINUE
 30                        CONTINUE
                        END IF
 40                  CONTINUE
               ELSE
                  PMAX = PPMAX
                  PMIN = PPMIN
                  IF (PMAX.LE.PMIN) THEN
                     PMAX = PMAX + 1.0
                     PMIN = PMIN - 1.0
                     END IF
                  PMAX = PMAX + 0.1 * (PMAX - PMIN)
                  PPMAX = PMAX + 0.025 * (PMAX - PMIN)
                  PPMIN = PMIN - 0.025 * (PMAX - PMIN)
                  PPSCL(LA,JP,JF) = (1000.0/MAXPLT) / (PPMAX - PPMIN)
                  PPOFF(LA,JP,JF) = PPMIN
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FIXOTH (MAXPLT, MSAMP, MPOL, MIF, YVAL, IANT, PPOFF,
     *   PPSCL)
C-----------------------------------------------------------------------
C   FIXOTH checks the non-phases to set scaling - merges IFs and
C   polarizations if needed for OPCODE
C   Inputs:
C      MAXPLT   I            Number plots per page
C      MSAMP    I            Number samples
C      MPOL     I            Number polarizations
C      MIF      I            Number IFs
C      IANT     I(MSAMP)     Antenna values
C   In/Out:
C      YVAL     R(MSAMP,MIF) Phase values
C      PPOFF    R(NANT)      Min phase values for plotting
C      PPSCL    R(NANT)      Phase plot scaling
C-----------------------------------------------------------------------
      INCLUDE 'ANBPL.INC'
      INTEGER   MAXPLT, MSAMP, MPOL, MIF, IANT(*)
      REAL      YVAL(MSAMP,MPOL,MIF), PPOFF(MAXANT,4,*),
     *   PPSCL(MAXANT,4,*)
C
      INTEGER   LA, LF, LS, LP, LP1, LP2, LF1, LF2, JF, JF2, JP, JP2
      REAL      PMIN, PMAX, PP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      JP2 = MPOL
      JF2 = MIF
      IF (OPCODE.EQ.'ALIF') THEN
         JF2 = 1
      ELSE IF (OPCODE.EQ.'ALST') THEN
         JP2 = 1
      ELSE IF (OPCODE.EQ.'ALSI') THEN
         JF2 = 1
         JP2 = 1
         END IF
      DO 100 LA = 1,NANT
         DO 90 JP = 1,JP2
            DO 80 JF = 1,JF2
               LF1 = JF
               LF2 = JF
               LP1 = JP
               LP2 = JP
               IF (OPCODE.EQ.'ALIF') THEN
                  LF1 = 1
                  LF2 = MIF
               ELSE IF (OPCODE.EQ.'ALST') THEN
                  LP1 = 1
                  LP2 = MPOL
               ELSE IF (OPCODE.EQ.'ALSI') THEN
                  LF1 = 1
                  LF2 = MIF
                  LP1 = 1
                  LP2 = MPOL
                  END IF
               PMIN = 1.E10
               PMAX = -1.E10
               DO 20 LS = 1,NSOLIN
                  IF (LA.EQ.IANT(LS)) THEN
                     DO 15 LF = LF1,LF2
                        DO 10 LP = LP1,LP2
                           PP = YVAL(LS,LP,LF)
                           IF (PP.NE.FBLANK) THEN
                              PMIN = MIN (PP, PMIN)
                              PMAX = MAX (PP, PMAX)
                              END IF
 10                        CONTINUE
 15                     CONTINUE
                     END IF
 20               CONTINUE
               IF (PMAX.GT.PMIN) THEN
                  PMAX = PMAX + 0.1 * (PMAX - PMIN)
                  PP = PMAX - PMIN
                  PMAX = PMAX + 0.025 * PP
                  PMIN = PMIN - 0.025 * PP
                  PPSCL(LA,JP,JF) = (1000.0/MAXPLT) / (PMAX - PMIN)
                  PPOFF(LA,JP,JF) = PMIN
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ANPLOT (MAXPLT, MSAMP, MPOL, MIF, XVAL, YVAL, IANT,
     *   PHVAL, IRET)
C-----------------------------------------------------------------------
C   ANPLOT plots the data thru calls to PLTAN.
C   Inputs:
C      MAXPLT    I    The maximum number of plots per page.
C   Outputs:
C      IRET      I     Error code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   MAXPLT, MSAMP, MPOL, MIF, IANT(*), IRET
      REAL      XVAL(*), YVAL(MSAMP,*), PHVAL(MSAMP,*)
C
      INTEGER   IPLOT, LOOP, NLOOP, ILOOP, HMXPLT, TYPEX, LF, LF1, LF2,
     *   LP, LP1, LP2, JF, JF2, JP, JP2, NS
      REAL      TEMPSC, TEMPOF
      LOGICAL   TEMP
      INCLUDE 'ANBPL.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      HMXPLT = MAXPLT
      IF (AMPPH) HMXPLT = MAXPLT / 2
      IRET = 0
      IPLOT = 0
      JP2 = MPOL
      JF2 = MIF
      IF (OPCODE.EQ.'ALIF') THEN
         JF2 = 1
      ELSE IF (OPCODE.EQ.'ALST') THEN
         JP2 = 1
      ELSE IF (OPCODE.EQ.'ALSI') THEN
         JF2 = 1
         JP2 = 1
         END IF
C                                       Find last plot
      NLOOP = 0
      DO 50 LOOP = 1,NANT
         DO 40 JP = 1,JP2
            DO 30 JF = 1,JF2
               LF1 = JF
               LF2 = JF
               LP1 = JP
               LP2 = JP
               IF (OPCODE.EQ.'ALIF') THEN
                  LF1 = 1
                  LF2 = MIF
               ELSE IF (OPCODE.EQ.'ALST') THEN
                  LP1 = 1
                  LP2 = MPOL
               ELSE IF (OPCODE.EQ.'ALSI') THEN
                  LF1 = 1
                  LF2 = MIF
                  LP1 = 1
                  LP2 = MPOL
                  END IF
               NS = 0
               DO 20 LP = LP1,LP2
                  DO 10 LF = LF1,LF2
                     IF (NPOINT(LOOP,LP,LF).GT.0) NS = NS +
     *                  NPOINT(LOOP,LP,LF)
 10                  CONTINUE
 20               CONTINUE
               IF (NS.GT.0) NLOOP = NLOOP + 1
               NPOINT(LOOP,JP,JF) = NS
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C                                       No baselines
      IF (NLOOP.LE.0) THEN
         IRET = 8
         MSGTXT = 'ANPLOT: NO BASELINES TO BE PLOTTED'
         CALL MSGWRT (8)
C                                       Do plots
      ELSE
         ILOOP = 0
         DO 190 LOOP = 1,NANT
            DO 180 JF = 1,JF2
               DO 170 JP = 1,JP2
                  IF (NPOINT(LOOP,JP,JF).GT.0) THEN
                     LF1 = JF
                     LF2 = JF
                     LP1 = JP
                     LP2 = JP
                     IF (OPCODE.EQ.'ALIF') THEN
                        LF1 = 1
                        LF2 = MIF
                     ELSE IF (OPCODE.EQ.'ALST') THEN
                        LP1 = 1
                        LP2 = MPOL
                     ELSE IF (OPCODE.EQ.'ALSI') THEN
                        LF1 = 1
                        LF2 = MIF
                        LP1 = 1
                        LP2 = MPOL
                        END IF
                     ILOOP = ILOOP + 1
                     IPLOT = MOD (IPLOT, HMXPLT) + 1
                     IF (FLOTEM) THEN
                        XYSCL(2) = YYMAX(LOOP,JP,JF)
                        XYOFF(2) = YYMIN(LOOP,JP,JF)
                        END IF
                     TYPEX = TYPEAX(2)
                     IF (OPCODE.EQ.'ALIF') THEN
                        LF1 = 1
                        LF2 = MIF
                     ELSE IF (OPCODE.EQ.'ALST') THEN
                        LP1 = 1
                        LP2 = MPOL
                     ELSE IF (OPCODE.EQ.'ALSI') THEN
                        LF1 = 1
                        LF2 = MIF
                        LP1 = 1
                        LP2 = MPOL
                        END IF
                     IF (AMPPH) THEN
                        CALL PLTAN (2*IPLOT-1, MAXPLT, LOOP, YVAL, 0,
     *                     MSAMP, MPOL, MIF, LP1, LP2, LF1,LF2, XVAL,
     *                     IANT, IRET)
                        IF (IRET.NE.0) GO TO 999
                        IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
                        TYPEX = TYPEAX(3)
                        TEMP = SCALEM(2)
                        TEMPSC = XYSCL(2)
                        TEMPOF = XYOFF(2)
                        SCALEM(2) = SCALEM(3)
                        XYSCL(2) = XYSCL(3)
                        XYOFF(2) = XYOFF(3)
                        IF (FLOTEM) THEN
                           XYSCL(2) = PHMAX(LOOP,JP,JF)
                           XYOFF(2) = PHMIN(LOOP,JP,JF)
                           END IF
                        CALL PLTAN (2*IPLOT, MAXPLT, LOOP, PHVAL, 2,
     *                     MSAMP, MPOL, MIF, LP1, LP2, LF1, LF2,XVAL,
     *                     IANT, IRET)
                        IF (IRET.NE.0) GO TO 999
                        SCALEM(2) = TEMP
                        XYSCL(2) = TEMPSC
                        XYOFF(2) = TEMPOF
                     ELSE
                        IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
                        CALL PLTAN (IPLOT, MAXPLT, LOOP, YVAL, 0, MSAMP,
     *                     MPOL, MIF, LP1, LP2, LF1, LF2, XVAL, IANT,
     *                     IRET)
                        IF (IRET.NE.0) GO TO 999
                        END IF
                     END IF
 170              CONTINUE
 180           CONTINUE
 190        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLTAN (IPLOT, MAXPLT, THEANT, YYVAL, KBPARM, MSAMP,
     *   MPOL, MIF, LP1, LP2, LF1, LF2, XVAL, IANT, IRET)
C-----------------------------------------------------------------------
C   PLTAN actually plots uv data one panel at a time.
C   Input:
C      IPLOT    I      Plot number on current page. If negative then
C                      this the last plot.
C      MAXPLT   I      The number of plots per page.
C      THEANT   I      The antenna number in this plot
C      YYVAL    R(*)   Array of Y-values
C      KBPARM   I      0, or 2 identifies the fix scale in AMP&PHASE
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, MAXPLT, THEANT, KBPARM, MSAMP, MPOL, MIF,
     *   LP1, LP2, LF1, LF2, IANT(*), IRET
      REAL      XVAL(*), YYVAL(MSAMP,MPOL,MIF)
C
      INCLUDE 'ANBPL.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, CHTMP*18, ADATE*12,
     *   AUNITS(17)*8, CHTYPE(17)*9, STRING*8, SAVPRE(2)*5, TXTMSG*80
      HOLLERITH CATH(256)
      INTEGER   I, BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, FINDPL,
     *   IAPARM(8), INCHAR, INP, J, IAXLAB, IAPLOT, IT(3), ID(3), IAXL1,
     *   IAXL2, NGOOD, NNOFIT, NUMVIS, LTYPE, ISOLIN, J2, ISYM, LF, LP
      REAL      BLC(2), TRC(2), DX, DY, TR, TI, XY(2), CATR(256), SIZE,
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), XZY(2), XMULT(2),
     *   XMIN, XMAX, YMIN, YMAX, XTEMP, BOTT, TOPP, TEMP, COL(3), AX(5),
     *   AY(5), AMULT(2)
      LOGICAL   T, F, GOOD, CATUP, DO3C
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATR, CATH)
      SAVE AMULT, SAVPRE, BUFFER
      DATA LUNPL /26/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'IAT days', 'IAT hrs ', ' ', 'Hours', 'Degrees ',
     *   'Degrees ', 'Degrees', 3*' ', 'Janskys ', 'Janskys ',
     *   'Degrees ', 'Degrees ', 'Janskys ', 'Janskys ', '1/Jy**2'/
      DATA CHTYPE /'Time', 'Time hrs', 'Source', 'Hour angl',
     *   'Elevation', 'Azimuth', 'Para angl', 3*' ',
     *   'Amplitude', 'Compl Amp', 'Phase', 'Phase!amp', 'Real', 'Imag',
     *   'Weight'/
C-----------------------------------------------------------------------
      XSTOK = STOKES
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
      CATUP = T
      ISYM = XSYM + 0.5
      IF ((ISYM.LE.0) .OR. (ISYM.GT.24)) ISYM = 23
      IF (FACTOR.LE.0.005) FACTOR = 1.0
      DO3C = XDO3C.GT.0.0
      CALL CHR2H (4, STOKES, 1, XSTOK)
C                                       User sets the scales
      PLTINC = 1000. / MAXPLT
      DO 10 I = 1,2
         IF (.NOT.SCALEM(I)) THEN
            IF (I.EQ.1) THEN
               XYSCL(I) = INISCL(2)
               XYOFF(I) = INISCL(1)
            ELSE
               XYSCL(I) = INISCL(4 + KBPARM)
               XYOFF(I) = INISCL(3 + KBPARM)
               END IF
            IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
            SIZE = 1000.
            IF (I.EQ.2) SIZE = PLTINC
            XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
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
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.2) LABTYP(LOCNUM) = 7
      AXTYP(LOCNUM) = 0
      IF (ABS(IPLOT).EQ.1) THEN
         TR = PLTINC / XYSCL(2)
         TI = TR
         SAVPRE(2) = ' '
         CALL METSCL (LABEL, TR, SAVPRE(2), GOOD)
         AMULT(2) = TR / TI
         TR = 1000. / XYSCL(1)
         TI = TR
         SAVPRE(1) = ' '
         IF (TYPEAX(1).NE.2) THEN
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
      DO 20 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
         CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
 20      CONTINUE
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS(IPLOT).NE.MAXPLT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
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
         ITYPE = 32
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
         IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *      1.333
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
            WRITE (MSGTXT,1050) VER
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
      WRITE (TXTMSG,1020) THEANT
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
         IF ((OPCODE.EQ.'ALIF') .OR. (OPCODE.EQ.'ALSI') .OR. (MIF.EQ.1))
     *      THEN
            WRITE (STRING,1051) BIF
            TEXT = 'IF ' // STRING(7:8)
            INP = 6
            IF (BIF.NE.EIF) THEN
               WRITE (STRING,1051) EIF
               TEXT(INP:) = ' - ' // STRING(7:8)
               INP = INP + 5
               END IF
            END IF
         WRITE (STRING,1051) BCHAN
         IF (INP.GT.1) THEN
            TEXT(INP:) = ' __CHAN ' // STRING(5:8)
            INP = INP + 3
         ELSE
            TEXT = 'CHAN ' // STRING(5:8)
            END IF
         INP = INP + 9
         IF (ECHAN.NE.BCHAN) THEN
            WRITE (STRING,1051) ECHAN
            TEXT(INP:) = ' - ' // STRING(5:8)
            INP = INP + 7
            END IF
         IF ((OPCODE.EQ.'ALSI') .OR. (OPCODE.EQ.'ALST') .OR.
     *      (MPOL.EQ.1)) THEN
            TEXT(INP:) = ' __STK ' // XSTOK
            INP = INP + 12
            END IF
C
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Plot type and file
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = CHTYPE(TYPEAX(2))
         INP = 10
         IF (AMPPH) THEN
            TEXT(INP:) = ' and ' // CHTYPE(TYPEAX(3))(1:8)
            INP = INP + 14
            END IF
         TEXT(INP:) = ' vs ' // CHTYPE(TYPEAX(1))(1:8) // ' _for_ '
         INP = INP + 20
         CHTMP = NAMEIN // CLAIN
         CALL NAMEST (CHTMP, CATUV(KIIMS), TEXT(INP:), INCHAR)
         INP = INP + 1 + INCHAR
C                                       scalar/vector
         TEXT(INP:) = '_Vect aver.'
         IF (((DTYPE.EQ.11) .AND. (AVGSCA)) .OR. (DTYPE.EQ.17))
     *      TEXT(INP:) = '_Scal aver.'
         INP = INP + 12
C
         IF (DOCAL) THEN
            WRITE (TEXT(INP:),1055) 'CL', CLUSE
            INP = INP + 10
            END IF
         IF ((FGVER.GT.0) .AND. (.NOT. AMPPH))
     *      WRITE (TEXT(INP:),1055) 'FG', FGVER
C
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF (LABEL.GT.1) THEN
            TEXT = ' '
            DY = DY + 1.333
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1060) VER, ADATE, ATIME
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
         ELSE IF ((MAXPLT.GE.6) .AND. (MAXPLT.LE.10)) THEN
            IAXL1 = 3
            IAXL2 = 6
         ELSE IF (MAXPLT.GT.10) THEN
            IAXL1 = 2 * INT(MAXPLT/8) + 1
            IAXL2 = 2 * INT(3*MAXPLT/8) + 2
            END IF
         IF (IAPLOT.NE.IAXL1) THEN
            IF (IAPLOT.EQ.IAXL2) THEN
               CTYP(2,LOCNUM) = AUNITS(TYPEAX(3))
            ELSE
               CPREF(2,LOCNUM) = '-1'
               END IF
            END IF
      ELSE
         IAXLAB = MAXPLT / 2 + 1
         IF (((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *      (IAPLOT.GT.IAXLAB)))) CPREF(2,LOCNUM) = '-1'
         END IF
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = 8.0/SQRT(FLOAT(MAXPLT))
      DX = DX * FACTOR
      IF (DX.LT.2.5) DX = 2.5
      DY = DX
      IF (DX/XYRATO.LT.2.5) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      NUMVIS = 0
      WRITE (TXTMSG,1025) THEANT
      CALL GCOMNT (-1, TXTMSG, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 70 LF = LF1,LF2
         DO 60 LP = LP1,LP2
C                                       COLOR
            IF (XDO3C.GT.0.0) THEN
               TEMP = (LF - 1.0) * MPOL + LP
               TEMP = (TEMP - 1.0) / (MPOL*MIF - 1.0)
               CALL COLOR3 (TEMP, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            DO 50 ISOLIN = 1,NSOLIN
               IF (IANT(ISOLIN).EQ.THEANT) THEN
C                                       found data
                  XZY(1) = XVAL(ISOLIN)
                  J2 = 1
                  XZY(2) = YYVAL(ISOLIN,LP,LF)
                  IF (XZY(2).NE.FBLANK) THEN
                     DO 30 J = 1,2
                        XY(J) = XYSCL(J) * (XZY(J)-XYOFF(J)) + YYOFF(J)
                        IF ((XY(J).LE.XBLC(J)) .OR. (XY(J).GT.XTRC(J)))
     *                     THEN
                           NNOFIT = NNOFIT + 1
                           GO TO 50
                           END IF
 30                     CONTINUE
                     NGOOD = NGOOD + 1
                     AX(1) = XY(1)
                     AY(1) = XY(2)
                     AX(2) = AX(1)
                     AX(3) = AX(1)
                     AX(4) = AX(1) - DX
                     AX(5) = AX(1) + DX
C                                       fix 'up' of the vertical dash
C                                       to max in unaverage points
                     XTEMP = XY(2) + DY
                     IF (XTEMP.GT.XTRC(2)) XTEMP = XTRC(2)
                     IF (XTEMP.LT.XBLC(2)) XTEMP = XBLC(2)
                     AY(2) = XTEMP
                     AY(5) = AY(1)
C                                       fix 'down' of the vertical dash
C                                       to min in unaverage points
                     XTEMP = XY(2) - DY
                     IF (XTEMP.GT.XTRC(2)) XTEMP = XTRC(2)
                     IF (XTEMP.LT.XBLC(2)) XTEMP = XBLC(2)
                     AY(3) = XTEMP
                     AY(4) = AY(1)
                     CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *                  DO3C, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
 50           CONTINUE
 60        CONTINUE
 70     CONTINUE
C                                       Plot label
      IF (KBPARM.EQ.0) THEN
         CALL GLTYPE (1, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         WRITE (TEXT,1105) STANAM(THEANT), THEANT
         INP = 17
         IF ((OPCODE.NE.'ALIF') .AND. (OPCODE.NE.'ALSI') .AND.
     *      (MIF.GT.1)) THEN
            WRITE (STRING,1051) (LF1+BIF-1)
            TEXT(INP:) = '_IF ' // STRING(7:8)
            INP = INP + 7
            END IF
         IF ((OPCODE.NE.'ALSI') .AND. (OPCODE.NE.'ALST') .AND.
     *      (MPOL.GT.1)) THEN
            TEXT(INP:) = ' _STK ' // TSTOK(LP1)
            END IF
         CALL REFRMT (TEXT, '_', INCHAR)
         DX = 1.5
         DY = -1.5
         CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Done: finish plot
      WRITE (MSGTXT,1110) NGOOD, THEANT
      CALL MSGWRT (2)
      WRITE (MSGTXT,1115) NNOFIT, THEANT
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      IF ((IPLOT.LE.0) .OR. (ABS (IPLOT).GE.MAXPLT)) THEN
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
         END IF
      IF (IERR.GT.0) GO TO 975
      IRET = MIN (IERR, 0)
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  MSGTXT = 'PLTAN: ERROR DURING GRAPH FILE CREATION'
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATUV, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'PLTAN: ERROR DURING GRAPHING. WILL TRY TO FINISH ' //
     *   'PARTIAL GRAPH'
      CALL MSGWRT (6)
      WRITE (MSGTXT,1110) NGOOD, THEANT
      CALL MSGWRT (2)
      WRITE (MSGTXT,1115) NNOFIT, THEANT
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
 1020 FORMAT ('Plottinng antenna',I3,' labels')
 1025 FORMAT ('Plottinng antenna',I3,' data')
 1050 FORMAT ('PLot file version',I4,'  created.')
 1051 FORMAT (I8)
 1055 FORMAT ('_',A,' #',I4)
 1060 FORMAT ('PLot file version',I4,'__created ',A,A)
 1105 FORMAT (A8,' ( ',I2,' )')
 1110 FORMAT ('PLTAN: ',I9,' points plotted: antenna ',I2)
 1115 FORMAT ('PLTAN: ',I9,' points did not fit: antenna ',I2)
      END
      SUBROUTINE ANTIN (VER, DISKI, CNOIN, FREQID, IERR)
C-----------------------------------------------------------------------
C   Selects station information and gets freq,and ref. date out of the
C   header for a given baseline.
C   Inputs:
C      VER           I    Antenna array number (AN file ver.)
C      DISKI         I    Vol number
C      CNOIN         I    CNO
C      FREQID        I    Selected FREQID
C   Outputs in common:
C      STANAM(*)     C*8  Names of stations
C      JDREF         D    Julian day# for the referens date.
C      GST0          D    Greenwich sidereal time at UT=0 at ref.d
C      IATUTC        R    IAT-UTC in seconds.
C      UT1XXX        R    UT1-UTC in seconds.
C   Programmer: L.B.Baath   Onsala Space Observatory  30 october 1982
C               L.R. Kogan  NRAO, Socorro added orbital antennas
C-----------------------------------------------------------------------
      INTEGER   IERR, IA, LUNA, CNOIN, IABUF(512), VER, NIF, FREQID,
     *   DISKI, NREC
      INCLUDE 'ANBPL.INC'
      CHARACTER STNAME*8, BNDCOD(MAXIF)*8
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), DX, DY, DZ
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
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GST0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1XXX, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                     check for bad freq value
      CALL JULDAY (RDATE, JDREF)
      DX = ARRAYC(1)
      DY = ARRAYC(2)
      DZ = ARRAYC(3)
      OBSRA = RA
      OBSDEC = DEC
      NREC = IABUF(5)
      CALL CFILL (MAXANT, ' ', STANAM)
C                                     Get antenna info.
      NANT = 0
      DO 30 IA = 1,NREC
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, STNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
C                      changed
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
            END IF
         STANAM(NOSTA) = STNAME
         NANT = MAX (NANT, NOSTA)
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
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTIN: ERROR IN OPEN AN-FILE IERR = ',I6)
 1010 FORMAT ('ANTIN: ERROR IN FINDING STATION',I3,' IERR=',I3)
 1030 FORMAT ('ANTIN: ERROR IN CLOSING AN-FILE IERR = ',I6)
      END
      SUBROUTINE BASAVG (NUMVIS, SCANV, PHAVG, NUSCAN, SCANUM, RPARM,
     *   VIS, AVTIM, MPOL, MIF, CANT, SUMS, IERR)
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
C     NANT         I    Number of antennas to average.
C     AVGSCA       L    If true do ampscalar averaging else vector.
C     DTYPE        I    Type of Y-axis
C     SOLINT       R    Averaging time in days
C     DOAVG        L    If TRUE spectral averaging to be done
C     AMPPH        L    If TRUE both AMP and PHASE plot together
C   Input/Output:
C     RPARM(20)    R    Random parameter array, first record of call.
C                      (1) = 'INDE' => don't use.
C                      (2) if single baseline averaged then RPARM
C                          reflects the data output.
C     VIS(3,*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(MAXANT)R   The result for baselines in ANT1, ANT2
C     PHAVG(MAXANT)R   The result PHASE for AMP&PH
C     NUSCAN       L    True IF the first record in a new scan.
C     AVTIM        R    Average time (days) of output record
C     IERR         I    Return code, 0 => OK,
C                       -1 => end of data found with no data to return
C                       -2 => end of data found with data to return
C                       > 0 => failed.
C   Output to common in D/CSOU.INC
C     SNAME(4)     R    Source name (16 char. 4 / word.)
C     QUAL         I    Source qualifier.
C     CALCOD       R    Calibrator code 4 char.
C     FLUX(4,IF)   R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)    D    Frequency offset (Hz)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMVIS, SCANUM, MPOL, MIF, CANT, IERR
      REAL      SCANV(MAXANT,4,*), PHAVG(MAXANT,4,*), RPARM(*),
     *   VIS(3,*), AVTIM, SUMS(CANT,CANT,MPOL,MIF,4)
      LOGICAL   NUSCAN
C
      INCLUDE 'ANBPL.INC'
      LOGICAL    DONE1, GOTDAT, REQBAS
      INTEGER   I, J, JA1, JA2, SUNUM, JERR, ISLUN, CNTTIM, IDAY,
     *   IVSCNT, LVIS, LIF, CIF, MXBL, PRTLEV, REFANT, MODE, NREF, LP
      REAL       T1, AMP, SQRT, SUMTIM, VISO(3), TFIRST, TLAST, CT, TCT,
     *   RE, IM, WT, FRACT(MAXANT), CREAL(MAXANT), CIMAG(MAXANT),
     *   WORK(MAXANT,MAXANT)
      DOUBLE PRECISION X8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      SAVE TLAST, TFIRST, IVSCNT, REFANT, NREF
      DATA ISLUN /26/,   REFANT /0/
C-----------------------------------------------------------------------
      IF (NUMVIS.EQ.0) THEN
         TLAST = -1.0
         IVSCNT = 0
         END IF
      PRTLEV = 0
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
      IERR = 0
C                                       Clear arrays
 10   MXBL = CANT * CANT * MPOL * MIF * 4
      CALL RFILL (MXBL, 0.0, SUMS)
      CNTTIM = 0
      SUMTIM = 0.0
      AVTIM  = 0.0
C                                       Initialize time
      T1 = 1.0E10
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
C                                       Loop reading data
      LVIS = 0
 100  IF (.NOT.DONE1) THEN
         CALL UVGET ('READ', RPARM, VIS, IERR)
         IVSCNT = IVSCNT + 1
         END IF
      IF (IERR.GT.0) GO TO 999
      IF (IERR.EQ.0) THEN
         DONE1 = .FALSE.
C
         CT = RPARM(ILOCT+1)
C                                       Set up first time boundary
         IF (IVSCNT.EQ.1) THEN
            IF (CT.GE.TBEG) THEN
               TCT = CT
            ELSE
               TCT = TBEG
               END IF
            IDAY = TCT
            X8 = (TCT - IDAY) / SOLINT
            TLAST = IDAY + DINT (X8) * SOLINT + SOLINT
            TFIRST = TLAST - SOLINT
            END IF
C                                       Test time range (TB sort)
         IF (CT.GT.TFIN) GO TO 200
         IF (CT.LT.TBEG) GO TO 100
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (.NOT.REQBAS (JA1, JA2, DESEL, IXANT, NXANT, IXBAS, NXBAS))
     *      GO TO 100
C
         GOTDAT = .TRUE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR. (CT.GT.TLAST))
     *      GO TO 200
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
C                                       Loop over IF
         DO 150 LIF = 1,MIF
            DO 140 LP = 1,MPOL
C                                       Spectral averaging
               CIF = LIF + BIF - 1
               IF (DOAVG) CIF = 0
               CALL AVGCIF (VIS, BCHAN, ECHAN, LP, CIF, BIF, EIF, VISO)
               WT = VISO(3)
C                                       good IF
               IF (WT.GT.0.0) THEN
                  RE = VISO(1)
                  IM = VISO(2)
                  AMP = SQRT (RE*RE + IM*IM)
                  SUMS(JA1,JA2,LP,LIF,1) = SUMS(JA1,JA2,LP,LIF,1) +
     *               RE * WT
                  SUMS(JA2,JA1,LP,LIF,1) = SUMS(JA2,JA1,LP,LIF,1) +
     *               IM * WT
                  SUMS(JA1,JA2,LP,LIF,2) = SUMS(JA1,JA2,LP,LIF,2) +
     *               RE * RE * WT
                  SUMS(JA2,JA1,LP,LIF,2) = SUMS(JA2,JA1,LP,LIF,2) +
     *               IM * IM * WT
                  SUMS(JA1,JA2,LP,LIF,3) = SUMS(JA1,JA2,LP,LIF,3) + WT
                  SUMS(JA2,JA1,LP,LIF,3) = SUMS(JA2,JA1,LP,LIF,3) + WT
                  SUMS(JA1,JA2,LP,LIF,4) = SUMS(JA1,JA2,LP,LIF,4) + 1.0
                  SUMS(JA2,JA1,LP,LIF,4) = SUMS(JA2,JA1,LP,LIF,4) + 1.0
                  END IF
 140           CONTINUE
 150        CONTINUE
         GO TO 100
         END IF
C                                       average what we got
 200  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (GOTDAT) THEN
         IF (IERR.EQ.-1) IERR = -2
C                                       average
         DO 240 LIF = 1,MIF
            DO 230 LP = 1,MPOL
               DO 215 I = 1,CANT
                  DO 210 J = 1,CANT
                     IF (SUMS(I,J,LP,LIF,4).GT.0.0) THEN
                        SUMS(I,J,LP,LIF,1) = SUMS(I,J,LP,LIF,1) /
     *                     SUMS(I,J,LP,LIF,3)
                        SUMS(I,J,LP,LIF,2) = SUMS(I,J,LP,LIF,2) /
     *                     SUMS(I,J,LP,LIF,3)
                        SUMS(I,J,LP,LIF,3) = (SUMS(I,J,LP,LIF,3) /
     *                     SUMS(I,J,LP,LIF,4)) ** 2
                        IF (DTYPE.EQ.11) SUMS(I,J,LP,LIF,1) =
     *                     SUMS(I,J,LP,LIF,1) ** 2
                     ELSE
                        SUMS(I,J,LP,LIF,1) = FBLANK
                        SUMS(I,J,LP,LIF,2) = FBLANK
                        SUMS(I,J,LP,LIF,3) = FBLANK
                        END IF
 210                 CONTINUE
 215              CONTINUE
C                                       solve: ampl scalar antenna based
               IF ((DTYPE.EQ.11) .OR. (DTYPE.EQ.17)) THEN
                  IF ((DTYPE.EQ.11)  .AND. (AVGSCA)) THEN
                     CALL ASOLVE (PRTLEV, 'ampl', TFIRST, TLAST, CANT,
     *                  8.0, SUMS(1,1,LP,LIF,2), SCANV(1,LP,LIF), FRACT)
C                                       solve: ampl antenna based
                  ELSE IF (DTYPE.EQ.11) THEN
                     CALL ASOLVE (PRTLEV, 'ampl', TFIRST, TLAST, CANT,
     *                  8.0, SUMS(1,1,LP,LIF,1), SCANV(1,LP,LIF), FRACT)
C                                       solve: weight
                  ELSE IF (DTYPE.EQ.17) THEN
                     CALL ASOLVE (PRTLEV, 'rms', TFIRST, TLAST, CANT,
     *                  8.0, SUMS(1,1,LP,LIF,3), SCANV(1,LP,LIF), FRACT)
                     END IF
                  DO 220 I = 1,CANT
                     IF (SCANV(I,LP,LIF).LE.0.0) SCANV(I,LP,LIF) =
     *                  FBLANK
 220                 CONTINUE
C                                       solve complex
               ELSE
                  MODE = 0
C                                       solve: phase normalized
                  IF (DTYPE.EQ.14) MODE = 2
                  CALL VSOLVE (SUMS(1,1,LP,LIF,1), SUMS(1,1,LP,LIF,3),
     *               CANT, REFANT, MODE, 3, 3.5, CREAL, CIMAG, NREF,
     *               FRACT, WORK, PRTLEV, JERR)
                  DO 225 I = 1,CANT
                     IF (JERR.NE.0) THEN
                        SCANV(I,LP,LIF) = FBLANK
                     ELSE IF ((CREAL(I).EQ.FBLANK) .OR.
     *                     (CIMAG(I).EQ.FBLANK)) THEN
                        SCANV(I,LP,LIF) = FBLANK
                     ELSE IF ((CREAL(I).EQ.0.0) .AND.
     *                     (CIMAG(I).EQ.0.0)) THEN
                        SCANV(I,LP,LIF) = FBLANK
                     ELSE IF (DTYPE.EQ.12) THEN
                        SCANV(I,LP,LIF) = SQRT (CREAL(I)*CREAL(I) +
     *                     CIMAG(I)*CIMAG(I))
                        PHAVG(I,LP,LIF) = ATAN2 (CIMAG(I),
     *                     CREAL(I)+1.E-20)
                     ELSE IF (DTYPE.EQ.15) THEN
                        SCANV(I,LP,LIF) = CREAL(I)
                     ELSE IF (DTYPE.EQ.16) THEN
                        SCANV(I,LP,LIF) = CIMAG(I)
                     ELSE
                        SCANV(I,LP,LIF) = ATAN2 (CIMAG(I),
     *                     CREAL(I)+1.E-20)
                        END IF
 225                 CONTINUE
                  END IF
 230           CONTINUE
 240        CONTINUE
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
C                                       Set up new time boundary
      IDAY = CT
      X8 = (CT - IDAY) / SOLINT
      TLAST = IDAY + DINT (X8) * SOLINT + SOLINT
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('BASAVG: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('BASAVG: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE XYSC (NUMVIS, XY, NAMPPH, MAXPLT, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         R    plotted parameters .
C      NAMPPH     I    =3 for AMP&PH; =2 elsewhere.
C      MAXPLT     I    Maximum number of plots per page.
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C-----------------------------------------------------------------------
      INTEGER   IRET, MAXPLT, I, JJ, NUMVIS, NAMPPH
      REAL      XY(3), SIZE
      LOGICAL   POSSDG
      INCLUDE 'ANBPL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      POSSDG = (TYPEAX(2).GT.13)
C                                       Are they in requested range?
      IF (NUMVIS.GE.0) THEN
         IRET = -1
         DO 10 I = 1,NAMPPH
            IF (TESTEM(I).NE.0) THEN
               JJ = 2*I - 1
               IF ((INISCL(JJ).LT.INISCL(JJ+1)) .AND. ((XY(I).LT.
     *            INISCL(JJ)) .OR. (XY(I).GT.INISCL(JJ+1)))) GO TO 999
               IF ((INISCL(JJ).GT.INISCL(JJ+1)) .AND. ((XY(I).GT.
     *            INISCL(JJ)) .OR. (XY(I).LT.INISCL(JJ+1)))) GO TO 999
               END IF
 10         CONTINUE
         IRET = 0
C                                       Find max, min from data
         DO 30 I = 1,NAMPPH
            IF (SCALEM(I)) THEN
               IF (XY(I).LT.XYOFF(I)) XYOFF(I) = XY(I)
               IF (XY(I).GT.XYSCL(I)) XYSCL(I) = XY(I)
               END IF
 30         CONTINUE
C                                       Convert to scaling factors
C                                       add 20% for label.
      ELSE
         XYSCL(2) = XYSCL(2) + 0.2 * (XYSCL(2) - XYOFF(2))
         IF (AMPPH) XYSCL(3) = XYSCL(3) + 0.2 * (XYSCL(3) - XYOFF(3))
         DO 130 I = 1,NAMPPH
            IF (SCALEM(I)) THEN
               IF (XYSCL(I).LE.XYOFF(I)) GO TO 980
               IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.1*XYSCL(I)))
     *            XYOFF(I) = 0.0
               IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.3*XYSCL(I))
     *            .AND. ((TYPEAX(I).EQ.11) .OR. (TYPEAX(I).EQ.12)))
     *            XYOFF(I) = 0.0
               SIZE = 0.025 * (XYSCL(I) - XYOFF(I))
               XYSCL(I) = XYSCL(I) + SIZE
               XYOFF(I) = XYOFF(I) - SIZE
               SIZE = 1000.
               IF ((I.EQ.2) .OR. (I.EQ.3)) SIZE = 1000. / MAXPLT
               XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
               END IF
 130        CONTINUE
         END IF
      GO TO 999
C
 980  IF (XDOTV.NE.0.0) THEN
         IRET = 1
         WRITE (MSGTXT,1980) I
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('XYSC: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE AVGCIF (VIS, BCHAN, ECHAN, LP, CIF, BIF, EIF, VISOUT)
C-----------------------------------------------------------------------
C   Routine to average a spectrum or group of IFs in frequency to
C   produce a so-called pseudo-continuum channel.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt)
C      BCHAN    I         Start channel to accept
C      ECHAN    I         Final channel to accept
C      LP       I         the polarization
C      CIF      I         Current IF: 0 -> ALL
C      BIF      I         Start IF to accept
C      EIF      I         Final IF to accept
C   inputs in common:
C      DTYPE    I         Type of Y-axis
C   Output:
C      VISOUT   R(3)      Pseudo-continuum visibility
C-----------------------------------------------------------------------
      INTEGER   BCHAN, ECHAN, LP, CIF, BIF, EIF
      REAL      VIS(*), VISOUT(*)
C
      INCLUDE 'ANBPL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPIF, LOOPF, INDEX, INP, FCHAN, SCHAN, FIF, SIF
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM, AMP, SWT, SRE, SIM,
     *   XN, FI, RE, IM, SUMAMP, SAMP
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      FCHAN = 1
      SCHAN = FCHAN + ECHAN - BCHAN
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
         INDEX = 1 + (LP-1)*INCS + (LOOPIF-BIF)*INCIF
         DO 20 LOOPF = FCHAN,SCHAN
            INP = INDEX + (LOOPF-1)*INCF
            WT = VIS(INP+2)
            IF (WT.LE.0.0) GO TO 20
            RE = VIS(INP)
            IM = VIS(INP+1)
            AMP = SQRT (RE*RE + IM*IM)
            FI = 0.0
            IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) FI = ATAN2 (IM, RE)
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
C                                       normalize sums of phases
C                                       and amplitudes
         XN = 1.0
         IF (SWT.GT.1.0E-10) THEN
            XN = 1.0 / SWT
            RE = SRE * XN
            IM = SIM * XN
            FI = 0.0
            IF ((RE.NE.0.0) .OR. (IM.NE.0.0)) FI = ATAN2 (IM, RE)
            AMP = SQRT (RE*RE + IM*IM)
            IF (AVGSCA) AMP = SAMP * XN
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
      VISOUT(3) = SUMWT
C
 999  RETURN
      END
      SUBROUTINE ANPRCT (MSAMP, MPOL, MIF, XVAL, YVAL, IANT, PHVAL,
     *   IRET)
C-----------------------------------------------------------------------
C   ANPRIN counts the lines to be printed from the data arrays
C   Inputs:
C      MAXPLT   I                   Number plots per page
C      MSAMP    I                   Number samples
C      MPOL     I                   Number polarizations
C      MIF      I                   Number IFs
C      IANT     I(MSAMP)            Antenna values
C      YVAL     R(MSAMP,MPOL,MIF)   Data values
C      PHVA     R(MSAMP,MPOL,MIF)   Phase values when AMPPH is true
C   Return:
C      IRET   I                     Return code, 0 => OK, else abort.
C                                   -1 => skipped printing
C-----------------------------------------------------------------------
      INTEGER   MSAMP, MPOL, MIF, IANT(*), IRET
      REAL      XVAL(*), YVAL(MSAMP,MPOL,MIF), PHVAL(MSAMP,MPOL,MIF)
C
      INCLUDE 'ANBPL.INC'
      INTEGER   NACROS, MANT, LS, LF, NSAMP(MAXANT), IA1, ANUM(MAXANT),
     *   LA, IA2, I, CVAL(30), CNUM, PAGE, IPCNT, JVAL, ITT(4), NDIG,
     *   LP, NCOUNT, TTY(2)
      REAL      DMAX, DMIN, LTIME, DSCALE, TEMP, TROUN
      CHARACTER SCRTCH*132, STR*10
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
      PAGE = 0
      NDIG = XNDIG + 0.1
      NDIG = MAX (3, MIN (8, NDIG))
      NACROS = MAX (72, NCHPRT)
      MANT = (NACROS - 13) / (NDIG+1)
      TROUN = 1.0 - 5.0 * (10.0**(-NDIG-1))
C                                       loop over IF
      LF = 0
 10   LF = LF + 1
      IF (LF.LE.MIF) THEN
         LP = 0
 15      LP = LP + 1
         IF (LP.LE.MPOL) THEN
C                                       find overall max, min
            DMAX = -1.E10
            DMIN = -DMAX
            CALL FILL (MAXANT, 0, NSAMP)
            DO 20 LS = 1,NSOLIN
               IF (YVAL(LS,LP,LF).NE.FBLANK) THEN
                  DMAX = MAX (DMAX, YVAL(LS,LP,LF))
                  DMIN = MIN (DMIN, YVAL(LS,LP,LF))
                  NSAMP(IANT(LS)) = NSAMP(IANT(LS)) + 1
                  END IF
 20            CONTINUE
            IF (DMAX.LE.DMIN) GO TO 15
C                                       scale display
            DMAX = MAX (DMAX, ABS(DMIN))
            TEMP = LOG10 (DMAX/TROUN)
            I = TEMP + 10.0**NDIG - 1.0
            I =  10**NDIG + NDIG - 2 - I
            DSCALE = 10.0 ** I
            IF ((DTYPE.EQ.13) .OR. (DTYPE.EQ.14)) THEN
               IF (DSCALE.GT.1.0) DSCALE = DSCALE / 10.0
               END IF
C                                       which antennas this pass
            IA2 = 0
C                                       loop point for passes
 50         IA1 = IA2 + 1
            CNUM = 0
            IF (IA1.LE.NANT) THEN
               I = 0
               CALL FILL (MAXANT, 0, ANUM)
               DO 60 LA = IA1,NANT
                  IF ((NSAMP(LA).GT.0) .AND. (I.LT.MANT)) THEN
                     I = I + 1
                     ANUM(LA) = I
                     IA2 = LA
                     CVAL(I) = LA
                     END IF
 60               CONTINUE
               CNUM = I
               END IF
C                                       some columns to list
            IF (CNUM.GT.0) THEN
C                                       first page titles
               IPCNT = 998
               IF (DOCRT.GT.-2.5) THEN
                  NCOUNT = NCOUNT + 4
                  END IF
C                                       loop over the data
                  JVAL = 0
                  LTIME = -1.E6
                  DO 80 LS = 1,NSOLIN
C                                       print accumulated time
                     IF (XVAL(LS).GT.LTIME+SOLINT/10.) THEN
                        IF (JVAL.GT.0) THEN
                           NCOUNT = NCOUNT + 1
                           END IF
                        JVAL = 0
                        LTIME = XVAL(LS)
                        CALL TODHMS (XVAL(LS), ITT)
                        END IF
                  IF ((YVAL(LS,LP,LF).NE.FBLANK) .AND. (IANT(LS).GE.IA1)
     *               .AND. (IANT(LS).LE.IA2)) THEN
                     JVAL = JVAL + 1
                     END IF
 80               CONTINUE
               IF (JVAL.GT.0) THEN
                  NCOUNT = NCOUNT + 1
                  END IF
               GO TO 50
               END IF
            GO TO 15
            END IF
         GO TO 10
         END IF
C                                       phase in AMPPH
      IF (AMPPH) THEN
C                                       loop over IF
         LF = 0
 110     LF = LF + 1
         IF (LF.LE.MIF) THEN
            LP = 0
 115        LP = LP + 1
            IF (LP.LE.MPOL) THEN
C                                       find overall max, min
               DMAX = -1.E10
               DMIN = -DMAX
               CALL FILL (MAXANT, 0, NSAMP)
               DO 120 LS = 1,NSOLIN
                  IF (PHVAL(LS,LP,LF).NE.FBLANK) THEN
                     DMAX = MAX (DMAX, PHVAL(LS,LP,LF))
                     DMIN = MIN (DMIN, PHVAL(LS,LP,LF))
                     NSAMP(IANT(LS)) = NSAMP(IANT(LS)) + 1
                     END IF
 120              CONTINUE
               IF (DMAX.LE.DMIN) GO TO 115
C                                       scale display
               DMAX = MAX (DMAX, ABS(DMIN))
               TEMP = LOG10 (DMAX/TROUN)
               I = TEMP + 10.0**NDIG - 1.0
               I =  10**NDIG + NDIG - 2 - I
               DSCALE = 10.0 ** I
               IF (DSCALE.GT.1.0) DSCALE = DSCALE / 10.0
C                                       which antennas this pass
               IA2 = 0
C                                       loop point for passes
 150           IA1 = IA2 + 1
               CNUM = 0
               IF (IA1.LE.NANT) THEN
                  I = 0
                  CALL FILL (MAXANT, 0, ANUM)
                  DO 160 LA = IA1,NANT
                     IF ((NSAMP(LA).GT.0) .AND. (I.LT.MANT)) THEN
                        I = I + 1
                        ANUM(LA) = I
                        IA2 = LA
                        CVAL(I) = LA
                        END IF
 160                 CONTINUE
                  CNUM = I
                  END IF
C                                       some columns to list
               IF (CNUM.GT.0) THEN
C                                       first page titles
                  IPCNT = 998
                  IF (DOCRT.GT.-2.5) THEN
                     NCOUNT = NCOUNT + 4
                     END IF
C                                       loop over the data
                  JVAL = 0
                  LTIME = -1.E6
                  DO 180 LS = 1,NSOLIN
C                                       print accumulated time
                     IF (XVAL(LS).GT.LTIME+SOLINT/10.) THEN
                        IF (JVAL.GT.0) THEN
                           NCOUNT = NCOUNT + 1
                           END IF
                        JVAL = 0
                        LTIME = XVAL(LS)
                        CALL TODHMS (XVAL(LS), ITT)
                        END IF
                     IF ((PHVAL(LS,LP,LF).NE.FBLANK) .AND.
     *                  (IANT(LS).GE.IA1) .AND. (IANT(LS).LE.IA2)) THEN
                        JVAL = JVAL + 1
                        END IF
 180                 CONTINUE
                  IF (JVAL.GT.0) THEN
                     NCOUNT = NCOUNT + 1
                     END IF
                  GO TO 150
                  END IF
               GO TO 115
               END IF
            GO TO 110
            END IF
         END IF
C                                       ask if needed
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) IRET = -1
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
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1950) IRET
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('Requested print job is',I10,' lines long!')
 1950 FORMAT ('PRINTER ERROR',I5)
      END
      SUBROUTINE ANPRIN (MSAMP, MPOL, MIF, XVAL, YVAL, IANT, PHVAL,
     *   IRET)
C-----------------------------------------------------------------------
C   ANPRIN prints the data arrays
C   Inputs:
C      MAXPLT   I                   Number plots per page
C      MSAMP    I                   Number samples
C      MPOL     I                   Number polarizations
C      MIF      I                   Number IFs
C      IANT     I(MSAMP)            Antenna values
C      YVAL     R(MSAMP,MPOL,MIF)   Data values
C      PHVA     R(MSAMP,MPOL,MIF)   Phase values when AMPPH is true
C   Return:
C      IRET   I                     Return code, 0 => OK, else abort.
C-----------------------------------------------------------------------
      INTEGER   MSAMP, MPOL, MIF, IANT(*), IRET
      REAL      XVAL(*), YVAL(MSAMP,MPOL,MIF), PHVAL(MSAMP,MPOL,MIF)
C
      INCLUDE 'ANBPL.INC'
      INTEGER   LUNP, FINDP, NACROS, MANT, LS, LF, NSAMP(MAXANT), IA1,
     *   ANUM(MAXANT), LA, IA2, I, CVAL(30), CNUM, PAGE, IPCNT,
     *   JVAL, ITT(4), IROUND, NDIG, J, LP
      REAL      DMAX, DMIN, LTIME, DSCALE, TEMP, TROUN
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132, STR*10,
     *   CHTYPE(17)*9, TITL3*132
      INCLUDE 'INCS:DDCH.INC'
      DATA CHTYPE /'Time', 'Time hrs', 'Source', 'Hour angl',
     *   'Elevation', 'Azimuth', 'Para angl', 3*' ',
     *   'Amplitude', 'Cmplx Amp', 'Phase', 'Phase!amp', 'Real', 'Imag',
     *   'Weight'/
C-----------------------------------------------------------------------
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
      PAGE = 0
      NDIG = XNDIG + 0.1
      NDIG = MAX (3, MIN (8, NDIG))
      MANT = (NACROS - 13) / (NDIG+1)
      TROUN = 1.0 - 5.0 * (10.0**(-NDIG-1))
C                                       loop over IF
      LF = 0
 10   LF = LF + 1
      IF (LF.LE.MIF) THEN
         LP = 0
 15      LP = LP + 1
         IF (LP.LE.MPOL) THEN
C                                       find overall max, min
            DMAX = -1.E10
            DMIN = -DMAX
            CALL FILL (MAXANT, 0, NSAMP)
            DO 20 LS = 1,NSOLIN
               IF (YVAL(LS,LP,LF).NE.FBLANK) THEN
                  DMAX = MAX (DMAX, YVAL(LS,LP,LF))
                  DMIN = MIN (DMIN, YVAL(LS,LP,LF))
                  NSAMP(IANT(LS)) = NSAMP(IANT(LS)) + 1
                  END IF
 20            CONTINUE
            IF (DMAX.LE.DMIN) GO TO 15
C                                       scale display
            DMAX = MAX (DMAX, ABS(DMIN))
            TEMP = LOG10 (DMAX/TROUN)
            I = TEMP + 10.0**NDIG - 1.0
            I =  10**NDIG + NDIG - 2 - I
            DSCALE = 10.0 ** I
            IF ((DTYPE.EQ.13) .OR. (DTYPE.EQ.14)) THEN
               IF (DSCALE.GT.1.0) DSCALE = DSCALE / 10.0
               END IF
C                                       which antennas this pass
            IA2 = 0
C                                       loop point for passes
 50         IA1 = IA2 + 1
            CNUM = 0
            IF (IA1.LE.NANT) THEN
               I = 0
               CALL FILL (MAXANT, 0, ANUM)
               DO 60 LA = IA1,NANT
                  IF ((NSAMP(LA).GT.0) .AND. (I.LT.MANT)) THEN
                     I = I + 1
                     ANUM(LA) = I
                     IA2 = LA
                     CVAL(I) = LA
                     END IF
 60               CONTINUE
               CNUM = I
               END IF
C                                       some columns to list
            IF (CNUM.GT.0) THEN
C                                       first page titles
               IPCNT = 998
               TITL1 = ' '
               TITL2 = ' '
               TITL3 = 'Time-Antenna'
               DO 65 J = 1,CNUM
                  WRITE (STR,1075) CVAL(J)
                  I = (NDIG+1) * (J - 1) + 13
                  TITL3(I:I+NDIG) = STR(10-NDIG:10)
 65               CONTINUE
               IF (DOCRT.GT.-2.5) THEN
                  WRITE (LINE,1060) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  I = LF + KBIF - 1
                  WRITE (LINE,1061) CHTYPE(DTYPE), TSTOK(LP), I, DSCALE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  LINE = TITL3
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  LINE = ' '
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  END IF
C                                       titles
                  WRITE (TITL1,1065) NAMEIN, CLAIN, SEQIN,
     *               CHTYPE(DTYPE), I, DSCALE
                  TITL2 = TITL3
C                                       loop over the data
                  JVAL = 0
                  LTIME = -1.E6
                  DO 80 LS = 1,NSOLIN
C                                       print accumulated time
                     IF (XVAL(LS).GT.LTIME+SOLINT/10.) THEN
                        IF (JVAL.GT.0) THEN
                           CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                        TITL1, TITL2, LINE, IPCNT, PAGE, SCRTCH,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 950
                           END IF
                        JVAL = 0
                        LTIME = XVAL(LS)
                        CALL TODHMS (XVAL(LS), ITT)
                        WRITE (LINE,1070) ITT
                        END IF
                  IF ((YVAL(LS,LP,LF).NE.FBLANK) .AND. (IANT(LS).GE.IA1)
     *               .AND. (IANT(LS).LE.IA2)) THEN
                     TEMP = YVAL(LS,LP,LF) * DSCALE
                     I = IROUND (TEMP)
                     WRITE (STR,1075) I
                     I = (NDIG+1) * (ANUM(IANT(LS)) - 1) + 13
                     LINE(I:I+NDIG) = STR(10-NDIG:10)
                     JVAL = JVAL + 1
                     END IF
 80               CONTINUE
               IF (JVAL.GT.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  END IF
               GO TO 50
               END IF
            GO TO 15
            END IF
         GO TO 10
         END IF
C                                       phase in AMPPH
      IF (AMPPH) THEN
C                                       loop over IF
         LF = 0
 110     LF = LF + 1
         IF (LF.LE.MIF) THEN
            LP = 0
 115        LP = LP + 1
            IF (LP.LE.MPOL) THEN
C                                       find overall max, min
               DMAX = -1.E10
               DMIN = -DMAX
               CALL FILL (MAXANT, 0, NSAMP)
               DO 120 LS = 1,NSOLIN
                  IF (PHVAL(LS,LP,LF).NE.FBLANK) THEN
                     DMAX = MAX (DMAX, PHVAL(LS,LP,LF))
                     DMIN = MIN (DMIN, PHVAL(LS,LP,LF))
                     NSAMP(IANT(LS)) = NSAMP(IANT(LS)) + 1
                     END IF
 120              CONTINUE
               IF (DMAX.LE.DMIN) GO TO 115
C                                       scale display
               DMAX = MAX (DMAX, ABS(DMIN))
               TEMP = LOG10 (DMAX/TROUN)
               I = TEMP + 10.0**NDIG - 1.0
               I =  10**NDIG + NDIG - 2 - I
               DSCALE = 10.0 ** I
               IF (DSCALE.GT.1.0) DSCALE = DSCALE / 10.0
C                                       which antennas this pass
               IA2 = 0
C                                       loop point for passes
 150           IA1 = IA2 + 1
               CNUM = 0
               IF (IA1.LE.NANT) THEN
                  I = 0
                  CALL FILL (MAXANT, 0, ANUM)
                  DO 160 LA = IA1,NANT
                     IF ((NSAMP(LA).GT.0) .AND. (I.LT.MANT)) THEN
                        I = I + 1
                        ANUM(LA) = I
                        IA2 = LA
                        CVAL(I) = LA
                        END IF
 160                 CONTINUE
                  CNUM = I
                  END IF
C                                       some columns to list
               IF (CNUM.GT.0) THEN
C                                       first page titles
                  IPCNT = 998
                  TITL1 = ' '
                  TITL2 = ' '
                  TITL3 = 'Time-Antenna'
                  DO 165 J = 1,CNUM
                     WRITE (STR,1075) CVAL(J)
                     I = (NDIG+1) * (J - 1) + 13
                     TITL3(I:I+NDIG) = STR(10-NDIG:10)
 165                 CONTINUE
                  IF (DOCRT.GT.-2.5) THEN
                     WRITE (LINE,1060) NAMEIN, CLAIN, SEQIN, DISKIN,
     *                  NLUSER
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 950
                     I = LF + KBIF - 1
                     WRITE (LINE,1061) CHTYPE(13), TSTOK(LP), I, DSCALE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 950
                     LINE = TITL3
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 950
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 950
                     END IF
C                                       titles
                  WRITE (TITL1,1065) NAMEIN, CLAIN, SEQIN, CHTYPE(13),
     *               I, DSCALE
                  TITL2 = TITL3
C                                       loop over the data
                  JVAL = 0
                  LTIME = -1.E6
                  DO 180 LS = 1,NSOLIN
C                                       print accumulated time
                     IF (XVAL(LS).GT.LTIME+SOLINT/10.) THEN
                        IF (JVAL.GT.0) THEN
                           CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                        TITL1, TITL2, LINE, IPCNT, PAGE, SCRTCH,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 950
                           END IF
                        JVAL = 0
                        LTIME = XVAL(LS)
                        CALL TODHMS (XVAL(LS), ITT)
                        WRITE (LINE,1070) ITT
                        END IF
                     IF ((PHVAL(LS,LP,LF).NE.FBLANK) .AND.
     *                  (IANT(LS).GE.IA1) .AND. (IANT(LS).LE.IA2)) THEN
                        TEMP = PHVAL(LS,LP,LF) * DSCALE
                        I = IROUND (TEMP)
                        WRITE (STR,1075) I
                        I = (NDIG+1) * (ANUM(IANT(LS)) - 1) + 13
                        LINE(I:I+NDIG) = STR(10-NDIG:10)
                        JVAL = JVAL + 1
                        END IF
 180                 CONTINUE
                  IF (JVAL.GT.0) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 950
                     END IF
                  GO TO 150
                  END IF
               GO TO 115
               END IF
            GO TO 110
            END IF
         END IF
C
 950  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1950) IRET
         CALL MSGWRT (8)
      ELSE
         IRET = MAX (0, IRET)
         END IF
      CALL LPCLOS (LUNP, FINDP, IPCNT, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING THE PRINTER')
 1060 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1061 FORMAT ('Data are ',A,'  from pol ',A,'  IF',I3,'  multiplied by',
     *   1PE8.1)
 1065 FORMAT (A12,'.',A6,'.',I4,4X,A,'   IF',I3,'   X',1PE12.4)
 1070 FORMAT (I2,'/',2(I2.2,':'),I2.2)
 1075 FORMAT (I10)
 1950 FORMAT ('PRINTER ERROR',I5)
      END

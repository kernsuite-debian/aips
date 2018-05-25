LOCAL INCLUDE 'PVBPLT.INC'
C                                       Parameter include for VBPLT
      INTEGER   MAXCC
C                                       MAXCC = max. no. model
C                                       components.
      PARAMETER (MAXCC = 10000)
LOCAL END
LOCAL INCLUDE 'VBPLT.INC'
C                                       Local include for VBPLT
      INCLUDE 'PVBPLT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XXSTOK,
     *   XXSOUR(4,30), XXCALC, XPPLOT(2), XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, XSTOK*4,
     *   STANAM(MAXANT)*8, XSOUR(30)*16, XCALCO*4, PPLOT*8, OPTYPE*4
      REAL      USERID, XSIN, XDISIN, XS2IN, XDI2IN, XBCHAN, XECHAN,
     *   XBIF, XEIF, XINC, UVRANG(2), XTIME(8), XANT(50), XBASE(50),
     *   YTYPE, SOLINT, BPARM(10), XINV2, XNIT, XSUBA, XDOTV, XGRCH,
     *   XQUAL, XBAND, XFREQ, XFQID, XDOCAL, XGUSE, XDOPOL, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XSYM, XLABEL
      REAL      BUFF1(UVBFSS), XPOS(MAXCC), YPOS(MAXCC), SFLUX(MAXCC),
     *   GAUSA(MAXCC), GAUSB(MAXCC), GAUSC(MAXCC),
     *   ZTERM, IATUTC, UT1XXX
      INTEGER   JBUFSZ, NUMCC, NANT, NSRC, IDN(30), OLDCNO,
     *   CATKEP(256)
      LOGICAL   DOGAUS, DOSPHE, DOTV, MULTI, DOAVG, AMPVEC
      REAL      TBEG, TFIN, XYSCL(2), XYOFF(2), XYVALS(MAXANT,MAXANT),
     *   TIMMIN, TIMMAX, CHOUT(4)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, NPARMS,
     *   TYPEAX(2), NCH, IBIF, VER2, TESTEM(2), POLPLT, IPOLPT,
     *   OKDAT(MAXANT,MAXANT), ANTS(50), BASE(50), ISUB, NBASE,
     *   ANT1(MXBASE), ANT2(MXBASE), GRCHN, TVCHN, TVCORN(4),
     *   CHNSEL(3,10), NUMBAS, ANTLAB(MXBASE,2), DTYPE, LABEL
      LOGICAL   UVREV, SCALEM(2), NOUVR, FLOTEM, DROPEM, DOERRB,
     *   DECICL, INDECL
      DOUBLE PRECISION XB(MAXANT), YB(MAXANT), ZB(MAXANT), JDREF,
     *   AFREQ, GST0, ANLONG, OBSRA, OBSDEC
C                                       WARNING: many of these commons
C                                       are declared locally with
C                                       variables of different names!!!
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XNAM2I, XCLA2I, XS2IN, XDI2IN, XINV2, XNIT, XXSOUR, XQUAL,
     *   XXCALC, XBAND, XFREQ, XFQID, XBCHAN, XECHAN, XBIF, XEIF, XINC,
     *   UVRANG, XTIME, XXSTOK, XANT, XBASE, XDOCAL, XGUSE, XDOPOL,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XSUBA, YTYPE,
     *   SOLINT, XOPTYP, BPARM, XPPLOT, XSYM, XLABEL, XDOTV, XGRCH
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /VBPCOM/ TBEG, TFIN, XYSCL, XYOFF, XYVALS, TIMMIN,
     *   TIMMAX, UVREV, SCALEM, NOUVR, FLOTEM, DROPEM, IAW1, IAW2, INC,
     *   SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH, IBIF, VER2, TESTEM,
     *   OKDAT, POLPLT, IPOLPT, ANTS, BASE, ISUB, NBASE, ANT1, ANT2,
     *   GRCHN, TVCHN, TVCORN, NSRC, IDN, OLDCNO, CATKEP, NUMBAS,
     *   ANTLAB, DTYPE, CHNSEL, DOTV, DOERRB, MULTI, DOAVG, AMPVEC,
     *   DECICL, INDECL, NPARMS, LABEL, CHOUT
      COMMON /CANIN/ XB, YB, ZB, JDREF, AFREQ, GST0, ANLONG,
     *   OBSRA, OBSDEC, IATUTC, UT1XXX, NANT
      COMMON /COMPS/ XPOS, YPOS, SFLUX, GAUSA, GAUSB, GAUSC,
     *   ZTERM, DOGAUS, DOSPHE, NUMCC
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAM2IN, CLA2IN, XSTOK, STANAM,
     *   XSOUR, XCALCO, PPLOT, OPTYPE
LOCAL END
      PROGRAM VBPLT
C-----------------------------------------------------------------------
C! Plots selected uv data and model values.
C# UV Plot-appl VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   VBPLT plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     USERID                       UV data file owner #
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input UV data.
C     IN2NAME        NAM2IN        CLEAN components file name.
C     IN2CLASS       CLA2IN        CLEAN components file class.
C     IN2SEQ         XS2IN         CLEAN components file sequence no.
C     IN2DISK        XDI2IN        CLEAN components file disk no.
C     IN2VERS        XINV2         CLEAN components file version no.
C     NITER          XNIT          Number of Clean Components.
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
C     BPARM......Control parameters:
C        1 = type of X-axis (where  1 = amplitude (Jy), 2 = phase
C           (degrees), 3 = u,v distance (klambda), 4 = u,v p.a.
C           (degrees, clockwise from v-axis), 5 = time (iat days),
C           6 = u, 7 = v, 8 = w (all in klambda), 9 = real part (Jy),
C           10 = imaginary part(Jy), and 11 = folded G.S.T.(hours),
C           12 = time (IAT hms), 13 = source no.,
C           14 = freqid, 15 = int. time, 16 = weight,
C           17 = RMS) 0 => 12
C        2 = type of Y-axis
C        3 = Do not autoscale if non-zero, use following values:
C        4 = Minimum of X-axis,
C        5 = Maximum of X-axis,
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis,
C        8 = Maximum number of plots per page (min 1)
C        9 = Interferometer type (where 0 = VLBI, 1 = VLA,
C            2 = MERLIN, 3 = WSRT)
C       10 > 0  => plot missing baselines (if there's a model)
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, MAXPLT
      INCLUDE 'VBPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'VBPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VBPIN (PRGM, MAXPLT, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       determine scaling
      CALL SCALVB (MAXPLT, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Do plots
      CALL VBPLOT (MAXPLT, IRET)
      IRET = MAX (0, IRET)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE VBPIN (PRGM, MAXPLT, JERR)
C-----------------------------------------------------------------------
C   VBPIN gets input parameters for VBPLT .
C   Inputs:  PRGM   C*6       Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C            MAXPLT I         Maximum number of plots per page.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, STOKTY(8)*4, UTYPE*2, STAT*4
      CHARACTER BLANK*4, AUTO*4, CROS*4
      INTEGER  JERR, MAXPLT, IERR, ITEMP, LIM1, NSTOK,
     *   IUSER, I, VER, L, J, MXANT, LUNA, NUMAN(2000), IROUND,
     *   LUN
      REAL      CATR(256), EPS
      LOGICAL   DOMODL, MATCH, TRYMOD, DESEL
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VBPLT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATR, CATBLK)
      EQUIVALENCE (NUMAN, YPOS)
      DATA MXANT /MAXANT/
      DATA NSTOK, STOKTY /8,'RR  ','LL  ','RL  ','LR  ','I   ','Q   ',
     *   'U   ','V   '/
      DATA BLANK, AUTO, CROS /'    ', 'AUTO', 'CROS'/
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
      NPARMS = 288
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
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Crunch input parameters.
      EPS = 0.1
      IUSER = ABS(USERID) + EPS
      IF (IUSER.EQ.0) IUSER = NLUSER
      IF (IUSER.EQ.32000) IUSER = 0
      IF (XINC.LT.1.0) XINC = 1.0
      INC = XINC + EPS
      SEQIN = XSIN + EPS
      DISKIN = XDISIN + EPS
      ISUB = XSUBA + EPS
      SUBARR = ISUB
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
      IF (XSYM.LT.0.0) XSYM = 0.0
      IF (XSYM.GT.1.0) XSYM = 1.0
C                                       'cros' or 'auto'
      IF ((OPTYPE .EQ. CROS) .OR. (OPTYPE .EQ. BLANK)) THEN
         DOACOR = .FALSE.
         DOXCOR = .TRUE.
      ELSE
         DOACOR = .TRUE.
         DOXCOR = .FALSE.
      END IF
      TRYMOD = ((NAM2IN(1:4).NE.'    ') .OR. (CLA2IN.NE.'      '))
C                                       Timerange
      TBEG = XTIME(1) + (XTIME(2)+(XTIME(3)+XTIME(4)/60.)/60.)/24.
      TFIN = XTIME(5) + (XTIME(6)+(XTIME(7)+XTIME(8)/60.)/60.)/24.
C                                       Test time, UV ranges
      IF (TFIN.LE.TBEG) TFIN = 1.0E10
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.0E10
      NOUVR = ABS (UVRANG(2) - UVRANG(1)) .LT. 0.01
      UVRANG(1) = UVRANG(1) * 1.0E3
      UVRANG(2) = UVRANG(2) * 1.0E3
C                                       Test type of plot
      IF ((BPARM(1).LT.1.0) .OR. (BPARM(1).GT.17.0)) BPARM(1) = 12.0
      IF ((BPARM(2).LT.1.0) .OR. (BPARM(2).GT.17.0)) BPARM(2) = 1.0
      IF ((BPARM(1).NE.3) .AND. ((BPARM(1).LT.6) .OR. (BPARM(1).GT.8)))
     *   GO TO 30
         BPARM(4) = BPARM(4) * 1.0E3
         BPARM(5) = BPARM(5) * 1.0E3
   30 IF ((BPARM(2).NE.3) .AND. ((BPARM(2).LT.6) .OR. (BPARM(2).GT.8)))
     *      GO TO 35
         BPARM(6) = BPARM(6) * 1.0E3
         BPARM(7) = BPARM(7) * 1.0E3
   35 XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
      LIM1 = MXANT - 1
      DO 50 I = 1,LIM1
         L = I+1
         DO 40 J = L,MXANT
            XYVALS(I,J) = XYSCL(2)
            XYVALS(J,I) = XYOFF(2)
            OKDAT(I,J) = 1
            OKDAT(J,I) = 1
   40       CONTINUE
   50    CONTINUE
      DECICL = .FALSE.
C                                       If plotting uv only
C                                       then plot conjugate points
      ITEMP = BPARM(1) * BPARM(2) + 0.1
      UVREV = (ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56)
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      DROPEM = (BPARM(9).LE.0.0)
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
      FLOTEM = (BPARM(3).EQ.0.0) .AND. (TESTEM(2).EQ.0)
C                                       Get CATBLK from file.
      DOERRB = YTYPE.GT.0.0
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      LABEL = IROUND (XLABEL)
      IF ((LABEL.EQ.0) .OR. (ABS(LABEL).GT.10)) LABEL = 3
      IF (LABEL.GT.7) LABEL = 7
      IF ((LABEL.GE.4) .AND. (LABEL.LE.6)) LABEL = 3
      IF (LABEL.LT.-7) LABEL = -7
      IF ((LABEL.LE.-4) .AND. (LABEL.GE.-6)) LABEL = -3
      CALL FILL (4, 0, TVCORN)
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
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
      USERID = IUSER
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
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
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL RCOPY (2, UVRANG, UVRNG)
      DOCAL = XDOCAL.GT.0.0
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      DOWTCL = .TRUE.
      BLVER = IROUND (XBLVER)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BPVER = IROUND (XBPVER)
      DOBAND = IROUND (XDOBND)
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Multi-source
      CALL MULSDB (CATBLK, MULTI)
C                                       Check pre-average time
      SOLINT = SOLINT / 1440.0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
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
      DOAVG = ECHAN.GT.BCHAN
      IF (DOAVG) THEN
         WRITE (MSGTXT,1040) BCHAN, ECHAN
         CALL MSGWRT (6)
         END IF
      CALL FILL (30, 0, CHNSEL)
      CHNSEL(1,1) = 1
      CHNSEL(2,1) = CATBLK(KINAX+JLOCF)
      XBCHAN = BCHAN
      XECHAN = ECHAN
C                                       IF number
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
           IF (EIF.EQ.0) EIF = BIF
         IF (EIF.LT.BIF) EIF = BIF
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF (.NOT.DOAVG) DOAVG = EIF.GT.BIF
      IF (EIF.GT.BIF) THEN
         WRITE (MSGTXT,1050) BIF, EIF
         CALL MSGWRT (6)
         END IF
      XBIF = BIF
      XEIF = EIF
C                                       Save results in input Parms
 90   TYPEAX(1) = BPARM(1) + EPS
      TYPEAX(2) = BPARM(2) + EPS
C                                       scalar avg. for weight
      AMPVEC = .FALSE.
      IF (TYPEAX(2) .EQ. 16) THEN
         AMPVEC = .TRUE.
         DTYPE = 3
         END IF
      IF (TYPEAX(2).EQ.1) DTYPE = 0
      IF (TYPEAX(2).EQ.2) DTYPE = 1
      IF (TYPEAX(2).EQ.17) DTYPE = 2
      DOMODL = (TYPEAX(1).NE.1) .AND. (TYPEAX(1).NE.2) .AND.
     *   (TYPEAX(1).NE.9) .AND. (TYPEAX(1).NE.10) .AND.
     *   ((TYPEAX(2).EQ.1) .OR. (TYPEAX(2).EQ.2) .OR. (TYPEAX(2).EQ.9)
     *   .OR. (TYPEAX(2).EQ.10))
C                                       Block model when can't handle
      IF (DOMODL .AND. MULTI .AND. (NSRC.NE.1)) THEN
         IF (TRYMOD) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1120)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1130)
            CALL MSGWRT (6)
            DOMODL = .FALSE.
            END IF
         END IF
      IF (.NOT.DOMODL) THEN
         DROPEM = .TRUE.
         XNIT = 0.
         NUMCC = 0
         END IF
C                                       Find baselines to plot
      LUNA = 28
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      CALL GETNAN (DISKIN, OLDCNO, CATBLK, LUNA, XPOS, NUMAN, JERR)
      IF (JERR.NE.0) GO TO 999
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, BUFF1, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Fill in list of all antenna
C                                       - baseline pairs and names
      IF (.NOT.DOACOR) THEN
         NBASE = MXBASE
         CALL BASLST (NUMAN, SUBARR, XANT, XBASE, NBASE, ANT1, ANT2)
      ELSE IF (DOACOR) THEN
         NBASE = 0
         DO 100 I = 1,50
            ANT1(I) = IROUND (XANT(I))
            IF (ANT1(I).LT.0) DESEL = .TRUE.
            ANT1(I) = ABS(XANT(I))
            IF (ANT1(I).GT.0) THEN
               NBASE = NBASE + 1
               ANT2(I) = ANT1(I)
               END IF
  100       CONTINUE
         IF (ANT1(1).EQ.0) THEN
            NBASE = NUMAN(SUBARR+1)
            DO 120 I = 1, NBASE
               ANT1(I) = TELNO(I)
               ANT2(I) = ANT1(I)
  120          CONTINUE
            END IF
         END IF
C                                       Maximum number of
C                                       plots per page
      MAXPLT = 3
      IF (BPARM(8).GE.0.95) MAXPLT = BPARM(8) + EPS
      IF ((NBASE.EQ.1) .AND. ((ANT1(1).NE.0) .AND. (ANT2(1).NE.0)))
     *   MAXPLT = 1
      BPARM(8) = MAXPLT
C                                       Get antenna info.
      VER = XSUBA + 0.01
      CALL ANTIN (VER, BCHAN, IBIF, DISKIN, OLDCNO, FRQSEL, JERR)
      IF (JERR.NE.0) GO TO 999
      XSUBA = VER
C                                       Get CLEAN components.
      VER2 = XINV2 + 0.01
      IF (DOMODL) THEN
         IF (MULTI .AND. TRYMOD) THEN
            WRITE (MSGTXT,1100)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1101)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1102)
            CALL MSGWRT (6)
            CALL SOURNU (SOURCS, SELQUA, 1, DISKIN, OLDCNO, NSRC,
     *         BUFF1, IDN, JERR)
            CALL GETSOU (IDN(1), DISKIN, OLDCNO, CATBLK, LUN, JERR)
            RA = RAEPO * RAD2DG
            DEC = DECEPO * RAD2DG
            END IF
         CALL REEDIN (VER2, JERR)
         END IF
      IF (NUMCC.LE.0) DROPEM = .TRUE.
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
 1035 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1040 FORMAT ('Averaging from channel ',I4,'-',I4)
 1050 FORMAT ('Averaging from IF ',I4,'-',I4)
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('Trying to plot indep. cl. phases, but ANTENNAS is ',
     *   'non - zero')
 1090 FORMAT ('Set ANTENNAS adverb array to 0')
 1100 FORMAT ('WARNING: You are attempting to plot a model against')
 1101 FORMAT ('         multi-source data, make sure you select the')
 1102 FORMAT ('         correct source and CC file.')
 1110 FORMAT ('You may be trying to plot a model without specifying')
 1120 FORMAT ('a sourcename - I do not allow that and am switching')
 1130 FORMAT ('that option off.')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE SCALVB (MAXPLT, IRET)
C-----------------------------------------------------------------------
C   SCALVB sends uv points one at a time to XYOFF .
C   IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'VBPLT.INC'
      INTEGER   IRET, MAXPLT, IARR, IARRW, I, NUMTEM,
     *   IA1, IA2, ILAST, MXANT, LOOP
      INTEGER   NUMVIS, XUMVIS, NUMFRQ, IPOL, ITIME(8), SCANUM
      REAL      XY(2), RPARM(20), SCANV(MXBASE), AVTIM, AVU, AVV, AVW,
     *   DMULT
      LOGICAL   GOOD, NOSCAL, NOACHK, KEEP, NUSCAN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
      RPARM(1) = FBLANK
      NOSCAL = (.NOT.SCALEM(1)) .AND. (.NOT.SCALEM(2))
      IRET = 0
      IF ((NOSCAL) .AND. (.NOT.DROPEM)) GO TO 999
      IARRW = ISUB
      IF (DROPEM) THEN
         DO 5 IA1 = 1,MXANT
            DO 5 IA2 = 1,MXANT
               OKDAT(IA1,IA2) = -1
 5          CONTINUE
         END IF
C                                       Check if valid POLTYP
      CALL COPY (256, CATBLK, CATUV)
      CALL POLCHK (PPLOT, POLPLT)
      IF (POLPLT.GT.0) THEN
         IF ((POLPLT.EQ.3) .OR. (POLPLT.EQ.6)) THEN
            STOKES='HALF'
         ELSE
            STOKES='FULL'
            END IF
         END IF
C                                       Set up for time
C                                       averaging
      IF (SOLINT.GT.0.0) THEN
         CALL COLSET (ANT1, ANT2, DISKIN, OLDCNO, NUMBAS, ANTLAB,
     *      IPOL, IRET)
         IF (IRET.GT.1) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       ANTLAB(I,*) for 'auto'
         IF (DOACOR) THEN
            NUMBAS = NBASE
            DO 7 I = 1, MXANT
               IF (I .LE. NUMBAS) THEN
                  ANTLAB(I,1) = ANT1(I)
                  ANTLAB(I,2) = ANT2(I)
                  ANTENS(I) = ANT2(I)
               ELSE
                  ANTLAB(I,1) = 0
                  ANTLAB(I,2) = 0
                  ANTENS(I) = 0
                  END IF
    7          CONTINUE
            END IF
         END IF
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.EQ.-1) GO TO 200
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
 10   NUMVIS = 0
      XUMVIS = 0
C                                       Setup for antenna selection
      IAW1 = 0
      IAW2 = 0
      NOACHK =  (NBASE.EQ.1) .AND. (ANT1(1).EQ.0)
C                                       Loop
 100  CONTINUE
      IF (SOLINT.EQ.0.0) THEN
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF1, IRET)
         IF (IRET.EQ.-1) GO TO 200
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
C                                       Spectral averaging
         IF (DOAVG) THEN
            NUMFRQ = CATBLK(KINAX+JLOCF)
            NUMTEM = CATBLK(KINAX+JLOCS)
            CALL AVGCIF (BUFF1, NUMTEM, NUMFRQ, BCHAN, ECHAN, BIF,
     *         EIF, CHNSEL, KNCS, BUFF1)
            END IF
C                                       Modify VIS as appropriate
C                                       based on POLPLT.
         IF (POLPLT.GT.0) CALL POLVIS (BUFF1, POLPLT, IPOLPT)
C                                       Pick up some points
         IA1 = RPARM(1+ILOCB) / 256.
         IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Get array #
         IARR = (RPARM(1+ILOCB)-IA1*256.0-IA2) * 100.0 + 1.1
         RPARM(1+ILOCT) = RPARM(1+ILOCT) - (IARR-1)*5.
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) GO TO 130
C                                       Test antennas
         IF (NOACHK) GO TO 120
         DO 115 LOOP = 1,NBASE
            IF ((IA1.EQ.ANT1(LOOP)) .AND. ((IA2.EQ.ANT2(LOOP))
     *         .OR. (0.EQ.ANT2(LOOP)))) GO TO 120
 115        CONTINUE
C                                       Not wanted
         GO TO 130
C                                       Is this a valid point ?
 120     CALL WANTED (BUFF1, RPARM, IA1, IA2, GOOD)
C                                       Find scales
         IF ((.NOT.GOOD) .OR. (NOSCAL)) GO TO 130
         CALL FNDXY (BUFF1, RPARM, XY)
         CALL XYSCAL (NUMVIS, XY, MAXPLT, IA1, IA2, IRET)
         IF (IRET.LT.0) GO TO 130
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) IRET
            GO TO 990
            END IF
         XUMVIS = XUMVIS + 1
         OKDAT(IA1,IA2) = 1
         OKDAT(IA2,IA1) = 1
 130     ILAST = I
         IF ((NOSCAL) .AND. (GOOD)) OKDAT(IA1,IA2) = 1
         IF ((NOSCAL) .AND. (GOOD)) OKDAT(IA2,IA1) = 1
 150     CONTINUE
         GO TO 100
         END IF
C
      IF (SOLINT.GT.0.0) THEN
C                                       Read vis. recs and form
C                                       averages
         CALL BASAVG (NUMVIS, NUMBAS, ANTLAB, AMPVEC, DTYPE, SOLINT,
     *      CHNSEL, DOAVG, SCANV, ITIME, NUSCAN, SCANUM, RPARM,
     *      BUFF1, AVTIM, AVU, AVV, AVW, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 4
            GO TO 990
            END IF
C                                       Pick up some points
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Get array #
         IARR = SUBARR
         AVTIM = AVTIM - (IARR-1)*5.
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) GO TO 100
C                                       Find scales
         DMULT = 1.0
         IF (DTYPE.EQ.1) DMULT = RAD2DG
         IF (.NOT.NOSCAL) THEN
            DO 160 I = 1, NUMBAS
               IA1 = ANTLAB(I,1)
               IA2 = ANTLAB(I,2)
C                                       time interval
               IF ((SCANV(I).NE.FBLANK) .AND.
     *              (AVTIM .LE. TFIN) .AND.
     *              (AVTIM .GE. TBEG)) THEN
                  XY(1) = AVTIM
                  XY(2) = SCANV(I) * DMULT
                  CALL XYSCAL (NUMVIS, XY, MAXPLT, IA1, IA2,
     *               IRET)
                  OKDAT(IA1,IA2) = 1
                  OKDAT(IA2,IA1) = 1
                  END IF
 160           CONTINUE
            END IF
         XUMVIS = XUMVIS + 1
         GO TO 100
         END IF
C                                       Any points found
 200  IF (NOSCAL) GO TO 220
         IF (XUMVIS.GT.1) GO TO 210
            IRET = 4
            WRITE (MSGTXT,1200) XUMVIS
            GO TO 990
C                                       Final call to XYSCL.
 210     NUMVIS = -1
         CALL XYSCAL (NUMVIS, XY, MAXPLT, IA1, IA2, IRET)
         IF (IRET.LE.0) GO TO 220
            WRITE (MSGTXT,1110) IRET
            GO TO 990
 220  IRET = 0
      CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      CALL COPY (256, CATKEP, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCALVB: ERROR',I3,' INIT VIS FILE')
 1010 FORMAT ('SCALVB: ERROR',I3,' SETTING UP TIME AVG PARAMETERS')
 1100 FORMAT ('SCALVB: ERROR',I3,' READING VIS FILE')
 1110 FORMAT ('SCALVB: XYSCL ERROR',I3)
 1200 FORMAT ('FOUND',I4,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE VBPLOT (MAXPLT, IRET)
C-----------------------------------------------------------------------
C   VBPLOT plots the data thru calls to PLTVB.  Its function depends
C   on whether all baselines were desired, baselines to one antenna
C   were desired or one baseline was desired.
C   Inputs:
C      MAXPLT    I    The maximum number of plots per page.
C   Outputs:
C      IRET      I     Error code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   MAXPLT, IRET, IA1, IA2, IPLOT, LIM1, LIM2, LOOP, NLOOP,
     *   ILOOP, JJ
      REAL      DMS, DMO
      INCLUDE 'VBPLT.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL      AN1LIS(MXBASE), AN2LIS(MXBASE)
      INTEGER MXANT
      DATA MXANT /MAXANT/
C-----------------------------------------------------------
      IRET = 0
      IPLOT = 0
      DMS = XYSCL(2)
      DMO = XYOFF(2)
C                                        Check type of plots desired.
      IF ((NBASE.GT.1) .AND. (ANT2(1).GT.0)) GO TO 500
      IAW1 = ANT1(1)
      IAW2 = ANT2(1)
      IF (IAW2.GT.0) GO TO 300
      IF (IAW1.GT.0) GO TO 200
C                                        All baselines.
 100  LIM1 = NANT
      NLOOP = 0
      DO 120 IA1 = 1,LIM1
         LIM2 = IA1
         DO 110 IA2 = LIM2,NANT
            IF (OKDAT(IA1,IA2).GT.0) THEN
               NLOOP = NLOOP + 1
               AN1LIS(NLOOP) = IA1
               AN2LIS(NLOOP) = IA2
               END IF
 110        CONTINUE
 120     CONTINUE
      IF (NLOOP.LE.0) GO TO 980
      ILOOP = 0
      DO 140 IA1 = 1,LIM1
         LIM2 = IA1 + 1
         DO 130 IA2 = LIM2,NANT
            IF (OKDAT(IA1,IA2).GT.0) THEN
               ILOOP = ILOOP + 1
               IAW1 = IA1
               IAW2 = IA2
               IPLOT = MOD (IPLOT, MAXPLT) + 1
               IF (IPLOT.EQ.1) THEN
                  JJ = MIN (NLOOP-ILOOP+1, MAXPLT)
                  CALL RFILL (50, 0.0, XANT)
                  CALL RCOPY (JJ, AN1LIS(ILOOP), XANT)
                  CALL RCOPY (JJ, AN2LIS(ILOOP), XBASE)
                  END IF
               IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
               IF (FLOTEM) THEN
                  XYSCL(2) = XYVALS(IA1,IA2)
                  XYOFF(2) = XYVALS(IA2,IA1)
                  END IF
               CALL PLTVB (IPLOT, MAXPLT, DMS, DMO, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 130        CONTINUE
 140     CONTINUE
      GO TO 999
C                                        All baselines to one antenna.
 200  NLOOP = 0
      DO 210 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA1 = MIN (NANT, IA1)
         DO 205 IA2 = 1,NANT
            IF ((IA1.LE.IA2) .AND. (OKDAT(IA1,IA2).GT.0)) THEN
               NLOOP = NLOOP + 1
               AN1LIS(NLOOP) = IA1
               AN2LIS(NLOOP) = IA2
               END IF
 205        CONTINUE
 210     CONTINUE
      IF (NLOOP.LE.0) GO TO 980
      ILOOP = 0
      DO 230 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA1 = MIN (NANT, IA1)
         DO 220 IA2 = 1,NANT
            IF ((IA1.LE.IA2) .AND. (OKDAT(IA1,IA2).GT.0)) THEN
               ILOOP = ILOOP + 1
               IAW1 = MIN (IA1, IA2)
               IAW2 = MAX (IA1, IA2)
               IPLOT = MOD (IPLOT, MAXPLT) + 1
               IF (IPLOT.EQ.1) THEN
                  JJ = MIN (NLOOP-ILOOP+1, MAXPLT)
                  CALL RFILL (50, 0.0, XANT)
                  CALL RCOPY (JJ, AN1LIS(ILOOP), XANT)
                  CALL RCOPY (JJ, AN2LIS(ILOOP), XBASE)
                  END IF
               IF (ILOOP.EQ.NLOOP) IPLOT = -IPLOT
               IF (FLOTEM) THEN
                  XYSCL(2) = XYVALS(IAW1,IAW2)
                  XYOFF(2) = XYVALS(IAW2,IAW1)
                  END IF
               CALL PLTVB (IPLOT, MAXPLT, DMS, DMO, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 220        CONTINUE
 230     CONTINUE
      GO TO 999
C                                        One baseline.
 300  IPLOT = -1
      IA1 = ANT1(1)
      IA2 = ANT2(1)
      IF (OKDAT(IA1,IA2).LE.0) GO TO 980
      IA1 = MIN (IA1, NANT)
      IA2 = MIN (IA2, NANT)
      IAW1 = MIN (IA1, IA2)
      IAW2 = MAX (IA1, IA2)

      XANT(1) = IA1
      XBASE(1) = IA2
      XANT(2) = 0.
      IF (FLOTEM) THEN
C                                       temporaly IAW2 = MXANT
         IF (DOACOR) IAW2 = MXANT
         XYSCL(2) = XYVALS(IAW1,IAW2)
         XYOFF(2) = XYVALS(IAW2,IAW1)
         END IF
      IF (DOACOR)    IAW2 = IAW1
      CALL PLTVB (IPLOT, MAXPLT, DMS, DMO, IRET)
      GO TO 999
C                                       Random baselines
C                                       Find last plot
 500  NLOOP = 0
      DO 520 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA2 = ANT2(LOOP)
         IF (OKDAT(IA1,IA2).GT.0) THEN
            NLOOP = NLOOP + 1
            AN1LIS(NLOOP) = IA1
            AN2LIS(NLOOP) = IA2
            END IF
 520     CONTINUE
      IF (NLOOP.LE.0) GO TO 980
C                                       Do plots
      ILOOP = 0
      DO 540 LOOP = 1,NBASE
         IA1 = ANT1(LOOP)
         IA2 = ANT2(LOOP)
         IF (OKDAT(IA1,IA2).GT.0) THEN
            ILOOP = ILOOP + 1
            IAW1 = IA1
            IAW2 = IA2
            IPLOT = MOD (IPLOT, MAXPLT) + 1
            IF (IPLOT.EQ.1) THEN
               JJ = MIN (NLOOP-ILOOP+1, MAXPLT)
               CALL RFILL (50, 0.0, XANT)
               CALL RCOPY (JJ, AN1LIS(ILOOP), XANT)
               CALL RCOPY (JJ, AN2LIS(ILOOP), XBASE)
               END IF
            IF (ILOOP.GE.NLOOP) IPLOT = -IPLOT
            IF (FLOTEM) THEN
C                                       temporaly IA2=MXANT
               IF (DOACOR) IA2 = MXANT
               XYSCL(2) = XYVALS(IA1,IA2)
               XYOFF(2) = XYVALS(IA2,IA1)
               END IF
            IF (DOACOR) IA2 = IA1
            CALL PLTVB (IPLOT, MAXPLT, DMS, DMO, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 540     CONTINUE
      GO TO 999
C                                       No baselines
 980  IRET = 8
      WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('VBPLOT: NO BASELINES TO BE PLOTTED')
      END
      SUBROUTINE PLTVB (IPLOT, MAXPLT, DMS, DMO, IRET)
C-----------------------------------------------------------------------
C   PLTVB actually plots uv data one panel at a time.
C   Input:
C      IPLOT    I    Plot number on current page. If negative then this
C                   the last plot.
C      MAXPLT   I    The number of plots per page.
C   In/out:           (Changed to user value on .not.scalem(2))
C      DMS      I    Scaling parameter for max y range
C      DMO      I    Low value parm for max y range
C   Output:
C      IRET     I    Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INCLUDE 'VBPLT.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, CHTMP*18, UTYPE*2,
     *   ADATE*12, VERSUS*4, CHFOR*8, ASTS*4, CTEMP*12, STAT*4,
     *   AUNITS(8)*8, CHTYPE(17)*8, CHCOR(8)*4, POLLAB*5
      HOLLERITH CATH(256)
      INTEGER   IRET, IPLOT, MAXPLT, I, IDUM, OBUFF(1024), ITIME(8),
     *   SCANUM, NUMTEM, LTYPE, BUFFER(256), VER, IERR, ITYPE, IPSIZE,
     *   LUNPL, FINDPL, IAPARM(8), INCHAR, INP, LENBU, IA1, IA2, JJJ, J,
     *   CPTRIP(3), IARR, IARRW, IAXLAB, IAPLOT, IT(3), ID(3),
     *   SVAXTP(2), VO, BO, NGOOD, NNOFIT, NUMVIS, IDX1, IDX2, NUMFRQ
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, TI, XY(2), SWT,
     *   CATR(256), SIZE, GWT, GERR, RPARM(20), XTRC(2), XBLC(2),
     *   TLC(2), PLTINC, YYOFF(2), XZY(2), DMS, DMO, XMULT(2), XMIN,
     *   XMAX, YMIN, YMAX, XTEMP, SCANV(MXBASE), AVTIM, AVU, AVV, AVW,
     *   DMULT, SRPARM(20)
      DOUBLE PRECISION    TRA, TDEC, DTORAD
      LOGICAL   T, F, GOOD, CATUP, KEEP, CHSTAT, NUSCAN
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATR, CATH)
      DATA VO, BO, LENBU, LUNPL /0, 1, 16, 26/
      DATA T, F /.TRUE.,.FALSE./
      DATA VERSUS, CHFOR, ASTS /' vs ',' for    ','****'/
      DATA AUNITS /'Janskys ', 'Degrees ',  'Wavlngth',
     *   'Degrees ', 'IAT days', 'GST hrs ','Seconds',
     *   ' '/
      DATA CHTYPE /'Ampltude', 'Phase   ', 'UV dist ',
     *   'UV pa   ', 'Time    ', 'U       ', 'V      ',
     *   'W       ', 'Real    ', 'Imag    ', 'GST     ',
     *   'Time    ', 'Source #', 'Freqid  ', 'Int.time',
     *   'Weight  ', 'Rms     '/
      DATA CHCOR /'RR  ','LL  ','RL  ','LR  ','Ipol','Vpol','Qpol',
     *   'Upol'/
C-----------------------------------------------------------------------
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      CALL RFILL (20, 0.0, RPARM)
      RPARM(1) = FBLANK
      XSTOK = STOKES
      IBIF = BIF
      IF (IERR.EQ.-1) GO TO 200
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      DTORAD = 3.14159265358979326D0 / 180.0D0
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
      CATUP = T
      IARRW = ISUB
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *   UTYPE, IDUM, STAT, OBUFF, IRET)
      IF (STAT.EQ.'READ') THEN
C                                       Change status
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *      UTYPE, IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLRD'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *      UTYPE, IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'WRIT'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       User sets the scales
      PLTINC = 1000. / MAXPLT
      DO 10 I = 1,2
         IF (SCALEM(I)) GO TO 10
            XYSCL(I) = BPARM(3+2*I)
            XYOFF(I) = BPARM(2+2*I)
            IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
            SIZE = 1000.
            IF (I.EQ.2) SIZE = PLTINC
            XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
            IF (I.EQ.1) GO TO 10
               DMS = XYSCL(I)
               DMO = XYOFF(I)
 10      CONTINUE
C                                       Fill in last of actual parms
 20   BPARM(5) = 1000.0 / XYSCL(1) + XYOFF(1)
      BPARM(7) = PLTINC / XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
      SCALEM(1) = T
      SCALEM(2) = T
      IF (FLOTEM) BPARM(6) = 0.0
      IF (FLOTEM) BPARM(7) = 0.0
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.0
      CALL FILL (5, 1, IAPARM)
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       Set up location common
 40   LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.12) LABTYP(LOCNUM) = 7
      IF (TYPEAX(2).EQ.12) LABTYP(LOCNUM) = 70
      CALL COPY (2, TYPEAX, SVAXTP)
      AXTYP(LOCNUM) = 0
      TR = PLTINC / DMS
      TI = TR
      CPREF(2,LOCNUM) = ' '
      IF (TYPEAX(2).NE.12) CALL METSCA (TR, CPREF(2,LOCNUM), GOOD)
      IF (TYPEAX(2).NE.12) XMULT(2) = TR / TI
      IF (TYPEAX(2).EQ.12) XMULT(2) = 360.0
      TR = 1000. / XYSCL(1)
      TI = TR
      CPREF(1,LOCNUM) = ' '
      IF (TYPEAX(1).NE.12) CALL METSCA (TR, CPREF(1,LOCNUM), GOOD)
      IF (TYPEAX(1).NE.12) XMULT(1) = TR / TI
      IF (TYPEAX(1).EQ.12) XMULT(1) = 360.0
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
         IF (TYPEAX(I).GT.12) CTYP(I,LOCNUM) = AUNITS(8)
         IF (TYPEAX(I).EQ.15) CTYP(I,LOCNUM) = AUNITS(7)
         IF (TYPEAX(I).EQ.17) CTYP(I,LOCNUM) = AUNITS(1)
 50      CONTINUE
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.MAXPLT)) THEN
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
         END IF
      CALL CHR2H (8, POLLAB, 1, XPPLOT)
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
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
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 10
         CALL GINIT (DISKIN, OLDCNO, PFILE, IPSIZE, ITYPE, NPARMS,
     *      USERID, DOTV, TVCHN, GRCHN, TVCORN, CATUV, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
         CALL RFILL (4, 0.5, CHOUT)
C                                       Note that TICINC not fully
C                                       initialized as yet. -> INP being
C                                       larger than may be actually
C                                       plotted on this subplot.  This
C                                       is probably desirable.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
         LTYPE = ABS (LABEL)
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 3
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *      1.333
C                                       Init for line drawing.
         IRET = 3
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1010) VER
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
         WRITE (TEXT,1040) BIF, EIF, BCHAN, ECHAN, POLLAB, VER2
         CALL TRIM (TEXT, 132, TEXT, INP)
         INCHAR = INP
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Plot type and file
         TEXT = ' '
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INCHAR = 8
         INP = 1
         TEXT = ' '
         TEXT(INP:INP+7) = CHTYPE(TYPEAX(2))(1:8)
         CALL TRIM (TEXT, INCHAR, TEXT, INP)
         INCHAR = INP + 12
         INP = INP + 1
         TEXT(INP:INP+3) = VERSUS(1:4)
         INP = INP + 4
         TEXT(INP:INP+7) = CHTYPE(TYPEAX(1))(1:8)
         CALL TRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         TEXT(INP:INP+5) = CHFOR(1:5)
         INP = INP + 5
         CHTMP = NAMEIN // CLAIN
         CALL NAMEST (CHTMP, CATUV(KIIMS), TEXT(INP:), INCHAR)
         INP = INP + INCHAR - 1 + 4
         CALL TRIM (TEXT, 132, TEXT, INP)
         INCHAR = INP
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
            WRITE (TEXT,1030) VER, ADATE, ATIME
            INCHAR = 51
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Only label Y axis once.
      IAXLAB = MAXPLT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IPLOT.GT.0) .AND. (IPLOT.NE.IAXLAB)) CPREF(2,LOCNUM) = '-1'
      IF ((IPLOT.LT.0) .AND. (IAPLOT.GT.IAXLAB)) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
      NUMVIS = 0
C                                       Initialize saved copy of
C                                       RPARM and saved weight
      CALL RCOPY (20, RPARM, SRPARM)
      SWT = 0.0
C                                       Loop
 100  CONTINUE
      IF (SOLINT.EQ.0.0) THEN
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF1, IERR)
         IF (IERR.EQ.-1) GO TO 200
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (8)
            IRET = 4
            GO TO 970
            END IF
C                                       Spectral averaging
         IF (DOAVG) THEN
            NUMFRQ = CATBLK(KINAX+JLOCF)
            NUMTEM = CATBLK(KINAX+JLOCS)
            CALL AVGCIF (BUFF1, NUMTEM, NUMFRQ, BCHAN, ECHAN, BIF,
     *         EIF, CHNSEL, KNCS, BUFF1)
            END IF
C                                       Modify VIS as appropriate
C                                       based on POLPLT.
         IF (POLPLT.GT.0) CALL POLVIS (BUFF1, POLPLT, IPOLPT)
C                                       There are points this block
         IA1 = RPARM(1+ILOCB) / 256.
         IA2 = RPARM(1+ILOCB) - IA1*256. + 0.1
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Get array #
         IARR = (RPARM(1+ILOCB)-IA1*256.0-IA2) * 100.0 + 1.1
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) GO TO 130
         RPARM(1+ILOCT) = RPARM(1+ILOCT)-(IARR-1)*5.
C                                       Is this a valid point ?
         CALL WANTED (BUFF1, RPARM, IA1, IA2, GOOD)
C                                       Get and scale X, Y
         IF (.NOT.GOOD) GO TO 130
         CALL FNDXY (BUFF1, RPARM, XZY)
         GWT = BUFF1(3+IPOLPT*INCS)
         GERR = 0.0
         IF (GWT.GT.0.0) GERR = 1.0 / SQRT (GWT)
         IF (BPARM(2).EQ.2.0) THEN
            IDX1 = 1 + IPOLPT*INCS
            IDX2 = 2 + IPOLPT*INCS
            GWT = SQRT (BUFF1(IDX1)*BUFF1(IDX1) +
     *         BUFF1(IDX2)*BUFF1(IDX2))
            IF (GWT.GT.0.0) THEN
               GERR = (GERR / GWT) / DTORAD
            ELSE
               GERR = 0.0
               END IF
            END IF
         GERR = GERR * XYSCL(2)
         END IF
C                                       Time averaging section
      DMULT = 1.0
      IF (DTYPE.EQ.1) DMULT = RAD2DG
      IF (SOLINT.GT.0.0) THEN
C                                       Read vis. recs and form
C                                       averages
         ANTLAB(1,1) = IAW1
         ANTLAB(1,2) = IAW2
         NUMBAS = 1
C                                       Restore RPARM array and
C                                       first weight so that
C                                       BASAVG does not get
C                                       get confused
         CALL RCOPY (20, SRPARM, RPARM)
         BUFF1(3) = SWT
C
         CALL BASAVG (NUMVIS, NUMBAS, ANTLAB, AMPVEC, DTYPE, SOLINT,
     *      CHNSEL, DOAVG, SCANV, ITIME, NUSCAN, SCANUM, RPARM,
     *      BUFF1, AVTIM, AVU, AVV, AVW, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 4
            GO TO 970
            END IF
C
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Get array #
         IA1 = ANTLAB(1,1)
         IA2 = ANTLAB(1,2)
         IARR = (RPARM(1+ILOCB)-IA1*256.0-IA2) * 100.0 + 1.1
C                                       Save a copy of the
C                                       random parameters and
C                                       the first weight for
C                                       BASAVG
         CALL RCOPY (20, RPARM, SRPARM)
         SWT = BUFF1(3)
C                                       Check if desired array
         IF ((IARRW.NE.0) .AND. (IARR.NE.IARRW)) GO TO 130
         RPARM(1+ILOCT) = AVTIM - (IARR-1)*5.
C                                       Is this a valid point ?
         IF (SCANV(1).EQ.FBLANK) BUFF1(3) = 0.0
         RPARM(ILOCU+1) = AVU
         RPARM(ILOCV+1) = AVV
         RPARM(ILOCW+1) = AVW
         CALL WANTED (BUFF1, RPARM, IA1, IA2, GOOD)
C                                       Get and scale X, Y
         IF (.NOT.GOOD) GO TO 130
         XZY(1) = RPARM(ILOCT+1)
         XZY(2) = SCANV(1) * DMULT
         GWT = BUFF1(3+IPOLPT*INCS)
         GERR = 0.0
         IF (GWT.GT.0.0) GERR = 1.0 / SQRT (GWT)
         IF (BPARM(2).EQ.2.0) THEN
            IDX1 = 1 + IPOLPT*INCS
            IDX2 = 2 + IPOLPT*INCS
            GWT = SQRT (BUFF1(IDX1)*BUFF1(IDX1) +
     *         BUFF1(IDX2)*BUFF1(IDX2))
            IF (GWT.GT.0.0) THEN
               GERR = (GERR / GWT) / DTORAD
            ELSE
               GERR = 0.0
               END IF
            END IF
         GERR = GERR * XYSCL(2)
         END IF

      DO 128 JJJ = 1,2
         DO 120 J = 1,2
            XY(J) = XYSCL(J) * (XZY(J)-XYOFF(J)) +
     *         YYOFF(J)
            IF ((XY(J).GE.XBLC(J)) .AND.
     *         (XY(J).LE.XTRC(J))) GO TO 120
            NNOFIT = NNOFIT + 1
            GO TO 127
 120        CONTINUE
         NGOOD = NGOOD + 1
C                                       Mark the point
         IF (DOERRB) THEN
            DY = 5.0
            IF ((GERR*2.0).GT.5.0) DY = GERR / 2.0
            END IF
         IF (XSYM.EQ.0.0) THEN
            XTEMP = XY(1) + DX
            CALL GPOS (XTEMP, XY(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            XTEMP = XY(1) - DX
            CALL GVEC (XTEMP, XY(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         XTEMP = XY(2) + DY
         IF (XTEMP.GT.XTRC(2)) XTEMP = XTRC(2)
         CALL GPOS (XY(1), XTEMP, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         XTEMP = XY(2) - DY
         IF (XTEMP.LT.XBLC(2)) XTEMP = XBLC(2)
         CALL GVEC (XY(1), XTEMP, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Plot u,v,w ?
 127     IF (.NOT.UVREV) GO TO 130
         XZY(1) = -XZY(1)
         XZY(2) = -XZY(2)
 128     CONTINUE
 130  GO TO 100
C                                       Check if changed status
 200  IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *      UTYPE, IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLWR'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *      UTYPE, IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'READ'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Plot model
      TRA = RA
      TDEC = DEC
      IF ((ABS(RA-OBSRA).GT.1.5) .OR. (ABS(DEC-OBSDEC).GT.1.5))
     *   GO TO 205
         TRA = OBSRA
         TDEC = OBSDEC
 205  CALL VBMDL (TRA, TDEC, XMIN, XMAX, YMIN, YMAX, YYOFF, TLC,
     *   TYPEAX(1), TYPEAX(2), CPTRIP, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL COPY (2, SVAXTP, TYPEAX)
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD, IAW1, IAW2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1201) NNOFIT, IAW1, IAW2
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
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATUV, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD, IAW1, IAW2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1201) NNOFIT, IAW1, IAW2
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
 1010 FORMAT ('PLot file version',I4,'  created.')
 1030 FORMAT ('PLot file version',I4,'  created ',A,A)
 1040 FORMAT ('IF ',I1,'-',I1,' CH ',I4,'-',I4,' STK ',A5,' CC ver ',I3)
 1050 FORMAT ('PLTVB: ERROR',I3,' INIT VIS FILE')
 1080 FORMAT ('PLTVB: ERROR ',I3,' CHANGING ',A4,' STATUS')
 1100 FORMAT ('PLTVB: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('PLTVB: ',I9,' points plotted: baseline ',I2,' - ',I2)
 1201 FORMAT ('PLTVB: ',I9,' points did not fit: baseline ',I2,' - ',I2)
 1960 FORMAT ('PLTVB: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTVB: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE FNDXY (BUFR, RPARM, XY)
C-----------------------------------------------------------------------
C   FNDXY extracts the desired X and Y values from the Vis record.
C   Inputs:  BUFR   R(*)     Visibility record
C            RPARM  R(*)     Random parms
C   Outputs: XY     R(2)     X, Y values
C-----------------------------------------------------------------------
      REAL      BUFR(*), XY(2), RPARM(*)
      DOUBLE PRECISION    GMST, GAST
      REAL      TR, TI, R2D
      INTEGER   NV, I, J
      INCLUDE 'VBPLT.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA R2D /57.29578/
C-----------------------------------------------------------------------
C                                       Loop over axes
      DO 900 I = 1,2
         NV = 0
         TR = 0.0
         TI = 0.0
         XY(I) = 0.0
         J = TYPEAX(I)
         GO TO (100, 100, 300, 400, 500, 600, 700, 800, 100, 100,
     *          850, 500, 860, 870, 880, 890), J
C                                       amplitudes and phases
 100     TR = BUFR(1+IPOLPT*INCS)
         TI = BUFR(2+IPOLPT*INCS)
         GO TO 190
C                                       amplitude
 190     IF (J.GT.1) GO TO 200
         XY(I) = SQRT (TR*TR + TI*TI)
         GO TO 900
C                                       phase
 200     IF (J.GT.2) GO TO 250
         IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(I) = R2D *
     *      ATAN2 (TI, TR)
         GO TO 900
C                                       Real , Imag parts
 250     IF (J.EQ.9) XY(I) = TR
         IF (J.EQ.10) XY(I) = TI
         GO TO 900
C                                       U, V distance
 300     XY(I) = SQRT (RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2)
         GO TO 900
C                                       U, V position angle
 400     TR = RPARM(1+ILOCU)
         TI = RPARM(1+ILOCV)
         IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(I) = R2D *
     *      ATAN2 (TI, TR)
         GO TO 900
C                                       Time
 500     XY(I) = RPARM(1+ILOCT)
         GO TO 900
C                                       U projected spacing
 600     XY(I) = RPARM(1+ILOCU)
         GO TO 900
C                                       V projected spacing
 700     XY(I) = RPARM(1+ILOCV)
         GO TO 900
C                                       W projected spacing
 800     XY(I) = RPARM(1+ILOCW)
         GO TO 900
C                                       Folded G.S.T.
 850     CALL SIDERL (JDREF, RPARM(1+ILOCT), IATUTC, GMST, GAST)
         XY(I) = GAST
         GO TO 900
C                                       Source no.
 860     IF (ILOCSU.GT.0) THEN
            XY(I) = RPARM(1+ILOCSU)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       FREQID
 870     IF (ILOCFQ.GT.0) THEN
            XY(I) = RPARM(1+ILOCFQ)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       Int. time
 880     IF (ILOCIT.GT.0) THEN
            XY(I) = RPARM(1+ILOCIT)
         ELSE
            XY(I) = 1.0
            END IF
         GO TO 900
C                                       Weight
 890     XY(I) = BUFR(3+IPOLPT*INCS)
 900     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE WANTED (BUFR, RPARM, IA1, IA2, GOOD)
C-----------------------------------------------------------------------
C   WANTED determines whether the current visibility sample is valid
C   and selected via the selection parameters.
C   Inputs:  BUFR   R(*)     Visibility record
C            RPARM  R(*)     Random parms
C            IA1    I        Ant number 1
C            IA2    I        Ant number 2
C   Outputs: GOOD   L        T => data valid and selected
C-----------------------------------------------------------------------
      REAL      BUFR(*), TEMP, RPARM(*)
      INTEGER   IA1, IA2
      LOGICAL   GOOD
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'VBPLT.INC'
C-----------------------------------------------------------------------
      GOOD = .FALSE.
C                                       Check UV range
      IF (NOUVR) GO TO 10
         TEMP = SQRT (RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2)
         IF ((TEMP.LT.UVRANG(1)) .OR. (TEMP.GT.UVRANG(2))) GO TO 999
C                                       Test time range
 10   TEMP = RPARM(1+ILOCT)
      IF ((TEMP.LT.TBEG) .OR. (TEMP.GT.TFIN)) GO TO 999
C                                       Test antennas
      IF ((IAW1.NE.0) .AND. (IAW1.NE.IA1) .AND. (IAW1.NE.IA2)) GO TO 999
      IF ((IAW2.NE.0) .AND. (IAW2.NE.IA1) .AND. (IAW2.NE.IA2)) GO TO 999
C                                       Are data flagged?
      GOOD = BUFR(3+IPOLPT*INCS).GT.0.0
C
 999  RETURN
      END
      SUBROUTINE XYSCAL (NUMVIS, XY, MAXPLT, IA1, IA2, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         R    plotted parameters .
C      MAXPLT     I    Maximum number of plots per page.
C      IA1, IA2   I82  Antenna numbers
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C
C   Programmer T. J. Cornwell, Oct.  1981.
C-----------------------------------------------------------------------
      INTEGER   IRET, MAXPLT, I, JJ, IA1, IA2, LA1, LA2, L, J,
     *   MXANT, LIM1, NUMVIS
      REAL      XY(2), SIZE
      LOGICAL   POSSDG
      INCLUDE 'VBPLT.INC'
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
      IRET = 0
      POSSDG = (TYPEAX(2).GT.12)
C                                       Are they in requested range?
      IF (NUMVIS.LT.0) GO TO 100
         LA1 = MIN (IA1, IA2)
         LA2 = MAX (IA1, IA2)
C                                       for 'auto' data is at the
C                                       last column and last row
         IF (IA1 .EQ. IA2) THEN
            LA1 = IA1
            LA2 = MXANT
            END IF
         IRET = -1
         DO 10 I = 1,2
            IF (TESTEM(I).EQ.0) GO TO 10
               JJ = 2*I + 2
               IF ((BPARM(JJ).LT.BPARM(JJ+1)) .AND. ((XY(I).LT.
     *            BPARM(JJ)) .OR. (XY(I).GT.BPARM(JJ+1)))) GO TO 999
               IF ((BPARM(JJ).GT.BPARM(JJ+1)) .AND. ((XY(I).GT.
     *            BPARM(JJ)) .OR. (XY(I).LT.BPARM(JJ+1)))) GO TO 999
 10         CONTINUE
         IRET = 0
C                                       Find max, min from data
         DO 30 I = 1,2
            IF (.NOT.SCALEM(I)) GO TO 30
               IF (XY(I).LT.XYOFF(I)) XYOFF(I) = XY(I)
               IF (XY(I).GT.XYSCL(I)) XYSCL(I) = XY(I)
 30         CONTINUE
         IF (.NOT.FLOTEM) GO TO 999
            IF (XY(2).LT.XYVALS(LA2,LA1)) XYVALS(LA2,LA1) = XY(2)
            IF (XY(2).GT.XYVALS(LA1,LA2)) XYVALS(LA1,LA2) = XY(2)
            GO TO 999
C                                       Convert to scaling factors
C                                       add 20% for label.
 100  IF (.NOT.FLOTEM) GO TO 120
         LIM1 = MXANT - 1
         IF (POSSDG) THEN
            XYSCL(2) = XYSCL(2) + (XYSCL(2) * 0.05)
            XYOFF(2) = XYOFF(2) - (XYOFF(2) * 0.05)
            END IF
         DO 115 I = 1,LIM1
            L = I + 1
            DO 110 J = L,MXANT
               IF (XYVALS(J,I).LT.XYVALS(I,J)) GO TO 105
                  XYVALS(J,I) = XYOFF(2)
                  XYVALS(I,J) = XYSCL(2)
 105           XYVALS(I,J) = XYVALS(I,J) + 0.2 *
     *            (XYVALS(I,J) - XYVALS(J,I))
 110           CONTINUE
 115        CONTINUE
 120  XYSCL(2) = XYSCL(2) + 0.2 * (XYSCL(2) - XYOFF(2))
      DO 130 I = 1,2
         IF (.NOT.SCALEM(I)) GO TO 130
            IF (XYSCL(I).LE.XYOFF(I)) GO TO 980
C                                       Deal with U,V,W axes
            IF (.NOT.UVREV) GO TO 125
               XYSCL(I) = MAX (ABS(XYSCL(I)), ABS(XYOFF(I)))
               XYOFF(I) = -XYSCL(I)
 125        IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.1*XYSCL(I)))
     *         XYOFF(I) = 0.0
            IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.3*XYSCL(I)) .AND.
     *         (TYPEAX(I).EQ.1)) XYOFF(I) = 0.0
            SIZE = 0.025 * (XYSCL(I) - XYOFF(I))
            XYSCL(I) = XYSCL(I) + SIZE
            XYOFF(I) = XYOFF(I) - SIZE
            SIZE = 1000.
            IF (I.EQ.2) SIZE = 1000. / MAXPLT
            XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
 130     CONTINUE
      IF (.NOT.FLOTEM) GO TO 999
         LIM1 = MXANT - 1
         DO 150 I = 1,LIM1
            L = I + 1
            DO 140 J = L,MXANT
               IF (XYVALS(I,J).LE.XYVALS(J,I)) GO TO 980
C                                       Deal with U,V,W axes
               IF (.NOT.UVREV) GO TO 135
                  XYVALS(I,J) = MAX (ABS(XYVALS(I,J)),
     *               ABS(XYVALS(J,I)))
                  XYVALS(J,I) = -XYVALS(I,J)
 135           IF ((XYVALS(J,I).GT.0.0) .AND. (XYVALS(J,I).LT.
     *            0.1*XYVALS(I,J))) XYVALS(J,I) = 0.0
               IF ((XYVALS(J,I).GT.0.0) .AND. (TYPEAX(2).EQ.1) .AND.
     *            (XYVALS(J,I).LT.0.3*XYVALS(I,J))) XYVALS(J,I) = 0.0
               SIZE = 0.025 * (XYVALS(I,J) - XYVALS(J,I))
               XYVALS(I,J) = XYVALS(I,J) + SIZE
               XYVALS(J,I) = XYVALS(J,I) - SIZE
               SIZE = 1000. / MAXPLT
               XYVALS(I,J) = SIZE / (XYVALS(I,J) - XYVALS(J,I))
 140           CONTINUE
 150        CONTINUE
         GO TO 999
C
 980  IRET = 1
      WRITE (MSGTXT,1980) I
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('XYSCAL: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE MODSET (XMIN, XMAX, MPLO, NPLO, DRA, DDEC, CPTRIP,
     *   XDATA, YDATA, IERR)
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
C          NPLO      I     Type of Y-axis in the same manner
C          DRA,DDEC  D     Ra and Dec of the source at the epoch of
C                          observation (in degrees, and as D).
C          CPTRIP(3) I     if plotting closure phases, defines the
C                          3 antennas of the triplet
C   Input from common /COMPS/
C      XPOS(MAXCC)    R     Distance from ref. X-pixel in turns.
C      YPOS(MAXCC)    R     Distance from ref. Y-pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      ZTERM         R     Z portion of map center shift.
C      NUMCC         I     Number of pixels actually processed.
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      DOUBLE PRECISION    U(3), V(3), W(3), PI, TWOPI, DTR, HTR, FREP,
     *   BX(3), BY(3), BZ(3), B1(3), B2(3), GH(3), GMST, GAST, RA,
     *   DEC, DRA, DDEC, GSEC, AA, RATE
      REAL      XXPOS, PA, XDEL, XREL, HAI, BL, CSUM, SSUM, TEMP, AMODL,
     *   FAZ, ARG, XMIN, XMAX, XDATA(*), YDATA(2,*), FTEMP
      INTEGER   MPLO, NPLO, I, K, J, IERR, NUV, CPTRIP(3), IT1, IT2,
     *   IT3
      CHARACTER OPCODE*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'VBPLT.INC'
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
      IT1 = IAW1
      IT2 = IAW2
      IT3 = 1
C                                     divide X-axis into 200 pixels
      XDEL = (XMAX - XMIN) / 200.0
      XXPOS = XMIN - XDEL
C                                     if this is a IAT-plot:
C                                     compute GST at first time
C                                     interval
C      IF ((MPLO.EQ.5) .OR. (MPLO.EQ.11) .OR. (MPLO.EQ.12))
C     *   CALL SIDERL (JDREF, XMIN, IATUTC, GMST, GAST)
      IF ((MPLO.EQ.5) .OR. (MPLO.EQ.11) .OR. (MPLO.EQ.12)) THEN
         CALL GSTROT (JDREF, GMST, GAST, RATE)
         GAST = GAST * (24.D0/360.D0)
         GAST = GAST + ((XMIN*24.D0) * GSEC)
         GAST = MOD (GAST, 24.D0)
         END IF
C                                     determine baseline parms
      CALL BASLIN (XB(IT1), YB(IT1), ZB(IT1), XB(IT2), YB(IT2),
     *   ZB(IT2), XB(IT3), YB(IT3), ZB(IT3), OPCODE, FREP,
     *   ANLONG, BPARM(10), BX, BY, BZ, B1, B2, GH)
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
     *      210, 210, 210), MPLO
C                                       IAT in days
  15        NUV = 1
            GAST = GAST + GSEC*XDEL
            GO TO 60
C                                       GST
 30         NUV = 1
            GAST = XDATA(I)
C                                       u,v,w from HA
 60         HAI = GAST*HTR - RA - GH(1)
            U(1) = B2(1) * SIN(HAI)
            V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
            W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
            GO TO 120
C                                       Baseline length
 70         BL = XDATA(I)
            FTEMP = B2(1) * COS(DEC)
            IF (FTEMP.EQ.0.0) GO TO 200
            TEMP =  B1(1)*B1(1) + B2(1)*B2(1) - BL*BL
            IF (TEMP.LT.0.0) GO TO 200
            TEMP = SQRT(TEMP)
            HAI = (-B1(1) * SIN(DEC) + TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 75
               HAI = ACOS(HAI)
               U(1) = B2(1) * SIN(HAI)
               V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
               HAI = SQRT (U(1)*U(1) + V(1)*V(1))
               IF (ABS(HAI-BL).LT.1.E-4*BL) GO TO 78
 75         HAI = (-B1(1) * SIN(DEC) - TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 200
               HAI = ACOS(HAI)
               U(1) = B2(1) * SIN(HAI)
               V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
               HAI = SQRT (U(1)*U(1) + V(1)*V(1))
               IF (ABS(HAI-BL).GT.1.E-3*BL) GO TO 200
 78         W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
            U(2) = -U(1)
            V(2) = V(1)
            W(2) = W(1)
            NUV = 2
            GO TO 120
C                                       Baseline position angle
 80         PA = XDATA(I) * DTR
            FTEMP = B2(1) * (1.0 - (COS(DEC) * COS(PA)) ** 2)
            IF (FTEMP.EQ.0.0) GO TO 200
            TEMP = B2(1)*B2(1) - (B1(1)*B1(1) + B2(1)*B2(1))
     *         * ((COS(DEC)*COS(PA))**2)
            IF (SIN(DEC)*COS(PA).EQ.0.0) TEMP = 0.0
            IF (TEMP.LT.0.) GO TO 200
            TEMP = SIN(DEC) * COS(PA) * SQRT (TEMP)
            NUV = 1
            HAI = (B1(1) * COS(DEC) * COS(PA) * SIN(PA) + TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 85
               HAI = ASIN (HAI)
               U(1) = B2(1) * SIN(HAI)
               V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
               W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
               BL = ATAN2 (V(1), U(1)) / DTR
               IF (ABS(XDATA(I)-BL).LT.0.25) GO TO 120
 85         HAI = (B1(1) * COS(DEC) * COS(PA) * SIN(PA) - TEMP) / FTEMP
            IF ((HAI.LT.-1.0) .OR. (HAI.GT.1.0)) GO TO 200
               HAI = ASIN (HAI)
               U(1) = B2(1) * SIN(HAI)
               V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
               W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
               BL = ATAN2 (V(1), U(1)) / DTR
               IF (ABS(XDATA(I)-BL).LT.0.5) GO TO 120
               GO TO 200
C                                       Along U-axis
 100        U(1) = XDATA(I)
            U(2) = XDATA(I)
            IF (B2(1).EQ.0.0D0) GO TO 200
            TEMP = XDATA(I) / B2(1)
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ASIN (TEMP)
            V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
            W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
            HAI = PI - HAI
            V(2) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
            W(2) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
            NUV = 2
            GO TO 120
C                                       Along V-axis
 110        V(1) = XDATA(I)
            V(2) = XDATA(I)
            TEMP = B2(1) * SIN(DEC)
            IF (TEMP.EQ.0.0) GO TO 200
            TEMP = (B1(1) * COS(DEC) - V(1)) / TEMP
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ACOS (TEMP)
            U(1) = B2(1) * SIN(HAI)
            W(1) = B1(1) * SIN(DEC) + B2(1) * COS(DEC) * COS(HAI)
            U(2) = -U(1)
            W(2) = W(1)
            NUV = 2
            GO TO 120
C                                       Along W-axis
 115        W(1) = XDATA(I)
            W(2) = XDATA(I)
            TEMP = B2(1) * COS(DEC)
            IF (TEMP.EQ.0.0) GO TO 200
            TEMP = (W(1) - B1(1) * SIN(DEC)) / TEMP
            IF ((TEMP.LT.-1.0) .OR. (TEMP.GT.1.0)) GO TO 200
            HAI = ACOS (TEMP)
            U(1) = B2(1) * SIN(HAI)
            V(1) = B1(1) * COS(DEC) - B2(1) * SIN(DEC) * COS(HAI)
            U(2) = -U(1)
            V(2) = V(1)
            NUV = 2
            GO TO 120
C                                       Compute model values
 120     DO 190 J = 1,NUV
            CSUM = 0.0
            SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Point and gaussian.
            IF (DOSPHE) GO TO 131
            IF (DOGAUS) GO TO 126
               DO 125 K = 1,NUMCC
                  FTEMP = SFLUX(K)
                  TEMP = U(J) * XPOS(K) + V(J) * YPOS(K) + W(J) * ZTERM
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 125              CONTINUE
               GO TO 138
C                                       Gaussian
 126     CONTINUE
               DO 130 K = 1,NUMCC
                  ARG = U(J)*U(J)*GAUSA(K) + U(J)*V(J)*GAUSB(K)
     *               + V(J)*V(J)*GAUSC(K)
                  IF (ARG.LE.-8.0) GO TO 130
                     FTEMP = SFLUX(K) * EXP(ARG)
                     TEMP = U(J)*XPOS(K) + V(J)*YPOS(K) + W(J)*ZTERM
                     SSUM = SSUM + FTEMP * SIN (TEMP)
                     CSUM = CSUM + FTEMP * COS (TEMP)
 130              CONTINUE
               GO TO 138
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
 131        CONTINUE
               DO 135 K = 1,NUMCC
                  AA = GAUSA(K) * SQRT (U(J)*U(J) + V(J)*V(J))
                  IF (AA.LT.6.28D-2) AA = 6.28D-2
                  FTEMP = 3.0D0 * SFLUX(K) *
     *               ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
                  TEMP = U(J) * XPOS(K) + V(J) * YPOS(K) + W(J) * ZTERM
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 135           CONTINUE
C                                       Finished computing model
 138        AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
            IF (NPLO.EQ.1) YDATA(J,I) = AMODL
            IF (NPLO.EQ.9) YDATA(J,I) = CSUM
            IF (NPLO.EQ.10) YDATA(J,I) = SSUM
            IF (NPLO.NE.2) GO TO 190
               IF (AMODL.LE.1.E-10) GO TO 190
               FAZ = ATAN2 (SSUM, CSUM) / DTR
 140           IF (FAZ.GT.180.0E0) FAZ = FAZ - 360.0E0
                  IF (FAZ.LT.-180.0E0) FAZ = FAZ + 360.0E0
                  IF (FAZ.GT.180.0E0) GO TO 140
                  IF (FAZ.LT.-180.0E0) GO TO 140
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
      SUBROUTINE VBMDL (RA, DEC, XMIN, XMAX, YMIN, YMAX, YYOFF, TLC,
     *   MPLO, NPLO, CPTRIP, BUFFER, IERR)
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
C      CPTRIP(3)         I     Closure phase triplet plotted
C      IERR              I     Standard AIPS error code.
C-----------------------------------------------------------------------
      CHARACTER TEXT*132, MINUS*4
      INTEGER   MPLO, NPLO, IERR, BUFFER(1), INP, INCHAR, CPTRIP(3)
      LOGICAL   DOMDL
      REAL      XMIN, XMAX, YMIN, YMAX, YYOFF(2), TLC(2)
      DOUBLE PRECISION    RA, DEC
      REAL      XDATA(200), YDATA(2,200), XXXX(10), DX, DY
      INCLUDE 'VBPLT.INC'
      COMMON /CPLOTU/ XXXX
      DATA MINUS /'----'/
C-----------------------------------------------------------------------
C                                     Write baseline name on plot.
      TEXT = ' '
      INCHAR = 8
      INP = 1
      TEXT(INP:INP+7) = STANAM(IAW1)(1:8)
      CALL TRIM (TEXT, INCHAR, TEXT, INP)
      INCHAR = INP + 9
      INP = INP + 1
      TEXT(INP:INP) = MINUS(1:1)
      INP = INP + 1
      TEXT(INP:INP+7) = STANAM(IAW2)(1:8)
      CALL TRIM (TEXT, INCHAR, TEXT, INP)
      DX = 1.5
      DY = -1.5
      CALL GPOS (TLC(1), TLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                     Check if model exists.
      DOMDL = (NPLO.EQ.1) .OR. (NPLO.EQ.2) .OR. (NPLO.EQ.9) .OR.
     *   (NPLO.EQ.10)
      IF ((NUMCC.LE.0) .OR. (.NOT.DOMDL)) GO TO 999
C                                     setup model
         CALL MODSET (XMIN, XMAX, MPLO, NPLO, RA, DEC, CPTRIP,
     *      XDATA, YDATA, IERR)
         IF (IERR.NE.0) GO TO 999
C                                     setup plotting common
         CALL PLTSET (XMIN, XMAX, YMIN, YMAX, XYSCL, XYOFF, YYOFF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                     plot model
         CALL MODPLT (MPLO, NPLO, XDATA, YDATA, BUFFER, IERR)
C
 999  RETURN
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
C      XA,YA,ZA(*)   D    Cartesian coordinates for stations in meters.
C      STANAM(*)     C*8  Names of stations
C      JDREF         D    Julian day# for the referens date.
C      AFREQ         D    Frequency of channel NNCH, IF IIBIF in Hz.
C      GST0          D    Greenwich sidereal time at UT=0 at ref.d
C      IATUTC        R    IAT-UTC in seconds.
C      UT1XXX        R    UT1-UTC in seconds.
C   Programmer: L.B.Baath   Onsala Space Observatory  30 october 1982
C-----------------------------------------------------------------------
      INTEGER   IERR, IA, LUNA, CNOIN, IABUF(512), VER, NIF, FREQID,
     *   NNCH, IIBIF, DISKI
      INCLUDE 'VBPLT.INC'
      CHARACTER STNAME*8, CHATCA*8
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), DX, DY, DZ
      LOGICAL DOVLBI, DOATCA
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CHATCA /'ATCA    '/
C-----------------------------------------------------------------------
C                                     open antenna file
      LUNA = 28
      CALL ANTINI ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GST0, DEGPDY, AFREQ, RDATE,
     *   POLRXY, UT1XXX, IATUTC, TIMSYS, ANAME, NUMORB, NOPCAL, ANFQID,
     *   IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
C                                     check for bad freq value
 10   IF ( AFREQ .EQ. 0.0D0 ) THEN
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
      IF ((DX.NE.0.0D0) .OR. (DY.NE.0.0D0)) ANLONG = ATAN2 (DY,DX)
C                                     Check if ATCA, which uses
C                                     earth-centre as array ref. pos.
C                                     but right handed coord. system
C                                     (VLBI uses lefthanded: Y=>-Y)
      DOATCA = ANAME .EQ. CHATCA
      DOVLBI = (ABS(DX).LE.1000.0D0) .AND. (ABS (DY).LE.1000.0D0)
     *    .AND. (ABS (DZ).LE.1000.0D0) .AND. (.NOT.DOATCA)
C                                     Get antenna info.
      DO 30 IA = 1,NANT
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV,
     *      STNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.EQ.0) GO TO 20
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
 20      XB(NOSTA) = STAXYZ(1) + DX
         IF (DOVLBI) THEN
            YB(NOSTA) = STAXYZ(2) + DY
            ELSE
            YB(NOSTA) = -(STAXYZ(2) + DY)
            END IF
         ZB(NOSTA) = STAXYZ(3) + DZ
         STANAM(NOSTA) = STNAME
 30      CONTINUE
C                                     close antenna file
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
C                                       Get frequency info for correct
C                                       FREQID/IF combination from
C                                       FQ or CH table
 40   VER = 1
      CALL CHNDAT ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   NIF, FOFF, ISBAND, FINC, FREQID, IERR)
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
C      XPOS(MAXCC)    R     Distance from ref. X-pixel in turns.
C      YPOS(MAXCC)    R     Distance from ref. Y-pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      ZTERM         R     Z portion of map center shift.
C      NUMCC         I     Number of pixels actually processed.
C-----------------------------------------------------------------------
      CHARACTER CHLL*8, CHRA*8, CHMM*8, CHDEC*8, KEYS(7)*8, UTYPE*2
      HOLLERITH CATCLH(256)
      INTEGER   NITER, LIMIT, I, NCOUNT
      INTEGER   IERR, IABUF(512), CNO,
     *   DISK2, SEQ2, IUSER, LUNA, LUNB, VER, NREC,
     *   NKEY, KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, BMAKOL, BMIKOL,
     *   BPAKOL, IBUFF1(1024), NKOL,
     *   INDA, CATCLN(256), ICLL, ICMM, IPOINT
      REAL      ABUF(256), PI, CONST, CATCLR(256),
     *   CCROT, SSROT, XXROT, YYROT,  XPOFF, YPOFF,
     *   EPS, RECORD(20), XTEMPC, TA, TB, AM, AN, MAPROT
      DOUBLE PRECISION    CATCLD(128), CLNRA, CLNDEC, RAOFF,
     *   DECOFF, XAOFF, XECOFF, DTORAD, A8BUF(128), RAX, DECX
      LOGICAL   T, F
      INCLUDE 'VBPLT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
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
      DATA PI /3.1415926/
      DATA CHLL, CHMM /'LL      ','MM      '/
      DATA CHRA, CHDEC /'RA---SIN','DEC--SIN'/
      DATA LUNA, LUNB /17, 18/
C-----------------------------------------------------------------------
      EPS = 0.1
      DISK2 = XDI2IN + EPS
      IUSER = USERID
      SEQ2 = XS2IN + EPS
      NITER = XNIT + EPS
      IF (NITER.GT.MAXCC) NITER = MAXCC
      NUMCC = 0
      IF ((NAM2IN.EQ.' ') .AND. (CLA2IN.EQ.' ')) GO TO 200
      IF (NITER.LE.0) GO TO 200
C                                      get CATCLN
      MSGSUP = 32000
C                                       Look for uv data
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISK2, NAM2IN, CLA2IN, SEQ2, UTYPE, IUSER,
     *   LUNA, INDA, CNO, CATCLN, IABUF, IERR)
C                                       If not try MA file
      IF (IERR.EQ.3) THEN
         UTYPE = 'MA'
         CALL MAPOPN ('READ', DISK2, NAM2IN, CLA2IN, SEQ2, UTYPE, IUSER,
     *      LUNA, INDA, CNO, CATCLN, IABUF, IERR)
         END IF
      MSGSUP = 0
      XS2IN = SEQ2
      XDI2IN = DISK2
      CALL CHR2H (12, NAM2IN, 1, XNAM2I)
      CALL CHR2H (6, CLA2IN, 1, XCLA2I)
C                                      If not found => no model plot.
      IF (IERR.EQ.0) GO TO 40
         IF (IERR.EQ.3) GO TO 200
            WRITE (MSGTXT,1035) IERR
            CALL MSGWRT (8)
            GO TO 999
C                                     Get RA and Dec offsets.
 40   CALL AXEFND (8, CHLL, KICTPN, CATCLR(KHCTP), ICLL, IERR)
      IF (IERR.NE.0) CALL AXEFND (2, CHRA, KICTPN, CATCLR(KHCTP), ICLL,
     *   IERR)
      WRITE (MSGTXT,1041) IERR, CHLL, CHRA
      IF (IERR.NE.0) GO TO 990
      CALL AXEFND (8, CHMM, KICTPN, CATCLR(KHCTP), ICMM, IERR)
      IF (IERR.NE.0) CALL AXEFND (3, CHDEC, KICTPN, CATCLR(KHCTP), ICMM,
     *   IERR)
      WRITE (MSGTXT,1041) IERR, CHMM, CHRA
      IF (IERR.NE.0) GO TO 990
C                                       Get RA and Dec offsets from
C                                       uv data reference position.
      DTORAD = 3.14159265358979326D0 / 180.0D0
      CONST = 1.745329E-2 * 2.0 * 3.1415926
      CLNRA = CATCLD(KDCRV+ICLL) + CATCLR(KRXSH)
      CLNDEC = CATCLD(KDCRV+ICMM) + CATCLR(KRYSH)
      RAX = RA + CATR(KRXSH)
      DECX = DEC + CATR(KRYSH)
      RAOFF = SIN (DTORAD * (CLNRA - RAX)) * COS (CLNDEC * DTORAD)
      DECOFF = COS (DECX * DTORAD) * SIN (CLNDEC * DTORAD) -
     *   SIN (DECX * DTORAD) * COS (CLNDEC * DTORAD) *
     *   COS ((CLNRA - RAX) * DTORAD)
C                                     Prepare to shift & rotate.
      XXROT = 0.0
      YYROT = 0.0
      CALL ROTFND (CATCLR, XXROT, IERR)
      CALL ROTFND (CATR, YYROT, IERR)
      MAPROT = YYROT - XXROT
C                                       Get reference pixel offsets
      CCROT = COS (1.745329E-2 * (XXROT))
      SSROT = SIN (1.745329E-2 * (XXROT))
      XPOFF = -(RAOFF * CCROT - DECOFF * SSROT) / DTORAD
      YPOFF= -(DECOFF * CCROT + RAOFF * SSROT) / DTORAD
      RAOFF = - RAOFF * DTORAD * 360.0D0
      DECOFF = -  DECOFF * DTORAD * 360.0D0
      CCROT = COS (1.745329E-2*(YYROT))
      SSROT = SIN (1.745329E-2*(YYROT))
      XAOFF = RAOFF * CCROT - DECOFF * SSROT
      XECOFF = DECOFF*CCROT+RAOFF*SSROT
      RAOFF = XAOFF
      DECOFF = XECOFF
      XAOFF = RAOFF / CONST
      XECOFF = DECOFF / CONST
      ZTERM = -0.5*CONST*(XAOFF*XAOFF+XECOFF*XECOFF)*
     *   1.745329E-2
      CCROT = COS (1.745329E-2 * (YYROT-XXROT))
      SSROT = SIN (1.745329E-2 * (YYROT-XXROT))
C                                       Get model.
C                                       Open CLEAN component file.
      NKOL = 0
      NREC = 1
      NKEY = 0
      NCOUNT = 0
      CALL TABINI ('READ', 'CC', DISK2, CNO, VER, CATCLN, LUNB, NKEY,
     *   NREC, NKOL, ABUF, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1000) IERR
         GO TO 990
 60   CALL RFILL (7, 0.0, RECORD)
C                                       Find columns (physical)
      TYPKOL = 7
      BMAKOL = 0
      BMIKOL = 0
      BPAKOL = 0
      NKEY = NKOL
      IF (NKEY.GT.7) NKEY = 7
      CALL FNDCOL (NKEY, KEYS, 8, F, BUFF1, KOLS, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1010) IERR
         GO TO 990
C                                       Get number of components
 70     LIMIT = IBUFF1(5)
        IF (NITER.LT.LIMIT) LIMIT = NITER
        DOGAUS = F
        DOSPHE = F
        DO 90 I = 1,LIMIT
            CALL TABIO ('READ', 0, I, RECORD, BUFF1, IERR)
            IF (IERR.EQ.0) GO TO 80
               IF (IERR.LT.0) GO TO 90
                  WRITE (MSGTXT,1030) IERR, I
                  GO TO 990
C                                       Deal with component.
 80            NCOUNT = NCOUNT + 1
C                                       No need to shift the components
C                                       since this already done in the
C                                       CLEANing task.
               XPOS(NCOUNT) = (RECORD(RAKOL) * CCROT +
     *            RECORD(DECKOL) * SSROT) * DTORAD * 2.0 * PI
               YPOS(NCOUNT) = (RECORD(DECKOL) * CCROT -
     *            RECORD(RAKOL) * SSROT) * DTORAD * 2.0 * PI
               SFLUX(NCOUNT) = RECORD(FLXKOL)
               GAUSA(NCOUNT) = 0
               GAUSB(NCOUNT) = 0
               GAUSC(NCOUNT) = 0
C                                       See if gaussian
               IF ((ABS (RECORD(TYPKOL)-1.).GT.0.5) .AND.
     *         (ABS (RECORD(TYPKOL)-3.).GT.0.5)) GO TO 90
C                                       Convert to radians
                  GAUSA(NCOUNT) = RECORD(BMAKOL) * DTORAD
                  GAUSB(NCOUNT) = RECORD(BMIKOL) * DTORAD
                  GAUSC(NCOUNT) = RECORD(BPAKOL) * DTORAD
                  IF (ABS (RECORD(TYPKOL)-1.).LT.0.5) DOGAUS = T
                  IF (ABS (RECORD(TYPKOL)-3.).LT.0.5) DOSPHE = T
 90               CONTINUE
C                                       Close CLNFIL.
      CALL TABIO ('CLOS', 0, IPOINT, RECORD, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 100
         WRITE (MSGTXT,1060) IERR
         GO TO 990
C                                       Unmark file
 100  CALL MAPCLS ('READ', DISK2, CNO, LUNA, INDA, CATCLN, F, ABUF,
     *   IERR)
      NUMCC = NCOUNT
C                                       Convert gaussian parameters
      IF ((NUMCC.LE.0) .OR. (.NOT.DOGAUS)) GO TO 160
         DO 150 I = 1,NUMCC
            XTEMPC = GAUSC(I)
            AM = COS ((XTEMPC+MAPROT)*3.1415927/180.)
            AN = SIN ((XTEMPC+MAPROT)*3.1415927/180.)
            TA = GAUSA(I) * 3.1415927 / 1.1774
            TB = GAUSB(I) * 3.1415927 / 1.1774
            GAUSA(I) = -(TA*TA*AM*AM + TB*TB*AN*AN)
            GAUSB(I) = -((TB*TB-TA*TA) * AN*AM)
            GAUSC(I) = -(TA*TA*AN*AN + TB*TB*AM*AM)
 150        CONTINUE
C                                       Convert sphere parameters
 160  IF ((NUMCC.LE.0) .OR. (.NOT.DOSPHE)) GO TO 200
         DO 170 I = 1,NUMCC
            GAUSA(I) = GAUSA(I) * 6.283185
 170        CONTINUE
C
 200  IERR = 0
      XNIT = NCOUNT
      IF ((NITER.LE.0) .OR. (NUMCC.LE.0)) XNIT = 0.0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REEDIN: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('REEDIN: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('REEDIN: READ ERROR',I3,' RECORD ',I5)
 1035 FORMAT ('REEDIN: ERROR IN OPENING FILE IERR = ',I5)
 1041 FORMAT ('REEDIN: ERROR',I3,' FINDING CLEAN AXIS ',A8,' OR ',A8)
 1060 FORMAT ('REEDIN: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE COLSET (ANT1, ANT2, DISKIN, CNOIN, NUMBAS, ANTLAB,
     *   IPOL, IRET)
C-----------------------------------------------------------------------
C   Routine to set up for column listing; checks that data in "TB"
C   order and prepares the list of antennas (ANTENS) for UVGET that
C   includes all antennas mentioned in either XANT or XBASE.
C   Also checks allowed Stokes types.
C   Input:
C      DISKIN             I    Data disc
C      CNOIN              I    Data seq #
C      ANT1(*)            I    Antenna array
C      ANT2(*)            I    Baseline array
C   Output:
C      NUMBAS             I    The number of baselines selected.
C      ANTLAB(MXBASE,2)   I    The First (*,1) and second (*,2) antenna
C                              numbers of each baseline selected.
C      IPOL               I    Polarization type 1=I,2=Q,3=U,4=V
C                              R or L => 1
C      IRET               I    Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS(*)          I    Selected antenna numbers
C      STOKES             C*4? Stokes' parameter selected (changed if
C                              an unallowed type specified)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER DISKIN, CNOIN, ANT1(*), ANT2(*)
      CHARACTER STO(8)*4
      INTEGER   NUMBAS, IPOL, IRET, I, J, K, NEXT, LIMIT, MXBAS,
     *   NUMAN(20), NOANT, IANT1, IANT2, IANT, LUN, NSTO, JANT, LIM,
     *   XANT(MXBASE), XBASE(MXBASE)
      LOGICAL   T, F, ALLANT, ALLBAS
      INTEGER   ANTLAB(MXBASE,2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (NUMAN, UBUFF(1025))
      DATA NSTO, STO /8, 'I   ','Q   ','U   ','V   ','RR  ','LL  ',
     *   'RL  ','LR  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA MXBAS /MXBASE/
C-----------------------------------------------------------------------
      IRET = 0
      CALL COPY (MXBASE, ANT1, XANT)
      CALL COPY (MXBASE, ANT2, XBASE)
C                                       Check Stokes
      DO 10 I = 1,NSTO
         IPOL = I
         IF (STOKES.EQ.STO(I)) GO TO 30
 10      CONTINUE
C                                       Not allowed type, use 'I' or
C                                       'RR' or 'LL'
         IF (ICOR0.GT.0) THEN
            WRITE (MSGTXT,1010) STOKES, STO(1)
            STOKES = STO(1)
            IPOL = 1
         ELSE IF (ICOR0.LT.0) THEN
            IF (ICOR0.EQ.-1) THEN
               WRITE (MSGTXT,1010) STOKES, STO(5)
               STOKES = STO(5)
               IPOL = 1
            ELSE IF (ICOR0.EQ.-2) THEN
               WRITE (MSGTXT,1010) STOKES, STO(6)
               STOKES = STO(6)
               IPOL = 1
               END IF
            END IF
         CALL MSGWRT (6)
 30   IF (IPOL.GT.4) IPOL = 1
C                                       Check sort order
      IF (ISORT.EQ.'TB') GO TO 50
         IRET = 1
         WRITE (MSGTXT,1030) ISORT
         GO TO 990
C                                       Check for all ant. selected
 50   ALLANT = T
      ALLBAS = T
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (XANT(I).EQ.0)
         ALLBAS = ALLBAS .AND. (XBASE(I).EQ.0)
CCC         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
CCC         ALLBAS = ALLBAS .AND. (ABS (XBASE(I)).LE.1.0E-10)
 100     CONTINUE
      IF (ALLANT.OR.ALLBAS) GO TO 500
C                                       Not all selected - make list
         NEXT = 0
C                                       ANTENNAS array.
         DO 120 I = 1,50
            IF (XANT(I).EQ.0) GO TO 120
               NEXT = NEXT + 1
               ANTENS(NEXT) = XANT(I)
 120        CONTINUE
C                                       Remove duplicates
         CALL IMERGE (NEXT, ANTENS, NEXT)
C                                       Fix input array for latter
         DO 130 I = 1,50
            IF (I.LE.NEXT) XANT(I) = ANTENS(I)
            IF (I.GT.NEXT) XANT(I) = 0
 130        CONTINUE
C                                       BASELINE array.
         DO 150 I = 1,50
            IF (XBASE(I).EQ.0) GO TO 150
C                                       See if already have
               DO 140 J = 1, NEXT
                  IF (XBASE(I).EQ.ANTENS(J)) GO TO 150
 140              CONTINUE
C                                       New antenna
                  NEXT = NEXT + 1
                  ANTENS(NEXT) = XBASE(I)
 150        CONTINUE
C                                       Fill ANTLAB with baseline
C                                       numbers.
 500  DO 510 I = 1,MXBAS
         ANTLAB(I,1) = 0
         ANTLAB(I,2) = 0
 510     CONTINUE
C                                       Find number of antennas
      LUN = 28
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, UBUFF, NUMAN, IRET)
      IF ((IRET.EQ.0) .AND. (NUMAN(1).GE.SUBARR)) GO TO 530
C                                       If no AN tables assume that this
C                                       must be the VLA.
         IF (IRET.EQ.10) THEN
            MSGTXT = 'No AN tables - I assume that this must be VLA' //
     *         ' data with 28 antennas'
            CALL MSGWRT (5)
            NUMAN(1) = MAX (1, SUBARR)
            CALL FILL (NUMAN(1), 28, NUMAN(2))
            IRET = 0
            GO TO 530
            END IF
         IF (NUMAN(1).LE.SUBARR) WRITE (MSGTXT,1510) NUMAN(1),
     *      SUBARR
         IF (IRET.NE.0) WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
 530  NOANT = NUMAN(SUBARR+1)
      IF (SUBARR.LE.0) NOANT = NUMAN(2)
C                                       Fill  arrays
      NEXT = 1
      DO 600 I = 1,50
         IANT1 = XANT(I)
         IF (ALLANT) IANT1 = I
         IF ((IANT1.LE.0) .OR. (IANT1.GT.NOANT)) GO TO 600
         LIMIT = 1
         IF (ALLBAS) LIMIT = I + 1
         IF (LIMIT.GT.50) LIMIT = 50
         DO 590 J = LIMIT,50
            IANT2 = XBASE(J)
            IF (ALLBAS) IANT2 = J
            IF ((IANT2.LE.0) .OR. (IANT2.GT.NOANT)) GO TO 590
            IF (IANT1.EQ.IANT2) GO TO 590
            IF (ALLANT .AND. (IANT2.LT.IANT1)) GO TO 590
            IF (ALLANT.OR.ALLBAS.OR.(IANT2.GT.IANT1)) GO TO 580
C                                       Check for redundant baselines
            JANT = MIN (IANT1, IANT2)
            IANT = MAX (IANT1, IANT2)
            LIM = NEXT - 1
            DO 570 K = 1,LIM
               IF ((JANT.EQ.ANTLAB(K,1)) .AND. (IANT.EQ.ANTLAB(K,2)))
     *            GO TO 590
 570           CONTINUE
 580           ANTLAB(NEXT,1) = MIN (IANT1, IANT2)
               ANTLAB(NEXT,2) = MAX (IANT1, IANT2)
               NEXT = NEXT + 1
 590           CONTINUE
 600     CONTINUE
      NUMBAS = NEXT - 1
      IF (NUMBAS.GT.0) GO TO 999
C                                       No baselines
      IRET = 2
      WRITE (MSGTXT,1600)
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Warning: Stokes=',A4,' not allowed, using Stokes=',A4)
 1030 FORMAT ('INCORRECT SORT ORDER = ',A2,' NOT TB')
 1510 FORMAT ('FEWER SUBARRAYS AVAILABLE (',I3,') THAN SPECIFIED ',I4)
 1511 FORMAT ('COLSET: GETNAN ERROR ',I3,' SEARCHING ANTENNA TABLES')
 1600 FORMAT ('ERROR: NO BASELINES SPECIFIED!')
      END
      SUBROUTINE BASAVG (NUMVIS, NUMBAS, ANTLAB, AMPVEC, DTYPE, DT,
     *   CHNSEL, DOSPEC, SCANV, TIME, NUSCAN, SCANUM, RPARM, VIS, AVTIM,
     *   AVU, AVV, AVW, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of amp, phase or the RMS
C   scatter for selected baselines.
C   Needs to be initialized by a call to UVGET.
C   The  order of the baselines returned in SCANV is defined by
C   the order in the array NATLAB.  All data specified  (channels, IFs)
C   are averaged but only one polarization is allowed.
C   Inputs:
C     NUMVIS       I    Current visibility number
C     NUMBAS       I    Number of baselines to average.
C     ANTLAB(MXBASE,2)  I    The First (*,1) and second (*,2) antenna
C                      numbers of each baseline selected.
C     AMPVEC       L    If true do ampscalar averaging else vector.
C     DTYPE        I    Output type, 0 => amplitude, 1=> phase,
C                       2 => amplitude RMS, 3 => weight.
C     DT           R    Averaging time in days
C     CHNSEL(3,10) I    Channel selection
C     DOSPEC       L    TRUE if spectral averaging to be done
C   Input/Output:
C     RPARM(20)    R    Random parameter array, first record of call.
C                      (1) = 'INDE' => don't use.
C                      (2) if single baseline averaged then RPARM
C                          reflects the data output.
C     VIS(3,*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(MXBASE)R   The result for baselines in ANTLAB
C     TIME(8)      I    Time range, start, stop; days, hours, min, sec.
C                       Unless NUSCAN only first 4 values are set.
C     NUSCAN       L    True IF the first record in a new scan.
C     AVTIM        R    Average time (days) of output record
C     AVU          R    Average u of output record
C     AVV          R    Average v of output record
C     AVW          R    Average w of output record
C     IERR         I    Return code, 0 => OK, -1 => out of data,
C                       > 0 => failed.
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
      LOGICAL   AMPVEC, NUSCAN, DONE1, GOTDAT, DOSPEC
      INTEGER   DTYPE, TIME(8), NUMBAS, IERR, MXANT, I, JA1, JA2,
     *   SUNUM, JERR, ISLUN, IBASE, IVIS, COUNT(MXBASE), KVIS, SCANUM,
     *   CNTTIM, NUMFRQ, NUMPLZ, CHNSEL(3,10), ANTLAB(MXBASE,2),
     *   NUMVIS, IDAY, IVSCNT
      REAL      RPARM(20), VIS(3,*), DT, T1, AMP, SQRT, SUMTIM,
     *   WORK(MXBASE,4), TEMP, TEMP1, TEMP2, SCANV(MXBASE),
     *   SUMU, SUMV, SUMW, AVTIM, AVU, AVV, AVW, TLAST, CT, DTUTC
      DOUBLE PRECISION X8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      SAVE TLAST, DTUTC, IVSCNT
      DATA ISLUN /26/
      DATA MXANT /MAXANT/
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
 10   CONTINUE
      DO 50 I = 1,NUMBAS
         COUNT(I) = 0
         WORK(I,1) = 0.0
         WORK(I,2) = 0.0
         WORK(I,3) = 0.0
         WORK(I,4) = 0.0
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0
      SUMU = 0.0
      SUMV = 0.0
      SUMW = 0.0
      AVTIM  = 0.0
      AVU = 0.0
      AVV = 0.0
      AVW = 0.0
C                                       Initialize time
      T1 = 1.0E10
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
      KVIS = (LREC-NRPARM) / 3
      NUMFRQ = CATBLK(KINAX+JLOCF)
      NUMPLZ = CATBLK(KINAX+JLOCS)
C                                       Loop reading data
 100     IF (.NOT.DONE1) THEN
            CALL UVGET ('READ', RPARM, VIS, IERR)
            IVSCNT = IVSCNT + 1
            END IF
         IF (IERR.GT.0) GO TO 999
         IF (IERR.EQ.-1) GO TO 500
         DONE1 = .FALSE.
C
         CT = RPARM(ILOCT+1) - DTUTC
C                                       Set up first time boundary
         IF (IVSCNT.EQ.1) THEN
            IDAY = CT
            X8 = (CT - IDAY) / DT
            TLAST = IDAY + DINT (X8) * DT + DT
            END IF
C                                       Spectral averaging
         IF ((DOSPEC) .AND. (IVSCNT.GT.0)) THEN
            CALL AVGCIF (VIS, NUMPLZ, NUMFRQ, BCHAN, ECHAN, BIF,
     *         EIF, CHNSEL, KNCS, VIS)
            END IF
C                                       Antenna numbers
         JA1 = RPARM(ILOCB+1) / 256. + 0.1
         JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
C                                       Find baseline
         DO 110 I = 1,NUMBAS
            IBASE = I
            IF ((JA1.EQ.ANTLAB(I,1) .AND. (JA2.EQ.ANTLAB(I,2))))
     *         GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
C
 120     GOTDAT = .TRUE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (CT.GT.TLAST)) GO TO 500
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         SUMU = SUMU + RPARM(ILOCU+1)
         SUMV = SUMV + RPARM(ILOCV+1)
         SUMW = SUMW + RPARM(ILOCW+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
         IF (AMPVEC) GO TO 200
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
            DO 150 IVIS = 1,KVIS
               IF (VIS(3,IVIS).LE.0.0) GO TO 150
               COUNT(IBASE) = COUNT(IBASE) + 1
               WORK(IBASE,1) = WORK(IBASE,1) + VIS(1,IVIS)
               WORK(IBASE,2) = WORK(IBASE,2) + VIS(2,IVIS)
               WORK(IBASE,3) = WORK(IBASE,3) + VIS(1,IVIS) ** 2
               WORK(IBASE,4) = WORK(IBASE,4) + VIS(2,IVIS) ** 2
 150           CONTINUE
         GO TO 100
C                                       Ampscalar averaging
      INCLUDE 'INCS:ZVD.INC'
 200     DO 250 IVIS = 1,KVIS
            IF (VIS(3,IVIS).LE.0.0) GO TO 250
            AMP = SQRT (VIS(1,IVIS) * VIS(1,IVIS) +
     *         VIS(2,IVIS) * VIS(2,IVIS))
            VIS(1,IVIS) = VIS(1,IVIS) + 1.0E-20
            COUNT(IBASE) = COUNT(IBASE) + 1
            WORK(IBASE,1) = WORK(IBASE,1) + AMP
            IF (DTYPE.EQ.1) WORK(IBASE,2) = WORK(IBASE,2) +
     *         ATAN2 (VIS(2,IVIS), VIS(1,IVIS))
            WORK(IBASE,3) = WORK(IBASE,3) + AMP*AMP
            WORK(IBASE,4) = WORK(IBASE,4) + VIS(3,IVIS)
 250        CONTINUE
            GO TO 100
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      IF (.NOT.AMPVEC) THEN
C                                       Vector averaging
         DO 550 I = 1,NUMBAS
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = SQRT (WORK(I,1)*WORK(I,1) +
     *            WORK(I,2)*WORK(I,2)) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = ATAN2 (WORK(I,2), WORK(I,1)+1.0E-20)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP1 = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               TEMP2 = (COUNT(I) * WORK(I,4) - (WORK(I,2)**2))
               TEMP = TEMP1 + TEMP2
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
 550           CONTINUE
      ELSE
C                                       Else Amp scalar ave
         DO 650 I = 1,NUMBAS
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,1) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,2) / COUNT(I)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
C                                       Weight
            IF ((DTYPE.EQ.3) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,4) / COUNT(I)
 650        CONTINUE
C                                       end if vector averaging
         END IF
C                                       Get source info
      IF (.NOT.NUSCAN) GO TO 800
      CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
      IF (JERR.EQ.11) THEN
C                                       Didn't find source
         WRITE (MSGTXT,1750) SUNUM
         CALL MSGWRT (8)
         JERR = 0
         END IF
      IF (JERR.LE.0) GO TO 800
         IERR = JERR
         WRITE (MSGTXT,1700) JERR
         GO TO 990
C                                       Time
 800  T1 = 0
      IF (CNTTIM.GT.0) THEN
         T1 = SUMTIM / CNTTIM
         AVTIM = T1
         AVU = SUMU / CNTTIM
         AVV = SUMV / CNTTIM
         AVW = SUMW / CNTTIM
         END IF
      CALL TODHMS (T1, TIME)
C                                       Set up new time boundary
      IDAY = CT
      X8 = (CT - IDAY) / DT
      TLAST = IDAY + DINT (X8) * DT + DT
C                                       If end of data, close UVGET
CCC      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
CCC      IF (JERR.NE.0) IERR = JERR
CCC      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) IERR = -2
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('BASAVG: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('BASAVG: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE BASLST (NUMAN, ISUB, ANTENN, BASELN, NBASE, ANT1, ANT2)
C-----------------------------------------------------------------------
C   Determine a list of selected antenna pairs from the standard
C   ANTENNA and BASELINE adverbs.  BASLST differs from AN10RS in that
C   BASLST will always produce a full list of antenna pairs while
C   AN10RS will produce an abbreviated list if there are no
C   entries in ANTENNAS or BASELINE.
C
C   Inputs:
C      NUMAN    I(51)   NUMAN(1) is the number of subarrays present.
C                       NUMAN(i) is the maximum antenna number for
C                       subarray i-1, for i > 1.
C      ISUB     I       The subarray number for which a list of
C                       baselines is required: must be greater than
C                       zero (and is not checked!).
C      ANTENN   R(50)   The values of the ANTENNAS adverb.
C      BASELN   R(50)   The values of the BASELINE adverb.
C
C   Input/output:
C      NBASE    I       The maximum number of baselines to list on
C                       input.  Either the number of baselines found
C                       or zero or zero on output.  A value of zero
C                       indicates that more than the maximum number of
C                       baselines met the selection criteria.
C
C   Outputs:
C      ANT1     I(*)    List of first elements of antenna pairs.
C      ANT2     I(*)    List of second elements of antenna pairs.
C                       For all i, 1 <= i <= NBASE, ANT2(i) > ANT1(i).
C-----------------------------------------------------------------------
      INTEGER   NUMAN(51), ISUB
      REAL      ANTENN(50), BASELN(50)
      INTEGER   NBASE, ANT1(*), ANT2(*)
C                                        DOSEL is true if baselines
C                                        listed in the adverbs are
C                                        to be deselected rather
C                                        selected.
      LOGICAL   DESEL
C                                        ANT lists the absolute values
C                                        of entries in the ANTENNAS
C                                        adverb, NANT is the number
C                                        of non-zero entries.
      INTEGER   ANT(50), NANT
C                                        BAS lists the absolute values
C                                        of entries in the BASELINE
C                                        adverb, NBAS is the number
C                                        of non-zero entries.
      INTEGER   BAS(50), NBAS
C                                        NREQ is the number of
C                                        baselines that match the
C                                        request criteria that have
C                                        been found so far.
      INTEGER   NREQ
C
      INTEGER   I, J
C
      LOGICAL   REQBAS
      EXTERNAL  REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
      IF ((ISUB.GT.0) .AND. (ISUB.LE.NUMAN(1))) THEN
         DESEL = .FALSE.
         NREQ = 0
         NANT = 0
         NBAS = 0
         DO 10 I = 1, 50
            IF (ANTENN(I).LT.0.0) DESEL = .TRUE.
            IF (ABS (ANTENN(I)).GT.0.5) THEN
               NANT = NANT + 1
               ANT(NANT) = NINT (ABS (ANTENN(I)))
               END IF
            IF (ABS (BASELN(I)).GT.0.5) THEN
               NBAS = NBAS + 1
               BAS(NBAS) = NINT (ABS (BASELN(I)))
               END IF
   10       CONTINUE
C                                       If NANT = 0 and NBAS > 0 then
C                                       the code below will fail, in
C                                       this case we need to fill up the
C                                       local array ANT with the antenna
C                                       numbers.
         IF ((NANT.EQ.0) .AND. (NBAS.GT.0)) THEN
            NANT = NUMAN(ISUB+1)
            DO 40 I = 1, NANT
               ANT(I) = TELNO(I)
   40          CONTINUE
            END IF
C
         DO 30 I = 1, NUMAN(ISUB+1) - 1
            DO 20 J = I+1, NUMAN(ISUB+1)
               IF (REQBAS (I, J, DESEL, ANT, NANT, BAS, NBAS)) THEN
                  IF (NREQ.EQ.NBASE) THEN
C                                        Too many baselines.
                     NBASE = 0
                     GO TO 999
                  ELSE
                     NREQ = NREQ + 1
                     ANT1(NREQ) = I
                     ANT2(NREQ) = J
                     END IF
                  END IF
   20          CONTINUE
   30       CONTINUE
         NBASE = NREQ
      ELSE
C                                        Non-existent subarray.
         NBASE = 0
         END IF
C
  999 RETURN
      END

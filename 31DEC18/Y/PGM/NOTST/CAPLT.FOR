LOCAL INCLUDE 'PCAPLT.INC'
C                                       Parameter include for CAPLT
      INTEGER   MAXQAD, MAXCC
C                                       MAXQAD = max. no. quads
      PARAMETER (MAXQAD=1000)
C                                       MAXCC = max. no. model
C                                       components.
      PARAMETER (MAXCC = 200000)
      INTEGER MAXSAT
      PARAMETER (MAXSAT = 5*6)
LOCAL END
LOCAL INCLUDE 'CAPLT.INC'
C                                       Local include for CAPLT
      INCLUDE 'PCAPLT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XXSTOK,
     *   XXSOUR(4,30), XXCALC, XPPLOT(2), XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, XSTOK*4,
     *   STANAM(MAXANT)*8, XSOUR(30)*16, XCALCO*4, PPLOT*8,
     *   OPCODE*4
      REAL      USERID, XSIN, XDISIN, XS2IN, XDI2IN, XBCHAN, XECHAN,
     *   XBIF, XEIF, XINC, UVRANG(2), XTIME(8), XANT(50), YTYPE,
     *   BPARM(10), XINV2, XCOMP(MAXAFL), XFLUX, XNMAP, XSUBA, XDOTV,
     *   XGRCH, XQUAL, AMPMIN, XBAND, XFREQ, XFQID, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   XSOLIN, XSYM, XFACT, XLABEL, XYRATO, BADD(10)
      REAL      BUFF1(UVBFSS), CCPOS(3,MAXCC), SFLUX(MAXCC),
     *   GAUSA(MAXCC), GAUSB(MAXCC), GAUSC(MAXCC), CHOUT(4), IATUTC,
     *   UT1XXX
      INTEGER   JBUFSZ, NUMCC, NANT, NSRC, IDN(30), OLDCNO,
     *   CATKEP(256)
      LOGICAL   DOGAUS, DOSPHE, DOTV, MULTI
      REAL      TBEG, TFIN, XYSCL(2), XYOFF(2), XYVALS(MAXANT,MAXANT),
     *   XYCLOS(2,MAXQAD), TIMMIN, TIMMAX, UVSCAL, INISCL(4)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, LABEL,
     *   TYPEAX(2), NCH, IBIF, VER2, TESTEM(2), POLPLT, IPOLPT,
     *   OKDAT(MAXANT,MAXANT), ANTS(50), ISUB, CPQUAD(4,MAXQAD),
     *   NUMQAD, GRCHN, TVCHN, TVCORN(4), GOODCL(MAXQAD), CHNSEL(3,10),
     *   IORBIT(MAXANT), DOMODL, LTYPE
      LOGICAL   UVREV, SCALEM(2), NOUVR, FLOTEM, DROPEM, DOERRB,
     *   DECICL, INDECL
C
      DOUBLE PRECISION XB(MAXANT), YB(MAXANT), ZB(MAXANT),
     *   ORBITA(MAXSAT), JDREF, AFREQ, GST0, ARRLON, OBSRA, OBSDEC
C                                       WARNING: many of these commons
C                                       are declared locally with
C                                       variables of different names!!!
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XNAM2I, XCLA2I, XS2IN, XDI2IN, XINV2, XCOMP, XFLUX, XNMAP,
     *   XXSOUR, XQUAL, XXCALC, XBAND, XFREQ, XFQID, XBCHAN, XECHAN,
     *   XBIF, XEIF, XINC, UVRANG, XTIME, XXSTOK, XANT, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XSUBA,
     *   YTYPE, XSOLIN, XOPCOD, AMPMIN, BPARM, XPPLOT, XSYM, XFACT,
     *   XLABEL, XDOTV, XGRCH, XYRATO, BADD
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /VBPCOM/ TBEG, TFIN, XYSCL, XYOFF, XYVALS, XYCLOS, TIMMIN,
     *   TIMMAX, UVREV, SCALEM, NOUVR, FLOTEM, DROPEM, IAW1, IAW2, INC,
     *   SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH, IBIF, VER2, TESTEM,
     *   OKDAT, POLPLT, IPOLPT, ANTS, ISUB, LABEL, CHOUT, LTYPE,
     *   GRCHN, TVCHN, TVCORN, NSRC, IDN, OLDCNO, CATKEP, GOODCL,
     *   CHNSEL, DOTV, DOERRB, MULTI, DECICL, INDECL, UVSCAL, NUMQAD,
     *   CPQUAD, DOMODL, INISCL
C
      COMMON /CANIN/ XB, YB, ZB, ORBITA, JDREF, AFREQ, GST0, ARRLON,
     *   OBSRA, OBSDEC, IATUTC, UT1XXX, NANT, IORBIT

      COMMON /COMPS/ CCPOS, SFLUX, GAUSA, GAUSB, GAUSC, DOGAUS, DOSPHE,
     *   NUMCC
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAM2IN, CLA2IN, XSTOK, STANAM,
     *   XSOUR, XCALCO, PPLOT, OPCODE
LOCAL END
LOCAL INCLUDE 'CLAVER.INC'
      LOGICAL   GOTDAT
      INTEGER   COUNT(4,MAXQAD), COUNT1(4,MAXQAD), COUNTA(MAXQAD),
     *   CNTTIM, IVSCNT
      REAL      SUMTIM, WORK(2,4,MAXQAD), WORKC(MAXQAD), GAMP(4,MAXQAD),
     *   GERR(4,MAXQAD), TLAST, DTUTC, UVW(3,4,MAXQAD)
      COMMON /CLAVG/ COUNT, COUNT1, COUNTA, WORK, WORKC, GAMP, GERR,
     *   UVW, TLAST, DTUTC, IVSCNT, GOTDAT, CNTTIM, SUMTIM
LOCAL END
      PROGRAM CAPLT
C-----------------------------------------------------------------------
C! Plots selected uv data and model values in clusure amplitudes
C# UV Plot-appl VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2012, 2014-2016, 2018
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
C   CAPLT plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways.  It plots closure amplitudes,
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
C     NMAPS          XNMAPS        Number of fields
C     NCOMP          XCOMP         Array of number of clean components;
C                                  one for each field.
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
C     YTYPE.....If > 0, plot error bars
C     BPARM......Control parameters:
C        1 = type of X-axis (where  1 = amplitude (Jy), 2 = phase
C           (degrees), 3 = u,v distance (klambda), 4 = u,v p.a.
C           (degrees, clockwise from v-axis), 5 = time (iat days),
C           6 = u, 7 = v, 8 = w (all in klambda), 9 = real part (Jy),
C           10 = imaginary part(Jy), and 11 = folded G.S.T.(hours),
C           12 = time (IAT hms), 13 = source no.,
C           14 = freqid, 15 = int. time, 16 = weight) 0 => 12
C        2 = unused
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
      INCLUDE 'CAPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'CAPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VBPIN (PRGM, MAXPLT, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Scaling
      CALL SCALCL (IRET)
      IF (IRET.NE.0) GO TO 995
C
      CALL CLPLOT (MAXPLT, IRET)
      IRET = MAX (0, IRET)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE VBPIN (PRGM, MAXPLT, JERR)
C-----------------------------------------------------------------------
C   VBPIN gets input parameters for CAPLT .
C   Inputs:  PRGM   C*6       Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C            MAXPLT I         Maximum number of plots per page.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UTYPE*2, STAT*4
      INTEGER   JERR, MAXPLT, NPARM, IERR, ITEMP, LIM1, IUSER, I, VER,
     *   L, J, MXANT, MXQUAD, IROUND, LUN
      REAL      CATR(256), EPS
      LOGICAL   DOMDL, MATCH, TRYMOD
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'CAPLT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA MXANT /MAXANT/
      DATA MXQUAD /MAXQAD/
C-----------------------------------------------------------------------
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
      NPARM = 253 + MAXAFL
      CALL GTPARM (PRGM, NPARM, RQUICK, USERID, BUFF1, IERR)
      XSUBA = MAX (XSUBA, 1.0)
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
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
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
C                                       Check SOLINT
      IF (XSOLIN.EQ.0.0) THEN
         XSOLIN = 5.0
         WRITE (MSGTXT,1020)
         CALL MSGWRT (3)
         END IF
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCLA2I, CLA2IN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (8, 1, XPPLOT, PPLOT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IF (XSTOK.EQ.' ') XSTOK = 'I   '
      IF (STOKES.EQ.' ') STOKES = 'I   '
      IF (OPCODE.NE.'INDE') OPCODE = 'CLOS'
      TRYMOD = ((NAM2IN(1:4).NE.' ') .OR. (CLA2IN.NE.' '))
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
      IF (BPARM(1).EQ.2.0) THEN
         BPARM(1) = 11.0
      ELSE
         BPARM(1) = 12.0
         END IF
      BPARM(2) = 1.0
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
      XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
      LIM1 = MXANT - 1
      DO 37 I = 1,LIM1
         L = I+1
         DO 36 J = L,MXANT
            XYVALS(I,J) = XYSCL(2)
            XYVALS(J,I) = XYOFF(2)
            OKDAT(I,J) = 1
            OKDAT(J,I) = 1
 36         CONTINUE
 37      CONTINUE
      DECICL = .FALSE.
      DO 38 I = 1, MXQUAD
         XYCLOS(1,I) = XYSCL(2)
         XYCLOS(2,I) = XYOFF(2)
 38      CONTINUE
C                                       Check for independent
C                                       closure phases
      INDECL = .FALSE.
      IF (OPCODE.EQ.'INDE') THEN
         INDECL = .TRUE.
         DO 40 I = 1,50
            IF (XANT(I).NE.0.0) INDECL = .FALSE.
 40         CONTINUE
         IF (.NOT.INDECL) THEN
            WRITE (MSGTXT,1080)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1090)
            JERR = 1
            GO TO 990
            END IF
         OPCODE = 'CLOS'
         END IF
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
      CALL RCOPY (4, BPARM(4), INISCL)
C                                       Tv and label parms
      DOERRB = YTYPE.GT.0.0
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (ABS(LTYPE).GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.GE.0) THEN
         LABEL = (LABEL/100)*100 + LTYPE
      ELSE
         LABEL = (LABEL/100)*100 - LTYPE
         END IF
      XLABEL = LABEL
C                                       Get CATBLK from file.
      LUNI = 16
      UTYPE = 'UV'
      OLDCNO = 1
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      IUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
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
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Multi-source
      CALL MULSDB (CATBLK, MULTI)
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
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (ECHAN.GT.BCHAN) THEN
         WRITE (MSGTXT,1040) BCHAN, ECHAN
         CALL MSGWRT (2)
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
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF (EIF.GT.BIF) THEN
         WRITE (MSGTXT,1050) BIF, EIF
         CALL MSGWRT (2)
         END IF
      XBIF = BIF
      XEIF = EIF
      IBIF = BIF
C                                       Save results in input Parms
      TYPEAX(1) = BPARM(1) + EPS
      TYPEAX(2) = BPARM(2) + EPS
      DOMDL = (TYPEAX(1).NE.1) .AND. (TYPEAX(1).NE.2) .AND.
     *   (TYPEAX(1).NE.9) .AND. (TYPEAX(1).NE.10) .AND.
     *   ((TYPEAX(2).EQ.1) .OR. (TYPEAX(2).EQ.2) .OR. (TYPEAX(2).EQ.9)
     *   .OR. (TYPEAX(2).EQ.10))
      DOMODL = IROUND (XNMAP)
      IF (DOMODL.LT.0) THEN
         DOMODL = 1
      ELSE IF (DOMODL.GT.0) THEN
         DOMODL = 2
      ELSE
         DOMDL = .FALSE.
         END IF
C                                       Block model when can't handle
      IF (DOMDL .AND. MULTI .AND. (NSRC.NE.1)) THEN
         IF (TRYMOD) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1120)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1130)
            CALL MSGWRT (6)
            DOMDL = .FALSE.
            END IF
         END IF
      IF (.NOT.DOMDL) THEN
         DROPEM = .TRUE.
         NUMCC = 0
         END IF
C                                       Maximum number of plots per page
      MAXPLT = 3
      IF (BPARM(8).GE.0.95) MAXPLT = BPARM(8) + EPS
      BPARM(8) = MAXPLT
C                                       Get antenna info.
      VER = XSUBA + 0.01
      CALL ANTIN (VER, BCHAN, IBIF, DISKIN, OLDCNO, FRQSEL, JERR)
      IF (JERR.NE.0) GO TO 999
      XSUBA = VER
C                                       Get CLEAN components.
      VER2 = XINV2 + 0.01
      IF (DOMDL) THEN
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
            END IF
         CALL REEDIN (VER2, JERR)
         END IF
      IF (NUMCC.LE.0) THEN
         DOMODL = 0
         DROPEM = .TRUE.
         END IF
      XINV2 = VER2
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VBPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('VBPIN: SOLINT was 0, resetting to 5 minutes')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1040 FORMAT ('Averaging from channel ',I4,'-',I4)
 1050 FORMAT ('Averaging from IF ',I4,'-',I4)
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('Trying to plot indep. cl. amps, but TRIANGLE is ',
     *   'non - zero')
 1090 FORMAT ('Set TRIANGLE adverb array to 0')
 1110 FORMAT ('You may be trying to plot a model without specifying')
 1120 FORMAT ('a sourcename - I do not allow that and am switching')
 1130 FORMAT ('that option off.')
      END
      SUBROUTINE SCALCL (IRET)
C-----------------------------------------------------------------------
C   SCALCL determines the necessary quadrangle-based scalings from the
C   data.
C   IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'CAPLT.INC'
      INTEGER   IRET, I, J, K, L, MXQUAD, NUMVIS, XUMVIS,
     *   TIME(8), NPOSS(MAXQAD), NCLOSE(MAXQAD), IQUADR, NEGANT,
     *   SCANUM, KIKANT(50), IROUND, INEG, KPLOT
      REAL      RPARM(20), DT, SCANV(MAXQAD), CLERR(MAXQAD), TDAYS,
     *   SCANM(1)
      LOGICAL   NOSCAL, KEEP, DOALCL, TMPERR, NUSCAN
      DOUBLE PRECISION GSEC, GMST, GAST, RATE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA MXQUAD /MAXQAD/
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      NOSCAL = (.NOT.SCALEM(1)) .AND. (.NOT.SCALEM(2))
      IRET = 0
      IF ((NOSCAL) .AND. (.NOT.DROPEM)) GO TO 999
      DOALCL = .TRUE.
      NEGANT = 0
      J = IROUND (XANT(1))
      IF (J.LT.0) NEGANT = 1
      DO 10 I = 1,50
         J = IROUND (XANT(I))
         IF (J.NE.0) THEN
            IF (NEGANT.GT.0) THEN
               KIKANT(NEGANT) = ABS(J)
               NEGANT = NEGANT + 1
            ELSE
               IF (J.GT.0) DOALCL = .FALSE.
               END IF
            END IF
 10      CONTINUE
      NEGANT = MAX (0, NEGANT-1)
C                                        Determine the number of
C                                        quadruplets to be plotted.
      IF (.NOT.DOALCL) THEN
         CALL CLOSET (XANT, NUMQAD, CPQUAD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                        All independent
      ELSE IF (INDECL) THEN
         CALL INDCLT (NANT, NUMQAD, CPQUAD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                        Else plot them all
      ELSE
         IQUADR = 0
         DO 50 I = 1,NANT
            DO 40 J = 1,NANT
               DO 30 K = 1,NANT
                  DO 20 L = 1,NANT
                     IF ((I.LT.J) .AND. (J.LT.K) .AND. (K.LT.L)) THEN
                        DO 15 INEG = 1, NEGANT
                           IF ((I.EQ.KIKANT(INEG)) .OR.
     *                        (J.EQ.KIKANT(INEG)) .OR.
     *                        (K.EQ.KIKANT(INEG)) .OR.
     *                        (L.EQ.KIKANT(INEG))) GO TO 50
 15                        CONTINUE
                        IF (IQUADR.LT.MXQUAD) THEN
                           IQUADR = IQUADR + 1
                           CPQUAD(1,IQUADR) = I
                           CPQUAD(2,IQUADR) = J
                           CPQUAD(3,IQUADR) = K
                           CPQUAD(4,IQUADR) = L
                           END IF
                        END IF
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
         NUMQAD = IQUADR
         END IF
C
      IF (NUMQAD.GT.MXQUAD) NUMQAD = MXQUAD
      DO 60 I = 1,MXQUAD
         NPOSS(I) = 0
         NCLOSE(I) = 0
         GOODCL(I) = 0
 60      CONTINUE
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.EQ.-1) GO TO 200
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      UVSCAL = FREQ / UVFREQ
      NUMVIS = 0
      XUMVIS = 0
      TIMMIN = 1.0E10
      TIMMAX = -1.0E10
      DO 80 I = 1, MXQUAD
         XYCLOS(1,I) = 1.0E10
         XYCLOS(2,I) = -1.0E10
 80      CONTINUE
C                                       Loop
      DT = XSOLIN / 1440.0
      TMPERR = .FALSE.
      IF (BPARM(1).EQ.11.0) THEN
         CALL GSTROT (JDREF, GMST, GAST, RATE)
         GAST = GAST * (24.D0/360.D0)
         GAST = MOD (GAST, 24.D0)
         END IF
C
 100  CONTINUE
C                                       Read vis. record and form
C                                       closure phase
         CALL CLOSAV (NUMVIS, NUMQAD, CPQUAD, DT, SCANV, CLERR, TMPERR,
     *      TIME, TDAYS, NUSCAN, SCANUM, RPARM, UVSCAL, BUFF1, NPOSS,
     *      NCLOSE, 0, SCANM, IRET)
         IF (IRET.EQ.-2) GO TO 100
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 4
            GO TO 990
            END IF
         IF (BPARM(1).EQ.11.0) THEN
            TDAYS = TDAYS * 24.0
            TDAYS = GAST + TDAYS * GSEC
            TDAYS = MOD (TDAYS, 24.0)
            TDAYS = TDAYS / 24.0
            END IF
         TIMMIN = MIN (TDAYS,TIMMIN)
         TIMMAX = MAX (TDAYS,TIMMAX)
C                                       Pick up some points
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Find scales
         IF (.NOT.NOSCAL) THEN
            DO 120 J = 1, NUMQAD
               IF (SCANV(J).NE.FBLANK) THEN
                  XYCLOS(1,J) = MIN (XYCLOS(1,J),SCANV(J))
                  XYCLOS(2,J) = MAX (XYCLOS(2,J),SCANV(J))
                  END IF
 120           CONTINUE
            END IF
         XUMVIS = XUMVIS + 1
         GO TO 100
C                                       Any points found
 200  IF (NOSCAL) GO TO 220
      IF (XUMVIS.LT.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
C
 220  IRET = 0
      CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      CALL COPY (256, CATKEP, CATBLK)
      XYOFF(1) = TIMMIN - 2 * ABS(DT) - 0.025 * (TIMMAX - TIMMIN)
      XYSCL(1) = TIMMAX + 2 * ABS(DT) + 0.025 * (TIMMAX - TIMMIN)
      CALL COPY (MXQUAD, NCLOSE, GOODCL)
      DECICL = .TRUE.
      KPLOT = 0
      DO 250 I = 1, NUMQAD
         IF (GOODCL(I).GT.0) KPLOT = KPLOT + 1
 250     CONTINUE
      IF (INDECL) THEN
         WRITE (MSGTXT,1110) KPLOT
      ELSE
         WRITE (MSGTXT,1120) KPLOT
         END IF
      CALL MSGWRT (6)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCALCL: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('SCALCL: ERROR',I3,' READING VIS FILE')
 1110 FORMAT (I4,' closure quadrangles contain data, all independent')
 1120 FORMAT (I4,' closure quadrangles contain data, ',
     *   'but not all independent')
 1200 FORMAT ('FOUND',I4,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE CLPLOT (MAXPLT, IRET)
C-----------------------------------------------------------------------
C   CLPLOT plots the closure amplitudes thru calls to PLOTCL.
C   Inputs:
C      MAXPLT    I    The maximum number of plots per page.
C   Outputs:
C      IRET      I     Error code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      INCLUDE 'CAPLT.INC'
      INTEGER   MAXPLT, IRET, IPLOT, I, PLOTBF(256), J
      REAL      DMS, DMO
      LOGICAL   CLOSED
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
      DMS = XYSCL(2)
      DMO = XYOFF(2)
C                                       trim the list
      J = 0
      DO 60 I = 1,NUMQAD
         IF ((GOODCL(I).GT.0) .AND. ((ABS(XYCLOS(1,I)).GE.AMPMIN)
     *      .OR. (ABS(XYCLOS(2,I)).GE.AMPMIN))) THEN
            J = J + 1
            GOODCL(J) = GOODCL(I)
            XYCLOS(1,J) = XYCLOS(1,I)
            XYCLOS(2,J) = XYCLOS(2,I)
            CALL COPY (4, CPQUAD(1,I), CPQUAD(1,J))
            END IF
 60      CONTINUE
      NUMQAD = J
 70   IF (MAXPLT.GT.1) THEN
         J = (NUMQAD-1)/MAXPLT + 1
         I = (NUMQAD-1)/(MAXPLT-1) + 1
         IF (I.EQ.J) THEN
            MAXPLT = MAXPLT - 1
            GO TO 70
            END IF
         END IF
C                                        Loop over the quadrulets.
      DO 100 I = 1,NUMQAD
         IF ((GOODCL(I).GT.0) .AND. ((ABS(XYCLOS(1,I)).GE.AMPMIN)
     *      .OR. (ABS(XYCLOS(2,I)).GE.AMPMIN))) THEN
            IPLOT = MOD (IPLOT, MAXPLT) + 1
            IF (I.EQ.NUMQAD) IPLOT = -IPLOT
            CALL PLOTCL (IPLOT, MAXPLT, DMS, DMO, I, CPQUAD(1,I),
     *         CLOSED, PLOTBF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
      IF (.NOT.CLOSED) CALL GFINIS (PLOTBF, IRET)
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLOTCL (IPLOT, MAXPLT, DMS, DMO, PLTTRP, CPQD, CLOSED,
     *   BUFFER, IRET)
C-----------------------------------------------------------------------
C   PLOTCL plots closures one panel at a time.
C   Input:
C      IPLOT    I    Plot number on current page. If negative then this
C                    the last plot.
C      MAXPLT   I    The number of plots per page.
C      PLTTRP   I    The number of the triplet being plotted
C      CPQD     I(4) The 4 antennas of the quadruplet
C   In/out:           (Changed to user value on .not.scalem(2))
C      DMS      R    Scaling parameter for max y range
C      DMO      R    Low value parm for max y range
C      BUFFER   I(*) Plot buffer
C   Output:
C      CLOSED   L    Plot file has been closed
C      IRET     I    Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, MAXPLT, PLTTRP, CPQD(4), BUFFER(256), IRET
      REAL      DMS, DMO
      LOGICAL   CLOSED
C
      INTEGER   MAXMOD
      PARAMETER (MAXMOD = 1000)
C
      INCLUDE 'CAPLT.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, CHTMP*18, UTYPE*2,
     *   ADATE*12, CTEMP*12, STAT*4, AUNITS(6)*8, CHTYPE(12)*8, POLLAB*5
      HOLLERITH CATH(256)
      INTEGER   I, IDUM, OBUFF(1024), VER, IERR, ITYPE, IPSIZE, NPARMS,
     *   LUNPL, FINDPL, IAPARM(8), INCHAR, INP, J, IARRW, IAXLAB,
     *   IAPLOT, IT(3), ID(3), SVAXTP(2), NGOOD, NNOFIT, NUMVIS, NUMQ,
     *   TIME(8), NPOSS(MAXQAD), NCLOSE(MAXQAD), ISYM, IMODEL,
     *   TRIANG(4,MAXQAD), SCANUM
      REAL      BLC(2), TRC(2), DX, DY, TR, TI, XY(2), CATR(256), SIZE,
     *   GERR, RPARM(20), XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2),
     *   XYZ(2), XMULT(2), XMIN, XMAX, YMIN, YMAX, XTEMP, TDAYS, DT,
     *   SCANV(MAXQAD), CLERR(MAXQAD), AX(5), AY(5), SCANM(1),
     *   MODEL(2,MAXMOD)
      DOUBLE PRECISION    TRA, TDEC, DTORAD, GSEC, GMST, GAST, RATE
      LOGICAL   T, F, GOOD, CATUP, KEEP, CHSTAT, NUSCAN, DO3C, DONE
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (CATUV, CATR, CATH)
      DATA LUNPL /26/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Janskys', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'GST hrs '/
      DATA CHTYPE /'Ampltude', 'Phase   ', 'UV dist ',
     *   'UV pa   ', 'Time    ', 'U       ', 'V      ',
     *   'W       ', 'Real    ', 'Imag    ','GST     ',
     *   'Time    '/
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      CLOSED = F
      ISYM = XSYM + 0.5
      IF ((ISYM.LE.0) .OR. (ISYM.GT.24)) ISYM = 23
      IF (XFACT.LT.0.01) XFACT = 1.0
      DO3C = .FALSE.
      DONE = .FALSE.
C                                       reset antenna list
      CALL FILL (MAXANT, 0, ANTENS)
      CALL COPY (4, CPQD, ANTENS)
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      CALL RFILL (20, 0.0, RPARM)
      RPARM(1) = FBLANK
      XSTOK = STOKES
      IBIF = BIF
      IF (IERR.EQ.-1) GO TO 220
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      UVSCAL = FREQ / UVFREQ
      DTORAD = 3.14159265358979326D0 / 180.0D0
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
      CATUP = T
      IARRW = ISUB
      IMODEL = 0
C                                       Determine status of file
      UTYPE = 'UV'
      CALL CATDIR ('INFO', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *   UTYPE, IDUM, STAT, OBUFF, IRET)
C                                       Change status
      CHSTAT = (STAT.NE.'WRIT')
      IF (CHSTAT) CALL STATCH (STAT, 'WRIT', DISKIN, OLDCNO, UTYPE,
     *   OBUFF, IRET)
C                                       User sets the scales
      DT = XSOLIN / 1440.0
      PLTINC = 1000. / MAXPLT
      XYOFF(1) = TIMMIN
      XYSCL(1) = TIMMAX
      IF (XYCLOS(1,PLTTRP).LT.0.1*XYCLOS(2,PLTTRP)) XYCLOS(1,PLTTRP)= 0.
      XYOFF(2) = XYCLOS(1,PLTTRP)
      XYSCL(2) = XYCLOS(2,PLTTRP)
      DO 10 I = 1,2
         IF (.NOT.SCALEM(I)) THEN
            XYSCL(I) = INISCL(2*I)
            XYOFF(I) = INISCL(2*I-1)
            END IF
         IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
         DX = XYSCL(I) - XYOFF(I)
         IF (I.EQ.1) THEN
            XYOFF(I) = XYOFF(I) - 2*ABS(DT) - 0.025 * DX
            XYSCL(I) = XYSCL(I) + 2*ABS(DT) + 0.025 * DX
            SIZE = 1000.
         ELSE
            XYOFF(I) = XYOFF(I) - 0.025 * DX
            XYSCL(I) = XYSCL(I) + 0.025 * DX + 0.1*DX
            SIZE = PLTINC
            END IF
         XYSCL(I) = SIZE / (XYSCL(I) - XYOFF(I))
         IF (I.NE.1) THEN
            DMS = XYSCL(I)
            DMO = XYOFF(I)
            END IF
 10      CONTINUE
C                                       Fill in last of actual parms
      BPARM(5) = 1000.0 / XYSCL(1) + XYOFF(1)
      BPARM(7) = PLTINC / XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
      IF (FLOTEM) BPARM(6) = 0.0
      IF (FLOTEM) BPARM(7) = 0.0
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
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
      LOCNUM = 1
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
      IF (TYPEAX(2).NE.12) THEN
         CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
         XMULT(2) = TR / TI
      ELSE
         XMULT(2) = 360.0
         END IF
      TR = 1000. / XYSCL(1)
      TI = TR
      CPREF(1,LOCNUM) = ' '
      IF (TYPEAX(1).NE.12) THEN
         CALL METSCL (LABEL, TR, CPREF(1,LOCNUM), GOOD)
         XMULT(1) = TR / TI
      ELSE
         XMULT(1) = 360.0
         END IF
      DO 50 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
         IF (TYPEAX(I).GT.5) CTYP(I,LOCNUM) = AUNITS(3)
         IF (TYPEAX(I).LE.5) CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
         IF ((TYPEAX(I).EQ.9) .OR. (TYPEAX(I).EQ.10))
     *      CTYP(I,LOCNUM) = AUNITS(1)
         IF (TYPEAX(I).EQ.11) CTYP(I,LOCNUM) = AUNITS(6)
 50      CONTINUE
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
         ITYPE = 35
         NPARMS = 279
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
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 4
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
         WRITE (TEXT,1040) BIF, EIF, BCHAN, ECHAN, POLLAB, VER2
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Plot type and file
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = 'Closure Amplitude _vs_ ' // CHTYPE(TYPEAX(1))(:8) //
     *      ' for_'
         CHTMP = NAMEIN // CLAIN
         CALL NAMEST (CHTMP, CATUV(KIIMS), TEXT(37:), INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF (LABEL.GT.1) THEN
            DY = DY + 1.333
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1030) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Blank bottom label.
      IF ((IPLOT.LT.0) .OR. (ABS (IPLOT).EQ.MAXPLT)) GO TO 55
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
C                                       Only label Y axis once.
 55   IAXLAB = MAXPLT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IAPLOT.EQ.IAXLAB) .OR. ((IPLOT.LT.0) .AND.
     *   (IAPLOT.LE.IAXLAB))) GO TO 60
         CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
 60   CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      NUMVIS = 0
C                                       Set up for closure phases
      NUMQ = 1
      TRIANG(1,1) = CPQD(1)
      TRIANG(2,1) = CPQD(2)
      TRIANG(3,1) = CPQD(3)
      TRIANG(4,1) = CPQD(4)
      NPOSS(1) = 0
      NCLOSE(1) = 0
      DX = 5.0 * XFACT
      DY = 5.0 * XFACT
      IF (DX/XYRATO.LT.2.5) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
C                                       GST plotting?
      IF (BPARM(1).EQ.11.0) THEN
         CALL GSTROT (JDREF, GMST, GAST, RATE)
         GAST = GAST * (24.D0/360.D0)
         GAST = MOD (GAST, 24.D0)
         END IF
C                                       Loop
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
 100  CONTINUE
C                                       Read vis. record and form
C                                       closure phase
         CALL CLOSAV (NUMVIS, NUMQ, TRIANG, DT, SCANV, CLERR, DOERRB,
     *      TIME, TDAYS, NUSCAN, SCANUM, RPARM, UVSCAL, BUFF1, NPOSS,
     *      NCLOSE, DOMODL, SCANM, IERR)
         IF (IERR.EQ.-2) GO TO 100
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            GO TO 200
            END IF
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (8)
            IRET = 4
            GO TO 970
            END IF
C
         NUMVIS = NUMVIS + 1
         KEEP = MOD(NUMVIS,INC) .EQ. 0
         IF (.NOT. KEEP) GO TO 100
C                                       Get and scale X, Y
         IF (BPARM(1).EQ.11.0) THEN
            TDAYS = TDAYS * 24.0
            TDAYS = GAST + TDAYS * GSEC
            TDAYS = MOD (TDAYS, 24.0)
            TDAYS = TDAYS / 24.0
            END IF
         XYZ(1) = TDAYS
         XYZ(2) = SCANV(1)
         IF (DOERRB) GERR = CLERR(1) * XYSCL(2)
         DO 120 J = 1,2
            XY(J) = XYSCL(J) * (XYZ(J)-XYOFF(J)) + YYOFF(J)
            IF ((XY(J).LT.XBLC(J)) .OR. (XY(J).GT.XTRC(J))) THEN
               NNOFIT = NNOFIT + 1
               GO TO 100
               END IF
 120        CONTINUE
         NGOOD = NGOOD + 1
C                                       Mark the point
         IF (DOERRB) THEN
            DY = 5.0 * XFACT
            IF (GERR.GT.5.0) DY = GERR * XFACT
            END IF
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
         CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE., DO3C,
     *      BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (DOMODL.EQ.2) THEN
            IMODEL = IMODEL + 1
            MODEL(1,IMODEL) = TDAYS
            MODEL(2,IMODEL) = SCANM(1)
            END IF
         IF (IMODEL.LE.MAXMOD) GO TO 100
C                                       plot the current bits of model
 200  IF (IMODEL.GT.1) THEN
         CALL GLTYPE (3, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         GOOD = .FALSE.
         DO 210 I = 1,IMODEL
            DO 205 J = 1,2
               XY(J) = XYSCL(J) * (MODEL(J,I)-XYOFF(J)) + YYOFF(J)
 205           CONTINUE
            IF ((XY(1).GE.XBLC(1)) .AND. (XY(1).LE.XTRC(1)) .AND.
     *         (XY(2).GE.XBLC(2)) .AND. (XY(2).LE.XTRC(2))) THEN
               IF (GOOD) THEN
                  CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
               ELSE
                  CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               GOOD = .TRUE.
            ELSE
               GOOD = .FALSE.
               END IF
 210        CONTINUE
         IF (.NOT.DONE) THEN
            MODEL(1,1) = MODEL(1,IMODEL)
            MODEL(2,1) = MODEL(2,IMODEL)
            IMODEL = 1
            CALL GLTYPE (4, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            GO TO 100
            END IF
         END IF
C                                     Write quadrangle name on plot.
      TEXT = STANAM(CPQD(1))(1:8) // '-' // STANAM(CPQD(2))(1:8) //
     *   '-' // STANAM(CPQD(3))(1:8) // '-' // STANAM(CPQD(4))(1:8)
      CALL DEFRMT (TEXT, ' ', INP)
      DX = 1.5
      DY = -1.5
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (TLC(1), TLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GICHAR (1, INP, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (TEXT,1210) CPQD
      CALL DEFRMT (TEXT,' ', INP)
      DX = -1.5 - INP
      CALL GPOS (XTRC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GICHAR (1, INP, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Check if changed status
 220  IF (CHSTAT) CALL STATCH ('WRIT', STAT, DISKIN, OLDCNO, UTYPE,
     *   OBUFF, IRET)
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Plot model
      IF (DOMODL.EQ.1) THEN
         TRA = RA
         TDEC = DEC
         IF ((TRA.EQ.0.0D0) .AND. (TDEC.EQ.0.0D0)) THEN
            TRA = OBSRA
            TDEC = OBSDEC
            END IF
         CALL VBMDL (TRA, TDEC, XMIN, XMAX, YMIN, YMAX, YYOFF,
     *      TYPEAX(1), TYPEAX(2), CPQD, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      CALL COPY (2, SVAXTP, TYPEAX)
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD, CPQD(1), CPQD(2), CPQD(3), CPQD(4)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1201) NNOFIT, CPQD(1), CPQD(2), CPQD(3), CPQD(4)
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      IF ((IPLOT.GT.0) .AND. (ABS (IPLOT).LT.MAXPLT)) GO TO 230
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         CLOSED = T
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
 230  IF (IERR.GT.0) GO TO 975
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
      WRITE (MSGTXT,1200) NGOOD, CPQD(1), CPQD(2), CPQD(3), CPQD(4)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1201) NNOFIT, CPQD(1), CPQD(2), CPQD(3), CPQD(4)
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      CLOSED = T
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
 1030 FORMAT ('PLot file version',I4,'__created ',A,A)
 1040 FORMAT ('IF',I3,'-',I2,' CH',I5,'-',I4,' STK ',A5,' CC ver ',I3)
 1050 FORMAT ('PLOTCL: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('PLOTCL: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('PLOTCL: ',I9,' points plotted: quadrangle ',
     *   I2,'-',I2,'-',I2,'-',I2)
 1201 FORMAT ('PLOTCL: ',I9,' points did not fit: quadrangle ',
     *   I2,'-',I2,'-',I2,'-',I2)
 1210 FORMAT (3(I6,'-'),I6)
 1960 FORMAT ('PLOTCL: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLOTCL: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE CLOSET (XANT, NUMQ, CPQD, IRET)
C-----------------------------------------------------------------------
C   Routine to set up the list of antennas (ANTENS) for UVGET that
C   includes all antennas mentioned in XANT.
C   Input:
C      XANT     R(*)     Antenna array
C   Output:
C      NUMQ     I        The number of quadruplets selected.
C      CPQD     I(4,*)   The antenna numbers in each quadruplet.
C                           numbers of each baseline selected.
C      IRET     I        Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS   I(*)     Selected antenna numbers
C-----------------------------------------------------------------------
      REAL      XANT (*)
      INTEGER   NUMQ, CPQD(4,*), IRET
C
      INTEGER   I, NEXT, NA, J, K, KK, AA(4)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PCAPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set antenna list.
      CALL FILL (MAXANT, 0, ANTENS)
      NEXT = 0
C                                       Find triplets requested
      NUMQ = 0
      NA = MIN (50, MAXANT)
      DO 20 I = 1,NA,4
         IF ((XANT(I).GT.0.0) .AND. (NUMQ.LT.MAXQAD)) THEN
            AA(1) = XANT(I) + 0.5
            AA(2) = XANT(I+1) + 0.5
            AA(3) = XANT(I+2) + 0.5
            AA(4) = XANT(I+3) + 0.5
C                                       Check validity
            IF ((AA(1).GT.0) .AND. (AA(2).GT.0) .AND. (AA(3).GT.0) .AND.
     *         (AA(4).GT.0) .AND. (AA(1).NE.AA(2)) .AND.
     *         (AA(1).NE.AA(3)) .AND. (AA(1).NE.AA(4)) .AND.
     *         (AA(2).NE.AA(3)) .AND. (AA(2).NE.AA(4)) .AND.
     *         (AA(3).NE.AA(4))) THEN
C                                       List antennas in ascending order
               NUMQ = NUMQ + 1
               DO 15 J = 1,4
                  KK = 0
                  CPQD(J,NUMQ) = 999
                  DO 10 K = 1,4
                     IF (CPQD(J,NUMQ).GT.AA(K)) THEN
                        CPQD(J,NUMQ) = AA(K)
                        KK = K
                        END IF
 10                  CONTINUE
                  AA(KK) = 99999
 15               CONTINUE
C                                       Selected antennas:
               CALL COPY (4, CPQD(1,NUMQ), ANTENS(NEXT+1))
               NEXT = NEXT + 4
               END IF
            END IF
 20      CONTINUE
C                                       Remove duplicate antennas
      CALL IMERGE (NEXT, ANTENS, NEXT)
C                                       Make sure some quadruplets were
C                                       specified.
      IF (NUMQ.LE.0) THEN
         MSGTXT = 'NO QUADRUPLETS SPECIFIED!'
         IRET = 5
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLAVER (OP, NUMQ, CPQD, T1, DT, DOERRB, DOMODL, RPARM,
     *   VIS)
C-----------------------------------------------------------------------
C   Adds a sample in to, or zeros, the summing arrays
C-----------------------------------------------------------------------
      CHARACTER OP*(*)
      INTEGER   NUMQ, CPQD(4,*), DOMODL
      LOGICAL   DOERRB
      REAL      T1, DT, RPARM(*), VIS(3,*)
C
      INTEGER   I, IVIS, KVIS, JA1, JA2, IDAY, STTRIP, ITRIP, IBASE
      REAL      CT, TEMP
      DOUBLE PRECISION X8
      INCLUDE 'PCAPLT.INC'
      INCLUDE 'CLAVER.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       zero
      IF (OP.EQ.'ZERO') THEN
         TLAST = -1.0
         DTUTC = DATUTC / 86400.0
         IVSCNT = 0
         CALL FILL (NUMQ, 0, COUNTA)
         CALL RFILL (NUMQ, 0.0, WORKC)
         I = NUMQ * 4
         CALL FILL (I, 0, COUNT)
         CALL FILL (I, 0, COUNT1)
         CALL RFILL (I, 0.0, GAMP)
         CALL RFILL (I, 0.0, GERR)
         I = 2 * I
         CALL RFILL (I, 0.0, WORK)
         I = NUMQ * 4 * 3
         CALL RFILL (I, 0.0, UVW)
         CNTTIM = 0
         SUMTIM = 0.0
C                                       add one vis into arrays:
      ELSE
         KVIS = (LREC-NRPARM) / 3
C                                       Set up first time boundary
         CT = RPARM(ILOCT+1) - DTUTC
         IVSCNT = IVSCNT + 1
         IF (IVSCNT.EQ.1) THEN
            IDAY = CT
            X8 = (CT - IDAY) / ABS (DT)
            TLAST = IDAY + DINT (X8) * ABS (DT) + ABS (DT)
            END IF
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         STTRIP = 1
C                                       Return to here to look for
C                                       further quadruplets involving
C                                       this baseline.
C                                       Find quadruplet and member
 105     DO 110 I = STTRIP,NUMQ
            ITRIP = I
            IBASE = 1
            IF ((JA1.EQ.CPQD(1,I).AND.(JA2.EQ.CPQD(2,I)))) GO TO 120
            IBASE = 2
            IF ((JA1.EQ.CPQD(3,I).AND.(JA2.EQ.CPQD(4,I)))) GO TO 120
            IBASE = 3
            IF ((JA1.EQ.CPQD(1,I).AND.(JA2.EQ.CPQD(3,I)))) GO TO 120
            IBASE = 4
            IF ((JA1.EQ.CPQD(2,I).AND.(JA2.EQ.CPQD(4,I)))) GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 999
C                                       wanted
 120     GOTDAT = .TRUE.
         STTRIP = ITRIP + 1
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
         DO 130 IVIS = 1,KVIS
            IF (VIS(3,IVIS).GT.0.0) THEN
               COUNT(IBASE,ITRIP) = COUNT(IBASE,ITRIP) + 1
               COUNT1(IBASE,ITRIP) = COUNT1(IBASE,ITRIP) + 1
               WORK(1,IBASE,ITRIP) = WORK(1,IBASE,ITRIP) +
     *            VIS(1,IVIS)
               WORK(2,IBASE,ITRIP) = WORK(2,IBASE,ITRIP) +
     *            VIS(2,IVIS)
               IF (DOERRB) THEN
                  GAMP(IBASE,ITRIP) = GAMP(IBASE,ITRIP) +
     *               SQRT (VIS(1,IVIS)*VIS(1,IVIS) + VIS(2,IVIS)*
     *               VIS(2,IVIS))
                  GERR(IBASE,ITRIP) = GERR(IBASE,ITRIP) +
     *               SQRT (1.0/VIS(3,IVIS))
                  END IF
               IF (DOMODL.EQ.2) THEN
                  UVW(1,IBASE,ITRIP) = UVW(1,IBASE,ITRIP) +
     *               RPARM(1+ILOCU)
                  UVW(2,IBASE,ITRIP) = UVW(2,IBASE,ITRIP) +
     *               RPARM(1+ILOCV)
                  UVW(3,IBASE,ITRIP) = UVW(3,IBASE,ITRIP) +
     *               RPARM(1+ILOCW)
                  END IF
               END IF
 130        CONTINUE
C                                       averaging closure phases
         IF (DT.LT.0.0) THEN
C                                       completed a triangle
            IF ((COUNT1(1,ITRIP).GT.0) .AND. (COUNT1(2,ITRIP).GT.0)
     *         .AND. (COUNT1(3,ITRIP).GT.0).AND. (COUNT1(4,ITRIP).GT.0))
     *         THEN
               TEMP = SQRT (WORK(1,3,ITRIP)**2 + WORK(2,3,ITRIP)**2) *
     *            SQRT (WORK(1,4,ITRIP)**2 + WORK(2,4,ITRIP)**2) /
     *            (COUNT1(3,ITRIP) * COUNT1(4,ITRIP))
               IF (TEMP.NE.0.0) THEN
                  WORKC(ITRIP) = WORKC(ITRIP) +
     *               SQRT (WORK(1,1,ITRIP)**2 + WORK(2,1,ITRIP)**2) *
     *               SQRT (WORK(1,2,ITRIP)**2 + WORK(2,2,ITRIP)**2) /
     *               (COUNT1(3,ITRIP) * COUNT1(4,ITRIP)) / TEMP
                  COUNTA(ITRIP) = COUNTA(ITRIP) + 1
                  END IF
               CALL RFILL (8, 0.0, WORK(1,1,ITRIP))
               CALL FILL (4, 0, COUNT1(1,ITRIP))
               END IF
            END IF
C                                       This baseline may be involved in
C                                       more triplets.
         GO TO 105
C
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLOSAV (NUMVIS, NUMQ, CPQD, DT, SCANV, CLERR, DOERRB,
     *   TIME, T1, NUSCAN, SCANUM, RPARM, UVSCAL, VIS, NPOSS, NCLOSE,
     *   DOMODL, MODELV, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of closure phases for
C   selected triplets.  The triplets are specified in array CPQD.
C   Needs to be initialized by a call to UVGET.
C   Inputs:
C     NUMVIS     I    Current visibility number
C     NUMQ       I    The number of quadruplets selected
C     CPQD       I(3,*) The antenna numbers involved in the triplets.
C     DT         R    Averaging time in days
C     DOERRB     L    If true calculate the closure phase error
C     UVSCAL     R    Scale factor for uv data
C   Input/Output:
C     RPARM      R(*) Random parameter array, first record of call.
C                     (1) = 'INDE' => don't use.
C     VIS        R(3,*) Visibility array, first record of call.
C     NPOSS      I(*) Number of visibility points for which closure
C                     phases may be possible.
C     NCLOSE     I(*) Number of visibility points for which closure
C                     phases are formed.
C   Outputs:
C     SCANV      R(*) The closure phase values for the selected
C                     triplets, corresponds to CPQD.
C                     Undefined values will contain 'INDE'.
C     CLERR      R(*) The formal error associated with the closure
C                     phase, calculated as CLERR = SQRT (err(12)**2
C                     + err(13)**2 + err(23)**2)
C     TIME(8)    I    Time range, start, stop; days, hours, min, sec.
C                     Unless NUSCAN only first 4 values are set.
C     TDAYS      R    Time of current record in days
C     NUSCAN     L    True IF the first record in a new scan.
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in DSOU.INC
C     SNAME      C*16 Source name
C     QUAL       I    Source qualifier.
C     CALCOD     C*4  Calibrator code
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INTEGER   NUMQ, CPQD(4,*), TIME(8), SCANUM, NPOSS(*), NUMVIS,
     *   NCLOSE(*), DOMODL, IERR
      LOGICAL   NUSCAN, DOERRB
      REAL      RPARM(*), UVSCAL, VIS(3,*), DT, SCANV(*), CLERR(*),
     *   TDAYS, MODELV(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PCAPLT.INC'
      LOGICAL   GOOD1
      INTEGER   I, SUNUM, JERR, ISLUN, KVIS, IDAY, J, K
      REAL      T1, CP, CT, TEMP
      DOUBLE PRECISION X8
      INCLUDE 'CLAVER.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA ISLUN /26/
C-----------------------------------------------------------------------
      IF (NUMVIS.EQ.0) CALL CLAVER ('ZERO', NUMQ, CPQD, T1, DT, DOERRB,
     *   DOMODL, RPARM, VIS)
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
C                                       Initialize time
      T1 = 1.0E10
      KVIS = (LREC-NRPARM) / 3
C                                       Loop reading data
 100  CONTINUE
         CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         IF (IERR.EQ.-1) GO TO 200
         CT = RPARM(ILOCT+1) - DTUTC
C                                       scale u,v,w
         IF (UVSCAL.NE.1.0) THEN
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            END IF
C                                       Set up first time boundary
         IF (IVSCNT.EQ.0) THEN
            IDAY = CT
            X8 = (CT - IDAY) / ABS (DT)
            TLAST = IDAY + DINT (X8) * ABS (DT) + ABS (DT)
            END IF
C                                       Check if avg. or scan done
         IF ((INXRNO.LE.SCANUM) .AND. (CT.LE.TLAST)) THEN
            CALL CLAVER ('AVER', NUMQ, CPQD, T1, DT, DOERRB, DOMODL,
     *         RPARM, VIS)
            IF (GOTDAT) SUNUM = CURSOU
            GO TO 100
            END IF
C                                       Integration done:
C                                       Go through sums
 200  GOOD1 = .FALSE.
      IF (GOTDAT) THEN
C                                       Vector averaging
         CP = TWOPI
         DO 210 I = 1,NUMQ
            NPOSS(I) = NPOSS(I) + 1
            IF ((COUNT(1,I).GT.0) .AND. (COUNT(2,I).GT.0) .AND.
     *         (COUNT(3,I).GT.0) .AND. (COUNT(4,I).GT.0)) THEN
               IF (DT.GT.0.0) THEN
                  SCANV(I) = SQRT (WORK(1,1,I)**2 + WORK(2,1,I)**2) *
     *               SQRT (WORK(1,2,I)**2 + WORK(2,2,I)**2) /
     *               (COUNT(1,I) * COUNT(2,I))
                  TEMP = SQRT (WORK(1,3,I)**2 + WORK(2,3,I)**2) *
     *               SQRT (WORK(1,4,I)**2 + WORK(2,4,I)**2) /
     *               (COUNT(3,I) * COUNT(4,I))
               ELSE
                  SCANV(I) = WORKC(I)
                  TEMP = COUNTA(I)
                  END IF
               IF (TEMP.NE.0.0) THEN
                  SCANV(I) = SCANV(I) / TEMP
                  IF (DOERRB) THEN
                     GERR(1,I) = GERR(1,I) / COUNT(1,I)
                     GERR(2,I) = GERR(2,I) / COUNT(2,I)
                     GERR(3,I) = GERR(3,I) / COUNT(3,I)
                     GERR(4,I) = GERR(4,I) / COUNT(4,I)
                     CLERR(I) = SQRT (
     *                  GERR(1,I)*GERR(1,I)/MAX(1,COUNT(1,I)-1) +
     *                  GERR(2,I)*GERR(2,I)/MAX(1,COUNT(2,I)-1) +
     *                  GERR(3,I)*GERR(3,I)/MAX(1,COUNT(3,I)-1) +
     *                  GERR(4,I)*GERR(4,I)/MAX(1,COUNT(4,I)-1))
                     END IF
                  IF (DOMODL.EQ.2) THEN
                     DO 205 K = 1,4
                        DO 204 J = 1,3
                           UVW(J,K,I) = UVW(J,K,I) / COUNT(K,I)
 204                       CONTINUE
 205                    CONTINUE
                     CALL MODPTS (UVW(1,1,I), MODELV(1))
                     END IF
                  GOOD1 = .TRUE.
                  NCLOSE(I) = NCLOSE(I) + 1
               ELSE
                  SCANV(I) = FBLANK
                  CLERR(I) = FBLANK
                  END IF
            ELSE
               SCANV(I) = FBLANK
               CLERR(I) = FBLANK
               END IF
 210        CONTINUE
         END IF
C                                       have some results
      IF (GOOD1) THEN
C                                       Get source info
         IF (NUSCAN) THEN
            CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
            IF (JERR.EQ.11) THEN
               WRITE (MSGTXT,1750) SUNUM
               CALL MSGWRT (8)
               JERR = 0
               END IF
            IF (JERR.GT.0) THEN
               IERR = JERR
               WRITE (MSGTXT,1700) JERR
               GO TO 990
               END IF
            END IF
C                                       Time
         T1 = 0
         IF (CNTTIM.GT.0) T1 = SUMTIM / CNTTIM
         TDAYS = T1
         CALL TODHMS (T1, TIME)
         END IF
C                                       save current sample
      IF (IERR.EQ.0) THEN
         CALL CLAVER ('ZERO', NUMQ, CPQD, T1, DT, DOERRB, DOMODL, RPARM,
     *      VIS)
         CALL CLAVER ('AVER', NUMQ, CPQD, T1, DT, DOERRB, DOMODL, RPARM,
     *      VIS)
         END IF
C                                       End of data, calling routine
C                                       will close.
      IF (.NOT.GOOD1) THEN
         IF ((IERR.EQ.0) .AND. (SCANUM.GE.INXRNO)) GO TO 100
         IF (IERR.EQ.0) IERR = -2
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('CLOSAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('CLOSAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE INDCLT (NANT, NUMQ, CPQD, IRET)
C-----------------------------------------------------------------------
C   Routine to set up the list of antennas (ANTENS) for UVGET that
C   includes all antennas mentioned in XANT.
C   Input:
C      NANT   I        Number of antennas in array, more importantly
C                      gives us the maximum antennas number
C   Output:
C      NUMQ   I        The number of quadruplets selected.
C      CPQD   I(4,*)   The antenna numbers in each quadruplet
C      IRET   I        Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   NANT, NUMQ, CPQD(4,*), IRET
C
      INTEGER   I, J, A1, NUMIND
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       What we should get
      NUMIND = (NANT-1) * (NANT-2) / 2
C                                       Set antenna list.
      CALL FILL (MAXANT, 0, ANTENS)
C                                       To determine the # of
C                                       independent quadrangles use
C                                       following algorithm:
C                                       for j = 4, nant
C                                         tria = 1, 2, 3, j
C                                                2, 3, 4, j
C                                                3, 4, 5, j
C                                                   .
C                                                   .
C                                             j-3, j-2, j-1, j
C
      NUMQ = 0
      DO 100 J = 3, NANT
         A1 = 1
         DO 50 I = 1, NANT
            IF (A1.LT.(J-2)) THEN
               NUMQ = NUMQ + 1
               CPQD(1,NUMQ) = A1
               CPQD(2,NUMQ) = A1 + 1
               CPQD(3,NUMQ) = A1 + 2
               CPQD(4,NUMQ) = J
               END IF
            A1 = A1 + 1
 50         CONTINUE
 100     CONTINUE
C
      DO 200 I = 1,NANT
         ANTENS(I) = I
 200     CONTINUE
C                                       Make sure some triplets were
C                                       specified.
      IF (NUMQ.LE.0) THEN
         MSGTXT = 'NO QUADRUPLETS SPECIFIED!'
         IRET = 5
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE MODSET (XMIN, XMAX, MPLO, NPLO, DRA, DDEC, CPQD,
     *   XDATA, YDATA, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set up model values for a given set of clean
C   components and for a given pair of antennas, or closure quadruple
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
C          CPQD(4)   I     defines the 4 antennas of the quadruplet
C   Input from common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X,Y,Z pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      REAL      XMIN, XMAX, XDATA(*), YDATA(2,*)
      INTEGER   NPLO, MPLO, CPQD(4), IERR
      DOUBLE PRECISION DRA, DDEC
C
      DOUBLE PRECISION    U(6), V(6), W(6), PI, TWOPI, DTR, HTR, FREP,
     *   BX(6), BY(6), BZ(6), B1(6), B2(6), GH(6), GMST, GAST, RA,
     *   DEC, GSEC, AA, RATE, XBL(4), YBL(4), ZBL(4)
      REAL      XXPOS, XDEL, XREL, HAI, CSUM, SSUM, TEMP, AMODL,
     *   FAZ, ARG, FTEMP, CLOMOD(6)
      INTEGER   I, K, IBAS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CAPLT.INC'
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      IF ((MPLO.LT.1) .OR. (MPLO.GT.15)) GO TO 990
C                                       program constants
      PI = 3.14159265358979D0
      TWOPI = 2.D0 * PI
      DTR = PI / 180.D0
      HTR = PI / 12.D0
      U(1) = 0.0D0
      V(1) = 0.0D0
      W(1) = 0.0D0
      GAST = 0.0D0
      IERR = 1
      RA = DRA * DTR
      DEC = DDEC * DTR
      FREP = AFREQ
      DO 10 I = 1,4
         XBL(I) = XB(CPQD(I))
         YBL(I) = YB(CPQD(I))
         ZBL(I) = ZB(CPQD(I))
 10      CONTINUE
C                                     divide X-axis into 200 pixels
      XDEL = (XMAX - XMIN) / 200.0
      XXPOS = XMIN - XDEL
C                                     if this is a IAT-plot:
C                                     compute GST at first time
C                                     interval
      IF ((MPLO.EQ.5) .OR. (MPLO.EQ.11) .OR. (MPLO.EQ.12)) THEN
         CALL GSTROT (JDREF, GMST, GAST, RATE)
         GAST = GAST * (24.D0/360.D0)
         GAST = GAST + ((XMIN*24.D0) * GSEC)
         GAST = MOD (GAST, 24.D0)
         END IF
C                                     determine baseline parms
      CALL BASLIN (4, XBL, YBL, ZBL, FREP, ARRLON, BX, BY, BZ,
     *   B1, B2, GH)
C
      XREL = XDEL
      IF ((MPLO.EQ.5).OR.(MPLO.EQ.12)) XDEL = XDEL * 24.0
      GAST = GAST - GSEC*XDEL
      ARG = -10.0
C                                       closure plots here
      DO 700 I = 1,200
         YDATA(1,I) = FBLANK
         YDATA(2,I) = FBLANK
         XXPOS = XXPOS + XREL
         XDATA(I) = XXPOS
C                                       IAT in days
         IF ((MPLO.EQ.5) .OR. (MPLO.EQ.12)) THEN
            GAST = GAST + GSEC*XDEL
         ELSE IF (MPLO.EQ.11) THEN
            GAST = XDATA(I) * 24.D0
         ELSE
            GO TO 710
            END IF
C                                       Now need to compute model on
C                                       4 baselines and combine to
C                                       form closure model.
C                                       u,v,w from HA
         DO 600 IBAS = 1,6
            HAI = GAST*HTR - RA - GH(IBAS)
            U(IBAS) = B2(IBAS) * SIN(HAI)
            V(IBAS) = B1(IBAS) * COS(DEC) -
     *          B2(IBAS) * SIN(DEC) * COS(HAI)
            W(IBAS) = B1(IBAS) * SIN(DEC) +
     *          B2(IBAS) * COS(DEC) * COS(HAI)
C
            CSUM = 0.0
            SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
            IF (DOSPHE) THEN
               DO 510 K = 1,NUMCC
                  AA = GAUSA(K) * SQRT (U(IBAS)*U(IBAS) +
     *                V(IBAS)*V(IBAS))
                  IF (AA.LT.6.28D-2) AA = 6.28D-2
                  FTEMP = 3.0D0 * SFLUX(K) *
     *               ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
                  TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K) +
     *               W(IBAS) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 510              CONTINUE
C                                       Gaussian
            ELSE IF (DOGAUS) THEN
               DO 520 K = 1,NUMCC
                  ARG = U(IBAS)*U(IBAS)*GAUSA(K) +
     *               U(IBAS)*V(IBAS)*GAUSB(K) +
     *               V(IBAS)*V(IBAS)*GAUSC(K)
                  IF (ARG.GT.-8.0) THEN
                     FTEMP = SFLUX(K) * EXP(ARG)
                     TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K)
     *                  + W(IBAS) * CCPOS(3,K)
                     SSUM = SSUM + FTEMP * SIN (TEMP)
                     CSUM = CSUM + FTEMP * COS (TEMP)
                     END IF
 520              CONTINUE
C                                       CC's
            ELSE
               DO 550 K = 1,NUMCC
                  FTEMP = SFLUX(K)
                  TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K) +
     *               W(IBAS) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 550              CONTINUE
               END IF
C                                       Finished computing model
            AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
            IF (NPLO.EQ.1) CLOMOD(IBAS) = AMODL
            IF (NPLO.EQ.9) CLOMOD(IBAS) = CSUM
            IF (NPLO.EQ.10) CLOMOD(IBAS) = SSUM
            IF ((NPLO.EQ.2) .AND. (AMODL.GT.1.0E-10)) THEN
               FAZ = ATAN2 (SSUM, CSUM) / DTR
 560           IF (FAZ.GT.180.0E0) FAZ = FAZ - 360.0E0
               IF (FAZ.LT.-180.0E0) FAZ = FAZ + 360.0E0
               IF (FAZ.GT.180.0E0) GO TO 560
               IF (FAZ.LT.-180.0E0) GO TO 560
               CLOMOD(IBAS) = FAZ
               END IF
 600        CONTINUE
         TEMP = CLOMOD(2) * CLOMOD(5)
         IF (TEMP.NE.0) THEN
            YDATA(1,I) = CLOMOD(1) * CLOMOD(6) / TEMP
         ELSE
            YDATA(1,I) = FBLANK
            END IF
 700     CONTINUE
 710     IERR = 0
      GO TO 999
C                                       bad X-axis type
 990  WRITE (MSGTXT,1990) MPLO
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('MODSET: BAD X-AXIS CODE =',I6)
      END
      SUBROUTINE MODORB (XMIN, XMAX, MPLO, DRA, DDEC, CPQD, XDATA,
     *   YDATA, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set up model values for a given set of clean
C   components and for a given pair of antennas, or closure quadrangle
C   if one or both antennas is at an orbit of Earth satalite.
C
C   Inputs:
C      XMIN,XMAX      R    min and max values of the X-axis
C      MPLO           I    type of X-axis: 12 --> time (hms)
C                                          otherwise not plot
C      DRA,DDEC       D     Ra and Dec of the source at the epoch of
C                           observation (in degrees, and as D).
C      CPQD(4)        I     if plotting closure phases, defines the
C                           3 antennas of the triplet
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
C      IORBIT         I(*)  Array of satalites number.
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
      DOUBLE PRECISION    U(4), V(4), W(4), HAI, TIME, GMST, GAST, RA,
     *   X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, BAX(4), BAY(4),
     *   BAZ(4), DEC, DRA, DDEC, GSEC, AA, RATE, CLOMOD(4), ORBIT1(6),
     *   ORBIT2(6), ORBIT3(6), ORBIT4(6), VX, VY, VZ
      REAL      XXPOS, XDEL, CSUM, SSUM, TEMP, AMODL, FAZ, ARG, XMIN,
     *   XMAX, XDATA(*), YDATA(2,*), FTEMP
      INTEGER   MPLO, I, K, IERR, NUV, CPQD(4), IORB1, IORB2, IORB3,
     *   IORB4, IT1, IT2, IT3, IT4, IBAS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'CAPLT.INC'
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      IERR = 1
      IT1 = CPQD(1)
      IT2 = CPQD(2)
      IT3 = CPQD(3)
      IT4 = CPQD(4)
      IORB1 = IORBIT(IT1)
      IORB2 = IORBIT(IT2)
      IORB3 = IORBIT(IT3)
      IORB4 = IORBIT(IT4)
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
      ORBIT3(1) = ORBITA(1 + (IORB3-1)*6)
      ORBIT3(2) = ORBITA(2 + (IORB3-1)*6)
      ORBIT3(3) = ORBITA(3 + (IORB3-1)*6)
      ORBIT3(4) = ORBITA(4 + (IORB3-1)*6)
      ORBIT3(5) = ORBITA(5 + (IORB3-1)*6)
      ORBIT3(6) = ORBITA(6 + (IORB3-1)*6)
      ORBIT4(1) = ORBITA(1 + (IORB4-1)*6)
      ORBIT4(2) = ORBITA(2 + (IORB4-1)*6)
      ORBIT4(3) = ORBITA(3 + (IORB4-1)*6)
      ORBIT4(4) = ORBITA(4 + (IORB4-1)*6)
      ORBIT4(5) = ORBITA(5 + (IORB4-1)*6)
      ORBIT4(6) = ORBITA(6 + (IORB4-1)*6)
      X1 = XB(IT1)
      Y1 = YB(IT1)
      Z1 = ZB(IT1)
      X2 = XB(IT2)
      Y2 = YB(IT2)
      Z2 = ZB(IT2)
      X3 = XB(IT3)
      Y3 = YB(IT3)
      Z3 = ZB(IT3)
      X4 = XB(IT4)
      Y4 = YB(IT4)
      Z4 = ZB(IT4)
C                                       X can be only time;
      IF (MPLO.NE.12) GO TO 990
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
         NUV = 1
         TIME = TIME + XDEL
C                                       reference time at the current
C                                       point, in days
         GAST = GAST + 360.0D0*XDEL*GSEC
C                                       GAST in degrees at the current
C                                       time
         HAI = GAST * DG2RAD
C                                       find the baseline cartesian
C                                       coordinates: X-RA, Z-North
C                                       baseline 1-2
         CALL BACOOR (IORB1, IORB2, ORBIT1, ORBIT2, X1, Y1, Z1,
     *      X2, Y2, Z2, HAI, TIME, BAX(1), BAY(1), BAZ(1), VX, VY, VZ)
C                                       baseline 3-4
         CALL BACOOR (IORB3, IORB4, ORBIT3, ORBIT4, X3, Y3, Z3,
     *      X4, Y4, Z4, HAI, TIME, BAX(2), BAY(2), BAZ(2), VX, VY, VZ)
C                                       baseline 2-3
         CALL BACOOR (IORB1, IORB3, ORBIT1, ORBIT3, X1, Y1, Z1,
     *      X3, Y3, Z3, HAI, TIME, BAX(3), BAY(3), BAZ(3), VX, VY, VZ)
C                                       baseline 2-3
         CALL BACOOR (IORB2, IORB4, ORBIT2, ORBIT4, X2, Y2, Z2,
     *      X4, Y4, Z4, HAI, TIME, BAX(4), BAY(4), BAZ(4), VX, VY, VZ)
C                                       Now need to compute model on
C                                       3 baselines and combine to
C                                       form closure model.
C                                       u,v,w from HA
         DO 160 IBAS = 1,4
C                                       U, V, W in meters
            U(IBAS) = BAX(IBAS)*SIN(RA) - BAY(IBAS)*COS(RA)
            V(IBAS) = BAX(IBAS)*COS(RA)*SIN(DEC) +
     *         BAY(IBAS)*SIN(RA)*SIN(DEC) - BAZ(IBAS)*COS(DEC)
            W(IBAS) = BAX(IBAS)*COS(RA)*COS(DEC) + BAY(IBAS)*SIN(RA)
     *         *COS(DEC) + BAZ(IBAS)*SIN(DEC)
C                                       U, V, W in wavelength
            U(IBAS) = U(IBAS) * ( AFREQ / VELITE)
            V(IBAS) = V(IBAS) * ( AFREQ / VELITE)
            W(IBAS) = W(IBAS) * ( AFREQ / VELITE)
            CSUM = 0.0
            SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
            IF (DOSPHE) THEN
               DO 110 K = 1,NUMCC
                  AA = GAUSA(K) * SQRT (U(IBAS)*U(IBAS) +
     *                V(IBAS)*V(IBAS))
                  IF (AA.LT.6.28D-2) AA = 6.28D-2
                  FTEMP = 3.0D0 * SFLUX(K) *
     *               ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
                  TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K) +
     *               W(IBAS) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 110              CONTINUE
C
            ELSE IF (DOGAUS) THEN
C                                       Gaussian
               DO 120 K = 1,NUMCC
                  ARG = U(IBAS)*U(IBAS)*GAUSA(K) +
     *               U(IBAS)*V(IBAS)*GAUSB(K) +
     *               V(IBAS)*V(IBAS)*GAUSC(K)
                  IF (ARG.LE.-8.0) GO TO 120
                     FTEMP = SFLUX(K) * EXP(ARG)
                     TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K)
     *                  + W(IBAS) * CCPOS(3,K)
                     SSUM = SSUM + FTEMP * SIN (TEMP)
                     CSUM = CSUM + FTEMP * COS (TEMP)
 120              CONTINUE
C                                       CC's
            ELSE
               DO 130 K = 1,NUMCC
                  FTEMP = SFLUX(K)
                  TEMP = U(IBAS) * CCPOS(1,K) + V(IBAS) * CCPOS(2,K) +
     *               W(IBAS) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
 130              CONTINUE
               END IF
C                                       Finished computing model
            AMODL = SQRT (SSUM*SSUM + CSUM*CSUM)
            CLOMOD(IBAS) = AMODL
 160        CONTINUE
         FAZ = CLOMOD(3) * CLOMOD(4)
         IF (FAZ.NE.0.0) THEN
            YDATA(1,I) = CLOMOD(1) * CLOMOD(2) / FAZ
         ELSE
            YDATA(1,I) = FBLANK
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
      SUBROUTINE VBMDL (RA, DEC, XMIN, XMAX, YMIN, YMAX, YYOFF, MPLO,
     *   NPLO, CPQD, BUFFER, IERR)
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
C      MPLO              I     Type of X-axis plot
C      NPLO              I     Type of Y-axis plot
C      CPQD(4)           I     Closure phase triplet plotted
C      IERR              I     Standard AIPS error code.
C-----------------------------------------------------------------------
      INTEGER   MPLO, NPLO, IERR, BUFFER(1), CPQD(4), IT1, IT2, IT3, IT4
      LOGICAL   DOMDL
      REAL      XMIN, XMAX, YMIN, YMAX, YYOFF(2)
      DOUBLE PRECISION    RA, DEC
      REAL      XDATA(200), YDATA(2,200)
      INCLUDE 'CAPLT.INC'
C-----------------------------------------------------------------------
C                                     Check if model exists.
      DOMDL = (NPLO.EQ.1) .OR. (NPLO.EQ.2) .OR. (NPLO.EQ.9) .OR.
     *   (NPLO.EQ.10)
      IF ((NUMCC.LE.0) .OR. (.NOT.DOMDL)) GO TO 999
C                                       Are all antennas ground?
      IT1 = CPQD(1)
      IT2 = CPQD(2)
      IT3 = CPQD(3)
      IT4 = CPQD(4)
C                                       setup model if one or both
C                                       antennas are in orbit
      IF ((IORBIT(IT1).GT.0) .OR. (IORBIT(IT2).GT.0)
     *   .OR. (IORBIT(IT3).GT.0) .OR. (IORBIT(IT4).GT.0)) THEN
         CALL MODORB (XMIN, XMAX, MPLO, RA, DEC, CPQD, XDATA, YDATA,
     *      IERR)
C                                       setup model if both antennas are
C                                       on Earth
      ELSE
         CALL MODSET (XMIN, XMAX, MPLO, NPLO, RA, DEC, CPQD, XDATA,
     *      YDATA, IERR)
         END IF
      IF (IERR.NE.0) GO TO 999
C                                     setup plotting common
      CALL PLTSET (XMIN, XMAX, YMIN, YMAX, XYSCL, XYOFF, YYOFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                     plot model
      CALL GLTYPE (2, BUFFER, IERR)
      IF (IERR.EQ.0) CALL MODPLT (NPLO, 200, XDATA, YDATA, BUFFER, IERR)
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
C      XB,YB,ZB(*)   D    Cartesian coordinates for ground based
C                         stations in meters.
C      IORBIT        I(*)  Array of satalites number.
C                           =0 if ground based
C      ORBITA        D(IP + (IS-1)*6)  Parameters of the orbits
C                           IP - parameters number;  IS - satalite #
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
     *   NNCH, IIBIF, DISKI, IORB, I, IORPRM, NUMREC
      INCLUDE 'CAPLT.INC'
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
      NUMREC = IABUF(5)
      ARRLON = 0.0D0
      IF ((DX.NE.0.0D0) .OR. (DY.NE.0.0D0)) ARRLON = ATAN2 (DY, DX)
C                                       Get antenna info.
C                                       IORB is an orbital antenna
C                                       number
      IORB = 0
      NANT = 0
      DO 30 IA = 1,NUMREC
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, STNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
            END IF
         NANT = MAX (NANT, NOSTA)
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
      INTEGER   VER, IERR
C
      CHARACTER KEYS(7)*8
      HOLLERITH CATCLH(256)
      INTEGER   LIMIT, I, NCOUNT, IROUND
      INTEGER   IABUF(512), DISK2, SEQ2, IUSER, LUNB, NREC, NKEY,
     *   KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, BMAKOL, BMIKOL, BPAKOL,
     *   IBUFF1(1024), NKOL, CATCLN(256), IPOINT, IMAP, NMAP, MODEL,
     *   METHOD
      REAL      ABUF(256), CATCLR(256), EPS, RECORD(20), XTEMPC, TA, TB,
     *   AM, AN, XYZ(3), XP(3), UMAT(3,3), PMAT(3,3), XXOFF, YYOFF,
     *   ZZOFF
      DOUBLE PRECISION    CATCLD(128), A8BUF(128), XRA, XDEC
      LOGICAL   T, F, WASOME
      INCLUDE 'CAPLT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   BITER(MAXFLD), NITER(MAXFLD)
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
      IF (NMAP.LE.0) GO TO 200
      NCOUNT = 0
      LIMFLX = XFLUX
      NONEG = F
      WASOME = F
      DO 10 IMAP = 1,NMAP
         IF (IMAP.LE.MAXAFL) THEN
            NITER(IMAP) = ABS(XCOMP(IMAP)) + 0.1
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
      CALL FACSET (DISKIN, OLDCNO, IBIF, IDN(1), 1, 1.0, IERR)
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
            CALL H2CHR (6, KHIMCO, CATCLH(KHIMC), NAM2IN)
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
         IF (IERR .NE. 0) THEN
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
         IF (IERR .NE. 0) THEN
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
            IF (IERR .GT. 0) THEN
               WRITE (MSGTXT,1030) IERR, I
               GO TO 990
               END IF
            IF (ABS(RECORD(FLXKOL)).LT.LIMFLX) GO TO 100
            IF ((NONEG) .AND. (RECORD(FLXKOL).LT.0.0)) GO TO 100
C                                       Deal with component.
            NCOUNT = NCOUNT + 1
            IF (NCOUNT.GT.MAXCC) GO TO 100
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
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
         IF (NCOUNT.GT.MAXCC) GO TO 130
 120     CONTINUE
      NUMCC = NCOUNT
      GO TO 140
 130  NUMCC = MAXCC
C                                       Convert gaussian parameters
 140  CALL UNSETG (BUFF1)
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
 1000 FORMAT ('REEDIN: ERROR',I3,' OPENING CC FILES')
 1010 FORMAT ('REEDIN: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('REEDIN: READ ERROR',I3,' RECORD ',I5)
 1060 FORMAT ('REEDIN: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE MODPTS (UVW, ADATA)
C-----------------------------------------------------------------------
C   Subroutine to return 1 model value for a given set of clean
C   components and for a single data sample on 3 baselines
C   Inputs:
C      IVW     R(3,4)   U,V,W of 4 baselines
C   Input from common /COMPS/
C      CCPOS(3,MAXCC) R     Distance from ref. X-pixel in turns.
C      SFLUX(MAXCC)   R     Flux in pixel XX, YY in Jansky
C      GAUSA(MAXCC)   R     Gaussian coefficient for u*u
C      GAUSB(MAXCC)   R     Gaussian coefficient for u*v
C      GAUSC(MAXCC)   R     Gaussian coefficient for v*v
C      NUMCC          I     Number of pixels actually processed.
C   Output:
C      ADATA          R     Closure amplitude of model
C   This subroutine gets information from common /CANIN/ set up by
C   subroutine ANTIN
C-----------------------------------------------------------------------
      REAL      UVW(3,4), ADATA
C
      DOUBLE PRECISION AA
      REAL      CSUM, SSUM, TEMP, AMP(4), ARG, FTEMP
      INTEGER   K, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'CAPLT.INC'
C-----------------------------------------------------------------------
      DO 100 I = 1,4
         CSUM = 0.0
         SSUM = 0.0
C                                       calculate model sin/cos amps.
C                                       Spherical model
C                                       Trap very unresolved - needed to
C                                       prevent serious precision loss.
         IF (DOSPHE) THEN
            DO 25 K = 1,NUMCC
               AA = GAUSA(K) * SQRT (UVW(1,I)*UVW(1,I) +
     *            UVW(2,I)*UVW(2,I))
               IF (AA.LT.6.28D-2) AA = 6.28D-2
               FTEMP = 3.0D0 * SFLUX(K) *
     *            ((SIN(AA) / (AA*AA*AA)) - (COS(AA) / (AA*AA)))
               TEMP = UVW(1,I) * CCPOS(1,K) + UVW(2,I) * CCPOS(2,K) +
     *            UVW(3,I) * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
 25            CONTINUE
C                                       Gaussian
         ELSE IF (DOGAUS) THEN
            DO 30 K = 1,NUMCC
               ARG = UVW(1,I) * UVW(1,I) * GAUSA(K) +
     *            UVW(1,I) * UVW(2,I) * GAUSB(K) +
     *            UVW(2,I) * UVW(2,I) * GAUSC(K)
               IF (ARG.GT.-8.0) THEN
                  FTEMP = SFLUX(K) * EXP(ARG)
                  TEMP = UVW(1,I) * CCPOS(1,K) + UVW(2,I) * CCPOS(2,K) +
     *               UVW(3,I) * CCPOS(3,K)
                  SSUM = SSUM + FTEMP * SIN (TEMP)
                  CSUM = CSUM + FTEMP * COS (TEMP)
                  END IF
 30            CONTINUE
C                                       Point
         ELSE
            DO 35 K = 1,NUMCC
               FTEMP = SFLUX(K)
               TEMP = UVW(1,I) * CCPOS(1,K) + UVW(2,I) * CCPOS(2,K) +
     *            UVW(3,I) * CCPOS(3,K)
               SSUM = SSUM + FTEMP * SIN (TEMP)
               CSUM = CSUM + FTEMP * COS (TEMP)
 35            CONTINUE
            END IF
C                                       Finished computing model
         AMP(I) = SQRT (SSUM*SSUM + CSUM*CSUM)
 100     CONTINUE
C                                       combine
      TEMP = AMP(3) * AMP(4)
      IF (TEMP.LE.0.0) THEN
         ADATA = FBLANK
      ELSE
         ADATA = AMP(1) * AMP(2) / TEMP
         END IF
C
 999  RETURN
      END

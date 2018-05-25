LOCAL INCLUDE 'UVPLT.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMPRM, MAXB
      PARAMETER (NUMPRM = 20)
      PARAMETER (MAXB = 4096)
C
      CHARACTER NAMEIN*12, CLAIN*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XSTOK
      REAL   USERID, XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID,
     *   XTIME(8), XANT(50), XBASE(50), XUVRA(2), XSUBA, XBCHAN, XECHAN,
     *   XNCHAV, XCHINC, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XINC, APARM(10),
     *   BPARM(10), DOAC, DOSCAL, XDOWT, XREFAN, XROTPA, FACTOR, DO3COL,
     *   XLABEL, XBADD(10), XDOTV, XGRCH, XYRATO, WMIN, WMAX
      REAL      BUFF1(UVBFSS), TBEG, TFIN, XYSCL(3), XYOFF(3),
     *   RPARM(20), XYMIN(3), XYMAX(3), FINC(MAXIF), ROTATE
      DOUBLE PRECISION FOFF(MAXIF), SUM(MAXB), SUM2(MAXB), XCNT(MAXB),
     *   SUMI(MAXB), SUMI2(MAXB)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX(2),
     *   NCH, VER, JBUFSZ, IANT(50), NANT, IBAS(50), NBAS, CNOIN,
     *   IFRQ, NFRQ, NSUBA, GRCHN, TVCHN, TVCORN(4), ISBAND(MAXIF),
     *   EXCLFQ(MAXIF,MAXFQ), CHINC, LABEL, FIXSCL, CSOU, SBUFF(512),
     *   REFANT, NPARMS, KCNT(MAXB), NPOL, IPHASE, NNVIS, NCHAV,
     *   INSNUM, DOPLAN
      LOGICAL   UVREV, ISUVR, MULTI, DESEL, DOTV, ISCROS(4), DOSBIN,
     *   DOLINE, INVAX(2), SPHASE, DOMIRR, DOUVMI
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR,
     *   XQUAL, XCALC, XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE,
     *   XUVRA, XSUBA, XBCHAN, XECHAN, XNCHAV, XCHINC, XBIF, XEIF,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XINC, APARM, BPARM, DOAC, DOSCAL, XDOWT, XREFAN,
     *   XROTPA, FACTOR, DO3COL, XLABEL, XBADD, XDOTV, XGRCH, XYRATO,
     *   WMIN, WMAX
      COMMON /CHPARM/ NAMEIN, CLAIN
      COMMON /BUFRS/ BUFF1, SBUFF, RPARM, JBUFSZ
      COMMON /UVPCOM/ FOFF, SUM, SUM2, XCNT, SUMI, SUMI2, FINC, ISBAND,
     *   TBEG, TFIN, XYSCL, XYOFF, XYMIN, XYMAX, UVREV, ISUVR, MULTI,
     *   DOTV, IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH,
     *   VER, CNOIN, IFRQ, NFRQ, NSUBA, TVCHN, GRCHN, TVCORN, EXCLFQ,
     *   CHINC, LABEL, FIXSCL, ISCROS, CSOU, REFANT, ROTATE, NPARMS,
     *   KCNT, DOSBIN, DOLINE, INVAX, NPOL, IPHASE, SPHASE, NNVIS,
     *   DOMIRR, DOUVMI, NCHAV, INSNUM, DOPLAN
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS
LOCAL END
      PROGRAM UVPLT
C-----------------------------------------------------------------------
C! UVPLT plots uvdata, makes a standard 'PL' extension file
C# EXT-appl Graphics Plot appl UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2015, 2017-2018
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
C   UVPLT plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     USERID                       UV data file owner # ignored
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input VU data.
C     CHANNEL        NCH           Channel #: 0 or 1 ok for cont.
C     BIF            BIF           IF number to start
C     EIF            EIF           Through IF number
C     XINC.......Skip this number of vis. records between plotting.
C     UVRANGE....Range of UV projected spacings to include (Klambda)
C     APARM......Selection parameters:
C        1 = Start IAT day (day 0 = first day in data base)
C        2 = Start IAT hour
C        3 = Start IAT minute
C        4 = Start IAT second
C        5 = Stop IAT day (day 0 = first day in data base)
C        6 = Stop IAT hour
C        7 = Stop IAT minute
C        8 = Stop IAT second
C        9 = antenna number
C            0 => all antennas.
C            100*M + N => correlator (M,N) with M<N
C        10: 1 => plot RR data only
C            2 => plot LL data only
C            3 => plot RL data only
C            4 => plot LR data only
C            5 => plot IPOL
C            6 => plot VPOL
C            7 => plot QPOL
C            8 => plot UPOL
C     BPARM......Control parameters:
C        1 = type of X-axis (where  1 = amplitude (Jy), 2 = phase
C           (degrees), 3 = u,v distance (klambda), 4 = u,v p.a.
C           (degrees, clockwise from v-axis), 5 = time (iat days),
C           6 = u, 7 = v, 8 = w (all in klambda), 9 = real part (Jy),
C           10 = imaginary part(Jy), 11 time (hours), 12 log(ampl),
C           13 weight, 14 hour angle (hours), 15 elevation (deg),
C           16 parallactic angle (deg), 17 u,v distance (klambda)
C           along specified PA.), 18 azimuth, 19 frequency
C        2 = type of Y-axis
C        3 = Do not autoscale if non-zero, use following values:
C        4 = Minimum of X-axis,
C        5 = Maximum of X-axis,
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis,
C        8 = Number of bins on X-axis for averaging, 0=> no averaging.
C        9   If > 0 then write binned values to message file
C       10   If > then plot auto-correlations as well
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C
C     MRC 90/Jan/23: Allow all frequencies to be plotted together.
C     RCW change to make each axis limit separately self scaling.
C     Redo the logic of the self scaling.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IERR, IRET, TVPLAN(2), PLANXY(3), JJJ
      LONGINT   PTVPLN
      LOGICAL   GETSCL
      INCLUDE 'UVPLT.INC'
      REAL      XZY(3,MAXCIF)
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UVPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Determine limits for all axes
C                                       if any autoscaling will be done.
      GETSCL = (FIXSCL.LE.0) .OR. (BPARM(4).GE.BPARM(5)) .OR.
     *   (BPARM(6).GE.BPARM(7))
      CALL SCAL (GETSCL, XZY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       actually 3 color?
      JJJ = (ECHAN - BCHAN) / CHINC + 1
      JJJ = (EIF - BIF + 1) * JJJ * NPOL
      IF (JJJ.LE.1) DO3COL = -1.0
C                                       do planes?
      DOSBIN = ((FIXSCL.LE.0) .OR. (BPARM(6).GE.BPARM(7))) .AND.
     *   (BPARM(8).GE.0.5)
      DOPLAN = 0
      IF (.NOT.DOSBIN) THEN
         IF (DOTV) THEN
            DOPLAN = 1
         ELSE
            DOPLAN = 3
            END IF
         IF (DO3COL.GT.0.0) DOPLAN = DOPLAN + 1
         END IF
      IF ((.NOT.DOTV) .AND. (BPARM(10).GT.0.0)) DOPLAN = 0
      IF (BPARM(10).GT.1.0) DOPLAN = 0
      IF (DOSBIN) THEN
         CALL SCLBIN (NPOL, XZY, IRET)
         IF (IRET.NE.0) GO TO 995
         END IF
      IF (DOPLAN.GT.0) THEN
         CALL PLPLAN (PLANXY, TVPLAN, PTVPLN, NPOL, XZY, IRET)
         IF (IRET.NE.0) GO TO 995
      ELSE
         PLANXY(1) = 0
         PLANXY(2) = 0
         PLANXY(3) = 0
         PTVPLN = 0
         END IF
C                                       Do plot
      CALL PLTUV (PLANXY(1), PLANXY(2), PLANXY(3), TVPLAN(1+PTVPLN),
     *   NPOL, XZY,IRET)
C                                       Clear catlg on error
      IF ((IRET.NE.0) .AND. (NCFILE.GE.1) .AND. (FRW(1).LE.0) .AND.
     *   (.NOT.DOTV)) THEN
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'READ', CATBLK, SBUFF,
     *      VER, IERR)
         CALL ZCLOSE (LUNI, INDI, IERR)
         NCFILE = NCFILE - 1
         END IF
C                                       Close down
 995  CALL DIE (IRET, SBUFF)
C
 999  STOP
      END
      SUBROUTINE UVPIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVPIN gets input parameters for UVPLT .
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   JERR
C
      CHARACTER UTYPE*2, STAT*4
      INTEGER  IUSER, I, IERR, ITEMP, IROUND, LUNTB, LUN, FQVER, NIF,
     *   LTYPE
      LOGICAL   T, TABLE, FITASC, F, MATCH, SNEXST, EXIST
      INCLUDE 'UVPLT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA LUNTB /19/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      CALL DFILL (MAXB, 0.0D0, SUM)
      CALL DFILL (MAXB, 0.0D0, SUM2)
      CALL DFILL (MAXB, 0.0D0, SUMI)
      CALL DFILL (MAXB, 0.0D0, SUMI2)
      CALL DFILL (MAXB, 0.0D0, XCNT)
      CALL FILL (MAXB, 0, KCNT)
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      VER = 10000
      NPOL = 0
C                                       Get input parameters.
      NPARMS = 304
      CALL GTPARM (PRGM, NPARMS, RQUICK, USERID, SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 10
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, SBUFF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 25      CONTINUE
      SELQUA = IROUND (XQUAL)
C                                       Crunch input parameters.
      USERID = NLUSER
      IUSER = NLUSER
      IF (XINC.LT.1.0) XINC = 1.0
      INC = IROUND (XINC)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      REFANT = XREFAN + 0.01
      ROTATE = XROTPA
      CALL FILL (4, 0, TVCORN)
      DOLINE = FACTOR.LT.0.0
      FACTOR = ABS (FACTOR)
      IF (FACTOR.LT.0.1) FACTOR = 1.
      IF (FACTOR.GT.1000.) FACTOR = 1.
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
C                                       Get CATBLK from file.
      LUNI = 48
      UTYPE = 'UV'
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, UTYPE, IUSER,
     *   LUNI, INDI, CNOIN, CATBLK, SBUFF, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1035) IERR
         GO TO 990
 40   CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
C                                       Multi-source file?
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, SBUFF, TABLE, MULTI,
     *      FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       If calibrating, does SN exist
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      IF ((DOCAL) .AND. (.NOT.MULTI)) THEN
C                                       Look for SN file
         CALL ISTAB ('SN', DISKIN, CNOIN, 1, LUNTB, SBUFF, TABLE,
     *      SNEXST, FITASC, JERR)
         IF ((.NOT.SNEXST) .OR. (JERR.NE.0)) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (8)
            DOCAL = .FALSE.
            END IF
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL COPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      IUDISK = DISKIN
      IUCNO = CNOIN
      USEQ = SEQIN
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      IF (TYPUVD.GT.0) CALL RFILL (2, 0.0, UVRNG)
      IF (UVRNG(2).LE.0.0) UVRNG(2) = 1.0E10
C                                       WMIN, WMAX, in klambda
      WMIN = APARM(1) * 1000.0
C      IF (WMIN .EQ. 0.0) WMIN = -1.0E10
      IF (WMIN .LE. 0.0) WMIN = 0
      WMAX = APARM(2) * 1000.0
      IF (WMAX .LE. 0.0) WMAX = 1.0E13
      IF (WMIN .GT. WMAX) THEN
         WMIN = 0
         WMAX = 1.0E13
         END IF
C                                       plot the UVW mirrors?
      DOMIRR = ABS(APARM(3)).LE. 0.01
C                                       which half of the UV to plot?
      DOUVMI = APARM(3) .GT. 0.5
C
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, BCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) BCHAN = CATBLK(KINAX+JLOCF)
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      CHINC = IROUND (XCHINC)
      NCHAV = IROUND (XNCHAV)
      NCHAV = MAX (1, MIN (ECHAN-BCHAN+1, NCHAV))
      CHINC = IROUND (XCHINC)
      IF (CHINC.LE.0) CHINC = NCHAV
      IF (NCHAV.GE.ECHAN-BCHAN+1) CHINC = NCHAV
      I = (ECHAN + 1 - BCHAN - NCHAV) / CHINC
      ECHAN = BCHAN + I * CHINC + NCHAV - 1
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XBIF = BIF
      XEIF = EIF
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
C                                       Allow multiple subarrays
      CALL FNDEXT ('AN', CATBLK, NSUBA)
      IF ((SUBARR.GT.0) .AND. (SUBARR.LE.NSUBA)) NSUBA = 1
      NSUBA = MAX (1, NSUBA)
C                                       Allow multiple FQ ids
      NFRQ = 1
      IF ((FRQSEL.LE.0) .AND. (SELBAN.LE.0.0) .AND. (SELFRQ.LE.0D0))
     *   THEN
         FRQSEL = 1
C                                       Determine the number of FREQIDs.
         FQVER = 1
         CALL ISTAB ('FQ', DISKIN, CNOIN, FQVER, LUN, FQBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, FQVER, CATBLK,
     *         LUN, IFQRNO, FQKOLS, FQNUMV, NIF, JERR)
            IF (JERR.NE.0) GO TO 999
            NFRQ = FQBUFF(5)
            IF (NFRQ.GT.1) THEN
               WRITE (MSGTXT,1060) NFRQ
               CALL MSGWRT (3)
               END IF
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Find specified FQ id
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                        Retain auto-correlations ?
      DOACOR = DOAC.GT.0.
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
C                                       Get axis types.
      TYPEAX(1) = IROUND (BPARM(1))
      TYPEAX(2) = IROUND (BPARM(2))
      INVAX(1) = TYPEAX(1).LT.0
      INVAX(2) = TYPEAX(2).LT.0
      TYPEAX(1) = ABS (TYPEAX(1))
      TYPEAX(2) = ABS (TYPEAX(2))
      IF (TYPEAX(1).EQ.6) INVAX(1) = .NOT.INVAX(1)
      IF (TYPEAX(2).EQ.6) INVAX(2) = .NOT.INVAX(2)
      FIXSCL = IROUND (BPARM(3))
C                                       Test type of plot
      IF (TYPUVD.LE.0) THEN
         IF ((TYPEAX(1).LT.1) .OR. (TYPEAX(1).GT.NUMPRM)) TYPEAX(1) = 3
         IF ((TYPEAX(2).LT.1) .OR. (TYPEAX(2).GT.NUMPRM)) TYPEAX(2) = 1
         IF ((TYPEAX(1).EQ.3) .OR. ((TYPEAX(1).GE.6) .AND.
     *      (TYPEAX(1).LE.8)) .OR. ((TYPEAX(1).EQ.17))) THEN
            BPARM(4) = BPARM(4) * 1.0E3
            BPARM(5) = BPARM(5) * 1.0E3
            END IF
         IF ((TYPEAX(2).EQ.3) .OR. ((TYPEAX(2).GE.6) .AND.
     *      (TYPEAX(2).LE.8)) .OR. ((TYPEAX(2).EQ.17))) THEN
            BPARM(6) = BPARM(6) * 1.0E3
            BPARM(7) = BPARM(7) * 1.0E3
            END IF
      ELSE
         IF ((TYPEAX(1).LT.1) .OR. (TYPEAX(1).GT.NUMPRM)) TYPEAX(1) = 11
         IF ((TYPEAX(2).LT.1) .OR. (TYPEAX(2).GT.NUMPRM)) TYPEAX(2) = 9
         IF (TYPEAX(1).EQ.1) TYPEAX(1) = 9
         IF (TYPEAX(1).EQ.2) TYPEAX(1) = 10
         IF (TYPEAX(1).EQ.3) TYPEAX(1) = 6
         IF (TYPEAX(1).EQ.4) TYPEAX(1) = 7
         IF (TYPEAX(1).EQ.8) TYPEAX(1) = 6
         IF (TYPEAX(2).EQ.1) TYPEAX(2) = 9
         IF (TYPEAX(2).EQ.2) TYPEAX(2) = 10
         IF (TYPEAX(2).EQ.3) TYPEAX(2) = 6
         IF (TYPEAX(2).EQ.4) TYPEAX(2) = 7
         IF (TYPEAX(2).EQ.8) TYPEAX(2) = 6
         END IF
      XYMAX(1) = -1.0E10
      XYMAX(2) = XYMAX(1)
      XYMAX(3) = XYMAX(1)
      XYMIN(1) = 1.E10
      XYMIN(2) = XYMIN(1)
      XYMIN(3) = XYMIN(1)
      BPARM(1) = TYPEAX(1)
      BPARM(2) = TYPEAX(2)
      IPHASE = 0
      IF (TYPEAX(1).EQ.2) IPHASE = 1
      IF (TYPEAX(2).EQ.2) IPHASE = 2
      SPHASE = .FALSE.
      IF ((TYPEAX(1).NE.1) .AND. (TYPEAX(1).NE.9) .AND.
     *   (TYPEAX(1).NE.10) .AND. (TYPEAX(1).NE.12) .AND.
     *   (TYPEAX(2).NE.1) .AND. (TYPEAX(2).NE.9) .AND.
     *   (TYPEAX(2).NE.10) .AND. (TYPEAX(2).NE.12)) DOSCAL = -1.0
C                                       If plotting uv only
C                                       then plot conjugate points
      ITEMP = TYPEAX(1) * TYPEAX(2)
      UVREV = (ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56)
C                                       plot mirror vaselines?
      UVREV = UVREV .AND. DOMIRR
      UVREV = (UVREV) .AND. (TYPUVD.LE.0)
      IF ((UVREV) .AND. (BPARM(8).GE.0.5)) THEN
         MSGTXT = 'PLOTS OF UV SAMPLES ARE NOT BINNED'
         CALL MSGWRT (7)
         BPARM(8) = 0.0
         END IF
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       Update catalog header.
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, FCNO(1), CATBLK, SBUFF, T, 'READ',
     *      VER, JERR)
         FRW(NCFILE) = 0
         IF (JERR.NE.0) NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1035 FORMAT ('ERROR',I3,' FINDING THE UV DATA SET')
 1050 FORMAT ('NO SN FILE FOUND, BUT DOCALIB IS TRUE: NO CAL APPLIED')
 1060 FORMAT ('Plotting',I4,' frequency IDs.')
      END
      SUBROUTINE SCAL (GETSCL, XZY, IRET)
C-----------------------------------------------------------------------
C   SCAL sends uv points one at a time to XYOFF .
C   Input:
C      GETSCL   L   If false, do not read all the data, just go through
C                   the rest of the motions
C   Output:
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   GETSCL
      REAL      XZY(3,*)
      INTEGER   IRET
C
      INTEGER   I, NUMVIS, J, JJJ, ISUB, JSUB, NXVER, NIF, NXLUN,
     *   IROUND, ISOU, LSOU, LIF, IP
      LOGICAL   REQBAS, REQAS
      INCLUDE 'UVPLT.INC'
      REAL      SV, CATR(256), SPIX(3)
      DOUBLE PRECISION CATD(128), XUMVIS
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATD, CATR)
C-----------------------------------------------------------------------
      NUMVIS = 0
      XUMVIS = 0.0D0
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 100
      IXLUN = 28
      INSNUM = 0
      CALL SOUFIL (IRET)
      IF ((NSOUWD.EQ.1) .AND. (DOSWNT)) INSNUM = SOUWAN(1)
      CSOU = -1
      LSOU = -1
      SPIX(1) = 0.0
      SPIX(2) = -1.E6
      SPIX(3) = 0.0
C                                       14,15,16,18 ha,el,pa,az
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     * ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
      REQAS = REQAS .OR. (TYPEAX(1).EQ.18) .OR. (TYPEAX(2).EQ.18)
C                                       Loop for each FREQID.
      DO 150 IFRQ = 1,NFRQ
         CALL FILL (MAXIF, 0, EXCLFQ(1,IFRQ))
         IF (NFRQ.GT.1) THEN
            FRQSEL = IFRQ
            WRITE (MSGTXT,1000) IFRQ
            CALL MSGWRT (5)
            END IF
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Init vis file for read.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) THEN
               IRET = 0
               EXCLFQ(ISUB,IFRQ) = 1
               GO TO 140
               END IF
            IF (IRET.GT.0) GO TO 999
C                                       polarizations
            IF (NPOL.LE.0) THEN
               NPOL = CATBLK(KINAX+JLOCS)
               DO 20 I = 1,NPOL
                  ISCROS(I) = .TRUE.
                  SV = CATD(KDCRV+JLOCS) + (I-CATR(KRCRP+JLOCS)) *
     *               CATR(KRCIC+JLOCS)
                  J = IROUND (SV)
                  IF ((J.GE.-2) .AND. (J.LE.1)) ISCROS(I) = .FALSE.
                  IF ((J.GE.-6) .AND. (J.LE.-5)) ISCROS(I) = .FALSE.
 20               CONTINUE
               END IF
C                                       Loop Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1100) IRET
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (GETSCL)) THEN
C                                       Do we need this baseline?
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS)) THEN
C                                       do the following UVW
C                                       transformations only if U,V
C                                       plotting (For any case LK)
                  IF ((TYPEAX(1).EQ.6 .AND. TYPEAX(2).EQ.7) .OR.
     *               (TYPEAX(1).EQ.7 .AND. TYPEAX(2).EQ.6)) THEN
C                                       convert vis to positive W
                     IF (RPARM(ILOCW+1) .LE. 0.0) THEN
                        RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                        RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                        RPARM(ILOCW+1) = -RPARM(ILOCW+1)
                        END IF
C                                       convert to the mirror UV
                     IF (DOUVMI) THEN
                        RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                        RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                        END IF
C                                       reject if W.LT.WIN .OR.
C                                       W.GT.WMAX
                     IF ((RPARM(ILOCW+1).LT.WMIN) .OR.
     *                  (RPARM(ILOCW+1).GT.WMAX)) GO TO 100
                     END IF
C
                  NUMVIS = NUMVIS + 1
                  IF (MOD(NUMVIS,INC).EQ.0) THEN
C                                       scaling info
                     IF (DOSCAL.GT.0.0) THEN
                        ISOU = 0
                        IF (ILOCSU.GE.0) ISOU = IROUND
     *                     (RPARM(1+ILOCSU))
                        IF (ISOU.LE.0) ISOU = INSNUM
                        IF (ISOU.NE.LSOU) THEN
                           LSOU = ISOU
                           CALL GETSOU (LSOU, DISKIN, CNOIN, CATUV,
     *                        NXLUN,IRET)
                           DO 135 LIF = BIF,EIF
                              IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF)
     *                           = 1.0
 135                          CONTINUE
                           IF (DOSCAL.GT.1.5) THEN
                              IP = 1
                              IF (DOSCAL.GT.2.5) IP = 2
                              CALL FNDSPX (DISKIN, CNOIN, LSOU,
     *                           FRQSEL, CATUV, IP, SPIX, IRET)
                           ELSE
                              SPIX(1) = 0.0
                              SPIX(2) = 0.0
                              SPIX(3) = 0.0
                              END IF
                           END IF
                     ELSE IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
                        CSOU = CURSOU
                        CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV,
     *                     NXLUN, IRET)
                        IF (IRET.NE.0) THEN
                           MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                           CALL MSGWRT (6)
                           END IF
                        END IF
C                                       Find scales
                     CALL UVPLTS (FLUX, SPIX, NPOL, BUFF1)
                     CALL FNDXY (RPARM, BUFF1, NPOL, XZY)
                     CALL XYSCAL (NUMVIS, NPOL, XZY, JJJ, IRET)
                     IF (IRET.EQ.0) XUMVIS = XUMVIS + JJJ
                     END IF
                  END IF
               GO TO 100
               END IF
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
C                                       put NUMVIS into common
      NNVIS = NUMVIS
      SUBARR = JSUB
      IRET = 0
      IF (.NOT.GETSCL) GO TO 999
C                                       Any valid points
      IF (XUMVIS.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
C                                       Final call to XYSCAL
      NUMVIS = -1
      CALL XYSCAL (NUMVIS, NPOL, XZY, JJJ, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1110) IRET
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing FREQID =',I3)
 1100 FORMAT ('SCAL: ERROR',I3,' READING VIS FILE')
 1110 FORMAT ('SCAL: XYSCAL ERROR',I3)
 1200 FORMAT ('FOUND',F8.0,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE SCLBIN (NP, XZY, IRET)
C-----------------------------------------------------------------------
C   SCLBIN bins the uv data and determines the Y scaling to use
C   Output:
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NP, IRET
      REAL      XZY(3,NP,*)
C
      INTEGER   I, NUMVIS, J, ISUB, JSUB, NXVER, NIF, NXLUN, NBIN, IBIN,
     *   ICO, LC, LF, IC, IP
      LOGICAL   REQBAS, REQAS, DOWT
      INCLUDE 'UVPLT.INC'
      REAL      XMIN, XMAX, XOFF, XSCL, X, AVERG, STDEV, TEMP, TR, TI
      DOUBLE PRECISION WT, XUMVIS
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NUMVIS = 0
      XUMVIS = 0.0D0
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 100
      CSOU = -1
C                                       14,15,16,18 ha,el,pa,az
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     * ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
      REQAS = REQAS .OR. (TYPEAX(1).EQ.18) .OR. (TYPEAX(2).EQ.18)
C                                       bin scaling on X axis
      NBIN = BPARM(8) + 0.5
      NBIN = MIN (NBIN, MAXB)
      DOWT = XDOWT.GT.0.0
      WT = 1.0D0
      SPHASE = .FALSE.
      IF ((FIXSCL.GT.0) .AND. (BPARM(5).GT.BPARM(4))) THEN
         XMIN = BPARM(4)
         XMAX = BPARM(5)
         IF ((IPHASE.EQ.1) .AND. (XMIN.GE.0.0)) SPHASE = .TRUE.
      ELSE
         XMIN = XYMIN(1)
         XMAX = XYMAX(1)
         IF ((IPHASE.EQ.1) .AND.
     *      (XYMAX(3)-XYMIN(3).LT.XYMAX(1)-XYMIN(1))) THEN
            XMIN = XYMIN(3)
            XMAX = XYMAX(3)
            SPHASE = .TRUE.
            END IF
         END IF
      TEMP = 0.025 * (XMAX - XMIN)
      IF (XMIN.NE.BPARM(4)) XMIN = XMIN - TEMP
      IF (XMAX.NE.BPARM(5)) XMAX = XMAX + TEMP
      XOFF = XMIN
      XSCL = 1000. / (XMAX - XMIN)
C                                       Loop for each FREQID.
      DO 150 IFRQ = 1,NFRQ
         CALL FILL (MAXIF, 0, EXCLFQ(1,IFRQ))
         IF (NFRQ.GT.1) THEN
            FRQSEL = IFRQ
            WRITE (MSGTXT,1000) IFRQ
            CALL MSGWRT (5)
            END IF
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Init vis file for read.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) THEN
               IRET = 0
               EXCLFQ(ISUB,IFRQ) = 1
               GO TO 140
               END IF
            IF (IRET.GT.0) GO TO 999
C                                       Loop Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1100) IRET
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
C                                       Do we need this baseline?
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS)) THEN
C                                       do the following UVW
C                                       transformations only if U,V
C                                       plotting (for any case)
                  IF ((TYPEAX(1).EQ.6 .AND. TYPEAX(2).EQ.7) .OR.
     *               (TYPEAX(1).EQ.7 .AND. TYPEAX(2).EQ.6)) THEN
C
C                                       convert vis to positive W
                     IF (RPARM(ILOCW+1) .LE. 0.0) THEN
                        RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                        RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                        RPARM(ILOCW+1) = -RPARM(ILOCW+1)
                        END IF
C                                       convert to the mirror UV
                     IF (DOUVMI) THEN
                        RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                        RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                        END IF
C                                       reject if W.LT.WIN .OR.
C                                       W.GT.WMAX
                     IF ((RPARM(ILOCW+1).LT.WMIN) .OR.
     *                  (RPARM(ILOCW+1).GT.WMAX)) GO TO 100
                     END IF
C
                  NUMVIS = NUMVIS + 1
                  IF (MOD(NUMVIS,INC).EQ.0) THEN
                     IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
                        CSOU = CURSOU
                        CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                           CALL MSGWRT (6)
                           END IF
                        END IF
C                                       Find scales
                     CALL FNDXY (RPARM, BUFF1, NP, XZY)
                     ICO = ECHAN - BCHAN + 1
                     LC = 0
                     DO 135 LF = BIF,EIF
                        DO 130 IC = 1,ICO,CHINC
                           LC = LC + 1
                           DO 125 IP = 1,NPOL
                              IF ((XZY(1,IP,LC).NE.FBLANK) .AND.
     *                           (XZY(2,IP,LC).NE.FBLANK)) THEN
                                 IF ((SPHASE) .AND.
     *                              (XZY(1,IP,LC).LT.0.0)) XZY(1,IP,LC)
     *                              = XZY(1,IP,LC)+ 360.0
                                 X = XSCL * (XZY(1,IP,LC) - XOFF)
                                 IF (DOWT) WT = XZY(3,IP,LC)
                                 IF ((X.GE.0.0) .AND. (X.LE.1000.0)
     *                              .AND. (WT.GT.0.0)) THEN
                                    XUMVIS = XUMVIS + 1.0D0
                                    IBIN = 1 + NBIN * X / 1000.0
                                    IBIN = MAX (1, MIN (NBIN, IBIN))
                                    IF (IPHASE.EQ.2) THEN
                                       TR = COS (DG2RAD * XZY(2,IP,LC))
                                       TI = SIN (DG2RAD * XZY(2,IP,LC))
                                       SUM(IBIN) = SUM(IBIN) + WT * TR
                                       SUM2(IBIN) = SUM2(IBIN) +
     *                                    WT * TR*TR
                                       SUMI(IBIN) = SUMI(IBIN) + WT * TI
                                       SUMI2(IBIN) = SUMI2(IBIN) +
     *                                    WT * TI*TI
                                    ELSE
                                       SUM(IBIN) = SUM(IBIN) + WT *
     *                                    XZY(2,IP,LC)
                                       SUM2(IBIN) = SUM2(IBIN) + WT *
     *                                    (XZY(2,IP,LC)**2)
                                       END IF
                                    XCNT(IBIN) = XCNT(IBIN) + WT
                                    KCNT(IBIN) = KCNT(IBIN) + 1
                                    END IF
                                 END IF
 125                          CONTINUE
 130                       CONTINUE
 135                    CONTINUE
                     END IF
                  END IF
               GO TO 100
               END IF
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
      SUBARR = JSUB
      IRET = 0
C                                       Any valid points
      IF (XUMVIS.LE.1.0D0) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
      WRITE (MSGTXT,1201) XUMVIS, NBIN
      CALL REFRMT (MSGTXT, '_', IBIN)
      CALL MSGWRT (2)
C                                       set scale
      IF (IPHASE.NE.2) THEN
         XYMIN(2) = 1.E20
         XYMAX(2) = -1.E20
         DO 210 IBIN = 1,NBIN
            IF (KCNT(IBIN).GT.0) THEN
               AVERG = SUM(IBIN) / XCNT(IBIN)
               STDEV = 0.0
               IF (KCNT(IBIN).GE.2) STDEV = SQRT (ABS ((SUM2(IBIN) /
     *            XCNT(IBIN)) - AVERG*AVERG)/(KCNT(IBIN) - 1))
               XYMIN(2) = MIN (XYMIN(2), AVERG-STDEV)
               XYMAX(2) = MAX (XYMAX(2), AVERG+STDEV)
               END IF
 210        CONTINUE
         IF ((XYMIN(2).GT.0.0) .AND. (XYMIN(2).LT.0.15*XYMAX(2)))
     *      XYMIN(2) = 0.0
      ELSE
         XYMIN(2) = 1.E20
         XYMAX(2) = -1.E20
         IF (IPHASE.GT.1) THEN
            XYMIN(3) = 1.E20
            XYMAX(3) = -1.E20
            END IF
         DO 220 IBIN = 1,NBIN
            IF (KCNT(IBIN).GT.0) THEN
               TR = SUM(IBIN) / XCNT(IBIN)
               TI = SUMI(IBIN) / XCNT(IBIN)
               AVERG = RAD2DG * ATAN2 (TI, TR)
               STDEV = 0.0
               IF (KCNT(IBIN).GE.2) THEN
                  TR = SUM2(IBIN)/XCNT(IBIN) - TR*TR
                  TI = SUMI2(IBIN)/XCNT(IBIN) - TI*TI
                  STDEV = RAD2DG * SQRT (ABS(TR+TI)/(KCNT(IBIN) - 1))
                  END IF
               XYMIN(2) = MIN (XYMIN(2), AVERG-STDEV)
               XYMAX(2) = MAX (XYMAX(2), AVERG+STDEV)
               IF (IPHASE.GT.1) THEN
                  IF (AVERG.LT.0.0) AVERG = AVERG + 360.0
                  XYMIN(3) = MIN (XYMIN(3), AVERG-STDEV)
                  XYMAX(3) = MAX (XYMAX(3), AVERG+STDEV)
                  END IF
               END IF
 220        CONTINUE
         IF ((XYMIN(2).GT.0.0) .AND. (XYMIN(2).LT.0.15*XYMAX(2)))
     *      XYMIN(2) = 0.0
         IF (IPHASE.GT.1) THEN
            IF ((XYMIN(3).GT.0.0) .AND. (XYMIN(3).LT.0.15*XYMAX(3)))
     *         XYMIN(3) = 0.0
            END IF
         END IF
      IRET = 0
      GO TO 999

C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing FREQID =',I3)
 1100 FORMAT ('SCLBIN: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('FOUND',F8.0,' POINTS: NOT ENOUGH TO SELF-SCALE')
 1201 FORMAT ('SCLBIN: Binned',F13.0,' samples in',I5,' bins')
      END
      SUBROUTINE PLPLAN (NXY, TVPLAN, PTVPLN, NP, XZY, IRET)
C-----------------------------------------------------------------------
C   PLPLAN plots uv data into an array of 0's and 1's
C   Inputs:
C      NP       I      Number polarizations
C   Output:
C      NXY      I(3)   Dimensions of image
C      TVPLAN   I(2)   Base address of plane
C      PTVPLN   L      Pointer to actual plane
C      XZY      R(*)   Work buffer for data (3, NP, *)
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                       1 => failed to add to catalog
C                       2 => failed to create
C                       3 => graph file write error
C                       4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NXY(3), TVPLAN(2), NP, IRET
      LONGINT   PTVPLN
      REAL      XZY(3,NP,*)
C
      INCLUDE 'UVPLT.INC'
      CHARACTER PFILE*48, AUNITS(NUMPRM)*8, CHTYPE(NUMPRM)*9,
     *   CHTYP2(NUMPRM)*9, BUNITS(NUMPRM)*8, BNDCOD(MAXIF)*8
      INTEGER   BUFFER(256), IERR, ITYPE, IPSIZE, I, LTYPE, LUNPL, IBIN,
     *   FINDPL, IAPARM(8), INP, IC, J, JJJ, NBIN, IROUND, NGOOD, ICO,
     *   NNOFIT, NUMVIS, JSUB, ISUB, NXLUN, NIF, NXVER, LC, LF, XUMVIS,
     *   IP, ISOU, LSOU, LIF, NWORDS, ICOL(3)
      REAL      BLC(2), TRC(2), CHOUT(4), DX, DY, TR, TI, XY(2), TEMP,
     *   SPIX(3), SAVMIN(3), SAVMAX(3), COLV, DCOLV
      DOUBLE PRECISION WT
      HOLLERITH CATH(256)
      LOGICAL   GOOD, NOCHK, REQBAS, DOWT, REQAS, FIRST
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA LUNPL /26/
      DATA AUNITS /'Janskys ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Janskys ',
     *   'Janskys ', 'IAT hrs ', 'log (Jy)', '1/Jy**2', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
      DATA BUNITS /'Kelvins ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Kelvins ',
     *   'Kelvins ', 'IAT hrs ', 'log (K)', '1/(K**2)', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
      DATA CHTYPE /'Amplitude', 'Phase   ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'U       ', 'V       ',
     *   'W        ', 'Real    ', 'Imag    ', 'Time hrs',
     *   'log (amp)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
      DATA CHTYP2 /'Flux', 'Offset', 'UV dist', 'UV pa', 'Time',
     *   'Longitude', 'Latitude', 'W', 'Flux', 'Offset', 'Time hrs',
     *   'log(flux)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=', 'Azimuth', 'Frequency', 'Channel'/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      XUMVIS = 0
      IRET = 1
      CSOU = -1
      LSOU = -1
      SPIX(1) = 0.0
      SPIX(2) = -1.E6
      SPIX(3) = 0.0
      CALL RCOPY (3, XYMIN, SAVMIN)
      CALL RCOPY (3, XYMAX, SAVMAX)
C                                       See if data range check
      NOCHK = ((BPARM(8).GE.0.5) .AND. (FIXSCL.GT.0))
C                                       14,15,16,18 ha,el,pa,az
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     * ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
      REQAS = REQAS .OR. (TYPEAX(1).EQ.18) .OR. (TYPEAX(2).EQ.18)
C                                       Prepare for possible binning of
C                                       data
      NBIN = IROUND (ABS (BPARM(8)))
      DOWT = XDOWT.GT.0.0
      IF (NBIN.GT.MAXB) NBIN = MAXB
      SPHASE = .FALSE.
      IF ((IPHASE.GT.0) .AND.
     *   (XYMAX(3)-XYMIN(3).LT.XYMAX(IPHASE)-XYMIN(IPHASE))) THEN
         SPHASE = .TRUE.
         XYMIN(IPHASE) = XYMIN(3)
         XYMAX(IPHASE) = XYMAX(3)
         END IF
C                                       User sets the scales
C                                       Note that case of FIXSCL<0
C                                       is handled in setting XYMIN
C                                       and XYMAX.
      IF (FIXSCL.GT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = BPARM(4)
            XYMAX(1) = BPARM(5)
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = BPARM(6)
            XYMAX(2) = BPARM(7)
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
      IF (FIXSCL.LT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = MAX (XYMIN(1), BPARM(4))
            XYMAX(1) = MIN (XYMAX(1), BPARM(5))
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = MAX (XYMIN(2), BPARM(6))
            XYMAX(2) = MIN (XYMAX(2), BPARM(7))
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
C                                       Provide room at edges.
      DO 10 I = 1,2
         TEMP = 0.025 * (XYMAX(I) - XYMIN(I))
         IF (XYMIN(I).NE.BPARM(2*I+2)) XYMIN(I) = XYMIN(I) - TEMP
         IF (XYMAX(I).NE.BPARM(2*I+3)) XYMAX(I) = XYMAX(I) + TEMP
 10      CONTINUE
C                                       Now set the offset and scale.
      DO 15 I = 1,2
         IF (XYMAX(I).EQ.XYMIN(I)) GO TO 999
         IF (INVAX(I)) THEN
            XYOFF(I) = XYMAX(I)
            XYSCL(I) = 1000. / (XYMIN(I)-XYMAX(I))
         ELSE
            XYOFF(I) = XYMIN(I)
            XYSCL(I) = 1000. / (XYMAX(I)-XYMIN(I))
            END IF
 15      CONTINUE
      CALL RCOPY (3, SAVMIN, XYMIN)
      CALL RCOPY (3, SAVMAX, XYMAX)
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 8
C                                      Initialize UV reading
      ISUVR = (UVRNG(1).GE.0.0) .AND. (UVRNG(2).GT.UVRNG(1))
      IF ((UVRNG(1).EQ.0.0) .AND. (UVRNG(2).GE.1.E10)) ISUVR = .FALSE.
      XUVRA(1) = UVRNG(1)
      XUVRA(2) = UVRNG(2)
      CALL RCOPY (8, TIMRNG, XTIME)
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XCHINC = CHINC
C                                       UV range set
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARMS, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF ((DOTV) .AND. (TYPEAX(1)*TYPEAX(2).NE.42)) THEN
            DX = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *         + CHOUT(3))
            DY = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *         + CHOUT(4))
            XYRATO = 1.0
            IF (DY.GT.0.0) XYRATO = DX / DY
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      IF ((.NOT.DOTV) .AND. (DOPLAN.GT.0) .AND. (XYRATO.NE.1.0)) THEN
         IF (XYRATO.GT.1.0) THEN
            I = 1000.0 * XYRATO + 0.5
            TRC(1) = I
            XYSCL(1) = XYSCL(1) * TRC(1) / 1000.0
         ELSE
            I = 1000.0 / XYRATO + 0.5
            TRC(2) = I
            XYSCL(2) = XYSCL(2) * TRC(2) / 1000.0
            END IF
         END IF
      IRET = 3
      CALL FILL (5, 1, IAPARM)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.11) LABTYP(LOCNUM) = 7
      IF (TYPEAX(2).EQ.11) LABTYP(LOCNUM) = 70
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = 1000.0 / XYSCL(I)
         TI = TR
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
         IF (TYPEAX(I).NE.11) THEN
            CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
         ELSE
            CPREF(I,LOCNUM) = ' '
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * 360.
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * 360.
            CTYP(I,LOCNUM) = AUNITS(11)
            GO TO 30
            END IF
C                                       interferometer
         IF (TYPUVD.LE.0) THEN
            CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
C                                       single dish
         ELSE
            CTYP(I,LOCNUM) = BUNITS(TYPEAX(I))
            END IF
 30      CONTINUE
C                                       character surrounding
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF ((LTYPE.GT.2) .AND. (INP.GT.0)) CHOUT(1) = INP + 4
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   CHOUT(4) = CHOUT(4) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         INP = 2
         IF (DOFQSL) INP = INP + 1
         IF (.NOT.ISUVR) INP = INP - 1
         IF ((TIMRNG(1).LE.-1.0E5) .AND. (TIMRNG(5).GE.1.0E5))
     *      INP = INP - 1
         IF (REQAS) INP = INP + 1
         CHOUT(2) = CHOUT(2) + 1.333 * INP
         CHOUT(4) = CHOUT(4) + 1.333 + 1.5
         END IF
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       make plane memory: TV
      IF (DOPLAN.LE.2) THEN
         NXY(1) = GPHSCX + 1.01
         NXY(2) = GPHSCY + 1.01
      ELSE
         NXY(1) = TRC(1) - BLC(1) + 1.01
         NXY(2) = TRC(2) - BLC(2) + 1.01
         END IF
      NXY(3) = 1
      IF (MOD(DOPLAN,2).EQ.0) NXY(3) = 3
      NWORDS = (NXY(1) * NXY(2) * NXY(3) - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', 'PLPLAN', NWORDS, TVPLAN, PTVPLN, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         PTVPLN = 0
         DOPLAN = 0
         IRET = 0
         GO TO 999
         END IF
      NWORDS = NXY(1) * NXY(2) * NXY(3)
      CALL FILL (NWORDS, 0, TVPLAN(1+PTVPLN))
      JJJ = (ECHAN - BCHAN) / CHINC + 1
      JJJ = (EIF - BIF + 1) * JJJ * NPOL
      IF (DO3COL.GT.0.0) DCOLV = 0.97 / (JJJ - 1.0)
C                                       Init vis file for read.
      DX = FACTOR
      DY = FACTOR
      IF (DX/XYRATO.LT.FACTOR) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      NUMVIS = 0
      WT = 1.0D0
C                                       Loop for each FREQID.
      NXLUN = 100
      NXVER = 1
      JSUB = SUBARR
C                                       skip if binned & already done
      FIRST = .TRUE.
      DO 150 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) FRQSEL = IFRQ
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (EXCLFQ(ISUB,IFRQ).NE.0) GO TO 145
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Initialize UV reading.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1050) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 970
               END IF
C                                       Loop: Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
               IF (IRET.EQ.-1) GO TO 140
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  CALL MSGWRT (8)
                  IRET = 4
                  GO TO 970
                  END IF
C                                       Check whether we need this
C                                       baseline
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *            GO TO 100
C                                       do the following UVW
C                                       transformations only if U,V
C                                       plotting (for any case LK)
               IF ((TYPEAX(1).EQ.6 .AND. TYPEAX(2).EQ.7) .OR.
     *               (TYPEAX(1).EQ.7 .AND. TYPEAX(2).EQ.6)) THEN
C
C                                       convert vis to positive W
                  IF (RPARM(ILOCW+1) .LE. 0.0) THEN
                     RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                     RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                     RPARM(ILOCW+1) = -RPARM(ILOCW+1)
                     END IF
C                                       convert to the mirror UV
                  IF (DOUVMI) THEN
                     RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                     RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                     END IF
C                                       reject if W.LT.WIN .OR.
C                                       W.GT.WMAX
                  IF (RPARM(ILOCW+1) .LT. WMIN .OR.
     *                  RPARM(ILOCW+1) .GT. WMAX) GO TO 100
                  END IF
C
               NUMVIS = NUMVIS + 1
               IF (MOD(NUMVIS,INC).NE.0) GO TO 100
C                                       scaling info
               IF (DOSCAL.GT.0.0) THEN
                  ISOU = 0
                  IF (ILOCSU.GE.0) ISOU = IROUND (RPARM(1+ILOCSU))
                  IF (ISOU.LE.0) ISOU = INSNUM
                  IF (ISOU.NE.LSOU) THEN
                     LSOU = ISOU
                     CALL GETSOU (LSOU, DISKIN, CNOIN, CATUV, NXLUN,
     *                  IRET)
                     DO 115 LIF = BIF,EIF
                        IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 115                    CONTINUE
                     IF (DOSCAL.GT.1.5) THEN
                        IP = 1
                        IF (DOSCAL.GT.2.5) IP = 2
                        SPIX(3) = 0.0
                        CALL FNDSPX (DISKIN, CNOIN, LSOU, FRQSEL,
     *                     CATUV, IP, SPIX, IRET)
                     ELSE
                        SPIX(1) = 0.0
                        SPIX(2) = 0.0
                        SPIX(3) = 0.0
                        END IF
                     END IF
               ELSE IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                     CALL MSGWRT (6)
                     END IF
                  END IF
C                                       Get and scale X, Y
               CALL UVPLTS (FLUX, SPIX, NPOL, BUFF1)
               CALL FNDXY (RPARM, BUFF1, NP, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               COLV = 0.0
               ICOL(1) = 32767
               ICOL(2) = 0
               ICOL(3) = 0
               DO 135 LF = BIF,EIF
                  DO 130 IC = 1,ICO,CHINC
                     LC = LC + 1
                     DO 125 IP = 1,NPOL
                        IF (DO3COL.GT.0.0) THEN
                           CALL PLCOL3 (COLV, ICOL)
                           COLV = COLV + DCOLV
                           END IF
                        IF ((XZY(1,IP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,IP,LC).NE.FBLANK)) THEN
                           IF ((SPHASE) .AND. (IPHASE.GT.0)) THEN
                              TEMP = XZY(IPHASE,IP,LC)
                              IF (TEMP.LT.0) TEMP = TEMP + 360.0
                              XZY(IPHASE,IP,LC) = TEMP
                              END IF
                           DO 124 JJJ = 1,2
                              DO 120 J = 1,2
                                 XY(J) = XYSCL(J) * (XZY(J,IP,LC) -
     *                              XYOFF(J))
 120                             CONTINUE
                              IF (DOWT) WT = XZY(3,IP,LC)
C                                       If binning - accumulate
                              IF ((NBIN.GT.0) .AND. (WT.GT.0.0) .AND.
     *                           (XY(1).GE.BLC(1)) .AND.
     *                           (XY(1).LE.TRC(1))) THEN
                                 XUMVIS = XUMVIS + 1
                                 IBIN = 1 + NBIN * XY(1) / 1000.
                                 IF (IBIN.LT.1) IBIN = 1
                                 IF (IBIN.GT.NBIN) IBIN = NBIN
                                    IF (IBIN.EQ.2) THEN
                                       MSGTXT = 'WE ARE HERE'
                                       END IF
                                 IF (IPHASE.EQ.2) THEN
                                    TR = COS (DG2RAD * XZY(2,IP,LC))
                                    TI = SIN (DG2RAD * XZY(2,IP,LC))
                                    SUM(IBIN) = SUM(IBIN) + WT * TR
                                    SUM2(IBIN) = SUM2(IBIN) + WT * TR*TR
                                    SUMI(IBIN) = SUMI(IBIN) + WT * TI
                                    SUMI2(IBIN) = SUMI2(IBIN) + WT*TI*TI
                                 ELSE
                                    SUM(IBIN) = SUM(IBIN) + WT *
     *                                 XZY(2,IP,LC)
                                    SUM2(IBIN) = SUM2(IBIN) + WT *
     *                                 (XZY(2,IP,LC)**2)
                                    END IF
                                 XCNT(IBIN) = XCNT(IBIN) + WT
                                 KCNT(IBIN) = KCNT(IBIN) + 1
                                 END IF
C                                       Mark the point
                              IF ((BPARM(8).LE.0.0) .AND. (WT.GT.0.0))
     *                           THEN
                                 IF ((XY(1).LT.BLC(1)) .OR.
     *                              (XY(1).GT.TRC(1)) .OR.
     *                              (XY(2).LT.BLC(2)) .OR.
     *                              (XY(2).GT.TRC(2))) THEN
                                    NNOFIT = NNOFIT + 1
                                 ELSE
                                    NGOOD = NGOOD + 1
                                    IF ((.NOT.FIRST) .AND. (DOLINE))
     *                                 THEN
                                       CALL GPLVEC (XY(1), XY(2), ICOL,
     *                                    NXY, DOPLAN, TVPLAN(1+PTVPLN),
     *                                    IRET)
                                       IF (IRET.NE.0) GO TO 970
                                       END IF
                                    CALL GPLPOS (XY(1)+DX, XY(2),
     *                                 DOPLAN, TVPLAN(1+PTVPLN), IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    CALL GPLVEC (XY(1)-DX, XY(2), ICOL,
     *                                 NXY, DOPLAN, TVPLAN(1+PTVPLN),
     *                                 IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    CALL GPLPOS (XY(1), XY(2)+DY,
     *                                 DOPLAN, TVPLAN(1+PTVPLN), IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    CALL GPLVEC (XY(1), XY(2)-DY, ICOL,
     *                                 NXY, DOPLAN, TVPLAN(1+PTVPLN),
     *                                 IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    IF (DOLINE) THEN
                                       CALL GPLPOS (XY(1), XY(2),
     *                                    DOPLAN, TVPLAN(1+PTVPLN),
     8                                    IRET)
                                       IF (IRET.NE.0) GO TO 970
                                       FIRST = .FALSE.
                                       END IF
                                    END IF
                                 END IF
                              IF (.NOT.UVREV) GO TO 125
                                 XZY(1,IP,LC) = -XZY(1,IP,LC)
                                 XZY(2,IP,LC) = -XZY(2,IP,LC)
 124                             CONTINUE
                           END IF
 125                    CONTINUE
 130                 CONTINUE
 135              CONTINUE
               GO TO 100
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
      IF (DOTV) THEN
         CALL TVCLOS (BUFF1, IRET)
      ELSE
         CALL ZCLOSE (LUNPL, FINDPL, IRET)
         END IF
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      GO TO 999
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      IF (NGOOD.GT.0) CALL MSGWRT (2)
      WRITE (MSGTXT,1991) NNOFIT
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('PLPLAN: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('PLPLAN: ERROR',I3,' READING VIS FILE')
 1970 FORMAT ('PLPLAN: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
 1990 FORMAT ('PLPLAN: ',I10,' Points plotted')
 1991 FORMAT ('PLPLAN: ',I10,' Points did not fit')
      END
      SUBROUTINE GPLPOS (X, Y, DOPLAN, BUFF, IERR)
C-----------------------------------------------------------------------
C   Inputs:
C      X        R      X position
C      Y        R      Y position
C      DOPLAN   I      > 2 -> plot file
C   Outputs
C      BUFF     I(*)   unused
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   DOPLAN, BUFF(*), IERR
      REAL      X, Y
C
      REAL      RX, RY, RLIM
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Scale X and Y.
      RLIM = 4.0
      IF (GPHIX2.EQ.GPHIX1) THEN
         RX = 0.0
      ELSE
         RX = (X - GPHX1) / (GPHX2 - GPHX1)
         END IF
      IF (GPHIY2.EQ.GPHIY1) THEN
         RY = 0.0
      ELSE
         RY = (Y - GPHY1) / (GPHY2 - GPHY1)
         END IF
      RX = MAX (-RLIM, MIN (RLIM, RX))
      RY = MAX (-RLIM, MIN (RLIM, RY))
C                                       tv
      IF (DOPLAN.LE.2) THEN
         GPHIXL = RX * GPHSCX + 1.5
         GPHIYL = RY * GPHSCY + 1.5
      ELSE
         GPHIXL = RX * (GPHIX2 - GPHIX1) + GPHIX1 + 1.5
         GPHIYL = RY * (GPHIY2 - GPHIY1) + GPHIY1 + 1.5
         END IF
C
 999  RETURN
      END
      SUBROUTINE GPLVEC (X, Y, ICOL, NXY, DOPLAN, TVPLAN, IERR)
C-----------------------------------------------------------------------
C   Inputs:
C      X        R      X position
C      Y        R      Y position
C      ICOL     I(3)   RGB color to use
C      NXY      I(3)   dimensions of BUFF
C      DOPLAN   I      > 2 -> plot file
C   Outputs
C      TVPLAN   I(*)   unused
C      IERR     i      error code
C-----------------------------------------------------------------------
      INTEGER   ICOL(3), NXY(3), DOPLAN, TVPLAN(NXY(1),NXY(2),*), IERR
      REAL      X, Y
C
      REAL      RX, RY, RLIM, ALPHA, BETA
      INTEGER   IXN, IYN, X1, X2, Y1, Y2, XLIM, YLIM, ZLIM
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Scale X and Y.
      RLIM = 4.0
      IF (GPHIX2.EQ.GPHIX1) THEN
         RX = 0.0
      ELSE
         RX = (X - GPHX1) / (GPHX2 - GPHX1)
         END IF
      IF (GPHIY2.EQ.GPHIY1) THEN
         RY = 0.0
      ELSE
         RY = (Y - GPHY1) / (GPHY2 - GPHY1)
         END IF
      RX = MAX (-RLIM, MIN (RLIM, RX))
      RY = MAX (-RLIM, MIN (RLIM, RY))
      XLIM = NXY(1)
      YLIM = NXY(2)
      ZLIM = NXY(3)
C                                       scale
      IF (DOPLAN.LE.2) THEN
         IXN = RX * GPHSCX + 1.5
         IYN = RY * GPHSCY + 1.5
      ELSE
         IXN = RX * (GPHIX2 - GPHIX1) + GPHIX1 + 1.5
         IYN = RY * (GPHIY2 - GPHIY1) + GPHIY1 + 1.5
         END IF
      X1 = MAX (1, MIN (XLIM, GPHIXL))
      Y1 = MAX (1, MIN (YLIM, GPHIYL))
      X2 = MAX (1, MIN (XLIM, IXN))
      Y2 = MAX (1, MIN (YLIM, IYN))
      IF ((X1.NE.GPHIXL) .OR. (Y1.NE.GPHIYL)) THEN
         ALPHA = 1.0
         IF (IXN.NE.GPHIXL) ALPHA = REAL(X1-IXN)/REAL(GPHIXL-IXN)
         BETA = 1.0
         IF (IYN.NE.GPHIYL) BETA = REAL(Y1-IYN)/REAL(GPHIYL-IYN)
         ALPHA = MIN (ALPHA, BETA)
         X1 = IXN + ALPHA * (GPHIXL-IXN) + 0.5
         Y1 = IYN + ALPHA * (GPHIYL-IYN) + 0.5
         END IF
      IF ((X2.NE.IXN) .OR. (Y2.NE.IYN)) THEN
         ALPHA = 1.0
         IF (IXN.NE.GPHIXL) ALPHA = REAL(X2-GPHIXL)/REAL(IXN-GPHIXL)
         BETA = 1.0
         IF (IYN.NE.GPHIYL) BETA = REAL(Y2-GPHIYL)/REAL(IYN-GPHIYL)
         ALPHA = MIN (ALPHA, BETA)
         X2 = GPHIXL + ALPHA * (IXN-GPHIXL) + 0.5
         Y2 = GPHIYL + ALPHA * (IYN-GPHIYL) + 0.5
         END IF
      CALL PLVECT (XLIM, YLIM, ZLIM, ICOL, X1, Y1, X2, Y2, TVPLAN)
C
 999  RETURN
      END
      SUBROUTINE PLVECT (NX, NY, NZ, ICOL, X1, Y1, X2, Y2, TVPLAN)
C-----------------------------------------------------------------------
C   Fills a line in an array
C   Inputs:
C      NX       I      X dimension of plane
C      NY       I      Y dimension of plane
C      NZ       I      Z dimension of plane
C      ICOL     I(3)   rgb colors to use
C      X1       I      X of first point
C      Y1       i      Y of first point
C      X2       I      X of 2nd point
C      Y2       I      Y of 2nd point
C   Output:
C      TVPLAN   I(*)   plane
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, ICOL(3), X1, X2, Y1, Y2, TVPLAN(NX,NY,*)
C
      REAL      X, Y, SLOPE
      INTEGER   IX, IY, IZ, IROUND
C-----------------------------------------------------------------------
      IX = ABS (X2-X1)
      IY = ABS (Y2-Y1)
C                                       loop over X
      IF (IX.GE.IY) THEN
         SLOPE = FLOAT (Y2 - Y1) / FLOAT (X2 - X1)
         IF (X1.LT.X2) THEN
            DO 10 IX = X1,X2
               Y = Y1 + SLOPE * (IX - X1)
               IY = IROUND (Y)
               IF ((IX.GE.1) .AND. (IX.LE.NX) .AND. (IY.GE.1) .AND.
     *            (IY.LE.NY)) THEN
                  DO 5 IZ = 1,NZ
                     TVPLAN(IX,IY,IZ) = ICOL(IZ)
 5                   CONTINUE
                  END IF
 10            CONTINUE
         ELSE IF (X2.LT.X1) THEN
            DO 20 IX = X2,X1
               Y = Y1 + SLOPE * (IX - X1)
               IY = IROUND (Y)
               IF ((IX.GE.1) .AND. (IX.LE.NX) .AND. (IY.GE.1) .AND.
     *            (IY.LE.NY)) THEN
                  DO 15 IZ = 1,NZ
                     TVPLAN(IX,IY,IZ) = ICOL(IZ)
 15                  CONTINUE
                  END IF
 20            CONTINUE
C                                       single pixel
         ELSE
            IX = X1
            IY = Y1
            IF ((IX.GE.1) .AND. (IX.LE.NX) .AND. (IY.GE.1) .AND.
     *         (IY.LE.NY)) THEN
               DO 22 IZ = 1,NZ
                  TVPLAN(IX,IY,IZ) = ICOL(IZ)
 22               CONTINUE
               END IF
            END IF
C                                       loop over Y
      ELSE
         SLOPE = FLOAT (X2 - X1) / FLOAT (Y2 - Y1)
         IF (Y1.LT.Y2) THEN
            DO 30 IY = Y1,Y2
               X = X1 + SLOPE * (IY - Y1)
               IX = IROUND (X)
               IF ((IX.GE.1) .AND. (IX.LE.NX) .AND. (IY.GE.1) .AND.
     *            (IY.LE.NY)) THEN
                  DO 25 IZ = 1,NZ
                     TVPLAN(IX,IY,IZ) = ICOL(IZ)
 25                  CONTINUE
                  END IF
 30            CONTINUE
         ELSE
            DO 40 IY = Y2,Y1
               X = X1 + SLOPE * (IY - Y1)
               IX = IROUND (X)
               IF ((IX.GE.1) .AND. (IX.LE.NX) .AND. (IY.GE.1) .AND.
     *            (IY.LE.NY)) THEN
                  DO 35 IZ = 1,NZ
                     TVPLAN(IX,IY,IZ) = ICOL(IZ)
 35                  CONTINUE
                  END IF
 40            CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLCOL3 (COLV, ICOL)
C-----------------------------------------------------------------------
C   Color integers returned
C   Input:
C      COLV   R      Color level 0 - 1
C   Output
C      ICOL   I(3)   R, G, B colors (0 to 1) * 32767
C-----------------------------------------------------------------------
      REAL      COLV
      INTEGER   ICOL(3)
C
      INTEGER   I
      REAL      COL(3)
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IF (MAXINT.LE.0) MAXINT = 8191
      CALL COLOR3 (COLV, .FALSE., COL)
      DO 10 I = 1,3
         ICOL(I) = COL(I) * MAXINT + 0.5
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLTUV (NX, NY, NZ, TVPLAN, NP, XZY, IRET)
C-----------------------------------------------------------------------
C   PLTUV actually plots uv data.
C   Input
C      NX       I       X dimension of TVPLAN
C      NY       I       Y dimension of TVPLAN
C      TVPLAN   I(*)    Plane filled with 1's and 0's to plot if NX>0
C      NP       I       Number of polarizations
C   Output:
C      XZY      R(*)    Data buffer (3,NP,*)
C      IRET     I       Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), NP, IRET
      REAL      XZY(3,NP,*)
C
      INCLUDE 'UVPLT.INC'
      CHARACTER TEXT*80, PFILE*48, TIME*8, DATE*12, AUNITS(NUMPRM)*8,
     *   CHTYPE(NUMPRM)*9, CHTYP2(NUMPRM)*9, CTEMP*18, BUNITS(NUMPRM)*8,
     *   BNDCOD(MAXIF)*8
      INTEGER   BUFFER(256), IERR, ITYPE, IPSIZE, I, ITRIM, LTYPE,
     *   LUNPL, FINDPL, IAPARM(8), INCHAR, INP, IC, J, JJJ, NBIN, IBIN,
     *   IROUND, IT(3), ID(3), NGOOD, ICO, NNOFIT, ITIM(8), NUMVIS,
     *   PFQSID(MAXIF), JSUB, ISUB, NXLUN, NIF, NXVER, LC, LF, IROTAT,
     *   NBOFF, IP, ITEMP, ISOU, LSOU, LIF
      REAL      BLC(2), TRC(2), CHOUT(4), DX, DY, TR, TI, AVERG, STDEV,
     *   PBW, UVR(2), PFQTBW(MAXIF), PFQCHW(MAXIF), XY(2), TEMP,
     *   COL(3), COLV, DCOLV, SPIX(3), X, Y
      DOUBLE PRECISION PFQFRQ(MAXIF), PFREQ, WT, XUMVIS
      HOLLERITH CATH(256)
      LOGICAL   GOOD, NOCHK, DOGRID, REQBAS, DOWT, REQAS, FIRST
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA LUNPL /26/
      DATA AUNITS /'Janskys ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Janskys ',
     *   'Janskys ', 'IAT hrs ', 'log (Jy)', '1/Jy**2', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
      DATA BUNITS /'Kelvins ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Kelvins ',
     *   'Kelvins ', 'IAT hrs ', 'log (K)', '1/(K**2)', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
      DATA CHTYPE /'Amplitude', 'Phase   ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'U       ', 'V       ',
     *   'W        ', 'Real    ', 'Imag    ', 'Time hrs',
     *   'log (amp)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
      DATA CHTYP2 /'Flux', 'Offset', 'UV dist', 'UV pa', 'Time',
     *   'Longitude', 'Latitude', 'W', 'Flux', 'Offset', 'Time hrs',
     *   'log(flux)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=', 'Azimuth', 'Frequency', 'Channel'/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      XUMVIS = 0.0D0
      IRET = 1
      CSOU = -1
      LSOU = -1
      SPIX(1) = 0.0
      SPIX(2) = -1.E6
      SPIX(3) = 0.0
C                                       See if data range check
      NOCHK = ((BPARM(8).GE.0.5) .AND. (FIXSCL.GT.0))
C                                       14,15,16,18 ha,el,pa,az
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     * ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
      REQAS = REQAS .OR. (TYPEAX(1).EQ.18) .OR. (TYPEAX(2).EQ.18)
C                                       Prepare for possible binning of
C                                       data
      NBIN = IROUND (ABS (BPARM(8)))
      DOWT = XDOWT.GT.0.0
      IF (NBIN.GT.MAXB) NBIN = MAXB
      SPHASE = .FALSE.
      IF ((IPHASE.GT.0) .AND.
     *   (XYMAX(3)-XYMIN(3).LT.XYMAX(IPHASE)-XYMIN(IPHASE))) THEN
         SPHASE = .TRUE.
         XYMIN(IPHASE) = XYMIN(3)
         XYMAX(IPHASE) = XYMAX(3)
         END IF
C                                       User sets the scales
C                                       Note that case of FIXSCL<0
C                                       is handled in setting XYMIN
C                                       and XYMAX.
      IF (FIXSCL.GT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = BPARM(4)
            XYMAX(1) = BPARM(5)
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = BPARM(6)
            XYMAX(2) = BPARM(7)
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
      IF (FIXSCL.LT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = MAX (XYMIN(1), BPARM(4))
            XYMAX(1) = MIN (XYMAX(1), BPARM(5))
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = MAX (XYMIN(2), BPARM(6))
            XYMAX(2) = MIN (XYMAX(2), BPARM(7))
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
C                                       Provide room at edges.
      DO 10 I = 1,2
         TEMP = 0.025 * (XYMAX(I) - XYMIN(I))
         IF (XYMIN(I).NE.BPARM(2*I+2)) XYMIN(I) = XYMIN(I) - TEMP
         IF (XYMAX(I).NE.BPARM(2*I+3)) XYMAX(I) = XYMAX(I) + TEMP
 10      CONTINUE
C                                       Now set the offset and scale.
      DO 15 I = 1,2
         IF (XYMAX(I).EQ.XYMIN(I)) GO TO 999
         IF (INVAX(I)) THEN
            XYOFF(I) = XYMAX(I)
            XYSCL(I) = 1000. / (XYMIN(I)-XYMAX(I))
         ELSE
            XYOFF(I) = XYMIN(I)
            XYSCL(I) = 1000. / (XYMAX(I)-XYMIN(I))
            END IF
 15      CONTINUE
C                                       Fill in last of actual parms
      BPARM(5) = 1000.0/XYSCL(1) + XYOFF(1)
      BPARM(7) = 1000.0/XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 8
C                                      Initialize UV reading
      ISUVR = (UVRNG(1).GE.0.0) .AND. (UVRNG(2).GT.UVRNG(1))
      IF ((UVRNG(1).EQ.0.0) .AND. (UVRNG(2).GE.1.E10)) ISUVR = .FALSE.
      XUVRA(1) = UVRNG(1)
      XUVRA(2) = UVRNG(2)
      CALL RCOPY (8, TIMRNG, XTIME)
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XCHINC = CHINC
C                                       UV range set
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARMS, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF ((DOTV) .AND. (TYPEAX(1)*TYPEAX(2).NE.42)) THEN
            DX = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *         + CHOUT(3))
            DY = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *         + CHOUT(4))
            XYRATO = 1.0
            IF (DY.GT.0.0) XYRATO = DX / DY
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      IF ((.NOT.DOTV) .AND. (DOPLAN.GT.0) .AND. (XYRATO.NE.1.0)) THEN
         IF (XYRATO.GT.1.0) THEN
            I = 1000.0 * XYRATO + 0.5
            TRC(1) = I
            XYSCL(1) = XYSCL(1) * TRC(1) / 1000.0
         ELSE
            I = 1000.0 / XYRATO + 0.5
            TRC(2) = I
            XYSCL(2) = XYSCL(2) * TRC(2) / 1000.0
            END IF
         END IF
      IRET = 3
      CALL FILL (5, 1, IAPARM)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.11) LABTYP(LOCNUM) = 7
      IF (TYPEAX(2).EQ.11) LABTYP(LOCNUM) = 70
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = 1000.0 / XYSCL(I)
         TI = TR
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
         IF (TYPEAX(I).NE.11) THEN
            CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
         ELSE
            CPREF(I,LOCNUM) = ' '
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * 360.
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * 360.
            CTYP(I,LOCNUM) = AUNITS(11)
            GO TO 30
            END IF
C                                       interferometer
         IF (TYPUVD.LE.0) THEN
            CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
C                                       single dish
         ELSE
            CTYP(I,LOCNUM) = BUNITS(TYPEAX(I))
            END IF
 30      CONTINUE
C                                       character surrounding
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF ((LTYPE.GT.2) .AND. (INP.GT.0)) CHOUT(1) = INP + 4
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   CHOUT(4) = CHOUT(4) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         INP = 2
         IF (DOFQSL) INP = INP + 1
         IF (.NOT.ISUVR) INP = INP - 1
         IF ((TIMRNG(1).LE.-1.0E5) .AND. (TIMRNG(5).GE.1.0E5))
     *      INP = INP - 1
         IF (REQAS) INP = INP + 1
         CHOUT(2) = CHOUT(2) + 1.333 * INP
         CHOUT(4) = CHOUT(4) + 1.333 + 1.5
         END IF
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       If a graphics plane already
      IF (NX.GT.0) THEN
         IF ((DOPLAN.EQ.1) .OR. (DOPLAN.EQ.2)) CALL DRPLAN (NX, NY, NZ,
     *      TVPLAN, BUFFER, IERR)
         IF ((DOPLAN.EQ.3) .OR. (DOPLAN.EQ.4)) CALL GRPLAN (NX, NY, NZ,
     *      TVPLAN, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       Draw border
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
C                                       Top labels: type & name
      DX = 0.0
      DY = CHOUT(4) - 1.5
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (TEXT,1030) VER, DATE, TIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DY = DY - 1.333
         END IF
C                                       Top labels: type & name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INCHAR = 80
         INP = 1
         IF (TYPUVD.LE.0) THEN
            TEXT = CHTYPE(TYPEAX(2))
         ELSE
            TEXT = CHTYP2(TYPEAX(2))
         END IF
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         IF (TYPEAX(2).EQ.17) THEN
            IROTAT = IROUND(ROTATE)
            WRITE (TEXT(INP:),3273) IROTAT
            CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
            INP = INP + 1
         END IF
         TEXT(INP:INP+3) = ' vs '
         INP = INP + 4
         IF (TYPUVD.LE.0) THEN
            TEXT(INP:INP+8) = CHTYPE(TYPEAX(1))
         ELSE
            TEXT(INP:INP+8) = CHTYP2(TYPEAX(1))
         END IF
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         IF (TYPEAX(1).EQ.17) THEN
            IROTAT = IROUND(ROTATE)
            WRITE (TEXT(INP:),3273) IROTAT
            CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
            INP = INP + 1
         END IF
         CALL REFRMT (TEXT, ' ', INP)
         INP = INP + 1
         TEXT(INP:INP+4) = ' for '
         INP = INP + 5
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), CTEMP(1:12))
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CTEMP(13:18))
         CALL NAMEST (CTEMP, SEQIN, TEXT(INP:), INCHAR)
         INCHAR = 80
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         TEXT(INP:) = '   Several Sources'
         IF (NSOUWD.EQ.0) THEN
            IF (.NOT.MULTI) THEN
               TEXT(INP:) = '   Source:'
               INP = INP + 10
               CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
               TEXT(INP:) = CTEMP(:8)
               END IF
         ELSE IF (NSOUWD.EQ.1) THEN
            IF (DOSWNT) THEN
               TEXT(INP:) = '   Source:'
               INP = INP + 10
               TEXT(INP:) = SOURCS(1)
               END IF
            END IF
         INCHAR = 80
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         CALL GCHAR (INP, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DY = DY - 1.333
C                                       antennas and correlators
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (EIF.LE.BIF) THEN
            IF (ECHAN.LE.BCHAN) THEN
               WRITE (TEXT,1031) IANT(1), IBAS(1), STOKES, BIF, BCHAN
            ELSE
               WRITE (TEXT,1032) IANT(1), IBAS(1), STOKES, BIF, BCHAN,
     *            ECHAN
               END IF
         ELSE
            IF (ECHAN.LE.BCHAN) THEN
               WRITE (TEXT,1033) IANT(1), IBAS(1), STOKES, BIF, EIF,
     *            BCHAN
            ELSE
               WRITE (TEXT,1034) IANT(1), IBAS(1), STOKES, BIF, EIF,
     *            BCHAN, ECHAN
               END IF
            END IF
         JJJ = (ECHAN - BCHAN) / CHINC + 1
         JJJ = (EIF - BIF + 1) * JJJ * NPOL
         IF (JJJ.LE.1) DO3COL = -1.0
         IF (DO3COL.GT.0.0) DCOLV = 0.97 / (JJJ - 1.0)
         INCHAR = 80
         INP = 6
         IF ((NANT.GT.1) .AND. (.NOT.DESEL)) TEXT(INP:INP+2) = '  *'
         IF (NANT.EQ.0) TEXT(INP:INP+2) = '  *'
         INP = 11
         IF ((NBAS.NE.1) .AND. (.NOT.DESEL)) TEXT(INP:INP+2) = ' * '
         IF (NBAS.EQ.0) TEXT(INP:INP+2) = ' * '
         IF (DESEL) THEN
            TEXT(1:4) = 'NOT '
            IF ((NANT.GT.1) .OR. (NBAS.GT.1)) TEXT(1:4) = 'NOT*'
            END IF
C                                       W range and NUMVIS
C                                       Substitude the 3d line at the
C                                       plot header only if U,V,W and
C                                       if at least one APARM.NE.0
         ITEMP = TYPEAX(1) * TYPEAX(2)
         IF (((ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56))
     *      .AND. (APARM(1).NE.0 .OR. APARM(2).NE.0 .OR. APARM(3).NE.0))
     *      THEN
            IF (WMAX.GT.1E12) THEN
               WRITE (TEXT,1400) NNVIS
            ELSE
               WRITE (TEXT,1430) WMIN, WMAX, NNVIS
               END IF
            END IF
C
         CALL REFRMT (TEXT, '_', INP)
         CALL GCHAR (INP, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       labels below
         DY = -2.833
         IF (LTYPE.GT.2) DY = DY - 1.333
C                                       refant information
         IF (REQAS) THEN
            INP = 0
            IF ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) INP =
     *         TYPEAX(1)
            IF (TYPEAX(1).EQ.18) INP = 18
            IF (INP.EQ.0) INP = TYPEAX(2)
            CALL GPOS (BLC(1), BLC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            IF (REFANT.GT.0) THEN
               WRITE (TEXT,1040) CHTYPE(INP), REFANT, STNNAM(REFANT)
            ELSE
               WRITE (TEXT,1041) CHTYPE(INP)
               END IF
            CALL REFRMT (TEXT, '_', INP)
            CALL GCHAR (INP, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DY = DY - 1.333
            END IF
C                                       FQ info
         IF (DOFQSL) THEN
            CALL GPOS (BLC(1), BLC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            IF (NFRQ.EQ.1) THEN
               CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATUV, 45, PFQFRQ,
     *            PFQTBW, PFQCHW, PFQSID, BNDCOD, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1080) IERR
                  GO TO 990
                  END IF
               PFREQ = PFQFRQ(BIF) / 1.0D9
               PBW = PFQTBW(BIF) / 1.0E6
               WRITE (TEXT,1060) PFREQ, PBW
            ELSE
               WRITE (TEXT,1036) NFRQ
               END IF
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            DY = DY - 1.333
            END IF
C                                       Time range
         IF ((TIMRNG(1).GE.0.0) .OR. (TIMRNG(5).LE.999)) THEN
            CALL GPOS (BLC(1), BLC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            TR = MAX (0.0, TSTART)
            CALL TODHMS (TR, ITIM(1))
            TR = MIN (999.0, TEND)
            CALL TODHMS (TR, ITIM(5))
            WRITE (TEXT,1035) (ITIM(I), I = 1,8)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DY = DY - 1.333
            END IF
C                                       UV range
         IF (ISUVR) THEN
            CALL GPOS (BLC(1), BLC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            UVR(1) = UVRNG(1) * 1000.
            UVR(2) = UVRNG(2) * 1000.
            WRITE (TEXT,1045) UVR
            INCHAR = ITRIM (TEXT)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Put on labels and ticks
      DOGRID = .FALSE.
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, DOGRID, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Init vis file for read.
      DX = FACTOR
      DY = FACTOR
      IF (DX/XYRATO.LT.FACTOR) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      NUMVIS = 0
      WT = 1.0D0
C                                       Loop for each FREQID.
      NXLUN = 100
      NXVER = 1
      JSUB = SUBARR
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       skip if binned & already done
      IF (DOSBIN) GO TO 200
      IF (NX.GT.0) GO TO 200
      FIRST = .TRUE.
      DO 150 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) FRQSEL = IFRQ
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (EXCLFQ(ISUB,IFRQ).NE.0) GO TO 145
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Initialize UV reading.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1050) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 970
               END IF
C                                       Loop: Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
               IF (IRET.EQ.-1) GO TO 140
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  CALL MSGWRT (8)
                  IRET = 4
                  GO TO 970
                  END IF
C                                       Check whether we need this
C                                       baseline
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *            GO TO 100
C                                       do the following UVW
C                                       transformations only if U,V
C                                       plotting (for any case LK)
               IF ((TYPEAX(1).EQ.6 .AND. TYPEAX(2).EQ.7) .OR.
     *               (TYPEAX(1).EQ.7 .AND. TYPEAX(2).EQ.6)) THEN
C
C                                       convert vis to positive W
                  IF (RPARM(ILOCW+1) .LE. 0.0) THEN
                     RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                     RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                     RPARM(ILOCW+1) = -RPARM(ILOCW+1)
                     END IF
C                                       convert to the mirror UV
                  IF (DOUVMI) THEN
                     RPARM(ILOCU+1) = -RPARM(ILOCU+1)
                     RPARM(ILOCV+1) = -RPARM(ILOCV+1)
                     END IF
C                                       reject if W.LT.WIN .OR.
C                                       W.GT.WMAX
                  IF (RPARM(ILOCW+1) .LT. WMIN .OR.
     *                  RPARM(ILOCW+1) .GT. WMAX) GO TO 100
                  END IF
C
               NUMVIS = NUMVIS + 1
               IF (MOD(NUMVIS,INC).NE.0) GO TO 100
C                                       scaling info
               IF (DOSCAL.GT.0.0) THEN
                  ISOU = 0
                  IF (ILOCSU.GE.0) ISOU = IROUND (RPARM(1+ILOCSU))
                  IF (ISOU.LE.0) ISOU = INSNUM
                  IF (ISOU.NE.LSOU) THEN
                     LSOU = ISOU
                     CALL GETSOU (LSOU, DISKIN, CNOIN, CATUV, NXLUN,
     *                  IRET)
                     DO 115 LIF = BIF,EIF
                        IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 115                    CONTINUE
                     IF (DOSCAL.GT.1.5) THEN
                        IP = 1
                        IF (DOSCAL.GT.2.5) IP = 2
                        SPIX(3) = 0.0
                        CALL FNDSPX (DISKIN, CNOIN, LSOU, FRQSEL,
     *                     CATUV, IP, SPIX, IRET)
                     ELSE
                        SPIX(1) = 0.0
                        SPIX(2) = 0.0
                        SPIX(3) = 0.0
                        END IF
                     END IF
               ELSE IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                     CALL MSGWRT (6)
                     END IF
                  END IF
C                                       Get and scale X, Y
               CALL UVPLTS (FLUX, SPIX, NPOL, BUFF1)
               CALL FNDXY (RPARM, BUFF1, NP, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               COLV = 0.0
               DO 135 LF = BIF,EIF
                  DO 130 IC = 1,ICO,CHINC
                     LC = LC + 1
                     DO 125 IP = 1,NPOL
                        IF (DO3COL.GT.0.0) THEN
                           CALL COLOR3 (COLV, .FALSE., COL)
                           COLV = COLV + DCOLV
                           CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 970
                           END IF
                        IF ((XZY(1,IP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,IP,LC).NE.FBLANK)) THEN
                           IF ((SPHASE) .AND. (IPHASE.GT.0)) THEN
                              TEMP = XZY(IPHASE,IP,LC)
                              IF (TEMP.LT.0) TEMP = TEMP + 360.0
                              XZY(IPHASE,IP,LC) = TEMP
                              END IF
                           DO 124 JJJ = 1,2
                              DO 120 J = 1,2
                                 XY(J) = XYSCL(J) * (XZY(J,IP,LC) -
     *                              XYOFF(J))
 120                             CONTINUE
                              IF (DOWT) WT = XZY(3,IP,LC)
C                                       If binning - accumulate
                              IF ((NBIN.GT.0) .AND. (WT.GT.0.0) .AND.
     *                           (XY(1).GE.BLC(1)) .AND.
     *                           (XY(1).LE.TRC(1))) THEN
                                 XUMVIS = XUMVIS + 1.0D0
                                 IBIN = 1 + NBIN * XY(1) / 1000.
                                 IF (IBIN.LT.1) IBIN = 1
                                 IF (IBIN.GT.NBIN) IBIN = NBIN
                                 IF (IPHASE.EQ.2) THEN
                                    TR = COS (DG2RAD * XZY(2,IP,LC))
                                    TI = SIN (DG2RAD * XZY(2,IP,LC))
                                    SUM(IBIN) = SUM(IBIN) + WT * TR
                                    SUM2(IBIN) = SUM2(IBIN) + WT * TR*TR
                                    SUMI(IBIN) = SUMI(IBIN) + WT * TI
                                    SUMI2(IBIN) = SUMI2(IBIN) + WT*TI*TI
                                 ELSE
                                    SUM(IBIN) = SUM(IBIN) + WT *
     *                                 XZY(2,IP,LC)
                                    SUM2(IBIN) = SUM2(IBIN) + WT *
     *                                 (XZY(2,IP,LC)**2)
                                    END IF
                                 XCNT(IBIN) = XCNT(IBIN) + WT
                                 KCNT(IBIN) = KCNT(IBIN) + 1
                                 END IF
C                                       Mark the point
                              IF ((BPARM(8).LE.0.0) .AND. (WT.GT.0.0))
     *                           THEN
                                 IF ((XY(1).LT.BLC(1)) .OR.
     *                              (XY(1).GT.TRC(1)) .OR.
     *                              (XY(2).LT.BLC(2)) .OR.
     *                              (XY(2).GT.TRC(2))) THEN
                                    NNOFIT = NNOFIT + 1
                                 ELSE
                                    NGOOD = NGOOD + 1
                                    IF ((.NOT.FIRST) .AND. (DOLINE))
     *                                 THEN
                                       IF (DO3COL.GT.0.0) THEN
                                          CALL G3VEC (XY(1), XY(2),
     *                                       BUFFER, IRET)
                                       ELSE
                                          CALL GVEC (XY(1), XY(2),
     *                                       BUFFER, IRET)
                                          END IF
                                       IF (IRET.NE.0) GO TO 970
                                       END IF
                                    X = MAX (BLC(1), MIN (TRC(1),
     *                                 XY(1)+DX))
                                    CALL GPOS (X, XY(2), BUFFER,IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    X = MAX (BLC(1), MIN (TRC(1),
     *                                 XY(1)-DX))
                                    IF (DO3COL.GT.0.0) THEN
                                       CALL G3VEC (X, XY(2), BUFFER,
     *                                    IRET)
                                    ELSE
                                       CALL GVEC (X, XY(2), BUFFER,
     *                                    IRET)
                                       END IF
                                    IF (IRET.NE.0) GO TO 970
                                    Y = MAX (BLC(2), MIN (TRC(2),
     *                                 XY(2)+DY))
                                    CALL GPOS (XY(1), Y, BUFFER, IRET)
                                    IF (IRET.NE.0) GO TO 970
                                    Y = MAX (BLC(2), MIN (TRC(2),
     *                                 XY(2)-DY))
                                    IF (DO3COL.GT.0.0) THEN
                                       CALL G3VEC (XY(1), Y, BUFFER,
     *                                    IRET)
                                    ELSE
                                       CALL GVEC (XY(1), Y, BUFFER,
     *                                    IRET)
                                       END IF
                                    IF (IRET.NE.0) GO TO 970
                                    IF (DOLINE) THEN
                                       CALL GPOS (XY(1), XY(2), BUFFER,
     *                                    IRET)
                                       IF (IRET.NE.0) GO TO 970
                                       FIRST = .FALSE.
                                       END IF
                                    END IF
                                 END IF
                              IF (.NOT.UVREV) GO TO 125
                                 XZY(1,IP,LC) = -XZY(1,IP,LC)
                                 XZY(2,IP,LC) = -XZY(2,IP,LC)
 124                             CONTINUE
                           END IF
 125                    CONTINUE
 130                 CONTINUE
 135              CONTINUE
               GO TO 100
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
C                                       Plot binned data.
 200  NBOFF = 0
      IF (NBIN.GT.0) THEN
         WRITE (MSGTXT,1201) XUMVIS, NBIN
         CALL REFRMT (MSGTXT, '_', IBIN)
         IF (XUMVIS.GT.0) CALL MSGWRT (2)
         CALL GLTYPE (3, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         FIRST = .TRUE.
         DO 210 IBIN = 1,NBIN
            IF (KCNT(IBIN).GT.0) THEN
               STDEV = 0.0
               IF (IPHASE.NE.2) THEN
                  AVERG = SUM(IBIN) / XCNT(IBIN)
                  IF (KCNT(IBIN).GE.2)
     *               STDEV = SQRT (ABS ((SUM2(IBIN)/XCNT(IBIN)) -
     *               AVERG*AVERG)/(KCNT(IBIN) - 1))
               ELSE
                  TR = SUM(IBIN) / XCNT(IBIN)
                  TI = SUMI(IBIN) / XCNT(IBIN)
                  AVERG = RAD2DG * ATAN2 (TI, TR)
                  IF ((SPHASE) .AND. (AVERG.LT.0.0)) AVERG = AVERG+360.0
                  IF (KCNT(IBIN).GE.2) THEN
                     TR = SUM2(IBIN)/XCNT(IBIN) - TR*TR
                     TI = SUMI2(IBIN)/XCNT(IBIN) - TI*TI
                     STDEV = RAD2DG * SQRT (ABS(TR+TI)/(KCNT(IBIN) - 1))
                     END IF
                  END IF
               DX = 5.0 * FACTOR
               DY = STDEV * FACTOR * XYSCL(2)
               IF (KCNT(IBIN).GE.2) DY = MAX (DX, DY)
               XY(1) = (IBIN * 1000. - 500.) / NBIN
               XY(2) = XYSCL(2) * (AVERG - XYOFF(2))
               IF ((XY(2).GE.BLC(2)) .AND. (XY(2).LE.TRC(2))) THEN
                  IF ((DOLINE) .AND. (.NOT.FIRST)) THEN
                     CALL GVEC (XY(1), XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     END IF
                  CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  IF (DOLINE) THEN
                     CALL GPOS (XY(1), XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     FIRST = .FALSE.
                     END IF
               ELSE
                  NBOFF = NBOFF + 1
                  END IF
C                                       Write binned values
               IF (BPARM(9).GT.0.) THEN
                  XY(1) = XY(1) / XYSCL(1) + XYOFF(1)
                  WRITE (MSGTXT,1200) XY(1), AVERG, STDEV, KCNT(IBIN)
                  CALL MSGWRT (5)
                  END IF
               END IF
 210        CONTINUE
         END IF
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
      IRET = 0
      GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         END IF
      GO TO 999
C                                       No catalog update
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      IF (NGOOD.GT.0) CALL MSGWRT (2)
      WRITE (MSGTXT,1991) NNOFIT
      IF (NNOFIT.GE.1) CALL MSGWRT (2)
      WRITE (MSGTXT,1992) NBOFF
      IF (NBOFF.GE.1) CALL MSGWRT (2)
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1995) VER
         CALL MSGWRT (2)
         CALL HIPLOT (DISKIN, FCNO(1), VER, BUFFER, IERR)
         END IF
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1031 FORMAT ('Ants ',I3,' -',I3,'___','Stokes ',A4,' _IF# ',I2,
     *   '_ Chan# ',I4)
 1032 FORMAT ('Ants ',I3,' -',I3,'___','Stokes ',A4,' _IF# ',I2,
     *   ' _Chan# ',I4,' - ',I4)
 1033 FORMAT ('Ants ',I3,' -',I3,'___','Stokes ',A4,' _IF# ',I2,
     *   ' - ',I2,'_ Chan# ',I4)
 1034 FORMAT ('Ants ',I3,' -',I3,'___','Stokes ',A4,' _IF# ',I2,
     *   ' - ',I2,' _Chan# ',I4,' - ',I4)
 1035 FORMAT ('From_',I4,'/',2(I2.2,':'),I2.2,'__to_',I4,'/',
     *   2(I2.2,':'),I2.2)
 1036 FORMAT ('Plotted frequency IDs 1 to',I4)
 1040 FORMAT (A,' axis for antenna',I3,' (_',A8,' ) only')
 1041 FORMAT (A,' axis for each baseline separately')
 1045 FORMAT ('UVrange ',2(1PE11.3),' wavelengths')
 1050 FORMAT ('PLTUV: ERROR',I3,' INIT VIS FILE')
 1060 FORMAT ('Freq =',F8.4,' GHz, Bw =',F8.3,' MHz')
 1080 FORMAT ('PLTUV: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
 1100 FORMAT ('PLTUV: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('X=',1PE12.5,', Y=',E12.5,', SIG=',E12.5,', N=',I10)
 1201 FORMAT ('PLTUV: Binned',F13.0,' samples in',I5,' bins')
 1400 FORMAT ('All W', '  NUMVIS=', I10)
 1430 FORMAT ('W =',1PE9.2,'-',1PE9.2,' lambda;','  NUMVIS=',I10)
 1970 FORMAT ('PLTUV: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
 1990 FORMAT ('PLTUV: ',I10,' Points plotted')
 1991 FORMAT ('PLTUV: ',I10,' Points did not fit')
 1992 FORMAT ('PLTUV: ',I10,' Bin points did not fit')
 1995 FORMAT ('PLTUV: Plot file version',I5,'  created.')
 3273 FORMAT (I4)
      END
      SUBROUTINE XYSCAL (NUMVIS, NP, XZY, NGOD, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS   I        Visibility number, -1 => final call, no data
C                        passed -> change to scaling factor from max/min
C      XZY      R(3,NP,*)   plotted parameters: 1 X, 2 Y, 3 weight
C   Outputs:
C      NGOD     I        Number good samples
C      IRET     I        Error return code , non-zero if error .
C   Outputs (common):
C      XYOFF    R(2)     when added to XY changes minimum to zero .
C      XYSCL    R(2)     scale XY so that maximum is 1000.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NP, NGOD
      REAL      XZY(3,NP,*)
C
      INTEGER   IRET, I, IC, ICO, LF, LC, J, IP
      REAL      TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVPLT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get the data limits.
C                                       Will reset to BPARMS where
C                                       requested later in plot routine.
      NGOD = 0
      IF (NUMVIS.GE.0) THEN
         IRET = -1
         ICO = ECHAN - BCHAN + 1
         LC = 0
         DO 50 LF = BIF,EIF
            DO 40 IC = 1,ICO,CHINC
               LC = LC + 1
               DO 30 IP = 1,NPOL
                  IF ((XZY(1,IP,LC).NE.FBLANK) .AND.
     *               (XZY(2,IP,LC).NE.FBLANK)) THEN
                     IRET = 0
                     NGOD = NGOD + 1
C                                       Find max, min from data
C                                       For autoscaling within range
C                                       (bparm(3)<0), don't look at
C                                       all points.
                     DO 20 I = 1,2
                        J = 2*I+2
                        TEMP = XZY(I,IP,LC)
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J).LE.TEMP)) THEN
                           IF (TEMP.LT.XYMIN(I)) XYMIN(I) = TEMP
                           END IF
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J+1).GE.TEMP)) THEN
                           IF (TEMP.GT.XYMAX(I)) XYMAX(I) = TEMP
                           END IF
 20                     CONTINUE
                     IF (IPHASE.GT.0) THEN
                        J = 2*IPHASE + 2
                        TEMP = XZY(IPHASE,IP,LC)
                        IF (TEMP.LT.0.0) TEMP = TEMP + 360.0
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J).LE.TEMP)) THEN
                           IF (TEMP.LT.XYMIN(3)) XYMIN(3) = TEMP
                           END IF
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J+1).GE.TEMP)) THEN
                           IF (TEMP.GT.XYMAX(3)) XYMAX(3) = TEMP
                           END IF
                        END IF
                     END IF
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
C                                       Last call:
      ELSE
         DO 120 I = 1,2
            IF (XYMAX(I).LE.XYMIN(I)) GO TO 980
C                                       Deal with U,V,W axes
            IF (UVREV) THEN
               XYMAX(I) = MAX (ABS(XYMAX(I)), ABS(XYMIN(I)))
               XYMIN(I) = -XYMAX(I)
               END IF
            IF ((XYMIN(I).GT.0.0) .AND. (XYMIN(I).LT.0.15*XYMAX(I)))
     *         XYMIN(I) = 0.0
 120        CONTINUE
         IF ((XYMIN(3).GT.0.0) .AND. (XYMIN(3).LT.0.15*XYMAX(3)))
     *      XYMIN(3) = 0.0
         IF ((FIXSCL.EQ.2) .AND. (TYPEAX(1).GE.6) .AND. (TYPEAX(1).LE.8)
     *      .AND. (TYPEAX(2).GE.6) .AND. (TYPEAX(2).LE.8)) THEN
            XYMAX(1) = MAX (XYMAX(1), XYMAX(2))
            XYMAX(2) = XYMAX(1)
            XYMIN(1) = MIN (XYMIN(1), XYMIN(2))
            XYMIN(2) = XYMIN(1)
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
 1980 FORMAT ('XYSCAL: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE DRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Draw TVPLAN into a graphics plane or memory planes of TV only
C   Inputs:
C      NX       I      X dimension
C      NY       I      Y dimension
C      TVPLAN   I(*)   plane to draw
C   Outputs:
C      BUFFER   I(*)   plot buffer
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), BUFFER(*), IERR
C
      INTEGER   LX, IY, LY, CHAN, LC
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
C                                       graphics
      IF (NZ.LE.1) THEN
         CALL GLTYPE (4, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Channel okay?
         GPHLTY = MAX (1, GPHLTY)
         CHAN = GPHTVG(GPHLTY)
         CALL GCINIT (CHAN, 0, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       load image
         LX = GPHIX0
         LY = GPHIY0 - 1
         DO 10 IY = 1,NY
            LY = LY + 1
            CALL YIMGIO ('WRIT', CHAN, LX, LY, 0, NX, TVPLAN(1,IY,1),
     *         IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
C                                       3 color grey scale
      ELSE
         DO 50 LC = 1,NZ
            CHAN = GPHTVC(LC)
            CALL GCINIT (CHAN, LC, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       load image
            LX = GPHIX0
            LY = GPHIY0 - 1
            DO 30 IY = 1,NY
               LY = LY + 1
               CALL YIMGIO ('WRIT', CHAN, LX, LY, 0, NX,
     *            TVPLAN(1,IY,LC), IERR)
               IF (IERR.NE.0) GO TO 999
 30            CONTINUE
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRPLAN (NX, NY, NZ, TVPLAN, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Draw TVPLAN into a plot file
C   Inputs:
C      NX       I      X dimension
C      NY       I      Y dimension
C      NZ       I      Z dimension (1 B&W, 3 color)
C      TVPLAN   I(*)   plane to draw
C   Outputs:
C      BUFFER   I(*)   plot buffer
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, TVPLAN(NX,NY,*), BUFFER(*), IERR
C
      INTEGER   LX, IY, LY, IGLO, IGHI
      REAL      RANGE(2), RANGES(2,3), X, Y
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IF (MAXINT.LE.0) MAXINT = 8191
C                                       Black & white
      IF (NZ.LE.1) THEN
         IGLO = 0
         IGHI = MAXINT
         RANGE(1) = 0.0
         RANGE(2) = MAXINT
         CALL GINITG (IGLO, IGHI, RANGE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       load image
         LX = GPHIX1
         X = LX
         LY = GPHIY1 - 1
         DO 10 IY = 1,NY
            LY = LY + 1
            Y = LY
            CALL GPOS (X, Y, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GRAYPX (NX, 0, TVPLAN(1,IY,1), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
C                                       3 color grey scale
      ELSE
         IGLO = 0
         IGHI = MAXINT
         RANGES(1,1) = 0.0
         RANGES(2,1) = MAXINT
         RANGES(1,2) = 0.0
         RANGES(2,2) = MAXINT
         RANGES(1,3) = 0.0
         RANGES(2,3) = MAXINT
         CALL GINITC (IGLO, IGHI, RANGES, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       load image
         LX = GPHIX1
         X = LX
         LY = GPHIY1 - 1
         DO 20 IY = 1,NY
            LY = LY + 1
            Y = LY
            CALL GPOS (X, Y, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL G3COLR (NX, 0, TVPLAN(1,IY,1), TVPLAN(1,IY,2),
     *         TVPLAN(1,IY,3), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE UVPLTS (FLUX, SPIX, NPOL, BUFR)
C-----------------------------------------------------------------------
C   Scales data buffer by flux and spectral index
C   Inputs:
C      FLUX   R(4,*)   Flux by IF
C      SPIX   R(3)     Flux at 1 GHz, spectral index, curvature
C                        spix(2): < -1000 no scale
C   In/out:
C      BUFR   R(3,*)   data buffer
C-----------------------------------------------------------------------
      INTEGER   NPOL
      REAL      FLUX(4,*), SPIX(3), BUFR(*)
C
      INTEGER   LF, LC, LP, NCHAN, LAD
      REAL      CATUVR(256), SCALE, DEN
      DOUBLE PRECISION CATUVD(128), REFREQ, REFPIX
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATUVD, CATUVR)
C-----------------------------------------------------------------------
      IF (SPIX(2).GT.-1000.) THEN
         REFREQ = CATUVD(KDCRV+JLOCF)
         REFPIX = CATUVR(KRCRP+JLOCF)
         NCHAN = CATUV(KINAX+JLOCF)
         DO 20 LF = BIF,EIF
            SCALE = 1.0 / FLUX(1,LF)
            IF (SPIX(2).NE.0.0) DEN = (REFREQ + FOFF(LF) +
     *         FINC(LF) * (NCHAN/2.0 - REFPIX)) / 1.D9
            DO 15 LC = BCHAN,ECHAN
C                                       scale w spectral index
               IF (SPIX(2).NE.0.0) THEN
                  SCALE = ((REFREQ + FOFF(LF) + FINC(LF) *
     *               (LC - REFPIX)) / 1.D9)
                  IF ((SPIX(3).EQ.0.0) .OR. (SPIX(1).LE.0.0)) THEN
                     SCALE = (SCALE / DEN) ** SPIX(2)
                     SCALE = 1.0 / (SCALE * FLUX(1,LF))
C                                       and curvature
                  ELSE
                     SCALE = LOG10 (SCALE)
                     SCALE = SPIX(2)*SCALE + SPIX(3)*SCALE*SCALE
                     SCALE = 1.0 / (SPIX(1) * (10.0 ** SCALE))
                     END IF
                  END IF
               DO 10 LP = 1,NPOL
                  LAD = (LC - BCHAN) * INCF + (LF - BIF) * INCIF +
     *               (LP - 1) * INCS
                  IF (BUFR(LAD+3).GT.0) THEN
                     BUFR(LAD+1) = BUFR(LAD+1) * SCALE
                     BUFR(LAD+2) = BUFR(LAD+2) * SCALE
                     BUFR(LAD+3) = BUFR(LAD+3) / SCALE / SCALE
                     END IF
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FNDXY (RANDP, BUFR, NP, XZY)
C-----------------------------------------------------------------------
C   FNDXY extracts the desired X and Y values from the Vis record.
C   Inputs:
C      RANDP   R(*)     Random parameters
C      BUFR    R(*)     Visibility record
C      NP      I        Number pol in XY
C   Outputs:
C      XY      R(3,*)   X, Y values and weight
C-----------------------------------------------------------------------
      INTEGER   NP
      REAL      RANDP(*), BUFR(*), XZY(3,NP,*)
C
      REAL      TR, TI, CATUVR(256), WT, H1, E1, H2, E2, RADPA, A1, A2,
     *   S1, S2, C1, C2
      INTEGER   I, J, LAD, IC, ICO, IA1, IA2, LF, LC, IP, LL, L
      DOUBLE PRECISION FRQMUL, CATUVD(128), FZ, FI, TT
      INCLUDE 'UVPLT.INC'
      REAL      PA(MAXANT)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATUVD, CATUVR)
C-----------------------------------------------------------------------
      IF (ILOCB.GE.0) THEN
         IA1 = RANDP(ILOCB+1) / 256 + 0.1
         IA2 = RANDP(ILOCB+1) - 256 * IA1 + 0.1
      ELSE
         IA1 = RANDP(ILOCA1+1) + 0.1
         IA2 = RANDP(ILOCA2+1) + 0.1
         END IF
C                                       Loop over channels
      ICO = ECHAN - BCHAN + 1
      LC = 0
      FRQMUL = 1.0D0
      DO 920 LF = BIF,EIF
         FZ = FOFF(LF) / UVFREQ + 1.0D0
         FI = FINC(LF) / UVFREQ
         DO 910 IC = 1,ICO,CHINC
            LC = LC + 1
            IF (TYPUVD.LE.0) FRQMUL = FZ + FI * (IC - 1 + BCHAN +
     *         (NCHAV-1.0)/2.0 - CATUVR(KRCRP+KLOCFY))
            DO 905 IP = 1,NPOL
               LAD = 1 + (IC - 1) * INCF + (LF - BIF) * INCIF +
     *            (IP - 1) * INCS
               WT = BUFR(LAD+2)
C                                       Loop over axes
               DO 900 I = 1,2
                  TR = 0.0
                  TI = 0.0
                  XZY(I,IP,LC) = FBLANK
                  J = TYPEAX(I)
C                                       amplitudes and phases
                  IF ((J.EQ.1) .OR. (J.EQ.2) .OR. (J.EQ.9) .OR.
     *               (J.EQ.10) .OR. (J.EQ.12) .OR. (J.EQ.13)) THEN
C                                       average channels
                     IF (NCHAV.GT.1) THEN
                        WT = 0.0
                        TR = 0.0
                        TI = 0.0
                        LL = LAD
                        DO 110 L = 1,NCHAV
                           IF (BUFR(LL+2).GT.0.0) THEN
                              WT = WT + BUFR(LL+2)
                              TR = TR + BUFR(LL+2)* BUFR(LL)
                              TI = TI + BUFR(LL+2)* BUFR(LL+1)
                              END IF
                           LL = LL + INCF
 110                       CONTINUE
                        IF (WT.GT.0.0) THEN
                           TR = TR / WT
                           TI = TI / WT
                           END IF
                     ELSE
                        TR = BUFR(LAD)
                        TI = BUFR(LAD+1)
                        END IF
                     IF (WT.LE.0.0) GO TO 900
C                                       amplitude
                     IF (J.EQ.1) THEN
                        IF ((IA1.EQ.IA2) .AND. (.NOT.ISCROS(IP))) THEN
                           XZY(I,IP,LC) = TR
                        ELSE
                           XZY(I,IP,LC) = SQRT (TR*TR + TI*TI)
                           END IF
C                                       log (ampl)
                     ELSE IF (J.EQ.12) THEN
                        XZY(I,IP,LC) = SQRT (TR*TR + TI*TI)
                        XZY(I,IP,LC) = LOG10 (MAX (1.E-12,XZY(I,IP,LC)))
C                                       phase
                     ELSE IF (J.EQ.2) THEN
                        IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XZY(I,IP,LC) =
     *                     RAD2DG * ATAN2 (TI, TR)
C                                       Real , Imag , Weight parts
                     ELSE IF (J.EQ.9) THEN
                        XZY(I,IP,LC) = TR
                     ELSE IF (J.EQ.10) THEN
                        XZY(I,IP,LC) = TI
                     ELSE IF (J.EQ.13) THEN
                        XZY(I,IP,LC) = WT
                        END IF
C                                       U, V distance
                  ELSE IF (J.EQ.3) THEN
                     XZY(I,IP,LC) = SQRT (RANDP(1+ILOCU)**2 +
     *                  RANDP(1+ILOCV)**2) * FRQMUL
C                                       U, V position angle
                  ELSE IF (J.EQ.4) THEN
                     TR = RANDP(1+ILOCU)
                     TI = RANDP(1+ILOCV)
                     IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XZY(I,IP,LC) =
     *                  RAD2DG * ATAN2 (TI, TR)
C                                       Time
                  ELSE IF ((J.EQ.5) .OR. (J.EQ.11)) THEN
                     XZY(I,IP,LC) = RANDP(1+ILOCT)
C                                       U projected spacing
                  ELSE IF (J.EQ.6) THEN
                     XZY(I,IP,LC) = RANDP(1+ILOCU) * FRQMUL
C                                       V projected spacing
                  ELSE IF (J.EQ.7) THEN
                     XZY(I,IP,LC) = RANDP(1+ILOCV) * FRQMUL
C                                       W projected spacing
                  ELSE IF (J.EQ.8) THEN
                     XZY(I,IP,LC) = RANDP(1+ILOCW) * FRQMUL
C                                       HA, Elevation, azimuth
                  ELSE IF ((J.EQ.14) .OR. (J.EQ.15) .OR. (J.EQ.18)) THEN
                     TT = RANDP(1+ILOCT)
                     IF (REFANT.GT.0) THEN
                        CALL SOUELV (REFANT, TT, H1, E1, A1)
                        IF (J.EQ.14) THEN
                           XZY(I,IP,LC) = H1 * RAD2DG / 15.0
                        ELSE IF (J.EQ.18) THEN
                           XZY(I,IP,LC) = A1 * RAD2DG
                        ELSE
                           XZY(I,IP,LC) = E1 * RAD2DG
                           END IF
                     ELSE
                        CALL SOUELV (IA1, TT, H1, E1, A1)
                        CALL SOUELV (IA2, TT, H2, E2, A2)
                        IF (J.EQ.14) THEN
                           S1 = SIN (H1)
                           S2 = SIN (H2)
                           C1 = COS (H1)
                           C2 = COS (H2)
                           XZY(I,IP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
     *                        / 15.0
                        ELSE IF (J.EQ.18) THEN
                           S1 = SIN (A1)
                           S2 = SIN (A2)
                           C1 = COS (A1)
                           C2 = COS (A2)
                           XZY(I,IP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                        ELSE
                           S1 = SIN (E1)
                           S2 = SIN (E2)
                           C1 = COS (E1)
                           C2 = COS (E2)
                           XZY(I,IP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                           END IF
                        END IF
C                                       Parallactic angle
                  ELSE IF (J.EQ.16) THEN
                     H1 = RANDP(1+ILOCT)
                     CALL PARANG (H1, PA)
                     IF (REFANT.GT.0) THEN
                        XZY(I,IP,LC) = PA(REFANT) * RAD2DG
                     ELSE
                        S1 = SIN (PA(IA1))
                        S2 = SIN (PA(IA2))
                        C1 = COS (PA(IA1))
                        C2 = COS (PA(IA2))
                        XZY(I,IP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                        END IF
C                                       Projected spacing along PA
                  ELSE IF (J.EQ.17) THEN
                     RADPA = ROTATE / RAD2DG
                     XZY(I,IP,LC) = ABS(RANDP(1+ILOCU)*SIN(RADPA) +
     *                  RANDP(1+ILOCV)*COS(RADPA)) * FRQMUL
C                                       frequency
                  ELSE IF (J.EQ.19) THEN
                     XZY(I,IP,LC) = FRQMUL * UVFREQ
C                                       channel
                  ELSE IF (J.EQ.20) THEN
                     XZY(I,IP,LC) = (LF - 1) * CATUV(KINAX+KLOCFY) +
     *                  BCHAN - 1 + IC
                     END IF
 900              CONTINUE
               IF (J.EQ.13) THEN
                  XZY(3,IP,LC) = 1.0
               ELSE
                  XZY(3,IP,LC) = WT
                  END IF
 905           CONTINUE
 910        CONTINUE
 920     CONTINUE
C
 999  RETURN
      END

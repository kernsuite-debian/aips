LOCAL INCLUDE 'ELFIT.INC'
      INTEGER   NCODE
      PARAMETER (NCODE=27)
C                                       Local include for ELFIT
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XQUAL, XTIME(8), XBAND, XFREQ,
     *   XFQID, XSUBA, XBIF, XEIF, XANT(50), PIXR(2), XNCOU, XORDER,
     *   XDO3C, XPCNUM, ABSICA, XSYM, FACTOR, XDOBL, CUTOFF, XLABEL,
     *   XDOTV, XGRCH, APARM(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE, XXSOUR(4,30), XXSTOK,
     *   XOPTY
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2, XSOUR(30)*16, XSTOK*4,
     *   OPTYPE*4
C
      INTEGER   MXSCAN
      PARAMETER (MXSCAN=5000)
C
      REAL      TSTART, TSTOP, TINT, XYSCL(2), XYOFF(2), YYMX,
     *   YYMN, XMX, XMN, XMXW, XMNW, GMMOD, RATFAC(MAXIF), SELBAN,
     *   XSTART, XSTOP, CHOUT(4), XXMIN(MAXANT), XXMAX(MAXANT),
     *   YYMIN(2,MAXIF,MAXANT), PPMAX(2,MAXIF,MAXANT),
     *   YYMAX(2,MAXIF,MAXANT), PPMIN(2,MAXIF,MAXANT), PRAN(2,2),
     *   TCAL(4,MAXIF,MAXANT), CSMIN, CSMAX
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, ANTS(50), NCOUNT,
     *   ICODES, NPARMS, NID, SID(500), NANTSL, NPLOTS, SUMSTK, ISTOK,
     *   FRQSEL, ORDER, GRCHN, TVCHN, TVCORN(4), XVAR, ISOU, OSOU, IANT,
     *   EIF, ITVER, PCNUM, LABEL, SUBARR, MUMPOL, MUMIF, MUMANT, NTONE,
     *   NUMPTS(MAXANT), ISYM, BSYM, NANREC(MAXANT), FANREC(MAXANT),
     *   STRANS(MXSCAN)
      LOGICAL   DOAWNT, DOTV, NNODAT, SWAP, FULLX
      DOUBLE PRECISION SELFRQ, GNRECD(XCLRSZ/2)
C                                       SN/CL table info
      INTEGER CLBUFF(512), NCLINR, NUMANT, NUMPOL, NUMIF, ICLRNO,
     *   KOLS(40), KOLTYP(40), KOLDIM(40), ICLUN, GNRECI(XCLRSZ),
     *   TIMKOL, INTKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, IFRKOL,
     *   GEOKOL, DOPKOL, ATMKOL, DATKOL,
     *   MB1KOL, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL, RF1KOL, TS1KOL,
     *   TA1KOL, CK1KOL, DC1KOL, DS1KOL, DD1KOL,
     *   MB2KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, RF2KOL, TS2KOL,
     *   TA2KOL, CK2KOL, DC2KOL, DS2KOL, DD2KOL,
     *   MBKOL(4), REKOL(4), IMKOL(4), DLKOL(4), RAKOL(4), WTKOL(4),
     *   RFKOL(4), TSKOL(4), TAKOL(4), CKKOL(4), DCKOL(4), DSKOL(4),
     *   DDKOL(4), STKOL(4),
     *   DOPLKL, DOP3KL, CLTIME, CABKOL, ST1KOL, ST2KOL
      REAL GNREC(XCLRSZ)
C                                       Constants
      DOUBLE PRECISION SIDER, CLIGHT
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XTYPE, XNVER,
     *   XXSOUR, XQUAL, XTIME, XXSTOK, XBAND, XFREQ, XFQID, XSUBA, XBIF,
     *   XEIF, XANT, PIXR, XNCOU, XORDER, XOPTY, XDO3C, XPCNUM, ABSICA,
     *   XSYM, FACTOR, XDOBL, CUTOFF, XLABEL, XDOTV, XGRCH, APARM
      COMMON /VPARM/ SEQIN, DISKIN, CNOIN, IVER, BIF, EIF, ANTS, NCOUNT,
     *   ICODES, NPARMS, GRCHN, TVCHN, TVCORN, XVAR, ISOU, OSOU, IANT,
     *   ITVER, PCNUM, DOTV, NNODAT, LABEL, CHOUT, STRANS, SWAP, FULLX
      COMMON /VGNCOM/ SELFRQ,
     *   TSTART, TSTOP, TINT, XYSCL, XYOFF, SELBAN, XMX, XMN, XMXW,
     *   XMNW, XSTART, XSTOP, GMMOD, RATFAC, NID, SID, NANTSL, PRAN,
     *   NPLOTS, DOAWNT, ISTOK, SUMSTK, FRQSEL, ORDER,
     *   SUBARR, MUMPOL, MUMIF, MUMANT, NUMPTS, NTONE,
     *   XXMIN, XXMAX, YYMIN, YYMAX, PPMIN, PPMAX, YYMX, YYMN, ISYM,
     *   BSYM, TCAL, NANREC, FANREC, CSMIN, CSMAX
      COMMON /VGNCHR/ NAMEIN, CLAIN, TYPE, XSOUR, XSTOK, OPTYPE
      COMMON /TABCOM/ GNREC, CLBUFF, NCLINR, NUMANT, NUMPOL, NUMIF,
     *   ICLRNO, KOLS, KOLTYP, KOLDIM, ICLUN,
     *   MBKOL, REKOL, IMKOL, DLKOL, RAKOL, WTKOL, RFKOL, TSKOL,
     *   TAKOL, CKKOL, DCKOL, DSKOL, DDKOL, STKOL,
     *   DOPLKL, DOP3KL, CLTIME
      COMMON /CONST/ SIDER, CLIGHT
      EQUIVALENCE (GNREC, GNRECD, GNRECI)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL), (KOLS(4), ANTKOL), (KOLS(5), SUBKOL),
     *   (KOLS(6), FRQKOL), (KOLS(7), IFRKOL),
     *   (KOLS(8), GEOKOL), (KOLS(9), DOPKOL), (KOLS(10), ATMKOL),
     *   (KOLS(11), DATKOL)
      EQUIVALENCE (KOLS(12), MB1KOL),
     *   (KOLS(13), RE1KOL), (KOLS(14), IM1KOL),
     *   (KOLS(15), RA1KOL), (KOLS(16), DL1KOL), (KOLS(17), WT1KOL),
     *   (KOLS(18), RF1KOL), (KOLS(19), TS1KOL), (KOLS(20), TA1KOL),
     *   (KOLS(21), CK1KOL), (KOLS(22), DC1KOL),
     *   (KOLS(23), DS1KOL), (KOLS(24), DD1KOL)
      EQUIVALENCE (KOLS(25), MB2KOL),
     *   (KOLS(26), RE2KOL), (KOLS(27), IM2KOL),
     *   (KOLS(28), RA2KOL), (KOLS(29), DL2KOL), (KOLS(30), WT2KOL),
     *   (KOLS(31), RF2KOL), (KOLS(32), TS2KOL), (KOLS(33), TA2KOL),
     *   (KOLS(34), CK2KOL), (KOLS(35), DC2KOL),
     *   (KOLS(36), DS2KOL), (KOLS(37), DD2KOL),
     *   (KOLS(38), CABKOL), (KOLS(39), ST1KOL), (KOLS(40), ST2KOL)
C                                                          End ELFIT
LOCAL END
      PROGRAM ELFIT
C-----------------------------------------------------------------------
C! Plots data from a SN, TY, PC or CL table
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2017-2018
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
C   ELFIT plots SN or CL extension files. A 'PL' extension file is made
C   which can be displayed in the usual ways .
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INEXT......'SN','TY','PC' or 'CL' table to be plotted
C      INVERS.....Version number of table to plot, 0=>highest no.
C      SOURCES....Source list.  '*' = all; a "-" before a source
C                 name means all except ANY source named.
C      TIMERANG...Time range of the data to be plotted. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      STOKES.....The desired Stokes type of the output data:
C                 'R' = RCP, 'L' = LCP
C      BIF........IF to plot
C      ANTENNAS...A list of the antennas to be plotted. All 0 => all.
C                 If any number is negative then all antennas listed
C                 are NOT to be plotted and all others are.
C      PIXRANGE...Limit the plot to values between PIXR(1) and
C                 PIXR(2).  The plots will not exceed the min/max in
C                 the actual gains.  Basically, if PIXR(1) < PIXR(2),
C                 all plots will be on the same scale and be limited
C                 to max (datamin, PIXR(1)) through min (datamax,
C                 PIXR(2)).  If PIXR(1) >= PIXR(2), each plot will be
C                 self-scaled individually.
C      NCOUNT.....Number of antennas to plot per page (try 5).
C      OPTYPE.....Data to be plotted: 'PHAS' = phase, 'AMP '=  ampl.,
C                 'DELA' = delay, 'RATE' = rate, 'TSYS' = sys. temp.
C                 'SUM ' = summary, 'DOPL' = doppler offset, 'SNR' =
C                 signal to noise ratio, 'CCAL' = cable-cal,
C                 'DDLY' = dispersive delay  'IFR' Faraday rotation
C                 '    ' => 'PHAS'
C      XTYPE......Variable data to be plotted against,
C                 1 = IAT time; 2 = elevation; 3 = HA, 4 = LST
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
C
      CHARACTER PRGN*6
      REAL      PLTPTS(2)
      LONGINT   PPLTPT
      INTEGER   IRET, MVAL, NWORDS, NROWS
      INCLUDE 'ELFIT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'ELFIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNPIN (PRGN, NROWS, IRET)
      MUMANT = MAX (1, MUMANT)
      MVAL = 2 + MUMPOL * MUMIF
      NWORDS = (MVAL * NROWS - 1) / 1024  + 21
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PLTPTS, PPLTPT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         END IF
C                                       read data to figure out
C                                       distribution
      NWORDS = NWORDS * 1024
      IF (IRET.EQ.0) CALL SNPCNT (NWORDS, IRET)
C                                       Fetch data, determine scaling
      IF (IRET.EQ.0) CALL SNPMAX (MVAL, PLTPTS(1+PPLTPT), IRET)
C                                       Do plots
      IF (IRET.EQ.0) CALL SNPLOT (MVAL, MUMANT, PLTPTS(1+PPLTPT), IRET)
      IF (IRET.LT.0) IRET = 0
C                                       Close down
      CALL DIE (IRET, CLBUFF)
C
 999  STOP
      END
      SUBROUTINE SNPIN (PRGN, NROWS, IERR)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for ELFIT.
C   Inputs:
C      PRGN    C*6  Program name
C   Output in common:
C      SUMSTK  I    Selected Stokes 0=both, 1=R, 2=L, 4=difference
C   Output:
C      IERR    I    Error code: 0 => ok
C      ISTOK   I    1 = R, 2 = L
C      ICODE   I    1='PHAS', 2='AMP ', 3='DELA', 4='RATE', 5='TSYS',
C                   6='----', 7='DOPL', 8='SNR', 9='MDEL', 10='TANT',
C                   11='ATM', 12='GEO', 13='CCAL', 14='DDLY'
C                   15='REAL', 16='IMAG', 17='IFR', 18='PDIF',
C                   19='PSUM', 20=PGN ', 21='PON ', 22='POFF', 23='PSYS'
C                   24='PDGN', 25='PSGN', 26='POWR', 27='PODB'
C-----------------------------------------------------------------------
      INTEGER   NROWS, IERR
      CHARACTER PRGN*6
C
      INTEGER   NTPLT
      PARAMETER (NTPLT=5)
C
      INCLUDE 'ELFIT.INC'
      CHARACTER STAT*4, CODE(NCODE)*4, TYPTMP*2, TPLOT(NTPLT)*4
      INTEGER   IRET, BUFF(256), I, J, K, JERR, QUAL(30), NSOUR, LTYPE,
     *   BUFFER(512), IROUND, LUN, VER, NIF, NSTOK, FRQTMP, ICODE
      LOGICAL T, F, MATCH, ISTYPE
      DOUBLE PRECISION FOFF(MAXIF)
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA CODE /'PHAS', 'AMP ', 'DELA', 'RATE', 'TSYS', '-+-+', 'DOPL',
     *   'SNR ', 'MDEL', 'TANT', 'ATM ', 'GEO ', 'CCAL', 'DDLY', 'REAL',
     *   'IMAG', 'IFR ', 'PDIF', 'PSUM', 'PGN ', 'PON ', 'POFF', 'PSYS',
     *   'PDGN', 'PSGN', 'POWR', 'PODB'/
      DATA TPLOT /'ALIF', 'ALST', 'ALSI', 'IFDF', 'IFRA'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OSOU = -1
      NPARMS = 220
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XTYPE, TYPE)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
      ISYM = IROUND (XSYM)
      IF ((ISYM.LE.0) .OR. (ISYM.GT.24)) ISYM = 1
      BSYM = IROUND (XDOBL)
      IF ((BSYM.GT.0) .AND. (BSYM.EQ.ISYM)) BSYM = MOD (ISYM, 24) + 1
C                                       OPTYPE='REAL', 'IMAG' only for
C                                       SN table
      IF ((OPTYPE.EQ.'REAL' .OR. OPTYPE.EQ.'IMAG')
     *   .AND. (TYPE.NE.'SN' .AND. TYPE.NE.'CL')) THEN
         MSGTXT = '!!OPTYPE=REAL/IMAG works only with SN/CL table!!'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
C
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         QUAL(I) = IROUND (XQUAL)
 20      CONTINUE
      IF (TYPE.EQ.' ') TYPE ='SN'
      CUTOFF = MAX (0.0, CUTOFF)
      IF ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC') .OR. (TYPE.EQ.'SY'))
     *   CUTOFF = -1000.0
      CALL FILL (MAXANT, 0, NUMPTS)
C                                       Do not treat the weight for TY
C                                       and PC because these tables do
C                                       not have weight's collumn
      XTYPE = HBLANK
      CALL CHR2H (2, TYPE, 1, XTYPE)
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
      NCOUNT = IROUND (XNCOU)
      IF (NCOUNT.LE.0) NCOUNT = 5
      XNCOU = NCOUNT
      ORDER = IROUND (XORDER)
      ORDER = MAX (0, MIN (8, ORDER))
      PCNUM = IROUND (XPCNUM)
      IF (PCNUM.LE.0) PCNUM = 1
      XORDER = ORDER
      XVAR = IROUND (ABSICA)
      FULLX = XVAR.LT.0
      XVAR = ABS (XVAR)
      IF ((XVAR.EQ.0) .OR. (XVAR.GT.6) .OR. (XVAR.EQ.4)) XVAR = 2
      FACTOR = ABS (FACTOR)
      IF (FACTOR.LT.0.1) FACTOR = 1.0
      IF (FACTOR.GT.10.0) FACTOR = 1.0
      ICODE = 1
      DO 30 I = 1,NCODE
         IF (OPTYPE.EQ.CODE(I)) ICODE = I
 30      CONTINUE
      CALL CHR2H (4, CODE(ICODE), 1, XOPTY)
      ICODES = ICODE
      IF (XVAR.EQ.1) THEN
         IF (APARM(2).LE.0.0) APARM(2) = 20.
         IF (APARM(3).LE.0.0) APARM(3) = 80.
      ELSE IF (XVAR.EQ.2) THEN
         IF (APARM(2).LE.0.0) APARM(2) = 70.
         IF (APARM(3).LE.0.0) APARM(3) = 10.
         END IF
C                                       Time range
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*3600.0))
      TSTOP = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*3600.0))
      IF (TSTART.GE.TSTOP) THEN
         TSTART = 0.0
         TSTOP = 999.0
         END IF
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
C                                       Find input catalog
      CNOIN = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      'UV', NLUSER
         GO TO 990
         END IF
C                                       Save name class etc.
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
C                                       Read catalog header
      STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
      XDISIN = DISKIN
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
C                                       Subarray
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.EQ.0) SUBARR = 1
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.GE.0) THEN
         LUN = 25
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *      FRQSEL, IERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            IERR = 1
            GO TO 990
            END IF
         IF (IERR.GT.0) GO TO 999
         END IF
C                                       IF'S
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         IF (EIF.GT.CATBLK(KINAX+JLOCIF)) EIF = CATBLK(KINAX+JLOCIF)
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Look up sources
      NID = 500
      NSOUR = 30
      MSGSUP = 32000
      CALL SOURNU (XSOUR, QUAL, NSOUR, DISKIN, CNOIN, NID, BUFFER, SID,
     *   JERR)
      MSGSUP = 0
      IF (JERR.LT.0) THEN
         MSGTXT = 'SPECIFIED SOURCE(S) NOT FOUND - CONTINUING'
         CALL MSGWRT (6)
         END IF
      IF (JERR.NE.0) NID = 0
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 70 J = 1,50
         ANTS(J) = IROUND (XANT(J))
         IF (ANTS(J).LT.0) DOAWNT = F
C                                       Make positive
         ANTS(J) = ABS (ANTS(J))
         IF (NANTSL.LT.1) GO TO 60
            DO 50 K = 1,NANTSL
               IF (ANTS(J).EQ.ANTS(K)) ANTS(J) = 0
 50            CONTINUE
C                                       Check for multiple entries
 60      IF (ABS (ANTS(J)).GE.1) NANTSL = J
 70      CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
C                                       Get antenna names
      CALL GETANT (DISKIN, CNOIN, MAX (1, SUBARR), CATBLK, BUFFER, JERR)
      MUMANT = NSTNS
      IF (MUMANT.LE.1) THEN
         MUMANT = MAXANT
         TIMLAB = 'IAT'
         END IF
C                                       Rate scaling to Hz
      DO 71 I = BIF,EIF
         RATFAC(I-BIF+1) = FREQ
 71      CONTINUE
      IF ((OPTYPE.EQ.'RATE') .OR. (ISTYPE(4))) THEN
         VER = 1
         LUN = 25
         IF (FRQSEL.LE.0) FRQTMP = 1
         IF (FRQSEL.GT.0) FRQTMP = FRQSEL
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, VER, CATBLK, LUN,
     *      NIF, FOFF, ISBAND, FINC, BNDCOD, FRQTMP, JERR)
         IF (JERR.EQ.0) THEN
            DO 72 I = BIF,EIF
               RATFAC(I-BIF+1) = FREQ + FOFF(I)
 72            CONTINUE
            END IF
         END IF
C                                       Check Stokes' (R or IPOL)
C                                       Set stokes request
      NSTOK = CATBLK(KINAX+JLOCS)
      MUMPOL = 1
      IF ((ICOR0.EQ.1) .OR. (ICOR0.EQ.-2) .OR. (ICOR0.EQ.-6) .OR.
     *   (((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)) .AND. (NSTOK.EQ.1))) THEN
         ISTOK = ABS (ICOR0)
         SUMSTK = 1
         XSTOK = 'I'
         IF (ICOR0.EQ.-2) XSTOK='L'
         IF (ICOR0.EQ.-1) XSTOK='R'
      ELSE IF (ICOR0.EQ.-1) THEN
         IF ((XSTOK.EQ.'R') .OR. (XSTOK.EQ.'RR')) THEN
            ISTOK = 1
            SUMSTK = 1
            XSTOK = 'R'
         ELSE IF ((XSTOK.EQ.'L') .OR. (XSTOK.EQ.'LL')) THEN
            ISTOK = 2
            SUMSTK = 2
            XSTOK = 'L'
         ELSE
            ISTOK = 1
            SUMSTK = 0
            XSTOK = 'R&L'
            MUMPOL = 2
            END IF
      ELSE IF (ICOR0.EQ.-5) THEN
         IF ((XSTOK.EQ.'X') .OR. (XSTOK.EQ.'XX')) THEN
            ISTOK = 1
            SUMSTK = 1
            XSTOK = 'X'
         ELSE IF ((XSTOK.EQ.'Y') .OR. (XSTOK.EQ.'YY')) THEN
            ISTOK = 2
            SUMSTK = 2
            XSTOK = 'Y'
         ELSE
            ISTOK = 1
            SUMSTK = 0
            XSTOK = 'X&Y'
            MUMPOL = 2
            END IF
         END IF
      CALL CHR2H (4, XSTOK, 1, XXSTOK)
C                                       Open table to check
C                                       Open SN, CL, TY or PC table
      IF ((TYPE.EQ.'SN') .OR. (TYPE.EQ.'CL') .OR. (TYPE.EQ.'TY') .OR.
     *   (TYPE.EQ.'PC') .OR. (TYPE.EQ.'SY')) THEN
         CALL SNPOPN (NROWS, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Invalid table type
      ELSE
         IERR = 5
         MSGTXT = 'INVALID TABLE TYPE =' // TYPE
         GO TO 990
         END IF
C                                       Get TCals
      IF ((TYPE.EQ.'SY') .AND. (ISTYPE(23))) THEN
         J = 0
         CALL GETCDS (DISKIN, CNOIN, J, SUBARR, FRQSEL, CATBLK, TCAL,
     *      JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      XNVER = IVER
      MUMIF = EIF - BIF + 1
      XBIF = BIF
      XEIF = EIF
      CSMAX = -100000
      CSMIN = 1000000
      CALL RFILL (MAXANT, 1.E5, XXMIN)
      CALL RFILL (MAXANT, -1.E5, XXMAX)
      I = 2 * MAXIF * MAXANT
      CALL RFILL (I, 1.E8, PPMIN)
      CALL RFILL (I, -1.E8, PPMAX)
      CALL RFILL (I, 1.E8, YYMIN)
      CALL RFILL (I, -1.E8, YYMAX)
      YYMX = -1.E8
      YYMN = 1.E8
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR;',I7,'GETTING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SNPOPN (NROWS, IERR)
C-----------------------------------------------------------------------
C   Routine to open SN, CL, PC, TY, SY table and get necessary
C   information
C   Input from Common:
C      TYPE     C*2  'SN', 'CL', 'PC', 'TY', SY
C      DISKIN   I     Disk number
C      CNOIN    I     Catalog slot number
C      CATBLK   I(*)  Catalog header
C      SUMSTK   I     Stokes type requested 0=both, 1=R, 2=L,
C                     3=difference, 4=ratio
C   Output:
C      IERR     I     Error code, 0=OK else failed.
C   Output in common:
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of gain records in file.
C      NUMANT       I    Number of antennas
C      NUMPOL       I    Number of polarizations
C      NUMIF        I    Number of IFs.
C      ITVER        I    Version number opened.
C      KOLS         I(*) Column pointers
C      KOLTYP       I(*) Column data types
C      KOLDIM       I(*) Column dimension
C-----------------------------------------------------------------------
      INTEGER   NROWS, IERR
      INCLUDE 'ELFIT.INC'
C
      INTEGER MAXPCC
      PARAMETER (MAXPCC = 40)
      CHARACTER KEYW(4)*8, COLHD1(11)*24, COLHD2(13)*24, COLHD3(13)*24,
     *   COLTAB(40)*24, COLHED(37)*24, COLPC(MAXPCC)*24, KEYSN(4)*8,
     *   KEYPC(3)*8, COLPC1(20)*24, COLPC2(20)*24
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), IPOINT, KEYTYP(4),
     *   KLOCS(4), KEYVAL(6), I, KP, MSGSAV
      LOGICAL   T, ISTYPE
      REAL      KEYVR(6)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (KEYVAL, KEYVR)
      EQUIVALENCE (COLHED(1), COLHD1), (COLHED(12), COLHD2),
     *   (COLHED(25), COLHD3)
      EQUIVALENCE (COLPC(1), COLPC1), (COLPC(21), COLPC2)
      DATA COLHD1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA COLHD2 /'MBDELAY1      ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'TSYS 1                  ', 'TANT 1                  ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 '/
      DATA COLHD3 /'MBDELAY2      ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                ',
     *   'TSYS 2                  ', 'TANT 2                  ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 '/
      DATA COLPC1 /'TIME                ',
     *   'TIME_INTERVAL           ', 'SOURCE_ID               ',
     *   'ANTENNA_NO              ', 'ARRAY                   ',
     *   'FREQID                  ', 'DUM1                    ',
     *   'DUM2                    ', 'DUM3                    ',
     *   'DUM4                    ', 'DUM5                    ',
     *   'PC_FREQ 1               ', 'PC_REAL 1               ',
     *   'PC_IMAG 1               ', 'PC_RATE 1               ',
     *   'DUM6                    ', 'DUM7                    ',
     *   'DUM8                    ', 'DUM9                    ',
     *   'DUM10                   ' /
      DATA COLPC2 /'DUM11                   ',
     *   'DUM12                   ', 'DUM13                   ',
     *   'DUM14                   ', 'PC_FREQ 2               ',
     *   'PC_REAL 2               ', 'PC_IMAG 2               ',
     *   'PC_RATE 2               ', 'DUM15                   ',
     *   'DUM16                   ', 'DUM17                   ',
     *   'DUM18                   ', 'DUM19                   ',
     *   'DUM20                   ', 'DUM21                   ',
     *   'DUM22                   ', 'DUM23                   ',
     *   'CABLE_CAL               ', 'STATE 1                 ',
     *   'STATE 2                 ' /
      DATA KEYSN /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ','MGMOD   '/
      DATA KEYPC /'NO_POL  ', 'NO_BAND ', 'NO_TONES' /
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF (TYPE.EQ.'SY') THEN
         IF ((ICODES.LT.18) .OR. (ICODES.GT.25)) THEN
            MSGTXT = 'DATA OF REQUESTED TYPE NOT IN SY TABLES'
            IERR = 5
            GO TO 980
            END IF
         END IF
      IF ((ISTYPE(3)) .AND. ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC'))) THEN
         MSGTXT = 'NO SINGLEBAND DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(4)) .AND. (TYPE.EQ.'TY')) THEN
         MSGTXT = 'NO RESIDUAL RATE IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(5)) .AND. (TYPE.NE.'TY')) THEN
         MSGTXT = 'NO TSYS IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(7)) .AND. (TYPE.NE.'CL')) THEN
         MSGTXT = 'NO DOPPLER OFFSET IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(8)) .AND. ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC'))) THEN
         MSGTXT = 'NO SNR IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(9)) .AND. ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC'))) THEN
         MSGTXT = 'NO MULTIBAND DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(10)) .AND. (TYPE.NE.'TY')) THEN
         MSGTXT = 'NO TANT IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(11)) .AND. (TYPE.NE.'CL')) THEN
         MSGTXT = 'NO ATMOS. DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(12)) .AND. (TYPE.NE.'CL')) THEN
         MSGTXT = 'NO GEOM. DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(13)) .AND. (TYPE.NE.'PC')) THEN
         MSGTXT = 'NO CABLE CAL IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(14)) .AND. (TYPE.NE.'CL') .AND. (TYPE.NE.'SN')) THEN
         MSGTXT = 'NO DISP. DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ISTYPE(17)) .AND. (TYPE.NE.'CL') .AND. (TYPE.NE.'SN')) THEN
         MSGTXT = 'NO FARADAY ROT. IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', TYPE, DISKIN, CNOIN, IVER, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, TYPE, IVER
         GO TO 980
         END IF
      ITVER = IVER
C                                       Get number of scans
      NCLINR = CLBUFF(5)
      NROWS = NCLINR
C                                       Check if empty
      IF (NCLINR.LE.0) THEN
         IERR = 6
         MSGTXT = 'ERROR: SELECTED TABLE IS EMPTY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get column pointers
      NKEY = 40
      IF (TYPE.EQ.'PC') NKEY = MAXPCC
      DO 10 I = 1,NKEY
         IF (TYPE.EQ.'PC') THEN
            COLTAB(I) = COLPC(I)
         ELSE
            COLTAB(I) = COLHED(I)
            END IF
 10      CONTINUE
C                                       SY uses Re/Im/Wt for
C                                       DIF, SUM, GAIN
C                                       CKnKOL for cal type
      IF (TYPE.EQ.'SY') THEN
         COLTAB(13) = 'POWER DIF1'
         COLTAB(14) = 'POWER SUM1'
         COLTAB(17) = 'POST GAIN1'
         COLTAB(21) = 'CAL TYPE'
         COLTAB(26) = 'POWER DIF2'
         COLTAB(27) = 'POWER SUM2'
         COLTAB(30) = 'POST GAIN2'
         COLTAB(34) = 'CAL TYPE'
         END IF
      CALL FNDCOL (NKEY, COLTAB, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
C                                       Time column logical number
      CLTIME = KOLS(1)
C                                       Convert to pointers, types
      DO 20 I = 1,NKEY
         KP = KOLS(I)
         IF (KP.GT.0) THEN
            KOLS(I) = DATP(KP,1)
            KOLTYP(I) = MOD (DATP(KP,2), 10)
            KOLDIM(I) = DATP(KP,2) / 10
         ELSE
            KOLS(I) = -1
            KOLTYP(I) = -1
            KOLDIM(I) = 0
            END IF
 20      CONTINUE
C                                       Table keywords
      NKEY = 4
      IF (TYPE.EQ.'PC') NKEY = 3
      DO 30 I = 1,NKEY
         IF (TYPE .EQ. 'PC') THEN
            KEYW(I) = KEYPC(I)
         ELSE
            KEYW(I) = KEYSN(I)
            END IF
 30      CONTINUE
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF ((IERR.GE.1) .AND. (IERR.LE.20)) GO TO 999
      IERR = 0
C                                       Retrieve keyword values: PC
      IF (TYPE.EQ.'PC') THEN
         NUMANT = NSTNS
         GMMOD = 1.0
C                                       No. poln.
         NUMPOL = 1
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF
         NUMIF = 1
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. tones
         NTONE = 1
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NTONE = KEYVAL(IPOINT)
C                                       Retrieve keyword values: other
      ELSE
         NTONE = 1
C                                       No. antennas.
         NUMANT = NSTNS
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. poln.
         NUMPOL = 1
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF
         NUMIF = 1
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Mean gain modulus
         GMMOD = 1.0
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(4).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
            ELSE
               KEYVAD = KEYVR(IPOINT)
               END IF
            IF (KEYVAD.GT.0.0) GMMOD = 1.0 / KEYVAD
            END IF
         END IF
C                                       Set pointers
      DOPKOL = DOPKOL + BIF - 1
      DOP3KL = DOPKOL + EIF - 1
      DOPLKL = DOPKOL
      IF (TYPE.NE.'PC') PCNUM = 1
      IF (PCNUM.GT.NTONE) PCNUM = 1
      PCNUM = MAX (1, PCNUM)
C                                       1st poln
      IF ((ISTOK.EQ.ABS (ICOR0)) .OR. (ISTOK.EQ.ABS (ICOR0+4))) THEN
         MBKOL(1) = MB1KOL
         REKOL(1) = RE1KOL + ((BIF-1) * NTONE) + PCNUM - 1
         IMKOL(1) = IM1KOL + ((BIF-1) * NTONE) + PCNUM - 1
         DLKOL(1) = DL1KOL + BIF - 1
         RAKOL(1) = RA1KOL + BIF - 1
         WTKOL(1) = WT1KOL + BIF - 1
         RFKOL(1) = RF1KOL + BIF - 1
         TSKOL(1) = TS1KOL + BIF - 1
         TAKOL(1) = TA1KOL + BIF - 1
         CKKOL(1) = CK1KOL
         DCKOL(1) = DC1KOL
         DSKOL(1) = DS1KOL
         DDKOL(1) = DD1KOL
         STKOL(1) = ST1KOL + BIF - 1
C                                       2nd poln
      ELSE
         MBKOL(1) = MB2KOL
         REKOL(1) = RE2KOL + ((BIF-1) * NTONE) + PCNUM - 1
         IMKOL(1) = IM2KOL + ((BIF-1) * NTONE) + PCNUM - 1
         DLKOL(1) = DL2KOL + BIF - 1
         RAKOL(1) = RA2KOL + BIF - 1
         WTKOL(1) = WT2KOL + BIF - 1
         RFKOL(1) = RF2KOL + BIF - 1
         TSKOL(1) = TS2KOL + BIF - 1
         TAKOL(1) = TA2KOL + BIF - 1
         CKKOL(1) = CK2KOL
         DCKOL(1) = DC2KOL
         DSKOL(1) = DS2KOL
         DDKOL(1) = DD2KOL
         STKOL(1) = ST2KOL + BIF - 1
         END IF
C                                       2nd Poln
      MBKOL(2) = MB2KOL
      REKOL(2) = RE2KOL + ((BIF-1) * NTONE) + PCNUM - 1
      IMKOL(2) = IM2KOL + ((BIF-1) * NTONE) + PCNUM - 1
      DLKOL(2) = DL2KOL + BIF - 1
      RAKOL(2) = RA2KOL + BIF - 1
      WTKOL(2) = WT2KOL + BIF - 1
      RFKOL(2) = RF2KOL + BIF - 1
      TSKOL(2) = TS2KOL + BIF - 1
      TAKOL(2) = TA2KOL + BIF - 1
      CKKOL(2) = CK2KOL
      DCKOL(2) = DC2KOL
      DSKOL(2) = DS2KOL
      DDKOL(2) = DD2KOL
      STKOL(2) = ST2KOL + BIF - 1
C                                       Phase, amplitude, summary
      IF ((ISTYPE(1)) .OR. (ISTYPE(2)) .OR. (ISTYPE(6))
     *      .OR. (ISTYPE(15)) .OR. (ISTYPE(16))
     *      .OR. (ISTYPE(24)) .OR. (ISTYPE(25))) THEN
         IF ((REKOL(1).LT.0) .AND. (IMKOL(1).LT.0)) GO TO 500
         IF (((MUMPOL.EQ.2)) .AND. ((REKOL(2).LT.0) .OR.
     *      (IMKOL(2).LT.0))) GO TO 500
         END IF
C                                       Singleband Delay
      IF (ISTYPE(3)) THEN
         IF (DLKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (DLKOL(2).LT.0)) GO TO 500
         END IF
C                                       Rate
      IF (ISTYPE(4)) THEN
         IF (RAKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (RAKOL(2).LT.0)) GO TO 500
         END IF
C                                       System temperature
      IF (ISTYPE(5)) THEN
         IF (TSKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (TSKOL(2).LT.0)) GO TO 500
         END IF
C                                       Doppler offset
      IF (ISTYPE(7)) THEN
         IF (DOPLKL.LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       SNR
      IF (ISTYPE(8)) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         END IF
C                                       MDEL (multiband delay)
      IF (ISTYPE(9)) THEN
         IF (MBKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (MBKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       MDEL (multiband delay)
      IF (ISTYPE(9)) THEN
         IF (MBKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (MBKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       IF Antenna Temp
      IF (ISTYPE(10)) THEN
         IF (TAKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (TAKOL(2).LT.0)) GO TO 500
         END IF
C                                       Atmosphere
      IF (ISTYPE(11)) THEN
         IF (ATMKOL.LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       If geometric delay
      IF (ISTYPE(12)) THEN
         IF (WTKOL(1).LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       If cable cal
      IF (ISTYPE(13)) THEN
         IF (CABKOL.LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       DDLY (dispersive delay)
      IF (ISTYPE(14)) THEN
         IF (DSKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (DSKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       Ionospheric Faraday rotation
      IF (ISTYPE(17)) THEN
         IF (IFRKOL.LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         END IF
C                                       SY Pdif
      IF (ISTYPE(18)) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         END IF
C                                       SY Psum
      IF (ISTYPE(19)) THEN
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         END IF
C                                       SY post gain
      IF (ISTYPE(20)) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         END IF
C                                       SY Pon and Poff
      IF ((ISTYPE(21)) .OR. (ISTYPE(22))) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         END IF
C                                       SY TSYS
      IF (ISTYPE(23)) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         END IF
      GO TO 999
C                                       Requested data not in table
 500  WRITE(MSGTXT,1500) OPTYPE, TYPE
      IERR = 10
      GO TO 980
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1500 FORMAT (' REQUESTED DATA ',A,' NOT IN ',A,' TABLE ')
      END
      LOGICAL FUNCTION ISTYPE (FTYPE)
C-----------------------------------------------------------------------
C   ISTYPE inquires if type in list of requested types
C   Inputs:
C      FTYPE    I   Test type
C   Output:
C      ISTYPE   L   T => TYPE in ICODES
C-----------------------------------------------------------------------
      INTEGER   FTYPE
C
      INCLUDE 'ELFIT.INC'
C-----------------------------------------------------------------------
      ISTYPE = (ICODES.EQ.FTYPE)
C
 999  RETURN
      END
      SUBROUTINE SNPCNT (NWORDS, IERR)
C-----------------------------------------------------------------------
C   SNPCNT reads the SN or CL table to find the number of samples for
C   each antenna
C   Input:
C      NWORDS   I      Size of work array
C   Input/Output in common:
C      TSTART   R      Start time of plot
C      TSTOP    R      Stop time of plot
C   Output:
C      IERR     I      Error code, 0=OK else failed
C   Outputs in common:
C-----------------------------------------------------------------------
      INTEGER   NWORDS, IERR
C
      INCLUDE 'ELFIT.INC'
      LOGICAL   NODATA, OKAY
      INTEGER   I, NP, IFNUM, SCNT(MXSCAN)
      REAL      TB, TE, GTIME, XVARIB, CSOU
      REAL      VALUE(2*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL FILL (MAXANT, 0, NANREC)
      CALL FILL (MXSCAN, 0, SCNT)
      NODATA = .TRUE.
      TB = 1.0E5
      TE = -1.0E5
      XMX = TE
      XMN = TB
      XMXW = TE
      XMNW = TB
C                                       Loop thru data
      TINT = -1.0
      IF (INTKOL.LE.0) TINT = 10.0 / 86400.0
      NP = MUMPOL * MUMIF
      DO 100 ICLRNO = 1,NCLINR
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Solution interval
         IF (TINT.LE.0) TINT = GNREC(INTKOL)
C                                       Check weight per IF
C                                       If weight < CUTOFF them
C                                       set amp, phase, delay and
C                                       rate to FBLANK FOR EACH IF
         IF ((CUTOFF.GE.0.0) .AND. (WT1KOL.GT.0)) THEN
            IF (NUMPOL.EQ.2) THEN
               IF (SUMSTK.EQ.0) THEN
                  DO 5 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
                    IF (GNREC(WTKOL(2) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(2) + IFNUM) = FBLANK
                       GNREC(IMKOL(2) + IFNUM) = FBLANK
                       GNREC(DLKOL(2) + IFNUM) = FBLANK
                       GNREC(RAKOL(2) + IFNUM) = FBLANK
                       ENDIF
5                 CONTINUE
               ELSE IF (SUMSTK.EQ.1) THEN
                  DO 15 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
15                CONTINUE
               ELSE IF (SUMSTK.EQ.2) THEN
                  DO 25 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(2) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(2) + IFNUM) = FBLANK
                       GNREC(IMKOL(2) + IFNUM) = FBLANK
                       GNREC(DLKOL(2) + IFNUM) = FBLANK
                       GNREC(RAKOL(2) + IFNUM) = FBLANK
                       ENDIF
25                CONTINUE
                  END IF
               END IF
            IF (NUMPOL.EQ.1) THEN
                  DO 35 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
35                CONTINUE
               END IF
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(CLTIME).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF ((GNRECI(FRQKOL).GT.0) .AND. (GNRECI(FRQKOL).NE.FRQSEL)
     *      .AND. (FRQSEL.GT.0)) GO TO 100
C                                       Subarray
         IF (TYPE.NE.'PC') THEN
            IF ((GNRECI(SUBKOL).GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (GNRECI(SUBKOL).NE.SUBARR)) GO TO 100
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 50 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                       Check source
 60      IF (NID.GT.0) THEN
            ISOU = GNRECI(SOUKOL)
            DO 70 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 80
 70            CONTINUE
            GO TO 100
            END IF
C                                      Get start, stop times
 80      TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value
         CALL SNPDAT (VALUE, XVARIB, CSOU, OKAY)
C                                       Max. - Min
         IF (((OKAY) .OR. (BSYM.GT.0)) .AND. (XVARIB.NE.FBLANK)) THEN
            IF (OKAY) NODATA = .FALSE.
            NANREC(IANT) = NANREC(IANT) + 1
            I = CSOU + 0.1
            IF ((I.GT.0) .AND. (I.LE.MXSCAN)) SCNT(I) = SCNT(I) + 1
            END IF
 100     CONTINUE
      FANREC(1) = 1
      DO 120 I = 2,MAXANT
         FANREC(I) = FANREC(I-1) + NANREC(I-1)
 120     CONTINUE
      IF (NWORDS.LT.FANREC(MAXANT)+NANREC(MAXANT)) THEN
         MSGTXT = 'MEMORY TOO SMALL'
         IERR = 10
         END IF
C                                       source number translation
      NP = 0
      CALL FILL (MXSCAN, 0, STRANS)
      DO 130 I = 1,MXSCAN
         IF (SCNT(I).GT.0) THEN
            NP = NP + 1
            STRANS(I) = NP
            END IF
 130     CONTINUE
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNPCNT: ERROR =',I3,' FROM TABIO')
      END
      SUBROUTINE SNPMAX (NV, PLTPTS, IERR)
C-----------------------------------------------------------------------
C   SNPMAX reads the SN or CL table to find the max and min values for
C   each station or IF prior to plotting.
C   Input:
C      NV       I      Number values per time (source, X, n*Y)
C   Input/Output in common:
C      TSTART   R      Start time of plot
C      TSTOP    R      Stop time of plot
C   Output:
C      PLTPTS   R(*)   Data to be plotted (NV, *)
C      IERR     I      Error code, 0=OK else failed
C   Outputs in common:
C-----------------------------------------------------------------------
      INTEGER   NV, IERR
      REAL      PLTPTS(NV,*)
C
      LOGICAL   NODATA, OKAY
      INTEGER   I, NP, NN, IP, IIF, IIS, IFNUM, IS, KK
      REAL      TB, TE, TMAX, TMIN, GTIME, XVARIB, CSOU, TEMP
      INCLUDE 'ELFIT.INC'
      REAL      VALUE(2*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NODATA = .TRUE.
      TB = 1.0E5
      TE = -1.0E5
      XMX = TE
      XMN = TB
      XMXW = TE
      XMNW = TB
C                                       Loop thru data
      TINT = -1.0
      IF (INTKOL.LE.0) TINT = 10.0 / 86400.0
      NP = MUMPOL * MUMIF
      DO 100 ICLRNO = 1,NCLINR
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Solution interval
         IF (TINT.LE.0) TINT = GNREC(INTKOL)
C                                       Check weight per IF
C                                       If weight < CUTOFF them
C                                       set amp, phase, delay and
C                                       rate to FBLANK FOR EACH IF
         IF ((CUTOFF.GE.0.0) .AND. (WT1KOL.GT.0)) THEN
            IF (NUMPOL.EQ.2) THEN
               IF (SUMSTK.EQ.0) THEN
                  DO 5 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
                    IF (GNREC(WTKOL(2) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(2) + IFNUM) = FBLANK
                       GNREC(IMKOL(2) + IFNUM) = FBLANK
                       GNREC(DLKOL(2) + IFNUM) = FBLANK
                       GNREC(RAKOL(2) + IFNUM) = FBLANK
                       ENDIF
5                 CONTINUE
               ELSE IF (SUMSTK.EQ.1) THEN
                  DO 15 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
15                CONTINUE
               ELSE IF (SUMSTK.EQ.2) THEN
                  DO 25 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(2) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(2) + IFNUM) = FBLANK
                       GNREC(IMKOL(2) + IFNUM) = FBLANK
                       GNREC(DLKOL(2) + IFNUM) = FBLANK
                       GNREC(RAKOL(2) + IFNUM) = FBLANK
                       ENDIF
25                CONTINUE
                  END IF
               END IF
            IF (NUMPOL.EQ.1) THEN
                  DO 35 IFNUM = 0, MUMIF - 1
                    IF (GNREC(WTKOL(1) + IFNUM).LE.CUTOFF) THEN
                       GNREC(REKOL(1) + IFNUM) = FBLANK
                       GNREC(IMKOL(1) + IFNUM) = FBLANK
                       GNREC(DLKOL(1) + IFNUM) = FBLANK
                       GNREC(RAKOL(1) + IFNUM) = FBLANK
                       ENDIF
35                CONTINUE
               END IF
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(CLTIME).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF ((GNRECI(FRQKOL).GT.0) .AND. (GNRECI(FRQKOL).NE.FRQSEL)
     *      .AND. (FRQSEL.GT.0)) GO TO 100
C                                       Subarray
         IF (TYPE.NE.'PC') THEN
            IF ((GNRECI(SUBKOL).GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (GNRECI(SUBKOL).NE.SUBARR)) GO TO 100
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 50 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                       Check source
 60      IF (NID.GT.0) THEN
            ISOU = GNRECI(SOUKOL)
            DO 70 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 80
 70            CONTINUE
            GO TO 100
            END IF
C                                      Get start, stop times
 80      TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value
         CALL SNPDAT (VALUE, XVARIB, CSOU, OKAY)
C                                       Max. - Min
         IF (((OKAY) .OR. (BSYM.GT.0)) .AND. (XVARIB.NE.FBLANK)) THEN
            IF (OKAY) NODATA = .FALSE.
C                                       Put in array
            NUMPTS(IANT) = NUMPTS(IANT) + 1
            NN = FANREC(IANT) + NUMPTS(IANT) - 1
            IS = CSOU + 0.1
            IF ((IS.GT.0) .AND. (IS.LE.MXSCAN)) CSOU = STRANS(IS)
            PLTPTS(1,NN) = CSOU
            PLTPTS(2,NN) = XVARIB
            KK = 3
            CALL RCOPY (NP, VALUE, PLTPTS(KK,NN))
            KK = KK + NP
            XMX = MAX (XMX, XVARIB)
            XMN = MIN (XMN, XVARIB)
            IF (XVAR.NE.6) THEN
               XMX = MAX (XMX, XVARIB)
               XMN = MIN (XMN, XVARIB)
            ELSE
               TEMP = XVARIB
               IF (TEMP.LT.0.0) TEMP = TEMP + 360.
               XMX = MAX (XMX, TEMP)
               XMN = MIN (XMN, TEMP)
               IF (TEMP.GT.180.0) TEMP = TEMP - 360
               XMXW = MAX (XMXW, TEMP)
               XMNW = MIN (XMNW, TEMP)
               END IF
            XXMAX(IANT) = MAX (XXMAX(IANT), XVARIB)
            XXMIN(IANT) = MIN (XXMIN(IANT), XVARIB)
            CSMIN = MIN (CSMIN, CSOU)
            CSMAX = MAX (CSMAX, CSOU)
C                                       If not a summary plot
            IP = 0
            DO 90 IIF = 1,MUMIF
               DO 85 IIS = 1,MUMPOL
                  IP = IP + 1
                  IF (VALUE(IP).NE.FBLANK) THEN
                     YYMX = MAX (YYMX, VALUE(IP))
                     YYMN = MIN (YYMN, VALUE(IP))
                     IF (ICODES.EQ.1) THEN
                        IF (VALUE(IP).LE.-180.0) THEN
                           VALUE(IP) = VALUE(IP) + 360.
                        ELSE IF (VALUE(IP).GT.180.) THEN
                           VALUE(IP) = VALUE(IP) - 360.
                           END IF
                        YYMIN(IIS,IIF,IANT) = MIN (VALUE(IP),
     *                     YYMIN(IIS,IIF,IANT))
                        YYMAX(IIS,IIF,IANT) = MAX (VALUE(IP),
     *                     YYMAX(IIS,IIF,IANT))
                        IF (VALUE(IP).LT.0.0) VALUE(IP) =
     *                     VALUE(IP) + 360.0
                        PPMIN(IIS,IIF,IANT) = MIN (VALUE(IP),
     *                     PPMIN(IIS,IIF,IANT))
                        PPMAX(IIS,IIF,IANT) = MAX (VALUE(IP),
     *                     PPMAX(IIS,IIF,IANT))
                     ELSE IF (ICODES.EQ.6) THEN
                        YYMIN(1,1,1) = MIN (VALUE(IP),
     *                     YYMIN(1,1,1))
                        YYMAX(1,1,1) = MAX (VALUE(IP),
     *                     YYMAX(1,1,1))
                        IF (VALUE(IP).LE.-180.0) THEN
                           VALUE(IP) = VALUE(IP) + 360.
                        ELSE IF (VALUE(IP).GT.180.) THEN
                           VALUE(IP) = VALUE(IP) - 360.
                           END IF
                        YYMIN(2,1,1) = MIN (VALUE(IP),
     *                     YYMIN(2,1,1))
                        YYMAX(2,1,1) = MAX (VALUE(IP),
     *                     YYMAX(2,1,1))
                        IF (VALUE(IP).LT.0.0) VALUE(IP) =
     *                     VALUE(IP) + 360.
                        PPMIN(1,1,1) = MIN (VALUE(IP),
     *                     PPMIN(1,1,1))
                        PPMAX(1,1,1) = MAX (VALUE(IP),
     *                     PPMAX(1,1,1))
                     ELSE
                        YYMIN(IIS,IIF,IANT) = MIN (VALUE(IP),
     *                     YYMIN(IIS,IIF,IANT))
                        YYMAX(IIS,IIF,IANT) = MAX (VALUE(IP),
     *                        YYMAX(IIS,IIF,IANT))
                        END IF
                     END IF
 85               CONTINUE
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       reset max min on fixed scale
      IF (PIXR(1).LT.PIXR(2)) THEN
         YYMX = PIXR(2)
         YYMN = PIXR(1)
         DO 120 IANT = 1,MUMANT
            DO 115 IIF = 1,MUMIF
               DO 110 IIS = 1,MUMPOL
                  IF (YYMAX(IIS,IIF,IANT).GE.
     *               YYMIN(IIS,IIF,IANT)) THEN
                     YYMAX(IIS,IIF,IANT) = PIXR(2)
                     YYMIN(IIS,IIF,IANT) = PIXR(1)
                     IF (ICODES.EQ.1) THEN
                        PPMAX(IIS,IIF,IANT) = PIXR(2)
                        PPMIN(IIS,IIF,IANT) = PIXR(1)
                        END IF
                     END IF
 110              CONTINUE
 115           CONTINUE
 120        CONTINUE
         END IF
C                                       Set actual X range
      SWAP = .FALSE.
      IF (XVAR.EQ.6) THEN
         IF (XMX-XMN.LE.XMXW-XMNW) THEN
            XSTART = XMN
            XSTOP  = XMX
         ELSE
            XSTART = XMNW
            XSTOP  = XMXW
            SWAP = .TRUE.
            END IF
      ELSE
         XSTART = XMN
         XSTOP  = XMX
         END IF
      IF (FULLX) THEN
         IF (XVAR.LE.2) THEN
            XSTART = 0.0
            XSTOP = 90.0
         ELSE IF (XVAR.EQ.3) THEN
            XSTART = -0.5
            XSTOP = 0.5
         ELSE IF (XVAR.EQ.5) THEN
            XSTART = -180.0
            XSTOP = 180.0
         ELSE IF (XVAR.EQ.6) THEN
            XSTART = 0.0
            XSTOP = 360.0
            END IF
         END IF
C                                       Check for no data
      IF (NODATA) THEN
         IERR = 6
         MSGTXT = 'NO DATA SELECTED'
         GO TO 990
         END IF
C                                       check and set scaling
      TMAX = (XSTOP + 0.1 * (XSTOP - XSTART))
      TMIN = (XSTART- 0.1 * (XSTOP - XSTART))
      IF (XVAR.EQ.3) THEN
         TMAX = 360.0 * TMAX
         TMIN = 360.0 * TMIN
         END IF
C                                       If start time is stop time,
      IF (ABS (TMAX-TMIN) .LT. 0.01) THEN
         TMIN = MAX( TMIN-0.005, 0.0)
         TMAX = TMIN + 0.01
         END IF
      TSTART = TB
      TSTOP = TE
      XYOFF(1) = TMIN
      XYSCL(1) = 1000.0 / (TMAX - TMIN)
      PRAN(1,1) = TMIN
      PRAN(2,1) = TMAX
C                                       Send back time range
      XTIME(1) = TSTART
      XTIME(2) = 0.0
      XTIME(3) = 0.0
      XTIME(4) = 0.0
      XTIME(5) = TSTOP
      XTIME(6) = 0.0
      XTIME(7) = 0.0
      XTIME(8) = 0.0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNPMAX: ERROR =',I3,' FROM TABIO')
      END
      SUBROUTINE GETSCL (LST, LIF, LANT, DOIT)
C-----------------------------------------------------------------------
C   GETSCL converts a number of max/min's to a scale
C   Inputs:
C      LST     I   Stokes
C      LIF     I   IF number
C      LANT    I   Antenna number
C   Output:
C      DOIT    L      There were valid values
C   Output in common
C      XYSCL   R(2)   Scaling - only 2nd one changed
C      XYOFF   R(2)   Offset  - only second one changed
C-----------------------------------------------------------------------
      INTEGER   LST, LIF, LANT
      LOGICAL   DOIT
C
      INCLUDE 'ELFIT.INC'
      REAL      YMX, YMN, PMX, PMN, TMAX, TMIN, TDIF, TOLER(30), SIZEY
C                                       Minimum value range for each
C                                       ICODE
C                  phs    amp    delay    rate    Tsys Summary doppler
      DATA TOLER /0.001, 0.001, 1.0E-12, 1.0E-8, 0.001,  0.01,  0.001,
C                  snr  MB delay   Tant    Atm dly  geo dly  ccal
     *            0.001, 1.0E-12, 0.000001, 1.0E-12, 1.0E-12, 1.0E-14,
C                  ddely   real   imag  Faraday Pdif   Psum   Pgn
     *            1.0E-12, 0.001, 0.001, 0.005, 0.001, 0.001, 1.E-5,
C                 Pon   Poff  Psys   PDgain PSgain
     *            0.01, 0.01, 0.01,  0.001, 0.001, 5*0.0/
C-----------------------------------------------------------------------
      DOIT = .FALSE.
      YMX = -1.E8
      YMN = -YMX
      PMX = YMX
      PMN = YMN
      IF (YYMAX(LST,LIF,LANT).GE.YYMIN(LST,LIF,LANT)) THEN
         DOIT = .TRUE.
         YMX = MAX (YMX, YYMAX(LST,LIF,LANT))
         YMN = MIN (YMN, YYMIN(LST,LIF,LANT))
         IF (ICODES.EQ.1) THEN
            PMX = MAX (PMX, PPMAX(LST,LIF,LANT))
            PMN = MIN (PMN, PPMIN(LST,LIF,LANT))
            END IF
         END IF
      IF ((ICODES.EQ.1) .AND. (PMX-PMN.LT.YMX-YMN)) THEN
         YMX = PMX
         YMN = PMN
         END IF
      SIZEY = 1000.0 / NCOUNT
      TMAX = YMX + 0.1 * (YMX - YMN)
      TMIN = YMN - 0.1 * (YMX - YMN)
      IF (ABS (TMAX-TMIN) .LT. TOLER(ICODES)) THEN
         TMAX = TMAX + TOLER(ICODES)
         TMIN = TMIN - TOLER(ICODES)
         END IF
      TDIF = TMAX - TMIN
      IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
      XYOFF(2) = TMIN
      XYSCL(2) = 1000.0 / TDIF / NCOUNT
      PRAN(1,2) = TMIN
      PRAN(2,2) = TMAX
C
 999  RETURN
      END
      SUBROUTINE SNPDAT (VALUE, XVARIB, CSOU, OKAY)
C-----------------------------------------------------------------------
C   Routine to get the specified value from a SN/CL/TY table entry
C   Input from common:
C      GNREC    R(*)  Table record
C      ICODE    I     Plot code
C      SUMSTK   I     Selected Stokes 0=both, 1=R, 2=L
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R(*)   Table value, magic value blanked (amp on ICODE 6)
C      XVARIB   R      Value of associated x-axis variable
C      CSOU     R      source number
C      OKAY     L      Some values are good
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      VALUE(2*MAXIF), XVARIB, CSOU
      LOGICAL   OKAY
C
      INTEGER   IIS, IIF, IP1, IP2, LP, JP1, JP2, KP1, KP2, ICODE
      REAL      V, S, TC
      LOGICAL   T
      INCLUDE 'ELFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       In case the data is bad
      LP = MUMPOL * MUMIF
      CALL XCALC (XVARIB, CSOU)
      ICODE = ICODES
      CALL RFILL (LP, FBLANK, VALUE)
C                                       Select data type
C                                       Phase (deg)
      IF (ICODE.EQ.1) THEN
         DO 110 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            JP2 = IMKOL(2) - NTONE
            DO 105 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               IP2 =  IP2 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK)) THEN
                  VALUE(LP) = 57.296 *
     *               ATAN2 (GNREC(JP1), GNREC(IP1) + 1.0E-20)
                  END IF
 105           CONTINUE
 110        CONTINUE
C                                       Amplitude
      ELSE IF (ICODE.EQ.2) THEN
         DO 130 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            JP2 = IMKOL(2) - NTONE
            DO 125 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               IP2 =  IP2 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK)) THEN
                  VALUE(LP) = GMMOD *
     *               SQRT ((GNREC(IP1)**2) + (GNREC(JP1)**2))
                  END IF
 125           CONTINUE
 130        CONTINUE
C                                       Delay (sec)
      ELSE IF (ICODE.EQ.3) THEN
         DO 150 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = DLKOL(IIS) - 1
            IP2 = DLKOL(2) - 1
            DO 145 IIF = 1,MUMIF
               IP1 = IP1 + 1
               IP2 =  IP2 + 1
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 145           CONTINUE
 150        CONTINUE
C                                       Rate (Hz)
      ELSE IF (ICODE.EQ.4) THEN
         DO 170 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = RAKOL(IIS) - NTONE
            IP2 = RAKOL(2) - NTONE
            DO 165 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 = IP2 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1) * RATFAC(IIF)
                  END IF
 165           CONTINUE
 170        CONTINUE
C                                       System temperature (K)
      ELSE IF (ICODE.EQ.5) THEN
         DO 190 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = TSKOL(IIS) - 1
            IP2 = IP1
            IP2 = TSKOL(2) - 1
            DO 185 IIF = 1,MUMIF
               IP1 = IP1 + 1
               IP2 =  IP2 + 1
               LP = LP + MUMPOL
               IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
               IF (ABS(GNREC(IP2)-999.0).LT.0.1) GNREC(IP2) = FBLANK
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 185           CONTINUE
 190        CONTINUE
C                                       Doppler offset (Hz)
      ELSE IF (ICODE.EQ.7) THEN
         IF (GNREC(DOPLKL).NE.FBLANK) THEN
            VALUE(LP) = GNREC(DOPLKL)
            END IF
C                                       SNR (no units)
      ELSE IF (ICODE.EQ.8) THEN
         DO 250 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = WTKOL(IIS) - 1
            IP2 = WTKOL(2) - 1
            DO 245 IIF = 1,MUMIF
               IP1 = IP1 + 1
               IP2 =  IP2 + 1
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 245           CONTINUE
 250        CONTINUE
C                                       Multiband Delay (sec)
      ELSE IF (ICODE.EQ.9) THEN
         DO 270 IIS = 1,MUMPOL
            LP = IIS
            IP1 = MBKOL(IIS)
            IP2 = MBKOL(2)
            IF (GNREC(IP1).NE.FBLANK) THEN
               VALUE(LP) = GNREC(IP1)
               END IF
 270        CONTINUE
C                                       Tant (K)
      ELSE IF (ICODE.EQ.10) THEN
         DO 290 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = TAKOL(IIS) - 1
            IP2 = IP1
            IP2 = TAKOL(2) - 1
            DO 285 IIF = 1,MUMIF
               IP1 = IP1 + 1
               IP2 =  IP2 + 1
               LP = LP + MUMPOL
               IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
               IF (ABS(GNREC(IP2)-999.0).LT.0.1) GNREC(IP2) = FBLANK
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 285           CONTINUE
 290        CONTINUE
C                                       Atmos. Delay (sec)
      ELSE IF (ICODE.EQ.11) THEN
         IF (GNREC(ATMKOL).NE.FBLANK) VALUE(LP) = GNREC(ATMKOL)
C                                       Geometric Delay (sec)
      ELSE IF (ICODE.EQ.12) THEN
         IF (GNRECD(GEOKOL).NE.DBLANK) VALUE(LP) = GNRECD(GEOKOL)
C                                       Cable cal (sec)
      ELSE IF (ICODE.EQ.13) THEN
         IF (GNRECD(CABKOL).NE.DBLANK) VALUE(LP) = GNRECD(CABKOL)
C                                       Dispersive Delay (sec)
      ELSE IF (ICODE.EQ.14) THEN
         DO 370 IIS = 1,MUMPOL
            LP = IIS
            IP1 = DSKOL(IIS)
            IP2 = DSKOL(2)
            IF (GNREC(IP1).NE.FBLANK) THEN
               VALUE(LP) = GNREC(IP1)
               END IF
 370        CONTINUE
C                                       REAL
      ELSE IF (ICODE.EQ.15) THEN
         DO 395 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            DO 390 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 =  IP2 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GMMOD * GNREC(IP1)
                  END IF
 390           CONTINUE
 395        CONTINUE
C                                       IMAG
      ELSE IF (ICODE.EQ.16) THEN
         DO 420 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            JP1 = IMKOL(IIS) - NTONE
            JP2 = IMKOL(2) - NTONE
            DO 415 IIF = 1,MUMIF
               JP1 = JP1 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(JP1).NE.FBLANK) THEN
                  VALUE(LP) = GMMOD * GNREC(JP1)
                  END IF
 415           CONTINUE
 420        CONTINUE
C                                       Atmos. Delay (sec)
      ELSE IF (ICODE.EQ.17) THEN
         IF (GNREC(IFRKOL).NE.FBLANK) VALUE(LP) = GNREC(IFRKOL)
C                                       PDIF, PSUM, PGN
      ELSE IF ((ICODE.GE.18) .AND. (ICODE.LE.20)) THEN
         DO 430 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IF (ICODE.EQ.18) THEN
               IP1 = REKOL(IIS) - NTONE
               IP2 = REKOL(2) - NTONE
            ELSE IF (ICODE.EQ.19) THEN
               IP1 = IMKOL(IIS) - NTONE
               IP2 = IMKOL(2) - NTONE
            ELSE
               IP1 = WTKOL(IIS) - NTONE
               IP2 = WTKOL(2) - NTONE
               END IF
            DO 425 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 = IP2 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 425           CONTINUE
 430        CONTINUE
C                                       PON, POFF
      ELSE IF ((ICODE.GE.21) .AND. (ICODE.LE.22)) THEN
         S = 1.0
         IF (ICODE.EQ.22) S = -1.0
         DO 440 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            JP2 = IMKOL(2) - NTONE
            KP1 = WTKOL(IIS) - NTONE
            KP2 = WTKOL(2) - NTONE
            DO 435 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 = IP2 + NTONE
               JP1 = JP1 + NTONE
               JP2 = JP2 + NTONE
               KP1 = KP1 + NTONE
               KP2 = KP2 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK).AND. (GNREC(KP1).NE.FBLANK)
     *            .AND.(GNREC(KP1).NE.0.0)) THEN
                  V = (GNREC(JP1) + S*GNREC(IP1)) / (2.0*GNREC(KP1))
                  VALUE(LP) = V
                  END IF
 435           CONTINUE
 440        CONTINUE
C                                       PSUM / PDIF = Tsys/Tcal
      ELSE IF (ICODE.EQ.23) THEN
         DO 450 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            JP2 = IMKOL(2) - NTONE
            DO 445 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 = IP2 + NTONE
               JP1 = JP1 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF ((CKKOL(1).GT.0) .AND. (GNRECI(CKKOL(1)).EQ.1)) THEN
                  TC = TCAL(ISTOK+IIS+1,IIF-1+BIF,IANT)
               ELSE
                  TC = TCAL(ISTOK+IIS-1,IIF-1+BIF,IANT)
                  END IF
               IF (TC.LE.0.0) TC = FBLANK
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK).AND. (GNREC(IP1).GT.0.0)
     *            .AND. (TC.NE.FBLANK)) THEN
                  V = GNREC(JP1) / GNREC(IP1) / 2.0 * TC
                  VALUE(LP) = V
                  END IF
 445           CONTINUE
 450        CONTINUE
C                                       PDIF, PSUM corr by PGN
      ELSE IF ((ICODE.GE.24) .AND. (ICODE.LE.25)) THEN
         DO 460 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IF (ICODE.EQ.24) THEN
               IP1 = REKOL(IIS) - NTONE
               IP2 = REKOL(2) - NTONE
            ELSE
               IP1 = IMKOL(IIS) - NTONE
               IP2 = IMKOL(2) - NTONE
               END IF
            JP1 = WTKOL(IIS) - NTONE
            JP2 = WTKOL(2) - NTONE
            DO 455 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               IP2 = IP2 + NTONE
               JP1 = JP1 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK)) THEN
                  V = 4096. * GNREC(JP1) * GNREC(JP1)
                  VALUE(LP) = GNREC(IP1) / V
                  END IF
 455           CONTINUE
 460        CONTINUE
C                                       Gains as powers
      ELSE IF ((ICODE.EQ.26) .OR. (ICODE.EQ.27)) THEN
         DO 470 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            IP2 = REKOL(2) - NTONE
            JP2 = IMKOL(2) - NTONE
            DO 465 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               IP2 =  IP2 + NTONE
               JP2 = JP2 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND.
     *            (GNREC(JP1).NE.FBLANK)) THEN
                  VALUE(LP) = GMMOD *
     *               ((GNREC(IP1)**2) + (GNREC(JP1)**2))
                  IF (VALUE(LP).EQ.0.0) THEN
                     VALUE(LP) = FBLANK
                  ELSE IF (ICODE.EQ.27) THEN
                     VALUE(LP) = -10.0 * LOG10 (VALUE(LP))
                  ELSE
                     VALUE(LP) = 1.0 / VALUE(LP)
                     END IF
                  END IF
 465           CONTINUE
 470        CONTINUE
         END IF
C
      OKAY = .TRUE.
      LP = MUMPOL * MUMIF
      DO 910 IIS = 1,LP
         IF (VALUE(IIS).NE.FBLANK) GO TO 999
 910     CONTINUE
      OKAY = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE SNPLOT (NV, NA, PLTPTS, IRET)
C-----------------------------------------------------------------------
C   SNPLOT plots the data thru calls to PLTSN.
C   Input:
C      NV       I      Number values per antenna
C      NA       I      Number antennas in data array
C      PLTPTS   R(*)   Data (NV,*)
C   Output:
C      IRET     I      Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NV, NA, IRET
      REAL      PLTPTS(NV,*)
C
      INTEGER   IPLOT, JPLT, IPLT, NPLT, LUMIF, LUMST, IIF, IIS
      LOGICAL   DOIT
      INCLUDE 'ELFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
C                                       Loop thru stations to plot
      LUMIF = MUMIF
      LUMST = MUMPOL
C                                       count the plots
      NPLOTS = 0
      DO 40 IPLT = 1,MUMANT
         DO 35 IIF = 1,LUMIF
            DO 30 IIS = 1,LUMST
               CALL GETSCL (IIS, IIF, IPLT, DOIT)
               IF (DOIT) NPLOTS = NPLOTS + 1
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
C                                       Now plot
      NPLT = 0
      DO 100 IPLT = 1,MUMANT
         DO 90 IIF = 1,LUMIF
            DO 80 IIS = 1,LUMST
               CALL GETSCL (IIS, IIF, IPLT, DOIT)
               IF (DOIT) THEN
                  NPLT = NPLT + 1
                  JPLT = NPLT
                  IPLOT = MOD (NPLT-1, NCOUNT) + 1
                  IF (NPLT.EQ.NPLOTS) IPLOT = -IPLOT
                  CALL PLTSN (IPLOT, IIS, IIF, IPLT, NV, PLTPTS, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLTSN (IPLOT, LST, LIF, ANTNO, NV, PLTPTS, IRET)
C-----------------------------------------------------------------------
C   PLTSN actually plots data.
C   Input:
C      IPLOT    I      Plot number on current page. If neg. then this is
C                      last plot.
C      LST      I      Stokes this plot
C      LIF      I      IF this plot
C      ANTNO    I      Antenna number
C      NV       I      Number values
C      PLTPTS   R(*)   Data to plot (NV,*)
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                       -1 => user request termination
C                        1 => failed to add to catalog
C                        2 => failed to create
C                        3 => graph file write error
C                        4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, LST, LIF, ANTNO, NV, IRET
      REAL      PLTPTS(NV,*)
C
      INCLUDE 'ELFIT.INC'
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, CHTMP*18,
     *   AUNITS(NCODE+2)*8, CHTYPE(NCODE+2)*16, XUNITS(6)*20,
     *   CSAVE*5
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, JCODE, NN, IP, ILITY, NNN, MODERR
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   DBY, COLV, COL(3), AX(5), AY(5), OLDSRC
      DOUBLE PRECISION DMODEL(10), XX
      LOGICAL   T, F, GOOD, CATUP, DONEG, BLNKD, SCOLOR, DOCOLR
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      SAVE XMULT, CSAVE, LTYPE, BUFFER
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Degrees','Gain','Seconds','Hz','Kelvin',' ','Hz',
     *             'SNR', 'Seconds', 'Kelvin', 'Seconds', 'Seconds',
     *             'Seconds','Sec/m**2','Gain','Gain','Rad/m**2',
     *             'Counts', 'Counts', 'Gain', 'Counts', 'Counts',
     *             'Kelvin', 'Counts', 'Counts', 'Gain^-2', 'db',
     *             'Degrees', 'PCamp'/
      DATA CHTYPE /'Gain phs', 'Gain amp', 'Delay','Rate','Tsys',
     *             ' ', 'Doppler offset', 'SNR', 'Multiband delay',
     *             'Tant', 'Atmosphere delay', 'Geometric Delay',
     *             'Cable Cal','Dispersive delay', 'Real', 'Imag',
     *             'Ion. Faraday rot', 'Power difference',
     *             'Power sum', 'Post gain', 'Power NT on',
     *             'Power NT off', 'Tsys', 'Pdif/Pgain**2',
     *             'Psum/Pgain**2', 'Rel. Power', 'Power db',
     *             'Phase-cal Phase', 'Phase-cal Ampl'/
      DATA XUNITS /'Zenith angle', 'Elevation (degrees)',
     *   'Hour Angle (hr)', '??????????', 'Parallactic angle',
     *   'Azimuth (degrees)' /
C-----------------------------------------------------------------------
C                                       Time system from AN table
      SCOLOR = (XDO3C.GT.1.5) .AND. (CSMAX-CSMIN.GT.0.99)
      OLDSRC = -1000.0
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C                                       do the fitting
      CALL FITIT (LST, LIF, ANTNO, NV, PLTPTS, DMODEL, MODERR)
C
      JCODE = ICODES
      IF (TYPE.EQ.'PC') THEN
         IF (JCODE.EQ.1) JCODE = NCODE+1
         IF (JCODE.EQ.2) JCODE = NCODE+2
         END IF
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IRET = 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFFER, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
         CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 17
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      IF (DOTV) THEN
         TRC(1) = WINDTV(3) - WINDTV(1)
         TRC(2) = WINDTV(4) - WINDTV(2)
         CALL GCINIT (GPHTVG(4), 0, IERR)
         IF (IERR.NE.0) GO TO 960
         CALL GCINIT (GPHTVG(3), 0, IERR)
         IF (IERR.NE.0) GO TO 960
         END IF
      XYRATO = 1.0
      PLTINC = TRC(2) / NCOUNT
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = TRC(2) - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (XVAR.EQ.3) LABTYP(LOCNUM) = 8
      AXTYP(LOCNUM) = 0
      TR = 1.1 * (PRAN(2,2)-PRAN(1,2))
      IF (TR.LE.0.0) TR = 1.0
      IF (ABS(IPLOT).EQ.1) THEN
         TI = TR
         CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
         XMULT(2) = TR / TI
         CSAVE = CPREF(2,LOCNUM)
         END IF
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 50 I = 1,2
         SIZE = XTRC(I) - XBLC(I) + 1
         TR = PRAN(2,I) - PRAN(1,I)
         XYSCL(I) = (XTRC(I) - XBLC(I)) / TR
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 50      CONTINUE
      CTYP(1,LOCNUM) = XUNITS(XVAR)
      CTYP(2,LOCNUM) = AUNITS(JCODE)
C                                       Init plot calls again
C                                       Number of characters on each
C                                       side of the plot
      IF (ABS (IPLOT).EQ.1) THEN
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
         LTYPE = MOD (ABS (LABEL), 100)
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 4
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *      CHOUT(4) = CHOUT(4) + 1.333
C                                       Init for line drawing.
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, BUFFER, IERR)
         IRET = 3
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1000) VER
            CALL MSGWRT (2)
            END IF
         END IF
      IRET = 3
      CATUP = T
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
         DX = 0.0
         DY = 1.833
C                                       The second line of the header
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INCHAR = 16
         INP = 1
         TEXT = CHTYPE(JCODE)
         IF (ICOR0.LT.-4) THEN
            IF (TEXT(1:3).EQ.'R-L') THEN
               TEXT(1:3) = 'X-Y'
            ELSE IF (TEXT(:3).EQ.'R/L') THEN
               TEXT(1:3) = 'X/Y'
               END IF
            END IF
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         IF (XVAR.EQ.1) THEN
            TEXT(INP:INP+17) = ' vs zenith angle for '
            INP = INP + 21
         ELSE IF (XVAR.EQ.2) THEN
            TEXT(INP:INP+17) = ' vs elevation for '
            INP = INP + 18
         ELSE IF (XVAR.EQ.3) THEN
            TEXT(INP:INP+10) = ' vs HA for '
            INP = INP + 11
         ELSE IF (XVAR.EQ.4) THEN
            TEXT(INP:INP+16) = ' vs ?????????? '
            INP = INP + 17
         ELSE IF (XVAR.EQ.5) THEN
            TEXT(INP:INP+17) = ' vs parallactic angle for '
            INP = INP + 26
         ELSE IF (XVAR.EQ.6) THEN
            TEXT(INP:INP+17) = ' vs azimuth for '
            INP = INP + 16
            END IF
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(INP:), INCHAR)
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       the third line of header
         DY = 0.5
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INP = 1
C
         WRITE (TEXT(INP:),1010) TYPE, ITVER
         INP = INP + 8
         IF (TYPE .EQ. 'PC') THEN
            WRITE (TEXT(INP:),1015) PCNUM
            INP = INP + 9
            END IF
C                                       Stokes and IF
         IF (SUMSTK.GT.0) THEN
            TEXT(INP:) = XSTOK(:1) // 'pol_'
            INP = INP + 7
         ELSE
            TEXT(INP:) = 'Rpol & Lpol_'
            IF (ICOR0.LT.-4) TEXT(INP:) = 'Xpol & Ypol_'
            INP = INP + 14
            END IF
         IF (BIF.EQ.EIF) THEN
            WRITE (TEXT(INP:),1021) BIF
            INP = INP + 6
         ELSE
            WRITE (TEXT(INP:),1022) BIF, EIF
            INP = INP + 11
            END IF
C                                       Phase-cal tone
         IF ((TYPE.EQ.'PC') .AND. ((JCODE.EQ.NCODE+1) .OR.
     *         (JCODE.EQ.NCODE+2))) THEN
            WRITE (TEXT(INP:),1060) PCNUM
            END IF
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) THEN
            DY = 0.5 + 2 * 1.333
C                                       the first line of the header
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
C                                       station ID
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  1.5
      DY = -1.8
      WRITE (TEXT,1040) ANTNO
      INP = 4
      IF (MUMPOL.LE.1) THEN
         TEXT(INP:) = XSTOK(:1)
         INP = INP + 1
      ELSE IF (MUMIF.LE.1) THEN
         IF (ICOR0.LT.-4) THEN
            TEXT(INP:) = 'X'
            IF (LST.EQ.2) TEXT(INP:) = 'Y'
         ELSE
            TEXT(INP:) = 'R'
            IF (LST.EQ.2) TEXT(INP:) = 'L'
            END IF
         INP = INP + 1
         END IF
      TEXT(INP+1:) = STNNAM(ANTNO)
      CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Stokes, IF
      IF (MUMIF.GT.1) THEN
         INP = 1
         WRITE (TEXT(INP:),1021) LIF + BIF - 1
         INP = INP + 5
         IF (MUMPOL.GT.1) THEN
            IF (ICOR0.LT.-4) THEN
               TEXT(INP:) = 'X'
               IF (LST.EQ.2) TEXT(INP:) = 'Y'
            ELSE
               TEXT(INP:) = 'R'
               IF (LST.EQ.2) TEXT(INP:) = 'L'
               END IF
            INP = INP + 1
            END IF
         CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DX =  1.5
         DY = -3.133
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       model parms
      WRITE (TEXT,1025) (DMODEL(I), I = 1,ORDER+1)
      CALL REFRMT (TEXT, '_', INCHAR)
      DX = (XBLC(1) + XTRC(1)) / 2.0
      CALL GPOS (DX, XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = -INCHAR/2.0
      DY = -4.5
      CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Set up location common
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      CPREF(2,LOCNUM) = CSAVE
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Size of symbol.
      DBY = 0.5 * FACTOR
C                                       Loop
      ILITY = 4
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DOCOLR = SCOLOR
C                                       Outer loop: IF, stokes
      COLV = 0.0
      IP = (LIF-1) * MUMPOL + LST + 2
C                                       Point plot
      DO 120 NN = 1,NUMPTS(ANTNO)
         DONEG = (JCODE.EQ.1) .OR. (JCODE.EQ.NCODE+1)
C                                       Scale X
         NNN = NN - 1 + FANREC(ANTNO)
         XY(1) = PLTPTS(2,NNN)
         IF ((SWAP) .AND. (XY(1).GT.180.0)) XY(1) = XY(1) - 360.0
         IF (XVAR.EQ.3) XY(1) = XY(1) * 360.0
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
            NNOFIT = NNOFIT + 1
            GO TO 120
            END IF
C                                       source color
         IF ((SCOLOR) .AND. (ABS(PLTPTS(1,NNN)-OLDSRC).GT.0.1)) THEN
            COLV = 0.97 * (PLTPTS(1,NNN)-CSMIN) / (CSMAX-CSMIN)
            CALL COLOR3 (COLV, .FALSE., COL)
            CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            OLDSRC = PLTPTS(1,NNN)
            END IF
C                                       loop for points
         VALUE = PLTPTS(IP,NNN)
C                                       ?????????????????
         IF (VALUE.NE.FBLANK) THEN
 110        XY(2) = VALUE
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
               IF (DONEG) THEN
                  IF (VALUE.LT.0.) THEN
                     VALUE = VALUE + 360.
                  ELSE
                     VALUE = VALUE - 360.
                     END IF
                  DONEG = .FALSE.
                  GO TO 110
               ELSE
                  NNOFIT = NNOFIT + 1
                  END IF
            ELSE
               NGOOD = NGOOD + 1
C                                       Mark point
               DY = 5.0 * FACTOR
               DX = 5.0 * FACTOR
               IF (XYRATO.GT.1.0) THEN
                  DY = DY * XYRATO
               ELSE
                  DX = DX / XYRATO
                  END IF
               AX(1) = XY(1)
               AY(1) = XY(2)
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - DX
               AX(5) = AX(1) + DX
               AY(2) = AY(1) + DY
               AY(3) = AY(1) - DY
               AY(4) = AY(1)
               AY(5) = AY(1)
               IF (ILITY.NE.4) THEN
                  ILITY = 4
                  CALL GLTYPE (ILITY, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
               END IF
               CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *            DOCOLR, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
         ELSE IF (BSYM.GT.0) THEN
            DY = 5.0 * FACTOR
            DX = 5.0 * FACTOR
            IF (XYRATO.GT.1.0) THEN
               DY = DY * XYRATO
            ELSE
               DX = DX / XYRATO
               END IF
            XY(2) = XBLC(2) + DY
            AX(1) = XY(1)
            AY(1) = XY(2)
            AX(2) = AX(1)
            AX(3) = AX(1)
            AX(4) = AX(1) - DX
            AX(5) = AX(1) + DX
            AY(2) = AY(1) + DY
            AY(3) = AY(1) - DY
            AY(4) = AY(1)
            AY(5) = AY(1)
            IF (ILITY.NE.3) THEN
               ILITY = 3
               CALL GLTYPE (ILITY, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            CALL PNTPLT (BSYM, AX, AY, XBLC, XTRC, .FALSE.,
     *         DOCOLR, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 120     CONTINUE
C                                       Line plot
      IF (MODERR.EQ.0) THEN
         ILITY = 2
         CALL GLTYPE (ILITY, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         BLNKD = .TRUE.
         DO 140 NN = 1,1024
            XX = XSTART + (NN-1.0) * (XSTOP - XSTART) / 1023.0
            IF (XVAR.EQ.3) XX = XX * 360.0
            XY(1) = XX
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) GO TO 140
C                                       loop for points
            VALUE = DMODEL(1)
            DO 125 I = 1,ORDER
               VALUE = VALUE + DMODEL(I+1) * (XX ** I)
 125           CONTINUE
            XY(2) = VALUE
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(2).GE.XBLC(2)) .AND. (XY(2).LE.XTRC(2))) THEN
C                                       Mark point
               IF (BLNKD) THEN
                  CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                  BLNKD = .FALSE.
               ELSE
                  CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 970
               END IF
 140        CONTINUE
         END IF
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF ((IPLOT.GT.0) .AND. (ABS(IPLOT).LT.NCOUNT)) GO TO 210
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
 210  IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1010 FORMAT (A2,I4,'_')
 1015 FORMAT ('NTONE ',I2)
 1021 FORMAT ('IF ',I2)
 1022 FORMAT ('IF ',I2,' - ',I2)
 1025 FORMAT (9(1PE11.3))
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I3)
 1060 FORMAT ('_Phase tone ',I2)
 1200 FORMAT ('PLTSN:',I9,' points plotted')
 1202 FORMAT ('PLTSN:',I9,' points did not fit')
 1960 FORMAT ('PLTSN: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTSN: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE FITIT (LST, LIF, ANTNO, NV, PLTPTS, DMODEL, IERR)
C-----------------------------------------------------------------------
C   FITIT extracts the data from PLTPTS and calls a least-squares fit
C   routine
C   Inputs:
C      LST      I      Stokes this plot
C      LIF      I      IF this plot
C      ANTNO    I      Antenna number
C      NV       I      Number values
C      PLTPTS   R(*)   Data to plot (NV,*)
C   Output:
C      IERR     I      > 0 => modeel fit failed
C-----------------------------------------------------------------------
      INTEGER   LST, LIF, ANTNO, NV, IERR
      REAL      PLTPTS(NV,*)
      DOUBLE PRECISION DMODEL(*)
C
      INCLUDE 'ELFIT.INC'
C
      INTEGER   NN, NNN, IP, NFIT, NPOINT, I
      DOUBLE PRECISION XX(25000), YY(25000), WT(25000), VARRES, X(4),
     *   Y(4), YMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IP = (LIF-1) * MUMPOL + LST + 2
      I = 0
C                                       fill arrays
      YMIN = 1.E7
      DO 20 NN = 1,NUMPTS(ANTNO)
         NNN = NN - 1 + FANREC(ANTNO)
         IF ((PLTPTS(IP,NNN).NE.FBLANK) .AND. (I.LT.25000)) THEN
            I = I + 1
            YY(I) = PLTPTS(IP,NNN)
            XX(I) = PLTPTS(2,NNN)
            IF (XVAR.EQ.3) XX(I) = XX(I) * 360.0
            YMIN = MIN (YMIN, YY(I))
            WT(I) = 1.0D0
            END IF
 20      CONTINUE
      IF ((APARM(1).GT.0.0) .AND. (I.LT.24997)) THEN
         IF (XVAR.EQ.1) THEN
            XX(I+1) = 0.0
            XX(I+2) = 10.0
            XX(I+3) = 15.0
            YY(I+1) = YMIN
            YY(I+2) = YMIN
            YY(I+3) = YMIN
            WT(I+1) = APARM(1)
            WT(I+2) = APARM(1)
            WT(I+3) = APARM(1)
            I = I + 3
         ELSE IF (XVAR.EQ.2) THEN
            XX(I+1) = 90.0
            XX(I+2) = 80.0
            XX(I+3) = 75.0
            YY(I+1) = YMIN
            YY(I+2) = YMIN
            YY(I+3) = YMIN
            WT(I+1) = APARM(1)
            WT(I+2) = APARM(1)
            WT(I+3) = APARM(1)
            I = I + 3
            END IF
         END IF
C                                       order up fit
      NFIT = ORDER + 1
      NPOINT = I
      CALL DFITPN (XX, YY, WT, NFIT, NPOINT, DMODEL, VARRES, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) ANTNO, LIF, LST, IERR
         CALL MSGWRT (7)
      ELSE
         WRITE (MSGTXT,1025) ANTNO, LIF, LST, VARRES, DMODEL(1)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1026) (DMODEL(I), I = 2,MIN(6,NFIT))
         CALL MSGWRT (5)
         IF (NFIT.GT.6) THEN
            WRITE (MSGTXT,1026) (DMODEL(I), I = 7,NFIT)
            CALL MSGWRT (5)
            END IF
         IF (XVAR.EQ.1) THEN
            X(1) = XXMIN(ANTNO)
            X(2) = XXMAX(ANTNO)
            X(3) = APARM(2)
            X(4) = APARM(3)
         ELSE IF (XVAR.EQ.2) THEN
            X(1) = XXMAX(ANTNO)
            X(2) = XXMIN(ANTNO)
            X(3) = APARM(2)
            X(4) = APARM(3)
         ELSE
            GO TO 999
            END IF
         DO 30 IP = 1,4
            Y(IP) = DMODEL(1)
            DO 25 I = 1,ORDER
               Y(IP) = Y(IP) + DMODEL(I+1) * (X(IP)**I)
 25            CONTINUE
 30         CONTINUE
         WRITE (MSGTXT,1030) X(1), Y(1), X(2), Y(2), Y(2)-Y(1)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1030) X(3), Y(3), X(4), Y(4), Y(4)-Y(3)
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ANTENNA',I4,' IF',I3,' POL',I2,' FITTING ERROR',I3)
 1025 FORMAT ('Antenna',I4,' IF',I3,' Pol',I2,' rms',F10.5,' const',
     *   1PE13.5)
 1026 FORMAT (9X,5(1PE11.3))
 1030 FORMAT ('At X=',F5.1,' Y is',F7.1,3X,'At X=',F5.1,' Y is',F7.1,
     *   ' diff',F9.3)
      end
      SUBROUTINE XCALC (XVARIB, XSOU)
C-----------------------------------------------------------------------
C  Routine to use the source and antenna geometry information in order
C  to return the requested value of the x-variable against which the
C  data is to be plotted.
C  Inputs (in common):
C    XVAR        I       Type of variable to calculate:
C                        1 = source zenith angle
C                        2 = source elevation
C                        3 = HA
C                        5 = Parallactic angle
C                        6 = Azimuth
C  Output:
C    XVARIB      R       Value of requested variable
C                        Time, LST (days)
C                        Elevation, HA (degrees)
C    XSOU        I       source number
C----------------------------------------------------------------------
      REAL    XVARIB, XSOU
C
      REAL    HA, EL, PA, AZ
      INTEGER IERR, ISLUN, CSOU, I
      DOUBLE PRECISION LST, TIME
      INCLUDE 'ELFIT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA ISLUN /25/
C-----------------------------------------------------------------------
      CSOU = GNRECI(SOUKOL)
      XSOU = CSOU
C                                      Time
      IF (KOLTYP(CLTIME).EQ.1) THEN
         TIME = GNRECD(TIMKOL)
      ELSE
         TIME = GNREC(TIMKOL)
         END IF
C
C                                      Get source parameters
      IF ((OSOU.EQ.-1) .OR. (CSOU.NE.OSOU)) THEN
         I = MSGSUP
         MSGSUP = 32000
         CALL GETSOU (CSOU, DISKIN, CNOIN, CATBLK, ISLUN, IERR)
         MSGSUP = I
         OSOU = CSOU
         END IF
C                                      Geometry parameters
      CALL ANTGEO (IANT, TIME, HA, EL, LST, PA, AZ)
      IF (XVAR.EQ.1) THEN
         XVARIB = 90.0 - EL * RAD2DG
      ELSE IF (XVAR.EQ.2) THEN
         XVARIB = EL * RAD2DG
      ELSE IF (XVAR.EQ.3) THEN
         XVARIB = HA / TWOPI
      ELSE IF (XVAR.EQ.4) THEN
         XVARIB = LST / TWOPI
      ELSE IF (XVAR.EQ.5) THEN
         XVARIB = PA * RAD2DG
      ELSE IF (XVAR.EQ.6) THEN
         XVARIB = AZ * RAD2DG
         IF (XVARIB.LT.0.0) XVARIB = XVARIB + 360.0
         END IF
      IF (EL.LT.0.0) XVARIB = FBLANK
C
 999  RETURN
      END
      SUBROUTINE ANTGEO (ANTNO, TIME, HA, EL, ANTLST, PA, AZ)
C-----------------------------------------------------------------------
C   Subroutine to compute the apparent source elevations based on source
C   and antenna coordinates in common.  The routines GETANT and GETSOU
C   should be called before this routine to put the correct values in
C   the relevant commons.
C   Inputs:
C      ANTNO      I    Antenna number
C      TIME       D    Current data time (days).
C   Input from common:
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      STNLAT     D(*) Antenna latitude (rad).
C      STNLON     D(*) Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      HA         R    Source hour angle (rad)
C      EL         R    Source elevation (rad)
C      ANTLST     D    Antenna LST (rad)
C      PA         R    Parallactic angle (rad)
C      AZ         R    Azimuth (rad)
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME, ANTLST
      REAL      HA, EL, PA, AZ
C
      LOGICAL   ISVLA
      DOUBLE PRECISION    HRANG, ARLONG, ARLAT, DARG, DRA, DDEC, DARG2,
     *   DAZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
C                                       Antenna LST - wrong
C     ANTLST = GSTIAT + STNLON(ANTNO) * 1.002738D0 + TIME * ROTIAT
C                                       Antenna LST
      ANTLST = GSTIAT + STNLON(ANTNO) + TIME * ROTIAT
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAAPP
      DDEC = DECAPP
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAEPO
         DDEC = DECEPO
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
      HRANG = DMOD (HRANG, TWOPI)
      IF (HRANG.GT.PI) HRANG = HRANG - TWOPI
      IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
      HA = HRANG
C                                       Elevation angle
      DARG = SIN (STNLAT(ANTNO)) * SIN (DDEC) + COS (STNLAT(ANTNO))
     *   * COS (DDEC) * COS (HRANG)
      EL = (PI/2.0D0 - ACOS (DARG))
C                                       AZ = ATAN2(SD*CL - CD*SL*CH,
C                                       CD*SH)
      DARG = SIN (DDEC) * COS (STNLAT(ANTNO)) -
     *       COS (DDEC) * SIN (STNLAT(ANTNO)) * COS(HRANG)
      DARG2 = COS (DDEC) * SIN (HRANG)
      DAZ = ATAN2 (DARG, DARG2)
      DAZ = MOD (DAZ - PI/2.0D0, TWOPI)
      IF (DAZ.LT.0.0D0) DAZ = DAZ + TWOPI
      AZ = DAZ
C                                       Is this the VLA?
      ISVLA = (ABS (CNTRX + 1.601162D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRY + 5.042003D6) .LE. 10.0D0) .AND.
     *   (ABS (CNTRZ - 3.554915D6) .LE. 10.0D0)
C                                       All VLA antennas have the same
C                                       parallactic angle.
      PA = 0.0
      IF (ISVLA) THEN
         ARLONG = ATAN2 (CNTRY, CNTRX)
         ARLAT = ASIN (CNTRZ / SQRT (CNTRX*CNTRX + CNTRY*CNTRY +
     *      CNTRZ*CNTRZ))
      ELSE
         ARLONG = STNLON(ANTNO)
         ARLAT = STNLAT(ANTNO)
         END IF
C                                       Dont compute Equatorial or space
      IF ((MNTYP(ANTNO).NE.1) .AND. (MNTYP(ANTNO).NE.2)) THEN
C                                       Antenna LST
         HRANG = GSTIAT + ARLONG + TIME * ROTIAT
C                                       Hour angle
         HRANG = HRANG - RAAPP
C                                       Parallactic angle
         PA = ATAN2 (COS (ARLAT) * SIN (HRANG),
     *     (SIN (ARLAT) * COS (DECAPP) -
     *     COS (ARLAT) * SIN (DECAPP) * COS(HRANG)))
C                                       EW-mount
         IF (MNTYP(ANTNO).EQ.3) THEN
            PA =  ATAN2 (COS(HRANG), SIN(HRANG)*SIN(DECAPP))
C                                       Right  Nasmyth
         ELSE IF (MNTYP(ANTNO).EQ.4) THEN
            PA = PA + EL
C                                       Left Nasmyth
         ELSE IF (MNTYP(ANTNO).EQ.5) THEN
            PA = PA -EL
            END IF
         END IF
C
 999  RETURN
      END

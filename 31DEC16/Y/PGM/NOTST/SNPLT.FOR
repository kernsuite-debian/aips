LOCAL INCLUDE 'SNPLT.INC'
      INTEGER   NCODE
      PARAMETER (NCODE=27)
C                                       Local include for SNPLT
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XQUAL, XTIME(8), XBAND, XFREQ,
     *   XFQID, XSUBA, XBIF, XEIF, XANT(50), PIXR(2), XNCOU, XXINC,
     *   APARM(10), BPARM(10), CPARM(10), XDO3C, XPCNUM, ABSICA, XSYM,
     *   FACTOR, XDOBL, CUTOFF, XSCAN, XLABEL, XDOTV, XGRCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE, XXSOUR(4,30), XXSTOK,
     *   XOPTY, XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2, XSOUR(30)*16, XSTOK*4,
     *   OPTYPE*4, OPCODE*4
C                                       Program info
C
      INTEGER   MXSCAN
      PARAMETER (MXSCAN=5000)
C
      REAL      TSTART, TSTOP, TINT, XYSCL(2), XYOFF(2), YYMX(10),
     *   YYMN(10), XMX, XMN, XMXW, XMNW, GMMOD, RATFAC(MAXIF), SELBAN,
     *   XSTART, XSTOP, CHOUT(4), XXMIN(MAXANT), XXMAX(MAXANT),
     *   YYMIN(2,MAXIF,MAXANT,10), TSCAN(MXSCAN), PPMAX(2,MAXIF,MAXANT),
     *   YYMAX(2,MAXIF,MAXANT,10), PPMIN(2,MAXIF,MAXANT), PRAN(2,2),
     *   DO3COL, TCAL(2,MAXIF,MAXANT), CSMIN, CSMAX
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, ANTS(50), NCOUNT,
     *   ICODES(10), NCODES, NPARMS, NID, SID(500), NANTSL, NPLOTS,
     *   SUMSTK, ISTOK, FRQSEL, XINC, GRCHN, TVCHN, TVCORN(4), XVAR,
     *   ISOU, OSOU, IANT, EIF, ITPLOT, ITVER, PCNUM, LABEL, SUBARR,
     *   MUMPAR, MUMPOL, MUMIF, MUMANT, NTONE, NUMPTS(MAXANT), ISYM,
     *   BSYM, NANREC(MAXANT), FANREC(MAXANT), NOSCAN, STRANS(MXSCAN)
      LOGICAL   DOAWNT, DOTV, NNODAT, DOLINE, SWAP
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
     *   XEIF, XANT, PIXR, XNCOU, XXINC, XOPTY, XOPCOD, APARM, BPARM,
     *   CPARM, XDO3C, XPCNUM, ABSICA, XSYM, FACTOR, XDOBL, CUTOFF,
     *   XSCAN, XLABEL, XDOTV, XGRCH
      COMMON /VPARM/ SEQIN, DISKIN, CNOIN, IVER, BIF, EIF, ANTS, NCOUNT,
     *   ICODES, NCODES, NPARMS, GRCHN, TVCHN, TVCORN, XVAR, ISOU, OSOU,
     *   IANT, ITPLOT, ITVER, PCNUM, DOTV, NNODAT, LABEL, CHOUT, DO3COL,
     *   DOLINE, NOSCAN, TSCAN, STRANS, SWAP
      COMMON /VGNCOM/ SELFRQ,
     *   TSTART, TSTOP, TINT, XYSCL, XYOFF, SELBAN, XMX, XMN, XMXW,
     *   XMNW, XSTART, XSTOP, GMMOD, RATFAC, NID, SID, NANTSL, PRAN,
     *   NPLOTS, DOAWNT, ISTOK, SUMSTK, FRQSEL, XINC,
     *   SUBARR, MUMPAR, MUMPOL, MUMIF, MUMANT, NUMPTS, NTONE,
     *   XXMIN, XXMAX, YYMIN, YYMAX, PPMIN, PPMAX, YYMX, YYMN, ISYM,
     *   BSYM, TCAL, NANREC, FANREC, CSMIN, CSMAX
      COMMON /VGNCHR/ NAMEIN, CLAIN, TYPE, XSOUR, XSTOK, OPTYPE,
     *   OPCODE
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
C                                                          End SNPLT
LOCAL END
      PROGRAM SNPLT
C-----------------------------------------------------------------------
C! Plots data from a SN, TY, PC or CL table
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2005, 2007-2012, 2014-2015
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
C   SNPLT plots SN or CL extension files. A 'PL' extension file is made
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
C                 'R' = RCP, 'L' = LCP, 'DIFF' = difference
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
C      OPCODE.....'IFDF' = IF difference, 'PLIF' = all IF's for
C                 specified antennas, ' ' = just OPTYPE
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
      INCLUDE 'SNPLT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'SNPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNPIN (PRGN, NROWS, IRET)
      MUMANT = MAX (1, MUMANT)
      MVAL = 2 + MUMPAR*MUMPOL*MUMIF*NCODES
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
C   Gets the inputs parameters for SNPLT.
C   Inputs:
C      PRGN    C*6  Program name
C   Output in common:
C      SUMSTK  I    Selected Stokes 0=both, 1=R, 2=L, 4=difference
C   Output:
C      IERR    I    Error code: 0 => ok
C      ISTOK   I    1 = R, 2 = L
C      ICODE   I    1='PHAS', 2='AMP ', 3='DELA', 4='RATE', 5='TSYS',
C                   6='SUM ', 7='DOPL', 8='SNR', 9='MDEL', 10='TANT',
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
      INCLUDE 'SNPLT.INC'
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
      DATA CODE /'PHAS', 'AMP ', 'DELA', 'RATE', 'TSYS', 'SUM ', 'DOPL',
     *   'SNR ', 'MDEL', 'TANT', 'ATM ', 'GEO ', 'CCAL', 'DDLY', 'REAL',
     *   'IMAG', 'IFR ', 'PDIF', 'PSUM', 'PGN ', 'PON ', 'POFF', 'PSYS',
     *   'PDGN', 'PSGN', 'POWR', 'PODB'/
      DATA TPLOT /'ALIF', 'ALST', 'ALSI', 'IFDF', 'IFRA'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OSOU = -1
      NPARMS = 242
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
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
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
      DO3COL = XDO3C
      IF (XDO3C.GT.1.5) DO3COL = -1.
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
      XINC = IROUND (XXINC)
      IF (XINC.LE.0) XINC = 1
      PCNUM = IROUND (XPCNUM)
      IF (PCNUM.LE.0) PCNUM = 1
      XXINC = XINC
      XVAR = IROUND (ABSICA)
      DOLINE = FACTOR.LT.0.0
      FACTOR = ABS (FACTOR)
      IF ((.NOT.DOLINE) .AND. (FACTOR.LT.0.1)) FACTOR = 1.0
      IF (FACTOR.GT.10.0) FACTOR = 1.0
      IF ((XVAR.LE.0) .OR. (XVAR.GT.6)) XVAR = 1
C                                       plot types
      IF (OPTYPE.EQ.'MULT') THEN
         NCODES = 0
         DO 25 I = 1,10
            ICODE = IROUND (APARM(I))
            IF ((ICODE.LT.1) .OR. (ICODE.GT.NCODE) .OR. (ICODE.EQ.6))
     *         GO TO 26
            NCODES = NCODES + 1
            ICODES(NCODES) = ICODE
 25         CONTINUE
 26      IF (NCODES.LE.0) THEN
            MSGTXT = 'OPTYPE ''MULT'' HAS NO VALID TYPE CODES'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 999
            END IF
      ELSE
         ICODE = 1
         DO 30 I = 1,NCODE
            IF (OPTYPE.EQ.CODE(I)) ICODE = I
 30         CONTINUE
         CALL CHR2H (4, CODE(ICODE), 1, XOPTY)
         NCODES = 1
         ICODES(1) = ICODE
         BPARM(1) = PIXR(1)
         CPARM(1) = PIXR(2)
         END IF
      MUMPAR = 1
      IF (ICODE.EQ.6) MUMPAR = 2
      ITPLOT = 0
      DO 35 I = 1,NTPLT
         IF (OPCODE.EQ.TPLOT(I)) ITPLOT = I
 35      CONTINUE
      IF (ITPLOT.GT.0) CALL CHR2H (4, TPLOT(ITPLOT), 1, XOPCOD)
      IF ((ITPLOT.LE.0) .OR. (ITPLOT.GT.3)) DO3COL = -1.0
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
      IF ((ITPLOT.GE.4) .AND. (EIF.LE.BIF)) THEN
         MSGTXT = 'OPCODE IFDF/IFRA REQUESTED WITH 1 IF - CONTINUING'
         CALL MSGWRT (6)
         ITPLOT = 0
         OPCODE = ' '
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
      ELSE IF (XSTOK.EQ.'DIFF') THEN
         ISTOK = 1
         SUMSTK = 3
      ELSE IF (XSTOK.EQ.'RATO') THEN
         ISTOK = 1
         SUMSTK = 4
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
            IF (ICODES(1).NE.6) MUMPOL = 2
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
            IF (ICODES(1).NE.6) MUMPOL = 2
            END IF
         END IF
      CALL CHR2H (4, XSTOK, 1, XXSTOK)
      IF ((SUMSTK.GE.3) .AND. (ITPLOT.GE.4)) THEN
         MSGTXT = 'CANNOT DIFFERENCE POLARIZATIONS AND IFS TOGETHER'
         CALL MSGWRT (6)
         ITPLOT = 0
         END IF
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
      IF (ITPLOT.GE.4) MUMIF = 1
      IF (ICODES(1).EQ.6) MUMIF = 1
      XBIF = BIF
      XEIF = EIF
      CSMAX = -100000
      CSMIN = 1000000
      CALL RFILL (MAXANT, 1.E5, XXMIN)
      CALL RFILL (MAXANT, -1.E5, XXMAX)
      I = 2 * MAXIF * MAXANT
      CALL RFILL (I, 1.E8, PPMIN)
      CALL RFILL (I, -1.E8, PPMAX)
      I = I * 10
      CALL RFILL (I, 1.E8, YYMIN)
      CALL RFILL (I, -1.E8, YYMAX)
      CALL RFILL (10, -1.E8, YYMX)
      CALL RFILL (10,  1.E8, YYMN)
      IF ((XVAR.EQ.1) .AND. (TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         CALL RFILL (MAXANT, TSTART, XXMIN)
         CALL RFILL (MAXANT, TSTOP, XXMAX)
         END IF
      IF ((XSCAN.GT.0.0) .AND. (XVAR.EQ.1)) THEN
         LUN = 25
         CALL GETNX (LUN, DISKIN, CNOIN, CATBLK, SUBARR, BUFFER, NOSCAN,
     *      TSCAN)
      ELSE
         NOSCAN = 0
         END IF
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
      INCLUDE 'SNPLT.INC'
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
         DO 110 I = 1,NCODES
            IF ((ICODES(I).LT.18) .OR. (ICODES(I).GT.25)) THEN
               MSGTXT = 'DATA OF REQUESTED TYPE NOT IN SY TABLES'
               IERR = 5
               GO TO 980
               END IF
 110        CONTINUE
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
      IF (TYPE.EQ.'SY') THEN
         COLTAB(13) = 'POWER DIF1'
         COLTAB(14) = 'POWER SUM1'
         COLTAB(17) = 'POST GAIN1'
         COLTAB(26) = 'POWER DIF2'
         COLTAB(27) = 'POWER SUM2'
         COLTAB(30) = 'POST GAIN2'
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
      IF ((ISTOK.EQ.ABS (ICOR0)) .OR. (ISTOK.EQ.ABS (ICOR0+4)) .OR.
     *   (SUMSTK.GE.3)) THEN
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
         IF (ITPLOT.GE.4) THEN
            MBKOL(3) = MB1KOL
            REKOL(3) = RE1KOL + ((EIF-1) * NTONE) + PCNUM - 1
            IMKOL(3) = IM1KOL + ((EIF-1) * NTONE) + PCNUM - 1
            DLKOL(3) = DL1KOL + EIF - 1
            RAKOL(3) = RA1KOL + EIF - 1
            WTKOL(3) = WT1KOL + EIF - 1
            RFKOL(3) = RF1KOL + EIF - 1
            TSKOL(3) = TS1KOL + EIF - 1
            TAKOL(3) = TA1KOL + EIF - 1
            CKKOL(3) = CK1KOL
            DCKOL(3) = DC1KOL
            DSKOL(3) = DS1KOL
            DDKOL(3) = DD1KOL
            STKOL(3) = ST1KOL + EIF - 1
            END IF
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
         IF (ITPLOT.GE.4) THEN
            MBKOL(3) = MB2KOL
            REKOL(3) = RE2KOL + ((EIF-1) * NTONE) + PCNUM - 1
            IMKOL(3) = IM2KOL + ((EIF-1) * NTONE) + PCNUM - 1
            DLKOL(3) = DL2KOL + EIF - 1
            RAKOL(3) = RA2KOL + EIF - 1
            WTKOL(3) = WT2KOL + EIF - 1
            RFKOL(3) = RF2KOL + EIF - 1
            TSKOL(3) = TS2KOL + EIF - 1
            TAKOL(3) = TA2KOL + EIF - 1
            CKKOL(3) = CK2KOL
            DCKOL(3) = DC2KOL
            DSKOL(3) = DS2KOL
            DDKOL(3) = DD2KOL
            STKOL(3) = ST2KOL + EIF - 1
            END IF
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
      IF (ITPLOT.GE.4) THEN
         MBKOL(4) = MB2KOL
         REKOL(4) = RE2KOL + ((EIF-1) * NTONE) + PCNUM - 1
         IMKOL(4) = IM2KOL + ((EIF-1) * NTONE) + PCNUM - 1
         DLKOL(4) = DL2KOL + EIF - 1
         RAKOL(4) = RA2KOL + EIF - 1
         WTKOL(4) = WT2KOL + EIF - 1
         RFKOL(4) = RF2KOL + EIF - 1
         TSKOL(4) = TS2KOL + EIF - 1
         TAKOL(4) = TA2KOL + EIF - 1
         CKKOL(4) = CK2KOL
         DCKOL(4) = DC2KOL
         DSKOL(4) = DS2KOL
         DDKOL(4) = DD2KOL
         STKOL(4) = ST2KOL + EIF - 1
         END IF
C                                       Phase, amplitude, summary
      IF ((ISTYPE(1)) .OR. (ISTYPE(2)) .OR. (ISTYPE(6))
     *      .OR. (ISTYPE(15)) .OR. (ISTYPE(16))
     *      .OR. (ISTYPE(24)) .OR. (ISTYPE(25))) THEN
         IF ((REKOL(1).LT.0) .AND. (IMKOL(1).LT.0)) GO TO 500
         IF (((SUMSTK.GE.3) .OR. (MUMPOL.EQ.2)) .AND. ((REKOL(2).LT.0)
     *      .OR. (IMKOL(2).LT.0))) GO TO 500
         IF (ISTYPE(1)) THEN
            SUMSTK = MIN (SUMSTK, 3)
            ITPLOT = MIN (ITPLOT, 4)
            END IF
         END IF
C                                       Singleband Delay
      IF (ISTYPE(3)) THEN
         IF (DLKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (DLKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (DLKOL(2).LT.0)) GO TO 500
         SUMSTK = MIN (SUMSTK, 3)
         ITPLOT = MIN (ITPLOT, 4)
         END IF
C                                       Rate
      IF (ISTYPE(4)) THEN
         IF (RAKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (RAKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (RAKOL(2).LT.0)) GO TO 500
         SUMSTK = MIN (SUMSTK, 3)
         ITPLOT = MIN (ITPLOT, 4)
         END IF
C                                       System temperature
      IF (ISTYPE(5)) THEN
         IF (TSKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (TSKOL(2).LT.0)) GO TO 500
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
         IF (ITPLOT.GE.4) ITPLOT = 0
         END IF
C                                       SNR
      IF (ISTYPE(8)) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (WTKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         END IF
C                                       MDEL (multiband delay)
      IF (ISTYPE(9)) THEN
         IF (MBKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (MBKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (MBKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       MDEL (multiband delay)
      IF (ISTYPE(9)) THEN
         IF (MBKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (MBKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (MBKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       IF Antenna Temp
      IF (ISTYPE(10)) THEN
         IF (TAKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (TAKOL(2).LT.0)) GO TO 500
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
         IF (ITPLOT.GE.4) ITPLOT = 0
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
         IF (ITPLOT.GE.4) ITPLOT = 0
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
         IF (ITPLOT.GE.4) ITPLOT = 0
         END IF
C                                       DDLY (dispersive delay)
      IF (ISTYPE(14)) THEN
         IF (DSKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (DSKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (DSKOL(2).LT.0)) GO TO 500
C                                       Only 1 per poln
         BIF = 1
         EIF = 1
         CUTOFF = -1000.0
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
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
         IF (ITPLOT.GE.4) ITPLOT = 0
         END IF
C                                       SY Pdif
      IF (ISTYPE(18)) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (REKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       SY Psum
      IF (ISTYPE(19)) THEN
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       SY post gain
      IF (ISTYPE(20)) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (WTKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       SY Pon and Poff
      IF ((ISTYPE(21)) .OR. (ISTYPE(22))) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (REKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (WTKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
C                                       SY TSYS
      IF (ISTYPE(23)) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (REKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((SUMSTK.GE.3) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF (ITPLOT.GE.4) ITPLOT = 0
         SUMSTK = MIN (SUMSTK, 3)
         END IF
      IF ((ITPLOT.LE.0) .OR. (ITPLOT.GE.4)) DO3COL = -1.0
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
      INTEGER   I
      INCLUDE 'SNPLT.INC'
C-----------------------------------------------------------------------
      ISTYPE = .FALSE.
      DO 10 I = 1,NCODES
         IF (ICODES(I).EQ.FTYPE) ISTYPE = .TRUE.
 10      CONTINUE
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
      INCLUDE 'SNPLT.INC'
      LOGICAL   NODATA, OKAY
      INTEGER   I, NP, IFNUM, SCNT(MXSCAN)
      REAL      TB, TE, GTIME, XVARIB, CSOU
      REAL      VALUE(2*MAXIF,10)
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
      IF ((XVAR.EQ.1) .AND. (TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TB = TSTART
         TE = TSTOP
         END IF
      XMX = TE
      XMN = TB
      XMXW = TE
      XMNW = TB
C                                       Loop thru data
      TINT = -1.0
      IF (INTKOL.LE.0) TINT = 10.0 / 86400.0
      NP = MUMPOL * MUMIF
      DO 100 ICLRNO = 1,NCLINR,XINC
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
               IF ((SUMSTK.EQ.0) .OR. (SUMSTK.GE.3)) THEN
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
      INCLUDE 'SNPLT.INC'
      REAL      VALUE(2*MAXIF,10)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NODATA = .TRUE.
      TB = 1.0E5
      TE = -1.0E5
      IF ((XVAR.EQ.1) .AND. (TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TB = TSTART
         TE = TSTOP
         END IF
      XMX = TE
      XMN = TB
      XMXW = TE
      XMNW = TB
C                                       Loop thru data
      TINT = -1.0
      IF (INTKOL.LE.0) TINT = 10.0 / 86400.0
      NP = MUMPOL * MUMIF
      DO 100 ICLRNO = 1,NCLINR,XINC
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
               IF ((SUMSTK.EQ.0) .OR. (SUMSTK.GE.3)) THEN
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
            DO 81 I = 1,NCODES
               CALL RCOPY (NP, VALUE(1,I), PLTPTS(KK,NN))
               KK = KK + NP
 81            CONTINUE
            IF (ICODES(1).EQ.6) CALL RCOPY (NP, VALUE(1,2),
     *         PLTPTS(KK,NN))
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
            DO 95 KK = 1,NCODES
               IP = 0
               DO 90 IIF = 1,MUMIF
                  DO 85 IIS = 1,MUMPOL
                     IP = IP + 1
                     IF (VALUE(IP,KK).NE.FBLANK) THEN
                        YYMX(KK) = MAX (YYMX(KK), VALUE(IP,KK))
                        YYMN(KK) = MIN (YYMN(KK), VALUE(IP,KK))
                        IF (ICODES(KK).EQ.1) THEN
                           IF (VALUE(IP,KK).LE.-180.0) THEN
                              VALUE(IP,KK) = VALUE(IP,KK) + 360.
                           ELSE IF (VALUE(IP,KK).GT.180.) THEN
                              VALUE(IP,KK) = VALUE(IP,KK) - 360.
                              END IF
                           YYMIN(IIS,IIF,IANT,KK) = MIN (VALUE(IP,KK),
     *                        YYMIN(IIS,IIF,IANT,KK))
                           YYMAX(IIS,IIF,IANT,KK) = MAX (VALUE(IP,KK),
     *                        YYMAX(IIS,IIF,IANT,KK))
                           IF (VALUE(IP,KK).LT.0.0) VALUE(IP,KK) =
     *                        VALUE(IP,KK) + 360.0
                           PPMIN(IIS,IIF,IANT) = MIN (VALUE(IP,KK),
     *                        PPMIN(IIS,IIF,IANT))
                           PPMAX(IIS,IIF,IANT) = MAX (VALUE(IP,KK),
     *                        PPMAX(IIS,IIF,IANT))
                        ELSE IF (ICODES(KK).EQ.6) THEN
                           YYMIN(1,1,1,KK) = MIN (VALUE(IP,KK),
     *                        YYMIN(1,1,1,KK))
                           YYMAX(1,1,1,KK) = MAX (VALUE(IP,KK),
     *                        YYMAX(1,1,1,KK))
                           IF (VALUE(IP,2).LE.-180.0) THEN
                              VALUE(IP,2) = VALUE(IP,2) + 360.
                           ELSE IF (VALUE(IP,2).GT.180.) THEN
                              VALUE(IP,2) = VALUE(IP,2) - 360.
                              END IF
                           YYMIN(2,1,1,KK) = MIN (VALUE(IP,2),
     *                        YYMIN(2,1,1,KK))
                           YYMAX(2,1,1,KK) = MAX (VALUE(IP,2),
     *                        YYMAX(2,1,1,KK))
                           IF (VALUE(IP,2).LT.0.0) VALUE(IP,2) =
     *                        VALUE(IP,2) + 360.
                           PPMIN(1,1,1) = MIN (VALUE(IP,2),
     *                        PPMIN(1,1,1))
                           PPMAX(1,1,1) = MAX (VALUE(IP,2),
     *                        PPMAX(1,1,1))
                        ELSE
                           YYMIN(IIS,IIF,IANT,KK) = MIN (VALUE(IP,KK),
     *                        YYMIN(IIS,IIF,IANT,KK))
                           YYMAX(IIS,IIF,IANT,KK) = MAX (VALUE(IP,KK),
     *                        YYMAX(IIS,IIF,IANT,KK))
                           END IF
                        END IF
 85                  CONTINUE
 90               CONTINUE
 95            CONTINUE
            END IF
 100     CONTINUE
C                                       reset max min on fixed scale
      DO 125 KK = 1,NCODES
         IF (BPARM(KK).LT.CPARM(KK)) THEN
            YYMX(KK) = CPARM(KK)
            YYMN(KK) = BPARM(KK)
            DO 120 IANT = 1,MUMANT
               DO 115 IIF = 1,MUMIF
                  DO 110 IIS = 1,MUMPOL
                     IF (YYMAX(IIS,IIF,IANT,KK).GE.
     *                  YYMIN(IIS,IIF,IANT,KK)) THEN
                        YYMAX(IIS,IIF,IANT,KK) = CPARM(KK)
                        YYMIN(IIS,IIF,IANT,KK) = BPARM(KK)
                        IF (ICODES(KK).EQ.1) THEN
                           PPMAX(IIS,IIF,IANT) = CPARM(KK)
                           PPMIN(IIS,IIF,IANT) = BPARM(KK)
                           END IF
                        END IF
 110                 CONTINUE
 115              CONTINUE
 120           CONTINUE
            END IF
         IF (ICODES(KK).EQ.6) THEN
            IF (YYMAX(2,1,1,KK)-YYMIN(2,1,1,KK).GT.
     *         PPMAX(1,1,1)-PPMIN(1,1,1)) THEN
               YYMAX(2,1,1,KK) = PPMAX(1,1,1)
               YYMIN(2,1,1,KK) = PPMIN(1,1,1)
               END IF
            END IF
 125     CONTINUE
C                                       Set actual X range
      SWAP = .FALSE.
      IF (XVAR.EQ.1) THEN
         XSTART = TB
         XSTOP = TE
      ELSE IF (XVAR.EQ.6) THEN
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
C                                       Check for no data
      IF (NODATA) THEN
         IERR = 6
         MSGTXT = 'NO DATA SELECTED'
         GO TO 990
         END IF
C                                       check and set scaling
      IF ((XVAR.EQ.1) .AND. (TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TMAX = (XSTOP + 0.03 * (XSTOP - XSTART)) * 360.0
         TMIN = (XSTART- 0.03 * (XSTOP - XSTART)) * 360.0
      ELSE IF ((XVAR.EQ.1) .OR. (XVAR.EQ.3) .OR. (XVAR.EQ.4)) THEN
         TMAX = (XSTOP + 0.1 * (XSTOP - XSTART)) * 360.0
         TMIN = (XSTART- 0.1 * (XSTOP - XSTART)) * 360.0
      ELSE
         TMAX = (XSTOP + 0.1 * (XSTOP - XSTART))
         TMIN = (XSTART- 0.1 * (XSTOP - XSTART))
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
      SUBROUTINE GETSCL (KK, LST1, LST2, LIF1, LIF2, LANT, DOIT)
C-----------------------------------------------------------------------
C   GETSCL converts a number of max/min's to a scale
C   Inputs:
C      KK      I   parameter number
C      LST1    I   1st Stokes
C      LST2    I   Upper Stokes
C      LIF1    I   1st IF number
C      LIF2    I   1st IF number
C      LANT    I   Antenna number
C   Output:
C      DOIT    L      There were valid values
C   Output in common
C      XYSCL   R(2)   Scaling - only 2nd one changed
C      XYOFF   R(2)   Offset  - only second one changed
C-----------------------------------------------------------------------
      INTEGER   KK, LST1, LST2, LIF1, LIF2, LANT
      LOGICAL   DOIT
C
      INCLUDE 'SNPLT.INC'
      INTEGER   IST, IIF
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
      DO 30 IIF = LIF1,LIF2
         DO 20 IST = LST1,LST2
            IF (YYMAX(IST,IIF,LANT,KK).GE.YYMIN(IST,IIF,LANT,KK)) THEN
               DOIT = .TRUE.
               YMX = MAX (YMX, YYMAX(IST,IIF,LANT,KK))
               YMN = MIN (YMN, YYMIN(IST,IIF,LANT,KK))
               IF (ICODES(KK).EQ.1) THEN
                  PMX = MAX (PMX, PPMAX(IST,IIF,LANT))
                  PMN = MIN (PMN, PPMIN(IST,IIF,LANT))
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
      IF ((ICODES(KK).EQ.1) .AND. (PMX-PMN.LT.YMX-YMN)) THEN
         YMX = PMX
         YMN = PMN
         END IF
      SIZEY = 1000.0 / NCOUNT
      TMAX = YMX + 0.1 * (YMX - YMN)
      TMIN = YMN - 0.1 * (YMX - YMN)
      IF (ABS (TMAX-TMIN) .LT. TOLER(ICODES(KK))) THEN
         TMAX = TMAX + TOLER(ICODES(KK))
         TMIN = TMIN - TOLER(ICODES(KK))
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
C      SUMSTK   I     Selected Stokes 0=both, 1=R, 2=L, 3=Difference
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R(*)   Table value, magic value blanked (amp on ICODE 6)
C      XVARIB   R      Value of associated x-axis variable
C      CSOU     R      source number
C      OKAY     L      Some values are good
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      VALUE(2*MAXIF,*), XVARIB, CSOU
      LOGICAL   OKAY
C
      INTEGER   IIS, IIF, IP1, IP2, LP, JP1, JP2, KP1, KP2, KK, ICODE,
     *   JIF
      REAL      AVAL, PVAL, AM, PH, V, S, TC
      LOGICAL   T, GOT, DORATO, DODIFF
      INCLUDE 'SNPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       In case the data is bad
      LP = MUMPOL * MUMIF
      CALL XCALC (XVARIB, CSOU)
      DO 900 KK = 1,NCODES
         ICODE = ICODES(KK)
         CALL RFILL (LP, FBLANK, VALUE(1,KK))
         DODIFF = (ITPLOT.EQ.4) .OR. (SUMSTK.EQ.3)
         DORATO = (ITPLOT.EQ.5) .OR. (SUMSTK.EQ.4)
C                                       Select data type
C                                       Phase (deg)
         IF (ICODE.EQ.1) THEN
            DO 110 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = REKOL(IIS) - NTONE
               JP1 = IMKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  IP2 = REKOL(IIS+2) - NTONE
                  JP2 = IMKOL(IIS+2) - NTONE
               ELSE
                  IP2 = REKOL(2) - NTONE
                  JP2 = IMKOL(2) - NTONE
                  END IF
               DO 105 IIF = 1,MUMIF
                  IP1 = IP1 + NTONE
                  JP1 = JP1 + NTONE
                  IP2 =  IP2 + NTONE
                  JP2 = JP2 + NTONE
                  LP = LP + MUMPOL
                  IF ((GNREC(IP1).NE.FBLANK) .AND.
     *               (GNREC(JP1).NE.FBLANK)) THEN
                     IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) THEN
                           VALUE(LP,KK) = 57.296 *
     *                        (ATAN2 (GNREC(JP1), GNREC(IP1) + 1.0E-20)
     *                        -ATAN2 (GNREC(JP2), GNREC(IP2) + 1.0E-20))
                           IF (VALUE(LP,KK).LE.-180.0) VALUE(LP,KK) =
     *                        VALUE(LP,KK) + 360.0
                           IF (VALUE(LP,KK).GT.+180.0) VALUE(LP,KK) =
     *                        VALUE(LP,KK) - 360.0
                           END IF
                     ELSE
                        VALUE(LP,KK) = 57.296 *
     *                     ATAN2 (GNREC(JP1), GNREC(IP1) + 1.0E-20)
                        END IF
                     END IF
 105              CONTINUE
 110           CONTINUE
C                                       Amplitude
         ELSE IF (ICODE.EQ.2) THEN
            DO 130 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = REKOL(IIS) - NTONE
               JP1 = IMKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  IP2 = REKOL(IIS+2) - NTONE
                  JP2 = IMKOL(IIS+2) - NTONE
               ELSE
                  IP2 = REKOL(2) - NTONE
                  JP2 = IMKOL(2) - NTONE
                  END IF
               DO 125 IIF = 1,MUMIF
                  IP1 = IP1 + NTONE
                  JP1 = JP1 + NTONE
                  IP2 =  IP2 + NTONE
                  JP2 = JP2 + NTONE
                  LP = LP + MUMPOL
                  IF ((GNREC(IP1).NE.FBLANK) .AND.
     *               (GNREC(JP1).NE.FBLANK)) THEN
                     IF (DORATO) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) VALUE(LP,KK) =
     *                     SQRT ((GNREC(IP1)**2 + GNREC(JP1)**2) /
     *                     (GNREC(IP2)**2 + GNREC(JP2)**2+1.E-20))
                     ELSE IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) VALUE(LP,KK) = GMMOD
     *                     * SQRT ((GNREC(IP1)-GNREC(IP2))**2  +
     *                     (GNREC(JP1)-GNREC(JP2))**2)
                     ELSE
                        VALUE(LP,KK) = GMMOD *
     *                     SQRT ((GNREC(IP1)**2) + (GNREC(JP1)**2))
                        END IF
                     END IF
 125              CONTINUE
 130           CONTINUE
C                                       Delay (sec)
         ELSE IF (ICODE.EQ.3) THEN
            DO 150 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = DLKOL(IIS) - 1
               IF (ITPLOT.GE.4) THEN
                  IP2 = DLKOL(IIS+2) - 1
               ELSE
                  IP2 = DLKOL(2) - 1
                  END IF
               DO 145 IIF = 1,MUMIF
                  IP1 = IP1 + 1
                  IP2 =  IP2 + 1
                  LP = LP + MUMPOL
                  IF (GNREC(IP1).NE.FBLANK) THEN
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK)
     *                     VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1)
                        END IF
                     END IF
 145              CONTINUE
 150           CONTINUE
C                                       Rate (Hz)
         ELSE IF (ICODE.EQ.4) THEN
            DO 170 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = RAKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  IP2 = RAKOL(IIS+2) - NTONE
                  JIF = IP2 - IP1 + 1
               ELSE
                  IP2 = RAKOL(2) - NTONE
                  END IF
               DO 165 IIF = 1,MUMIF
                  IP1 = IP1 + NTONE
                  IP2 = IP2 + NTONE
                  LP = LP + MUMPOL
                  IF (SUMSTK.EQ.3) JIF = IIF
                  IF (GNREC(IP1).NE.FBLANK) THEN
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK) VALUE(LP,KK) =
     *                     RATFAC(IIF) * GNREC(IP1) -
     *                     RATFAC(JIF) * GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1) * RATFAC(IIF)
                        END IF
                     END IF
 165              CONTINUE
 170           CONTINUE
C                                       System temperature (K)
         ELSE IF (ICODE.EQ.5) THEN
            DO 190 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = TSKOL(IIS) - 1
               IP2 = IP1
               IF (ITPLOT.GE.4) THEN
                  IP2 = TSKOL(IIS+2) - 1
               ELSE
                  IP2 = TSKOL(2) - 1
                  END IF
               DO 185 IIF = 1,MUMIF
                  IP1 = IP1 + 1
                  IP2 =  IP2 + 1
                  LP = LP + MUMPOL
                  IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
                  IF (ABS(GNREC(IP2)-999.0).LT.0.1) GNREC(IP2) = FBLANK
                  IF (GNREC(IP1).NE.FBLANK) THEN
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK)
     *                     VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                     ELSE IF (DORATO) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(IP2).NE.0.0))
     *                     VALUE(LP,KK) = GNREC(IP1) / GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1)
                        END IF
                     END IF
 185              CONTINUE
 190           CONTINUE
C                                       Extreme amplitude or phase
C                                       Selected IF
         ELSE IF (ICODE.EQ.6) THEN
            AVAL = 0.0
            PVAL = 0.0
            GOT = .FALSE.
            DO 210 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = REKOL(IIS) - 1
               JP1 = IMKOL(IIS) - 1
               IP2 = REKOL(2) - 1
               JP2 = IMKOL(2) - 1
               DO 205 IIF = BIF,EIF
                  IP1 = IP1 + 1
                  JP1 = JP1 + 1
                  IP2 = IP2 + 1
                  JP2 = JP2 + 1
                  LP = LP + MUMPOL
                  IF ((GNREC(IP1).EQ.FBLANK) .OR.
     *               (GNREC(JP1).EQ.FBLANK)) GO TO 205
                  IF (SUMSTK.GE.3) THEN
                     IF ((GNREC(IP2).EQ.FBLANK) .OR.
     *                  (GNREC(JP2).EQ.FBLANK)) GO TO 205
                     IF (SUMSTK.EQ.4) THEN
                        AM = SQRT ((GNREC(IP1)**2 + GNREC(JP1)**2) /
     *                     (GNREC(IP2)**2 + GNREC(JP2)**2)+1.E-10)
                     ELSE
                        AM = SQRT ((GNREC(IP1)-GNREC(IP2))**2 +
     *                     (GNREC(JP1)-GNREC(JP2))**2)
                        END IF
                     IF (ABS(AM).GT.ABS(AVAL)) AVAL = AM
                     PH = 57.296 *
     *                  (ATAN2 (GNREC(JP1), GNREC(IP1)+1.0E-20) -
     *                  ATAN2 (GNREC(JP2), GNREC(IP2)+1.0E-20))
                     IF (PH.LE.-180.0) PH = PH + 360.0
                     IF (PH.GT.+180.0) PH = PH - 360.0
                     IF (ABS(PH).GT.ABS(PVAL)) PVAL = PH
                     GOT = T
                  ELSE
                     AM = SQRT ((GNREC(IP1)**2) + (GNREC(JP1)**2))
                     IF (ABS(AM).GT.ABS(AVAL)) AVAL = AM
                     PH = 57.296 * ATAN2 (GNREC(JP1),GNREC(IP1)+1.0E-20)
                     IF (ABS(PH).GT.ABS(PVAL)) PVAL = PH
                     GOT = T
                     IF ((NUMPOL.GT.1) .AND. (SUMSTK.EQ.0)) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) THEN
                           AM = SQRT ((GNREC(IP2)**2)+(GNREC(JP2)**2))
                           IF (ABS(AM).GT.ABS(AVAL)) AVAL = AM
                           PH = 57.296 *
     *                        ATAN2 (GNREC(JP2), GNREC(IP2)+1.0E-20)
                           IF (ABS(PH).GT.ABS(PVAL)) PVAL = PH
                           END IF
                        END IF
                     END IF
 205              CONTINUE
 210           CONTINUE
            IF (GOT) THEN
               VALUE(1,KK) = AVAL
               IF (SUMSTK.EQ.4) VALUE(1,KK) = AVAL + 1.0
               VALUE(1,2) = PVAL
               END IF
C                                       Doppler offset (Hz)
         ELSE IF (ICODE.EQ.7) THEN
            IF (GNREC(DOPLKL).NE.FBLANK) THEN
               IF (ITPLOT.GE.4) THEN
                  IF (GNREC(DOP3KL).NE.FBLANK) VALUE(LP,KK) =
     *               GNREC(DOPLKL) - GNREC(DOP3KL)
               ELSE
                  VALUE(LP,KK) = GNREC(DOPLKL)
                  END IF
               END IF
C                                       SNR (no units)
         ELSE IF (ICODE.EQ.8) THEN
            DO 250 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = WTKOL(IIS) - 1
               IF (ITPLOT.GE.4) THEN
                  IP2 = WTKOL(IIS+2) - 1
               ELSE
                  IP2 = WTKOL(2) - 1
                  END IF
               DO 245 IIF = 1,MUMIF
                  IP1 = IP1 + 1
                  IP2 =  IP2 + 1
                  LP = LP + MUMPOL
                  IF (GNREC(IP1).NE.FBLANK) THEN
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK)
     *                     VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                     ELSE IF (DORATO) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(IP2).NE.0.0))
     *                     VALUE(LP,KK) = GNREC(IP1) / GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1)
                        END IF
                     END IF
 245              CONTINUE
 250           CONTINUE
C                                       Multiband Delay (sec)
         ELSE IF (ICODE.EQ.9) THEN
            DO 270 IIS = 1,MUMPOL
               LP = IIS
               IP1 = MBKOL(IIS)
               IP2 = MBKOL(2)
               IF (GNREC(IP1).NE.FBLANK) THEN
                  IF (SUMSTK.EQ.3) THEN
                     IF (GNREC(IP2).NE.FBLANK)
     *                  VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                  ELSE
                     VALUE(LP,KK) = GNREC(IP1)
                     END IF
                  END IF
 270           CONTINUE
C                                       Tant (K)
         ELSE IF (ICODE.EQ.10) THEN
            DO 290 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = TAKOL(IIS) - 1
               IP2 = IP1
               IF (ITPLOT.GE.4) THEN
                  IP2 = TAKOL(IIS+2) - 1
               ELSE
                  IP2 = TAKOL(2) - 1
                  END IF
               DO 285 IIF = 1,MUMIF
                  IP1 = IP1 + 1
                  IP2 =  IP2 + 1
                  LP = LP + MUMPOL
                  IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
                  IF (ABS(GNREC(IP2)-999.0).LT.0.1) GNREC(IP2) = FBLANK
                  IF (GNREC(IP1).NE.FBLANK) THEN
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK)
     *                     VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                     ELSE IF (DORATO) THEN
                        IF ((GNREC(IP2).NE.FBLANK)  .AND.
     *                     (GNREC(IP2).NE.0.0))
     *                     VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1)
                        END IF
                     END IF
 285              CONTINUE
 290           CONTINUE
C                                       Atmos. Delay (sec)
         ELSE IF (ICODE.EQ.11) THEN
            IF (GNREC(ATMKOL).NE.FBLANK) VALUE(LP,KK) = GNREC(ATMKOL)
C                                       Geometric Delay (sec)
         ELSE IF (ICODE.EQ.12) THEN
            IF (GNRECD(GEOKOL).NE.DBLANK) VALUE(LP,KK) = GNRECD(GEOKOL)
C                                       Cable cal (sec)
         ELSE IF (ICODE.EQ.13) THEN
            IF (GNRECD(CABKOL).NE.DBLANK) VALUE(LP,KK) = GNRECD(CABKOL)
C                                       Dispersive Delay (sec)
         ELSE IF (ICODE.EQ.14) THEN
            DO 370 IIS = 1,MUMPOL
               LP = IIS
               IP1 = DSKOL(IIS)
               IP2 = DSKOL(2)
               IF (GNREC(IP1).NE.FBLANK) THEN
                  IF (SUMSTK.EQ.3) THEN
                     IF (GNREC(IP2).NE.FBLANK)
     *                  VALUE(LP,KK) = GNREC(IP1) - GNREC(IP2)
                  ELSE
                     VALUE(LP,KK) = GNREC(IP1)
                     END IF
                  END IF
 370           CONTINUE
C                                       REAL
         ELSE IF (ICODE.EQ.15) THEN
            DO 395 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = REKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  IP2 = REKOL(IIS+2) - NTONE
               ELSE
                  IP2 = REKOL(2) - NTONE
                  END IF
               DO 390 IIF = 1,MUMIF
                  IP1 = IP1 + NTONE
                  IP2 =  IP2 + NTONE
                  LP = LP + MUMPOL
                  IF ( GNREC(IP1).NE.FBLANK ) THEN
                     IF (DORATO) THEN
                        IF (GNREC(IP2).NE.FBLANK)  VALUE(LP,KK) =
     *                     GNREC(IP1) / GNREC(IP2)
                     ELSE IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK) VALUE(LP,KK) = GMMOD *
     *                     (GNREC(IP1)-GNREC(IP2))
                     ELSE
                        VALUE(LP,KK) = GMMOD * GNREC(IP1)
                        END IF
                     END IF
 390              CONTINUE
 395           CONTINUE
C                                       IMAG
         ELSE IF (ICODE.EQ.16) THEN
            DO 420 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               JP1 = IMKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  JP2 = IMKOL(IIS+2) - NTONE
               ELSE
                  JP2 = IMKOL(2) - NTONE
                  END IF
               DO 415 IIF = 1,MUMIF
                  JP1 = JP1 + NTONE
                  JP2 = JP2 + NTONE
                  LP = LP + MUMPOL
                  IF (GNREC(JP1).NE.FBLANK) THEN
                     IF (DORATO) THEN
                        IF (GNREC(JP2).NE.FBLANK) VALUE(LP,KK) =
     *                     GNREC(JP1) / GNREC(JP2)
                     ELSE IF (DODIFF) THEN
                        IF (GNREC(JP2).NE.FBLANK) VALUE(LP,KK) = GMMOD *
     *                     (GNREC(JP1)-GNREC(JP2))
                     ELSE
                        VALUE(LP,KK) = GMMOD * GNREC(JP1)
                        END IF
                     END IF
 415              CONTINUE
 420           CONTINUE
C                                       Atmos. Delay (sec)
         ELSE IF (ICODE.EQ.17) THEN
            IF (GNREC(IFRKOL).NE.FBLANK) VALUE(LP,KK) = GNREC(IFRKOL)
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
                     IF (DODIFF) THEN
                        IF (GNREC(IP2).NE.FBLANK) VALUE(LP,KK) =
     *                     GNREC(IP1) - GNREC(IP2)
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1)
                        END IF
                     END IF
 425               CONTINUE
 430           CONTINUE
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
     *               (GNREC(JP1).NE.FBLANK).AND. (GNREC(KP1).NE.FBLANK)
     *               .AND.(GNREC(KP1).NE.0.0)) THEN
                     V = (GNREC(JP1) + S*GNREC(IP1)) / (2.0*GNREC(KP1))
                     IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK).AND.
     *                     (GNREC(KP2).NE.FBLANK) .AND.
     *                     (GNREC(KP2).NE.0.0)) VALUE(LP,KK) = V -
     *                     (GNREC(JP2)+S*GNREC(IP2)) / (2. * GNREC(KP2))
                     ELSE
                        VALUE(LP,KK) = V
                        END IF
                     END IF
 435              CONTINUE
 440           CONTINUE
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
                  TC = TCAL(ISTOK+IIS-1,IIF-1+BIF,IANT)
                  IF (TC.LE.0.0) TC = FBLANK
                  IF ((GNREC(IP1).NE.FBLANK) .AND.
     *               (GNREC(JP1).NE.FBLANK).AND. (GNREC(IP1).GT.0.0)
     *               .AND. (TC.NE.FBLANK)) THEN
                     V = GNREC(JP1) / GNREC(IP1) / 2.0 * TC
                     IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK) .AND.
     *                     (GNREC(IP2).GT.0.0)) VALUE(LP,KK) = V -
     *                     GNREC(JP2)/GNREC(KP2)/2.0 * TC
                     ELSE
                        VALUE(LP,KK) = V
                        END IF
                     END IF
 445              CONTINUE
 450           CONTINUE
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
     *               (GNREC(JP1).NE.FBLANK)) THEN
                     V = 4096. * GNREC(JP1) * GNREC(JP1)
                     IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) THEN
                           S = 4096. * GNREC(JP2) * GNREC(JP2)
                           VALUE(LP,KK) = GNREC(IP1)/V - GNREC(IP2)/S
                           END IF
                     ELSE
                        VALUE(LP,KK) = GNREC(IP1) / V
                        END IF
                     END IF
 455               CONTINUE
 460           CONTINUE
C                                       Gains as powers
         ELSE IF ((ICODE.EQ.26) .OR. (ICODE.EQ.27)) THEN
            DO 470 IIS = 1,MUMPOL
               LP = IIS - MUMPOL
               IP1 = REKOL(IIS) - NTONE
               JP1 = IMKOL(IIS) - NTONE
               IF (ITPLOT.GE.4) THEN
                  IP2 = REKOL(IIS+2) - NTONE
                  JP2 = IMKOL(IIS+2) - NTONE
               ELSE
                  IP2 = REKOL(2) - NTONE
                  JP2 = IMKOL(2) - NTONE
                  END IF
               DO 465 IIF = 1,MUMIF
                  IP1 = IP1 + NTONE
                  JP1 = JP1 + NTONE
                  IP2 =  IP2 + NTONE
                  JP2 = JP2 + NTONE
                  LP = LP + MUMPOL
                  IF ((GNREC(IP1).NE.FBLANK) .AND.
     *               (GNREC(JP1).NE.FBLANK)) THEN
                     IF (DORATO) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) VALUE(LP,KK) =
     *                     (GNREC(IP1)**2 + GNREC(JP1)**2) /
     *                     (GNREC(IP2)**2 + GNREC(JP2)**2+1.E-20)
                     ELSE IF (DODIFF) THEN
                        IF ((GNREC(IP2).NE.FBLANK) .AND.
     *                     (GNREC(JP2).NE.FBLANK)) VALUE(LP,KK) = GMMOD
     *                     * ((GNREC(IP1)-GNREC(IP2))**2  +
     *                     (GNREC(JP1)-GNREC(JP2))**2)
                     ELSE
                        VALUE(LP,KK) = GMMOD *
     *                     ((GNREC(IP1)**2) + (GNREC(JP1)**2))
                        END IF
                     IF (VALUE(LP,KK).EQ.0.0) THEN
                        VALUE(LP,KK) = FBLANK
                     ELSE IF (ICODE.EQ.27) THEN
                        VALUE(LP,KK) = -10.0 * LOG10 (VALUE(LP,KK))
                     ELSE
                        VALUE(LP,KK) = 1.0 / VALUE(LP,KK)
                        END IF
                     END IF
 465              CONTINUE
 470           CONTINUE
            END IF
 900     CONTINUE
C
      OKAY = .TRUE.
      LP = MUMPOL * MUMIF
      DO 910 IIS = 1,LP
         IF (VALUE(IIS,KK).NE.FBLANK) GO TO 999
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
      INTEGER   IPLOT, JPLT, IPLT, NPLT, DIFF, LUMIF, LIF1, LIF2, KK,
     *   LUMST, LST1, LST2, IIF, IIS
      LOGICAL   ONEIF, ONEST, DOIT
      INCLUDE 'SNPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
      DIFF = MAX (1, SUMSTK-1)
      IF ((DIFF.EQ.1) .AND. (ITPLOT.GE.4)) DIFF = ITPLOT - 2
C                                       Summary plot?
      IF (OPTYPE.EQ.'SUM ') THEN
         CALL SUMPLT (DIFF, NV, NA, PLTPTS, IRET)
C                                       Loop thru stations to plot
      ELSE
         ONEST = (OPCODE.EQ.'ALST') .OR. (OPCODE.EQ.'ALSI')
         ONEIF = (OPCODE.EQ.'ALIF') .OR. (OPCODE.EQ.'ALSI')
         IF (ONEIF) THEN
            LUMIF = 1
            LIF1 = 1
            LIF2 = MUMIF
         ELSE
            LUMIF = MUMIF
            END IF
         IF (ONEST) THEN
            LUMST = 1
            LST1 = 1
            LST2 = MUMPOL
         ELSE
            LUMST = MUMPOL
            END IF
C                                       count the plots
         NPLOTS = 0
         DO 40 IPLT = 1,MUMANT
            DO 35 IIF = 1,LUMIF
               IF (.NOT.ONEIF) THEN
                  LIF1 = IIF
                  LIF2 = IIF
                  END IF
               DO 30 IIS = 1,LUMST
                  IF (.NOT.ONEST) THEN
                     LST1 = IIS
                     LST2 = IIS
                     END IF
                  DO 25 KK = 1,NCODES
                     CALL GETSCL (KK, LST1, LST2, LIF1, LIF2, IPLT,
     *                  DOIT)
                     IF (DOIT) NPLOTS = NPLOTS + 1
 25                  CONTINUE
 30               CONTINUE
 35            CONTINUE
 40         CONTINUE
C                                       Now plot
         NPLT = 0
         DO 100 IPLT = 1,MUMANT
            DO 90 IIF = 1,LUMIF
               IF (.NOT.ONEIF) THEN
                  LIF1 = IIF
                  LIF2 = IIF
                  END IF
               DO 80 IIS = 1,LUMST
                  IF (.NOT.ONEST) THEN
                     LST1 = IIS
                     LST2 = IIS
                     END IF
                  DO 70 KK = 1,NCODES
                     CALL GETSCL (KK, LST1, LST2, LIF1, LIF2, IPLT,
     *                  DOIT)
                     IF (DOIT) THEN
                        NPLT = NPLT + 1
                        JPLT = NPLT
                        IPLOT = MOD (NPLT-1, NCOUNT) + 1
                        IF (NPLT.EQ.NPLOTS) IPLOT = -IPLOT
                        CALL PLTSN (IPLOT, KK, LST1, LST2, LIF1, LIF2,
     *                     IPLT, DIFF, NV, PLTPTS, IRET)
                        IF (IRET.NE.0) GO TO 999
                        END IF
 70                  CONTINUE
 80               CONTINUE
 90            CONTINUE
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLTSN (IPLOT, KK, LST1, LST2, LIF1, LIF2, ANTNO, DIFF,
     *   NV, PLTPTS, IRET)
C-----------------------------------------------------------------------
C   PLTSN actually plots data.
C   Input:
C      IPLOT    I      Plot number on current page. If neg. then this is
C                      last plot.
C      KK       I      Parameter number
C      LST1     I      1st Stokes this plot
C      LST2     I      last Stokes this plot
C      LIF1     I      1st IF this plot
C      LIF2     I      last IF this plot
C      ANTNO    I      Antenna number
C      DIFF     I      3/2 if plotting ratio/difference between Stokes
C                      parameters , 1 otherwise
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
      INTEGER   IPLOT, kk, LST1, LST2, LIF1, LIF2, ANTNO, DIFF, NV,
     *   IRET
      REAL      PLTPTS(NV,*)
C
      INCLUDE 'SNPLT.INC'
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, CHTMP*18,
     *   AUNITS(NCODE+2,3)*8, CHTYPE(NCODE+2,3)*16, XUNITS(6)*20,
     *   TXTMSG*80, CSAVE*5
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, JCODE, NN, IP, IST, IIF, LDIFF, ILITY, NNN
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   DBY, COLV, COL(3), COLR, AX(5), AY(5), OLDSRC
      LOGICAL   T, F, GOOD, CATUP, DONEG, DO3C, BLNKD, SCOLOR, DOCOLR
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
     *             'Degrees', 'PCamp',
     *             'Degrees','Gain','Seconds','Hz','Kelvin',' ','Hz',
     *             'SNR', 'Sec/m**2', 'Kelvin', 'Seconds', 'Seconds',
     *             'Seconds','Seconds','Gain','Gain','Rad/m**2',
     *             'Counts', 'Counts', 'Gain', 'Counts', 'Counts',
     *             'Kelvin', 'Counts', 'Counts', 'Gain^-2', 'db',
     *             'Degrees', 'PCamp',
     *             'Degrees','Ratio','Seconds','Hz','Ratio',' ','Hz',
     *             'Ratio', 'Seconds', 'Ratio', 'Seconds', 'Seconds',
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
     *             'Phase-cal Phase', 'Phase-cal Ampl',
C                                       differences
     *             'R-L phase diff','R-L amp diff','R-L delay diff',
     *             'R-L rate diff','R-L Tsys diff', ' ',
     *             'R-L Doppler diff', 'R-L SNR diff',
     *             'R-L MB delay dif', 'R-L Tant diff', 3*' ',
     *             'R-L Disdelay dif', 'R-L real diff', 'R-L imag diff',
     *             ' ',  'R-L Pdif diff', 'R-L Psum diff',
     *             'R-L Pgain diff', 'R-L Pon diff', 'R-L Poff diff',
     *             'R-L Tsys diff', 'R-L Pd/Pg diff', 'R-L Ps/Pg diff',
     *             'R-L Power diff', 'R-L pow diff db', 2*' ',
C                                       ratios
     *             ' ','R/L amp ratio',' ', ' ','R/L Tsys ratio', 2*' ',
     *             'R/L SNR ratio', ' ', 'R/L Tant ratio', 19*' '/
      DATA XUNITS /'IAT (hr)', 'Elevation (degrees)',
     *   'Hour Angle (hr)', 'LST (hr)', 'Parallactic angle',
     *   'Azimuth (degrees)' /
C-----------------------------------------------------------------------
C                                       Time system from AN table
      SCOLOR = (XDO3C.GT.1.5) .AND. (CSMAX-CSMIN.GT.0.99)
      OLDSRC = -1000.0
      XUNITS(1)(1:3) = TIMLAB(1:3)
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C
      JCODE = ICODES(KK)
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
         IF (DO3COL.LE.0.0) THEN
            CALL GCINIT (GPHTVG(4), 0, IERR)
            IF (IERR.NE.0) GO TO 960
            CALL GCINIT (GPHTVG(3), 0, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
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
      LABTYP(LOCNUM) = 7
      IF (XVAR.EQ.2) LABTYP(LOCNUM) = 0
      IF (XVAR.EQ.6) LABTYP(LOCNUM) = 0
      IF (XVAR.EQ.3) LABTYP(LOCNUM) = 8
      IF (XVAR.EQ.4) LABTYP(LOCNUM) = 9
      IF (XVAR.EQ.5) LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.1 * (PRAN(2,2)-PRAN(1,2))
      IF (TR.LE.0.0) TR = 1.0
      IF ((ABS(IPLOT).EQ.1) .OR. (NCODES.GT.1)) THEN
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
      CTYP(2,LOCNUM) = AUNITS(JCODE, DIFF)
      LDIFF = DIFF
      IF (ITPLOT.GE.4) LDIFF = 1
C                                       Init plot calls again
C                                       Number of characters on each
C                                       side of the plot
      IF (ABS (IPLOT).EQ.1) THEN
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
         IF (NCODES.GT.1) INP = MAX (INP, 7)
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
         TEXT = CHTYPE(JCODE,LDIFF)
         IF (NCODES.GT.1) TEXT = 'Multiple'
         IF (ICOR0.LT.-4) THEN
            IF (TEXT(1:3).EQ.'R-L') THEN
               TEXT(1:3) = 'X-Y'
            ELSE IF (TEXT(:3).EQ.'R/L') THEN
               TEXT(1:3) = 'X/Y'
               END IF
            END IF
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         IF (ITPLOT.GE.4) THEN
            TEXT(INP+1:) = ' IF diff'
            IF (ITPLOT.EQ.5) TEXT(INP+1:) = ' IF ratio'
            INCHAR = 132
            CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
            END IF
         INP = INP + 1
         IF (XVAR.EQ.1) THEN
            TEXT(INP:INP+16) = ' vs '// TIMLAB(1:3) // ' time for '
            INP = INP + 17
         ELSE IF (XVAR.EQ.2) THEN
            TEXT(INP:INP+17) = ' vs elevation for '
            INP = INP + 18
         ELSE IF (XVAR.EQ.3) THEN
            TEXT(INP:INP+10) = ' vs HA for '
            INP = INP + 11
         ELSE IF (XVAR.EQ.4) THEN
            TEXT(INP:INP+16) = ' vs LST time for '
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
         IF (LDIFF.EQ.1) THEN
            IF (SUMSTK.GT.0) THEN
               TEXT(INP:) = XSTOK(:1) // 'pol_'
               INP = INP + 7
            ELSE
               TEXT(INP:) = 'Rpol & Lpol_'
               IF (ICOR0.LT.-4) TEXT(INP:) = 'Xpol & Ypol_'
               INP = INP + 14
               END IF
            END IF
         IF (ITPLOT.GE.4) THEN
            WRITE (TEXT(INP:),1020) BIF, EIF
            INP = INP + 12
         ELSE IF (BIF.EQ.EIF) THEN
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
      ELSE IF ((MUMIF.LE.1) .OR. (LIF1.NE.LIF2)) THEN
         IF (ICOR0.LT.-4) THEN
            TEXT(INP:) = 'X'
            IF (LST1.EQ.2) TEXT(INP:) = 'Y'
         ELSE
            TEXT(INP:) = 'R'
            IF (LST1.EQ.2) TEXT(INP:) = 'L'
            END IF
         INP = INP + 1
         END IF
      TEXT(INP+1:) = STNNAM(ANTNO)
      CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Stokes, IF
      IF ((MUMIF.GT.1) .AND. (LIF1.EQ.LIF2)) THEN
         INP = 1
C         WRITE (TEXT(INP:),1021) LIF1
C
         WRITE (TEXT(INP:),1021) LIF1 + BIF - 1
         INP = INP + 5
         IF ((MUMPOL.GT.1) .AND. (LST1.EQ.LST2)) THEN
            IF (ICOR0.LT.-4) THEN
               TEXT(INP:) = 'X'
               IF (LST1.EQ.2) TEXT(INP:) = 'Y'
            ELSE
               TEXT(INP:) = 'R'
               IF (LST1.EQ.2) TEXT(INP:) = 'L'
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
C                                       type of plot
      IF (NCODES.GT.1) THEN
         TEXT = CHTYPE(JCODE,LDIFF)
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         CALL GPOS (XTRC(1), XTRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DX = -INP - 3.0
         DY = -2.8
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Set up location common
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF (NCODES.LE.1) THEN
         CPREF(2,LOCNUM) = CSAVE
         IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *      (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
         END IF
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Size of symbol.
      DBY = 0.5 * FACTOR
C                                       Loop
      ILITY = 4
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO3C = (DO3COL.GT.0.0) .AND. (LIF2.GT.LIF1)
      COLR = (LIF2 - LIF1 + 1.0) * (LST2 - LST1 + 1.0) - 1.0
      DOCOLR = DO3C .OR. SCOLOR
C                                       Outer loop: IF, stokes
      COLV = 0.0
      DO 200 IIF = LIF1,LIF2
         DO 190 IST = LST1,LST2
            IF (DO3COL.GT.0.0) THEN
               IF (DO3C) THEN
                  CALL COLOR3 (COLV, .FALSE., COL)
                  CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  COLV = COLV + 1.0 / COLR
               ELSE IF (2+IST.NE.ILITY) THEN
                  ILITY = 2 + IST
                  CALL GLTYPE (ILITY, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               END IF
            IP = (IIF-1) * MUMPOL + IST + 2 + (KK-1) * MUMPOL * MUMIF
C                                       Point plot
            DO 120 NN = 1,NUMPTS(ANTNO)
               DONEG = (JCODE.EQ.1) .OR. (JCODE.EQ.NCODE+1)
C                                       Scale X
               NNN = NN - 1 + FANREC(ANTNO)
               XY(1) = PLTPTS(2,NNN)
               IF ((SWAP) .AND. (XY(1).GT.180.0)) XY(1) = XY(1) - 360.0
               IF ((XVAR.EQ.1) .OR. (XVAR.EQ.3) .OR. (XVAR.EQ.4))
     *            XY(1) = XY(1) * 360.0
               XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
               IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
                  NNOFIT = NNOFIT + (LST2-LST1+1)*(LIF2-LIF1+1)
                  GO TO 120
                  END IF
C                                       source color
               IF ((SCOLOR) .AND. (ABS(PLTPTS(1,NNN)-OLDSRC).GT.0.1))
     *            THEN
                  COLV = 0.95 * (PLTPTS(1,NNN)-CSMIN) / (CSMAX-CSMIN)
                  CALL COLOR3 (COLV, .FALSE., COL)
                  CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  OLDSRC = PLTPTS(1,NNN)
                  END IF
C                                       loop for points
               VALUE = PLTPTS(IP,NNN)
C                                       ?????????????????
               IF (VALUE.NE.FBLANK) THEN
 110              XY(2) = VALUE
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
                     IF ((DO3COL.LE.0) .AND. (ILITY.NE.4)) THEN
                        ILITY = 4
                        CALL GLTYPE (ILITY, BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        END IF
                     CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *                  DOCOLR, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
               ELSE IF (BSYM.GT.0) THEN
                  DY = 5.0 * FACTOR
                  DX = 5.0 * FACTOR
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
                  IF ((DO3COL.LE.0) .AND. (ILITY.NE.3)) THEN
                     ILITY = 3
                     CALL GLTYPE (ILITY, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  CALL PNTPLT (BSYM, AX, AY, XBLC, XTRC, .FALSE.,
     *               DOCOLR, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
 120           CONTINUE
C                                       Line plot
            IF (DOLINE) THEN
               BLNKD = .TRUE.
               DO 140 NN = 1,NUMPTS(ANTNO)
                  DONEG = (JCODE.EQ.1) .OR. (JCODE.EQ.NCODE+1)
C                                       Scale X
                  NNN = NN - 1 + FANREC(ANTNO)
                  XY(1) = PLTPTS(2,NNN)
                  IF ((XVAR.NE.2) .AND. (XVAR.NE.5) .AND. (XVAR.NE.6))
     *               XY(1) = XY(1) * 360.0
                  XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                  IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)))
     *               GO TO 140
C                                       loop for points
                  VALUE = PLTPTS(IP,NNN)
                  IF (VALUE.EQ.FBLANK) THEN
                     BLNKD = .TRUE.
                  ELSE
 130                 XY(2) = VALUE
                     XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                     IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2)))
     *                  THEN
                        IF (DONEG) THEN
                           IF (VALUE.LT.0.) THEN
                              VALUE = VALUE + 360.
                           ELSE
                              VALUE = VALUE - 360.
                              END IF
                           DONEG = .FALSE.
                           GO TO 130
                           END IF
                     ELSE
                        NGOOD = NGOOD + 1
C                                       Mark point
                        IF (BLNKD) THEN
                           CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                           BLNKD = .FALSE.
                        ELSE IF (DOCOLR) THEN
                           CALL G3VEC (XY(1), XY(2), BUFFER, IERR)
                        ELSE
                           CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                           END IF
                        IF (IERR.NE.0) GO TO 970
                        END IF
                     END IF
 140              CONTINUE
               END IF
C                                       plot scan boundaries
            IF ((XSCAN.GT.0.0) .AND. (NOSCAN.GT.0)) THEN
               WRITE (TXTMSG,1150) NOSCAN
               CALL GCOMNT (-1, TXTMSG, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               I = 3
               CALL GLTYPE (I, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               AY(1) = XYSCL(2) * (PRAN(1,2)-XYOFF(2)) + YYOFF(2)
               AY(2) = XYSCL(2) * (PRAN(2,2)-XYOFF(2)) + YYOFF(2)
               IF (XSCAN.GE.1.5) AY(2) = AY(1) + 0.1*(AY(2)-AY(1))
               DO 160 I = 1,NOSCAN
                  AX(1) = XYSCL(1) * (TSCAN(I)*360.-XYOFF(1)) + YYOFF(1)
                  IF ((AX(1).GE.XBLC(1)) .AND. (AX(1).LE.XTRC(1))) THEN
                     CALL GPOS (AX(1), AY(1), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(2), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
 160              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
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
 1020 FORMAT ('IF ',I2,' vs',I3)
 1021 FORMAT ('IF ',I2)
 1022 FORMAT ('IF ',I2,' - ',I2)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I3)
 1060 FORMAT ('_Phase tone ',I2)
 1150 FORMAT ('Plotting',I4,' scan breaks')
 1200 FORMAT ('PLTSN:',I9,' points plotted')
 1202 FORMAT ('PLTSN:',I9,' points did not fit')
 1960 FORMAT ('PLTSN: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTSN: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE SUMPLT (DIFF, NV, NA, PLTPTS, IRET)
C-----------------------------------------------------------------------
C   Plots all amplitudes and all phases selected in 2 plots on a single
C   page.
C   Input:
C      DIFF   I    2 if plotting difference between R and L,
C                  1 otherwise
C   Inputs from Common:
C      GMN    R    Max. value to plot
C      GMX    R    Min. value to plot
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C                     1 => failed to add to catalog
C                     2 => failed to create
C                     3 => graph file write error
C                     4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   DIFF, NV, NA, IRET
      REAL      PLTPTS(NV,*)
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(2,3)*8,
     *   CHTYPE(2,3)*12, CHTMP*18, XUNITS(6)*20
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, FINDPL,
     *   DEPTH(5), INCHAR, INP, IT(3), ID(3), LTYPE, NNOFIT, I, NGOOD,
     *   IPLOT, NN, ANTNO, NNN
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   XVARIB, TMAX, TMIN, COLV, COL(3), COLR, OLDSRC, AX(5), AY(5)
      LOGICAL   T, F, CATUP, SCOLOR, DOCOLR
      INCLUDE 'SNPLT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Gain','Degrees', 'Gain','Degrees',
     *   'Ratio','Degrees'/
      DATA CHTYPE /'Gain amp','Gain phs', 'Diff amp','Phase diff',
     *   'Amp ratio','Phase diff' /
      DATA XUNITS /'IAT (hr)', 'Elevation (degrees)',
     *   'Hour Angle (hr)', 'LST (hr)', 'Parallactic angle',
     *   'Azimuth (degrees)'/
C-----------------------------------------------------------------------
      IRET = 1
      CATUP = T
      XUNITS(1)(1:3) = TIMLAB(1:3)
      DO3COL = XDO3C
      SCOLOR = XDO3C.GT.1.5
      IF (SCOLOR) DO3COL = -1.0
      IF (NA.LE.1) DO3COL = -1.0
      OLDSRC = -1
      IF (CSMAX-CSMIN.LE.0.9) SCOLOR = .FALSE.
      DOCOLR = SCOLOR .OR. (DO3COL.GT.0)
C                                       Loopover amplitude and phase
      DO 500 IPLOT = 1,2
C                                       Number of characters on each
C                                       side of the plot
         IF (IPLOT.EQ.1) THEN
C                                       Create plot file
C                                       Update catalog header.
            VER = 0
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFFER, CATUP,
     *            'WRIT', VER, IERR)
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
     *         XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER,
     *         LUNPL, FINDPL, IERR)
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
            END IF
         XYRATO = 1.0
         NCOUNT = 2
         PLTINC = TRC(2) / NCOUNT
         TMAX = YYMAX(IPLOT,1,1,1) + 0.1 * (YYMAX(IPLOT,1,1,1) -
     *      YYMIN(IPLOT,1,1,1))
         TMIN = YYMIN(IPLOT,1,1,1) - 0.1 * (YYMAX(IPLOT,1,1,1) -
     *      YYMIN(IPLOT,1,1,1))
         IF (TMAX-TMIN.LT.0.1) THEN
            TMAX = TMAX + 0.1
            TMIN = TMIN - 0.1
            END IF
         XYOFF(2) = TMIN
         XYSCL(2) = (TRC(2) - BLC(2)) / (TMAX - TMIN) / 2.0
         XYOFF(1) = PRAN(1,1)
         XYSCL(1) = (TRC(1) - BLC(1)) / (PRAN(2,1) - PRAN(1,1))
C                                       Set window for current plot.
         XBLC(1) = BLC(1)
         XBLC(2) = TRC(2) - IPLOT * PLTINC
         XTRC(1) = TRC(1)
         XTRC(2) = XBLC(2) + PLTINC - 1.0
         TLC(1) = XBLC(1)
         TLC(2) = XTRC(2)
C                                       Offsets for current plot.
         YYOFF(1) = XBLC(1)
         YYOFF(2) = XBLC(2)
C                                       play with location common
         LOCNUM = 1
         ROT(LOCNUM) = 0.0
         CORTYP(LOCNUM) = 0
         LABTYP(LOCNUM) = 7
         IF (XVAR.EQ.2) LABTYP(LOCNUM) = 0
         IF (XVAR.EQ.3) LABTYP(LOCNUM) = 8
         IF (XVAR.EQ.4) LABTYP(LOCNUM) = 9
         IF (XVAR.EQ.5) LABTYP(LOCNUM) = 0
         IF (XVAR.EQ.6) LABTYP(LOCNUM) = 0
         AXTYP(LOCNUM) = 0
         CPREF(1,LOCNUM) = '     '
         CPREF(2,LOCNUM) = '     '
         XMULT(1) = 1.0
         XMULT(2) = 1.0
         DO 50 I = 1,2
            SIZE = XTRC(I) - XBLC(I)
            IF (I.EQ.2) SIZE = PLTINC
            TR = SIZE / XYSCL(I)
            RPLOC(I,LOCNUM) = XBLC(I)
            RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
            AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 50         CONTINUE
         CTYP(1,LOCNUM) = XUNITS(XVAR)
         CTYP(2,LOCNUM) = AUNITS(IPLOT, DIFF)
C                                       Init for line drawing.
         IF (IPLOT.EQ.1) THEN
            CALL RFILL (4, 0.5, CHOUT)
C                                       Note that TICINC not fully
C                                       initialized as yet. -> INP being
C                                       larger than may be actually
C                                       plotted on this subplot.  This
C                                       is probably desirable.
            CALL CHNTIC (XBLC, XTRC, INP)
            INP = MAX (INP, 3)
            LTYPE = MOD (ABS (LABEL), 100)
            IF (LTYPE.EQ.2) CHOUT(1) = 2.5
            IF (LTYPE.GT.2) CHOUT(1) = INP + 4
            IF (LTYPE.GT.1) CHOUT(2) = 2.0
            IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
            IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
            IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *         CHOUT(4) = CHOUT(4) + 1.333
            CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, BUFFER, IERR)
            IRET = 3
            IF (IERR.NE.0) GO TO 970
            IF (.NOT.DOTV) THEN
               WRITE (MSGTXT,1000) VER
               CALL MSGWRT (2)
               END IF
            IRET = 3
            CATUP = T
            END IF
C                                       Set amp/phase flag
         NGOOD = 0
         NNOFIT = 0
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
         IF ((IPLOT.EQ.1) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
            DX = 0.0
            DY = 1.833
C                                       The second line of the header
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            TEXT = 'Vis data vs ' // TIMLAB(1:3) // ' time for '
C                                       File name
            CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
            CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(25:), INCHAR)
            CALL REFRMT (TEXT, ' ', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       the third line of header
            DY = 0.5
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Table type
            TEXT = TYPE // '_'
            INP = 5
C                                       Stokes and IF
            IF (ICOR0.LT.-4) THEN
               IF ((SUMSTK.EQ.1) .AND. (ICOR0.EQ.-1)) THEN
                  TEXT(INP:) = 'Xpol_'
               ELSE IF (SUMSTK.EQ.2) THEN
                  TEXT(INP:) = 'Ypol_'
               ELSE IF (DIFF.EQ.2) THEN
                  TEXT(INP:) = 'X-Y_'
               ELSE IF (DIFF.EQ.3) THEN
                  TEXT(INP:) = 'X/Y_'
               ELSE
                  TEXT(INP:) = 'Xpol & Ypol_'
                  END IF
            ELSE
               IF ((SUMSTK.EQ.1) .AND. (ICOR0.EQ.-1)) THEN
                  TEXT(INP:) = 'Rpol_'
               ELSE IF (SUMSTK.EQ.2) THEN
                  TEXT(INP:) = 'Lpol_'
               ELSE IF (DIFF.EQ.2) THEN
                  TEXT(INP:) = 'R-L_'
               ELSE IF (DIFF.EQ.3) THEN
                  TEXT(INP:) = 'R/L_'
               ELSE
                  TEXT(INP:) = 'Rpol & Lpol_'
                  END IF
               END IF
            INP = INP + 13
            WRITE (TEXT(INP:),1020) BIF, EIF
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
            IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) THEN
               DY = 0.5 + 2 * 1.333
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
C                                       Set up location common
         CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DX =  1.5
         DY = -1.8
         INCHAR = 8
         INP = 1
C                                       Blank bottom label for ampl.
         IF (IPLOT.EQ.1) THEN
            CPREF(1,LOCNUM) = ' '
            CTYP(1,LOCNUM) = ' '
            END IF
C                                       Put on labels and ticks
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Loop
         CALL GLTYPE (4, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (DO3COL.GT.0.0) THEN
            COLR = -1.0
            DO 195 ANTNO = 1,NA
               IF (NUMPTS(ANTNO).GT.0) COLR = COLR + 1.0
 195           CONTINUE
            IF (COLR.LE.0.0) DO3COL = -1.0
            END IF
         COLV = 0.0
         DO 200 ANTNO = 1,NA
            IF ((DO3COL.GT.0.0) .AND. (NUMPTS(ANTNO).GT.0)) THEN
               CALL COLOR3 (COLV, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               COLV = COLV + 1.0 / COLR
               END IF
            DO 190 NN = 1,NUMPTS(ANTNO)
               NNN = NN - 1 + FANREC(ANTNO)
               VALUE = PLTPTS(2+IPLOT,NNN)
               XVARIB = PLTPTS(2,NNN)
               IF ((SWAP) .AND. (XVARIB.GT.180.0)) XVARIB = XVARIB-360.0
C                                       source color
               IF ((SCOLOR) .AND. (ABS(PLTPTS(1,NNN)-OLDSRC).GT.0.1))
     *            THEN
                  COLV = 0.95 * (PLTPTS(1,NNN)-CSMIN) / (CSMAX-CSMIN)
                  CALL COLOR3 (COLV, .FALSE., COL)
                  CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  OLDSRC = PLTPTS(1,NNN)
                  END IF
               IF ((VALUE.NE.FBLANK) .AND. (XVARIB.NE.FBLANK)) THEN
                  IF (IPLOT.EQ.2) THEN
                     IF (VALUE.LT.YYMIN(2,1,1,1)) VALUE = VALUE + 360.
                     IF (VALUE.GT.YYMAX(2,1,1,1)) VALUE = VALUE - 360.
                     END IF
C                                       Scale X, Y
                  IF ((XVAR.EQ.1) .OR. (XVAR.EQ.3) .OR. (XVAR.EQ.4))
     *               THEN
                     XY(1) = XVARIB * 360.0
                  ELSE
                     XY(1) = XVARIB
                     END IF
                  XY(2) = VALUE
                  XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                  XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                  IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)) .OR.
     *               (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                     NNOFIT = NNOFIT + 1
                  ELSE
                     NGOOD = NGOOD + 1
C                                       Mark point
                     DY = 5.0 * FACTOR
                     DX = 5.0 * FACTOR
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
                     CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *                  DOCOLR, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
 190           CONTINUE
 200        CONTINUE
C                                       Done: finish plot
         WRITE (MSGTXT,1200) NGOOD, CHTYPE(IPLOT, DIFF)
         CALL MSGWRT (2)
         IF (NNOFIT.GT.0) THEN
            WRITE (MSGTXT,1201) NNOFIT
            CALL MSGWRT (2)
            END IF
 500     CONTINUE
C                                       Close down plot
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
      IF (.NOT.DOTV) CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
      IRET = 0
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
      WRITE (MSGTXT,1971) NGOOD
      CALL MSGWRT (2)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
         IERR = 0
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
 1020 FORMAT ('IF ',I2,' - ',I2)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1200 FORMAT ('SUMPLT: ',I9,1X,A,' points plotted')
 1201 FORMAT ('SUMPLT: ',I9,' points did not fit on plot')
 1960 FORMAT ('SUMPLT: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('SUMPLT: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
 1971 FORMAT ('SUMPLT: ',I9,' points plotted')
      END
      SUBROUTINE XCALC (XVARIB, XSOU)
C-----------------------------------------------------------------------
C  Routine to use the source and antenna geometry information in order
C  to return the requested value of the x-variable against which the
C  data is to be plotted.
C  Inputs (in common):
C    XVAR        I       Type of variable to calculate:
C                        1 = time (easy)
C                        2 = source elevation
C                        3 = HA
C                        4 = LST
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
      INCLUDE 'SNPLT.INC'
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
      IF (XVAR.EQ.1) THEN
         XVARIB = TIME
         GO TO 999
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
      IF (XVAR.EQ.2) THEN
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

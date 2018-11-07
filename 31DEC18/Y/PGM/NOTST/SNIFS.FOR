LOCAL INCLUDE 'SNIFS.INC'
C                                       Local include for SNIFS
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XQUAL, XTIME(8), XBAND, XFREQ,
     *   XFQID, XSUBA, XBIF, XEIF, XANT(50), PIXR(2), XNCOU,
     *   XDO3C, XPCNUM, SOLINT, XSYM, FACTOR, XDOBL, CUTOFF,
     *   XLABEL, XDOTV, XGRCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE, XXSOUR(4,30), XXSTOK,
     *   XOPTY, XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2, XSOUR(30)*16, XSTOK*4,
     *   OPTYPE*4, OPCODE*4
C                                       Program info
C
      INTEGER   MXSCAN
      PARAMETER (MXSCAN=5000)
C
      REAL      TSTART, TSTOP, TINT, XYSCL(2), XYOFF(2), YYMX, YYMN,
     *   XMX, XMN, GMMOD, RATFAC, SELBAN, XSTART, XSTOP, CHOUT(4),
     *   XXMIN, XXMAX, YYMIN(MAXANT), TSCAN(MXSCAN), YYMAX(MAXANT),
     *   PPMIN(MAXANT), PPMAX(MAXANT), PRAN(2,2), DO3COL,
     *   TCAL(4,MAXIF,MAXANT), PLTAVG(MAXIF,2,MAXANT)
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, ANTS(50), NCOUNT,
     *   ICODE, NPARMS, NID, SID(500), NANTSL, NPLOTS, SUMSTK, ISTOK,
     *   FRQSEL, GRCHN, TVCHN, TVCORN(4), ISOU, OSOU, IANT,
     *   EIF, ITPLOT, ITVER, PCNUM, LABEL, SUBARR, MUMPAR, MUMPOL,
     *   MUMIF, MUMANT, NTONE, NUMPTS(MAXANT), ISYM, BSYM,
     *   NANREC(MAXANT), FANREC(MAXANT), NOSCAN
      LOGICAL   DOAWNT, DOTV, NNODAT, DOLINE, SCALAR
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
     *   XEIF, XANT, PIXR, XNCOU, XOPTY, XOPCOD, XDO3C, XPCNUM,
     *   SOLINT, XSYM, FACTOR, XDOBL, CUTOFF, XLABEL, XDOTV,
     *   XGRCH
      COMMON /VPARM/ SEQIN, DISKIN, CNOIN, IVER, BIF, EIF, ANTS, NCOUNT,
     *   ICODE, NPARMS, GRCHN, TVCHN, TVCORN, ISOU, OSOU, IANT,
     *   ITPLOT, ITVER, PCNUM, DOTV, NNODAT, LABEL, CHOUT, DO3COL,
     *   DOLINE, NOSCAN, TSCAN, SCALAR
      COMMON /VGNCOM/ SELFRQ,
     *   TSTART, TSTOP, TINT, XYSCL, XYOFF, SELBAN, XMX, XMN, XSTART,
     *   XSTOP, GMMOD, RATFAC, NID, SID, NANTSL, PRAN,
     *   NPLOTS, DOAWNT, ISTOK, SUMSTK, FRQSEL,
     *   SUBARR, MUMPAR, MUMPOL, MUMIF, MUMANT, NUMPTS, NTONE,
     *   XXMIN, XXMAX, YYMIN, YYMAX, PPMIN, PPMAX, YYMX, YYMN, ISYM,
     *   BSYM, TCAL, NANREC, FANREC, PLTAVG
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
C                                                          End SNIFS
LOCAL END
      PROGRAM SNIFS
C-----------------------------------------------------------------------
C! Plots data from a SN, TY, PC or CL table
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2014-2015, 2017-2018
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
C   SNIFS plots SN or CL extension files. A 'PL' extension file is made
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
      INCLUDE 'SNIFS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'SNIFS '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNPIN (PRGN, NROWS, IRET)
      MUMANT = MAX (1, MUMANT)
      MVAL = 1 + MUMPAR*MUMPOL*MUMIF
      NWORDS = (MVAL * NROWS -1) / 1024 + 21
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PLTPTS, PPLTPT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         END IF
      NWORDS = NWORDS * 1024
C                                       read data to figure out
C                                       distribution
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
C   Gets the inputs parameters for SNIFS.
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
C-----------------------------------------------------------------------
      INTEGER   NROWS, IERR
      CHARACTER PRGN*6
C
      INTEGER   NCODE, NTPLT
C
      PARAMETER (NCODE=16, NTPLT=2)
C
      CHARACTER STAT*4, CODE(NCODE)*4, TYPTMP*2, TPLOT(NTPLT)*4
      INTEGER   IRET, BUFF(256), I, J, K, JERR, QUAL(30), NSOUR,
     *   BUFFER(512), IROUND, LUN, VER, NIF, NSTOK, FRQTMP, LTYPE
      LOGICAL T, F, MATCH
      INCLUDE 'SNIFS.INC'
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
C      DATA CODE /'PHAS', 'AMP ', 'DELA', 'RATE', 'TSYS', 'SUM ', 'DOPL',
C     *   'SNR ', 'MDEL', 'TANT', 'ATM ', 'GEO ', 'CCAL', 'DDLY', 'REAL',
C     *   'IMAG', 'IFR ', 'PDIF', 'PSUM', 'PGN ', 'PON ', 'POFF', 'PSYS'/
      DATA CODE /'AMP ', 'PHAS', 'REAL', 'IMAG', 'DELA', 'RATE', 'SNR ',
     *   'DOPL', 'TSYS', 'TANT', 'PDIF', 'PSUM', 'PGN ', 'PON ', 'POFF',
     *   'PSYS'/
      DATA TPLOT /'ALAN', 'ALTI'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OSOU = -1
      NPARMS = 210
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
C
      DO3COL = XDO3C
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         QUAL(I) = IROUND (XQUAL)
 20      CONTINUE
      IF (TYPE.EQ.' ') TYPE ='SN'
      CUTOFF = MAX (0.0, CUTOFF)
      IF ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC') .OR. (TYPE.EQ.'SY'))
     *   CUTOFF = -1000.0
      CALL FILL (MAXANT, 0, NUMPTS)
      IF (SOLINT.LE.0.0) SOLINT = 1.0
      SOLINT = SOLINT / (24.0 * 3600.0)
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
      PCNUM = IROUND (XPCNUM)
      IF (PCNUM.LE.0) PCNUM = 1
      DOLINE = FACTOR.LT.0.0
      FACTOR = ABS (FACTOR)
      IF ((.NOT.DOLINE) .AND. (FACTOR.LT.0.1)) FACTOR = 1.0
      IF (FACTOR.GT.10.0) FACTOR = 1.0
C                                       plot types
      SCALAR = OPTYPE.EQ.'SAMP'
      ICODE = 1
      DO 30 I = 1,NCODE
         IF (OPTYPE.EQ.CODE(I)) ICODE = I
 30      CONTINUE
      OPTYPE = CODE(ICODE)
      CALL CHR2H (4, CODE(ICODE), 1, XOPTY)
      MUMPAR = 1
      IF (ICODE.LE.2) MUMPAR = 2
      ITPLOT = 0
      DO 35 I = 1,NTPLT
         IF (OPCODE.EQ.TPLOT(I)) ITPLOT = I
 35      CONTINUE
      IF (ITPLOT.GT.0) CALL CHR2H (4, TPLOT(ITPLOT), 1, XOPCOD)
      IF ((ITPLOT.LE.0) .OR. (ITPLOT.GT.2)) DO3COL = -1.0
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
      IF ((LTYPE.EQ.0) .OR. (ABS(LTYPE).GT.10)) LTYPE = 3
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
         MSGTXT = 'ONLY 1 IF => MEANINGLESS PLOT'
         IERR = 1
         GO TO 990
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
         DO 50 K = 1,NANTSL
            IF (ANTS(J).EQ.ANTS(K)) ANTS(J) = 0
 50         CONTINUE
C                                       Check for multiple entries
         IF (ABS (ANTS(J)).GE.1) NANTSL = J
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
      RATFAC = FREQ
      IF (OPTYPE.EQ.'RATE') THEN
         VER = 1
         LUN = 25
         IF (FRQSEL.LE.0) FRQTMP = 1
         IF (FRQSEL.GT.0) FRQTMP = FRQSEL
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, VER, CATBLK, LUN,
     *      NIF, FOFF, ISBAND, FINC, BNDCOD, FRQTMP, JERR)
         IF (JERR.EQ.0) RATFAC = FREQ + FOFF(BIF)
         END IF
C                                       Check Stokes' (R or IPOL)
C                                       Set stokes request
      NSTOK = CATBLK(KINAX+JLOCS)
      MUMPOL = 1
      IF ((ICOR0.EQ.1) .OR. (ICOR0.EQ.-2) .OR. ((ICOR0.EQ.-1) .AND.
     *   (NSTOK.EQ.1))) THEN
         ISTOK = ABS (ICOR0)
         SUMSTK = 1
         XSTOK = 'I'
         IF (ICOR0.EQ.-2) XSTOK='L'
         IF (ICOR0.EQ.-1) XSTOK='R'
      ELSE IF ((XSTOK.EQ.'R') .OR. (XSTOK.EQ.'RR')) THEN
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
      IF ((TYPE.EQ.'SY') .AND. (OPTYPE.EQ.'PSYS')) THEN
         J = 0
         CALL GETCDS (DISKIN, CNOIN, J, SUBARR, FRQSEL, CATBLK, TCAL,
     *      JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      XNVER = IVER
      MUMIF = EIF - BIF + 1
      XBIF = BIF
      XEIF = EIF
      I = MAXANT
      XXMIN = BIF
      XXMAX = EIF
      IF (MUMPOL.EQ.2) XXMAX = XXMAX + EIF - BIF
      CALL RFILL (I, 1.E8, YYMIN)
      CALL RFILL (I, -1.E8, YYMAX)
      CALL RFILL (I, 1.E8, PPMIN)
      CALL RFILL (I, -1.E8, PPMAX)
      YYMX = -1.E8
      YYMN = 1.E8
      NOSCAN = 0
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
      INCLUDE 'SNIFS.INC'
C
      INTEGER MAXPCC
      PARAMETER (MAXPCC = 40)
      CHARACTER KEYW(4)*8, COLHD1(11)*24, COLHD2(13)*24, COLHD3(13)*24,
     *   COLTAB(40)*24, COLHED(37)*24, COLPC(MAXPCC)*24, KEYSN(4)*8,
     *   KEYPC(3)*8, COLPC1(20)*24, COLPC2(20)*24
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), IPOINT, KEYTYP(4),
     *   KLOCS(4), KEYVAL(6), I, KP, MSGSAV
      LOGICAL   T
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
      IF ((TYPE.EQ.'SY') .AND. (ICODE.LT.11)) THEN
         MSGTXT = 'DATA OF TYPE ' // OPTYPE // ' NOT IN SY TABLES'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.LE.4) .AND. (TYPE.EQ.'TY')) THEN
         MSGTXT = 'DATA OF TYPE ' // OPTYPE // ' NOT IN TY TABLES'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.5) .AND. ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC'))) THEN
         MSGTXT = 'NO SINGLEBAND DELAY IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.6) .AND. (TYPE.EQ.'TY')) THEN
         MSGTXT = 'NO RESIDUAL RATE IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.7) .AND. ((TYPE.EQ.'TY') .OR. (TYPE.EQ.'PC'))) THEN
         MSGTXT = 'NO SNR IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.8) .AND. (TYPE.NE.'CL')) THEN
         MSGTXT = 'NO DOPPLER OFFSET IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.9) .AND. (TYPE.NE.'TY')) THEN
         MSGTXT = 'NO TSYS IN THIS TABLE TYPE - CHECK INEXT'
         IERR = 5
         GO TO 980
         END IF
      IF ((ICODE.EQ.10) .AND. (TYPE.NE.'TY')) THEN
         MSGTXT = 'NO TANT IN THIS TABLE TYPE - CHECK INEXT'
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
      IF (ISTOK.EQ.ABS (ICOR0)) THEN
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
      IF ((ICODE.EQ.1) .OR. (ICODE.EQ.2) .OR. (ICODE.EQ.3) .OR.
     *   (ICODE.EQ.4) ) THEN
         IF ((REKOL(1).LT.0) .AND. (IMKOL(1).LT.0)) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. ((REKOL(2).LT.0) .OR. (IMKOL(2).LT.0)))
     *      GO TO 500
         IF (ICODE.EQ.1) THEN
            SUMSTK = MIN (SUMSTK, 3)
            END IF
C                                       Singleband Delay
      ELSE IF (ICODE.EQ.5) THEN
         IF (DLKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (DLKOL(2).LT.0)) GO TO 500
         SUMSTK = MIN (SUMSTK, 3)
C                                       Rate
      ELSE IF (ICODE.EQ.6) THEN
         IF (RAKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (RAKOL(2).LT.0)) GO TO 500
         SUMSTK = MIN (SUMSTK, 3)
C                                       SNR
      ELSE IF (ICODE.EQ.7) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
C                                       Doppler offset
      ELSE IF (ICODE.EQ.8) THEN
         IF (DOPLKL.LT.0) GO TO 500
C                                       Only 1 value
         MUMPOL = 1
         SUMSTK = 0
         CUTOFF = -1000.0
C                                       System temperature
      ELSE IF (ICODE.EQ.9) THEN
         IF (TSKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (TSKOL(2).LT.0)) GO TO 500
C                                       IF Antenna Temp
      ELSE IF (ICODE.EQ.10) THEN
         IF (TAKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (TAKOL(2).LT.0)) GO TO 500
C                                       SY Pdif
      ELSE IF (ICODE.EQ.11) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
C                                       SY Psum
      ELSE IF (ICODE.EQ.12) THEN
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
C                                       SY post gain
      ELSE IF (ICODE.EQ.13) THEN
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
C                                       SY Pon and Poff
      ELSE IF ((ICODE.EQ.14) .OR. (ICODE.EQ.15)) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
         IF (WTKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (WTKOL(2).LT.0)) GO TO 500
C                                       SY Pon and Poff
      ELSE IF (ICODE.EQ.16) THEN
         IF (REKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (REKOL(2).LT.0)) GO TO 500
         IF (IMKOL(1).LT.0) GO TO 500
         IF ((MUMPOL.EQ.2) .AND. (IMKOL(2).LT.0)) GO TO 500
C                                       OPCODE not okay
      ELSE
         IERR = 10
         MSGTXT = 'IMPROPER OPTYPE ''' // OPTYPE // ''''
         GO TO 980
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
      SUBROUTINE SNPCNT (NWORDS, IERR)
C-----------------------------------------------------------------------
C   SNPCNT reads the SN or CL table to find the number of samples for
C   each antenna
C   Input:
C      NA       I      Number antennas
C   Input/Output in common:
C      TSTART   R      Start time of plot
C      TSTOP    R      Stop time of plot
C   Output:
C      IERR     I      Error code, 0=OK else failed
C   Outputs in common:
C-----------------------------------------------------------------------
      INTEGER   NWORDS, IERR
C
      LOGICAL   NODATA, OKAY
      INTEGER   I, NP, IFNUM
      REAL      TB, TE, GTIME
      INCLUDE 'SNIFS.INC'
      REAL      VALUE(2*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL FILL (MAXANT, 0, NANREC)
      NODATA = .TRUE.
      TB = 1.0E5
      TE = -1.0E5
      IF ((TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TB = TSTART
         TE = TSTOP
         END IF
      XMX = TE
      XMN = TB
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
         CALL SNPDAT (VALUE, OKAY)
C                                       Max. - Min
         IF (((OKAY) .OR. (BSYM.GT.0))) THEN
            IF (OKAY) NODATA = .FALSE.
            NANREC(IANT) = NANREC(IANT) + 1
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
C      NV       I      Number values per time
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
      INTEGER   I, NP, NN, IP, IIF, IIS, IFNUM
      REAL      TB, TE, TMAX, TMIN, GTIME, PHASE, AMP
      INCLUDE 'SNIFS.INC'
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
      IF ((TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TB = TSTART
         TE = TSTOP
         END IF
      XMX = TE
      XMN = TB
C                                       Loop thru data
      TINT = -1.0
      IF (INTKOL.LE.0) TINT = 10.0 / 86400.0
      NP = MUMPAR * MUMPOL * MUMIF
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
         CALL SNPDAT (VALUE, OKAY)
C                                       Max. - Min
         IF (((OKAY) .OR. (BSYM.GT.0))) THEN
            IF (OKAY) NODATA = .FALSE.
C                                       Put in array
            NUMPTS(IANT) = NUMPTS(IANT) + 1
            NN = FANREC(IANT) + NUMPTS(IANT) - 1
            PLTPTS(1,NN) = GTIME
            CALL RCOPY (NP, VALUE, PLTPTS(2,NN))
C                                       If not a summary plot
            IP = 1 - MUMPAR
            DO 90 IIF = 1,MUMIF
               DO 85 IIS = 1,MUMPOL
                  IP = IP + MUMPAR
                  IF (VALUE(IP).NE.FBLANK) THEN
                     IF (ICODE.EQ.1) THEN
                        AMP = SQRT (VALUE(IP)*VALUE(IP) +
     *                     VALUE(IP+1)*VALUE(IP+1))
                        YYMX = MAX (YYMX, AMP)
                        YYMN = MIN (YYMN, AMP)
                        YYMIN(IANT) = MIN (AMP, YYMIN(IANT))
                        YYMAX(IANT) = MAX (AMP, YYMAX(IANT))
                     ELSE IF (ICODE.EQ.2) THEN
                        PHASE = 57.296 *
     *                     ATAN2 (VALUE(IP+1), VALUE(IP) + 1.0E-20)
                        IF (PHASE.LE.-180.0) THEN
                           PHASE = PHASE + 360.
                        ELSE IF (PHASE.GT.180.) THEN
                           PHASE = PHASE - 360.
                           END IF
                        YYMX = MAX (YYMX, PHASE)
                        YYMN = MIN (YYMN, PHASE)
                        YYMIN(IANT) = MIN (PHASE, YYMIN(IANT))
                        YYMAX(IANT) = MAX (PHASE, YYMAX(IANT))
                        IF (PHASE.LT.0.0) PHASE = PHASE +
     *                     360.0
                        PPMIN(IANT) = MIN (PHASE, PPMIN(IANT))
                        PPMAX(IANT) = MAX (PHASE, PPMAX(IANT))
                     ELSE
                        YYMX = MAX (YYMX, VALUE(IP))
                        YYMN = MIN (YYMN, VALUE(IP))
                        YYMIN(IANT) = MIN (VALUE(IP), YYMIN(IANT))
                        YYMAX(IANT) = MAX (VALUE(IP), YYMAX(IANT))
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
            IF (YYMAX(IANT).GE.YYMIN(IANT)) THEN
               YYMAX(IANT) = PIXR(2)
               YYMIN(IANT) = PIXR(1)
               IF (ICODE.EQ.2) THEN
                  PPMAX(IANT) = PIXR(2)
                  PPMIN(IANT) = PIXR(1)
                  END IF
               END IF
 120        CONTINUE
         END IF
C                                       Set actual X range
      XSTART = TB
      XSTOP = TE
C                                       Check for no data
      IF (NODATA) THEN
         IERR = 6
         MSGTXT = 'NO DATA SELECTED'
         GO TO 990
         END IF
      TSTART = TB
      TSTOP = TE
      TMIN = 0.0
      TMAX = MUMPOL * (MUMIF + 1)
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
      SUBROUTINE SNPDAT (VALUE, OKAY)
C-----------------------------------------------------------------------
C   Routine to get the specified value from a SN/CL/TY table entry
C   Input from common:
C      GNREC    R(*)  Table record
C      ICODE    I     Plot code
C      SUMSTK   I     Selected Stokes 0=both, 1=R, 2=L, 3=Difference
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R(*)   Table value, magic value blanked (amp on ICODE 6)
C      OKAY     L      Some values are good
C-----------------------------------------------------------------------
      REAL      VALUE(*)
      LOGICAL   OKAY
C
      INTEGER   IIS, IIF, IP1, LP, JP1, KP1
      REAL      V, S, TC
      INCLUDE 'SNIFS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       In case the data is bad
      LP = MUMPAR * MUMPOL * MUMIF
      CALL RFILL (LP, FBLANK, VALUE)
C                                       Select data type
C                                       Phase (deg), ampl
C                                       return Re and Im
      IF (ICODE.LE.2) THEN
         DO 110 IIS = 1,MUMPOL
            LP = 2*IIS - 1 - 2*MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            DO 105 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               LP = LP + 2*MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND. (GNREC(JP1).NE.FBLANK))
     *            THEN
                  VALUE(LP) = GNREC(IP1)
                  VALUE(LP+1) = GNREC(JP1)
                  END IF
 105           CONTINUE
 110        CONTINUE
C                                       Delay (sec)
      ELSE IF (ICODE.EQ.5) THEN
         DO 150 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = DLKOL(IIS) - 1
            DO 145 IIF = 1,MUMIF
               IP1 = IP1 + 1
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 145           CONTINUE
 150        CONTINUE
C                                       Rate (Hz)
      ELSE IF (ICODE.EQ.6) THEN
         DO 170 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = RAKOL(IIS) - NTONE
            DO 165 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1) * RATFAC
                  END IF
 165           CONTINUE
 170        CONTINUE
C                                       System temperature (K)
      ELSE IF (ICODE.EQ.9) THEN
         DO 190 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = TSKOL(IIS) - 1
            DO 185 IIF = 1,MUMIF
               IP1 = IP1 + 1
               LP = LP + MUMPOL
               IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 185           CONTINUE
 190        CONTINUE
C                                       SNR (no units)
      ELSE IF (ICODE.EQ.7) THEN
         DO 250 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = WTKOL(IIS) - 1
            DO 245 IIF = 1,MUMIF
               IP1 = IP1 + 1
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 245           CONTINUE
 250        CONTINUE
C                                       Doppler offset (Hz)
      ELSE IF (ICODE.EQ.8) THEN
         LP = IIS - 1
         IP1 = DOPLKL - 1
         DO 255 IIF = 1,MUMIF
            IP1 = IP1 + 1
            LP = LP + 1
            IF (GNREC(IP1).NE.FBLANK) VALUE(LP) = GNREC(IP1)
 255        CONTINUE
C                                       Tant (K)
      ELSE IF (ICODE.EQ.10) THEN
         DO 290 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = TAKOL(IIS) - 1
            DO 285 IIF = 1,MUMIF
               IP1 = IP1 + 1
               LP = LP + MUMPOL
               IF (ABS(GNREC(IP1)-999.0).LT.0.1) GNREC(IP1) = FBLANK
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 285           CONTINUE
 290        CONTINUE
C                                       REAL
      ELSE IF (ICODE.EQ.3) THEN
         DO 395 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            DO 390 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               LP = LP + MUMPOL
               IF ( GNREC(IP1).NE.FBLANK ) THEN
                  VALUE(LP) = GMMOD * GNREC(IP1)
                  END IF
 390           CONTINUE
 395        CONTINUE
C                                       IMAG
      ELSE IF (ICODE.EQ.4) THEN
         DO 420 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            JP1 = IMKOL(IIS) - NTONE
            DO 415 IIF = 1,MUMIF
               JP1 = JP1 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(JP1).NE.FBLANK)
     *            THEN
                  VALUE(LP) = GMMOD * GNREC(JP1)
                  END IF
 415           CONTINUE
 420        CONTINUE
C                                       PDIF, PSUM, PGN
      ELSE IF ((ICODE.GE.11) .AND. (ICODE.LE.13)) THEN
         DO 430 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IF (ICODE.EQ.11) THEN
               IP1 = REKOL(IIS) - NTONE
            ELSE IF (ICODE.EQ.12) THEN
               IP1 = IMKOL(IIS) - NTONE
            ELSE
               IP1 = WTKOL(IIS) - NTONE
               END IF
            DO 425 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               LP = LP + MUMPOL
               IF (GNREC(IP1).NE.FBLANK) THEN
                  VALUE(LP) = GNREC(IP1)
                  END IF
 425           CONTINUE
 430        CONTINUE
C                                       PON, POFF
      ELSE IF ((ICODE.GE.14) .AND. (ICODE.LE.15)) THEN
         S = 1.0
         IF (ICODE.EQ.15) S = -1.0
         DO 440 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            KP1 = WTKOL(IIS) - NTONE
            DO 435 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               KP1 = KP1 + NTONE
               LP = LP + MUMPOL
               IF ((GNREC(IP1).NE.FBLANK) .AND. (GNREC(JP1).NE.FBLANK)
     *            .AND. (GNREC(KP1).NE.FBLANK) .AND.
     *            (GNREC(KP1).NE.0.0)) THEN
                  V = (GNREC(JP1) + S*GNREC(IP1)) / (2.0 * GNREC(KP1))
                  VALUE(LP) = V
                  END IF
 435           CONTINUE
 440        CONTINUE
C                                       PSUM / PDIF = Tsys/Tcal
      ELSE IF (ICODE.EQ.16) THEN
         DO 450 IIS = 1,MUMPOL
            LP = IIS - MUMPOL
            IP1 = REKOL(IIS) - NTONE
            JP1 = IMKOL(IIS) - NTONE
            DO 445 IIF = 1,MUMIF
               IP1 = IP1 + NTONE
               JP1 = JP1 + NTONE
               LP = LP + MUMPOL
               IF ((CKKOL(1).GT.0) .AND. (GNRECI(CKKOL(1)).EQ.1)) THEN
                  TC = TCAL(ISTOK+IIS+1,IIF-1+BIF,IANT)
               ELSE
                  TC = TCAL(ISTOK+IIS-1,IIF-1+BIF,IANT)
                  END IF
               IF (TC.LE.0.0) TC = FBLANK
               IF ((GNREC(IP1).NE.FBLANK) .AND. (GNREC(JP1).NE.FBLANK)
     *            .AND. (GNREC(IP1).GT.0.0) .AND. (TC.NE.FBLANK)) THEN
                  V = GNREC(JP1) / GNREC(IP1) / 2.0 * TC
                  VALUE(LP) = V
                  END IF
 445           CONTINUE
 450        CONTINUE
         END IF
C
      OKAY = .TRUE.
      LP = MUMPAR * MUMPOL * MUMIF
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
      INTEGER   IPLOT, NPLT, MANT, IA, NTIMES, IT
      REAL      TRA(2)
      LOGICAL   DOIT
      INCLUDE 'SNIFS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   STREC(MAXANT), INREC(MAXANT), ENREC(MAXANT)
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
      MANT = NA
      IF (ITPLOT.EQ.1) MANT = 1
      DO 10 IA = 1,NA
         ENREC(IA) = FANREC(IA) + NANREC(IA) - 1
 10      CONTINUE
C                                       count the plots
      NPLOTS = 0
      NTIMES = 0
      CALL COPY (NA, FANREC, STREC)
 50   NTIMES = NTIMES + 1
         CALL FILL (NA, 0, INREC)
         CALL FINDNR (ITPLOT, NA, NV, PLTPTS, STREC, ENREC, SOLINT,
     *      INREC, DOIT)
         IF (DOIT) THEN
            DO 60 IA = 1,MANT
               IF (ITPLOT.EQ.1) THEN
                  NPLOTS = NPLOTS + 1
               ELSE IF (INREC(IA).GT.0) THEN
                  NPLOTS = NPLOTS + 1
                  END IF
 60            CONTINUE
            DO 70 IA = 1,NA
               IF (INREC(IA).GT.0) STREC(IA) = INREC(IA) + 1
 70            CONTINUE
            GO TO 50
            END IF
C                                       Now plot
      NPLT = 0
      CALL COPY (NA, FANREC, STREC)
 100  CALL FILL (NA, 0, INREC)
         CALL FINDNR (ITPLOT, NA, NV, PLTPTS, STREC, ENREC, SOLINT,
     *      INREC, DOIT)
         IF (DOIT) THEN
            DO 120 IA = 1,MANT
               DOIT = .FALSE.
               IF (ITPLOT.EQ.1) THEN
                  CALL GETSCL (NA, NV, PLTPTS, STREC, INREC, TRA, DOIT)
               ELSE IF (INREC(IA).GT.0) THEN
                  CALL GETSCL (1, NV, PLTPTS, STREC(IA), INREC(IA), TRA,
     *               DOIT)
                  END IF
               IF (DOIT) THEN
                  NPLT = NPLT + 1
                  IPLOT = MOD (NPLT-1, NCOUNT) + 1
                  IF (NPLT.EQ.NPLOTS) IPLOT = -IPLOT
                  IF (ITPLOT.EQ.2) THEN
                     IT = INREC(IA) - STREC(IA) + 1
                  ELSE IF (ITPLOT.EQ.1) THEN
                     IT = NA
                  ELSE
                     IT = 1
                     END IF
                  CALL PLTSN (IPLOT, IT, IA, TRA, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 120           CONTINUE
            DO 130 IA = 1,NA
               IF (INREC(IA).GT.0) STREC(IA) = INREC(IA) + 1
 130           CONTINUE
            GO TO 100
            END IF
C
 999  RETURN
      END
      SUBROUTINE FINDNR (ITPLOT, NA, NV, PLTPTS, STREC, ENREC, SOLINT,
     *   INREC, DOIT)
C-----------------------------------------------------------------------
C   FINDNR lookes in the data for the next solint interval
C   Inputs:
C      ITPLOT   I      =2 : limit to <= MAXANT times
C      NA       I      Number antennas
C      NV       I      Number values per data sample
C      PLTPTS   R(*)   Data sample array (1,*) is time
C      STREC    I(*)   Start point in array for each antenna
C      ENREC    I(*)   Max rec number allowed for each antenna
C      SOLINT   R      Averaging interval in days
C   Output:
C      INREC    I(*)   Highest record this integration
C      DOIT     L      False => we are all the way done
C-----------------------------------------------------------------------
      INTEGER   ITPLOT, NA, NV, STREC(*), ENREC(*), INREC(*)
      REAL      PLTPTS(NV,*), SOLINT
      LOGICAL   DOIT
C
      INTEGER   IA, IREC
      REAL      TMIN, TT, TE
      INCLUDE 'INCS:PUVD.INC'
C-----------------------------------------------------------------------
C                                       find min time in data set
      TMIN = 1.E10
      DO 20 IA = 1,NA
         IF (STREC(IA).LE.ENREC(IA)) THEN
            TT = PLTPTS(1,STREC(IA))
            TMIN = MIN (TT, TMIN)
            END IF
 20      CONTINUE
      DOIT = .FALSE.
      IF (TMIN.LE.1.E9) THEN
         TE = TMIN + SOLINT
         DO 40 IA = 1,NA
            INREC(IA) = 0
            DO 30 IREC = STREC(IA),ENREC(IA)
               TT = PLTPTS(1,IREC)
               IF (TT.GE.TE) GO TO 40
               INREC(IA) = IREC
               DOIT = .TRUE.
               IF ((ITPLOT.EQ.2) .AND.
     *            (INREC(IA)-STREC(IA)+1.EQ.MAXANT)) GO TO 40
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETSCL (NA, NV, PLTPTS, STREC, INREC, TRA, DOIT)
C-----------------------------------------------------------------------
C   GETSCL looks at the data for the next plot - prepares time averages
C   if needed and sets the plot scale
C   Inputs:
C      NA       I      Number of antennas to do
C      NV       I      Number of values per sample
C      PLTPTS   R(*)   Data array (NV,*)
C      STREC    I(*)   record number to start
C      INREC    I(*)   record number to end
C   Output:
C      TRA      R(2)   Time range of data
C      DOIT     L      There were valid values
C   Output in common
C      PLTAVG  R(*)    Data to plot (maxif,2,maxant)
C      XYSCL   R(2)    Scaling - only 2nd one changed
C      XYOFF   R(2)    Offset  - only second one changed
C-----------------------------------------------------------------------
      INTEGER   NA, NV, STREC(*), INREC(*)
      LOGICAL   DOIT
      REAL      PLTPTS(NV,*), TRA(2)
C
      INCLUDE 'SNIFS.INC'
      INTEGER   IIS, IIF, IP, IREC, IA
      REAL      SR, SI, SW, V, P, YMX, YMN, PMX, PMN, TMAX, TMIN, TDIF,
     *   TOLER(16), SIZEY, SA
      INCLUDE 'INCS:DDCH.INC'
C                                       Minimum value range for each
C                                       ICODE
C                  amp     phs   real   imag   delay    rate    snr
      DATA TOLER /0.001, 0.001, 0.001, 0.001, 1.0E-12, 1.0E-8, 0.001,
C                  dopl   Tsys   Tant      Pdif   Psum   Pgn    Pon
     *            0.001, 0.001, 0.000001, 0.001, 0.001, 1.E-5, 0.01,
C                 Poff  Psys
     *            0.01, 0.01/
C-----------------------------------------------------------------------
      DOIT = .FALSE.
      YMX = -1.E8
      YMN = -YMX
      PMX = YMX
      PMN = YMN
      TRA(1) = 1.E8
      TRA(2) = -1.E8
C                                       plot several times
      IF (ITPLOT.EQ.2) THEN
         IP = 2 - MUMPAR
         DO 30 IIF = 1,MUMIF
            DO 20 IIS = 1,MUMPOL
               IP = IP + MUMPAR
               DO 10 IREC = STREC(1),INREC(1)
                  V = PLTPTS(IP,IREC)
                  IF (V.NE.FBLANK) THEN
                     TRA(1) = MIN (TRA(1), PLTPTS(1,IREC))
                     TRA(2) = MAX (TRA(2), PLTPTS(1,IREC))
                     IF (ICODE.EQ.1) THEN
                        V = SQRT (V*V + PLTPTS(IP+1,IREC)**2)
                     ELSE IF (ICODE.EQ.2) THEN
                        V = 57.296 * ATAN2 (PLTPTS(IP+1,IREC), V+1.E-20)
                        IF (V.GT.180.0) V = V - 360.
                        IF (V.LE.-180.0) V = V + 360.
                        P = V
                        IF (P.LT.0.0) P = P + 360.
                        PMX = MAX (PMX, P)
                        PMN = MIN (PMN, P)
                        END IF
                     YMX = MAX (YMX, V)
                     YMN = MIN (YMN, V)
                     END IF
                  PLTAVG(IIF,IIS,IREC-STREC(1)+1) = V
 10               CONTINUE
 20            CONTINUE
 30         CONTINUE
C                                       average over time
      ELSE
         DO 100 IA = 1,NA
            IP = 2 - MUMPAR
            DO 90 IIF = 1,MUMIF
               DO 80 IIS = 1,MUMPOL
                  IP = IP + MUMPAR
                  SR = 0.0
                  SI = 0.0
                  SW = 0.0
                  SA = 0.0
                  DO 50 IREC = STREC(IA),INREC(IA)
                     V = PLTPTS(IP,IREC)
                     IF (V.NE.FBLANK) THEN
                        TRA(1) = MIN (TRA(1), PLTPTS(1,IREC))
                        TRA(2) = MAX (TRA(2), PLTPTS(1,IREC))
                        IF (ICODE.LE.2) SI = SI + PLTPTS(IP+1,IREC)
                        IF (ICODE.EQ.1) SA = SA + SQRT (V*V +
     *                     PLTPTS(IP+1,IREC)*PLTPTS(IP+1,IREC))
                        SR = SR + V
                        SW = SW + 1.0
                        END IF
 50                  CONTINUE
                  IF (SW.GT.0.0) THEN
                     SR = SR / SW
                     SI = SI / SW
                     SA = SA / SW
                     IF (ICODE.EQ.1) THEN
                        IF (SCALAR) THEN
                           V = SA
                        ELSE
                           V = SQRT (SR*SR + SI*SI)
                           END IF
                     ELSE IF (ICODE.EQ.2) THEN
                        V = 57.296 * ATAN2 (SI, SR+1.E-20)
                        IF (V.GT.180.0) V = V - 360.
                        IF (V.LE.-180.0) V = V + 360.
                        P = V
                        IF (P.LT.0.0) P = P + 360.
                        PMX = MAX (PMX, P)
                        PMN = MIN (PMN, P)
                     ELSE
                        V = SR
                        END IF
                     YMX = MAX (YMX, V)
                     YMN = MIN (YMN, V)
                  ELSE
                     V = FBLANK
                     END IF
                  PLTAVG(IIF,IIS,IA) = V
 80               CONTINUE
 90            CONTINUE
 100        CONTINUE
         END IF
C
      DOIT = YMX.GE.YMN
      IF (PIXR(2).GT.PIXR(1)) THEN
         YMX = PIXR(2)
         YMN = PIXR(1)
      ELSE IF (ICODE.EQ.2) THEN
         IF (PMX-PMN.LT.YMX-YMN) THEN
            YMX = PMX
            YMN = PMN
            END IF
         END IF
      SIZEY = 1000.0 / NCOUNT
      TMAX = YMX + 0.1 * (YMX - YMN)
      TMIN = YMN - 0.1 * (YMX - YMN)
      IF (ABS (TMAX-TMIN) .LT. TOLER(ICODE)) THEN
         TMAX = TMAX + TOLER(ICODE)
         TMIN = TMIN - TOLER(ICODE)
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
      SUBROUTINE PLTSN (IPLOT, NA, IA, TRA, IRET)
C-----------------------------------------------------------------------
C   PLTSN actually plots data.
C   Input:
C      IPLOT    I      Plot number on current page. If neg. then this is
C                      last plot.
C      NA       I      Number of antennas or times to plot
C      IA       I      antenna number to label
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                       -1 => user request termination
C                        1 => failed to add to catalog
C                        2 => failed to create
C                        3 => graph file write error
C                        4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, NA, IA, IRET
      REAL      TRA(2)
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(18)*8,
     *   CHTYPE(18)*16, CHTMP*18, TSIGN*1, STRNG*32
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, JCODE, IIF, ILITY, TIME(3,2), JA, IIP
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   DBY, COLV, COL(3), COLR, AX(5), AY(5), XL, TSEC(2)
      LOGICAL   T, F, GOOD, CATUP, DONEG, DO3C, BLNKD
      SAVE BUFFER
      INCLUDE 'SNIFS.INC'
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
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHTYPE /'Gain amp', 'Gain phs', 'Real', 'Imag', 'Delay',
     *             'Rate', 'SNR', 'Doppler offset', 'Tsys', 'Tant',
     *             'Power difference', 'Power sum', 'Post gain',
     *             'Power NT on', 'Power NT off', 'Tsys',
     *             'Phase-cal Phase', 'Phase-cal Ampl'/
      DATA AUNITS /'Gain', 'Degrees', 'Gain', 'Gain', 'Seconds',
     *             'Hz', 'SNR', 'Hz', 'Kelvin', 'Kelvin',
     *             'Counts', 'Counts', 'Gain',
     *             'Counts', 'Counts', 'Kelvin',
     *             'Degrees', 'PCamp'/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C
      JCODE = ICODE
      IF (TYPE.EQ.'PC') THEN
         IF (JCODE.EQ.2) JCODE = 17
         IF (JCODE.EQ.1) JCODE = 18
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
         ITYPE = 44
         SOLINT = SOLINT * (24.0 * 3600.0)
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         SOLINT = SOLINT / (24.0 * 3600.0)
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
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (YYMX - YYMN)
      IF (TR.LE.0.0) TR = 1.0
      TI = TR
      CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
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
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         TEXT(INP:) = ' vs IF for '
         INP = INP + 11
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
         IF (TYPE.EQ.'PC') THEN
            WRITE (TEXT(INP:),1015) PCNUM
            INP = INP + 9
            END IF
C                                       Stokes
         IF (SUMSTK.GT.0) THEN
            TEXT(INP:) = XSTOK(:1) // 'pol_'
            INP = INP + 7
         ELSE
            TEXT(INP:) = 'Rpol & Lpol_'
            INP = INP + 14
            END IF
C                                       Phase-cal tone
         IF ((TYPE.EQ.'PC') .AND. ((JCODE.EQ.24) .OR.
     *         (JCODE.EQ.25))) THEN
            WRITE (TEXT(INP:),1060) PCNUM
            END IF
C                                       time range
         IF (ITPLOT.NE.1) THEN
            TEXT(INP:) = 'Time'
            INP = INP + 5
            CALL TFDHMS (TRA(1), 1, TSIGN, TIME(1,1), TSEC(1))
            CALL TFDHMS (TRA(2), 1, TSIGN, TIME(1,2), TSEC(2))
            WRITE (STRNG,1020) TIME(1,1), TIME(2,1), TIME(3,1),
     *         TSEC(1), TIME(1,2), TIME(2,2), TIME(3,2), TSEC(2)
            IF (TIME(1,1)+TIME(1,2).LE.0) THEN
               STRNG(1:3) = ' '
               STRNG(16:18) = ' '
               END IF
            IF (STRNG(10:10).EQ.' ') STRNG(10:10) = '0'
            IF (STRNG(25:25).EQ.' ') STRNG(25:25) = '0'
            TEXT(INP:) = STRNG
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
C                                       station ID, not on ALAN
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  3.0
      DY = -2.5
      IF (ITPLOT.NE.1) THEN
         WRITE (TEXT,1040) IA
         INP = 4
         IF (MUMPOL.LE.1) THEN
            TEXT(INP:) = XSTOK(:1)
            INP = INP + 1
            END IF
         TEXT(INP+1:) = STNNAM(IA)
         CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       time range
      ELSE
         INP = 1
         TEXT(INP:) = 'Time'
         INP = INP + 5
         CALL TFDHMS (TRA(1), 1, TSIGN, TIME(1,1), TSEC(1))
         CALL TFDHMS (TRA(2), 1, TSIGN, TIME(1,2), TSEC(2))
         WRITE (STRNG,1020) TIME(1,1), TIME(2,1), TIME(3,1),
     *      TSEC(1), TIME(1,2), TIME(2,2), TIME(3,2), TSEC(2)
         IF (TIME(1,1)+TIME(1,2).LE.0) THEN
            STRNG(1:3) = ' '
            STRNG(16:18) = ' '
            END IF
         IF (STRNG(10:10).EQ.' ') STRNG(10:10) = '0'
         IF (STRNG(25:25).EQ.' ') STRNG(25:25) = '0'
         TEXT(INP:) = STRNG
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Set up location common
C                                       Blank bottom label.
      CPREF(1,LOCNUM) = ' '
      CTYP(1,LOCNUM) = 'NO TICKS'
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       x axis ticks
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CTYP(1,LOCNUM) = ' '
      ELSE
         CTYP(1,LOCNUM) = 'IFS'
         END IF
      CALL SNIFT (XBLC, XTRC, BIF, EIF, MUMPOL, LABEL, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       center line
      XL = (MUMIF + 1) * MUMPOL
      XL = (XTRC(1) - XBLC(1)) / XL
      IF (MUMPOL.GT.1) THEN
         XY(1) = (MUMIF+1) * XL + XBLC(1)
         CALL GPOS (XY, BLC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (XY, TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Size of symbol.
      DBY = 0.5 * FACTOR
C                                       Loop
      ILITY = 4
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO3C = (DO3COL.GT.0.0) .AND. (NA.GT.1)
      COLR = NA - 1.0
C                                       Outer loop: IF, stokes
      COLV = 0.0
      DO 200 JA = 1,NA
         IF (DO3C) THEN
            CALL COLOR3 (COLV, .FALSE., COL)
            CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            COLV = COLV + 0.97 / COLR
            END IF
         DO 130 IIP = 1,MUMPOL
            DO 120 IIF = 1,MUMIF
               DONEG = (ICODE.EQ.2)
               XY(1) = (IIF + (IIP-1) * (MUMIF+1)) * XL + XBLC(1)
               VALUE = PLTAVG(IIF,IIP,JA)
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
                     IF ((.NOT.DO3C) .AND. (ILITY.NE.4)) THEN
                        ILITY = 4
                        CALL GLTYPE (ILITY, BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        END IF
                     CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE.,
     *                  DO3C, BUFFER, IERR)
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
                  IF ((.NOT.DO3C) .AND. (ILITY.NE.3)) THEN
                     ILITY = 3
                     CALL GLTYPE (ILITY, BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  CALL PNTPLT (BSYM, AX, AY, XBLC, XTRC, .FALSE.,
     *               DO3C, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
 120           CONTINUE
 130        CONTINUE
C                                       Line plot
         IF (DOLINE) THEN
            DO 160 IIP = 1,MUMPOL
               BLNKD = .TRUE.
               DO 150 IIF = 1,MUMIF
                  DONEG = (ICODE.EQ.2)
                  XY(1) = (IIF + (IIP-1) * (MUMIF+1)) * XL + XBLC(1)
                  VALUE = PLTAVG(IIF,IIP,JA)
                  IF (VALUE.EQ.FBLANK) THEN
                     BLNKD = .TRUE.
                  ELSE
 140                 XY(2) = VALUE
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
                           GO TO 140
                           END IF
C                                       Mark point
                     ELSE
                        IF (BLNKD) THEN
                           CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                           BLNKD = .FALSE.
                        ELSE IF (DO3C) THEN
                           CALL G3VEC (XY(1), XY(2), BUFFER, IERR)
                        ELSE
                           CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                           END IF
                        IF (IERR.NE.0) GO TO 970
                        END IF
                     END IF
 150              CONTINUE
 160           CONTINUE
            END IF
 200     CONTINUE
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF ((IPLOT.LE.0) .OR. (ABS(IPLOT).GE.NCOUNT)) THEN
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
         END IF
      IF (IERR.GT.0) GO TO 975
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
      IF (IERR.EQ.0) THEN
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
         END IF
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
 1020 FORMAT (I2,'/',2(I2.2,':'),F4.1,' -',I2,'/',2(I2.2,':'),F4.1)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I3)
 1060 FORMAT ('_Phase tone ',I2)
 1200 FORMAT ('PLTSN:',I9,' points plotted')
 1202 FORMAT ('PLTSN:',I9,' points did not fit')
 1960 FORMAT ('PLTSN: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTSN: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE SNIFT (BLC, TRC, BC, EC, NG, ILTYPE, BUFFER, IERR)
C-----------------------------------------------------------------------
C   To do X axis where we have multiple sub panels with integer counts
C   Inputs:
C      BLC      R(2)    X, Y pixels to form bottom left hand corner
C      TRC      R(2)    X, Y pixels to form the top right hand corner
C      BC       I       Begin count
C      EC       I       End count
C      NG       I       Number of such groups
C      ILTYPE   I       label type: 1 none, 2 no ticks, 3 RA/DEC
C                          4 center relative
C   In/out:
C      BUFFER   I(256)  the updated graphics output buffer.
C      IERR     I       error indicator: 0 = No error.
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2)
      INTEGER   BC, EC, NG, ILTYPE, BUFFER(256), IERR
C
      INTEGER   NINTER
      PARAMETER (NINTER=15)
C
      INTEGER   INCHAR, IANGL, LTYPE, I, XINTER(NINTER), XINT, DIST,
     *   NOINT, NINT, IG, XVAL
      REAL      X, DCX, DCY, XL, XI, XPOS
      CHARACTER SPRTXT*20
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA XINTER /1, 2, 5, 10, 20, 50, 100, 200, 500, 1000,
     *   2000, 5000, 10000, 20000, 50000/
C-----------------------------------------------------------------------
      CALL CHECKL ('SNIFT')
      LTYPE = MOD (ABS (ILTYPE), 100)
      IF (LTYPE.EQ.1) GO TO 999
C                                       axis type
      IF (CTYP(1,LOCNUM).NE.' ') THEN
         X = (BLC(1) + TRC(1)) / 2.0
         CALL GPOS (X, BLC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         SPRTXT = CTYP(1,LOCNUM)
         CALL CHTRIM (SPRTXT, 20, SPRTXT, INCHAR)
         IANGL = 0
         DCX = -INCHAR / 2.0
         DCY = -2.83
         IF (LTYPE.EQ.2) DCY = -1.5
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       tick marks
      XINT = 32 / NG
      XINT = MAX (3, MIN (16, XINT))
      DIST = EC - BC + 2
      DO 20 I = 1,NINTER
         NOINT = DIST / XINTER(I)
         IF (NOINT.LE.XINT) GO TO 30
 20      CONTINUE
      MSGTXT = 'SNIFT: TICK MARK ALGORITHM FAILED'
      CALL MSGWRT (6)
      IERR = 0
      GO TO 999
C                                       plot tick marks
 30   XINT = XINTER(I)
      NINT = (NOINT * NG) / 16
      NINT = MAX (1, NINT)
      NOINT = NOINT + 2
      DCX = -0.5
      XL = DIST * NG
      XL = (TRC(1) - BLC(1)) / XL
      XI = (TRC(2) - BLC(2)) / 25.
      DCY = -1.5
      DO 50 IG = 1,NG
         XVAL = (BC / XINT) * XINT
         IF (XVAL.EQ.BC) XVAL = XVAL - XINT
         DO 40 I = 1,NOINT
            XVAL = XVAL + XINT
            IF ((XVAL.GE.BC) .AND. (XVAL.LE.EC)) THEN
               XPOS = (XVAL-BC+1 + (IG-1)*DIST) * XL + BLC(1)
               CALL GPOS (XPOS, TRC(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GVEC (XPOS, TRC(2)-XI, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GPOS (XPOS, BLC(2)+XI, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GVEC (XPOS, BLC(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               IF ((LTYPE.GT.2) .AND. (MOD(I,NINT).EQ.0) .AND.
     *            (CTYP(1,LOCNUM).NE.' ')) THEN
                  WRITE (MSGTXT,1030) XVAL
                  CALL CHTRIM (MSGTXT, 6, MSGTXT, INCHAR)
                  DCX = 0.5 - INCHAR
                  CALL GCHAR (INCHAR, 0, DCX, DCY, MSGTXT, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 999
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I6)
      END

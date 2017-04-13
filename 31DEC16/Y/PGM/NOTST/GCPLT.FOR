LOCAL INCLUDE 'GCPLT.INC'
C                                       Local include for GCPLT
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XANT(50), PIXR(2), XNCOU, XDO3C,
     *   XLABEL, XDOTV, XGRCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XBAND, XCALIN(12)
      CHARACTER NAMEIN*12, CLAIN*6, BAND*1, CALIN*48
C                                       Program info
C
      INTEGER   NOBAND, NOFREQ
      PARAMETER (NOBAND=10)
      PARAMETER (NOFREQ=6)
C
      REAL      XYSCL(2), XYOFF(2), CHOUT(4), XXMIN, XXMAX, PRAN(2,2),
     *   YYMIN(MAXANT), YYMAX(MAXANT), DO3COL, GAINS(90,NOFREQ,MAXANT),
     *   YYMX, YYMN
      INTEGER   SEQIN, DISKIN, CNOIN, ANTS(50), NCOUNT, NPARMS, NANTSL,
     *   NPLOTS, GRCHN, TVCHN, TVCORN(4), IANT, LABEL, LTYPE, NFGAIN
      LOGICAL   DOAWNT, DOTV
      DOUBLE PRECISION FGAINS(NOFREQ)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XBAND, XCALIN,
     *   XANT, PIXR, XNCOU, XDO3C, XLABEL, XDOTV, XGRCH
      COMMON /VPARM/ SEQIN, DISKIN, CNOIN, ANTS, NCOUNT, NPARMS, GRCHN,
     *   TVCHN, TVCORN, IANT, DOTV, LABEL, CHOUT, DO3COL, PRAN, YYMAX,
     *   YYMIN, LTYPE
      COMMON /VGNCOM/ FGAINS, NFGAIN, GAINS, XYSCL, XYOFF, NANTSL,
     *   NPLOTS, DOAWNT, XXMIN, XXMAX, YYMN, YYMX
      COMMON /VGNCHR/ NAMEIN, CLAIN, CALIN, BAND
C                                                          End GCPLT
LOCAL END
      PROGRAM GCPLT
C-----------------------------------------------------------------------
C! Plots data from a gain curve file
C# UV Plot EXT-appl Calibration VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2014-2015
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
C   GCPLT plots gain curves from a file. A 'PL' extension file is made
C   which can be displayed in the usual ways .
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INEXT......'SN','TY','PC' or 'CL' table to be plotted
C      'BAND'.....Select band to plot: defaul 'Q'
C      CALIN......File with gains: AIPSIONS:EVLA.GAINS by default
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
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
C
      CHARACTER PRGN*6
      INTEGER   IRET, BUFF(256)
      INCLUDE 'GCPLT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'GCPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL GCPLTI (PRGN, IRET)
C                                       read data to figure out
C                                       distribution
C                                       Fetch data, determine scaling
      IF (IRET.EQ.0) CALL GCPLTR (IRET)
C                                       Do plots
      IF (IRET.EQ.0) CALL GCPLOT (IRET)
      IF (IRET.LT.0) IRET = 0
C                                       Close down
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE GCPLTI (PRGN, IERR)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for GCPLT.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, TYPTMP*2
      INTEGER   IRET, I, J, K, JERR, BUFF(512), IROUND
      LOGICAL   T, F
      INCLUDE 'GCPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NPARMS = 77
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
      CALL H2CHR (1, 1, XBAND, BAND)
      CALL H2CHR (48, 1, XCALIN, CALIN)
      DO3COL = XDO3C
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      NCOUNT = IROUND (XNCOU)
      IF (NCOUNT.LE.0) NCOUNT = 5
      XNCOU = NCOUNT
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
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
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
      CALL GETANT (DISKIN, CNOIN, 1, CATBLK, BUFF, JERR)
      I = MAXANT
      CALL RFILL (I, 1.E8, YYMIN)
      CALL RFILL (I, -1.E8, YYMAX)
      I = 90 * 6 * MAXANT
      CALL RFILL (I, 1.0, GAINS)
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
      END
      SUBROUTINE GCPLTR (IERR)
C-----------------------------------------------------------------------
C   GCPLTR reads the gain curve table, computes the curve for each
C   antenna and frequency in the band
C   each station or IF prior to plotting.
C   Output:
C      IERR     I      Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   NODATA
      INTEGER   I, LUNGN, INDGN, MYBAND, IBAND, EVLA
      REAL      GPARMS(4)
      CHARACTER BANDS(10)*1, DATOBS*8, LINE*128
      DOUBLE PRECISION JD, VDATE(2), FF
      INCLUDE 'GCPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA BANDS /'4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q'/
C-----------------------------------------------------------------------
      NODATA = .TRUE.
C                                       open text file
      LUNGN = 3
      EVLA = 0
      IF (CALIN.EQ.' ') THEN
         CALIN = 'AIPSIONS:EVLA.GAINS'
         EVLA = 1
         END IF
      CALL ZTXOPN ('READ', LUNGN, INDGN, CALIN, .FALSE., IERR)
      IF (IERR.EQ.0) THEN
         CALL ZTXIO ('READ', LUNGN, INDGN, LINE, IERR)
         IF ((IERR.EQ.0) .AND. (LINE(:20).EQ.'; ANTENNA-GAIN-TABLE'))
     *      GO TO 10
         CALL ZTXCLS (LUNGN, INDGN, IERR)
         END IF
      MSGTXT = 'Standard gains file not valid or failed!'
      CALL MSGWRT (6)
      IERR = 1
      GO TO 999
C                                       band needed
 10   MYBAND = 10
      DO 15 I = 1,10
         IF (BAND.EQ.BANDS(I)) MYBAND = I
 15      CONTINUE
      NFGAIN = 0
      CALL H2CHR (8, 1, CATH(KHDOB), DATOBS)
      CALL JULDAY (DATOBS, JD)
C                                       Loop thru data
 20   CALL ZTXIO ('READ', LUNGN, INDGN, LINE, IERR)
      IF (IERR.EQ.0) THEN
         IF (LINE(:1).NE.';') THEN
            CALL PARSEL (LINE, EVLA, IBAND, FF, IANT, GPARMS, VDATE)
            IF ((JD.GE.VDATE(1)) .AND. (JD.LE.VDATE(2)) .AND.
     *         (IANT.GE.0).AND. (IBAND.EQ.MYBAND)) THEN
               IF (IANT.EQ.0) IANT = MAXANT
               IF ((NFGAIN.LE.0) .OR. (ABS(FF-FGAINS(NFGAIN)).GT.1.D6))
     *            THEN
                  NFGAIN = MAX (1, NFGAIN+1)
                  FGAINS(NFGAIN) = FF
                  END IF
               DO 25 I = 1,90
                  GAINS(I,NFGAIN,IANT) = GPARMS(1) + (I-1)*GPARMS(2) +
     *               (I-1.)*(I-1.)*GPARMS(3) + ((I-1.0)**3) * GPARMS(4)
                  YYMIN(IANT) = MIN (YYMIN(IANT), GAINS(I,NFGAIN,IANT))
                  YYMAX(IANT) = MAX (YYMAX(IANT), GAINS(I,NFGAIN,IANT))
 25               CONTINUE
               END IF
            END IF
         GO TO 20
         END IF
      CALL ZTXCLS (LUNGN, INDGN, I)
      IF (IERR.EQ.2) IERR = 0
C                                       blank out undesired antennas
C                                       count plots
      NPLOTS = 0
      DO 50 IANT = 1,MAXANT
         IF ((NANTSL.GT.0) .AND. (IANT.NE.MAXANT)) THEN
            DO 30 I = 1,NANTSL
               IF (IANT.EQ.ANTS(I)) THEN
                  IF (DOAWNT) THEN
                     GO TO 45
                  ELSE
                     GO TO 40
                     END IF
                  END IF
 30            CONTINUE
            IF (DOAWNT) THEN
               YYMIN(IANT) = 1000.
               YYMAX(IANT) = -1000.
               END IF
            GO TO 45
 40         YYMIN(IANT) = 1000.
            YYMAX(IANT) = -1000.
            END IF
 45      IF (YYMAX(IANT).GT.YYMIN(IANT)) NPLOTS = NPLOTS + 1
 50      CONTINUE
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PARSEL (INLINE, EVLA, IBAND, FF, IANT, GPARMS, VDATE)
C-----------------------------------------------------------------------
C   Parses a line from a gain curve file
C   Input:
C      INLINE   C*       Input line
C   Output:
C      IBAND    I        The "band number".
C      FF       D        Frequency
C      IANT     I        Antenna number
C      GPARMS   R(4)     Gain curve polynomial parameters
C      VDATE    D(2)     Julian date range over which these gain curve
C                        coefficients are valid
C-----------------------------------------------------------------------
      CHARACTER INLINE*(*)
      INTEGER   EVLA, IBAND, IANT
      REAL      GPARMS(4)
      DOUBLE PRECISION FF, VDATE(2)
C
      INTEGER   II, JJ, NL, JTRIM
      CHARACTER CBAND(10)*1
      DOUBLE PRECISION X, FC(10)
      INCLUDE 'INCS:DDCH.INC'
C
      DATA CBAND / '4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q'/
      DATA FC /74.D0, 300.D0, 1500.D0, 3000.D0, 6000.D0, 10000D0,
     *   15000.D0, 24250.D0, 33250.D0, 48000.D0/
C-----------------------------------------------------------------------
C                                       get band
      II = 1
      IBAND = 0
      IANT = -1
      NL = JTRIM (INLINE)
      IF (NL.LE.0) GO TO 999
C                                       Match band
      DO 25 JJ = 1,10
         IF (INLINE(:1).EQ.CBAND(JJ)) IBAND = JJ
 25      CONTINUE
C                                       Format known?
      IF (EVLA.EQ.0) THEN
         II = 3
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.EQ.DBLANK) GO TO 999
         IF (X.LT.50) THEN
            EVLA = -1
         ELSE
            EVLA = 1
            END IF
         END IF
C                                       get freq
      II = 3
      IF (EVLA.GT.0) THEN
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.EQ.DBLANK) GO TO 999
         FF = X * 1.D6
      ELSE
         FF = FC(IBAND) * 1.D6
         END IF
C                                       get antenna
      CALL GETNUM (INLINE, 80, II, X)
      IF (X.EQ.DBLANK) GO TO 999
      IANT = X + 0.01D0
C                                       get dates
      II = II + 1
      JJ = II
      DO 30 II = JJ,NL
         IF (INLINE(II:II).NE.' ') GO TO 35
 30      CONTINUE
      VDATE(1) = 1.D10
      GO TO 999
 35   CALL JULDAY (INLINE(II:II+7), VDATE(1))
      II = II + 9
      JJ = II
      DO 40 II = JJ,NL
         IF (INLINE(II:II).NE.' ') GO TO 45
 40      CONTINUE
      VDATE(2) = -1.D10
      GO TO 999
 45   CALL JULDAY (INLINE(II:II+7), VDATE(2))
      II = II + 9
C                                       get parms
      DO 50 JJ = 1,4
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.NE.DBLANK) GPARMS(JJ) = X
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GCPLOT (IRET)
C-----------------------------------------------------------------------
C   GCPLOT plots the data thru calls to PLTSN.
C   Input:
C   Output:
C      IRET     I      Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPLOT, NPLT
      REAL      TMAX, TMIN
      INCLUDE 'GCPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
C                                       plot loop
      NPLT = 0
      DO 100 IANT = 1,MAXANT
         IF (YYMAX(IANT).GT.YYMIN(IANT)) THEN
            IF (PIXR(2).GT.PIXR(1)) THEN
               YYMX = PIXR(2)
               YYMN = PIXR(1)
            ELSE
               YYMX = YYMAX(IANT)
               YYMN = YYMIN(IANT)
               END IF
            TMAX = YYMX + 0.1 * (YYMX - YYMN)
            TMIN = YYMN - 0.1 * (YYMX - YYMN)
            XYOFF(2) = TMIN
            XYSCL(2) = 1000.0 / (TMAX - TMIN) / NCOUNT
            PRAN(1,2) = TMIN
            PRAN(2,2) = TMAX
            TMAX = 93.
            TMIN = -4.
            XYOFF(1) = TMIN
            XYSCL(1) = 1000.0 / (TMAX - TMIN)
            PRAN(1,1) = TMIN
            PRAN(2,1) = TMAX
            NPLT = NPLT + 1
            IPLOT = MOD (NPLT-1, NCOUNT) + 1
            IF (NPLT.EQ.NPLOTS) IPLOT = -IPLOT
            CALL PLOTGC (IPLOT, IANT, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLOTGC (IPLOT, ANTNO, IRET)
C-----------------------------------------------------------------------
C   PLOTGC actually plots data.
C   Input:
C      IPLOT    I      Plot number on current page. If neg. then this is
C                      last plot.
C      ANTNO    I      Antenna number
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                       -1 => user request termination
C                        1 => failed to add to catalog
C                        2 => failed to create
C                        3 => graph file write error
C                        4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, ANTNO, IRET
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(2)*8,
     *   CHTYPE(2)*16, CHTMP*18
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, FINDPL,
     *   DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT, I, NGOOD,
     *   NNOFIT, NN, IIF, ILITY, JTRIM
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   COLV, COL(3), COLR
      LOGICAL   T, F, GOOD, CATUP, DO3C, BLNKD
      SAVE BUFFER
      INCLUDE 'GCPLT.INC'
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
      DATA AUNITS /'Degrees','Gain'/
      DATA CHTYPE /'Zenith angle', 'Gain amp'/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C
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
      CTYP(1,LOCNUM) = AUNITS(1)
      CTYP(2,LOCNUM) = AUNITS(2)
C                                       Init plot calls again
C                                       Number of characters on each
C                                       side of the plot
      IF (ABS (IPLOT).EQ.1) THEN
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
         DY = 0.5
C                                       The first line of the header
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INCHAR = 16
         INP = 1
         TEXT = CHTYPE(2)
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         TEXT(INP+1:) = ' versus ' // CHTYPE(1)
         INP = JTRIM (TEXT) + 2
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(INP:), INCHAR)
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       second line
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         WRITE (TEXT,1050) (FGAINS(I)/1.D6, I = 1,NFGAIN)
         INP = JTRIM(TEXT) + 2
         TEXT(INP:) = 'MHz'
         CALL REFRMT (TEXT, ' ', INCHAR)
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
C                                       station ID
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  1.5
      DY = -1.8
      WRITE (TEXT,1040) ANTNO
      IF (ANTNO.LT.MAXANT) THEN
         TEXT(INP+1:) = STNNAM(ANTNO)
      ELSE
         TEXT(INP+1:) = 'Default ant'
         END IF
      CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
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
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Loop
      ILITY = 4
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO3C = (DO3COL.GT.0.0) .AND. (NFGAIN.GT.3)
      COLR = NFGAIN - 1.0
C                                       Outer loop: IF, stokes
      COLV = 0.0
      DO 200 IIF = 1,NFGAIN
         IF (DO3COL.GT.0.0) THEN
            IF (DO3C) THEN
               CALL COLOR3 (COLV, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               COLV = COLV + 1.0 / COLR
            ELSE IF (1+IIF.NE.ILITY) THEN
               ILITY = 1 + IIF
               CALL GLTYPE (ILITY, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
C                                       Point plot
         BLNKD = .TRUE.
         DO 120 NN = 1,90
C                                       Scale X
            XY(1) = NN - 1
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
               NNOFIT = NNOFIT + NFGAIN
               GO TO 120
               END IF
C                                       loop for points
            VALUE = GAINS(NN,IIF,IANT)
            IF (VALUE.EQ.FBLANK) THEN
               BLNKD = .TRUE.
            ELSE
               XY(2) = VALUE
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                  NNOFIT = NNOFIT + 1
                  BLNKD = .TRUE.
               ELSE
                  NGOOD = NGOOD + 1
C                                       Mark point
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
 120        CONTINUE
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
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I3)
 1050 FORMAT ('Frequencies =',6F9.2)
 1200 FORMAT ('PLOTGC:',I9,' points plotted')
 1202 FORMAT ('PLOTGC:',I9,' points did not fit')
 1960 FORMAT ('PLOTGC: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLOTGC: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END

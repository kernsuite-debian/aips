LOCAL INCLUDE 'FGPLT.INC'
C                                       Local include for FGPLT
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XTIME(8), XFQID, XSUBA, XBIF,
     *   XEIF, XBCHN, XECHN, XANT(50), XBASE(50), XLABEL, XYRATO, XDOTV,
     *   XGRCH, TSTA, TSTO, XYSCL(2), XYOFF(2), CHOUT(4), TFMIN, TFMAX
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, ANTS(50), NPARMS,
     *   GRCHN, TVCHN, TVCORN(4), LABEL, AN1(MXBASE), AN2(MXBASE),
     *   NBASE, NFALL, NFANT(MAXANT), NFBL(MAXANT,MAXANT), NFBLS,
     *   NFANTS, ANMAX, LTYPE
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSTOK
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, NAME1(MXBASE)*8,
     *   NAME2(MXBASE)*8, STNS(MAXANT)*8
      LOGICAL   DESEL, DOTV, ISVLBA
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNVER, XTIME,
     *   XXSTOK, XFQID, XSUBA, XBIF, XEIF, XBCHN, XECHN, XANT, XBASE,
     *   XLABEL, XYRATO, XDOTV, XGRCH
      COMMON /VGNCOM/ TSTA, TSTO, TFMIN, TFMAX, XYSCL, XYOFF, DESEL,
     *   AN1, AN2, NBASE, ISVLBA, NFALL, NFANT, NFANTS, NFBL, NFBLS,
     *   SEQIN, DISKIN, CNOIN, IVER, ANTS, NPARMS, GRCHN, TVCHN, TVCORN,
     *   DOTV, LABEL, CHOUT, ANMAX, LTYPE
      COMMON /VGNCHR/ NAMEIN, CLAIN, XSTOK, NAME1, NAME2, STNS
C                                                          End FGPLT
LOCAL END
      PROGRAM FGPLT
C-----------------------------------------------------------------------
C! Plots data from a FG table
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2002-2003, 2009, 2010-2012, 2014-2015
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
C   FGPLT plots FG extension file. A 'PL' extension file is made
C   which can be displayed in the usual ways .
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INVERS.....Version number of table to plot, 0=>1.
C      TIMERANG...Time range of the data to be plotted. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      STOKES.....The desired Stokes type of the output data:
C                 'R' = RCP, 'L' = LCP, 'RL', 'LR'
C      BIF........Beginning IF to plot
C      EIF........Ending IF to plot
C      ANTENNAS...A list of the antennas to form baselines
C      BASELINE...A list of antennas to form baselines with antennas
C                 listed in ANTENNAS
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
C
      CHARACTER PRGN*6
      INTEGER   IRET
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'FGPLT.INC'
      DATA PRGN /'FGPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL FGPIN (PRGN, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Do plots
      IF (NFBLS.GT.0) THEN
         CALL FGBASL (IRET)
      ELSE
         CALL FGANT (IRET)
         END IF
      IRET = MAX (0, IRET)
C                                       Close down
 900  CALL DIE (IRET, UBUFF)
C
 999  STOP
      END
      SUBROUTINE FGPIN (PRGN, IERR)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for FGPLT and prepare data for plot
C   reading FG table
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'FGPLT.INC'
C
      INTEGER   IRET, BUFF(256), I, BUFFER(512), IROUND, LUN, IBASE,
     *   LUNI, IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC), NFGROW, SOURID,
     *   SUBA, FREQID, ANTFG(2), IFS(2), CHANS(2), IXBASL(50),
     *   IXANT(50), NXANT, NXBASL, IFMAX, IA1, IA2
      REAL      TIMER(2), STARTT, STOPT
      CHARACTER STAT*4, PRGN*6, TYPTMP*2, REASON*24
      LOGICAL   F, PFLAGS(4), DROP
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      NPARMS = 127
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING INPUT PARAMETERS'
         IRET = 8
         RQUICK = F
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
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
      NFALL = 0
      CALL FILL (MAXANT, 0, NFANT)
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NFBL)
      NFBLS = 0
      NFANTS = 0
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
         WRITE (MSGTXT,1000) IERR, 'READING UV DATA HEADER'
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
C                                       Time range
      TSTA = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*3600.0))
      TSTO = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*3600.0))
C                                       UV data times
      CALL UVTIME (DISKIN, CNOIN, CATBLK, STARTT, STOPT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FINDING UV DATA TIME RANGE'
         GO TO 990
         END IF
      IF (CATBLK(KIGCN).LT.20) THEN
         STARTT = 0.0
         STOPT = 9999.0
         END IF
      TFMAX = -100.
      TFMIN = 9999.
      IF ((TSTO.GT.STOPT) .OR. (TSTO.LE.TSTA)) TSTO = STOPT + 10./86400.
      IF (TSTA.LE.STARTT) TSTA = STARTT - 10.0/86400.
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TSTA
      XTIME(5) = TSTO
C                                       Subarray
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      XSUBA = SUBARR
C                                       Freq id
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = 1
C                                       IF'S
      IF (JLOCIF.GE.0) THEN
         IFMAX = CATBLK(KINAX+JLOCIF)
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, IFMAX))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = IFMAX
         IF (EIF.GT.IFMAX) EIF = IFMAX
      ELSE
         BIF = 1
         EIF = 1
         IFMAX = 1
         END IF
      BCHAN = IROUND (XBCHN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) ECHAN = CATBLK(KINAX+JLOCF)
      XBIF = BIF
      XEIF = EIF
      XBCHN = BCHAN
      XECHN = ECHAN
C                                       Find baselines to plot
      CALL SETANT (50, XANT, XBASE, NXANT, NXBASL, IXANT, IXBASL, DESEL)
C                                       Fill in list of all antenna
C                                       - baseline pairs and names.
C                                       Determine the list and number
C                                       of selected antennas.
      LUN = 25
      CALL FILANT (DISKIN, CNOIN, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, SUBARR, NBASE, AN1, AN2, NAME1, NAME2, STNS, ISVLBA,
     *   BUFFER, IERR)
C
      IF (XSTOK.EQ.'RR') XSTOK = 'R'
      IF (XSTOK.EQ.'LL') XSTOK = 'L'
      IF ((XSTOK.NE.'R') .AND. (XSTOK.NE.'L') .AND.
     *   (XSTOK.NE.'RL') .AND. (XSTOK.NE.'LR')) XSTOK = 'R'
      CALL CHR2H (4, XSTOK, 1, XXSTOK)
C
      XNVER = IVER
      NUMIF = EIF - BIF + 1
C                                       read FG table first time to
C                                       store number of records for
C                                       each ant, BL, all flags
C                                       Open FG file
      LUNI = 27
      CALL FLGINI ('READ', BUFFER, DISKIN, CNOIN, IVER, CATBLK, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IERR)
      IF (IERR.EQ.2) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING FLAG TABLE'
         GO TO 990
         END IF
C                                       # rows in the table
      NFGROW = BUFFER(5)
C                                       Loop and copy
      DO 50 I = 1,NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTFG, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING FLAG TABLE'
            GO TO 990
            END IF
         DROP = IERR.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((FRQSEL.GT.0) .AND. (FRQSEL.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this SUBARRAY selected ?
         DROP = DROP .OR. ((SUBARR.GT.0) .AND. (SUBARR.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       stokes
         DROP = DROP .OR. ( (XSTOK.EQ.'R' .AND. .NOT.PFLAGS(1)) .OR.
     *      (XSTOK.EQ.'L' .AND. .NOT.PFLAGS(2)) .OR.
     *      (XSTOK.EQ.'RL' .AND. .NOT.PFLAGS(3)) .OR.
     *      (XSTOK.EQ.'LR' .AND. .NOT.PFLAGS(4)))
C                                       timerange
         DROP = DROP .OR. (TSTO.LT.TIMER(1)) .OR. (TSTA.GT.TIMER(2))
C                                       channel
         IF (CHANS(1).LE.0) CHANS(1) = 1
         IF (CHANS(2).LE.0) CHANS(2) = CATBLK(KINAX+JLOCF)
         DROP = DROP .OR. (BCHAN.GT.CHANS(2)) .OR. (ECHAN.LT.CHANS(1))
C                                       IFs
         IF (IFS(1).LE.0) IFS(1) = 1
         IF (IFS(2).LE.0) IFS(2) = IFMAX
         DROP = DROP .OR. (BIF.GT.IFS(2)) .OR. (EIF.LT.IFS(1))
C                                       store the number of records
         IF (.NOT.DROP) THEN
            TFMIN = MIN (TFMIN, TIMER(1))
            TFMAX = MAX (TFMAX, TIMER(2))
            IF ((ANTFG(1).LE.0) .AND. (ANTFG(2).LE.0)) THEN
               NFALL = NFALL + 1
            ELSE IF (ANTFG(2).LE.0) THEN
               DO 10 IBASE = 1,NBASE
                  IF ((AN1(IBASE).EQ.ANTFG(1)) .OR.
     *               (AN2(IBASE).EQ.ANTFG(1))) THEN
                     NFANT(ANTFG(1)) = NFANT(ANTFG(1)) + 1
                     NFANTS = NFANTS + 1
                     GO TO 50
                     END IF
 10               CONTINUE
            ELSE
               IA1 = MIN (ANTFG(1), ANTFG(2))
               IA2 = MAX (ANTFG(1), ANTFG(2))
               DO 20 IBASE = 1,NBASE
                  IF (((AN1(IBASE).EQ.ANTFG(1)) .AND.
     *               (AN2(IBASE).EQ.ANTFG(2))) .OR.
     *               ((AN2(IBASE).EQ.ANTFG(1)) .AND.
     *               (AN1(IBASE).EQ.ANTFG(2)))) THEN
                     NFBL(IA1,IA2) = NFBL(IA1,IA2) + 1
                     NFBLS = NFBLS + 1
                     GO TO 50
                     END IF
 20               CONTINUE
               END IF
            END IF
 50     CONTINUE
C                                       Close the table
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IERR)
C                                       anything?
      IF (NFALL+NFANTS+NFBLS.LE.0) THEN
         IERR = 1
         MSGTXT = 'Nothing has been selected. Check the parameters'
         GO TO 990
         END IF
      IF ((TSTO.GT.9998.) .AND. (TSTA.LT.0.0)) THEN
         TSTA = TFMIN - 10.0/86400.0
         TSTO = TFMAX + 10.0/86400.0
         END IF
C                                       recount to count only once
      NFANTS = 0
      NFBLS = 0
      ANMAX = 0
      DO 70 IA1 = 1,MAXANT
         IF (NFANT(IA1).GT.0) THEN
            ANMAX = MAX (IA1, ANMAX)
            NFANTS = NFANTS + 1
            END IF
         DO 60 IA2 = IA1,MAXANT
            IF (NFBL(IA1,IA2).GT.0) THEN
               ANMAX = MAX (IA2, ANMAX)
               NFBLS = NFBLS + 1
               END IF
 60         CONTINUE
 70      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGPIN ERROR:',I7,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
      END
      SUBROUTINE FGBASL(IRET)
C-----------------------------------------------------------------------
C   FGBASL plots the baseline data thru calls to PLTFG
C   Output:
C      IRET     I      Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPLOT, NPLOT1, MAXLIN, KLINES, NLAST, IPL, INCANT,
     *   REBAS, J, I, KBL
      REAL      TMIN, TMAX, PLTINC, TIMDIF, TTMIN, TTMAX, SPACIN
      INCLUDE 'FGPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
C                                       adjust the list of baselines
      IF (NFALL.LE.0) THEN
         J = 0
         DO 10 I = 1,NBASE
            IF ((NFBL(AN1(I),AN2(I)).GT.0) .OR. (NFANT(AN1(I)).GT.0)
     *         .OR. (NFANT(AN2(I)).GT.0)) THEN
               J = J + 1
               IF (J.LT.I) THEN
                  AN1(J) = AN1(I)
                  AN2(J) = AN2(I)
                  NAME1(J) = NAME1(I)
                  NAME2(J) = NAME2(I)
                  END IF
               END IF
 10         CONTINUE
         NBASE = J
         END IF
C                                       max number of line at the page
C                                       Take more density for more IFs
      MAXLIN = 45 * ((MIN(NUMIF,8) + 1) / 2)
C                                       number baselines / plot
      INCANT = 0
      IF (NUMIF.GT.1) INCANT = 1
      KBL = (MAXLIN + 1) / (NUMIF + INCANT)
      KBL = MIN (KBL, NBASE)
C                                       number lines in one plot
      KLINES = KBL * (NUMIF + INCANT) - INCANT
C                                       spacing between the lines
      SPACIN = 1000.0 / KLINES
C                                       shift of the first line
      XYOFF(2) = SPACIN / 2
      XYSCL(2) = 1
C                                       80 is spacing for the X labels
      NPLOT1 = NBASE / KBL
      NLAST = NBASE - NPLOT1 * KBL
      IF (NLAST.GT.0) THEN
         NPLOT1 = NPLOT1 + 1
      ELSE
         NLAST = KBL
         END IF
      TIMDIF = (TSTO - TSTA)
      TTMIN = TSTA
      TTMAX = TTMIN + TIMDIF
      TMIN = (TSTA - 0.1 * TIMDIF) * 360
      TMAX = (TSTO + 0.01 * TIMDIF) * 360
      XYOFF(1) = TMIN
      XYSCL(1) = 1000.0 / (TMAX - TMIN)
      REBAS = 1
      IPLOT = 1
      PLTINC = SPACIN * KLINES
      DO 50 IPL = 1,NPLOT1
C                                       last plot flag etc.
         IF (IPL.EQ.NPLOT1) THEN
            IPLOT = -IPLOT
            PLTINC = SPACIN * (NLAST * (NUMIF+INCANT) - INCANT)
            KBL = NLAST
            END IF
         CALL BPLTFG (IPLOT, PLTINC, REBAS, KBL, INCANT, IRET)
         IF (IRET.GT.0) THEN
            MSGTXT = 'FGBASL GETS PLOT ERROR FROM BPLTFG - QUITTING'
            GO TO 990
         ELSE IF (IRET.LT.0) THEN
            MSGTXT = 'FGBASL: terminating by request'
            GO TO 990
            END IF
 50      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE BPLTFG (IPLOT, PLTINC, REBAS, KBL, INCANT, IRET)
C-----------------------------------------------------------------------
C   BPLTFG actually plots data.
C   Input:
C      IPLOT   I   Plot number on current page. If neg. then this is
C                   last plot.
C   Output:
C      IRET    I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IPLOT, REBAS, KBL, INCANT, IRET
      REAL      PLTINC
C
      INCLUDE 'FGPLT.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, CHTMP*18,
     *   XUNITS*20, NB1*4, NB2*4, TYPE*2, REASON*24
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), I, NGOOD, NNOFIT,
     *   ILINE, FGBUFF(512), LUNI, IFGRNO, KIF, FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), NFGROW, SOURID, SUBA, FREQID, ANTFG(2), IFS(2),
     *   CHANS(2), IBASE, NA1, NA2
      REAL      BLC(2), TRC(2), DX, DY, TR, VALUE, XY(2), XTRC(2),
     *   XBLC(2), TLC(2), YYOFF(2), SIZE, XMULT(2), TIMER(2), XMIN, YMIN
      LOGICAL   T, F, CATUP, PFLAGS(4), DROP
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      HOLLERITH CATH(256)
      EQUIVALENCE (CATBLK, CATH)
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA XUNITS /'IAT (hr)'/
C-----------------------------------------------------------------------
C                                       Time system from AN table
      XUNITS(1:3) = TIMLAB(1:3)
      TYPE = 'FG'
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC - (ABS (IPLOT)-1)*80
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
      AXTYP(LOCNUM) = 0
      XMULT(2) = 1
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      XMULT(1) = 1.0
C
      DO 50 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 50      CONTINUE
C                                       The following card is required
C                                       to label each plot's horizontal
C                                       asis
      CTYP(1,LOCNUM) = XUNITS
C                                       This stops y axis ticks/label
      CTYP(2,LOCNUM) = 'NO TICKS'
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
         ITYPE = 42
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
         CALL RFILL (4, 0.5, CHOUT)
         CHOUT(1) = 1.5
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *      1.333
C                                       default XYRATIO
         IF (XYRATO.LT.0.05) THEN
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
C
         TEXT(INP:) = 'Flagged intervals for'
         INP = INP + 22
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
         WRITE (TEXT(INP:),1100) TYPE, IVER
         INP = INP + 8
C                                       Stokes and IF
         IF ((XSTOK.EQ.'RL') .OR. (XSTOK.EQ.'LR')) THEN
            TEXT(INP:) = XSTOK(1:2) // 'pol_'
         ELSE
            TEXT(INP:) = XSTOK(:1) // 'pol_'
            END IF
         INP = INP + 7
         IF (BIF.EQ.EIF) THEN
            WRITE (TEXT(INP:),1200) BIF
         ELSE
            WRITE (TEXT(INP:),1300) BIF, EIF
            END IF
         INP = INP + 6
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF (LABEL.GT.1) THEN
            DY = 0.5 + 2 * 1.333
C                                       the first line of the header
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1400) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Put on labels and ticks
C                                       To have ticks only at the
C                                       horizontal axis use the private
C                                       version of CLAB1
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Loop: label baselines
      VALUE = PLTINC
      ILINE = 0
      DO 100 IBASE = REBAS,REBAS+KBL-1
         IF (NAME1(IBASE)(:3).EQ.'VLA') THEN
            NB1 = NAME1(IBASE)(5:7)
         ELSE
            NB1 = NAME1(IBASE)(1:3)
            END IF
         IF (NAME2(IBASE)(:3).EQ.'VLA') THEN
            NB2 = NAME2(IBASE)(5:7)
         ELSE
            NB2 = NAME2(IBASE)(1:3)
            END IF
         NA1 = AN1(IBASE)
         NA2 = AN2(IBASE)
         XY(2) = VALUE
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
C                                       value for the next line
         VALUE = VALUE - XYOFF(2) * 2 * (NUMIF + INCANT)
         IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
            NNOFIT = NNOFIT + 1
         ELSE
            CALL GPOS (0, XY(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DX = 0.4
            DY = -0.5
C                                       label baselines
            IF (ISVLBA) THEN
               TEXT = NB1(:2) // '-' // NB2(:2)
            ELSE
               WRITE (TEXT,1500) NA1, NA2
               END IF
            CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GLTYPE (1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 100     CONTINUE
C                                       Open FG file
      LUNI = 27
      CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, IVER, CATBLK, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IERR)
      IF (IERR.EQ.2) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING FLAG TABLE'
         GO TO 990
         END IF
C                                       # rows in the table
      NFGROW = FGBUFF(5)
C                                       Loop and copy
      DO 200 I = 1,NFGROW
         CALL TABFLG ('READ', FGBUFF, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTFG, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING FLAG TABLE'
            GO TO 990
            END IF
         DROP = IERR.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((FRQSEL.GT.0) .AND. (FRQSEL.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this SUBARRAY selected ?
         DROP = DROP .OR. ((SUBARR.GT.0) .AND. (SUBARR.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       stokes
         DROP = DROP .OR. ( (XSTOK.EQ.'R' .AND. .NOT.PFLAGS(1)) .OR.
     *      (XSTOK.EQ.'L' .AND. .NOT.PFLAGS(2)) .OR.
     *      (XSTOK.EQ.'RL' .AND. .NOT.PFLAGS(3)) .OR.
     *      (XSTOK.EQ.'LR' .AND. .NOT.PFLAGS(4)))
C                                       timerange
         DROP = DROP .OR. (TSTO.LT.TIMER(1)) .OR. (TSTA.GT.TIMER(2))
C                                       channel
         IF (CHANS(1).LE.0) CHANS(1) = 1
         IF (CHANS(2).LE.0) CHANS(2) = CATBLK(KINAX+JLOCF)
         DROP = DROP .OR. (BCHAN.GT.CHANS(2)) .OR. (ECHAN.LT.CHANS(1))
C                                       IFs
         IF (IFS(1).LE.0) IFS(1) = 1
         IF (IFS(2).LE.0) IFS(2) = EIF
         DROP = DROP .OR. (BIF.GT.IFS(2)) .OR. (EIF.LT.IFS(1))
C                                       store the number of records
         IF (.NOT.DROP) THEN
            IFS(1) = MAX (IFS(1), BIF)
            IFS(2) = MIN (IFS(2), EIF)
            TIMER(1) = MAX (TIMER(1), TSTA)
            TIMER(2) = MIN (TIMER(2), TSTO)
C                                       all of current ones
            IF ((ANTFG(1).LE.0) .AND. (ANTFG(2).LE.0)) THEN
               DO 120 IBASE = REBAS,REBAS+KBL-1
                  VALUE = PLTINC - (IFS(1)-BIF-1) * 2 * XYOFF(2) -
     *               (IBASE - REBAS) * 2 * XYOFF(2) * (NUMIF+INCANT)
                  DO 110 KIF = IFS(1),IFS(2)
                     VALUE = VALUE - 2 * XYOFF(2)
                     XY(2) = XYSCL(2)*(VALUE-XYOFF(2)) + YYOFF(2)
                     XY(1) = TIMER(1) * 360.
                     XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                     CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
C                                       Draw the horizontal line to
C                                       TIMRE
                     XY(1) = TIMER(2) * 360.
                     XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                     CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     NGOOD = NGOOD + 1
 110                 CONTINUE
 120              CONTINUE
            ELSE IF (ANTFG(2).LE.0) THEN
               DO 140 IBASE = REBAS,REBAS+KBL-1
                  IF ((AN1(IBASE).EQ.ANTFG(1)) .OR.
     *               (AN2(IBASE).EQ.ANTFG(1))) THEN
                     VALUE = PLTINC - (IFS(1)-BIF-1) * 2 * XYOFF(2) -
     *                  (IBASE - REBAS) * 2 * XYOFF(2) * (NUMIF+INCANT)
                     DO 130 KIF = IFS(1),IFS(2)
                        VALUE = VALUE - 2 * XYOFF(2)
                        XY(2) = XYSCL(2)*(VALUE-XYOFF(2)) + YYOFF(2)
                        XY(1) = TIMER(1) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        XY(1) = TIMER(2) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        NGOOD = NGOOD + 1
 130                    CONTINUE
                     END IF
 140              CONTINUE
            ELSE
               DO 160 IBASE = REBAS,REBAS+KBL-1
                  IF (((AN1(IBASE).EQ.ANTFG(1)) .AND.
     *               (AN2(IBASE).EQ.ANTFG(2))) .OR.
     *               ((AN2(IBASE).EQ.ANTFG(1)) .AND.
     *               (AN1(IBASE).EQ.ANTFG(2)))) THEN
                     VALUE = PLTINC - (IFS(1)-BIF-1) * 2 * XYOFF(2) -
     *                  (IBASE - REBAS) * 2 * XYOFF(2) * (NUMIF+INCANT)
                     DO 150 KIF = IFS(1),IFS(2)
                        VALUE = VALUE - 2 * XYOFF(2)
                        XY(2) = XYSCL(2)*(VALUE-XYOFF(2)) + YYOFF(2)
                        XY(1) = TIMER(1) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        XY(1) = TIMER(2) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        NGOOD = NGOOD + 1
 150                    CONTINUE
                     END IF
 160              CONTINUE
               END IF
            END IF
 200    CONTINUE
C                                       Close the table
      CALL TABIO ('CLOS', 0, IFGRNO, FGBUFF, FGBUFF, IERR)
C                                       Done: finish plot
      WRITE (MSGTXT,1600) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1700) NNOFIT
         CALL MSGWRT (2)
         END IF
      REBAS = REBAS + KBL - 1
C
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
         IERR = 0
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
      WRITE (MSGTXT,1600) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1700) NNOFIT
         CALL MSGWRT (2)
         END IF
C
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
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1100 FORMAT (A2,I4,'_')
 1200 FORMAT ('IF ',I2)
 1300 FORMAT ('IF ',I2,' - ',I2)
 1400 FORMAT ('Plot file version',I4,'__created ',A, A)
 1500 FORMAT (I2.2,'-',I2.2)
 1600 FORMAT ('BPLTFG:',I9,' lines plotted')
 1700 FORMAT ('BPLTFG:',I9,' points did not fit')
 1960 FORMAT ('BPLTFG: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('BPLTFG: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE FGANT (IRET)
C-----------------------------------------------------------------------
C   FGANT plots the baseline data thru calls to PLTFG
C   Output:
C      IRET     I      Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FGPLT.INC'
      INTEGER   IPLOT, NPLOT1, MAXLIN, KLINES, NLAST, IPL, INCANT,
     *   REBAS, J, I, KBL, ANWANT(MAXANT)
      CHARACTER ANNAME(MAXANT)*8
      REAL      TMIN, TMAX, PLTINC, TIMDIF, TTMIN, TTMAX, SPACIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
      CALL FILL (MAXANT, 0, ANWANT)
      KBL = 0
      DO 10 I = 1,NBASE
         ANWANT(AN1(I)) = 1
         ANWANT(AN2(I)) = 1
         KBL = MAX (KBL, AN2(I))
         ANNAME(AN1(I)) = NAME1(I)
         ANNAME(AN2(I)) = NAME2(I)
 10      CONTINUE
      J = 0
      DO 20 I = 1,KBL
         IF (ANWANT(I).GT.0) THEN
            J = J + 1
            AN1(J) = I
            NAME1(J) = ANNAME(I)
            END IF
 20      CONTINUE
      NBASE = J
C                                       adjust the list of baselines
      IF (NFALL.LE.0) THEN
         J = 0
         DO 30 I = 1,NBASE
            IF (NFANT(AN1(I)).GT.0) THEN
               J = J + 1
               IF (J.LT.I) THEN
                  AN1(J) = AN1(I)
                  NAME1(J) = NAME1(I)
                  END IF
               END IF
 30         CONTINUE
         NBASE = J
         END IF
C                                       max number of line at the page
C                                       Take more density for more IFs
      MAXLIN = 45 * ((MIN(NUMIF,8) + 1) / 2)
C                                       number baselines / plot
      INCANT = 0
      IF (NUMIF.GT.1) INCANT = 1
      KBL = (MAXLIN + 1) / (NUMIF + INCANT)
      KBL = MIN (KBL, NBASE)
C                                       number lines in one plot
      KLINES = KBL * (NUMIF + INCANT) - INCANT
C                                       spacing between the lines
      SPACIN = 1000.0 / KLINES
C                                       shift of the first line
      XYOFF(2) = SPACIN / 2
      XYSCL(2) = 1
C                                       80 is spacing for the X labels
      NPLOT1 = NBASE / KBL
      NLAST = NBASE - NPLOT1 * KBL
      IF (NLAST.GT.0) THEN
         NPLOT1 = NPLOT1 + 1
      ELSE
         NLAST = KBL
         END IF
      TIMDIF = (TSTO - TSTA)
      TTMIN = TSTA
      TTMAX = TTMIN + TIMDIF
      TMIN = (TSTA - 0.05 * TIMDIF) * 360
      TMAX = (TSTO + 0.01 * TIMDIF) * 360
      XYOFF(1) = TMIN
      XYSCL(1) = 1000.0 / (TMAX - TMIN)
      REBAS = 1
      IPLOT = 1
      PLTINC = SPACIN * KLINES
      DO 50 IPL = 1,NPLOT1
C                                       last plot flag etc.
         IF (IPL.EQ.NPLOT1) THEN
            IPLOT = -IPLOT
            PLTINC = SPACIN * (NLAST * (NUMIF+INCANT) - INCANT)
            KBL = NLAST
            END IF
         CALL APLTFG (IPLOT, PLTINC, REBAS, KBL, INCANT, IRET)
         IF (IRET.GT.0) THEN
            MSGTXT = 'FGANT GETS PLOT ERROR FROM APLTFG - QUITTING'
            GO TO 990
         ELSE IF (IRET.LT.0) THEN
            MSGTXT = 'FGBASL: terminating by request'
            GO TO 990
            END IF
 50      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE APLTFG (IPLOT, PLTINC, REBAS, KBL, INCANT, IRET)
C-----------------------------------------------------------------------
C   APLTFG actually plots antenna data
C   Input:
C      IPLOT   I   Plot number on current page. If neg. then this is
C                   last plot.
C   Output:
C      IRET    I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IPLOT, REBAS, KBL, INCANT, IRET
      REAL      PLTINC
C
      INCLUDE 'FGPLT.INC'
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, CHTMP*18,
     *   XUNITS*20, NB1*4, TYPE*2, REASON*24
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), I, NGOOD, NNOFIT,
     *   ILINE, FGBUFF(512), LUNI, IFGRNO, KIF, FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), NFGROW, SOURID, SUBA, FREQID, ANTFG(2), IFS(2),
     *   CHANS(2), IBASE, NA1
      REAL      BLC(2), TRC(2), DX, DY, TR, VALUE, XY(2), XTRC(2),
     *   XBLC(2), TLC(2), YYOFF(2), SIZE, XMULT(2), TIMER(2), XMIN, YMIN
      LOGICAL   T, F, CATUP, PFLAGS(4), DROP
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      HOLLERITH CATH(256)
      EQUIVALENCE (CATBLK, CATH)
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA XUNITS /'IAT (hr)'/
C-----------------------------------------------------------------------
C                                       Time system from AN table
      XUNITS(1:3) = TIMLAB(1:3)
      TYPE = 'FG'
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC - (ABS (IPLOT)-1)*80
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
      AXTYP(LOCNUM) = 0
      XMULT(2) = 1
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      XMULT(1) = 1.0
C
      DO 50 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 50      CONTINUE
C                                       The following card is required
C                                       to label each plot's horizontal
C                                       asis
      CTYP(1,LOCNUM) = XUNITS
C                                       This stops y axis ticks/label
      CTYP(2,LOCNUM) = 'NO TICKS'
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
         ITYPE = 42
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
         CALL RFILL (4, 0.5, CHOUT)
         CHOUT(1) = 1.5
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *      1.333
C                                       default XYRATIO
         IF (XYRATO.LT.0.05) THEN
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
C
         TEXT(INP:) = 'Flagged intervals for'
         INP = INP + 22
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
         WRITE (TEXT(INP:),1100) TYPE, IVER
         INP = INP + 8
C                                       Stokes and IF
         IF ((XSTOK.EQ.'RL') .OR. (XSTOK.EQ.'LR')) THEN
            TEXT(INP:) = XSTOK(1:2) // 'pol_'
         ELSE
            TEXT(INP:) = XSTOK(:1) // 'pol_'
            END IF
         INP = INP + 7
         IF (BIF.EQ.EIF) THEN
            WRITE (TEXT(INP:),1200) BIF
         ELSE
            WRITE (TEXT(INP:),1300) BIF, EIF
            END IF
         INP = INP + 6
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF (LABEL.GT.1) THEN
            DY = 0.5 + 2 * 1.333
C                                       the first line of the header
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1400) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       Put on labels and ticks
C                                       To have ticks only at the
C                                       horizontal axis use the private
C                                       version of CLAB1
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Loop: label baselines
      VALUE = PLTINC
      ILINE = 0
      DO 100 IBASE = REBAS,REBAS+KBL-1
         IF (NAME1(IBASE)(:3).EQ.'VLA') THEN
            NB1 = NAME1(IBASE)(5:7)
         ELSE
            NB1 = NAME1(IBASE)(1:3)
            END IF
         NA1 = AN1(IBASE)
         XY(2) = VALUE
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
C                                       value for the next line
         VALUE = VALUE - XYOFF(2) * 2 * (NUMIF + INCANT)
         IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
            NNOFIT = NNOFIT + 1
         ELSE
            CALL GPOS (0, XY(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DX = 0.4
            DY = -0.5
C                                       label baselines
            IF (ISVLBA) THEN
               TEXT = NB1(:2)
            ELSE
               WRITE (TEXT,1500) NA1
               END IF
            CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GLTYPE (1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 100     CONTINUE
C                                       Open FG file
      LUNI = 27
      CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, IVER, CATBLK, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IERR)
      IF (IERR.EQ.2) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING FLAG TABLE'
         GO TO 990
         END IF
C                                       # rows in the table
      NFGROW = FGBUFF(5)
C                                       Loop and copy
      DO 200 I = 1,NFGROW
         CALL TABFLG ('READ', FGBUFF, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTFG, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING FLAG TABLE'
            GO TO 990
            END IF
         DROP = IERR.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((FRQSEL.GT.0) .AND. (FRQSEL.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this SUBARRAY selected ?
         DROP = DROP .OR. ((SUBARR.GT.0) .AND. (SUBARR.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       stokes
         DROP = DROP .OR. ( (XSTOK.EQ.'R' .AND. .NOT.PFLAGS(1)) .OR.
     *      (XSTOK.EQ.'L' .AND. .NOT.PFLAGS(2)) .OR.
     *      (XSTOK.EQ.'RL' .AND. .NOT.PFLAGS(3)) .OR.
     *      (XSTOK.EQ.'LR' .AND. .NOT.PFLAGS(4)))
C                                       timerange
         DROP = DROP .OR. (TSTO.LT.TIMER(1)) .OR. (TSTA.GT.TIMER(2))
C                                       channel
         IF (CHANS(1).LE.0) CHANS(1) = 1
         IF (CHANS(2).LE.0) CHANS(2) = CATBLK(KINAX+JLOCF)
         DROP = DROP .OR. (BCHAN.GT.CHANS(2)) .OR. (ECHAN.LT.CHANS(1))
C                                       IFs
         IF (IFS(1).LE.0) IFS(1) = 1
         IF (IFS(2).LE.0) IFS(2) = EIF
         DROP = DROP .OR. (BIF.GT.IFS(2)) .OR. (EIF.LT.IFS(1))
C                                       store the number of records
         IF (.NOT.DROP) THEN
            IFS(1) = MAX (IFS(1), BIF)
            IFS(2) = MIN (IFS(2), EIF)
            TIMER(1) = MAX (TIMER(1), TSTA)
            TIMER(2) = MIN (TIMER(2), TSTO)
C                                       all of current ones
            IF ((ANTFG(1).LE.0) .AND. (ANTFG(2).LE.0)) THEN
               DO 120 IBASE = REBAS,REBAS+KBL-1
                  VALUE = PLTINC - (IFS(1)-BIF-1) * 2 * XYOFF(2) -
     *               (IBASE - REBAS) * 2 * XYOFF(2) * (NUMIF+INCANT)
                  DO 110 KIF = IFS(1),IFS(2)
                     VALUE = VALUE - 2 * XYOFF(2)
                     XY(2) = XYSCL(2)*(VALUE-XYOFF(2)) + YYOFF(2)
                     XY(1) = TIMER(1) * 360.
                     XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                     CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
C                                       Draw the horizontal line to
C                                       TIMRE
                     XY(1) = TIMER(2) * 360.
                     XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                     CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                     IF (IERR.NE.0) GO TO 970
                     NGOOD = NGOOD + 1
 110                 CONTINUE
 120              CONTINUE
            ELSE IF (ANTFG(2).LE.0) THEN
               DO 140 IBASE = REBAS,REBAS+KBL-1
                  IF (AN1(IBASE).EQ.ANTFG(1)) THEN
                     VALUE = PLTINC - (IFS(1)-BIF-1) * 2 * XYOFF(2) -
     *                  (IBASE - REBAS) * 2 * XYOFF(2) * (NUMIF+INCANT)
                     DO 130 KIF = IFS(1),IFS(2)
                        VALUE = VALUE - 2 * XYOFF(2)
                        XY(2) = XYSCL(2)*(VALUE-XYOFF(2)) + YYOFF(2)
                        XY(1) = TIMER(1) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        XY(1) = TIMER(2) * 360.
                        XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                        CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                        IF (IERR.NE.0) GO TO 970
                        NGOOD = NGOOD + 1
 130                    CONTINUE
                     END IF
 140              CONTINUE
               END IF
            END IF
 200    CONTINUE
C                                       Close the table
      CALL TABIO ('CLOS', 0, IFGRNO, FGBUFF, FGBUFF, IERR)
C                                       Done: finish plot
      WRITE (MSGTXT,1600) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1700) NNOFIT
         CALL MSGWRT (2)
         END IF
      REBAS = REBAS + KBL - 1
C
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
         IERR = 0
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
      WRITE (MSGTXT,1600) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1700) NNOFIT
         CALL MSGWRT (2)
         END IF
C
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
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1100 FORMAT (A2,I4,'_')
 1200 FORMAT ('IF ',I2)
 1300 FORMAT ('IF ',I2,' - ',I2)
 1400 FORMAT ('Plot file version',I4,'__created ',A, A)
 1500 FORMAT (I2.2)
 1600 FORMAT ('APLTFG:',I9,' lines plotted')
 1700 FORMAT ('APLTFG:',I9,' points did not fit')
 1960 FORMAT ('APLTFG: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('APLTFG: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE FILANT (DISK, CNO, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, NSUBA, NBASE, ANT1, ANT2, NAME1, NAME2, STNS, ISVLBA,
     *   SCRTCH, IRET)
C-----------------------------------------------------------------------
C   Fills in 2 arrays with all possible cominations of antenna numbers.
C   Find number and list of selected antennas.
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
C      NAME1    C(*)*8   1st antenna name of baseline pairs selected
C      NAME2    C(*)*8   2nd antenna name of baseline pairs selected
C      STNS     C(*)*8   station names
C      SCRTCH   I(*)   I/O buffer and related storage.
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                           10 = no AN files.
C   Output in common:
C      ANTENS   I(*)     Array of selected antennas
C      NANTSL   I        Number of selected antennas
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN, IXANT(50), IXBASL(50),
     *   NXANT, NXBASL, NSUBA, NBASE, ANT1(*), ANT2(*), SCRTCH(*),
     *   SUBSEQ(50), IRET, IANT
      LOGICAL   DESEL, ISVLBA
      CHARACTER STNS(*)*8, NAME1(*)*8, NAME2(*)*8
C
      INTEGER   NBUFF, II, NUMREC, J, MXNSTA, I1, IERR, ICNT, K, I
      LOGICAL   ACCEPT, REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
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
      ISVLBA = ANAME(1:4).EQ.'VLBA'
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
         SUBSEQ(II) = NOSTA
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
                  NAME1(NBASE) = STNS(I1)
                  NAME2(NBASE) = STNS(J)
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Find number of selected antennas
C                                       and their list
      IANT = 0
      DO 70 I = 1,50
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

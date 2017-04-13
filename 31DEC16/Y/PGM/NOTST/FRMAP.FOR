LOCAL INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INTEGER MXBUFF, MXBUF2, MXBUFT, MXAN, MXBAS, MXTIM, MXSET,
     *   MXCOMP,  MXSETC, MXPTX, MXPTY, MXPONT
      INTEGER MAXSAT
      PARAMETER (MAXSAT = 5*6)
      PARAMETER (MXPTX = 250)
      PARAMETER (MXPTY = 250)
      PARAMETER (MXPONT = 256)
      PARAMETER (MXCOMP = 20)
      PARAMETER (MXAN = 30)
      PARAMETER (MXBAS = 400)
      PARAMETER (MXTIM = 10)
      PARAMETER (MXSET = (MXBAS * MXTIM))
      PARAMETER (MXSETC = (MXSET * (MXCOMP/4)))
      PARAMETER (MXBUFF = (MXSET * MXPONT * 2))
      PARAMETER (MXBUF2 = (2*MXBUFF))
      PARAMETER (MXBUFT = (4*MXPONT))
      CHARACTER NAMEIN*12, CLAIN*6, OFILE*48, XSOUR(30)*16, XCALCO*4,
     *   STNS(50)*8, SAUCE*16
      HOLLERITH XNAMEI(3), XCLAIN(2), XOFILE(12), XXSOUR(4,30), XXSTOK,
     *   XXCALC
      REAL USERID, XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XUVR(2),
     *   XTIME(8), XBIF, XBCHAN, XECHAN, XCHAN, XSUBA, XANTEN(50),
     *   XBASE(50),  XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), APARM(10), BPARM(10), XLABEL,
     *   XBADD(10), XDOTV, XGRCH,
     *   PREAVG(MXSET), TIME1, TIME2, RATE(MXCOMP), AMPP(MXCOMP),
     *   FI(MXCOMP), DDRATE(MXCOMP), DDAMP(MXCOMP), DDFI(MXCOMP),
     *   PERIOD, DTIME, ACCUR, BLC(2), TRC(2), BUFF1(512), MYHI,
     *   SUMWT(MXBUFF), BUFF2(MXBUF2), CHOUT(4), MXLE, MXRI, MYLO
      DOUBLE PRECISION XA(MAXANT), YA(MAXANT), ZA(MAXANT),
     *   ORBITA(MAXSAT), LAMDA, ARRLON
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, PVER, NMAX,
     *   LUNPR, PFIND, TVCHN, TVCORN(4), PBCHAN, REFCHA, LEVPR, PECHAN,
     *   NCHANP, NIFP, NTRANS, NTIMEP(MXSET), NPADZ, ANT1(MXBASE),
     *   ANT2(MXBASE), NCHAVG,  NBASE, NCHAN, PLTBLK(256), IGLUN,
     *   IGFIND, SLOT, PLMAP, PLFIND, GRCHN, MMMAX, LABEL, LTYPE,
     *   IORBIT(MAXANT)
      LOGICAL   MULTI, DOTV, WPRINT, FA
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR,
     *   XQUAL, XXCALC, XBAND, XFREQ, XFQID, XUVR, XTIME, XXSTOK, XBIF,
     *   XBCHAN, XECHAN, XCHAN, XSUBA, XANTEN, XBASE, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, APARM,
     *   BPARM, XLABEL, XOFILE, XBADD, XDOTV, XGRCH,
     *   SEQIN, SEQOUT, DISKIN, CNOIN, NBASE, PBCHAN, PECHAN, NIFP,
     *   NCHANP, NTRANS, NTIMEP, NPADZ, NCHAVG, REFCHA, LEVPR, NCHAN,
     *   PLMAP, PLFIND, MMMAX, LABEL, IORBIT, LTYPE
      COMMON /VALPRM/ PREAVG, TIME1, TIME2, PERIOD, DTIME, ACCUR, ANT1,
     *   ANT2, MXLE, MXRI, MYLO, MYHI
      COMMON /FRRATE/ XA, YA, ZA, ORBITA, LAMDA, ARRLON, RATE, AMPP, FI,
     *   DDRATE, DDAMP,DDFI, NMAX
      COMMON /CHPARM/ NAMEIN, CLAIN, OFILE, XSOUR, XCALCO, STNS, SAUCE
      COMMON /PLOT/ PLTBLK, BLC, TRC, CHOUT, DOTV, TVCHN, GRCHN,
     *   TVCORN, IGLUN, IGFIND, SLOT, LUNPR, PFIND, FA
      COMMON /LABEL/ MULTI, PVER, WPRINT
      COMMON /BUFRS/ BUFF1, SUMWT, BUFF2
LOCAL END
      PROGRAM FRMAP
C-----------------------------------------------------------------------
C! FRMAP finds the source image using fringe rate spectra
C# Spectral Astrometry Coordinates UV VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000-2004, 2006-2007, 2009-2015
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
C   Build the map of the source using fringe rate spectra for selected
C   uv data.
C   Calibration and editing may be optionally applied.
C   Inputs:
C      AIPS adverb          Description.
C      USERID               User number ignored
C      INNAME               Input UV file name (name)
C      INCLASS              Input UV file name (class)
C      INSEQ                Input UV file name (seq. #)
C      INDISK               Input UV file disk unit #
C      SOURCES              Source list
C      QUAL                 Source qualifier -1=>all
C      CALCODE              Calibrator code '    '=>all
C      SELBAND              Bandwidth to select (kHz)
C      SELFREQ              Frequency to select (MHz)
C      FREQID               Freq. ID to select.
C      UVRANG               UV range to be plotted
C      TIMERANG             Time range to be plotted
C      STOKES               Stokes type to select.
C      BIF                  Lowest IF number 0=>1
C      EIF                  EIF = BIF
C      BCHAN                Lowest channel number 0=>all
C      ECHAN                Highest channel number 0=>all
C      SUBARRAY             Subarray, 0=>all
C      ANTENNAS             Antennas to select
C      DOCALIB              If >0 calibrate data
C      GAINUSE              CL (or SN) table to apply
C      DOPOL                If >0 correct polarization.
C      FLAGVER              Flag table version
C      DOBAND               If >0 apply bandpass cal.
C      BPVER                Bandpass table version
C      APARM                Control information.
C      BPARM                More control information.
C      OUTFILE              Filename in which to write
C                           map's components.
C      BADDISK              Disks to avoid for scratch
C      DOTV                 > 0 Do plot on the TV, else
C                           make a plot file
C
C     Programmer L. Kogan, September 1994
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, NPARMS
      INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'FRMAP '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL FRIN (PRGM, NPARMS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Accumulate the uv data.
      CALL FRUV (NPARMS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FRIN (PRGN, NPARMS, JERR)
C-----------------------------------------------------------------------
C   FRIN gets input parameters for FRMAP, finds the arrays of selected
C   baselines, list and number of selected antennas.
C   Input parameters:
C      PRGN     C*6   Program name
C   Output parameters:
C      NPARM    I    Number of real words read from AIPS
C      JERR     I    Error code: 0 => ok
C                       5 => catalog troubles
C                       8 => can't start
C   Input from common
C      KINAX    I(7)       Number of pixels on each axis.
C      APARM    R(10)      AIPS input parameter values.
C      BPARM    R(10)      AIPS input parameter values.
C      XBAND    R          Bandwidth to select.
C      XDOCAL   R          Apply calibration ? +ve => yes.
C      XDOPOL   R          Do polarization corrections ? +ve => yes.
C      XDOTV    R          Plot device; 0=>TV; -1=> PL file.
C      XFREQ    R          Frequency to select.
C      JLOCF    I          Order in data of frequency.
C      JLOCIF   I          Order in data of IF.
C   Input/output via common
C      BUFF1    R(256)     Buffer.
C      BPVER    I          BP table version to apply; 0=> highest.
C      DOBAND   I          >0 if bandpass calibration (-1).
C      FGVER    I          Flag table version number;
C                          -ve => no flagging; 0=> use highest ver.
C      CLUSE    I          Cal (CL or SN) file version no to apply.
C      CLVER    I          CL table version to apply.
C      DOCAL    L          If true apply calibration else not.
C                          correcting delay errors.
C      STNS     C(28)*8    Station names.
C      CLAIN    C*6        Input file class.
C      NAMEIN   C*12       Input file name.
C      OFILE    C*48       File name for printed output.
C      SAUCE    C*16       First source name.
C      XCALCO   C*4        Calibrator code.
C      DOPOL    I          If >0 then correct for feed polarization
C                          based on antenna file information.
C      XANTEN   R(50)      Antennas to select.
C      XCLAIN   H(2)       Input file class.
C      XNAMEI   H(3)       Input file name.
C      XOFILE   H(12)      File name for printed output.
C                          frequency (1=type,2=width,3=support).
C      XTIME    R(8)       Timerange to select (d,h,m,s,d,h,m,s).
C      XUVR     R(2)       UV range to select.
C      XXSOUR   H(4,30)    Source names.
C      CNOIN    I          Input file catalog number.
C      DISKIN   I          Input file disk number.
C      NCHANP   I          Channel range to map
C      NCHAVG   I          No of frequency channels to pre-average.
C      NIFP     I          IF range to map
C      NPADZ    I          Fringe rate FFT padding factor
C      NTIMEP   I          No of time bins for pre-averaging.
C      NTRANS   I          No of points in the fringe rate FFT.
C      PBCHAN   I          Start frequency channel to map.
C      PECHAN   I          End frequency channel to map.
C      BIF      I          Selected IF
C      IEF      I          EIF = BIF
C      SEQIN    I          Input file sequence number.
C      USERID   R          User AIPS number.
C      XBCHAN   R          Start frequency channel to select.
C      XBIF     R          Start IF to select.
C      XBPVER   R          BP table to apply.
C      XDISIN   R          Input file disk number.
C      XDOBND   R          Apply bandpass correction ? +ve => yes.
C      XECHAN   R          End frequency channel to select.
C      XFLAG    R          Flag table version to apply.
C      XFQID    R          Freq. id to select.
C      XGUSE    R          CL table to apply.
C      XQUAL    R          Source qualifier -1 => all.
C      XSIN     R          Input file sequence number.
C      XSUBA    R          Subarray to select.
C      XXCALC   H          Calibrator code.
C      XXSTOK   H          Stokes type to select eg.'RR','LL','RL'
C      MULTI    L          Multi-source file ?
C      CATBLK   I(256)     Catalog header block.
C      MSGTXT   C*80       AIPS message string.
C      TSKNAM   C*6        AIPS task name.
C      NLUSER   I          User number.
C      TVCORN   I(4)       Set to zeroes.
C      DOTV     L          Plot on TV device ?
C      TVCHN    I          TV channel.
C      ANTENS   I(50)      List of antennas selected
C      CATUV    I(256)     Catalog header.
C      TIMRNG   R(8)       Start d,h,m,s; end d,h,m,s (0=>all).
C      UVRNG    R(2)       Min and max baseline lengths to select.
C                          1000's wavelengths; 0's => all.
C      BCHAN    I          First frequency channel selected (1-rel).
C      ECHAN    I          Last frequency channel selected 0=>all.
C      FRQSEL   I          Default FQ table entry to select (-1.0)
C      IUCNO    I          Input file catalog number.
C      IUDISK   I          Input file disk no.
C      IXLUN    I          LU number for use by UVGET.
C      SELBAN   R          Bandwidth (Hz) to select (-1.0).
C      SELFRQ   D          Frequency (Hz) to select (-1.0).
C      SELQUA   R          Source qualifier to select; -1 => all.
C      SUBARR   I          Subarray selected; 0 => all.
C      UDISK    I          Input file disk number.
C      USEQ     I          Input file sequence number.
C      CALSOU   C(30)*16   Calibrator source to select.
C      SOURCS   C(30)*16   Sources to select; *=> all;
C                          prefix - => all except those listed.
C      SELCOD   C*4        Calibrator code to select.
C      STOKES   C*4        Stokes type wanted.
C      UCLAS    C*6        AIPS class of input file.
C      UNAME    C*12       AIPS name of input file.
C      PREAVG   R          Pre-average time interval in minutes
C      TIME1    R          Start time of data to FFT.
C      TIME2    R          Stop time of data to FFT.
C----------------------------------------------------------------------
      CHARACTER PRGN*6, STAT*4, UTYPE*2
      INTEGER   NPARMS, JERR, IROUND, IERR, I, LUNTB, IVER, IANT,
     *   NANT, LUNAN, IABUF(512), I4TEMP, IUSER, LUN, IDUM, ISTA(50),
     *   IXANT(50), IXBASL(50), NXANT, NXBASL, TNIF, IFQ,
     *   IORB, IORPRM
      REAL  EPS, TEMP, STARTD, STOPD
      DOUBLE PRECISION FRQREF
      LOGICAL   F, TABLE, FITASC, MATCH, DESEL, DOORB
      INCLUDE 'FRMAP.INC'
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION CATD(128)
      EQUIVALENCE (CATBLK, CATD)
      DATA F /.FALSE./
      DATA LUNTB, LUNAN, LUN / 39, 27, 28 /
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       DOXCOR = .TRUE.
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARMS = 305
      CALL GTPARM (PRGN, NPARMS, RQUICK, USERID, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBARR = IROUND (XSUBA)
      IF (SUBARR .LE. 0) SUBARR = 1
      LEVPR = IROUND (BPARM(9))
      BPARM(9) = LEVPR
      EPS = 0.1
      USERID = NLUSER
      IUSER = NLUSER
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
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0)  THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      IUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL RCOPY (2, XUVR, UVRNG)
      IF (UVRNG(1).EQ.0.0) UVRNG(1) = 1.E-9
C                                       Timerange
C                                       Determine start/stop of data
      CALL UVTIME (DISKIN, CNOIN, CATBLK, STARTD, STOPD, JERR)
      IF (JERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1025)
         GO TO 990
         END IF
      TIME1 = TIMRNG(1) + TIMRNG(2) / 24.0 + TIMRNG(3) / 1440.0 +
     *   TIMRNG(4) / 86400.0
      TIME2 = TIMRNG(5) + TIMRNG(6) / 24.0 + TIMRNG(7) / 1440.0 +
     *   TIMRNG(8) / 86400.0
      IF (TIME1 .EQ. 0.0) TIME1 = -1.0E6
      IF (TIME2 .EQ. 0.0) TIME2 = +1.0E6
      IF (TIME1 .LT. STARTD) TIME1 = STARTD
      IF (TIME2 .GT. STOPD) TIME2 = STOPD
C                                       Reverse inverted time range.
      IF (TIME2.LT.TIME1) THEN
         TEMP = TIME1
         TIME1 = TIME2
         TIME2 = TEMP
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
C                                       Save input header
      CALL RCOPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Determine if multi-source file
      CALL MULSDB (CATUV, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *      MULTI, FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       Channels and IF
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      NCHAN = CATBLK(KINAX+JLOCF)
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NUMPOL = CATBLK(KINAX+JLOCS)
      REFCHA = IROUND (XCHAN)
      IF (REFCHA.EQ.0) REFCHA = (BCHAN + ECHAN) / 2
      PBCHAN = BCHAN
      PECHAN = ECHAN
      XBCHAN = BCHAN
      XECHAN = ECHAN
C                                       Force BIF = EIF
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = BIF
      ELSE
         BIF = 1
         EIF = 1
         END IF
      XBIF = BIF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IFQ = FRQSEL
      IF (IFQ .LE. 0) IFQ =1
      IF (FRQSEL.EQ.0) FRQSEL = -1

      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       read FQ
      IVER = 1
      CALL CHNDAT ('READ', IABUF, DISKIN, CNOIN, IVER, CATBLK, LUN,
     *   TNIF, FOFF, ISBAND, FINC, BNDCOD, IFQ, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT, 1090) IFQ
         GO TO 990
         END IF
C                                       frequency in HZ
C                                       for the specified BIF and FQID
      FRQREF = CATD(KDCRV+JLOCF) + FOFF(BIF)
C      LAMDA = (VELITE/FRQREF) * 1.D6
C                                       wavelength in meters
C                                       for the specified BIF and FQID
      LAMDA = VELITE / FRQREF
C                                       Read antenna header -> NANT
      IVER = SUBARR
      CALL ANTINI ('READ', IABUF, DISKIN, CNOIN, IVER, CATBLK, LUNAN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, JERR)
      IF (JERR.NE.0) GO TO 999
      I4TEMP = IABUF(5)
      NANT = I4TEMP
      ARRLON = 0.0D0
C                                       use LH system
      IF ((ARRAYC(1).NE.0.0D0) .OR. (ARRAYC(2).NE.0.0D0)) ARRLON =
     *   ATAN2 (-ARRAYC(2), ARRAYC(1))
C                                       IORB is an orbital antenna
C                                       number
      IORB = 0
      DO 35 IANT = 1,NANT
         IANRNO = IANT
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, JERR)
         IF (JERR.NE.0) GO TO 990
         ISTA(IANT) = NOSTA
         STNS(NOSTA) = ANNAME
C
         IORBIT(NOSTA) = 0
         DOORB = MNTSTA .EQ. 2
         IF (DOORB) THEN
            IORB = IORB + 1
            IORBIT(NOSTA) = IORB
            DO 25 I = 1,6
               IORPRM = I + (IORB-1)*6
               ORBITA(IORPRM) = ORBPRM(I)
   25          CONTINUE
C                                       Store station coordinates
C                                       We use Left Hand (LH) system
         ELSE
            XA(NOSTA) = STAXYZ(1)
            YA(NOSTA) = -STAXYZ(2)
            ZA(NOSTA) = STAXYZ(3)
            END IF
 35      CONTINUE
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, JERR)
      IF (JERR.NE.0) GO TO 990
C                                       BADDISK
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      IUDISK = UDISK
      IUCNO = CNOIN
      IXLUN = 28
      DO 75 I = 1,30
         SOURCS(I) = ' '
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         CALSOU(I) = ' '
 75      CONTINUE
      SAUCE = SOURCS(1)
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
C                                       Fill in list of all antenna
C                                       - baseline pairs and names.
C                                       Determine the list and number
C                                       of selected antennas.
      CALL SETANT (50, XANTEN, XBASE, NXANT, NXBASL, IXANT, IXBASL,
     *   DESEL)
      CALL FILANT (DISKIN, CNOIN, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, SUBARR, NBASE, ANT1, ANT2, STNS, IABUF, JERR)
      IF (JERR.NE.0) GO TO 999
C
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BPVER = IROUND (XBPVER)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Time between intervals of
C                                       averaging in days
      PERIOD = BPARM(2) / (60.0 * 24.0)
      IF (PERIOD.EQ.0.0) PERIOD = TIME2 - TIME1 + 1.0E-6
      BPARM(2) = PERIOD * 60.0 * 24.0
C                                       interval of averaging in days
      DTIME = BPARM(3) / (60.0 * 24.0)
      IF (DTIME.EQ.0.0) DTIME = TIME2 - TIME1
      IF (DTIME.GT.PERIOD) DTIME = PERIOD
      BPARM(3) = DTIME * 60.0 * 24.0
C                                       No of frequency channels to
C                                       pre-average.
      NCHAVG = 1
      IF (APARM(3).GT.1) NCHAVG = APARM(3) + 0.1
      APARM(3) = NCHAVG
C                                       FFT zero padding option
      NPADZ = 1
      IF (APARM(2).GT.0) NPADZ = APARM(2) + 1.1
      APARM(2) = NPADZ - 1
C                                       Compute IF, chan, time axes
C                                       for pre-average buffer.
      NIFP = EIF - BIF + 1
      NCHANP = (PECHAN - PBCHAN)/NCHAVG + 1

C                                       Check Stokes
      CALL SETSTK (STOKES, DOCAL, IDUM)
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('FRIN: UNABLE TO DETERMINE DATA START/STOP TIME')
 1025 FORMAT ('Apply INDXR to get NX table')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1090 FORMAT ('FRIN: ERROR READING INPUT FQ/CH TABLE FOR FQID=', I3)
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE FRUV (NPARM, IRET)
C-----------------------------------------------------------------------
C   FRUV loops through the data averaging according to the selection
C   and command criteria, finds an array of fringe rate for each
C   baseline and time, and builds the map separately for each group
C   of avereged frequency channels.
C   Input parameters:
C      NPARM    I    Number of real words read from AIPS
C   Output parameter:
C      IRET     I     Return error code, 0=>OK, otherwise error.
C   Input from common
C      KDCRV    D(7)       Coordinate value at ref pixel.
C      KHDOB    H(2)       Observation date in format ('DD/MM/YY').
C      KINAX    I(7)       Number of pixels on each axis.
C      KRCIC    R(7)       Coordinate value increment along axis.
C      KRCRP    R(7)       Coordinate reference pixel.
C      NCHANP   I          No of groups of averaged channels.
C      NCHAVG   I          No of frequency channels to pre-average.
C      NIFP     I          IF range to map
C      PBCHAN   I          Start frequency channel to map.
C      PECHAN   I          End frequency channel to map.
C      BIF      I          Selected IF
C      EIF      I          EIF = BIF
C      CATBLK   I(256)     Catalog header block.
C      CURSOU   I          Current source number.
C      NANTSL   I          Number of antennas selected.
C      DEC      D          Declination (1950) (deg).
C      ILOCB    I          Offset from start of vis rec for baseline.
C      ILOCT    I          Offset from start of vis record for Time.
C      ILOCU    I          Offset from start of vis record for U.
C      ILOCV    I          Offset from start of vis record for V.
C      ILOCW    I          Offset from start of vis record for W.
C      INCF     I          Increment in data for frequency.
C      INCIF    I          Increment in data for IF.
C      INCS     I          Increment in data for Stokes.
C      JLOCF    I          Order in data of frequency.
C      JLOCIF   I          Order in data of IF.
C      JLOCS    I          Order in data of Stokes parameters.
C      TIME1    R          Start time of selected data
C      TIME2    R          Stop time of selected data
C   Input/output via common
C      BUFF2    R(*)       Pre-average data buffer.
C      SUMWT    R(*)       Accumulated weights for pre-averaged data.
C      NUMIF    I          Number of IF's in input file.
C      NUMPOL   I          No of polarizations in input file.
C      NTIMEP   I          No of time bins for pre-averaging.
C      NTRANS   I          No of points in the fringe rate FFT.
C      MSGTXT   C*80       AIPS message string.
C      NCHAN    I          Total no of freq channels in data file.
C      CATUV    I(256)     Catalog header.
C      NVIS     I          Number of visibilities.
C-----------------------------------------------------------------------
      INTEGER NPARM, INDSOU, IRET
      INCLUDE 'FRMAP.INC'
      INTEGER   LOOPS, LOOPIF, LOOPF, INP, LUN3, JRE, JIM, JRE0, JIM0,
     *   IANT1, IANT2, XCOUNT, INDEX, ITIME, IADDW, IADDW0, IADDR,
     *   IADDR0, NBUFF, NBUFF2, ITEMP, ITEMP0, ICHNO, I, IBASE, II, ID,
     *   KK, ITBASE, NBTIM, LPASS, NCH, ITRIM, NTOT, NMISS, BCHANT,
     *   ECHANT, NCHAVT, NCHANT, LMAX(MXSET), ADDD(MXSET), ADDDD(MXSET),
     *   NMCHAN, CHAN, KDAY, KH, KM, KS, ADD, ISET, INTIM, IT, NUMTIM,
     *   IROUND, NX, NY, MMXTIM
      LOGICAL   NLARGE
      CHARACTER LINE*132
      REAL AMPREF(MXSET), BTEMPR, BTEMPI, AMPLIT, PHASE, AMPLIR, PHASER,
     *   UU(MXSET), VV(MXSET), U, V, W, UDOT, VDOT, WDOT,  DX, DY,
     *   FRATE(MXSETC), FLUXX(MXSETC), DFRATE, BEGT(MXTIM), ENDT(MXTIM),
     *   MIDT(MXTIM), UMAX, VMAX, COEF, UVMID, DFRMID,
     *   RXM, RYM,FRMAX, AVER, RPARM(20), XNORM, WT, VIS(4098), TOTWT,
     *   PREAVS, RX, RY, DRX, DRY, AMXLE, AMXRI, AMYLO, AMYHI, AMAXX,
     *   AMAXY
      DOUBLE PRECISION TIMEN, GSTRA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN3 /25/
C-----------------------------------------------------------------------
      MMXTIM = MXTIM
      WPRINT = .FALSE.
      IF (OFILE(1:1).NE.' ') WPRINT = .TRUE.
C                                       open the outfile
      IF (WPRINT) THEN
         LUNPR = 10
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, FA, IRET)
         END IF
      NUMTIM = (TIME2 - TIME1) / PERIOD + 0.99
      IF (NUMTIM .GT. MMXTIM) THEN
         WRITE (MSGTXT,1120) NUMTIM
         IRET = 2
         GO TO 990
         END IF
      NBTIM = NUMTIM * NBASE
C                                       Position shift calculations
C                                       Main averaging loop
      XCOUNT = 0
      NTOT = 0
      TOTWT = 0.0
C                                       Get info about the source
      CALL GETSO (SAUCE, IUDISK, IUCNO, CATUV, LUN3, INDSOU, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       rotate coordinate system for VLA
      GSTRA = GSTIA0*DG2RAD - ARRLON
      ITBASE = 0
      UMAX = 0.0
      VMAX = 0.0
      UVMID = 0.0
      COEF = PI / (180.0 * 60.0 * 60.0)
      DO 30 IT = 1, NUMTIM
         BEGT(IT) = TIME1 + (IT - 1) * PERIOD
         ENDT(IT) = BEGT(IT) + DTIME
C                                       media of time of averaging
C                                       in days
         MIDT(IT) = (BEGT(IT) + ENDT(IT)) / 2.0
         TIMEN = MIDT(IT)
         DO 20 IBASE = 1, NBASE
            ITBASE = ITBASE + 1
            CALL UUVV (TIMEN, IBASE, GSTRA, U, V, W, UDOT, VDOT, WDOT)

C                                       consider DX = DALFA, so
C                                       there is COS(DECAPP) in U
C
C                                       UDOT, VDOT in mHz/milli arcsec
            UDOT = -COEF * UDOT * COS(DECAPP) / LAMDA
            UU(ITBASE) = UDOT
            VDOT = -COEF * VDOT / LAMDA
            VV(ITBASE) = VDOT
C                                       compute maxU, maxV, midUV
            IF (ABS(UDOT).GT.UMAX) UMAX = ABS(UDOT)
            IF (ABS(VDOT).GT.VMAX) VMAX = ABS(VDOT)
            UVMID = UVMID + ABS(UDOT) + ABS(VDOT)
   20       CONTINUE
   30    CONTINUE
      UVMID = UVMID / (2 * NUMTIM * NBASE)
C                                       RX, RY are semisize of window
C                                       in mili arcsec (phase<pi/2)
      COEF = 500.0 / (DTIME * DTIME * 86400)
      RXM = COEF * TAN(DECAPP) / VMAX
      RYM = COEF / (UMAX * TAN(DECAPP))
      RXM = ABS(RXM)
      RYM = ABS(RYM)
      IF ((APARM(6).EQ.0.0) .OR. (APARM(7).EQ.0)) THEN
         APARM(6) = RXM
         APARM(7) = RYM
         APARM(8) = 0.0
         APARM(9) = 0.0
         END IF
      RX = APARM(6)
      RY = APARM(7)
      DRX = APARM(8)
      DRY = APARM(9)
C                                       Steps at X and Y axis
      IF ((APARM(4).EQ.0) .OR. (APARM(5).EQ.0)) THEN
         APARM(4) =  RX / 50
         APARM(5) =  RY / 50
         END IF
      DX = APARM(4)
      DY = APARM(5)
C                                       Number of rectangulars at X and Y
C                                       Provide the odd number with
C                                       central at zero
      NX = RX / DX
      RX = DX * (NX + 0.5)
      NX = 2*NX + 1
      NY = RY / DY
      RY = DY * (NY + 0.5)
      NY = 2*NY + 1
      IF ((NX.GT.MXPTX) .OR. (NY.GT.MXPTY)) THEN
         IRET = 1
         WRITE (MSGTXT,1005) NX, NY, MXPTX, MXPTY
         GO TO 990
         END IF
C                                       Print maximum and selected window
         WRITE (MSGTXT,1006) RXM, RYM
         CALL MSGWRT (8)
         WRITE (MSGTXT,1007) RX, RY
         CALL MSGWRT (8)
      MXLE = DRX - RX
      MXRI = DRX + RX
      MYLO = DRY - RY
      MYHI = DRY + RY
      AMXLE = ABS(MXLE)
      AMXRI = ABS(MXRI)
      AMYLO = ABS(MYLO)
      AMYHI = ABS(MYHI)
      AMAXX = MAX(AMXLE, AMXRI)
      AMAXY = MAX(AMYLO, AMYHI)
C                                       Calculate array of time
C                                       averaging PREAVG depending upon
C                                       the maximum fringe rate
      ITBASE = 0
      ADD = 0
      NLARGE = .FALSE.
C                                       Pre-average intervals in days
      DO 50 IT = 1, NUMTIM
         DO 40 IBASE = 1, NBASE
            ITBASE = ITBASE + 1
            FRMAX = ABS(AMAXX * UU(ITBASE)) + ABS(AMAXY * VV(ITBASE))
            IF (APARM(1).GT.0.0) THEN
               PREAVG(ITBASE) = APARM(1) / 86400.0
            ELSE
               IF (APARM(1).EQ.0.0) PREAVG(ITBASE) = DTIME/256
               IF (APARM(1).LT.0.0)
     *            PREAVG(ITBASE) = 1.0 / (4.0 * FRMAX  * 86.4)
               END IF
C            IF(PREAVG(ITBASE).GT.DTIME / 32)
C     *         PREAVG(ITBASE) = DTIME / 32
C            IF(PREAVG(ITBASE).LT.DTIME / 512)
C     *         PREAVG(ITBASE) = DTIME / 512
            PREAVS = PREAVG(ITBASE)*86400
            NTIMEP(ITBASE) = DTIME / PREAVG(ITBASE) + 0.1
C                                       Force no of time bins to be even
            NTIMEP(ITBASE) = NTIMEP(ITBASE) - MOD (NTIMEP(ITBASE), 2)
            IF (NTIMEP(ITBASE)*NPADZ*2.GT.MXBUFT) NLARGE = .TRUE.
            ADDD(ITBASE) = ADD
            ADD = ADD + NTIMEP(ITBASE) * NPADZ
   40       CONTINUE
   50    CONTINUE
      NBUFF2 = (ADDD(NBTIM) + NTIMEP(NBTIM)*NPADZ)*NIFP * 2
      NBUFF = 2 * NBUFF2
      IF ((NBUFF.GT.MXBUF2) .OR. NLARGE) THEN
         WRITE (MSGTXT,1130)
         IRET = 2
         GO TO 990
         END IF
C                                       Zero output array
      CALL RFILL (NBUFF, 0.0, BUFF2)
      CALL RFILL (NBUFF2, 0.0, SUMWT)
C                                       write header to the file
      IF (WPRINT) THEN
         WRITE (LINE,1045) SAUCE
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
         WRITE (LINE,1050)
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
         DO 60 IT = 1, NUMTIM
            TIMEN = MIDT(IT)
            KDAY = TIMEN
            KH = (TIMEN - KDAY) * 24
            KM = ((TIMEN - KDAY)*24 - KH) * 60 +0.001
            KS = ((TIMEN - KDAY)*24*60 -KH*60 -KM) * 60 + 0.001
            WRITE (LINE,1060) KDAY, KH, KM, KS
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
   60       CONTINUE
         IF (REFCHA.GE.0) THEN
            WRITE (LINE,1070) REFCHA
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
         ELSE
            WRITE (LINE,1080)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
            END IF
         DO 70 I = 1, 3
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
   70       CONTINUE
         END IF
      DO 700 LPASS = 1,2
C                                       LPASS=1 => reference channel
         ICHNO = LPASS
         IF (LPASS.EQ.1) THEN
            BCHANT = PBCHAN
            ECHANT = PECHAN
            NCHAVT = NCHAVG
            NCHANT = NCHANP
            PBCHAN = REFCHA
            PECHAN = REFCHA
            NCHAVG = 1
            NCHANP = 1
         ELSE
            PBCHAN = BCHANT
            PECHAN = ECHANT
            NCHAVG = NCHAVT
            NCHANP = NCHANT
            END IF
C                                       D'nt do the first pass if
C                                       referencing is not provided
         IF ((REFCHA.LT.0) .AND. (LPASS.EQ.1)) GO TO 700
         DO 600 CHAN = 1, NCHANP
C                                       Force to zero BUFF2 and SUMWT
C                                       for the second pass
            DO 160 LOOPIF = BIF, EIF
               ITBASE = 0
               DO 150 IT = 1, NUMTIM
                  DO 140 IBASE = 1, NBASE
                     ITBASE = ITBASE + 1
                     NTRANS = NTIMEP(ITBASE) * NPADZ
                     ITEMP = (1 + 2*(LOOPIF-BIF)) *
     *                  (ADDD(NBTIM) + NTIMEP(NBTIM)*NPADZ) +
     *                  ADDD(ITBASE)
                     DO 130 ITIME = 1,NTRANS
                        IADDW = ITEMP + ITIME
                        IADDR = 2 * IADDW -1
                        JRE = IADDR
                        JIM = IADDR + 1
                        SUMWT(IADDW) = 0.0
                        BUFF2(JRE) = 0.0
                        BUFF2(JIM) = 0.0
  130                CONTINUE
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
            BCHAN = (CHAN-1) * NCHAVG + PBCHAN
            ECHAN = BCHAN + NCHAVG - 1
            NMCHAN = BCHAN
            IF (ECHAN.GT.PECHAN) ECHAN = PECHAN
C                                       Init vis file for reading
            CALL UVGET ('INIT', RPARM, VIS, IRET)
            IF (IRET.NE.0) THEN
               IF (IRET.EQ.-1) THEN
                  WRITE (MSGTXT,1000) IRET
                  GO TO 990
               ELSE
                  GO TO 999
                  END IF
               END IF

            DO 300 II = 1,NVIS
               CALL UVGET ('READ', RPARM, VIS, IRET)
               IF (IRET.LT.0) GO TO 400
               IF (IRET.NE.0) GO TO 999
C                                       Select our source
               IF (INDSOU .NE. 0 .AND. CURSOU.NE.INDSOU) GO TO 300
C                                       Determine antenna numbers
               IF (ILOCB.GE.0) THEN
                  IANT1 = RPARM(ILOCB+1) / 256.0 + 0.1
                  IANT2 = RPARM(ILOCB+1) - 256 * IANT1 + 0.1
               ELSE
                  IANT1 = RPARM(ILOCA1+1) + 0.1
                  IANT2 = RPARM(ILOCA2+1) + 0.1
                  END IF
               DO 170 IBASE = 1, NBASE
                  IF ((IANT1.EQ.ANT1(IBASE))
     *               .AND. (IANT2.EQ.ANT2(IBASE))) GO TO 180
  170             CONTINUE
               GO TO 300
  180          CONTINUE
               XCOUNT = XCOUNT + 1
C                                       NUMTIM is number of intervals
               INTIM = 0
               DO 185 IT = 1, NUMTIM
                  IF ((RPARM(ILOCT+1).GE.BEGT(IT)) .AND.
     *               (RPARM(ILOCT+1).LE.ENDT(IT))) THEN
                     ITBASE = IBASE + (IT - 1) * NBASE
                     INTIM = IT
                     GO TO 187
                  END IF
  185          CONTINUE
               GO TO 300
  187          CONTINUE
C               PREAVS = RPARM(ILOCIT + 1)
C               IF (PREAVS.LE.0.0) PREAVS = 2.0
C               IF (PREAVG(ITBASE)*86400.LE.PREAVS)
C     *            PREAVG(ITBASE) = PREAVS / 86400.0
               ITIME=(RPARM(ILOCT+1)-BEGT(INTIM))/PREAVG(ITBASE) + 1.5
               IF ((ITIME.GT.NTIMEP(ITBASE)) .OR. (INTIM.EQ.0))
     *            GO TO 300
C                                       accumalate data for the same
C                                       time and ICHNO
               LOOPS = 1
               DO 200 LOOPIF = BIF, EIF
                  INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-BIF) * INCIF
                  DO 190 LOOPF = BCHAN, ECHAN
                     ITEMP = ((ICHNO-1) + 2*(LOOPIF-BIF)) *
     *                  (ADDD(NBTIM) + NTIMEP(NBTIM)*NPADZ) +
     *                  ADDD(ITBASE)
                     IADDW = ITEMP + ITIME
                     IADDR = 2 * IADDW -1
                     IF ((IADDR.GT.MXBUF2).OR.(IADDW.GT.MXBUFF))
     *                  GO TO 190
                     JRE = IADDR
                     JIM = IADDR + 1
C                                       Address in visibility array.
                     INP = INDEX + (LOOPF-BCHAN) * INCF
                     WT = VIS(INP+2)

                     IF (WT.LE.0.0) WT = 0.0
                     AMPLIT = SQRT (VIS(INP)*VIS(INP) + VIS(INP+1)
     *                  *VIS(INP+1))*1000
                     PHASE = ATAN2(VIS(INP+1), VIS(INP))*180/3.1415926
C                                       Pre-average uv data.
                     IF (WT.GT.0.0) THEN
                        BUFF2(JRE) = BUFF2(JRE) + VIS(INP)   * WT
                        BUFF2(JIM) = BUFF2(JIM) + VIS(INP+1) * WT
                        SUMWT(IADDW) = SUMWT(IADDW) + WT
                        TOTWT = TOTWT + WT
                        NTOT = NTOT + 1
                     END IF
  190             CONTINUE
  200          CONTINUE
  300       CONTINUE
C                                       Normalize the accumulated data.
  400       CONTINUE
            AVER =  DTIME*24*60
            IF (LPASS.EQ.2) THEN
               WRITE (MSGTXT,1300) AVER
               CALL MSGWRT (8)
               END IF
            DO 550 LOOPIF = BIF,EIF
C                                       multiply by reference channel
C                                       and normalize to SQRT of its
C                                       power
               ADD = 0
               ISET = 0
               DFRMID = 0.0
               ITBASE = 0
               DO 480 IT = 1, NUMTIM
                  TIMEN = MIDT(IT)
                  KDAY = BEGT(IT)
                  KH = (BEGT(IT) - KDAY) * 24
                  KM = ((BEGT(IT) - KDAY)*24 - KH) * 60 +0.001
                  KS = ((BEGT(IT) - KDAY)*24*60 -KH*60 -KM) * 60 + 0.001
                  IF (LPASS.EQ.2) THEN
                     IF (NCHAVG .GT. 1) THEN
                        WRITE (MSGTXT,1350) KDAY, KH, KM, KS, NMCHAN,
     *                     NMCHAN+NCHAVG-1
                     ELSE
                        WRITE (MSGTXT,1400) KDAY, KH, KM, KS, NMCHAN
                        END IF
                     CALL MSGWRT (8)
                     END IF
                  DO 460 IBASE = 1, NBASE
                     ITBASE = ITBASE + 1
                     NTRANS = NTIMEP(ITBASE) * NPADZ
C                                       referencing ?
                     IF (REFCHA.GE.0) THEN
                        IF (LPASS.EQ.1) THEN
                           AMPREF(ITBASE) = 0.0
                        ELSE
                           IF (CHAN.EQ.1) THEN
                              AMPREF(ITBASE) = SQRT(AMPREF(ITBASE)
     *                           /NTRANS)
                           IF (AMPREF(ITBASE).LT.1.0E-20)
     *                           AMPREF(ITBASE) = 1.0
                           END IF
                        END IF
                     END IF
                     ITEMP = ((ICHNO-1) + 2*(LOOPIF-BIF)) *
     *                  (ADDD(NBTIM) + NTIMEP(NBTIM)*NPADZ) +
     *                  ADDD(ITBASE)
                     ITEMP0 = 2*(LOOPIF-BIF) *
     *                  (ADDD(NBTIM) + NTIMEP(NBTIM)*NPADZ) +
     *                  ADDD(ITBASE)
                     NMISS = 0
                     DO 420 ITIME = 1,NTRANS
                        IADDW = ITEMP + ITIME
                        IADDR = 2 * IADDW -1
                        JRE = IADDR
                        JIM = IADDR + 1
                        IADDW0 = ITEMP0 + ITIME
                        IADDR0 = 2 * IADDW0 -1
                        JRE0 = IADDR0
                        JIM0 = IADDR0 + 1
                        XNORM = 0.0
                        IF (SUMWT(IADDW).GT.0.0) XNORM = 1.0 /
     *                     SUMWT(IADDW)
C                                       Count the gaps in the data.
                        IF ((XNORM.EQ.0.0).AND.(ITIME.LE.NTIMEP(ITBASE))
     *                     ) NMISS = NMISS + 1
                        BUFF2(JRE) = BUFF2(JRE) * XNORM
                        BUFF2(JIM) = BUFF2(JIM) * XNORM
                        BTEMPR = BUFF2(JRE)
                        BTEMPI = BUFF2(JIM)
                        AMPLIT = SQRT (BTEMPR*BTEMPR + BTEMPI*BTEMPI)
     *                     *1000
                        PHASE = ATAN2(BTEMPI, BTEMPR)*180/3.1415926
C                                       referencing ?
                        IF (REFCHA.LT.0) GO TO 420
                        IF (LPASS.EQ.1) THEN
                           AMPREF(ITBASE) = AMPREF(ITBASE) +
     *                        BTEMPR*BTEMPR + BTEMPI*BTEMPI
                        ELSE
                           AMPLIR = SQRT (BUFF2(JIM0)*BUFF2(JIM0)
     *                        + BUFF2(JRE0)*BUFF2(JRE0) )*1000
                           PHASER = ATAN2(BUFF2(JIM0),BUFF2(JRE0)) *
     *                        180/3.1415926
                           BUFF2(JRE) = (BTEMPR*BUFF2(JRE0)
     *                        + BTEMPI*BUFF2(JIM0)) / AMPREF(ITBASE)
                           BUFF2(JIM) = (BTEMPI*BUFF2(JRE0)
     *                        - BTEMPR*BUFF2(JIM0)) / AMPREF(ITBASE)
                           END IF
  420                CONTINUE
                  IF (LPASS.EQ.1) GO TO 460
                  IADDR = 2 * ITEMP + 1
                  NMAX = 0
                  IF (NMISS.LT.NTRANS/2) THEN
                     IF (LEVPR.GT.1) THEN
                        WRITE (MSGTXT,1500) IT, IBASE, NMCHAN
                        CALL MSGWRT (8)
                        END IF
C                                       compute fringe rates
                     CALL FRRATS (BUFF2(IADDR), NTRANS)
                  END IF
C                                       find set of data for each
C                                       channel
                  IF (NMAX.EQ.0) GO TO 460
                  ISET = ISET + 1
                  UU(ISET) = UU(ITBASE)
                  VV(ISET) = VV(ITBASE)
                  LMAX(ISET) = NMAX
                  DO 440 ID = 1, NMAX
                     KK = ID + ADD
C                                       FRATE in mHZ
                     FRATE(KK) = RATE(ID) /
     *                  (NTIMEP(ITBASE)*PREAVG(ITBASE)* 86.4)
                     FLUXX(KK) = 1000 * AMPP(ID)
                     DFRATE = DDRATE(ID) /
     *                  (NTIMEP(ITBASE)*PREAVG(ITBASE)* 86.4)
                     DFRMID = DFRMID + DFRATE
                     IF (LEVPR.GT.1) THEN
                        WRITE (MSGTXT, 1900)  UU(ISET), VV(ISET),
     *                     FRATE(KK), DFRATE, FLUXX(KK)
                        CALL MSGWRT (8)
                        END IF
  440                CONTINUE
                  ADDDD(ISET) = ADD
                  ADD = ADD + NMAX
  460          CONTINUE
  480       CONTINUE
C                                       Close files
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         IF (LPASS.EQ.1) GO TO 500
C                                       Max. number of lines
         MMMAX = IROUND (BPARM(5))
         IF (MMMAX.EQ.0) MMMAX = ADDDD(ISET) + LMAX(ISET)
C                                       Nothing has found in a
C                                       fringe rate spectra for
C                                       given channels, if ADD=0
         IF (ADD .EQ. 0) THEN
            WRITE (MSGTXT,1085) BCHAN, ECHAN
            CALL MSGWRT (8)
            WRITE (MSGTXT,1090)
            CALL MSGWRT (8)
            GO TO 550
            END IF
         DFRMID = DFRMID / ADD
         IF (BPARM(4).EQ.0.0) BPARM(4) = DFRMID / UVMID
         ACCUR = BPARM(4)
C                                       write to the file
         IF (WPRINT) THEN
            IF (NCHAVG .GT. 1) THEN
               WRITE (LINE,1095) NMCHAN, NMCHAN+NCHAVG-1
            ELSE
               WRITE (LINE,1100) NMCHAN
               END IF
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR,PFIND, LINE(1:NCH), IRET)
C                                       print the blank line
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
         TSTART = TIME1
         TEND = TIME2
         CALL LINES (FRATE, FLUXX, ISET, NX, NY, UU, VV, LMAX,
     *         DX, DY, ADDDD, LOOPIF, CHAN, NPARM, IRET)
         IF (IRET .NE. 0) GO TO 999
         IF (WPRINT) THEN
C                                       print the blank lines
            DO 490 I = 1, 3
               LINE = ' '
               NCH = 1
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
  490          CONTINUE
            END IF
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
         IF (MMMAX.GE.0) THEN
            CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *         CATBLK, .FALSE., BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
  500    CONTINUE
  550    CONTINUE
  600 CONTINUE
  700 CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      IF (NVIS.LT.1) THEN
         WRITE (MSGTXT,1008)
         IRET = 1
         GO TO 990
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C
 995  IF (WPRINT) CALL ZTXCLS (LUNPR, PFIND, IRET)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRUV: ERROR',I5,' OPENING INPUT FILE')
 1005 FORMAT (' LINES: Number of points at X(Y) axis:',I5, '(', I5,')',
     *   ' bigger maxima', I4,'(', I4, ')')
 1006 FORMAT (' Maximum window halfwidths:  ',
     *   F10.0,'*',F10.0,' marcsec')
 1007 FORMAT (' And you have selected:      ',
     *   F10.0,'*',F10.0,' marcsec')
 1008 FORMAT ('FRUV: NO VISIBILITIES SELECTED - CHECK INPUT ',
     *        'PARMS OR SORT ORDER')
 1010 FORMAT ('FRUV: GETSO RETURNED ERROR',I5)
 1045 FORMAT (20X, 'Source:    ',A8)
 1050 FORMAT (20X, 'TIMES:')
 1060 FORMAT (2X, 3(I2,':'), I2)
 1070 FORMAT (12X, 'REFERENCE CHANNEL = ', I3)
 1080 FORMAT (12X, 'WITHOUT REFERENCING TO ANY CHANNEL')
 1085 FORMAT ('No maxima found in fringe rate spectra for channels',
     *            I4,'-',I4)
 1090 FORMAT ('Match preav. time -APARM(1) with your data or increase',
     *            ' threshold')
 1095 FORMAT (20X, 'CHANNEL # ',I3, '-',I3)
 1100 FORMAT (20X, 'CHANNEL # ',I3)
 1120 FORMAT ('Number of time intervals can not be >10. You have ',I4)
 1130 FORMAT ('FRUV: Parameter MXBUF2 needs to be increased')
 1300 FORMAT (' TIME OF AVERAGING = ',F4.0, ' minutes')
 1350 FORMAT (' TIME OF START  = ',3(I2,':'), I2, 5X,
     *   'CHANNEL = ', I3, '-',I3)
 1400 FORMAT (' TIME OF START  = ',3(I2,':'), I2, 5X,
     *   'CHANNEL = ', I3)
 1500 FORMAT (' IT = ',I2, ' IBASE = ', I3, ' CHANNEL = ', I3)
 1900 FORMAT (F8.5, ' * X  +  ', F8.5, ' * Y  = ',
     *   F7.3, '+-', F5.3, ' mHZ; ', F8.1, ' mJy')
      END
      SUBROUTINE FRRATS(DATA, NNN)
C-----------------------------------------------------------------------
C   Calculate the values of fringe rates corresponting to maximum
C   of amplitudes in a fringe rate spectrum. FFT procedure is used as
C   a first approach and least square resolves the task finally.
C   Inputs:
C      DATA     R(2*NNN) input array
C      NNN      I        Number of points at time axis
C   Output in common:
C      NMAX     I     Number of found maxima in the fringe rate spectrum
C      RATE     R(20) Array of found fringe rates
C      AMPP     R(20) Array of found amplitudes
C      FI       R(20) Array of found phases
C   Input/output:
C      WORK     R(2*NNN) work buffer
C-----------------------------------------------------------------------
      INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER MAXR, MAXM
      PARAMETER (MAXR = (3 * MXCOMP))
      PARAMETER (MAXM = (MAXR * MAXR))
      INTEGER  ISIGN, NNN, I, IMAX, NRATE, KRATE, IRATE,
     *   K, NNN2, NMAX3, LESOL, II, KK, III, KKK, L, KL, NUMAMP
      LOGICAL TMAX
      REAL     DATA(MXBUFT), WORK(MXBUFT), ARG1,  TWOPI,
     *   TAMP1, TAMP2, TAMP3, SIGMA, RATM, AMPTEM,
     *   FIM, INPDAT(MXBUFT), STRATE, DDSUM, DSUM, DRATE,
     *   R(MAXR), MATR(MAXM), NOBS, SUM, SSQ, SOL(100), VX(100),
     *   SSQRES, VARRES, VARY, FIT, REDELT, IMDELT, ARGK, ARGL,
     *   TWOP, SINK, SINL, COSK, COSL, AMPMIN, AMPMAX, DAMP, DRE,
     *   DIM, COEFF1, COEFF2, SUMRE(5000), SUMIM(5000), THRES
C-----------------------------------------------------------------------
      TWOPI = 6.2831853072
      ISIGN = -1
      NNN2 = 2*NNN
      DO 60 I = 1, NNN
         INPDAT(2*I-1) = DATA(2*I-1)
         INPDAT(2*I) = DATA(2*I)
C                                       Hanning weighting
         DATA(2*I-1) = DATA(2*I-1)*0.5*(1 - COS((TWOPI*(I-1))/NNN))
         DATA(2*I)   = DATA(2*I)  *0.5*(1 - COS((TWOPI*(I-1))/NNN))
  60    CONTINUE
C                                       FFT
      CALL FOURG (DATA, NNN, ISIGN, WORK)
C                                       Put zero frequency in the middle
      DO 80 I = 1, NNN
         IF (I.LE.NNN/2) THEN
            WORK(2*I - 1) = DATA(2*(I+NNN/2) -1)
            WORK(2*I) = DATA(2*(I+NNN/2))
         ELSE
            WORK(2*I - 1) = DATA(2*(I-NNN/2) -1)
            WORK(2*I) = DATA(2*(I-NNN/2))
            END IF
   80    CONTINUE
C                                       calculate the media amplitude
C                                       (averaging all points excluding
C                                       the central 1/8 part)
C                                       which has to be equal sigma
      SIGMA = 0
      DO 100 I = 1, NNN
         DATA(2*I - 1) = WORK(2*I - 1)
         DATA(2*I) = WORK(2*I)
         IF ((I.GT.NNN*(9.0/16.0)) .OR. (I.LT.NNN*(7.0/16.0)))
     *      SIGMA = SIGMA + SQRT (DATA(2*I-1)*DATA(2*I-1) +
     *      DATA(2*I)*DATA(2*I))
  100    CONTINUE
      SIGMA = SIGMA / NNN
      THRES = BPARM(1)
      IF (THRES.EQ.0) THRES = 4
      BPARM(1) = THRES
      SIGMA = THRES * (SIGMA /(NNN*(7.0/8.0)))
C                                       Find maxima greater SIGMA
C                                       with maximum-minimum diference
C                                       bigger than SIGMA
      IMAX = 0
      TMAX = .FALSE.
      AMPMIN = SQRT (DATA(1)*DATA(1) + DATA(2)*DATA(2)) / NNN
      DO 120 I = 1, NNN
         IF (I.LE.NNN-2) THEN
            TAMP1 = SQRT (DATA(2*I-1)*DATA(2*I-1) +
     *         DATA(2*I)*DATA(2*I)) / NNN
            TAMP2 = SQRT (DATA(2*I+1)*DATA(2*I+1) +
     *         DATA(2*I+2)*DATA(2*I+2)) / NNN
            TAMP3 =  SQRT (DATA(2*I+3)*DATA(2*I+3) +
     *         DATA(2*I+4)*DATA(2*I+4)) / NNN
            IF ((TAMP2.GT.TAMP1) .AND. (TAMP2.GT.TAMP3)) THEN
               TMAX = .TRUE.
               AMPMAX = TAMP2
               END IF
            IF ((TAMP2.LT.TAMP1) .AND. (TAMP2.LT.TAMP3))
     *            AMPMIN = TAMP2
            IF (TMAX .AND. (AMPMAX.GT.SIGMA) .AND.
     *         ((AMPMAX-AMPMIN).GT.0.2*SIGMA)) THEN
               TMAX = .FALSE.
               IMAX = IMAX + 1
               AMPP(IMAX) = 2 * AMPMAX
               RATE(IMAX) =  I + 1 - NNN/2 - 1
               FI(IMAX) = ATAN2 (DATA(2*I+2), DATA(2*I+1))
               END IF
            END IF
  120    CONTINUE
      SIGMA = 2 * SIGMA
C                                       threshold is 2*SIGMA due to
C                                       all amplitudes are multiplied
C                                       by 2 in FOURIER stage
      NMAX = IMAX
C                                       precise AMP, RATE and FI
C                                       using dependence
C                                       FI upon RATE
      NRATE = 5
      STRATE = 1.0 / NRATE
      COEFF1 = (TWOPI/2)*(1 - 1.0/NNN)
      COEFF2 = TWOPI/NNN
C                                       summarize all components
      DO 160 I = 1, NNN
         SUMRE(I) = 0.0
         SUMIM(I) = 0.0
C                                       force zero for model if data=0
         IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *      (INPDAT(2*I).EQ.0.0)) GO TO 160
         DO 140 K = 1, NMAX
            ARG1 = COEFF2 * (I - 1) * RATE(K) +FI(K)
            SUMRE(I) = SUMRE(I) + AMPP(K) * COS(ARG1)
            SUMIM(I) = SUMIM(I) + AMPP(K) * SIN(ARG1)
  140       CONTINUE
  160    CONTINUE
      DO 500 IMAX  = 1, NMAX
C                                       substract the given component
C                                       from model
         DO 180 I = 1, NNN
            IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *         (INPDAT(2*I).EQ.0.0)) GO TO 180
            ARG1 = COEFF2 * (I - 1) * RATE(IMAX) + FI(IMAX)
            SUMRE(I) = SUMRE(I) - AMPP(IMAX) * COS(ARG1)
            SUMIM(I) = SUMIM(I) - AMPP(IMAX) * SIN(ARG1)
  180       CONTINUE
C                                       optimize each component
C                                       independently
         RATM = RATE(IMAX)
         FIM = FI(IMAX)
         DDSUM = 1.0E10
         NUMAMP = 5
         DAMP = 1.15 * AMPP(IMAX) / NUMAMP
C                                       change AMP from 0 till 1.15*AMP
C                                       1.15 because AMP is more than
C                                       85% from real one in Hanning
         DO 450 L = 1, NUMAMP+1
            AMPP(IMAX) = DAMP * (L-1)
            DO 400 IRATE = 1, NRATE+1
               DRATE = (IRATE-1)*STRATE - 0.5
               RATE(IMAX) = RATM + DRATE
               FI(IMAX) = FIM - COEFF1 * DRATE
               DSUM = 0.0
               DO 300 I = 1, NNN
C                                       take only the points.NE.0
                  IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *                  (INPDAT(2*I).EQ.0.0)) GO TO 300
                  DRE = INPDAT(2*I-1)
                  DIM = INPDAT(2*I)
C                                       add the corrected component
                  ARG1 = COEFF2 * (I - 1) * RATE(IMAX) + FI(IMAX)
                  DRE = DRE - SUMRE(I) - AMPP(IMAX) * COS(ARG1)
                  DIM = DIM - SUMIM(I) - AMPP(IMAX) * SIN(ARG1)
                  DSUM = DSUM + DRE * DRE + DIM * DIM
  300             CONTINUE
               IF (DSUM.LT.DDSUM) THEN
                  DDSUM = DSUM
                  KRATE = IRATE
                  KL = L
                  END IF
  400          CONTINUE
  450       CONTINUE
         DRATE = (KRATE-1)*STRATE - 0.5
         RATE(IMAX) = RATM + DRATE
         FI(IMAX) = FIM - (TWOPI/2) * DRATE * (1- 1.0/NNN)
         AMPP(IMAX) = DAMP * (KL-1)
C                                       add the optimal component
C                                       to model
         DO 480 I = 1, NNN
            IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *         (INPDAT(2*I).EQ.0.0)) GO TO 480
            ARG1 = COEFF2 * (I - 1) * RATE(IMAX) + FI(IMAX)
            SUMRE(I) = SUMRE(I) + AMPP(IMAX) * COS(ARG1)
            SUMIM(I) = SUMIM(I) + AMPP(IMAX) * SIN(ARG1)
  480       CONTINUE
  500    CONTINUE
C                                       select the features with
C                                       new amplitudes > sigma
      IMAX = NMAX
      NMAX = 0
      DO 600 I = 1, IMAX
         IF (AMPP(I).GT.SIGMA) THEN
            NMAX = NMAX + 1
            RATE(NMAX) = RATE(I)
            FI(NMAX) = FI(I)
            AMPP(NMAX) = AMPP(I)
            END IF
  600 CONTINUE
C --------------------------------------------------------------------
C                   L E A S T    S Q U A R E
C
C        precise AMP, RATE and FI using non linear least square
C    fitting NMAX complex exponents to the given time dependent data.
C                We have NNN2 = 2*NNN points of data
C             (real and image for each moment of time)
C      We have NMAX3 = NMAX*3 unknown magnitudes: ampl., freq., and
C                     phase for each component.
C --------------------------------------------------------------------
      NMAX3 = 3*NMAX
C                                       Force result vector R(NMAX3),
C                                       matrix M(NMAX3*NMAX3) to zero
      DO 640 I = 1, NMAX
         DO 630 II = 1, 3
            III = (I - 1)*3 + II
            R(III) = 0.0
            DO 620 K = 1, NMAX
               DO 610 KK = 1, 3
                  KKK = (K - 1)*3 + KK
                  MATR(KKK + (III - 1)*NMAX3) = 0.0
  610             CONTINUE
  620          CONTINUE
  630       CONTINUE
  640    CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NMAX3)
C                                       and matrix MATR(NMAX3*NMAX3)
C                                       for routine LEASQR
      DO 690 I = 1, NNN
C                                       take only the points.NE.0
         IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *      (INPDAT(2*I).EQ.0.0)) GOTO 690
         NOBS = NOBS + 1
         REDELT = INPDAT (2*I -1)
         IMDELT = INPDAT (2*I)
         TWOP = (TWOPI/NNN)*(I-1)
C                                       difference of DATA and model
         DO 660 K = 1, NMAX
C            ARG1 = TWOP*(RATE(K)-1) + FI(K)
            ARG1 = TWOP * RATE(K) + FI(K)
            REDELT = REDELT - AMPP(K) * COS(ARG1)
            IMDELT = IMDELT - AMPP(K) * SIN(ARG1)
  660       CONTINUE
         SUM = SUM + REDELT + IMDELT
         SSQ = SSQ + REDELT*REDELT + IMDELT*IMDELT

         DO 680 K = 1, NMAX
C            ARGK = TWOP*(RATE(K)-1) + FI(K)
            ARGK = TWOP * RATE(K) + FI(K)
            SINK = SIN(ARGK)
            COSK = COS(ARGK)
            R((K-1)*3 + 1) =  R((K-1)*3 + 1) + REDELT * COSK
     *         + IMDELT * SINK
            R((K-1)*3 + 2) =  R((K-1)*3 + 2)
     *         - REDELT * AMPP(K) * SINK * TWOP
     *         + IMDELT * AMPP(K) * COSK * TWOP
            R((K-1)*3 + 3) =  R((K-1)*3 + 3) - REDELT * AMPP(K)*SINK
     *         + IMDELT * AMPP(K)*COSK
            DO 670 L = 1, K
C               ARGL = TWOP*(RATE(L)-1) + FI(L)
               ARGL = TWOP * RATE(L) + FI(L)
               SINL = SIN(ARGL)
               COSL = COS(ARGL)
               MATR((L-1)*3+1 + ((K-1)*3+0)*NMAX3) =
     *            MATR((L-1)*3+1 + ((K-1)*3+0)*NMAX3)
     *            + COSK*COSL + SINK*SINL
               MATR((L-1)*3+2 + ((K-1)*3+0)*NMAX3) =
     *            MATR((L-1)*3+2 + ((K-1)*3+0)*NMAX3)
     *            - COSK*AMPP(L)*SINL*TWOP
     *            + SINK*AMPP(L)*COSL*TWOP
               MATR((L-1)*3+3 + ((K-1)*3+0)*NMAX3) =
     *            MATR((L-1)*3+3 + ((K-1)*3+0)*NMAX3)
     *            - COSK*AMPP(L)*SINL + SINK*AMPP(L)*COSL
               MATR((L-1)*3+1 + ((K-1)*3+1)*NMAX3) =
     *            MATR((L-1)*3+1 + ((K-1)*3+1)*NMAX3)
     *            - AMPP(K)*SINK*COSL*TWOP
     *            + AMPP(K)*COSK*SINL*TWOP
               MATR((L-1)*3+2 + ((K-1)*3+1)*NMAX3) =
     *            MATR((L-1)*3+2 + ((K-1)*3+1)*NMAX3)
     *            + AMPP(K)*SINK*AMPP(L)*SINL*TWOP*TWOP
     *            + AMPP(K)*COSK*AMPP(L)*COSL*TWOP*TWOP
               MATR((L-1)*3+3 + ((K-1)*3+1)*NMAX3) =
     *            MATR((L-1)*3+3 + ((K-1)*3+1)*NMAX3)
     *            + AMPP(K)*SINK*AMPP(L)*SINL*TWOP
     *            + AMPP(K)*COSK*AMPP(L)*COSL*TWOP
               MATR((L-1)*3+1 + ((K-1)*3+2)*NMAX3) =
     *            MATR((L-1)*3+1 + ((K-1)*3+2)*NMAX3)
     *            - AMPP(K) * SINK * COSL
     *            + AMPP(K) * COSK * SINL
               MATR((L-1)*3+2 + ((K-1)*3+2)*NMAX3) =
     *            MATR((L-1)*3+2 + ((K-1)*3+2)*NMAX3)
     *            + AMPP(K) * SINK * AMPP(L) * SINL * TWOP
     *            + AMPP(K) * COSK * AMPP(L) * COSL * TWOP
               MATR((L-1)*3+3 + ((K-1)*3+2)*NMAX3) =
     *            MATR((L-1)*3+3 + ((K-1)*3+2)*NMAX3)
     *            + AMPP(K) * SINK * AMPP(L) * SINL
     *            + AMPP(K) * COSK * AMPP(L) * COSL
  670          CONTINUE
  680       CONTINUE
  690    CONTINUE
      NOBS = 2*NOBS
C                                       NOBS need to be real for LEASQR
      CALL LEASQR (NMAX3, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       find the solution and select
C                                       the features with
C                                       new amplitudes > sigma
      IMAX = NMAX
      NMAX = 0
      DO 800 I = 1, IMAX
         AMPTEM = AMPP(I) + SOL((I-1)*3 + 1)
         IF (AMPTEM.GT.SIGMA) THEN
            NMAX = NMAX + 1
            AMPP(NMAX) = AMPP(I) + SOL((I-1)*3 + 1)
            RATE(NMAX) = RATE(I) + SOL((I-1)*3 + 2)
            FI(NMAX) = FI(I) + SOL((I-1)*3 +3)
            DDAMP(NMAX) = SQRT (VX((I-1)*3 + 1))
            DDRATE(NMAX) = SQRT (VX((I-1)*3 + 2))
            DDFI(NMAX) =  SQRT (VX((I-1)*3 + 3))
            END IF
 800     CONTINUE
 999  RETURN
      END
      SUBROUTINE LINES (FRATE, FLUXX, NSET, NX, NY, UU, VV, NRATE,
     *   DX, DY, ADDD, LOOPIF, CHAN, NPARM, IRET)
C-----------------------------------------------------------------------
C   The whole window is devided on NX*NY rectangulars. The programm
C   calculates numbers of lines intersected each rectangular and finds
C   those with number of crossing lines bigger than a given threshold.
C   The intersection density map is used as a first approach. The least
C   square method is used to determine the final map. Least square is
C   repeated several times. After each iteration the line giving maximum
C   deviation is eliminated. The iteration process is terminated if the
C   number of lines has become too small; the number of iteration too
C   big; or if the expected acuracy has been achieved.
C   Inputs:
C      FRATE    R(*)     Array of fringe rates in mHz
C      FLUXX    R(*)     Array of fluxes in mJy
C      NSET     I        Number of different sets (baselines or times)
C      NX       I        Number of cells in X axis
C      NY       I        Number of cells in Y axis
C      UU       R(*)     DF/DX (mHZ/mili argsec)
C      VV       R(*)     DF/DY (mHZ/mili argsec)
C      NRATE    I(*)     Array of numbers of measured fringe rates
C                        for different sets
C      DX       R        Size of the cells in X axis in mHz
C      DY       R        Size of the cells in Y axis in mHz
C      ADDD     I(*)     Array of pointers at the arrays FRATE
C      LOOPIF   I        IF number
C      CHAN     I        Channel number
C      NPARM    I        Number of real words read from AIPS
C   Output:
C      IRET     I        Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FRMAP.INC'
      INTEGER  MXLINX
      PARAMETER (MXLINX = (MXPTX*MXSETC))
      CHARACTER LINE*132
      REAL FRATE(*), FLUXX(*), UU(*), VV(*), U, V, XR,
     *   DX, DY, F, FR, YR, XLEFT, XRIGHT, AMPFIN(MXSET),
     *   YLEFT, YRIGHT, R(2), MATR(4), NOBS, AMPAVE,
     *   XLE, XRI, YLE, YRI,
     *   FFIN(MXSET), XY(2), VX(2), SSQRES, VARRES, VARY, FIT,
     *   SUM, SSQ, SIGMAX, SIGMAY, RES, RESSQR, RESMAX,  SIGMA,
     *   UFIN(MXSET), VFIN(MXSET), DDY, XCFIN, YCFIN
      INTEGER NRATE(*), ADDD(*), NSET, ISET, ICOMP,  ADR, I, L,
     *   LLMIN(MXLINX), LLMAX(MXLINX), NRECT, ITER, NITER,
     *   ADRFIN(MXSETC), ADRREC(MXSETC), LESOL, SETLIN(MXSET),
     *   ADRL(MXSET), NLINE, ADRF(100), NLCROS(100), MAXLIN, ADD,
     *   LMIN, LMAX, KK, ISIGN, IRECT, J, IFIN(MXCOMP), KL, NCUR,
     *   JFIN(MXCOMP), NX, NY, K, IB, IE, MRECT, LCROS, LLL,
     *   IRET, NCH, ITRIM, HNSET, COMLIN(MXSET),
     *   IBB(MXSETC), IEE(MXSETC), LRECT(MXCOMP), N, ICUR, JCUR,
     *   NCOMP, LOOPIF, CHAN, KLINE, NPARM
      LOGICAL  CROSAR
C-----------------------------------------------------------------------
C
      IRET = 0
C                                       parameters for LINES
      IF (BPARM(6).NE.0.0) THEN
         HNSET = BPARM(6)*NSET
      ELSE
         HNSET = NSET*0.4
         END IF
      BPARM(6) = HNSET / FLOAT(NSET)
      IF (BPARM(7).NE.0.0) THEN
         NITER = BPARM(7)
      ELSE
         NITER = 4
         END IF
      BPARM(7) = NITER
      IF (BPARM(8).NE.0.0) THEN
         MAXLIN = BPARM(8)*NSET
      ELSE
         MAXLIN = 0.6*NSET
         END IF
      IF (MAXLIN.LT.4) MAXLIN = 4
      BPARM(8) = MAXLIN / FLOAT (NSET)
      DO 60 I = 1,2000
         IBB(I) = 0
         IEE(I) = 0
   60    CONTINUE
C                                       Determine the cells crossing by
C                                       lines
C                                       Loop lines of sets
      ADD = 0
      KLINE = 0
      DO 200 ISET = 1, NSET
         U = UU(ISET)
         V = VV(ISET)
         DO 150 ICOMP = 1, NRATE(ISET)
            ADR = ICOMP + ADDD(ISET)
            F = FRATE(ADR)
C                                       the line U*X + V*Y = F
C                                       find intersection of the line
C                                       with the window
            CALL FRANGE (U, V, F, CROSAR, XLEFT, XRIGHT, YLEFT, YRIGHT)
            IF (.NOT. CROSAR) GO TO 150
C                                       plot the line
            KLINE = KLINE + 1
C                                       devide by 1000 to receive
C                                       arcsecs
            FR = F/1000
            XLE = XLEFT / 1000
            XRI = XRIGHT / 1000
            YLE = YLEFT / 1000
            YRI = YRIGHT / 1000
            IF (MMMAX.GE.0) CALL FRPL (KLINE, NPARM, XLE, XRI, YLE, YRI,
     *         LOOPIF, CHAN, IRET)
            IF (APARM(10).NE.0.0) GO TO 150
            IB = (XLEFT  - MXLE)/DX + 1.0001
            IE = (XRIGHT - MXLE)/DX + 0.9999
            DDY = (U/(-V)) * DX
            ISIGN = 1
            IF (DDY.LT.0.0) ISIGN = -1
            XR = DX * IB + MXLE
            YR = (F - U*XR) / V
            LMIN = (YLEFT - MYLO)/DY + 1.0001
            IBB(ADR) = IB
            IEE(ADR) = IE
            L = 0
            DO 80 K = IB, IE
               IF (K.NE.IB) LMIN = LMAX
               LMAX = (YR - MYLO)/DY + 0.9999
               IF (K.EQ.IE) LMAX = (YRIGHT - MYLO)/DY + 0.9999
               YR = YR + DDY
               L = L + 1
               KK = L + ADD
               IF (ISIGN.GT.0) THEN
                  LLMIN(KK) = LMIN
                  LLMAX(KK) = LMAX
               ELSE
                  LLMIN(KK) = LMAX
                  LLMAX(KK) = LMIN
                  END IF
   80          CONTINUE
            NRECT = L
            ADRREC(ADR) = ADD
            ADD = ADD + NRECT
  150       CONTINUE
  200    CONTINUE
         IF (APARM(10).NE.0.0) GO TO 999
C                                       Go through all cells to
C                                       determines that ones crossed by
C                                       number of lines exceeded the
C                                       threshold
      ADD = 0
      IRECT = 0
      DO 400 I = 1, NX
         DO 300 J = 1, NY
C                                       look for lines crossing the
C                                       rectangular
            NLINE = 0
            DO 260 ISET = 1, NSET
               DO 240 ICOMP = 1, NRATE(ISET)
                  ADR = ICOMP + ADDD(ISET)
                  IB = IBB(ADR)
                  IE = IEE(ADR)
                  IF ((I.GE.IB) .AND. (I.LE.IE)) THEN
                     KK = I - IB + 1 + ADRREC(ADR)
                     IF ((J.GE.LLMIN(KK)) .AND.
     *                   (J.LE.LLMAX(KK))) THEN
                        NLINE = NLINE + 1
                        ADRL(NLINE) = ADR
                        END IF
                     END IF
  240             CONTINUE
  260          CONTINUE
C                                       store the data about rectangular
C                                       crossed be a large number of
C                                       lines
            IF (NLINE.GT.HNSET) THEN
               IRECT = IRECT + 1
               IFIN(IRECT) = I
               JFIN(IRECT) = J
               NLCROS(IRECT) = NLINE
               DO 280 L = 1, NLCROS(IRECT)
                  KK = L + ADD
                  ADRFIN(KK) = ADRL(L)
  280             CONTINUE
               ADRF(IRECT) = ADD
               ADD = ADD + NLCROS(IRECT)
               END IF
  300       CONTINUE
  400    CONTINUE
      MRECT = IRECT
      IF (MRECT.EQ.0) GO TO 600
C                                       We've found the rectangulars
C                                       IRECT with maximum crossing
C                                       lines NLCROS(IRECT), addre-
C                                       sses of the lines ADRFIN and
C                                       corresponded fringe rates
C                                       FFIN
C
C                                       If we obtained clusters of
C                                       rectangular leave only one
C                                       with maximum NLCROS
      DO 405 I =1, MRECT
         LRECT(I) = I
  405    CONTINUE
         DO 415 L = 1, MRECT-1
            I = IFIN(L)
            J = JFIN(L)
            N = NLCROS(L)
            DO 410 K = L+1, MRECT
               ICUR = IFIN(K)
               JCUR = JFIN(K)
               NCUR = NLCROS(K)
               IF (LRECT(K).EQ.0) GO TO 410
               IF ((IABS(I-ICUR).LE.1) .AND.
     *             (IABS(J-JCUR).LE.1)) THEN
                  IF (NCUR.GT.N) THEN
                     N = NCUR
                     I = ICUR
                     J = JCUR
                     IF (LRECT(L).NE.0) THEN
                        LRECT(L) = 0
                        KL = K
                     ELSE
                        IF (K.NE.KL) LRECT(KL) = 0
                        KL = K
                        END IF
                  ELSE
                     LRECT(K) = 0
                     END IF
                  END IF
  410          CONTINUE
  415       CONTINUE
C                                       Loop for founded rectangulars
      NCOMP = 0
      DO 417 IRECT = 1, MRECT
         LCROS = NLCROS(IRECT)
         IF (LCROS .GT. 60 .AND. LEVPR.GT.0) THEN
            WRITE (MSGTXT,1240) LCROS
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
  417    CONTINUE
C
      DO 500 IRECT = 1, MRECT
         IF (LRECT(IRECT).EQ.0) GO TO 500
         IF (NCOMP.EQ.0) THEN
            WRITE (MSGTXT,1700)
            CALL MSGWRT (8)
            IF (WPRINT) THEN
               WRITE (LINE,1700)
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
            END IF
         LCROS = NLCROS(IRECT)
         XCFIN = (IFIN(IRECT) - 0.5)*DX + MXLE
         YCFIN = (JFIN(IRECT) - 0.5)*DY + MYLO
         DO 420 L = 1, LCROS
            ADR = L + ADRF(IRECT)
            ADRFIN(L) = ADRFIN(ADR)
  420    CONTINUE
C                                       Now apply LEASQR for acurate
C                                       position determination
C
C                                       loop for excluding of lines with
C                                       maximum residual
         ITER = 1
  425    CONTINUE
C                                       Force result vector R and
C                                       matrix MATR to zero
         DO 440 I = 1, 2
            R(I) = 0.0
            DO 430 L = 1, 2
               MATR(L +(I-1)*2) = 0.0
  430          CONTINUE
  440       CONTINUE
C                                       Prepare solution column
C                                       and MATRIX for LEASQR
         DO 470 L = 1, LCROS
            DO 460 ISET = 1, NSET
               DO 450 ICOMP = 1, NRATE(ISET)
                  ADR = ICOMP + ADDD(ISET)
                  IF (ADRFIN(L).EQ.ADR) THEN
                     FFIN(L) = FRATE(ADR)
                     AMPFIN(L) = FLUXX(ADR)
                     COMLIN(L) = ICOMP
                     SETLIN(L) = ISET
                     U = UU(ISET)
                     V = VV(ISET)
                     UFIN(L) = U
                     VFIN(L) = V
                     R(1) = R(1) + U*FFIN(L)
                     R(2) = R(2) + V*FFIN(L)
                     MATR(1) = MATR(1) + U * U
                     MATR(2) = MATR(2) + U * V
                     MATR(3) = MATR(3) + U * V
                     MATR(4) = MATR(4) + V * V
                  END IF
  450             CONTINUE
  460          CONTINUE
  470       CONTINUE
            IF (LEVPR.GT.0) THEN
               WRITE (MSGTXT,1300) IRECT, (SETLIN(I), I=1,LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1400) IRECT, (COMLIN(I), I=1,LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1500) IRECT, (AMPFIN(I), I=1,LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1300) IRECT,(SETLIN(I),I=LCROS/4+1,LCROS/2)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1400) IRECT,(COMLIN(I),I=LCROS/4+1,LCROS/2)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1500) IRECT,(AMPFIN(I),I=LCROS/4+1,LCROS/2)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1300) IRECT, (SETLIN(I),
     *            I=LCROS/2+1,3*LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1400) IRECT, (COMLIN(I),
     *            I=LCROS/2+1,3*LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1500) IRECT, (AMPFIN(I),
     *            I=LCROS/2+1,3*LCROS/4)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1300) IRECT,(SETLIN(I),I=3*LCROS/4+1,LCROS)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1400) IRECT,(COMLIN(I),I=3*LCROS/4+1,LCROS)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1500) IRECT,(AMPFIN(I),I=3*LCROS/4+1,LCROS)
               CALL MSGWRT (8)
            END IF
C                                       Prepare sum of measures and
C                                       sum of squares of measures
         SUM = 0.0
         SSQ = 0.0
         DO 475 L = 1, LCROS
            SUM = SUM + FFIN(L)
            SSQ = SSQ + FFIN(L)*FFIN(L)
  475    CONTINUE
C                                       LEASQR requires number of
C                                       observations to be real
         NOBS = LCROS
         CALL LEASQR (2, NOBS, SUM, SSQ, R, MATR, XY, VX, SSQRES,
     *      VARRES, VARY, FIT, LESOL)
         SIGMAX = SQRT(VX(1))
         SIGMAY = SQRT(VX(2))
         SIGMA = SQRT(SIGMAX*SIGMAX + SIGMAY*SIGMAY)
         IF (LEVPR.GT.0) THEN
            WRITE (MSGTXT,1600) IRECT, XCFIN, XY(1), SIGMAX,
     *         YCFIN, XY(2), SIGMAY
            CALL MSGWRT (8)
            END IF
C                                       find the line with max residual
         RESSQR = 0.0
         RESMAX = 0.0
         DO 480 L = 1, LCROS
            U = UFIN(L)
            V = VFIN(L)
            RES = ABS(FFIN(L) - U * XY(1) - V * XY(2))
            IF (RES.GT.RESMAX) THEN
               RESMAX = RES
               LLL = L
               END IF
            RESSQR = RESSQR + RES*RES
  480       CONTINUE
            RESSQR = RESSQR/NOBS
C                                       exclude the line giving
C                                       maximum residual
         LCROS = LCROS - 1
         ITER = ITER + 1
         IF ((ITER.GT.NITER)  .OR.
     *       (LCROS.LT.MAXLIN) .OR.
     *       (SIGMA.LT.ACCUR)) THEN
            IF (LEVPR.GT.0) THEN
               WRITE (MSGTXT,1900) ITER,NITER,LCROS, MAXLIN, SIGMA,
     *            ACCUR
               CALL MSGWRT (8)
               END IF
         ELSE
C                                       repeat LEASQR deleting maximum
C                                       residual's line
            DO 490 L = 1, LCROS
               IF (L.GE.LLL) THEN
                  FFIN(L) = FFIN(L+1)
                  AMPFIN(L) = AMPFIN(L+1)
                  ADRFIN(L) = ADRFIN(L+1)
                  END IF
  490          CONTINUE
            GO TO 425
         END IF
         NCOMP = NCOMP + 1
         LCROS = LCROS + 1
         AMPAVE = 0.0
         DO 495 L = 1, LCROS
            AMPAVE = AMPAVE + AMPFIN(L)
  495       CONTINUE
         AMPAVE = AMPAVE / LCROS

         WRITE (MSGTXT,1800) NCOMP, XY(1), SIGMAX, XY(2),
     *      SIGMAY, AMPAVE
         CALL MSGWRT (8)
C                                       write to the file
         IF (WPRINT) THEN
            WRITE (LINE,1800) NCOMP, XY(1), SIGMAX, XY(2),
     *         SIGMAY, AMPAVE
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
  500    CONTINUE
         GO TO 999
  600    CONTINUE
            WRITE (MSGTXT,1010) HNSET
            CALL MSGWRT (8)
            IRET = 2
  999 RETURN
C---------------------------------------------------------------------------
 1010 FORMAT (' LINES: There is no any cell being crossed by',
     *   ' the number of lines bigger', I5)
 1240 FORMAT ('Number of lines = ',I5, ' It is > 60. Use BPARM(9)=0')
 1300 FORMAT (' RECT= ', I2,' SETS|', 15I4)
 1400 FORMAT (' RECT= ', I2,' COMS|', 15I4)
 1500 FORMAT (' RECT= ', I2,' FLUX|', 15F4.0)
 1600 FORMAT (I3, 2X, 'XC=',F7.1, ' X= ',F7.1,' +- ', F6.1, 3X,
     *    'YC=',F7.1, ' Y= ', F7.1, ' +- ',F6.1)
 1700 FORMAT (9X, 'RA, marcsec',13X, 'DEC, marcsec',8X,'FLUX, mJy')
 1800 FORMAT (I3, '.', 2X, F9.1,' +- ', F6.1, 5X,
     *   F9.1, ' +- ',F6.1, 5X, F8.1)
 1900 FORMAT (' ITER=',I3,' NITER=',I3,' LCROS=',I2,' MAXLIN=',I2,
     *   ' SIGMA=',F6.1,' ACCUR=',F5.1)
      END
      SUBROUTINE FRANGE (U, V, F, CROSAR, XLEFT, XRIGHT, YLEFT, YRIGHT)
C-----------------------------------------------------------------------
C   The programm finds the two points of intersection of line
C   U*X + V*Y = F with a border of rectangular BLC(MXLE,MYLO)
C   TRC(MXRI, MYHI).
C   Inputs:
C      U        R        Coefficient near X
C      V        R        Coefficient near Y
C      F        R        Right part of the line equation
C   Inputs from common:
C      MXLE     R        The most left point of the rectangular
C      MXRI     R        The most right point of the rectangular
C      MXLO     R        The most low point of the rectangular
C      MXHI     R        The most high point of the rectangular
C   Outputs:
C      CROSAR   L        Whether the line cross the rectangular or not
C      XLEFT    R        X coordinate of the left cross point
C      YLEFT    R        Y coordinate of the LEFT cross point
C      XRIGHT   R        X coordinate of the right cross point
C      YRIGHT   R        Y coordinate of the right cross point
C---------------------------------------------------------------------------
      REAL U, V, F, XLEFT, XRIGHT, YLEFT, YRIGHT, XB, YL,
     *   YR, XT
      LOGICAL CROSAR
      INCLUDE 'FRMAP.INC'
C---------------------------------------------------------------------------
      CROSAR = .FALSE.
      IF (V.EQ.0.0) THEN
         XB = F / U
         IF ((XB.GT.MXLE) .AND. (XB.LT.MXRI)) THEN
            CROSAR = .TRUE.
            XLEFT = XB
            XRIGHT = XB
            YLEFT = MYLO
            YRIGHT = MYHI
            END IF
      ELSE
         IF (U.EQ.0.0) THEN
            YL = F / V
            IF ((YL.GT.MYLO) .AND. (YL.LT.MYHI)) THEN
               CROSAR = .TRUE.
               XLEFT = MXLE
               XRIGHT = MXRI
               YLEFT = YL
               YRIGHT = YL
               END IF
         ELSE
            YL = (F - U * MXLE) / V
            YR = (F - U * MXRI) / V
            XB = (F - V * MYLO) / U
            XT = (F - V * MYHI) / U
            XLEFT = MXLE
            IF (YL.GT.MYHI) XLEFT = XT
            IF (YL.LT.MYLO) XLEFT = XB
            IF ((XLEFT.GT.MXLE*(1.0 - 0.00001))
     *         .OR. (XLEFT.LT.MXRI*(1.0 - 0.00001))) THEN
               CROSAR = .TRUE.
               XRIGHT = MXRI
               IF (YR.GT.MYHI) XRIGHT = XT
               IF (YR .LT.MYLO) XRIGHT = XB
               YLEFT  = (F - U*XLEFT) / V
               YRIGHT = (F - U*XRIGHT) / V
               END IF
            END IF
         END IF
      RETURN
      END
      SUBROUTINE FILANT (DISK, CNO, LUN, IXANT, IXBASL, NXANT, NXBASL,
     *   DESEL, NSUBA, NBASE, ANT1, ANT2, STNS, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned. Also fills in 2
C   arrays with all possible cominations of antenna numbers.
C   Fills array of selected antennas for possible using in UVGET.
C   Find  number and list of selected antennas.
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
C      SCRTCH   I(512)   I/O buffer and related storage.
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                           10 = no AN files.
C   Output in common:
C      ANTENS   I(*)     Array of selected antennas
C      NANTSL   I        Number of selected antennas
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN, IXANT(50), IXBASL(50),
     *   NXANT, NXBASL, NSUBA, NBASE, ANT1(*), ANT2(*), SCRTCH(512),
     *   SUBSEQ(50), IRET, IANT
      LOGICAL   DESEL, FOUND
      CHARACTER STNS(*)*8
C
      INTEGER   NBUFF, II, NUMREC, J, MXNSTA, I1, IERR, ICNT, K, I
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
      NBASE = 0
C
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
C                                       also fiddle with IXANT to deal
C                                       with a 'feature' in REQBAS
      NUMREC = SCRTCH(5)
      MXNSTA = 1
      ICNT = 0
      DO 10 II = 1,NUMREC
         CALL TABAN ('READ', SCRTCH, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         SUBSEQ(II) = NOSTA
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', NSUBA
            GO TO 990
            END IF
         MXNSTA = MAX (NOSTA, MXNSTA)
         STNS(NOSTA) = ANNAME
         IF ((NXANT.EQ.0) .AND. (NXBASL.EQ.0)) THEN
            IXANT(II) = NOSTA
            ICNT = ICNT + 1
            END IF
 10      CONTINUE
      NXANT = MAX (NXANT, ICNT)
C                                       Fill up the baseline arrays
C                                       option 'auto'
      IF (DOACOR) THEN
         NBASE = NXANT
         DO 30 I1 = 1,NBASE
            ANT1(I1) = IXANT(I1)
            ANT2(I1) = IXANT(I1)
 30         CONTINUE
      ELSE
         DO 50 I1 = 1,MXNSTA
            DO 40 J = I1,MXNSTA
               IF (((I1.LT.J) .AND. (.NOT.DOACOR)) .OR.
     *            ((DOACOR) .AND. (I1.EQ.J))) THEN
                  ACCEPT = REQBAS (I1, J, DESEL, IXANT, NXANT, IXBASL,
     *               NXBASL)
                  IF (ACCEPT) THEN
                     NBASE = NBASE + 1
                     ANT1(NBASE) = I1
                     ANT2(NBASE) = J
                     END IF
                  END IF
 40            CONTINUE
 50         CONTINUE
         END IF
C                                       Find number of selected antennas
C                                       and their list
      IANT = 0
      DO 70 I = 1, 50
         FOUND = .FALSE.
         DO 60 K = 1, NBASE
            IF (FOUND) GO TO 60
            IF (I.EQ.ANT1(K)) THEN
               FOUND = .TRUE.
               IANT = IANT + 1
               ANTENS(IANT) = ANT1(K)
               END IF
            IF (I.EQ.ANT2(K)) THEN
               FOUND = .TRUE.
               IANT = IANT + 1
               ANTENS(IANT) = ANT2(K)
               END IF
   60       CONTINUE
   70    CONTINUE
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
      SUBROUTINE FRPL (KK, NPARM, XLEFT, XRIGHT, YLEFT, YRIGHT, IFNO,
     *   ICHNO, IRET)
C-----------------------------------------------------------------------
C   FRPL does the plotting of the line crossing the given rectangular
C   Input parameters:
C      KK       I    Number in order of using the program
C      NPARM    I    Number of R   parms read form AIPS
C      XLEFT    R    X-coordinate of the left point of intersetion
C                    of the line with ractangular
C      XRIGHT   R    X-coordinate of the right point of intersetion
C                    of the line with ractangular
C      YLEFT    R    Y-coordinate of the left point of intersetion
C                    of the line with ractangular
C      YRIGHT   R    Y-coordinate of the right point of intersetion
C                    of the line with ractangular
C      IFNO     I    IF number to plot
C      ICHNO    I    Channel identifier. 1=first set of pre-averaged
C                    channels; 2=second etc.
C   Output parameter:
C      IRET     I    Return error code
C   Input from common
C      FBLANK   R          REAL value indicating blanking.
C      NCHANP   I          Channel range to plot.
C      NCHAVG   I          No of frequency channels to pre-average.
C      PBCHAN   I          Start frequency channel to plot.
C      PBIF     I          Start IF to plot.
C      PECHAN   I          End frequency channel to plot.
C      EXPDAT   C*8        Observation date ('DD/MM/YY').
C      SRCOBS   C*16       Source plotted.
C      DOTV     L          Plot on TV device ?
C      UVRNG    R(2)       Min and max baseline lengths to select.
C                          1000's wavelengths; 0's => all.
C      BCHAN    I          First frequency channel selected (1-rel).
C      DOFQSL   L          in /SELCAL/.
C   Input/output via common
C      BUFF2    R(*)       Pre-average data buffer.
C      OFILE    C*48       File name for printed output.
C      CNOIN    I          Input file catalog number.
C      DISKIN   I          Input file disk number.
C      PVER     I          PL version number.
C      CPREF    C(2)*5     Axis units prefix (axis#)
C      CTYP     C(4)*24    Axis units (axis#)
C      AXFUNC   I(7)       in /LOCATI/.
C      AXINC    R(4)       Axis increment / pixel (axis#).
C      RPLOC    R(4)       Ref pixel location (axis#).
C      RPVAL    R(4)       Ref pixel value on axis (axis#).
C      AXTYP    I          Axes type.
C      CORTYP   I          Coordinate type.
C      LABTYP   I          Label type.
C      ROT      R          Axes rotation.
C      CATBLK   I(256)     Catalog header block.
C      MSGTXT   C*80       AIPS message string.
C      CTIME    R          Centre time of selected data (days).
C      BLC      R(2)       Number of first pixel in row and col.
C      CHOUT    R(4)       No of characters outside pix on left
C                          bottom, right, top resp.
C      REFCHAN  I          Reference channel
C      PLTBLK   R(256)     Line plot buffer.
C      TRC      R(2)       Last pixel pos in row/col (npnts+1).
C      IGFIND   I          Location in FTAB for graphics file.
C      IGLUN    I          Graphics LU number.
C      SLOT     I          Catalog slot of PL file.
C-----------------------------------------------------------------------
      INTEGER  KK, NPARM, IFNO, ICHNO, IRET, DEPTH(5), IPTYPE, INP,
     *   IUSER, IROUND, IERR, BUFFI(256), I, ICHLOW, ICHHI
      CHARACTER AXUNIT*8, UTYPE*2
      REAL     XLEFT, XRIGHT, YLEFT, YRIGHT, AMP(2), MAXAMP, MINAMP,
     *   SCALX,  SCALA, OFX, OFY, X, Y, AMPRNG, XBLC(2), XTRC(2), MINX,
     *   MAXX, XRNG,CATR(256), XYRATI, YMULT,XREF, XREFO, XINC,
     *   RPARM(305)
      DOUBLE PRECISION CATD(128)
      LOGICAL F, PFLG
      INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RPARM, USERID)
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA F /.FALSE./
      DATA AXUNIT /'arcsec'/
C-----------------------------------------------------------------------
      IF (KK.GT.MMMAX) GO TO 999
      IF (KK.NE.1) GO TO 40
      MINX = MXLE / 1000
      MAXX = MXRI / 1000
      IPTYPE = 25
      IUSER = IROUND (USERID)
      MAXAMP = MYHI / 1000
      MINAMP = MYLO / 1000
      PVER = 0
      UTYPE = 'UV'
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATI = 1.0
      XRNG = MAXX - MINX
      MINX = MINX - 0.03 * XRNG
      MAXX = MAXX + 0.03 * XRNG
C                                       Minus to have RA positive
C                                       to the left
      XRNG = MINX - MAXX
      AMPRNG = ABS (MAXAMP - MINAMP)
      MINAMP = MINAMP - 0.03 * AMPRNG
      MAXAMP = MAXAMP + 0.03 * AMPRNG
C                                       finish setting in parms
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TSTART
      XTIME(5) = TEND
      AMPRNG = ABS (MAXAMP - MINAMP)
C                                       Create plot file
      CALL PLCREA (NPARM, RPARM, IPTYPE, IUSER, UTYPE, PVER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      WRITE (MSGTXT,1050) PVER
      IF (.NOT.DOTV) CALL MSGWRT (5)
C                                        Set up the location common
C                                        for tick marks etc.
      CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, F)
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
C                                       Set x-axis parms
      XINC = XRNG
      XREF = MAXX
      XREFO = XREF
      CALL METSCL (LABEL, XREF, CPREF(1,LOCNUM), PFLG)
      XINC = XINC * (XREF / XREFO)
      CTYP(1,LOCNUM) = AXUNIT
      RPVAL(1,LOCNUM) = XREF
      AXINC(1,LOCNUM) = XINC / (TRC(1) - BLC(1))
C                                       Determine apmlitude parms
      YMULT = MAXAMP
      CALL METSCL (LABEL, MAXAMP, CPREF(2,LOCNUM), PFLG)
      YMULT = MAXAMP / YMULT
C
      XBLC(1) = BLC(1)
      XBLC(2) = BLC(2)
      XTRC(1) = TRC(1)
      XTRC(2) = TRC(2)
      RPLOC(1,LOCNUM) = BLC(1)
      RPLOC(2,LOCNUM) = BLC(2)
C                                       Y axis
      RPVAL(2,LOCNUM) = MINAMP * YMULT
      AXINC(2,LOCNUM) = YMULT * AMPRNG / (TRC(2) - BLC(2))
      CTYP(2,LOCNUM) = AXUNIT
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (XBLC, XTRC, INP)
      INP = MAX (INP, 2)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) CHOUT(1) = INP + 4.0
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(4) = 3.333
         IF (LABEL.GT.1) CHOUT(4) = CHOUT(4) + 1.333
         IF (DOFQSL) CHOUT(4) = CHOUT(4) + 1.333
         CHOUT(2) = CHOUT(2) + 2 * 1.333
         IF (((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9))) CHOUT(2) =
     *     CHOUT(2) + 1.333
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4)) CHOUT(2) = CHOUT(2) +
     *      1.333
         END IF
C                                        Init. for line drawing
      CALL GINITL (BLC, TRC, XYRATI, CHOUT, DEPTH, PLTBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
C                                        Draw the box (ampl)
      CALL GLTYPE (1, PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (TRC(1), BLC(2), PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (TRC(1), TRC(2), PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (BLC(1), TRC(2), PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL GVEC (BLC(1), BLC(2), PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Labelling
      ICHLOW = (ICHNO - 1) * NCHAVG + PBCHAN
      ICHHI = ICHLOW + NCHAVG - 1
      IF (ICHHI.GT.PECHAN) ICHHI = PECHAN
      CALL POSSLB (IFNO, ICHLOW, ICHHI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
C                                       Label it
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATI, F, PLTBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1080) IRET
         GO TO 990
         END IF
C                                       Scaling
      SCALX = (TRC(1) - BLC(1)) / XRNG
      SCALA = (TRC(2) - BLC(2)) / AMPRNG
      OFX = 0.0
      OFY = BLC(2) - MINAMP * SCALA
   40 CONTINUE
C                                       Plot the lines
      CALL GLTYPE (2, PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
      AMP(1) = YLEFT
      AMP(2) = YRIGHT
      DO 50 I = 1, 2
         X = ((XLEFT-MAXX) + (XRIGHT-XLEFT) * (I-1)) * SCALX + OFX
         Y = AMP(I) * SCALA + OFY
         IF (I.EQ.1) THEN
            CALL GPOS (X, Y, PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
         CALL GVEC (X, Y, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
 50      CONTINUE
C                                       Finish up
      IF (KK.LT.MMMAX) GO TO 999
      CALL GFINIS (PLTBLK, IRET)
      IF (.NOT.DOTV) CALL HIPLOT (DISKIN, CNOIN, PVER, BUFFI, IERR)
      GO TO 999
 980  CONTINUE
      WRITE (MSGTXT,1100) IRET
      CALL MSGWRT (8)
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('FRPL: ERROR ',I3,' RECEIVED FROM PLCREA')
 1050 FORMAT ('Plot file version ',I3,' created')
 1060 FORMAT ('FRPL:  Error in GINITL')
 1070 FORMAT ('FRPL:  Error in POSSLB')
 1080 FORMAT ('FRPL:  Error in CLAB1')
 1100 FORMAT ('FRPL:  ERROR',I5,' Error in GPOS/GVEC')
      END
      SUBROUTINE PLCREA (NP, RPARM, IGTYPE, IUSER, DTYP, IVER, IERR)
C-----------------------------------------------------------------------
C   This routine will create and open a plot file, put it in the map
C   header and write the first record into the plot file.
C   PLCREA first opens the map file and marks it 'write'
C   Input parameters:
C      NP       I       Number of floating point words in parameter
C                       list received from AIPS.
C      RPARM    R(NP)   AIPS parameters.
C      IGTYPE   I       Plot file type: 1 misc., 2 CNTR, 3 GREYS,
C                       4 PROFL, 5 SL2PL, 6 PCNTR, 7 IMEAN (hist),
C                       8 UVPLT, 9 GNPLT, 10 VBPLT, 11 PFPLn,
C                       12 => GAPLT, 13 => PLCUB, 14 => IMVIM,
C                       15 => TAPLT, 16 => POSSM, 17 => SNPLT
C                       Use 1 unless your inputs match those of these
C                       tasks - or take a new number, but
C                       AIPSUB:AU8A will need to know about it too.
C      IUSER    I       User number
C      DTYP     C*2     ASCII data type of input e.g. 'UV', 'MA'
C   Output parameters:
C      IVER     I       Version number of plot file to generate
C      IERR     I       Error code. two digit, first digit indicates
C                       subroutine: 1: MAPOPN, 2: MADDEX, 3: ZPHFIL,
C                       4: GINIT, second digit indicates error code of
C                       that subroutine.
C   Input/output via common
C      CLAIN    C*6        Input file class.
C      NAMEIN   C*12       Input file name.
C      GPHIND   I          in /GPHCOM/.
C      DISKIN   I          Input file disk number.
C      PLFIND   I          PL file FTAB pointer.
C      PLMAP    I          LU number for PL file.
C      SEQIN    I          Input file sequence number.
C      CATBLK   I(256)     Catalog header block.
C      PLTBLK   R(256)     Line plot buffer.
C      TVCORN   I(4)       Set to zeroes.
C      DOTV     L          Plot on TV device ?
C      GRCHN    I          Graphics channel.
C      IGFIND   I          Location in FTAB for graphics file.
C      IGLUN    I          Graphics LU number.
C      SLOT     I          Catalog slot of PL file.
C      TVCHN    I          TV channel.
C-----------------------------------------------------------------------
      CHARACTER DTYP*2
      REAL      RPARM(*)
      INTEGER   NP, IGTYPE, IUSER, IVER, IERR
C
      CHARACTER PHNAME*48, STAT*4
      INTEGER   IWBLK(256), IGSIZE, IER
      LOGICAL   SAVE
      INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IGSIZE /0/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
      IF (IGTYPE.LT.1) IGTYPE = 1
C                                       Open map file
      PLMAP = 17
      IF (DOTV) THEN
         STAT = 'READ'
      ELSE
         STAT= 'HDWR'
         END IF
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, DTYP, IUSER,
     *   PLMAP, PLFIND, SLOT, CATBLK, IWBLK, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 10
         GO TO 999
         END IF
C                                       Add plot file to header
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, SLOT, CATBLK, IWBLK, SAVE, 'READ',
     *      IVER, IERR)
         IF (IERR.EQ.0) GO TO 20
            IERR = IERR + 20
            GO TO 999
         END IF
C                                       Make physical filename
 20   CALL ZPHFIL ('PL', DISKIN, SLOT, IVER, PHNAME, IERR)
      IF (IERR.EQ.0) GO TO 30
         IERR = IERR + 30
         GO TO 980
C                                       Open plot file
 30   CALL GINIT (DISKIN, SLOT, PHNAME, IGSIZE, IGTYPE, NP, RPARM,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND,
     *   IERR)
      IF (IERR.EQ.0) GO TO 999
         GPHIND = 0
         IERR = IERR + 40
C
 980  IF (.NOT.DOTV) CALL DELEXT ('PL', DISKIN, SLOT, 'WRIT', CATBLK,
     *   IWBLK, IVER, IER)
C
 999  RETURN
      END
      SUBROUTINE POSSLB (IFNO, ICHLOW, ICHHI, IERR)
C-----------------------------------------------------------------------
C   POSSLB provides the labels surrounding the plot produced by FRPL.
C   Input parameters:
C      IFNO        I       IF number to plot
C      ICHLOW      I       Start of freq channel range.
C      ICHHI       I       End of freq channel range.
C   Output parameter:
C      IERR        I       Error code
C   Input from common
C      STNS     C(28)*8    Station names.
C      SAUCE    C*16       First source name.
C      KHIMN    H(3)       Image name (12 characters).
C      KHOBJ    H(2)       Source name.
C      KIIMS    I          Image sequence number.
C      WFFTR    L          Plot type; 0=> Fringe rate spectrum,
C                          1=> Complex visibility vs time.
C      MULTI    L          Multi-source file ?
C      PVER     I          PL version number.
C      CHOUT    R(4)       No of characters outside pix on left
C                          bottom, right, top resp.
C      ANTENS   I(50)      List of antennas selected; 0=> all
C                          Any -ve => all except those listed.
C      DOFQSL   L          in /SELCAL/.
C      NANTSL   I          Number of antennas selected.
C      TEND     R          End of selected timerange (days).
C      TSTART   R          Start of selected timerange (days).
C      STOKES   C*4        Stokes type wanted.
C      REFCHA      I       Reference channel
C   Input/output via common
C      CNOIN    I          Input file catalog number.
C      DISKIN   I          Input file disk number.
C      CATBLK   I(256)     Catalog header.
C      CATBLK   I(256)     Catalog header block.
C      CATH     R(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C      BLC      R(2)       Number of first pixel in row and col.
C      PLTBLK   R(256)     Line plot buffer.
C      TRC      R(2)       Last pixel pos in row/col (npnts+1).
C      CATUV    I(256)     Catalog header.
C      TIMRNG   R(8)       Start d,h,m,s; end d,h,m,s (0=>all).
C      UVRNG    R(2)       Min and max baseline lengths to select.
C                          1000's wavelengths; 0's => all.
C      FRQSEL   I          Default FQ table entry to select (-1.0)
C-----------------------------------------------------------------------
      INCLUDE 'FRMAP.INC'
      INTEGER ICHLOW, ICHHI
      CHARACTER TEXT*132, ATIME*8, CTEMP*8, ADATE*12, TS*1, POLLAB*4,
     *   CTEMP1*18, LFRLAB*16, BNDCOD(MAXIF)*8
      HOLLERITH CATH(256)
      INTEGER   IERR, INCHAR, ID(3), IT(3), ITIM(8), IANGLE, IFNO,
     *   PFQSID(MAXIF), IP
      REAL      DX, DY, CATR(256), PFQTBW(MAXIF), PFQCHW(MAXIF), PBW
      DOUBLE PRECISION PFQFRQ(MAXIF), PFREQ
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA LFRLAB /'Fringe-rate map'/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((ABS(LABEL).LE.1) .OR. (ABS(LABEL).GE.7)) GO TO 999
C                                       Date/time/version
      DX = 0.0
      DY = CHOUT(4) - 1.5
      IF (LABEL.GT.1) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (TEXT,1050) PVER, ADATE, ATIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
         END IF
C                                       File name label
      CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (MULTI) THEN
         TEXT = SAUCE(:8)
      ELSE
         CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
         TEXT = CTEMP
         END IF
      IF (TEXT.NE.' ') THEN
         TEXT(11:11) = '_'
         IP = 12
      ELSE
         IP = 1
         END IF
      CALL H2CHR (18, 1, CATH(KHIMN), CTEMP1)
      CALL NAMEST (CTEMP1, CATBLK(KIIMS), TEXT(IP:), INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.333
C                                       Insert FQ info
      IF (DOFQSL) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATUV, 45, PFQFRQ, PFQTBW,
     *      PFQCHW, PFQSID, BNDCOD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1080) IERR
            GO TO 990
            END IF
         PFREQ = PFQFRQ(IFNO) / 1.0D9
         PBW = PFQTBW(IFNO) / 1.0E6
         WRITE (TEXT, 1090) PFREQ, PBW
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
         END IF
C                                       lower title line
      DY = -2.833
      IF (ABS(LABEL).GT.2) DY = DY - 1.333
      IF (REFCHA .GT. 0) THEN
         WRITE (TEXT,1060) LFRLAB, IFNO, ICHLOW, ICHHI, REFCHA
      ELSE
         WRITE (TEXT,1065) LFRLAB, IFNO, ICHLOW, ICHHI
         END IF
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL REFRMT (TEXT, '_', INCHAR)
      DX = 0.0
      IANGLE = 0
      CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.333
C                                       Selection comment
C                                       Timerange
      TEXT = ' '
      IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4)) THEN
         CALL T2DHMS (TSTART, TS, ITIM(1))
         CALL T2DHMS (TEND, TS, ITIM(5))
         WRITE (TEXT,1020) ITIM
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
         END IF
C                                       UV range
      IF (((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9))) THEN
         TEXT = ' '
         IF (UVRNG(1).EQ.1.E-9) UVRNG(1) = 0.0
         WRITE (TEXT,1010) UVRNG
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
         END IF
C                                       Antennas + Stokes
      TEXT = ' '
C                                       Decide on stokes labelling
      IF (STOKES.EQ.'    ') THEN
         POLLAB = 'RR  '
      ELSE
         POLLAB = STOKES
         END IF
      IF (POLLAB.EQ.'FULL') POLLAB = 'I   '
      IF (POLLAB(1:1).EQ.'I') POLLAB = 'I   '
      IF (POLLAB.EQ.'HALF') POLLAB = 'RR  '
      CTEMP = SAUCE(1:8)
      WRITE (TEXT,1070) CTEMP, POLLAB
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Uvrange:___',1PE10.3,' TO ',1PE10.3,' Klambda')
 1020 FORMAT ('Timerange: ',I3.3,'/',I2.2,1X,I2.2,1X, I2.2,
     *               ' to ',I3.3,'/',I2.2,1X,I2.2,1X, I2.2)
 1050 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1060 FORMAT (A,' _IF: ',I3,' _CH: (',I4,' - ',I4,') / ',I4)
 1065 FORMAT (A,' _IF: ',I3,' _CH: ',I4,' - ',I4)
 1070 FORMAT ('Source: ',A8,' ___Stokes: ',A4)
 1080 FORMAT ('POSSLB: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
 1090 FORMAT ('Freq = ',F8.4,' GHz, Bw = ',F8.3,' MHz')
      END
      SUBROUTINE GETSO (SOU, DISK, CNO, CATBLK, LUN, INDSOU, IERR)
C-----------------------------------------------------------------------
C   Routine to look up source info for a given source number.
C   If can't find SU table the info will be obtained from
C   the common maintained by UVPGET; UVPGET should be called before
C   calling GETSO.
C   Inputs:
C      DISK     I       Disk number for NX and SN tables.
C      CNO      I       Catalog slot number
C      CATBLK   I(256)  Catalog header
C      LUN      I       LUN to use. (e.g. 25)
C   Input/Output
C      SOU      C*16    Source name.
C      INDSOU   I       Source ID as defined in the SOURCE table.
C   Output in common (INCLUDE INCS:DSOU.INC)
C      QUAL     I       Source qualifier.
C      CALCOD   C*4     Calibrator code
C      FLUX     R(4,*)  Total flux density I, Q, U, V pol, (Jy)
C                          1 set per IF.
C      VELTYP   C*8     Velocity type ('LSR', 'HELIO')
C      VELDEF   C*8     Velocity def.'RADIO','OPTICAL
C      SUFQID   I       FQ ID for which flux, vel etc. were modified.
C                       -1 => have virgin values
C                       -999 => is not in the table
C      FREQO    D(*)    Frequency offset (Hz)
C      BANDW    D       Bandwidth (Hz)
C      RAEPO    D       Right ascension at mean EPOCH (radians)
C      DECEPO   D       Declination at mean EPOCH (radians)
C      EPOCH    D       Mean Epoch for position in yr. since year 0.0
C      RAAPP    D       Apparent Right ascension (radians)
C      DECAPP   D       Apparent Declination(radians)
C      LSRVEL   D(*)    LSR velocity (m/sec) of each IF
C      RESTFQ   D(*)    Line rest frequency (Hz) of IF.
C      PMRA     D       Proper motion (deg/day) in RA
C      PMDEC    D       Proper motion (deg/day) in declination
C   Output:
C      IERR     I       Return code. 0=OK, else failed.
C                          11 => Couldn't find source in SU table.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), LUN, INDSOU, IERR, CATUV(256)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), VER, KOLS(MAXSUC), NUMV(MAXSUC), NUMIF,
     *   JERR, MSGSAV, IRNO, NUMREC, LOOP, IEQ, DIR
      REAL      REQ, POLAR(2)
      DOUBLE PRECISION JD, DELDAT, OBSPOS(3)
      LOGICAL GR
      HOLLERITH CATH(256)
      CHARACTER SOU*16, BLAN*16, OBSDAT*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATUV, CATH)
      EQUIVALENCE (IEQ, REQ)
      DATA BLAN /'                '/
      DATA  DELDAT, DIR, OBSPOS, POLAR /1.D-6, 1.D0,
     *   0.D0, 0.D0, 0.D0, 0., 0./
C-----------------------------------------------------------------------
      CALL RCOPY (256, CATBLK, CATUV)
      IERR = 0
C                                       Initialize
      JERR = 0
      IERR = 0
C                                       Find Source Info
C                                       Open SU table
      MSGSAV = MSGSUP
      MSGSUP = 32000
      VER = 1
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, JERR)
      MSGSUP = MSGSAV
      IF (JERR.NE.0) GO TO 500
C                                       Get number of records
      NUMREC = BUFFER(5)
C                                       Find source
      DO 50 LOOP = 1,NUMREC
         IRNO = LOOP
         CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, IDSOUR, SNAME,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Desired source?
C                                       If SOU=BLANK then take
C                                       the first source
         IF (SOU.EQ.BLAN) THEN
            INDSOU = IDSOUR
            SOU = SNAME
C                                       Convert to radians
            RAAPP = RAAPP * DG2RAD
            DECAPP = DECAPP * DG2RAD
            RAEPO = RAEPO * DG2RAD
            DECEPO = DECEPO * DG2RAD
            GO TO 60
         ELSE
            IF (SNAME.EQ.SOU) THEN
               INDSOU = IDSOUR
C                                       Convert to radians
               RAAPP = RAAPP * DG2RAD
               DECAPP = DECAPP * DG2RAD
               RAEPO = RAEPO * DG2RAD
               DECEPO = DECEPO * DG2RAD
               GO TO 60
               END IF
            END IF
 50      CONTINUE
C                                       Didn't find source
      JERR = 11
      WRITE (MSGTXT,1050) SOU
 60   CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1060) IERR
         GO TO 990
 70   IERR = JERR
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Single source, get info from
C                                       header.
 500  CONTINUE
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY(OBSDAT, JD)
C                                       Find RAAPP, DECAP for
C                                       observation date.
C                                       Note use of EQUIVALENCE:
      IEQ = CATBLK(KREPO)
      EPOCH = REQ
      RAEPO = RA * DG2RAD
      DECEPO = DEC * DG2RAD
      GR = .TRUE.
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *   RAEPO, DECEPO, RAAPP, DECAPP)
      IERR = 0
      INDSOU = 0
      QUAL = 0
      CALCOD = '    '
      VELTYP = '        '
      VELDEF = '        '
      SUFQID = -999
      SOU = SOURCE
      BANDW = 0.0
      DO 550 LOOP = 1,NUMIF
         FREQO(LOOP) = 0.0D0
         FLUX(1,LOOP) = 0.0
         FLUX(2,LOOP) = 0.0
         FLUX(3,LOOP) = 0.0
         FLUX(4,LOOP) = 0.0
         LSRVEL(LOOP) = 0.0D0
         RESTFQ(LOOP) = 0.0D0
 550     CONTINUE
      PMRA = 0.0
      PMDEC = 0.0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETSO: TABSOU ERROR ',I3)
 1050 FORMAT ('GETSO: COULD NOT FIND DATA FOR SOURCE ', A16)
 1060 FORMAT ('GETSO: TABIO ERROR ',I3,' CLOSING SU TABLE')
      END
      SUBROUTINE UUVV(TIME, LBASE, GSTRA, U, V, W, UDOT, VDOT, WDOT)
C-----------------------------------------------------------------------
C   Calculate  U, V, W and its derivatives for both ground
C   based and orbiting antennas.
C   Inputs:
C      TIME     D        Time, UT in days
C      LBASE    I        Baseline number
C      GSTRA    D        GST  in radians
C   Input from common /FRRATE/
C      XA, YA, ZA     D(*)  Array of ground antennas cartesian
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
C      U        R        U in meters
C      V        R        V in meters
C      W        R        W in meters
C      UDOT     R        Derivative of U in meters/sec
C      VDOT     R        Derivative of V in meters/sec
C      WDOT     R        Derivative of W in meters/sec
C-----------------------------------------------------------------------
      INCLUDE 'FRMAP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSOU.INC'
      DOUBLE PRECISION GSTRA, GAST, SIDT, ORBIT1(6), ORBIT2(6),
     *   XG1, YG1, ZG1, XG2, YG2, ZG2, BX, BY, BZ, VX, VY, VZ,
     *   TIME
      REAL  U, V, W, UDOT, VDOT, WDOT, VW, VWDOT
      INTEGER  LBASE, JAN1, JAN2, IORB1, IORB2
      DATA SIDT /1.00273790935/
C-----------------------------------------------------------------------
C                                       find the antennas from LBASE
      JAN1 = ANT1(LBASE)
      JAN2 = ANT2(LBASE)
      IORB1 = IORBIT(JAN1)
      IORB2 = IORBIT(JAN2)
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
      XG1 = XA(JAN1)
      YG1 = YA(JAN1)
      ZG1 = ZA(JAN1)
      XG2 = XA(JAN2)
      YG2 = YA(JAN2)
      ZG2 = ZA(JAN2)
C                                       GST at the given time
      GAST = GSTRA + TWOPI * TIME * SIDT
C                                       calculate baseline
      CALL BACOOR (IORB1, IORB2, ORBIT1, ORBIT2, XG1, YG1, ZG1,
     *   XG2, YG2, ZG2, GAST, TIME, BX, BY, BZ, VX, VY, VZ)
C                                       U, V, W in meters
      VW = BX * COS(RAAPP) + BY * SIN(RAAPP)
      U = -BX * SIN(RAAPP) + BY * COS(RAAPP)
      V = -VW * SIN(DECAPP) + BZ * COS(DECAPP)
      W = VW * COS(DECAPP) + BZ * SIN(DECAPP)
      VWDOT = VX * COS(RAAPP) + VY * SIN(RAAPP)
C                                       UDOT, VDOT, WDOT in m/sec
      UDOT = -VX * SIN(RAAPP) + VY * COS(RAAPP)
      VDOT = - VWDOT * SIN(DECAPP) + VZ * COS(DECAPP)
      WDOT = VWDOT * COS(DECAPP) + VZ * SIN(DECAPP)

C      C = (PI * PI * SIDT) / (180 * 3.6 * 3.6 * 12)
C                                       consider DX = DALFA, so
C                                       there is COS(DECAPP) in U
C      U = C * (BX * COS(HANGLE) + BY * SIN(HANGLE)) * COS(DECAPP)
C      V = C * (BX * SIN(HANGLE) - BY * COS(HANGLE)) * SIN(DECAPP)
C                                       I do not know why the life
C                                       require to inverse the signs
C      U = -U
C      V = -V
      RETURN
      END

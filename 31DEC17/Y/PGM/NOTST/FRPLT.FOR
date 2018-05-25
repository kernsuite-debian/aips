LOCAL INCLUDE 'FRPLT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NUMTIM
      PARAMETER (NUMTIM = 32768)
      INTEGER MXTIFP, MXBUFT
      PARAMETER (MXTIFP = 350000)
      PARAMETER (MXBUFT = MXTIFP/10)
      CHARACTER NAMEIN*12, CLAIN*6, OFILE*48, XSOUR(30)*16, XCALCO*4,
     *   HISCRD(10)*72, STNS(MAXANT)*8, SAUCE*16, PPLOT*8,
     *   JY*2, STKLAB*4, POLLAB(4)*5
      HOLLERITH XNAMEI(3), XCLAIN(2), XOFILE(12), XXSOUR(4,30), XXSTOK,
     *   XXCALC, XCODET, XPPLOT(2)
      REAL      USERID, XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID,
     *   XUVR(2), XTIME(8), XBIF, XEIF, XBCHAN, XECHAN, XSUBA,
     *   XANTEN(50), XBASE(50), XDOAC, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), SHIFT(2), APARM(10),
     *   XSOLIN, XNCOUN, BPARM(10), XLABEL, FACTOR, XBADD(10), XDOTV,
     *   XGRCH
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, PLTBLK(256), PVER,
     *   NUMHIS, PBIF, PEIF, PLMAP, PLFIND, STRTIF, SLOT, STOPIF, GRCHN,
     *   TVCHN, TVCORN(4), IGLUN, IGFIND, NUMFRQ, PBCH, PECH, NOMIT,
     *   CODTYP, POLPLT, NCOUNT, XA1(MXBASE), XA2(MXBASE), NBASE,
     *   ICURNT, OFFSET, NUMXCF, CATSAV(256), NEXTSC, PFQSID(MAXIF),
     *   LABEL, PLSTVS, CHNUM, NPNL, POLNUM, STRPOL, LTYPE,
     *   STOPOL, NTIME, NFRATE, NTRANS, NTIMEP, NPADZ, VISTYP
      REAL   BLC(2), TRC(2), BUFF1(1024), SCAMP(MXTIFP), CHOUT(4),
     *   BUFF2(2,MXTIFP), WTS(MXTIFP), XWORK(MXTIFP), KEEPTM(8), UVSCAL,
     *   STARTD, STOPD, STARTU, STOPU, PFQTBW(MAXIF), PFQCHW(MAXIF),
     *   FINC(MAXIF), PREAVG
      DOUBLE PRECISION FOFF(MAXIF), PFQFRQ(MAXIF)
      LOGICAL MULTI, DIVCH0, DOCHIF, DOTV, DOSHFT, BLNKBF, SCANAV,
     *   FIXED, SELFSA, SELFSP, SELFSX, DOCHPL, DIDCHN, WFFTR
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR,
     *   XQUAL, XXCALC, XBAND, XFREQ, XFQID, XUVR, XTIME, XXSTOK, XBIF,
     *   XEIF, XBCHAN, XECHAN, XSUBA, XANTEN, XBASE, XDOAC, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH,
     *   SHIFT, APARM, XCODET, XPPLOT, XSOLIN, XNCOUN, BPARM, XOFILE,
     *   XLABEL, FACTOR, XBADD, XDOTV, XGRCH
      COMMON /FRPARM/ SEQIN, SEQOUT, DISKIN, CNOIN, PBIF, PEIF, PLMAP,
     *   PLFIND, STRTIF, STOPIF, NUMFRQ, PBCH, PECH,  NOMIT, CODTYP,
     *   POLPLT, NCOUNT, XA1, XA2, NBASE, ICURNT, OFFSET, NUMXCF,
     *   NEXTSC, LABEL, PLSTVS, CHNUM, NPNL, POLNUM, STRPOL, STOPOL,
     *   NTIME, NFRATE, NTRANS, NTIMEP, NPADZ, VISTYP, LTYPE
      COMMON /VALPRM/ PREAVG
      COMMON /LOGS/ DIVCH0, DOCHIF, SCANAV, FIXED, SELFSA, SELFSP,
     *   SELFSX, DOCHPL, DIDCHN, WFFTR
      COMMON /CHPARM/ NAMEIN, CLAIN, OFILE, XSOUR, XCALCO, HISCRD,
     *   STNS, SAUCE, PPLOT, JY, STKLAB, POLLAB
      COMMON /PLOT/ PLTBLK, BLC, TRC, CHOUT, DOTV, DOSHFT, BLNKBF,
     *   GRCHN, TVCHN, TVCORN, IGLUN, IGFIND, SLOT
      COMMON /LABEL/ MULTI, PVER
      COMMON /BUFRS/ CATSAV, FOFF, PFQFRQ, BUFF1, SCAMP, BUFF2, WTS,
     *   XWORK, KEEPTM, STARTD, STOPD, STARTU, STOPU, PFQTBW, PFQCHW,
     *   PFQSID, NUMHIS, FINC, UVSCAL
LOCAL END
LOCAL INCLUDE 'FRPL2.INC'
      CHARACTER ANTNAM(2)*8, EXPDAT*8, SRCOBS*16
      INTEGER   NCHAN, TELNUM(2), RAHR, RAMIN, DECDEG, DECMIN
      REAL      CTIME, TBW, AVWGHT, RASEC, DECSEC
      DOUBLE PRECISION MFREQ
      COMMON /OUTCHR/ ANTNAM, EXPDAT, SRCOBS
      COMMON /OUTDAT/ MFREQ, CTIME, TBW, AVWGHT, RASEC, DECSEC,
     *   NCHAN, TELNUM, RAHR, RAMIN, DECDEG, DECMIN
LOCAL END
LOCAL INCLUDE 'FRPL3.INC'
      INTEGER NCTOT, NXP, NYP
      CHARACTER ALABEL(2)*20, APREF(2)*5
      REAL   AMP(MXTIFP), PHASE(MXTIFP), MAXPHS, MINPHS, MAXAMP, MINAMP,
     *   AMPRNG, PHSRNG, LINT, XLINT, XYRATI, YBLC(2), YTRC(2), XMULT,
     *   XMIN, XMAX, XSLOPE, XOFF
      COMMON /PLPARM/ AMP, PHASE, MAXPHS, MINPHS, MAXAMP, MINAMP, LINT,
     *   XLINT, AMPRNG, PHSRNG, XYRATI, YBLC, YTRC, XMULT,
     *   NCTOT, NXP, NYP, XMIN, XMAX, XSLOPE, XOFF
      COMMON /AXLABS/ ALABEL, APREF
LOCAL END
LOCAL INCLUDE 'FRPL4.INC'
      INTEGER NXPANE, NYPANE, NXPIX, NYPIX, ICOUNT
      REAL    PLTXIN, PLTYIN
      LOGICAL CLOSED
      COMMON /FRESH/ PLTXIN, PLTYIN, NXPANE, NYPANE, NXPIX, NYPIX,
     *   ICOUNT, CLOSED
LOCAL END
LOCAL INCLUDE 'FRRANX.INC'
C                                       NX stuff
      INTEGER MAXNX
      PARAMETER (MAXNX = 10000)
      INTEGER   BUFFNX(512), NXVER, NXLUN, IRNONX, KOLSNX(MAXNXC),
     *   NUMVNX(MAXNXC), NXVISN(2,MAXNX), NUMNX, NXSOU(MAXNX)
      REAL      NXTIM(2,MAXNX)
      LOGICAL INDXT
      COMMON /PLOTNX/ BUFFNX, NXVER, NXLUN, IRNONX, KOLSNX, NUMVNX,
     *   NXVISN, NXSOU, INDXT, NUMNX, NXTIM
LOCAL END
      PROGRAM FRPLT
C-----------------------------------------------------------------------
C! FRPLT plots fringe rate spectra of uv-data and visibilities vs time.
C# Calibration Graphics Sdish Spectral UV VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2012, 2014-2017
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
C   Selects a range of line data and plots them
C   Calibration and editing may be optionally applied.
C   Inputs:
C      AIPS adverb          Description.
C      USERID.....User number ignored
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      SOURCES....Source list.
C      UVRANG.....UV range of the data to be plotted.
C      TIMERANG...Time range of the data to be plotted.
C      STOKES.....Stokes type to pass.
C      BIF........First IF to plot. 0=>all.
C      EIF........Highest IF to plot. 0=>all higher than BIF
C      BCHAN......First channel to select. 0=>all.
C      ECHAN......Highest channel to select. 0=>all
C      SUBARRAY...Subarray number to select. 0=>all.
C      ANTENS.....Antenna numbers to select. 0=>all.
C      XBASE......Baseline numbers to go with antennas. 0=> all
C      DOCALIB....If true (>0) then calibrate the data.
C      GAINUSE....Version number of the Cal. table to use.
C      FLAGVER....Specifies the version of the flagging table.
C      DOBAND.....If (>0) will do bandpass correction.
C      BPVER......Specifies the version of the bandpass table.
C      SMOOTH.....Smoothing function.
C      SHIFT......Position shift
C      APARM......Control information:
C                    1 => pre-average interval (sec)
C                    2 = 0 => self-scale
C                    2 > 0 => fixed scale
C                    3 = minimum ampl.
C                    4 = maximum ampl.
C                    5 = minimum phase.
C                    6 = maximum phase.
C                    7   plot type
C                        0 => fringe rate spectrum
C                        1 => visibility vs time
C                    8   8: zero padding for FFT
C                      = 1 =>  no padding
C                      = 2 => N zeroes
C                      = 3 => 2N zeroes etc.
C                    9 > 0 => plot several IF's
C                             and/or polarizations
C                             together
C      CODETYPE...Type of function to plot
C      POLPLOT....Polzn combination
C      SOLINT.....time interval for multiple plots
C      NCOUNT.....# plots/page
C      BPARM......Control options for divide by channel 0
C      OUTFILE....Filename in which to write plotted spectrum
C      BADDISK....Disks to avoid for scratch files.
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UTYPE*2
      INTEGER   IRET, NPARMS, IIF, JIF, IPOL, LPOLNM, LPEIF
      REAL      TSOLIN
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRRANX.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'FRPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      DIDCHN = .FALSE.
      CALL FRRAIN (PRGM, NPARMS, IRET)
      IF (IRET.NE.0) GO TO 990
      TSOLIN = XSOLIN / (60.0 * 24.0)
C                                       Single plot files
      IF (NCOUNT.EQ.0) THEN
   10    CONTINUE
         ICURNT = 1
         CALL FRRAUV (IRET)
         IF ((IRET.EQ.5) .AND. (XSOLIN.NE.0.0)) GO TO 200
         IF (IRET.NE.0) GO TO 990
C
         DO 15 IIF = 1,50
            XANTEN(IIF) = ANTENS(IIF)
 15         CONTINUE
C                                       Clear write status from FRRAIN
         UTYPE = 'UV'
         CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN,
     *      UTYPE, NLUSER, 'CLWR', BUFF1, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.10)) GO TO 990
C                                       Plot the result
         LPEIF = PEIF
         LPOLNM = POLNUM
         IF (DOCHIF) LPEIF = PBIF
         IF (DOCHPL) LPOLNM = 1
         DO 100 IPOL = 1,LPOLNM
         DO 100 IIF = PBIF,LPEIF
            IF ((IIF.EQ.LPEIF) .AND. (IPOL.EQ.LPOLNM)) THEN
               JIF = IIF
            ELSE
               JIF = -IIF
               END IF
            IF (DOCHIF) THEN
               STRTIF = PBIF
               STOPIF = PEIF
            ELSE
               STRTIF = IIF
               STOPIF = IIF
               END IF
            IF (DOCHPL) THEN
               STRPOL = 1
               STOPOL = POLNUM
            ELSE
               STRPOL = IPOL
               STOPOL = IPOL
               END IF
            CALL FILLPL (IIF, IPOL, IRET)
            IF (BLNKBF) GO TO 100
            CALL FRRAPL (NPARMS, JIF, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
            CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *         CATBLK, .FALSE., BUFF1, IRET)
            IF (IRET.NE.0) GO TO 990
 100        CONTINUE
C                                       Loop for more times?
 200     IF (XSOLIN.NE.0.0) THEN
C                                       Scan averaging?: use NX table
            IF (SCANAV) THEN
               NEXTSC = NEXTSC + 1
               IF (NEXTSC.GT.NUMNX) GO TO 990
               STARTU = NXTIM(1,NEXTSC)
               STOPU = NXTIM(2,NEXTSC)
               FSTVIS = NXVISN(1,NEXTSC)
               LSTVIS = NXVISN(2,NEXTSC)
            ELSE
               STARTU = STOPU + 0.02 / (24. * 60. * 60.)
               INITVS = PLSTVS + 1
               CALL FINDUV (STARTU, TSOLIN, PLSTVS, IRET)
               IF (IRET.NE.0) THEN
                  IRET = MAX (IRET, 0)
                  GO TO 990
                  END IF
               STOPU = STARTU + TSOLIN
               END IF
            STOPU = MIN (STOPU, STOPD)
            TIMRNG(1) = STARTU
            TIMRNG(5) = STOPU
            IF (STARTU.LT.STOPD) GO TO 10
            IRET = 0
            END IF
C                                       Multiple plot/page
      ELSE
         CALL MULTPL (NPARMS, IRET)
         IF (IRET.EQ.0) GO TO 990
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FRRAIN (PRGN, NPARM, JERR)
C-----------------------------------------------------------------------
C   FRRAIN gets input parameters for FRPLT, finds input file and
C   prepares the list of sources.  All selection criteria except the
C   source name is filled into the commons in D/CSEL.INC.
C   Inputs:
C      PRGN     C*6   Program name
C   Output:
C      NPARM    I    No. of real words read from AIPS
C      JERR     I    Error code: 0 => ok
C                       5 => catalog troubles
C                       8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRRANX.INC'
      CHARACTER STAT*4, PRGN*6, UTYPE*2, CODET*4, VSTNS(MAXANT)*8,
     *   BNDCOD(MAXIF)*8
      INTEGER   JERR,  NPARM, IROUND, IERR, I, LUNTB, IVER, IANT, NANT,
     *   LUNAN, IABUF(512), I4TEMP, IUSER, LUN, NXANT, NXBASL,
     *   IXANT(50), IXBASL(50), ISBAND(MAXIF), K
      REAL      CATR(256), EPS, TSOLIN
      LOGICAL   F, TABLE, FITASC, MATCH, DESEL, EXIST
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA F /.FALSE./
      DATA LUNTB, LUNAN, LUN / 39, 27, 28 /
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NUMHIS = 0
      NOMIT = 0
      ISVLBA = F
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 314
      CALL GTPARM (PRGN, NPARM, RQUICK, USERID, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XCODET, CODET)
      CALL H2CHR (8, 1, XPPLOT, PPLOT)
      CODTYP = 1
      IF (CODET.EQ.'AMP ') CODTYP = 2
      IF (CODET.EQ.'PHAS') CODTYP = 3
      IF (CODET.EQ.'R&I ') CODTYP = 4
      IF (CODET.EQ.'REAL') CODTYP = 5
      IF (CODET.EQ.'IMAG') CODTYP = 6
      IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.4)) THEN
         FIXED = (APARM(2).GT.0.0) .AND. (APARM(3).LT.APARM(4)) .AND.
     *      (APARM(5).LT.APARM(6))
      ELSE IF ((CODTYP.EQ.2) .OR. (CODTYP.EQ.2)) THEN
         FIXED = (APARM(2).GT.0.0) .AND. (APARM(3).LT.APARM(4))
      ELSE
         FIXED = (APARM(2).GT.0.0) .AND. (APARM(5).LT.APARM(6))
         END IF
      SELFSA = (APARM(2).LE.0.0) .OR. (APARM(3).GE.APARM(4))
      SELFSP = (APARM(2).LE.0.0) .OR. (APARM(5).GE.APARM(6))
      SELFSX = (APARM(2).LE.0.0) .OR. (BPARM(5).GE.BPARM(6))
      VISTYP = APARM(10) + 0.01
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      EPS = 0.1
      USERID = NLUSER
      IUSER = NLUSER
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      POLPLT = 0
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
C                                       plot fringe rate spectra or
C                                       visibilities?
      WFFTR = APARM(7) .LE. 0
C                                       number ov plots at a page
      NCOUNT = IROUND (XNCOUN)
      NCOUNT = MAX (0, NCOUNT)
      IF (NCOUNT.GT.9) THEN
         MSGTXT = 'NCOUNT TOO LARGE: SETTING TO 9'
         CALL MSGWRT (7)
         NCOUNT = 9
         END IF
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1200) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      IUSER
         GO TO 990
         END IF
C                                       check status
      CALL CATDIR ('INFO', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1300) JERR, NAMEIN, CLAIN, SEQIN, DISKIN
         GO TO 990
         END IF
      IF (((.NOT.DOTV) .AND. (STAT.NE.'REST')) .OR. ((DOTV) .AND.
     *   (STAT.EQ.'WRIT'))) THEN
         MSGTXT = 'UV FILE IS TOO BUSY, CURRENT STATUS = ' // STAT
         JERR = 10
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1400) JERR
         GO TO 990
         END IF
C                                       Save input header
      CALL COPY (256, CATBLK, CATUV)
      CALL COPY (256, CATBLK, CATSAV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      IF ((ISORT(1:1).NE.'T') .AND. (XSOLIN.NE.0.0)) THEN
         MSGTXT = 'SOLINT NOT 0 REQUIRES TIME-ORDERED DATA NOT '
     *      // ISORT
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
         END IF
C                                       Determine if multi-source file
      CALL MULSDB (CATUV, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *      MULTI, FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       Save header and pointers
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C
      JY = 'Jy'
      IF (TYPUVD.GT.0) JY = 'K'
      DOACOR = XDOAC.GT.0.0
      DOXCOR = .TRUE.
      IF ((NCOUNT.EQ.0) .AND. (DOACOR)) DOXCOR = .FALSE.
C
      I = IROUND (APARM(9))
      DOCHIF = (I.EQ.1) .OR. (I.GT.2)
      DOCHPL = (I.GE.2)
C                                       Read antenna header -> NANT
      NANT = 0
      IVER = 1
      CALL ISTAB ('AN', DISKIN, CNOIN, IVER, LUNAN, IABUF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.EQ.0) .AND. (EXIST)) THEN
         CALL ANTINI ('READ', IABUF, DISKIN, CNOIN, IVER, CATBLK, LUNAN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
         IF (JERR.EQ.2) GO TO 30
         IF (JERR.NE.0) GO TO 990
         I4TEMP = IABUF(5)
         NANT = I4TEMP
         DO 20 IANT = 1,MAXANT
            WRITE (STNS(IANT),1500) 'VLA', IANT
 20         CONTINUE
         DO 25 IANT = 1,NANT
            CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, JERR)
            IF (JERR.NE.0) GO TO 990
            STNS(NOSTA) = ANNAME
 25         CONTINUE
         CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, JERR)
         IF (JERR.NE.0) GO TO 990
         ISVLBA = ANAME(1:4) .EQ. 'VLBA'
         END IF
 30   IF (NANT.EQ.0) THEN
         DO 40 I = 1,MAXANT
            WRITE (STNS(IANT),1500) '???', I
 40         CONTINUE
         NANT = 50
         END IF
C                                       BADDISK
      DO 50 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 50      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      IUDISK = UDISK
      IUCNO = CNOIN
      IXLUN = 28
      DO 55 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         CALSOU(I) = ' '
 55      CONTINUE
      SAUCE = SOURCS(1)
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
C
      CALL RCOPY (2, XUVR, UVRNG)
      CALL FILL (50, 0, ANTENS)
C
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
C
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      PBCH = BCHAN
      PECH = ECHAN
      IF (JLOCIF.GE.0) THEN
         PBIF = IROUND (XBIF)
         PBIF = MAX (1, MIN (PBIF, CATBLK(KINAX+JLOCIF)))
         PEIF = IROUND (XEIF)
         IF (PBIF.GT.PEIF) PEIF = CATBLK(KINAX+JLOCIF)
         PEIF = MAX (1, MIN (PEIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         PBIF = 1
         PEIF = 1
         END IF
      BIF = PBIF
      EIF = PEIF
C                                       Pre-average interval in days
      IF (APARM(1).LE.0.0) APARM(1) = 2.0
      PREAVG = APARM(1) / 86400.0
C                                       FFT zero padding option
      NPADZ = 1
      IF (APARM(8).GT.0) NPADZ = APARM(8) + 0.1
      APARM(8) = NPADZ
C                                       Compute IF, chan, time axes
C                                       for pre-average buffer.
      NUMIF = PEIF - PBIF + 1
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (2*NUMIF*NUMPOL*NTRANS.GT.MXTIFP.OR.
     *      ((2*NTRANS) .GT. MXBUFT)) THEN
         JERR = 1
         MSGTXT = 'FILLPL: SPECTRA TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BPVER = IROUND (XBPVER)
      DOBAND = IROUND (XDOBND)
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Check Stokes
      CALL FNDPOL (STOKES, JERR)
      IF (JERR.NE.0) GO TO 999
      IF ((STOKES.EQ.'I') .OR. (STOKES.EQ.'Q') .OR. (STOKES.EQ.'U')
     *      .OR. (STOKES.EQ.'V') .OR. (STOKES.EQ.'RR') .OR.
     *      (STOKES.EQ.'LL')  .OR. (STOKES.EQ.'RL')) THEN
         POLNUM = 1
         POLLAB(1) = STOKES
      ELSE IF (STOKES.EQ.'IV') THEN
         POLNUM = 2
         POLLAB(1) = 'I'
         POLLAB(2) = 'V'
      ELSE IF (STOKES.EQ.'IQU') THEN
         POLNUM = 3
         POLLAB(1) = 'I'
         POLLAB(2) = 'Q'
         POLLAB(3) = 'U'
      ELSE IF (STOKES.EQ.'IQUV') THEN
         POLNUM = 4
         POLLAB(1) = 'I'
         POLLAB(2) = 'Q'
         POLLAB(3) = 'U'
         POLLAB(4) = 'V'
      ELSE IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'RRLL')) THEN
         POLNUM = 2
         POLLAB(1) = 'RR'
         POLLAB(2) = 'LL'
      ELSE IF ((STOKES.EQ.'FULL') .OR. (STOKES.EQ.'RLRL')) THEN
         POLNUM = 4
         POLLAB(1) = 'RR'
         POLLAB(2) = 'LL'
         POLLAB(3) = 'RL'
         POLLAB(4) = 'LR'
         END IF
C                                       Divide by channel 0?
      DIVCH0 = BPARM(1).GT.0.0
      IF (DIVCH0) THEN
         IF (DOCAL) THEN
            WRITE (MSGTXT,1600)
            CALL MSGWRT (2)
            DOCAL = .FALSE.
            END IF
         IF ((BCHAN.NE.1) .AND. (ECHAN.NE.CATBLK(KINAX+JLOCF))) THEN
            BCHAN = 1
            ECHAN = CATBLK(KINAX+JLOCF)
            END IF
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1700)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATBLK, LUN, PFQFRQ, PFQTBW,
     *   PFQCHW, PFQSID, BNDCOD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1800) IERR
         GO TO 990
         END IF
C                                       get freq info
      I = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, I, CATBLK, LUN, K,
     *   FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.GT.0) THEN
         MSGTXT = 'ERROR READING FQ FILE'
         GO TO 990
         END IF
C
      CALL SETANT (50, XANTEN, XBASE, NXANT, NXBASL, IXANT, IXBASL,
     *   DESEL)
C                                       Fill in list of all antenna
C                                       - baseline pairs and names
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      CALL FILANT (DISKIN, CNOIN, CATBLK, LUNAN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, SUBARR, DOACOR, DOXCOR, NBASE, XA1, XA2, VSTNS,
     *   IABUF, ANTENS, JERR)
      IF (JERR.NE.0) GO TO 999
      IF (NCOUNT.GT.NBASE) NCOUNT = NBASE
C                                                Get source list
      CALL SOUFIL (JERR)
C                                        Scan averaging
      SCANAV = XSOLIN.LT.0.0
C
      CALL RCOPY (8, XTIME, TIMRNG)
      STARTU = TIMRNG(1) + TIMRNG(2) / 24.0 + TIMRNG(3) / 1440.0
     *   + TIMRNG(4) / 86400.0
      STOPU = TIMRNG(5) + TIMRNG(6) / 24.0 + TIMRNG(7) / 1440.0
     *   + TIMRNG(8) / 86400.0
C                                       Determine stop time of data
      IF (STOPU.LE.STARTU) THEN
         CALL UVTIME (DISKIN, CNOIN, CATBLK, STARTD, STOPD, JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'FRRAIN: UNABLE TO DETERMINE DATA STOP TIME'
            GO TO 990
            END IF
         IF (STARTU.LE.0.0) STARTU = STARTD
         IF (STOPU.LT.STARTU) STOPU = STOPD
         END IF
C                                        plot subsequent times?
      IF (XSOLIN.NE.0.0) THEN
C                                       check for time
         IF (.NOT.SCANAV) THEN
            INITVS = 1
            CALL FINDUV (STARTU, TSOLIN, PLSTVS, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
C                                       check selected span of data
         STOPD = 24.0 * 60.0 * (STOPU - STARTU)
C                                       adjust solint if needed
         XSOLIN = MIN (XSOLIN, STOPD)
         TSOLIN = XSOLIN / (24.0 * 60.0)
         STARTD = STARTU
         STOPD = STOPU
         STOPU = STARTU + TSOLIN
         END IF
      CALL RFILL (8, 0.0, TIMRNG)
      TIMRNG(1) = STARTU
      TIMRNG(5) = STOPU
C                                       Position shift
      DOSHFT = (SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)
      DOSHFT = (DOSHFT) .AND. (DOXCOR)
      DXTIME = 0.0
C                                                Get scan list
      IF (SCANAV) THEN
         NXLUN = 45
         NEXTSC = 1
         CALL NXSET (DISKIN, CNOIN, XTIME, SOUWAN, DOSWNT, NSOUWD,
     *      SUBARR, FRQSEL, JERR)
         IF (JERR.NE.0) GO TO 999
C                                                Set first time
         STARTU = NXTIM(1,NEXTSC)
         STOPU = NXTIM(2,NEXTSC)
         INITVS = NXVISN(1,NEXTSC)
         TIMRNG(1) = STARTU
         TIMRNG(5) = STOPU
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRRAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1200 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1300 FORMAT ('ERROR',I3,' CHECKING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' STATUS')
 1400 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1500 FORMAT (A,':AN',I2.2)
 1600 FORMAT ('DOCALIB=1 & BPARM(1)=1 incompatible, setting ',
     *   'DOCALIB=-1')
 1700 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1800 FORMAT ('FRRAIN: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
      END
      SUBROUTINE FRRAUV (IRET)
C-----------------------------------------------------------------------
C   FRRAUV loops through the data averaging according to the selection
C   and command criteria and returns the averaged array for plotting.
C   Output:
C      IRET     I     Return error code, 0=>OK, otherwise error.
C                     If IRET = 5, no data found
C                     If IRET = 10, no more data coming from multiple
C                     plots/page - so exit
C   Output in common:
C      BUFF2    R(2,MXTIFP)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   IRET, LOOPIF, LOOPF, INP, LUN3, I, IFNO, IANT1, IANT2,
     *   XCOUNT, INDEX, NPRI, SRCDUN, INX, HM(2), OLDSRC, IPOLPT, IPOL,
     *   ITS(4), ITE(4), JERR, J, LNUMIF, MSGSAV, IROUND, ITIME, NMISS,
     *   LVIST
      REAL      RPARM(20), SUMWT(MAXIF,4), XNORM, WT, VIS(UVBFSL),
     *   LINWT, AVTIME, CATR(256), DXC, DYC, DZC, XX, AMP, XXTIME
      COMPLEX   ZZ, VS
      HOLLERITH  CATH(256)
      CHARACTER CHSIGN*1, CTS*1, CTE*1
      DOUBLE PRECISION FRSCL, CATD(128), DTEMP, RA0, DEC0, RFREQ, DFREQ,
     *   TRUEF
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK,CATH,CATR,CATD)
      DATA LUN3 /25/
C-----------------------------------------------------------------------
      NTIMEP = (STOPU - STARTU) / PREAVG + 1.1
C                                       Force no of time bins to be even
      NTIMEP = NTIMEP + MOD (NTIMEP, 2)
      NTRANS = NTIMEP * NPADZ
C
      NPRI = 0
      JERR = 0
      BLNKBF = .TRUE.
C                                       Set lengths of input axes.
      LNUMIF = PEIF - PBIF + 1
C
      IF (ECHAN.LT.BCHAN) ECHAN = CATSAV(KINAX+KLOCFY)
      NUMFRQ = PECH - PBCH + 1
C
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (UVRNG(1).EQ.0.0) UVRNG(1) = 1.E-9
C                                       Zero output array
      I = LNUMIF * NTRANS * POLNUM
      CALL RFILL (I, 0.0, SCAMP)
      CALL RFILL (I, 0.0, WTS)
      I = I * 2
      CALL RFILL (I, 0.0, BUFF2)
      BIF = PBIF
      EIF = PEIF
      AVTIME = 0.0
      OLDSRC = -1
      IF (XSOLIN.NE.0.0) THEN
         CALL T2DHMS (STARTU, CTS, ITS)
         CALL T2DHMS (STOPU, CTE, ITE)
         END IF
C                                       Check if valid POLTYP
      LVIST = VISTYP
      CALL COPY (256, CATSAV, CATUV)
      CALL POLCHK (PPLOT, POLPLT)
      IF (POLPLT.GT.0) THEN
         IF ((POLPLT.EQ.3) .OR. (POLPLT.EQ.6)) THEN
            STOKES='HALF'
         ELSE
            STOKES='FULL'
            END IF
         POLNUM = 1
C                                       Change POLLAB based on POLPLT
         IF (POLPLT.EQ.1) POLLAB(1) = 'RL/RR'
         IF (POLPLT.EQ.2) POLLAB(1) = 'LR/RR'
         IF (POLPLT.EQ.3) POLLAB(1) = 'LL/RR'
         IF (POLPLT.EQ.4) POLLAB(1) = 'RL/LL'
         IF (POLPLT.EQ.5) POLLAB(1) = 'LR/LL'
         IF (POLPLT.EQ.6) POLLAB(1) = 'RR/LL'
         END IF
C                                       Init vis file for reading
      MSGSAV = MSGSUP
      IF (XSOLIN.NE.0.0) MSGSUP = 32000
      IF (NCOUNT.GE.1) THEN
         CALL FILL (MAXANT, 0, ANTENS)
         ANTENS(1) = XA1(ICURNT)
         ANTENS(2) = XA2(ICURNT)
         END IF
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      MSGSUP = MSGSAV
      IF (IRET.EQ.0) GO TO 30
      IF (IRET.EQ.-1) THEN
         MSGTXT = 'NO DATA FOUND WITH SPECIFIED PARAMETERS: CHECK' //
     *      ' INPUTS'
         GO TO 990
      ELSE
         GO TO 999
         END IF
C                                       Fill in baseline names
 30   IF ((NANTSL.EQ.0) .OR. (NANTSL.GT.2)) THEN
         ANTNAM(1)(1:1) = '*'
         ANTNAM(2)(1:1) = '*'
      ELSE IF (NANTSL.EQ.1) THEN
         ANTNAM(1) = STNS(XA1(ICURNT))
         TELNUM(1) = XA1(ICURNT)
         ANTNAM(2)(1:1) = '*'
      ELSE
         ANTNAM(1) = STNS(XA1(ICURNT))
         TELNUM(1) = XA1(ICURNT)
         ANTNAM(2) = STNS(XA2(ICURNT))
         TELNUM(2) = XA2(ICURNT)
         END IF
      IF (NCOUNT.GE.1) THEN
         ANTNAM(1) = STNS(XA1(ICURNT))
         TELNUM(1) = XA1(ICURNT)
         ANTNAM(2) = STNS(XA2(ICURNT))
         TELNUM(2) = XA2(ICURNT)
         IF (TELNUM(1).EQ.TELNUM(2)) LVIST = 0
C                                       Guard against user optimism
         IF ((TELNUM(1).EQ.0) .OR. (TELNUM(2).EQ.0)) THEN
            JERR = 10
            GO TO 980
            END IF
      ELSE
         IF (DOACOR) LVIST = 0
         END IF
C                                       Position shift calculations
C                                       Main averaging loop
      UVSCAL = FREQ / UVFREQ
      XCOUNT = 0
C
      I = 4 * MAXIF
      CALL RFILL (I, 0.0, SUMWT)
      ITIME = 0
      DO 300 I = 1,NVIS
C
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.LT.0) GO TO 400
         IF (IRET.NE.0) GO TO 999
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       Modify VIS as appropriate
C                                       based on POLPLT.
C                                       Modify VIS as appropriate
         IF (POLPLT.GT.0) THEN
            CALL POLVIS (VIS, POLPLT, IPOLPT)
C                                       exclude the visibilities which
C                                       do not have both required
C                                       polarizations
            WT = 0
            DO 45 IFNO = 1,LNUMIF
               INDEX = 1 + (IFNO-1) * INCIF + (IPOLPT*INCS)
               DO 40 LOOPF = 1,NUMFRQ
                  INP = INDEX + (LOOPF-1+PBCH-BCHAN) * INCF
                  WT = MAX (WT, VIS(INP+2))
 40               CONTINUE
 45            CONTINUE
            IF (WT.LE.0) GO TO 300
            END IF
C                                       Determine antenna numbers
         IF (ILOCB.GE.0) THEN
            IANT1 = RPARM(ILOCB+1) / 256.0 + 0.1
            IANT2 = RPARM(ILOCB+1) - 256 * IANT1 + 0.1
         ELSE
            IANT1 = RPARM(ILOCA1+1) + 0.1
            IANT2 = RPARM(ILOCA2+1) + 0.1
            END IF
C                                       Baseline selection
         IF (NBASE.GT.0) THEN
            IF (NCOUNT.GE.1) THEN
               IF ((IANT1.EQ.XA1(ICURNT)) .AND.
     *            (IANT2.EQ.XA2(ICURNT))) GO TO 60
               GO TO 300
               END IF
            DO 50 J = 1, NBASE
               IF ((IANT1.EQ.XA1(J)) .AND. (IANT2.EQ.XA2(J)))
     *            GO TO 60
 50            CONTINUE
            GO TO 300
            END IF
C
   60    CONTINUE
C                                       Time index for buffer.
         XXTIME = (RPARM(ILOCT+1) - STARTU) / PREAVG + 1
         ITIME = IROUND (XXTIME)
         IF (ITIME.GT.NTIMEP) GO TO 300
         IF (DIVCH0) THEN
            CALL DIVCHZ (VIS, IRET)
            IF ((IRET.LT.0) .AND. (NPRI.LT.11)) THEN
               WRITE (MSGTXT,1000) IANT1, IANT2, RPARM(ILOCT+1)
               CALL MSGWRT (6)
               NPRI = NPRI + 1
               END IF
            END IF
C                                       Average.
         AVTIME = AVTIME + RPARM(ILOCT+1)
         XCOUNT = XCOUNT + 1
         SRCDUN = CURSOU
C                                       Fill in values in DSOU.INC
         IF (SRCDUN.NE.OLDSRC) THEN
            CALL GETSOU (SRCDUN,IUDISK,IUCNO,CATUV,LUN3,IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) IRET
               GO TO 990
               END IF
            OLDSRC = SRCDUN
            IF (DOSHFT) THEN
               RAEPO = RAEPO * RAD2DG
               DECEPO = DECEPO * RAD2DG
               RA0 = RA
               DEC0 = DEC
               RA = RA0
               IF (COS(DG2RAD*DEC0).NE.0.0D0) THEN
                  RA = RA0 + SHIFT(1) / COS (DG2RAD * DEC0) / 3600.0D0
                  RAEPO = RAEPO + SHIFT(1)/(COS(DG2RAD*DEC0) * 3600.0D0)
                  END IF
               DEC = DEC0 + SHIFT(2) / 3600.0D0
               DECEPO = DECEPO + SHIFT(2) / 3600.
C                                       Get RA and Dec offsets from
C                                       uv data reference position.
               DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
               DYC = COS ((DEC0) * DG2RAD) * SIN (DEC * DG2RAD) -
     *            SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *            COS (DG2RAD * (RA-RA0))
               DZC = SIN (DG2RAD * DEC0) * SIN (DG2RAD * DEC) +
     *            COS (DG2RAD * DEC0) * COS (DG2RAD * DEC) *
     *            COS (DG2RAD * (RA-RA0)) - 1.0D0
               DXC = DXC * 6.283185308
               DYC = DYC * 6.283185308
               DZC = DZC * 6.283185308
               RFREQ = CATD(KDCRV+JLOCF)
               END IF
            END IF
C                                       Do the accumulation
         DO 100 IPOL = 1,POLNUM
         DO 100 IFNO = 1,LNUMIF
C                                       LK September 11, 2010
            IF (DOSHFT) THEN
               TRUEF = 1.0D0 + FOFF(IFNO+PBIF-1) / RFREQ
               DFREQ = FINC(IFNO+PBIF-1) / RFREQ
               END IF
            IF (POLPLT.GT.0) THEN
               INDEX = 1 + (IFNO-1) * INCIF + (IPOLPT*INCS)
            ELSE
               INDEX = 1 + (IFNO-1) * INCIF + (IPOL-1)*INCS
               END IF
            LINWT = 0.0
            INX = (IFNO - 1 + (IPOL-1)*LNUMIF) * NTRANS + ITIME
C
            IF (INX .GT. MXTIFP) THEN
               IRET = 1
               WRITE (MSGTXT,1200) INX, MXTIFP
               CALL MSGWRT (8)
               WRITE (MSGTXT,1300)
               GO TO 990
               END IF
            DO 90 LOOPF = 1,NUMFRQ
               INP = INDEX + (LOOPF-1+PBCH-BCHAN) * INCF
               WT = MAX (0.0, VIS(INP+2))
               IF (WT.GT.0.0) THEN
                  IF (DOSHFT) THEN
                     FRSCL = TRUEF + DFREQ +
     *                  (LOOPF + PBCH - BCHAN - CATR(KRCRP+JLOCF))
                     XX = FRSCL * (RPARM(ILOCU+1) * DXC +
     *                  RPARM(ILOCV+1) * DYC + RPARM(ILOCW+1) * DZC)
                     ZZ = CMPLX (COS(XX), -SIN(XX))
                     VS = CMPLX (VIS(INP), VIS(INP+1)) * ZZ
                     VIS(INP) = REAL(VS)
                     VIS(INP+1) = AIMAG(VS)
                     END IF
C                                       Vector section
                  WTS(INX) = WTS(INX) + WT
                  BUFF2(1,INX) = BUFF2(1,INX) + VIS(INP)   * WT
                  BUFF2(2,INX) = BUFF2(2,INX) + VIS(INP+1) * WT
                  LINWT = LINWT + WT
                  END IF
 90            CONTINUE
C                                       LINWT is sum of weights of all
C                                       frequencis
            SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + LINWT/NUMFRQ
 100        CONTINUE
 300     CONTINUE
C                                       All selected data have been read
C
C                                       Normalize the output array
 400  CONTINUE
C                                       number of times
      NTIME = ITIME
      INX = 0
      DO 500 IPOL = 1,POLNUM
      DO 500 IFNO = 1,LNUMIF
         LOOPIF = IFNO + PBIF - 1
         IF (SUMWT(IFNO,IPOL).LE.0.0) THEN
            IF (ANTNAM(1).NE.ANTNAM(2)) THEN
               WRITE (MSGTXT,1400) LOOPIF
               CALL MSGWRT (6)
               WRITE (MSGTXT,1500) ANTNAM(1), ANTNAM(2)
               CALL MSGWRT (6)
               END IF
            GO TO 500
         ELSE
            NMISS = 0
            DO 450 ITIME = 1,NTRANS
               INX = (IFNO - 1 + (IPOL-1)*LNUMIF) * NTRANS +ITIME
               XNORM = 0.0
               IF (WTS(INX) .GT. 0.0) XNORM = 1.0 / WTS(INX)
C                                       Count the gaps in the data.
               IF ((XNORM.EQ.0.0).AND.(ITIME.LE.NTIMEP)) NMISS=NMISS+1
C                                       store The array of the
C                                       visibililties (in time) for
C                                       the given POL. and IF
               IF (XNORM.GT.0.0) THEN
                  BUFF2(1,INX) = BUFF2(1,INX) * XNORM
                  BUFF2(2,INX) = BUFF2(2,INX) * XNORM
C                                       phase or amp only
                  IF (LVIST.GT.0) THEN
                     AMP = SQRT (BUFF2(1,INX)*BUFF2(1,INX) +
     *                  BUFF2(2,INX)*BUFF2(2,INX))
                     IF ((LVIST.EQ.1) .AND. (AMP.GT.0.0)) THEN
                        BUFF2(1,INX) = BUFF2(1,INX) / AMP
                        BUFF2(2,INX) = BUFF2(2,INX) / AMP
                     ELSE
                        BUFF2(1,INX) = AMP
                        BUFF2(2,INX) = 0.0
                        END IF
                     END IF
C                                       flag indeterminate data
               ELSE
                  BUFF2(1,INX) = FBLANK
                  BUFF2(2,INX) = FBLANK
                  END IF
 450           CONTINUE
C                                       Transform to fringe rate
C                                       spectra if requested.
            IF (WFFTR) THEN
               INX = (IFNO - 1 + (IPOL-1)*LNUMIF) * NTRANS + 1
               CALL FRSPEC (NTRANS, NTIMEP, NMISS, BUFF2(1,INX),
     *            -1, XWORK)
               ENDIF
            END IF
 500     CONTINUE
C                                          Do we have any?
      XCOUNT = XCOUNT - NOMIT
      IF (XCOUNT.LT.1) THEN
         WRITE (MSGTXT,1600)
         JERR = 5
         IF ((NCOUNT.GE.1) .AND. (ICURNT.NE.NBASE)) THEN
            IRET = 0
            JERR = 0
            END IF
         GO TO 980
         END IF
C                                       Finish up
      BLNKBF = .FALSE.
      WRITE (MSGTXT,1700) XCOUNT, NVIS
      CALL MSGWRT (4)
      WRITE (MSGTXT,1500) ANTNAM(1), ANTNAM(2)
      CALL MSGWRT (4)
      IF (XSOLIN.NE.0.0) THEN
         WRITE (MSGTXT,1800) CTS, ITS, CTE, ITE
         CALL MSGWRT (3)
         END IF
      IF (NOMIT.GT.0) THEN
         WRITE (MSGTXT,1900) NOMIT
         CALL MSGWRT (3)
         END IF
C                                       Fill in values in DSOU.INC
      CALL GETSOU (SRCDUN, IUDISK, IUCNO, CATUV, LUN3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Fill in values for output
C                                       file labeling
      CALL H2CHR (8, 1, CATH(KHDOB), EXPDAT)
      SRCOBS = SNAME
      NCHAN = CATBLK(KINAX+JLOCF)
      CTIME = AVTIME / XCOUNT
      AVWGHT = SUMWT(1,1) / NVIS
      TBW = ABS (CATR(KRCIC+JLOCF)) / 1000.0
      IF (RESTFQ(1).LE.100.) RESTFQ(1) = CATD(KDRST)
      MFREQ = RESTFQ(1) / 1.0E6
      DTEMP = RAEPO * RAD2DG
      CALL COORDD (1, DTEMP, CHSIGN, HM, RASEC)
      RAHR = HM(1)
      RAMIN = HM(2)
      DTEMP = DECEPO * RAD2DG
      CALL COORDD (2, DTEMP, CHSIGN, HM, DECSEC)
      DECDEG = HM(1)
      DECMIN = HM(2)
      IF (CHSIGN.EQ.'-') DECDEG = -DECDEG
C                                       Close files
 980  CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IF (IRET.EQ.0) IRET = JERR
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Channel 0 low: ants:',2I3,' time',1PE13.6)
 1100 FORMAT ('FRRAUV: GETSOU RETURNED ERROR',I5)
 1200 FORMAT ('Number of buffer points',I7,' exceeds the maximum',I8)
 1300 FORMAT ('Increase the preaverage time or decrease SOLINT/TIMRANG')
 1400 FORMAT ('FRRAUV: Warning, No valid data for IF: ',I2)
 1500 FORMAT ('FRRAUV: on baseline(s) ',A8,' - ',A8)
 1600 FORMAT ('FRRAUV: NO VISIBILITIES SELECTED - CHECK INPUT ',
     *        'PARMS OR SORT ORDER')
 1700 FORMAT ('Averaged',I10,' visibilities from total data set of',I9)
 1800 FORMAT ('Covering timerange ',A,I2.2,'/',2(I2.2,':'),I2.2,' - ',
     *   A,I2.2,'/',2(I2.2,':'),I2.2)
 1900 FORMAT ('Rejected',I5,' visibilities due to low channel 0',
     *   ' amplitude')
      END
      SUBROUTINE DIVCHZ (VIS, IRET)
C-----------------------------------------------------------------------
C   DIVCHZ forms the so-called channel 0 (centre 75% of band) from
C   the visibility data and then the spectral data is divided by the
C   channel 0 data.
C   Input - common
C      NUMFRQ        I      # visibility channels in the spectrum
C   Input/Output
C      VIS(3,*)      R      On input the visibility spectrum, on
C                           output the corrected (ie divided spectrum)
C   Input from common:
C      INCF   I     Increment in freq. of data from UVGET
C      INCIF  I     Increment in IF of data from UVGET
C      INCS   I     Increment in Stokes' of data from UVGET
C   Output:
C      IRET   I     -1 => channel 0 sum too low
C-----------------------------------------------------------------------
      REAL      VIS(*)
      INTEGER   IRET
C
      INTEGER   FCHAN, LCHAN, NUMCHZ, I, LOOPS, LOOPIF, INDEX, INP,
     *   IROUND
      REAL      SUMRE, SUMWT, SUMIM, XNORM, TEMP, DENOM, WT
      LOGICAL   FLAGD
      INCLUDE 'FRPLT.INC'
      REAL    CHZ(2,MAXIF,4)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Determine channel 0 range
      IRET = 0
      BPARM(4) = MAX (0.0, BPARM(4))
      I = ECHAN - BCHAN + 1
      FCHAN = IROUND(BPARM(2))
      IF (FCHAN.GT.I) FCHAN = 0
      LCHAN = IROUND(BPARM(3))
      IF (LCHAN.GT.I) LCHAN = 0
      IF (FCHAN.LE.0) FCHAN = (I+1) / 8
      IF (LCHAN.LE.0) LCHAN = (((I+1) * 7) / 8) - 1
      NUMCHZ = LCHAN - FCHAN + 1
      IF (NUMCHZ.LT.2) THEN
         FCHAN = (I+1) / 8
         LCHAN = (((I+1) * 7) / 8) - 1
         NUMCHZ = LCHAN - FCHAN + 1
         END IF
C                                       Calculate channel 0 visibility
      DO 30 LOOPS = 1,NCOR
         DO 20 LOOPIF = PBIF,PEIF
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-PBIF) * INCIF
            SUMRE = 0.0
            SUMIM = 0.0
            SUMWT = 0.0
            DO 10 I = FCHAN,LCHAN
               INP = INDEX + (I-BCHAN) * INCF
               WT = VIS(INP+2)
               IF (WT.LE.0.0) WT = 0.0
               SUMRE = SUMRE + VIS(INP) * WT
               SUMIM = SUMIM + VIS(INP+1) * WT
               SUMWT = SUMWT + WT
 10            CONTINUE
            XNORM = 1.0
            IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
            CHZ(1,LOOPIF,LOOPS) = SUMRE * XNORM
            CHZ(2,LOOPIF,LOOPS) = SUMIM * XNORM
 20         CONTINUE
 30      CONTINUE
C                                       Do the division
      DO 300 LOOPS = 1,NCOR
         DO 200 LOOPIF = PBIF,PEIF
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-PBIF) * INCIF
            DENOM = CHZ(1,LOOPIF,LOOPS) * CHZ(1,LOOPIF,LOOPS) +
     *              CHZ(2,LOOPIF,LOOPS) * CHZ(2,LOOPIF,LOOPS)
            FLAGD = .FALSE.
            IF (DENOM.LE.BPARM(4)) THEN
               DENOM = 1.0
               FLAGD = .TRUE.
               NOMIT = NOMIT + 1
               IRET = -1
               END IF
            DO 100 I = PBCH,PECH
               INP = INDEX + (I-BCHAN) * INCF
               TEMP = VIS(INP)
               VIS(INP)   = (CHZ(1,LOOPIF,LOOPS)*TEMP +
     *                       CHZ(2,LOOPIF,LOOPS)*VIS(INP+1)) / DENOM
               VIS(INP+1) = (CHZ(1,LOOPIF,LOOPS)*VIS(INP+1) -
     *                       CHZ(2,LOOPIF,LOOPS)*TEMP) / DENOM
               IF (FLAGD) VIS(INP+2) = -1
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLCREA (NP, IVER, IERR)
C-----------------------------------------------------------------------
C   This routine will create and open a plot file, put it in the map
C   header and write the first record into the plot file.
C   PLCREA first opens the map file and marks it 'write'
C   Inputs:
C      NP       I       Number of floating point words in parameter
C                       list received from AIPS.
C   Inputs from Common:
C      RPARM    R(NP)   AIPS parameters.
C      IGTYPE   I       Plot file type: 1 misc., 2 CNTR, 3 GREYS,
C                       4 PROFL, 5 SL2PL, 6 PCNTR, 7 IMEAN (hist),
C                       8 UVPLT, 9 GNPLT, 10 VBPLT, 11 PFPLn,
C                       12 => GAPLT, 13 => PLCUB, 14 => IMVIM,
C                       15 => TAPLT, 16 => FRRAM, 17 => SNPLT
C                       Use 1 unless your inputs match those of these
C                       tasks - or take a new number, but
C                       AIPSUB:AU8A will need to know about it too.
C      DTYP     C*2     ASCII data type of input e.g. 'UV', 'MA'
C      DISKIN   I       Disc volume number
C      NAMEIN   R(3)    Name of input file
C      CLAIN    R(2)    Class of input file
C      SEQIN    I       Sequence number of input file
C   Output:
C      IVER     I       Version number of plot file to generate
C      IERR     I       Error code. two digit, first digit indicates
C                       subroutine: 1: MAPOPN, 2: MADDEX, 3: ZPHFIL,
C                       4: GINIT, second digit indicates error code of
C                       that subroutine.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, DTYP*2, STAT*4
      INTEGER   NP, IERR, IGTYPE, IVER, IUSER, IWBLK(256), IGSIZE,
     *   IER, IROUND
      LOGICAL   SAVE
      REAL      APSAVE(6)
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL3.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IGSIZE /0/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
      CALL RCOPY (4, APARM(3), APSAVE(1))
      CALL RCOPY (2, BPARM(5), APSAVE(5))
      APARM(3) = MINAMP
      APARM(4) = MAXAMP
      APARM(5) = MINPHS
      APARM(6) = MAXPHS
      XBIF = STRTIF
      XEIF = STOPIF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XSMOTH(1) = SMOOTH(1)
      XSMOTH(2) = SMOOTH(2)
      XSMOTH(3) = SMOOTH(3)
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = MAX (0.0, TSTART)
      IF (TEND.LT.1000.) XTIME(5) = TEND
      IGTYPE = 16
      IUSER = IROUND (USERID)
      DTYP = 'UV'
C                                       Open map file
      PLMAP = 17
      STAT= 'HDWR'
      IF (DOTV) STAT = 'READ'
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
 30   CALL GINIT (DISKIN, SLOT, PHNAME, IGSIZE, IGTYPE, NP, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND,
     *   IERR)
      CALL RCOPY (4, APSAVE(1), APARM(3))
      CALL RCOPY (2, APSAVE(5), BPARM(5))
      IF (IERR.EQ.0) GO TO 999
         GPHIND = 0
         IERR = IERR + 40
C
 980  IF (.NOT.DOTV) CALL DELEXT ('PL', DISKIN, SLOT, 'WRIT', CATBLK,
     *   IWBLK, IVER, IER)
C
 999  RETURN
      END
      SUBROUTINE FRRALB (IFNO, IERR)
C-----------------------------------------------------------------------
C   FRRALB provides the global labels surrounding the plot produced by
C   FRRAPL.
C   Inputs:
C      IFNO        I       IF number to plot
C   Input via commons
C      UVRNG       R(2)    UV min. and max. selected.
C      TIMRNG      R(8)    Time range selected.
C      ANTENS      R(50)   Antennas selected
C      STNS        R(*)    Antenna names
C   Outputs:
C      IERR        I       Error code
C-----------------------------------------------------------------------
      CHARACTER TEXT*132, ATIME*8, CTEMP*8, ADATE*12,
     *   CTEMP1*18, FRRATE*36, VISI*36,
     *   CT1*1, CT2*1
      HOLLERITH CATH(256)
      INTEGER   IERR, INCHAR, ID(3), IT(3), IANGLE, IFNO, IT1(4),
     *   IT2(4), ITRIM
      REAL      DX, DY, CATR(256), PBW, T1, T2
      DOUBLE PRECISION PFREQ
      LOGICAL MULTIF
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER BNDCOD(MAXIF)*8
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA
     *     FRRATE /'Fringe rate spectrum' /,
     *     VISI/'Visibility vs time function'/
C-----------------------------------------------------------------------
      IERR = 0
      MULTIF = PEIF.NE.PBIF
      MULTIF = MULTIF .AND. (NCOUNT.GE.1)
      MULTIF = MULTIF .AND. (.NOT.DOCHIF)
C                                       Date/time/version
      DX = 0.0
      DY = CHOUT(4) - 1.5
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (TEXT,1000) PVER, ADATE, ATIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
         END IF
C                                       File name label & source name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (MULTI) THEN
            IF (SCANAV) THEN
               IF (SRCOBS(1:1).EQ.'-') THEN
                  TEXT = SRCOBS(2:9)
               ELSE
                  TEXT = SRCOBS(1:8)
                  END IF
            ELSE
               IF (SAUCE(1:1).EQ.'-') THEN
                  TEXT = SAUCE(2:9)
               ELSE
                  TEXT = SAUCE(1:8)
                  END IF
               END IF
         ELSE
            CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
            TEXT = CTEMP
            END IF
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         INCHAR = INCHAR + 1
         IF (INCHAR.GT.1) THEN
            TEXT(INCHAR:) = '___'
            INCHAR = INCHAR + 3
            END IF
         CALL H2CHR (18, 1, CATH(KHIMN), CTEMP1)
         CALL NAMEST (CTEMP1, CATBLK(KIIMS), TEXT(INCHAR:), INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
C                                       Insert FQ info
         IF (DOFQSL) THEN
            CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATUV, 45, PFQFRQ,
     *         PFQTBW, PFQCHW, PFQSID, BNDCOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR
               GO TO 990
               END IF
            PFREQ = PFQFRQ(IFNO) / 1.0D9
            PBW = PFQTBW(IFNO) / 1.0E6
            WRITE (TEXT,1200) PFREQ, PBW
            CALL REFRMT (TEXT, '_', INCHAR)
C            CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
C            IF (IERR.NE.0) GO TO 999
C            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
C            IF (IERR.NE.0) GO TO 999
C            DY = DY - 1.333
            TEXT(INCHAR:) = '___'
            INCHAR = INCHAR + 3
         ELSE
            INCHAR = 1
            END IF
C                                       Calibration information
         IF (DOCAL) THEN
            IF (MULTI) THEN
               WRITE (TEXT(INCHAR:),1300) CLUSE
            ELSE
               WRITE (TEXT(INCHAR:),1400) CLUSE
               END IF
         ELSE
            TEXT(INCHAR:) = 'No calibration applied__'
            END IF
         INCHAR = ITRIM (TEXT)
         IF (DOBAND.GT.0) THEN
            IF (DOCAL) THEN
               WRITE (TEXT(INCHAR:),1500) BPVER, DOBAND
            ELSE
               WRITE (TEXT(INCHAR:),1600) BPVER, DOBAND
               END IF
          ELSE IF (DOBAND.LE.0) THEN
             IF (DOCAL) THEN
                TEXT(INCHAR:) = 'but no bandpass applied'
             ELSE
                TEXT(INCHAR:) = 'and no bandpass applied'
                END IF
             END IF
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
      DY = -2.833
      IF (LTYPE.GT.2) DY = DY - 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Axis labels for NCOUNT > 0
         IF (NCOUNT.GE.1) THEN
            TEXT = 'Lower frame: ' // APREF(1) // ALABEL(1)
            IF (ALABEL(2).NE.' ') THEN
               CALL REFRMT (TEXT, '_', INCHAR)
               TEXT(INCHAR+1:) = '___'
               INCHAR = INCHAR + 4
               TEXT(INCHAR:) = 'Top frame: ' // APREF(2) // ALABEL(2)
               END IF
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DX = 0.0
            IANGLE = 0
            CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DY = DY - 1.333
            END IF
C                                       Title line
         IF (WFFTR) THEN
            TEXT = FRRATE
         ELSE
            TEXT = VISI
            END IF
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         TEXT(INCHAR+1:) = '___'
         INCHAR = INCHAR+5
C
         IF (NANTSL.EQ.2) WRITE (TEXT(INCHAR:),1700) STNS(ANTENS(1)),
     *         ANTENS(1), STNS(ANTENS(2)), ANTENS(2)
         IF (NANTSL.EQ.0) TEXT(INCHAR:) = 'Baseline: * - *'
         IF ((NANTSL.GT.2) .AND. (NCOUNT.LT.1))
     *         TEXT(INCHAR:) = 'Several baselines averaged'
         IF ((NANTSL.GT.2) .AND. (NCOUNT.GE.1))
     *      TEXT(INCHAR:) = 'Several baselines displayed'
C
         CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL REFRMT (TEXT, '_', INCHAR)
         IANGLE = 0
         CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DY = DY - 1.333
C                                       Selection comment
C                                       Timerange
         TEXT = ' '
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4)) THEN
            T1 = MAX (0.0, TSTART)
            T2 = MIN (999.0, TEND)
            CALL T2DHMS (T1, CT1, IT1)
            CALL T2DHMS (T2, CT2, IT2)
            WRITE (TEXT,1800) CT1, IT1, CT2, IT2, BCHAN, ECHAN
            CALL REFRMT (TEXT, '_', INCHAR)
            DX = 0.0
            CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DY = DY - 1.333
            END IF
C                                       UV range
         IF ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9)) THEN
            TEXT = ' '
            IF (UVRNG(1).EQ.1.E-9) UVRNG(1) = 0.0
            WRITE (TEXT,1900) UVRNG
            CALL REFRMT (TEXT, '_', INCHAR)
            DX = 0.0
            CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DY = DY - 1.333
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1100 FORMAT ('FRRALB: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
 1200 FORMAT ('Freq = ',F8.4,' GHz, Bw = ',F8.3,' MHz')
 1300 FORMAT ('Calibrated with CL #',I4,'__')
 1400 FORMAT ('Calibrated with SN #',I4,'__')
 1500 FORMAT ('and BP #',I4,' (BP mode ',I2,')')
 1600 FORMAT ('but used BP #',I4,' (BP mode ',I2,')')
 1700 FORMAT ('Baseline:  ',A8,'(',I2.2,')',' - ',A8,'(',I2.2,')')
 1800 FORMAT ('Timerange:',A,I3.2,'/',2(I2.2,':'),I2.2,' to ',A,I3.2,
     *   '/',2(I2.2,':'),I2.2, ' CH:',I6,'-',I5)
 1900 FORMAT ('UVrange:_ ',1PE10.3,' TO ',E10.3,' Klambda')
      END
      SUBROUTINE GMARK (X, Y, DX, DY, IERR)
C-----------------------------------------------------------------------
C   Routine to place a small plus-sign at the exact position specified
C   by X, Y
C   Inputs:
C      X    R   position of cross on x-axis
C      Y    R   position of cross on y-axis
C      DX   R   size of x arm of plus
C      DY   R   size of y arm of plus
C-----------------------------------------------------------------------
      REAL      X, Y, DX, DY, XTEMP, YTEMP
      INTEGER   IERR
      INCLUDE 'FRPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      XTEMP = X - DX
      CALL GPOS (XTEMP, Y, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      XTEMP = X + DX
      CALL GVEC (XTEMP, Y, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      YTEMP = Y - DY
      CALL GPOS (X, YTEMP, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      YTEMP = Y + DY
      CALL GVEC (X, YTEMP, PLTBLK, IERR)
C
 999  RETURN
      END
      SUBROUTINE POLCHK (PPLOT, POLPLT)
C-----------------------------------------------------------------------
C  Routine to decide if the requested polzn plot type is valid.
C  Input:
C    PPLOT      C*8      requested pol operation, can be one of:
C                        ' ', 'RL/RR', 'LR/RR', 'LL/RR'
C                             'RL/LL', 'LR/LL', 'RR/LL'
C  Output:
C    POLPLT     I        Integer plottype corresponding to the above
C                        0,    1,       2,       3,
C                              4,       5,       6
C                        0 => don't perform the polzn operation.
C
C  Uses FNDPOL to determine if the operation is valid.
C-----------------------------------------------------------------------
      CHARACTER PPLOT*8
      INTEGER   POLPLT
C
      INTEGER   KLEN, IERR, ITRIM
      CHARACTER STKCHR*4
C-----------------------------------------------------------------------
      POLPLT = 0
C                                       Check validity of the string
      KLEN = ITRIM(PPLOT)
      IF (KLEN.NE.5) GO TO 999
      IF (PPLOT(3:3).NE.'/') GO TO 999
      IF ((PPLOT(1:2).NE.'RL') .AND. (PPLOT(1:2).NE.'LR') .AND.
     *   (PPLOT(1:2).NE.'RR') .AND. (PPLOT(1:2).NE.'LL')) GO TO 999
      IF ((PPLOT(4:5).NE.'RL') .AND. (PPLOT(4:5).NE.'LR') .AND.
     *   (PPLOT(4:5).NE.'RR') .AND. (PPLOT(4:5).NE.'LL')) GO TO 999
      IF (PPLOT(1:2).EQ.PPLOT(4:5)) GO TO 999
C                                       Set up for FNDPOL
      IF ((PPLOT(1:2).EQ.'RR') .OR. (PPLOT(1:2).EQ.'LL')) THEN
         STKCHR = 'HALF'
      ELSE
         STKCHR = 'FULL'
         END IF
      CALL FNDPOL (STKCHR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up POLPLT
      IF (STKCHR.EQ.'HALF') THEN
         IF (PPLOT(1:2).EQ.'RR') POLPLT = 6
         IF (PPLOT(1:2).EQ.'LL') POLPLT = 3
      ELSE IF (STKCHR.EQ.'FULL') THEN
         IF (PPLOT(4:5).EQ.'RR') THEN
            IF (PPLOT(1:2).EQ.'RL') POLPLT = 1
            IF (PPLOT(1:2).EQ.'LR') POLPLT = 2
         ELSE
            IF (PPLOT(1:2).EQ.'RL') POLPLT = 4
            IF (PPLOT(1:2).EQ.'LR') POLPLT = 5
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE UPDTIM (TIMRNG, XSOLN)
C-----------------------------------------------------------------------
C   Routine to update TIMRNG with XSOLN (minutes), must ensure TIMRNG
C   is within the correct range.
C   Inputs:
C     XSOLN       R       Time update interval (minutes)
C   Input/Output:
C     TIMRNG      R(8)    Start and stop times as needed by UVGET
C-----------------------------------------------------------------------
      REAL    TIMRNG(8), XSOLN
C-----------------------------------------------------------------------
C                                       First do start time
      IF (XSOLN.GE.1.0) TIMRNG(3) = TIMRNG(3) + XSOLN
      IF (XSOLN.LT.1.0) TIMRNG(4) = TIMRNG(4) + XSOLN*60.0
      IF (TIMRNG(4).GE.60.0) THEN
         TIMRNG(4) = TIMRNG(4) - 60.0
         TIMRNG(3) = TIMRNG(3) + 1.0
         END IF
      IF (TIMRNG(3).GE.60.0) THEN
         TIMRNG(3) = TIMRNG(3) - 60.0
         TIMRNG(2) = TIMRNG(2) + 1.0
         END IF
      IF (TIMRNG(2).GE.24.0) THEN
         TIMRNG(2) = TIMRNG(2) - 24.0
         TIMRNG(1) = TIMRNG(1) + 1.0
         END IF
C                                       Then do stop time
      IF (XSOLN.GE.1.0) TIMRNG(7) = TIMRNG(7) + XSOLN
      IF (XSOLN.LT.1.0) TIMRNG(8) = TIMRNG(8) + XSOLN*60.0
      IF (TIMRNG(8).GE.60.0) THEN
         TIMRNG(8) = TIMRNG(8) - 60.0
         TIMRNG(7) = TIMRNG(7) + 1.0
         END IF
      IF (TIMRNG(7).GE.60.0) THEN
         TIMRNG(7) = TIMRNG(7) - 60.0
         TIMRNG(6) = TIMRNG(6) + 1.0
         END IF
      IF (TIMRNG(6).GE.24.0) THEN
         TIMRNG(6) = TIMRNG(6) - 24.0
         TIMRNG(5) = TIMRNG(5) + 1.0
         END IF
C
      RETURN
      END
      SUBROUTINE FILLPL (IFNO, IPOL, IRET)
C-----------------------------------------------------------------------
C   FILLPL loads up the plotting array with the averaged spectrum , does
C   all the scaling.
C   Inputs:
C      IFNO     I    IF number to plot.
C      IPOL     I    Polarization # to plot
C   Inputs from common:
C      BUFF2    R(2,MXTIFP)   Array containing the averaged spectrum.
C   Output:
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      REAL   RD, MAXRHS, MINRHS, RT, AVG2, RMS2, SUM2, SQ2, AMPAVG,
     *   AMPRMS, AMPSUM, AMPSQ
      INTEGER   IRET, IERR, JIF, IADR, I, IFNO, NPLOT, INX,
     *   IPL, IPOL
      INTEGER  LNUMIF
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
C                                       Check sizes
      IF (NUMPOL*NTRANS*NUMIF.GT.MXTIFP) THEN
         IERR = 1
         MSGTXT = 'FILLPL: SPECTRA TOO BIG FOR BUFFERS'
         END IF
C                                       Describe operation
      I = EIF - BIF + 1
      IF ((DOCHIF) .AND. (DOCHPL)) THEN
         WRITE (MSGTXT,1000) NTRANS, I, POLNUM
      ELSE IF (DOCHIF) THEN
         WRITE (MSGTXT,1100) NTRANS, I, POLLAB(IPOL)
      ELSE IF (DOCHPL) THEN
         WRITE (MSGTXT,1200) NTRANS, IFNO, POLNUM
      ELSE
         WRITE (MSGTXT,1300) NTRANS, IFNO, POLLAB(IPOL)
         END IF
      CALL MSGWRT (3)
      RD = 180.0 / (4.0 * ATAN(1.0D0))
C                                       Initial scaling
      IF (SELFSA) THEN
         MAXAMP = -1.E10
         MINAMP = 1.E10
      ELSE
         MINAMP = APARM(3)
         MAXAMP = APARM(4)
         END IF
      IF (SELFSP) THEN
         IF (CODTYP.LT.4) THEN
            MAXPHS = -180.0
            MINPHS = 180.0
            MAXRHS = -180.0
            MINRHS = 180.0
         ELSE
            MAXPHS = -1.E10
            MINPHS = 1.E10
            END IF
      ELSE
         MINPHS = APARM(5)
         MAXPHS = APARM(6)
         END IF
C                                       Form ampl and phase arrays
      NPLOT = 0
      AMPSUM = 0.0
      AMPSQ = 0.0
      SUM2 = 0.0
      SQ2 = 0.0
      NCTOT = (STOPIF - STRTIF + 1) * NTRANS * (STOPOL - STRPOL + 1)
      NPNL = NCTOT / NTRANS
      CALL RFILL (NCTOT, 0.0, AMP)
      CALL RFILL (NCTOT, 0.0, PHASE)
      LNUMIF = EIF - BIF + 1
C
      DO 15 IPL = STRPOL,STOPOL
      DO 15 JIF = STRTIF,STOPIF
         DO 10 I = 1,NTRANS
            INX = (JIF - BIF + (IPL-1)*LNUMIF) * NTRANS + I
            IADR = (JIF - STRTIF + (IPL-STRPOL)*(STOPIF-STRTIF+1)) *
     *         NTRANS + I
C
            IF ((BUFF2(1,INX).EQ.FBLANK) .AND. (BUFF2(2,INX).EQ.FBLANK))
     *         THEN
               AMP(IADR) = FBLANK
               PHASE(IADR) = FBLANK
            ELSE
               NPLOT = NPLOT + 1
C
               IF (CODTYP.GT.3) THEN
                  AMP(IADR) = BUFF2(1,INX)
               ELSE
                  AMP(IADR) = SQRT (BUFF2(1,INX)*BUFF2(1,INX) +
     *               BUFF2(2,INX)*BUFF2(2,INX))
                  END IF
               AMPSUM = AMPSUM + AMP(IADR)
               AMPSQ = AMPSQ + AMP(IADR) * AMP(IADR)
               IF (SELFSA) THEN
                  IF (AMP(IADR).GT.MAXAMP) MAXAMP = AMP(IADR)
                  IF (AMP(IADR).LT.MINAMP) MINAMP = AMP(IADR)
               ELSE
                  IF (AMP(IADR).GT.MAXAMP) AMP(IADR) = MAXAMP
                  IF (AMP(IADR).LT.MINAMP) AMP(IADR) = MINAMP
                  END IF
               IF ((BUFF2(1,INX).EQ.0.) .AND. (BUFF2(2,INX).EQ.0.))
     *            THEN
                  PHASE(IADR) = 0.
               ELSE IF (CODTYP.GT.3) THEN
                  PHASE(IADR) = BUFF2(2,INX)
               ELSE
                  PHASE(IADR) = ATAN2 (BUFF2(2,INX), BUFF2(1,INX)) * RD
                  END IF
               SUM2 = SUM2 + PHASE(IADR)
               SQ2 = SQ2 + PHASE(IADR) * PHASE(IADR)
               IF (SELFSP) THEN
                  RT = PHASE(IADR)
                  IF (RT.GT.MAXPHS) MAXPHS = RT
                  IF (RT.LT.MINPHS) MINPHS = RT
                  IF (CODTYP.LT.4) THEN
                     IF (RT.LT.0.0) RT = RT + 360.
                     IF (RT.GT.MAXRHS) MAXRHS = RT
                     IF (RT.LT.MINRHS) MINRHS = RT
                     END IF
               ELSE
                  IF ((CODTYP.LT.4) .AND. ((PHASE(IADR).GT.MAXPHS) .OR.
     *               (PHASE(IADR).LT.MINPHS))) THEN
                     RT = PHASE(IADR) + 360.
                     IF ((RT.LE.MAXPHS) .AND. (RT.GE.MINPHS)) THEN
                        PHASE(IADR) = RT
                     ELSE
                        RT = PHASE(IADR) - 360.
                        IF ((RT.LE.MAXPHS) .AND. (RT.GE.MINPHS))
     *                     PHASE(IADR) = RT
                        END IF
                     END IF
                  IF (PHASE(IADR).GT.MAXPHS) PHASE(IADR) = MAXPHS
                  IF (PHASE(IADR).LT.MINPHS) PHASE(IADR) = MINPHS
                  END IF
               END IF
 10         CONTINUE
 15      CONTINUE
C                                       fix +- 180 problem
      IF ((SELFSP) .AND. (MAXPHS-MINPHS.GT.MAXRHS-MINRHS).AND.
     *   (CODTYP.LT.4)) THEN
         DO 40 I = 1,NCTOT
            IF (PHASE(I).LT.0.0) PHASE(I) = PHASE(I) + 360.
 40         CONTINUE
         MAXPHS = MAXRHS
         MINPHS = MINRHS
         END IF
C                                       Non blank plot file?
      IF (NPLOT.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT,1400)
         GO TO 990
         END IF
C                                       Print mean, rms
      IF (NPLOT.GT.0) THEN
         AMPAVG = AMPSUM / NPLOT
         AMPRMS = AMPSQ / NPLOT - AMPAVG * AMPAVG
         IF (AMPRMS.GT.0.0) AMPRMS = SQRT (AMPRMS)
         IF (CODTYP.LE.3) THEN
            WRITE (MSGTXT,1500) 'Amplitude', AMPAVG, AMPRMS
         ELSE
            WRITE (MSGTXT,1500) 'Real part', AMPAVG, AMPRMS
            END IF
         CALL MSGWRT (4)
         AVG2 = SUM2 / NPLOT
         RMS2 = SQ2 / NPLOT - AVG2 * AVG2
         IF (RMS2.GT.0.0) RMS2 = SQRT (RMS2)
         IF (CODTYP.LE.3) THEN
            WRITE (MSGTXT,1500) 'Phase', AVG2, RMS2
         ELSE
            WRITE (MSGTXT,1500) 'Imaginary', AVG2, RMS2
            END IF
         CALL MSGWRT (4)
         ENDIF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Will plot ',I5,' NTRANSs * ',I2,' IFs',I2,
     *   ' polarizations')
 1100 FORMAT ('Will plot ',I5,' NTRANSss * ',I2,' IFs',
     *   ' polarization ',A)
 1200 FORMAT ('Will plot ',I5,' fringe rates from IF# ',I2,
     *   ' * polarizations',I2)
 1300 FORMAT ('Will plot ',I5,' fringe rates from IF# ',I2,
     *   ' polarization ',A)
 1400 FORMAT ('FILLPL: PLOT BUFFER ALL BLANK - NO PLOT CREATED')
 1500 FORMAT ('FILLPL: ',A,' mean:',1PE12.4,'  rms:',1PE12.4)
      END
      SUBROUTINE FRRAPL (NPARM, IFNO, IRET)
C-----------------------------------------------------------------------
C   FRRAPL does the plotting of the averaged spectrum plus all
C   the scaling and controls the labelling.
C   Inputs:
C      BUFF2    R(2,MXTIFP)   Array containing the averaged spectrum.
C      NPARM    I    Number of R   parms read form AIPS
C      IFNO     I    > 0 last plot, < 0 not last.
C   Output:
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LINE*132, PHNAME*48
      REAL      DX, DY, SCALX, SCALP, SCALA, OFX, OFY, X, Y, CATR(256),
     *   PTEMP(4), TITSEC, REFPIX, XRN, XFAC, XINC, FRRATE
      DOUBLE PRECISION CATD(128), SFREQ, FREINC
      INTEGER   IRET, NPARM, DEPTH(5), IERR, BUFFI(256), JIF, I, IFNO,
     *   LUNPR, PFIND, ITIME, TIT(3), NCH, ITRIM, JP, INP, J, LIF,
     *   IPNL, IO, IIP
      LOGICAL   T, F, WPLOT, DOZERO, BLNK
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      REAL PDATA(MXTIFP,2)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (CATUV, CATR, CATD)
      EQUIVALENCE (PDATA(1,1), AMP),    (PDATA(1,2), PHASE)
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      NXP = 1
      NYP = 1
      XFAC = ABS (FACTOR)
      IF (XFAC.GE.100.0) XFAC = XFAC - 100.0
      IF (XFAC.LT.0.4) XFAC = 1.0
C                                       Scaling
      AMPRNG = ABS (MAXAMP - MINAMP)
      PHSRNG = ABS (MAXPHS - MINPHS)
C                                       Amplitude range
      IF (AMPRNG.EQ.0.0) THEN
         MINAMP = 0.0
         MAXAMP = MAXAMP * 1.1
      ELSE IF (AMPRNG.NE.0) THEN
         MINAMP = MINAMP - 0.04 * AMPRNG
         MAXAMP = MAXAMP + 0.04 * AMPRNG
         IF ((MINAMP.GT.0.0) .AND. (MINAMP.LT.0.25*MAXAMP) .AND.
     *      (SELFSA)) MINAMP = 0.0
         END IF
C                                       Phase range
      IF (PHSRNG.EQ.0.0) THEN
         MINPHS = -180.0
         MAXPHS = 180.0
      ELSE IF (PHSRNG.NE.0.0) THEN
         MINPHS = MINPHS - 0.06 * PHSRNG
         MAXPHS = MAXPHS + 0.06 * PHSRNG
         END IF
      AMPRNG = ABS (MAXAMP - MINAMP)
      PHSRNG = ABS (MAXPHS - MINPHS)
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      LINT = 1000.0
      IF (CODTYP.EQ.1) LINT = 700.
      IF (CODTYP.EQ.4) LINT = 500.
C                                       Ensure correct scaling parms
C                                       written to PL file, then
C                                       create the PL file
      CALL RCOPY (4, APARM(3), PTEMP(1))
      CALL PLCREA (NPARM, PVER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      IF (DOTV) THEN
         XYRATI = (WINDTV(3) - WINDTV(1) + 1.0) /
     *      (WINDTV(4) - WINDTV(2) + 1.0)
      ELSE
         XYRATI = 9 / 5.5
         WRITE (MSGTXT,1100) PVER
         CALL MSGWRT (5)
         END IF
      CALL RCOPY (4, PTEMP(1), APARM(3))
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL RFILL (4, 0.5, CHOUT)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
C                                       ????????
      IF (LTYPE.GT.2) THEN
         NCOUNT = -1 - ABS(NCOUNT)
         CALL LABLAX (.TRUE., IRET)
         NCOUNT = -NCOUNT - 1
         CALL CHNTIC (BLC, TRC, INP)
         IF (CODTYP.EQ.1) INP = MAX (INP, 4)
         CHOUT(1) = INP + 4.0
         END IF
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(2) = CHOUT(2) + 1.333
         IF ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9))
     *      CHOUT(2) = CHOUT(2) + 1.333
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4))
     *       CHOUT(2) = CHOUT(2) + 1.333
         IF (NCOUNT.GE.1) CHOUT(2) = CHOUT(2) + 1.333
         CHOUT(4) = 3.333
         IF (LABEL.GT.1) CHOUT(4) = CHOUT(4) + 1.333
         END IF
C                                        Init. for line drawing
      CALL GINITL (BLC, TRC, XYRATI, CHOUT, DEPTH, PLTBLK, IRET)
      IF (IRET.NE.0) GO TO 980
C                                        Draw the box
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
      IF (LINT.LT.TRC(2)-1.) THEN
         CALL GPOS (BLC(1), LINT, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL GVEC (TRC(1), LINT, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       Labelling
      CALL FRRALB (STRTIF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Label axes
      CALL LABLAX (.TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scaling
      XRN = (TRC(1) - BLC(1)) / NPNL
      SCALX = XRN / (NTRANS+1)
      IF ((CODTYP.EQ.3) .OR. (CODTYP.EQ.6)) THEN
         SCALA = (LINT - BLC(2)) / PHSRNG
         OFY = BLC(2) - MINPHS * SCALA
         JP = 2
         DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0)
      ELSE
         SCALA = (LINT - BLC(2)) / AMPRNG
         SCALP = (TRC(2) - LINT) / PHSRNG
         OFY = BLC(2) - MINAMP * SCALA
         JP = 1
         DOZERO = (MINAMP.LT.0.0) .AND. (MAXAMP.GT.0.0) .AND.
     *      (CODTYP.GT.2)
         END IF
      OFX = 0.0
C                                       Plot the lower zero line
      IF (DOZERO) THEN
         X = BLC(1)
         Y = OFY
         CALL GPOS (X, Y, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         X = TRC(1)
         CALL GVEC (X, Y, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       Plot the lower one
      DO 55 IPNL = 1,NPNL
         OFX = (IPNL - 1) * XRN
         IO = (IPNL - 1) * NTRANS
         DX = 5.0 / XYRATI * XFAC
         DY = 5.0 * XFAC
C                                       separate panels
         IF (IPNL.GT.1) THEN
            CALL GLTYPE (1, PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            X = OFX
            CALL GPOS (X, BLC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL GVEC (X, TRC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
         CALL GLTYPE (4, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         IF ((CODTYP.EQ.3) .OR. (FACTOR.GE.0.0)) THEN
            DO 40 I = 1,NTRANS
               X = I * XSLOPE + XOFF
               IF ((X.GE.XMIN) .AND. (X.LE.XMAX) .AND.
     *            (PDATA(I+IO,JP).NE.FBLANK)) THEN
                  X = (X - XMIN) * XMULT + OFX
                  Y = PDATA(I+IO,JP) * SCALA + OFY
                  CALL GMARK (X, Y, DX, DY, IRET)
                  IF (IRET.NE.0) GO TO 980
                  END IF
 40            CONTINUE
            END IF
         IF ((CODTYP.NE.3) .AND. (FACTOR.LT.100.0)) THEN
            BLNK = .TRUE.
            CALL GLTYPE (2, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            DO 50 I = 1,NTRANS
               X = I * XSLOPE + XOFF
               IF ((X.LT.XMIN) .OR. (X.GT.XMAX) .OR.
     *            (PDATA(I+IO,JP).EQ.FBLANK)) THEN
                  BLNK = .TRUE.
               ELSE
                  X = (X - XMIN)*XMULT + OFX
                  Y = PDATA(I+IO,JP) * SCALA + OFY
                  IF (BLNK) THEN
                     CALL GPOS (X, Y, PLTBLK, IRET)
                  ELSE
                     CALL GVEC (X, Y, PLTBLK, IRET)
                     END IF
                  BLNK = .FALSE.
                  END IF
               IF (IRET.NE.0) GO TO 980
 50            CONTINUE
            END IF
 55      CONTINUE
C                                       Plot the upper zero line
      OFY = LINT - SCALP * MINPHS
      DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0) .AND.
     *   (MOD(CODTYP,3).EQ.1)
      IF (DOZERO) THEN
         CALL GLTYPE (1, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         X = BLC(1)
         Y = OFY
         CALL GPOS (X, Y, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         X = TRC(1)
         CALL GVEC (X, Y, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       Plot the phase
      DO 75 IPNL = 1,NPNL
         OFX = (IPNL - 1) * XRN
         IO = (IPNL - 1) * NTRANS
         IF ((CODTYP.EQ.1) .OR. ((FACTOR.GE.0.0) .AND. (CODTYP.EQ.4)))
     *      THEN
            CALL GLTYPE (4, PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            DO 60 I = 1,NTRANS
               X = I * XSLOPE + XOFF
               IF ((X.GE.XMIN) .AND. (X.LE.XMAX) .AND.
     *            (PDATA(I+IO,2).NE.FBLANK)) THEN
                  X = (X - XMIN) * XMULT + OFX
                  Y = PDATA(I+IO,2) * SCALP + OFY
                  CALL GMARK (X, Y, DX, DY, IRET)
                  IF (IRET.NE.0) GO TO 980
                  END IF
 60            CONTINUE
            END IF
         IF ((CODTYP.EQ.4) .AND. (FACTOR.LT.100.0)) THEN
            CALL GLTYPE (2, PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            BLNK = .TRUE.
            DO 70 I = 1,NTRANS
               X = I * XSLOPE + XOFF
               IF ((X.LT.XMIN) .OR. (X.GT.XMAX) .OR.
     *            (PDATA(I+IO,2).EQ.FBLANK)) THEN
                  BLNK = .TRUE.
               ELSE
                  X = (X - XMIN) * XMULT + OFX
                  Y = PDATA(I+IO,2) * SCALP + OFY
                  IF (BLNK) THEN
                     CALL GPOS (X, Y, PLTBLK, IRET)
                     IF (IRET.NE.0) GO TO 980
                  ELSE
                     CALL GVEC (X, Y, PLTBLK, IRET)
                     END IF
                  BLNK = .FALSE.
                  END IF
               IF (IRET.NE.0) GO TO 980
 70            CONTINUE
            END IF
 75      CONTINUE
C                                       Write spectrum
      WPLOT = .FALSE.
      IF (OFILE(1:1).NE.' ') WPLOT = .TRUE.
      IF (WPLOT) THEN
         LUNPR = 10
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET
            GO TO 990
            END IF
C                                       First write header info.
         IF (BPARM(10).LE.0.0) THEN
            WRITE (LINE,1300)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Source name
            WRITE (LINE,1400) SRCOBS
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Ra/Dec
            WRITE (LINE,1500) RAHR, RAMIN, RASEC, DECDEG, DECMIN, DECSEC
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Date/time
            CALL PTIME (CTIME, F, TIT, TITSEC)
            WRITE (LINE,1600) EXPDAT, TIT, TITSEC
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       # channels
            WRITE (LINE,1700) NCHAN, PBCH
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       BW
            WRITE (LINE,1800) TBW
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Antenna
            IF (TELNUM(1).GT.0) THEN
               WRITE (LINE,1900) TELNUM(1), ANTNAM(1)
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (ANTNAM(2)(1:1).NE.'*') THEN
                  WRITE (LINE,1900) TELNUM(2), ANTNAM(2)
                  NCH = ITRIM(LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
                  END IF
               END IF
C                                       Freq, weight
            WRITE (LINE,2000) MFREQ, AVWGHT
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (CODTYP.LT.4) THEN
               LINE = 'DATA follow with format:' //
     *            ' (1X,I5,2X,I3,2X,A5,2X,F10.4,2X,G15.6,3X,F8.3)'
               ELSE
                  LINE = 'DATA follow with format:' //
     *               ' (1X,I5,2X,I3,2X,A5,2X,2(2X,G15.6))'
               END IF
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
            LINE = 'Ifrate   IF  Polar       Frate' //
     *         '         Ampl(Jy)      Phase'
            IF (CODTYP.GE.4) LINE(40:) = 'Real(Jy)         Imag(Jy)'
            IF (JY.EQ.'K') LINE(40:) = ' Real(K)          Imag(K)'
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
         AXDENU(LOCNUM) = AXDENU(LOCNUM) * XMULT
C                                       the fringe rate spectrum
C                                       step at miliHz
         XINC = 1000.0 / (NTRANS*APARM(1))
C
         LIF = 0
         DO 90 IPNL = 1,NPNL
            IIP = STRPOL + (IPNL-1) / (STOPIF-STRTIF+1)
            JIF = STRTIF + IPNL - 1 - (IIP-STRPOL)*(STOPIF-STRTIF+1)
            IF (JIF.NE.LIF) THEN
               REFPIX = CATR(KRCRP+KLOCFY)
               SFREQ = CATD(KDCRV+KLOCFY) + FOFF(JIF)
               FREINC = FINC(JIF)
               LIF = JIF
               END IF
            DO 85 I = 1,NTRANS
               J = I
               J = J + (IPNL - 1) * NTRANS
               LINE = ' '
C               ITIME = I - 1 + PBCH
C                                       ITIME is the point number at
C                                       the fringe rate spectrum
               ITIME = I
               FRRATE = -XINC * (I - 1 + NTRANS / 2.0)
C
               IF ((AMP(J).EQ.FBLANK) .OR. (PHASE(J).EQ.FBLANK)) THEN
                  WRITE (LINE,2100) ITIME, JIF, POLLAB(IIP), FRRATE,
     *               'FLAGGED'
               ELSE IF (CODTYP.LT.4) THEN
                  WRITE (LINE,2200) ITIME, JIF, POLLAB(IIP), FRRATE,
     *               AMP(J), PHASE(J)
               ELSE
                  WRITE (LINE,2300) ITIME, JIF, POLLAB(IIP), FRRATE,
     *               AMP(J), PHASE(J)
                  END IF
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,2400) IRET
                  CALL MSGWRT (7)
                  GO TO 981
                  END IF
 85            CONTINUE
 90         CONTINUE
         END IF
C                                       Finish up
      GPHPAG = .FALSE.
      IF (XSOLIN.NE.0.0) GPHPAG = STOPU.LT.STOPD
      IF (IFNO.LT.0) GPHPAG = .TRUE.
      CALL GFINIS (PLTBLK, IRET)
      IF (.NOT.DOTV) CALL HIPLOT (DISKIN, CNOIN, PVER, BUFFI, IERR)
      GO TO 995
C                                       plot troubles
 980  WRITE (MSGTXT,2500) IRET
      CALL MSGWRT (8)
 981  WRITE (MSGTXT,2600)
      CALL MSGWRT (7)
      CALL GFINIS (PLTBLK, IERR)
      IF (.NOT.DOTV) THEN
         IF (IERR.EQ.0) THEN
            CALL HIPLOT (DISKIN, CNOIN, PVER, BUFFI, IERR)
         ELSE
            CALL ZCLOSE (IGLUN, IGFIND, IERR)
            CALL ZPHFIL ('PL', DISKIN, SLOT, PVER, PHNAME, IERR)
            CALL ZDESTR (DISKIN, PHNAME, IERR)
            CALL DELEXT ('PL', DISKIN, SLOT, 'WRIT', CATBLK, BUFFI,
     *         PVER, IERR)
            END IF
         END IF
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  IF (WPLOT) CALL ZTXCLS (LUNPR, PFIND, IERR)
      IF (IERR.NE.0) IRET = IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRRAPL: ERROR ',I3,' RECEIVED FROM PLCREA')
 1100 FORMAT ('Plot file version ',I3,' created')
 1200 FORMAT ('FRRAPL: ERROR ',I3,' RECEIVED FROM ZTXOPN')
 1300 FORMAT ('Header information')
 1400 FORMAT ('Source: ',A)
 1500 FORMAT ('RA(1950):  ',2I3,F6.2,8X,'DEC(1950): ',2I3,F6.2)
 1600 FORMAT ('OBS. DATE: ',A,10X,'Time of record: ',
     *   I3,'/',2I3,F5.1)
 1700 FORMAT ('No. channels:',I6,8X,'First channel plotted',I5)
 1800 FORMAT ('Bw (kHz): ',F10.3)
 1900 FORMAT ('Antenna #',I3,5X, 'name: ',A8)
 2000 FORMAT ('Rest freq. (MHz) : ',F12.4,5X,' Av. weight : ',F12.4)
 2100 FORMAT (1X,I5,2X,I3,2X,A,2X,F10.4,2X,A)
 2200 FORMAT (1X,I5,2X,I3,2X,A,2X,F10.4,2X,G15.6,2X,F8.3)
 2300 FORMAT (1X,I5,2X,I3,2X,A,2X,F10.4,2X,G15.6,2X,G15.6)
 2400 FORMAT ('FRRAPL: ERROR ',I3,' RECEIVED FROM ZTXIO')
 2500 FORMAT ('FRRAPL: ERROR',I5,' FROM PLOTTING ROUTINES')
 2600 FORMAT ('FRRAPL: WILL TRY TO FINISH PARTIAL PLOT')
      END
      SUBROUTINE LABLAX (LAST, IRET)
C-----------------------------------------------------------------------
C   LABLAX controls the axis labelling for FRPLT
C   NCOUNT < 0 => prepare coordinate common, do no labeling
C   Input:
C      LAST     L    Last baseline/antenna
C   Output:
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      LOGICAL   LAST
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  AXFREQ*8, AXTIME*8, XCTYP*20, XPREF*5, TEXT*80
      REAL   XBLC(2), XTRC(2), CATR(256), YMULT, YMULT2, WBLC(2),
     *   WTRC(2), ATEMP, LBLC(2), LTRC(2), XREF, XREFO, XINC, CXMULT
      DOUBLE PRECISION CATD(128)
      INTEGER   IRET, DEPTH(5), I, WRVLAB, IPNL, WRV, IIF, INCHAR, IIP,
     *   NDRAW
      LOGICAL   F, PFLG, WRHLAB
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      REAL PDATA(MXTIFP,2)
      INCLUDE 'FRPL4.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (CATUV, CATR, CATD)
      EQUIVALENCE (PDATA(1,1), AMP),    (PDATA(1,2), PHASE)
      DATA F /.FALSE./
      DATA  AXFREQ /'Hz      '/, AXTIME /'Seconds '/
C-----------------------------------------------------------------------
      WRVLAB = 1
      IF ((NYPANE.EQ.2) .OR. (NYPANE.EQ.3)) WRVLAB = 2
      IF (NYPANE.EQ.4) WRVLAB = 3
      IF (NYPANE.GT.4) WRVLAB = 100
      IF ((FIXED) .AND. (NXP.NE.1)) WRVLAB = 0
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
      NPNL = NCTOT / NTRANS
C
      XMULT = 1.0
C                                       Determine lower plot parms
      IF (CODTYP.EQ.3) THEN
         CPREF(2,LOCNUM) = ' '
         YMULT = 1.0
      ELSE
         IF (CODTYP.EQ.6) THEN
            ATEMP = MAXPHS - MINPHS
            ATEMP = MAX (ATEMP, ABS(MAXPHS))
            ATEMP = MAX (ATEMP, ABS(MINPHS))
         ELSE
            ATEMP = MAXAMP - MINAMP
            ATEMP = MAX (ATEMP, ABS(MAXAMP))
            ATEMP = MAX (ATEMP, ABS(MINAMP))
            END IF
         YMULT = ATEMP
         CALL METSCL (LABEL, ATEMP, CPREF(2,LOCNUM), PFLG)
         YMULT = ATEMP / YMULT
         END IF
C
      XBLC(1) = BLC(1)
      XBLC(2) = BLC(2)
      XTRC(1) = TRC(1)
      XTRC(2) = LINT
      RPLOC(1,LOCNUM) = BLC(1)
      RPLOC(2,LOCNUM) = BLC(2)
      IF ((NCOUNT.EQ.0) .OR. (NCOUNT.EQ.-1)) XLINT = LINT
      IF ((NCOUNT.GE.1) .OR. (NCOUNT.LE.-2)) THEN
         XBLC(1) = YBLC(1)
         XBLC(2) = YBLC(2)
         XTRC(1) = YTRC(1)
         XTRC(2) = XLINT
         RPLOC(1,LOCNUM) = YBLC(1)
         RPLOC(2,LOCNUM) = YBLC(2)
         END IF
C                                       Set x-axis parms
C                                       Fringe rate spectra
      XMIN = BPARM(5)
      XMAX = BPARM(6)
      IF (WFFTR) THEN
         NDRAW = NTRANS
         XINC = 1.0 / (NTRANS * PREAVG * 86400.0)
         XREF = -XINC * (NTRANS / 2)
         IF (SELFSX) THEN
            XMIN = XREF
            XMAX = (NDRAW/2 + 1) * XINC
            END IF
         XSLOPE = XINC
         XOFF = XREF
         XREFO = XREF
         CALL METSCL (LABEL, XREF, CPREF(1,LOCNUM), PFLG)
         CXMULT = XREF / XREFO
         CTYP(1,LOCNUM) = AXFREQ
C                                       Visibility vs time
      ELSE
         NDRAW = NTIMEP
         XINC = PREAVG * 86400.0
         XSLOPE = XINC
         XREF = 0
         XOFF = XREF
         CTYP(1,LOCNUM) = AXTIME
         IF (SELFSX) THEN
            XMIN = XREF
            XMAX = (NDRAW + 1) * XINC
            END IF
         CXMULT = 1.0
         END IF
C                                       First value
      XMULT = REAL ((XTRC(1) - XBLC(1))/NPNL) / (XMAX - XMIN)
C
      RPVAL(1,LOCNUM) = XMIN * CXMULT
      AXINC(1,LOCNUM) = (XMAX-XMIN)*CXMULT / ((XTRC(1) - XBLC(1))/NPNL)
C                                       Y axis
      IF (MOD(CODTYP-1,3).EQ.2) THEN
         RPVAL(2,LOCNUM) = MINPHS * YMULT
         AXINC(2,LOCNUM) = YMULT * PHSRNG / (XLINT - XBLC(2))
      ELSE
         RPVAL(2,LOCNUM) = MINAMP * YMULT
         AXINC(2,LOCNUM) = YMULT * AMPRNG / (XLINT - XBLC(2))
         END IF
      IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.2)) THEN
         CTYP(2,LOCNUM) = 'Ampl ' // JY
         IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'ampl/ch0'
            END IF
      ELSE IF (CODTYP.EQ.3) THEN
         CTYP(2,LOCNUM) = 'Phas deg'
         IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'Phas-ch0'
            END IF
      ELSE IF ((CODTYP.EQ.4) .OR. (CODTYP.EQ.5)) THEN
         CTYP(2,LOCNUM) = 'Real ' // JY
         IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'real/ch0'
            END IF
      ELSE
         CTYP(2,LOCNUM) = 'Imag ' // JY
         IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'Imag/ch0'
            END IF
         END IF
C                                       Label it
      IF ((NCOUNT.EQ.0) .AND. (NPNL.EQ.1)) THEN
         I = KLOCF(LOCNUM)
         IIF = STRTIF
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATI, F, PLTBLK, IRET)
         KLOCF(LOCNUM) = I
         IF (IRET.NE.0) GO TO 999
         WRITE (TEXT,1000) IIF, POLLAB(STRPOL)
         CALL REFRMT (TEXT, '_', INCHAR)
         INCHAR = INCHAR+1
         TEXT(INCHAR:) = ')'
         CALL GPOS (XBLC(1), XBLC(2), PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GICHAR (1, INCHAR, 0, 2.0, 1.1, TEXT, PLTBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Need to roll my own for the
C                                       multiplots/page
      ELSE IF (NCOUNT.GE.0) THEN
C                                       Store axis labelling text.
         XCTYP = CTYP(1,LOCNUM)
         XPREF = CPREF(1,LOCNUM)
         ALABEL(1) = CTYP(2,LOCNUM)
         APREF(1) = CPREF(2,LOCNUM)
         IF (NCOUNT.GT.0) THEN
            CPREF(2,LOCNUM) = ' '
            CTYP(2,LOCNUM) = ' '
            END IF
         I = KLOCF(LOCNUM)
         KLOCF(LOCNUM) = 0
         WBLC(1) = BLC(1)
         WBLC(2) = BLC(2)
         WTRC(1) = TRC(1)
         WTRC(2) = LINT
         LBLC(2) = XBLC(2)
         LTRC(2) = XTRC(2)
         WRV = WRVLAB
         DO 20 IPNL = 1,NPNL
C                                       IF/Stokes label
            IIP = STRPOL + (IPNL-1) / (STOPIF-STRTIF+1)
            IIF = STRTIF + IPNL - 1 - (IIP-STRPOL)*(STOPIF-STRTIF+1)
            WRITE (TEXT,1000) IIF, POLLAB(IIP)
            CALL REFRMT (TEXT, '_', INCHAR)
            INCHAR = INCHAR+1
            TEXT(INCHAR:) = ')'
C                                       axis label
            WRHLAB = ((NYP.EQ.1) .OR. (LAST)) .AND. ((MOD(IPNL,2).EQ.1)
     *         .OR. (NPNL.LE.2))
            IF (.NOT.WRHLAB) THEN
               CPREF(1,LOCNUM) = ' '
               CTYP(1,LOCNUM) = ' '
            ELSE
               CPREF(1,LOCNUM) = XPREF
               CTYP(1,LOCNUM) = XCTYP
               END IF
            LBLC(1) = XBLC(1) + (IPNL-1) * ((XTRC(1)-XBLC(1)) / NPNL)
            LTRC(1) = XBLC(1) + IPNL * ((XTRC(1)-XBLC(1)) / NPNL)
            CALL GPOS (LBLC(1), LBLC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GICHAR (1, INCHAR, 0, 2.0, 1.2, TEXT, PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            RPLOC(1,LOCNUM) = LBLC(1)
            CALL MLAB1 (WBLC, WTRC, LBLC, LTRC, CHOUT, LABEL, XYRATI,
     *         PLTBLK, WRV, WRHLAB, IRET)
            IF (IRET.NE.0) GO TO 999
            WRV = 0
 20         CONTINUE
         CPREF(1,LOCNUM) = XPREF
         CTYP(1,LOCNUM) = XCTYP
         KLOCF(LOCNUM) = I
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Then the phase frame
      IF (((CODTYP.EQ.1) .OR. (CODTYP.EQ.4)) .AND. (NCOUNT.GE.0)) THEN
         XBLC(1) = BLC(1)
         XBLC(2) = XLINT
         XTRC(1) = TRC(1)
         XTRC(2) = TRC(2)
         RPLOC(2,LOCNUM) = XLINT
         CTYP(1,LOCNUM) = ' '
         CPREF(1,LOCNUM) = ' '
         IF ((NCOUNT.GE.1) .OR. (NCOUNT.LE.-2)) THEN
            XBLC(1) = YBLC(1)
            XBLC(2) = XLINT
            XTRC(1) = YTRC(1)
            XTRC(2) = YTRC(2)
            END IF
         IF (CODTYP.EQ.1) THEN
            CTYP(2,LOCNUM) = 'Phas deg'
            IF (BPARM(1).GT.0.0) THEN
               CTYP(2,LOCNUM) = 'Phas-ch0'
               END IF
            CPREF(2,LOCNUM) = ' '
            YMULT2 = 1.0
         ELSE
            CTYP(2,LOCNUM) = 'Imag ' // JY
            IF (BPARM(1).GT.0.0) THEN
               CTYP(2,LOCNUM) = 'Imag/ch0'
               END IF
            ATEMP = MAX (MAXPHS - MINPHS, ABS(MAXPHS))
            ATEMP = MAX (ATEMP, ABS(MINPHS))
            YMULT2 = ATEMP
            CALL METSCL (LABEL, ATEMP, CPREF(2,LOCNUM), PFLG)
            YMULT2 = ATEMP / YMULT2
            END IF
         RPVAL(2,LOCNUM) = MINPHS * YMULT2
         AXINC(2,LOCNUM) = YMULT2 * PHSRNG / (XTRC(2) - XLINT)
         IF ((NCOUNT.EQ.0) .AND. (NPNL.EQ.1)) THEN
            CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATI, F, PLTBLK,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Multiplots/page
         ELSE IF (NCOUNT.GE.0) THEN
C                                       Store axis labelling text.
            XCTYP = CTYP(1,LOCNUM)
            XPREF = CPREF(1,LOCNUM)
            ALABEL(2) = CTYP(2,LOCNUM)
            APREF(2) = CPREF(2,LOCNUM)
            ALABEL(2) = CTYP(2,LOCNUM)
            APREF(2) = CPREF(2,LOCNUM)
            IF (NCOUNT.GT.0) THEN
               CPREF(2,LOCNUM) = ' '
               CTYP(2,LOCNUM) = ' '
               END IF
            WBLC(1) = BLC(1)
            WBLC(2) = LINT
            WTRC(1) = TRC(1)
            WTRC(2) = TRC(2)
            LBLC(2) = XBLC(2)
            LTRC(2) = XTRC(2)
            WRV = WRVLAB
            DO 40 IPNL = 1,NPNL
               IIP = STRPOL + (IPNL-1) / (STOPIF-STRTIF+1)
               IIF = STRTIF + IPNL - 1 - (IIP-STRPOL)*(STOPIF-STRTIF+1)
C                                       axis label
               CPREF(1,LOCNUM) = ' '
               CTYP(1,LOCNUM) = ' '
               LBLC(1) = XBLC(1) + (IPNL-1) * ((XTRC(1)-XBLC(1)) / NPNL)
               LTRC(1) = XBLC(1) + IPNL * ((XTRC(1)-XBLC(1)) / NPNL)
               RPLOC(1,LOCNUM) = LBLC(1)
               CALL MLAB1 (WBLC, WTRC, LBLC, LTRC, CHOUT, LABEL, XYRATI,
     *            PLTBLK, WRV, F, IRET)
               WRV = 0
               IF (IRET.NE.0) GO TO 999
 40            CONTINUE
            CPREF(1,LOCNUM) = XPREF
            CTYP(1,LOCNUM) = XCTYP
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IF',I3,'(',A)
      END
      SUBROUTINE MULTPL (NPARM, IERR)
C-----------------------------------------------------------------------
C  Routine to write multiple plots to one page - a treesaving algorithm
C  Input:
C    NPARM     I      # input parameters from POPS
C  Output:
C    IERR      I      Error code
C
C The whole thing is controlled by 2 keywords. NBASE is the number of
C baselines (or antennas in total power mode) that it is FRRAible to
C plot. NCOUNT is the number of plot/page.
C
C The algorithm reads the data and fills the plot buffer first and then
C will write the plot to the page, so no blank plots will be created.
C However there is no look ahead facility so that if NCOUNT is > than
C the actual number of baselines or antennas there may be blank space
C left on the plot page.
C
C Each plot is independently scaled and labelled and the vertical axis
C labels are valid for each plot (they only appear once). The horizontal
C axis labels are placed under the bottom LHC plot - if that is missing
C for some reason the tick marks of all other plots are labelled but
C the type of the label will be missing.
C
C The vertical scales are rounded to the nearest integer for the sake
C of space on the page - if the user wants the correct label they should
C set NCOUNT to 0 and plot the specific frame they want on a single
C page.
C-----------------------------------------------------------------------
      INTEGER  NPARM, IERR
C
      INCLUDE 'FRPLT.INC'
      CHARACTER SPRTXT*16, PHNAME*48
      INTEGER   I, IIF, JP, J, IANGL, NCHAR, IROUND, JERR,
     *   BUFFI(256), IPNL, IO, IPOL, LPEIF, LPOLNM
      REAL      XBLC(2), XTRC(2), SCALX, SCALA, SCALP, OFX, OFY, X, Y,
     *   DX, DY, DCX, DCY, PLTXOF, PLTYOF, TEPS, TSOLIN, XRN, XFAC
      LOGICAL   DOZERO, NEWPAG, LEAVE,  MULTIF, BLNK, NEWEND, LAST
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      REAL PDATA(MXTIFP,2)
      INCLUDE 'FRPL4.INC'
      INCLUDE 'FRRANX.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (PDATA(1,1), AMP),    (PDATA(1,2), PHASE)
C-----------------------------------------------------------------------
      TEPS = 0.02 / (24. * 60. * 60.)
      OFFSET = 0
      ALABEL(1) = ' '
      ALABEL(2) = ' '
      APREF(1) = ' '
      APREF(2) = ' '
      MULTIF = PEIF.NE.PBIF
      NEWPAG = .FALSE.
      CLOSED = .TRUE.
      XFAC = ABS (FACTOR)
      IF (XFAC.GE.100.0) XFAC = XFAC - 100.0
      IF (XFAC.LT.0.4) XFAC = 1.0
C                                       Antenna list to plot
 10   CONTINUE
      CALL SOUFIL (IERR)
C                                      Self-scaling
C                                       Loop over antennas/baselines
C                                       How do we place plots on page?
      NYPANE = SQRT (REAL(NCOUNT))
      IF (NYPANE*NYPANE.LT.NCOUNT) NYPANE = NYPANE + 1
      NXPANE = NCOUNT/NYPANE
      IF (NXPANE*NYPANE.LT.NCOUNT) NXPANE = NXPANE + 1
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      LINT = 1000.0
      IF (CODTYP.EQ.1) LINT = 700.
      IF (CODTYP.EQ.4) LINT = 500.
      IF (FIXED) THEN
         PLTXIN = 1000.0 / NXPANE
         PLTYIN = 1000.0 / NYPANE
         PLTXOF = 0.
         PLTYOF = 0.
      ELSE
         PLTXIN = 1000.0 / (NXPANE - 0.25)
         PLTYIN = 1000.0 / (NYPANE - 0.10)
         PLTXOF = NXPANE * PLTXIN - 1000.
         PLTYOF = NYPANE * PLTYIN - 1000.
         END IF
      NXPIX = IROUND (TRC(1) - BLC(1)) + 1
      NYPIX = IROUND (TRC(2) - BLC(2)) + 1
      ICOUNT = 0
      CALL RFILL (50, 0.0, XANTEN)
      DO 300 I = 1,NBASE
         ICURNT = I + OFFSET
         NEWPAG = (MOD(ICOUNT,NCOUNT).EQ.0)
         IF (I.EQ.1) NEWPAG = .TRUE.
         CALL COPY (256, CATBLK, CATSAV)
         LAST = I.EQ.NBASE
C                                       Generate plot buffer
C                                       for fringe rate spectra
         CALL FRRAUV (IERR)
         IF ((IERR.EQ.5) .AND. (SCANAV)) THEN
            IF (NEXTSC.LT.NUMNX) IERR = 0
            END IF
         IF ((IERR.EQ.5) .OR. (IERR.EQ.10)) THEN
            IERR = 0
            IF ((XSOLIN.GT.0.0) .AND. (STOPU.GE.STOPD))
     *         GO TO 350
            END IF
         IF (IERR.NE.0) GO TO 350
         XANTEN(1) = XA1(ICURNT)
         XANTEN(2) = XA2(ICURNT)
         CALL COPY (256, CATSAV, CATBLK)
C                                       No data?
         IF (BLNKBF) THEN
C                                       Close down the plot file?
            NEWEND = ((MOD(ICOUNT,NCOUNT).EQ.0) .AND. (ICOUNT.GT.0))
            IF ((.NOT.NEWEND) .AND. (I.EQ.NBASE) .AND.
     *         (ICOUNT.GT.0)) NEWEND = .TRUE.
            IF (NEWEND) THEN
               NEWPAG = .TRUE.
               GPHPAG = DOTV
               LEAVE = .FALSE.
               CALL GFINIS (PLTBLK, IERR)
               IF (IERR.EQ.-1) LEAVE = .TRUE.
               CLOSED = .TRUE.
               ICOUNT = 0
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
               CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *            CATBLK, .FALSE., BUFF1, IERR)
               IF (IERR.NE.0) GO TO 999
               IF (LEAVE) GO TO 999
               END IF
            GO TO 300
            END IF
C                                      set IF counters
C
C                                       load up the plotting
C                                       arrays
         LPEIF = PEIF
         LPOLNM = POLNUM
         IF (DOCHIF) LPEIF = PBIF
         IF (DOCHPL) LPOLNM = 1
         DO 100 IPOL = 1,LPOLNM
         DO 100 IIF = PBIF,LPEIF
C                                       Plot the result
            IF (DOCHIF) THEN
               STRTIF = PBIF
               STOPIF = PEIF
            ELSE
               STRTIF = IIF
               STOPIF = IIF
               END IF
            IF (DOCHPL) THEN
               STRPOL = 1
               STOPOL = POLNUM
            ELSE
               STRPOL = IPOL
               STOPOL = IPOL
               END IF
C
            CALL FILLPL (IIF, IPOL, IERR)
            IF (IERR.EQ.1) GO TO 90
            ICOUNT = ICOUNT + 1
C                                       Determine scaling
            AMPRNG = ABS (MAXAMP - MINAMP)
            PHSRNG = ABS (MAXPHS - MINPHS)
C                                       Amplitude range
            IF (AMPRNG.EQ.0.0) THEN
               MINAMP = 0.0
               MAXAMP = MAXAMP * 1.1
            ELSE IF (AMPRNG.NE.0) THEN
               MINAMP = MINAMP - 0.04 * AMPRNG
               MAXAMP = MAXAMP + 0.04 * AMPRNG
               IF ((MINAMP.GT.0.0) .AND. (MINAMP.LT.0.25*MAXAMP)
     *            .AND. (SELFSA)) MINAMP = 0.0
               END IF
C                                       Phase range
            IF (PHSRNG.EQ.0.0) THEN
               MINPHS = -180.0
               MAXPHS = 180.0
            ELSE IF (PHSRNG.NE.0.0) THEN
               MINPHS = MINPHS - 0.06 * PHSRNG
               MAXPHS = MAXPHS + 0.06 * PHSRNG
               END IF
            AMPRNG = ABS (MAXAMP - MINAMP)
            PHSRNG = ABS (MAXPHS - MINPHS)
C                                       Set panel offsets.
            IF (NEWPAG) THEN
               NXP = 0
               NYP = NYPANE
               END IF
            NXP = NXP + 1
            IF (NXP.GT.NXPANE) THEN
               NXP = 1
               NYP = NYP - 1
               END IF
C                                       Set window for current plot.
            XBLC(1) = BLC(1) + ABS (NXP-1) * PLTXIN
            XBLC(2) = BLC(2) + ABS (NYP-1) * PLTYIN
            XTRC(1) = XBLC(1) + PLTXIN - 1.0 - PLTXOF
            XTRC(2) = XBLC(2) + PLTYIN - 1.0 - PLTYOF
            YBLC(1) = XBLC(1)
            YBLC(2) = XBLC(2)
            YTRC(1) = XTRC(1)
            YTRC(2) = XTRC(2)
            XLINT = XBLC(2) + (LINT/1000.0) * PLTYIN
            XLINT = MIN (XLINT, XTRC(2))
C                                       new plot file?
            IF (NEWPAG) THEN
               IF (CLOSED) CALL REFRSH (NPARM, LAST, IERR)
               CLOSED = .FALSE.
               NEWPAG = .FALSE.
               END IF
C                                       Draw borders.
            CALL GLTYPE (1, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GPOS (XBLC(1), XBLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GVEC (XTRC(1), XBLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GVEC (XTRC(1), XTRC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GVEC (XBLC(1), XTRC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GVEC (XBLC(1), XBLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            IF (LINT.LT.1000.0) THEN
               CALL GPOS (XBLC(1), XLINT, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               CALL GVEC (XTRC(1), XLINT, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               END IF
C                                       Axis labels
            CALL LABLAX (LAST, IERR)
C                                       Global labels
            IF (ICOUNT.EQ.1) CALL FRRALB (STRTIF, IERR)
C                                       Label each frame with
C                                       antenna name
            IANGL = 0
            DCX = 2.
            DCY = -1.83
            IF (ANTNAM(1)(1:4).EQ.'VLBA') THEN
               SPRTXT = ANTNAM(1)(6:8)
            ELSE IF (ANTNAM(1)(1:5).EQ.'EVLA:') THEN
               SPRTXT = ANTNAM(1)(6:8)
            ELSE IF (ANTNAM(1)(1:5).EQ.'VLA: ') THEN
               SPRTXT = ANTNAM(1)(6:8)
            ELSE IF (ANTNAM(1)(1:5).EQ.'VLA:_') THEN
               SPRTXT = ANTNAM(1)(6:8)
            ELSE IF (ANTNAM(1)(1:4).EQ.'VLA:') THEN
               SPRTXT = ANTNAM(1)(5:7)
            ELSE
               SPRTXT = ANTNAM(1)(1:4)
               END IF
            SPRTXT(5:) = ' -'
            IF (ANTNAM(2)(1:4).EQ.'VLBA') THEN
               SPRTXT(8:) = ANTNAM(2)(6:8)
            ELSE IF (ANTNAM(2)(1:5).EQ.'EVLA:') THEN
               SPRTXT(8:) = ANTNAM(2)(6:8)
            ELSE IF (ANTNAM(2)(1:5).EQ.'VLA: ') THEN
               SPRTXT(8:) = ANTNAM(2)(6:8)
            ELSE IF (ANTNAM(2)(1:5).EQ.'VLA:_') THEN
               SPRTXT(8:) = ANTNAM(2)(6:8)
            ELSE IF (ANTNAM(2)(1:4).EQ.'VLA:') THEN
               SPRTXT(8:) = ANTNAM(2)(5:7)
            ELSE
               SPRTXT(8:) = ANTNAM(2)(1:4)
               END IF
C
            CALL REFRMT (SPRTXT, '_', NCHAR)
            CALL GPOS (XBLC(1), XLINT, PLTBLK, IERR)
            CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLTBLK,
     *         IERR)
            MSGTXT = 'Doing: ' // SPRTXT
C                                       antenna number
            IANGL = 0
            DCY = -1.83
            WRITE (SPRTXT,1026) TELNUM(1), TELNUM(2)
            CALL REFRMT (SPRTXT, '_', NCHAR)
            DCX = -(NCHAR+1) - 1.0
            CALL GPOS (XTRC(1), XLINT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLTBLK,
     *         IERR)
            IF (IERR.NE.0) GO TO 350
C                                       IF labelling
            WRITE (SPRTXT,1000) IIF
            CALL REFRMT (SPRTXT, '_', NCHAR)
            MSGTXT(20:) = SPRTXT
            CALL MSGWRT (3)
C                                       Scaling
            XRN = (XTRC(1) - XBLC(1)) / NPNL
            SCALX = XRN / (NTRANS + 1)
            IF ((CODTYP.EQ.3) .OR. (CODTYP.EQ.6)) THEN
               SCALA = (XLINT - XBLC(2)) / PHSRNG
               OFY = XBLC(2) - MINPHS * SCALA
               JP = 2
               DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0)
            ELSE
               SCALA = (XLINT - XBLC(2)) / AMPRNG
               SCALP = (XTRC(2) - XLINT) / PHSRNG
               OFY = XBLC(2) - MINAMP * SCALA
               JP = 1
               DOZERO = (MINAMP.LT.0.0) .AND. (MAXAMP.GT.0.0) .AND.
     *            (CODTYP.GT.2)
               END IF
            OFX = XBLC(1)
C                                       Plot the lower zero line
            IF (DOZERO) THEN
               CALL GLTYPE (1, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               X = XBLC(1)
               Y = OFY
               CALL GPOS (X, Y, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               X = XTRC(1)
               CALL GVEC (X, Y, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               END IF
C                                       Plot the lower one
            DO 55 IPNL = 1,NPNL
               IO = (IPNL - 1) * NTRANS
               OFX = XBLC(1) + (IPNL - 1) * XRN
C                                       separate panels
               IF (IPNL.GT.1) THEN
                  CALL GLTYPE (1, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  X = OFX
                  CALL GPOS (X, XBLC(2), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  CALL GVEC (X, XTRC(2), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  END IF
               CALL GLTYPE (4, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               DX = 5.0 / XYRATI * XFAC
               DY = 5.0 * XFAC
               IF ((CODTYP.EQ.3) .OR. (FACTOR.GE.0.0)) THEN
                  DO 40 J = 1,NTRANS
                     X = J * XSLOPE + XOFF
                     IF ((X.GE.XMIN) .AND. (X.LE.XMAX) .AND.
     *                  (PDATA(I+IO,JP).NE.FBLANK)) THEN
                        X = (X - XMIN) * XMULT + OFX
                        Y = PDATA(J+IO,JP) * SCALA + OFY
                        CALL GMARK (X, Y, DX, DY, IERR)
                        IF (IERR.NE.0) GO TO 350
                        END IF
 40                  CONTINUE
                  END IF
               IF ((CODTYP.NE.3) .AND. (FACTOR.LT.100.0)) THEN
                  BLNK = .TRUE.
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 50 J = 1,NTRANS
                     X = J * XSLOPE + XOFF
                     IF ((X.LT.XMIN) .OR. (X.GT.XMAX) .OR.
     *                  (PDATA(I+IO,JP).EQ.FBLANK)) THEN
                        BLNK = .TRUE.
                     ELSE
                        X = (X - XMIN)*XMULT + OFX
                        Y = PDATA(J+IO,JP) * SCALA + OFY
                        IF (BLNK) THEN
                           CALL GPOS (X, Y, PLTBLK, IERR)
                        ELSE
                           CALL GVEC (X, Y, PLTBLK, IERR)
                           END IF
                        BLNK = .FALSE.
                        IF (IERR.NE.0) GO TO 350
                        END IF
 50                  CONTINUE
                  END IF
 55            CONTINUE
C                                       Plot the upper zero line
            OFY = XLINT - SCALP * MINPHS
            DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0) .AND.
     *         (MOD(CODTYP,3).EQ.1)
            IF (DOZERO) THEN
               CALL GLTYPE (1, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               X = XBLC(1)
               Y = OFY
               CALL GPOS (X, Y, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               X = XTRC(1)
               CALL GVEC (X, Y, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               END IF
C                                       Plot the phase
            DO 75 IPNL = 1,NPNL
               IO = (IPNL - 1) * NTRANS
               OFX = XBLC(1) + (IPNL - 1) * XRN
               IF ((CODTYP.EQ.1) .OR. ((FACTOR.GE.0.0) .AND.
     *            (CODTYP.EQ.4))) THEN
                  CALL GLTYPE (4, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 60 J = 1,NTRANS
                     X = J * XSLOPE + XOFF
                     IF ((X.GE.XMIN) .AND. (X.LE.XMAX) .AND.
     *                  (PDATA(I+IO,2).NE.FBLANK)) THEN
                        X = (X - XMIN) * XMULT + OFX
                        X = J * SCALX + OFX
                        Y = PDATA(J+IO,2) * SCALP + OFY
                        CALL GMARK (X, Y, DX, DY, IERR)
                        IF (IERR.NE.0) GO TO 350
                        END IF
 60                  CONTINUE
                  END IF
               IF ((CODTYP.EQ.4) .AND. (FACTOR.LT.100.0)) THEN
                  BLNK = .TRUE.
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 70 J = 1,NTRANS
                     X = J * XSLOPE + XOFF
                     IF ((X.LT.XMIN) .OR. (X.GT.XMAX) .OR.
     *                  (PDATA(I+IO,2).EQ.FBLANK)) THEN
                        BLNK = .TRUE.
                     ELSE
                        X = (X - XMIN)*XMULT + OFX
                        Y = PDATA(J+IO,2) * SCALP + OFY
                        IF (BLNK) THEN
                           CALL GPOS (X, Y, PLTBLK, IERR)
                        ELSE
                           CALL GVEC (X, Y, PLTBLK, IERR)
                           END IF
                        IF (IERR.NE.0) GO TO 350
                        BLNK = .FALSE.
                        END IF
 70                  CONTINUE
                  END IF
 75            CONTINUE
C                                       Tidy up messages
            MSGTXT = '-------------------------'
            CALL MSGWRT (4)
C                                       Close down the plot file
 90         NEWEND = ((MOD(ICOUNT,NCOUNT).EQ.0) .AND. (ICOUNT.GT.0))
            IF ((I.EQ.NBASE) .AND. (IIF.EQ.LPEIF) .AND.
     *         (IPOL.EQ.LPOLNM) .AND. (ICOUNT.GT.0)) NEWEND = .TRUE.
            IF (NEWEND) THEN
               NEWPAG = .TRUE.
               GPHPAG = DOTV
               IF ((I.EQ.NBASE) .AND. (IIF.EQ.LPEIF) .AND.
     *            (IPOL.EQ.LPOLNM) .AND. ((XSOLIN.EQ.0.0) .OR.
     *            (STOPU+TEPS.GE.STOPD))) GPHPAG = .FALSE.
               CALL GFINIS (PLTBLK, IERR)
               LEAVE = (IERR.EQ.-1)
               CLOSED = .TRUE.
               ICOUNT = 0
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
               CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *            CATBLK, .FALSE., BUFF1, IERR)
               IF (IERR.NE.0) GO TO 999
               IF (LEAVE) GO TO 999
               END IF
 100        CONTINUE
C
 300     CONTINUE
C                                       Loop for more times?
      IF (XSOLIN.NE.0.0) THEN
C                                       Scan averaging?
         TSOLIN = XSOLIN / (60.0 * 24.0)
         IF (SCANAV) THEN
            NEXTSC = NEXTSC + 1
            IF (NEXTSC.GT.NUMNX) GO TO 350
            STARTU = NXTIM(1,NEXTSC)
            STOPU = NXTIM(2,NEXTSC)
            INITVS = NXVISN(1,NEXTSC)
         ELSE
            STARTU = STOPU + 0.02 / (24. * 60. * 60.)
            INITVS = PLSTVS + 1
            CALL FINDUV (STARTU, TSOLIN, PLSTVS, IERR)
            IF (IERR.NE.0) THEN
               IERR = MAX (IERR, 0)
               GO TO 350
               END IF
            STOPU = STARTU + TSOLIN
            END IF
         STOPU = MIN (STOPU, STOPD)
         TIMRNG(1) = STARTU
         TIMRNG(5) = STOPU
         IF (STARTU.LT.STOPD) GO TO 10
         IERR = 0
         END IF
C                                       Close down final plot
 350  IF (.NOT.CLOSED) THEN
         GPHPAG = .FALSE.
         IF ((XSOLIN.NE.0.0) .AND. (IERR.EQ.0)) GPHPAG = DOTV
         CALL GFINIS (PLTBLK, JERR)
         LEAVE = JERR.EQ.-1
         CLOSED = .TRUE.
         ICOUNT = 0
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
         CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *      CATBLK, .FALSE., BUFF1, JERR)
C                                       close out files
         IF (.NOT.DOTV) THEN
            IF (IERR.EQ.0) THEN
               CALL HIPLOT (DISKIN, CNOIN, PVER, BUFFI, IERR)
            ELSE
               CALL ZCLOSE (IGLUN, IGFIND, JERR)
               CALL ZPHFIL ('PL', DISKIN, SLOT, PVER, PHNAME, JERR)
               CALL ZDESTR (DISKIN, PHNAME, JERR)
               CALL DELEXT ('PL', DISKIN, SLOT, 'WRIT', CATBLK, BUFFI,
     *            PVER, JERR)
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IF: ',I2)
 1026 FORMAT (I3,' -',I3)
      END
      SUBROUTINE MLAB1 (WBLC, WTRC, BLC, TRC, CH, ILTYPE, XYR, IBUFF,
     *   WRVLAB, WRHLAB, IERR)
C-----------------------------------------------------------------------
C   MLAB1 controls some axis drawing and labeling functions:
C   labels each axis with RA/DEC or the 8-char type
C   call MTICS to draw tics & tick labels
C   Inputs:C
C      WBLC     R(2)    X, Y pixels of bottom left hand corner of
C                       whole page
C      WTRC     R(2)    X, Y pixels of top right hand corner of
C                       whole page
C      BLC      R(2)    X, Y pixels to form bottom left hand corner
C      TRC      R(2)    X, Y pixels to form the top right hand corner
C      CH       R(4)    left, bot, right, top : total character offsets
C      ILTYPE   I       label type: 1 none, 2 no ticks, 3 RA/DEC
C                          4 center relative
C      XYR      R       The ratio of the distance between X axis pixels
C                       and the distance between Y axis pixels on plot
C      WRVLAB   I       > 0 write the vertical label, every tick #
C      WRHLAB   L       T => write the horizontal numbers
C   In/out:
C      IBUFF    I(256)  the updated graphics output buffer.
C      IERR     I       error indicator: 0 = No error.
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), CH(4), XYR, WBLC(2), WTRC(2)
      INTEGER   ILTYPE, WRVLAB, IBUFF(256), IERR
      LOGICAL   WRHLAB
C
      REAL      X, X0, X1, Y, Y0, Y1, DCX, DCY
      INTEGER   I, IANGL, IERR2, INCHAR, LTYPE, NLABEL(9), IROUND, IE,
     *   IEPO, WRNUM
      DOUBLE PRECISION  ZERO
      CHARACTER SPRTXT*30, PLABEL(9)*16, CPT(2)*5, CTY(2)*20, SUBR*6,
     *   ELABEL(2)*8
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ZERO /0.0D0/
      DATA NLABEL /14, 13, 14, 13, 15, 11, 11, 10, 11/
      DATA PLABEL /'ECLIPTIC LONG.  ', 'ECLIPTIC LAT.   ',
     *   'GALACTIC LONG.  ', 'GALACTIC LAT.   ', 'RIGHT ASCENSION ',
     *   'DECLINATION     ', 'IAT (HOURS)     ', 'HA (HOURS)     ',
     *   'LST (HOURS)     '/
      DATA ELABEL /' (B1950)', ' (J2000)'/
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS (ILTYPE), 100)
      IF (LTYPE.EQ.1) GO TO 999
      IEPO = IROUND (REPOCH(LOCNUM))
      IF (IEPO.EQ.1950) THEN
         IE = 1
      ELSE IF (IEPO.EQ.2000) THEN
         IE = 2
      ELSE
         IE = 0
         END IF
C                                       Initial values.
      X0 = WBLC(1)
      X1 = WTRC(1)
      Y0 = WBLC(2)
      Y1 = WTRC(2)
C                                       vertical axes
      IF (((CPREF(2,LOCNUM).EQ.' ') .AND. (CTYP(2,LOCNUM).EQ.' ')) .OR.
     *   (CPREF(2,LOCNUM).EQ.'-1')) GO TO 10
         I = LABTYP(LOCNUM) / 10
         IF (I.GT.9) I = 0
         Y = (Y1-Y0)/2.0 + Y0
         CALL GPOS (X0, Y, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         IF (I.GT.0) THEN
            SPRTXT = PLABEL(I)
            INCHAR = NLABEL(I)
            IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6)))
     *         SPRTXT(INCHAR+1:) = ELABEL(IE)
         ELSE
            SPRTXT = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
            END IF
         CALL REFRMT (SPRTXT, '_', INCHAR)
         IANGL = 1
         DCX = -CH(1) + 0.5
         DCY = INCHAR / 2.0 - 1.0
         IF (WRVLAB.GT.0) THEN
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
            SUBR = 'GCHAR'
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       horizontal axes
 10   IF (((CPREF(1,LOCNUM).EQ.' ') .AND. (CTYP(1,LOCNUM).EQ.' ')) .OR.
     *   (CPREF(1,LOCNUM).EQ.'-1')) GO TO 20
         X0 = BLC(1)
         X1 = TRC(1)
         Y0 = BLC(2)
         Y1 = TRC(2)
         I = MOD (LABTYP(LOCNUM), 10)
         IF (I.GT.9) I = 0
         X = (X1-X0)/2.0 + X0
         CALL GPOS (X, Y0, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         IF (I.GT.0) THEN
            SPRTXT = PLABEL(I)
            INCHAR = NLABEL(I)
            IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6)))
     *         SPRTXT(INCHAR+1:) = ELABEL(IE)
         ELSE
            SPRTXT = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
            END IF
         CALL REFRMT (SPRTXT, '_', INCHAR)
         IANGL = 0
         DCX = -INCHAR / 2.0
         DCY = -2.833
         IF (LTYPE.EQ.2) DCY = -1.5
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
C                                       do ticks
 20   IF (LTYPE.NE.2) GO TO 30
         CPT(1) = CPREF(1,LOCNUM)
         CPT(2) = CPREF(2,LOCNUM)
         CTY(1) = CTYP(1,LOCNUM)
         CTY(2) = CTYP(2,LOCNUM)
         CPREF(1,LOCNUM) = ' '
         CPREF(2,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         CTYP(2,LOCNUM) = ' '
 30   IF (AXINC(1,LOCNUM).NE.0) THEN
         WRNUM = 0
         IF (WRHLAB) WRNUM = 1
         CALL MTICS (1, BLC, TRC, XYR, ZERO, IBUFF, WRNUM, IERR2)
         IF (IERR2.EQ.2) THEN
            IERR = 2
            GO TO 999
            END IF
         END IF
      IF (AXINC(2,LOCNUM).NE.0) THEN
         WRNUM = WRVLAB
         CALL MTICS (2, BLC, TRC, XYR, ZERO, IBUFF, WRNUM, IERR2)
         IF (IERR2.EQ.2) THEN
            IERR = 2
            GO TO 999
            END IF
         END IF
      IF (LTYPE.EQ.2) THEN
         CPREF(1,LOCNUM) = CPT(1)
         CPREF(2,LOCNUM) = CPT(2)
         CTYP(1,LOCNUM) = CTY(1)
         CTYP(2,LOCNUM) = CTY(2)
         END IF
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1000) IERR, SUBR
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MLAB1: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
      END
      SUBROUTINE MTICS (LAXIS, BLC, TRC, XYRATO, YX, IBUFF, WRNUM, IERR)
C-----------------------------------------------------------------------
C   MTICS writes tick marks and tick labels to a plot file.
C   If CPREF(IAX) and CTYP(IAX) are all blank, no tick labels are done.
C   Inputs:
C      LAXIS   I        1 => horizontal,  2 => vertical full plots
C                       3 => horiz subplot 4 => vertical subplot
C      BLC     R(2)     X and Y pixels to form bottom left hand
C                       corner of the graph.
C      TRC     R(2)     X and Y pixels to form the top right hand
C                       corner of the graph.
C      XYRATO  R        X to Y scaling factor
C      YX      D        LAXIS=3: plot x axis at y = YX; 4: plot y axis
C                       at x = YX - out range => BLC(1,2) value
C      WRNUM   I        Write numbers WRNUM ticks
C   In/out:
C      IBUFF   I(256)   buffer being used for output to
C                       the graphics file.
C   Outputs:
C      IERR    I        error code: 0 => ok
C                                   1 => bad IAXIS
C                                   2 => graph drawing error
C                                   3 => tic algorithm fails
C-----------------------------------------------------------------------
      INTEGER   LAXIS, WRNUM, IBUFF(256), IERR
      REAL      BLC(2), TRC(2), XYRATO
      DOUBLE PRECISION YX
C
      CHARACTER SPRTXT*80, CHDL*4, SUBR*6
      DOUBLE PRECISION DEG, DEGC, DTX, DX, DTY, DY, PT5SEC, TICX, TICY,
     *   DEGC0, DEG0, DEGC1, LDX, LDY, LLDX, LLDY, UPLIM, LOLIM, AYX
      REAL      DCX, DSP, X, Y, TICT, DCY, XT, YT, TICL
      INTEGER   HML(2), IAXIS, AXISTP, I, IANGL, ILEN, ITRY, COOTYP,
     *   INOI, JERR, KERR, LWRN
      LOGICAL   NONUM, FIRST
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      IAXIS = LAXIS
      IF (LAXIS.GT.2) IAXIS = IAXIS - 2
      LWRN = WRNUM
C                                       Assign initial values.
      IANGL = 0
C                                       vertical
      IF (IAXIS.EQ.2) THEN
         AYX = YX
         CALL TICINC (LAXIS, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 995
         UPLIM = 1.E20
         LOLIM = -1.E20
         IF ((AXTYP(LOCNUM).EQ.1) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
            IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.6)) THEN
               UPLIM = 90.0D0
               LOLIM = -90.0D0
               END IF
            IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.5)) THEN
               UPLIM = RPVAL(2,LOCNUM) + 180.0D0
               LOLIM = RPVAL(2,LOCNUM) - 180.0D0
               END IF
            END IF
         DCX = -1.0
         DCY = -0.5
         AXISTP = LABTYP(LOCNUM) / 10
C                                       horizontal
      ELSE IF (IAXIS.EQ.1) THEN
         DCX = 0.5
         DCY = -1.5
         AYX = YX
         CALL TICINC (LAXIS, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 995
         IF ((AXTYP(LOCNUM).EQ.1) .OR. (AXTYP(LOCNUM).NE.2)) THEN
            IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.4)) THEN
               UPLIM = 90.0D0
               LOLIM = -90.0D0
               END IF
            IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.3)) THEN
               UPLIM = RPVAL(1,LOCNUM) + 180.0D0
               LOLIM = RPVAL(1,LOCNUM) - 180.0D0
               END IF
            END IF
         AXISTP = MOD (LABTYP(LOCNUM), 10)
      ELSE
         GO TO 990
         END IF
      TICX = 1.2 * TICX
      TICY = 1.2 * TICY
      TICL = 1.2 * TICL
C                                       Determine FRRAible tic intervls
      NONUM = (CPREF(IAXIS,LOCNUM).EQ.' ') .AND.
     *   (CTYP(IAXIS,LOCNUM).EQ.' ')
C                                       Always write the numbers
      NONUM = WRNUM.LE.0
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.GE.7)) COOTYP = 1
      LDX = -1.D10
      LLDX = -1.D10
      LDY = -1.D10
      LLDY = -1.D10
      LWRN = MIN (LWRN, INOI - 1)
      IF (LWRN.EQ.0) LWRN = 1
C                                       Draw tic marks and values.
      HML(1) = -32000
      HML(2) = -32000
      CHDL = '$'
      DEGC0 = DEGC
      FIRST = .TRUE.
      DEG0 = DEG
      DEGC1 = DEGC0 - DEG
      DO 290 I = 1,INOI
         IF (LAXIS.EQ.2) THEN
            DY = DEGC
            CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 285
         ELSE IF (LAXIS.EQ.1) THEN
            DX = DEGC
            CALL FNDY (BLC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 285
         ELSE IF (LAXIS.EQ.4) THEN
            DY = DEGC
            DX = AYX
         ELSE
            DX = DEGC
            DY = AYX
            END IF
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 285
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 285
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 285
         IF (DX.NE.LDX) LLDX = LDX
         IF (DY.NE.LDY) LLDY = LDY
         LDX = DX
         LDY = DY
         CALL GPOS (X, Y, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
C                                       Find end of tic.
         DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 235
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF (TICL.LE.0.) TICL = 1.
         IF ((TICT.LE.TICL) .AND. (TICT.GE.0.1*TICL)) GO TO 221
            IF (TICT.EQ.0.0) GO TO 235
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 235
 221     IF ((XT.LT.BLC(1)-0.01) .OR. (XT.GT.TRC(1)+0.01)) GO TO 235
         IF ((YT.LT.BLC(2)-0.01) .OR. (YT.GT.TRC(2)+0.01)) GO TO 235
C                                       Simple tick
C                                       Position at end of tic.
         CALL GPOS (XT, YT, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
         CALL GVEC (X, Y, IBUFF, IERR)
         SUBR = 'GVEC'
         IF (IERR.NE.0) GO TO 980
         DEGC1 = DEGC - DEG
         IF (FIRST) DEGC0 = DEGC
         FIRST = .FALSE.
C                                       Convert degrees to DEC/RA.
 235     IF (NONUM) GO TO 285
C                                       labels offset
         IF ((LAXIS.NE.3) .AND. (LAXIS.NE.4)) GO TO 245
            IF (LAXIS.EQ.3) CALL FNDY (BLC(2), DX, DY, JERR)
            IF (LAXIS.EQ.4) CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 285
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 285
C                                       Position for labels
 245     CALL GPOS (X, Y, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *      SPRTXT, ILEN)
         DSP = DCX - ILEN
         IF (MOD(INOI-I,LWRN).EQ.0) THEN
            CALL GCHAR (ILEN, IANGL, DSP, DCY, SPRTXT, IBUFF, IERR)
            SUBR = 'GCHAR'
            IF (IERR.NE.0) GO TO 980
            END IF
 285     DEGC = DEGC - DEG
 290     CONTINUE
C                                       No top/right in subplots
      IF ((LAXIS.EQ.3) .OR. (LAXIS.EQ.4)) GO TO 999
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
      IF (IAXIS.EQ.1) GO TO 310
         CALL TICINC (6, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 395
         GO TO 320
C                                       horizontal
 310  CONTINUE
         CALL TICINC (5, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 395
 320  IF (INOI.LE.0) GO TO 395
C                                       Loop for other border.
      DO 390 I= 1,INOI
         IF (IAXIS.EQ.1) GO TO 330
            DY = DEGC
            CALL FNDX (TRC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 380
            GO TO 340
 330     CONTINUE
            DX = DEGC
            CALL FNDY (TRC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 380
C                                       Convert degrees to pixels.
 340     CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 380
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 380
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 380
C                                       Find end of tic.
         DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 380
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF ((TICT.LE.TICL) .AND. (TICT.GE.0.1*TICL)) GO TO 341
            IF (TICT.EQ.0.0) GO TO 380
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 380
 341     IF ((XT.LT.BLC(1)-0.01) .OR. (XT.GT.TRC(1)+0.01)) GO TO 380
         IF ((YT.LT.BLC(2)-0.01) .OR. (YT.GT.TRC(2)+0.01)) GO TO 380
C                                       Simple ticks only
C                                       Position at end of tic.
         CALL GPOS (XT, YT, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
         CALL GVEC (X, Y, IBUFF, IERR)
         SUBR = 'GVEC'
         IF (IERR.NE.0) GO TO 980
 380     DEGC = DEGC - DEG
 390     CONTINUE
C                                       Full curves from top?
 395  GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR, SUBR
      CALL MSGWRT (7)
      IERR = 2
      GO TO 999
C                                       Invalid axis type.
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C                                       bad ticks algorithm
 995  IERR = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('MTICS: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
 1990 FORMAT ('MTICS: INVALID AXIS TYPE.')
      END
      SUBROUTINE REFRSH (NPARMS, LAST, IERR)
C-----------------------------------------------------------------------
C Routine to refresh the plot page, i.e. open a new plot file, reset
C counters etc.
C-----------------------------------------------------------------------
      INTEGER NPARMS, IERR
      LOGICAL LAST
      REAL    PTEMP(4)
C
      INTEGER  DEPTH(5), INP, IRET
      INCLUDE 'FRPLT.INC'
      INCLUDE 'FRPL2.INC'
      INCLUDE 'FRPL3.INC'
      INCLUDE 'FRPL4.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      CLOSED = .FALSE.
C                                       Ensure correct scaling parms
C                                       written to PL file
      CALL RCOPY (4, APARM(3), PTEMP(1))
      CALL PLCREA (NPARMS, PVER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IF (DOTV) THEN
         XYRATI = (WINDTV(3) - WINDTV(1) + 1.0) /
     *      (WINDTV(4) - WINDTV(2) + 1.0)
      ELSE
         XYRATI = 9 / 5.5
         WRITE (MSGTXT,1010) PVER
         CALL MSGWRT (5)
         END IF
      CALL RCOPY (4, PTEMP(1), APARM(3))
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL RFILL (4, 0.5, CHOUT)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) THEN
         NCOUNT = -1 - ABS(NCOUNT)
         CALL LABLAX (LAST, IRET)
         NCOUNT = -NCOUNT - 1
         CALL CHNTIC (BLC, TRC, INP)
         IF (CODTYP.EQ.1) INP = MAX (INP, 4)
         CHOUT(1) = INP + 4.0
         END IF
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(2) = CHOUT(2) + 1.333
         IF ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9))
     *      CHOUT(2) = CHOUT(2) + 1.333
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4))
     *       CHOUT(2) = CHOUT(2) + 1.333
         IF (NCOUNT.GE.1) CHOUT(2) = CHOUT(2) + 1.333
         CHOUT(4) = 3.333
         IF (LABEL.GT.1) CHOUT(4) = CHOUT(4) + 1.333
         END IF
C                                        Init. for line drawing
      CALL GINITL (BLC, TRC, XYRATI, CHOUT, DEPTH, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REFRSH: ERROR ',I3,' RECEIVED FROM PLCREA')
 1010 FORMAT ('Plot file version ',I3,' created')
      END
      SUBROUTINE FILANT (DISK, CNO, CATBLK, LUN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, NSUBA, DOACOR, DOXCOR, NBASE, ANT1, ANT2, STNS,
     *   SCRTCH, ANTENS, IRET)
C-----------------------------------------------------------------------
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned. Also fills in 2
C   arrays with all FRRAible cominations of antenna numbers
C   Inputs:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C      IXANT    I(50)    List of user supplied antennas
C      IXBASL   I(50)    Baselines to match XANTEN
C      NXANT    I        # entries in XANTEN
C      NXBASL   I        # entries in XBASE
C      DESEL    L        True if entries are to be de-selected rather
C                        than selected
C      NSUBA    I        Subarray used
C      DOACOR   L        Do autocorrelations?
C      DOXCOR   L        Do cross-correlations?
C      IRET     I        0 normal, 10 => no AN files
C   Output:
C      NBASE    I        Max # baselines
C      ANT1     I(*)     1st antenna number of baseline pairs selected
C      ANT2     I(*)     2nd antenna number of baseline pairs selected
C      STNS     C(*)*8   station names
C      SCRTCH   I(512)   I/O buffer and related storage.
C      ANTENS   I(*)     antenna numbers requested
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), LUN, IXANT(50), IXBASL(50),
     *   NXANT, NXBASL, NSUBA, NBASE, ANT1(*), ANT2(*), SCRTCH(512),
     *   ANTENS(*), IRET
      LOGICAL   DESEL, DOACOR, DOXCOR
      CHARACTER STNS(*)*8
C
      INTEGER   NBUFF, II, NUMREC, J, MXNSTA, I1, IERR, ICNT, NOUT,
     *   ANTNDX(50), NONZER, I
      LOGICAL   ACCEPT, REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       Set default results.
      CALL FILL (MXBASE, 0, ANT1)
      CALL FILL (MXBASE, 0, ANT2)
      NBUFF = 1024
      NBASE = 1
C
C                                       read the antenna file
C                                       Open file
      IF (IRET.EQ.0) THEN
         CALL ANTINI ('READ', SCRTCH, DISK, CNO, NSUBA, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
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
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ', NSUBA
               GO TO 990
               END IF
            MXNSTA = MAX (NOSTA, MXNSTA)
            STNS(NOSTA) = ANNAME
            IF ((NXANT.EQ.0) .AND. (NXBASL.EQ.0)) THEN
               ICNT = ICNT + 1
               IXANT(ICNT) = NOSTA
               END IF
 10         CONTINUE
C                                       Close
         CALL TABIO ('CLOS', 0, II, SCRTCH, SCRTCH, IERR)
      ELSE
         MXNSTA = 50
         END IF
      IRET = 0
C                                       Fill up the baseline arrays
      DO 20 I1 = 1,MXNSTA
         DO 15 J = I1,MXNSTA
            IF (((I1.LT.J) .AND. (DOXCOR)) .OR.
     *         ((DOACOR) .AND. (I1.EQ.J))) THEN
               ACCEPT = REQBAS (I1, J, DESEL, IXANT, NXANT, IXBASL,
     *            NXBASL)
               IF (ACCEPT) THEN
                  ANT1(NBASE) = I1
                  ANT2(NBASE) = J
                  NBASE = NBASE + 1
                  ANTENS(I1) = I1
                  ANTENS(J) = J
                  END IF
               END IF
 15         CONTINUE
 20      CONTINUE
      NBASE = NBASE - 1
C                                       Sort out ANTENS
      CALL COPY (50, ANTENS, ANTNDX)
      NONZER = 0
      DO 50 I = 1,50
         IF (ANTNDX(I).GT.0) THEN
            NONZER = NONZER + 1
            ANTENS(NONZER) = ANTNDX(I)
            END IF
 50      CONTINUE
      CALL IMERGE (50, ANTENS, NOUT)
      CALL COPY (50, ANTENS, ANTNDX)
      NONZER = 0
      CALL FILL (50, 0, ANTENS)
      DO 60 I = 1,50
         IF (ANTNDX(I).GT.0) THEN
            NONZER = NONZER + 1
            ANTENS(NONZER) = ANTNDX(I)
            END IF
 60      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILANT: ERROR',I3,1X,A4,'ING AN FILE ',I5)
      END
      SUBROUTINE NXSET (DISKIN, CNOIN, TIMRNG, SOUWAN, DOSWNT, NSOUWD,
     *   SUBARR, FRQSEL, IRET)
C-----------------------------------------------------------------------
C   Routine to read the index table and set up the arrau NXVISN which
C   is used to determine the scan boundaries.
C   Input:
C      DISKIN        I     Data disc number
C      CNOIN         I     Data catalogue number
C      TIMRNG        R(8)  Start/stop time between which to form
C                          scan information
C      SOUWAN        I(*)  source numbers wanted
C      DOSWNT        L     keep them (T) or reject them (F)
C      NSOUWD        I     # entries in SOUWAN array
C      SUBARR        I     required subarray
C      FRQSEL        I     required FQID
C   Output:
C      IRET          I     0 => OK, anything else = fails
C   Output in common:
C      NXVISN(2,*)   I     1,* => first vis number of scan n
C                          2,* => last vis number of scan n
C      NXTIM(2,*)    R     start and finish times of scan n
C      NXSOU(*)      I     source of scan
C-----------------------------------------------------------------------
      INTEGER DISKIN, CNOIN, NSOUWD, SOUWAN(*), SUBARR, FRQSEL, IRET
      REAL    TIMRNG(8)
      LOGICAL DOSWNT
C
      INTEGER NUMNXT, I, IDSOUR, ISUBA, VSTART, VEND, FREQID, NUMACT,
     *   J
      REAL    TIME, DTIME, START, STOP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FRRANX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      CALL FNDEXT ('NX', CATBLK, NUMNXT)
      IF (NUMNXT.EQ.0) THEN
        INDXT = .FALSE.
        MSGTXT = 'YOU ARE TRYING TO SCAN AVERAGE WITHOUT AN NX TABLE'
        CALL MSGWRT (6)
        MSGTXT = 'EITHER RUN INDXR, OR CHANGE SOLINT'
        CALL MSGWRT (6)
        IRET = 1
        GO TO 999
        END IF
C                                        Set timerange
      START = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *  + TIMRNG(4) / (24. * 60. * 60.)
      STOP = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *   + TIMRNG(8) / (24. * 60. * 60.)
      IF (START.EQ.0.0) START = -1.0E10
      IF (STOP.EQ.0.0) STOP = 1.0E10
      NUMACT = 0
C
      INDXT = .TRUE.
      NXVER = 1
      CALL NDXINI ('READ', BUFFNX, DISKIN, CNOIN, NXVER, CATBLK, NXLUN,
     *   IRNONX, KOLSNX, NUMVNX, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C
      NUMNX = BUFFNX(5)
      IF (NUMNX.EQ.0) THEN
         INDXT = .FALSE.
         GO TO 999
         END IF
      IF (NUMNX .GT. MAXNX) THEN
         IRET = 1
         WRITE (MSGTXT,1010) NUMNX
         GO TO 990
         END IF
C                                       Read and load NX entries
      DO 100 I = 1,NUMNX
         IRNONX = I
         CALL TABNDX ('READ', BUFFNX, IRNONX, KOLSNX, NUMVNX, TIME,
     *      DTIME, IDSOUR, ISUBA, VSTART, VEND, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       check subarray
         IF ((SUBARR.GT.0) .AND. (ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR))
     *      GO TO 100
C                                       check FQID
         IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND. (FRQSEL.NE.FREQID))
     *      GO TO 100
C                                       check time range
         IF (((TIME-0.5*DTIME).GT.STOP).OR.
     *      ((TIME+0.5*DTIME).LT.START)) GO TO 100
C                                       check sources
         IF (NSOUWD.GT.0) THEN
            DO 70 J = 1,NSOUWD
               IF (IDSOUR.EQ.SOUWAN(J)) THEN
                  IF (DOSWNT) GO TO 90
                  GO TO 100
                  END IF
 70            CONTINUE
C                                       Not in list
            IF (.NOT.DOSWNT) GO TO 90
            GO TO 100
            END IF
 90      NUMACT = NUMACT + 1
         NXVISN(1,NUMACT) = VSTART
         NXVISN(2,NUMACT) = VEND
         NXTIM(1,NUMACT) = TIME - 0.5 * DTIME
         NXTIM(2,NUMACT) = TIME + 0.5 * DTIME
         NXSOU(NUMACT) = IDSOUR
 100     CONTINUE
C
      NUMNX = NUMACT
      CALL TABIO ('CLOS', 0, IRNONX, BUFFNX, BUFFNX, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NXSET: ERROR ',I4,' OPENING NX TABLE')
 1010 FORMAT ('NXSET: ',I8,' NX ENTRIES TOO LARGE, INCREASE MAXNX')
 1020 FORMAT ('NXSET: ERROR ',I4,' READING NX TABLE')
      END
      SUBROUTINE FRSPEC (NDATA, NPTS, NMISS, XDATA, ISIGN, XWORK)
C--------------------------------------------------------------------
C   Generate a fringe rate spectrum from a visibility time series.
C   The time origin is taken as the midpoint in the series. The
C   fringe rate spectrum is normalised by 1/(NPTS-NMISS).
C   Input parameters:
C     NDATA       I       No of points to FFT (including zero padding)
C     NPTS        I       No of actual data points (excluding zero
C                         padding but including missing values).
C                         Expected to be even.
C     NMISS       I       No of missing values within NPTS.
C     ISIGN       I       Sign of transform (-1, +1)
C   Input/output parameters:
C     XDATA    R(NDATA)   On input contains NPTS  actual data points
C                         in increasing time order / contains fringe
C                         rate spectrum on output.
C     XWORK   R(2,NDATA)  Work buffer.
C--------------------------------------------------------------------
      REAL XDATA(*), XWORK(*)
      INTEGER NDATA, NPTS, ISIGN, NMISS
C
      REAL    FACT, X, WS, W
      INTEGER N, N2, J, JLIM, JD, K, KD, NZERO, JLIM1, JLIM2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
      IF (NMISS.GT.0) THEN
         N2 = 2 * NPTS
         WRITE (MSGTXT,1000) NMISS, NPTS
         CALL MSGWRT (6)
         DO 40 J = 1,N2
            IF (XDATA(J).EQ.FBLANK) THEN
               K = J
               X = 0.0
               WS = 0.0
 10            K = K - 2
               IF (K.GT.0) THEN
                  IF (XDATA(K).EQ.FBLANK) GO TO 10
                  W = 1.0 / (J - K)
                  X = XDATA(K) * W
                  WS = W
                  END IF
               K = J
 15            K = K + 2
               IF (K.LE.N2) THEN
                  IF (XDATA(K).EQ.FBLANK) GO TO 15
                  W = 1.0 / (K - J)
                  X = X + XDATA(K) * W
                  WS = WS + W
                  END IF
               IF (WS.GT.0.) THEN
                  XDATA(J) = X / WS
               ELSE
                  XDATA(J) = 0.0
                  END IF
               END IF
 40         CONTINUE
         END IF
C                                       Reduce NPTS by one if odd
      N = NPTS
      IF (MOD(N,2) .GT. 0) N = NPTS - 1
      N2 = N / 2
C                                       Fold time sequence about
C                                       mid-point which is assumed
C                                       to be the origin.
C                                       First copy -ve times to end
C                                       of FFT array.
      JLIM = N2 - 1
      DO 50 J = 1,JLIM
         JD = 2 * J - 1
         K = NDATA - JLIM + J
         KD = 2 * K - 1
         XWORK(KD) = XDATA(JD)
         XWORK(KD+1) = XDATA(JD+1)
  50     CONTINUE
C                                       Copy +ve times to start of
C                                       FFT array.
      K = 0
      DO 100 J = N2, N
         JD = 2 * J - 1
         K = K + 1
         KD = 2 * K - 1
         XWORK(KD) = XDATA(JD)
         XWORK(KD+1) = XDATA(JD+1)
 100     CONTINUE
C                                       Fill in zero padding.
      NZERO = NDATA - N
      JLIM1 = N2 + 2
      JLIM2 = N2 + NZERO + 1
      DO 150 J = JLIM1, JLIM2
         JD = 2 * J - 1
         XWORK(JD) = 0.0
         XWORK(JD+1) = 0.0
 150     CONTINUE
C                                       Fourier transform FFT array.
      CALL FOURG (XWORK, NDATA, ISIGN, XDATA)
C                                       Fold fringe rate spectrum to
C                                       put 0 freq in centre channel.
      FACT = 1.0 / N
      JLIM = NDATA / 2
      DO 200 J = 1,NDATA
         K = JLIM + J + 1
         IF (K.GT.NDATA) K = K - NDATA
         JD = 2 * J - 1
         KD = 2 * K - 1
         XDATA(JD) = FACT * XWORK(KD)
         XDATA(JD+1) = FACT * XWORK(KD+1)
 200     CONTINUE
C
C                                       Exit
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRSPEC: interpolating over',I6,' empty time bins from',
     *   I7)
      END

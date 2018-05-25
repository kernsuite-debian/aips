LOCAL INCLUDE 'POSSM.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, OFILE*48, XSOUR(30)*16, XCALCO*4,
     *   HISCRD(10)*72, STNS(MAXANT)*8, SAUCE*16, PPLOT*8, VELSUB*8,
     *   JY*2, STKLAB*4, POLLAB(4)*7
      HOLLERITH XNAMEI(3), XCLAIN(2), XOFILE(12), XXSOUR(4,30), XXSTOK,
     *   XXCALC, XCODET, XPPLOT(2)
      REAL      USERID, XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID,
     *   XUVR(2), XTIME(8), XBIF, XEIF, XBCHAN, XECHAN, XSUBA,
     *   XANTEN(50), XBASE(50), XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), SHIFT(2), APARM(10), XSOLIN,
     *   XNCOUN, BPARM(10), XLABEL, FACTOR, XYRTIO, XBADD(10), XDOTV,
     *   XGRCH
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, PLTBLK(256), PVER,
     *   NUMHIS, PBIF, PEIF, PLMAP, PLFIND, STRTIF, SLOT, STOPIF, GRCHN,
     *   TVCHN, TVCORN(4), IGLUN, IGFIND, NUMFRQ, PBCH, PECH, NOMIT,
     *   CODTYP, POLPLT, NCOUNT, XA1(MXBASE), XA2(MXBASE), NBASE,
     *   ICURNT, NUMXCF, CATSAV(256), NEXTSC, PFQSID(MAXIF),
     *   CBCHAN, CECHAN, LABEL, PLSTVS, CHNUM, NPNL, POLNUM, STRPOL,
     *   STOPOL, CROSS, BUFFER(512)
      REAL   BLC(2), TRC(2), BUFF1(1024), SCAMP(MAXCIF), CHOUT(4),
     *   BUFF2(2,2*MAXCIF), WTS(MAXCIF), KEEPTM(8), UVSCAL, STARTD,
     *   STOPD, STARTU, STOPU, PFQTBW(MAXIF), PFQCHW(MAXIF),
     *   FINC(MAXIF), PSMTAB(256)
      DOUBLE PRECISION FOFF(MAXIF), PFQFRQ(MAXIF)
      LOGICAL MULTI, SCALAR, AUTO, BPPLOT, DIVCH0, DOCHIF, DOTV, DOSHFT,
     *   BLNKBF, ACF, XCF, SCANAV, REVERS, FIXED, SELFSA, SELFSP, AUTO1,
     *   DOCHPL, DIDCHN, BDPLOT, PDPLOT, CPPLOT, TAPLOT, CPPLTI, PCPLOT,
     *   NEWPAG, SMTHIT
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR,
     *   XQUAL, XXCALC, XBAND, XFREQ, XFQID, XUVR, XTIME, XXSTOK, XBIF,
     *   XEIF, XBCHAN, XECHAN, XSUBA, XANTEN, XBASE, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, SHIFT,
     *   APARM, XCODET, XPPLOT, XSOLIN, XNCOUN, BPARM, XOFILE, XLABEL,
     *   FACTOR, XYRTIO, XBADD, XDOTV,  XGRCH
      COMMON /OTHPRM/ SEQIN, SEQOUT, DISKIN, CNOIN, PBIF, PEIF, PLMAP,
     *   PLFIND, STRTIF, STOPIF, NUMFRQ, PBCH, PECH,  NOMIT, CODTYP,
     *   POLPLT, NCOUNT, XA1, XA2, NBASE, ICURNT, NUMXCF, NEXTSC, LABEL,
     *   PLSTVS, CHNUM, NPNL, POLNUM, STRPOL, STOPOL, NEWPAG, SMTHIT,
     *   CROSS
      COMMON /LOGS/ SCALAR, AUTO, BPPLOT, DIVCH0, DOCHIF, ACF, XCF,
     *   SCANAV, REVERS, FIXED, SELFSA, SELFSP, AUTO1, DOCHPL, DIDCHN,
     *   BDPLOT, PDPLOT, CPPLOT, TAPLOT, CPPLTI, PCPLOT
      COMMON /CHPARM/ NAMEIN, CLAIN, OFILE, XSOUR, XCALCO, HISCRD,
     *   STNS, SAUCE, PPLOT, VELSUB, JY, STKLAB, POLLAB
      COMMON /PLOT/ PLTBLK, BLC, TRC, CHOUT, DOTV, DOSHFT, BLNKBF,
     *   GRCHN, TVCHN, TVCORN, IGLUN, IGFIND, SLOT
      COMMON /LABEL/ MULTI, PVER
      COMMON /BUFRS/  CATSAV, FOFF, PFQFRQ, BUFFER, BUFF1, SCAMP, BUFF2,
     *   WTS, KEEPTM, STARTD, STOPD, STARTU, STOPU, PFQTBW, PFQCHW,
     *   PFQSID, NUMHIS, CBCHAN, CECHAN, FINC, UVSCAL, PSMTAB
LOCAL END
LOCAL INCLUDE 'POSS2.INC'
      CHARACTER ANTNAM(2)*8, EXPDAT*8, SRCOBS*16
      INTEGER   NCHAN, TELNUM(2), RAHR, RAMIN, DECDEG, DECMIN
      REAL      CTIME, TBW, AVWGHT, RASEC, DECSEC
      DOUBLE PRECISION MFREQ
      COMMON /OUTCHR/ ANTNAM, EXPDAT, SRCOBS
      COMMON /OUTDAT/ MFREQ, CTIME, TBW, AVWGHT, RASEC, DECSEC,
     *   NCHAN, TELNUM, RAHR, RAMIN, DECDEG, DECMIN
LOCAL END
LOCAL INCLUDE 'POSS3.INC'
      INTEGER NCTOT, NXP, NYP
      CHARACTER ALABEL(2)*16, APREF(2)*8
      REAL   AMP(MAXCIF), PHASE(MAXCIF), MAXPHS, MINPHS, MAXAMP, MINAMP,
     *   AMPRNG, PHSRNG, LINT, XLINT, XYRATI, YBLC(2), YTRC(2), XMULT,
     *   VELINC, VELFPX
      LOGICAL   LASTP
      COMMON /PLPARM/ AMP, PHASE, MAXPHS, MINPHS, MAXAMP, MINAMP, LINT,
     *   XLINT, AMPRNG, PHSRNG, XYRATI, YBLC, YTRC, XMULT, VELINC,
     *   VELFPX, NCTOT, NXP, NYP, LASTP
      COMMON /AXLABS/ ALABEL, APREF
LOCAL END
LOCAL INCLUDE 'POSS4.INC'
      INTEGER NXPANE, NYPANE, NXPIX, NYPIX, ICOUNT, JCOUNT
      REAL    PLTXIN, PLTYIN
      LOGICAL CLOSED
      COMMON /FRESH/ PLTXIN, PLTYIN, NXPANE, NYPANE, NXPIX, NYPIX,
     *   ICOUNT, CLOSED, JCOUNT
LOCAL END
LOCAL INCLUDE 'POSSNX.INC'
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
      PROGRAM POSSM
C-----------------------------------------------------------------------
C! POSSM plots spectra of uv-data and BP tables.
C# Calibration Graphics Sdish Spectral UV VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2017
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
C   Selects a range of line data and plots either the vector-averaged,
C   or the scalar-averaged cross-spectrum, or autocorrelation spectra.
C   Calibration and editing may be optionally applied.
C   Inputs:
C      AIPS adverb          Description.
C      USERID.....User number  ignored
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
C                    1 = 0 => scalar average
C                    1 > 0 => vector average
C                    2 = 0 => self-scale
C                    2 > 0 => fixed scale
C                    3 = minimum ampl.
C                    4 = maximum ampl.
C                    5 = minimum phase.
C                    6 = maximum phase.
C                    7 = 0 => x label in channels
C                      = 1 => x label in Hz
C                      = 2 => x label in km/s
C                    8 = 0 => plot visibility data
C                      = 1 => plot autocorrelation data
C                      = 2 => plot BP table data
C                      = 3 => ACF
C                      = 4 => XCF
C                    9 = 1 => plot all IF's in one spectrum
C                   10 = 1 => reverse direction of plotted spectrum
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
      INTEGER   IRET, NPARMS, IIF, KANT, JIF, IPOL, LPOLNM, LPEIF, LANT,
     *   JRET
      REAL      TSOLIN
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSSNX.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'POSSM '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      DIDCHN = .FALSE.
      CALL POSSIN (PRGM, NPARMS, IRET)
      IF (IRET.NE.0) GO TO 990
      TSOLIN = XSOLIN / (60.0 * 24.0)
C                                       Single plot files
      IF (NCOUNT.EQ.0)  THEN
C                                       Bandpass plotting
 10      IF (BPPLOT) THEN
            KANT = 0
            CALL POSSBP (KANT, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.NE.0) GO TO 990
         ELSE IF (BDPLOT) THEN
            KANT = 0
            LANT = 0
            CALL POSSBD (KANT, LANT, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.NE.0) GO TO 990
         ELSE IF (PDPLOT) THEN
            KANT = 0
            CALL POSSPD (KANT, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.NE.0) GO TO 990
         ELSE IF (PCPLOT) THEN
            KANT = 0
            CALL POSSPC (KANT, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.NE.0) GO TO 990
C                                       Spectral plotting
         ELSE
            ICURNT = 1
            NOMIT = 0
            CALL POSSUV (IRET)
            IF ((IRET.EQ.5) .AND. (XSOLIN.NE.0.0)) GO TO 200
            IF (IRET.NE.0) GO TO 990
            END IF
         DO 15 IIF = 1,50
            XANTEN(IIF) = ANTENS(IIF)
 15         CONTINUE
C                                       Clear write status from POSSIN
         UTYPE = 'UV'
         CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN,
     *      UTYPE, NLUSER, 'CLWR', BUFF1, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.10)) GO TO 990
C                                       Plot the result
         LPEIF = PEIF
         LPOLNM = POLNUM
         IF (CPPLOT) LPOLNM = 1
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
            CALL POSSPL (NPARMS, JIF, IRET)
            IF (IRET.GT.0) GO TO 990
C                                       Close down map file, because
C                                       was opened in PLCREA, and may
C                                       need to open it again
            CALL MAPCLS ('READ', DISKIN, CNOIN, PLMAP, PLFIND,
     *         CATBLK, .FALSE., BUFF1, JRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 990
               END IF
            IRET = JRET
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
            ELSE IF (TAPLOT) THEN
               STARTU = STOPU + 0.02 / (24. * 60. * 60.)
               STOPU = STARTU + TSOLIN
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
      SUBROUTINE POSSIN (PRGN, NPARM, JERR)
C-----------------------------------------------------------------------
C   POSSIN gets input parameters for POSSM, finds input file and
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
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSSNX.INC'
      CHARACTER STAT*4, PRGN*6, UTYPE*2, CODET*4, VSTNS(MAXANT)*8,
     *   BNDCOD(MAXIF)*8, TTYPE*2
      INTEGER   JERR,  NPARM, IROUND, IERR, I, LUNTB, IVER, IANT, NANT,
     *   LUNAN, IABUF(512), I4TEMP, IUSER, LUN, NXANT, NXBASL,
     *   IXANT(50), IXBASL(50), J1, J2, CVER, LUNP, TVER, ISBAND(MAXIF),
     *   ISTOKE, DROUND, LTYPE, NSTOKE
      REAL      CATR(256), EPS, TSOLIN
      LOGICAL   F, TABLE, FITASC, MATCH, TOTAL, DESEL, EXIST
      DOUBLE PRECISION CATD(128), DSTOK
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (CATR, CATD, CATBLK)
      DATA F /.FALSE./
      DATA LUNTB, LUNAN, LUN / 49, 27, 28 /
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NEWPAG = .TRUE.
      NUMHIS = 0
      NOMIT = 0
      VELSUB = ' '
      ISVLBA = .FALSE.
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
      IF (CODET.EQ.'LA&P') CODTYP = 2
      IF (CODET.EQ.'AMP ') CODTYP = 3
      IF (CODET.EQ.'LAMP') CODTYP = 4
      IF (CODET.EQ.'PHAS') CODTYP = 5
      IF (CODET.EQ.'R&I ') CODTYP = 6
      IF (CODET.EQ.'REAL') CODTYP = 7
      IF (CODET.EQ.'IMAG') CODTYP = 8
      IF (BPARM(5).LE.0.0) BPARM(5) = 1.0
      FIXED = (APARM(2).GT.0.0) .AND. (APARM(3).LT.APARM(4)) .AND.
     *   (APARM(5).LT.APARM(6))
      SELFSA = (APARM(2).LE.0.0) .OR. (APARM(3).GE.APARM(4))
      SELFSP = (APARM(2).LE.0.0) .OR. (APARM(5).GE.APARM(6))
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
      IF ((LTYPE.EQ.0) .OR. (ABS(LTYPE).GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
      NCOUNT = IROUND (XNCOUN)
      IF (NCOUNT.GT.9) THEN
         MSGTXT = 'NPLOTS TOO LARGE, CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       check status
      CALL CATDIR ('INFO', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR, NAMEIN, CLAIN, SEQIN, DISKIN
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
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
C                                       Save input header
      CALL COPY (256, CATBLK, CATUV)
      CALL COPY (256, CATBLK, CATSAV)
C                                       Determine if multi-source file
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *      MULTI, FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       no solint on BP like plots
      I = IROUND (APARM(8))
      IF ((I.EQ.2) .AND. (XSOLIN.NE.0.0)) XSOLIN = -1.0
      IF ((I.EQ.5) .AND. (XSOLIN.NE.0.0)) XSOLIN = -1.0
      IF ((I.EQ.9) .AND. (XSOLIN.NE.0.0)) XSOLIN = -1.0
      IF ((I.GE.6) .AND. (I.LE.8)) XSOLIN = 0.0
      IF ((ISORT(1:1).NE.'T') .AND. (XSOLIN.NE.0.0)) THEN
         MSGTXT = 'SOLINT NOT 0 REQUIRES TIME-ORDERED DATA NOT '
     *      // ISORT
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
         END IF
C                                       Save header and pointers
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Plotting control
      SCALAR = APARM(1).LT.0.0
C                                       get STOKES set
      DSTOK = CATD(KDCRV+JLOCS) + (1.0 - CATR(KRCRP+JLOCS)) *
     *   CATR(KRCIC+JLOCS)
      ISTOKE = DROUND (DSTOK)
      NSTOKE = CATBLK(KINAX+JLOCS)
      IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'RRLL') .OR.
     *   (STOKES.EQ.'XXYY')) THEN
         IF ((NSTOKE.EQ.1). AND .(ISTOKE.EQ.-1)) STOKES = 'RR'
         IF (ISTOKE.EQ.-2) STOKES = 'LL'
         IF ((NSTOKE.EQ.1) .AND. (ISTOKE.EQ.-5)) STOKES = 'XX'
         IF (ISTOKE.EQ.-6) STOKES = 'YY'
         END IF
      IF (STOKES.EQ.'CROS') THEN
         IF (ISTOKE.LE.-5) THEN
            STOKES = 'XYYX'
         ELSE IF (ISTOKE.GE.1) THEN
            STOKES = 'QU'
         ELSE
            STOKES = 'RLLR'
            END IF
         END IF
      CALL FNDPOL (STOKES, JERR)
      AUTO = (ABS (APARM(8)-1.0).LE.0.49)
      AUTO1 = AUTO .AND. (STOKES.NE.'RL') .AND. (STOKES.NE.'LR') .AND.
     *   (STOKES.NE.'RLLR') .AND. (STOKES.NE.'IQU') .AND.
     *   (STOKES.NE.'QU') .AND. (STOKES.NE.'FULL') .AND.
     *   (STOKES.NE.'IQUV')
C                                       AUTO - selection auto
C                                       AUTO1- plotting auto
C                                       AUTO.AND.AUTO1-just auto
C                                       AUTO.AND. .NOT. AUTO1-
C                                       auto with both ampl and phase
      ACF = ABS (APARM(8)-3.0).LE.0.49
      XCF = ABS (APARM(8)-4.0).LE.0.49
      REVERS = APARM(10).GT.0.0
      IF (XCF .OR. ACF) THEN
         IF ((CODTYP.EQ.2) .OR. (CODTYP.EQ.4)) THEN
            CODTYP = 4
         ELSE
            CODTYP = 3
            END IF
C         APARM(7) = 0.0
         END IF
      IF (ACF) AUTO = .TRUE.
      IF (TYPUVD.GT.0) AUTO = .TRUE.
      JY = 'Jy'
      IF (TYPUVD.GT.0) JY = 'K'
      DOACOR = AUTO
      DOXCOR = .NOT.AUTO
      BPPLOT = ABS (APARM(8)-2.0).LE.0.49
      BDPLOT = ABS (APARM(8)-5.0).LE.0.49
      PDPLOT = ABS (APARM(8)-6.0).LE.0.49
      CPPLOT = ABS (APARM(8)-7.0).LE.0.49
      CPPLTI = ABS (APARM(8)-8.0).LE.0.49
      CPPLOT = CPPLOT .OR. CPPLTI
      PCPLOT = ABS (APARM(8)-9.0).LE.0.49
      IF (PCPLOT) APARM(7) = 0.0
      TAPLOT = BPPLOT .OR. BDPLOT .OR. PDPLOT .OR. CPPLOT .OR. CPPLTI
     *   .OR. PCPLOT
      IF (CPPLOT) NCOUNT = MAX (1, NCOUNT)
      IF (AUTO1) THEN
         IF ((CODTYP.EQ.2) .OR. (CODTYP.EQ.4)) THEN
            CODTYP = 4
         ELSE
            CODTYP = 7
            END IF
         END IF
      IF (TAPLOT) SCALAR = .FALSE.
      IF (CODTYP.GT.4) SCALAR = .FALSE.
      IF (AUTO) SCALAR = .FALSE.
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
         ISVLBA = ANAME.EQ.'VLBA'
         I4TEMP = IABUF(5)
         NANT = I4TEMP
         DO 20 IANT = 1,MAXANT
            WRITE (STNS(IANT),1020) 'VLA', IANT
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
         END IF
 30   IF (NANT.EQ.0) THEN
         DO 40 I = 1,MAXANT
            WRITE (STNS(IANT),1020) '???', I
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
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL RCOPY (2, XUVR, UVRNG)
      CALL FILL (50, 0, ANTENS)
      BCHAN = IROUND (XBCHAN)
      IF (ACF .OR. XCF) THEN
         CBCHAN = BCHAN
         BCHAN = 0
         CBCHAN = MAX (1, MIN (CBCHAN, CATBLK(KINAX+JLOCF)))
         END IF
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ACF .OR. XCF) THEN
         CECHAN = ECHAN
         ECHAN = 0
         IF (CECHAN.EQ.0) THEN
            CECHAN = CATBLK(KINAX+JLOCF)
            CECHAN = MAX (1, MIN (CECHAN, CATBLK(KINAX+JLOCF)))
            END IF
         END IF
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (XCF .OR. ACF) THEN
         CBCHAN = CBCHAN * 2 - 1
         CECHAN = CECHAN * 2
         END IF
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
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      BPVER = IROUND (XBPVER)
      DOBAND = IROUND (XDOBND)
C                                       Spectral smoothing
      PSMTAB(1) = -1.0
      I = IROUND (XSMOTH(1))
      SMTHIT = (I.GE.9) .AND. (I.LE.16)
      I = IROUND (APARM(8))
      IF ((I.EQ.2) .OR. (I.GT.4)) SMTHIT = .FALSE.
      IF (SMTHIT) THEN
         CALL RFILL (3, 0.0, SMOOTH)
      ELSE
         CALL RCOPY (3, XSMOTH, SMOOTH)
         END IF
C                                       Check Stokes
      IF (.NOT.TAPLOT) THEN
         CALL FNDPOL (STOKES, JERR)
         IF (JERR.NE.0) GO TO 999
         IF ((STOKES.EQ.'I') .OR. (STOKES.EQ.'F').OR. (STOKES.EQ.'V')
     *      .OR. (STOKES.EQ.'RR') .OR. (STOKES.EQ.'LL') .OR.
     *      (STOKES.EQ.'XX') .OR. (STOKES.EQ.'YY')) THEN
            POLNUM = 1
            POLLAB(1) = STOKES
            CROSS = -1
         ELSE IF ((STOKES.EQ.'Q') .OR. (STOKES.EQ.'U') .OR.
     *      (STOKES.EQ.'RL') .OR. (STOKES.EQ.'LR') .OR.(STOKES.EQ.'XY')
     *      .OR. (STOKES.EQ.'YX')) THEN
            POLNUM = 1
            POLLAB(1) = STOKES
            CROSS = 1
         ELSE IF (STOKES.EQ.'RLLR') THEN
            POLNUM = 2
            POLLAB(1) = 'RL'
            POLLAB(2) = 'LR'
            CROSS = 1
         ELSE IF (STOKES.EQ.'XYYX') THEN
            POLNUM = 2
            POLLAB(1) = 'XY'
            POLLAB(2) = 'YX'
            CROSS = 1
         ELSE IF (STOKES.EQ.'IV') THEN
            POLNUM = 2
            POLLAB(1) = 'I'
            POLLAB(2) = 'V'
            CROSS = -1
         ELSE IF (STOKES.EQ.'IQU') THEN
            POLNUM = 3
            POLLAB(1) = 'I'
            POLLAB(2) = 'Q'
            POLLAB(3) = 'U'
            CROSS = 0
         ELSE IF (STOKES.EQ.'IQUV') THEN
            POLNUM = 4
            POLLAB(1) = 'I'
            POLLAB(2) = 'Q'
            POLLAB(3) = 'U'
            POLLAB(4) = 'V'
            CROSS = 0
         ELSE IF (STOKES.EQ.'FV') THEN
            POLNUM = 2
            POLLAB(1) = 'F'
            POLLAB(2) = 'V'
            CROSS = -1
         ELSE IF (STOKES.EQ.'FQU') THEN
            POLNUM = 3
            POLLAB(1) = 'F'
            POLLAB(2) = 'Q'
            POLLAB(3) = 'U'
            CROSS = 0
         ELSE IF (STOKES.EQ.'FQUV') THEN
            POLNUM = 4
            POLLAB(1) = 'F'
            POLLAB(2) = 'Q'
            POLLAB(3) = 'U'
            POLLAB(4) = 'V'
            CROSS = 0
         ELSE IF (STOKES.EQ.'QU') THEN
            POLNUM = 2
            POLLAB(1) = 'Q'
            POLLAB(2) = 'U'
            CROSS = 1
            STOKES = 'QU'
         ELSE IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'RRLL') .OR.
     *      (STOKES.EQ.'XXYY')) THEN
            POLNUM = 2
            CROSS = -1
            IF (ICOR0.LE.-5) THEN
               POLLAB(1) = 'XX'
               POLLAB(2) = 'YY'
            ELSE
               POLLAB(1) = 'RR'
               POLLAB(2) = 'LL'
               END IF
         ELSE IF ((STOKES.EQ.'FULL') .OR. (STOKES.EQ.'RLRL') .OR.
     *      (STOKES.EQ.'XYXY')) THEN
            POLNUM = 4
            CROSS = 0
            IF (ICOR0.LE.-5) THEN
               POLLAB(1) = 'XX'
               POLLAB(2) = 'YY'
               POLLAB(3) = 'XY'
               POLLAB(4) = 'YX'
            ELSE
               POLLAB(1) = 'RR'
               POLLAB(2) = 'LL'
               POLLAB(3) = 'RL'
               POLLAB(4) = 'LR'
               END IF
            END IF
         END IF
C                                       Divide by channel 0
      DIVCH0 = BPARM(1).GT.0.0
      IF (DIVCH0) THEN
         BPARM(4) = MAX (0.0, BPARM(4))
         BPARM(4) = BPARM(4) * BPARM(4)
         I = CATBLK(KINAX+JLOCF)
         J1 = BPARM(2) + 0.1
         J2 = BPARM(3) + 0.1
         IF (J1.LE.0.0) J1 = (I+1) / 8
         IF (J2.LE.0.0) J2 = (((I+1) * 7) / 8) - 1
         IF (J2.LT.J1) THEN
            J1 = (I+1) / 8
            J2 = (((I+1) * 7) / 8) - 1
            END IF
         IF (J2.GT.ECHAN) J2 = ECHAN
         IF (J1.LT.BCHAN) J1 = BCHAN
         BPARM(2) = J1
         BPARM(3) = J2
         WRITE (MSGTXT,1100) J1, J2
         CALL MSGWRT (3)
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATBLK, LUN, PFQFRQ, PFQTBW,
     *   PFQCHW, PFQSID, BNDCOD, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1080) JERR
         GO TO 990
         END IF
C
      TOTAL = AUTO .OR. BPPLOT .OR. PDPLOT .OR. PCPLOT
      CALL SETANT (50, XANTEN, XBASE, NXANT, NXBASL, IXANT, IXBASL,
     *   DESEL)
      IF (TOTAL) THEN
         IF ((NXANT.LE.0) .AND. (NXBASL.GT.0)) THEN
            CALL COPY (NXBASL, IXBASL, IXANT)
            NXANT = NXBASL
            END IF
         NXBASL = 0
         END IF
C                                       Fill in list of all antenna
C                                       - baseline pairs and names
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      CALL FILANT (DISKIN, CNOIN, CATBLK, LUNAN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, SUBARR, TOTAL, NBASE, XA1, XA2, VSTNS, IABUF,
     *   ANTENS, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get source list
      CALL SOUFIL (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Scan averaging
      SCANAV = XSOLIN.LT.0.0
C                                       plot subsequent times?
      IF (XSOLIN.NE.0.0) THEN
         STARTU = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *      + TIMRNG(4) / (24. * 60. * 60.)
         STOPU = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *      + TIMRNG(8) / (24. * 60. * 60.)
C                                       Determine stop time of data
         CALL UVTIME (DISKIN, CNOIN, CATBLK, STARTD, STOPD, JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'POSSIN: UNABLE TO DETERMINE DATA STOP TIME'
            GO TO 990
            END IF
         IF (STARTU.LE.0.0) STARTU = STARTD
         IF (STOPU.LT.STARTU) STOPU = STOPD
C                                       check selected span of data
         STOPD = 24.0 * 60.0 * (STOPU - STARTU)
C                                       adjust solint if needed
         XSOLIN = MIN (XSOLIN, STOPD)
         TSOLIN = XSOLIN / (24.0 * 60.0)
C                                       check for time
         IF (.NOT.(SCANAV .OR. TAPLOT)) THEN
            INITVS = 1
            CALL FINDUV (STARTU, TSOLIN, PLSTVS, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         STARTD = STARTU
         STOPD = STOPU
         STOPU = STARTU + TSOLIN
         CALL RFILL (8, 0.0, TIMRNG)
         TIMRNG(1) = STARTU
         TIMRNG(5) = STOPU
         END IF
C                                       Position shift
      DOSHFT = (SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)
      DOSHFT = (DOSHFT) .AND. (DOXCOR)
      DXTIME = 0.0
C                                       fill freq table
      CVER = 1
      IF (.NOT.DIDCHN) THEN
         LUNP = 49
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, CVER, CATUV, LUNP,
     *      NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.EQ.0) DIDCHN = .TRUE.
C                                        Single-source file
         IF (IERR.NE.0) THEN
            CALL DFILL (MAXIF, 0.0D0, FOFF)
            CALL FILL (MAXIF, 1, ISBAND)
            CALL RFILL (MAXIF, CATR(KRCIC+KLOCFY), FINC)
            IERR = 0
            END IF
         END IF
C                                                Get scan list
      IF (SCANAV) THEN
         NXLUN = 45
         NEXTSC = 1
         IF (BPPLOT) THEN
            TTYPE = 'BP'
            TVER = BPVER
         ELSE IF (BDPLOT) THEN
            TTYPE = 'BD'
            TVER = BPVER
         ELSE IF (PCPLOT) THEN
            TTYPE = 'PC'
            TVER = BPVER
         ELSE
            TTYPE = 'NX'
            TVER = 1
            END IF
         CALL NXSET (DISKIN, CNOIN, TTYPE, TVER, XTIME, SOUWAN, DOSWNT,
     *      NSOUWD, SUBARR, FRQSEL, WTS, BUFF2, JERR)
         IF (JERR.NE.0) GO TO 999
C                                                Set first time
         STARTU = NXTIM(1,NEXTSC)
         STOPU = NXTIM(2,NEXTSC)
         INITVS = NXVISN(1,NEXTSC)
         TIMRNG(1) = STARTU
         TIMRNG(5) = STOPU
         IF (TTYPE.EQ.'NX') THEN
            STOPD = MIN (STOPD, NXTIM(2,NUMNX))
         ELSE
            STOPD = MAX (STOPD, NXTIM(2,NUMNX))
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POSSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT (A,':AN',I2.2)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('ERROR',I3,' CHECKING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' STATUS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('POSSIN: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
 1100 FORMAT ('Using channels',I6,' to',I6,' as channel 0')
      END
      SUBROUTINE POSSUV (IRET)
C-----------------------------------------------------------------------
C   POSSUV loops through the data averaging according to the selection
C   and command criteria and returns the averaged array for plotting.
C   Output:
C      IRET     I     Return error code, 0=>OK, otherwise error.
C                     If IRET = 5, no data found
C                     If IRET = 10, no more data coming from multiple
C                     plots/page - so exit
C   Output in common:
C      BUFF2    R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   IRET, LOOPIF, LOOPF, INP, LUN3, I, IFNO, IANT1, IANT2,
     *   XCOUNT, INDEX, NPRI, SRCDUN, INX, HM(2), OLDSRC, IPOLPT, IPOL,
     *   ITS(4), ITE(4), JERR, J, ISB, CVER, LUNP, CPTR, LNUMIF, MSGSAV,
     *   LPOL, NUMIFQ
      REAL      RPARM(20), SUMWT(MAXIF,4), XNORM, WT, VIS(UVBFSL),
     *   LINWT, AVTIME, CATR(256), DXC, DYC, DZC, XX, BUFF3(2,MAXCHA),
     *   BUFFH(2*MAXCIF)
      COMPLEX   ZZ, VS
      HOLLERITH  CATH(256)
      CHARACTER CHSIGN*1, CTS*1, CTE*1, BNDCOD(MAXIF)*8
      DOUBLE PRECISION FRSCL, CATD(128), DTEMP
      INTEGER   ISBAND(MAXIF)
      LOGICAL   GOTSOM
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK,CATH,CATR,CATD)
      DATA LUN3 /25/
C-----------------------------------------------------------------------
      NPRI = 0
      JERR = 0
      BLNKBF = .TRUE.
C                                       Set lengths of input axes.
      LNUMIF = PEIF - PBIF + 1
C
      IF (ECHAN.LE.BCHAN) ECHAN = CATSAV(KINAX+KLOCFY)
      NUMFRQ = PECH - PBCH + 1
      IF ((NUMFRQ.LT.2) .AND. (LNUMIF.EQ.1)) THEN
         WRITE (MSGTXT,1008) NUMFRQ
         IRET = 1
         GO TO 990
      ELSE IF ((NUMFRQ.LT.2) .AND. (LNUMIF.GT.1) .AND.
     *   (.NOT.DOCHIF)) THEN
         WRITE (MSGTXT,1009)
         IRET = 1
         GO TO 990
         END IF
C
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (.NOT.AUTO) THEN
         IF (UVRNG(1).EQ.0.0) UVRNG(1) = 1.E-9
      ELSE
         UVRNG(1) = 0.0
         UVRNG(2) = 1.E-9
         END IF
C                                       Zero output array
      I = LNUMIF * NUMFRQ * POLNUM
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
C                                       don't look for everything
      IF ((NBASE.GT.0) .AND. (NCOUNT.GE.1)) THEN
         CALL FILL (50, 0, ANTENS)
         ANTENS(1) = XA1(ICURNT)
         IF (.NOT.AUTO) ANTENS(2) = XA2(ICURNT)
      ELSE IF (AUTO) THEN
         DO 20 I = 1,50
            ANTENS(I) = XA1(I)
 20         CONTINUE
         END IF
C                                       Init vis file for reading
      MSGSAV = MSGSUP
      IF (XSOLIN.NE.0.0) MSGSUP = 32000
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      MSGSUP = MSGSAV
      IF (IRET.EQ.-1) THEN
         MSGTXT = 'NO DATA FOUND WITH SPECIFIED PARAMETERS: CHECK' //
     *      ' INPUTS'
         IF (XSOLIN.EQ.0.0) GO TO 990
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         IRET = 5
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Fill in baseline names
      IF ((NANTSL.EQ.0) .OR. (NANTSL.GT.2)) THEN
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
C                                       Guard against user optimism
         IF (.NOT.AUTO) THEN
            IF ((TELNUM(1).EQ.0) .OR. (TELNUM(2).EQ.0)) THEN
               JERR = 10
               GO TO 980
               END IF
            END IF
         END IF
C                                       Position shift calculations
C                                       Main averaging loop
      UVSCAL = FREQ / UVFREQ
      XCOUNT = 0
      IF ((NVIS.EQ.0) .AND. (XSOLIN.NE.0.0))THEN
         WRITE (MSGTXT,1520) CTS, ITS, CTE, ITE
C         CALL MSGWRT (3)
         END IF
      I = 4 * MAXIF
      CALL RFILL (I, 0.0, SUMWT)
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
               IF (AUTO) THEN
                  IF (IANT1.EQ.XA1(ICURNT)) GO TO 60
               ELSE
                  IF ((IANT1.EQ.XA1(ICURNT)) .AND.
     *               (IANT2.EQ.XA2(ICURNT))) GO TO 60
                  END IF
               GO TO 300
               END IF
            DO 50 J = 1, NBASE
               IF (AUTO) THEN
                  IF (IANT1.EQ.XA1(J)) GO TO 60
               ELSE
                  IF ((IANT1.EQ.XA1(J)) .AND. (IANT2.EQ.XA2(J)))
     *               GO TO 60
                  END IF
 50            CONTINUE
            GO TO 300
            END IF
C
 60      IF (DIVCH0) CALL DIVCHZ (VIS, IRET)
C                                       Average.
         AVTIME = AVTIME + RPARM(ILOCT+1)
         XCOUNT = XCOUNT + 1
         SRCDUN = CURSOU
C                                       Fill in values in DSOU.INC
         IF (SRCDUN.NE.OLDSRC) THEN
            CALL GETSOU (SRCDUN,IUDISK,IUCNO,CATUV,LUN3,IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET
               GO TO 990
               END IF
            OLDSRC = SRCDUN
            IF (DOSHFT) THEN
               RAEPO = RAEPO + SHIFT(1) / 3600. / RAD2DG
               DECEPO = DECEPO + SHIFT(2) / 3600. / RAD2DG
C                                       Get RA and Dec offsets from
C                                       uv data reference position.
               DXC = SIN (DG2RAD * (SHIFT(1)/3600.)) *
     *            COS (DEC * DG2RAD)
               DYC = COS ((DEC - SHIFT(2)/3600.) * DG2RAD) *
     *            SIN (DEC * DG2RAD) - SIN ((DEC  - SHIFT(2)/3600.) *
     *            DG2RAD) * COS (DEC * DG2RAD) * COS ((SHIFT(1)/3600.) *
     *            DG2RAD)
               DZC = SIN ((DEC - SHIFT(2)/3600.) * DG2RAD) *
     *            SIN (DEC * DG2RAD) + COS ((DEC  - SHIFT(2)/3600.) *
     *            DG2RAD) * COS (DEC * DG2RAD) * COS ((SHIFT(1)/3600.) *
     *            DG2RAD)
               DZC = (DZC - 1.0D0) * 6.283185308
               DXC = DXC * 6.283185308
               DYC = DYC * 6.283185308
               END IF
            END IF
C                                       Do the accumulation
         DO 100 IPOL = 1,POLNUM
         DO 100 IFNO = 1,LNUMIF
            IF (POLPLT.GT.0) THEN
               INDEX = 1 + (IFNO-1) * INCIF + (IPOLPT*INCS)
            ELSE
               INDEX = 1 + (IFNO-1) * INCIF + (IPOL-1)*INCS
               END IF
            LINWT = 0.0
            INX = (IFNO - 1 + (IPOL-1)*LNUMIF) * NUMFRQ
            DO 90 LOOPF = 1,NUMFRQ
               INX = INX + 1
               INP = INDEX + (LOOPF-1+PBCH-BCHAN) * INCF
               WT = MAX (0.0, VIS(INP+2))
               IF (WT.GT.0.0) THEN
                  IF (DOSHFT) THEN
                     FRSCL = 1.D0 + (FOFF(IFNO+PBIF-1) +
     *                  FINC(IFNO+PBIF-1) * (LOOPF + PBCH - BCHAN -
     *                  CATR(KRCRP+JLOCF))) / CATD(KDCRV+JLOCF)
                     XX = RPARM(ILOCU+1) * FRSCL * DXC +
     *                  RPARM(ILOCV+1) * FRSCL * DYC +
     *                  RPARM(ILOCW+1) * FRSCL * DZC
                     ZZ = CMPLX (COS(XX), -SIN(XX))
                     VS = CMPLX (VIS(INP), VIS(INP+1)) * ZZ
                     VIS(INP) = REAL(VS)
                     VIS(INP+1) = AIMAG(VS)
                     END IF
C                                       Scalar section
                  IF (SCALAR) THEN
C                                       Total power
                     IF (AUTO1) THEN
                        VIS(INP+1) = 0.
C                                       Cross power
                     ELSE
                        SCAMP(INX) = SCAMP(INX) + WT * (SQRT (VIS(INP) *
     *                     VIS(INP) + VIS(INP+1) * VIS(INP+1)))
                        END IF
                     END IF
C                                       Vector section
                  WTS(INX) = WTS(INX) + WT
                  BUFF2(1,INX) = BUFF2(1,INX) + VIS(INP)   * WT
                  IF (.NOT.AUTO1) BUFF2(2,INX) = BUFF2(2,INX) +
     *               VIS(INP+1) * WT
                  LINWT = LINWT + WT
                  END IF
 90            CONTINUE
            SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + LINWT/NUMFRQ
 100        CONTINUE
 300     CONTINUE
C                                       Normalize the output array
 400  INX = 0
      GOTSOM = .FALSE.
      DO 500 IPOL = 1,POLNUM
      DO 500 IFNO = 1,LNUMIF
         LOOPIF = IFNO + PBIF - 1
         IF ((XSOLIN.EQ.0.0) .AND. (SUMWT(IFNO,IPOL).LE.0.0)) THEN
            IF (AUTO) THEN
               WRITE (MSGTXT,1400) LOOPIF
               CALL MSGWRT (6)
               WRITE (MSGTXT,1420) ANTNAM(1)
               CALL MSGWRT (6)
            ELSE
               IF (ANTNAM(1).NE.ANTNAM(2)) THEN
                  WRITE (MSGTXT,1400) LOOPIF
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1410) ANTNAM(1), ANTNAM(2)
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
         DO 450 LOOPF = 1,NUMFRQ
            INX = INX + 1
            IF (((BUFF2(1,INX).EQ.0.0) .AND. (BUFF2(2,INX).EQ.0.0))
     *         .OR. (WTS(INX).LE.0.)) THEN
               BUFF2(1,INX) = FBLANK
               BUFF2(2,INX) = FBLANK
               SCAMP(INX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(INX)
               BUFF2(1,INX) = BUFF2(1,INX) * XNORM
               BUFF2(2,INX) = BUFF2(2,INX) * XNORM
               SCAMP(INX) = SCAMP(INX) * XNORM
               GOTSOM = .TRUE.
               END IF
 450        CONTINUE
 500     CONTINUE
C                                          Do we have any?
      IF (.NOT.GOTSOM) THEN
         WRITE (MSGTXT,1500)
         JERR = 5
         IF ((NCOUNT.GE.1) .AND. (ICURNT.NE.NBASE)) THEN
            IRET = 0
            JERR = 0
            END IF
         GO TO 980
         END IF
C                                          ACF/XCF wanted ?
      IF (ACF .OR. XCF) THEN
C                                          Fill frequency table
         CVER = 1
         FOFF(1) = 0.0
         LUNP = 49
         IF (.NOT.DIDCHN) THEN
            CALL CHNDAT ('READ', BUFF3, DISKIN, CNOIN, CVER, CATUV,
     *         LUNP, NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1060) IRET
               GO TO 990
            ELSE
               DIDCHN = .TRUE.
               END IF
            END IF
         NUMIFQ = (PEIF - PBIF + 1) * NUMFRQ
         DO 601 LPOL = 1,NUMPOL
         DO 600 IFNO = PBIF,PEIF
            INX = (IFNO-PBIF) * NUMFRQ + (LPOL - 1) * NUMIFQ
            LOOPF = 2 * NUMFRQ
            CALL RCOPY (LOOPF, BUFF2(1,INX+1), BUFF3)
            IF ((SCALAR) .AND. (XCF)) THEN
               DO 550 LOOPF = 1,NUMFRQ
                  INX = INX + 1
                  BUFF3(1,LOOPF) = SCAMP(INX)
 550              CONTINUE
               END IF
            ISB = 1
            IF (FINC(IFNO).LT.0.0) ISB = -1
C                                       FFT AC's
            IF (ACF) THEN
               CALL ACTRNS (BUFF3, ISB, NUMFRQ, WTS)
               NUMXCF = (CECHAN-CBCHAN+1)
               INX = (IFNO-PBIF) * NUMXCF + (LPOL-1) * NUMIFQ * 2
               DO 570 LOOPF = CBCHAN,CECHAN
                  CPTR = LOOPF - CBCHAN + 1 + INX
                  BUFFH(CPTR) = BUFF3(1,LOOPF)
 570              CONTINUE
C                                       FFT XC's
            ELSE IF (XCF) THEN
               CALL XCTRNS (BUFF3, ISB, NUMFRQ, WTS)
               NUMXCF = (CECHAN-CBCHAN+1)
               INX = (IFNO-PBIF) * NUMXCF + (LPOL-1) * NUMIFQ * 2
               DO 580 LOOPF = CBCHAN,CECHAN
                  CPTR = LOOPF - CBCHAN + 1 + INX
                  BUFFH(CPTR) = BUFF3(1,LOOPF)
 580              CONTINUE
               END IF
 600        CONTINUE
 601        CONTINUE
         END IF
C                                       move from hold buffer
      IF (ACF .OR. XCF) THEN
         INX = (PEIF - PBIF + 1) * NUMXCF + NUMPOL * 2 * NUMIFQ
         DO 610 LOOPF = 1,INX
            BUFF2(1,LOOPF) = BUFFH(LOOPF)
            BUFF2(2,LOOPF) = 0.0
            IF (SCALAR .AND. XCF) SCAMP(LOOPF) = BUFFH(LOOPF)
 610        CONTINUE
         END IF
C                                       Finish up
      BLNKBF = .FALSE.
      WRITE (MSGTXT,1510) XCOUNT, NVIS
      CALL MSGWRT (4)
      IF (AUTO) THEN
         WRITE (MSGTXT,1420) ANTNAM(1)
      ELSE
         WRITE (MSGTXT,1410) ANTNAM(1), ANTNAM(2)
         END IF
      CALL MSGWRT (4)
      IF (XSOLIN.NE.0.0) THEN
         WRITE (MSGTXT,1520) CTS, ITS, CTE, ITE
         CALL MSGWRT (3)
         END IF
      IF (NOMIT.GT.0) THEN
         WRITE (MSGTXT,1515) NOMIT
         CALL MSGWRT (3)
         END IF
C                                       Fill in values in DSOU.INC
      CALL GETSOU (SRCDUN, IUDISK, IUCNO, CATUV, LUN3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
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
 1008 FORMAT ('POSSUV: ONLY ',I3,' CHANNELS SELECTED, THATS NO GOOD')
 1009 FORMAT ('POSSUV: SET APARM(9)=1 TO PLOT 1 CH & MULTIPLE IFS')
 1010 FORMAT ('POSSUV: GETSOU RETURNED ERROR',I5)
 1060 FORMAT ('POSSUV : ERROR ',I3,' RETURNED BY CHNDAT')
 1400 FORMAT ('POSSUV: Warning, No valid data for IF: ',I2)
 1410 FORMAT ('POSSUV: on baseline(s) ',A8,' - ',A8)
 1420 FORMAT ('POSSUV: for antenna(s) ',A8)
 1500 FORMAT ('POSSUV: NO VISIBILITIES SELECTED - CHECK INPUT ',
     *        'PARMS OR SORT ORDER')
 1510 FORMAT ('Averaged',I10,' visibilities from total data set of',I9)
 1515 FORMAT ('Rejected',I5,' correlators due to low channel 0',
     *   ' amplitude')
 1520 FORMAT ('Covering timerange ',A,I2.2,'/',2(I2.2,':'),I2.2,' - ',
     *   A,I2.2,'/',2(I2.2,':'),I2.2)
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
     *   IROUND, NFAIL
      REAL      SUMRE, SUMWT, SUMIM, XNORM, TEMP, DENOM, WT
      LOGICAL   FLAGD
      INCLUDE 'POSSM.INC'
      REAL    CHZ(2,MAXIF,4)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE NFAIL
      DATA NFAIL /0/
C-----------------------------------------------------------------------
C                                       Determine channel 0 range
      IRET = 0
      FCHAN = IROUND (BPARM(2))
      LCHAN = IROUND (BPARM(3))
      NUMCHZ = LCHAN - FCHAN + 1
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
            DENOM = CHZ(1,LOOPIF,LOOPS) * CHZ(1,LOOPIF,LOOPS) +
     *              CHZ(2,LOOPIF,LOOPS) * CHZ(2,LOOPIF,LOOPS)
            IF (DENOM.LE.0.0) THEN
               NFAIL = NFAIL + 1
               END IF
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
      SUBROUTINE POSSBP (KANT, IRET)
C-----------------------------------------------------------------------
C   POSSBP is designed to extract bandpass functions from a BP table
C   average them and pass them into the plotting array.
C   Input from common:
C      NCOUNT      I     If > 0 then will plot multiple plots/page,
C                        if so POSSBP will be called multiple times and
C                        will return 1 antenna/call
C   INPUT:
C      KANT        I     Antenna number to be passed if NCOUNT > 0
C   Output:
C      IRET        I     Return error code, 0=>OK, otherwise error.
C                                          10=>no valid data
C   Output in common:
C      BUFF2       R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  IRET, IFNO, LUNBP, LUN3, BEGCHN, I, J, NROWS, INX, LNX,
     *   SOURID, SUBA, BPREF(2), LOOPF, IFRQ, IPOLC, I4TEMP, FREQID,
     *   KANT, HM(2), IPOL, NMAX, KK, IERR, JNX, IP2, ANT
      REAL     BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), XCOUNT, A, B,
     *   SUMWT(MAXIF,2), XNORM, INTERV, WTP1, WTP2, CR(MAXCHA),
     *   CI(MAXCHA), SUMWTS
      DOUBLE PRECISION TIME, CHNSHF(MAXIF), DTEMP, DPOLYN(MAXCHA),
     *   CATD(128)
      CHARACTER CHSIGN*1, LBPKEY*8
      LOGICAL   AVGPOL, F, DIDMSG
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATD)
      SAVE DIDMSG
      DATA LUNBP, LUN3 /27, 29/
      DATA F /.FALSE./
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
      DO 10 I = 1,50
         ANTENS(I) = XA1(I)
 10      CONTINUE
      CALL SOUFIL (IRET)
C                                       Fill in antenna name
      IF (NCOUNT.GE.1) THEN
         ANTNAM(1) = STNS(KANT)
         TELNUM(1) = KANT
         ANTNAM(2) = ' '
      ELSE
         IF (ANTENS(1).GE.1) THEN
            TELNUM(1) = ANTENS(1)
            ANTNAM(1) = STNS (ANTENS(1))
            ANTNAM(2) = '*'
            END IF
         END IF
C                                       Be clever for labelling
C                                       of plot in POSSLB
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Initialize BP table
      CALL BPINI ('READ', BPBUFF, DISKIN, CNOIN, BPVER, CATBLK, LUNBP,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, BEGCHN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.NE.0) GO TO 999
      I4TEMP = BPBUFF(5)
      NROWS = I4TEMP
C                                       Check IFs
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (NUMIF.GT.NIFBP) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IFNO, NIFBP
         GO TO 990
         END IF
C                                       Check polzn
      IPOLC = 1
      AVGPOL = F
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NPOLBP.GE.1) THEN
         IF ((NUMPOL.LE.1) .OR. (NPOLBP.EQ.1)) THEN
            POLNUM = 1
            IF (ICOR0.EQ.1) THEN
               IPOLC = 1
               STOKES = 'I   '
            ELSE IF (ICOR0.EQ.-1) THEN
               IPOLC = 1
               STOKES = 'RR  '
            ELSE IF (ICOR0.EQ.-2) THEN
               IPOLC = 2
               STOKES = 'LL  '
            ELSE IF (ICOR0.EQ.-5) THEN
               IPOLC = 1
               STOKES = 'XX  '
            ELSE IF (ICOR0.EQ.-6) THEN
               IPOLC = 2
               STOKES = 'YY  '
               END IF
            POLLAB(1) = STOKES(1:1)
         ELSE IF (NUMPOL.GT.1) THEN
            IF (STOKES.EQ.'I') THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'I'
               AVGPOL = ICOR0.EQ.-1
               IF (AVGPOL) POLLAB(1) = '(R+L)/2'
            ELSE IF ((STOKES.EQ.'V') .AND. (ICOR0.EQ.-1)) THEN
               IPOLC = 1
               POLNUM = 1
               AVGPOL = .TRUE.
               POLLAB(1) = '(R-L)/2'
            ELSE IF ((STOKES.EQ.'R') .OR. (STOKES.EQ.'RR')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'R'
            ELSE IF ((STOKES.EQ.'L') .OR. (STOKES.EQ.'LL')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'L'
            ELSE IF ((STOKES.EQ.'X') .OR. (STOKES.EQ.'XX')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'X'
            ELSE IF ((STOKES.EQ.'Y') .OR. (STOKES.EQ.'YY')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'Y'
            ELSE
               STOKES = 'HALF'
               IPOLC = 1
               POLNUM = 2
               IF (ICOR0.LT.-4) THEN
                  POLLAB(1) = 'X'
                  POLLAB(2) = 'Y'
               ELSE
                  POLLAB(1) = 'R'
                  POLLAB(2) = 'L'
                  END IF
               END IF
            END IF
         END IF
C                                       Determine size of spectrum
C                                       to be plotted
      IF (PBCH.EQ.0) PBCH = 1
      NUMFRQ = PECH - PBCH + 1
      IF (BEGCHN.GT.PBCH) THEN
         WRITE (MSGTXT,1000) PBCH, BEGCHN
         CALL MSGWRT (6)
         PBCH = BEGCHN
         NUMFRQ = PECH - PBCH + 1
         END IF
      IF (NUMFRQ.EQ.0) NUMFRQ = NCHNBP
      IF (NUMFRQ.GT.NCHNBP) THEN
         WRITE (MSGTXT,1001) NUMFRQ, NCHNBP
         CALL MSGWRT (6)
         NUMFRQ = NCHNBP
         END IF
C                                       Zero output array
      CALL RFILL (2*MAXCIF, 0.0, BUFF2)
      CALL RFILL (MAXCIF, 0.0, WTS)
      DO 30 IFNO = 1,NUMIF
         SUMWT(IFNO,1) = 0.0
         SUMWT(IFNO,2) = 0.0
 30      CONTINUE
      SUMWTS = 0.0
C                                       Initialize for data
C                                       selection
      IF ((TIMRNG(1) + TIMRNG(2) + TIMRNG(3) + TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5) + TIMRNG(6) + TIMRNG(7) + TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IF (TEND.EQ.0.0) TEND = 1.0E10
C                                       Main averaging loop
      XCOUNT = 0
      WRITE (MSGTXT,1002) NROWS
      IF (.NOT.DIDMSG) CALL MSGWRT (3)
      DIDMSG = .TRUE.
      DO 200 I = 1,NROWS
         IF (IBPRNO.GT.NROWS) GO TO 300
C
         CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANT,  CHNBND,
     *      CHNSHF, FREQID, BPREF, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Data selection
C                                       Time
         IF (TIME.LT.TSTART) GO TO 200
         IF (TIME.GT.TEND) GO TO 200
C                                       Antennas
         IF (NCOUNT.GE.1) THEN
            IF (ANT.EQ.KANT) GO TO 60
            GO TO 200
            END IF
         IF (NANTSL.LE.0) GO TO 60
         DO 50 J = 1,NANTSL
            IF ((ANT.EQ.ANTENS(J)) .AND. (DOAWNT)) GO TO 60
            IF ((ANT.EQ.ANTENS(J)) .AND. (.NOT.DOAWNT)) GO TO 200
 50         CONTINUE
         IF (.NOT.DOAWNT) GO TO 60
         GO TO 200
C                                       Check subarray
 60      IF ((SUBARR.GT.0) .AND. (SUBA.NE.SUBARR) .AND. (SUBA.GT.0))
     *      GO TO 200
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND. (FREQID.GT.0))
     *      GO TO 200
C                                       Sources
         IF ((NSOUWD.LE.0) .OR. (SOURID.EQ.0)) GO TO 100
         DO 80 J = 1,NSOUWD
            IF (SOURID.EQ.SOUWAN(J)) GO TO 90
 80         CONTINUE
         IF (.NOT.DOSWNT) GO TO 100
         GO TO 200
 90      IF (DOSWNT) GO TO 100
         GO TO 200
C                                       If polynomial BP type then
C                                       expand coefficients into a
C                                       spectrum.
 100     IF (LBPKEY.NE.' ') THEN
            A = 1.0
            B = NCHNBP
            DO 125 IPOL = 1,NPOLBP
               DO 120 IFNO = 1,NIFBP
C                                       Copy to temporary arrays
                  INX = (IFNO - 1) * NCHNBP + (IPOL - 1) * NCHNBP *
     *               NIFBP
                  DO 110 IFRQ = 1,NCHNBP
                     INX = INX + 1
                     CR(IFRQ) = BNDPAS(1,INX)
                     CI(IFRQ) = BNDPAS(2,INX)
 110                 CONTINUE
C
                  INX = INX - NCHNBP + 1
                  KK = 2 - BEGCHN
                  CALL BPCOEF (LBPKEY, CR, CI, 1, 1, NCHNBP, FBLANK,
     *               0.0D0, BNDPAS(1,INX), BNDPAS(2,INX), 2, 2, KK,
     *               NCHNBP, A, B, 0, .FALSE., DPOLYN, MAXCHA, NMAX,
     *               .FALSE., IRET)
                  IF (IRET.NE.0) GO TO 999
 120              CONTINUE
 125           CONTINUE
            END IF
C                                       Average
         XCOUNT = XCOUNT + 1
         IF (IPOLC.EQ.0) IPOLC = 1
         IF (AVGPOL) IPOLC = 1
         IF (NPOLBP.LT.2) IPOLC = 1
         IP2 = IPOLC + POLNUM - 1
         DO 150 IPOL = IPOLC,IP2
         DO 150 IFNO = PBIF,PEIF
            INX = (IFNO - 1) * NCHNBP + PBCH - 1 +
     *         (IPOL - 1) * NCHNBP * NIFBP
            JNX = INX + NCHNBP * NIFBP
            LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
            WTP1 = WEIGHT(IFNO + (IPOL-1)*NIFBP)
            IF (AVGPOL) THEN
               WTP2 = WTP1
               IF (STOKES.EQ.'V') WTP2 = -WTP1
               DO 135 IFRQ = 1,NUMFRQ
                  INX = INX + 1
                  LNX = LNX + 1
                  JNX = JNX + 1
                  IF ((BNDPAS(1,INX).NE.FBLANK) .AND.
     *               (BNDPAS(2,INX).NE.FBLANK) .AND.
     *               (BNDPAS(1,JNX).NE.FBLANK) .AND.
     *               (BNDPAS(2,JNX).NE.FBLANK)) THEN
                     BUFF2(1,LNX) = BUFF2(1,LNX) + BNDPAS(1,INX) * WTP1
     *                  + BNDPAS(1,JNX) * WTP2
                     BUFF2(2,LNX) = BUFF2(2,LNX) + BNDPAS(2,INX) * WTP1
     *                  + BNDPAS(2,JNX) * WTP2
                     WTS(LNX) = WTS(LNX) + 2.*WTP1
                     SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + 2.*WTP1
                     SUMWTS = SUMWTS + 1.0
                     END IF
 135              CONTINUE
            ELSE
               DO 140 IFRQ = 1,NUMFRQ
                  INX = INX + 1
                  LNX = LNX + 1
                  JNX = JNX + 1
                  IF ((BNDPAS(1,INX).NE.FBLANK) .AND.
     *               (BNDPAS(2,INX).NE.FBLANK)) THEN
                     BUFF2(1,LNX) = BUFF2(1,LNX) + BNDPAS(1,INX) * WTP1
                     BUFF2(2,LNX) = BUFF2(2,LNX) + BNDPAS(2,INX) * WTP1
                     WTS(LNX) = WTS(LNX) + WTP1
                     SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + WTP1
                     SUMWTS = SUMWTS + 1.0
                     END IF
 140              CONTINUE
               END IF
 150        CONTINUE
 200     CONTINUE
C                                       Average the output array
 300  DO 320 IPOL = IPOLC,IP2
      DO 320 IFNO = PBIF,PEIF
         IF ((SUMWT(IFNO,IPOL).LE.0.0) .AND. (SUMWTS.GT.0.0)) THEN
            WRITE (MSGTXT, 1300) IFNO, IPOL
            CALL MSGWRT (6)
            END IF
         LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
         DO 310 LOOPF = 1,NUMFRQ
            LNX = LNX + 1
            IF ((WTS(LNX).LE.0.0) .OR.
     *         ((BUFF2(1,LNX).EQ.0.0) .AND. (BUFF2(2,LNX).EQ.0.0))) THEN
               BUFF2(1,LNX) = FBLANK
               BUFF2(2,LNX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(LNX)
               BUFF2(1,LNX) = BUFF2(1,LNX) * XNORM
               BUFF2(2,LNX) = BUFF2(2,LNX) * XNORM
               END IF
 310        CONTINUE
 320     CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      BLNKBF = .FALSE.
      IRET = 0
      IF (NVIS.LT.1) THEN
         MSGTXT = 'POSSBP: NO TABLE ENTRIES SELECTED - CHECK INPUT ' //
     *      'PARMS'
         IF (XSOLIN.EQ.0.0) CALL MSGWRT (6)
         IRET = -1
         BLNKBF = .TRUE.
      ELSE
         WRITE (MSGTXT,1006) NVIS
         CALL MSGWRT (4)
         END IF
C                                       Fill in values in DSOU.INC
      CALL GETSOU (SOURID, IUDISK, IUCNO, CATUV, LUN3, IERR)
      IF (IERR.NE.0) THEN
         IRET = IERR
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       time varying
C                                       Fill in values for output
C                                       file labeling
      SRCOBS = SAUCE
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (NVIS.GT.0) AVWGHT = SUMWT(1,1) / NVIS
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
      CALL TABBP ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, CHNBND, CHNSHF,
     *   FREQID, BPREF, WEIGHT, BNDPAS, IERR)
      IF (IERR.NE.0) IRET = IERR
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POSSBP: BCHAN =',I6,' BUT BCHAN IN TABLE =',I6)
 1001 FORMAT ('POSSBP: YOU WANT',I6,' CHANNELS PLOTTED, ONLY HAVE',
     *   I6,' IN THE TABLE')
 1002 FORMAT ('POSSBP: Total number of table rows =',I6)
 1006 FORMAT ('POSSBP: Averaged ',I5,' table entries')
 1010 FORMAT ('POSSBP: GETSOU RETURNED ERROR',I5)
 1020 FORMAT ('POSSBP: REQUESTED IF (',I3,') > MAX IN TABLE (',I3,')')
 1300 FORMAT ('POSSBP: Warning, no valid data for IF/POL: ',2I2)
      END
      SUBROUTINE POSSBD (KANT, LANT, IRET)
C-----------------------------------------------------------------------
C   POSSBD is designed to extract bandpass functions from a BD table
C   average them and pass them into the plotting array.
C   Input from common:
C      NCOUNT      I     If > 0 then will plot multiple plots/page,
C                        if so POSSBD will be called multiple times and
C                        will return 1 baseline/call
C   Output:
C      IRET        I     Return error code, 0=>OK, otherwise error.
C                                          10=>no valid data
C   Output in common:
C      BUFF2       R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INTEGER  KANT, LANT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  IFNO, LUNBP, LUN3, I, J, NROWS, INX, LNX, SOURID, SUBA,
     *   LOOPF, IFRQ, IPOLC, I4TEMP, FREQID, HM(2), IPOL, IERR, JNX,
     *   IP2, ANT1, ANT2, II, SUMWTS
      REAL     BNDPAS(2,MAXCIF), XCOUNT, SUMWT(MAXIF,2), XNORM, TIME
      DOUBLE PRECISION DTEMP,CATD(128)
      CHARACTER CHSIGN*1
      LOGICAL   AVGPOL, F, DIDMSG
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATD)
      SAVE DIDMSG
      DATA LUNBP, LUN3 /27, 29/
      DATA F /.FALSE./
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
      DO 10 I = 1,50
         ANTENS(I) = XA1(I)
 10      CONTINUE
      CALL SOUFIL (IRET)
C                                       Fill in antenna names
      IF (NCOUNT.GE.1) THEN
         ANTNAM(1) = STNS(XA1(ICURNT))
         TELNUM(1) = XA1(ICURNT)
         ANTNAM(2) = STNS(XA2(ICURNT))
         TELNUM(2) = XA2(ICURNT)
      ELSE IF ((NANTSL.EQ.0) .OR. (NANTSL.GT.2)) THEN
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
C                                       Be clever for labelling
C                                       of plot in POSSLB
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Initialize BP table
      CALL BDINI ('READ', BPBUFF, DISKIN, CNOIN, BPVER, CATBLK, LUNBP,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, IRET)
      IF (IRET.NE.0) GO TO 999
      I4TEMP = BPBUFF(5)
      NROWS = I4TEMP
C                                       Check IFs
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (NUMIF.GT.NIFBP) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IFNO, NIFBP
         GO TO 990
         END IF
C                                       Check polzn
      IPOLC = 1
      AVGPOL = F
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NPOLBP.GE.1) THEN
         IF ((NUMPOL.LE.1) .OR. (NPOLBP.EQ.1)) THEN
            POLNUM = 1
            IF (ICOR0.EQ.1) THEN
               IPOLC = 1
               STOKES = 'I   '
            ELSE IF (ICOR0.EQ.-1) THEN
               IPOLC = 1
               STOKES = 'RR  '
            ELSE IF (ICOR0.EQ.-2) THEN
               IPOLC = 2
               STOKES = 'LL  '
            ELSE IF (ICOR0.EQ.-5) THEN
               IPOLC = 1
               STOKES = 'XX  '
            ELSE IF (ICOR0.EQ.-6) THEN
               IPOLC = 2
               STOKES = 'YY  '
               END IF
            POLLAB(1) = STOKES(1:1)
         ELSE IF (NUMPOL.GT.1) THEN
            IF (STOKES.EQ.'I') THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'I'
               AVGPOL = ICOR0.EQ.-1
               IF (AVGPOL) POLLAB(1) = 'R+L'
            ELSE IF ((STOKES.EQ.'R') .OR. (STOKES.EQ.'RR')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'R'
            ELSE IF ((STOKES.EQ.'L') .OR. (STOKES.EQ.'LL')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'L'
            ELSE IF ((STOKES.EQ.'X') .OR. (STOKES.EQ.'XX')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'X'
            ELSE IF ((STOKES.EQ.'Y') .OR. (STOKES.EQ.'YY')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'Y'
            ELSE
               STOKES = 'HALF'
               IPOLC = 1
               POLNUM = 2
               IF (ICOR0.LT.-4) THEN
                  POLLAB(1) = 'X'
                  POLLAB(2) = 'Y'
               ELSE
                  POLLAB(1) = 'R'
                  POLLAB(2) = 'L'
                  END IF
               END IF
            END IF
         END IF
C                                       Determine size of spectrum
C                                       to be plotted
      IF (PBCH.EQ.0) PBCH = 1
      NUMFRQ = PECH - PBCH + 1
      IF (NUMFRQ.EQ.0) NUMFRQ = NCHNBP
      IF (NUMFRQ.GT.NCHNBP) THEN
         WRITE (MSGTXT,1001) NUMFRQ, NCHNBP
         CALL MSGWRT (6)
         NUMFRQ = NCHNBP
         END IF
C                                       Zero output array
      CALL RFILL (2*MAXCIF, 0.0, BUFF2)
      CALL RFILL (MAXCIF, 0.0, WTS)
      DO 30 IFNO = 1,NUMIF
         SUMWT(IFNO,1) = 0.0
         SUMWT(IFNO,2) = 0.0
 30      CONTINUE
      SUMWTS = 0
C                                       Initialize for data
C                                       selection
      IF ((TIMRNG(1) + TIMRNG(2) + TIMRNG(3) + TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5) + TIMRNG(6) + TIMRNG(7) + TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IF (TEND.EQ.0.0) TEND = 1.0E10
C                                       Main averaging loop
      XCOUNT = 0
      WRITE (MSGTXT,1002) NROWS
      IF (.NOT.DIDMSG) CALL MSGWRT (3)
      DIDMSG = .TRUE.
      DO 200 I = 1,NROWS
         IF (IBPRNO.GT.NROWS) GO TO 300
C
         CALL TABBD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, TIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *      BNDPAS, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Data selection
C                                       Time
         IF (TIME.LT.TSTART) GO TO 200
         IF (TIME.GT.TEND) GO TO 200
C                                       Antennas
         IF (NCOUNT.GE.1) THEN
            IF ((ANT1.EQ.KANT) .AND. (ANT2.EQ.LANT)) GO TO 60
            GO TO 200
            END IF
         DO 50 II = 1,NBASE
            IF ((ANT1.EQ.XA1(II)) .AND. (ANT2.EQ.XA2(II))) GO TO 60
 50         CONTINUE
         GO TO 200
C                                       Check subarray
 60      IF ((SUBARR.GT.0) .AND. (SUBA.NE.SUBARR) .AND. (SUBA.GT.0))
     *      GO TO 200
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND. (FREQID.GT.0))
     *      GO TO 200
C                                       Sources
         IF ((NSOUWD.LE.0) .OR. (SOURID.EQ.0)) GO TO 100
         DO 80 J = 1,NSOUWD
            IF (SOURID.EQ.SOUWAN(J)) GO TO 90
 80         CONTINUE
         IF (.NOT.DOSWNT) GO TO 100
         GO TO 200
 90      IF (DOSWNT) GO TO 100
         GO TO 200
C                                       Average
 100     XCOUNT = XCOUNT + 1
         IF (IPOLC.EQ.0) IPOLC = 1
         IF (AVGPOL) IPOLC = 1
         IF (NPOLBP.LT.2) IPOLC = 1
         IP2 = IPOLC + POLNUM - 1
         DO 150 IPOL = IPOLC,IP2
         DO 150 IFNO = PBIF,PEIF
            INX = (IFNO - 1) * NCHNBP + PBCH - 1 +
     *         (IPOL - 1) * NCHNBP * NIFBP
            JNX = INX + NCHNBP * NIFBP
            LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
            DO 140 IFRQ = 1,NUMFRQ
               INX = INX + 1
               LNX = LNX + 1
               JNX = JNX + 1
               IF ((BNDPAS(1,INX).NE.FBLANK) .AND.
     *            (BNDPAS(2,INX).NE.FBLANK)) THEN
                  BUFF2(1,LNX) = BUFF2(1,LNX) + BNDPAS(1,INX)
                  BUFF2(2,LNX) = BUFF2(2,LNX) + BNDPAS(2,INX)
                  WTS(LNX) = WTS(LNX) + 1.0
                  SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + 1.0
                  SUMWTS = SUMWTS + 1
                  END IF
               IF ((AVGPOL) .AND. (BNDPAS(1,JNX).NE.FBLANK) .AND.
     *            (BNDPAS(2,JNX).NE.FBLANK)) THEN
                  BUFF2(1,LNX) = BUFF2(1,LNX) + BNDPAS(1,JNX)
                  BUFF2(2,LNX) = BUFF2(2,LNX) + BNDPAS(2,JNX)
                  WTS(LNX) = WTS(LNX) + 1.0
                  SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + 1.0
                  SUMWTS = SUMWTS + 1
                  END IF
 140           CONTINUE
 150        CONTINUE
 200     CONTINUE
C                                       Average the output array
 300  DO 320 IPOL = IPOLC,IP2
      DO 320 IFNO = PBIF,PEIF
         IF ((SUMWT(IFNO,IPOL).LE.0.0) .AND. (SUMWTS.GT.0)) THEN
            WRITE (MSGTXT,1300) IFNO, IPOL
            CALL MSGWRT (6)
            END IF
         LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
         DO 310 LOOPF = 1,NUMFRQ
            LNX = LNX + 1
            IF ((WTS(LNX).LE.0.0) .OR.
     *         ((BUFF2(1,LNX).EQ.0.0) .AND. (BUFF2(2,LNX).EQ.0.0))) THEN
               BUFF2(1,LNX) = FBLANK
               BUFF2(2,LNX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(LNX)
               BUFF2(1,LNX) = BUFF2(1,LNX) * XNORM
               BUFF2(2,LNX) = BUFF2(2,LNX) * XNORM
               END IF
 310        CONTINUE
 320     CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      BLNKBF = .FALSE.
      IRET = 0
      IF (NVIS.LT.1) THEN
         MSGTXT = 'POSSBD: NO TABLE ENTRIES SELECTED - CHECK INPUT ' //
     *      'PARMS'
         IF (XSOLIN.EQ.0.0) CALL MSGWRT (6)
         IRET = -1
         BLNKBF = .TRUE.
      ELSE
         WRITE (MSGTXT,1006) NVIS
         CALL MSGWRT (4)
         END IF
C                                       Fill in values in DSOU.INC
      CALL GETSOU (SOURID, IUDISK, IUCNO, CATUV, LUN3, IERR)
      IF (IERR.NE.0) THEN
         IRET = IERR
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       time varying
C                                       Fill in values for output
C                                       file labeling
      SRCOBS = SAUCE
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (NVIS.GT.0) AVWGHT = SUMWT(1,1) / NVIS
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
      CALL TABBD ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, TIME, SOURID, SUBA, ANT1, ANT2, FREQID, BNDPAS, IERR)
      IF (IRET.GT.0) GO TO 999
      IF (IERR.NE.0) IRET = IERR
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('POSSBD: YOU WANT',I6,' CHANNELS PLOTTED, ONLY HAVE',
     *   I6,' IN THE TABLE')
 1002 FORMAT ('POSSBD: Total number of table rows =',I6)
 1006 FORMAT ('POSSBD: Averaged',I6,' table entries')
 1010 FORMAT ('POSSBD: GETSOU RETURNED ERROR',I5)
 1020 FORMAT ('POSSBD: REQUESTED IF (',I3,') > MAX IN TABLE (',I3,')')
 1300 FORMAT ('POSSBD: Warning, no valid data for IF/POL: ',2I2)
      END
      SUBROUTINE POSSPD (KANT, IRET)
C-----------------------------------------------------------------------
C   POSSPD is designed to extract polarization bandpass functions from
C   a PD table and pass them into the plotting array.
C   Input from common:
C      NCOUNT      I     If > 0 then will plot multiple plots/page,
C                        if so POSSPD will be called multiple times and
C                        will return 1 antenna/call
C   Input:
C      KANT        I     Antenna number to be passed if NCOUNT > 0
C                        = 0 => average all
C   Output:
C      IRET        I     Return error code, 0=>OK, otherwise error.
C                                          10=>no valid data
C   Output in common:
C      BUFF2       R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INTEGER   KANT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IFNO, LUNPD, I, J, NROWS, INX, LNX, IP2, SUBA, ANT,
     *   PDREF(2), LOOPF, IFRQ, IPOLC, I4TEMP, FREQID, HM(2), IPOL,
     *   IERR, JNX, PDBUFF(512), IPDRNO, PDKOLS(9), PDNUMV(9), NANTPD,
     *   NPOLPD, NIFPD, NCHNPD
      REAL     DTERMS(2,MAXCIF), XCOUNT, XNORM, PHDIFF(MAXCIF)
      DOUBLE PRECISION DTEMP, CATD(128)
      CHARACTER CHSIGN*1, POLTYP*8
      LOGICAL   AVGPOL, F, DIDMSG
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATD)
      SAVE DIDMSG
      DATA LUNPD /27/
      DATA F /.FALSE./
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
      DO 10 I = 1,50
         ANTENS(I) = XA1(I)
 10      CONTINUE
      CALL SOUFIL (IRET)
C                                       Fill in antenna name
      IF (NCOUNT.GE.1) THEN
         ANTNAM(1) = STNS(KANT)
         TELNUM(1) = KANT
         ANTNAM(2) = ' '
      ELSE
         IF (ANTENS(1).GE.1) THEN
            TELNUM(1) = ANTENS(1)
            ANTNAM(1) = STNS (ANTENS(1))
            ANTNAM(2) = '*'
            END IF
         END IF
C                                       Be clever for labelling
C                                       of plot in POSSLB
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Initialize PD table
      CALL PDINI ('READ', PDBUFF, DISKIN, CNOIN, BPVER, CATBLK, LUNPD,
     *   IPDRNO, PDKOLS, PDNUMV, NANTPD, NPOLPD, NIFPD, NCHNPD, POLTYP,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      I4TEMP = PDBUFF(5)
      NROWS = I4TEMP
C                                       Check IFs
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (NUMIF.GT.NIFPD) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IFNO, NIFPD
         GO TO 990
         END IF
C                                       Check polzn
      IPOLC = 1
      AVGPOL = F
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NPOLPD.GE.1) THEN
         IF ((NUMPOL.LE.1) .OR. (NPOLPD.EQ.1)) THEN
            POLNUM = 1
            IF (ICOR0.EQ.1) THEN
               IPOLC = 1
               STOKES = 'I   '
            ELSE IF (ICOR0.EQ.-1) THEN
               IPOLC = 1
               STOKES = 'RR  '
            ELSE IF (ICOR0.EQ.-2) THEN
               IPOLC = 2
               STOKES = 'LL  '
            ELSE IF (ICOR0.EQ.-5) THEN
               IPOLC = 1
               STOKES = 'XX  '
            ELSE IF (ICOR0.EQ.-6) THEN
               IPOLC = 2
               STOKES = 'YY  '
               END IF
            POLLAB(1) = STOKES(1:1)
         ELSE IF (NUMPOL.GT.1) THEN
            IF (STOKES.EQ.'I') THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'I'
               AVGPOL = ICOR0.EQ.-1
               IF (AVGPOL) POLLAB(1) = 'R+L'
            ELSE IF ((STOKES.EQ.'R') .OR. (STOKES.EQ.'RR')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'R'
            ELSE IF ((STOKES.EQ.'L') .OR. (STOKES.EQ.'LL')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'L'
            ELSE IF ((STOKES.EQ.'X') .OR. (STOKES.EQ.'XX')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'X'
            ELSE IF ((STOKES.EQ.'Y') .OR. (STOKES.EQ.'YY')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'Y'
            ELSE
               STOKES = 'HALF'
               IPOLC = 1
               POLNUM = 2
               IF (ICOR0.LT.-4) THEN
                  POLLAB(1) = 'X'
                  POLLAB(2) = 'Y'
               ELSE
                  POLLAB(1) = 'R'
                  POLLAB(2) = 'L'
                  END IF
               END IF
            END IF
         END IF
C                                       Determine size of spectrum
C                                       to be plotted
      IF (PBCH.EQ.0) PBCH = 1
      NUMFRQ = PECH - PBCH + 1
      IF (NUMFRQ.EQ.0) NUMFRQ = NCHNPD
      IF (NUMFRQ.GT.NCHNPD) THEN
         WRITE (MSGTXT,1001) NUMFRQ, NCHNPD
         CALL MSGWRT (6)
         NUMFRQ = NCHNPD
         END IF
C                                       Zero output array
      CALL RFILL (2*MAXCIF, 0.0, BUFF2)
      CALL RFILL (MAXCIF, 0.0, WTS)
C                                       Main averaging loop
      XCOUNT = 0
      WRITE (MSGTXT,1002) NROWS
      IF (.NOT.DIDMSG) CALL MSGWRT (3)
      DIDMSG = .TRUE.
      DO 200 I = 1,NROWS
         IF (IPDRNO.GT.NROWS) GO TO 300
C
         CALL TABPD ('READ', PDBUFF, IPDRNO, PDKOLS, PDNUMV, NIFPD,
     *      NCHNPD, NPOLPD, ANT, SUBA, FREQID, PDREF, PHDIFF, DTERMS,
     *      IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Data selection
C                                       Antennas
         IF (NCOUNT.GE.1) THEN
            IF (ANT.EQ.KANT) GO TO 60
            GO TO 200
            END IF
         IF (NANTSL.LE.0) GO TO 60
         DO 50 J = 1,NANTSL
            IF ((ANT.EQ.ANTENS(J)) .AND. (DOAWNT)) GO TO 60
            IF ((ANT.EQ.ANTENS(J)) .AND. (.NOT.DOAWNT)) GO TO 200
 50         CONTINUE
         IF (.NOT.DOAWNT) GO TO 60
         GO TO 200
C                                       Check subarray
 60      IF ((SUBARR.GT.0) .AND. (SUBA.NE.SUBARR) .AND. (SUBA.GT.0))
     *      GO TO 200
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND. (FREQID.GT.0))
     *      GO TO 200
C                                       Average
         XCOUNT = XCOUNT + 1
         IF (IPOLC.EQ.0) IPOLC = 1
         IF (AVGPOL) IPOLC = 1
         IF (NPOLPD.LT.2) IPOLC = 1
         IP2 = IPOLC + POLNUM - 1
         DO 150 IPOL = IPOLC,IP2
         DO 150 IFNO = PBIF,PEIF
            INX = (IFNO - 1) * NCHNPD + PBCH - 1 +
     *         (IPOL - 1) * NCHNPD * NIFPD
            JNX = INX + NCHNPD * NIFPD
            LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
            DO 140 IFRQ = 1,NUMFRQ
               INX = INX + 1
               LNX = LNX + 1
               JNX = JNX + 1
               IF ((DTERMS(1,INX).NE.FBLANK) .AND.
     *            (DTERMS(2,INX).NE.FBLANK)) THEN
                  BUFF2(1,LNX) = BUFF2(1,LNX) + DTERMS(1,INX)
                  BUFF2(2,LNX) = BUFF2(2,LNX) + DTERMS(2,INX)
                  WTS(LNX) = WTS(LNX) + 1.0
                  END IF
               IF ((AVGPOL) .AND. (DTERMS(1,JNX).NE.FBLANK) .AND.
     *            (DTERMS(2,JNX).NE.FBLANK)) THEN
                  BUFF2(1,LNX) = BUFF2(1,LNX) + DTERMS(1,JNX)
                  BUFF2(2,LNX) = BUFF2(2,LNX) + DTERMS(2,JNX)
                  WTS(LNX) = WTS(LNX) + 1.0
                  END IF
 140           CONTINUE
 150        CONTINUE
 200     CONTINUE
C                                       Average the output array
 300  DO 320 IPOL = IPOLC,IP2
      DO 320 IFNO = PBIF,PEIF
         LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
         DO 310 LOOPF = 1,NUMFRQ
            LNX = LNX + 1
            IF ((WTS(LNX).LE.0.0) .OR.
     *         ((BUFF2(1,LNX).EQ.0.0) .AND. (BUFF2(2,LNX).EQ.0.0))) THEN
               BUFF2(1,LNX) = FBLANK
               BUFF2(2,LNX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(LNX)
               BUFF2(1,LNX) = BUFF2(1,LNX) * XNORM
               BUFF2(2,LNX) = BUFF2(2,LNX) * XNORM
               END IF
 310        CONTINUE
 320     CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      BLNKBF = .FALSE.
      IRET = 0
      IF (NVIS.LT.1) THEN
         MSGTXT = 'POSSPD: NO TABLE ENTRIES SELECTED - CHECK INPUT ' //
     *      'PARMS'
         IF (XSOLIN.EQ.0.0) CALL MSGWRT (6)
         IRET = -1
         BLNKBF = .TRUE.
      ELSE
         WRITE (MSGTXT,1006) NVIS
         CALL MSGWRT (4)
         END IF
C                                       time varying
C                                       Fill in values for output
C                                       file labeling
      SRCOBS = SAUCE
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (NVIS.GT.0) AVWGHT = 1.0
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
      CALL TABPD ('CLOS', PDBUFF, IPDRNO, PDKOLS, PDNUMV, NIFPD, NCHNPD,
     *   NPOLPD, ANT, SUBA, FREQID, PDREF, PHDIFF, DTERMS, IERR)
      IF (IERR.NE.0) IRET = IERR
      TSTART = -1000.
      TEND = 1.E6
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('POSSPD: YOU WANT',I6,' CHANNELS PLOTTED, ONLY HAVE',
     *   I6,' IN THE TABLE')
 1002 FORMAT ('POSSPD: Total number of table rows =',I6)
 1006 FORMAT ('POSSPD: Averaged',I6,' table entries')
 1020 FORMAT ('POSSPD: REQUESTED IF (',I3,') > MAX IN TABLE (',I3,')')
      END
      SUBROUTINE POSSCP (KSRC, IRET)
C-----------------------------------------------------------------------
C   POSSCP is designed to extract source polarization bandpass functions
C   from a CP table and pass them into the plotting array.
C   ASSUMES THAT NCOUNT > 0.
C   Input from common:
C      NCOUNT      I     If > 0 then will plot multiple plots/page,
C                        if so POSSCP will be called multiple times and
C                        will return 1 antenna/call
C   Input:
C      KSRC        I     Source number to be passed if NCOUNT > 0
C   Output:
C      IRET        I     Return error code, 0=>OK, otherwise error.
C                                          10=>no valid data
C   Output in common:
C      BUFF2       R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INTEGER   KSRC, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IFNO, LUNCP, I, NROWS, INX, LNX, LOOPF, IFRQ, I4TEMP,
     *   FREQID, HM(2), IERR, CPBUFF(512), ICPRNO, CPKOLS(6), CPNUMV(6),
     *   NIFCP, NCHNCP, SUID
      REAL      VFLUX(4,MAXCIF), XCOUNT, XNORM
      DOUBLE PRECISION DTEMP, CATD(128)
      CHARACTER CHSIGN*1, SOURSE*16
      LOGICAL   DIDMSG, DOIDIV
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATD)
      SAVE DIDMSG
      DATA LUNCP /27/
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
C                                       Fill in antenna name
      ANTNAM(1) = ' '
      TELNUM(1) = KSRC
      ANTNAM(2) = ' '
      TELNUM(2) = 0
      DOIDIV = ABS(APARM(8)-8.0).LE.0.49
      CALL GETSOU (KSRC, DISKIN, CNOIN, CATBLK, LUNCP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) KSRC
         CALL MSGWRT (8)
         IRET = -1
         GO TO 999
         END IF
C                                       Be clever for labelling
C                                       of plot in POSSLB
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Initialize CP table
      CALL CPINI ('READ', CPBUFF, DISKIN, CNOIN, BPVER, CATBLK, LUNCP,
     *   ICPRNO, CPKOLS, CPNUMV, NIFCP, NCHNCP, FREQID, IRET)
      IF (IRET.NE.0) GO TO 999
      I4TEMP = CPBUFF(5)
      NROWS = I4TEMP
C                                       Check IFs
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (NUMIF.GT.NIFCP) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IFNO, NIFCP
         GO TO 990
         END IF
C                                       Determine size of spectrum
C                                       to be plotted
      IF (PBCH.EQ.0) PBCH = 1
      NUMFRQ = PECH - PBCH + 1
      IF (NUMFRQ.EQ.0) NUMFRQ = NCHNCP
      IF (NUMFRQ.GT.NCHNCP) THEN
         WRITE (MSGTXT,1001) NUMFRQ, NCHNCP
         CALL MSGWRT (6)
         NUMFRQ = NCHNCP
         END IF
C                                       Zero output array
      CALL RFILL (2*MAXCIF, 0.0, BUFF2)
      CALL RFILL (MAXCIF, 0.0, WTS)
C                                       Main averaging loop
      XCOUNT = 0
      WRITE (MSGTXT,1002) NROWS
      IF (.NOT.DIDMSG) CALL MSGWRT (3)
      DIDMSG = .TRUE.
      DO 200 I = 1,NROWS
         IF (ICPRNO.GT.NROWS) GO TO 300
C
         CALL TABCP ('READ', CPBUFF, ICPRNO, CPKOLS, CPNUMV, NIFCP,
     *      NCHNCP, SOURSE, SUID, VFLUX, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Data selection
         IF (SUID.EQ.KSRC) THEN
            SAUCE = SOURSE
C                                       Average
            XCOUNT = XCOUNT + 1
            DO 150 IFNO = PBIF,PEIF
               INX = (IFNO - 1) * NCHNCP + PBCH - 1
               LNX = (IFNO - PBIF) * NUMFRQ
               DO 140 IFRQ = 1,NUMFRQ
                  INX = INX + 1
                  LNX = LNX + 1
                  IF (DOIDIV ) THEN
                     IF ((VFLUX(3,INX).NE.FBLANK) .AND.
     *                  (VFLUX(2,INX).NE.FBLANK) .AND.
     *                  (VFLUX(1,INX).NE.FBLANK)) THEN
                        BUFF2(1,LNX) = BUFF2(1,LNX) + VFLUX(2,INX) /
     *                     VFLUX(1,INX)
                        BUFF2(2,LNX) = BUFF2(2,LNX) + VFLUX(3,INX) /
     *                     VFLUX(1,INX)
                        WTS(LNX) = WTS(LNX) + 1.0
                        END IF
                  ELSE
                     IF ((VFLUX(3,INX).NE.FBLANK) .AND.
     *                  (VFLUX(2,INX).NE.FBLANK)) THEN
                        BUFF2(1,LNX) = BUFF2(1,LNX) + VFLUX(2,INX)
                        BUFF2(2,LNX) = BUFF2(2,LNX) + VFLUX(3,INX)
                        WTS(LNX) = WTS(LNX) + 1.0
                        END IF
                     END IF
 140              CONTINUE
 150           CONTINUE
            END IF
 200     CONTINUE
C                                       Average the output array
 300  DO 320 IFNO = PBIF,PEIF
         LNX = (IFNO - PBIF) * NUMFRQ
         DO 310 LOOPF = 1,NUMFRQ
            LNX = LNX + 1
            IF ((WTS(LNX).LE.0.0) .OR.
     *         ((BUFF2(1,LNX).EQ.0.0) .AND. (BUFF2(2,LNX).EQ.0.0))) THEN
               BUFF2(1,LNX) = FBLANK
               BUFF2(2,LNX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(LNX)
               BUFF2(1,LNX) = BUFF2(1,LNX) * XNORM
               BUFF2(2,LNX) = BUFF2(2,LNX) * XNORM
               END IF
 310        CONTINUE
 320     CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      BLNKBF = .FALSE.
      IRET = 0
      IF (NVIS.LT.1) THEN
         MSGTXT = 'POSSCP: NO TABLE ENTRIES FOUND FOR ' // SNAME
         CALL MSGWRT (6)
         IRET = -1
         BLNKBF = .TRUE.
      ELSE
         WRITE (MSGTXT,1006) NVIS
         CALL MSGWRT (4)
         END IF
C                                       Fill in values for output
C                                       file labeling
      SRCOBS = SAUCE
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (NVIS.GT.0) AVWGHT = 1.0
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
      CALL TABCP ('CLOS', CPBUFF, ICPRNO, CPKOLS, CPNUMV, NIFCP, NCHNCP,
     *   SOURSE, SUID, VFLUX, IRET)
      IF (IRET.GT.0) GO TO 999
      IF (IERR.NE.0) IRET = IERR
      TSTART = -1000.
      TEND = 1.E6
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POSSCP: SOURCE NUMBER',I5,' NOT FOUND IN SU TABLE')
 1001 FORMAT ('POSSCP: YOU WANT',I6,' CHANNELS PLOTTED, ONLY HAVE',
     *   I6,' IN THE TABLE')
 1002 FORMAT ('POSSCP: Total number of table rows =',I6)
 1006 FORMAT ('POSSCP: Averaged ',I5,' table entries')
 1020 FORMAT ('POSSCP: REQUESTED IF (',I3,') > MAX IN TABLE (',I3,')')
      END
      SUBROUTINE POSSPC (KANT, IRET)
C-----------------------------------------------------------------------
C   POSSPC is designed to extract pulse-cal tones from a PC table
C   average them and pass them into the plotting array.
C   Input from common:
C      NCOUNT      I     If > 0 then will plot multiple plots/page,
C                        if so POSSPC will be called multiple times and
C                        will return 1 antenna/call
C   INPUT:
C      KANT        I     Antenna number to be passed if NCOUNT > 0
C   Output:
C      IRET        I     Return error code, 0=>OK, otherwise error.
C                                          10=>no valid data
C   Output in common:
C      BUFF2       R(2,MAXCIF)   Buffer containing averaged spectrum.
C-----------------------------------------------------------------------
      INTEGER   KANT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INTEGER  IFNO, LUNPC, LUN3, I, J, NROWS, LNX, SOURID,
     *   SUBA, LOOPF, IFRQ, IPOLC, I4TEMP, FREQID, HM(2),
     *   IPOL, IERR, IP2, ANT, PCKOLS(MAXPCC),
     *   PCNUMV(MAXPCC), PCBUFF(512), IPCRNO, NIFPC, NPOLPC, NUMTON
      REAL     XCOUNT, SUMWT(MAXIF,2), XNORM, INTERV, SUMWTS,
     *   STATE(2,4,MAXIF), PCREAL(2,MAXTON,MAXIF),
     *   PCIMAG(2,MAXTON,MAXIF), PCRATE(2,MAXTON,MAXIF)
      DOUBLE PRECISION TIME, DTEMP, CATD(128), CABCAL,
     *   PCFREQ(2,MAXTON,MAXIF)
      CHARACTER CHSIGN*1
      LOGICAL   AVGPOL, F, DIDMSG
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATD)
      SAVE DIDMSG
      DATA LUNPC, LUN3 /27, 29/
      DATA F /.FALSE./
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
      DO 10 I = 1,50
         ANTENS(I) = XA1(I)
 10      CONTINUE
      CALL SOUFIL (IRET)
C                                       Fill in antenna name
      IF (NCOUNT.GE.1) THEN
         ANTNAM(1) = STNS(KANT)
         TELNUM(1) = KANT
         ANTNAM(2) = ' '
      ELSE
         IF (ANTENS(1).GE.1) THEN
            TELNUM(1) = ANTENS(1)
            ANTNAM(1) = STNS (ANTENS(1))
            ANTNAM(2) = '*'
            END IF
         END IF
C                                       Be clever for labelling
C                                       of plot in POSSLB
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      DOFQSL = ILOCFQ.GT.0
C                                       Initialize PC table
      CALL PCINI ('READ', PCBUFF, DISKIN, CNOIN, BPVER, CATBLK, LUNPC,
     *   IPCRNO, PCKOLS, PCNUMV, NPOLPC, NIFPC, NUMTON, IRET)
      IF (IRET.NE.0) GO TO 999
      I4TEMP = PCBUFF(5)
      NROWS = I4TEMP
C                                       Check IFs
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (NUMIF.GT.NIFPC) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IFNO, NIFPC
         GO TO 990
         END IF
C                                       Check polzn
      IPOLC = 1
      AVGPOL = F
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NPOLPC.GE.1) THEN
         IF ((NUMPOL.LE.1) .OR. (NPOLPC.EQ.1)) THEN
            POLNUM = 1
            IF (ICOR0.EQ.1) THEN
               IPOLC = 1
               STOKES = 'I   '
            ELSE IF (ICOR0.EQ.-1) THEN
               IPOLC = 1
               STOKES = 'RR  '
            ELSE IF (ICOR0.EQ.-2) THEN
               IPOLC = 2
               STOKES = 'LL  '
            ELSE IF (ICOR0.EQ.-5) THEN
               IPOLC = 1
               STOKES = 'XX  '
            ELSE IF (ICOR0.EQ.-6) THEN
               IPOLC = 2
               STOKES = 'YY  '
               END IF
            POLLAB(1) = STOKES(1:1)
         ELSE IF (NUMPOL.GT.1) THEN
            IF (STOKES.EQ.'I') THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'I'
               AVGPOL = ICOR0.EQ.-1
               IF (AVGPOL) POLLAB(1) = '(R+L)/2'
            ELSE IF ((STOKES.EQ.'V') .AND. (ICOR0.EQ.-1)) THEN
               IPOLC = 1
               POLNUM = 1
               AVGPOL = .TRUE.
               POLLAB(1) = '(R-L)/2'
            ELSE IF ((STOKES.EQ.'R') .OR. (STOKES.EQ.'RR')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'R'
            ELSE IF ((STOKES.EQ.'L') .OR. (STOKES.EQ.'LL')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'L'
            ELSE IF ((STOKES.EQ.'X') .OR. (STOKES.EQ.'XX')) THEN
               IPOLC = 1
               POLNUM = 1
               POLLAB(1) = 'X'
            ELSE IF ((STOKES.EQ.'Y') .OR. (STOKES.EQ.'YY')) THEN
               IPOLC = 2
               POLNUM = 1
               POLLAB(1) = 'Y'
            ELSE
               STOKES = 'HALF'
               IPOLC = 1
               POLNUM = 2
               IF (ICOR0.LT.-4) THEN
                  POLLAB(1) = 'X'
                  POLLAB(2) = 'Y'
               ELSE
                  POLLAB(1) = 'R'
                  POLLAB(2) = 'L'
                  END IF
               END IF
            END IF
         END IF
C                                       Determine size of spectrum
C                                       to be plotted
      PBCH = MAX (1, PBCH)
      NUMFRQ = PECH - PBCH + 1
      IF ((NUMFRQ.EQ.0) .OR. (PECH.GT.NUMTON) .OR. (NUMFRQ.GT.NUMTON))
     *   THEN
         NUMFRQ = NUMTON
         PBCH = 1
         PECH = NUMTON
         END IF
C                                       Zero output array
      CALL RFILL (2*MAXCIF, 0.0, BUFF2)
      CALL RFILL (MAXCIF, 0.0, WTS)
      DO 30 IFNO = 1,NUMIF
         SUMWT(IFNO,1) = 0.0
         SUMWT(IFNO,2) = 0.0
 30      CONTINUE
      SUMWTS = 0.0
C                                       Initialize for data
C                                       selection
      IF ((TIMRNG(1) + TIMRNG(2) + TIMRNG(3) + TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5) + TIMRNG(6) + TIMRNG(7) + TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IF (TEND.EQ.0.0) TEND = 1.0E10
C                                       Main averaging loop
      XCOUNT = 0
      WRITE (MSGTXT,1002) NROWS
      IF (.NOT.DIDMSG) CALL MSGWRT (3)
      DIDMSG = .TRUE.
      DO 200 I = 1,NROWS
         IF (IPCRNO.GT.NROWS) GO TO 300
C
         CALL TABPC ('READ', PCBUFF, IPCRNO, PCKOLS, PCNUMV, NPOLPC,
     *      TIME, INTERV, SOURID, ANT, SUBA, FREQID, CABCAL, STATE,
     *      PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Data selection
C                                       Time
         IF (TIME.LT.TSTART) GO TO 200
         IF (TIME.GT.TEND) GO TO 200
C                                       Antennas
         IF (NCOUNT.GE.1) THEN
            IF (ANT.EQ.KANT) GO TO 60
            GO TO 200
            END IF
         IF (NANTSL.LE.0) GO TO 60
         DO 50 J = 1,NANTSL
            IF ((ANT.EQ.ANTENS(J)) .AND. (DOAWNT)) GO TO 60
            IF ((ANT.EQ.ANTENS(J)) .AND. (.NOT.DOAWNT)) GO TO 200
 50         CONTINUE
         IF (.NOT.DOAWNT) GO TO 60
         GO TO 200
C                                       Check subarray
 60      IF ((SUBARR.GT.0) .AND. (SUBA.NE.SUBARR) .AND. (SUBA.GT.0))
     *      GO TO 200
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND. (FREQID.GT.0))
     *      GO TO 200
C                                       Sources
         IF ((NSOUWD.LE.0) .OR. (SOURID.EQ.0)) GO TO 100
         DO 80 J = 1,NSOUWD
            IF (SOURID.EQ.SOUWAN(J)) GO TO 90
 80         CONTINUE
         IF (.NOT.DOSWNT) GO TO 100
         GO TO 200
 90      IF (DOSWNT) GO TO 100
         GO TO 200
C                                       Average
 100     XCOUNT = XCOUNT + 1
         IF (IPOLC.EQ.0) IPOLC = 1
         IF (AVGPOL) IPOLC = 1
         IF (NPOLBP.LT.2) IPOLC = 1
         IP2 = IPOLC + POLNUM - 1
         DO 150 IPOL = IPOLC,IP2
         DO 150 IFNO = PBIF,PEIF
            LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
            IF (AVGPOL) THEN
               DO 135 IFRQ = PBCH,PECH
                  LNX = LNX + 1
                  IF ((PCREAL(1,IFRQ,IFNO).NE.FBLANK) .AND.
     *               (PCREAL(2,IFRQ,IFNO).NE.FBLANK) .AND.
     *               (PCIMAG(1,IFRQ,IFNO).NE.FBLANK) .AND.
     *               (PCIMAG(2,IFRQ,IFNO).NE.FBLANK)) THEN
                     BUFF2(1,LNX) = BUFF2(1,LNX) + PCREAL(1,IFRQ,IFNO)
     *                  + PCREAL(2,IFRQ,IFNO)
                     BUFF2(2,LNX) = BUFF2(2,LNX) + PCIMAG(1,IFRQ,IFNO)
     *                  + PCIMAG(2,IFRQ,IFNO)
                     WTS(LNX) = WTS(LNX) + 2.
                     SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + 2.
                     SUMWTS = SUMWTS + 1.0
                     END IF
 135              CONTINUE
            ELSE
               DO 140 IFRQ = 1,NUMFRQ
                  LNX = LNX + 1
                  IF ((PCREAL(IPOL,IFRQ,IFNO).NE.FBLANK) .AND.
     *               (PCIMAG(IPOL,IFRQ,IFNO).NE.FBLANK)) THEN
                     BUFF2(1,LNX) = BUFF2(1,LNX) +PCREAL(IPOL,IFRQ,IFNO)
                     BUFF2(2,LNX) = BUFF2(2,LNX) +PCIMAG(IPOL,IFRQ,IFNO)
                     WTS(LNX) = WTS(LNX) + 1.
                     SUMWT(IFNO,IPOL) = SUMWT(IFNO,IPOL) + 1.
                     SUMWTS = SUMWTS + 1.0
                     END IF
 140              CONTINUE
               END IF
 150        CONTINUE
 200     CONTINUE
C                                       Average the output array
 300  DO 320 IPOL = IPOLC,IP2
      DO 320 IFNO = PBIF,PEIF
         IF ((SUMWT(IFNO,IPOL).LE.0.0) .AND. (SUMWTS.GT.0.0)) THEN
            WRITE (MSGTXT, 1300) IFNO, IPOL
            CALL MSGWRT (6)
            END IF
         LNX = (IFNO - PBIF + (IPOL-IPOLC)*(PEIF-PBIF+1)) * NUMFRQ
         DO 310 LOOPF = 1,NUMFRQ
            LNX = LNX + 1
            IF ((WTS(LNX).LE.0.0) .OR.
     *         ((BUFF2(1,LNX).EQ.0.0) .AND. (BUFF2(2,LNX).EQ.0.0))) THEN
               BUFF2(1,LNX) = FBLANK
               BUFF2(2,LNX) = FBLANK
            ELSE
               XNORM = 1.0 / WTS(LNX)
               BUFF2(1,LNX) = BUFF2(1,LNX) * XNORM
               BUFF2(2,LNX) = BUFF2(2,LNX) * XNORM
               END IF
 310        CONTINUE
 320     CONTINUE
C                                       Finish up
      NVIS = XCOUNT
      BLNKBF = .FALSE.
      IRET = 0
      IF (NVIS.LT.1) THEN
         MSGTXT = 'POSSPC: NO TABLE ENTRIES SELECTED - CHECK INPUT ' //
     *      'PARMS'
         IF (XSOLIN.EQ.0.0) CALL MSGWRT (6)
         IRET = -1
         BLNKBF = .TRUE.
      ELSE
         WRITE (MSGTXT,1006) NVIS
         CALL MSGWRT (4)
         END IF
C                                       Fill in values in DSOU.INC
      CALL GETSOU (SOURID, IUDISK, IUCNO, CATUV, LUN3, IERR)
      IF (IERR.NE.0) THEN
         IRET = IERR
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       time varying
C                                       Fill in values for output
C                                       file labeling
      SRCOBS = SAUCE
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (NVIS.GT.0) AVWGHT = SUMWT(1,1) / NVIS
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
      CALL TABPC ('CLOS', PCBUFF, IPCRNO, PCKOLS, PCNUMV, NPOLPC,
     *   TIME, INTERV, SOURID, ANT, SUBA, FREQID, CABCAL, STATE,
     *   PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
      IF (IERR.NE.0) IRET = IERR
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT ('POSSPC: Total number of table rows =',I6)
 1006 FORMAT ('POSSPC: Averaged ',I5,' table entries')
 1010 FORMAT ('POSSPC: GETSOU RETURNED ERROR',I5)
 1020 FORMAT ('POSSPC: REQUESTED IF (',I3,') > MAX IN TABLE (',I3,')')
 1300 FORMAT ('POSSPC: Warning, no valid data for IF/POL: ',2I2)
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
C                       15 => TAPLT, 16 => POSSM, 17 => SNPLT
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
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IGSIZE /0/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
      APARM(3) = MINAMP
      APARM(4) = MAXAMP
      APARM(5) = MINPHS
      APARM(6) = MAXPHS
      XBIF = STRTIF
      XEIF = STOPIF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      IF (.NOT.SMTHIT) THEN
         XSMOTH(1) = SMOOTH(1)
         XSMOTH(2) = SMOOTH(2)
         XSMOTH(3) = SMOOTH(3)
         END IF
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
      IF (IERR.EQ.0) GO TO 999
         GPHIND = 0
         IERR = IERR + 40
C
 980  IF (.NOT.DOTV) CALL DELEXT ('PL', DISKIN, SLOT, 'WRIT', CATBLK,
     *   IWBLK, IVER, IER)
C
 999  RETURN
      END
      SUBROUTINE POSSLB (IFNO, IERR)
C-----------------------------------------------------------------------
C   POSSLB provides the global labels surrounding the plot produced by
C   POSSPL.
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
      CHARACTER TEXT*132, ATIME*8, CTEMP*8, ADATE*12, SCAL*44, VECT*44,
     *   ACS(3)*44, BPS*44, ACFNC(3)*44, XCFNCV*44, XCFNCS*44, PCS*64,
     *   CTEMP1*18, CT1*1, CT2*1, BDS*44, PDS*44, CPS*44, RCPS*44
      HOLLERITH CATH(256)
      INTEGER   IERR, INCHAR, ID(3), IT(3), IANGLE, IFNO, IT1(4),
     *   IT2(4), ITRIM, NPL, LTYPE
      REAL      DX, DY, CATR(256), PBW, T1, T2
      DOUBLE PRECISION PFREQ
      LOGICAL MULTIF
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER BNDCOD(MAXIF)*8
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA SCAL  /'Scalar averaged cross-power spectrum'/,
     *     VECT  /'Vector averaged cross-power spectrum'/,
     *     ACS   /'Auto-corr total-power spectrum',
     *            'Auto-corr total & cross-power spectrum',
     *            'Auto-corr cross-power spectrum'/,
     *     BPS   /'Bandpass table spectrum'/,
     *     BDS   /'BLCHN table spectrum'/,
     *     PDS   /'PCAL antenna solution spectrum'/,
     *     CPS   /'PCAL source polarization spectrum'/,
     *     PCS   /'Pulse-cal tone spectrum'/,
     *     RCPS  /'PCAL source relative polarization spectrum'/,
     *     ACFNC /'Autocorrelation function',
     *            'Auto & cross-correlation function',
     &            'Cross-correlation function'/,
     *     XCFNCV /'Vector averaged cross-corr. fn.'/,
     *     XCFNCS /'Scalar averaged cross-corr. fn.'/
C-----------------------------------------------------------------------
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      MULTIF = PEIF.NE.PBIF
      MULTIF = MULTIF .AND. (NCOUNT.GE.1)
      MULTIF = MULTIF .AND. (.NOT.DOCHIF)
      NPL = POLNUM
      IF (DOCHPL) NPL = 1
      IF (MULTIF) NPL = NPL * (PEIF - PBIF + 1)
C                                       Date/time/version
      DX = 0.0
      DY = CHOUT(4) - 1.5
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
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
C                                       File name label & source name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (MULTI) THEN
            IF (SCANAV) THEN
               IF (SRCOBS(1:1).EQ.'-') THEN
                  TEXT = SRCOBS(2:)
               ELSE
                  TEXT = SRCOBS(1:)
                  END IF
            ELSE
               IF (SAUCE(1:1).EQ.'-') THEN
                  TEXT = SAUCE(2:)
               ELSE
                  TEXT = SAUCE(1:)
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
               WRITE (MSGTXT,1080) IERR
               GO TO 990
               END IF
            PFREQ = PFQFRQ(IFNO) / 1.0D9
            PBW = PFQTBW(IFNO) / 1.0E6
            WRITE (TEXT,1090) PFREQ, PBW
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
               WRITE (TEXT(INCHAR:),1100) CLUSE
            ELSE
               WRITE (TEXT(INCHAR:),1110) CLUSE
               END IF
         ELSE
            TEXT(INCHAR:) = 'No calibration applied__'
            END IF
         IF (BPPLOT) THEN
            WRITE (TEXT(INCHAR:),1120) BPVER
         ELSE IF (BDPLOT) THEN
            WRITE (TEXT(INCHAR:),1121) BPVER
         ELSE IF (PDPLOT) THEN
            WRITE (TEXT(INCHAR:),1122) BPVER
         ELSE IF (CPPLOT) THEN
            WRITE (TEXT(INCHAR:),1123) BPVER
         ELSE IF (PCPLOT) THEN
            WRITE (TEXT(INCHAR:),1124) BPVER
         ELSE
            INCHAR = ITRIM (TEXT)
            IF (DOBAND.GT.0) THEN
               IF (DOCAL) THEN
                  WRITE (TEXT(INCHAR:),1130) BPVER, DOBAND
               ELSE
                  WRITE (TEXT(INCHAR:),1140) BPVER, DOBAND
                  END IF
            ELSE IF (DOBAND.LE.0) THEN
               IF (DOCAL) THEN
                  TEXT(INCHAR:) = 'but no bandpass applied'
               ELSE
                  TEXT(INCHAR:) = 'and no bandpass applied'
                  END IF
               END IF
            END IF
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Velocity subscript
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
         IF (BPPLOT) THEN
            TEXT = BPS
         ELSE IF (BDPLOT) THEN
            TEXT = BDS
         ELSE IF (PDPLOT) THEN
            TEXT = PDS
         ELSE IF (CPPLTI) THEN
            TEXT = RCPS
         ELSE IF (CPPLOT) THEN
            TEXT = CPS
         ELSE IF (PCPLOT) THEN
            TEXT = PCS
         ELSE IF (AUTO) THEN
            IF (ACF) THEN
               TEXT = ACFNC(CROSS+2)
            ELSE
               TEXT = ACS(CROSS+2)
               END IF
         ELSE IF ((.NOT.AUTO1)  .AND.  (.NOT.TAPLOT)) THEN
            IF (SCALAR) THEN
               IF (XCF) THEN
                  TEXT = XCFNCS
               ELSE
                  TEXT = SCAL
                  END IF
            ELSE
               IF (XCF) THEN
                  TEXT = XCFNCV
               ELSE
                  TEXT = VECT
                  END IF
               END IF
            END IF
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         TEXT(INCHAR+1:) = '___'
         INCHAR = INCHAR+5
C                                       antennas
         IF ((AUTO) .OR. (TAPLOT)) THEN
            IF (NCOUNT.EQ.0) THEN
               TEXT(INCHAR:) = 'Several antennas averaged'
               IF ((NANTSL.EQ.1) .AND. (DOAWNT))
     *            WRITE (TEXT(INCHAR:),1025) STNS(ANTENS(1)), ANTENS(1)
            ELSE
               TEXT(INCHAR:) = 'Antenna: *'
               IF ((NANTSL.EQ.1) .AND. (DOAWNT) .AND. (NPL.GE.NCOUNT))
     *            WRITE (TEXT(INCHAR:),1025) STNS(ANTENS(1)), ANTENS(1)
               END IF
            IF (CPPLOT) TEXT(INCHAR:) = ' '
            IF ((PDPLOT) .AND. (NCOUNT.GT.0)) TEXT(INCHAR:) = ' '
         ELSE
            IF (NANTSL.EQ.2) WRITE (TEXT(INCHAR:),1040) STNS(ANTENS(1)),
     *         ANTENS(1), STNS(ANTENS(2)), ANTENS(2)
            IF (NANTSL.EQ.0) TEXT(INCHAR:) = 'Baseline: * - *'
            IF ((NANTSL.GT.2) .AND. (NCOUNT.LT.1))
     *         TEXT(INCHAR:) = 'Several baselines averaged'
            IF (NPL.LT.NCOUNT)
     *         TEXT(INCHAR:) = 'Several baselines displayed'
            END IF
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
            WRITE (TEXT,1020) CT1, IT1, CT2, IT2
            CALL REFRMT (TEXT, '_', INCHAR)
            DX = 0.0
            CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GCHAR (INCHAR, IANGLE, DX, DY, TEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DY = DY - 1.333
            END IF
C                                       UV range
         IF ((.NOT.AUTO) .AND. (.NOT.TAPLOT) .AND.
     *      ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9))) THEN
            TEXT = ' '
            IF (UVRNG(1).EQ.1.E-9) UVRNG(1) = 0.0
            WRITE (TEXT,1010) UVRNG
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
 1010 FORMAT ('UVrange:_ ',1PE10.3,' TO ',E10.3,' Klambda')
 1020 FORMAT ('Timerange:',A,I3.2,'/',2(I2.2,':'),I2.2,' to ',A,I3.2,
     *   '/',2(I2.2,':'),I2.2)
 1025 FORMAT ('Antenna: ',A8,'(',I2.2,')')
 1040 FORMAT ('Baseline:  ',A8,'(',I2.2,')',' - ',A8,'(',I2.2,')')
 1050 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1080 FORMAT ('POSSLB: ERROR ',I3,' GETTING FQ INFO FOR PLOT')
 1090 FORMAT ('Freq = ',F8.4,' GHz, Bw = ',F8.3,' MHz')
 1100 FORMAT ('Calibrated with CL #',I4,'__')
 1110 FORMAT ('Calibrated with SN #',I4,'__')
 1120 FORMAT ('Bandpass table #',I4)
 1121 FORMAT ('BLCHN spectral table #',I4)
 1122 FORMAT ('PCAL antenna spectral table #',I4)
 1123 FORMAT ('PCAL source spectral table #',I4)
 1124 FORMAT ('Pulse cal spectral table #',I4)
 1130 FORMAT ('and BP #',I4,' (BP mode ',I2,')')
 1140 FORMAT ('but used BP #',I4,' (BP mode ',I2,')')
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
      INCLUDE 'POSSM.INC'
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
      SUBROUTINE LABVEL (IFNO, VELLAB, OPTICL, ALTRFP, REFVEL, IERR)
C-----------------------------------------------------------------------
C   Routine to determine the velocity interval per channel and the
C   velocity of the first channel from information held either in the
C   alternate reference pixels, or the SU table.
C   Inputs:
C      IFNO     I     IF number being processed.
C   Outputs:
C      VELLAB   L     If TRUE have enough information to label x-axis
C                     with velocity tags - assumed TRUE on input
C      VELSUB   C*8   Frame of reference label for plot
C                     e.g. '(LSR)', '(HELIO)' etc.
C      IERR     I     If 0 all ok, otherwise cannot label
C   Outputs in common:
C      VELINC   R     Velocity interval per channel
C      VELFPX   R     Velocity of the first channel (pixel)
C-----------------------------------------------------------------------
      INTEGER   ALTAX, IERR, IFNO, CVER, OUTLEN, LCHAR, LUNP
      REAL      ALTRFP, NUX, CATR(256), REFPIX
      DOUBLE PRECISION REFF, REFVEL, CATD(128), VELITE
      LOGICAL   VELLAB, RADIO, OPTICL, MSG
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATUV, CATR, CATD)
      SAVE MSG
      DATA VELITE / 2.997924562D8 /
      DATA LUNP /30/
      DATA MSG /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
C                                        Set base reference freq.
      REFF  = CATD(KDCRV+KLOCFY)
C                                          Fill frequency table
      CVER = 1
      IF (.NOT.DIDCHN) THEN
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, CVER, CATUV, LUNP,
     *      NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.EQ.0) DIDCHN = .TRUE.
         END IF
C                                        Single-source file
      IF (.NOT.MULTI) THEN
         IF (IERR.NE.0) THEN
            CALL DFILL (MAXIF, 0.0D0, FOFF)
            CALL FILL (MAXIF, 1, ISBAND)
            CALL RFILL (MAXIF, CATR(KRCIC+KLOCFY), FINC)
            IERR = 0
            END IF
C                                        Cope with alternate ref.
C                                        pixels
         ALTAX = CATUV(KIALT)
         IF (ALTAX.LE.0) THEN
            MSGTXT = 'LABVEL: NO VELOCITY INFO AVAILABLE - USE ALTDEF'
            GO TO 990
            END IF
         OPTICL = .TRUE.
         IF (ALTAX.GT.256) THEN
            RADIO = .TRUE.
            OPTICL = .FALSE.
            ALTAX = ALTAX - 256
            END IF
         IF (ALTAX.EQ.1) THEN
            VELSUB = '(LSR)'
         ELSE IF (ALTAX.EQ.2) THEN
            VELSUB = '(HELIO)'
         ELSE
            VELSUB = ' '
            END IF
         ALTRFP = CATR(KRARP)
C                                       convert to pixel in another IF
         IF (FINC(IFNO).EQ.0.0) FINC(IFNO) = CATR(KRCIC+KLOCFY)
         IF (IFNO.NE.1) THEN
            ALTRFP = CATR(KRCRP+KLOCFY)+ (FOFF(1)-FOFF(IFNO)
     *      + (ALTRFP-CATR(KRCRP+KLOCFY))*FINC(1)) / FINC(IFNO)
            MSGTXT = 'WARNING: VELOCITY EXTENDED FROM IF 1 TO ' //
     *         'HIGHER IFS'
            IF (MSG) CALL MSGWRT (6)
            MSG = .FALSE.
            END IF
         REFVEL = CATD(KDARV)
         NUX = REFF + FOFF(IFNO) + (ALTRFP - CATR(KRCRP+KLOCFY)) *
     *      FINC(IFNO)
         IF (OPTICL) THEN
            VELINC = -(FINC(IFNO) * (VELITE + REFVEL)) /  NUX
            IF (ISBAND(IFNO).EQ.0) ISBAND(IFNO) = 1
            AXDENU(LOCNUM) = - VELINC / (VELITE + REFVEL)
            END IF
         IF (RADIO) THEN
            VELINC = -(FINC(IFNO) * (VELITE - REFVEL)) / NUX
            IF (ISBAND(IFNO).EQ.0) ISBAND(IFNO) = 1
            END IF
C                                          Multi source file
      ELSE
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
C                                          Determine velocity type
         IF (VELTYP.EQ.' ') THEN
            VELSUB = ' '
         ELSE
            VELSUB(1:1) = '('
            VELSUB(2:) = VELTYP(1:6)
            CALL CHTRIM (VELSUB, 8, VELSUB, OUTLEN)
            LCHAR = OUTLEN + 1
            IF (LCHAR.GT.8) LCHAR = 8
            VELSUB(LCHAR:LCHAR) = ')'
            END IF
C                                          Determine velocity definition
         IF (VELDEF(1:8).EQ.'RADIO') THEN
            RADIO = .TRUE.
         ELSE IF (VELDEF(1:8).EQ.'OPTICAL') THEN
            OPTICL = .TRUE.
         ELSE
            MSGTXT = 'LABVEL: UNKNOWN VELOCITY DEFINITION'
            VELLAB = .FALSE.
            GO TO 990
            END IF
C                                           Calculate parms
         REFPIX = CATR(KRCRP+KLOCFY)
         ALTRFP = REFPIX
         REFVEL = LSRVEL(IFNO)
C                                       check it
         IF ((IFNO.GT.1) .AND. (FOFF(IFNO).NE.FOFF(1)) .AND.
     *      (LSRVEL(IFNO).EQ.LSRVEL(1)) .AND.
     *      (RESTFQ(IFNO).EQ.RESTFQ(1))) THEN
            ALTRFP = REFPIX + (FOFF(1) - FOFF(IFNO)) / FINC(IFNO)
            MSGTXT = 'WARNING: VELOCITY EXTENDED FROM IF 1 TO ' //
     *         'HIGHER IFS'
            IF (MSG) CALL MSGWRT (6)
            MSG = .FALSE.
            END IF
         NUX = REFF + FOFF(IFNO) + (ALTRFP - REFPIX) * FINC(IFNO)
C??         NUX = (REFF + FOFF(IFNO)) + FINC(IFNO)*(REFPIX-1.0)
         IF (OPTICL) THEN
            VELINC = - (FINC(IFNO) * (VELITE + REFVEL)) / NUX
            IF (ISBAND(IFNO).EQ.0) ISBAND(IFNO) = 1
            AXDENU(LOCNUM) = - VELINC / (VELITE + REFVEL)
            END IF
         IF (RADIO) THEN
            VELINC = - (FINC(IFNO) * (VELITE - REFVEL)) / NUX
            IF (ISBAND(IFNO).EQ.0) ISBAND(IFNO) = 1
            END IF
         END IF
      VELFPX = REFVEL + VELINC * (1.0 - ALTRFP)
      AXINC(1,LOCNUM) = XMULT * VELINC / 1000.0
      AXDENU(LOCNUM) = XMULT * AXDENU(LOCNUM) / 1000.0
      AXFUNC(1,LOCNUM) = 1
      RPVAL(1,LOCNUM) = (REFVEL + VELINC * (PBCH-1 - ALTRFP)) / 1000.0
      IF (REVERS) THEN
         RPVAL(1,LOCNUM) = RPVAL(1,LOCNUM) + (CHNUM+1)*VELINC/1000.0
         AXINC(1,LOCNUM) = -AXINC(1,LOCNUM)
         AXDENU(LOCNUM) = -AXDENU(LOCNUM)
         END IF
      VELFPX = VELFPX / 1000.0
      VELINC = VELINC / 1000.
      REFVEL = REFVEL / 1000.0D0
      CPREF(1,LOCNUM) = 'KM/S'
      CTYP(1,LOCNUM) = VELSUB
      GO TO 995
C
 990  CALL MSGWRT (8)
      IERR = 1
C
 995  IF (IERR.NE.0) VELLAB = .FALSE.
      IF (.NOT.VELLAB) VELSUB = ' '
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('LABVEL: ERROR ',I3,' RETURNED BY CHNDAT')
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
C      BUFF2    R(2,MAXCIF)   Array containing the averaged spectrum.
C   Output:
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      REAL   RD, MAXRHS, MINRHS, RT, AVG2, RMS2, SUM2, SQ2, AMPAVG,
     *   AMPRMS, AMPSUM, AMPSQ
      INTEGER   IRET, IERR, JIF, IADR, I, J, IFNO, NPLOT, INX, IOFF, K,
     *   IPL, IPOL
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
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
      CHNUM = NUMFRQ
      IF (XCF .OR. ACF) CHNUM = NUMXCF
      IF (NUMPOL*CHNUM*NUMIF.GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'FILLPL: SPECTRA TOO BIG FOR BUFFERS'
         END IF
C                                       Describe operation
      I = EIF - BIF + 1
      IF ((DOCHIF) .AND. (DOCHPL)) THEN
         WRITE (MSGTXT,1000) CHNUM, I, POLNUM
      ELSE IF (DOCHIF) THEN
         WRITE (MSGTXT,1005) CHNUM, I, POLLAB(IPOL)
      ELSE IF (DOCHPL) THEN
         WRITE (MSGTXT,1010) CHNUM, IFNO, POLNUM
      ELSE
         WRITE (MSGTXT,1015) CHNUM, IFNO, POLLAB(IPOL)
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
         IF (CODTYP.LT.6) THEN
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
      NCTOT = (STOPIF - STRTIF + 1) * CHNUM * (STOPOL - STRPOL + 1)
      NPNL = NCTOT / CHNUM
      CALL RFILL (NCTOT, 0.0, AMP)
      CALL RFILL (NCTOT, 0.0, PHASE)
      DO 15 IPL = STRPOL,STOPOL
      DO 15 JIF = STRTIF,STOPIF
         INX = (JIF - BIF + (IPL-1)*(EIF-BIF+1)) * CHNUM
         CALL PSMOTH (CHNUM, BUFF2(1,INX+1), SCAMP(INX+1))
         IADR = (JIF - STRTIF + (IPL-STRPOL)*(STOPIF-STRTIF+1)) * CHNUM
         DO 10 I = 1,CHNUM
            IADR = IADR + 1
            INX = INX + 1
            IF ((BUFF2(1,INX).EQ.FBLANK) .OR. (BUFF2(2,INX).EQ.FBLANK))
     *         THEN
               AMP(IADR) = FBLANK
               PHASE(IADR) = FBLANK
            ELSE
               NPLOT = NPLOT + 1
               IF ((AUTO1) .OR. (CODTYP.GT.5)) THEN
                  AMP(IADR) = BUFF2(1,INX)
               ELSE IF (SCALAR) THEN
                  AMP(IADR) = SCAMP(INX)
               ELSE
                  AMP(IADR) = SQRT (BUFF2(1,INX)*BUFF2(1,INX) +
     *               BUFF2(2,INX)*BUFF2(2,INX))
                  END IF
               AMPSUM = AMPSUM + AMP(IADR)
               AMPSQ = AMPSQ + AMP(IADR) * AMP(IADR)
               IF ((CODTYP.EQ.2) .OR. (CODTYP.EQ.4)) AMP(IADR) =
     *            BPARM(5) * LOG10 (MAX (1.E-6, AMP(IADR)))
               IF (SELFSA) THEN
                  IF (AMP(IADR).GT.MAXAMP) MAXAMP = AMP(IADR)
                  IF (AMP(IADR).LT.MINAMP) MINAMP = AMP(IADR)
                  END IF
               IF ((BUFF2(1,INX).EQ.0.) .AND. (BUFF2(2,INX).EQ.0.))
     *            THEN
                  PHASE(IADR) = 0.
               ELSE IF (CODTYP.GT.5) THEN
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
                  IF (CODTYP.LT.6) THEN
                     IF (RT.LT.0.0) RT = RT + 360.
                     IF (RT.GT.MAXRHS) MAXRHS = RT
                     IF (RT.LT.MINRHS) MINRHS = RT
                     END IF
               ELSE
                  IF ((CODTYP.LT.6) .AND. ((PHASE(IADR).GT.MAXPHS) .OR.
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
                  END IF
               END IF
 10         CONTINUE
 15      CONTINUE
C                                       fix +- 180 problem
      IF ((SELFSP) .AND. (MAXPHS-MINPHS.GT.MAXRHS-MINRHS).AND.
     *   (CODTYP.LT.6)) THEN
         DO 40 I = 1,NCTOT
            IF (PHASE(I).LT.0.0) PHASE(I) = PHASE(I) + 360.
 40         CONTINUE
         MAXPHS = MAXRHS
         MINPHS = MINRHS
         END IF
C                                       Reverse spectrum?
      IF (REVERS) THEN
         DO 60 K = 1,NPNL
            IOFF = (K - 1) * CHNUM
            DO 50 I = 1,CHNUM
               J = CHNUM - I + 1
               IF (J.GT.I) THEN
                  RT = AMP(I+IOFF)
                  AMP(I+IOFF) = AMP(J+IOFF)
                  AMP(J+IOFF) = RT
                  RT = PHASE(I+IOFF)
                  PHASE(I+IOFF) = PHASE(J+IOFF)
                  PHASE(J+IOFF) = RT
                  END IF
 50            CONTINUE
 60         CONTINUE
         END IF
C                                       Non blank plot file?
      IF (NPLOT.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                       Print mean, rms
      IF (NPLOT.GT.0) THEN
         AMPAVG = AMPSUM / NPLOT
         AMPRMS = AMPSQ / NPLOT - AMPAVG * AMPAVG
         IF (AMPRMS.GT.0.0) AMPRMS = SQRT (AMPRMS)
         IF (CODTYP.LE.5) THEN
            WRITE (MSGTXT,1020) 'Amplitude', AMPAVG, AMPRMS
         ELSE
            WRITE (MSGTXT,1020) 'Real part', AMPAVG, AMPRMS
            END IF
         CALL MSGWRT (4)
         IF ((AMPRMS.EQ.0.0) .AND. (SELFSA)) THEN
            MAXAMP = AMPAVG + MAX (0.001, 0.01*AMPAVG)
            MINAMP = AMPAVG - MAX (0.001, 0.01*AMPAVG)
            END IF
         AVG2 = SUM2 / NPLOT
         RMS2 = SQ2 / NPLOT - AVG2 * AVG2
         IF (RMS2.GT.0.0) RMS2 = SQRT (RMS2)
         IF (CODTYP.LE.5) THEN
            WRITE (MSGTXT,1020) 'Phase', AVG2, RMS2
         ELSE
            WRITE (MSGTXT,1020) 'Imaginary', AVG2, RMS2
            END IF
         CALL MSGWRT (4)
         IF ((RMS2.EQ.0.0) .AND. (SELFSP)) THEN
            MAXPHS = AVG2 + MAX (1.0, 0.1*AVG2)
            MINPHS = AVG2 - MAX (1.0, 0.1*AVG2)
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Will plot',I6,' channels * ',I2,' IFs',I2,
     *   ' polarizations')
 1005 FORMAT ('Will plot',I6,' channels * ',I2,' IFs',
     *   ' polarization ',A)
 1010 FORMAT ('Will plot',I6,' spectral channels from IF# ',I2,
     *   ' * polarizations',I2)
 1015 FORMAT ('Will plot',I6,' spectral channels from IF# ',I2,
     *   ' polarization ',A)
 1020 FORMAT ('FILLPL: ',A,' mean:',1PE12.4,'  rms:',1PE12.4)
 1030 FORMAT ('FILLPL: PLOT BUFFER ALL BLANK - NO PLOT CREATED')
      END
      SUBROUTINE POSSPL (NPARM, IFNO, IRET)
C-----------------------------------------------------------------------
C   POSSPL does the plotting of the averaged spectrum plus all
C   the scaling and controls the labelling.
C   Inputs:
C      BUFF2    R(2,MAXCIF)   Array containing the averaged spectrum.
C      NPARM    I    Number of R   parms read form AIPS
C      IFNO     I    > 0 last plot, < 0 not last.
C   Output:
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LINE*132, PHNAME*48, ELABEL(2)*8
      REAL      DX, DY, SCALX, SCALP, SCALA, OFX, OFY, X, Y, CATR(256),
     *   PTEMP(4), TITSEC, VEL, PDATA(MAXCIF,2), ALTRFP, REFPIX, XRN,
     *   XFAC, PDMIN(2), PDMAX(2)
      DOUBLE PRECISION CATD(128), REFVEL, SFREQ, FREINC, CFREQ
      INTEGER   IRET, NPARM, DEPTH(5), IERR, LTYPE, BUFFI(256), JIF, I,
     *   IFNO, LUNPR, PFIND, ICHAN, TIT(3), NCH, ITRIM, JP, INP, J, LIF,
     *   IPNL, IO, IIP, IE, IEPO, IROUND
      LOGICAL   T, F, WPLOT, OPTICL, DOZERO, VELLAB, BLNK
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
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
      DATA ELABEL /' (B1950)', ' (J2000)'/
C-----------------------------------------------------------------------
      NXP = 1
      NYP = 1
      VELLAB = .TRUE.
      XFAC = ABS (FACTOR)
      IF (XFAC.GT.100.0) XFAC = XFAC - 100.0
      IF (XFAC.LT.0.4) XFAC = 1.0
C                                       Scaling
      AMPRNG = ABS (MAXAMP - MINAMP)
      PHSRNG = ABS (MAXPHS - MINPHS)
C                                       Amplitude range
      IF (AMPRNG.EQ.0.0) THEN
         MINAMP = 0.0
         MAXAMP = MAXAMP * 1.1
         IF (MAXAMP.LE.0.0) MAXAMP = 1.0
      ELSE IF (AMPRNG.NE.0) THEN
         MINAMP = MINAMP - 0.04 * AMPRNG
         MAXAMP = MAXAMP + 0.04 * AMPRNG
         IF ((MINAMP.GT.0.0) .AND. (MINAMP.LT.0.25*MAXAMP) .AND.
     *      (SELFSA)) MINAMP = 0.0
         END IF
C                                       Phase range
      IF (PHSRNG.EQ.0.0) THEN
         MINPHS = MINPHS - 5.5
         MAXPHS = MAXPHS + 5.5
      ELSE IF (PHSRNG.NE.0.0) THEN
         MINPHS = MINPHS - 0.06 * PHSRNG
         MAXPHS = MAXPHS + 0.06 * PHSRNG
         END IF
      AMPRNG = ABS (MAXAMP - MINAMP)
      PHSRNG = ABS (MAXPHS - MINPHS)
      PDMIN(1) = MINAMP
      PDMAX(1) = MAXAMP
      PDMIN(2) = MINPHS
      PDMAX(2) = MAXPHS
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      LINT = 1000.0
      IF (CODTYP.EQ.1) LINT = 700.
      IF (CODTYP.EQ.2) LINT = 700.
      IF (CODTYP.EQ.6) LINT = 500.
      IF (AUTO1) LINT = 1000.
C                                       Ensure correct scaling parms
C                                       written to PL file, then
C                                       create the PL file
      CALL RCOPY (4, APARM(3), PTEMP(1))
      CALL PLCREA (NPARM, PVER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      IF (DOTV) THEN
         XYRATI = (WINDTV(3) - WINDTV(1) + 1.0) /
     *      (WINDTV(4) - WINDTV(2) + 1.0)
      ELSE
         XYRATI = 9 / 5.5
         WRITE (MSGTXT,1050) PVER
         CALL MSGWRT (5)
         END IF
      IF (XYRTIO.GT.0.0) XYRATI = XYRTIO
      XYRTIO = XYRATI
      CALL RCOPY (4, PTEMP(1), APARM(3))
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL RFILL (4, 0.5, CHOUT)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
C                                       ????????
      IF (LTYPE.GT.2) THEN
         NCOUNT = -1 - ABS(NCOUNT)
         CALL LABLAX (OPTICL, REFVEL, ALTRFP, .TRUE., IRET)
         NCOUNT = -NCOUNT - 1
         CALL CHNTIC (BLC, TRC, INP)
         IF (CODTYP.EQ.1) INP = MAX (INP, 4)
         IF (CODTYP.EQ.2) INP = MAX (INP, 4)
         CHOUT(1) = INP + 4
         END IF
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(2) = CHOUT(2) + 1.333
         IF ((.NOT.AUTO1) .AND. (.NOT.TAPLOT) .AND.
     *      ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9)))
     *      CHOUT(2) = CHOUT(2) + 1.333
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4))
     *       CHOUT(2) = CHOUT(2) + 1.333
         IF (NCOUNT.GE.1) CHOUT(2) = CHOUT(2) + 1.333
         CHOUT(4) = 3.333
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) CHOUT(4) = CHOUT(4)+1.333
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
      CALL POSSLB (STRTIF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Label axes
      CALL LABLAX (OPTICL, REFVEL, ALTRFP, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scaling
      XRN = (TRC(1) - BLC(1)) / NPNL
      SCALX = XRN / (CHNUM+1)
      IF ((CODTYP.EQ.5) .OR. (CODTYP.EQ.8)) THEN
         SCALA = (LINT - BLC(2)) / PHSRNG
         OFY = BLC(2) - MINPHS * SCALA
         JP = 2
         DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0)
      ELSE
         SCALA = (LINT - BLC(2)) / AMPRNG
         IF (.NOT.AUTO1) SCALP = (TRC(2) - LINT) / PHSRNG
         OFY = BLC(2) - MINAMP * SCALA
         JP = 1
         DOZERO = (MINAMP.LT.0.0) .AND. (MAXAMP.GT.0.0) .AND.
     *      (CODTYP.GE.5)
         END IF
      OFX = 0.0
C                                       Plot the lower zero line
      IF (DOZERO) THEN
         CALL GLTYPE (1, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
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
         CALL GLTYPE (1, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         OFX = (IPNL - 1) * XRN
         IO = (IPNL - 1) * CHNUM
         DX = 5.0 / XYRATI * XFAC
         DY = 5.0 * XFAC
C                                       separate panels
         IF (IPNL.GT.1) THEN
            X = OFX
            CALL GPOS (X, BLC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL GVEC (X, TRC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
         CALL GLTYPE (4, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         IF ((CODTYP.EQ.5) .OR. (FACTOR.GE.0.0)) THEN
            DO 40 I = 1,CHNUM
               IF ((PDATA(I+IO,JP).NE.FBLANK) .AND.
     *            (PDATA(I+IO,JP).GE.PDMIN(JP)) .AND.
     *            (PDATA(I+IO,JP).LE.PDMAX(JP))) THEN
                  X = I * SCALX + OFX
                  Y = PDATA(I+IO,JP) * SCALA + OFY
                  CALL GMARK (X, Y, DX, DY, IRET)
                  IF (IRET.NE.0) GO TO 980
                  END IF
 40            CONTINUE
            END IF
         IF ((CODTYP.NE.5) .AND. (FACTOR.LT.100.0)) THEN
            BLNK = .TRUE.
            CALL GLTYPE (2, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            DO 50 I = 1,CHNUM
               IF ((PDATA(I+IO,JP).EQ.FBLANK) .OR.
     *            (PDATA(I+IO,JP).LT.PDMIN(JP)) .OR.
     *            (PDATA(I+IO,JP).GT.PDMAX(JP))) THEN
                  BLNK = .TRUE.
               ELSE
                  X = I * SCALX + OFX
                  Y = PDATA(I+IO,JP) * SCALA + OFY
                  IF (BLNK) THEN
                     CALL GPOS (X, Y, PLTBLK, IRET)
                  ELSE
                     CALL GVEC (X, Y, PLTBLK, IRET)
                     END IF
                  BLNK = .FALSE.
                  IF (IRET.NE.0) GO TO 980
                  END IF
 50            CONTINUE
            END IF
 55      CONTINUE
C                                       Plot the upper zero line
      OFY = LINT - SCALP * MINPHS
      DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0) .AND.
     *   ((CODTYP.LE.2) .OR. (CODTYP.EQ.6))
      IF (DOZERO) THEN
         CALL GLTYPE (1, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
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
         IO = (IPNL - 1) * CHNUM
         IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.2) .OR.
     *      ((CODTYP.EQ.6) .AND. (FACTOR.GE.0.0))) THEN
            CALL GLTYPE (4, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            DO 60 I = 1,CHNUM
               IF ((PDATA(I+IO,2).NE.FBLANK) .AND.
     *            (PDATA(I+IO,2).GE.PDMIN(2)) .AND.
     *            (PDATA(I+IO,2).LE.PDMAX(2))) THEN
                  X = I * SCALX + OFX
                  Y = PDATA(I+IO,2) * SCALP + OFY
                  CALL GMARK (X, Y, DX, DY, IRET)
                  IF (IRET.NE.0) GO TO 980
                  END IF
 60            CONTINUE
            END IF
         IF ((CODTYP.EQ.6) .AND. (FACTOR.LT.100.0)) THEN
            BLNK = .TRUE.
            CALL GLTYPE (2, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            DO 70 I = 1,CHNUM
               IF ((PDATA(I+IO,2).EQ.FBLANK) .OR.
     *            (PDATA(I+IO,2).LT.PDMIN(2)) .OR.
     *            (PDATA(I+IO,2).GT.PDMAX(2))) THEN
                  BLNK = .TRUE.
               ELSE
                  X = I * SCALX + OFX
                  Y = PDATA(I+IO,2) * SCALP + OFY
                  IF (BLNK) THEN
                     CALL GPOS (X, Y, PLTBLK, IRET)
                     IF (IRET.NE.0) GO TO 980
                  ELSE
                     CALL GVEC (X, Y, PLTBLK, IRET)
                     END IF
                  BLNK = .FALSE.
                  IF (IRET.NE.0) GO TO 980
                  END IF
 70            CONTINUE
            END IF
 75      CONTINUE
C                                       Write spectrum
      WPLOT = .FALSE.
      IF (OFILE(1:1).NE.' ') WPLOT = .TRUE.
      IF (WPLOT) THEN
         IEPO = IROUND (REPOCH(LOCNUM))
         IF (IEPO.EQ.1950) THEN
            IE = 1
         ELSE
            IE = 2
            END IF
         LUNPR = 10
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       First write header info.
         IF (BPARM(10).LE.0.0) THEN
            IF (BPPLOT) THEN
               WRITE (LINE,1150) 'BP', BPVER
            ELSE IF (BDPLOT) THEN
               WRITE (LINE,1150) 'BD', BPVER
            ELSE IF (PDPLOT) THEN
               WRITE (LINE,1150) 'PD', BPVER
            ELSE IF (CPPLOT) THEN
               WRITE (LINE,1150) 'CP', BPVER
            ELSE IF (PCPLOT) THEN
               WRITE (LINE,1150) 'PC', BPVER
            ELSE
               WRITE (LINE,1200)
               END IF
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Source name
            WRITE (LINE,1201) SRCOBS
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Ra/Dec
            WRITE (LINE,1202) RAHR, RAMIN, RASEC, DECDEG, DECMIN,
     *         DECSEC, ELABEL(IE)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Date/time
            IF (.NOT.TAPLOT) THEN
               CALL PTIME (CTIME, F, TIT, TITSEC)
               WRITE (LINE,1203) EXPDAT, TIT, TITSEC
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
C                                       # channels, vel
            WRITE (LINE,1204) NCHAN, PBCH
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       BW
            IF (.NOT.TAPLOT) THEN
               WRITE (LINE,1206) TBW
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
C                                       Antenna
            IF (TELNUM(1).GT.0) THEN
               WRITE (LINE,1207) TELNUM(1), ANTNAM(1)
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (ANTNAM(2)(1:1).NE.'*') THEN
                  WRITE (LINE,1207) TELNUM(2), ANTNAM(2)
                  NCH = ITRIM(LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
                  END IF
               END IF
C                                       Freq, weight
            WRITE (LINE,1208) MFREQ, AVWGHT
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (CODTYP.LT.6) THEN
               LINE = 'DATA follow with format:' //
     *            ' (1X,I5,2X,I3,2X,A5,2X,F12.4,2(2X,G15.6),2X,F8.3)'
               ELSE
                  LINE = 'DATA follow with format:' //
     *               ' (1X,I5,2X,I3,2X,A5,2X,F12.4,3(2X,G15.6))'
               END IF
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            LINE = 'Channel  IF  Polar     Frequency     Velocity' //
     *         '         Ampl(Jy)      Phase'
            IF (CODTYP.GE.6) LINE(55:) = 'Real(Jy)         Imag(Jy)'
            IF (JY.EQ.'K') LINE(55:) = ' Real(K)          Imag(K)'
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
         AXDENU(LOCNUM) = AXDENU(LOCNUM) / XMULT
C                                       List arrays.  The channels and
C                                       IFs will always be in numerical
C                                       order regardless of APARM(10).
         LIF = 0
         DO 90 IPNL = 1,NPNL
            IIP = STRPOL + (IPNL-1) / (STOPIF-STRTIF+1)
            JIF = STRTIF + IPNL - 1 - (IIP-STRPOL)*(STOPIF-STRTIF+1)
            IF (JIF.NE.LIF) THEN
               CALL LABVEL (JIF, VELLAB, OPTICL, ALTRFP, REFVEL, IRET)
               REFPIX = CATR(KRCRP+KLOCFY)
               SFREQ = CATD(KDCRV+KLOCFY) + FOFF(JIF)
               FREINC = FINC(JIF)
               LIF = JIF
               END IF
            DO 85 I = 1,CHNUM
               IF (REVERS) THEN
                  J = CHNUM + 1 - I
               ELSE
                  J = I
                  END IF
               J = J + (IPNL - 1) * CHNUM
               LINE = ' '
               ICHAN = I - 1 + PBCH
               CFREQ = (SFREQ + FREINC * (ICHAN - REFPIX)) / 1.E6
               IF (OPTICL) THEN
                  VEL = REFVEL + VELINC * (ICHAN - ALTRFP) /
     *               (1.0D0 + AXDENU(LOCNUM) * (ICHAN - ALTRFP))
               ELSE
                  VEL = VELFPX + VELINC * (ICHAN - 1.)
                  END IF
               IF ((AMP(J).EQ.FBLANK) .OR. (PHASE(J).EQ.FBLANK)) THEN
                  WRITE (LINE,1060) ICHAN, JIF, POLLAB(IIP),
     *               CFREQ, VEL, 'FLAGGED'
               ELSE IF (CODTYP.LT.6) THEN
                  WRITE (LINE,1070) ICHAN, JIF, POLLAB(IIP),
     *               CFREQ, VEL, AMP(J), PHASE(J)
               ELSE
                  WRITE (LINE,1071) ICHAN, JIF, POLLAB(IIP),
     *               CFREQ, VEL, AMP(J), PHASE(J)
                  END IF
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1080) IRET
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
 980  WRITE (MSGTXT,1100) IRET
      CALL MSGWRT (8)
 981  WRITE (MSGTXT,1110)
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
 1020 FORMAT ('POSSPL: ERROR ',I3,' RECEIVED FROM ZTXOPN')
 1040 FORMAT ('POSSPL: ERROR ',I3,' RECEIVED FROM PLCREA')
 1050 FORMAT ('Plot file version ',I3,' created')
 1060 FORMAT (1X,I5,2X,I3,2X,A,2X,F12.4,2X,G15.6,2X,A)
 1070 FORMAT (1X,I5,2X,I3,2X,A,2X,F12.4,2X,G15.6,2X,G15.6,2X,F8.3)
 1071 FORMAT (1X,I5,2X,I3,2X,A,2X,F12.4,2X,G15.6,2X,G15.6,2X,G15.6)
 1080 FORMAT ('POSSPL: ERROR ',I3,' RECEIVED FROM ZTXIO')
 1100 FORMAT ('POSSPL: ERROR',I5,' FROM PLOTTING ROUTINES')
 1110 FORMAT ('POSSPL: WILL TRY TO FINISH PARTIAL PLOT')
 1150 FORMAT (10X,A,' TABLE NUMBER',I2)
 1200 FORMAT ('Header information')
 1201 FORMAT ('Source: ',A)
 1202 FORMAT ('RA:  ',2I3,F6.2,8X,'DEC: ',2I3,F6.2,2X,A)
 1203 FORMAT ('OBS. DATE: ',A,10X,'Time of record: ',
     *   I3,'/',2I3,F5.1)
 1204 FORMAT ('No. channels:',I6,10X,' First channel plotted',I6)
 1206 FORMAT ('Bw (kHz): ',F10.3)
 1207 FORMAT ('Antenna #',I3,5X, 'name: ',A8)
 1208 FORMAT ('Rest freq. (MHz) : ',F12.4,5X,' Av. weight : ',F12.4)
      END
      SUBROUTINE LABLAX (OPTICL, REFVEL, ALTRFP, LAST, IRET)
C-----------------------------------------------------------------------
C   LABLAX controls the axis labelling for POSSM
C   NCOUNT < 0 => prepare coordinate common, do no labeling
C   Output:
C      OPTICL   L    Use optical velocity convention?
C      REFVEL   D    Reference velocity
C      ALTRFP   R    Alternate reference pixel.
C      IRET     I    Return error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER AXUNIT*8, XCTYP*16, XPREF*8, TEXT*80, CTEMP*3, PREF*8,
     *   PREF2*8
      REAL      XBLC(2), XTRC(2), CATR(256), YMULT, YMULT2, WBLC(2),
     *   WTRC(2), PDATA(MAXCIF,2), ATEMP, ALTRFP, NUMPIX, ZERPIX,
     *   DELINC, SDEL, OLDDEL, LBLC(2), LTRC(2), SHDX
      DOUBLE PRECISION CATD(128), REFVEL
      INTEGER   IRET, DEPTH(5), I, CVER, LUNP, WRVLAB, IPNL, WRV, IIF,
     *   INCHAR, IIP, IBP5, JTRIM
      LOGICAL   F, FRLAB, VELLAB, PFLG, OPTICL, CORRLB, WRHLAB, LAST,
     *   SHORT
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'POSS4.INC'
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8, LSTPOL*7
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
      SAVE YMULT, YMULT2, PREF, PREF2
      DATA F /.FALSE./
      DATA LUNP /30/
      DATA AXUNIT /'Channels'/
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
      NPNL = NCTOT / CHNUM
      SHORT = NPNL*MAX(1,NXPANE).GT.12
C                                       Decide on x-axis labelling
C                                       scheme
      FRLAB = ABS (APARM(7)-1.0).LT.0.49
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  MULTI-IF ERROR
      VELLAB = ABS (APARM(7)-2.0).LT.0.49
      CORRLB = XCF .OR. ACF
      IF (CORRLB) FRLAB = .FALSE.
C                                       Determine frequency parms
      VELFPX = 0.
      VELINC = 0.
      REFVEL = 0.0D0
      ALTRFP = 1.
      OPTICL = .FALSE.
      XMULT = 1.0
      IF (VELLAB) CALL LABVEL (STRTIF, VELLAB, OPTICL, ALTRFP, REFVEL,
     *   IRET)
C                                       Finish label parameters
      IF ((FRLAB) .AND. (.NOT.DIDCHN)) THEN
         CVER = 1
         FOFF(1) = 0.0
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, CVER, CATUV,
     *      LUNP, NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
            GO TO 990
            END IF
         DIDCHN = .TRUE.
C                                       Determine velocity parms
      ELSE IF (VELLAB) THEN
         CPREF(1,LOCNUM) = 'Kilo'
         END IF
C                                       Deal with corr fn labelling
      IF (CORRLB) THEN
         NUMPIX = CATUV(KINAX+KLOCFY) * 2
         ZERPIX = (NUMPIX/2.) + 1.
         DELINC = 1.0
         IF (FINC(1).EQ.0.0) APARM(7) = 0.0
         IF (ABS(APARM(7)-1.).LT.0.49) DELINC = NUMPIX / FINC(1)
         SDEL = (CBCHAN - ZERPIX - 1.0) * DELINC
         OLDDEL = SDEL
         CALL METSCL (LABEL, SDEL, CPREF(1,LOCNUM), PFLG)
         DELINC = DELINC * (SDEL/OLDDEL)
         END IF
C                                       Determine lower plot parms
      IF (NEWPAG) THEN
         IF (CODTYP.EQ.5) THEN
            PREF = ' '
            YMULT = 1.0
         ELSE
            IF (CODTYP.EQ.8) THEN
               ATEMP = MAXPHS - MINPHS
               ATEMP = MAX (ATEMP, ABS(MAXPHS))
               ATEMP = MAX (ATEMP, ABS(MINPHS))
            ELSE
               ATEMP = MAXAMP - MINAMP
               ATEMP = MAX (ATEMP, ABS(MAXAMP))
               ATEMP = MAX (ATEMP, ABS(MINAMP))
               END IF
            YMULT = ATEMP
            CALL METSCL (LABEL, ATEMP, PREF, PFLG)
            YMULT = ATEMP / YMULT
            END IF
         END IF
      CPREF(2,LOCNUM) = PREF
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
C                                       First value
      XMULT = REAL ((CHNUM + 1)) / REAL ((XTRC(1) - XBLC(1))/NPNL)
C                                       Increment, value, label
      IF (FRLAB) THEN
         CALL LABFRQ (STRTIF, FOFF, FINC, XMULT, PBCH, PECH, REVERS)
      ELSE IF (VELLAB) THEN
         CALL LABVEL (STRTIF, VELLAB, OPTICL, ALTRFP, REFVEL, IRET)
      ELSE IF (CORRLB) THEN
         RPVAL(1,LOCNUM) = SDEL
         AXINC(1,LOCNUM) = XMULT * DELINC
         CTYP(1,LOCNUM) = AXUNIT
         IF (ABS(APARM(7)-1.0).LT.0.49) CTYP(1,LOCNUM) = 'Seconds'
      ELSE
         CTYP(1,LOCNUM) = AXUNIT
         RPVAL(1,LOCNUM) = PBCH - 1
         AXINC(1,LOCNUM) = XMULT
         IF (REVERS) THEN
            RPVAL(1,LOCNUM) = CHNUM + PBCH
            AXINC(1,LOCNUM) = -XMULT
            END IF
         END IF
C                                       Y axis
      IF ((CODTYP.EQ.5) .OR. (CODTYP.EQ.8)) THEN
         RPVAL(2,LOCNUM) = MINPHS * YMULT
         AXINC(2,LOCNUM) = YMULT * PHSRNG / (XLINT - XBLC(2))
      ELSE
         RPVAL(2,LOCNUM) = MINAMP * YMULT
         AXINC(2,LOCNUM) = YMULT * AMPRNG / (XLINT - XBLC(2))
         END IF
      IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.3)) THEN
         CTYP(2,LOCNUM) = 'Ampl ' // JY
         IF (BPPLOT) THEN
            CTYP(2,LOCNUM) = 'BP ampl'
         ELSE IF (BDPLOT) THEN
            CTYP(2,LOCNUM) = 'BD ampl'
         ELSE IF (PDPLOT) THEN
            CTYP(2,LOCNUM) = 'PD ampl'
         ELSE IF (CPPLTI) THEN
            CTYP(2,LOCNUM) = 'CP amp/i'
         ELSE IF (CPPLOT) THEN
            CTYP(2,LOCNUM) = 'CP ampl'
         ELSE IF (PCPLOT) THEN
            CTYP(2,LOCNUM) = 'PC ampl'
         ELSE IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'ampl/ch0'
            END IF
      ELSE IF ((CODTYP.EQ.2) .OR. (CODTYP.EQ.4)) THEN
         IBP5 = BPARM(5) + 0.01
         WRITE (CTEMP,1010) IBP5
         IF (IBP5.EQ.1) THEN
            IIP = 1
         ELSE IF (IBP5.LT.10) THEN
            IIP = 3
            CTYP(2,LOCNUM) = CTEMP(2:)
         ELSE
            IIP = 4
            CTYP(2,LOCNUM) = CTEMP
            END IF
         CTYP(2,LOCNUM)(IIP:) = 'Log10(Amp) ' // JY
         IF (BPPLOT) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(BP amp)'
         ELSE IF (BDPLOT) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(BD amp)'
         ELSE IF (PDPLOT) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(PD amp)'
         ELSE IF (CPPLTI) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(CP a/i)'
         ELSE IF (CPPLOT) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(CP amp)'
         ELSE IF (PCPLOT) THEN
            CTYP(2,LOCNUM)(IIP:) = 'Log10(PC amp)'
         ELSE IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM)(IIP:) = 'log10(ampl/ch0)'
            END IF
      ELSE IF (CODTYP.EQ.5) THEN
         CTYP(2,LOCNUM) = 'Phas deg'
         IF (BPPLOT) THEN
            CTYP(2,LOCNUM) = 'BP phase'
         ELSE IF (BDPLOT) THEN
            CTYP(2,LOCNUM) = 'BD phase'
         ELSE IF (PDPLOT) THEN
            CTYP(2,LOCNUM) = 'PD angle'
         ELSE IF (CPPLOT) THEN
            CTYP(2,LOCNUM) = 'CP angle'
         ELSE IF (PCPLOT) THEN
            CTYP(2,LOCNUM) = 'PC phase'
         ELSE IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'Phas-ch0'
            END IF
      ELSE IF ((CODTYP.EQ.6) .OR. (CODTYP.EQ.7)) THEN
         CTYP(2,LOCNUM) = 'Real ' // JY
         IF (BPPLOT) THEN
            CTYP(2,LOCNUM) = 'BP real'
         ELSE IF (BDPLOT) THEN
            CTYP(2,LOCNUM) = 'BD real'
         ELSE IF (PDPLOT) THEN
            CTYP(2,LOCNUM) = 'PD real'
         ELSE IF (CPPLTI) THEN
            CTYP(2,LOCNUM) = 'CP Q/I'
         ELSE IF (CPPLOT) THEN
            CTYP(2,LOCNUM) = 'CP Qpol'
         ELSE IF (PCPLOT) THEN
            CTYP(2,LOCNUM) = 'PC real'
         ELSE IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'real/ch0'
            END IF
      ELSE
         CTYP(2,LOCNUM) = 'Imag ' // JY
         IF (BPPLOT) THEN
            CTYP(2,LOCNUM) = 'BP imag'
         ELSE IF (BDPLOT) THEN
            CTYP(2,LOCNUM) = 'BD imag'
         ELSE IF (PDPLOT) THEN
            CTYP(2,LOCNUM) = 'PD imag'
         ELSE IF (CPPLTI) THEN
            CTYP(2,LOCNUM) = 'CP U/I'
         ELSE IF (CPPLOT) THEN
            CTYP(2,LOCNUM) = 'CP Upol'
         ELSE IF (PCPLOT) THEN
            CTYP(2,LOCNUM) = 'PC imag'
         ELSE IF (BPARM(1).GT.0.0) THEN
            CTYP(2,LOCNUM) = 'Imag/ch0'
            END IF
         END IF
C                                       Label it
      IF ((NCOUNT.EQ.0) .AND. (NPNL.EQ.1)) THEN
         I = KLOCF(LOCNUM)
         IIF = STRTIF
         IF (FRLAB) CALL LABFRQ (IIF, FOFF, FINC, XMULT, PBCH, PECH,
     *      REVERS)
         IF (VELLAB) CALL LABVEL (IIF, VELLAB, OPTICL, ALTRFP, REFVEL,
     *      IRET)
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATI, F, PLTBLK, IRET)
         KLOCF(LOCNUM) = I
         IF (IRET.NE.0) GO TO 999
         IF (JTRIM(POLLAB(STRPOL)).GT.0) THEN
            WRITE (TEXT,1020) IIF, POLLAB(STRPOL)
            CALL REFRMT (TEXT, '_', INCHAR)
            INCHAR = INCHAR+1
            TEXT(INCHAR:) = ')'
         ELSE
            WRITE (TEXT,1021) IIF
            CALL REFRMT (TEXT, '_', INCHAR)
            END IF
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
         LSTPOL = ' '
         DO 20 IPNL = 1,NPNL
C                                       IF/Stokes label
            LBLC(1) = XBLC(1) + (IPNL-1) * ((XTRC(1)-XBLC(1)) / NPNL)
            LTRC(1) = XBLC(1) + IPNL * ((XTRC(1)-XBLC(1)) / NPNL)
            IIP = STRPOL + (IPNL-1) / (STOPIF-STRTIF+1)
            IIF = STRTIF + IPNL - 1 - (IIP-STRPOL)*(STOPIF-STRTIF+1)
            IF (SHORT) THEN
               IF ((JTRIM(POLLAB(IIP)).GT.0) .AND.
     *            (POLLAB(IIP).NE.LSTPOL)) THEN
                  TEXT = '(' // POLLAB(IIP)
                  CALL REFRMT (TEXT, '_', INCHAR)
                  INCHAR = INCHAR + 1
                  TEXT(INCHAR:) = ')'
                  CALL GPOS (LBLC(1), LBLC(2), PLTBLK, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GICHAR (1, INCHAR, 0, 0.15, 2.53, TEXT, PLTBLK,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  LSTPOL = POLLAB(IIP)
                  END IF
               WRITE (TEXT,1022) IIF
               CALL CHTRIM (TEXT, 3, TEXT, INCHAR)
               SHDX = 0.15
               IF (NPNL*MAX(1,NXPANE).LT.48) SHDX = 0.75
            ELSE
               SHDX = 1.3
               IF (JTRIM(POLLAB(IIP)).GT.0) THEN
                  WRITE (TEXT,1020) IIF, POLLAB(IIP)
                  CALL REFRMT (TEXT, '_', INCHAR)
                  INCHAR = INCHAR+1
                  TEXT(INCHAR:) = ')'
               ELSE
                  WRITE (TEXT,1021) IIF
                  CALL REFRMT (TEXT, '_', INCHAR)
                  END IF
               END IF
C                                       axis label
            IF (FRLAB) CALL LABFRQ (IIF, FOFF, FINC, XMULT, PBCH,
     *         PECH, REVERS)
            IF (VELLAB) CALL LABVEL (IIF, VELLAB, OPTICL, ALTRFP,
     *         REFVEL, IRET)
            WRHLAB = ((FRLAB) .OR. (VELLAB))
            WRHLAB = (NYP.EQ.1) .OR. (LAST)
C              .OR. (WRHLAB)
            IF ((NPNL.GT.1) .AND. (IPNL.NE.(NPNL+1)/2) .AND.
     *         (.NOT.DOCHIF)) THEN
               CPREF(1,LOCNUM) = ' '
               CTYP(1,LOCNUM) = ' '
               WRHLAB = .FALSE.
            ELSE IF ((.NOT.WRHLAB) .OR. ((NYP.NE.1) .AND. (.NOT.LAST)))
     *         THEN
               CPREF(1,LOCNUM) = ' '
               CTYP(1,LOCNUM) = ' '
            ELSE IF (IPNL.NE.(NPNL+1)/2) THEN
               CPREF(1,LOCNUM) = ' '
               CTYP(1,LOCNUM) = ' '
            ELSE
               CPREF(1,LOCNUM) = XPREF
               CTYP(1,LOCNUM) = XCTYP
               END IF
            CALL GPOS (LBLC(1), LBLC(2), PLTBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GICHAR (1, INCHAR, 0, SHDX, 1.2, TEXT, PLTBLK, IRET)
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
      IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.2).OR. (CODTYP.EQ.6)) THEN
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
         IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.2)) THEN
            CTYP(2,LOCNUM) = 'Phas deg'
            IF (BPPLOT) THEN
               CTYP(2,LOCNUM) = 'BP phase'
            ELSE IF (BDPLOT) THEN
               CTYP(2,LOCNUM) = 'BD phase'
            ELSE IF (PDPLOT) THEN
               CTYP(2,LOCNUM) = 'PD angle'
            ELSE IF (CPPLOT) THEN
               CTYP(2,LOCNUM) = 'CP angle'
            ELSE IF (PCPLOT) THEN
               CTYP(2,LOCNUM) = 'PC phase'
            ELSE IF (BPARM(1).GT.0.0) THEN
               CTYP(2,LOCNUM) = 'Phas-ch0'
               END IF
            PREF2 = ' '
            YMULT2 = 1.0
         ELSE
            CTYP(2,LOCNUM) = 'Imag ' // JY
            IF (BPPLOT) THEN
               CTYP(2,LOCNUM) = 'BP imag'
            ELSE IF (BDPLOT) THEN
               CTYP(2,LOCNUM) = 'BD imag'
            ELSE IF (PDPLOT) THEN
               CTYP(2,LOCNUM) = 'PD imag'
            ELSE IF (CPPLTI) THEN
               CTYP(2,LOCNUM) = 'CP U/I'
            ELSE IF (CPPLOT) THEN
               CTYP(2,LOCNUM) = 'CP Upol'
            ELSE IF (PCPLOT) THEN
               CTYP(2,LOCNUM) = 'PC imag'
            ELSE IF (BPARM(1).GT.0.0) THEN
               CTYP(2,LOCNUM) = 'Imag/ch0'
               END IF
            IF (NEWPAG) THEN
               ATEMP = MAX (MAXPHS - MINPHS, ABS(MAXPHS))
               ATEMP = MAX (ATEMP, ABS(MINPHS))
               YMULT2 = ATEMP
               CALL METSCL (LABEL, ATEMP, PREF2, PFLG)
               YMULT2 = ATEMP / YMULT2
               END IF
            END IF
         CPREF(2,LOCNUM) = PREF2
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
               IF (FRLAB) CALL LABFRQ (IIF, FOFF, FINC, XMULT, PBCH,
     *            PECH, REVERS)
               IF (VELLAB) CALL LABVEL (IIF, VELLAB, OPTICL, ALTRFP,
     *            REFVEL, IRET)
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
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LABLAX : ERROR ',I3,' RETURNED BY CHNDAT')
 1010 FORMAT (I2,'*')
 1020 FORMAT ('IF',I3,'(',A)
 1021 FORMAT ('IF',I3)
 1022 FORMAT (I3)
      END
      SUBROUTINE LABFRQ (IFNO, FOFF, FINC, XMULT, PBCH, PECH, REVERS)
C-----------------------------------------------------------------------
C   LABFRQ sets up the labeling parameters for the particular IF
C   Inputs:
C      IFNO     I      IF number
C      FOFF     D(*)   Freq offset by IF
C      FINC     R(*)   Freq incr by IF
C      XMULT    R      Increment multiplier
C      PBCH     I      Begin channel
C      PECH     I      End channel
C   Outputs: common CPREF, RPVAL, AXINC
C-----------------------------------------------------------------------
      INTEGER   IFNO, PBCH, PECH
      DOUBLE PRECISION FOFF(*)
      REAL      FINC(*), XMULT
      LOGICAL   REVERS
C
      INTEGER   PFLG
      REAL      CATR(256), REFPIX, FREINC
      DOUBLE PRECISION CATD(128), SFREQ, AFREQ, BFREQ
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (CATUV, CATR, CATD)
C-----------------------------------------------------------------------
      REFPIX = CATR(KRCRP+KLOCFY)
      SFREQ = CATD(KDCRV+KLOCFY) + FOFF(IFNO)
      FREINC = FINC(IFNO)
      AFREQ = SFREQ
      BFREQ = AFREQ
      CALL METSCD (AFREQ, CPREF(1,LOCNUM), PFLG)
      FREINC = FREINC * (AFREQ/BFREQ)
      SFREQ = SFREQ * (AFREQ/BFREQ)
C                                       Ensure the labelling
C                                       is sensible
      IF (CPREF(1,LOCNUM).EQ.'Giga') THEN
         CPREF(1,LOCNUM) = 'Mega'
         SFREQ = SFREQ * 1000.0
         FREINC = FREINC * 1000.0
         END IF
      IF ((CPREF(1,LOCNUM).EQ.'Mega') .OR. (CPREF(1,LOCNUM).EQ.'Kilo'))
     *   THEN
         CTYP(1,LOCNUM) = '  ' // CPREF(1,LOCNUM)(:1) // 'Hz'
      ELSE
         CTYP(1,LOCNUM) = ' ' // CPREF(1,LOCNUM)(:5) // 'Hz'
         END IF
      CPREF(1,LOCNUM) = 'FREQ'
C                                       location common
      RPVAL(1,LOCNUM) = SFREQ + (FREINC * (PBCH - 1 - REFPIX))
      AXINC(1,LOCNUM) = XMULT * FREINC
      IF (REVERS) THEN
         RPVAL(1,LOCNUM) = SFREQ + (FREINC * (PECH + 1 - REFPIX))
         AXINC(1,LOCNUM) = -AXINC(1,LOCNUM)
         END IF
C
 999  RETURN
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
C baselines (or antennas in total power mode) that it is possible to
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
C   The vertical scales are rounded to the nearest integer for the sake
C   of space on the page - if the user wants the correct label they
C   should set NCOUNT to 0 and plot the specific frame they want on a
C   single page.
C-----------------------------------------------------------------------
      INTEGER  NPARM, IERR
C
      INCLUDE 'POSSM.INC'
      CHARACTER SPRTXT*16, PHNAME*48
      INTEGER   IANT, I, IIF, JP, J, IANGL, NCHAR, IROUND, JERR, IO,
     *   BUFFI(256), IPNL, IPOL, LPEIF, LPOLNM, MCOUNT, KANT, LANT,
     *   LIMIT, JTRIM, II
      REAL      XBLC(2), XTRC(2), SCALX, SCALA, SCALP, OFX, OFY, X, Y,
     *   DX, DY, PDATA(MAXCIF,2), DCX, DCY, PLTXOF, PLTYOF, ALTRFP,
     *   TSOLIN, XRN, XFAC, TEPS, PDMIN(2), PDMAX(2)
      LOGICAL   DOZERO, LEAVE,  MULTIF, OPTICL, BLNK, NEWEND, LAST
      DOUBLE PRECISION REFVEL
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'POSS4.INC'
      INCLUDE 'POSSNX.INC'
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
      ALABEL(1) = ' '
      ALABEL(2) = ' '
      APREF(1) = ' '
      APREF(2) = ' '
      MULTIF = PEIF.NE.PBIF
      NEWPAG = .FALSE.
      CLOSED = .TRUE.
      XFAC = ABS (FACTOR)
      IF (XFAC.GT.100.0) XFAC = XFAC - 100.0
      IF (XFAC.LT.0.5) XFAC = 1.0
C                                       Antenna list to plot
 10   IF (AUTO .OR. BPPLOT .OR. PDPLOT .OR. PCPLOT) THEN
         DO 20 I = 1,50
            ANTENS(I) = XA1(I)
 20         CONTINUE
         END IF
      CALL SOUFIL (IERR)
      LPEIF = PEIF
      IF (POLNUM.LE.0) THEN
         POLNUM = CATBLK(KINAX+JLOCS)
         IF (TAPLOT) POLNUM = MIN (2, POLNUM)
         END IF
      LPOLNM = POLNUM
      IF (DOCHIF) LPEIF = PBIF
      IF (DOCHPL) LPOLNM = 1
      IF (CPPLOT) LPOLNM = 1
      MCOUNT = NBASE * LPOLNM * (LPEIF - PBIF + 1)
      NCOUNT = MIN (MCOUNT, NCOUNT)
C
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
      IF (CODTYP.EQ.2) LINT = 700.
      IF (CODTYP.EQ.6) LINT = 500.
      IF (AUTO1) LINT = 1000.
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
      JCOUNT = 0
      CALL RFILL (50, 0.0, XANTEN)
      LIMIT = NBASE
      IF (CPPLOT) NBASE = NSOUWD
      DO 300 I = 1,NBASE
         ICURNT = I
         NEWPAG = (MOD(ICOUNT,NCOUNT).EQ.0)
         IF (I.EQ.1) NEWPAG = .TRUE.
         CALL COPY (256, CATBLK, CATSAV)
C                                       Generate plot buffer
C                                       For bandpass plotting
         IF (BPPLOT) THEN
            IANT = ANTENS(I)
            CALL POSSBP (IANT, IERR)
            IF ((IERR.EQ.10) .OR. (IERR.LT.0)) IERR = 0
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = IANT
         ELSE IF (BDPLOT) THEN
            KANT = XA1(ICURNT)
            LANT = XA2(ICURNT)
            CALL POSSBD (KANT, LANT, IERR)
            IF ((IERR.EQ.10) .OR. (IERR.LT.0)) IERR = 0
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = XA1(ICURNT)
            XANTEN(2) = XA2(ICURNT)
         ELSE IF (PDPLOT) THEN
            IANT = ANTENS(I)
            CALL POSSPD (IANT, IERR)
            IF ((IERR.EQ.10) .OR. (IERR.LT.0)) IERR = 0
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = IANT
         ELSE IF (CPPLOT) THEN
            IANT = SOUWAN(I)
            CALL POSSCP (IANT, IERR)
            IF ((IERR.EQ.10) .OR. (IERR.LT.0)) IERR = 0
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = IANT
         ELSE IF (PCPLOT) THEN
            IANT = ANTENS(I)
            CALL POSSPC (IANT, IERR)
            IF ((IERR.EQ.10) .OR. (IERR.LT.0)) IERR = 0
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = IANT
C                                       For cross-power spectra
         ELSE
            NOMIT = 0
            CALL POSSUV (IERR)
            IF ((IERR.EQ.5) .AND. (SCANAV)) THEN
               IF (NEXTSC.LT.NUMNX) IERR = 0
               END IF
            IF ((IERR.EQ.5) .OR. (IERR.EQ.10)) THEN
               IERR = 0
               IF ((XSOLIN.GT.0.0) .AND. (STOPU.GE.STOPD))
     *            GO TO 350
               END IF
            IF (IERR.NE.0) GO TO 350
            XANTEN(1) = XA1(ICURNT)
            IF (.NOT.AUTO) XANTEN(2) = XA2(ICURNT)
            END IF
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
         CHNUM = NUMFRQ
         IF (XCF .OR. ACF) CHNUM = NUMXCF
         LPEIF = PEIF
         LPOLNM = POLNUM
         IF (DOCHIF) LPEIF = PBIF
         IF (DOCHPL) LPOLNM = 1
         IF (CPPLOT) LPOLNM = 1
         MCOUNT = NBASE * LPOLNM * (LPEIF - PBIF + 1)
         NCOUNT = MIN (MCOUNT, NCOUNT)
C                                       load up the plotting
C                                       arrays
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
            JCOUNT = JCOUNT + 1
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
            PDMIN(1) = MINAMP
            PDMAX(1) = MAXAMP
            PDMIN(2) = MINPHS
            PDMAX(2) = MAXPHS
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
C                                       last plots
            LAST = (I.EQ.NBASE) .AND. (JCOUNT.GT.MCOUNT-NXPANE)
            LAST = (JCOUNT.GT.MCOUNT-NXPANE)
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
            CALL LABLAX (OPTICL, REFVEL, ALTRFP, LAST, IERR)
            IF (IERR.NE.0) GO TO 350
C                                       Global labels
            IF (ICOUNT.EQ.1) THEN
               CALL POSSLB (STRTIF, IERR)
               IF (IERR.NE.0) GO TO 350
               END IF
C                                       Label each frame with
C                                       antenna name
            IANGL = 0
            DCX = 2.
            DCY = -1.83
            II = JTRIM (ANTNAM(2))
            II = JTRIM (ANTNAM(1))
            IF (ANTNAM(1).NE.' ') THEN
               IF (AUTO .OR. BPPLOT .OR. PDPLOT .OR. PCPLOT) THEN
                  WRITE (SPRTXT,1020) ANTNAM(1)
               ELSE
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
                  END IF
               CALL REFRMT (SPRTXT, '_', NCHAR)
               CALL GPOS (XBLC(1), XLINT, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLTBLK,
     *            IERR)
               IF (IERR.NE.0) GO TO 350
               END IF
            MSGTXT = 'Doing: ' // SPRTXT
C                                       antenna number
            IANGL = 0
            DCY = -1.83
            IF (CPPLOT) THEN
               SPRTXT = SRCOBS
            ELSE
               IF (AUTO.OR.BPPLOT.OR.PDPLOT.OR.PCPLOT) THEN
                  WRITE (SPRTXT,1025) TELNUM(1)
               ELSE
                  WRITE (SPRTXT,1026) TELNUM(1), TELNUM(2)
                  END IF
               END IF
            CALL REFRMT (SPRTXT, '_', NCHAR)
            DCX = -(NCHAR+1) - 1.0
            CALL GPOS (XTRC(1), XLINT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 350
            CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLTBLK,
     *         IERR)
            IF (IERR.NE.0) GO TO 350
C                                       IF labelling
            WRITE (SPRTXT,1030) IIF
            CALL REFRMT (SPRTXT, '_', NCHAR)
            MSGTXT(20:) = SPRTXT
            CALL MSGWRT (3)
C                                       Scaling
            XRN = (XTRC(1) - XBLC(1)) / NPNL
            SCALX = XRN / (CHNUM + 1)
            IF ((CODTYP.EQ.5) .OR. (CODTYP.EQ.8)) THEN
               SCALA = (XLINT - XBLC(2)) / PHSRNG
               OFY = XBLC(2) - MINPHS * SCALA
               JP = 2
               DOZERO = (MINPHS.LT.0.0) .AND. (MAXPHS.GT.0.0)
            ELSE
               SCALA = (XLINT - XBLC(2)) / AMPRNG
               IF (.NOT.AUTO1) SCALP = (XTRC(2) - XLINT) / PHSRNG
               OFY = XBLC(2) - MINAMP * SCALA
               JP = 1
               DOZERO = (MINAMP.LT.0.0) .AND. (MAXAMP.GT.0.0) .AND.
     *            (CODTYP.GT.4)
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
               IO = (IPNL - 1) * CHNUM
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
               DX = 5.0 / XYRATI * XFAC
               DY = 5.0 * XFAC
               CALL GLTYPE (4, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 350
               IF ((CODTYP.EQ.5) .OR. (FACTOR.GE.0.0)) THEN
                  DO 40 J = 1,CHNUM
                     IF ((PDATA(J+IO,JP).NE.FBLANK) .AND.
     *                  (PDATA(J+IO,JP).GE.PDMIN(JP)) .AND.
     *                  (PDATA(J+IO,JP).LE.PDMAX(JP))) THEN
                        X = J * SCALX + OFX
                        Y = PDATA(J+IO,JP) * SCALA + OFY
                        CALL GMARK (X, Y, DX, DY, IERR)
                        IF (IERR.NE.0) GO TO 350
                        END IF
 40                  CONTINUE
                  END IF
               IF ((CODTYP.NE.5) .AND. (FACTOR.LT.100.0)) THEN
                  BLNK = .TRUE.
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 50 J = 1,CHNUM
                     IF ((PDATA(J+IO,JP).EQ.FBLANK) .OR.
     *                  (PDATA(J+IO,JP).LT.PDMIN(JP)) .OR.
     *                  (PDATA(J+IO,JP).GT.PDMAX(JP))) THEN
                        BLNK = .TRUE.
                     ELSE
                        X = J * SCALX + OFX
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
     *         ((CODTYP.LE.2) .OR. (CODTYP.EQ.6))
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
               IO = (IPNL - 1) * CHNUM
               OFX = XBLC(1) + (IPNL - 1) * XRN
               IF ((CODTYP.EQ.1) .OR. (CODTYP.EQ.2) .OR.
     *            ((CODTYP.EQ.6) .AND. (FACTOR.GE.0.0))) THEN
                  CALL GLTYPE (4, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 60 J = 1,CHNUM
                     IF ((PDATA(J+IO,2).NE.FBLANK) .AND.
     *                  (PDATA(J+IO,2).GE.PDMIN(2)) .AND.
     *                  (PDATA(J+IO,2).LE.PDMAX(2))) THEN
                        X = J * SCALX + OFX
                        Y = PDATA(J+IO,2) * SCALP + OFY
                        CALL GMARK (X, Y, DX, DY, IERR)
                        IF (IERR.NE.0) GO TO 350
                        END IF
 60                  CONTINUE
                  END IF
               IF ((CODTYP.EQ.6) .AND. (FACTOR.LT.100.0)) THEN
                  BLNK = .TRUE.
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 350
                  DO 70 J = 1,CHNUM
                     IF ((PDATA(J+IO,2).EQ.FBLANK) .OR.
     *                  (PDATA(J+IO,2).LT.PDMIN(2)) .OR.
     *                  (PDATA(J+IO,2).GT.PDMAX(2))) THEN
                        BLNK = .TRUE.
                     ELSE
                        X = J * SCALX + OFX
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
            IERR = 0
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
         ELSE IF (TAPLOT) THEN
            STARTU = STOPU + TEPS
            STOPU = STARTU + TSOLIN
         ELSE
            STARTU = STOPU + TEPS
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
         IF ((XSOLIN.NE.0.0) .AND. (IERR.EQ.0)) GPHPAG = (DOTV) .AND.
     *      (STOPU.LT.STOPD)
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
 1020 FORMAT (A8)
 1025 FORMAT (I3)
 1026 FORMAT (I3,' -',I3)
 1030 FORMAT ('IF:',I3)
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
         DCX = -CH(1) + 1.0
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
 980  WRITE (MSGTXT,1980) IERR, SUBR
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('MLAB1: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
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
C                                       Determine possible tic intervls
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
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.EQ.0.0) GO TO 235
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 235
            END IF
C                                       Simple tick
         IF ((XT.GE.BLC(1)-0.01) .AND. (XT.LE.TRC(1)+0.01) .AND.
     *      (YT.GE.BLC(2)-0.01) .AND. (YT.LE.TRC(2)+0.01)) THEN
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
            END IF
C                                       Convert degrees to DEC/RA.
 235     IF (.NOT.NONUM) THEN
C                                       labels offset
            IF ((LAXIS.EQ.3) .OR. (LAXIS.EQ.4)) THEN
               IF (LAXIS.EQ.3) CALL FNDY (BLC(2), DX, DY, JERR)
               IF (LAXIS.EQ.4) CALL FNDX (BLC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 285
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (JERR.NE.0) GO TO 285
               END IF
C                                       Position for labels
            CALL GPOS (X, Y, IBUFF, IERR)
            SUBR = 'GPOS'
            IF (IERR.NE.0) GO TO 980
            CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *         SPRTXT, ILEN)
            DSP = DCX - ILEN
            IF (MOD(INOI-I,LWRN).EQ.0) THEN
               CALL GCHAR (ILEN, IANGL, DSP, DCY, SPRTXT, IBUFF, IERR)
               SUBR = 'GCHAR'
               IF (IERR.NE.0) GO TO 980
               END IF
            END IF
 285     DEGC = DEGC - DEG
 290     CONTINUE
C                                       No top/right in subplots
      IF ((LAXIS.EQ.3) .OR. (LAXIS.EQ.4)) GO TO 999
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
      IF (IAXIS.NE.1) THEN
         CALL TICINC (6, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 395
C                                       horizontal
      ELSE
         CALL TICINC (5, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) GO TO 395
         END IF
C                                       Loop for other border.
      DO 390 I= 1,INOI
         IF (IAXIS.NE.1) THEN
            DY = DEGC
            CALL FNDX (TRC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 380
         ELSE
            DX = DEGC
            CALL FNDY (TRC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 380
            END IF
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 380
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 380
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 380
C                                       Find end of tic.
         DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 380
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.EQ.0.0) GO TO 380
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 380
            END IF
C                                       Simple ticks only
         IF ((XT.GE.BLC(1)-0.01) .AND. (XT.LE.TRC(1)+0.01) .AND.
     *      (YT.GE.BLC(2)-0.01) .AND. (YT.LE.TRC(2)+0.01)) THEN
C                                       Position at end of tic.
            CALL GPOS (XT, YT, IBUFF, IERR)
            SUBR = 'GPOS'
            IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
            CALL GVEC (X, Y, IBUFF, IERR)
            SUBR = 'GVEC'
            IF (IERR.NE.0) GO TO 980
            END IF
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
      INTEGER   NPARMS, IERR
      REAL      PTEMP(4), ALTRFP
      LOGICAL   OPTICL, LAST
      DOUBLE PRECISION REFVEL
C
      INTEGER  DEPTH(5), LTYPE, INP, IRET
      INCLUDE 'POSSM.INC'
      INCLUDE 'POSS2.INC'
      INCLUDE 'POSS3.INC'
      INCLUDE 'POSS4.INC'
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
      IF (XYRTIO.GT.0.0) XYRATI = XYRTIO
      XYRTIO = XYRATI
      CALL RCOPY (4, PTEMP(1), APARM(3))
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL RFILL (4, 0.5, CHOUT)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) THEN
         NCOUNT = -1 - ABS(NCOUNT)
         CALL LABLAX (OPTICL, REFVEL, ALTRFP, LAST, IRET)
         NCOUNT = -NCOUNT - 1
         CALL CHNTIC (BLC, TRC, INP)
         IF (CODTYP.EQ.1) INP = MAX (INP, 4)
         IF (CODTYP.EQ.2) INP = MAX (INP, 4)
         CHOUT(1) = INP + 2.666
         END IF
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(2) = CHOUT(2) + 1.333
         IF ((.NOT.AUTO1) .AND. (.NOT.TAPLOT) .AND.
     *      ((UVRNG(1).GT.1.E-8) .OR. (UVRNG(2).LT.1.E9)))
     *      CHOUT(2) = CHOUT(2) + 1.333
         IF ((TSTART.GT.0) .OR. (TEND.LT.1.0E4))
     *       CHOUT(2) = CHOUT(2) + 1.333
         IF (NCOUNT.GE.1) CHOUT(2) = CHOUT(2) + 1.333
         CHOUT(4) = 3.333
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) CHOUT(4) = CHOUT(4)+1.333
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
      SUBROUTINE ACTRNS (DATA, ISB, NIFRQ, WORK)
C-----------------------------------------------------------------------
C  Routine to FFT the total-power spectra to produce an ACF.
C  Routine is actually set up to handle two AC spectra
C  simultaneously - will modify so that the "second" is
C  zeroed on entry.
C
C  Inputs:
C      DATA(2,*)     R        The total-power spectrum to be
C                             FFT'd. Will also contain transformed
C                             result
C      ISB           I        Sideband
C      NFRQ          I        No. freq. channels in spectrum
C      WORK(*)       R        Work array (same size as data array)
C-----------------------------------------------------------------------
      REAL      DATA(2,*), WORK(*)
      INTEGER   ISB, NIFRQ
C
      INTEGER   NSHIFT, I, N, NTRANS, FFTDIR
      REAL      T1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Reflect spectrum
      NSHIFT = 2 * (NIFRQ + 1)
      DO 10 I = 2,NIFRQ
         N = NSHIFT - I
         DATA(1,N) = DATA(1,I)
C                                       Zero "second" AC spectrum
         DATA(2,I) = 0.0
         DATA(2,N) = 0.0
 10      CONTINUE
      DATA(1,NIFRQ+1) = 0.
      DATA(2,NIFRQ+1) = 0.
C                                       Do FFT
      NTRANS = 2 * NIFRQ
      FFTDIR = -ISB
      CALL FOURG (DATA, NTRANS, FFTDIR, WORK)
C                                       rearrange
      DO 20 I = 1,NIFRQ
         T1 = DATA(1,I)
         DATA(1,I) = DATA(1,NIFRQ+I)
         DATA(1,NIFRQ+I) = T1
         T1 = DATA(2,I)
         DATA(2,I) = DATA(1,NIFRQ+I)
         DATA(2,NIFRQ+I) = T1
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XCTRNS (DATA, ISB, NFRQ, WORK)
C-----------------------------------------------------------------------
C  Routine to transform a complex spectrum DATA of length NFRQ
C  to a complex correlation function.
C
C  Inputs:
C     DATA(2,*)    R         Complex spectrum
C     ISB          I         Sideband
C     NFRQ         I         No. frq channels in input spectrum
C     WORK(2,*)    R         Work array (same size as data array)
C
C  Outputs:
C     DATA(2,*)    R         Complex cross correlation function
C-----------------------------------------------------------------------
      INTEGER   ISB, NFRQ, KSTART, KSTOP, K, KK, LL, NOUT
      INTEGER   FFTDIR, KPTS
      REAL      WORK(2,*), DATA(2,*), TEMP1, TEMP2
      INCLUDE 'INCS:PUVD.INC'
C-----------------------------------------------------------------------
C                                       Set some parameters
      KPTS = NFRQ*2
      NOUT = NFRQ*2
C                                       Fill lower sideband array
C                                       slots with zeroes
      KSTART = NFRQ + 1
      KSTOP  = KPTS
      DO 10 K = KSTART,KSTOP
         DATA(1,K) = 0.
         DATA(2,K) = 0.
 10      CONTINUE
C                                       Transform to XCF
      FFTDIR = -ISB
      CALL FOURG (DATA, KPTS, FFTDIR, WORK)
C                                       Flip data around to
C                                       centre correlation function
C                                       in first half of array
      KSTOP = NFRQ
      DO 20 K = 1,KSTOP
         KK = KPTS - KSTOP + K
         LL = NOUT - KSTOP + K
         TEMP1 = DATA(1,K)
         TEMP2 = DATA(2,K)
         DATA(1,K) = DATA(1,KK) / KPTS
         DATA(2,K) = DATA(2,KK) / KPTS
         DATA(1,LL) = TEMP1 / KPTS
         DATA(2,LL) = TEMP2 / KPTS
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FILANT (DISK, CNO, CATBLK, LUN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, NSUBA, DOACOR, NBASE, ANT1, ANT2, STNS, SCRTCH,
     *   ANTENS, IRET)
C-----------------------------------------------------------------------
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned. Also fills in 2
C   arrays with all possible cominations of antenna numbers
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
      LOGICAL   DESEL, DOACOR
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
            IF (((I1.LT.J) .AND. (.NOT.DOACOR)) .OR.
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
      SUBROUTINE SOUFIL (IERR)
C-----------------------------------------------------------------------
C   Fills in arrays of source numbers to be included or excluded.
C   Local version - fills in source list always.
C   Inputs from common /SELCAL/; also checks antennas to be selected.
C      SOURCS(30)   C*16 Names of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified. (- no longer removed)
C      CALSOU(30)   C*16 Names of up to 30 calibrators,
C                        '*' or blank =>all, first character of name '-'
C                        => all except those specified. (- no longer
C                        removed)
C      SELQUA       I    Source qualifiers to be selected, -1=>any.
C                        Applied to both SOURCS and CALSOU.
C      SELCOD       C*4  Calibrator codes to select.
C                        '    '  => any,
C                        '*   ' => any non blank calibrator code.
C                        '-CAL' => blank only (no calibrators)
C                        anything else => matching CALcodes.
C                        Applied to SOURCS or CALSOU as controlled by
C                        DOAPPL
C      DOAPPL       L    If true then selection of the sources in
C                        CALSOU is conditioned on SELCOD else
C                        selection of SOURCS is conditioned on SELCOD.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C                        (no longer made all positive)
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise source file
C                        exists but cannot be read.
C                        1=TABIO problem, 2=no sources or calibrators
C   Output to common /SELCAL/:
C      NSOUWD       I    Number of sources included or excluded;
C                        will fill in entirely
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The source numbers of sources included or
C                        excluded.
C      SOUWTB(30)   I    The SoUrce table row numbers corresponding
C                        to SOUWAN.
C      NCALWD       I    Number of calibrators included or excluded.
C      DOCWNT       L    If .TRUE. then calibrators in CALWAN are
C                        included, if .FALSE. then excluded.
C      CALWAN(30)   I    The source numbers of calibrators included or
C                        excluded.
C      CALWTB(30)   I    The SoUrce table row numbers corresponding
C                        to CALWAN.
C      NANTSL       I    Number of antennas selected/excluded in ANTENS
C                        0 = All included.
C      DOAWNT       L    If .TRUE. then antennas in ANTENS included.
C                        If .FALSE. then excluded.
C   Note: also uses FGBUFF and UBUFF from /SELCAL/
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   DATP(128,2), JERR, IBUFF(512), IDSOU, SUKOLS(MAXSUC), J,
     *   K, SUNUMV(MAXSUC), QUAL, INOGRP, NSOU, NCAL, NSOURC, I, ISURNO,
     *   I1, BADCNT, YSTBSZ, SUFQID
      LOGICAL   T, F, TABLE, EXIST, FITASC, ALLSOU, ALLCAL, DOCALC,
     *   DOQUAL, ANYCC, NOCAL, TAOPEN, NMONLY
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (DATP, UBUFF)
      DATA T, F /.TRUE.,.FALSE./
      DATA YSTBSZ /XSTBSZ/
C-----------------------------------------------------------------------
C                                       Setup for CALCODE and qualifier
C                                       selection.
      IERR = 0
      DOSWNT = T
      DOCWNT = T
      NSOUWD = 0
      NCALWD = 0
      TAOPEN = .FALSE.
C                                       See if SU file exists.
      CALL ISTAB ('SU', IUDISK, IUCNO, 1, IXLUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((.NOT.EXIST) .AND. (JERR.EQ.0)) GO TO 900
      IF ((.NOT.EXIST) .OR. (.NOT.TABLE) .OR. (JERR.NE.0)) THEN
         IERR = 1
         IF (.NOT.EXIST) THEN
            WRITE (MSGTXT,1000) JERR
         ELSE IF (TABLE) THEN
            WRITE (MSGTXT,1001) JERR
         ELSE
            MSGTXT = 'SU FILE DAMAGED - NO LONGER A TABLE'
            END IF
         GO TO 980
         END IF
C                                       Open SU table
      CALL SOUINI ('READ', IBUFF, IUDISK, IUCNO, 1, CATUV, IXLUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   JERR)
      IF (JERR.NE.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000) JERR
         GO TO 980
         END IF
C                                       Get number of sources.
      NSOURC = IBUFF(5)
      TAOPEN = .TRUE.
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 900
      ALLSOU = F
      ALLCAL = F
      NSOU = 0
      NCAL = 0
C                                       adverb parameters
      DOQUAL = SELQUA.NE.-1
      ANYCC = SELCOD.EQ.'*   '
      DOCALC = (SELCOD.NE.'    ') .AND.
     *   ((SELCOD.NE.'*   ') .AND. (SELCOD.NE.'-CAL'))
      NOCAL = SELCOD.EQ.'-CAL'
      NMONLY = .NOT.DOQUAL
      IF (.NOT.DOAPPL) NMONLY = (.NOT.DOQUAL) .AND. (SELCOD.EQ.' ')
C                                       Check if source/calib excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         ALLSOU = ALLSOU .OR. (SOURCS(J)(1:1).EQ.'*')
C                                       Find number of sources
         IF (SOURCS(J).NE.' ') NSOU = J
         IF (SOURCS(J)(1:1).EQ.'-') DOSWNT = F
C                                       Calibrator
         ALLCAL = ALLCAL .OR. (CALSOU(J)(1:1).EQ.'*')
C                                       Find number of calibrators
         IF (CALSOU(J).NE.' ') NCAL = J
         IF (CALSOU(J)(1:1).EQ.'-') DOCWNT = F
 30      CONTINUE
C                                       Make sure need to look at table
      ALLSOU = ALLSOU .OR. (NSOU.LE.0)
      ALLCAL = ALLCAL .OR. (NCAL.LE.0)
      IF (NSOU.LE.1) NSOU = 1
      IF (NCAL.LE.1) NCAL = 1
      BADCNT = 0
C                                       Sources
C                                       Loop through records
      DO 100 I = 1,NSOURC
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1001) JERR
            GO TO 980
            END IF
C                                       Search lists
         DO 80 J = 1,NSOU
C                                       Check qualifier
            IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 80
C                                       Check CALCODE
            IF (.NOT.DOAPPL) THEN
               IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 80
               IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 80
               IF ((DOCALC) .AND. (CALCOD.NE.SELCOD)) GO TO 80
               END IF
C                                       Source list
            IF (.NOT.ALLSOU) THEN
               I1 = 1
               IF (SOURCS(J)(1:1).EQ.'-') I1 = 2
               IF (SOURCS(J)(I1:).NE.SOUNAM) THEN
                  IF (DOSWNT .OR. NMONLY) GO TO 80
               ELSE
                  IF (.NOT.(DOSWNT .OR. NMONLY)) GO TO 80
                  END IF
               END IF
C                                       Redundancy check
            IF (NSOUWD.GE.1) THEN
               DO 40 K = 1,NSOUWD
                  IF (SOUWAN(K).EQ.IDSOU) GO TO 80
 40               CONTINUE
               END IF
C                                       Add source
            IF (NSOUWD.LT.YSTBSZ) THEN
               NSOUWD = NSOUWD + 1
               SOUWAN(NSOUWD) = IDSOU
               IF (NSOUWD.LE.30) SOUWTN(NSOUWD) = ISURNO
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 80         CONTINUE
 100     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1100) BADCNT, YSTBSZ
         CALL MSGWRT (6)
         END IF
      IF (.NOT.NMONLY) DOSWNT = .TRUE.
C                                       No sources found
      IF (NSOUWD.LE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1101)
         GO TO 980
         END IF
C                                       Calibrator sources
      BADCNT = 0
      NMONLY = .NOT.DOQUAL
      IF (DOAPPL) NMONLY = (.NOT.DOQUAL) .AND. (SELCOD.EQ.' ')
C                                       Loop through records
      ISURNO = 1
      DO 200 I = 1,NSOURC
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1001) JERR
            GO TO 980
            END IF
C                                       Calibrator list
C                                       Search lists
         DO 180 J = 1,NCAL
C                                       Check qualifier
            IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 180
C                                       Check CALCODE
            IF (DOAPPL) THEN
               IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 180
               IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 180
               IF ((DOCALC) .AND. (CALCOD.NE.SELCOD)) GO TO 180
               END IF
            I1 = 1
            IF (CALSOU(J)(1:1).EQ.'-') I1 = 2
            IF (.NOT.ALLCAL) THEN
               IF (CALSOU(J)(I1:).NE.SOUNAM) THEN
                  IF (DOCWNT .OR. NMONLY) GO TO 180
               ELSE
                  IF (.NOT.(DOCWNT .OR. NMONLY)) GO TO 180
                  END IF
               END IF
C                                       Redundancy check
            IF (NCALWD.GE.1) THEN
               DO 160 K = 1,NCALWD
                  IF (CALWAN(K).EQ.IDSOU) GO TO 180
 160              CONTINUE
               END IF
C                                       Add calibrator
            IF (NCALWD.LT.YSTBSZ) THEN
               NCALWD = NCALWD + 1
               CALWAN(NCALWD) = IDSOU
               IF (NCALWD.LE.30) CALWTN(NCALWD) = ISURNO
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 180        CONTINUE
 200     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1200) BADCNT, YSTBSZ
         CALL MSGWRT (6)
         END IF
C                                       No calibrators found
      IF (NCALWD.LE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1201)
         GO TO 980
         END IF
      IF (.NOT.NMONLY) DOCWNT = .TRUE.
      GO TO 900
C                                       No SOURCE file
C                                       Close file
 900  IF (TAOPEN) CALL TABIO ('CLOS', 0, I, UBUFF, IBUFF, JERR)
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 930 J = 1,50
         IF (ANTENS(J).LT.0) DOAWNT = F
         IF (NANTSL.GE.1) THEN
            DO 910 K = 1,NANTSL
               IF (ABS(ANTENS(J)).EQ.ABS(ANTENS(K))) ANTENS(J) = 0
 910           CONTINUE
            END IF
C                                       Check for multiple entries
         IF (ABS(ANTENS(J)).GE.1) NANTSL = J
 930     CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
      IF (TAOPEN) CALL TABIO ('CLOS', 0, I, UBUFF, IBUFF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUFIL OR ISTAB: ERROR',I3,' OPENING SOURCE TABLE')
 1001 FORMAT ('SOUFIL/ISTAB: ERROR',I4,' READING SOURCE TABLE')
 1100 FORMAT ('SOUFIL: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
 1101 FORMAT ('SOUFIL: ALL SOURCES REJECTED BY SELECTION CRITERIA')
 1200 FORMAT ('SOUFIL: ',I5,' MORE CALIBRATORS SELECTED THAN ',I6,
     *   ' ALLOWED')
 1201 FORMAT ('SOUFIL: ALL CALIBRATORS REJECTED BY SELECTION CRITERIA')
      END
      SUBROUTINE NXSET (DISKIN, CNOIN, TTYPE, TVER, TIMRNG, SOUWAN,
     *   DOSWNT, NSOUWD, SUBARR, FRQSEL, WTS, BNDP, IRET)
C-----------------------------------------------------------------------
C   Routine to read the index table and set up the array NXVISN which
C   is used to determine the scan boundaries.  Makes index like entries
C   from BP and BD tables too.
C   Input:
C      DISKIN   I        Data disc number
C      CNOIN    I        Data catalogue number
C      TTYPE    C*2      Table type NX, BP, BD
C      TVER     I        Table version to use
C      TIMRNG   R(8)     Start/stop time between which to form
C                        scan information
C      SOUWAN   I(*)     source numbers wanted
C      DOSWNT   L        keep them (T) or reject them (F)
C      NSOUWD   I        # entries in SOUWAN array
C      SUBARR   I        required subarray
C      FRQSEL   I        required FQID
C   Output:
C      IRET     I        0 => OK, anything else = fails
C   Output in common:
C      NXVISN   I(2,*)   1,* => first vis number of scan n
C                        2,* => last vis number of scan n
C      NXTIM    R(2,*)   start and finish times of scan n
C      NXSOU    I(*)     source of scan
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, TVER, NSOUWD, SOUWAN(*), SUBARR, FRQSEL,
     *   IRET
      CHARACTER TTYPE*2
      REAL      TIMRNG(8), WTS(*), BNDP(*)
      LOGICAL   DOSWNT
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMNXT, I, IDSOUR, ISUBA, VSTART, VEND, FREQID, NUMACT,
     *   J, BPKOLS(MAXBPC), BPNUMV(MAXBPC), NUMPOL, NUMIF, NUMFRQ,
     *   BCHAN, NUMSHF, ANT, REFANT(2), ANT2
      REAL      TIME, DTIME, START, STOP, LOWSHF, DELSHF, BANDW, TB, TE
      CHARACTER LBPTYP*8
      DOUBLE PRECISION DPTIME, CHSHFT(MAXIF)
      INCLUDE 'POSSNX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                        Set timerange
      START = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *   + TIMRNG(4) / (24. * 60. * 60.)
      STOP = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *   + TIMRNG(8) / (24. * 60. * 60.)
      IF (START.EQ.0.0) START = -1.0E10
      IF (STOP.EQ.0.0) STOP = 1.0E10
      NUMACT = 0
C                                       NX table
C                                       use NX table to guide PC avg
      IF ((TTYPE.EQ.'NX') .OR. (TTYPE.EQ.'PC')) THEN
         CALL FNDEXT ('NX', CATBLK, NUMNXT)
         IF (NUMNXT.EQ.0) THEN
            INDXT = .FALSE.
            MSGTXT =
     *         'YOU ARE TRYING TO SCAN AVERAGE WITHOUT AN NX TABLE'
            CALL MSGWRT (6)
            MSGTXT = 'EITHER RUN INDXR, OR CHANGE SOLINT'
            CALL MSGWRT (6)
            IRET = 1
            GO TO 999
            END IF
C
         INDXT = .TRUE.
         J = 1
         CALL NDXINI ('READ', BUFFNX, DISKIN, CNOIN, J, CATBLK,
     *      NXLUN, IRNONX, KOLSNX, NUMVNX, IRET)
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
         IF (NUMNX.GT.MAXNX) THEN
            IRET = 1
            WRITE (MSGTXT,1010) NUMNX
            GO TO 990
            END IF
C                                       Read and load NX entries
         DO 90 I = 1,NUMNX
            IRNONX = I
            CALL TABNDX ('READ', BUFFNX, IRNONX, KOLSNX, NUMVNX, TIME,
     *         DTIME, IDSOUR, ISUBA, VSTART, VEND, FREQID, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
C                                       check subarray
            IF ((SUBARR.GT.0) .AND. (ISUBA.GT.0).AND.(ISUBA.NE.SUBARR))
     *         GO TO 90
C                                       check FQID
            IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) GO TO 90
C                                       check time range
            IF (((TIME-0.5*DTIME).GT.STOP).OR.
     *         ((TIME+0.5*DTIME).LT.START)) GO TO 90
C                                       check sources
            IF (NSOUWD.GT.0) THEN
               DO 70 J = 1,NSOUWD
                  IF (IDSOUR.EQ.SOUWAN(J)) THEN
                     IF (DOSWNT) GO TO 80
                     GO TO 90
                     END IF
 70               CONTINUE
C                                       Not in list
               IF (.NOT.DOSWNT) GO TO 80
               GO TO 90
               END IF
 80         NUMACT = NUMACT + 1
            NXVISN(1,NUMACT) = VSTART
            NXVISN(2,NUMACT) = VEND
            NXTIM(1,NUMACT) = TIME - 0.5 * DTIME
            NXTIM(2,NUMACT) = TIME + 0.5 * DTIME
            NXSOU(NUMACT) = IDSOUR
 90         CONTINUE
C                                       BP table
      ELSE IF (TTYPE.EQ.'BP') THEN
         CALL FNDEXT ('BP', CATBLK, NUMNXT)
         IF (NUMNXT.EQ.0) THEN
            INDXT = .FALSE.
            MSGTXT =
     *         'YOU ARE TRYING TO PLOT BP TABLES WHICH ARE ABSENT'
            CALL MSGWRT (6)
            IRET = 1
            GO TO 999
            END IF
C
         INDXT = .TRUE.
         CALL BPINI ('READ', BUFFNX, DISKIN, CNOIN, TVER, CATBLK, NXLUN,
     *      IRNONX, BPKOLS, BPNUMV, ANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *      NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
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
C                                       Read and load NX entries
         DO 190 I = 1,NUMNX
            IRNONX = I
            CALL TABBP ('READ', BUFFNX, IRNONX, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, DPTIME, DTIME, IDSOUR, ISUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WTS, BNDP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
C                                       check subarray
            IF ((SUBARR.GT.0) .AND. (ISUBA.GT.0).AND.(ISUBA.NE.SUBARR))
     *         GO TO 190
C                                       check FQID
            IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) GO TO 190
C                                       check time range
            IF (((DPTIME-0.5*DTIME).GT.STOP).OR.
     *         ((DPTIME+0.5*DTIME).LT.START)) GO TO 190
C                                       check sources
            IF (NSOUWD.GT.0) THEN
               DO 170 J = 1,NSOUWD
                  IF (IDSOUR.EQ.SOUWAN(J)) THEN
                     IF (DOSWNT) GO TO 180
                     GO TO 190
                     END IF
 170              CONTINUE
C                                       Not in list
               IF (.NOT.DOSWNT) GO TO 180
               GO TO 190
               END IF
 180        TB = DPTIME - 0.5*DTIME
            TE = DPTIME + 0.5*DTIME
            DO 185 J = 1,NUMACT
               IF (((TB.GE.NXTIM(1,J)) .AND. (TB.LE.NXTIM(2,J))) .OR.
     *            ((TE.GE.NXTIM(1,J)) .AND. (TE.LE.NXTIM(2,J))) .OR.
     *            ((TB.LT.NXTIM(1,J)) .AND. (TE.GT.NXTIM(2,J))))
     *            GO TO 190
 185           CONTINUE
            IF (NUMACT.EQ.MAXNX) THEN
               IRET = 1
               WRITE (MSGTXT,1010) NUMACT
               GO TO 990
               END IF
            NUMACT = NUMACT + 1
            NXTIM(1,NUMACT) = DPTIME - 0.5 * DTIME
            NXTIM(2,NUMACT) = DPTIME + 0.5 * DTIME
            NXSOU(NUMACT) = IDSOUR
 190        CONTINUE
C                                       BD table
      ELSE IF (TTYPE.EQ.'BD') THEN
         CALL FNDEXT ('BD', CATBLK, NUMNXT)
         IF (NUMNXT.EQ.0) THEN
            INDXT = .FALSE.
            MSGTXT =
     *         'YOU ARE TRYING TO PLOT BD TABLES WHICH ARE ABSENT'
            CALL MSGWRT (6)
            IRET = 1
            GO TO 999
            END IF
C
         INDXT = .TRUE.
         CALL BDINI ('READ', BUFFNX, DISKIN, CNOIN, TVER, CATBLK, NXLUN,
     *      IRNONX, BPKOLS, BPNUMV, ANT, NUMPOL, NUMIF, NUMFRQ, IRET)
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
C                                       Read and load NX entries
         DTIME = 5 / (24.0 * 60.0)
         DO 290 I = 1,NUMNX
            IRNONX = I
            CALL TABBD ('READ', BUFFNX, IRNONX, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, IDSOUR, ISUBA, ANT, ANT2, FREQID,
     *         BNDP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
C                                       check subarray
            IF ((SUBARR.GT.0) .AND. (ISUBA.GT.0).AND.(ISUBA.NE.SUBARR))
     *         GO TO 290
C                                       check FQID
            IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) GO TO 290
C                                       check time range
            IF (((TIME-0.5*DTIME).GT.STOP).OR.
     *         ((TIME+0.5*DTIME).LT.START)) GO TO 290
C                                       check sources
            IF (NSOUWD.GT.0) THEN
               DO 270 J = 1,NSOUWD
                  IF (IDSOUR.EQ.SOUWAN(J)) THEN
                     IF (DOSWNT) GO TO 280
                     GO TO 290
                     END IF
 270              CONTINUE
C                                       Not in list
               IF (.NOT.DOSWNT) GO TO 280
               GO TO 290
               END IF
 280        TB = TIME - 0.5*DTIME
            TE = TIME + 0.5*DTIME
            DO 285 J = 1,NUMACT
               IF (((TB.GE.NXTIM(1,J)) .AND. (TB.LE.NXTIM(2,J))) .OR.
     *            ((TE.GE.NXTIM(1,J)) .AND. (TE.LE.NXTIM(2,J))) .OR.
     *            ((TB.LT.NXTIM(1,J)) .AND. (TE.GT.NXTIM(2,J))))
     *            GO TO 290
 285           CONTINUE
            IF (NUMACT.EQ.MAXNX) THEN
               IRET = 1
               WRITE (MSGTXT,1010) NUMACT
               GO TO 990
               END IF
            NUMACT = NUMACT + 1
            NXTIM(1,NUMACT) = TIME - 0.5 * DTIME
            NXTIM(2,NUMACT) = TIME + 0.5 * DTIME
            NXSOU(NUMACT) = IDSOUR
 290        CONTINUE
C                                       unknown
      ELSE
         INDXT = .FALSE.
         IRET = 1
         MSGTXT = 'NXSET: UNRECOGNIZED TABLE TYPE = ''' // TTYPE // ''''
         GO TO 990
         END IF
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
      SUBROUTINE PSMOTH (NC, VD, AD)
C-----------------------------------------------------------------------
C   Applies the post-bandpass smoothing to the averaged spectrum
C   Inputs:
C      NC       I        Number channels
C   In/Out:
C      VD       R(2,*)   real, imaginary data to smooth
C      AD       R(*)     amplitude data to smooth
C   Inputs: (via common)
C      XSMOTH   R(3)      Array containing smoothing parms
C                         SMOOTH(1) = type of function
C                               (2) = width of function in channels
C                               (3) = support of function in channels
C                         Type of function supported are:
C                            0 => no smoothing
C                            5 => hanning
C                            6 => gaussian
C                            7 => boxcar
C                            8 => sin(x)/x
C-----------------------------------------------------------------------
      INTEGER   NC
      REAL      VD(2,*), AD(*)
C
      INTEGER   I, N, LSPECT, IROUND, IT, SUPRAD, LT, J, J1, J2, IFRQ, L
      REAL      FX, X, W, WIDTHS(4), SUPS(4), S
      INCLUDE 'POSSM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      REAL      TEMP(MAXCHA)
      SAVE SUPRAD
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
      IT = IROUND (XSMOTH(1))
      IF ((IT.LE.8) .OR. (IT.GT.16)) GO TO 999
C                                       we are supposed to do it:
C                                       compute the table on first call
      IF (PSMTAB(1).LT.0.0) THEN
         LT = MOD (IT-1, 4) + 1
C                                       Convolution: parms & tables
         XSMOTH(1) = IT
         LSPECT = MAX (12, NC)
         IF ((XSMOTH(2).LT.0.5) .OR. (XSMOTH(2).GT.LSPECT/3.))
     *      XSMOTH(2) = WIDTHS(LT)
         IF ((XSMOTH(3).GT.4.*SUPS(LT)*XSMOTH(2)) .OR.
     *      (XSMOTH(3).LT.XSMOTH(2)))XSMOTH(3) = SUPS(LT) * XSMOTH(2)
         SUPRAD = XSMOTH(3) / 2.0 + 0.1
         IF (SUPRAD+1.GT.256) THEN
            SUPRAD = 255
            XSMOTH(2) = (2. * SUPRAD) / SUPS(LT)
            END IF
         XSMOTH(3) = 2.0 * SUPRAD + 1.0
         CALL RFILL (256, 0.0, PSMTAB)
         N = 1 + SUPRAD
         FX = 2.0 / XSMOTH(2)
         PSMTAB(1) = 1.0
C                                       Compute look-up tables
         W = PSMTAB(1)
C                                       Hanning smooth
         IF (LT.EQ.1) THEN
            DO 20 I = 2,N
               X = I - 1.0
               PSMTAB(I) = MAX (0.0, 1.0-FX*X)
               W = W + 2 * PSMTAB(I)
 20            CONTINUE
C                                       Gaussian smooth
         ELSE IF (LT.EQ.2) THEN
            FX = -LOG(2.0) * FX * FX
            DO 30 I = 2,N
               X = I - 1.0
               PSMTAB(I) = EXP (FX * X * X)
               W = W + 2 * PSMTAB(I)
 30            CONTINUE
C                                       Boxcar smooth
         ELSE IF (LT.EQ.3) THEN
            FX = 1.0 / FX
            DO 40 I = 2,N
               X = I - 1.0
               IF (X.LT.FX) THEN
                  PSMTAB(I) = 1.0
               ELSE IF (X.EQ.FX) THEN
                  PSMTAB(I) = 0.5
                  END IF
               W = W + 2 * PSMTAB(I)
 40            CONTINUE
C                                      Sinc smooth
         ELSE IF (LT.EQ.4) THEN
            FX = 3.14159 * FX
            DO 50 I = 2,N
               X = (I - 1.0) * FX
               PSMTAB(I) = SIN(X) / X
               W = W + 2 * PSMTAB(I)
 50            CONTINUE
            END IF
C                                       Normalize integral
         IF (W.LE.0.0) W = 1.0
         DO 70 I = 1,N
            PSMTAB(I) = PSMTAB(I) / W
 70         CONTINUE
         END IF
C                                       Loop over real/imaginary
      DO 140 I = 1,2
C                                       Copy data to temp array
         DO 110 IFRQ = 1,NC
            TEMP(IFRQ) = VD(I,IFRQ)
 110        CONTINUE
C                                       Convolve the data
         DO 130 IFRQ = 1,NC
C                                       preserve the flagging
            IF ((TEMP(IFRQ).NE.FBLANK) .OR. (IT.GT.12)) THEN
               J1 = MAX (IFRQ - SUPRAD, 1)
               J2 = MIN (IFRQ + SUPRAD, NC)
               S = 0.0
               W = 0.0
               DO 120 J = J1,J2
                  IF (TEMP(J).NE.FBLANK) THEN
                     L = ABS(IFRQ-J) + 1
                     S = TEMP(J) * PSMTAB(L) + S
                     W = PSMTAB(L) + W
                     END IF
 120              CONTINUE
               IF (W.GT.0.0) THEN
                  VD(I,IFRQ) = S / W
               ELSE
                  VD(I,IFRQ) = FBLANK
                  END IF
               END IF
 130        CONTINUE
 140     CONTINUE
C                                       Loop over real/imaginary
C                                       Copy data to temp array
      DO 210 IFRQ = 1,NC
         TEMP(IFRQ) = AD(IFRQ)
 210     CONTINUE
C                                       Convolve the data
      DO 230 IFRQ = 1,NC
C                                       preserve the flagging
         IF (TEMP(IFRQ).NE.FBLANK) THEN
            J1 = MAX (IFRQ - SUPRAD, 1)
            J2 = MIN (IFRQ + SUPRAD, NC)
            S = 0.0
            W = 0.0
            DO 220 J = J1,J2
               IF (TEMP(J).NE.FBLANK) THEN
                  L = ABS(IFRQ-J) + 1
                  S = TEMP(J) * PSMTAB(L) + S
                  W = PSMTAB(L) + W
                  END IF
 220           CONTINUE
            IF (W.GT.0.0) THEN
               AD(IFRQ) = S / W
            ELSE
               AD(IFRQ) = FBLANK
               END IF
            END IF
 230     CONTINUE
C
 999  RETURN
      END

LOCAL INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, LPNAME*48, TITL1*132, TITL2*132,
     *   SCRTCH*132, LINE*132, CHSIG1*1, CHSIG2*1, RSTOKS*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XSTOK,
     *   XLPNAM(12)
      REAL      XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XUVRA(2), XSUBA, XCHAN, XBIF, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XBCNT, XNCNT, XINC, XUVFAC, DPARM(10), DOCRT,
     *   XBADD(10)
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      BUFF(UVBFSS), XAMP, XWT, NWT, UVM, FINC(MAXIF)
      INTEGER   UVINC, SEQIN, DISKIN, LUNI, INDI, JBUFSZ, IANT(50),
     *   NANT, IBAS(50), NBAS, CNOIN, NACROS, OTYPE, STOFF, NCOLS, JS,
     *   LUNP, FINDP, PAGE, IPCNT, HM(2), DD(2), IBCNT, LQUAL, NCORP,
     *   NUMCH
      LOGICAL   MULTI, DESEL, ISCROS(4), LPOPN, DOAMPH
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XUVRA, XSUBA,
     *   XCHAN, XBIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH, XBCNT, XNCNT, XINC, XUVFAC, DPARM,
     *   DOCRT, XLPNAM, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, LPNAME, TITL1, TITL2, SCRTCH,
     *   LINE, RSTOKS, CHSIG1, CHSIG2
      COMMON /BUFRS/ BUFF, JBUFSZ
      COMMON /UVPCOM/ FOFF, FINC, MULTI, UVINC, SEQIN, DISKIN, LUNI,
     *   INDI, CNOIN, NACROS, OTYPE, STOFF, NCOLS, JS, LUNP, FINDP,
     *   PAGE, IPCNT, HM, DD, IBCNT, LQUAL, XAMP, XWT, NWT, UVM, ISCROS,
     *   LPOPN, DOAMPH, NCORP, NUMCH
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS
LOCAL END
      PROGRAM UVPRT
C-----------------------------------------------------------------------
C! UVPRT prints uvdata with calibration
C# Printer appl UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2007, 2009-2016
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
C   UVPRT prints uv data with calibration.
C   Inputs:
C      INNAME         NAMEIN        File name to be imaged
C      INCLASS        CLAIN         File class to be imaged
C      INSEQ          SEQIN         File sequence number
C      INDISK         DISKIN        Disk volume on which file resides
C      SOURCES        XSOUR(4,30)   Sources selected
C      QUAL           XQUAL         Source qualifier #, -1 => all
C      CALCODE                      Calibrator code, ' ' all
C      STOKES         XSTOK         Stokes' parameter
C      SELBAND        XBAND         Bandwidth to select (kHz)
C      SELFREQ        XFREQ         Frequency to select (MHz)
C      FREQID         XFQID         Freq. ID to select.
C      TIMERANG       XTIME(8)      Timerange
C      ANTENNAS       XANT(50)      Antenna numbers
C      BASELINE       XBASE(50)     Antenna numbers to pair up
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      SUBARRAY       SUBARR        Subarray: 0 => all
C      CHANNEL        XCHAN         Channel number
C      BIF            XBIF          IF number: begin
C      DOCALIB        DOCAL         Calibrate?
C      GAINUSE        GAUSE         CL version to apply.
C      DOPOL                        If >0 correct polarization.
C      BLVER                        BL table to apply.
C      FLAGVER        FGVER         Flag table version
C      DOBAND                       Bandpass calibration?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      BPRINT         XBCNT         Begin sample # from selected
C      NPRINT         XNCNT         # samples printed
C      XINC           XINC          Increment in selected samples
C                                   between those actually printed
C      DOCRT          DOCRT         <= 0 use line printer/file, > 0
C                                   use CRT, >72 -> CRT width
C      OUTPRINT       XLPNAM        File name to hold line printer out
C      BADDISK        XBADD(10)     Disks to avoid for scratch
C-----------------------------------------------------------------------
      CHARACTER  PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UVPRT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVPRIN (PRGM, IRET)
C                                       count lines
      IF (IRET.EQ.0) CALL UVPRCH (IRET)
C                                       Do print
      IF ((IRET.EQ.0) .AND. (DPARM(1).LT.0.0)) CALL UVPRSC (IRET)
C                                       Do print
      IF (IRET.EQ.0) CALL UVPRDO (IRET)
C                                       Close down
      IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE UVPRIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   UVPIN gets input parameters for UVPRT.
C   Inputs:
C      PRGM   C*6   Program name
C    Output:
C      IRET   I      Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INCLUDE 'UVPRT.INC'
      CHARACTER UTYPE*2, STAT*4, CHSTOK(23)*4, BNDCOD(MAXIF)*8
      INTEGER   IUSER, I, IERR, IROUND, NPARM, LUNTB, LUN, IVER,
     *   ISBAND(MAXIF), NIF
      LOGICAL   T, TABLE, FITASC, F, MATCH, SNEXST
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA LUNTB /39/
      DATA CHSTOK /'I   ','Q   ','U   ','V   ','IQU ','IQUV','IV  ',
     *   'QU  ', 'RR  ','LL  ','RL  ','LR  ','HALF','FULL','RRLL',
     *   'RLLR', 'XX','YY','XY','YX','XXYY','XYYX','CROS'/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      LPOPN = .FALSE.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 294
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .TRUE.
         IF (IRET.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      IF (DPARM(10).LE.0.0) DPARM(10) = 1.0
C                                       Print Amp, Phas or Re, Im
      DOAMPH = DPARM(7).LT.0.01
C                                       Crunch input parameters.
      IUSER = NLUSER
      IF (XINC.LT.1.0) XINC = 1.0
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Get CATBLK from file.
      LUNI = 48
      UTYPE = 'UV'
      STAT = 'READ'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, UTYPE, IUSER,
     *   LUNI, INDI, CNOIN, CATBLK, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      IF (XUVFAC.LE.1.E-10) XUVFAC = 0.001
      IF (XNCNT.LT.1.0) THEN
         XNCNT = PRTMAX - 10
         IF ((DOCRT.GT.0.0).OR.(LPNAME(1:1).NE.' ')) XNCNT = 30000
      ELSE IF ((DOCRT.LE.0.0).AND.(LPNAME(1:1).EQ.' ')) THEN
         XNCNT = MIN (2000., XNCNT)
         END IF
      IBCNT = IROUND (XBCNT)
      IBCNT = MAX (1, IBCNT)
      UVINC = IROUND (XINC)
      UVINC = MAX (1, UVINC)
C                                       Multi-source file?
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF, TABLE, MULTI,
     *      FITASC, IERR)
         MULTI = MULTI .AND. (IERR.EQ.0)
         END IF
C                                       If calibrating, does SN exist
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Look for SN file
      IF ((DOCAL) .AND. (.NOT.MULTI)) THEN
         CALL ISTAB ('SN', DISKIN, CNOIN, 1, LUNTB, BUFF, TABLE, SNEXST,
     *      FITASC, IERR)
         IF ((.NOT.SNEXST) .OR. (IERR.NE.0)) THEN
            WRITE (MSGTXT,1020)
            CALL MSGWRT (8)
            DOCAL = .FALSE.
            END IF
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      IF (IBCNT.GE.NVIS) IBCNT = 1
      LQUAL = 0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      BCHAN = IROUND (XCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = BCHAN
      IF (JLOCIF.LT.0) THEN
         BIF = 1
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         END IF
      EIF = BIF
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      SELQUA = IROUND (XQUAL)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
C                                       Find specified FQ id
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1030)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                        Retain auto-correlations ?
      DOACOR = .TRUE.
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 40 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 40      CONTINUE
C                                       Clear antenna selection
C                                       criteria for UVGET
      CALL FILL (50, 0, ANTENS)
      DESEL = .FALSE.
C                                       ANTENNAS array.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       get frequency info
      IVER = 1
      CALL CHNDAT ('READ', BUFF, DISKIN, CNOIN, IVER, CATBLK, LUN, NIF,
     *   FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1035) IRET
         GO TO 990
         END IF
C                                       Open "line printer"
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      LPOPN = .TRUE.
C                                       Check STOKES - requested
      JS = 0
      DO 70 I = 1,23
         IF (STOKES.EQ.CHSTOK(I)) JS = I
 70      CONTINUE
      IF (JS.EQ.23) THEN
         IF (ICOR0.LE.-5) THEN
            JS = 22
         ELSE IF (ICOR0.GT.0) THEN
            JS = 8
         ELSE
            JS = 16
            END IF
         STOKES = CHSTOK(JS)
      ELSE IF (JS.EQ.0) THEN
         IF (ICOR0.GT.0) THEN
            JS = 1
            IF (NCOR.GE.3) JS = 5
            IF (NCOR.GE.4) JS = 6
         ELSE IF (ICOR0.GT.-5) THEN
            JS = 8 - ICOR0
            IF ((NCOR.GE.2) .AND. (ICOR0.EQ.-1)) JS = 13
            IF (ICOR0-NCOR+1.LE.-4) JS = 14
         ELSE
            JS = 12 - ICOR0
            IF ((NCOR.GE.2) .AND. (ICOR0.EQ.-5)) JS = 13
            IF (ICOR0-NCOR+1.LE.-8) JS = 14
            END IF
         STOKES = CHSTOK(JS)
         END IF
C      RSTOKS = STOKES
C      IF (JS.GT.14) STOKES = CHSTOK(JS-2)
C      IF (JS.EQ.8) STOKES = CHSTOK(5)
      ISCROS(1) = F
      ISCROS(2) = F
      ISCROS(3) = F
      ISCROS(4) = F
      STOFF = 0
      NCOLS = 1
      IF (JS.EQ.5) THEN
         NCOLS = 3
      ELSE IF (JS.EQ.6) THEN
         NCOLS = 4
      ELSE IF (JS.EQ.7) THEN
         NCOLS = 2
      ELSE IF (JS.EQ.8) THEN
         NCOLS = 2
      ELSE IF ((JS.EQ.11) .OR. (JS.EQ.19)) THEN
         ISCROS(1) = T
      ELSE IF ((JS.EQ.12) .OR. (JS.EQ.20)) THEN
         ISCROS(1) = T
      ELSE IF (JS.EQ.13) THEN
         NCOLS = 2
      ELSE IF (JS.EQ.14) THEN
         NCOLS = 4
         ISCROS(3) = T
         ISCROS(4) = T
      ELSE IF ((JS.EQ.15) .OR. (JS.EQ.21)) THEN
         NCOLS = 2
      ELSE IF ((JS.EQ.16) .OR. (JS.EQ.22)) THEN
         NCOLS = 2
         ISCROS(1) = T
         ISCROS(2) = T
         END IF
C                                    Determine output type
      NCORP = NCOLS
      IF (DOAMPH) THEN
         IF (NACROS.GE.52+19*NCOLS) THEN
            OTYPE = 1
            NUMCH = (NACROS - 52) / (19 * NCOLS)
         ELSE IF (NACROS.GE.46+18*NCOLS) THEN
            OTYPE = 2
            NUMCH = (NACROS - 46) / (18 * NCOLS)
         ELSE IF (NACROS.GE.37+17*NCOLS) THEN
            OTYPE = 3
            NUMCH = (NACROS - 37) / (17 * NCOLS)
         ELSE IF (NACROS.GE.35+15*NCOLS) THEN
            OTYPE = 4
            NUMCH = (NACROS - 35) / (15 * NCOLS)
         ELSE IF (NACROS.GE.16+14*NCOLS) THEN
            OTYPE = 5
            NUMCH = (NACROS - 16) / (14 * NCOLS)
         ELSE
            MSGTXT = 'FORMAT TOO WIDE FOR PRINTER: TRUNCATING'
            CALL MSGWRT (6)
            NUMCH = 1
            NCOLS = (NACROS - 16) / 14
            OTYPE = 5
            END IF
      ELSE
         IF (NACROS.GE.52+23*NCOLS) THEN
            OTYPE = 1
            NUMCH = (NACROS - 52) / (23 * NCOLS)
         ELSE IF (NACROS.GE.46+22*NCOLS) THEN
            OTYPE = 2
            NUMCH = (NACROS - 46) / (22 * NCOLS)
         ELSE IF (NACROS.GE.37+21*NCOLS) THEN
            OTYPE = 3
            NUMCH = (NACROS - 37) / (21 * NCOLS)
         ELSE IF (NACROS.GE.35+19*NCOLS) THEN
            OTYPE = 4
            NUMCH = (NACROS - 35) / (19 * NCOLS)
         ELSE IF (NACROS.GE.16+17*NCOLS) THEN
            OTYPE = 5
            NUMCH = (NACROS - 16) / (17 * NCOLS)
         ELSE
            MSGTXT = 'FORMAT TOO WIDE FOR PRINTER: TRUNCATED'
            CALL MSGWRT (6)
            NUMCH = 1
            NCOLS = (NACROS - 16) / 17
            OTYPE = 5
            END IF
         END IF
      I = CATBLK(KINAX+JLOCF) - BCHAN + 1
      NUMCH = MAX (1, MIN (NUMCH, I))
      NUMCH = MIN (NUMCH, 4)
      NCOLS = NCOLS * NUMCH
      ECHAN = BCHAN + NUMCH - 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING THE UV DATA SET')
 1020 FORMAT ('NO SN FILE FOUND, BUT DOCALIB IS TRUE: NO CAL APPLIED')
 1030 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1035 FORMAT ('ERROR',I5,' GETTING FREQUENCIES')
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE UVPRSC (IRET)
C-----------------------------------------------------------------------
C   determines scaling parameters
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   ICH, IROUND, INDX, K, IA1, IA2, NUMVIS, PCOUNT, SOUID,
     *   OLDSOU, LBL, JERR, IUSER, INVERT, JTT(3), JJTT(4), KBL
      CHARACTER PREFIX*5, UVCH*4, WWCH*4, UVCC*5, WWCC*5
      LOGICAL   REQBAS, FLAG, UVOPN
      REAL      AMP(4), WEIGHT, YAMP, UVN, U, V, W, XX, YY, XDAY, TEMP,
     *   RPARM(20), LUVFAC, CATR(256)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATBLK, CATR)
C-----------------------------------------------------------------------
C                                       init the range parameters
      XAMP = 0.0
      UVM = 0.0
      XWT = 0.0
      NWT = 1.E10
      MSGTXT = 'Finding the scaling parameters to set formats'
      CALL MSGWRT (1)
C                                       Set pointers, counters
      ICH = BCHAN
      LBL = 1
      KBL = 1
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *   + TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *   + TIMRNG(8) / (24. * 60. * 60.)
C                                       Holography loop point
      UVOPN = .FALSE.
      OLDSOU = -1
      INVERT = 1
C                                      UV scaling
      TEMP = 1.01 / XUVFAC
      CALL METSCA (TEMP, PREFIX, FLAG)
      IF (ABS(TEMP-1.0).LT.0.02) THEN
         UVCH = PREFIX(:4)
         IF (UVCH.EQ.' ') UVCH = 'Lam '
         UVCC = PREFIX
         IF (UVCC.EQ.' ') UVCC = 'Lambd'
      ELSE
         UVCH = 'S*La'
         UVCC = 'S*Lam'
         END IF
      WWCH = UVCH
      WWCC = UVCC
C                                      first page
      IUSER = NLUSER
      INITVS = IBCNT
C                                       Initialize reading VIS. file.
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1090) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      LUVFAC = (FREQ + CATR(KRCIC+JLOCF) * (1.0 - CATR(KRCRP+JLOCF)))
     *   / MAX (1.0D0, UVFREQ)
      UVOPN = .TRUE.
      NUMVIS = IBCNT - 1
      PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
      MULTI = ILOCSU.GE.0
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF, IRET)
         IF (IRET.EQ.-1) THEN
            IRET = 0
            GO TO 970
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (8)
            IRET = 4
            GO TO 970
            END IF
C                                       Check whether we need this
C                                       baseline
         IF (ILOCB.GE.0) THEN
            IA1 = INT (RPARM(ILOCB+1)) / 256
            IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
         ELSE
            IA1 = RPARM(ILOCA1+1) + 0.1
            IA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), NANT, IBAS(LBL),
     *      NBAS))
     *      GO TO 100
C                                       Include this count?
         NUMVIS = NUMVIS + 1
         IF (NUMVIS.LT.IBCNT) GO TO 100
         IF (MOD(NUMVIS-IBCNT,UVINC).NE.0) GO TO 100
            W = RPARM(ILOCW+1)
C                                       If time, decode it.
            IF (ILOCT.GE.0) THEN
               XDAY = RPARM(ILOCT+1)
            ELSE
               XDAY = 0.0D0
               END IF
C                                       Check source
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            CALL SOURS (.FALSE., SOUID, OLDSOU, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR PROCESSING NEW SOURCE'
               CALL MSGWRT (8)
               GO TO 970
               END IF
C                                       Scale uvw as user desires
            U = RPARM(ILOCU+1) * XUVFAC * LUVFAC
            V = RPARM(ILOCV+1) * XUVFAC * LUVFAC
            W = RPARM(ILOCW+1) * XUVFAC * LUVFAC
            UVM = MAX (UVM, U)
            UVM = MAX (UVM, V)
            IF (OTYPE.LE.2) UVM = MAX (UVM, W)
            UVN = U
            UVN = MIN (UVN, V)
            IF (OTYPE.LE.2) UVN = MIN (UVN, W)
            UVM = MAX (UVM, -10.0*UVN)
C                                       Get vis.
            YAMP = 0.0
            DO 110 K = 1,NCOLS
               INDX = 3 * (K - 1 + STOFF) + 1
               XX = BUFF(INDX)
               YY = BUFF(INDX+1)
               WEIGHT = BUFF(INDX+2)
               IF (WEIGHT.NE.0.0) THEN
                  XWT = MAX (XWT, ABS(WEIGHT))
                  NWT = MIN (NWT, ABS(WEIGHT))
                  END IF
               IF ((IA1.NE.IA2) .OR. (ISCROS(K))) THEN
                  AMP(K) = SQRT (XX*XX+YY*YY) * DPARM(10)
               ELSE
                  AMP(K) = XX * DPARM(10)
                  END IF
               XAMP = MAX (XAMP, AMP(K))
               YAMP = MIN (YAMP, AMP(K))
 110           CONTINUE
C                                       Write VIS data
            XAMP = MAX (XAMP, -10.0*YAMP)
            PCOUNT = PCOUNT + 1
            IF (PCOUNT.LT.XNCNT) GO TO 100
C                                       Close files.
 970  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      IF (IRET.LT.0) IRET = 0
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
      END
      SUBROUTINE UVPRCH (IRET)
C-----------------------------------------------------------------------
C   Counts lines that will be printed on line printer
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IROUND, I, IA1, IA2, NUMVIS, PCOUNT, SOUID, OLDSOU, LBL,
     *   JERR, JTT(3), JJTT(4), KBL, TTY(2), NCOUNT
      CHARACTER STR*4
      LOGICAL   REQBAS, UVOPN, TXOPN
      REAL      RPARM(20), CATR(256), CATUR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATUV, CATUR)
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      TXOPN = .FALSE.
C                                       Initialize reading VIS. file.
      INITVS = IBCNT
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         IRET = 4
         GO TO 960
         END IF
      UVOPN = .TRUE.
C                                       Set pointers, counters
      LBL = 1
      KBL = 1
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *   + TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *   + TIMRNG(8) / (24. * 60. * 60.)
      PAGE = 0
      OLDSOU = -1
C                                      first page
      NCOUNT = 0
C                                       omit header on -3
      IF (DOCRT.GT.-2.5) THEN
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.4))) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 4
         END IF
      NUMVIS = IBCNT - 1
      PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
      MULTI = ILOCSU.GE.0
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF, IRET)
         IF (IRET.EQ.-1) THEN
            IRET = 0
            GO TO 970
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 4
            GO TO 960
            END IF
C                                       Check whether we need this
C                                       baseline
         IF (ILOCB.GE.0) THEN
            IA1 = INT (RPARM(ILOCB+1)) / 256
            IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
         ELSE
            IA1 = RPARM(ILOCA1+1) + 0.1
            IA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), NANT, IBAS(LBL),
     *      NBAS))
     *      GO TO 100
C                                       Include this count?
         NUMVIS = NUMVIS + 1
         IF (NUMVIS.LT.IBCNT) GO TO 100
         IF (MOD(NUMVIS-IBCNT,UVINC).EQ.0) THEN
C                                       Check source
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            CALL SOURS (.TRUE., SOUID, OLDSOU, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR PROCESSING NEW SOURCE'
               GO TO 960
               END IF
C                                       Write VIS data
            NCOUNT = NCOUNT + 1
            PCOUNT = PCOUNT + 1
            END IF
         IF (PCOUNT.LT.XNCNT) GO TO 100
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, I)
            IRET = -1
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 960
         WRITE (SCRTCH,1180) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 960
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 960
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, I)
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 990
C                                       Close files.
 960  CALL MSGWRT (8)
 970  IF (IRET.LT.0) IRET = 0
      IF (LPOPN) CALL LPCLOS (LUNP, FINDP, IPCNT, JERR)
 990  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 1180 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE UVPRDO (IRET)
C-----------------------------------------------------------------------
C   Prints selected portions of uv data file.
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NNCH, JJ, IROUND, INDX, I, K, IA1, IA2, NUMVIS, PCOUNT,
     *   PHASE(4), IWT(4), SOUID, OLDSOU, ITT(4), JCOR, LBL, JERR, II,
     *   IUSER, INVERT, JTT(3), JJTT(4), KBL, WTFM, ICH(4), INDEX, L
      CHARACTER ISTOKE(4)*2, JSTOKE(4,3)*2, TCHAR*12, PREFIX*5, UVCH*4,
     *   TCHR1*13, WWCH*4, UVCC*5, WWCC*5, RCHAR*16, RCHR1*17
      LOGICAL   REQBAS, FLAG, UVOPN, TXOPN
      REAL      AMP(4), WEIGHT, TPHS, U, V, W, XX, YY, RWT(4), XDAY,
     *   TEMP, RPHAS(4), TIMS, RPARM(20), TADAY, WTSC, LUVFAC, RE(4),
     *   IM(4), CATR(256), CATUR(256)
      DOUBLE PRECISION LFREQ, CATD(128)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATUV, CATUR)
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA JSTOKE /'XX','YY','XY','YX','RR','LL','RL','LR',
     *   'I ','Q ','U ','V '/
C-----------------------------------------------------------------------
      TXOPN = .FALSE.
      IF (DPARM(1).GE.0.0) THEN
         XWT = DPARM(2)
         NWT = DPARM(3)
         XAMP = DPARM(4)
         UVM = DPARM(5)
         IF (NWT.LE.0.0) NWT = 0.11
         IF (XWT.LE.0.0) XWT = 9.9
         IF (XAMP.LE.0.0) XAMP = 99.0
         IF (UVM.LE.0.0) UVM = 10000.
         END IF
C                                       Initialize reading VIS. file.
      INITVS = IBCNT
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1090) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
C                                       Set pointers, counters
C                                       Stokes labels
      NNCH = BCHAN - 1
      DO 15 II = 1,4,NCORP
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCORP
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ - CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            IF (I.LE.-5) THEN
               L = 1
               I = I + 4
            ELSE IF (I.LT.0) THEN
               L = 2
            ELSE
               L = 3
               END IF
            ISCROS(JJ) = (I.EQ.-3) .OR. (I.EQ.-4)
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
 10         CONTINUE
 15      CONTINUE
      LFREQ = FREQ + (BCHAN - CATUR(KRCRP+JLOCF))*FINC(BIF)
      LFREQ = LFREQ * 1.D-9
      LBL = 1
      KBL = 1
      TADAY = -100.
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.)
     *   + TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.)
     *   + TIMRNG(8) / (24. * 60. * 60.)
C                                       Regular printing
      WTSC = 1.0
C                                       weight scaling
      IF (DOAMPH) THEN
         TCHAR = ' Amp Phas Wt'
         TCHR1 = 'Amp  Phas  Wt'
      ELSE
         RCHAR = 'Real   Imag  Wt'
         IF (OTYPE.EQ.4) RCHAR = 'Real    Imag  Wt'
         RCHR1 = 'Real    Imag  Wt'
         END IF
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      IF (OTYPE.LE.3) THEN
         TEMP = XWT / NWT
         IF (TEMP.GT.10**(6-OTYPE)) THEN
            MSGTXT = 'Full dynamic range of weights cannot' //
     *         ' be printed'
            CALL MSGWRT (6)
            END IF
         WTFM = 4 - OTYPE
         IF (OTYPE.EQ.1) THEN
            IF ((XWT.LT.9.99995) .AND. (NWT.GE.0.0001)) THEN
               WTSC = 1.0
               WTFM = 4
               IF (XWT.LT.0.09995) WTSC = 10.0
            ELSE IF ((XWT.LT.99.9995) .AND. (NWT.GE.0.001)) THEN
               WTSC = 1.0
               WTFM = 3
            ELSE IF ((XWT.LT.999.995) .AND. (NWT.GE.0.01)) THEN
               WTSC = 1.0
               WTFM = 2
            ELSE IF ((XWT.LT.9999.95) .AND. (NWT.GE.0.1)) THEN
               WTSC = 1.0
               WTFM = 1
               END IF
         ELSE IF (OTYPE.EQ.2) THEN
            IF ((XWT.LT.9.99995) .AND. (NWT.GE.0.001)) THEN
               WTSC = 1.0
               WTFM = 3
               IF (XWT.LT.0.09995) WTSC = 10.0
            ELSE IF ((XWT.LT.99.9995) .AND. (NWT.GE.0.01)) THEN
               WTSC = 1.0
               WTFM = 2
            ELSE IF ((XWT.LT.999.995) .AND. (NWT.GE.0.1)) THEN
               WTSC = 1.0
               WTFM = 1
            END IF
         ELSE IF (OTYPE.EQ.3) THEN
            IF ((XWT.LT.9.99995) .AND. (NWT.GE.0.01)) THEN
               WTSC = 1.0
               WTFM = 2
               IF (XWT.LT.0.09995) WTSC = 10.0
            ELSE IF ((XWT.LT.99.9995) .AND. (NWT.GE.0.1)) THEN
               WTSC = 1.0
               WTFM = 1
               END IF
            END IF
      ELSE
         TEMP = XWT / NWT
         IF (TEMP.GT.99.50) THEN
            MSGTXT = 'Full dynamic range of weights cannot' //
     *         ' be printed'
            CALL MSGWRT (6)
            END IF
         END IF
      PAGE = 0
C                                       Former holography loop point
      UVOPN = .FALSE.
      OLDSOU = -1
      INVERT = 1
C                                      UV scaling
      TEMP = 1.01 / XUVFAC
      CALL METSCA (TEMP, PREFIX, FLAG)
      IF (ABS(TEMP-1.0).LT.0.02) THEN
         UVCH = PREFIX(:4)
         IF (UVCH.EQ.' ') UVCH = 'Lam '
         UVCC = PREFIX
         IF (UVCC.EQ.' ') UVCC = 'Lambd'
      ELSE
         UVCH = 'S*La'
         UVCC = 'S*Lam'
         END IF
      WWCH = UVCH
      WWCC = UVCC
C                                      first page
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IUSER = NLUSER
C                                       omit header on -3
      IF (DOCRT.GT.-2.5) THEN
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1050) NAMEIN, CLAIN, SEQIN, DISKIN, IUSER,
     *         ICH(1), BIF
         ELSE
            WRITE (LINE,1051) NAMEIN, CLAIN, SEQIN, DISKIN, IUSER,
     *         ICH(1), BIF
            END IF
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         WRITE (LINE,1055) LFREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
C                                       UV scaling
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.4))) THEN
            IF (UVCH.EQ.'S*La') THEN
               TEMP = 1.0 / XUVFAC
               WRITE (LINE,1056) TEMP
            ELSE
               LINE = 'U, V, W are in ' // PREFIX // 'wavelengths '
     *            // 'at the selected frequency'
               END IF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 960
            END IF
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1057) WTSC
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 960
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
C                                       Page titles
      IF (DOAMPH) THEN
         IF (OTYPE.EQ.1) THEN
            WRITE (TITL1,1061) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,1071) UVCC, UVCC, WWCC, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.2) THEN
            WRITE (TITL1,1062) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,1072) UVCH, UVCH, WWCH, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.3) THEN
            WRITE (TITL1,1063) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,1073) UVCH, UVCH, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.4) THEN
            WRITE (TITL1,1064) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,1074) UVCH, UVCH, (TCHAR, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.5) THEN
            WRITE (TITL1,1065) SOURCE, LFREQ, (ICH(JCOR), ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1075) (TCHAR, JCOR = 1,NCOLS)
            END IF
      ELSE
         IF (OTYPE.EQ.1) THEN
            WRITE (TITL1,3061) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,3071) UVCC, UVCC, WWCC, (RCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.2) THEN
            WRITE (TITL1,3062) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,3072) UVCH, UVCH, WWCH, (RCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.3) THEN
            WRITE (TITL1,3063) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,3073) UVCH, UVCH, (RCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.4) THEN
            WRITE (TITL1,3064) SOURCE, LFREQ, ISORT, (ICH(JCOR),
     *         ISTOKE(JCOR), JCOR = 1,NCOLS)
            WRITE (TITL2,3074) UVCH, UVCH, (RCHAR, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.5) THEN
            WRITE (TITL1,3065) SOURCE, LFREQ, (ICH(JCOR), ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,3075) (RCHAR, JCOR = 1,NCOLS)
            END IF
         END IF
      LUVFAC = (FREQ + CATR(KRCIC+JLOCF) * (1.0 - CATR(KRCRP+JLOCF)))
     *   / MAX (1.0D0, UVFREQ)
      UVOPN = .TRUE.
      NUMVIS = IBCNT - 1
      PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
      MULTI = ILOCSU.GE.0
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF, IRET)
         IF (IRET.EQ.-1) THEN
            IRET = 0
            GO TO 970
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (8)
            IRET = 4
            GO TO 970
            END IF
C                                       Check whether we need this
C                                       baseline
         IF (ILOCB.GE.0) THEN
            IA1 = INT (RPARM(ILOCB+1)) / 256
            IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
         ELSE
            IA1 = RPARM(ILOCA1+1) + 0.1
            IA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), NANT, IBAS(LBL),
     *      NBAS))
     *      GO TO 100
C                                       Include this count?
         NUMVIS = NUMVIS + 1
         IF (NUMVIS.LT.IBCNT) GO TO 100
         IF (MOD(NUMVIS-IBCNT,UVINC).NE.0) GO TO 100
            W = RPARM(ILOCW+1)
C                                       If time, decode it.
            IF (ILOCT.GE.0) THEN
               XDAY = RPARM(ILOCT+1)
            ELSE
               XDAY = 0.0D0
               END IF
            CALL T2DHMS (0, XDAY, ITT, TIMS)
            CALL T2DHMS (1, XDAY, JJTT, TIMS)
C                                       Check source
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            CALL SOURS (.TRUE., SOUID, OLDSOU, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR PROCESSING NEW SOURCE'
               CALL MSGWRT (8)
               GO TO 970
               END IF
C                                       Scale uvw as user desires
            U = RPARM(ILOCU+1) * XUVFAC * LUVFAC
            V = RPARM(ILOCV+1) * XUVFAC * LUVFAC
            W = RPARM(ILOCW+1) * XUVFAC * LUVFAC
C                                       Get vis.
            DO 110 K = 1,NCOLS
               INDX = 3 * (K - 1 + STOFF) + 1
               XX = BUFF(INDX)
               YY = BUFF(INDX+1)
               WEIGHT = BUFF(INDX+2)
               TPHS = WEIGHT * WTSC
               RWT(K) = TPHS
               IWT(K) = IROUND (TPHS)
               IF (IWT(K).EQ.0) THEN
                  IF (TPHS.LT.0.0) IWT(K) = -1
                  IF (TPHS.GT.0.0) IWT(K) = 1
                  END IF
               IF ((IA1.NE.IA2) .OR. (ISCROS(K))) THEN
                  AMP(K) = SQRT (XX*XX+YY*YY) * DPARM(10)
                  TPHS = INVERT * 57.296 * ATAN2 (YY, XX+1.0E-20)
               ELSE
                  AMP(K) = XX * DPARM(10)
                  TPHS = 0.0
                  END IF
               RPHAS(K) = TPHS
               PHASE(K) = IROUND (TPHS)
               IF ((PHASE(K).LT.-90) .AND. (DPARM(6).GT.0.0)) PHASE(K) =
     *            PHASE(K) + 360
               RE(K) = XX * DPARM(10)
               IM(K) = YY * DPARM(10)
 110           CONTINUE
C                                       Write VIS data
C                                       Printing
            IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.1)) THEN
               IF (DOAMPH) THEN
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,2101,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,2111,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     END IF
               ELSE
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,3101,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,3111,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.2)) THEN
               IF (DOAMPH) THEN
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,2102,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,2112,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     END IF
               ELSE
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,3102,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,3112,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.3)) THEN
               IF (DOAMPH) THEN
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,2103,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,2113,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     END IF
               ELSE
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,3103,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,3113,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.4)) THEN
               IF (DOAMPH) THEN
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,2104,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,2114,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     END IF
               ELSE
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,3104,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,3114,ERR=180) JTT, TIMS, IA1, IA2, U,
     *                  V, W, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.1)) THEN
               IF (UVM.LE.9999.98) THEN
                  IF (DOAMPH) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2201,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2211,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3201,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3211,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (DOAMPH) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2221,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2231,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3221,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3231,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.2)) THEN
               IF (DOAMPH) THEN
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2202,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2212,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2222,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2232,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1, NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3202,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3212,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3222,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3232,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1, NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.3)) THEN
               IF (DOAMPH) THEN
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2203,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2203,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2223,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2233,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (AMP(K), PHASE(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3203,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3203,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3223,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3233,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, W, (RE(K), IM(K), RWT(K),
     *                     K = 1,NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.3) .AND. (WTFM.EQ.1)) THEN
               IF (DOAMPH) THEN
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2301,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2311,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2321,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2331,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3301,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3311,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3321,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3331,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF ((OTYPE.EQ.3) .AND. (WTFM.EQ.2)) THEN
               IF (DOAMPH) THEN
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2302,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2312,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2322,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2332,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3302,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3312,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3322,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3332,ERR=180) JTT, TIMS, IA1, IA2,
     *                     U, V, (RE(K), IM(K), RWT(K), K = 1,NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF (OTYPE.EQ.4) THEN
               IF (DOAMPH) THEN
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2400,ERR=180) ITT, IA1, IA2, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2410,ERR=180) ITT, IA1, IA2, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2420,ERR=180) ITT, IA1, IA2, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2430,ERR=180) ITT, IA1, IA2, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                        END IF
                     END IF
               ELSE
                  IF (UVM.LE.9999.98) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3400,ERR=180) ITT, IA1, IA2, U, V,
     *                     (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3410,ERR=180) ITT, IA1, IA2, U, V,
     *                     (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                        END IF
                  ELSE
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,3420,ERR=180) ITT, IA1, IA2, U, V,
     *                     (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,3430,ERR=180) ITT, IA1, IA2, U, V,
     *                     (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                        END IF
                     END IF
                  END IF
            ELSE IF (OTYPE.EQ.5) THEN
               IF (DOAMPH) THEN
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,2500,ERR=180) JTT(2), JTT(3), TIMS,
     *                  IA1, IA2, (AMP(K), PHASE(K), IWT(K),
     *                  K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,2510,ERR=180) JTT(2), JTT(3), TIMS,
     *                  IA1, IA2, (AMP(K), PHASE(K), IWT(K),
     *                  K = 1,NCOLS)
                     END IF
               ELSE
                  IF (XAMP.LE.99.9) THEN
                     WRITE (LINE,3500,ERR=180) JTT(2), JTT(3), TIMS,
     *                  IA1, IA2, (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                  ELSE
                     WRITE (LINE,3510,ERR=180) JTT(2), JTT(3), TIMS,
     *                  IA1, IA2, (RE(K), IM(K), IWT(K), K = 1,NCOLS)
                     END IF
                  END IF
               END IF
C                                       Leading zero(s)
            IF (OTYPE.EQ.5) THEN
               IF (LINE(7:7).EQ.' ') LINE(7:7) = '0'
               IF (LINE(8:8).EQ.' ') LINE(8:8) = '0'
            ELSE
               IF (LINE(10:10).EQ.' ') LINE(10:10) = '0'
               IF (LINE(11:11).EQ.' ') LINE(11:11) = '0'
               END IF
C                                       Write VIS data
 180        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 960
            PCOUNT = PCOUNT + 1
            IF (PCOUNT.LT.XNCNT) GO TO 100
C                                       CRT error
 960  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1960) IRET
         CALL MSGWRT (8)
         END IF
C                                       Close files.
 970  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      IF (IRET.LT.0) IRET = 0
      IF (LPOPN) CALL LPCLOS (LUNP, FINDP, IPCNT, JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5,5X,'Channel =',I5,5X,'IF =',I3)
 1051 FORMAT (A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,'  Channel=',
     *   I5,'  IF=',I3)
 1055 FORMAT ('Chan freq=',F13.9,' GHz  Ncor=',I3,'  No. vis=',I10,
     *   '  Sort order= ',A2)
 1056 FORMAT ('U, V, W are in',F11.5,'''s of wavelengths at the ',
     *   'reference frequency')
 1057 FORMAT ('Weights have been multiplied by',1PE9.1)
 1061 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,1X,
     *   4(4X,I4,3X,A2,6X))
 1062 FORMAT (A8,4X,'Freq=',F13.9,4X,'Sort= ',A2,4X,5(4X,I4,3X,A2,5X))
 1063 FORMAT (A8,2X,'Freq=',F13.9,2X,'Sort= ',A2,1X,4(2X,I4,2X,A2,7X))
 1064 FORMAT (A8,1X,'Freq=',F13.9,1X,'Sort= ',A2,1X,4(2X,I4,2X,A2,5X))
 1065 FORMAT (A8,F8.4,4(4X,I4,1X,A2,3X))
 1071 FORMAT (4X,'Time',7X,'Ant    U(',A,')   V(',A,')   W(',A,
     *   ')',4(3X,A,3X))
 1072 FORMAT (4X,'Time',7X,'Ant   U(',A,')  V(',A,')  W(',A,
     *   ')',4(3X,A,2X))
 1073 FORMAT (4X,'Time',7X,'Ant   U(',A,')  V(',A,')',4(3X,A,1X))
 1074 FORMAT (3X,'Time',6X,'Ant   U(',A,')  V(',A,')',4(3X,A))
 1075 FORMAT (3X,'Time',5X,'Ant ',4(2X,A))
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 2101 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,I4,F7.1))
 2102 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,I4,F7.2))
 2103 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,I4,F7.3))
 2104 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,I4,F7.4))
 2111 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,I4,F7.1))
 2112 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,I4,F7.2))
 2113 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,I4,F7.3))
 2114 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,I4,F7.4))
 2201 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,I4,F6.1))
 2202 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,I4,F6.2))
 2203 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,I4,F6.3))
 2211 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.1,I4,F6.1))
 2212 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.1,I4,F6.2))
 2221 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,I4,F6.1))
 2222 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,I4,F6.2))
 2223 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,I4,F6.3))
 2231 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,I4,F6.1))
 2232 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,I4,F6.2))
 2233 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,I4,F6.3))
 2301 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.3,I4,F5.1))
 2302 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.3,I4,F5.2))
 2311 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.1,I4,F5.1))
 2312 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.1,I4,F5.2))
 2321 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.3,I4,F5.1))
 2322 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.3,I4,F5.2))
 2331 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.1,I4,F5.1))
 2332 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.1,I4,F5.2))
 2400 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.2,
     *   4(F8.3,I4,I3))
 2410 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.2,
     *   4(F8.1,I4,I3))
 2420 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.0,
     *   4(F8.3,I4,I3))
 2430 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.0,
     *   4(F8.1,I4,I3))
 2500 FORMAT (I2.2,':',I2.2,':',F4.1,I3,'-',I2,4(F7.3,I4,I3))
 2510 FORMAT (I2.2,':',I2.2,':',F4.1,I3,'-',I2,4(F7.1,I4,I3))
 3061 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,1X,
     *   4(5X,I4,5X,A2,7X))
 3062 FORMAT (A8,4X,'Freq=',F13.9,4X,'Sort= ',A2,4X,5(5X,I4,5X,A2,6X))
 3063 FORMAT (A8,2X,'Freq=',F13.9,2X,'Sort= ',A2,4(4X,I4,4X,A2,7X))
 3064 FORMAT (A8,1X,'Freq=',F13.9,1X,'Sort= ',A2,1X,4(3X,I4,2X,A2,8X))
 3065 FORMAT (A8,F8.4,4(4X,I4,1X,A2,6X))
 3071 FORMAT (4X,'Time',7X,'Ant    U(',A,')   V(',A,')   W(',A,
     *   ')',4(3X,A,3X))
 3072 FORMAT (4X,'Time',7X,'Ant   U(',A,')  V(',A,')  W(',A,
     *   ')',4(3X,A,2X))
 3073 FORMAT (4X,'Time',7X,'Ant   U(',A,')  V(',A,')',4(3X,A,1X))
 3074 FORMAT (3X,'Time',6X,'Ant   U(',A,')  V(',A,')',4(3X,A))
 3075 FORMAT (3X,'Time',5X,'Ant ',1X,4(1X,A))
 3101 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,F8.3,F7.1))
 3102 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,F8.3,F7.2))
 3103 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,F8.3,F7.3))
 3104 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.3,F8.3,F7.4))
 3111 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,F8.3,F7.1))
 3112 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,F8.3,F7.2))
 3113 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,F8.3,F7.3))
 3114 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F11.2,
     *   4(F8.1,F8.3,F7.4))
 3201 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,F8.3,F6.1))
 3202 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,F8.3,F6.2))
 3203 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.3,F8.3,F6.3))
 3211 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.1,F8.3,F6.1))
 3212 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.2,
     *   4(F8.1,F8.3,F6.2))
 3221 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,F8.3,F6.1))
 3222 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,F8.3,F6.2))
 3223 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.3,F8.3,F6.3))
 3231 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,F8.1,F6.1))
 3232 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,F8.1,F6.2))
 3233 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,3F9.0,
     *   4(F8.1,F8.1,F6.3))
 3301 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.3,F8.3,F5.1))
 3302 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.3,F8.3,F5.2))
 3311 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.1,F8.1,F5.1))
 3312 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.2,
     *   4(F8.1,F8.1,F5.2))
 3321 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.3,F8.3,F5.1))
 3322 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.3,F8.3,F5.2))
 3331 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.1,F8.1,F5.1))
 3332 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,I3,'-',I2,2F9.0,
     *   4(F8.1,F8.1,F5.2))
 3400 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.2,
     *   4(F8.3,F8.3,I3))
 3410 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.2,
     *   4(F8.1,F8.1,I3))
 3420 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.0,
     *   4(F8.3,F8.3,I3))
 3430 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,I3,'-',I2,2F9.0,
     *   4(F8.1,F8.1,I3))
 3500 FORMAT (I2.2,':',I2.2,':',F4.1,I3,'-',I2,4(F7.3,F7.3,I3))
 3510 FORMAT (I2.2,':',I2.2,':',F4.1,I3,'-',I2,4(F7.1,F7.1,I3))
 1960 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL, PRINTER, OR FILE')
      END
      SUBROUTINE SOURS (DOPRT, SOUID, OLDSOU, IRET)
C-----------------------------------------------------------------------
C   Process the next source number adding header info if the source
C   changes. Calls GETSOU to fill in commons with source info using
C   GETSOU.
C   Input:
C      DOPRT   L      Print things?
C      SOUID   I      Source ID number
C   Input from common:
C      MULTI   L      If true then this is a multi source file.
C      DISK    I      Input file disk number.
C      CNO     I      Input file catalog slot number
C      NSOUWD  I      Number of source numbers to check in SOUWAN
C      SOUWAN  I(*)   List of source numbers desired.
C      LUNP    I      LUN for output.
C      FINDP   I      FTAB pointer for output.
C      DOCRT   R      Requested output type and width.
C      NACROS  I      Actual output width.
C      OTYPE   I      Output width type.
C   Input/output:
C      OLDSOU  I      Last source number, -1 on input => first call.
C   Input/output from common:
C      TITL1   C*132  First title line
C      TITL2   C*132  Second title line
C      IPCNT   I      Line count on page
C      PAGE    I      Page number
C   Output:
C      IRET    I      Return error code, 0=>OK else failed
C   Output in common:C      SNAME   C*8    Source name (DUVH.INC)
C      All values in DSOU.INC
C-----------------------------------------------------------------------
      INTEGER   SOUID, OLDSOU, IRET
      LOGICAL   DOPRT
C
      INTEGER   SULUN, ILQUAL, LPAGE, OLDQ
      CHARACTER OLDNAM*8, LLCH*4, MMCH*4
      HOLLERITH CATUVH(256)
      REAL      RSEC, DSEC
      INCLUDE 'UVPRT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATUV, CATUVH)
      DATA SULUN /17/
C-----------------------------------------------------------------------
      IRET = 0
C                                       If same source as last skip.
      IF (SOUID.EQ.OLDSOU) GO TO 999
C                                       Get new source info
      OLDQ = LQUAL
      IF (MULTI) THEN
         CALL GETSOU (SOUID, DISKIN, CNOIN, CATUV, SULUN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, SOUID
            GO TO 990
            END IF
         SOURCE = SNAME
         RA = RAEPO / DG2RAD
         DEC = DECEPO / DG2RAD
         LQUAL = QUAL
         END IF
C                                       Change source name in TITL1
      IF (DOPRT) THEN
         IF (OTYPE.LE.1) THEN
            OLDNAM = TITL1(9:16)
            TITL1(9:16) = SOURCE
         ELSE
            OLDNAM = TITL1(:8)
            TITL1(1:8) = SOURCE
            END IF
C                                       RA-Dec labels
         CALL H2CHR (4, 1, CATUVH(KHCTP+JLOCR*2), LLCH)
         CALL H2CHR (4, 1, CATUVH(KHCTP+JLOCD*2), MMCH)
         IF (LLCH(:2).EQ.'RA') THEN
            CALL COORDD (1, RA, CHSIG1, HM, RSEC)
         ELSE
            CALL COORDD (2, RA, CHSIG1, HM, RSEC)
            END IF
         CALL COORDD (2, DEC, CHSIG2, DD, DSEC)
C                                       Blank line for new source
         LPAGE = PAGE
         IF ((OLDSOU.GE.0) .AND. (DOCRT.GT.-2.5)) THEN
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
         ELSE IF ((OLDSOU.LT.0) .AND. (DOCRT.LE.-2.5)) THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL1, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL2, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            END IF
C                                       Title for subsequent new source.
         IF ((OLDSOU.LT.0) .OR. (SOURCE.NE.OLDNAM) .OR. (OLDQ.NE.LQUAL))
     *      THEN
            ILQUAL = MAX (-9999, MIN (99999, QUAL))
            WRITE (LINE,1055) SOURCE, ILQUAL, LLCH, CHSIG1, HM, RSEC,
     *         MMCH, CHSIG2, DD, DSEC
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            IF ((LPAGE.GE.PAGE) .AND. (DOCRT.GT.-2.5)) THEN
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *            TITL2, TITL1, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               END IF
            IF ((LPAGE.GE.PAGE) .AND. (DOCRT.GT.-2.5)) THEN
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *            TITL2, TITL2, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               END IF
            END IF
C                                       Blank line after first
         IF ((OLDSOU.LT.0) .AND. (DOCRT.GT.-2.5)) THEN
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *         TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            END IF
         END IF
C                                       Save old source number
      OLDSOU = SOUID
      GO TO 999
C                                       Error writing output
 950  WRITE (MSGTXT,1950) IRET
C                                       Error
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR:',I7,' OBTAINING DATA FOR SOURCE #',I5)
 1055 FORMAT ('Source ',A8,' (',I5,')  ',A4,' = ',A1,I2.2,I3.2,F6.2,2X,
     *   A4,' = ',A1,I2.2,I3.2,F5.1)
 1950 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL OR PRINTER')
      END
      SUBROUTINE T2DHMS (NDIG, TIMEIN, TIME, TIMS)
C-----------------------------------------------------------------------
C   Convert from Time to Days Hours Minutes Seconds format
C   Input:
C      NDIG     I       Number of digits in display, determines rounding
C                       for TIMS.
C      TIMEIN   R       Time in days
C   Output:
C      TIME     I*(4)   Output Time in Days Hours Minutes Seconds
C      TIMS     R       Output Time in Seconds
C-----------------------------------------------------------------------
      REAL     TIMEIN, TIMS
      INTEGER  NDIG, TIME(4)
C
      REAL     T
      INTEGER  MUL
C-----------------------------------------------------------------------
      MUL = 10 ** (MAX (0, NDIG))
      T = TIMEIN
      IF (TIMEIN.LT.0.0) T = -T
C
      TIME(1) = T
      T = (T - TIME(1)) * 24.0
      TIME(2) = T
      T = (T - TIME(2)) * 60.0
      TIME(3) = T
      T = (T - TIME(3)) * 60.0
      TIMS = T
      TIME(4) = T*MUL + 0.5
C                                       Now Remove 60 seconds
      IF (TIME(4).GE.60*MUL) THEN
         TIME(4) = TIME(4) - 60*MUL
         TIME(3) = TIME(3) + 1
         END IF
C                                       Now Remove 60 minutes
      IF (TIME(3).GE.60) THEN
         TIME(3) = TIME(3) - 60
         TIME(2) = TIME(2) + 1
         END IF
C                                       Now Remove 24 hours
      IF (TIME(2).GE.24) THEN
         TIME(2) = TIME(2) - 24
         TIME(1) = TIME(1) + 1
         END IF
C                                       Sign
      IF (TIMEIN.LT.0.0) TIME(1) = -TIME(1)
C                                       Seconds
      TIMS = REAL (TIME(4)) / REAL (MUL)
      TIME(4) = TIMS + 0.5
C
 999  RETURN
      END

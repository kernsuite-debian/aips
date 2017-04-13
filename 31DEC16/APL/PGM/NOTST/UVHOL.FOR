LOCAL INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAMEIN*12, CLAIN*6, LPNAME(4)*96, TITL1*132, TITL2*132,
     *   SCRTCH*132, LINE*132, CHSIG1*1, CHSIG2*1, OPER*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XLPNAM(12),
     *   XOPER, XSTOK
      REAL      XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XSUBA, XCHANS(4,20), XBIF, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XNCNT, XUVFAC, XNPTS, APARM(10), DPARM(10), XBDROP,
     *   DOCRT, XBADD(10)
      REAL      BUFF(UVBFSS), XAMP, XWT, NWT, UVM, WBASE, ACH
      INTEGER   UVINC, SEQIN, DISKIN, LUNI, INDI, JBUFSZ, IANT(50),
     *   NANT, IBAS(50), NBAS, CNOIN, NACROS, OTYPE, NCOLS, LUNP(4),
     *   FINDP(4), PAGE, IPCNT, HM(2), DD(2), LQUAL, NSTKS, CHNSEL(3,20)
      LOGICAL   MULTI, DESEL, ISCROS(4), LPOPN(4), DOCONV
      DOUBLE PRECISION LFREQ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XSUBA, XCHANS,
     *   XBIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XNCNT, XUVFAC, XOPER, XNPTS, APARM, XBDROP,
     *   DPARM, DOCRT, XLPNAM, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, LPNAME, TITL1, TITL2, SCRTCH,
     *   LINE, OPER, CHSIG1, CHSIG2
      COMMON /BUFRS/ BUFF, JBUFSZ
      COMMON /UVPCOM/ LFREQ, MULTI, UVINC, SEQIN, DISKIN, LUNI, INDI,
     *   CNOIN, NACROS, OTYPE, NCOLS, LUNP, FINDP, PAGE, IPCNT, HM, DD,
     *   LQUAL, XAMP, XWT, NWT, UVM, ISCROS, LPOPN, NSTKS, DOCONV,
     *   WBASE, CHNSEL, ACH
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS
LOCAL END
      PROGRAM UVHOL
C-----------------------------------------------------------------------
C! UVHOL prints holography uvdata with calibration
C# Printer appl UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2000, 2003-2007, 2009-2012, 2014-2016
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
C   UVHOL prints uv data with calibration.
C   Inputs:
C      INNAME         NAMEIN        File name to be imaged
C      INCLASS        CLAIN         File class to be imaged
C      INSEQ          SEQIN         File sequence number
C      INDISK         DISKIN        Disk volume on which file resides
C      SOURCES        XSOUR(4,30)   Sources selected
C      QUAL           XQUAL         Source qualifier #, -1 => all
C      CALCODE                      Calibrator code, ' ' all
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
C      NCOUNT         XNCNT         # samples printed
C      DOCRT          DOCRT         <= 0 use line printer/file, > 0
C                                   use CRT, >72 -> CRT width
C      OUTPRINT       XLPNAM        File name to hold line printer out
C      BADDISK        XBADD(10)     Disks to avoid for scratch
C-----------------------------------------------------------------------
      CHARACTER  PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UVHOL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVHOIN (PRGM, IRET)
C                                       check line printer job size
      IF (IRET.EQ.0) CALL UVHOCH (IRET)
C                                       Do print scaling, uv init
      IF ((IRET.EQ.0) .AND. (DPARM(1).LT.0.0)) CALL UVHOSC (IRET)
C                                       Do print
      IF (IRET.EQ.0) CALL UVHODO (IRET)
C                                       Close down
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE UVHOIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   UVPIN gets input parameters for UVHOL.
C   Inputs:
C      PRGM   C*6   Program name
C    Output:
C      IRET   I      Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER UTYPE*2, STAT*4, JSTOKE(13)*2
      INTEGER   IUSER, I, IERR, IROUND, NPARM, LUNTB, LUN, JS, J, K
      LOGICAL   T, TABLE, FITASC, F, MATCH, SNEXST
      REAL      CATR(256), RTEMP
      DOUBLE PRECISION CATD(128)
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T, F /.TRUE., .FALSE./
      DATA LUNTB /39/
      DATA JSTOKE /'YX','XY','YY','XX','LR','RL','LL','RR','  ','I',
     *   'Q','U','V'/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      LPOPN(1) = .FALSE.
      LPOPN(2) = .FALSE.
      LPOPN(3) = .FALSE.
      LPOPN(4) = .FALSE.
C                                       Get input parameters.
      NPARM = 382
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
      IF (IRET.EQ.0) THEN
         CALL H2CHR (4, 1, XOPER, OPER)
         IF (OPER.EQ.'HOLG') DOCRT = MIN (-1.0, DOCRT)
         END IF
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
C                                       scaling
      IF (DPARM(1).GE.0.0) THEN
         IF (DPARM(2).LE.0.0) DPARM(2) = 99.9
         IF (DPARM(3).LE.0.0) DPARM(3) = 0.11
         IF (DPARM(4).LE.0.0) DPARM(4) = 9.9
         IF (DPARM(5).LE.0.0) DPARM(5) = 99.9
         XWT = DPARM(2)
         NWT = DPARM(3)
         XAMP = DPARM(4)
         UVM = DPARM(5)
         END IF
C                                       Crunch input parameters.
      IUSER = NLUSER
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
         CALL ISTAB ('SN', DISKIN, CNOIN, 1, LUNTB, BUFF,
     *      TABLE, SNEXST, FITASC, IERR)
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
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL RCOPY (8, XTIME, TIMRNG)
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
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
C                                       channel sel
      K = 0
      CALL FILL (60, 0, CHNSEL)
      DO 20 J = 1,20
         I = IROUND (XCHANS(2,J))
         IF (I.GT.0) THEN
            I = IROUND (XCHANS(4,J))
            IF ((I.LE.0) .OR. (I.EQ.BIF)) THEN
               K = K + 1
               CHNSEL(1,K) = IROUND (XCHANS(1,J))
               CHNSEL(2,K) = IROUND (XCHANS(2,J))
               CHNSEL(3,K) = IROUND (XCHANS(3,J))
               IF (CHNSEL(3,K).LE.0) CHNSEL(3,K) = 1
               END IF
            END IF
 20      CONTINUE
      IF (K.EQ.0) THEN
         CHNSEL(1,1) = (ECHAN+1)/8 + 1
         CHNSEL(2,1) = ECHAN - ((ECHAN+1)/8)
         CHNSEL(3,1) = 1
         END IF
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
C                                       get average channel and freq
      CALL SETFRQ (IRET)
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
C                                       Holography
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
      ANTENS(1) = IANT(1)
      ANTENS(2) = IBAS(1)
      IF ((IANT(1).LE.0) .OR. (IBAS(1).LE.0) .OR. DESEL) THEN
         IRET = 1
         MSGTXT = 'HOLOGRAPHY REQUIRES ANTEN(1) AND BASELI(1), NO DESEL'
         GO TO 990
         END IF
C                                       get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF, IRET)
      IF (ANAME.EQ.'EVLA') THEN
         WBASE = 0.0
         LQUAL = 40000
      ELSE
         WBASE = 40000.0
         LQUAL = 0
         END IF
C                                       Decode printer parms
C                                       Open "line printer"
      IF (OPER.NE.'HOLG') THEN
         LPNAME(1) = ' '
         CALL H2CHR (48, 1, XLPNAM, LPNAME(1))
         IF (LPNAME(1).EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME(1), DOCRT, LUNP(1), FINDP(1), NACROS, BUFF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
         LPOPN(1) = .TRUE.
      ELSE
         NACROS = 132
         END IF
      IF (XNCNT.LT.1.0) THEN
         XNCNT = PRTMAX - 10
         IF ((DOCRT.GT.0.0).OR.(LPNAME(1)(1:1).NE.' ')) XNCNT = 30000
         END IF
      IF (OPER.EQ.'HOLG') THEN
         XNCNT = 1000000
         IF ((XUVFAC.LT.0.1) .OR. (XUVFAC.GT.10.0)) THEN
            MSGTXT = 'LM scaling factor changed to 1.0'
            IF (XUVFAC.GT.0.) CALL MSGWRT (2)
            XUVFAC = 1.0
            END IF
      ELSE
         IF (XUVFAC.LE.1.E-10) XUVFAC = 1000.0
         END IF
C                                       Check STOKES - requested
      JS = 14
      NSTKS = CATBLK(KINAX+JLOCS)
      IF (NSTKS.EQ.1) THEN
         RTEMP = CATD(KDCRV+JLOCS) + (1 - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         J = IROUND (RTEMP) + 9
         STOKES = JSTOKE(J)
         DOCONV = .FALSE.
      ELSE IF (NSTKS.EQ.2) THEN
         DOCONV = (STOKES(1:1).EQ.'I') .AND. (ICOR0.LT.0)
         IF (OPER.EQ.'HOLG') JS = 15
         IF (.NOT.DOCONV) THEN
            STOKES = 'HALF'
         ELSE
            STOKES = 'IV'
            IF (CATD(KDCRV+JLOCS).LE.-5.0D0) STOKES = 'I'
            END IF
      ELSE IF (NSTKS.EQ.4) THEN
         DOCONV = STOKES(1:1).EQ.'I'
         NSTKS = 2
         IF (STOKES.EQ.'FULL') NSTKS = 4
         IF (STOKES.EQ.'IQUV') NSTKS = 4
C                                       Holography = RR and LL only
         IF (OPER.EQ.'HOLG') THEN
            IF (DOCONV) THEN
               IF (STOKES.NE.'IQUV') THEN
                  STOKES = 'IV'
                  IF (CATD(KDCRV+JLOCS).LE.-5.0D0) STOKES = 'I'
                  JS = 15
                  END IF
            ELSE
               IF (STOKES.NE.'FULL') THEN
                  STOKES = 'HALF'
                  JS = 15
                  END IF
               END IF
         ELSE
            STOKES = 'FULL'
            IF (DOCONV) STOKES = 'IQUV'
            END IF
         END IF
      IF (STOKES.EQ.'I') NSTKS = 1
      ISCROS(1) = F
      ISCROS(2) = F
      ISCROS(3) = F
      ISCROS(4) = F
      NCOLS = 1
      IF (JS.EQ.14) THEN
         NCOLS = 4
         IF (DOCONV) THEN
            ISCROS(2) = T
            ISCROS(3) = T
         ELSE
            ISCROS(3) = T
            ISCROS(4) = T
            END IF
      ELSE
         NCOLS = 2
         IF (STOKES.EQ.'I') NCOLS = 1
         END IF
      NCOLS = MIN (NCOLS, CATBLK(KINAX+JLOCS))
C                                    Determine output type
      IF (NACROS.GE.46+19*NCOLS) THEN
         OTYPE = 1
      ELSE IF (NACROS.GE.41+18*NCOLS) THEN
         OTYPE = 2
      ELSE IF (NACROS.GE.37+17*NCOLS) THEN
         OTYPE = 3
      ELSE IF (NACROS.GE.35+15*NCOLS) THEN
         OTYPE = 4
      ELSE IF (NACROS.GE.16+14*NCOLS) THEN
         OTYPE = 5
      ELSE
         MSGTXT = 'FORMAT TOO WIDE FOR PRINTER: CHANGE STOKES'
         IRET = 2
         GO TO 990
         END IF
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
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE SETFRQ (IRET)
C-----------------------------------------------------------------------
C   Find average channel number and frequency of observation
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVHOL.INC'
      INTEGER  NUMC, I, J, LUNO, LUNTMP, VER
      REAL     SUMC, CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATD, CATR)
C-----------------------------------------------------------------------
C                                       average channel
      NUMC = 0
      SUMC = 0.0
      DO 20 J = 1,20
         IF ((CHNSEL(1,J).GT.0) .AND. (CHNSEL(2,J).GT.0) .AND.
     *      (CHNSEL(2,J).GE.CHNSEL(1,J))) THEN
            DO 10 I = CHNSEL(1,J),CHNSEL(2,J)
               NUMC = NUMC + 1
               SUMC = SUMC + I
 10            CONTINUE
            END IF
 20      CONTINUE
      IF (NUMC.LT.1) THEN
         MSGTXT = 'NO CHANNELS SPECIFIED'
         GO TO 990
         END IF
      ACH = SUMC / NUMC
C                                       frequencies
      LUNO = LUNTMP (1)
C                                       get FQ settings first
      VER = 1
      CALL CHNDAT ('READ', BUFF, DISKIN, CNOIN, VER, CATBLK, LUNO,
     *   NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE FOR VELOCITIES'
         GO TO 990
         END IF
      LFREQ = CATD(KDCRV+JLOCF) + FOFF(BIF) + (ACH - CATR(KRCRP+JLOCF))
     *   * FINC(BIF)
      LFREQ = LFREQ * 1.D-9
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SETFRQ ERROR',I4,' ON ',A)
      END
      SUBROUTINE CHWANT (NC, NI, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NC       I            Number spectral chans
C      NI       I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   I(*,*)       1 => use, 0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NC, NI, CHNSEL(3,20,*), CHFLGS(NC,NI)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NC * NI
      CALL FILL (J, 0, CHFLGS)
      DO 30 K = 1,NI
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DATAVG (NC, NS, CHFLGS, VIS, AVIS)
C-----------------------------------------------------------------------
C   DATAVG averages a spectrum
C   Inputs:
C      NC       I      Number spectral channels
C      NS       I      Number Stokes
C      CHFLGS   I(*)   =1 -> use channel
C      VIS      R(3,*)   Input vis
C   Outputs:
C      AVIS     R(3,*)   Average vis
C-----------------------------------------------------------------------
      INTEGER   NC, NS, CHFLGS(*)
      REAL      VIS(3,*), AVIS(7,*)
C
      INTEGER   IS, IC, I
C-----------------------------------------------------------------------
      I = 7 * NS
      CALL RFILL (I, 0.0, AVIS)
      I = 1
      DO 20 IC = 1,NC
         IF (CHFLGS(IC).GT.0) THEN
            DO 10 IS = 1,NS
               IF (VIS(3,I).GT.0.0) THEN
                  AVIS(1,IS) = AVIS(1,IS) + VIS(3,I) * VIS(1,I)
                  AVIS(2,IS) = AVIS(2,IS) + VIS(3,I) * VIS(2,I)
                  AVIS(4,IS) = AVIS(4,IS) + VIS(3,I) * (VIS(1,I) ** 2)
                  AVIS(5,IS) = AVIS(5,IS) + VIS(3,I) * (VIS(2,I) ** 2)
                  AVIS(3,IS) = AVIS(3,IS) + VIS(3,I)
                  AVIS(6,IS) = AVIS(6,IS) + 1.0
C                 AVIS(7,IS) = AVIS(7,IS) + VIS(3,I) *
C    *               SQRT (VIS(1,I)*VIS(1,I) + VIS(2,I)*VIS(2,I))
                  END IF
               I = I + 1
 10            CONTINUE
         ELSE
            I = I + NS
            END IF
 20      CONTINUE
      DO 30 IS = 1,NS
         IF (AVIS(3,IS).GT.0.0) THEN
            AVIS(1,IS) = AVIS(1,IS) / AVIS(3,IS)
            AVIS(2,IS) = AVIS(2,IS) / AVIS(3,IS)
            AVIS(4,IS) = AVIS(4,IS) / AVIS(3,IS)
            AVIS(5,IS) = AVIS(5,IS) / AVIS(3,IS)
C            AVIS(7,IS) = AVIS(7,IS) / AVIS(3,IS)
            AVIS(7,IS) = SQRT (AVIS(1,IS)**2 + AVIS(2,IS)**2)
            END IF
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE UVHOSC (IRET)
C-----------------------------------------------------------------------
C   determines scaling parameters
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IROUND, K, IA1, IA2, PCOUNT, SOUID, OLDSOU,
     *   LBL, JERR, IUSER, JTT(3), JJTT(4), M, KBL, MBAS, MANT, LASTM,
     *   RPMM(2,10000), NOPTIO, NNBEG, NNEND, NSAMP , INVERT, I, ISAMP,
     *   KSAMP, MINSAM, NPOINT, NN
      LOGICAL   REQBAS, UVOPN, DOAVER, DOMAVE, DOTIME, FINISH
      REAL      AMP, WEIGHT, YAMP, U, V, W, XX, YY, RPARM(20), NSTIME,
     *   CATR(256), RPTIME(10000), RPUV(2,10000), VIS(5,4,10000), EPS,
     *   VDAT(28)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER   CHFLGS(MAXCHA)
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATBLK, CATR)
C-----------------------------------------------------------------------
C                                       init the range parameters
      EPS = 0.1 / (3600. * 24.)
      LASTM = -40001
      XAMP = 0.0
      UVM = 0.0
      XWT = 0.0
      NWT = 1.E10
      MSGTXT = 'Finding the scaling parameters to set formats'
      CALL MSGWRT (1)
      CALL CHWANT (ECHAN, 1, CHNSEL, CHFLGS)
C                                       Set pointers, counters
      LBL = 1
      KBL = 1
      MBAS = 1
      MANT = 1
      DOAVER = APARM(5).GT.0.0
      IF (DOAVER) MANT = NANT
      DOMAVE = APARM(8).GT.0.0
      IF (DOMAVE) MBAS = NBAS
      DOTIME = APARM(6).GT.0.0
      NOPTIO = APARM(1) + 0.1
      NNBEG = APARM(2) + 0.1
      NNEND = APARM(3) + 0.1
      MINSAM = XBDROP + 0.1
      NPOINT = XNPTS + 0.1
      IF (NPOINT.LE.0) NPOINT = 100
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
 20   UVOPN = .FALSE.
      OLDSOU = -1
      CALL COPY (MANT, IANT(KBL), ANTENS)
      CALL COPY (MBAS, IBAS(LBL), ANTENS(MANT+1))
C                                      first page
      IUSER = NLUSER
      INITVS = 1
C                                       Initialize reading VIS. file.
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1090) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      UVOPN = .TRUE.
      PCOUNT = XNCNT
      IF (OPER.NE.'HOLG') PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
C                                       zero summing arrays
      NSAMP = 0
      CALL RFILL (10000, 0.0, RPTIME)
      CALL RFILL (20000, 0.0, RPUV)
      CALL FILL (20000, 0, RPMM)
      CALL RFILL (200000, 0.0, VIS)
      NSTIME = -100.0
C                                       Start looping thru data.
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, BUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
      ELSE
         FINISH = IRET.EQ.-1
         IRET = 0
C                                       Check whether we need this
C                                       baseline
         IF (.NOT.FINISH) THEN
            IF (ILOCB.GE.0) THEN
               IA1 = INT (RPARM(ILOCB+1)) / 256
               IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), MANT,
     *         IBAS(LBL), MBAS)) GO TO 100
            INVERT = 1
            DO 110 K = 1,MBAS
               IF (IA1.EQ.IBAS(LBL+K-1)) INVERT = -1
 110           CONTINUE
            CALL DATAVG (ECHAN, NCOLS, CHFLGS, BUFF, VDAT)
            END IF
C                                       Include this count?
         IF (FINISH) THEN
            M = -40001
            SOUID = -2
         ELSE
            W = RPARM(ILOCW+1)
            M = W - WBASE + 0.1
            IF (M.LE.0) THEN
               LASTM = -10000
               GO TO 100
               END IF
            M = MOD (M, 256)
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            END IF
C                                       accumulate buffer
         IF ((M.EQ.LASTM) .AND. (SOUID.EQ.OLDSOU) .AND.
     *      (LQUAL.GE.40000)) THEN
            IF (RPARM(ILOCT+1)-NSTIME.GT.EPS) THEN
               NSAMP = NSAMP + 1
               NSTIME = RPARM(ILOCT+1)
               END IF
            RPTIME(NSAMP) = RPTIME(NSAMP) + RPARM(ILOCT+1)
            RPUV(1,NSAMP) = RPUV(1,NSAMP) + RPARM(ILOCU+1)
            RPUV(2,NSAMP) = RPUV(2,NSAMP) + RPARM(ILOCV+1)
            RPMM(1,NSAMP) = M
            RPMM(2,NSAMP) = RPMM(2,NSAMP) + 1
            I = 1
            DO 120 K = 1,NCOLS
               VIS(1,K,NSAMP) = VIS(1,K,NSAMP) + VDAT(I) * VDAT(I+2)
               VIS(2,K,NSAMP) = VIS(2,K,NSAMP) +
     *            INVERT * VDAT(I+1) * VDAT(I+2)
               VIS(3,K,NSAMP) = VIS(3,K,NSAMP) + VDAT(I+2)
               I = I + 7
 120           CONTINUE
C                                       test and restart buffer
         ELSE
            IF (NOPTIO.GT.0) THEN
               ISAMP = MIN (NSAMP-1, NNBEG) + 1
               KSAMP = MAX (1, NSAMP - NNEND)
               IF ((NSAMP.LT.MINSAM) .OR. (NSAMP.EQ.0)) KSAMP = -1
            ELSE
               ISAMP = MAX (0, NSAMP-NPOINT) + 1
               KSAMP = NSAMP
               IF (NSAMP.LT.MINSAM) KSAMP = -1
               END IF
            IF (LQUAL.LT.40000) KSAMP = -1
            IF ((DOTIME) .AND. (KSAMP.GT.ISAMP)) THEN
               DO 130 NN = ISAMP+1,KSAMP
                  RPTIME(ISAMP) = RPTIME(ISAMP) + RPTIME(NN)
                  RPUV(1,ISAMP) = RPUV(1,ISAMP) + RPUV(1,NN) * XUVFAC
                  RPUV(2,ISAMP) = RPUV(2,ISAMP) + RPUV(2,NN) * XUVFAC
                  RPMM(2,ISAMP) = RPMM(2,ISAMP) + RPMM(2,NN)
                  DO 125 K = 1,NCOLS
                     VIS(1,K,ISAMP) = VIS(1,K,ISAMP) + VIS(1,K,NN)
                     VIS(2,K,ISAMP) = VIS(2,K,ISAMP) + VIS(2,K,NN)
                     VIS(3,K,ISAMP) = VIS(3,K,ISAMP) + VIS(3,K,NN)
 125                 CONTINUE
 130              CONTINUE
               KSAMP = ISAMP
               END IF
            DO 150 NN = ISAMP,KSAMP
               U = RPUV(1,NN) / MAX (1, RPMM(2,NN))
               V = RPUV(2,NN) / MAX (1, RPMM(2,NN))
               M = RPMM(1,NN)
               IF (U.GT.0.0) THEN
                  UVM = MAX (UVM, U)
               ELSE
                  UVM = MAX (UVM, -10.0*U)
                  END IF
               IF (V.GT.0.0) THEN
                  UVM = MAX (UVM, V)
               ELSE
                  UVM = MAX (UVM, -10.0*V)
                  END IF
C                                       Get vis.
               DO 140 K = 1,NCOLS
                  WEIGHT = VIS(3,K,NN)
                  IF (WEIGHT.GT.0.0) THEN
                     XX = VIS(1,K,NN) / WEIGHT
                     YY = VIS(2,K,NN) / WEIGHT
                     XWT = MAX (XWT, WEIGHT)
                     NWT = MIN (NWT, WEIGHT)
                     AMP = SQRT (XX*XX + YY*YY)
                     XAMP = MAX (XAMP, AMP)
                     YAMP = MIN (YAMP, AMP)
                     END IF
 140              CONTINUE
 150           CONTINUE
C                                       Check source
            IF (.NOT.FINISH) THEN
               CALL SOURS (.FALSE., SOUID, OLDSOU, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 970
                  END IF
               NSAMP = 1
               W = RPARM(ILOCW+1)
               M = W - WBASE + 0.1
               M = MOD (M, 256)
               CALL RFILL (100, 0.0, RPTIME)
               CALL RFILL (200, 0.0, RPUV)
               CALL FILL (200, 0, RPMM)
               CALL RFILL (1200, 0.0, VIS)
               NSTIME = RPARM(ILOCT+1)
               RPTIME(NSAMP) = RPARM(ILOCT+1)
               RPUV(1,NSAMP) = RPARM(ILOCU+1) * XUVFAC
               RPUV(2,NSAMP) = RPARM(ILOCV+1) * XUVFAC
               RPMM(1,NSAMP) = M
               RPMM(2,NSAMP) = 1
               I = 1
               DO 155 K = 1,NCOLS
                  VIS(1,K,NSAMP) = VDAT(I) * VDAT(I+2)
                  VIS(2,K,NSAMP) = INVERT * VDAT(I+1) * VDAT(I+2)
                  VIS(3,K,NSAMP) = VDAT(I+2)
                  I = I + 7
 155              CONTINUE
               LASTM = M
               END IF
            END IF
         PCOUNT = PCOUNT + 1
         IF ((PCOUNT.LT.XNCNT) .AND. (.NOT.FINISH)) GO TO 100
         END IF
C                                       Close files.
 970  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      IF (IRET.EQ.0) THEN
         KBL = KBL + MANT
         IF ((KBL.LE.50) .AND. (IANT(KBL).GT.0)) GO TO 20
         KBL = 1
         END IF
      IF (IRET.EQ.0) THEN
         LBL = LBL + MBAS
         IF ((LBL.LE.50) .AND. (IBAS(LBL).GT.0)) GO TO 20
         END IF
      IF (IRET.LT.0) IRET = 0
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
      END
      SUBROUTINE UVHOCH (IRET)
C-----------------------------------------------------------------------
C   Prints selected portions of uv data file.
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   ICH, IROUND, I, K, IA1, IA2, PCOUNT, KBL, SOUID, OLDSOU,
     *   LBL, JERR, IUSER, JTT(3), JJTT(4), M, LASTM, NSAMP, ISAMP, NN,
     *   INVERT, KSAMP, LSTSOU, LSTANT, MANT, MBAS, SUMCNT, NCNT,
     *   NCOUNT, TTY(2), NNBEG, NNEND, MINSAM, NOPTIO, NPOINT
      CHARACTER STR*24
      LOGICAL   T, REQBAS, UVOPN, FINISH, DOAVER, DOMAVE, DOTIME
      REAL      W, RPARM(20), TADAY, CATR(256), EPS, NSTIME
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATBLK, CATR)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
      IF ((OPER.EQ.'HOLG') .OR. (DOCRT.GT.0.0) .OR. (LPNAME(1).NE.' '))
     *   GO TO 999
      LASTM = -40001
      NPOINT = XNPTS + 0.1
      IF (NPOINT.LE.0) NPOINT = 100
      MINSAM = XBDROP + 0.1
      NOPTIO = APARM(1) + 0.1
      NNBEG = APARM(2) + 0.1
      NNEND = APARM(3) + 0.1
      DOTIME = APARM(6).GT.0.0
      DOAVER = APARM(5).GT.0.0
      DOMAVE = APARM(8).GT.0.0
      MBAS = 1
      MANT = 1
      IF (DOAVER) MANT = NANT
      IF (DOMAVE) MBAS = NBAS
      LSTSOU = 0
      LSTANT = -1
      EPS = 0.1 / (3600. * 24.)
C                                       Set pointers, counters
      ICH = ACH + 0.5
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
      NCOUNT = 0
C                                       Regular printing
C                                       Holography loop point
 20   UVOPN = .FALSE.
      OLDSOU = -1
      CALL COPY (MANT, IANT(KBL), ANTENS(1))
      CALL COPY (MBAS, IBAS(LBL), ANTENS(MANT+1))
C                                       zero summing arrays
      NSAMP = 0
      NSTIME = -100.0
      SUMCNT = 0
      NCNT = 0
C                                      first page
      IPCNT = 998
      IUSER = NLUSER
      IF (DOCRT.GT.-2.5) THEN
         NCOUNT = NCOUNT + 4
C                                       UV scaling
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.4))) NCOUNT = NCOUNT + 1
         END IF
C                                       Page titles
C                                       Initialize reading VIS. file.
      INITVS = 1
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      UVOPN = .TRUE.
      PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, BUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
      ELSE
         FINISH = IRET.LT.0
         IRET = 0
C                                       Check whether we need this
C                                       baseline
         IF (.NOT.FINISH) THEN
            IF (ILOCB.GE.0) THEN
               IA1 = INT (RPARM(ILOCB+1)) / 256
               IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), MANT,
     *         IBAS(LBL), MBAS)) GO TO 100
            INVERT = 1
            DO 102 K = 1,MBAS
               IF (IA1.EQ.IBAS(LBL+K-1)) INVERT = -1
 102           CONTINUE
            END IF
C                                       Include this count?
         IF (FINISH) THEN
            M = -40001
            SOUID = -2
         ELSE
            W = RPARM(ILOCW+1)
            M = W - WBASE + 0.1
            IF (M.LE.0) THEN
               LASTM = -10000
               GO TO 100
               END IF
            M = MOD (M, 256)
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            END IF
C                                       accumulate buffer
         IF ((M.EQ.LASTM) .AND. (SOUID.EQ.OLDSOU) .AND.
     *      (LQUAL.GE.40000)) THEN
            IF (RPARM(ILOCT+1)-NSTIME.GT.EPS) THEN
               NSAMP = NSAMP + 1
               NSTIME = RPARM(ILOCT+1)
               END IF
C                                       print and restart buffer
         ELSE
            IF (NOPTIO.GT.0) THEN
               ISAMP = MIN (NSAMP-1, NNBEG) + 1
               KSAMP = MAX (1, NSAMP - NNEND)
               IF ((NSAMP.LT.MINSAM) .OR. (NSAMP.EQ.0)) KSAMP = -1
            ELSE
               ISAMP = MAX (0, NSAMP-NPOINT) + 1
               KSAMP = NSAMP
               IF (NSAMP.LT.MINSAM) KSAMP = -1
               END IF
            IF (LQUAL.LT.40000) KSAMP = -1
            IF ((DOTIME) .AND. (KSAMP.GT.ISAMP)) KSAMP = ISAMP
            DO 150 NN = ISAMP,KSAMP
               NCOUNT = NCOUNT + 1
               PCOUNT = PCOUNT + 1
               IF (ILOCB.GE.0) THEN
                  LSTANT = IROUND (RPARM(ILOCB+1)) / 256
               ELSE
                  LSTANT = RPARM(ILOCA1+1) + 0.1
                  END IF
               LSTSOU = OLDSOU
               IF(FINISH) THEN
                  LSTSOU = 0
                  LSTANT = -1
                  ENDIF
               IF (PCOUNT.GE.XNCNT) GO TO 970
 150           CONTINUE
C                                       Check source
            IF (.NOT.FINISH) THEN
               CALL SOURS (.FALSE., SOUID, OLDSOU, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 970
                  END IF
               NSAMP = 1
               W = RPARM(ILOCW+1)
               M = W - WBASE + 0.1
               M = MOD (M, 256)
               LASTM = M
               NSTIME = RPARM(ILOCT+1)
               END IF
            END IF
         IF (.NOT.FINISH) GO TO 100
         END IF
C                                       Close files.
 970  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      IF (IRET.EQ.0) THEN
         KBL = KBL + MANT
         IF ((KBL.LE.50) .AND. (IANT(KBL).GT.0)) GO TO 20
         KBL = 1
         END IF
      IF (IRET.EQ.0) THEN
         LBL = LBL + MBAS
         IF ((LBL.LE.50) .AND. (IBAS(LBL).GT.0)) GO TO 20
         END IF
      WRITE (MSGTXT,4000) NCOUNT
      CALL MSGWRT (8)
 4000 FORMAT ('CHECK EXPECTS NCOUNT=',I10)
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IRET = -1
            IPCNT = -1
            CALL LPCLOS (LUNP(1), FINDP(1), IPCNT, IRET)
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 975
         WRITE (SCRTCH,1970) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 975
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 975
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            IPCNT = -1
            CALL LPCLOS (LUNP(1), FINDP(1), IPCNT, IRET)
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
 975  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 1970 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE UVHODO (IRET)
C-----------------------------------------------------------------------
C   Prints selected portions of uv data file.
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   ICH, JJ, IROUND, I, K, IA1, IA2, PCOUNT, PHASE(4), KBL,
     *   IWT(4), SOUID, OLDSOU, ITT(4), JCOR, LBL, JERR, FLEN, JTRIM,
     *   IUSER, NWR(4), JTT(3), JJTT(4), M, WTFM, LASTM, RPMM(2,10000),
     *   NSAMP, NPOINT, ISAMP, MINSAM, NN, NNBEG, NNEND, INVERT, J,
     *   KSAMP, NOPTIO, ILEN, LUNTS(4), BUFFER(512), SULUN, IPH(4),
     *   LSTSOU, LSTANT, NXVER, LOOP, NXSOUR, NXSUB, NXVS, NXVE,
     *   NXFQI, INXLUN, MANT, MBAS, IAR, IAM, SUMCNT, NCNT, KK
      CHARACTER ISTOKE(4)*2, JSTOKE(4,3)*2, TCHAR*12, PREFIX*5, UVCH*4,
     *   TCHR1*13, STR*24, UVCC*5
      LOGICAL   T, REQBAS, FLAG, UVOPN, FINISH, DOELAZ, TABLE, EXIST,
     *   FITASC, NEW(500), DOTIME, DOAVER, DOMAVE
      REAL      AMP(4), WEIGHT, TPHS, U, V, W, XX, YY, RWT(4), XDAY,
     *   TEMP, RPHAS(4), TDAY, TIMS, RPARM(20), TADAY, WTSC, CATR(256),
     *   RPTIME(10000), RPUV(2,10000), VIS(7,4,10000), HA, EL, AZ,
     *   BEGSC(1000), ENDSC(1000), NXTIME, NXDTIM, EPS, NSTIME, RRE(4),
     *   RIM(4), SRE(4), SIM(4), SAM(4), SPH(4), RX, RY, RAMP, VDAT(28)
      DOUBLE PRECISION LSTIME, RA2DEG
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER   CHFLGS(MAXCHA), NCOUNT
      EQUIVALENCE (JJTT, JTT)
      EQUIVALENCE (CATBLK, CATR)
      DATA T /.TRUE./
      DATA JSTOKE /'XX','YY','XY','YX','RR','LL','RL','LR',
     *   'I_','Q_','U_','V_'/
      DATA LUNTS /3,11,10,91/
      DATA SULUN, INXLUN /17,18/
C-----------------------------------------------------------------------
      CALL CHWANT (ECHAN, 1, CHNSEL, CHFLGS)
      NCOUNT = 0
      DEVTAB(LUNTS(4)) = 3
      RA2DEG = 57.29577951
      LASTM = -40001
      NPOINT = XNPTS + 0.1
      IF (NPOINT.LE.0) NPOINT = 100
      MINSAM = XBDROP + 0.1
      NOPTIO = APARM(1) + 0.1
      NNBEG = APARM(2) + 0.1
      NNEND = APARM(3) + 0.1
      DOAVER = APARM(5).GT.0.0
      DOMAVE = APARM(8).GT.0.0
      DOTIME = APARM(6).GT.0.0
      MBAS = 1
      MANT = 1
      IF (DOAVER) MANT = NANT
      IF (DOMAVE) MBAS = NBAS
      LSTSOU = 0
      LSTANT = -1
      LSTIME = -1.D0
      DOELAZ = (APARM(4).GT.0.0) .AND. (OPER.EQ.'HOLG')
      EPS = 0.1 / (3600. * 24.)
C                                       Set pointers, counters
      ICH = ACH + 0.5
C                                       If get elevation and azimuth
C                                       run GETANT, and initalize
C                                       NX table
      IF (DOELAZ) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, BUFFER, IRET)
         CALL ISTAB ('NX', DISKIN, CNOIN, 1, INXLUN, BUFFER, TABLE,
     *      EXIST, FITASC, IRET)
         IF ((.NOT.EXIST) .OR. (IRET.NE.0)) THEN
            MSGTXT = 'ERROR IN ISTAB INITALIZING NX TABLE'
            IRET = 1
            GO TO 960
            END IF
         NXVER = 1
         CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      INXLUN, INXRNO, NXKOLS, NXNUMV, IRET)
         IF (.NOT.EXIST.OR.IRET.NE.0) THEN
            MSGTXT = 'ERROR IN NDXINI INITALIZING NX TABLE'
            IRET = 1
            GO TO 960
            END IF
C                                      Get number of index records
         NINDEX = NXBUFF(5)
         IF (NINDEX.GT.500)  THEN
            MSGTXT = 'NO MORE THAN 500 SCANS ALLOWED'
            CALL MSGWRT(8)
            MSGTXT = 'THE AIPS MANAGER COULD FIX THIS'
            CALL MSGWRT(8)
            GOTO 960
            END IF
C                                      Get begin and end scan times
         DO 10 LOOP = 1,NINDEX
            INXRNO = LOOP
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *         NXDTIM, NXSOUR, NXSUB, NXVS, NXVE, NXFQI, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1012) IRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       Convert time
            BEGSC(LOOP) = NXTIME - 0.5 * NXDTIM
            ENDSC(LOOP) = NXTIME + 0.5 * NXDTIM
 10         CONTINUE
         CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, IRET)
         END IF
C                                       Stokes labels
      JJ = 2
      IF (ICOR0.LT.-4) JJ = 1
      IF (DOCONV) JJ = 3
      DO 15 I = 1,NCOLS
         ISTOKE(I) = JSTOKE(I,JJ)
 15      CONTINUE
      IF (STOKES.EQ.'IV') ISTOKE(2) = 'V_'
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
      IF (OPER.NE.'HOLG') THEN
C                                       weight scaling
         TCHAR = ' Amp Phas Wt'
         TCHR1 = 'Amp  Phas  Wt'
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
     *            ' be printed'
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
     *            ' be printed'
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
      PAGE = 0
C                                       Holography loop point
 20   UVOPN = .FALSE.
      OLDSOU = -1
      CALL COPY (MANT, IANT(KBL), ANTENS(1))
      CALL COPY (MBAS, IBAS(LBL), ANTENS(MANT+1))
C                                       zero summing arrays
      NSAMP = 0
      CALL RFILL (10000, 0.0, RPTIME)
      CALL RFILL (20000, 0.0, RPUV)
      CALL FILL (20000, 0, RPMM)
      CALL RFILL (280000, 0.0, VIS)
      NSTIME = -100.0
C                                       Initalize NEW for DOELAZ
      CALL LFILL (500, .TRUE., NEW)
      SUMCNT = 0
      NCNT = 0
C                                       Holography
C                                       Append stokes to filename
      IF ((OPER.EQ.'HOLG') .AND. (KBL.EQ.1)) THEN
         DO 30 I = 1,NSTKS
            LPNAME(I) = ' '
            CALL H2CHR (48, 1, XLPNAM, LPNAME(I))
            FLEN = INDEX (LPNAME(I), ':')
            IF (FLEN.LE.0) THEN
               FLEN = JTRIM (LPNAME(I))
               LPNAME(I)(FLEN+1:) = ':'
               END IF
            ILEN = JTRIM (LPNAME(I))
            FLEN = INDEX (LPNAME(I), ':')
            IF (ILEN.LE.FLEN) THEN
               JJ = 0
               IF (IANT(2).LE.0) JJ = IANT(KBL)
               KK = IBAS(LBL)
               IF (MBAS.GT.1) KK = 0
               WRITE (STR,1015) KK, JJ, ISTOKE(I), BIF
               LPNAME(I)(FLEN+1:) = STR
            ELSE
               WRITE (STR,1016) ISTOKE(I), BIF
               LPNAME(I)(ILEN+1:) = STR
               END IF
C                                       Open 2 text files
            LUNP(I) = LUNTS(I)
            CALL ZTXOPN ('WRIT', LUNP(I), FINDP(I), LPNAME(I), T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1030) IRET
               CALL MSGWRT (8)
               GO TO 960
               END IF
            LPOPN(I) = .TRUE.
            NWR(I) = 0
 30         CONTINUE
         END IF
C                                       Holography comment lines
      IF (OPER.EQ.'HOLG') THEN
         DO 40 I = 1,NSTKS
            JJ = IANT(KBL)
            IF (MANT.GT.1) JJ = 100
            KK = IBAS(LBL)
            IF (MBAS.GT.1) KK = 100
            WRITE (LINE,1020) JJ, KK, ISTOKE(I), LFREQ
            CALL ZTXIO ('WRIT', LUNP(I), FINDP(I), LINE(1:JTRIM(LINE)),
     *         IRET)
            IF (IRET.NE.0) GO TO 960
            IF (MBAS.GT.1) THEN
               JJ = 1
 31            IF (JJ.LE.MBAS) THEN
                  K = MIN (MBAS, JJ+13)
                  WRITE (LINE,2020) (IBAS(LBL+IA1-1), IA1 = JJ,K)
                  CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *               LINE(1:JTRIM(LINE)), IRET)
                  IF (IRET.NE.0) GO TO 960
                  JJ = K + 1
                  GO TO 31
                  END IF
               END IF
            IF (NOPTIO.EQ.0) THEN
               WRITE (LINE,1021) MINSAM, NPOINT
               CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *            LINE(1:JTRIM(LINE)), IRET)
            ELSE
               WRITE (LINE,2021) MINSAM, NNBEG, NNEND
               CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *            LINE(1:JTRIM(LINE)), IRET)
               END IF
            IF (IRET.NE.0) GO TO 960
            WRITE (LINE,1022) BIF, ACH
            CALL ZTXIO ('WRIT', LUNP(I), FINDP(I), LINE(1:JTRIM(LINE)),
     *         IRET)
            IF (IRET.NE.0) GO TO 960
            TDAY = MAX (-99.0, TSTART)
            TIMS = MIN (999.0, TEND)
            CALL TODHMS (TDAY, ITT)
            CALL TODHMS (TIMS, JJTT)
            WRITE (LINE,1023) ITT, JJTT
            CALL ZTXIO ('WRIT', LUNP(I), FINDP(I), LINE(1:JTRIM(LINE)),
     *         IRET)
            IF (IRET.NE.0) GO TO 960
            IF (MANT.GT.1) THEN
               JJ = 1
 35            IF (JJ.LE.MANT) THEN
                  K = MIN (MANT, JJ+13)
                  WRITE (LINE,1040) (IANT(IA1), IA1 = JJ,K)
                  CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *               LINE(1:JTRIM(LINE)), IRET)
                  IF (IRET.NE.0) GO TO 960
                  JJ = K + 1
                  GO TO 35
                  END IF
               END IF
            WRITE (LINE,1041) DOCAL, DOPOL
            CALL ZTXIO ('WRIT', LUNP(I), FINDP(I), LINE(1:JTRIM(LINE)),
     *         IRET)
            IF (IRET.NE.0) GO TO 960
            DO 38 J = 1,20
               IF ((CHNSEL(1,J).GT.0) .AND. (CHNSEL(2,J).GT.0) .AND.
     *            (CHNSEL(2,J).GE.CHNSEL(1,J))) THEN
                  WRITE (LINE,1042) CHNSEL(1,J), CHNSEL(2,J),
     *               CHNSEL(3,J)
                  CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *               LINE(1:JTRIM(LINE)), IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
 38            CONTINUE
 40         CONTINUE
         END IF
C                                      UV scaling
      TEMP = 1.01 / XUVFAC
      CALL METSCA (TEMP, PREFIX, FLAG)
      IF (ABS(TEMP-1.0).LT.0.02) THEN
         UVCH = PREFIX(:4)
         IF (UVCH.EQ.' ') UVCH = 'ASin'
         UVCC = PREFIX
         IF (UVCC.EQ.' ') UVCC = 'ASin'
      ELSE
         UVCH = 'ASin'
         UVCC = 'ASin'
         END IF
C                                      first page
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IUSER = NLUSER
      IF ((OPER.NE.'HOLG') .AND. (DOCRT.GT.-2.5)) THEN
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1050) NAMEIN, CLAIN, SEQIN, DISKIN, IUSER,
     *         ACH, BIF
         ELSE
            WRITE (LINE,1051) NAMEIN, CLAIN, SEQIN, DISKIN, IUSER,
     *         ACH, BIF
            END IF
         CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         WRITE (LINE,1055) LFREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         NCOUNT = NCOUNT + 2
C                                       UV scaling
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.4))) THEN
            LINE = 'l and m, not w, are in ' // PREFIX //
     *         '''s of asin(l or m)'
            CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *         TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 960
            NCOUNT = NCOUNT + 1
            END IF
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1057) WTSC
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *        IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 960
            NCOUNT = NCOUNT + 1
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         NCOUNT = NCOUNT + 1
         END IF
C                                       Page titles
      IF (OPER.NE.'HOLG') THEN
         IF (OTYPE.EQ.1) THEN
            WRITE (TITL1,1061) SOURCE, LFREQ, ISORT, (ICH, ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1071) UVCC, UVCC, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.2) THEN
            WRITE (TITL1,1062) SOURCE, LFREQ, ISORT, (ICH, ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1072) UVCH, UVCH, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.3) THEN
            WRITE (TITL1,1063) SOURCE, LFREQ, ISORT, (ICH, ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1073) UVCH, UVCH, (TCHR1, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.4) THEN
            WRITE (TITL1,1064) SOURCE, LFREQ, ISORT, (ICH, ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1074) UVCH, UVCH, (TCHAR, JCOR = 1,NCOLS)
         ELSE IF (OTYPE.EQ.5) THEN
            WRITE (TITL1,1065) SOURCE, LFREQ, (ICH, ISTOKE(JCOR),
     *         JCOR = 1,NCOLS)
            WRITE (TITL2,1075) (TCHAR, JCOR = 1,NCOLS)
            END IF
         END IF
C                                       Initialize reading VIS. file.
      INITVS = 1
      CALL UVGET ('INIT', RPARM, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1090) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
         END IF
      UVOPN = .TRUE.
      PCOUNT = 0
      SOUID = 1
      IF (NSOUWD.EQ.1) SOUID = SOUWAN(1)
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, BUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         CALL MSGWRT (8)
         IRET = 4
         GO TO 970
      ELSE
         FINISH = IRET.LT.0
         IRET = 0
C                                       Check whether we need this
C                                       baseline
         IF (.NOT.FINISH) THEN
            IF (ILOCB.GE.0) THEN
               IA1 = INT (RPARM(ILOCB+1)) / 256
               IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT(KBL), MANT,
     *         IBAS(LBL), MBAS)) GO TO 100
            INVERT = 1
            DO 102 K = 1,MBAS
               IF (IA1.EQ.IBAS(LBL+K-1)) INVERT = -1
 102           CONTINUE
            CALL DATAVG (ECHAN, NCOLS, CHFLGS, BUFF, VDAT)
            END IF
C                                       Include this count?
         IF (FINISH) THEN
            M = -40001
            SOUID = -2
         ELSE
            W = RPARM(ILOCW+1)
            M = W - WBASE + 0.1
            IF (M.LE.0) THEN
               LASTM = -10000
               GO TO 100
               END IF
            M = MOD (M, 256)
            IF (ILOCSU.GE.0) SOUID = IROUND (RPARM(ILOCSU+1))
            END IF
C                                       accumulate buffer
         IF ((M.EQ.LASTM) .AND. (SOUID.EQ.OLDSOU) .AND.
     *      (LQUAL.GE.40000)) THEN
            IF (RPARM(ILOCT+1)-NSTIME.GT.EPS) THEN
               NSAMP = NSAMP + 1
               NSTIME = RPARM(ILOCT+1)
               END IF
            RPTIME(NSAMP) = RPTIME(NSAMP) + RPARM(ILOCT+1)
            RPUV(1,NSAMP) = RPUV(1,NSAMP) + RPARM(ILOCU+1) * XUVFAC
            RPUV(2,NSAMP) = RPUV(2,NSAMP) + RPARM(ILOCV+1) * XUVFAC
            RPMM(1,NSAMP) = M
            RPMM(2,NSAMP) = RPMM(2,NSAMP) + 1
            I = 1
            DO 105 K = 1,NCOLS
               VIS(1,K,NSAMP) = VIS(1,K,NSAMP) + VDAT(I) * VDAT(I+2)
               VIS(2,K,NSAMP) = VIS(2,K,NSAMP) +
     *            INVERT * VDAT(I+1) * VDAT(I+2)
               VIS(3,K,NSAMP) = VIS(3,K,NSAMP) + VDAT(I+2)
               VIS(4,K,NSAMP) = VIS(4,K,NSAMP) + VDAT(I+3) * VDAT(I+2)
               VIS(5,K,NSAMP) = VIS(5,K,NSAMP) + VDAT(I+4) * VDAT(I+2)
               VIS(6,K,NSAMP) = VIS(6,K,NSAMP) + VDAT(I+5)
               VIS(7,K,NSAMP) = VIS(7,K,NSAMP) + VDAT(I+6) * VDAT(I+2)
               I = I + 7
 105           CONTINUE
C                                       print and restart buffer
         ELSE
            IF (NOPTIO.GT.0) THEN
               ISAMP = MIN (NSAMP-1, NNBEG) + 1
               KSAMP = MAX (1, NSAMP - NNEND)
               IF ((NSAMP.LT.MINSAM) .OR. (NSAMP.EQ.0)) KSAMP = -1
            ELSE
               ISAMP = MAX (0, NSAMP-NPOINT) + 1
               KSAMP = NSAMP
               IF (NSAMP.LT.MINSAM) KSAMP = -1
               END IF
            IF (LQUAL.LT.40000) KSAMP = -1
            IF ((DOTIME) .AND. (KSAMP.GT.ISAMP)) THEN
               DO 110 NN = ISAMP+1,KSAMP
                  RPTIME(ISAMP) = RPTIME(ISAMP) + RPTIME(NN)
                  RPUV(1,ISAMP) = RPUV(1,ISAMP) + RPUV(1,NN)
                  RPUV(2,ISAMP) = RPUV(2,ISAMP) + RPUV(2,NN)
                  RPMM(2,ISAMP) = RPMM(2,ISAMP) + RPMM(2,NN)
                  DO 109 K = 1,NCOLS
                     VIS(1,K,ISAMP) = VIS(1,K,ISAMP) + VIS(1,K,NN)
                     VIS(2,K,ISAMP) = VIS(2,K,ISAMP) + VIS(2,K,NN)
                     VIS(3,K,ISAMP) = VIS(3,K,ISAMP) + VIS(3,K,NN)
                     VIS(4,K,ISAMP) = VIS(4,K,ISAMP) + VIS(4,K,NN)
                     VIS(5,K,ISAMP) = VIS(5,K,ISAMP) + VIS(5,K,NN)
                     VIS(6,K,ISAMP) = VIS(6,K,ISAMP) + VIS(6,K,NN)
                     VIS(7,K,ISAMP) = VIS(7,K,ISAMP) + VIS(7,K,NN)
 109                 CONTINUE
 110              CONTINUE
               KSAMP = ISAMP
               END IF
            DO 150 NN = ISAMP,KSAMP
               XDAY = RPTIME(NN) / MAX (1, RPMM(2,NN))
               CALL T2DHMS (0, XDAY, ITT, TIMS)
               CALL T2DHMS (1, XDAY, JJTT, TIMS)
C                                       Scale uvw as user desires
               U = RPUV(1,NN) / MAX (1, RPMM(2,NN))
               V = RPUV(2,NN) / MAX (1, RPMM(2,NN))
               M = RPMM(1,NN)
               SUMCNT = SUMCNT + RPMM(2,NN)
               NCNT = NCNT + 1
C                                       Get vis.
               DO 115 K = 1,NCOLS
                  WEIGHT = VIS(3,K,NN)
                  XX = VIS(1,K,NN) / MAX (1.E-8, WEIGHT)
                  YY = VIS(2,K,NN) / MAX (1.E-8, WEIGHT)
                  RX = VIS(4,K,NN) / MAX (1.E-8, WEIGHT)
                  RY = VIS(5,K,NN) / MAX (1.E-8, WEIGHT)
                  RAMP = VIS(7,K,NN) / MAX (1.E-8, WEIGHT)
                  RX = RX - XX * XX
                  RY = RY - YY * YY
                  RX = SQRT (MAX (0.0, RX))
                  RY = SQRT (MAX (0.0, RY))
                  RRE(K) = XX
                  RIM(K) = YY
                  SRE(K) = RX / SQRT (MAX (1.0, VIS(6,K,NN)-1.0))
                  SIM(K) = RY / SQRT (MAX (1.0, VIS(6,K,NN)-1.0))
                  TPHS = WEIGHT * WTSC
                  RWT(K) = TPHS
                  IWT(K) = IROUND (TPHS)
                  IF (IWT(K).EQ.0) THEN
                     IF (TPHS.LT.0.0) IWT(K) = -1
                     IF (TPHS.GT.0.0) IWT(K) = 1
                     END IF
                  IF ((IA1.NE.IA2) .OR. (ISCROS(K))) THEN
                     AMP(K) = SQRT (XX*XX+YY*YY)
                     TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                     SAM(K) = 0.0
                     SPH(K) = 0.0
                     IF (AMP(K).GT.0.0) THEN
                        SAM(K) = SQRT ((XX*RX/AMP(K))**2 +
     *                     (YY*RY/AMP(K))**2)
     *                     / SQRT (MAX (1.0, VIS(6,K,NN)-1.0))
                        SPH(K) = 57.296 * SQRT ((XX*RY)**2 + (YY*RX)**2)
     *                     / (XX*XX + YY*YY)
     *                     / SQRT (MAX (1.0, VIS(6,K,NN)-1.0))
                        END IF
                     IF (APARM(9).GT.0.0) THEN
                        RRE(K) = RRE(K) / AMP(K) * RAMP
                        RIM(K) = RIM(K) / AMP(K) * RAMP
                        SRE(K) = SRE(K) / AMP(K) * RAMP
                        SIM(K) = SIM(K) / AMP(K) * RAMP
                        SAM(K) = SAM(K) / AMP(K) * RAMP
                        AMP(K) = RAMP
                        END IF
                  ELSE
                     AMP(K) = XX
                     TPHS = 0.0
                     END IF
                  RPHAS(K) = TPHS
                  PHASE(K) = IROUND (TPHS)
                  IPH(K) = IROUND (SPH(K))
 115              CONTINUE
C                                       Write VIS data
C                                       Holography viscera
               IF (OPER.EQ.'HOLG') THEN
C                                       Print beg/end time, el and az
                  IF (DOELAZ) THEN
                     DO 120 LOOP = 1, NINDEX
                        IF (RPTIME(NN).GE.BEGSC(LOOP).AND.
     *                     NEW(LOOP).AND.
     *                     RPTIME(NN).LE.ENDSC(LOOP)) THEN
                           IF (LSTANT.GT.0.AND.LSTIME.GT.0.0) THEN
                              CALL GETSOU (LSTSOU, DISKIN, CNOIN, CATUV,
     *                           SULUN, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1013) IRET
                                 CALL MSGWRT (8)
                                 GO TO 999
                                 END IF
                              CALL SOUELV (LSTANT, LSTIME, HA, EL, AZ)
                              DO 118 I = 1,NSTKS
                                 WRITE (LINE,1166) LSTIME, EL*RA2DEG,
     *                              AZ*RA2DEG
                                 CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *                              LINE(1:JTRIM(LINE)), IRET)
                                 IF (IRET.NE.0) GO TO 960
 118                             CONTINUE
                              END IF
                           NEW(LOOP)=.FALSE.
                           CALL GETSOU (OLDSOU, DISKIN, CNOIN, CATUV,
     *                        SULUN, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1013) IRET
                              CALL MSGWRT (8)
                              GO TO 999
                              END IF
                           CALL SOUELV (IA1, DBLE(RPTIME(NN)), HA, EL,
     *                        AZ)
                           DO 119 I = 1,NSTKS
                              WRITE (LINE,1165) RPTIME(NN), EL*RA2DEG,
     *                           AZ*RA2DEG
                              CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *                           LINE(1:JTRIM(LINE)), IRET)
                              IF (IRET.NE.0) GO TO 960
 119                          CONTINUE
                           END IF
 120                    CONTINUE
                     END IF
C                                       Write to file
                  DO 125 I = 1,NSTKS
                     IF (RWT(I).GT.0) THEN
                        IF (APARM(7).GT.0.0) THEN
                           WRITE (LINE,1160,ERR=145) U, V, AMP(I),
     *                        RPHAS(I), SAM(I), SPH(I)
                        ELSE
                           WRITE (LINE,1160,ERR=145) U, V, RRE(I),
     *                        RIM(I), SRE(I), SIM(I)
                           END IF
                        CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *                     LINE(1:JTRIM(LINE)), IRET)
                        IF (IRET.NE.0) GO TO 960
                        NWR(I) = NWR(I) + 1
                        END IF
 125                 CONTINUE
                  IF ((DOELAZ) .AND. (FINISH) .AND. (NN.EQ.KSAMP)) THEN
                     CALL GETSOU (OLDSOU, DISKIN, CNOIN, CATUV, SULUN,
     *                  IRET)
                     IF(IRET.NE.0) THEN
                        WRITE (MSGTXT,1013) IRET
                        CALL MSGWRT (8)
                        GO TO 999
                        END IF
                     CALL SOUELV (IA1, DBLE(RPTIME(NN)), HA, EL, AZ)
                     DO 130 I = 1,NSTKS
                        WRITE (LINE,1166) RPTIME(NN), EL*RA2DEG,
     *                     AZ*RA2DEG
                        CALL ZTXIO ('WRIT', LUNP(I), FINDP(I),
     *                     LINE(1:JTRIM(LINE)), IRET)
                        IF (IRET.NE.0) GO TO 960
 130                    CONTINUE
                     END IF
C                                       Printing
               ELSE
                  IAR = IANT(KBL)
                  IF (MANT.GT.1) IAR = 100
                  IAM = IBAS(LBL)
                  IF (MBAS.GT.1) IAM = 100
                  IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.1)) THEN
                     WRITE (LINE,2101,ERR=140) JTT, TIMS, IAR, IAM, U,
     *                  V, M, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.2)) THEN
                     WRITE (LINE,2102,ERR=140) JTT, TIMS, IAR, IAM, U,
     *                  V, M, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.3)) THEN
                     WRITE (LINE,2103,ERR=140) JTT, TIMS, IAR, IAM, U,
     *                  V, M,(AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE IF ((OTYPE.EQ.1) .AND. (WTFM.EQ.4)) THEN
                     WRITE (LINE,2104,ERR=140) JTT, TIMS, IAR, IAM, U,
     *                  V, M, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                  ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.1)) THEN
                     IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2201,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                     ELSE
                        WRITE (LINE,2211,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                        END IF
                  ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.2)) THEN
                     IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2202,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                     ELSE
                        WRITE (LINE,2212,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                        END IF
                  ELSE IF ((OTYPE.EQ.2) .AND. (WTFM.EQ.3)) THEN
                    IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2203,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                     ELSE
                        WRITE (LINE,2213,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, M, (AMP(K), PHASE(K), RWT(K), K =
     *                     1,NCOLS)
                        END IF
                  ELSE IF ((OTYPE.EQ.3) .AND. (WTFM.EQ.1)) THEN
                     IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2301,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2311,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE IF ((OTYPE.EQ.3) .AND. (WTFM.EQ.2)) THEN
                     IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2302,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2312,ERR=140) JTT, TIMS, IAR, IAM,
     *                     U, V, (AMP(K), PHASE(K), RWT(K), K = 1,NCOLS)
                        END IF
                  ELSE IF (OTYPE.EQ.4) THEN
                     IF (UVM.LE.9999.98) THEN
                        WRITE (LINE,2400,ERR=140) ITT, IAR, IAM, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                     ELSE
                        WRITE (LINE,2410,ERR=140) ITT, IAR, IAM, U, V,
     *                     (AMP(K), PHASE(K), IWT(K), K = 1,NCOLS)
                        END IF
                  ELSE IF (OTYPE.EQ.5) THEN
                     IF (XAMP.LE.99.9) THEN
                        WRITE (LINE,2500,ERR=140) JTT(2), JTT(3), TIMS,
     *                     IAR, IAM, (AMP(K), PHASE(K), IWT(K), K =
     *                     1,NCOLS)
                     ELSE
                        WRITE (LINE,2510,ERR=140) JTT(2), JTT(3), TIMS,
     *                     IAR, IAM, (AMP(K), PHASE(K), IWT(K), K =
     *                     1,NCOLS)
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
 140              CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 960
                  NCOUNT = NCOUNT + 1
                  END IF
 145           PCOUNT = PCOUNT + 1
               LSTIME = DBLE(RPTIME(NN))
               IF (ILOCB.GE.0) THEN
                  LSTANT = IROUND (RPARM(ILOCB+1)) / 256
               ELSE
                  LSTANT = RPARM(ILOCA1+1) + 0.1
                  END IF
               LSTSOU = OLDSOU
               IF(FINISH) THEN
                  LSTSOU = 0
                  LSTANT = -1
                  LSTIME = -1.D0
                  ENDIF
               IF (PCOUNT.GE.XNCNT) GO TO 970
 150           CONTINUE
C                                       Check source
            IF (.NOT.FINISH) THEN
               CALL SOURS (.TRUE., SOUID, OLDSOU, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 970
                  END IF
               NSAMP = 1
               W = RPARM(ILOCW+1)
               M = W - WBASE + 0.1
               M = MOD (M, 256)
               CALL RFILL (10000, 0.0, RPTIME)
               CALL RFILL (20000, 0.0, RPUV)
               CALL FILL (20000, 0, RPMM)
               CALL RFILL (280000, 0.0, VIS)
               NSTIME = RPARM(ILOCT+1)
               RPTIME(NSAMP) = RPARM(ILOCT+1)
               RPUV(1,NSAMP) = RPARM(ILOCU+1) * XUVFAC
               RPUV(2,NSAMP) = RPARM(ILOCV+1) * XUVFAC
               RPMM(1,NSAMP) = M
               RPMM(2,NSAMP) = 1
               I = 1
               DO 155 K = 1,NCOLS
                  VIS(1,K,NSAMP) = VDAT(I) * VDAT(I+2)
                  VIS(2,K,NSAMP) = INVERT * VDAT(I+1) * VDAT(I+2)
                  VIS(3,K,NSAMP) = VDAT(I+2)
                  VIS(4,K,NSAMP) = VDAT(I+3) * VDAT(I+2)
                  VIS(5,K,NSAMP) = VDAT(I+4) * VDAT(I+2)
                  VIS(6,K,NSAMP) = VDAT(I+5)
                  VIS(7,K,NSAMP) = VDAT(I+6) * VDAT(I+2)
                  I = I + 7
 155              CONTINUE
               LASTM = M
               END IF
            END IF
         IF (.NOT.FINISH) GO TO 100
         END IF
C                                       CRT error
 960  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1960) IRET
         CALL MSGWRT (8)
         END IF
C                                       Close files.
 970  IF (UVOPN) CALL UVGET ('CLOS', RPARM, BUFF, JERR)
      IF (IRET.EQ.0) THEN
         KBL = KBL + MANT
         IF ((KBL.LE.50) .AND. (IANT(KBL).GT.0)) GO TO 20
         KBL = 1
         END IF
      WRITE (MSGTXT,4000) NCOUNT
      CALL MSGWRT (8)
 4000 FORMAT ('PRINT EXPECTS NCOUNT=',I10)
      IF (OPER.EQ.'HOLG') THEN
         WEIGHT = SUMCNT
         IF (NCNT.GT.0) WEIGHT = WEIGHT / NCNT
         WRITE (LINE,1970) WEIGHT
         DO 975 I = 1,NCOLS
            CALL ZTXIO ('WRIT', LUNP(I), FINDP(I), LINE(1:JTRIM(LINE)),
     *         JERR)
            IF (LPOPN(I)) CALL ZTXCLS (LUNP(I), FINDP(I), JERR)
            LPOPN(I) = .FALSE.
 975        CONTINUE
         LPOPN(2) = .FALSE.
         LPOPN(3) = .FALSE.
         LPOPN(4) = .FALSE.
         WRITE (MSGTXT,1975) NWR(1), ISTOKE(1), NWR(2), ISTOKE(2)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1975) NWR(3), ISTOKE(3), NWR(4), ISTOKE(4)
         IF (NSTKS.GT.2) CALL MSGWRT (5)
         END IF
      IF (IRET.EQ.0) THEN
         LBL = LBL + MBAS
         IF ((LBL.LE.50) .AND. (IBAS(LBL).GT.0)) GO TO 20
         END IF
      IF (IRET.LT.0) IRET = 0
      IF ((OPER.NE.'HOLG') .AND. (LPOPN(1))) CALL LPCLOS (LUNP(1),
     *   FINDP(1), IPCNT, JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1012 FORMAT ('ERROR',I5,' filling antenna common for el and az calc')
 1013 FORMAT ('ERROR',I5,' filling source common for el and az calc')
 1015 FORMAT ('HOLO',I2.2,'-',I2.2,A2,I2.2)
 1016 FORMAT (A2,I2.2)
 1020 FORMAT ('#! RefAnt = ',I2.2,' Antenna = ',I2.2,' Stokes = ''',
     *   A2,''' Freq =',F13.9)
 2020 FORMAT ('#! Averaged antennas =',14(I3,','))
 1021 FORMAT ('#! MINsamp =',I4,'  Npoint =',I4)
 2021 FORMAT ('#! MINsamp =',I4, ';', I3,',', I3,
     *   ' samples dropped at the beginning and at the end')
 1022 FORMAT ('#! IFnumber =',I4,'   Channel =',F8.1)
 1023 FORMAT ('#! TimeRange =',I4,3(',',I3),',',I5,3(',',I3))
 1030 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
 1040 FORMAT ('#! Averaged Ref-Ants =',14(I3,','))
 1041 FORMAT ('#! DOCAL = ',L1,'  DOPOL =',I2)
 1042 FORMAT ('#! BCHAN=',I6,' ECHAN=',I6,' CHINC=',I3,' averaged')
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5,5X,'Channel =',F7.1,3X,'IF =',I3)
 1051 FORMAT (A12,'.',A6,'.',I4,' Vol=',I2,' User=',I5,'  Channel=',
     *   F7.1,'  IF=',I3)
 1055 FORMAT ('Ref freq=',F13.9,' GHz  Ncor=',I3,'  No. vis=',I10,
     *   '  Sort order= ',A2)
 1057 FORMAT ('Weights have been multiplied by',1PE12.4)
 1061 FORMAT ('Source= ',A8,3X,'Freq= ',F13.9,3X,'Sort= ',A2,1X,
     *   4(I4,3X,A2,6X,4X))
 1062 FORMAT (A8,4X,'Freq=',F13.9,4X,'Sort= ',A2,3X,4(I4,3X,A2,9X))
 1063 FORMAT (A8,2X,'Freq=',F13.9,2X,'Sort= ',A2,1X,4(2X,I4,2X,A2,7X))
 1064 FORMAT (A8,1X,'Freq=',F13.9,1X,'Sort= ',A2,1X,4(2X,I4,2X,A2,5X))
 1065 FORMAT (A8,F8.4,4(4X,I4,1X,A2,3X))
 1071 FORMAT (4X,'Time',7X,'Ant    l(',A,')   m(',A,')    w',4(3X,A,3X))
 1072 FORMAT (4X,'Time',7X,'Ant   l(',A,')  m(',A,')   w',4(3X,A,2X))
 1073 FORMAT (4X,'Time',7X,'Ant   l(',A,')  m(',A,')',4(3X,A,1X))
 1074 FORMAT (3X,'Time',6X,'Ant   l(',A,')  m(',A,')',4(3X,A))
 1075 FORMAT (3X,'Time',5X,'Ant ',4(2X,A))
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 2101 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F11.2,
     *   I5,4(F8.3,I4,F7.1))
 2102 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F11.2,
     *   I5,4(F8.3,I4,F7.2))
 2103 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F11.2,
     *   I5,4(F8.3,I4,F7.3))
 2104 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F11.2,
     *   I5,4(F8.3,I4,F7.4))
 2201 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.2,
     *   I4,4(F8.3,I4,F6.1))
 2202 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.2,
     *   I4,4(F8.3,I4,F6.2))
 2203 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.2,
     *   I4,4(F8.3,I4,F6.3))
 2211 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.0,
     *   I4,4(F8.3,I4,F6.1))
 2212 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.0,
     *   I4,4(F8.3,I4,F6.2))
 2213 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.0,
     *   I4,4(F8.3,I4,F6.3))
 2301 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.2,
     *   4(F8.3,I4,F5.1))
 2302 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.2,
     *   4(F8.3,I4,F5.2))
 2311 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.0,
     *   4(F8.3,I4,F5.1))
 2312 FORMAT (I2,'/',I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,2F9.0,
     *   4(F8.3,I4,F5.2))
 2400 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,1X,I2.2,'-',I2.2,2F9.2,
     *   4(F8.3,I4,I3))
 2410 FORMAT (I2,'/',I2.2,':',I2.2,':',I2.2,1X,I2.2,'-',I2.2,2F9.0,
     *   4(F8.3,I4,I3))
 2500 FORMAT (I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,4(F7.3,I4,I3))
 2510 FORMAT (I2.2,':',I2.2,':',F4.1,1X,I2.2,'-',I2.2,4(F7.1,I4,I3))
 1160 FORMAT (6F15.7)
 1165 FORMAT ('#! Time_beg =',F9.6,' El_beg=',F9.4,' Az_beg=',F9.4)
 1166 FORMAT ('#! Time_end =',F9.6,' El_end=',F9.4,' Az_end=',F9.4)
 1960 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL, PRINTER, OR FILE')
 1970 FORMAT ('#! Average number samples per point =',F8.3)
 1975 FORMAT ('Wrote',I8,1X,A2,' and',I8,1X,A2,' vis to text files')
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
      INCLUDE 'UVHOL.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
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
         SOURCE = SNAME(:8)
         RA = RAEPO / DG2RAD
         DEC = DECEPO / DG2RAD
         LQUAL = QUAL
         IF (WBASE.EQ.0.0) LQUAL = QUAL + 40000
         END IF
C                                       Change source name in TITL1
      IF ((DOPRT) .AND. (LQUAL.GE.40000)) THEN
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
         IF (OPER.NE.'HOLG') THEN
            IF ((OLDSOU.GE.0) .AND. (DOCRT.GT.-2.5)) THEN
               LINE = ' '
               CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *            TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
            ELSE IF ((OLDSOU.LT.0) .AND. (DOCRT.LE.-2.5)) THEN
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            TITL1, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               END IF
C                                       Title for subsequent new source.
            IF ((OLDSOU.LT.0) .OR. (SOURCE.NE.OLDNAM) .OR.
     *         (OLDQ.NE.LQUAL)) THEN
               ILQUAL = MAX (-9999, MIN (99999, QUAL))
               WRITE (LINE,1055) SOURCE, ILQUAL, LLCH, CHSIG1, HM, RSEC,
     *            MMCH, CHSIG2, DD, DSEC
               CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *            TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               IF ((LPAGE.GE.PAGE) .AND. (DOCRT.GT.-2.5)) THEN
                  CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *               TITL2, TITL1, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  END IF
               IF ((LPAGE.GE.PAGE) .AND. (DOCRT.GT.-2.5)) THEN
                  CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *               TITL2, TITL2, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 950
                  END IF
               END IF
C                                       Blank line after first
            IF ((OLDSOU.LT.0) .AND. (DOCRT.GT.-2.5)) THEN
               LINE = ' '
               CALL PRTLIN (LUNP(1), FINDP(1), DOCRT, NACROS, TITL1,
     *            TITL2, LINE, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 950
               END IF
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
      SUBROUTINE DATPOL (IA1, IA2, TIME, VIS, IERR)
C-----------------------------------------------------------------------
C   LOCAL VERSION - DOES NOT ROTATE BY PARALLACTIC ANGLE
C
C   Applies polarization correction to data.  This operation is only
C   really defined if KNCOR = 4.  However, it will also apply the
C   parallel hand corrections if KNCOR = 2.
C   Inputs:
C      IA1     I      First antenna number
C      IA2     I      Second antenna number
C      TIME    R      Time in days; used for parallactic angle.
C   Inputs from common in DSEL.INC
C      PARTIM  R      Time of current parallactic angles.
C      PARAGL  R(2,*) Cos and sin of the parallactic angles of
C                     antennas.
C      PARSOU  I      Source ID for current parallactic angles.
C      IFR     R(*)   Ionospheric Faraday RM for each antenna
C      LAMBDA  R(*)   Wavelength of each channel and IF
C      STNPST  C*8    Polarization model type NOW IGNORED
C      SOLTYP  I      Model type numeric code NOW USED (from POLSET)
C      POLCAL  R(2,*) Polarization correction
C                     Values in order:
C                     By baseline
C                        By IF (EIF-BIF+1)
C                           A 4x4 complex matrix to be multiplied by
C                               the observed polarization vector
C                               (RR,LL,RL,LR or XX,YY,XY,YX) to produce
C                                the corrected data.
C                    Indexing scheme: an entry defined by ant1<ant2
C                    starts in element:
C      (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C   Input/output:
C      VIS     R(*)  Input visibility array in form of "RR,LL,LR,RL"
C              or "XX,YY,XY,YY"  on output = "RR,LL,RL,LR"
C   NOTE: Uses AIPS LUN 49.
C     Output:
C      IERR         I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, IERR
      REAL      TIME, VIS(*)
C
      INTEGER   IIF, IPOL, IFQ, I, LUN, INCPX, IOFF, JOFF, INDEX, JNDEX,
     *   BLNDX, LIMIT, LENTRY, BLPNT, IPNT, PNT(8), OFF(32), OFF2(8),
     *   STORDR, STINC, LOFF, JRL, JLR
      REAL      GR, GI, TR, TI, VTEMP(2,16), XTEMP(2,16), VTEMPX(32),
     *   DPANG, GR1, GI1, TINC, PAINC
      LOGICAL   DOORI, WFLAG
      INCLUDE 'INCS:PUVD.INC'
      REAL      PANGLE(MAXANT), PANGO(MAXANT)
      LONGINT   PP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DPDC.INC'
      EQUIVALENCE (VTEMP, VTEMPX)
C
      SAVE OFF, OFF2, PANGLE, PANGO, DOORI, INCPX, STINC
C
      DATA PNT /1,2,1,2,1,2,1,2/
      DATA PANGO /MAXANT*100.0/
      DATA LUN /49/
C                                       Time increment (days) to update
C                                       correction matrices (0.1 sec.)
      DATA TINC /1.15E-6/
C                                       Parallac. angle change(rad) to
C                                       update corr. matrices (0 deg)
      DATA PAINC /0.0/
C-----------------------------------------------------------------------
C                                       Orientation-ellipticity?
C                                       DOORI = STNPST.EQ.'ORI-ELP '
      DOORI = SOLTYP.EQ.2
      IF (BCHANS.LE.0) BCHANS = BCHAN
      IF (ECHANS.LE.0) ECHANS = ECHAN
C                                       This routine has no defined
C                                       function at all unless
C                                       KNCOR = 2 or 4.
      IF ((KNCOR.NE.2) .AND. (KNCOR.NE.4)) GO TO 999
C                                       See if parallactic angles
C                                       current
      IF ((CURSOU.NE.PARSOU) .OR. (ABS(TIME-PARTIM).GT.TINC)) THEN
C                                       Source info
         IF ((CURSOU.NE.PARSOU) .OR. (CURSOU.NE.IDSOUR)) THEN
            CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, LUN, IERR)
            IF (IERR.NE.0) GO TO 999
            PANGO(1) = 1000.0
            END IF
         PARSOU = CURSOU
C                                       Parallactic angles
         CALL PARANG (TIME, PANGLE)
         DPANG = 0.0
         DO 10 I = 1,NSTNS
            PARAGL(1,I) = COS (PANGLE(I))
            PARAGL(2,I) = -SIN (PANGLE(I))
            DPANG = MAX (DPANG, ABS (PANGO(I)-PANGLE(I)))
 10         CONTINUE
C                                       Update par. angle time
         PARTIM = TIME
C                                       Set visibility increment
         INCPX = CATUV(KINAX)
C                                       Compressed data expanded
         IF (INCPX.EQ.1) INCPX = 3
C                                       Find order of Stokes axis in
C                                       input data.
         CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATUV(KHCTP), STORDR,
     *      IERR)
         IF ((IERR.NE.0) .OR. (STORDR.LT.1)) THEN
            MSGTXT = 'DATPOL: STOKES AXIS NOT FOUND'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Find Stokes Increment
         STINC = INCPX
         IF (STORDR.GT.1) STINC = STINC * MAX (1, CATUV(KINAX+1))
         IF (STORDR.GT.2) STINC = STINC * MAX (1, CATUV(KINAX+2))
         IF (STORDR.GT.3) STINC = STINC * MAX (1, CATUV(KINAX+3))
         IF (STORDR.GT.4) STINC = STINC * MAX (1, CATUV(KINAX+4))
C                                       Offset array for vis data
         LIMIT = 8 * KNCOR
         IPNT = -1
         DO 50 IPOL = 1,LIMIT,8
            OFF(IPOL) = IPNT + 1
            OFF(IPOL+1) = IPNT + 2
            OFF(IPOL+2) = IPNT + 1
            OFF(IPOL+3) = IPNT + 2
            OFF(IPOL+4) = IPNT + 1
            OFF(IPOL+5) = IPNT + 2
            OFF(IPOL+6) = IPNT + 1
            OFF(IPOL+7) = IPNT + 2
            IPNT = IPNT + STINC
 50         CONTINUE
         LIMIT = 2 * KNCOR
         IPNT = -1
         DO 60 IPOL = 1,LIMIT,2
            OFF2(IPOL) = IPNT + 1
            OFF2(IPOL+1) = IPNT + 2
            IPNT = IPNT + STINC
 60         CONTINUE
C                                       Time for a new set of matrices
C                                       for linear feeds? Check change
C                                       in parallactic angle.
         IF ((SOLTYP.GT.1) .AND. (DPANG.GT.PAINC)) THEN

C                                       Save parallactic angles
            DO 70 I = 1,NSTNS
               PANGO(I) = PANGLE(I)
 70            CONTINUE
C                                       Use central channel.
            IFQ = (BCHAN + ECHAN)/2
C                                       Get matrices for
C                                       orientation-ellipticity model.
            IF (DOORI) THEN
               CALL ORIPOL (PANGLE)
C                                       Get matrices for lin. pol.
            ELSE IF (SOLTYP.EQ.3) THEN
               CALL LXYPOL (PANGLE, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Get  matrices for VLBI
            ELSE IF (SOLTYP.EQ.4) THEN
               CALL VLBPOL (PANGLE)
               END IF
            END IF
         END IF
C                                       Set baseline index
      BLNDX = ((IA1-1)*NSTNS) - (((IA1-1)*IA1)/2) + IA2
      LENTRY = 32 * PCLIF  * PCLCH
C                                       Loop thru IF
      DO 400 IIF = BIF,EIF
         IOFF = (IIF-1) * KNCIF
         BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH + 1
C                                       Loop thru channels
         DO 300 IFQ = BCHANS,ECHANS
            JOFF = ((IFQ-1) * KNCF + IOFF) * INCPX + 1
C                                       Loop thru polarization
C                                       Deal with case of missing
C                                       parallel poln; use one present
C                                       for correction.
            WFLAG = .FALSE.
C                                       1st par. poln missing
            IF (VIS(JOFF+2).LE.0.0) THEN
               VIS(JOFF) = VIS(JOFF+OFF2(3))
               VIS(JOFF+1) = VIS(JOFF+OFF2(4))
               VIS(JOFF+2) = 0.0
               END IF
C                                       2nd par. poln missing
            IF (VIS(JOFF+OFF2(3)+2).LE.0.0) THEN
               VIS(JOFF+OFF2(3)) = VIS(JOFF)
               VIS(JOFF+OFF2(4)) = VIS(JOFF+1)
               VIS(JOFF+OFF2(3)+2) = 0.0
C                                       Flag all if neither parallel
C                                       poln present.
               IF (VIS(JOFF+2).LE.0.0) WFLAG = .TRUE.
               END IF
C                                       Check for missing cross-hand
C                                       data
            IF (KNCOR.GT.2) THEN
               JRL = JOFF + OFF2(5)
               JLR = JOFF + OFF2(7)
C                                       Check if both missing
               IF ((VIS(JRL+2).LE.0.0).AND.(VIS(JLR+2).LE.0.0)) THEN
C                                       Zero cross-hand data used in
C                                       correction
                  CALL RFILL (2, 0.0, VIS(JRL))
                  CALL RFILL (2, 0.0, VIS(JLR))
C                                       1st cross-hand missing
               ELSE IF (VIS(JRL+2).LE.0.0) THEN
C                                       Use RL=conjg(LR) approx.
                  VIS(JRL) = VIS(JLR)
                  VIS(JRL+1) = -VIS(JLR+1)
C                                       2nd cross-hand missing
               ELSE IF (VIS(JLR+2).LE.0.0) THEN
C                                       Use LR=conjg(RL) approx.
                  VIS(JLR) = VIS(JRL)
                  VIS(JLR+1) = -VIS(JRL+1)
                  END IF
               END IF
C                                       If DOPOL > 2 check for
C                                       any missing correlations
            IF ((DOPOL.EQ.3) .OR. (DOPOL.EQ.8)) THEN
               LIMIT = 2 * KNCOR
               DO 100 IPOL = 1, LIMIT, 2
                  IF (VIS(JOFF+OFF2(IPOL)+2).LE.0.0) WFLAG = .TRUE.
100               CONTINUE
               END IF
C                                       Check for blanked IFR
            IF ((IFR(IA1).EQ.FBLANK) .OR. (IFR(IA2).EQ.FBLANK)) THEN
               WFLAG = .TRUE.
               END IF
C                                       Flag all output data if
C                                       both par. hands missing or
C                                       (DOPOL > 2) and any polzn.
C                                       correlations are missing.
C                                       Also flag output data if
C                                       IFR corrections are blanked
            IF (WFLAG) THEN
               LIMIT = 2 * KNCOR
               DO 120 IPOL = 1, LIMIT, 2
                  VIS(JOFF+OFF2(IPOL)+2) = 0.0
120               CONTINUE
               END IF
C                                       Save old data
            LIMIT = 8 * KNCOR
            DO 150 IPOL = 1,LIMIT
               JNDEX = JOFF + OFF(IPOL)
               VTEMPX(IPOL) = VIS(JNDEX)
 150           CONTINUE
C                                       Clear XTEMP (in case KNCOR < 4)
            CALL RFILL (2*16, 0.0, XTEMP)
C                                       Matrix x vector multiply
            PP = BLPNT + PPOLCL
            LIMIT = 4 * KNCOR
            DO 200 IPOL = 1,LIMIT
               XTEMP(1,IPOL) = VTEMP(1,IPOL) * POLCAL(PP) -
     *            VTEMP(2,IPOL) * POLCAL(PP+1)
               XTEMP(2,IPOL) = VTEMP(1,IPOL) * POLCAL(PP+1) +
     *            VTEMP(2,IPOL) * POLCAL(PP)
               PP = PP + 2
 200           CONTINUE
            IF (PDVER.GT.0) BLPNT = BLPNT + 32
C                                       sum
            INDEX = 1
            LIMIT = KNCOR * 2
            DO 250 IPOL = 1,LIMIT
               INDEX = ((IPOL-1)/2) + 1
               IPNT = PNT(IPOL)
               JNDEX = JOFF + OFF2(IPOL)
               VIS(JNDEX) = XTEMP(IPNT,INDEX) + XTEMP(IPNT,INDEX+4) +
     *            XTEMP(IPNT,INDEX+8) + XTEMP(IPNT,INDEX+12)
 250           CONTINUE
C                                       Done if 'ORI-ELI'
            IF (DOORI) GO TO 300
C                                       Done if not 4 polarizations
            IF (KNCOR .LT. 4) GO TO 300
C                                       PARALLACTIC angle
C            GR = PARAGL(1,IA1) * PARAGL(1,IA2) - PARAGL(2,IA1) *
C     *         PARAGL(2,IA2)
C            GI = PARAGL(1,IA1) * PARAGL(2,IA2) + PARAGL(2,IA1) *
C     *         PARAGL(1,IA2)
C                                       no rotation4
            GR = 1.0
            GI = 0.0
C                                       Correct RL,LR for parallactic
C                                       angle and ionospheric Faraday
C                                       rotation:
            LOFF = (IIF - 1) * NLAMDA + IFQ
            GR1 = GR * COS (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2))) -
     *         GI * SIN (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2)))
            GI1 = GI * COS (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2))) +
     *         GR * SIN (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2)))
C                                       Correct RL
            IPNT = 5
            TR = VIS(JOFF+OFF2(IPNT))
            TI = VIS(JOFF+OFF2(IPNT+1))
            VIS(JOFF+OFF2(IPNT)) = TR * GR1 + TI * GI1
            VIS(JOFF+OFF2(IPNT+1)) = TI * GR1 - TR * GI1
C                                       Correct LR
            IPNT = 7
            TR = VIS(JOFF+OFF2(IPNT))
            TI = VIS(JOFF+OFF2(IPNT+1))
            VIS(JOFF+OFF2(IPNT)) = TR * GR1 - TI * GI1
            VIS(JOFF+OFF2(IPNT+1)) =  TI * GR1 + TR * GI1
 300        CONTINUE
 400     CONTINUE
C
 999  RETURN
      END

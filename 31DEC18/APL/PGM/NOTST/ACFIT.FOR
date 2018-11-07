LOCAL INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, NAME2*12, CLAS2*6,
     *   XCALCO*4, NAMOUT*12, CLAOUT*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XNAME2(3),
     *   XCLAS2(2), XXCALC, XNAMOU(3), XCLAOU(2)
      REAL     XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XSUBA, XANT(50), XS2, XD2, XDOCAL, XGUSE, XFLAG, XSOLIN,
     *   XDOBAN, XBPVER, XSMOTH(3), XBCHAN, XECHAN, APARM(10),
     *   BPARM(10), CPARM(10), DPARM(10), EPARM(10), XSNVER, XSOUT,
     *   XDSOUT, XBADD(10), ACSMAX, MINGAN, MAXGAN, MXGERR, XRFANT
      INTEGER   SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2, PRTLV,
     *   IBEGIN, IEND, NPOLY, BASE(10), NPOLZN, NUMBIF, NUMTEL, NUMFRQ,
     *   MAXANO, IACMAX, ACSOUR, REFANT, NPOLTP, SEQOUT, DISOUT, CNOOUT,
     *   LUNOUT, FINDOU, NCHNTP
      REAL   SOLINT, BUFF1(UVBFSL), BUFF2(UVBFSL), RTSYS(10), LTSYS(10),
     *   RJYDEG, LJYDEG, BUFOUT(UVBFSL)
      LOGICAL   DOBASE, DOAUTO, DOWRIT, SINGLE
C                                       Template information
      REAL    TPLATE(MAXCIF), TPLMAX
      INTEGER TSIZE, ITPMAX
      LOGICAL DOTBAS
C                                       SN table information
      INTEGER SNKOLS(MAXSNC), SNNUMV(MAXSNC), ISNRNO, NUMNOD, SNVER,
     *   SNLUN, SNBUFF(512)
      REAL    GNMOD
      LOGICAL ISAPPL
      DOUBLE PRECISION RANOD, DECNOD
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XBAND, XFREQ, XFQID, XTIME, XSUBA, XANT, XNAME2,
     *   XCLAS2, XS2, XD2, XRFANT, XDOCAL, XGUSE, XFLAG, XSOLIN, XDOBAN,
     *   XBPVER, XSMOTH, XBCHAN, XECHAN, APARM, BPARM, CPARM, DPARM,
     *   EPARM, XSNVER, XNAMOU, XCLAOU, XSOUT, XDSOUT, XBADD
      COMMON /ACPARM/ SEQIN, SEQ2, SEQOUT, DISK2, DISKIN, DISOUT, CNOIN,
     *   CNOIN2, CNOOUT, LUNOUT, FINDOU, NCHNTP, SOLINT, RTSYS, LTSYS,
     *   RJYDEG, LJYDEG, PRTLV, IBEGIN, IEND, NPOLY,  NPOLTP, BASE,
     *   NPOLZN, NUMBIF, NUMTEL, NUMFRQ, MAXANO, REFANT, BUFF1, BUFF2,
     *   ACSMAX, MINGAN, MAXGAN, MXGERR, IACMAX, BUFOUT, ACSOUR, DOBASE,
     *   DOAUTO, DOWRIT, SINGLE
      COMMON /TMPLAT/ TSIZE, ITPMAX, TPLATE, TPLMAX, DOTBAS
      COMMON /CHPARM/ NAMEIN, CLAIN, XSTOK, NAME2, CLAS2, XCALCO,
     *   NAMOUT, CLAOUT
      COMMON /SNPARM/ RANOD, DECNOD, SNBUFF, SNKOLS, SNNUMV, ISNRNO,
     *   NUMNOD, SNVER, SNLUN, GNMOD, ISAPPL
LOCAL END
      PROGRAM ACFIT
C-----------------------------------------------------------------------
C! ACFIT estimates antenna gains from autocorrelation data
C# Calibration Spectral VLA VLB EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2003-2009, 2011-2012, 2015-2016
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
C   Task:  To create a 'SN' (SOLUTION) table which will contain
C   estimates of the real gains of the antennas. This is done
C   by performing a least-squares fit of a "perfectly calibrated"
C   template spectrum from one antenna to autocorrelation spectra
C   from other antennas. The resultant 'quality factors' are
C   written into an SN table.
C   AIPS Adverbs:
C     INNAME.....Input UV file name (name).      Standard defaults.
C     INCLASS....Input UV file name (class).     Standard defaults.
C     INSEQ......Input UV file name (seq. #).    0 => highest.
C     INDISK.....Disk drive # of input UV file.  0 => any.
C     SOURCES....Source list.  If the data is a multi-source file
C                BPASS will form the cross-power spectrum for the
C                first source specified. If the data is a single
C                source file no source name need be specified.
C     UVRANGE....Range (min, max) of projected baselines to include
C                0,0 => all baselines (units: klamda)
C     TIMERANG...Time range of the data to be selected. In order:
C                Start day, hour, min. sec,
C                end day, hour, min. sec. Days relative to ref.
C                date.
C     BCHAN......First channel to select. 0=>all.
C     ECHAN......Highest channel to select.
C     SUBARRAY...Subarray number to select. 0=>all.
C     ANTENNAS...A list of the antennas to be plotted.
C                If any number is negative then all antennas listed
C                are NOT to be selected and all others are.
C                                      CLEAN map (optional)
C     IN2NAME....Cleaned map name (name)
C     IN2CLASS...Cleaned map name (class)
C     IN2SEQ.....Cleaned map name (seq. #)
C     IN2DISK....Cleaned map disk unit #
C     INVERS.....CC file version #.
C     NCOMP......# comps to use for model.
C                1 value per field
C     NMAPS......No. Clean map files
C     SMODEL.....Source model to use instead of CLEAN map
C     DOCALIB....If true (>0) then calibrate the data using
C                information in the specified Cal (CL or SN).
C     GAINUSE....version number of the CL table to apply to
C                multisource files or the SN table for single
C                source files.  0 => highest.
C     FLAGVER....specifies the version of the flagging table to be
C                applied. 0 => highest numbered table.
C                <0 => no flagging to be applied.
C     SOLINT.....the interval over which to average the data
C                before solving for the bandpasses. (0 => scan)
C     BPVER......the version of the BP table to fill. (0 => 1)
C     SMOOTH.....Smoothing function.
C     ANTWT......Antenna weights for up to 30 antennas. (0=>1.0)
C     APARM......Control parameters
C     BADDISK....A list of disks on which scratch files are not to
C                be placed.  This will not affect the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'ACFIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL ACFTIN (PRGM, IRET)
      IF ( IRET.NE.0 ) GO TO 990
C                                       Read data and estimate gains
      CALL ESTGAN (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write history
      CALL ACFHIS
C
 990  CALL DIE (IRET, BUFF1)
C
      STOP
      END
      SUBROUTINE ACFTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C  ACFTIN gets input parameters for ACFIT and finds input file.
C  All selection criteria are filled into commons in D/CSEL.INC
C
C  Inputs:
C      PRGN      C*6)     Program name (2chars/word)
C  Outputs:
C      JERR      I        Error code : 0 => OK
C                                      5 => catalog troubles
C                                      8 => can't start
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, UTYPE*2
      INTEGER  NPARM, IROUND, IERR, IRET, I, IUSER, LUNTB, LUNAN,
     *   CAT2(256), LUNTPL, FRQID2, LUN, TBASE(10), NUMANS(513), JERR,
     *   NPLIM
      REAL   CATR(256)
      LOGICAL   T, F, TABLE, MULTI, FITASC, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR,CATBLK)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNTB, LUNAN, LUNTPL /29, 27, 30/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 278
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
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
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQ2  = IROUND (XS2)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISK2  = IROUND (XD2)
      DISOUT = IROUND (XDSOUT)
      IUSER = NLUSER
      SNVER = IROUND (XSNVER)
      REFANT = IROUND (XRFANT)
C                                        Baseline-independent fit ?
      DOAUTO = APARM(9) .GT. 0
      IF (DOAUTO) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         END IF
C                                       Write out baseline corrected
C                                       autocorrelations?
      DOWRIT = APARM(10).GT.0
      IF (DOAUTO) DOWRIT = .FALSE.
      IF (DOWRIT) THEN
         WRITE (MSGTXT,1110)
         CALL MSGWRT (6)
         END IF
C                                       Get CATBLK etc for template
C                                       file
      CNOIN2 = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLAS2, SEQ2, UTYPE,
     *   NLUSER, STAT, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, CNOIN2, CATBLK, 'REST', BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISK2, CNOIN2, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         IERR = 1
         GO TO 990
         END IF
      IF (IERR.GT.0) GO TO 999
      FRQID2 = FRQSEL
C                                       Get the template
      CALL COPY (256, CATBLK, CAT2)
      CALL COPY (256, CATBLK, CATUV)
      NPOLTP = MAX (IROUND (APARM(2)), 0)
      DOTBAS = F
      DO 150 I = 1, 10
         TBASE(I) = IROUND(CPARM(I))
         IF (TBASE(I).GT.0) DOTBAS = T
 150     CONTINUE
      NPOLZN = 1
      IF (NCOR.GT.1) NPOLZN = 2
      CALL GETTPL (TBASE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1050) IERR
         GO TO 990
         END IF
C                                       Get CATBLK from main file.
      CALL SELINI
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Determine if multi-source file
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *      MULTI, FITASC, IERR)
         MULTI = MULTI .AND. (IERR.EQ.0)
         END IF
      SINGLE = .NOT. MULTI
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check channel compatibility
      IF (NCHNTP.NE.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1010) NCHNTP
         CALL MSGWRT (6)
         WRITE (MSGTXT,1015) CATBLK(KINAX+JLOCF)
         JERR = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1080)
         IERR = 1
         GO TO 990
         END IF
      IF (IERR.GT.0) GO TO 999
      IF ((FRQSEL.NE.FRQID2).AND.(FRQID2.GT.0)) THEN
         WRITE (MSGTXT,1090) FRQSEL, FRQID2
         IERR = 1
         GO TO 990
         END IF
C                                       BADDISK
      DO 10 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 10      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 20 I= 1,30
         CALSOU(I) = ' '
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
 20      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RCOPY (8, XTIME, TIMRNG)
      DO 30 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 30      CONTINUE
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.EQ.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      CLUSE = IROUND (XGUSE)
      DXTIME = 0.0
      DOBAND = IROUND(XDOBAN)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Channel range to fit
      IBEGIN = IROUND (XBCHAN)
      IBEGIN = MAX (1, MIN (IBEGIN, CATBLK(KINAX+JLOCF)))
      IEND = IROUND (XECHAN)
      IF (IEND.LT.IBEGIN) IEND = CATBLK(KINAX+JLOCF)
      IEND = MAX (1, MIN (IEND, CATBLK(KINAX+JLOCF)))
C                                        Solution interval
      IF (XSOLIN.LT.0.0) THEN
         SOLINT = 0.0
      ELSE
         SOLINT = XSOLIN / (24.0 * 60.0)
         END IF
C                                       Default multisource = scan
      IF ((.NOT.SINGLE) .AND. (SOLINT.LE.1.0E-10)) SOLINT = 1.0
      IF (SOLINT.LT.1.0E-10) SOLINT = 1.0E10
C                                        Use autocorrelations only
      DOACOR = T
      DOXCOR = F
C                                        Print level
      PRTLV = IROUND (APARM(8))
C                                       Tsys, gain
      DO 45 I = 1, 10
         RTSYS(I) = DPARM(I)
         IF (RTSYS(I) .LE. 0.0) RTSYS(I) = 1.0
         LTSYS(I) = EPARM(I)
         IF (LTSYS(I) .LE. 0.0) LTSYS(I) = 1.0
   45 CONTINUE
      RJYDEG = APARM(3)
      LJYDEG = APARM(4)
      IF (RJYDEG.LE.0.0) RJYDEG = 1.0
      IF (LJYDEG.LE.0.0) LJYDEG = 1.0
C                                       Max,Min allowed gains
      MINGAN = APARM(5)
      MAXGAN = APARM(6)
      IF (MAXGAN.EQ.0.0) MAXGAN = 1.0E10
      MXGERR = APARM(7)
C                                       Polynomial fitting
      NPOLY = MAX (IROUND (APARM(1)), 0)
C                                        Check degree for baseline-
C                                       independent fit.
      IF (DOAUTO) THEN
         NPLIM = IEND - IBEGIN - 3
         NPLIM = MIN (NPLIM, 24)
         IF (NPOLY.GT.NPLIM) THEN
            NPOLY = NPLIM
            WRITE (MSGTXT,1100) NPOLY
            CALL MSGWRT (6)
            END IF
         END IF
C
      DOBASE = F
      DO 50 I = 1, 10
         BASE(I) = IROUND(BPARM(I))
         IF (BASE(I).GT.0) DOBASE = T
 50      CONTINUE
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATUV)
      LUNAN = 27
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUNAN, BUFF1, NUMANS, IRET)
      NUMTEL = NUMANS(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
      IF (JERR.EQ.0) JERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACFTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ACFTIN: TEMPLATE HAS ',I4,' CHANNELS')
 1015 FORMAT ('ACFTIN: MAIN FILE HAS ',I4,' CHANNELS - INCOMPATIBLE')
 1020 FORMAT ('Baseline-independent fit; ignoring BPARM & CPARM')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' OBTAINING TEMPLATE SPECTRUM')
 1070 FORMAT ('NO MATCH TO FQ ADVERBS IN TEMPLATE FILE - CHECK INPUTS')
 1080 FORMAT ('NO MATCH TO FQ ADVERBS IN DATA FILE - CHECK INPUTS')
 1090 FORMAT ('FRQSEL IN DATA = ',I2,', IN TEMPLATE = ',I2)
 1100 FORMAT ('ACFTIN: NPOLY too large; reduced to ',I4)
 1110 FORMAT ('Will write out baseline-corrected autocorrelations')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE ESTGAN (IRET)
C-----------------------------------------------------------------------
C   Routine to read data, apply any requested calibration, average
C   over selected time interval, fit template spectra to A/C data
C   and then write results of fitting into SN table.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      LOGICAL EOF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
C                                       Parameters for arrays
C                                       256*28*2*3+eps
      INTEGER BIGSIZ
      PARAMETER (BIGSIZ=3*MAXCIF)
      INTEGER   WORK1(512), ANTLAB(MAXANT), TIME(8), SCANUM, I, J,
     *   VISSIZ, IANT, NODENO, IDUM, IPOL, IIF, TIT(4), LUNSO, L,
     *   NNSOU, IREFA(2,MAXIF), VISOFF, BIND, LENBU, LRECO, BO, NOUT,
     *   KBUFSZ, NIO, NONOT, KEYLOC, KEYTYP, ORIGIN, NUMKEY, MXANT
      LONGINT   VISOO, KLONG
      REAL      VISOUT(2), BFIT(11), RPARM(20), VIS(BIGSIZ),
     *   GFACT(MAXANT,MAXIF,2), GERR(MAXANT,MAXIF,2), DISP(2), DDISP(2),
     *   CONST(MAXANT,MAXIF,2), CONERR(MAXANT,MAXIF,2), MBDELY(2),
     *   GREAL(2,MAXIF), GIMAG(2,MAXIF), GDELA(2,MAXIF), GRATE(2,MAXIF),
     *   GWT(2,MAXIF), RIFR, PRTIME, HAT, EL, TEMP, SOLACT, AZ
      DOUBLE PRECISION AVTIME(MAXANT), TIMRA(2), AVTMAX
      HOLLERITH CATH(256)
      EQUIVALENCE (CATBLK, CATH)
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, REASON*12, NAME*48,
     *   NOTTYP(5)*2
      LOGICAL   T, F, FIRST, NUSCAN, HAVFIT(2,MAXIF), CHSTAT, ALLFLG
      DATA T, F /.TRUE., .FALSE./
      DATA MXANT /MAXANT/, RIFR /0.0/, LUNSO /49/, NODENO /0/
      DATA MBDELY, DISP, DDISP /6*0.0/
      DATA NONOT, NOTTYP /5, 'NX', 'SN', 'CL', 'BP', 'SU'/
C-----------------------------------------------------------------------
      I = REFANT
      I = MAX (1, I)
      J = 2 * MAXIF
      CALL FILL (J, I, IREFA)
C                                       Purge old SN table
      IF (SNVER.GT.0) THEN
C                                       Determine NSOUWD & NANTSL
         CALL UVGET ('INIT', RPARM, VIS, IRET)
         IF (IRET.GT.0) GO TO 990
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         IF (IRET.GT.0) GO TO 990
         NNSOU = NSOUWD
         TIMRA(1) = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0)
     *      + (XTIME(4) / (24.0*60.0*60.0))
         TIMRA(2) = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0)
     *      + (XTIME(8) / (24.0*60.0*60.0))
         IF ((TIMRA(2).LT.TIMRA(1)) .OR. (TIMRA(2).LT.1.0E-5))
     *      TIMRA(2) = 1.0E20
         CALL CALSEL (DISKIN, CNOIN, 'SN', SNVER, CATUV, SNBUFF, NNSOU,
     *      SOUWAN, DOSWNT, NANTSL, ANTENS, DOAWNT, TIMRA, SUBARR,
     *      FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Init. file reading
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Create output file if
C                                       needed
      IF (DOWRIT) THEN
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT,
     *      CLAOUT, SEQOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
         CCNO = 1
         FRW(NCFILE+1) = 3
         CALL UVCREA (DISOUT, CCNO, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) IRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CNOOUT = CCNO
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISOUT
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISOUT, CCNO, IRET)
C                                       Open output file.
         LUNOUT = 18
         CALL ZPHFIL ('UV', DISOUT, CNOOUT, 1, NAME, IRET)
         CALL ZOPEN (LUNOUT, FINDOU, DISOUT, NAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1130) IRET
            GO TO 990
            END IF
C                                       Init output vis file for write
         LENBU = 1
         LRECO = LREC
         KBUFSZ = UVBFSL
         BO = 1
         NOUT = 0
         CALL UVINIT ('WRIT', LUNOUT, FINDOU, CATBLK(KIGCN),
     *      VISOFF, LRECO, LENBU, KBUFSZ, BUFOUT, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1140) IRET
            GO TO 990
            END IF
         END IF
C                                       Determine # polzns, antennas,
C                                       IF's and frequency channels
      NUMFRQ = CATUV(KINAX+KLOCFY)
      NUMBIF = CATUV(KINAX+KLOCIF)
C                                       Change status to 'writ'
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', IUDISK, IUCNO, CTEMP, CTEMP, IDUM, UTYPE,
     *   IDUM, STAT, WORK1, IRET)
C                                       Change status
      IF (STAT.EQ.'READ') THEN
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', IUDISK, IUCNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, WORK1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLRD'
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', IUDISK, IUCNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, WORK1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRIT'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       Create SN table
      ISAPPL = F
      GNMOD = 1.0
      RANOD = 0.D0
      DECNOD = 0.D0
      SNLUN = 48
      CALL SNINI ('WRIT', SNBUFF, IUDISK, IUCNO, SNVER, CATUV, SNLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMTEL, NPOLZN, NUMBIF, NUMNOD,
     *   GNMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
      WRITE (MSGTXT,1010) SNVER
      CALL MSGWRT (6)
C                                       Add the ORIGIN keyword
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      IF (SINGLE) ORIGIN = 1
      CALL TABKEY ('WRIT', 'SNORIGIN', NUMKEY, SNBUFF, KEYLOC, ORIGIN,
     *   KEYTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 990
         END IF
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', IUDISK, IUCNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, WORK1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLWR'
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', IUDISK, IUCNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, WORK1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            GO TO 990
            END IF
         END IF
C                                       Set up ANTLAB array
      CALL LABSET (ANTLAB, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Message
      IF (NPOLZN.EQ.1) THEN
         WRITE (MSGTXT,1044) RJYDEG
         CALL MSGWRT (4)
         WRITE (MSGTXT,1039) (RTSYS(I), I=1,8)
         CALL MSGWRT (4)
         END IF
      IF (NPOLZN.GT.1) THEN
         WRITE (MSGTXT,1040) (RTSYS(I), I=1,8)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1041) (LTSYS(I), I=1,8)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1045) RJYDEG
         CALL MSGWRT (4)
         WRITE (MSGTXT,1046) LJYDEG
         CALL MSGWRT (4)
         END IF
      IF (DOBAND.GT.0) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (4)
         END IF
C                                       If PRTLV>1 need some source
C                                       and antenna info.
      IF (PRTLV.GT.1) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
         END IF
C                                       Read data, calibrate and
C                                       average over specified
C                                       time interval
      EOF = F
      FIRST = T
      SCANUM = -1
C                                       dynamic array for averaging
      VISSIZ = 3 * NCOR * NUMBIF * NUMFRQ
      IANT = VISSIZ * MAXANO
      IANT = (IANT - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'ESTGAN',  IANT, VISOUT, VISOO, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO ALLOCATE ENOUGH MEMORY, SORRY'
         CALL MSGWRT (8)
         GO TO 999
         END IF

 100  CALL DATAVG (ANTLAB, FIRST, SOLINT, NUMBIF, NUMFRQ, VISSIZ,
     *   MAXANO, VISOUT(1+VISOO), AVTIME, SOLACT, TIME, NUSCAN, SCANUM,
     *   ACSOUR, RPARM, VIS, IRET)
      IF (IRET.GT.0) GO TO 990
      IF (IRET.LT.0) EOF = T
      AVTMAX = 0.0D0
      DO 110 I = 1,MAXANO
         AVTMAX = MAX (AVTMAX, AVTIME(I))
 110     CONTINUE
      IF (EOF .AND. (AVTMAX.EQ.0.D0)) GO TO 800
C                                       Loop over antennas to do
C                                       the fitting.
      DO 500 I = 1,MAXANO
C                                       Find antenna number
         IANT = 0
         DO 150 J = 1,MXANT
            IF (I.EQ.ANTLAB(J)) IANT = ANTLAB(J)
 150        CONTINUE
         IF (IANT.EQ.0) GO TO 500
C                                       Copy data to small array
         KLONG = 1 + VISOO + (IANT-1) * VISSIZ
C                                       Fit with template
         IF (DOAUTO) THEN
            CALL AUTOFT (NUMFRQ, NUMBIF, NPOLZN, VISOUT(KLONG), TPLATE,
     *         NPOLY, IBEGIN, IEND, I, MINGAN, MAXGAN, GFACT, GERR,
     *         HAVFIT, ACSMAX, IACMAX, IRET)
         ELSE
            CALL DOACFT (NUMFRQ, NUMBIF, NPOLZN, VISOUT(KLONG), TPLATE,
     *         NPOLY, BASE, DOBASE, IBEGIN, IEND, I, MINGAN, MAXGAN,
     *         GFACT, GERR, CONST, CONERR, HAVFIT, ACSMAX, IACMAX, BFIT,
     *         IRET)
            END IF
         REASON = 'BAD SOLUTION'
C                                       If desired write out the
C                                       corrected data
         IF (DOWRIT) THEN
            BUFOUT(BIND+ILOCU) = 0.0
            BUFOUT(BIND+ILOCV) = 0.0
            BUFOUT(BIND+ILOCW) = 0.0
            IF (ILOCB.GE.0) THEN
               BUFOUT(BIND+ILOCB) = I + 256*I
            ELSE
               BUFOUT(BIND+ILOCA1) = I
               BUFOUT(BIND+ILOCA2) = I
               BUFOUT(BIND+ILOCSA) = 1
               END IF
            BUFOUT(BIND+ILOCT) = AVTIME(I)
            CALL RCOPY (VISSIZ, VISOUT(KLONG), BUFOUT(BIND+NRPARM))
            NIO = 1
            NOUT = NOUT + 1
            CALL UVDISK ('WRIT', LUNOUT, FINDOU, BUFOUT, NIO, BIND,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            END IF
C                                       Write the SN entry
         ALLFLG = T
         DO 400 IPOL = 1,NPOLZN
            DO 300 IIF = 1,NUMBIF
               IF (PRTLV.GT.0) THEN
C                                       Write a message
                  PRTIME = AVTIME(I)
                  CALL TODHMS (PRTIME, TIT)
C                                       Check percentage error
                  IF (MXGERR.GT.0.0) THEN
                     IF (HAVFIT(IPOL,IIF)) THEN
                        TEMP = GERR(I,IIF,IPOL) /
     *                     GFACT(I,IIF,IPOL)
                        TEMP = TEMP * 100.0
                        IF (TEMP.GT.MXGERR) THEN
                           HAVFIT(IPOL,IIF) = .FALSE.
                           GFACT(I,IIF,IPOL) = FBLANK
                           GERR(I,IIF,IPOL) = FBLANK
                           REASON = 'LARGE ERROR'
                           END IF
                        END IF
                     END IF
                  IF (HAVFIT(IPOL,IIF)) THEN
                     WRITE (MSGTXT,1020) TIT, IPOL, IIF, I,
     *                  GFACT(I,IIF,IPOL), GERR(I,IIF,IPOL)
                     CALL MSGWRT (4)
                     IF (IPOL.EQ.1) THEN
                        TEMP = SQRT (GFACT(I,IIF,IPOL) * RTSYS(IIF)
     *                     * RJYDEG)
                     ELSE IF (IPOL.EQ.2) THEN
                        TEMP = SQRT (GFACT(I,IIF,IPOL) * LTSYS(IIF)
     *                     * LJYDEG)
                        END IF
                     WRITE (MSGTXT,1021) TEMP
                  ELSE
                     WRITE (MSGTXT,1025) TIT, IPOL, IIF, I,
     *                  REASON
                     END IF
                  CALL MSGWRT (4)
                  IF (PRTLV.GT.1) THEN
                     CALL GETSOU (ACSOUR, DISKIN, CNOIN, CATUV, LUNSO,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1060) IRET
                        GO TO 990
                        END IF
                     CALL SOUELV (I, AVTIME(I), HAT, EL, AZ)
                     EL = EL * RAD2DG
                     IF (DOAUTO) THEN
                        WRITE (MSGTXT,1032) EL
                     ELSE
                        WRITE (MSGTXT,1030) CONST(I,IIF,IPOL),
     *                     CONERR(I,IIF,IPOL), EL
                        END IF
                     CALL MSGWRT (4)
                     WRITE (MSGTXT,1070) TPLMAX, ITPMAX,
     *                  ACSMAX, IACMAX
                     CALL MSGWRT (4)
                     IF (.NOT.DOAUTO)WRITE(MSGTXT,1080)(BFIT(L),L=1,5)
                     CALL MSGWRT (4)
                     IF (.NOT.DOAUTO)WRITE(MSGTXT,1090)(BFIT(L),L=6,11)
                     CALL MSGWRT (4)
                     END IF
                  END IF
               IF (HAVFIT(IPOL,IIF)) THEN
C                                       Store the gain
                  IF (IPOL.EQ.1) THEN
                     GREAL(IPOL,IIF) =
     *                  SQRT (GFACT(I,IIF,IPOL) * RTSYS(IIF) * RJYDEG)
                  ELSE IF (IPOL.EQ.2) THEN
                     GREAL(IPOL,IIF) =
     *                  SQRT (GFACT(I,IIF,IPOL) * LTSYS(IIF) * LJYDEG)
                     END IF
                  IF (GERR(I,IIF,IPOL).GT.0.0) THEN
                     GWT(IPOL,IIF) = 1. / GERR(I,IIF,IPOL)
                  ELSE
                     GWT(IPOL,IIF) = 1000.0
                     END IF
                  IF (GERR(I,IIF,IPOL).EQ.FBLANK)
     *                  GWT(IPOL,IIF) = 0.0
                  ALLFLG = F
                  GIMAG(IPOL,IIF) = 0.0
                  GDELA(IPOL,IIF) = 0.0
                  GRATE(IPOL,IIF) = 0.0
C                                       No gain solution
               ELSE
                  GREAL(IPOL,IIF) = FBLANK
                  GIMAG(IPOL,IIF) = FBLANK
                  GDELA(IPOL,IIF) = FBLANK
                  GRATE(IPOL,IIF) = FBLANK
                  GWT(IPOL,IIF) = FBLANK
                  END IF
 300           CONTINUE
 400        CONTINUE
         IF (.NOT.ALLFLG)
     *      CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV,
     *         NPOLZN, AVTIME(I), SOLACT, ACSOUR, I, SUBARR, FRQSEL,
     *         RIFR, NODENO, MBDELY, DISP, DDISP, GREAL, GIMAG, GDELA,
     *         GRATE, GWT, IREFA, IRET)
         CALL RFILL (11, 0.0, BFIT)
 500     CONTINUE
      IF (PRTLV.GE.1) THEN
         WRITE (MSGTXT,1110)
         CALL MSGWRT (4)
         END IF
C                                       End?
 800  IF (.NOT. EOF) GO TO 100
      CALL TABIO ('CLOS', 0, ISNRNO, BUFF1, SNBUFF, IRET)
C                                       Flush output
      IF (DOWRIT) THEN
         NIO = 0
         CALL UVDISK ('FLSH', LUNOUT, FINDOU, BUFOUT, NIO, BIND,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1160) IRET
            GO TO 990
            END IF
C                                       Compress output file.
         NVIS = NOUT + VISOFF
         CALL UCMPRS (NVIS, DISOUT, CNOOUT, LUNOUT, CATBLK, IRET)
C                                       Put vis. count in CATBLK
         CATBLK(KIGCN) = NVIS
C                                        Copy tables
         IF (ILOCSU.GE.0) NONOT = NONOT - 1
         CALL ALLTAB (NONOT, NOTTYP, IULUN, LUNOUT, IUDISK, DISOUT,
     *      IUCNO, CNOOUT, CATBLK, BUFF1, BUFOUT, IRET)
         IF (IRET.GT.2) THEN
            WRITE (MSGTXT,1200)
            CALL MSGWRT (6)
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST',
     *      BUFOUT, IRET)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ESTGAN: ERROR ',I3,' CHANGING ',A4,' STATUS')
 1010 FORMAT ('ESTGAN: Writing SN table version ',I3)
 1015 FORMAT ('ESTGAN: ERROR ',I3,' WRITING ORIGIN KEYWORD TO SN TABLE')
 1020 FORMAT (I3,'/',3I3,' Pol:',I1,' IF:',I2,' Ant:',I2,
     *   ' Gain = ',F8.3,' +/- ',F10.3)
 1021 FORMAT (23X,'SN value: Gain = ',F8.3)
 1025 FORMAT (I3,'/',3I3,' Pol:',I1,' IF:',I2,' Ant:',I2,
     *   ' Gain = FLAGGED: ',A)
 1030 FORMAT (10X,' Offset = ',F8.5,' +/- ',F8.5,' El: ',F4.1)
 1032 FORMAT (10X,'El: ',F4.1)
 1070 FORMAT (10X,' Template max = ',F7.2,1X,I3,
     *   ' Scan max = ',F7.2,1X,I3)
 1080 FORMAT ('Poly coeffs: ',5(1X,F8.5))
 1090 FORMAT (4X,6(1X,F8.5))
 1039 FORMAT ('Tsys = ',8F6.1,';')
 1040 FORMAT ('Rpol Tsys = ',8F6.1,';')
 1041 FORMAT ('Lpol Tsys = ',8F6.1,';')
 1044 FORMAT ('JY/DEG = ',F5.2)
 1045 FORMAT ('Rpol JY/DEG =', F5.2)
 1046 FORMAT ('Lpol JY/DEG =', F5.2)
 1050 FORMAT ('ESTGAN: ERROR ',I3,' GETTING ANTENNA INFORMATION')
 1060 FORMAT ('ESTGAN: ERROR ',I3,' GETTING SOURCE INFORMATION')
 1100 FORMAT ('Applying bandpass corrections to data before fitting')
 1110 FORMAT ('----------------------------------------------------')
 1120 FORMAT ('ESTGAN: ERROR',I3,' CREATING OUTPUT FILE')
 1130 FORMAT ('ESTGAN: ERROR',I3,' OPENING OUTPUT FILE')
 1140 FORMAT ('ESTGAN: ERROR',I5,' INIT. OUTPUT FILE')
 1150 FORMAT ('ESTGAN: ERROR',I5,' WRITING OUTPUT FILE')
 1160 FORMAT ('ESTGAN: ERROR',I5,' FLUSHING OUTPUT FILE')
 1200 FORMAT ('ESTGAN: ERROR COPYING TABLES')
      END
      SUBROUTINE LABSET (ANTLAB, IRET)
C-----------------------------------------------------------------------
C   Routine to set up the antenna selection array; checks that data in
C   "TB" order and prepares the list of antennas (ANTENS) for UVGET that
C   includes all antennas mentioned in either XANT or XBASE.
C   Also checks allowed Stokes types.
C   Input from common:
C      ISORT              C    Two char. sort order, must be 'TB'
C      XANT(*)            R    Antenna array
C      XBASE(*)           R    Baseline array
C   Output:
C      ANTLAB(MAXANT)     I    The antenna number selected
C      IRET               I    Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS(*)          I    Selected antenna numbers
C-----------------------------------------------------------------------
      INTEGER   IRET, I, NEXT, IARG, MXANT, IROUND, J, NOSEL, NUMAN(51),
     *   NOANT, IANT1, LUN, WBUFF(1024)
      LOGICAL   T, ALLANT, DESEL
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT)
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA T /.TRUE./
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check sort order
      IF (ISORT.NE.'TB') THEN
         IRET = 1
         WRITE (MSGTXT,1030) ISORT
         GO TO 990
         END IF
C                                       Check for all ant. selected
      ALLANT = T
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
 100     CONTINUE
C                                       Not all selected - make list
      DESEL = .FALSE.
      IF (.NOT.ALLANT) THEN
         NEXT = 1
C                                       ANTENNAS array.
         DO 120 I = 1,50
            IARG = IROUND (XANT(I))
            IF (IARG.NE.0) THEN
               IF (IARG.LT.0) DESEL = .TRUE.
               ANTENS(NEXT) = IARG
               NEXT = NEXT + 1
               END IF
 120        CONTINUE
         NOSEL = NEXT - 1
         END IF
C                                       Fill ANTLAB with antenna
C                                       numbers.
      CALL FILL (MXANT, 0, ANTLAB)
C                                       Find number of antennas
      LUN = 44
      CALL GETNAN (DISKIN, CNOIN, CATUV, LUN, WBUFF, NUMAN, IRET)
      IF ((IRET.NE.0) .OR. (NUMAN(1).LT.SUBARR)) THEN
         IF (NUMAN(1).LE.SUBARR) WRITE (MSGTXT,1510) NUMAN(1),
     *      SUBARR
         IF (IRET.NE.0) WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
         END IF
      NOANT = NUMAN(SUBARR+1)
      IF (SUBARR.LE.0) NOANT = NUMAN(2)
C                                       Fill  arrays
      MAXANO = 0
      NEXT = 0
      IF (DESEL) THEN
         DO 550 I = 1,NOANT
            DO 540 J = 1,NOSEL
               IF (I.EQ.ABS(ANTENS(J))) GO TO 550
 540           CONTINUE
            NEXT = NEXT + 1
            ANTLAB(NEXT) = I
            MAXANO = MAX (MAXANO, I)
 550        CONTINUE
      ELSE
         DO 560 I = 1,50
            IF (ALLANT) THEN
               IANT1 = I
            ELSE
               IANT1 = ANTENS(I)
               END IF
            IF ((IANT1.GT.0) .AND. (IANT1.LE.NOANT)) THEN
               NEXT = NEXT + 1
               ANTLAB(NEXT) = IANT1
               MAXANO = MAX (MAXANO, IANT1)
               END IF
 560        CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('INCORRECT SORT ORDER = ',A2,' NOT TB')
 1510 FORMAT ('FEWER SUBARRAYS AVAILABLE (',I3,') THAN SPECIFIED ',I4)
 1511 FORMAT ('COLSET: GETNAN ERROR ',I3,' SEARCHING ANTENNA TABLES')
      END
      SUBROUTINE DOACFT (NUMFRQ, NUMBIF, NPOLZN, VIS, TPLATE, NPOLY,
     *   IBASE, DOBASE, IBEGIN, IEND, ANT, MINGAN, MAXGAN, GFACT, GERR,
     *   CONST, CONERR, HAVFIT, ACSMAX, IACMAX, BFIT, IRET)
C-----------------------------------------------------------------------
C  Routine determines antenna gain factors by fitting a template
C  autocorrelation spectrum to spectra from all antennas.
C
C  Inputs:
C    NUMFRQ             I         # channels in spectrum
C    VIS (*)            R         Autocorrelation spectrum
C    TPLATE(*)          R         Template spectrum
C    NPOLY              I         Degree of polynomial to be
C                                 removed before fitting
C    IBASE(2,5)         I         Array of start and stop channel
C                                 numbers specifying the channels
C                                 to be used for baseline fitting
C    DOBASE             L         .TRUE. if baseline fitting
C    IBEGIN             I         Start channel for AC fitting
C    IEND               I         End channel for AC fitting
C    ANT                I         Antenna being fitted
C    MINGAN             R         Minimum acceptable gain
C    MAXGAN             R         Maximum acceptable gain
C
C  Outputs:
C    GFACT(*,*,*)       R         Gain factor for each antenna, IF,
C                                 polzn
C    GERR(*,*,*)        R         Associated error
C    CONST(*,*,*)       R         Constant offset for each antenna,IF,
C                                 polzn
C    CONERR(*,*,*)      R         Associated error
C    HAVFIT(4,MAXIF)    L         .TRUE. if fit succesful
C    ACSMAX             R         Max value in spectrum
C    IACMAX             I         Channel of max value
C    BFIT(11)           R         Array containing polynomial
C                                 removed from data
C    IRET               I         Error code, 0 => OK
C
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NUMFRQ, NPOLY, IBASE(2,5), IBEGIN, IEND, IRET, ANT,
     *   IACMAX, NPOLZN, NUMBIF
      REAL    VIS(*), TPLATE(MAXCIF), ACSMAX, BFIT(11), MINGAN, MAXGAN
      LOGICAL HAVFIT(2,MAXIF)
      REAL    GFACT(MAXANT,MAXIF,2), GERR(MAXANT,MAXIF,2),
     *   CONST(MAXANT,MAXIF,2), CONERR(MAXANT,MAXIF,2)
C
      INTEGER IB0, IE0, INDEX, INP, I, LOOPS, LOOPIF, NFLAG, INX
      REAL    SCALE, ESCALE, CNSTNT, ECONST, RHO, SIGSQ,
     *   ACSPEC(MAXCHA), TPLSPC(MAXCHA), INVSC
      LOGICAL DOBASE
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      ACSMAX = -1.0E10
C                                       Check defaults on channel
C                                       fitting range
      IB0 = IBEGIN
      IF (IBEGIN .EQ. 0)  IB0 = 5
      IE0 = IEND
      IF (IEND .EQ. 0)  IE0 = NUMFRQ - 5
C                                       Loop over IF's, polzns
      INX = 0
      DO 300 LOOPS = 1,NPOLZN
         DO 200 LOOPIF = 1,NUMBIF
            HAVFIT(LOOPS,LOOPIF) = .FALSE.
            NFLAG = 0
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            DO 100 I = 1,NUMFRQ
               INX = INX + 1
               INP = INDEX + (I-1) * INCF
               IF (VIS(INP+2).LE.0.0) THEN
                  NFLAG = NFLAG + 1
                  ACSPEC(I) = FBLANK
               ELSE
                  ACSPEC(I) = VIS(INP)
                  END IF
               TPLSPC(I) = TPLATE(INX)
 100           CONTINUE
            IF (NFLAG.EQ.NUMFRQ) GO TO 200
C                                       Remove baseline from spectrum
            IF (DOBASE) THEN
               CALL BLINE (ACSPEC, NUMFRQ, NPOLY, IBASE, BFIT, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            DO 110 I = 1, NUMFRQ
               IF ((ACSPEC(I).GT.ACSMAX) .AND. (ACSPEC(I).NE.FBLANK))
     *            THEN
                  ACSMAX = ACSPEC(I)
                  IACMAX = I
                  END IF
               INP = INDEX + (I-1) * INCF
               VIS(INP) = ACSPEC(I)
               IF (VIS(INP+2).LE.0.0) VIS(INP) = 0.0
 110           CONTINUE
C                                       Do the fit
            CALL FIT (IB0, IE0, ACSPEC, TPLSPC, SCALE, ESCALE, CNSTNT,
     *         ECONST, RHO, SIGSQ, IRET)
            IF (IRET.EQ.1) GO TO 999
            INVSC = -1.0E10
            IF (SCALE.NE.0.0) INVSC = 1.0/SCALE
            IF (SIGSQ.GT.0.0)  HAVFIT(LOOPS,LOOPIF) = .TRUE.
            IF ((INVSC.LT.MINGAN) .OR. (INVSC.GT.MAXGAN))
     *         HAVFIT(LOOPS,LOOPIF) = .FALSE.
C                                       Fill in values
            IF (HAVFIT(LOOPS,LOOPIF)) THEN
               GFACT(ANT,LOOPIF,LOOPS) = 1./SCALE
               GERR(ANT,LOOPIF,LOOPS)  =
     *            GFACT(ANT,LOOPIF,LOOPS)**2*ESCALE
               CONST(ANT,LOOPIF,LOOPS) = CNSTNT
               CONERR(ANT,LOOPIF,LOOPS)= ECONST
            ELSE
               GFACT(ANT,LOOPIF,LOOPS) = FBLANK
               GERR(ANT,LOOPIF,LOOPS)  = FBLANK
               CONST(ANT,LOOPIF,LOOPS) = FBLANK
               CONERR(ANT,LOOPIF,LOOPS)= 0.0
               END IF
 200        CONTINUE
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DATAVG (ANTLAB, FIRST, DT, NMIFS, NMFRQS, VISSIZ,
     *   MXANT, VISOUT, AVTIME, SOLACT, TIME, NUSCAN, SCANUM, ACSOUR,
     *   RPARM,  VIS, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and averages spectra over a specified
C   time period.
C   Needs to be initialized by a call to UVGET.
C   The  order of the baselines returned in VISOUT is defined by
C   the order in the array ANTLAB.  All data specified
C   are averaged.
C   Inputs:
C     ANTLAB(MAXANT)    I    The antennas selected.
C     FIRST             L    Logical flag specifying if first call
C     DT                R    Averaging time in days
C     NMIFS             I    # IF's in data
C     NMFRQS            I    # freq. channels in data
C     NBGSIZ            I    Size of VIS array.
C    Input/output:
C     VISOUT(VISSIZ,*)  R    Array containing the result
C                            of the average, maximum size needs to
C                            be MAXANT*VISSIZ*3
C     AVTIME(MAXANT)    D    Mid time of averaged record. (Days)
C     SOLACT            R    Actual averaging time used (days).
C     TIME(8)           I    Center, end time (days, hours, min, sec)
C                            Unless NUSCAN only first 4 values are set.
C     NUSCAN            L    True IF the first record in a new scan.
C     ACSOUR            I    Source just averaged
C     IERR              I    Return code, 0 => OK, -1 => out of data,
C                            > 0 => failed.
C     RPARM            R(*)  Random parameters from last UVGET call.
C     VIS              R(*)  Visibility array from last UVGET call.
C   Output to common in D/CSOU.INC
C     SNAME            C*16  Source name (16 char. 4 / word.)
C     QUAL               I   Source qualifier.
C     CALCOD           C*4   Calibrator code 4 char.
C     FLUX(4,IF)         R   Total flux density I, Q, U, V pol,
C                            (Jy) each IF
C     FREQO(IF)          D   Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), NMIFS, NMFRQS, VISSIZ, MXANT, TIME(8),
     *   SCANUM
      REAL      DT, VIS(*), RPARM(*), VISOUT(VISSIZ,*), SOLACT
      DOUBLE PRECISION AVTIME(MAXANT)
      LOGICAL   NUSCAN, FIRST
C
      LOGICAL   DONE1, GOTDAT, T, F, WPCNT
      DOUBLE PRECISION DTIMSM(MAXANT), DWTSUM(MAXANT)
      INTEGER   IERR, I, J, K, JA1, JA2, SUNUM, JERR, ISLUN, IANT,
     *   CNTTIM, INDEX, INP, ACSOUR, NPNTS(MAXANT,4,4), NPNTMX
      REAL      T1, SUMTIM, TLAST, FRACN, TIMLST
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      SAVE TLAST, DONE1, NPNTMX
      DATA ISLUN, T, F /26, .TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       See if first record read
      IF (FIRST) THEN
         DONE1 = F
         NPNTMX = 1
         END IF
C                                       Clear arrays
 10   CONTINUE
      FIRST = T
      GOTDAT = .FALSE.
      DO 50 IANT = 1,MXANT
         DTIMSM(IANT) = 0.0D0
         DWTSUM(IANT) = 0.0D0
         DO 40 I = 1,NCOR
            DO 30 J = 1,NMIFS
               NPNTS(IANT,I,J) = 0
 30            CONTINUE
 40         CONTINUE
         CALL RFILL (VISSIZ, 0.0, VISOUT(1,IANT))
 50      CONTINUE
      SUMTIM = 0.0
      CNTTIM = 0
C                                       Initialize time
      T1 = 1.0E10
      TIMLST = 0.0
      IF (FIRST) TLAST = -1.0
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
C                                       Loop reading data
 100     IF (.NOT.DONE1) CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         DONE1 = F
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (JA1.NE.JA2) GO TO 100
C                                       Find antenna
         DO 110 I = 1,MXANT
            IANT = ANTLAB(I)
            IF (JA1.EQ.ANTLAB(I)) GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
 120     CONTINUE
C                                             Times
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         IF (FIRST) THEN
            IF (TLAST.EQ.-1.0) TLAST = T1 + DT
            END IF
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(ILOCT+1).GT.(TLAST))) GO TO 500
C                                       Time
         TIMLST = RPARM(ILOCT+1)
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
         ACSOUR = SUNUM
C                                       Average:
         DO 150 I = 1, NCOR
            DO 140 J = 1, NMIFS
               INDEX = 1 + (I-1)*INCS + (J-1)*INCIF
               DO 130 K = 1, NMFRQS
                  INP = INDEX + (K-1)*INCF
                  IF (VIS(INP+2).GT.0.0) THEN
                     GOTDAT = .TRUE.
                     VISOUT(INP,IANT) = VISOUT(INP,IANT) + VIS(INP) *
     *                  VIS(INP+2)
                     VISOUT(INP+1,IANT) = VISOUT(INP+1,IANT) +
     *                  VIS(INP+1) * VIS(INP+2)
                     VISOUT(INP+2,IANT) = VISOUT(INP+2,IANT) +
     *                  VIS(INP+2)
                     NPNTS(IANT,I,J) = NPNTS(IANT,I,J) + 1
                     NPNTMX = MAX (NPNTS(IANT,I,J), NPNTMX)
                     DTIMSM(IANT) = DTIMSM(IANT) + TIMLST * VIS(INP+2)
                     DWTSUM(IANT) = DWTSUM(IANT) + VIS(INP+2)
                     END IF
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
         GO TO 100
C                                       Scan done
C                                       See if have enough data.
C                                       Must have more than 20% of
C                                       the maximum no of points
C                                       averaged in any previous
C                                       interval, for at least one
C                                         ant/if/polzn.
 500  SOLACT = TIMLST - T1
      WPCNT = F
      DO 515 IANT = 1,MXANT
         DO 510 I = 1,NCOR
            DO 505 J = 1,NMIFS
               FRACN = (1.0 * NPNTS(IANT,I,J)) / NPNTMX
               IF (FRACN.GT.0.2) WPCNT = T
 505           CONTINUE
 510        CONTINUE
 515     CONTINUE
      GOTDAT = GOTDAT .AND. WPCNT
      IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
C                                       Normalization
      IF (RPARM(ILOCT+1).GT.TLAST) DONE1 = T
      DO 550 IANT = 1, MXANT
         AVTIME(IANT) = SUMTIM / CNTTIM
         DO 540 I = 1,NCOR
            DO 530 J = 1, NMIFS
               INDEX = 1 + (I-1)*INCS + (J-1)*INCIF
               FRACN = (1.0 * NPNTS(IANT,I,J)) / NPNTMX
C                                           Reject if too few points
               IF (FRACN.LT.0.2) GO TO 530
C                                       Mean time for this antenna
               IF (DWTSUM(IANT).GT.0)
     *            AVTIME(IANT) = DTIMSM(IANT) / DWTSUM(IANT)
               DO 520 K = 1, NMFRQS
                  INP = INDEX + (K-1)*INCF
                  IF (VISOUT(INP+2,IANT).GT.0.0) THEN
                     VISOUT(INP,IANT) = VISOUT(INP,IANT) /
     *                  VISOUT(INP+2,IANT)
                     VISOUT(INP+1,IANT) = VISOUT(INP+1,IANT) /
     *                  VISOUT(INP+2,IANT)
                     IF (I.LE.2) VISOUT(INP+1,IANT) = 0.0
                     END IF
 520              CONTINUE
 530           CONTINUE
 540        CONTINUE
 550     CONTINUE
C                                       Get source info
      IF (.NOT.NUSCAN) GO TO 800
      CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
      IF (JERR.EQ.11) THEN
C                                       Didn't find source
         WRITE (MSGTXT,1750) SUNUM
         CALL MSGWRT (8)
         JERR = 0
         END IF
      IF (JERR.LE.0) GO TO 800
         IERR = JERR
         WRITE (MSGTXT,1700) JERR
         GO TO 990
C                                       Time
 800  T1 = 0
      IF (CNTTIM.GT.0) T1 = SUMTIM / CNTTIM
      TIME(1) = T1
      T1 = (T1 - TIME(1)) * 24.0
      TIME(2) = T1
      T1 = (T1 - TIME(2)) * 60.0
      TIME(3) = T1
      T1 = (T1 - TIME(3)) * 60.0
      TIME(4) = T1 + 0.5
      FIRST = F
C                                       If end of data, close UVGET
      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
      IF (JERR.NE.0) IERR = JERR
      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) IERR = -2
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('DATAVG: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('DATAVG: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE BLINE (ACSPEC, NUMFRQ, NP, BASE, BFIT, IRET)
C-----------------------------------------------------------------------
C  Routine BLINE fits a polynomial baseline to spectra. The
C  fitted parameters are returned and the data is returned
C  with the baseline removed.
C  The routine assumes that only one IF/POLZN is passed down.
C  The calling routine must call BLINE over a loop of # IF's
C  and # polzns.
C
C  Inputs:
C     ACSPEC(*)      R        Input spectrum
C     NUMFRQ         I        # freq. channels in the spectrum
C     NP             I        The degree of polynomial to fit
C     BASE(2,5)      I        Array of start and stop channel
C                             numbers specifying the channels
C                             to be used for fitting
C  Outputs:
C     ACSPEC(*)      R        Output spectrum
C     BFIT(11)       R        The fitted parameters
C     IRET           I        Error code: 0 => OK
C                                         1 => error
C-----------------------------------------------------------------------
      REAL    ACSPEC(*), BFIT(11)
      INTEGER NUMFRQ, NP, BASE(2,5), IRET
C
      INTEGER IB, IBB, IBB1, I, NCHNLS, J, N
      REAL    XB(1024), YB(1024), YBAVG, TEMP
      REAL VARRES
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NCHNLS = 0
C                                       Check channel ranges
      DO 100 IB = 1, 5
         IF (BASE(1,IB).LT.0 .OR. BASE(2,IB).LT.0) THEN
            IRET = 1
            WRITE (MSGTXT,1000) BASE(1,IB), BASE(2,IB)
            GO TO 990
            END IF
         IF (BASE(1,IB).GT.NUMFRQ .OR. BASE(2,IB).GT.NUMFRQ) THEN
            IRET = 1
            WRITE (MSGTXT,1010) BASE(1,IB), BASE(2,IB), NUMFRQ
            GO TO 990
            END IF
         IF (BASE(2,IB).LT.BASE(1,IB)) THEN
            IRET = 1
            WRITE (MSGTXT,1020) BASE(1,IB), BASE(2,IB)
            GO TO 990
            END IF
C                                       Select data
         IBB=BASE(1,IB)
         IBB1=BASE(2,IB)
         IF ((IBB.EQ.0) .OR. (IBB1.EQ.0)) GO TO 100
         DO 50 I=IBB,IBB1
            IF (ACSPEC(I).NE.FBLANK) THEN
               NCHNLS = NCHNLS + 1
               XB(NCHNLS) = I
               YB(NCHNLS) = ACSPEC(I)
               END IF
 50         CONTINUE
 100     CONTINUE
C                                       Special case, NP = 0
      IF (NP.EQ.0) THEN
         YBAVG = 0.0
         IF (NCHNLS.EQ.0) GO TO 120
         DO 110 I = 1, NCHNLS
            YBAVG = YBAVG + YB(I)
 110        CONTINUE
         YBAVG = YBAVG / NCHNLS
 120     DO 150 I = 1, NUMFRQ
            IF (ACSPEC(I).NE.FBLANK) ACSPEC(I) = ACSPEC(I) - YBAVG
 150        CONTINUE
         BFIT(1) = YBAVG
         GO TO 999
         END IF
C                                       Set up for polynomial fit
C                                       Jump out if nothing to fit
      IF (NCHNLS.EQ.0) GO TO 999
C                                       fit polinom to the data
      N = NP + 1
      CALL POLINO (XB, YB, N, NCHNLS, BFIT, VARRES, IRET)
C                                       Subtract baseline from data
      DO 700 I=1,NUMFRQ
         TEMP=0.0
         DO 650 J=1,N
            TEMP=TEMP+BFIT(J)*I**(J-1)
  650       CONTINUE
         IF (ACSPEC(I).NE.FBLANK) ACSPEC(I)=ACSPEC(I)-TEMP
  700    CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' CHECK BPARMS')
 1010 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' NUMFRQ = ',I5)
 1020 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' INVERTED??')
      END
      SUBROUTINE FIT (I0, I1, SPEC, TPLSPC, SCALE, ERRSCL, CONST,
     *   ERRCON, RHO, SIGSQ, IRET)
C-----------------------------------------------------------------------
C Routine which actually does the least squares fit of the
C template spectrum to the AC spectrum.
C
C Inputs:
C   I0           I         Start channel of the fit
C   I1           I         End channel of the fit
C   SPEC(*)      R         Spectrum to be fitted
C   TPLSPC(*)    R         Template spectrum
C Outputs:
C   SCALE        R         Scale factor - result of fit
C   ERRSCL       R         Error on scale factor
C   CONST        R         Constant offset - result of fit
C   ERRCON       R         Error on offset
C   RHO          R
C   SIGSQ        R
C   IRET         I         Error flag, 0 => OK
C                                      1 => fatal error
C                                      2 => ignore this value
C-----------------------------------------------------------------------
      INTEGER I0, I1, IRET
      REAL    SPEC(*), TPLSPC(*), SCALE, ERRSCL, CONST, ERRCON, RHO,
     *   SIGSQ
C
      INTEGER ASKIP(2,5), N, I, J
      REAL    TSUM, TSQ, DSUM, DSQ, PRODCT, DETERM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set initial values
      IRET = 0
      N      = 0
      TSUM   = 0.
      TSQ    = 0.
      DSUM   = 0.
      DSQ    = 0.
      PRODCT = 0.
C                                       ASKIP is here for later
C                                       when I figure out how to
C                                       pass bad channels thru AIPS
      CALL FILL (10, 0, ASKIP(1,1))
C                                       Form sums, squares etc
      DO 200 I = I0,I1
C                                       Check for bad channels
         DO 100 J = 1,5
            IF (ASKIP(J,1).EQ.0) GO TO 110
            IF ((I.GE.ASKIP(J,1)) .AND. (I.LE.ASKIP(J,2))) GO TO 200
 100        CONTINUE
 110     N = N + 1
         TSUM = TSUM + TPLSPC(I)
         TSQ = TSQ + TPLSPC(I)**2
         IF (SPEC(I).NE.FBLANK) THEN
            DSUM = DSUM + SPEC(I)
            DSQ = DSQ + SPEC(I)**2
            PRODCT = PRODCT + SPEC(I)*TPLSPC(I)
            END IF
 200     CONTINUE
C                                       Ensure N > 2
      IF (N.LE.2) THEN
         WRITE (MSGTXT,1000) N
         IRET = 1
         GO TO 990
         END IF
C                                       Determine scale & offset
      DETERM = N*TSQ - TSUM**2
      IF (DETERM.LE.0) THEN
         WRITE (IRET,1100)  DETERM
         IRET = 2
         GO TO 990
         END IF
      SCALE = (N*PRODCT - TSUM*DSUM)/DETERM
      CONST = (-TSUM*PRODCT + TSQ*DSUM)/DETERM
C                                       Determine sigma square etc
      SIGSQ = 0.
      DO 300 I = I0,I1
         DO 250 J = 1,5
            IF (ASKIP(J,1).EQ.0) GO TO 260
            IF ((I.GE.ASKIP(J,1)) .AND. (I.LE.ASKIP(J,2))) GO TO 300
 250        CONTINUE
 260     IF (SPEC(I).NE.FBLANK)
     *      SIGSQ = SIGSQ + (SPEC(I) - (TPLSPC(I)*SCALE+CONST))**2
 300     CONTINUE
      SIGSQ = SIGSQ / (N-2.)
      ERRSCL = SQRT( SIGSQ*N/DETERM )
      ERRCON = SQRT( SIGSQ*TSQ/DETERM )
      RHO = -TSUM/SQRT(N*TSQ)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIT: ONLY ',I2,' CHANNELS AVAILABLE FOR GAIN FITTING')
 1100 FORMAT ('FIT: WARNING DETERMINANT = ',E12.3,' GAIN IS FLAGGED')
      END
      SUBROUTINE GETTPL (KBASE, IERR)
C-----------------------------------------------------------------------
C  Routine to get the template spectrum from a uv-file described
C  by the input parms. Uses UVGET so that things have to be reset
C  afterwards.
C  Input:
C     KBASE   I(2,5)   Array of start and stop channel numbers
C                      specifying the channels to be used for
c                      baseline fitting
C  Output:
C     IERR    I        Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   KBASE(2,5), IERR
      INCLUDE 'INCS:PUVD.INC'
C
      HOLLERITH CATH(256)
      INTEGER   NTVIS, IROUND, IA1, IA2, NUMTFQ, I, IPOL, IIF, IFRQ,
     *   INDEX, OIA1, KFREQ, CNTTIM, TIT(3), INX, NUMTIF
      REAL      TIME, WT, RPARM(20), VIS(3*MAXCIF+20), LINWT(2,MAXIF),
     *   XNORM, SUMWT, TFIT(11), TSPEC(MAXCHA), SUMTIM, AVTIME, TITSEC
      LOGICAL   T, F
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATH, CATBLK)
      DATA T, F / .TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Set up all variables for UVGET
      CALL SELINI
      UNAME = NAME2
      UCLAS = CLAS2
      UDISK = DISK2
      USEQ = SEQ2
      DO 20 I = 1,30
         CALSOU(I) = ' '
         SOURCS(I) = ' '
 20      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RFILL (8, 0, TIMRNG)
      DO 30 I = 1,50
         ANTENS(I) = 0
 30      CONTINUE
      BIF = 1
      EIF = 0
      BCHAN = 0
      ECHAN = 0
      DOCAL = F
      DOAPPL = F
      DOXCOR = F
      DOACOR = T
      DOBAND = -1
      DXTIME = 0.0
      FRQSEL = IROUND (XFQID)
C                                       Init I/O
      CALL UVGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         GO TO 990
         END IF
      NUMTIF = EIF - BIF + 1
      NUMTFQ = ECHAN - BCHAN + 1
      NTVIS = NVIS
      SUMTIM = 0.0
      CNTTIM = 0
      NCHNTP = CATUV(KINAX+JLOCF)
C                                       Check # freq. channels
      IF (NUMTFQ.LT.2) THEN
         MSGTXT = 'Your template spectrum has too few channels'
         IERR = 1
         GO TO 990
         END IF
C                                       Zero arrays
      I = NPOLZN * NUMTIF * NUMTFQ
      CALL RFILL (I, 0.0, TPLATE)
      I = 2 * MAXIF
      CALL RFILL (I, 0.0, LINWT)
      OIA1 = 0
C                                       Read the data
      DO 300 I = 1,NTVIS
         CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.LT.0) GO TO 400
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
C
         TIME = RPARM(1+ILOCT)
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB) / 256.0 + 0.1
            IA2 = RPARM(1+ILOCB) - 256 * IA1 + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         IF (IA1.NE.IA2) GO TO 300
         IF ((REFANT.GT.0) .AND. (IA1.NE.REFANT)) GO TO 300
         IF (OIA1.EQ.0) OIA1 = IA1
         IF ((OIA1.NE.0) .AND. (IA1.NE.OIA1)) THEN
            WRITE (MSGTXT, 1010) IA1, OIA1
            IERR = 1
            CALL MSGWRT (6)
            GO TO 990
            END IF
         SUMTIM = SUMTIM + TIME
         CNTTIM = CNTTIM + 1
         TPLMAX = -1.0E10
C                                       Average the data to obtain
C                                       the template spectrum
         INX = 0
         DO 230 IPOL = 1,NPOLZN
            DO 220 IIF = 1,NUMTIF
               KFREQ = 0
               SUMWT = 0.0
               DO 210 IFRQ = 1,NUMTFQ
                  INX = INX + 1
                  INDEX = (IFRQ-1) * INCF + (IIF-1) * INCIF +
     *               (IPOL-1) * INCS + 1
                  WT = VIS(INDEX+2)
                  IF (WT.GT.0.0) THEN
                     KFREQ = KFREQ + 1
                     SUMWT = SUMWT + WT
                     TPLATE(INX) = TPLATE(INX) + VIS(INDEX) * WT
                     END IF
 210              CONTINUE
               IF (KFREQ.GT.0) LINWT(IPOL,IIF) = LINWT(IPOL,IIF)
     *            + (SUMWT / KFREQ)
 220           CONTINUE
 230        CONTINUE
 300     CONTINUE
C                                       Print average time
 400  IF (CNTTIM.LE.0) THEN
         WRITE (MSGTXT,1400) REFANT
         IERR = 4
         GO TO 990
         END IF
      AVTIME = SUMTIM / CNTTIM
      CALL PTIME (AVTIME, F, TIT, TITSEC)
      WRITE (MSGTXT,1020) TIT, TITSEC
      CALL MSGWRT (4)
C                                       Normalize averaged spectrum
C                                       and remove constant offset.
      INX = 0
      DO 430 IPOL = 1,NPOLZN
         DO 420 IIF = 1,NUMTIF
            XNORM = 1.0
            IF (LINWT(IPOL,IIF).GT.1.0E-10) XNORM = 1.0 /
     *         LINWT(IPOL,IIF)
            DO 410 IFRQ = 1,NUMTFQ
               INX = INX + 1
               TSPEC(IFRQ) = TPLATE(INX) * XNORM
 410           CONTINUE
C                                       Remove the baseline unless
C                                       baseline-independent fit.
            IF (DOTBAS .AND. (.NOT.DOAUTO)) THEN
               CALL BLINE (TSPEC, NUMTFQ, NPOLTP, KBASE, TFIT, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Print fitting message
               IF (TFIT(1).NE.0.0) THEN
                  WRITE (MSGTXT,1030) IPOL, IIF, TFIT(1)
                  CALL MSGWRT (4)
                  END IF
               END IF
            INX = INX - NUMTFQ
            DO 415 IFRQ = 1,NUMTFQ
               INX = INX + 1
               IF (TSPEC(IFRQ).GT.TPLMAX) THEN
                  TPLMAX = TSPEC(IFRQ)
                  ITPMAX = IFRQ
                  END IF
               TPLATE(INX) = TSPEC(IFRQ)
 415           CONTINUE
 420        CONTINUE
 430     CONTINUE
C
      CALL UVGET ('CLOS', RPARM, VIS, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETTPL: ERROR ',I3,1X,A4,'ING TEMPLATE FILE')
 1010 FORMAT ('GETTPL: ANTENNA ',I3,' DIFFERS FROM FIRST FOUND ',I3)
 1020 FORMAT ('Mid time of template scan = ',I3,'/',2I3,1X,F4.1)
 1030 FORMAT ('Template: POL: ',I2,' IF: ',I2,' offset of',F13.5,
     *   ' removed')
 1400 FORMAT ('GETTPL: NO DATA FOR REFANT',I4,' FOUND')
      END
      SUBROUTINE ACFHIS
C-----------------------------------------------------------------------
C   ACFHIS copies and updates history file.  It also copies any tables
C   extension files.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   IERR, I, J, LUN, LUN2, TIME(3), DATE(3), I1, I2
      REAL      CATR(128), SOLMIN
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ACFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATR)
      DATA LUN, LUN2 /28, 29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      IF (DOWRIT) THEN
         CALL HISCOP (LUN, LUN2, DISKIN, DISOUT, CNOIN,
     *      CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 200
            END IF
         END IF
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Template file
      WRITE (HILINE,2000) TSKNAM, DISK2, NAME2, CLAS2, SEQ2
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Source list
C                                       Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
         I1 = 1
         IF (SOURCS(1)(1:1).EQ.'-') I1 = 2
         I2 = 1
         IF (SOURCS(2)(1:1).EQ.'-') I2 = 2
         WRITE (HILINE,3003) TSKNAM, SOURCS(1)(I1:), SOURCS(2)(I2:)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of sources
         DO 20 I = 1,NSOUWD,2
            IF ((SOURCS(I).NE.' ') .OR. (SOURCS(I+1).NE.' ')) THEN
               I1 = 1
               IF (SOURCS(I)(1:1).EQ.'-') I1 = 2
               I2 = 1
               IF (SOURCS(I+1)(1:1).EQ.'-') I2 = 2
               WRITE (HILINE,3004) TSKNAM, SOURCS(I)(I1:),
     *            SOURCS(I+1)(I2:)
               CALL HIADD (LUN, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 20         CONTINUE
         END IF
C                                       QUAL, CALCODE
      WRITE (HILINE,3010) TSKNAM, SELQUA, SELCOD
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       FQ adverbs
      WRITE (HILINE,3005) TSKNAM, FRQSEL, SELBAN
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,3006) TSKNAM, SELFRQ
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF'S
      WRITE (HILINE,3020) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      WRITE (HILINE,2003) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Antennas
      IF (NANTSL.LE.0) THEN
         WRITE (HILINE,3030) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3031) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3032) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 10 and label.
         WRITE (HILINE,3033) TSKNAM, (ANTENS(I),I=1,10)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         IF (NANTSL.GT.10) THEN
C                                       Rest of antennas
            DO 30 I = 11,NANTSL,10
               WRITE (HILINE,3034) TSKNAM, (ANTENS(J),J=I,I+9)
               CALL HIADD (LUN, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
 30            CONTINUE
            END IF
         END IF
C                                       Calibration
C                                       Tables
      IF (DOCAL) THEN
         WRITE (HILINE,2005) TSKNAM, CLVER
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Flag table
      WRITE (HILINE,2006) TSKNAM, FGVER
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Bandpass correction
      IF (DOBAND.GT.0) THEN
         WRITE (HILINE,2007) TSKNAM, DOBAND, BPVER
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       smoothing type
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,2008) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Solution interval
      IF (SOLINT.GE.1.0) THEN
         WRITE (HILINE,2018) TSKNAM
      ELSE
         SOLMIN = SOLINT * 24 * 60
         WRITE (HILINE,2011) TSKNAM, SOLMIN
         END IF
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Fitting range
      WRITE (HILINE,2012) TSKNAM, IBEGIN, IEND
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Polynomial
      WRITE (HILINE,2013) TSKNAM, APARM(1)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2015) TSKNAM
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2014) TSKNAM, (BASE(I),I=1,10)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Tsys, Jydeg
      WRITE (HILINE,2016) TSKNAM, (RTSYS(I), I=1,8)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2019) TSKNAM, (LTSYS(I), I=1,8)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2116) TSKNAM,  APARM(3)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2117) TSKNAM,  APARM(4)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Min, Max gains
      WRITE (HILINE,2017) TSKNAM, APARM(5), APARM(6)
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFF1, IERR)
      IF (DOWRIT) CALL HICLOS (LUN2, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACFHIS: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' Template file: Vol = ',I3,1X,A12,1X,A6,1X,I4)
 2003 FORMAT (A6,'SUBARRAY =',I4)
 2005 FORMAT (A6,'GAINVER =',I3,' / Calibrating with this table')
 2006 FORMAT (A6,'FLAGVER =',I3,' / FG table')
 2007 FORMAT (A6,'DOBAND =',I2,' BPVER = ',I3,' / Bandpass ')
 2008 FORMAT (A6,'SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Frequency smoothing function')
 2011 FORMAT (A6,'SOLINT = ',F8.2,' / Data average interval (mins)')
 2018 FORMAT (A6,'Data averaged over whole scan before fitting')
 2012 FORMAT (A6,'BCHAN = ',I3,' ECHAN = ',I3,' / Spectral ',
     *   'fitting range')
 2013 FORMAT (A6,'APARM(1) = ',F4.0,' / Order of polynomial')
 2014 FORMAT (A6,'BASE= ',10(1X,I3))
 2015 FORMAT (A6,'/ Start & stop channels for polynomial removal')
 2016 FORMAT (A6,'Rpol TSYS= ',8F6.1)
 2019 FORMAT (A6,'Lpol TSYS= ',8F6.1)
 2116 FORMAT (A6,'Rpol JYDEG= ',F5.2,' / Template cal')
 2117 FORMAT (A6,'Lpol JYDEG= ',F5.2,' / Template cal')
 2017 FORMAT (A6,'APARM(5,6) = ',2(F8.2,1X), ' / Min, max gains')
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded from gain estimation:')
 3002 FORMAT (A6,'/Sources used to estimate gain:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'FRQSEL =',I3,' SELBAN =',F8.3,' Hz')
 3006 FORMAT (A6,'SELFREQ = ',F20.5,' Hz')
 3010 FORMAT (A6,'QUAL = ',I4,' CALCODE = ',A4)
 3020 FORMAT (A6,'BIF = ',I4,' EIF = ',I4)
 3030 FORMAT (A6,'ANTENNAS = 0     /All antennas selected')
 3031 FORMAT (A6,'/Antennas excluded from gain estimation:')
 3032 FORMAT (A6,'/Antennas used in gain estimation:')
 3033 FORMAT (A6,'Antennas = ',10(1X,I3))
 3034 FORMAT (A6,'           ',10(1X,I3))
      END
      SUBROUTINE MTXINV (DA, N, NMAX, DET)
C---------------------------------------------------------------------
C   MTXINV uses the Gauss-Jordan method as described in
C   Introduction to Numerical Analysis, Stoer and Bulirsch, p171.
C   Input parameters:
C      DA     D(N,N)    Double precision square matrix.
C      N      I         Degree of matrix (<= 100).
C      NMAX   I         Degree of array A in calling program.
C   Output parameter:
C      DET    D         Determinant.
C---------------------------------------------------------------------
      INTEGER N, NMAX
      DOUBLE PRECISION DA(NMAX,NMAX), DET
C
      DOUBLE PRECISION DTMPCL(100), DCOLMX, DTEMP
      INTEGER ISWAP(100), I, J, K, IMAX, JP1, ITEMP,  NSWAP
C---------------------------------------------------------------------
      NSWAP = 0
      DET = 0.0
      IF (N.GT.100) GO TO 999
C                                              Use ISWAP to track row swaps.
      DET = 1.0
      DO 50 I = 1,N
         ISWAP(I) = I
  50     CONTINUE
C                                       Loop through the columns.
      DO 300 J = 1,N
C                                        Find a pivot in this column.
         DCOLMX = DA(J,J)
         IMAX = J
         JP1 = J + 1
         DO 100 I = JP1,N
            IF (ABS(DA(I,J)) .GT. ABS(DCOLMX)) THEN
               DCOLMX = DA(I,J)
               IMAX = I
               END IF
 100        CONTINUE
C                                        Accumulate the determinant
         DET = DET * DCOLMX
C                                        Is the matrix singular ?
         IF (DCOLMX.EQ.0) GO TO 999
C                                       Swap rows if neccessary.
         IF (IMAX.GT.J) THEN
            NSWAP = NSWAP + 1
            DO 150 K = 1,N
               DTEMP = DA(J,K)
               DA(J,K) = DA(IMAX,K)
               DA(IMAX,K) = DTEMP
150            CONTINUE
            ITEMP = ISWAP(J)
            ISWAP(J) = ISWAP(IMAX)
            ISWAP(IMAX) = ITEMP
            END IF
C                                       Transform the matrix;
C                                       excl current column.
         DTEMP = DA(J,J)
         DO 220 I = 1,N
            DO 200 K = 1,N
               IF ((I.EQ.J).OR.(K.EQ.J)) GO TO 200
               DA(I,K) = DA(I,K) - DA(I,J) * DA(J,K) / DTEMP
 200           CONTINUE
 220        CONTINUE
C                                       Set up current column.
         DO 250 I=1,N
            DA(I,J) = -DA(I,J) / DTEMP
 250        CONTINUE
C                                       Normalise the current row.
         DO 280 K = 1,N
            DA(J,K) = DA(J,K) / DTEMP
 280        CONTINUE
         DA(J,J) = 1.0 / DTEMP
C
 300     CONTINUE
C                                        Restore the original row order.
      DO 350 I = 1,N
         DO 320 K = 1,N
            DTMPCL(ISWAP(K)) = DA(I,K)
 320        CONTINUE
         DO 330 K = 1,N
            DA(I,K) = DTMPCL(K)
 330        CONTINUE
 350     CONTINUE
      IF (MOD(NSWAP,2).NE.0) DET = -DET
C                                        Exit
 999  RETURN
      END
      SUBROUTINE AUTOFT (NUMFRQ, NUMBIF, NPOLZN, VIS, TPLATE, NPOLY,
     *   IBEGIN, IEND, ANT, MINGAN, MAXGAN, GFACT, GERR, HAVFIT, ACSMAX,
     *   IACMAX, IRET)
C-----------------------------------------------------------------------
C  Routine determines antenna gain factors by fitting a template
C  autocorrelation spectrum to spectra from all antennas.
C  The method does not require a baseline range to be specified for
C  the template or AC spectrum but does assume the baseline
C  to be a polynomial with degree no higher than NPOLY.
C  Inputs:
C    NUMFRQ             I         # channels in spectrum
C    VIS (*)            R         Autocorrelation spectrum
C    TPLATE(*)          R         Template spectrum
C    NPOLY              I         Degree of polynomial describing
C                                 the baseline.
C    IBEGIN             I         Start channel for AC fitting.
C    IEND               I         End channel for AC fitting.
C    ANT                I         Antenna being fitted
C    MINGAN             R         Minimum acceptable gain
C    MAXGAN             R         Maximum acceptable gain
C
C  Outputs:
C    GFACT(*,*,*)       R         Gain factor for each antenna, IF,
C                                 polzn
C    GERR(*,*,*)        R         Associated error
C    HAVFIT(4,MAXIF)    L         .TRUE. if fit succesful
C    ACSMAX             R         Max value in spectrum
C    IACMAX             I         Channel of max value
C    IRET               I         Error code, 0 => OK
C
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NUMFRQ, NUMBIF, NPOLZN, NPOLY, IBEGIN, IEND, IRET, ANT,
     *   IACMAX
      REAL    VIS(*), TPLATE(MAXCIF), ACSMAX, MINGAN, MAXGAN
      LOGICAL HAVFIT(2,MAXIF)
      REAL    GFACT(MAXANT,MAXIF,2), GERR(MAXANT,MAXIF,2)
C
      INTEGER  INDEX, INP, I, LOOPS, LOOPIF, NFLAG, IERR, INX
      REAL    SCALE, ESCALE, ACSPEC(MAXCHA), TPLSPC(MAXCHA)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      ACSMAX = -1.0E10
C                                       Loop over IF's, polzns
      INX = 0
      IRET = 10
      DO 300 LOOPS = 1,NPOLZN
         DO 200 LOOPIF = 1,NUMBIF
            HAVFIT(LOOPS,LOOPIF) = .FALSE.
            NFLAG = 0
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            DO 100 I = 1,NUMFRQ
               INX = INX + 1
               INP = INDEX + (I-1) * INCF
               IF (VIS(INP+2).LE.0.0) THEN
                  NFLAG = NFLAG + 1
                  ACSPEC(I) = FBLANK
               ELSE
                  ACSPEC(I) = VIS(INP)
                  IF (ACSPEC(I).GT.ACSMAX) THEN
                     ACSMAX = ACSPEC(I)
                     IACMAX = I
                     END IF
                  END IF
               TPLSPC(I) = TPLATE(INX)
 100           CONTINUE
            IF (NFLAG.EQ.NUMFRQ) GO TO 200
C                                       Do the fit
            CALL GAINFT (ACSPEC, TPLSPC, NUMFRQ, FBLANK, NPOLY,
     *         IBEGIN, IEND, SCALE, ESCALE, IERR)
            HAVFIT(LOOPS,LOOPIF) = (IERR.EQ.0) .AND. (SCALE.NE.FBLANK)
     *         .AND. (SCALE.GE.MINGAN) .AND. (SCALE.LE.MAXGAN) .AND.
     *         (SCALE.NE.0.0)
C                                       Fill in values
            IF (HAVFIT(LOOPS,LOOPIF)) THEN
               GFACT(ANT,LOOPIF,LOOPS) = SCALE
               GERR(ANT,LOOPIF,LOOPS)  = ESCALE
               IRET = 0
            ELSE
               GFACT(ANT,LOOPIF,LOOPS) = FBLANK
               GERR(ANT,LOOPIF,LOOPS)  = FBLANK
               END IF
 200        CONTINUE
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GAINFT (ACSPEC, TEMPL, NCHAN, SNULL, NP,
     *   ISTART, IEND, SCALE, ERRSCL, IRET)
C---------------------------------------------------------------------
C  Determine the scaling of an AC spectrum with respect to the
C  template AC spectrum, thus determining the gain of an antenna.
C  This routine does "automatic" baseline removal, i.e. no
C  no baselines are removed from the spectra prior to fitting and
C  the baselines are assumed to have polynomial form of degree <= NP.
C  The source spectrum is fitted in the form
C   Gain*Template + Poly (Degree NP)
C  using linear least squares over the channel range (ISTART,IEND).
C   Input parameters:
C      ACSPEC R(NCHAN)  Spectrum.
C      TEMPL  R(NCHAN)  Template spectrum.
C      NCHAN  I         Number of frequency channels.
C      SNULL  R         Value of blanked pixel in spectra.
C      NP     I         Degree of polynomial expected for baseline.
C      ISTART I         Start channel for AC fit.
C      IEND   I         End channel for AC fit.
C   Output parameters:
C      SCALE  R         Scale factor to multiply ACSPEC.
C                       Returns SNULL if no solution.
C      ERRSCL R         Error in scale factor.
C                       Returns SNULL if no solution.
C      IRET   I         Return code (0 => ok).
C---------------------------------------------------------------------
      INTEGER NCHAN, NP, IRET, ISTART, IEND
      REAL ACSPEC(NCHAN), TEMPL(NCHAN), SNULL, SCALE, ERRSCL
C
      INTEGER MAXNP
      PARAMETER (MAXNP = 26)
      DOUBLE PRECISION DCURV(MAXNP,MAXNP), DB(MAXNP), DSUM,
     *  DSUMJ, DSUMK, DX, DET, DVAR
      REAL A(MAXNP), AERR(MAXNP)
      INTEGER J, K, M, NP2, NPMAX, N
      DATA NPMAX /MAXNP/
C--------------------------------------------------------------------
C                                        Initialisation
      NP2 = NP + 2
      N = IEND - ISTART + 1
      IRET = 0
      SCALE = SNULL
      ERRSCL = SNULL
      IF ((NP2.GT.N).OR.(NP2.LE.0).OR.(NP2.GT.NPMAX)) IRET = 1
      IF (IRET.NE.0) GO TO 999
C                                        Accumulate the curvature matrix
      DO 200 J = 1,NP2
         DO 100 K = J,NP2
            DCURV(J,K) = 0.0D0
            DO 50 M = ISTART,IEND
               IF ((M.LE.0).OR.(ACSPEC(M).EQ.SNULL).OR.
     *           (TEMPL(M).EQ.SNULL)) GO TO 50
               DX = 1.0D0 * M
               DSUMJ = 1.0D0
               IF (J.GT.2) DSUMJ = DX ** (J-2)
               IF (J.EQ.1) DSUMJ = TEMPL(M)
               DSUMK = 1.0D0
               IF (K.GT.2) DSUMK = DX ** (K-2)
               IF (K.EQ.1) DSUMK = TEMPL(M)
               DCURV(J,K) = DCURV(J,K) + DSUMJ * DSUMK
  50           CONTINUE
 100        CONTINUE
 200     CONTINUE
C                                        Fill in the symmetric elements.
      DO 300 J = 2,NP2
         M = J - 1
         DO 250 K = 1,M
            DCURV(J,K) = DCURV(K,J)
 250        CONTINUE
 300     CONTINUE
C                                        Generate the row matrix.
      DO 400 J = 1,NP2
         DB(J) = 0.0D0
         DO 350 M = ISTART,IEND
            IF ((M.LE.0).OR.(ACSPEC(M).EQ.SNULL).OR.
     *         (TEMPL(M).EQ.SNULL)) GO TO 350
            DX = 1.0D0 * M
            DSUM = 1.0D0
            IF (J.GT.2) DSUM = DX ** (J-2)
            IF (J.EQ.1) DSUM = TEMPL(M)
            DB(J) = DB(J) + ACSPEC(M) * DSUM
 350        CONTINUE
 400     CONTINUE
C                                        Invert the curvature matrix.
      CALL MTXINV (DCURV, NP2, NPMAX, DET)
      IF (DET.EQ.0.0D0) IRET = 2
      IF (IRET.NE.0) GO TO 999
C                                        Calculate the coefficients
      DO 500 J = 1,NP2
         DSUM = 0.0D0
         DO 450 K = 1,NP2
            DSUM = DSUM + DCURV(J,K) * DB(K)
 450        CONTINUE
         A(J) = DSUM
 500     CONTINUE
C                                       Calculate the variance for
C                                       the fit.
      DVAR = 0.0D0
      DO 600 M = ISTART,IEND
         IF ((M.LE.0).OR.(ACSPEC(M).EQ.SNULL).OR.
     *      (TEMPL(M).EQ.SNULL)) GO TO 600
         DSUM = TEMPL(M) * A(1)
         IF (NP2.GE.2) DSUM = DSUM + A(2)
         DO 550 K = 3,NP2
            DX = 1.0D0 * M
            DSUM = DSUM + A(K) * DX ** (K - 2)
 550        CONTINUE
         DVAR = DVAR + (ACSPEC(M) - DSUM) ** 2
 600     CONTINUE
      IF (N.GT.NP2) DVAR = DVAR / (N - NP2)
C                                       Error for coefficients
      DO 650 J = 1,NP2
         AERR(J) = 0.0
         IF (DCURV(J,J).GT.0.0D0) AERR(J) = SQRT (DVAR * DCURV(J,J))
 650     CONTINUE
C                                        Return gain and gain error
      IF (A(1).GT.0) THEN
         SCALE = 1.0 / A(1)
         ERRSCL = AERR(1) / (A(1) * A(1))
         END IF
C
 999  RETURN
      END
